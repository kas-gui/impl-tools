// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Implementation of the `#[autoimpl]` attribute

use crate::generics::{clause_to_toks, WhereClause};
use crate::{ForDeref, SimplePath};
use proc_macro2::{Span, TokenStream as Toks};
use proc_macro_error::emit_error;
use quote::{quote, TokenStreamExt};
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{parse2, Field, Fields, Ident, Index, Item, ItemStruct, Member, Path, Token};

mod impl_misc;
mod impl_using;

pub use impl_misc::*;
pub use impl_using::*;

/// List of all builtin trait implementations
pub const STD_IMPLS: &[&dyn ImplTrait] = &[
    &ImplClone,
    &ImplDebug,
    &ImplDefault,
    &ImplDeref,
    &ImplDerefMut,
];

#[allow(non_camel_case_types)]
mod kw {
    use syn::custom_keyword;

    custom_keyword!(ignore);
    custom_keyword!(using);
}

/// The `#[autoimpl]` attribute
pub enum Attr {
    /// Autoimpl for types supporting `Deref`
    ForDeref(ForDeref),
    /// Autoimpl for trait targets
    ImplTraits(ImplTraits),
}

/// Autoimpl for trait targets
pub struct ImplTraits {
    targets: Vec<Path>,
    args: ImplArgs,
}

/// Error type
pub enum Error {
    /// Emit an error clarifying that `using self.FIELD` is required
    RequireUsing,
    /// Emit an error over the target trait name using `message`
    CallSite(&'static str),
    /// Emit an error with the given `span` and `message`
    WithSpan(Span, &'static str),
}

/// Result type
pub type Result<T> = std::result::Result<T, Error>;

mod parsing {
    use super::*;
    use syn::parse::{Parse, ParseStream, Result};

    impl Parse for Attr {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut empty_or_trailing = true;
            let mut lookahead = input.lookahead1();

            if lookahead.peek(Token![for]) {
                return input.call(ForDeref::parse).map(Attr::ForDeref);
            }

            let mut targets = Vec::new();
            let mut using = None;
            let mut ignores = Vec::new();
            let mut clause = None;

            while !input.is_empty() {
                if lookahead.peek(Token![where])
                    || lookahead.peek(kw::using)
                    || lookahead.peek(kw::ignore)
                {
                    break;
                }

                if empty_or_trailing {
                    if lookahead.peek(Ident) {
                        targets.push(input.parse()?);
                        empty_or_trailing = false;
                        lookahead = input.lookahead1();
                        continue;
                    }
                } else if input.peek(Comma) {
                    let _ = input.parse::<Comma>()?;
                    empty_or_trailing = true;
                    lookahead = input.lookahead1();
                    continue;
                }
                return Err(lookahead.error());
            }

            while !input.is_empty() {
                lookahead = input.lookahead1();
                if clause.is_none() && using.is_none() && lookahead.peek(kw::using) {
                    let _: kw::using = input.parse()?;
                    let _ = input.parse::<Token![self]>()?;
                    let _ = input.parse::<Token![.]>()?;
                    using = Some(input.parse()?);
                } else if clause.is_none() && ignores.is_empty() && lookahead.peek(kw::ignore) {
                    let _: kw::ignore = input.parse()?;
                    let _ = input.parse::<Token![self]>()?;
                    let _ = input.parse::<Token![.]>()?;
                    ignores.push(input.parse()?);
                    while input.peek(Comma) {
                        let _ = input.parse::<Comma>()?;
                        if input.peek(Token![self]) {
                            let _ = input.parse::<Token![self]>()?;
                            let _ = input.parse::<Token![.]>()?;
                            ignores.push(input.parse()?);
                            continue;
                        }
                        break;
                    }
                } else if lookahead.peek(Token![where]) {
                    // Note: assigning to clause disables other match branches since clause must come last!
                    clause = Some(input.parse()?);
                } else {
                    return Err(lookahead.error());
                }
            }

            let args = ImplArgs {
                ignores,
                using,
                clause,
            };
            Ok(Attr::ImplTraits(ImplTraits { targets, args }))
        }
    }
}

impl ImplTraits {
    /// Expand over the given `item`
    ///
    /// This attribute does not modify the item.
    /// The caller should append the result to `item` tokens.
    pub fn expand(
        mut self,
        item: Toks,
        find_impl: impl Fn(&Path) -> Option<&'static dyn ImplTrait>,
    ) -> Toks {
        let item = match parse2::<Item>(item) {
            Ok(Item::Struct(item)) => item,
            Ok(item) => {
                emit_error!(item, "expected struct");
                return Toks::new();
            }
            Err(err) => {
                emit_error!(err);
                return Toks::new();
            }
        };

        let mut not_supporting_ignore = None;
        let mut not_supporting_using = None;

        let mut impl_targets: Vec<(Span, _)> = Vec::with_capacity(self.targets.len());
        for target in self.targets.drain(..) {
            let target_impl = match find_impl(&target) {
                Some(impl_) => impl_,
                None => {
                    emit_error!(target, "unsupported trait");
                    return Toks::new();
                }
            };

            if not_supporting_ignore.is_none() && !target_impl.support_ignore() {
                not_supporting_ignore = Some(target.clone());
            }
            if not_supporting_using.is_none() && !target_impl.support_using() {
                not_supporting_using = Some(target.clone());
            }

            impl_targets.push((target.span(), target_impl));
        }

        if !self.args.ignores.is_empty() {
            if let Some(ref target) = not_supporting_ignore {
                emit_error!(target, "target does not support `ignore`-d fields",);
            }
        }
        if self.args.using.is_some() {
            if let Some(target) = not_supporting_using.as_ref() {
                emit_error!(target, "`target does not support `using` a field",);
            }
        }

        fn check_is_field(mem: &Member, fields: &Fields) {
            match (fields, mem) {
                (Fields::Named(fields), Member::Named(ref ident)) => {
                    if fields
                        .named
                        .iter()
                        .any(|field| field.ident.as_ref() == Some(ident))
                    {
                        return;
                    }
                }
                (Fields::Unnamed(fields), Member::Unnamed(index)) => {
                    if (index.index as usize) < fields.unnamed.len() {
                        return;
                    }
                }
                _ => (),
            }
            emit_error!(mem, "not a struct field");
        }

        let mut toks = Toks::new();
        for mem in &self.args.ignores {
            check_is_field(mem, &item.fields);
        }
        if let Some(mem) = self.args.using_member() {
            check_is_field(mem, &item.fields);
        }

        for (span, target) in impl_targets.drain(..) {
            match target.struct_impl(&item, &self.args) {
                Ok(items) => toks.append_all(items),
                Err(error) => match error {
                    Error::RequireUsing => {
                        emit_error!(span, "target requires argument `using self.FIELD`")
                    }
                    Error::CallSite(msg) => emit_error!(span, msg),
                    Error::WithSpan(span, msg) => emit_error!(span, msg),
                },
            }
        }
        toks
    }
}

/// Arguments passed to [`ImplTrait`] implementation methods
pub struct ImplArgs {
    /// Fields ignored in attribute
    pub ignores: Vec<Member>,
    /// Field specified to 'use' in attribute
    pub using: Option<Member>,
    /// Where clause added to attribute
    pub clause: Option<WhereClause>,
}

impl ImplArgs {
    /// If true, this field is ignored
    pub fn ignore(&self, member: &Member) -> bool {
        self.ignores.iter().any(|ig| *ig == *member)
    }

    /// If true, this named field is ignored
    pub fn ignore_named(&self, ident: &Ident) -> bool {
        self.ignores.iter().any(|ig| match ig {
            Member::Named(m) => m == ident,
            _ => false,
        })
    }

    /// If true, this unnamed field is ignored
    pub fn ignore_unnamed(&self, index: &Index) -> bool {
        self.ignores.iter().any(|ig| match ig {
            Member::Unnamed(m) => m == index,
            _ => false,
        })
    }

    /// Field to "use", if any
    pub fn using_member(&self) -> Option<&Member> {
        self.using.as_ref()
    }

    /// Find find to "use", if any
    pub fn using_field<'b>(&self, fields: &'b Fields) -> Option<&'b Field> {
        match fields {
            Fields::Named(fields) => fields.named.iter().find(|field| match self.using {
                Some(Member::Named(ref ident)) => ident == field.ident.as_ref().unwrap(),
                _ => false,
            }),
            Fields::Unnamed(fields) => {
                fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .find_map(|(i, field)| match self.using {
                        Some(Member::Unnamed(ref index)) => {
                            (*index == Index::from(i)).then(|| field)
                        }
                        _ => None,
                    })
            }
            Fields::Unit => None,
        }
    }
}

/// Trait required by extensions
pub trait ImplTrait {
    /// Trait path
    ///
    /// This path is matched against trait names in `#[autoimpl]` parameters.
    fn path(&self) -> SimplePath;

    /// True if this target supports ignoring fields
    fn support_ignore(&self) -> bool;

    /// True if this target supports using a field
    fn support_using(&self) -> bool;

    /// Generate an impl for a struct item
    ///
    /// The default implementation is a wrapper around [`Self::struct_items`]
    /// and suffices for most cases. It may be overridden, e.g. to generate
    /// multiple implementation items. It is not recommended to modify the
    /// generics.
    fn struct_impl(&self, item: &ItemStruct, args: &ImplArgs) -> Result<Toks> {
        let type_ident = &item.ident;
        let (impl_generics, ty_generics, item_wc) = item.generics.split_for_impl();

        let (path, items) = self.struct_items(item, args)?;

        let wc = clause_to_toks(&args.clause, item_wc, &path);

        Ok(quote! {
            impl #impl_generics #path for #type_ident #ty_generics #wc {
                #items
            }
        })
    }

    /// Generate struct items
    ///
    /// On success, this method returns the tuple `(trait_path, items)`. These
    /// are used to generate the following implementation:
    /// ```
    /// impl #impl_generics #trait_path for #type_ident #ty_generics #where_clause {
    ///     #items
    /// }
    /// ```
    ///
    /// Note: this method is *only* called by the default implementation of [`Self::struct_impl`].
    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)>;
}
