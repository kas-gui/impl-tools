// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Implementation of the `#[autoimpl]` attribute

use crate::generics::{clause_to_toks, WhereClause};
use crate::{ForDeref, SimplePath};
use proc_macro2::{Span, TokenStream};
use proc_macro_error::emit_error;
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{parse2, Field, Fields, Ident, Index, Item, ItemStruct, Member, Path, Token};

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
    clause: Option<WhereClause>,
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

            let args = ImplArgs { ignores, using };
            Ok(Attr::ImplTraits(ImplTraits {
                targets,
                args,
                clause,
            }))
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
        item: TokenStream,
        find_impl: impl Fn(&Path) -> Option<&'static dyn ImplTrait>,
    ) -> TokenStream {
        let item = match parse2::<Item>(item) {
            Ok(Item::Struct(item)) => item,
            Ok(item) => {
                emit_error!(item, "expected struct");
                return TokenStream::new();
            }
            Err(err) => {
                emit_error!(err);
                return TokenStream::new();
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
                    return TokenStream::new();
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

        let mut toks = TokenStream::new();
        for mem in &self.args.ignores {
            check_is_field(mem, &item.fields);
        }
        if let Some(mem) = self.args.using_member() {
            check_is_field(mem, &item.fields);
        }

        let type_ident = &item.ident;
        let (impl_generics, ty_generics, item_wc) = item.generics.split_for_impl();

        for (span, target) in impl_targets.drain(..) {
            match target.struct_items(&item, &self.args) {
                Ok(items) => {
                    let path = target.path().to_token_stream();
                    let wc = clause_to_toks(&self.clause, item_wc, &path);
                    toks.append_all(quote_spanned! {span=>
                        impl #impl_generics #path for #type_ident #ty_generics #wc {
                            #items
                        }
                    });
                }
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
    ignores: Vec<Member>,
    using: Option<Member>,
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
    /// The full path is printed in implementations. Only the last component is
    /// normally used when matching.
    fn path(&self) -> SimplePath;

    /// True if this target supports ignoring fields
    fn support_ignore(&self) -> bool;

    /// True if this target supports using a field
    fn support_using(&self) -> bool;

    /// Generate struct items
    ///
    /// The resulting items are injected into an impl of the form
    /// `impl<..> TraitName for StructName<..> where .. { #items }`.
    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<TokenStream>;
}

/// List of all builtin trait implementations
pub const STD_IMPLS: &[&dyn ImplTrait] = &[
    &ImplClone,
    &ImplDebug,
    &ImplDefault,
    &ImplDeref,
    &ImplDerefMut,
];

/// Implement [`std::clone::Clone`]
pub struct ImplClone;
impl ImplTrait for ImplClone {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "std", "clone", "Clone"])
    }

    fn support_ignore(&self) -> bool {
        true
    }

    fn support_using(&self) -> bool {
        false
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<TokenStream> {
        let type_ident = &item.ident;
        let inner = match &item.fields {
            Fields::Named(fields) => {
                let mut toks = TokenStream::new();
                for field in fields.named.iter() {
                    let ident = field.ident.as_ref().unwrap();
                    if args.ignore_named(ident) {
                        toks.append_all(quote! { #ident: Default::default(), });
                    } else {
                        toks.append_all(quote! { #ident: self.#ident.clone(), });
                    }
                }
                quote! { #type_ident { #toks } }
            }
            Fields::Unnamed(fields) => {
                let mut toks = TokenStream::new();
                for i in 0..fields.unnamed.len() {
                    let index = Index::from(i);
                    if args.ignore_unnamed(&index) {
                        toks.append_all(quote! { Default::default(), });
                    } else {
                        toks.append_all(quote! { self.#index.clone(), });
                    }
                }
                quote! { #type_ident ( #toks ) }
            }
            Fields::Unit => quote! { #type_ident },
        };
        Ok(quote! {
            fn clone(&self) -> Self {
                #inner
            }
        })
    }
}

/// Implement [`std::fmt::Debug`]
pub struct ImplDebug;
impl ImplTrait for ImplDebug {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "std", "fmt", "Debug"])
    }

    fn support_ignore(&self) -> bool {
        true
    }

    fn support_using(&self) -> bool {
        false
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<TokenStream> {
        let type_name = item.ident.to_string();
        let mut inner;
        match &item.fields {
            Fields::Named(fields) => {
                inner = quote! { f.debug_struct(#type_name) };
                let mut no_skips = true;
                for field in fields.named.iter() {
                    let ident = field.ident.as_ref().unwrap();
                    if !args.ignore_named(ident) {
                        let name = ident.to_string();
                        inner.append_all(quote! {
                            .field(#name, &self.#ident)
                        });
                    } else {
                        no_skips = false;
                    }
                }
                if no_skips {
                    inner.append_all(quote! { .finish() });
                } else {
                    inner.append_all(quote! { .finish_non_exhaustive() });
                };
            }
            Fields::Unnamed(fields) => {
                inner = quote! { f.debug_tuple(#type_name) };
                for i in 0..fields.unnamed.len() {
                    let index = Index::from(i);
                    if !args.ignore_unnamed(&index) {
                        inner.append_all(quote! {
                            .field(&self.#index)
                        });
                    } else {
                        inner.append_all(quote! {
                            .field(&format_args!("_"))
                        });
                    }
                }
                inner.append_all(quote! { .finish() });
            }
            Fields::Unit => inner = quote! { f.write_str(#type_name) },
        };
        Ok(quote! {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                #inner
            }
        })
    }
}

/// Implement [`std::default::Default`]
pub struct ImplDefault;
impl ImplTrait for ImplDefault {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "std", "default", "Default"])
    }

    fn support_ignore(&self) -> bool {
        false
    }

    fn support_using(&self) -> bool {
        false
    }

    fn struct_items(&self, item: &ItemStruct, _: &ImplArgs) -> Result<TokenStream> {
        let type_ident = &item.ident;
        let mut inner;
        match &item.fields {
            Fields::Named(fields) => {
                inner = quote! {};
                for field in fields.named.iter() {
                    let ident = field.ident.as_ref().unwrap();
                    inner.append_all(quote! { #ident: Default::default(), });
                }
                inner = quote! { #type_ident { #inner } };
            }
            Fields::Unnamed(fields) => {
                inner = quote! {};
                for _ in 0..fields.unnamed.len() {
                    inner.append_all(quote! { Default::default(), });
                }
                inner = quote! { #type_ident(#inner) };
            }
            Fields::Unit => inner = quote! { #type_ident },
        }
        Ok(quote! {
            fn default() -> Self {
                #inner
            }
        })
    }
}

/// Implement [`std::ops::Deref`]
pub struct ImplDeref;
impl ImplTrait for ImplDeref {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "std", "ops", "Deref"])
    }

    fn support_ignore(&self) -> bool {
        false
    }

    fn support_using(&self) -> bool {
        true
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<TokenStream> {
        if let Some(field) = args.using_field(&item.fields) {
            let ty = field.ty.clone();
            let member = args.using_member().unwrap();
            Ok(quote! {
                type Target = #ty;
                fn deref(&self) -> &Self::Target {
                    &self.#member
                }
            })
        } else {
            Err(Error::RequireUsing)
        }
    }
}

/// Implement [`std::ops::DerefMut`]
pub struct ImplDerefMut;
impl ImplTrait for ImplDerefMut {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "std", "ops", "DerefMut"])
    }

    fn support_ignore(&self) -> bool {
        false
    }

    fn support_using(&self) -> bool {
        true
    }

    fn struct_items(&self, _: &ItemStruct, args: &ImplArgs) -> Result<TokenStream> {
        if let Some(member) = args.using_member() {
            Ok(quote! {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.#member
                }
            })
        } else {
            Err(Error::RequireUsing)
        }
    }
}
