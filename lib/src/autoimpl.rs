// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Implementation of the `#[autoimpl]` attribute

use crate::generics::{clause_to_toks, WhereClause};
use crate::SimplePath;
use proc_macro2::{Span, TokenStream as Toks};
use proc_macro_error2::emit_error;
use quote::{quote, TokenStreamExt};
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{
    parse2, Field, Fields, Ident, Index, Item, ItemEnum, ItemStruct, Member, Path, PathArguments,
    Token,
};

mod for_deref;
mod impl_misc;
mod impl_using;

pub use for_deref::ForDeref;
pub use impl_misc::*;
pub use impl_using::*;

/// List of all builtin trait implementations
pub const STD_IMPLS: &[&dyn ImplTrait] = &[
    &ImplClone,
    &ImplCopy,
    &ImplDebug,
    &ImplDefault,
    &ImplPartialEq,
    &ImplEq,
    &ImplPartialOrd,
    &ImplOrd,
    &ImplHash,
    &ImplBorrow,
    &ImplBorrowMut,
    &ImplAsRef,
    &ImplAsMut,
    &ImplDeref,
    &ImplDerefMut,
];

/// Trait required by extensions
pub trait ImplTrait {
    /// Trait path
    ///
    /// This path is matched against trait names in `#[autoimpl]` parameters.
    fn path(&self) -> SimplePath;

    /// True if this target supports path arguments
    fn support_path_arguments(&self) -> bool {
        false
    }

    /// True if this target supports ignoring fields
    ///
    /// Default implementation: `false`
    fn support_ignore(&self) -> bool {
        false
    }

    /// If the target does not support `ignore` but does tolerate `ignore` in
    /// the presence of another target (e.g. `autoimpl(Eq, PartialEq ignore self.foo)`),
    /// return the path of that other target here.
    fn allow_ignore_with(&self) -> Option<SimplePath> {
        None
    }

    /// True if this target supports using a field
    ///
    /// Default implementation: `false`
    fn support_using(&self) -> bool {
        false
    }

    /// Generate an impl for an enum item
    ///
    /// The default implementation is a wrapper around [`Self::enum_items`]
    /// and suffices for most cases. It may be overridden, e.g. to generate
    /// multiple implementation items. It is not recommended to modify the
    /// generics.
    fn enum_impl(&self, item: &ItemEnum, args: &ImplArgs) -> Result<Toks> {
        let type_ident = &item.ident;
        let (impl_generics, ty_generics, item_wc) = item.generics.split_for_impl();

        let (path, items) = self.enum_items(item, args)?;

        let wc = clause_to_toks(&args.clause, item_wc, &path);

        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics #path for #type_ident #ty_generics #wc {
                #items
            }
        })
    }

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
            #[automatically_derived]
            impl #impl_generics #path for #type_ident #ty_generics #wc {
                #items
            }
        })
    }

    /// Generate enum items
    ///
    /// On success, this method returns the tuple `(trait_path, items)`. These
    /// are used to generate the following implementation:
    /// ```ignore
    /// impl #impl_generics #trait_path for #type_ident #ty_generics #where_clause {
    ///     #items
    /// }
    /// ```
    ///
    /// Note: this method is *only* called by the default implementation of [`Self::enum_impl`].
    ///
    /// Default implementation: returns an error indicating that enum expansion is not supported.
    fn enum_items(&self, item: &ItemEnum, args: &ImplArgs) -> Result<(Toks, Toks)> {
        let _ = (item, args);
        Err(Error::CallSite("enum expansion not supported"))
    }

    /// Generate struct items
    ///
    /// On success, this method returns the tuple `(trait_path, items)`. These
    /// are used to generate the following implementation:
    /// ```ignore
    /// impl #impl_generics #trait_path for #type_ident #ty_generics #where_clause {
    ///     #items
    /// }
    /// ```
    ///
    /// Note: this method is *only* called by the default implementation of [`Self::struct_impl`].
    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)>;
}

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
    /// Emit an error regarding path arguments
    PathArguments(&'static str),
}

impl Error {
    /// Report via [`proc_macro_error::emit_error`].
    pub fn emit(self, target: Span, path_args: Span) {
        match self {
            Error::RequireUsing => {
                emit_error!(target, "target requires argument `using self.FIELD`")
            }
            Error::CallSite(msg) => emit_error!(target, msg),
            Error::WithSpan(span, msg) => emit_error!(span, msg),
            Error::PathArguments(msg) => emit_error!(path_args, msg),
        }
    }
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
                path_arguments: PathArguments::None,
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
    ///
    /// Errors are reported via [`proc_macro_error::emit_error`].
    pub fn expand(
        self,
        item: Toks,
        find_impl: impl Fn(&Path) -> Option<&'static dyn ImplTrait>,
    ) -> Toks {
        match parse2::<Item>(item) {
            Ok(Item::Enum(item)) => self.expand_enum(item, find_impl),
            Ok(Item::Struct(item)) => self.expand_struct(item, find_impl),
            Ok(item) => {
                emit_error!(item, "expected struct");
                Toks::new()
            }
            Err(err) => err.into_compile_error(),
        }
    }

    fn expand_enum(
        self,
        item: ItemEnum,
        find_impl: impl Fn(&Path) -> Option<&'static dyn ImplTrait>,
    ) -> Toks {
        let ImplTraits {
            mut targets,
            mut args,
        } = self;

        if !args.ignores.is_empty() {
            let ignores = args.ignores.iter();
            let list = quote! { #(#ignores)* };
            emit_error!(list, "enum expansion does not currently support `ignore`",);
            return Toks::new();
        }
        if let Some(mem) = args.using {
            emit_error!(mem, "enum expansion does not currently support `using`",);
            return Toks::new();
        }

        let mut impl_targets: Vec<(Span, _, _)> = Vec::with_capacity(targets.len());
        for mut target in targets.drain(..) {
            let target_span = target.span();
            let path_args = target
                .segments
                .last_mut()
                .map(|seg| std::mem::take(&mut seg.arguments))
                .unwrap_or(PathArguments::None);
            let target_impl = match find_impl(&target) {
                Some(impl_) => impl_,
                None => {
                    emit_error!(target, "unsupported trait");
                    return Toks::new();
                }
            };

            if !(path_args.is_empty() || target_impl.support_path_arguments()) {
                emit_error!(
                    target_span,
                    "target {} does not support path arguments",
                    target_impl.path()
                );
            }

            impl_targets.push((target.span(), target_impl, path_args));
        }

        let mut toks = Toks::new();

        for (span, target, path_args) in impl_targets.drain(..) {
            let path_args_span = path_args.span();
            args.path_arguments = path_args;
            match target.enum_impl(&item, &args) {
                Ok(items) => toks.append_all(items),
                Err(error) => error.emit(span, path_args_span),
            }
        }
        toks
    }

    fn expand_struct(
        self,
        item: ItemStruct,
        find_impl: impl Fn(&Path) -> Option<&'static dyn ImplTrait>,
    ) -> Toks {
        let ImplTraits {
            mut targets,
            mut args,
        } = self;

        let mut not_supporting_ignore = vec![];
        let mut not_supporting_using = vec![];

        let mut impl_targets: Vec<(Span, _, _)> = Vec::with_capacity(targets.len());
        for mut target in targets.drain(..) {
            let target_span = target.span();
            let path_args = target
                .segments
                .last_mut()
                .map(|seg| std::mem::take(&mut seg.arguments))
                .unwrap_or(PathArguments::None);
            let target_impl = match find_impl(&target) {
                Some(impl_) => impl_,
                None => {
                    emit_error!(target, "unsupported trait");
                    return Toks::new();
                }
            };

            if !target_impl.support_ignore() {
                let except_with = target_impl.allow_ignore_with();
                not_supporting_ignore.push((target.clone(), except_with));
            }
            if !target_impl.support_using() {
                not_supporting_using.push(target.clone());
            }
            if !(path_args.is_empty() || target_impl.support_path_arguments()) {
                emit_error!(
                    target_span,
                    "target {} does not support path arguments",
                    target_impl.path()
                );
            }

            impl_targets.push((target.span(), target_impl, path_args));
        }

        if !args.ignores.is_empty() {
            for (target, except_with) in not_supporting_ignore.into_iter() {
                if let Some(path) = except_with {
                    if impl_targets
                        .iter()
                        .any(|(_, target_impl, _)| path == target_impl.path())
                    {
                        continue;
                    }
                }
                emit_error!(target, "target does not support `ignore`",);
            }
        }
        if args.using.is_some() {
            for target in not_supporting_using.into_iter() {
                emit_error!(target, "target does not support `using`",);
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
        for mem in &args.ignores {
            check_is_field(mem, &item.fields);
        }
        if let Some(mem) = args.using_member() {
            check_is_field(mem, &item.fields);
        }

        for (span, target, path_args) in impl_targets.drain(..) {
            let path_args_span = path_args.span();
            args.path_arguments = path_args;
            match target.struct_impl(&item, &args) {
                Ok(items) => toks.append_all(items),
                Err(error) => error.emit(span, path_args_span),
            }
        }
        toks
    }
}

/// Arguments passed to [`ImplTrait`] implementation methods
pub struct ImplArgs {
    /// Path arguments to trait
    ///
    /// Example: if the target is `Deref<Target = T>`, this is `<Target = T>`.
    /// This is always empty unless [`ImplTrait::support_path_arguments`] returns true.
    pub path_arguments: PathArguments,
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

    /// Find field to "use", if any
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

    /// Call the given closure over all non-ignored fields
    pub fn for_fields<'f>(&self, fields: &'f Fields, f: impl FnMut(Member, &'f Field)) {
        self.for_fields_iter(fields.iter().enumerate(), f);
    }

    /// Call the given closure over all non-ignored fields
    pub fn for_fields_iter<'f>(
        &self,
        fields: impl Iterator<Item = (usize, &'f Field)>,
        mut f: impl FnMut(Member, &'f Field),
    ) {
        for (i, field) in fields {
            let member = match field.ident.clone() {
                Some(ident) => Member::Named(ident),
                None => Member::Unnamed(Index::from(i)),
            };
            if !self.ignore(&member) {
                f(member, field);
            }
        }
    }
}
