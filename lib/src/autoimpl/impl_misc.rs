// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Miscellaneous impls

use super::{ImplArgs, ImplTrait, Result};
use crate::{IdentFormatter, SimplePath};
use proc_macro2::TokenStream as Toks;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{Fields, Index, ItemEnum, ItemStruct, Member, Token};

/// Implement [`core::clone::Clone`]
pub struct ImplClone;
impl ImplTrait for ImplClone {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "clone", "Clone"])
    }

    fn support_ignore(&self) -> bool {
        true
    }

    fn enum_items(&self, item: &ItemEnum, args: &ImplArgs) -> Result<(Toks, Toks)> {
        let mut idfmt = IdentFormatter::new();
        let name = &item.ident;
        let mut variants = Toks::new();
        for v in item.variants.iter() {
            for attr in &v.attrs {
                if attr.path().get_ident().is_some_and(|path| path == "cfg") {
                    attr.to_tokens(&mut variants);
                }
            }

            let ident = &v.ident;
            let tag = quote! { #name :: #ident };
            variants.append_all(match v.fields {
                Fields::Named(ref fields) => {
                    let mut idents = Toks::new();
                    let mut toks = Toks::new();
                    for field in fields.named.iter() {
                        for attr in &field.attrs {
                            if attr.path().get_ident().is_some_and(|path| path == "cfg") {
                                attr.to_tokens(&mut idents);
                                attr.to_tokens(&mut toks);
                            }
                        }

                        let ident = field.ident.as_ref().unwrap();
                        idents.append_all(quote! { ref #ident, });
                        toks.append_all(if args.ignore_named(ident) {
                            quote! { #ident: Default::default(), }
                        } else {
                            quote! { #ident: ::core::clone::Clone::clone(#ident), }
                        });
                    }
                    quote! { #tag { #idents } => #tag { #toks }, }
                }
                Fields::Unnamed(ref fields) => {
                    let mut bindings = Toks::new();
                    let mut toks = Toks::new();
                    for (i, field) in fields.unnamed.iter().enumerate() {
                        let ident = idfmt.make_call_site(format_args!("_{i}"));

                        for attr in &field.attrs {
                            if attr.path().get_ident().is_some_and(|path| path == "cfg") {
                                attr.to_tokens(&mut bindings);
                                attr.to_tokens(&mut toks);
                            }
                        }

                        bindings.append_all(quote! { ref #ident, });
                        toks.append_all(quote! { ::core::clone::Clone::clone(#ident), });
                    }
                    quote! { #tag ( #bindings ) => #tag ( #toks ), }
                }
                Fields::Unit => quote! { #tag => #tag, },
            });
        }
        let method = quote! {
            fn clone(&self) -> Self {
                match *self {
                    #variants
                }
            }
        };
        Ok((quote! { ::core::clone::Clone }, method))
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        let type_ident = &item.ident;
        let inner = match &item.fields {
            Fields::Named(fields) => {
                let mut toks = Toks::new();
                for field in fields.named.iter() {
                    for attr in &field.attrs {
                        if attr.path().get_ident().is_some_and(|path| path == "cfg") {
                            attr.to_tokens(&mut toks);
                        }
                    }

                    let ident = field.ident.as_ref().unwrap();
                    toks.append_all(if args.ignore_named(ident) {
                        quote! { #ident: Default::default(), }
                    } else {
                        quote! { #ident: ::core::clone::Clone::clone(&self.#ident), }
                    });
                }
                quote! { #type_ident { #toks } }
            }
            Fields::Unnamed(fields) => {
                let mut toks = Toks::new();
                for (i, field) in fields.unnamed.iter().enumerate() {
                    for attr in &field.attrs {
                        if attr.path().get_ident().is_some_and(|path| path == "cfg") {
                            attr.to_tokens(&mut toks);
                        }
                    }

                    let index = Index::from(i);
                    toks.append_all(if args.ignore_unnamed(&index) {
                        quote! { Default::default(), }
                    } else {
                        quote! { ::core::clone::Clone::clone(&self.#index), }
                    });
                }
                quote! { #type_ident ( #toks ) }
            }
            Fields::Unit => quote! { #type_ident },
        };
        let method = quote! {
            fn clone(&self) -> Self {
                #inner
            }
        };
        Ok((quote! { ::core::clone::Clone }, method))
    }
}

/// Implement [`core::marker::Copy`]
pub struct ImplCopy;
impl ImplTrait for ImplCopy {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "marker", "Copy"])
    }

    fn allow_ignore_with(&self) -> Option<SimplePath> {
        Some(SimplePath::new(&["", "core", "clone", "Clone"]))
    }

    fn enum_items(&self, _: &ItemEnum, _: &ImplArgs) -> Result<(Toks, Toks)> {
        Ok((quote! { ::core::marker::Copy }, quote! {}))
    }

    fn struct_items(&self, _: &ItemStruct, _: &ImplArgs) -> Result<(Toks, Toks)> {
        Ok((quote! { ::core::marker::Copy }, quote! {}))
    }
}

/// Implement [`core::fmt::Debug`]
pub struct ImplDebug;
impl ImplTrait for ImplDebug {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "fmt", "Debug"])
    }

    fn support_ignore(&self) -> bool {
        true
    }

    fn enum_items(&self, item: &ItemEnum, args: &ImplArgs) -> Result<(Toks, Toks)> {
        let mut idfmt = IdentFormatter::new();
        let name = &item.ident;
        let type_name = item.ident.to_string();
        let mut variants = Toks::new();
        for v in item.variants.iter() {
            let ident = &v.ident;
            let var_name = ident.to_string();
            let tag = quote! { #name :: #ident };
            variants.append_all(match v.fields {
                Fields::Named(ref fields) => {
                    let mut idents = Toks::new();
                    let mut stmts = quote! { let mut debug = f.debug_struct(#var_name); };
                    for field in fields.named.iter() {
                        for attr in &field.attrs {
                            if attr.path().get_ident().is_some_and(|path| path == "cfg") {
                                attr.to_tokens(&mut idents);
                                attr.to_tokens(&mut stmts);
                            }
                        }

                        let ident = field.ident.as_ref().unwrap();
                        idents.append_all(quote! { ref #ident, });
                        if !args.ignore_named(ident) {
                            let name = ident.to_string();
                            stmts.append_all(quote! { { debug.field(#name, #ident); } });
                        }
                    }
                    stmts.append_all(quote! { debug.finish() });

                    quote! { #tag { #idents } => { #stmts }, }
                }
                Fields::Unnamed(ref fields) => {
                    let mut bindings = Toks::new();
                    let mut stmts = quote! { let mut debug = f.debug_tuple(#var_name); };
                    for (i, field) in fields.unnamed.iter().enumerate() {
                        for attr in &field.attrs {
                            if attr.path().get_ident().is_some_and(|path| path == "cfg") {
                                attr.to_tokens(&mut bindings);
                                attr.to_tokens(&mut stmts);
                            }
                        }

                        let ident = idfmt.make_call_site(format_args!("_{i}"));
                        bindings.append_all(quote! { ref #ident, });
                        stmts.append_all(quote! { { debug.field(#ident); } });
                    }
                    quote! {
                        #tag ( #bindings ) => { #stmts; debug.finish() },
                    }
                }
                Fields::Unit => quote! { #tag => f.write_str(#var_name), },
            })
        }

        // Note: unlike #[derive(Debug)], we include the name of the enum!
        let method = quote! {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{}::", #type_name)?;
                match *self {
                    #variants
                }
            }
        };
        Ok((quote! { ::core::fmt::Debug }, method))
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        let type_name = item.ident.to_string();
        let inner = match &item.fields {
            Fields::Named(fields) => {
                let mut stmts = quote! { let mut debug = f.debug_struct(#type_name); };
                let mut no_skips = true;
                for field in fields.named.iter() {
                    for attr in &field.attrs {
                        if attr.path().get_ident().is_some_and(|path| path == "cfg") {
                            attr.to_tokens(&mut stmts);
                        }
                    }

                    let ident = field.ident.as_ref().unwrap();
                    if !args.ignore_named(ident) {
                        let name = ident.to_string();
                        stmts.append_all(quote! { { debug.field(#name, &self.#ident); } });
                    } else {
                        no_skips = false;
                    }
                }
                if no_skips {
                    quote! { #stmts; debug.finish() }
                } else {
                    quote! { #stmts; debug.finish_non_exhaustive() }
                }
            }
            Fields::Unnamed(fields) => {
                let mut stmts = quote! { let mut debug = f.debug_tuple(#type_name); };
                for (i, field) in fields.unnamed.iter().enumerate() {
                    for attr in &field.attrs {
                        if attr.path().get_ident().is_some_and(|path| path == "cfg") {
                            attr.to_tokens(&mut stmts);
                        }
                    }

                    let index = Index::from(i);
                    if !args.ignore_unnamed(&index) {
                        stmts.append_all(quote! { { debug.field(&self.#index); } });
                    } else {
                        stmts.append_all(quote! { { debug.field(&format_args!("_")); } });
                    }
                }
                quote! { #stmts; debug.finish() }
            }
            Fields::Unit => quote! { f.write_str(#type_name) },
        };

        let method = quote! {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                #inner
            }
        };
        Ok((quote! { ::core::fmt::Debug }, method))
    }
}

/// Implement [`core::default::Default`]
pub struct ImplDefault;
impl ImplTrait for ImplDefault {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "default", "Default"])
    }

    fn struct_items(&self, item: &ItemStruct, _: &ImplArgs) -> Result<(Toks, Toks)> {
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
        let method = quote! {
            fn default() -> Self {
                #inner
            }
        };
        Ok((quote! { ::core::default::Default }, method))
    }
}

/// Implement [`core::cmp::PartialEq`]
///
/// Restriction: `Rhs == Self`
pub struct ImplPartialEq;
impl ImplTrait for ImplPartialEq {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "cmp", "PartialEq"])
    }

    fn support_ignore(&self) -> bool {
        true
    }

    fn enum_items(&self, item: &ItemEnum, _: &ImplArgs) -> Result<(Toks, Toks)> {
        let mut idfmt = IdentFormatter::new();
        let name = &item.ident;
        let mut variants = Toks::new();
        for v in item.variants.iter() {
            let ident = &v.ident;
            let tag = quote! { #name :: #ident };
            variants.append_all(match v.fields {
                Fields::Named(ref fields) => {
                    let mut l_args = quote! {};
                    let mut r_args = quote! {};
                    let mut cond = quote! {};
                    for (i, field) in fields.named.iter().enumerate() {
                        let ident = field.ident.as_ref().unwrap();
                        let li = idfmt.make_call_site(format_args!("__l{i}"));
                        let ri = idfmt.make_call_site(format_args!("__r{i}"));
                        l_args.append_all(quote! { #ident: #li, });
                        r_args.append_all(quote! { #ident: #ri, });
                        if !cond.is_empty() {
                            cond.append_all(quote! { && });
                        }
                        cond.append_all(quote! { #li == #ri });
                    }

                    quote! { (#tag { #l_args }, #tag { #r_args }) => #cond, }
                }
                Fields::Unnamed(ref fields) => {
                    let len = fields.unnamed.len();
                    let mut l_args = quote! {};
                    let mut r_args = quote! {};
                    let mut cond = quote! {};
                    for i in 0..len {
                        let li = idfmt.make_call_site(format_args!("__l{i}"));
                        let ri = idfmt.make_call_site(format_args!("__r{i}"));
                        l_args.append_all(quote! { #li, });
                        r_args.append_all(quote! { #ri, });
                        if !cond.is_empty() {
                            cond.append_all(quote! { && });
                        }
                        cond.append_all(quote! { #li == #ri });
                    }

                    quote! { (#tag ( #l_args ), #tag ( #r_args )) => #cond, }
                }
                Fields::Unit => quote! { (#tag, #tag) => true, },
            });
        }
        variants.append_all(quote! { (_, _) => false, });

        let method = quote! {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    #variants
                }
            }
        };
        Ok((quote! { ::core::cmp::PartialEq }, method))
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        let mut toks = Toks::new();
        let mut require_sep = false;
        args.for_fields(&item.fields, |member: Member, _| {
            if require_sep {
                <Token![&&]>::default().to_tokens(&mut toks);
            }
            toks.append_all(quote! { self.#member == other.#member });
            require_sep = true;
        });
        if toks.is_empty() {
            toks = quote! { true };
        }

        let method = quote! {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                #toks
            }
        };
        Ok((quote! { ::core::cmp::PartialEq }, method))
    }
}

/// Implement [`core::cmp::Eq`]
pub struct ImplEq;
impl ImplTrait for ImplEq {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "cmp", "Eq"])
    }

    fn allow_ignore_with(&self) -> Option<SimplePath> {
        Some(SimplePath::new(&["", "core", "cmp", "PartialEq"]))
    }

    fn enum_items(&self, _: &ItemEnum, _: &ImplArgs) -> Result<(Toks, Toks)> {
        Ok((quote! { ::core::cmp::Eq }, quote! {}))
    }

    fn struct_items(&self, _: &ItemStruct, _: &ImplArgs) -> Result<(Toks, Toks)> {
        Ok((quote! { ::core::cmp::Eq }, quote! {}))
    }
}

/// Implement [`core::cmp::PartialOrd`]
///
/// Restriction: `Rhs == Self`
pub struct ImplPartialOrd;
impl ImplTrait for ImplPartialOrd {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "cmp", "PartialOrd"])
    }

    fn support_ignore(&self) -> bool {
        true
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        let mut toks = Toks::new();
        args.for_fields_iter(item.fields.iter().enumerate().rev(), |member: Member, _| {
            let cmp =
                quote! { ::core::cmp::PartialOrd::partial_cmp(&self.#member, &other.#member) };
            if toks.is_empty() {
                toks = cmp;
            } else {
                toks = quote! {
                    match #cmp {
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal) => #toks,
                        cmp => cmp,
                    }
                }
            }
        });
        if toks.is_empty() {
            toks = quote! { ::core::option::Option::Some(::core::cmp::Ordering::Equal) };
        }

        let method = quote! {
            #[inline]
            fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                #toks
            }
        };
        Ok((quote! { ::core::cmp::PartialOrd }, method))
    }
}

/// Implement [`core::cmp::Ord`]
pub struct ImplOrd;
impl ImplTrait for ImplOrd {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "cmp", "Ord"])
    }

    fn support_ignore(&self) -> bool {
        true
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        let mut toks = Toks::new();
        args.for_fields_iter(item.fields.iter().enumerate().rev(), |member: Member, _| {
            let cmp = quote! { ::core::cmp::Ord::cmp(&self.#member, &other.#member) };
            if toks.is_empty() {
                toks = cmp;
            } else {
                toks = quote! {
                    match #cmp {
                        ::core::cmp::Ordering::Equal => #toks,
                        cmp => cmp,
                    }
                }
            }
        });
        if toks.is_empty() {
            toks = quote! { ::core::cmp::Ordering::Equal };
        }

        let method = quote! {
            #[inline]
            fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                #toks
            }
        };
        Ok((quote! { ::core::cmp::Ord }, method))
    }
}

/// Implement [`core::hash::Hash`]
pub struct ImplHash;
impl ImplTrait for ImplHash {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "hash", "Hash"])
    }

    fn support_ignore(&self) -> bool {
        true
    }

    fn enum_items(&self, item: &ItemEnum, _: &ImplArgs) -> Result<(Toks, Toks)> {
        let mut idfmt = IdentFormatter::new();
        let name = &item.ident;
        let mut variants = Toks::new();
        for v in item.variants.iter() {
            let ident = &v.ident;
            variants.append_all(quote! { #name :: #ident });
            variants.append_all(match v.fields {
                Fields::Named(ref fields) => {
                    let idents = fields.named.iter().map(|f| f.ident.as_ref().unwrap());
                    let hashes = fields.named.iter().map(|f| {
                        let ident = f.ident.as_ref().unwrap();
                        quote! { ::core::hash::Hash::hash(&#ident, state); }
                    });
                    quote! { { #(ref #idents),* } => { #(#hashes);* } }
                }
                Fields::Unnamed(ref fields) => {
                    let len = fields.unnamed.len();
                    let mut bindings = Vec::with_capacity(len);
                    let mut hashes = quote! {};
                    for i in 0..len {
                        let ident = idfmt.make_call_site(format_args!("_{i}"));
                        bindings.push(quote! { ref #ident });
                        hashes.append_all(quote! {
                            ::core::hash::Hash::hash(#ident, state);
                        });
                    }
                    quote! { ( #(#bindings),* ) => { #hashes } }
                }
                Fields::Unit => quote! { => (), },
            });
        }
        let method = quote! {
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                match *self {
                    #variants
                }
            }
        };
        Ok((quote! { ::core::hash::Hash }, method))
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        let mut toks = Toks::new();
        args.for_fields_iter(item.fields.iter().enumerate().rev(), |member: Member, _| {
            toks.append_all(quote! { ::core::hash::Hash::hash(&self.#member, state); });
        });

        let method = quote! {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                #toks
            }
        };
        Ok((quote! { ::core::hash::Hash }, method))
    }
}
