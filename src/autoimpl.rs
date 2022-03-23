// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

use crate::generics::{
    clause_to_toks, impl_generics, GenericParam, Generics, TypeParamBound, WhereClause,
    WherePredicate,
};
use proc_macro2::{Literal, Span, TokenStream};
use proc_macro_error::{emit_call_site_error, emit_error};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::parse::{Error, Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{
    Field, Fields, FnArg, Ident, ItemStruct, ItemTrait, Member, Path, PathArguments, Token,
    TraitItem, Type, TypePath,
};

#[allow(non_camel_case_types)]
mod kw {
    use syn::custom_keyword;

    custom_keyword!(ignore);
    custom_keyword!(using);
}

/// Traits targetting many fields
#[derive(Clone, Copy, Debug)]
enum TraitMany {
    Clone(Span),
    Debug(Span),
    Default(Span),
}
/// Traits targetting one field
#[derive(Clone, Copy, Debug)]
enum TraitOne {
    Deref(Span),
    DerefMut(Span),
}
#[derive(Clone, Copy, Debug)]
#[allow(clippy::enum_variant_names)]
enum Class {
    Many(TraitMany),
    One(TraitOne),
}
fn class(ident: &Ident) -> Option<Class> {
    if ident == "Clone" {
        Some(Class::Many(TraitMany::Clone(ident.span())))
    } else if ident == "Debug" {
        Some(Class::Many(TraitMany::Debug(ident.span())))
    } else if ident == "Default" {
        Some(Class::Many(TraitMany::Default(ident.span())))
    } else if ident == "Deref" {
        Some(Class::One(TraitOne::Deref(ident.span())))
    } else if ident == "DerefMut" {
        Some(Class::One(TraitOne::DerefMut(ident.span())))
    } else {
        None
    }
}

#[derive(Debug)]
enum Body {
    For {
        generics: Generics,
        definitive: Ident,
        targets: Punctuated<Type, Comma>,
    },
    Many {
        targets: Vec<TraitMany>,
        ignores: Vec<Member>,
        clause: Option<WhereClause>,
    },
    One {
        targets: Vec<TraitOne>,
        using: Member,
        clause: Option<WhereClause>,
    },
}

pub struct AutoImpl {
    body: Body,
}

impl Parse for AutoImpl {
    fn parse(input: ParseStream) -> Result<Self> {
        enum Mode {
            None,
            One,
            Many,
            Default,
        }
        let mut mode = Mode::None;

        let mut empty_or_trailing = true;
        let mut lookahead = input.lookahead1();

        if lookahead.peek(Token![for]) {
            let _ = input.parse::<Token![for]>()?;
            let mut generics: Generics = input.parse()?;

            let targets = Punctuated::parse_separated_nonempty(input)?;

            lookahead = input.lookahead1();
            if lookahead.peek(Token![where]) {
                generics.where_clause = Some(input.parse()?);
                lookahead = input.lookahead1();
            }

            if !input.is_empty() {
                return Err(lookahead.error());
            }

            let mut definitive: Option<Ident> = None;
            for param in &generics.params {
                if let GenericParam::Type(param) = param {
                    for bound in &param.bounds {
                        if matches!(bound, TypeParamBound::TraitSubst(_)) {
                            definitive = Some(param.ident.clone());
                            break;
                        }
                    }
                }
            }
            if definitive.is_none() {
                if let Some(clause) = generics.where_clause.as_ref() {
                    for pred in &clause.predicates {
                        if let WherePredicate::Type(pred) = pred {
                            for bound in &pred.bounds {
                                if matches!(bound, TypeParamBound::TraitSubst(_)) {
                                    match pred.bounded_ty {
                                        Type::Path(TypePath {
                                            qself: None,
                                            path:
                                                Path {
                                                    leading_colon: None,
                                                    ref segments,
                                                },
                                        }) if segments.len() == 1
                                            && matches!(
                                                segments[0].arguments,
                                                PathArguments::None
                                            ) =>
                                        {
                                            definitive = Some(segments[0].ident.clone());
                                            break;
                                        }
                                        _ => (),
                                    }
                                }
                            }
                        }
                    }
                }
            }
            let definitive = match definitive {
                Some(def) => def,
                None => {
                    return Err(Error::new(Span::call_site(), "no definitive type parameter — either a type parameter must have bound like `T: trait` or the type must be specified explicitly, e.g. `using dyn MyTrait`"));
                }
            };

            let body = Body::For {
                generics,
                definitive,
                targets,
            };
            return Ok(AutoImpl { body });
        }

        let mut targets_many = Vec::new();
        let mut targets_one = Vec::new();
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
                    const MSG: &str = "incompatible: traits targetting a single field and traits targetting multiple fields may not be derived simultaneously";
                    let target = input.parse()?;
                    match class(&target) {
                        Some(Class::Many(TraitMany::Default(span))) => {
                            targets_many.push(TraitMany::Default(span));
                            match mode {
                                Mode::None | Mode::Many => mode = Mode::Default,
                                Mode::One => return Err(Error::new(target.span(), MSG)),
                                Mode::Default => (),
                            }
                        }
                        Some(Class::Many(trait_)) => {
                            targets_many.push(trait_);
                            match mode {
                                Mode::None => mode = Mode::Many,
                                Mode::One => return Err(Error::new(target.span(), MSG)),
                                Mode::Many | Mode::Default => (),
                            }
                        }
                        Some(Class::One(trait_)) => {
                            targets_one.push(trait_);
                            match mode {
                                Mode::None => mode = Mode::One,
                                Mode::One => (),
                                Mode::Many | Mode::Default => {
                                    return Err(Error::new(target.span(), MSG))
                                }
                            }
                        }
                        None => {
                            return Err(Error::new(target.span(), "unsupported trait"));
                        }
                    }
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

        lookahead = input.lookahead1();
        if matches!(mode, Mode::One) {
            let _: kw::using = input.parse()?;
            let _ = input.parse::<Token![self]>()?;
            let _ = input.parse::<Token![.]>()?;
            using = Some(input.parse()?);
            lookahead = input.lookahead1();
        } else if lookahead.peek(kw::ignore) {
            let ignore: kw::ignore = input.parse()?;
            if matches!(mode, Mode::Default) {
                emit_error!(
                    ignore,
                    "cannot ignore fields when implementing std::default::Default"
                );
            }
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
            lookahead = input.lookahead1();
        }

        if lookahead.peek(Token![where]) {
            clause = Some(input.parse()?);
            lookahead = input.lookahead1();
        }

        if !input.is_empty() {
            return Err(lookahead.error());
        }

        let body = if matches!(mode, Mode::One) {
            Body::One {
                targets: targets_one,
                using: using.unwrap(),
                clause,
            }
        } else {
            Body::Many {
                targets: targets_many,
                ignores,
                clause,
            }
        };

        Ok(AutoImpl { body })
    }
}

pub fn autoimpl_trait(mut attr: AutoImpl, item: ItemTrait) -> TokenStream {
    let mut toks = TokenStream::new();
    match &mut attr.body {
        Body::For {
            generics,
            definitive,
            targets,
        } => {
            let trait_ident = &item.ident;
            let (_, ty_generics, _) = item.generics.split_for_impl();
            let trait_ty = quote! { #trait_ident #ty_generics };
            let impl_generics = impl_generics(generics, &trait_ty);
            let where_clause = clause_to_toks(
                &generics.where_clause,
                item.generics.where_clause.as_ref(),
                &trait_ty,
            );

            let definitive = quote! { < #definitive as #trait_ty > };

            for target in targets {
                let mut impl_items = TokenStream::new();
                for item in &item.items {
                    match item {
                        TraitItem::Const(item) => {
                            let ident = &item.ident;
                            let ty = &item.ty;
                            impl_items.append_all(quote! {
                                const #ident : #ty = #definitive :: #ident;
                            });
                        }
                        TraitItem::Method(item) => {
                            let sig = &item.sig;
                            let ident = &sig.ident;
                            let params = sig.inputs.iter().map(|arg| match arg {
                                FnArg::Receiver(arg) => &arg.self_token as &dyn ToTokens,
                                FnArg::Typed(arg) => &arg.pat,
                            });
                            impl_items.append_all(quote! {
                                #sig {
                                    #definitive :: #ident ( #(#params),* )
                                }
                            });
                        }
                        TraitItem::Type(item) => {
                            let ident = &item.ident;
                            impl_items.append_all(quote! {
                                type #ident = #definitive :: #ident;
                            });
                        }
                        TraitItem::Macro(item) => {
                            emit_error!(item, "unsupported: macro item in trait");
                        }
                        TraitItem::Verbatim(item) => {
                            emit_error!(item, "unsupported: verbatim item in trait");
                        }

                        #[cfg(test)]
                        TraitItem::__TestExhaustive(_) => unimplemented!(),
                        #[cfg(not(test))]
                        _ => (),
                    }
                }

                toks.append_all(quote! {
                    impl #impl_generics #trait_ty for #target #where_clause {
                        #impl_items
                    }
                });
            }
        }
        _ => emit_call_site_error!("autoimpl: expected `for<Params..> Types..` on trait item"),
    }
    toks
}

pub fn autoimpl_struct(attr: AutoImpl, item: ItemStruct) -> TokenStream {
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
    match &attr.body {
        Body::For { .. } => {
            emit_call_site_error!("autoimpl: unexpected: `for<..> ..` on struct item")
        }
        Body::Many { ignores, .. } => {
            for mem in ignores {
                check_is_field(mem, &item.fields);
            }
        }
        Body::One { using, .. } => check_is_field(using, &item.fields),
    }

    let mut toks = TokenStream::new();
    match attr.body {
        Body::For { .. } => {
            emit_call_site_error!("autoimpl: `for<..>` not supported on struct item")
        }
        Body::Many {
            targets,
            ignores,
            ref clause,
        } => autoimpl_many(targets, ignores, item, clause, &mut toks),
        Body::One {
            targets,
            using,
            ref clause,
        } => autoimpl_one(targets, using, item, clause, &mut toks),
    }
    toks
}

fn autoimpl_many(
    mut targets: Vec<TraitMany>,
    ignores: Vec<Member>,
    item: ItemStruct,
    clause: &Option<WhereClause>,
    toks: &mut TokenStream,
) {
    let no_skips = ignores.is_empty();
    let ignore = |item: &Member| -> bool { ignores.iter().any(|mem| *mem == *item) };
    let ident = &item.ident;
    let (impl_generics, ty_generics, item_wc) = item.generics.split_for_impl();

    for target in targets.drain(..) {
        match target {
            TraitMany::Clone(span) => {
                let mut inner = quote! {};
                for (i, field) in item.fields.iter().enumerate() {
                    let mem = if let Some(ref id) = field.ident {
                        inner.append_all(quote! { #id: });
                        Member::from(id.clone())
                    } else {
                        Member::from(i)
                    };

                    if ignore(&mem) {
                        inner.append_all(quote! { Default::default(), });
                    } else {
                        inner.append_all(quote! { self.#mem.clone(), });
                    }
                }
                let inner = match &item.fields {
                    Fields::Named(_) => quote! { Self { #inner } },
                    Fields::Unnamed(_) => quote! { Self( #inner ) },
                    Fields::Unit => quote! { Self },
                };
                let wc = clause_to_toks(clause, item_wc, &quote! { std::clone::Clone });
                toks.append_all(quote_spanned! {span=>
                    impl #impl_generics std::clone::Clone for #ident #ty_generics #wc {
                        fn clone(&self) -> Self {
                            #inner
                        }
                    }
                });
            }
            TraitMany::Debug(span) => {
                let name = ident.to_string();
                let mut inner;
                match item.fields {
                    Fields::Named(ref fields) => {
                        inner = quote! { f.debug_struct(#name) };
                        for field in fields.named.iter() {
                            let ident = field.ident.as_ref().unwrap();
                            if !ignore(&ident.clone().into()) {
                                let name = ident.to_string();
                                inner.append_all(quote! {
                                    .field(#name, &self.#ident)
                                });
                            }
                        }
                        if no_skips {
                            inner.append_all(quote! { .finish() });
                        } else {
                            inner.append_all(quote! { .finish_non_exhaustive() });
                        };
                    }
                    Fields::Unnamed(ref fields) => {
                        inner = quote! { f.debug_tuple(#name) };
                        for i in 0..fields.unnamed.len() {
                            if !ignore(&i.into()) {
                                let lit = Literal::usize_unsuffixed(i);
                                inner.append_all(quote! {
                                    .field(&self.#lit)
                                });
                            }
                        }
                        inner.append_all(quote! { .finish() });
                    }
                    Fields::Unit => {
                        inner = quote! { #name };
                    }
                }
                let wc = clause_to_toks(clause, item_wc, &quote! { std::fmt::Debug });
                toks.append_all(quote_spanned! {span=>
                    impl #impl_generics std::fmt::Debug for #ident #ty_generics #wc {
                        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                            #inner
                        }
                    }
                });
            }
            TraitMany::Default(span) => {
                let mut inner;
                match item.fields {
                    Fields::Named(ref fields) => {
                        inner = quote! {};
                        for field in fields.named.iter() {
                            let field = field.ident.as_ref().unwrap();
                            inner.append_all(quote! { #field: Default::default(), });
                        }
                        inner = quote! { #ident { #inner } };
                    }
                    Fields::Unnamed(ref fields) => {
                        inner = quote! {};
                        for _ in 0..fields.unnamed.len() {
                            inner.append_all(quote! { Default::default(), });
                        }
                        inner = quote! { #ident(#inner) };
                    }
                    Fields::Unit => {
                        inner = quote! { #ident };
                    }
                }
                let wc = clause_to_toks(clause, item_wc, &quote! { std::default::Default });
                toks.append_all(quote_spanned! {span=>
                    impl #impl_generics std::default::Default for #ident #ty_generics #wc {
                        fn default() -> Self {
                            #inner
                        }
                    }
                });
            }
        }
    }
}

fn autoimpl_one(
    mut targets: Vec<TraitOne>,
    using: Member,
    item: ItemStruct,
    clause: &Option<WhereClause>,
    toks: &mut TokenStream,
) {
    fn for_field<T, F: Fn(&Field) -> T>(fields: &Fields, mem: &Member, f: F) -> Option<T> {
        match (fields, mem) {
            (Fields::Named(ref fields), Member::Named(ref ident)) => {
                for field in fields.named.iter() {
                    if field.ident.as_ref() == Some(ident) {
                        return Some(f(field));
                    }
                }
            }
            (Fields::Unnamed(ref fields), Member::Unnamed(index)) => {
                if let Some(field) = fields.unnamed.iter().nth(index.index as usize) {
                    return Some(f(field));
                }
            }
            _ => (),
        }
        None
    }

    let ident = &item.ident;
    let (impl_generics, ty_generics, item_wc) = item.generics.split_for_impl();

    for target in targets.drain(..) {
        match target {
            TraitOne::Deref(span) => {
                let wc = clause_to_toks(clause, item_wc, &quote! { std::ops::Deref });
                let ty = for_field(&item.fields, &using, |field| field.ty.clone()).unwrap();
                toks.append_all(quote_spanned! {span=>
                    impl #impl_generics std::ops::Deref for #ident #ty_generics #wc {
                        type Target = #ty;
                        fn deref(&self) -> &Self::Target {
                            &self.#using
                        }
                    }
                });
            }
            TraitOne::DerefMut(span) => {
                let wc = clause_to_toks(clause, item_wc, &quote! { std::ops::DerefMut });
                toks.append_all(quote_spanned! {span=>
                    impl #impl_generics std::ops::DerefMut for #ident #ty_generics #wc {
                        fn deref_mut(&mut self) -> &mut Self::Target {
                            &mut self.#using
                        }
                    }
                });
            }
        }
    }
}
