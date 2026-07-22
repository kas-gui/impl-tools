// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! `#[split_impl]`

use crate::utils::{self, PathAsStr, copy_non_doc_attrs};
use proc_macro_error3::emit_error;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Generics, ImplItem, ItemTrait, Token, TraitItem, Type, parse_quote};

/// `#[split_impl]` attribute
pub struct SplitImpl {
    for_: Token![for],
    generics: Generics,
    target: Box<Type>,
}

mod parsing {
    use super::*;
    use syn::parse::{Parse, ParseStream, Result};

    impl Parse for SplitImpl {
        fn parse(input: ParseStream) -> Result<Self> {
            let for_ = input.parse::<Token![for]>()?;
            let mut generics = if input.peek(Token![<]) {
                input.parse()?
            } else {
                Generics::default()
            };
            let target = input.parse()?;
            if input.peek(Token![where]) {
                generics.where_clause = Some(input.parse()?);
            }

            Ok(SplitImpl {
                for_,
                generics,
                target,
            })
        }
    }
}

impl SplitImpl {
    /// Process input
    pub fn process(self, mut trait_: ItemTrait) -> TokenStream {
        let mut attrs = Vec::with_capacity(trait_.attrs.len());
        for attr in &trait_.attrs {
            if utils::propagate_attr_to_impl(attr) {
                attrs.push(attr.clone());
            }
        }

        let mut items = Vec::with_capacity(trait_.items.len());
        for item in trait_.items.iter_mut() {
            match item {
                TraitItem::Const(item) => {
                    let Some((eq_token, expr)) = item.default.take() else {
                        emit_error!(item, "definition not found");
                        continue;
                    };

                    items.push(ImplItem::Const(syn::ImplItemConst {
                        attrs: copy_non_doc_attrs(&item.attrs),
                        vis: syn::Visibility::Inherited,
                        modifiers: syn::ConstModifiers::default(),
                        const_token: item.const_token,
                        ident: item.ident.clone(),
                        generics: item.generics.clone(),
                        colon_token: item.colon_token,
                        ty: item.ty.clone(),
                        eq_token,
                        expr,
                        semi_token: item.semi_token,
                    }));
                }
                TraitItem::Fn(item) => {
                    let Some(block) = item.default.take() else {
                        emit_error!(item, "definition not found");
                        continue;
                    };

                    items.push(ImplItem::Fn(syn::ImplItemFn {
                        attrs: copy_non_doc_attrs(&item.attrs),
                        vis: syn::Visibility::Inherited,
                        modifiers: syn::FnModifiers::default(),
                        sig: item.sig.clone(),
                        block,
                    }));

                    item.attrs.retain(|attr| attr.path_as_string() != "inline");
                }
                TraitItem::Type(item) => {
                    let Some((eq_token, ty)) = item.default.take() else {
                        emit_error!(item, "definition not found");
                        continue;
                    };

                    items.push(ImplItem::Type(syn::ImplItemType {
                        attrs: copy_non_doc_attrs(&item.attrs),
                        vis: syn::Visibility::Inherited,
                        modifiers: syn::TypeModifiers::default(),
                        type_token: item.type_token,
                        ident: item.ident.clone(),
                        generics: item.generics.clone(),
                        eq_token,
                        ty,
                        semi_token: item.semi_token,
                    }));
                }
                other => emit_error!(other, "unsupported trait item"),
            }
        }

        let mut generics = self.generics;
        utils::extend_generics(&mut generics, &trait_.generics);

        let ident = &trait_.ident;
        let (_, ty_generics, _) = trait_.generics.split_for_impl();
        let path = parse_quote! { #ident #ty_generics };

        let impl_ = syn::ItemImpl {
            attrs,
            modifiers: syn::ImplModifiers::default(),
            unsafety: None,
            impl_token: Default::default(),
            generics,
            trait_: Some((path, self.for_)),
            self_ty: self.target,
            brace_token: Default::default(),
            items,
        };

        quote! { #trait_ #impl_ }
    }
}
