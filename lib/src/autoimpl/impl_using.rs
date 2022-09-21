// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Impls "using" a field

use super::{Error, ImplArgs, ImplTrait, Result};
use crate::SimplePath;
use proc_macro2::TokenStream;
use quote::quote;
use syn::ItemStruct;

/// Implement [`core::ops::Deref`]
pub struct ImplDeref;
impl ImplTrait for ImplDeref {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "ops", "Deref"])
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

/// Implement [`core::ops::DerefMut`]
pub struct ImplDerefMut;
impl ImplTrait for ImplDerefMut {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "ops", "DerefMut"])
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
