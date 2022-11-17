// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Impls "using" a field

use super::{Error, ImplArgs, ImplTrait, Result};
use crate::SimplePath;
use proc_macro2::TokenStream as Toks;
use quote::quote;
use syn::{ItemStruct, PathArguments};

/// Implement [`core::borrow::Borrow`]
pub struct ImplBorrow;
impl ImplTrait for ImplBorrow {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "borrow", "Borrow"])
    }

    fn support_using(&self) -> bool {
        true
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        if let Some(field) = args.using_field(&item.fields) {
            let ty = field.ty.clone();
            let member = args.using_member().unwrap();
            let method = quote! {
                fn borrow(&self) -> & #ty {
                    &self.#member
                }
            };
            Ok((quote! { ::core::borrow::Borrow<#ty> }, method))
        } else {
            Err(Error::RequireUsing)
        }
    }
}

/// Implement [`core::borrow::BorrowMut`]
pub struct ImplBorrowMut;
impl ImplTrait for ImplBorrowMut {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "borrow", "BorrowMut"])
    }

    fn support_using(&self) -> bool {
        true
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        if let Some(field) = args.using_field(&item.fields) {
            let ty = field.ty.clone();
            let member = args.using_member().unwrap();
            let method = quote! {
                fn borrow_mut(&mut self) -> &mut #ty {
                    &mut self.#member
                }
            };
            Ok((quote! { ::core::borrow::BorrowMut<#ty> }, method))
        } else {
            Err(Error::RequireUsing)
        }
    }
}

/// Implement [`core::convert::AsRef`]
pub struct ImplAsRef;
impl ImplTrait for ImplAsRef {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "convert", "AsRef"])
    }

    fn support_using(&self) -> bool {
        true
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        if let Some(field) = args.using_field(&item.fields) {
            let ty = field.ty.clone();
            let member = args.using_member().unwrap();
            let method = quote! {
                fn as_ref(&self) -> & #ty {
                    &self.#member
                }
            };
            Ok((quote! { ::core::convert::AsRef<#ty> }, method))
        } else {
            Err(Error::RequireUsing)
        }
    }
}

/// Implement [`core::convert::AsMut`]
pub struct ImplAsMut;
impl ImplTrait for ImplAsMut {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "convert", "AsMut"])
    }

    fn support_using(&self) -> bool {
        true
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        if let Some(field) = args.using_field(&item.fields) {
            let ty = field.ty.clone();
            let member = args.using_member().unwrap();
            let method = quote! {
                fn as_mut(&mut self) -> &mut #ty {
                    &mut self.#member
                }
            };
            Ok((quote! { ::core::convert::AsMut<#ty> }, method))
        } else {
            Err(Error::RequireUsing)
        }
    }
}

/// Implement [`core::ops::Deref`]
pub struct ImplDeref;
impl ImplTrait for ImplDeref {
    fn path(&self) -> SimplePath {
        SimplePath::new(&["", "core", "ops", "Deref"])
    }

    fn support_path_args(&self) -> bool {
        true
    }

    fn support_using(&self) -> bool {
        true
    }

    fn struct_items(&self, item: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        if let Some(field) = args.using_field(&item.fields) {
            let target = match args.path_args {
                PathArguments::None => field.ty.clone(),
                PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) => {
                    let mut result = None;
                    for arg in args {
                        if let syn::GenericArgument::Binding(b) = arg {
                            if b.ident == "Target" && result.is_none() {
                                result = Some(b.ty.clone());
                                continue;
                            }
                        }
                        return Err(Error::PathArgs("expected `<Target = ..>`"));
                    }
                    match result {
                        Some(r) => r,
                        None => return Err(Error::PathArgs("expected `<Target = ..>`")),
                    }
                }
                PathArguments::Parenthesized(_) => return Err(Error::PathArgs("unexpected")),
            };

            let member = args.using_member().unwrap();
            let method = quote! {
                type Target = #target;
                fn deref(&self) -> &Self::Target {
                    &self.#member
                }
            };
            Ok((quote! { ::core::ops::Deref }, method))
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

    fn support_using(&self) -> bool {
        true
    }

    fn struct_items(&self, _: &ItemStruct, args: &ImplArgs) -> Result<(Toks, Toks)> {
        if let Some(member) = args.using_member() {
            let method = quote! {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.#member
                }
            };
            Ok((quote! { ::core::ops::DerefMut }, method))
        } else {
            Err(Error::RequireUsing)
        }
    }
}
