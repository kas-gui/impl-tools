// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{Expr, Generics, Ident};

pub struct Attr {
    expr: Option<Expr>,
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut expr = None;

        if !input.is_empty() {
            expr = Some(input.parse()?);
        }

        Ok(Attr { expr })
    }
}

impl Attr {
    pub fn as_expr(self) -> Option<AsExpr> {
        self.expr.map(AsExpr)
    }
}

pub struct AsExpr(Expr);
impl AsExpr {
    pub fn gen(self, ident: Ident, generics: Generics) -> TokenStream {
        let (impl_generics, ty_generics, wc) = generics.split_for_impl();
        let expr = self.0;

        quote! {
            impl #impl_generics std::default::Default for #ident #ty_generics #wc {
                fn default() -> Self {
                    #expr
                }
            }
        }
    }
}
