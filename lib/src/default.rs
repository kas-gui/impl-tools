// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

use crate::fields::{Fields, FieldsNamed, FieldsUnnamed};
use crate::generics::{clause_to_toks, WhereClause};
use crate::{Scope, ScopeAttr, ScopeItem};
use proc_macro2::{Span, TokenStream};
use proc_macro_error::emit_error;
use quote::quote;
use syn::parse::{Error, Parse, ParseStream, Result};
use syn::{parse2, Expr, Generics, Ident, Item, Token};

/// `#[impl_default]` attribute
pub struct ImplDefault {
    expr: Option<Expr>,
    where_clause: Option<WhereClause>,
    span: Span,
}

impl ImplDefault {
    /// Expand over the given `item`
    ///
    /// This attribute (in this form of invocation) does not modify the item.
    /// The caller should append the result to `item` tokens.
    pub fn expand(self, item: TokenStream) -> TokenStream {
        let attr_span = self.span;
        if self.expr.is_some() {
            let item = match parse2::<Item>(item) {
                Ok(item) => item,
                Err(err) => {
                    emit_error!(err.span(), "{}", err);
                    return TokenStream::new();
                }
            };

            match item {
                Item::Enum(syn::ItemEnum {
                    ident, generics, ..
                })
                | Item::Struct(syn::ItemStruct {
                    ident, generics, ..
                })
                | Item::Type(syn::ItemType {
                    ident, generics, ..
                })
                | Item::Union(syn::ItemUnion {
                    ident, generics, ..
                }) => self.gen_expr(&ident, &generics),
                item => {
                    emit_error!(
                        item,
                        "default: only supports enum, struct, type alias and union items"
                    );
                    TokenStream::new()
                }
            }
        } else {
            emit_error!(attr_span, "invalid use outside of `impl_scope!` macro");
            TokenStream::new()
        }
    }

    fn gen_expr(self, ident: &Ident, generics: &Generics) -> TokenStream {
        let (impl_generics, ty_generics, _) = generics.split_for_impl();
        let wc = clause_to_toks(
            &self.where_clause,
            generics.where_clause.as_ref(),
            &quote! { Default },
        );
        let expr = self.expr.unwrap();

        quote! {
            impl #impl_generics std::default::Default for #ident #ty_generics #wc {
                fn default() -> Self {
                    #expr
                }
            }
        }
    }
}

/// [`ScopeAttr`] rule enabling `#[impl_default]` within `impl_scope!`
pub const ATTR_IMPL_DEFAULT: ScopeAttr = (&["impl_default"], impl_default);
fn impl_default(attr: TokenStream, attr_span: Span, scope: &mut Scope) -> Result<()> {
    let attr: ImplDefault = syn::parse2(attr)?;

    if attr.expr.is_some() {
        scope
            .generated
            .push(attr.gen_expr(&scope.ident, &scope.generics));
    } else {
        let fields = match &mut scope.item {
            ScopeItem::Struct { fields, .. } => match fields {
                Fields::Named(FieldsNamed { fields, .. })
                | Fields::Unnamed(FieldsUnnamed { fields, .. }) => {
                    let iter = fields.iter_mut().map(|field| {
                        let ident = &field.ident;
                        if let Some(expr) = field.assign.take().map(|a| a.1) {
                            quote! { #ident : #expr }
                        } else {
                            quote! { #ident : Default::default() }
                        }
                    });
                    quote! { #(#iter),* }
                }
                Fields::Unit => quote! {},
            },
            _ => {
                return Err(Error::new(
                    attr_span,
                    "must specify value as `#[impl_default(value)]` on non-struct type",
                ));
            }
        };

        let ident = &scope.ident;
        let (impl_generics, ty_generics, _) = scope.generics.split_for_impl();
        let wc = clause_to_toks(
            &attr.where_clause,
            scope.generics.where_clause.as_ref(),
            &quote! { Default },
        );

        scope.generated.push(quote! {
            impl #impl_generics std::default::Default for #ident #ty_generics #wc {
                fn default() -> Self {
                    #ident {
                        #fields
                    }
                }
            }
        });
    }
    Ok(())
}

impl Parse for ImplDefault {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut expr = None;
        let mut where_clause = None;
        let span = input.span();

        if !input.peek(Token![where]) && !input.is_empty() {
            expr = Some(input.parse()?);
        }

        if input.peek(Token![where]) {
            where_clause = Some(input.parse()?);
        }

        if !input.is_empty() {
            return Err(Error::new(input.span(), "unexpected"));
        }

        Ok(ImplDefault {
            expr,
            where_clause,
            span,
        })
    }
}
