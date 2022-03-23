// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

use crate::scope::{Scope, ScopeItem};
use proc_macro2::{Span, TokenStream};
use proc_macro_error::emit_error;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Error, Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{token, Attribute, Expr, Generics, Ident, Token, Type, Visibility};

pub fn impl_default(attr: TokenStream, attr_span: Span, scope: &mut Scope) -> Result<()> {
    let attr: Attr = syn::parse2(attr)?;

    if let Some(expr) = attr.as_expr() {
        scope
            .generated
            .push(expr.gen(&scope.ident, &scope.generics));
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
        let (impl_generics, ty_generics, wc) = scope.generics.split_for_impl();

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

pub struct Attr {
    expr: Option<Expr>,
    pub span: Span,
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut expr = None;
        let span = input.span();

        if !input.is_empty() {
            expr = Some(input.parse()?);
        }

        Ok(Attr { expr, span })
    }
}

impl Attr {
    #[allow(clippy::wrong_self_convention)]
    pub fn as_expr(self) -> Option<AsExpr> {
        self.expr.map(AsExpr)
    }
}

pub struct AsExpr(Expr);
impl AsExpr {
    pub fn gen(self, ident: &Ident, generics: &Generics) -> TokenStream {
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

#[derive(Debug)]
pub enum Fields {
    Named(FieldsNamed),
    Unnamed(FieldsUnnamed),
    Unit,
}

#[derive(Debug)]
pub struct FieldsNamed {
    brace_token: token::Brace,
    fields: Punctuated<Field, Token![,]>,
}

#[derive(Debug)]
pub struct FieldsUnnamed {
    paren_token: token::Paren,
    fields: Punctuated<Field, Token![,]>,
}

#[derive(Debug)]
pub struct Field {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Option<Ident>,
    colon_token: Option<Token![:]>,
    ty: Type,
    assign: Option<(Token![=], Expr)>,
}

// Copied from syn, modified
pub(crate) mod parsing {
    use super::*;
    use syn::ext::IdentExt;
    use syn::{braced, parenthesized, WhereClause};

    impl Parse for FieldsNamed {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            let brace_token = braced!(content in input);
            Ok(FieldsNamed {
                brace_token,
                fields: content.parse_terminated(Field::parse_named)?,
            })
        }
    }

    impl Parse for FieldsUnnamed {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            let paren_token = parenthesized!(content in input);
            Ok(FieldsUnnamed {
                paren_token,
                fields: content.parse_terminated(Field::parse_unnamed)?,
            })
        }
    }

    impl Field {
        pub fn parse_named(input: ParseStream) -> Result<Self> {
            Ok(Field {
                attrs: input.call(Attribute::parse_outer)?,
                vis: input.parse()?,
                ident: Some(if input.peek(Token![_]) {
                    input.call(Ident::parse_any)
                } else {
                    input.parse()
                }?),
                colon_token: Some(input.parse()?),
                ty: input.parse()?,
                assign: if input.peek(Token![=]) {
                    Some((input.parse()?, input.parse()?))
                } else {
                    None
                },
            })
        }

        pub fn parse_unnamed(input: ParseStream) -> Result<Self> {
            Ok(Field {
                attrs: input.call(Attribute::parse_outer)?,
                vis: input.parse()?,
                ident: None,
                colon_token: None,
                ty: input.parse()?,
                assign: if input.peek(Token![=]) {
                    Some((input.parse()?, input.parse()?))
                } else {
                    None
                },
            })
        }
    }

    pub(crate) fn data_struct(
        input: ParseStream,
    ) -> Result<(Option<WhereClause>, Fields, Option<Token![;]>)> {
        let mut lookahead = input.lookahead1();
        let mut where_clause = None;
        if lookahead.peek(Token![where]) {
            where_clause = Some(input.parse()?);
            lookahead = input.lookahead1();
        }

        if where_clause.is_none() && lookahead.peek(token::Paren) {
            let fields = input.parse()?;

            lookahead = input.lookahead1();
            if lookahead.peek(Token![where]) {
                where_clause = Some(input.parse()?);
                lookahead = input.lookahead1();
            }

            if lookahead.peek(Token![;]) {
                let semi = input.parse()?;
                Ok((where_clause, Fields::Unnamed(fields), Some(semi)))
            } else {
                Err(lookahead.error())
            }
        } else if lookahead.peek(token::Brace) {
            let fields = parse_braced(input)?;
            Ok((where_clause, Fields::Named(fields), None))
        } else if lookahead.peek(Token![;]) {
            let semi = input.parse()?;
            Ok((where_clause, Fields::Unit, Some(semi)))
        } else {
            Err(lookahead.error())
        }
    }

    fn parse_braced(input: ParseStream) -> Result<FieldsNamed> {
        let content;
        let brace_token = braced!(content in input);
        let fields = content.parse_terminated(Field::parse_named)?;
        Ok(FieldsNamed {
            brace_token,
            fields,
        })
    }
}

mod printing {
    use super::*;
    use std::{iter, slice};
    use syn::AttrStyle;

    pub struct TokensOrDefault<'a, T: 'a>(pub &'a Option<T>);

    impl<'a, T> ToTokens for TokensOrDefault<'a, T>
    where
        T: ToTokens + Default,
    {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self.0 {
                Some(t) => t.to_tokens(tokens),
                None => T::default().to_tokens(tokens),
            }
        }
    }

    pub trait FilterAttrs<'a> {
        type Ret: Iterator<Item = &'a Attribute>;

        fn outer(self) -> Self::Ret;
        fn inner(self) -> Self::Ret;
    }

    impl<'a> FilterAttrs<'a> for &'a [Attribute] {
        type Ret = iter::Filter<slice::Iter<'a, Attribute>, fn(&&Attribute) -> bool>;

        fn outer(self) -> Self::Ret {
            fn is_outer(attr: &&Attribute) -> bool {
                match attr.style {
                    AttrStyle::Outer => true,
                    AttrStyle::Inner(_) => false,
                }
            }
            self.iter().filter(is_outer)
        }

        fn inner(self) -> Self::Ret {
            fn is_inner(attr: &&Attribute) -> bool {
                match attr.style {
                    AttrStyle::Inner(_) => true,
                    AttrStyle::Outer => false,
                }
            }
            self.iter().filter(is_inner)
        }
    }

    impl ToTokens for FieldsNamed {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.brace_token.surround(tokens, |tokens| {
                self.fields.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for FieldsUnnamed {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.paren_token.surround(tokens, |tokens| {
                self.fields.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for Field {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.vis.to_tokens(tokens);
            if let Some(ident) = &self.ident {
                ident.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.ty.to_tokens(tokens);

            if let Some(ref assign) = self.assign {
                emit_error!(
                    assign.0,
                    "default value on `struct` field in output";
                    help = "did you mean to use the `#[impl_default]` attribute?",
                );
            }
        }
    }
}
