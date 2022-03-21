// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{token, Attribute, Expr, Generics, Ident, Token, Type, Visibility};

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

pub struct Struct {
    attrs: Vec<Attribute>,
    vis: Visibility,
    struct_token: Token![struct],
    ident: Ident,
    generics: Generics,
    fields: Fields,
    semi_token: Option<Token![;]>,
}

impl Struct {
    pub fn gen(self) -> TokenStream {
        let mut toks = TokenStream::new();
        self.to_tokens(&mut toks);

        let ident = &self.ident;
        let (impl_generics, ty_generics, wc) = self.generics.split_for_impl();

        let fields = match self.fields {
            Fields::Named(FieldsNamed { fields, .. })
            | Fields::Unnamed(FieldsUnnamed { fields, .. }) => {
                let iter = fields
                    .iter()
                    .map(|field| field.assign.as_ref().map(|a| &a.1));
                quote! { #(#iter),* }
            }
            Fields::Unit => quote! {},
        };

        toks.append_all(quote! {
            impl #impl_generics std::default::Default for #ident #ty_generics #wc {
                fn default() -> Self {
                    #ident {
                        #fields
                    }
                }
            }
        });

        toks
    }
}

enum Fields {
    Named(FieldsNamed),
    Unnamed(FieldsUnnamed),
    Unit,
}

pub struct FieldsNamed {
    brace_token: token::Brace,
    fields: Punctuated<Field, Token![,]>,
}

struct FieldsUnnamed {
    paren_token: token::Paren,
    fields: Punctuated<Field, Token![,]>,
}

struct Field {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Option<Ident>,
    colon_token: Option<Token![:]>,
    ty: Type,
    assign: Option<(Token![=], Expr)>,
}

// Copied from syn, modified
mod parsing {
    use super::*;
    use syn::ext::IdentExt;
    use syn::{braced, bracketed, parenthesized, AttrStyle, Path, WhereClause};

    impl Parse for Struct {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut attrs = input.call(Attribute::parse_outer)?;
            let vis = input.parse::<Visibility>()?;
            let struct_token = input.parse::<Token![struct]>()?;
            let ident = input.parse::<Ident>()?;
            let generics = input.parse::<Generics>()?;
            let (where_clause, fields, semi_token) = parsing::data_struct(input, &mut attrs)?;
            Ok(Struct {
                attrs,
                vis,
                struct_token,
                ident,
                generics: Generics {
                    where_clause,
                    ..generics
                },
                fields,
                semi_token,
            })
        }
    }

    impl Parse for FieldsNamed {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(FieldsNamed {
                brace_token: braced!(content in input),
                fields: content.parse_terminated(Field::parse_named)?,
            })
        }
    }

    impl Parse for FieldsUnnamed {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(FieldsUnnamed {
                paren_token: parenthesized!(content in input),
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

    pub(super) fn data_struct(
        input: ParseStream,
        attrs: &mut Vec<Attribute>,
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
            let fields = parse_braced(input, attrs)?;
            Ok((where_clause, Fields::Named(fields), None))
        } else if lookahead.peek(Token![;]) {
            let semi = input.parse()?;
            Ok((where_clause, Fields::Unit, Some(semi)))
        } else {
            Err(lookahead.error())
        }
    }

    fn parse_braced(input: ParseStream, attrs: &mut Vec<Attribute>) -> Result<FieldsNamed> {
        let content;
        let brace_token = braced!(content in input);
        parse_inner(&content, attrs)?;
        let fields = content.parse_terminated(Field::parse_named)?;
        Ok(FieldsNamed {
            brace_token,
            fields,
        })
    }

    fn parse_inner(input: ParseStream, attrs: &mut Vec<Attribute>) -> Result<()> {
        while input.peek(Token![#]) && input.peek2(Token![!]) {
            attrs.push(input.call(single_parse_inner)?);
        }
        Ok(())
    }

    fn single_parse_inner(input: ParseStream) -> Result<Attribute> {
        let content;
        Ok(Attribute {
            pound_token: input.parse()?,
            style: AttrStyle::Inner(input.parse()?),
            bracket_token: bracketed!(content in input),
            path: content.call(Path::parse_mod_style)?,
            tokens: content.parse()?,
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

    impl ToTokens for Struct {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.struct_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            match &self.fields {
                Fields::Named(fields) => {
                    self.generics.where_clause.to_tokens(tokens);
                    fields.to_tokens(tokens);
                }
                Fields::Unnamed(fields) => {
                    fields.to_tokens(tokens);
                    self.generics.where_clause.to_tokens(tokens);
                    TokensOrDefault(&self.semi_token).to_tokens(tokens);
                }
                Fields::Unit => {
                    self.generics.where_clause.to_tokens(tokens);
                    TokensOrDefault(&self.semi_token).to_tokens(tokens);
                }
            }
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

            // Note: we deliberately ignore self.assign here
        }
    }
}
