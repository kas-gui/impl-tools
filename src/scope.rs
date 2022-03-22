// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

use proc_macro2::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token::{Brace, Comma, Semi};
use syn::{
    Attribute, Fields, FieldsNamed, Generics, Ident, ItemImpl, Result, Token, Type, Variant,
    Visibility,
};

/// Content of items supported by [`Scope`] that are not common to all variants
#[derive(Debug)]
pub enum ScopeItem {
    Enum {
        token: Token![enum],
        brace: Brace,
        variants: Punctuated<Variant, Comma>,
    },
    Struct {
        token: Token![struct],
        fields: Fields,
    },
    Type {
        token: Token![type],
        eq_token: Token![=],
        ty: Box<Type>,
    },
    Union {
        token: Token![union],
        fields: FieldsNamed,
    },
}

#[derive(Debug)]
pub struct Scope {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub ident: Ident,
    pub generics: Generics,
    pub item: ScopeItem,
    pub semi: Option<Semi>,
    pub impls: Vec<ItemImpl>,
}

mod parsing {
    use super::*;
    use syn::parse::{Parse, ParseStream};
    use syn::{braced, token::Paren, Field, WhereClause};

    impl Parse for Scope {
        fn parse(input: ParseStream) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let vis = input.parse::<Visibility>()?;

            enum Token {
                Enum(Token![enum]),
                Struct(Token![struct]),
                Type(Token![type]),
                Union(Token![union]),
            }
            let lookahead = input.lookahead1();
            let token;
            if lookahead.peek(Token![enum]) {
                token = Token::Enum(input.parse()?);
            } else if lookahead.peek(Token![struct]) {
                token = Token::Struct(input.parse()?);
            } else if lookahead.peek(Token![type]) {
                token = Token::Type(input.parse()?);
            } else if lookahead.peek(Token![union]) {
                token = Token::Union(input.parse()?);
            } else {
                return Err(lookahead.error());
            }

            let ident = input.parse::<Ident>()?;
            let mut generics = input.parse::<Generics>()?;

            let item;
            let mut semi = None;
            match token {
                Token::Enum(token) => {
                    let (wc, brace, variants) = data_enum(input)?;
                    generics.where_clause = wc;
                    item = ScopeItem::Enum {
                        token,
                        brace,
                        variants,
                    };
                }
                Token::Struct(token) => {
                    let (wc, fields, semi_token) = data_struct(input)?;
                    generics.where_clause = wc;
                    semi = semi_token;
                    item = ScopeItem::Struct { token, fields };
                }
                Token::Type(token) => {
                    let eq_token = input.parse()?;
                    let ty = input.parse()?;
                    let semi_token = input.parse()?;
                    semi = Some(semi_token);
                    item = ScopeItem::Type {
                        token,
                        eq_token,
                        ty,
                    };
                }
                Token::Union(token) => {
                    let (wc, fields) = data_union(input)?;
                    generics.where_clause = wc;
                    item = ScopeItem::Union { token, fields };
                }
            }

            let mut impls = Vec::new();
            while !input.is_empty() {
                impls.push(input.parse()?);
            }

            Ok(Scope {
                attrs,
                vis,
                ident,
                generics,
                item,
                semi,
                impls,
            })
        }
    }

    pub fn data_struct(
        input: ParseStream,
    ) -> Result<(Option<WhereClause>, Fields, Option<Token![;]>)> {
        let mut lookahead = input.lookahead1();
        let mut where_clause = None;
        if lookahead.peek(Token![where]) {
            where_clause = Some(input.parse()?);
            lookahead = input.lookahead1();
        }

        if where_clause.is_none() && lookahead.peek(Paren) {
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
        } else if lookahead.peek(Brace) {
            let fields = parse_braced(input)?;
            Ok((where_clause, Fields::Named(fields), None))
        } else if lookahead.peek(Token![;]) {
            let semi = input.parse()?;
            Ok((where_clause, Fields::Unit, Some(semi)))
        } else {
            Err(lookahead.error())
        }
    }

    pub fn data_enum(
        input: ParseStream,
    ) -> Result<(Option<WhereClause>, Brace, Punctuated<Variant, Token![,]>)> {
        let where_clause = input.parse()?;

        let content;
        let brace = braced!(content in input);
        let variants = content.parse_terminated(Variant::parse)?;

        Ok((where_clause, brace, variants))
    }

    pub fn data_union(input: ParseStream) -> Result<(Option<WhereClause>, FieldsNamed)> {
        let where_clause = input.parse()?;
        let fields = parse_braced(input)?;
        Ok((where_clause, fields))
    }

    pub(crate) fn parse_braced(input: ParseStream) -> Result<FieldsNamed> {
        let content;
        let brace_token = braced!(content in input);
        let named = content.parse_terminated(Field::parse_named)?;
        Ok(FieldsNamed { brace_token, named })
    }
}

mod printing {
    use super::*;
    use quote::{ToTokens, TokenStreamExt};

    impl ToTokens for Scope {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.iter());
            self.vis.to_tokens(tokens);
            match &self.item {
                ScopeItem::Enum { token, .. } => token.to_tokens(tokens),
                ScopeItem::Struct { token, .. } => token.to_tokens(tokens),
                ScopeItem::Type { token, .. } => token.to_tokens(tokens),
                ScopeItem::Union { token, .. } => token.to_tokens(tokens),
            }
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            match &self.item {
                ScopeItem::Enum {
                    brace, variants, ..
                } => {
                    self.generics.where_clause.to_tokens(tokens);
                    brace.surround(tokens, |tokens| {
                        variants.to_tokens(tokens);
                    });
                }
                ScopeItem::Struct { fields, .. } => match fields {
                    Fields::Named(fields) => {
                        self.generics.where_clause.to_tokens(tokens);
                        fields.to_tokens(tokens);
                    }
                    Fields::Unnamed(fields) => {
                        fields.to_tokens(tokens);
                        self.generics.where_clause.to_tokens(tokens);
                    }
                    Fields::Unit => {
                        self.generics.where_clause.to_tokens(tokens);
                    }
                },
                ScopeItem::Type { eq_token, ty, .. } => {
                    self.generics.where_clause.to_tokens(tokens);
                    eq_token.to_tokens(tokens);
                    ty.to_tokens(tokens);
                }
                ScopeItem::Union { fields, .. } => {
                    self.generics.where_clause.to_tokens(tokens);
                    fields.to_tokens(tokens);
                }
            }
            if let Some(semi) = self.semi.as_ref() {
                semi.to_tokens(tokens);
            }

            tokens.append_all(self.impls.iter());
        }
    }
}

pub fn scope(scope: Scope) -> Result<TokenStream> {
    Ok(quote! { #scope })
}
