// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Custom version of [`syn`] fields types supporting initializers

use proc_macro2::TokenStream;
use proc_macro_error::emit_error;
use quote::{ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{token, Attribute, Expr, Ident, Token, Type, Visibility};

/// Struct style: unit/tuple/regular
#[derive(Debug)]
pub enum StructStyle {
    /// A unit struct (e.g. `struct Foo;`)
    Unit(Token![;]),
    /// A tuple struct (e.g. `struct Foo(f32, f32);`)
    Tuple(token::Paren, Token![;]),
    /// A regular struct (e.g. `struct Foo { x: f32, y: f32 }`)
    Regular(token::Brace),
}

/// Data stored within an enum variant or struct.
///
/// This is a variant of [`syn::Fields`] supporting field initializers.
#[derive(Debug)]
pub enum Fields {
    /// Named fields of a struct or struct variant such as `Point { x: f64, y: f64 }`.
    Named(FieldsNamed),
    /// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
    Unnamed(FieldsUnnamed),
    /// Unit struct or unit variant such as `None`.
    Unit,
}

/// Named fields of a struct or struct variant such as `Point { x: f64, y: f64 }`.
///
/// This is a variant of [`syn::FieldsNamed`] supporting field initializers.
#[derive(Debug)]
pub struct FieldsNamed {
    /// `{ ... }` around fields
    pub brace_token: token::Brace,
    /// Fields
    pub fields: Punctuated<Field, Token![,]>,
}

/// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
///
/// This is a variant of [`syn::FieldsUnnamed`] supporting field initializers.
#[derive(Debug)]
pub struct FieldsUnnamed {
    /// `( ... )` around fields
    pub paren_token: token::Paren,
    /// Fields
    pub fields: Punctuated<Field, Token![,]>,
}

/// A field of a struct or enum variant.
///
/// This is a variant of [`syn::Field`] supporting field initializers.
#[derive(Debug)]
pub struct Field {
    /// Attributes tagged on the field.
    pub attrs: Vec<Attribute>,
    /// Visibility of the field.
    pub vis: Visibility,
    /// Name of the field, if any.
    pub ident: Option<Ident>,
    /// `:` token before type
    pub colon_token: Option<Token![:]>,
    /// Type of the field.
    pub ty: Type,
    /// Optional field initializer.
    ///
    /// This is considered legal input when parsing, but not legal output. An
    /// attribute rule such as [`AttrImplDefault`](crate::scope::AttrImplDefault)
    /// must remove the initializer before output is generated.
    pub assign: Option<(Token![=], Expr)>,
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
                fields: content.parse_terminated(Field::parse_named, Token![,])?,
            })
        }
    }

    impl Parse for FieldsUnnamed {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            let paren_token = parenthesized!(content in input);
            Ok(FieldsUnnamed {
                paren_token,
                fields: content.parse_terminated(Field::parse_unnamed, Token![,])?,
            })
        }
    }

    impl Field {
        /// Parse a named field
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

        /// Parse an unnamed field
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
        let fields = content.parse_terminated(Field::parse_named, Token![,])?;
        Ok(FieldsNamed {
            brace_token,
            fields,
        })
    }
}

mod printing {
    use super::*;

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
                self.colon_token.unwrap_or_default().to_tokens(tokens);
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
