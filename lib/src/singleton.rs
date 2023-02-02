// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

use crate::fields::{Field, Fields, FieldsNamed, FieldsUnnamed, StructStyle};
use crate::{IdentFormatter, Scope, ScopeItem};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::token::{Brace, Colon, Comma, Eq, Paren, Semi};
use syn::{parse_quote, punctuated::Punctuated, spanned::Spanned};
use syn::{Attribute, GenericParam, Generics, Ident, ItemImpl, Member, Token, Type, TypePath};

/// A field of a [`Singleton`]
#[derive(Debug)]
pub struct SingletonField {
    /// Field attributes
    pub attrs: Vec<Attribute>,
    /// Field visibility
    pub vis: syn::Visibility,
    /// Field identifier (regular structs only, and optional even there)
    pub ident: Option<Ident>,
    /// Separator between identifier and type
    ///
    /// This is present only given an explicit identifier *and* an explicit type.
    pub colon_token: Option<Colon>,
    /// Field type
    ///
    /// In case the type is omitted, this has value [`Type::Infer`].
    pub ty: Type,
    /// Optional non-default value assignment
    pub assignment: Option<(Eq, syn::Expr)>,
}

/// A struct with a single instantiation
///
/// The singleton macro may be used to conveniently declare a struct's type,
/// implementations and construct an instance.
/// This struct represents the macro's input.
#[derive(Debug)]
pub struct Singleton {
    /// Struct attributes
    pub attrs: Vec<Attribute>,
    /// `struct` token
    pub token: Token![struct],
    /// (Explicit) struct generics
    ///
    /// Note: the macro instantiated type may have extra generics.
    pub generics: Generics,
    /// Struct style: unit/tuple/regular
    pub style: StructStyle,
    /// Struct fields
    pub fields: Punctuated<SingletonField, Comma>,
    /// (Explicit) struct implementations
    pub impls: Vec<ItemImpl>,
}

impl Singleton {
    /// Convert to a [`SingletonScope`]
    pub fn into_scope(mut self) -> SingletonScope {
        let mut idfmt = IdentFormatter::new();

        let mut fields = Punctuated::<Field, Comma>::new();
        let mut field_val_toks = quote! {};

        for (index, pair) in self.fields.into_pairs().enumerate() {
            let (field, opt_comma) = pair.into_tuple();

            let mut ident = field.ident.clone();
            let ty = &field.ty;
            let field_span = match field.assignment {
                None => quote! { #ty }.span(),
                Some((ref eq, ref expr)) => quote! { #ty #eq #expr }.span(),
            };
            let mem = match self.style {
                StructStyle::Regular(_) => {
                    let id = ident
                        .unwrap_or_else(|| idfmt.make(format_args!("_field{}", index), field_span));
                    ident = Some(id.clone());
                    Member::Named(id)
                }
                StructStyle::Tuple(_, _) => Member::Unnamed(syn::Index {
                    index: index as u32,
                    span: field_span,
                }),
                _ => unreachable!(),
            };
            let ty_name = match ident {
                None => format!("_Field{}", index),
                Some(ref id) => {
                    let ident = id.to_string();
                    let mut buf = "_Field".to_string();
                    buf.reserve(ident.len());
                    let mut next_upper = true;
                    for c in ident.chars() {
                        if c == '_' {
                            next_upper = true;
                            continue;
                        }

                        if next_upper {
                            buf.extend(c.to_uppercase());
                            next_upper = false;
                        } else {
                            buf.push(c);
                        }
                    }
                    buf
                }
            };

            let ty: Type = match field.ty {
                Type::ImplTrait(syn::TypeImplTrait { impl_token, bounds }) => {
                    let span = quote! { #impl_token #bounds }.span();
                    let ty = Ident::new(&ty_name, span);

                    self.generics.params.push(parse_quote! { #ty: #bounds });

                    Type::Path(TypePath {
                        qself: None,
                        path: ty.into(),
                    })
                }
                Type::Infer(infer_token) => {
                    let ty = Ident::new(&ty_name, infer_token.span());
                    self.generics.params.push(parse_quote! { #ty });

                    Type::Path(TypePath {
                        qself: None,
                        path: ty.into(),
                    })
                }
                mut ty => {
                    struct ReplaceInfers<'a> {
                        index: usize,
                        params: Vec<GenericParam>,
                        idfmt: &'a mut IdentFormatter,
                        ty_name: &'a str,
                    }
                    let mut replacer = ReplaceInfers {
                        index: 0,
                        params: vec![],
                        idfmt: &mut idfmt,
                        ty_name: &ty_name,
                    };

                    impl<'a> syn::visit_mut::VisitMut for ReplaceInfers<'a> {
                        fn visit_type_mut(&mut self, node: &mut Type) {
                            let (span, bounds) = match node {
                                Type::ImplTrait(syn::TypeImplTrait { impl_token, bounds }) => {
                                    (impl_token.span, std::mem::take(bounds))
                                }
                                Type::Infer(infer) => (infer.span(), Punctuated::new()),
                                _ => return,
                            };

                            let ident = self
                                .idfmt
                                .make(format_args!("{}{}", self.ty_name, self.index), span);
                            self.index += 1;

                            self.params.push(GenericParam::Type(syn::TypeParam {
                                attrs: vec![],
                                ident: ident.clone(),
                                colon_token: Some(Default::default()),
                                bounds,
                                eq_token: None,
                                default: None,
                            }));

                            *node = Type::Path(TypePath {
                                qself: None,
                                path: ident.into(),
                            });
                        }
                    }
                    syn::visit_mut::visit_type_mut(&mut replacer, &mut ty);

                    self.generics.params.extend(replacer.params);
                    ty
                }
            };

            if let Some((_, ref value)) = field.assignment {
                field_val_toks.append_all(quote! { #mem: #value, });
            } else {
                field_val_toks.append_all(quote! { #mem: Default::default(), });
            }

            fields.push_value(Field {
                attrs: field.attrs,
                vis: field.vis,
                ident,
                colon_token: field.colon_token.or_else(|| Some(Default::default())),
                ty,
                assign: None,
            });
            if let Some(comma) = opt_comma {
                fields.push_punct(comma);
            }
        }

        let (fields, semi) = match self.style {
            StructStyle::Unit(semi) => (Fields::Unit, Some(semi)),
            StructStyle::Regular(brace_token) => (
                Fields::Named(FieldsNamed {
                    brace_token,
                    fields,
                }),
                None,
            ),
            StructStyle::Tuple(paren_token, semi) => (
                Fields::Unnamed(FieldsUnnamed {
                    paren_token,
                    fields,
                }),
                Some(semi),
            ),
        };

        let scope = Scope {
            attrs: self.attrs,
            vis: syn::Visibility::Inherited,
            ident: parse_quote! { _Singleton },
            generics: self.generics,
            item: ScopeItem::Struct {
                token: self.token,
                fields,
            },
            semi,
            impls: self.impls,
            generated: vec![],
        };

        SingletonScope(scope, field_val_toks)
    }
}

/// A [`Scope`] plus field values
///
/// This supports dereference to a [`Scope`], allowing macro expansion via
/// [`Scope::apply_attrs`] and other manipulation.
///
/// Tokens may be generated by [`Self::expand`].
#[derive(Debug)]
pub struct SingletonScope(Scope, TokenStream);

impl std::ops::Deref for SingletonScope {
    type Target = Scope;
    fn deref(&self) -> &Scope {
        &self.0
    }
}
impl std::ops::DerefMut for SingletonScope {
    fn deref_mut(&mut self) -> &mut Scope {
        &mut self.0
    }
}

impl SingletonScope {
    /// Generate the [`TokenStream`]
    ///
    /// This is a convenience function. It is valid to, instead, (1) call
    /// [`Scope::expand_impl_self`] on self, then (2) use the [`ToTokens`]
    /// impl on `Scope`.
    pub fn expand(mut self) -> TokenStream {
        self.expand_impl_self();
        self.into_token_stream()
    }
}

impl ToTokens for SingletonScope {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let scope = &self.0;
        let field_val_toks = &self.1;

        tokens.append_all(quote! {
            {
                #scope

                _Singleton {
                    #field_val_toks
                }
            }
        });
    }
}

mod parsing {
    use super::*;
    use proc_macro_error::abort;
    use syn::parse::{Error, Parse, ParseStream, Result};
    use syn::{braced, bracketed, parenthesized};

    fn parse_attrs_inner(input: ParseStream, attrs: &mut Vec<Attribute>) -> Result<()> {
        while input.peek(Token![#]) && input.peek2(Token![!]) {
            let pound_token = input.parse()?;
            let style = syn::AttrStyle::Inner(input.parse()?);
            let content;
            let bracket_token = bracketed!(content in input);
            let path = content.call(syn::Path::parse_mod_style)?;
            let tokens = content.parse()?;
            attrs.push(Attribute {
                pound_token,
                style,
                bracket_token,
                path,
                tokens,
            });
        }
        Ok(())
    }

    fn parse_impl(in_ident: Option<&Ident>, input: ParseStream) -> Result<ItemImpl> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let defaultness: Option<Token![default]> = input.parse()?;
        let unsafety: Option<Token![unsafe]> = input.parse()?;
        let impl_token: Token![impl] = input.parse()?;

        let has_generics = input.peek(Token![<])
            && (input.peek2(Token![>])
                || input.peek2(Token![#])
                || (input.peek2(Ident) || input.peek2(syn::Lifetime))
                    && (input.peek3(Token![:])
                        || input.peek3(Token![,])
                        || input.peek3(Token![>])
                        || input.peek3(Token![=]))
                || input.peek2(Token![const]));
        let mut generics: Generics = if has_generics {
            input.parse()?
        } else {
            Generics::default()
        };

        let mut first_ty: Type = input.parse()?;
        let self_ty: Type;
        let trait_;

        let is_impl_for = input.peek(Token![for]);
        if is_impl_for {
            let for_token: Token![for] = input.parse()?;
            let mut first_ty_ref = &first_ty;
            while let Type::Group(ty) = first_ty_ref {
                first_ty_ref = &ty.elem;
            }
            if let Type::Path(_) = first_ty_ref {
                while let Type::Group(ty) = first_ty {
                    first_ty = *ty.elem;
                }
                if let Type::Path(TypePath { qself: None, path }) = first_ty {
                    trait_ = Some((None, path, for_token));
                } else {
                    unreachable!();
                }
            } else {
                return Err(Error::new(for_token.span(), "for without target trait"));
            }
            self_ty = input.parse()?;
        } else {
            trait_ = None;
            self_ty = first_ty;
        }

        generics.where_clause = input.parse()?;

        if self_ty != parse_quote! { Self } {
            if let Some(ident) = in_ident {
                if !matches!(self_ty, Type::Path(TypePath {
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        ref segments,
                    }
                }) if segments.len() == 1 && segments.first().unwrap().ident == *ident)
                {
                    abort!(
                        self_ty.span(),
                        format!(
                            "expected `Self` or `{0}` or `{0}<...>` or `Trait for Self`, etc",
                            ident
                        )
                    );
                }
            } else {
                abort!(self_ty.span(), "expected `Self` or `Trait for Self`");
            }
        }

        let content;
        let brace_token = braced!(content in input);
        parse_attrs_inner(&content, &mut attrs)?;

        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }

        Ok(ItemImpl {
            attrs,
            defaultness,
            unsafety,
            impl_token,
            generics,
            trait_,
            self_ty: Box::new(self_ty),
            brace_token,
            items,
        })
    }

    impl SingletonField {
        fn check_is_fixed(ty: &Type, input_span: Span) -> Result<()> {
            let is_fixed = match ty {
                Type::ImplTrait(_) | Type::Infer(_) => false,
                ty => {
                    struct IsFixed(bool);
                    let mut checker = IsFixed(true);

                    impl<'ast> syn::visit::Visit<'ast> for IsFixed {
                        fn visit_type(&mut self, node: &'ast Type) {
                            if matches!(node, Type::ImplTrait(_) | Type::Infer(_)) {
                                self.0 = false;
                            }
                        }
                    }
                    syn::visit::visit_type(&mut checker, ty);

                    checker.0
                }
            };

            if is_fixed {
                Ok(())
            } else {
                Err(Error::new(
                    input_span,
                    "require either a fixed type or a value assignment",
                ))
            }
        }

        fn parse_named(input: ParseStream) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let vis = input.parse()?;

            let ident = if input.peek(Token![_]) {
                let _: Token![_] = input.parse()?;
                None
            } else {
                Some(input.parse::<Ident>()?)
            };

            let mut colon_token = None;

            // Note: Colon matches `::` but that results in confusing error messages
            let ty = if input.peek(Colon) && !input.peek2(Colon) {
                colon_token = Some(input.parse()?);
                input.parse()?
            } else {
                parse_quote! { _ }
            };

            let mut assignment = None;
            if let Ok(eq) = input.parse::<Eq>() {
                assignment = Some((eq, input.parse()?));
            } else {
                Self::check_is_fixed(&ty, input.span())?;
            }

            Ok(SingletonField {
                attrs,
                vis,
                ident,
                colon_token,
                ty,
                assignment,
            })
        }

        fn parse_unnamed(input: ParseStream) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let vis = input.parse()?;

            let ty = input.parse()?;

            let mut assignment = None;
            if let Ok(eq) = input.parse::<Eq>() {
                assignment = Some((eq, input.parse()?));
            } else {
                Self::check_is_fixed(&ty, input.span())?;
            }

            Ok(SingletonField {
                attrs,
                vis,
                ident: None,
                colon_token: None,
                ty,
                assignment,
            })
        }
    }

    impl Parse for Singleton {
        fn parse(input: ParseStream) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let token = input.parse::<Token![struct]>()?;

            let mut generics = input.parse::<Generics>()?;

            let mut lookahead = input.lookahead1();
            if lookahead.peek(Token![where]) {
                generics.where_clause = Some(input.parse()?);
                lookahead = input.lookahead1();
            }

            let style;
            let fields;
            if generics.where_clause.is_none() && lookahead.peek(Paren) {
                let content;
                let paren_token = parenthesized!(content in input);
                fields = content.parse_terminated(SingletonField::parse_unnamed)?;

                lookahead = input.lookahead1();
                if lookahead.peek(Token![where]) {
                    generics.where_clause = Some(input.parse()?);
                    lookahead = input.lookahead1();
                }

                if lookahead.peek(Semi) {
                    style = StructStyle::Tuple(paren_token, input.parse()?);
                } else {
                    return Err(lookahead.error());
                }
            } else if lookahead.peek(Brace) {
                let content;
                let brace_token = braced!(content in input);
                style = StructStyle::Regular(brace_token);
                fields = content.parse_terminated(SingletonField::parse_named)?;
            } else if lookahead.peek(Semi) {
                style = StructStyle::Unit(input.parse()?);
                fields = Punctuated::new();
            } else {
                return Err(lookahead.error());
            }

            let mut impls = Vec::new();
            while !input.is_empty() {
                impls.push(parse_impl(None, input)?);
            }

            Ok(Singleton {
                attrs,
                token,
                generics,
                style,
                fields,
                impls,
            })
        }
    }
}
