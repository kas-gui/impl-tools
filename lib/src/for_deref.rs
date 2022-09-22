// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Implementation of the `#[autoimpl]` attribute

use crate::generics::{impl_generics, GenericParam, Generics, TypeParamBound, WherePredicate};
use proc_macro2::{Span, TokenStream};
use proc_macro_error::{emit_call_site_error, emit_error};
use quote::{quote, ToTokens, TokenStreamExt};
use std::{iter, slice};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Colon2, Comma, Eq};
use syn::{Attribute, FnArg, Ident, Item, Token, TraitItem, Type, TypePath};

/// Autoimpl for types supporting `Deref`
pub struct ForDeref {
    generics: Generics,
    definitive: Ident,
    targets: Punctuated<Type, Comma>,
}

// Copied from syn
trait FilterAttrs<'a> {
    type Ret: Iterator<Item = &'a Attribute>;

    fn outer(self) -> Self::Ret;
}

impl<'a> FilterAttrs<'a> for &'a [Attribute] {
    type Ret = iter::Filter<slice::Iter<'a, Attribute>, fn(&&Attribute) -> bool>;

    fn outer(self) -> Self::Ret {
        fn is_outer(attr: &&Attribute) -> bool {
            match attr.style {
                syn::AttrStyle::Outer => true,
                syn::AttrStyle::Inner(_) => false,
            }
        }
        self.iter().filter(is_outer)
    }
}

mod parsing {
    use super::*;
    use syn::parse::{Error, Parse, ParseStream, Result};

    impl Parse for ForDeref {
        fn parse(input: ParseStream) -> Result<Self> {
            let _ = input.parse::<Token![for]>()?;
            let mut generics: Generics = input.parse()?;

            let targets = Punctuated::parse_separated_nonempty(input)?;

            let mut lookahead = input.lookahead1();
            if lookahead.peek(Token![where]) {
                generics.where_clause = Some(input.parse()?);
                lookahead = input.lookahead1();
            }

            if !input.is_empty() {
                return Err(lookahead.error());
            }

            let mut definitive: Option<Ident> = None;
            for param in &generics.params {
                if let GenericParam::Type(param) = param {
                    for bound in &param.bounds {
                        if matches!(bound, TypeParamBound::TraitSubst(_)) {
                            definitive = Some(param.ident.clone());
                            break;
                        }
                    }
                }
            }
            if definitive.is_none() {
                if let Some(clause) = generics.where_clause.as_ref() {
                    for pred in &clause.predicates {
                        if let WherePredicate::Type(pred) = pred {
                            for bound in &pred.bounds {
                                if matches!(bound, TypeParamBound::TraitSubst(_)) {
                                    if let Type::Path(TypePath { qself: None, path }) =
                                        &pred.bounded_ty
                                    {
                                        if let Some(ident) = path.get_ident() {
                                            definitive = Some(ident.clone());
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            let definitive = match definitive {
                Some(def) => def,
                None => {
                    return Err(Error::new(
                        Span::call_site(),
                        "no definitive type parameter: require a parameter bound like `T: trait``",
                    ));
                }
            };

            Ok(ForDeref {
                generics,
                definitive,
                targets,
            })
        }
    }
}

fn has_bound_on_self(gen: &syn::Generics) -> bool {
    if let Some(ref clause) = gen.where_clause {
        for pred in clause.predicates.iter() {
            if let syn::WherePredicate::Type(ref ty) = pred {
                if let Type::Path(ref bounded) = ty.bounded_ty {
                    if bounded.qself.is_none() && bounded.path.is_ident("Self") {
                        if ty
                            .bounds
                            .iter()
                            .any(|bound| matches!(bound, syn::TypeParamBound::Trait(_)))
                        {
                            return true;
                        }
                    }
                }
            }
            // Note: we ignore lifetime bounds, since Self: 'a implies that 'a
            // is a parameter with lifetime shorter than Self (thus is more a
            // bound on 'a than it is on Self), while 'a: Self is not supported.
        }
    }

    false
}

impl ForDeref {
    /// Expand over the given `item`
    ///
    /// This attribute does not modify the item.
    /// The caller should append the result to `item` impl_items.
    pub fn expand(self, item: TokenStream) -> TokenStream {
        let item = match syn::parse2::<Item>(item) {
            Ok(Item::Trait(item)) => item,
            Ok(item) => {
                emit_error!(item, "expected trait");
                return TokenStream::new();
            }
            Err(err) => {
                emit_error!(err);
                return TokenStream::new();
            }
        };

        let trait_ident = &item.ident;
        let (_, ty_generics, _) = item.generics.split_for_impl();
        let trait_ty = quote! { #trait_ident #ty_generics };
        let (impl_generics, where_clause) = impl_generics(self.generics, &item.generics, &trait_ty);

        let definitive = self.definitive;
        let definitive = quote! { < #definitive as #trait_ty > };

        let mut toks = TokenStream::new();
        for target in self.targets {
            // Tokenize, like ToTokens impls for syn::TraitItem*, but for definition
            let mut impl_items = TokenStream::new();
            let tokens = &mut impl_items;
            for item in &item.items {
                match item {
                    TraitItem::Const(item) => {
                        tokens.append_all(item.attrs.outer());
                        item.const_token.to_tokens(tokens);
                        item.ident.to_tokens(tokens);
                        item.colon_token.to_tokens(tokens);
                        item.ty.to_tokens(tokens);

                        Eq::default().to_tokens(tokens);
                        definitive.to_tokens(tokens);
                        Colon2::default().to_tokens(tokens);
                        item.ident.to_tokens(tokens);

                        item.semi_token.to_tokens(tokens);
                    }
                    TraitItem::Method(item) => {
                        if has_bound_on_self(&item.sig.generics) {
                            // If the method has a bound on Self, we cannot use a dereferencing
                            // implementation since the definitive type is not guaranteed to match
                            // the bound (we also cannot add a bound).

                            if item.default.is_none() {
                                emit_call_site_error!(
                                    "unable to write automatic trait implementations";
                                    note = item.span() => "this method has a bound on Self and no default implementation";
                                );
                            }

                            continue;
                        }

                        tokens.append_all(item.attrs.outer());
                        item.sig.to_tokens(tokens);

                        let ident = &item.sig.ident;
                        let params = item.sig.inputs.iter().map(|arg| match arg {
                            FnArg::Receiver(arg) => &arg.self_token as &dyn ToTokens,
                            FnArg::Typed(arg) => &arg.pat,
                        });
                        tokens.append_all(quote! { {
                            #definitive :: #ident ( #(#params),* )
                        } });
                    }
                    TraitItem::Type(item) => {
                        if has_bound_on_self(&item.generics) {
                            emit_call_site_error!(
                                "unable to write automatic trait implementations";
                                note = item.span() => "this type has a bound on Self";
                            );
                        }

                        tokens.append_all(item.attrs.outer());
                        item.type_token.to_tokens(tokens);
                        item.ident.to_tokens(tokens);

                        let (_, ty_generics, where_clause) = item.generics.split_for_impl();
                        ty_generics.to_tokens(tokens);

                        Eq::default().to_tokens(tokens);
                        definitive.to_tokens(tokens);
                        Colon2::default().to_tokens(tokens);
                        item.ident.to_tokens(tokens);
                        ty_generics.to_tokens(tokens);

                        where_clause.to_tokens(tokens);
                        item.semi_token.to_tokens(tokens);
                    }
                    TraitItem::Macro(item) => {
                        emit_error!(item, "unsupported: macro item in trait");
                    }
                    TraitItem::Verbatim(item) => {
                        emit_error!(item, "unsupported: verbatim item in trait");
                    }

                    /* Testing of exhaustive matching is disabled: syn 1.0.90 breaks it.
                    #[cfg(test)]
                    TraitItem::__TestExhaustive(_) => unimplemented!(),
                    #[cfg(not(test))]
                    */
                    _ => (),
                }
            }

            toks.append_all(quote! {
                #[automatically_derived]
                impl #impl_generics #trait_ty for #target #where_clause {
                    #impl_items
                }
            });
        }
        toks
    }
}
