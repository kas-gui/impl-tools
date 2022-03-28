// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Implementation of the `#[autoimpl]` attribute

use crate::generics::{
    clause_to_toks, impl_generics, GenericParam, Generics, TypeParamBound, WherePredicate,
};
use proc_macro2::{Span, TokenStream};
use proc_macro_error::emit_error;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{parse2, FnArg, Ident, Item, Path, PathArguments, Token, TraitItem, Type, TypePath};

/// Autoimpl for types supporting `Deref`
pub struct ForDeref {
    generics: Generics,
    definitive: Ident,
    targets: Punctuated<Type, Comma>,
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
                                    match pred.bounded_ty {
                                        Type::Path(TypePath {
                                            qself: None,
                                            path:
                                                Path {
                                                    leading_colon: None,
                                                    ref segments,
                                                },
                                        }) if segments.len() == 1
                                            && matches!(
                                                segments[0].arguments,
                                                PathArguments::None
                                            ) =>
                                        {
                                            definitive = Some(segments[0].ident.clone());
                                            break;
                                        }
                                        _ => (),
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

impl ForDeref {
    /// Expand over the given `item`
    ///
    /// This attribute does not modify the item.
    /// The caller should append the result to `item` tokens.
    pub fn expand(self, item: TokenStream) -> TokenStream {
        let item = match parse2::<Item>(item) {
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
        let impl_generics = impl_generics(&self.generics, &trait_ty);
        let where_clause = clause_to_toks(
            &self.generics.where_clause,
            item.generics.where_clause.as_ref(),
            &trait_ty,
        );

        let definitive = self.definitive;
        let definitive = quote! { < #definitive as #trait_ty > };

        let mut toks = TokenStream::new();
        for target in self.targets {
            let mut impl_items = TokenStream::new();
            for item in &item.items {
                match item {
                    TraitItem::Const(item) => {
                        let ident = &item.ident;
                        let ty = &item.ty;
                        impl_items.append_all(quote! {
                            const #ident : #ty = #definitive :: #ident;
                        });
                    }
                    TraitItem::Method(item) => {
                        let sig = &item.sig;
                        let ident = &sig.ident;
                        let params = sig.inputs.iter().map(|arg| match arg {
                            FnArg::Receiver(arg) => &arg.self_token as &dyn ToTokens,
                            FnArg::Typed(arg) => &arg.pat,
                        });
                        impl_items.append_all(quote! {
                            #sig {
                                #definitive :: #ident ( #(#params),* )
                            }
                        });
                    }
                    TraitItem::Type(item) => {
                        let ident = &item.ident;
                        impl_items.append_all(quote! {
                            type #ident = #definitive :: #ident;
                        });
                    }
                    TraitItem::Macro(item) => {
                        emit_error!(item, "unsupported: macro item in trait");
                    }
                    TraitItem::Verbatim(item) => {
                        emit_error!(item, "unsupported: verbatim item in trait");
                    }

                    #[cfg(test)]
                    TraitItem::__TestExhaustive(_) => unimplemented!(),
                    #[cfg(not(test))]
                    _ => (),
                }
            }

            toks.append_all(quote! {
                impl #impl_generics #trait_ty for #target #where_clause {
                    #impl_items
                }
            });
        }
        toks
    }
}
