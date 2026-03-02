// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Utilities

use quote::ToTokens;
use syn::{Attribute, GenericParam, Generics};

pub trait PathAsStr {
    fn path_as_string(&self) -> String;
}

impl PathAsStr for Attribute {
    fn path_as_string(&self) -> String {
        self.path().to_token_stream().to_string()
    }
}

/// Determine whether to copy an attribute from a declaration to an implementation.
///
/// This is a HACK: there is no definitive determination of which attributes
/// should be emitted on the generated impl fn items. We use a whitelist.
pub fn propegate_attr_to_impl(attr: &Attribute) -> bool {
    matches!(
        attr.path_as_string().as_str(),
        "cfg" | "allow" | "warn" | "deny" | "forbid"
    )
}

/// Copy all attrs except `#[doc]`
pub fn copy_non_doc_attrs(attrs: &[Attribute]) -> Vec<Attribute> {
    let mut dest = Vec::with_capacity(attrs.len());
    for attr in attrs {
        if attr.path_as_string() != "doc" {
            dest.push(attr.clone());
        }
    }
    dest
}

/// Copy all generics from `gen2` into `generics`
///
/// Also removes default values for generics.
pub fn extend_generics(generics: &mut Generics, gen2: &Generics) {
    if generics.lt_token.is_none() {
        debug_assert!(generics.params.is_empty());
        debug_assert!(generics.gt_token.is_none());
        generics.lt_token = gen2.lt_token;
        generics.params = gen2.params.clone();
        generics.gt_token = gen2.gt_token;
    } else if gen2.lt_token.is_none() {
        debug_assert!(gen2.params.is_empty());
        debug_assert!(gen2.gt_token.is_none());
    } else {
        if !generics.params.empty_or_trailing() {
            generics.params.push_punct(Default::default());
        }
        generics.params.extend(gen2.params.clone().into_pairs());
    }

    // Strip defaults which are legal on the struct but not on impls
    for param in &mut generics.params {
        match param {
            GenericParam::Type(p) => {
                p.eq_token = None;
                p.default = None;
            }
            GenericParam::Lifetime(_) => (),
            GenericParam::Const(p) => {
                p.eq_token = None;
                p.default = None;
            }
        }
    }

    if let Some(ref mut clause1) = generics.where_clause {
        if let Some(ref clause2) = gen2.where_clause {
            if !clause1.predicates.empty_or_trailing() {
                clause1.predicates.push_punct(Default::default());
            }
            clause1
                .predicates
                .extend(clause2.predicates.clone().into_pairs());
        }
    } else {
        generics.where_clause = gen2.where_clause.clone();
    }
}
