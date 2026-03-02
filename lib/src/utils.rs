// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! Utilities

use quote::ToTokens;
use syn::Attribute;

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
