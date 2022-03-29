// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

//! # Impl-tools-lib
//!
//! To implement the proc-macros, copy and modify the
//! [`impl-tools`](https://github.com/kas-gui/impl-tools/) crate, which is
//! merely documentation plus wrappers around this crate.

#![deny(missing_docs)]

pub mod autoimpl;
mod default;
pub mod fields;
mod for_deref;
pub mod generics;
mod scope;

pub use default::{AttrImplDefault, ImplDefault};
pub use for_deref::ForDeref;
pub use scope::{Scope, ScopeAttr, ScopeItem};

/// Simple, allocation-free path representation
pub struct SimplePath(&'static [&'static str]);

impl SimplePath {
    /// Construct, verifying validity
    ///
    /// If the first component is an empty string, this is treated as a leading
    /// colon (e.g. `["", "abc", "Def"] == `::abc::Def`). No other component may
    /// be empty. At least one non-empty component is required.
    ///
    /// Panics if requirements are not met.
    pub fn new(path: &'static [&'static str]) -> Self {
        let mut is_empty = false;
        for (i, s) in path.iter().enumerate() {
            is_empty = is_empty && s.is_empty();
            if i > 0 && s.is_empty() {
                panic!("empty component");
            }
        }
        if is_empty {
            panic!("empty path");
        }
        SimplePath(path)
    }

    /// True if this matches a [`syn::Path`]
    ///
    /// This must match the path exactly, with one exception: if `path` has no
    /// leading colon but `self` does (empty first component), then `path` may
    /// still match the remaining components of `self`.
    pub fn matches(&self, path: &syn::Path) -> bool {
        let mut q = self.0;
        assert!(!q.is_empty());
        if path.leading_colon.is_some() && !q[0].is_empty() {
            return false;
        }
        if q[0].is_empty() {
            q = &q[1..];
        }

        if path.segments.len() != q.len() {
            return false;
        }

        for (x, y) in path.segments.iter().zip(q.iter()) {
            if x.ident != y || !x.arguments.is_empty() {
                return false;
            }
        }

        true
    }

    /// True if the last component matches a [`syn::Ident`](struct@syn::Ident)
    pub fn matches_ident(&self, ident: &syn::Ident) -> bool {
        assert!(!self.0.is_empty());
        self.0.iter().last().map(|s| ident == s).unwrap_or(false)
    }

    /// If input `path` has a single component with no leading colon, then
    /// match via [`Self::matches_ident`]; otherwise match via
    /// [`Self::matches`].
    pub fn matches_ident_or_path(&self, path: &syn::Path) -> bool {
        if path.leading_colon.is_none() && path.segments.len() == 1 {
            let seg = &path.segments[0];
            seg.arguments.is_empty() && self.matches_ident(&seg.ident)
        } else {
            self.matches(path)
        }
    }
}

mod printing {
    use super::SimplePath;
    use proc_macro2::{Ident, Span, TokenStream};
    use quote::{quote, ToTokens, TokenStreamExt};

    impl ToTokens for SimplePath {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let mut iter = self.0.iter();
            let first = iter.next().unwrap();
            if !first.is_empty() {
                tokens.append(Ident::new(first, Span::call_site()));
            }
            for next in iter {
                let ident = Ident::new(next, Span::call_site());
                tokens.append_all(quote! { :: #ident });
            }
        }
    }
}
