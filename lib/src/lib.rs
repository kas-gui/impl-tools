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
    /// Required: path is non-empty, and only first component is allowed to be
    /// zero-length.
    pub fn new(&self, path: &'static [&'static str]) -> Self {
        if path.is_empty() {
            panic!("empty path");
        }
        for s in path.iter().skip(1) {
            if s.is_empty() {
                panic!("empty component");
            }
        }
        SimplePath(path)
    }

    /// True if this matches a [`syn::Path`]
    pub fn matches(&self, path: &syn::Path) -> bool {
        let mut q = self.0;
        assert!(!q.is_empty());
        if path.leading_colon.is_some() {
            if !q[0].is_empty() {
                return false;
            }
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
