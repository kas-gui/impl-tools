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
// Lint advocates use of bool::then_some, stablizied in rustc 1.62.0
#![allow(clippy::unnecessary_lazy_evaluations)]
#![allow(clippy::style)]

mod anon;
pub mod autoimpl;
mod default;
pub mod fields;
mod for_deref;
pub mod generics;
mod scope;

pub use anon::{Anon, AnonField, AnonScope};
pub use default::{find_attr_impl_default, AttrImplDefault, ImplDefault};
pub use for_deref::ForDeref;
use proc_macro2::Span;
pub use scope::{Scope, ScopeAttr, ScopeItem};
use syn::Ident;

/// Tool to make a formatted [`Ident`](struct@Ident)
pub struct IdentFormatter(String);
impl IdentFormatter {
    /// Construct a formatter
    pub fn new() -> Self {
        IdentFormatter(String::with_capacity(32))
    }

    /// Construct a new [`Ident`](struct@Ident)
    pub fn make(&mut self, args: std::fmt::Arguments, span: Span) -> Ident {
        use std::fmt::Write;

        self.0.clear();
        self.0.write_fmt(args).unwrap();
        Ident::new(&self.0, span)
    }

    /// Construct a new [`Ident`](struct@Ident), using [`Span::call_site`]
    ///
    /// # Example
    ///
    /// ```
    /// # use impl_tools_lib::IdentFormatter;
    /// let mut idfmt = IdentFormatter::new();
    /// let ident = idfmt.make_call_site(format_args!("x{}", 6));
    /// assert_eq!(ident, "x6");
    /// ```
    #[inline]
    pub fn make_call_site(&mut self, args: std::fmt::Arguments) -> Ident {
        self.make(args, Span::call_site())
    }
}

/// Simple, allocation-free path representation
#[derive(PartialEq, Eq)]
pub struct SimplePath(&'static [&'static str]);

impl std::fmt::Display for SimplePath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if !self.0.is_empty() {
            write!(f, "{}", self.0[0])?;
            for component in &self.0[1..] {
                write!(f, "::{}", component)?;
            }
        }

        Ok(())
    }
}

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
    /// This must match the path exactly, with two exceptions:
    ///
    /// -   if `path` has no leading colon but `self` does (empty first
    ///     component), the paths may still match
    /// -   if the first component of `self` is `core` or `alloc` but the first
    ///     component of `path` is `std`, the paths may still match
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

        let mut first = true;
        for (x, y) in path.segments.iter().zip(q.iter()) {
            if !x.arguments.is_empty() {
                return false;
            }

            #[allow(clippy::if_same_then_else)]
            if x.ident == y {
            } else if first && (*y == "core" || *y == "alloc") && x.ident == "std" {
            } else {
                return false;
            }

            first = false;
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
