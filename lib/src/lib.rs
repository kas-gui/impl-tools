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
pub mod generics;
mod scope;

pub use default::{AttrImplDefault, ImplDefault};
pub use scope::{Scope, ScopeAttr, ScopeItem};
