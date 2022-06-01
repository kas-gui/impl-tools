// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

#![allow(clippy::needless_doctest_main)]

//! # Impl-tools
//!
//! `#[autoimpl]` is a partial replacement for `#[derive]`, supporting:
//!
//! -   Explicit `where` clause on generic parameters
//! -   No implicit bounds on generic parameters beyond those required by the type
//! -   Traits like `Deref` by `using` a named field
//! -   Traits like `Debug` may `ignore` named fields
//!
//! `#[autoimpl]` may also be used on trait definitions to *re-implement* the
//! trait for given reference types.
//!
//! `impl_scope!` is a function-like macro used to define a type plus its
//! implementations. It supports two things:
//!
//! -   `impl Self` syntax
//! -   Evaluation of advanced attribute macros, which may use field
//!     initializers and read/write other impls within the scope
//!
//! User-extensions to both `#[autoimpl]` and `impl_scope!` are possible, by
//! writing your own proc-macro crate depending on
//! [impl-tools-lib](https://crates.io/crates/impl-tools-lib).

#[cfg(doctest)]
doc_comment::doctest!("../README.md");

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro_error::{emit_call_site_error, proc_macro_error};
use syn::parse_macro_input;

use impl_tools_lib::{autoimpl, AttrImplDefault, ImplDefault, Scope, ScopeAttr};

/// Impl `Default` with given field or type initializers
///
/// This macro may be used in one of two ways.
///
/// ### Type-level initializer
///
/// ```
/// # use impl_tools::impl_default;
/// /// A simple enum; default value is Blue
/// #[impl_default(Colour::Blue)]
/// enum Colour {
///     Red,
///     Green,
///     Blue,
/// }
///
/// fn main() {
///     assert!(matches!(Colour::default(), Colour::Blue));
/// }
/// ```
///
/// A where clause is optional: `#[impl_default(EXPR where BOUNDS)]`.
///
/// ### Field-level initializer
///
/// This variant only supports structs. Fields specified as `name: type = expr`
/// will be initialized with `expr`, while other fields will be initialized with
/// `Default::default()`.
///
/// ```
/// # use impl_tools::{impl_default, impl_scope};
/// impl_scope! {
///     #[impl_default]
///     struct Person {
///         name: String = "Jane Doe".to_string(),
///         age: u32 = 72,
///         occupation: String,
///     }
/// }
///
/// fn main() {
///     let person = Person::default();
///     assert_eq!(person.name, "Jane Doe");
///     assert_eq!(person.age, 72);
///     assert_eq!(person.occupation, "");
/// }
/// ```
///
/// A where clause is optional: `#[impl_default(where BOUNDS)]`.
#[proc_macro_attribute]
#[proc_macro_error]
pub fn impl_default(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut toks = item.clone();
    match syn::parse::<ImplDefault>(attr) {
        Ok(attr) => toks.extend(TokenStream::from(attr.expand(item.into()))),
        Err(err) => {
            emit_call_site_error!(err);
            // Since this form of invocation only adds implementations, we can
            // safely output the original item, thus reducing secondary errors.
        }
    }
    toks
}

/// A variant of the standard `derive` macro
///
/// `#[autoimpl]` may be used in two ways:
///
/// -   On a type definition, to implement a specified trait (like `#[derive]`)
/// -   On a trait definition, to re-implement that trait for a specified type
///     supporting [`Deref`]
///
/// If using `autoimpl` **and** `derive` macros with Rust < 1.57.0, the
/// `autoimpl` attribute must come first (see rust#81119).
///
/// [`proc_macro_derive`]: https://doc.rust-lang.org/reference/procedural-macros.html#derive-macros
///
/// # Trait implementation
///
/// | Name | Path | *ignore* | *using* |
/// |----- |----- |--- |--- |
/// | `Clone` | `::std::clone::Clone` | initialized with `Default::default()` | - |
/// | `Debug` | `::std::fmt::Debug` | field is not printed | - |
/// | `Default` | `::std::default::Default` | - | - |
/// | `Deref` | `::std::ops::Deref` | - | deref target |
/// | `DerefMut` | `::std::ops::DerefMut` | - | deref target |
///
/// Note: [`macro@impl_default`] is a more flexible alternative to `Default`.
///
/// *Ignore:* trait supports ignoring fields (e.g. `#[autoimpl(Debug ignore self.foo)]`).
///
/// *Using:* trait requires a named field to "use". Example:
/// `#[autoimpl(Deref using self.foo)]` implements [`Deref`] to return (a
/// reference to) field `self.foo`.
///
/// Traits are matched according to the path, via one of three forms:
///
/// -   Only the last component, e.g. `#[autoimpl(Clone)]`
/// -   The full path except leading `::`, e.g. `#[autoimpl(std::clone::Clone)]`
/// -   The full path with leading `::`, e.g. `#[autoimpl(::std::clone::Clone)]`
///
/// Note regarding `no_std` usage: the above table is wrong in that traits use
/// paths in `core` or `alloc`, but also match `std`. That is,
/// `#[autoimpl(std::clone::Clone)]` and `#[autoimpl(core::clone::Clone)]` are
/// equivalent and generate an impl for `::core::clone::Clone`.
///
/// ### Parameter syntax
///
/// > _ParamsMulti_ :\
/// > &nbsp;&nbsp; ( _Trait_ ),+ _Using_? _Ignores_? _WhereClause_?
/// >
/// > _Using_ :\
/// > &nbsp;&nbsp; `using` `self` `.` _Member_
/// >
/// > _Ignores_ :\
/// > &nbsp;&nbsp; `ignore` ( `self` `.` _Member_ ),+
/// >
/// > _WhereClause_ :\
/// > &nbsp;&nbsp; `where` ( _WherePredicate_ ),*
///
/// ### Generics and where clause
///
/// Type generics are inherited from the type definition. Bounds defined by the
/// type are inherited, but unlike `#[derive]` no additional bounds for the
/// trait being implemented are assumed.
///
/// A `where` clause, e.g. `where T: Foo`, may be used.
/// A special bound syntax, `T: trait`, indicates that `T` must support the
/// trait being implemented.
///
/// ### Examples
///
/// Implement `std::fmt::Debug`, ignoring the last field:
/// ```
/// # use impl_tools::autoimpl;
/// #[autoimpl(Debug ignore self.f)]
/// struct PairWithFn<T> {
///     x: f32,
///     y: f32,
///     f: fn(&T),
/// }
/// ```
///
/// Implement `Clone` and `Debug` on a wrapper, with the required bounds:
/// ```
/// # use impl_tools::autoimpl;
/// #[autoimpl(Clone, Debug where T: trait)]
/// struct Wrapper<T>(pub T);
/// ```
///
/// Implement `Deref` and `DerefMut`, dereferencing to the given field:
/// ```
/// # use impl_tools::autoimpl;
/// #[autoimpl(Deref, DerefMut using self.1)]
/// struct AnnotatedWrapper<T>(String, T);
/// ```
///
/// # Trait re-implementation
///
/// User-defined traits may be implemented over any type supporting `Deref`
/// (and if required `DerefMut`) to another type supporting the trait.
///
/// ### Parameter syntax
///
/// > _ParamsTrait_ :\
/// > &nbsp;&nbsp; `for` _Generics_ ( _Type_ ),+ _Definitive_? _WhereClause_?
/// >
/// > _Generics_ :\
/// > &nbsp;&nbsp; `<` ( _GenericParam_ ) `>`
/// >
/// > _Definitive_ :\
/// > &nbsp;&nbsp; `using` _Type_
///
/// ### Examples
///
/// Implement `MyTrait` for `&T`, `&mut T` and `Box<dyn MyTrait>`:
/// ```
/// # use impl_tools::autoimpl;
/// #[autoimpl(for<'a, T: trait + ?Sized> &'a T, &'a mut T, Box<T>)]
/// trait MyTrait {
///     fn f(&self) -> String;
/// }
/// ```
/// Note that the first parameter bound like `T: trait` is used as the
/// definitive type (required). For example, here, `f` is implemented with the
/// body `<T as MyTrait>::f(self)`.
///
/// Note further: if the trait uses generic parameters itself, these must be
/// introduced explicitly in the `for<..>` parameter list.
///
/// [`Deref`]: std::ops::Deref
#[proc_macro_attribute]
#[proc_macro_error]
pub fn autoimpl(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut toks = item.clone();
    match syn::parse::<autoimpl::Attr>(attr) {
        Ok(autoimpl::Attr::ForDeref(ai)) => toks.extend(TokenStream::from(ai.expand(item.into()))),
        Ok(autoimpl::Attr::ImplTraits(ai)) => {
            // We could use lazy_static to construct a HashMap for fast lookups,
            // but given the small number of impls a "linear map" is fine.
            let find_impl = |path: &syn::Path| {
                autoimpl::STD_IMPLS
                    .iter()
                    .cloned()
                    .find(|impl_| impl_.path().matches_ident_or_path(path))
            };
            toks.extend(TokenStream::from(ai.expand(item.into(), find_impl)))
        }
        Err(err) => {
            emit_call_site_error!(err);
            // Since autoimpl only adds implementations, we can safely output
            // the original item, thus reducing secondary errors.
        }
    }
    toks
}

/// Scope supporting `impl Self` and advanced attribute macros
///
/// This macro has three raisons d'être:
///
/// -   To support `impl Self` syntax
/// -   To allow field initializers, as used by [`macro@impl_default`]
/// -   To allow user-defined attribute macros to read/write other impls within
///     the `impl_scope`
///
/// Caveat: `rustfmt` can not yet format contents (see
/// [rustfmt#5254](https://github.com/rust-lang/rustfmt/issues/5254)).
///
/// ## Syntax
///
/// > _ImplScope_ :\
/// > &nbsp;&nbsp; `impl_scope!` `{` _ScopeItem_ _ItemImpl_ * `}`
/// >
/// > _ScopeItem_ :\
/// > &nbsp;&nbsp; _ItemEnum_ | _ItemStruct_ | _ItemType_ | _ItemUnion_
///
/// That is, one type definition followed by a set of implementations.
/// Impls must take one of two forms:
///
/// -   `impl Self { ... }` — generic parameters and bounds of the type are used
/// -   `impl MyType { ... }` where `MyType` matches the name of the defined type
///
/// Generic parameters from the type are included implicitly with the first form.
/// Additional generic parameters and where clauses are supported (parameters
/// and bounds are merged).
///
/// ## Example
///
/// ```
/// impl_tools::impl_scope! {
///     struct Pair<T>(T, T);
///
///     impl Self {
///         pub fn new(a: T, b: T) -> Self {
///             Pair(a, b)
///         }
///     }
///
///     impl Self where T: Clone {
///         pub fn splat(a: T) -> Self {
///             let b = a.clone();
///             Pair(a, b)
///         }
///     }
/// }
/// ```
#[proc_macro_error]
#[proc_macro]
pub fn impl_scope(input: TokenStream) -> TokenStream {
    let mut scope = parse_macro_input!(input as Scope);
    scope.apply_attrs(|path| {
        AttrImplDefault
            .path()
            .matches(path)
            .then(|| &AttrImplDefault as &dyn ScopeAttr)
    });
    scope.expand().into()
}
