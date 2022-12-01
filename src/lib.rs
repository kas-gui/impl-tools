// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

#![allow(clippy::needless_doctest_main)]
// Lint advocates use of bool::then_some, stablizied in rustc 1.62.0
#![allow(clippy::unnecessary_lazy_evaluations)]

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
//! `singleton!` is a function-like macro used to define and instantiate a
//! unique (single-use) type. It supports everything supported by `impl_scope!`
//! plus field initializers and (limited) automatic typing of fields.
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

use impl_tools_lib::{self as lib, autoimpl};

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
pub fn impl_default(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut toks = item.clone();
    match syn::parse::<lib::ImplDefault>(args) {
        Ok(attr) => toks.extend(TokenStream::from(attr.expand(item.into()))),
        Err(err) => {
            emit_call_site_error!(err);
            // Since this form of invocation only adds implementations, we can
            // safely output the original item, thus reducing secondary errors.
        }
    }
    toks
}

/// An alternative to the standard `derive` macro
///
/// `#[autoimpl]` may be used in two ways:
///
/// -   [On a type definition](#on-type-definitions), to implement a specified trait (like `#[derive]`)
/// -   [On a trait definition](#on-trait-definitions), to implement the trait for specified types
///     supporting [`Deref`]
///
/// If using `autoimpl` **and** `derive` macros with Rust < 1.57.0, the
/// `autoimpl` attribute must come first (see rust#81119).
///
/// [`proc_macro_derive`]: https://doc.rust-lang.org/reference/procedural-macros.html#derive-macros
///
/// # On type definitions
///
/// | Path | *ignore* | *using* | *notes* |
/// |----- |--- |--- |--- |
/// | [`::core::borrow::Borrow<T>`] | - | borrow target | `T` is type of target field |
/// | [`::core::borrow::BorrowMut<T>`] | - | borrow target | `T` is type of target field |
/// | [`::core::clone::Clone`] | yes | - | ignored fields use `Default::default()` |
/// | [`::core::cmp::Eq`] | * | - | *allowed with `PartialEq` |
/// | [`::core::cmp::Ord`] | yes | - | |
/// | [`::core::cmp::PartialEq`] | yes | - | |
/// | [`::core::cmp::PartialOrd`] | yes | - | |
/// | [`::core::convert::AsRef<T>`] | - | ref target | `T` is type of target field |
/// | [`::core::convert::AsMut<T>`] | - | ref target | `T` is type of target field |
/// | [`::core::default::Default`] | - | - | [`macro@impl_default`] is a more flexible alternative |
/// | [`::core::fmt::Debug`] | yes | - | |
/// | [`::core::hash::Hash`] | yes | - | |
/// | [`::core::marker::Copy`] | * | - | *allowed with `Clone` |
/// | [`::core::ops::Deref`] | - | deref target | See [`Deref::Target` type](#dereftarget-type) below |
/// | [`::core::ops::DerefMut`] | - | deref target | |
///
/// Traits are matched from the path, as follows:
///
/// -   Only the last component, e.g. `#[autoimpl(Clone)]`
/// -   The full path with leading `::`, e.g. `#[autoimpl(::core::clone::Clone)]`
/// -   The full path without leading `::`, e.g. `#[autoimpl(core::clone::Clone)]`
/// -   The full path with/without leading `::`, using `std` instead of `core` or `alloc`,
///     e.g. `#[autoimpl(std::clone::Clone)]`
///
/// *Ignore:* some trait implementations supports ignoring listed fields.
/// For example, `#[autoimpl(PartialEq ignore self.foo)]` will implement
/// `PartialEq`, comparing all fields except `foo`.
/// Note: `Copy` and `Eq` do not *use* `ignore`, but tolerate its usage by a
/// companion trait (e.g. `#[autoimpl(PartialEq, Eq ignore self.a)]`).
///
/// *Using:* some trait implementations require a named field to "use".
/// For example, `#[autoimpl(Deref using self.foo)]` implements [`Deref`] to
/// return a reference to field `self.foo`.
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
/// **Targets:** each *Trait* listed is implemented for the annotated type.
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
/// ### `Deref::Target` type
///
/// The [`Deref`] trait has two members:
///
/// - `type Target: ?Sized`
/// - `fn deref(&self) -> &Self::Target`
///
/// `#[autoimpl(Deref using self.x)]` implements `Deref` as follows:
///
/// - `type Target = X` where field `x` has type `X`
/// - `fn deref(&self) -> &Self::Target { &self.x }`
///
/// For some uses this is fine, but in other cases a different `Target` type is
/// preferred. To achieve this, `Target` may be given explicitly:
///
/// ```
/// # use impl_tools::autoimpl;
/// #[autoimpl(Deref<Target = T> using self.0)]
/// struct MyBoxingWrapper<T: ?Sized>(Box<T>);
/// ```
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
/// # On trait definitions
///
/// User-defined traits may be implemented over any type supporting `Deref`
/// (and if required `DerefMut`) to another type supporting the trait.
///
/// ### Parameter syntax
///
/// > _ParamsTrait_ :\
/// > &nbsp;&nbsp; `for` _Generics_ ( _Type_ ),+ _WhereClause_?
///
/// **Targets:** the annotated trait is implemented for each *Type* listed.
///
/// **Definitive type:**
/// It is required that some generic type parameter has bound `trait`
/// (e.g. `T: trait`). The first such parameter is designated the *definitive type*.
///
/// ### Trait items
///
/// Assuming definitive type `T`, trait items are implemented as follows:
///
/// -   associated constant `const C`: `const C = T::C;`
/// -   associated type `type X`: `type X = T::X;`
/// -   method `fn foo(a: A, b: B)`: `T::foo(a, b)`
/// -   (unexpanded) macro items: not supported
///
/// Generics and where clauses on types and methods are supported.
///
/// Items with a where clause with a type bound on `Self` are not supported
/// since the item is not guaranteed to exist on the definitive type.
/// Exception: methods with a default implementation (in this case the item is
/// skipped).
///
/// ### Examples
///
/// Implement `MyTrait` for `&T`, `&mut T` and `Box<dyn MyTrait>`:
/// ```
/// # use impl_tools::autoimpl;
/// #[autoimpl(for<T: trait + ?Sized> &T, &mut T, Box<T>)]
/// trait MyTrait {
///     fn f(&self) -> String;
/// }
/// ```
/// The definitive type is `T`. For example, here, `f` is implemented with the
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
/// This macro facilitates definition of a type (struct, enum or union) plus
/// implementations via `impl Self { .. }` syntax: `Self` is expanded to the
/// type's name, including generics and bounds (as defined on the type).
///
/// Caveat: `rustfmt` can not yet format contents (see
/// [rustfmt#5254](https://github.com/rust-lang/rustfmt/issues/5254),
/// [rustfmt#5538](https://github.com/rust-lang/rustfmt/pull/5538)).
///
/// ## Special attribute macros
///
/// Additionally, `impl_scope!` supports special attribute macros evaluated
/// within its scope:
///
/// -   [`#[impl_default]`](macro@impl_default): implement [`Default`] using
///     field initializers (which are not legal syntax outside of `impl_scope!`)
///
/// Note: matching these macros within `impl_scope!` does not use path
/// resolution. Using `#[impl_tools::impl_default]` would resolve the variant
/// of this macro which *doesn't support* field initializers.
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
    let mut scope = parse_macro_input!(input as lib::Scope);
    scope.apply_attrs(lib::find_attr_impl_default);
    scope.expand().into()
}

/// Construct a single-instance struct
///
/// Rust doesn't currently support [`impl Trait { ... }` expressions](https://github.com/canndrew/rfcs/blob/impl-trait-expressions/text/0000-impl-trait-expressions.md)
/// or implicit typing of struct fields. This macro is a **hack** allowing that.
///
/// Example:
/// ```
/// use std::fmt;
/// fn main() {
///     let world = "world";
///     let says_hello_world = impl_tools::singleton! {
///         struct(&'static str = world);
///         impl fmt::Display for Self {
///             fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
///                 write!(f, "hello {}", self.0)
///             }
///         }
///     };
///     assert_eq!(format!("{}", says_hello_world), "hello world");
/// }
/// ```
///
/// That is, this macro creates an anonymous struct type (must be a struct),
/// which may have trait implementations, then creates an instance of that
/// struct.
///
/// Struct fields may have a fixed type or may be generic. Syntax is as follows:
///
/// -   **regular struct:** `ident: ty = value`
/// -   **regular struct:** `ident: ty` (uses `Default` to construct value)
/// -   **regular struct:** `ident = value` (type is generic without bounds)
/// -   **tuple struct:** `ty = value`
/// -   **tuple struct:** `ty` (uses `Default` to construct value)
///
/// The field name, `ident`, may be `_` (anonymous field).
///
/// The field type, `ty`, may be or may contain inferred types (`_`) and/or
/// `impl Trait` type expressions. These are substituted with generics on the
/// type.
///
/// Refer to [examples](https://github.com/search?q=singleton+repo%3Akas-gui%2Fkas+path%3Aexamples&type=Code) for usage.
#[proc_macro_error]
#[proc_macro]
pub fn singleton(input: TokenStream) -> TokenStream {
    let mut scope = parse_macro_input!(input as lib::Singleton).into_scope();
    scope.apply_attrs(lib::find_attr_impl_default);
    scope.expand().into()
}
