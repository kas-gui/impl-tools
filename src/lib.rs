// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

#![allow(clippy::needless_doctest_main)]

//! # Impl-tools
//!
//! ## Autoimpl
//!
//! `#[autoimpl]` is a variant of `#[derive]`, supporting:
//!
//! -   explicit generic parameter bounds
//! -   ignored fields
//! -   traits defined using a primary field
//! -   generic re-implementations for traits
//!
//! ```
//! use impl_tools::autoimpl;
//! use std::fmt::Debug;
//!
//! #[autoimpl(for<'a, T: trait + ?Sized> Box<T>)]
//! // Generates: impl<'a, T: Animal + ?Sized> Animal for Box<T> { .. }
//! trait Animal {
//!     fn number_of_legs(&self) -> u32;
//! }
//!
//! #[autoimpl(Debug ignore self.animal where T: Debug)]
//! // Generates: impl<T, A: Animal> std::fmt::Debug for Named<A> where T: Debug { .. }
//! #[autoimpl(Deref, DerefMut using self.animal)]
//! // Generates: impl<T, A: Animal> std::ops::Deref for Named<A> { .. }
//! // Generates: impl<T, A: Animal> std::ops::DerefMut for Named<A> { .. }
//! struct Named<T, A: Animal> {
//!     name: T,
//!     animal: A,
//! }
//!
//! fn main() {
//!     struct Fish;
//!     impl Animal for Fish {
//!         fn number_of_legs(&self) -> u32 {
//!             0
//!         }
//!     }
//!
//!     let my_fish = Named {
//!         name: "Nemo",
//!         animal: Box::new(Fish),
//!     };
//!
//!     assert_eq!(
//!         format!("{my_fish:?} has {} legs!", my_fish.number_of_legs()),
//!         r#"Named { name: "Nemo", .. } has 0 legs!"#
//!     );
//! }
//! ```
//!
//! ## Derive Default
//!
//! `#[impl_default]` implements `std::default::Default`:
//!
//! ```
//! use impl_tools::{impl_default, impl_scope};
//!
//! #[impl_default(Tree::Ash)]
//! enum Tree { Ash, Beech, Birch, Willow }
//!
//! impl_scope! {
//!     #[impl_default]
//!     struct Copse {
//!         tree_type: Tree,
//!         number: u32 = 7,
//!     }
//! }
//! ```
//!
//! ## Impl Scope
//!
//! `impl_scope!` is a function-like macro used to define a type plus its
//! implementations. It supports `impl Self` syntax:
//!
//! ```
//! use impl_tools::impl_scope;
//! use std::fmt::Display;
//!
//! impl_scope! {
//!     /// I don't know why this exists
//!     pub struct NamedThing<T: Display, F> {
//!         name: T,
//!         func: F,
//!     }
//!
//!     // Repeats generic parameters of type
//!     impl Self {
//!         fn format_name(&self) -> String {
//!             format!("{}", self.name)
//!         }
//!     }
//!
//!     // Merges generic parameters of type
//!     impl<O> Self where F: Fn(&str) -> O {
//!         fn invoke(&self) -> O {
//!             (self.func)(&self.format_name())
//!         }
//!     }
//! }
//! ```

#[cfg(doctest)]
doc_comment::doctest!("../README.md");

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro_error::{emit_call_site_error, proc_macro_error};
use syn::parse_macro_input;

use impl_tools_lib::{autoimpl, AttrImplDefault, ImplDefault, Scope};

/// Implement `Default`
///
/// This macro may be used in one of two ways.
///
/// ### Type-level initialiser
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
/// ### Field-level initialiser
///
/// This variant only supports structs. Fields specified as `name: type = expr`
/// will be initialised with `expr`, while other fields will be initialised with
/// `Defualt::default()`.
///
/// ```
/// # use impl_tools::{impl_default, impl_scope};
///
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
/// This macro is similar to `#[derive(Trait)]`, but with a few differences.
///
/// If using `autoimpl` **and** `derive` macros with Rust < 1.57.0, the
/// `autoimpl` attribute must come first (see rust#81119).
///
/// Unlike `derive`, `autoimpl` is not extensible by third-party crates. The
/// "trait names" provided to `autoimpl` are matched directly, unlike
/// `derive(...)` arguments which are paths to [`proc_macro_derive`] instances.
/// Without language support for this there appears to be no option for
/// third-party extensions.
///
/// [`proc_macro_derive`]: https://doc.rust-lang.org/reference/procedural-macros.html#derive-macros
///
/// ### Bounds on generic parameters
///
/// If a type has generic parameters, generated implementations will assume the
/// same parameters and bounds as specified in the type, but not additional
/// bounds for the trait implemented.
///
/// Additional bounds may be specified via a `where` clause. A special predicate
/// is supported: `T: trait`; here `trait` is replaced the name of the trait
/// being implemented.
///
/// # Multi-field traits
///
/// Some trait implementations make use of all fields (except those ignored):
///
/// -   `Clone` — implements `std::clone::Clone`; ignored fields are
///     initialised with `Default::default()`
/// -   `Debug` — implements `std::fmt::Debug`; ignored fields are not printed
/// -   `Default` — implements `std::default::Default` using
///     `Default::default()` for all fields (see also [`impl_default`](macro@impl_default))
///
/// ### Parameter syntax
///
/// > _ParamsMulti_ :\
/// > &nbsp;&nbsp; ( _Trait_ ),+ _Ignores_? _WhereClause_?
/// >
/// > _Ignores_ :\
/// > &nbsp;&nbsp; `ignore` ( `self` `.` _Member_ ),+
/// >
/// > _WhereClause_ :\
/// > &nbsp;&nbsp; `where` ( _WherePredicate_ ),*
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
/// Note: `T: trait` is a special predicate implying that for each
/// implementation the type `T` must support the trait being implemented.
///
/// # Single-field traits
///
/// Other traits are implemented using a single field (for structs):
///
/// -   `Deref` — implements `std::ops::Deref`
/// -   `DerefMut` — implements `std::ops::DerefMut`
///
/// ### Parameter syntax
///
/// > _ParamsSingle_ :\
/// > &nbsp;&nbsp; ( _Trait_ ),+ _Using_ _WhereClause_?
/// >
/// > _Using_ :\
/// > &nbsp;&nbsp; `using` `self` `.` _Member_
///
/// ### Examples
///
/// Implement `Deref` and `DerefMut`, dereferencing to the given field:
/// ```
/// # use impl_tools::autoimpl;
/// #[autoimpl(Deref, DerefMut using self.0)]
/// struct MyWrapper<T>(T);
/// ```
///
/// # Trait re-implementations
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
#[proc_macro_attribute]
#[proc_macro_error]
pub fn autoimpl(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut toks = item.clone();
    match syn::parse::<autoimpl::Attr>(attr) {
        Ok(autoimpl::Attr::ForDeref(ai)) => toks.extend(TokenStream::from(ai.expand(item.into()))),
        Ok(autoimpl::Attr::ImplTraits(ai)) => {
            // We could use lazy_static to construct a HashMap for fast lookups,
            // but given the small number of impls a "linear map" is fine.
            let find_impl = |ident: &syn::Ident| {
                autoimpl::STD_IMPLS
                    .iter()
                    .cloned()
                    .find(|impl_| impl_.path().matches_ident(ident))
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

/// Implementation scope
///
/// Supports `impl Self` syntax.
///
/// Also supports struct field assignment syntax for `Default`: see [`impl_default`](macro@impl_default).
///
/// Caveat: `rustfmt` will not format contents (see
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
/// The result looks a little like a module containing a single type definition
/// plus its implementations, but is injected into the parent module.
///
/// Implementations must target the type defined at the start of the scope. A
/// special syntax for the target type, `Self`, is added:
///
/// > _ScopeImplItem_ :\
/// > &nbsp;&nbsp; `impl` _GenericParams_? _ForTrait_? _ScopeImplTarget_ _WhereClause_? `{`
/// > &nbsp;&nbsp; &nbsp;&nbsp; _InnerAttribute_*
/// > &nbsp;&nbsp; &nbsp;&nbsp; _AssociatedItem_*
/// > &nbsp;&nbsp; `}`
/// >
/// > _ScopeImplTarget_ :\
/// > &nbsp;&nbsp; `Self` | _TypeName_ _GenericParams_?
///
/// That is, implementations may take one of two forms:
///
/// -   `impl MyType { ... }`
/// -   `impl Self { ... }`
///
/// Generic parameters from the type are included automatically, with bounds as
/// defined on the type. Additional generic parameters and an additional where
/// clause are supported (generic parameter lists and bounds are merged).
///
/// ## Example
///
/// ```
/// use impl_tools::impl_scope;
/// use std::ops::Add;
///
/// impl_scope! {
///     struct Pair<T>(T, T);
///
///     impl Self where T: Clone + Add {
///         fn sum(&self) -> <T as Add>::Output {
///             self.0.clone().add(self.1.clone())
///         }
///     }
/// }
/// ```
#[proc_macro_error]
#[proc_macro]
pub fn impl_scope(input: TokenStream) -> TokenStream {
    let mut scope = parse_macro_input!(input as Scope);
    scope.apply_attrs(&[&AttrImplDefault]);
    scope.generate().into()
}
