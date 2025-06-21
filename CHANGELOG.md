# Changelog
The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# [0.11.3] (lib only)

-   Fix `#[autoimpl(Clone)]` and `#[autoimpl(Hash)]` for non-`Copy` enums (#55, #56)

# [0.11.2]

Version numbers are re-synchronised. In addition:

-   Bump MSRV to 1.65 (#51)
-   Add `scope::{ScopeModAttrs, ScopeMod}` to `impl-tools-lib` (#53)
-   Add `#[impl_self]` to `impl-tools` (#53)

## [0.10.3], [impl-tools-lib-0.11.1] — 2024-12-21

-   Let `#[autoimpl]` on traits support function arguments using `mut` and destructuring patterns (#45)
-   Improve documentation for `#[autoimpl]` (#46)

## [0.10.2], [impl-tools-lib-0.11.0] — 2024-12-09

Re-release, bumping `impl-tools-lib` to v0.11.0 since it turns out that switching to `proc-macro-error2` was an API-breaking release for `impl-tools-lib` (this is only apparent if a macro emits an error, resulting in a message like "help: message: proc-macro-error2 API cannot be used outside of `entry_point` invocation, perhaps you forgot to annotate your #[proc_macro] function with `#[proc_macro_error]
").

`impl-tools` is unaffected excepting where an incompatible version of `impl-tools-lib` is used. Broken versions will be yanked.

## [0.10.1] — 2024-10-21

-   Improve CI workflows (#38)
-   Explicit MSRV = 1.58 (#38)
-   Replace dependency `proc-macro-error` with `proc-macro-error2` (#41)
-   Bump MSRV to 1.61 (#42)

## [0.10.0] — 2023-09-07

-   Rename `singleton!` → `impl_anon!` (#36)
-   Reorganise `impl-tools-lib`: new `anon` and `scope` public modules (#36)

## [0.9.1] — 2023-09-07

-   Fix clone for fields which auto-deref (issue #34)

## [0.9.0] — 2023-06-28

-   Update to syn v2.0.0

## [0.8.0] — 2023-02-07

-   Bump MSRV to 1.58.0 (#31)
-   `#[autoimpl(Clone, Debug, PartialEq, Eq, Hash)]` now all support enums
    (with optional `where` clause, without `ignore` clauses) (#31)
-   Add `impl_tools_lib::ImplTrait::enum_impl`, `enum_items` with default impls;
    `ImplTraits::expand` now supports enums (#31)
-   Add `impl_tools_lib::Ident_formatter` utility (#31)

Note: `PartialOrd, Ord` *could* now support enums (unimplemented). `ignore` and
`using` clauses are deliberately not supported (due to syntactic ambiguity).

## [0.6.2], `impl-tools-lib` [0.7.1] — 2022-12-16

-   Fix `#[autoimpl]` on traits: copy `#[cfg(..)]` attributes (#30)

## [0.6.1], `impl-tools-lib` [0.7.0] — 2022-12-01

-   Better diagnostics for trait-redefinition: require `Deref` bound (#28)
-   Document `Deref` with custom `Target` type

`impl-tools-lib` has breaking changes and therefore a higher version number:

-   Replace free function `impl_generics` with method `Generics::impl_generics`
-   Add method `Generics::ty_generics`

Note: next breaking release for `impl-tools` should bump version to match `-lib`.

## [0.6.0] — 2022-11-17

-   Add `ImplTrait::support_path_args`, `ImplArgs::path_args` (#26)
-   Path args: support `Deref<Target = Foo>` (#26)

## [0.5.2] — 2022-10-06

-   Add `singleton!` macro (#25)

## [0.5.1] — 2022-09-23

-   Fix: do not copy attributes on trait items (#24)

## [0.5.0] — 2022-09-22

-   `#[autoimpl]` on traits now merges trait generics with macro generics (#21)
-   `lib::autoimpl::struct_items` returns the trait path in addition to impl items (#22)
-   Add `lib::autoimpl::ImplArgs::for_fields`, `for_fields_iter` (#22)
-   Add autoimpl support for `Copy`, `AsRef`, `AsMut`, `Borrow`, `BorrowMut`,
    `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash` (#22)
-   Add `#[automatically_derived]` annotation to generated impls (#22)

## [0.4.4] — 2022-09-19

-   Fix `#[autoimpl]` on traits for items with where bounds on `Self` (#20)

## [0.4.3] — 2022-09-17

-   Fix `#[autoimpl]` on traits for GATs with where clauses (#19)

## [0.4.2] — 2022-09-17

-   Correct release of 0.4.1 (which bumped the version of impl-tools without
    bumping impl-tools-lib) (#18)
-   Fix `#[autoimpl]` on traits for GATs and attributes on trait const/method/type items (#17)

## [0.4.1] — 2022-09-17

No changes (prefer 0.4.2 instead).

## [0.4.0] — 2022-08-19

Change signature of `ScopeAttr::apply`: replace `args: TokenStream, attr_span: Span`
with `attr: Attribute` (#15).

## [0.3.2] — 2022-06-01

Support `no_std`. Support matching standard traits via paths from `core`/`alloc`
as well as via paths from `std`.

## [0.3.1] — 2022-04-17

Documentation improvements only.

## [0.3.0] — 2022-03-29

The library now supports extensibility. Most code has been moved to a new crate,
`impl-tools-lib`. Users may replace `impl-tools` with their own front-end
proc-macro crate, adding/removing the traits supported by `#[autoimpl]` and the
attributes supported by `impl_scope!`.

-   Extensibility for `impl_scope!`
-   Extensibility for `#[autoimpl]`
-   Permit path arguments in `#[autoimpl]`
-   Bug-fix for `#[autoimpl(Debug)]` on tuple and unit structs
-   Lots of internal code revision

## [0.2.0] — 2022-03-23

Add `impl_scope!` function-like macro (derived from `kas_macros::widget!`) and
`#[impl_default]` attribute macro.

## [0.1.0] — 2022-03-21

New release, including the `#[autoimpl]` attribute macro (extracted from
`kas-macros` crate).
