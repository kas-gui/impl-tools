# Changelog
The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
