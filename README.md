Impl-tools
=======

[![Test Status](https://github.com/kas-gui/impl-tools/workflows/Tests/badge.svg?event=push)](https://github.com/kas-gui/impl-tools/actions)
[![Latest version](https://img.shields.io/crates/v/impl-tools.svg)](https://crates.io/crates/impl-tools)
[![API](https://docs.rs/impl-tools/badge.svg)](https://docs.rs/impl-tools)
[![Minimum rustc version](https://img.shields.io/badge/rustc-1.56+-lightgray.svg)](https://github.com/kas-gui/impl-tools#supported-rust-versions)

A set of helper macros


Macros
------

### Autoimpl

`#[autoimpl]` is a partial replacement for `#[derive]`, supporting:

-   Explicit `where` clause on generic parameters
-   No implicit bounds on generic parameters beyond those required by the type
-   Traits like `Deref` by `using` a named field
-   Traits like `Debug` may `ignore` named fields

`#[autoimpl]` may also be used on trait definitions to impl for specified types
supporting `Deref`.

Unlike [alternatives](#alternatives), `#[autoimpl]` has minimal and intuitive syntax.

```rust
use impl_tools::autoimpl;
use std::fmt::Debug;

#[autoimpl(for<'a, T: trait + ?Sized> Box<T>)]
// Generates: impl<'a, T: Animal + ?Sized> Animal for Box<T> { .. }
trait Animal {
    fn number_of_legs(&self) -> u32;
}

#[autoimpl(Debug ignore self.animal where T: Debug)]
// Generates: impl<T, A: Animal> std::fmt::Debug for Named<A> where T: Debug { .. }
#[autoimpl(Deref, DerefMut using self.animal)]
// Generates: impl<T, A: Animal> std::ops::Deref for Named<A> { .. }
// Generates: impl<T, A: Animal> std::ops::DerefMut for Named<A> { .. }
struct Named<T, A: Animal> {
    name: T,
    animal: A,
}

fn main() {
    struct Fish;
    impl Animal for Fish {
        fn number_of_legs(&self) -> u32 {
            0
        }
    }

    let my_fish = Named {
        name: "Nemo",
        animal: Box::new(Fish),
    };

    assert_eq!(
        format!("{my_fish:?} has {} legs!", my_fish.number_of_legs()),
        r#"Named { name: "Nemo", .. } has 0 legs!"#
    );
}
```

### Impl Default

`#[impl_default]` implements `std::default::Default`:

```rust
use impl_tools::{impl_default, impl_scope};

#[impl_default(Tree::Ash)]
enum Tree { Ash, Beech, Birch, Willow }

impl_scope! {
    #[impl_default]
    struct Copse {
        tree_type: Tree,
        number: u32 = 7,
    }
}
```

### Impl Scope

`impl_scope!` is a function-like macro used to define a type plus its
implementations. It supports `impl Self` syntax:

```rust
use impl_tools::impl_scope;
use std::fmt::Display;

impl_scope! {
    /// I don't know why this exists
    pub struct NamedThing<T: Display, F> {
        name: T,
        func: F,
    }

    // Repeats generic parameters of type
    impl Self {
        fn format_name(&self) -> String {
            format!("{}", self.name)
        }
    }

    // Merges generic parameters of type
    impl<O> Self where F: Fn(&str) -> O {
        fn invoke(&self) -> O {
            (self.func)(&self.format_name())
        }
    }
}
```

Caveat: `rustfmt` won't currently touch the contents. Hopefully that
[can be fixed](https://github.com/rust-lang/rustfmt/issues/5254)!


Extensibility
-------------

Rust's `#[derive]` macro is extensible via `#[proc_macro_derive]` in a `proc-macro` crate. Our macros cannot be extended in the same way, but they can be extended via a new front-end:

1.  Create a copy of the `impl-tools` crate to create a new "front-end" (`proc-macro` crate).
    This crate is contains only a little code over the [`impl-tools-lib`] crate.
2.  To extend `#[autoimpl]`, write an impl of [`ImplTrait`] and add it to the attribute's definition.
    To extend `impl_scope!`, write an impl of [`ScopeAttr`] and add it to the macro's definition.
3.  Depend on your new front end crate instead of `impl-tools`.

[`impl-tools-lib`]: https://docs.rs/impl-tools-lib/
[`ImplTrait`]: https://docs.rs/impl-tools-lib/latest/impl_tools_lib/autoimpl/trait.ImplTrait.html
[`ScopeAttr`]: https://docs.rs/impl-tools-lib/latest/impl_tools_lib/trait.ScopeAttr.html

Supported Rust Versions
------------------------------

The MSRV is 1.56.0 for no particular reason other than that it is the first to support Edition 2021.

`no_std` is not yet supported (see #7).


Alternatives
------------

Both [Educe](https://crates.io/crates/educe) and [Derivative](https://crates.io/crates/derivative)
have similar functionality: the ability to implement various traits with more flexibility than
libstd's `#[derive]`. They also support more functionality such as tweaking the output of `Debug`.
Both have less clean syntax, requiring a minimum of two attributes to do anything, with further
attributes to customise implementations (e.g. to ignore a field).

[derive_more](https://crates.io/crates/derive_more) isn't exactly an "alternative", simply
supporting `#[derive]` for more standard traits. Possible functionality overlap in the future
(though for now `#[autoimpl]` doesn't support half the traits supported by `#[derive]`).

[auto_impl](https://crates.io/crates/auto_impl/) allows implementing a trait for reference types
(`&`, `&mut`, `Box`, `Rc`, `Arc`) as well as function types.


Copyright and Licence
---------------------

The [COPYRIGHT](COPYRIGHT) file includes a list of contributors who claim
copyright on this project. This list may be incomplete; new contributors may
optionally add themselves to this list.

The impl-tools library is published under the terms of the Apache License, Version 2.0.
You may obtain a copy of this licence from the [LICENSE](LICENSE) file or on
the following webpage: <https://www.apache.org/licenses/LICENSE-2.0>

