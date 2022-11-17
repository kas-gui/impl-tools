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

// Impl Animal for Box<T> where T: Animal + ?Sized
#[autoimpl(for<T: trait + ?Sized> Box<T>)]
trait Animal {
    fn number_of_legs(&self) -> u32;
}

// Impl Debug for Named<T, A: Animal> omitting field animal from output
#[autoimpl(Debug ignore self.animal where T: Debug)]
// Impl Deref and DerefMut to field animal for Named<T, A: Animal>
#[autoimpl(Deref, DerefMut using self.animal)]
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

#### New-type wrappers

A combination of `Deref` on the new-type and trait-reimplementation on the
trait allows succinct new-type patterns:

```rust
use impl_tools::autoimpl;
use std::sync::Arc;

// Impl Foo for &T, &mut T and Arc<T>
#[autoimpl(for<T: trait + ?Sized> &T, &mut T, Arc<T>)]
// Optional: impl Foo for NewFoo (requires NewFoo: Deref<Target = T>)
#[autoimpl(for<T: trait> NewFoo<T>)]
pub trait Foo {
    fn success(&self) -> bool;
}

// Impl Deref and DerefMut to a Target which itself supports Foo
#[autoimpl(Deref<Target = T>, DerefMut using self.0)]
pub struct NewFoo<T: Foo>(T);

// Impl Deref and DerefMut to a Target which itself supports Foo
#[autoimpl(Deref<Target = dyn Foo>, DerefMut using self.0)]
pub struct ArcDynFoo(Arc<dyn Foo>);

#[test]
fn test_foo_newtypes() {
    struct Success;
    impl Foo for Success {
        fn success(&self) -> bool { true }
    }

    // We can now directly call Foo's methods on the wrapper:
    assert!(NewFoo(Success).success());
    assert!(ArcDynFoo(Arc::new(Success)).success());
}
```

See [`tests/newtype.rs`](https://github.com/kas-gui/impl-tools/blob/master/tests/newtype.rs) for more variants of this pattern.


### Impl Default

`#[impl_default]` implements `std::default::Default`:

```rust
#[impl_tools::impl_default(Tree::Ash)]
enum Tree { Ash, Beech, Birch, Willow }

impl_tools::impl_scope! {
    #[impl_default]
    struct Copse {
        tree_type: Tree,
        number: u32 = 7,
    }
}
```

Note: `#[impl_default]` is matched within an `impl_scope!` regardless of imports.

### Impl Scope

`impl_scope!` is a function-like macro used to define a type plus its
implementations. It supports `impl Self` syntax:

```rust
use std::fmt::Display;

impl_tools::impl_scope! {
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
[can be fixed](https://github.com/rust-lang/rustfmt/pull/5538)!

### Singleton

`singleton!` is a function-like macro to construct a single-use struct with
custom implementations (similar: [RFC#2604](https://github.com/rust-lang/rfcs/pull/2604)).

Example:
```rust
use std::fmt;
fn main() {
    let world = "world";
    let says_hello_world = impl_tools::singleton! {
        struct(&'static str = world);
        impl fmt::Display for Self {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "hello {}", self.0)
            }
        }
    };
    assert_eq!(format!("{}", says_hello_world), "hello world");
}
```


Extensibility
-------------

Rust's `#[derive]` macro is extensible via `#[proc_macro_derive]` in a `proc-macro` crate.
Our macros cannot be extended in the same way, but they can be extended via a new front-end:

1.  Create a copy of the `impl-tools` crate to create a new "front-end" (`proc-macro` crate).
    This crate is contains only a little code over the [`impl-tools-lib`] crate.
2.  To extend `#[autoimpl]`, write an impl of [`ImplTrait`] and add it to the attribute's definition.
    To extend `impl_scope!`, write an impl of [`ScopeAttr`] and add it to the macro's definition.
3.  Depend on your new front end crate instead of `impl-tools`.

For an example of this approach, see [kas-macros](https://github.com/kas-gui/kas/tree/master/crates/kas-macros).

[`impl-tools-lib`]: https://docs.rs/impl-tools-lib/
[`ImplTrait`]: https://docs.rs/impl-tools-lib/latest/impl_tools_lib/autoimpl/trait.ImplTrait.html
[`ScopeAttr`]: https://docs.rs/impl-tools-lib/latest/impl_tools_lib/trait.ScopeAttr.html


Supported Rust Versions
------------------------------

The MSRV is 1.56.0 for no particular reason other than that it is the first to support Edition 2021.

When using a sufficiently recent compiler version (presumably 1.65.0), generic associated types
are supported (only applicable to `#[autoimpl]` on trait definitions using GATs).


Alternatives
------------

### Derive alternatives

Both [Educe](https://crates.io/crates/educe) and [Derivative](https://crates.io/crates/derivative)
have similar functionality: the ability to implement standard traits with more flexibility than
libstd's `#[derive]`.

In comparison, impl-tools' `#[autoimpl]` has cleaner syntax but is less flexible:
```rust,ignore
#[derive(Derivative)]
#[derivative(PartialEq, Eq)]
struct Foo<S, T: ?Sized> {
    foo: S,
    #[derivative(PartialEq="ignore")]
    bar: u8,
    #[derivative(PartialEq(bound=""), Eq(bound=""))]
    ptr: *const T,
}

#[derive(Educe)]
#[educe(PartialEq(bound = "S: PartialEq"), Eq(bound = "S: Eq"))]
struct Foo<S, T: ?Sized> {
    foo: S,
    #[educe(PartialEq(ignore))]
    bar: u8,
    ptr: *const T,
}

// impl-tools:
#[autoimpl(PartialEq, Eq ignore self.bar where S: trait)]
struct Foo<S, T: ?Sized> {
    foo: S,
    bar: u8,
    ptr: *const T,
}
```

Note: `#[derive]` and `Derivative` add bounds like `S: PartialEq, T: PartialEq` on generic parameters by default; `Educe` and `impl-tools` do not.

### Derive extensions

[derive_more](https://crates.io/crates/derive_more) isn't exactly an "alternative", simply
supporting `#[derive]` for more standard traits such as `Add` and `From`.
This is not (currently) supported by `#[autoimpl]` (or, to my knowledge, any alternative).

[auto_impl](https://crates.io/crates/auto_impl/) allows implementing a trait for reference types
(`&`, `&mut`, `Box`, `Rc`, `Arc`) as well as function types. The former (reference types) is
supported by `#[autoimpl]` (and is slightly more general):
```rust,ignore
// auto_impl:
#[auto_impl(&, Box)]
trait Foo {
    fn foo(&self);
}

// impl-tools:
#[autoimpl(for<T: trait + ?Sized> &T, Box<T>)]
trait Foo {
    fn foo(&self);
}
```


Copyright and Licence
---------------------

The [COPYRIGHT](COPYRIGHT) file includes a list of contributors who claim
copyright on this project. This list may be incomplete; new contributors may
optionally add themselves to this list.

The impl-tools library is published under the terms of the Apache License, Version 2.0.
You may obtain a copy of this licence from the [LICENSE](LICENSE) file or on
the following webpage: <https://www.apache.org/licenses/LICENSE-2.0>

