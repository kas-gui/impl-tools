Impltools
=======

A set of helper macros


Macros
------

### Autoimpl

`#[autoimpl]` is a variant of `#[derive]`, supporting:

-   explicit generic parameter bounds
-   ignored fields
-   traits defined using a primary field
-   generic re-implementations for traits

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

## Impl Default

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


Supported Rust Versions
------------------------------

The MSRV is 1.56.0 (first to support Edition 2021).

Using a nightly compiler will improve diagnostics.


Copyright and Licence
---------------------

The [COPYRIGHT](COPYRIGHT) file includes a list of contributors who claim
copyright on this project. This list may be incomplete; new contributors may
optionally add themselves to this list.

The KAS library is published under the terms of the Apache License, Version 2.0.
You may obtain a copy of this licence from the [LICENSE](LICENSE) file or on
the following webpage: <https://www.apache.org/licenses/LICENSE-2.0>

