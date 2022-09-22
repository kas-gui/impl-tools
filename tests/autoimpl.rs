//! Test #[autoimpl] for trait implementations

// Test no_std
#![no_std]
extern crate alloc;
use alloc::format;

use core::fmt::Debug;
use core::marker::PhantomData;
use core::ops::DerefMut;
use impl_tools::autoimpl;

fn test_has_clone(_: impl Clone) {}

#[autoimpl(std::clone::Clone, core::fmt::Debug)]
struct Unit;

#[test]
fn unit() {
    test_has_clone(Unit);
    assert_eq!(format!("{:?}", Unit), "Unit");
}

#[autoimpl(Clone, Debug ignore self.1 where T: trait)]
struct Wrapper<T>(pub T, ());

#[test]
fn wrapper() {
    test_has_clone(Wrapper(0i32, ()));
    assert_eq!(format!("{:?}", Wrapper((), ())), "Wrapper((), _)");
}

#[autoimpl(Clone, Default where A: trait, B: trait)]
#[autoimpl(Debug ignore self.c where A: Debug)]
struct X<A, B: Debug, C> {
    a: A,
    b: B,
    c: PhantomData<C>,
}

#[test]
fn x() {
    let x = X {
        a: 1i8,
        b: "abc",
        c: PhantomData::<fn()>,
    };
    let y = x.clone();
    assert_eq!(x.a, y.a);
    assert_eq!(x.b, y.b);
    assert_eq!(format!("{:?}", x), "X { a: 1, b: \"abc\", .. }");
}

#[autoimpl(Deref, DerefMut using self.t)]
#[autoimpl(Borrow, BorrowMut using self.t)]
#[autoimpl(AsRef, AsMut using self.t)]
struct Y<S, T> {
    _s: S,
    t: T,
}

#[test]
fn y() {
    use core::borrow::{Borrow, BorrowMut};
    use core::ops::Deref;

    let mut y = Y { _s: (), t: 1i32 };

    *y.deref_mut() = 2;
    assert_eq!(y.deref(), &2);
    assert_eq!(y.t, 2);

    *y.borrow_mut() = 15;
    assert_eq!(Borrow::<i32>::borrow(&y), &15);

    *y.as_mut() = 12;
    assert_eq!(y.as_ref(), &12);
}

#[autoimpl(Clone, Debug)]
#[autoimpl(PartialEq, Eq ignore self.f)]
struct MixedComponents {
    i: i32,
    s: &'static str,
    f: fn() -> i32,
}

#[test]
fn mixed_components() {
    let a = MixedComponents {
        i: 31,
        s: "abc",
        f: || 9,
    };
    assert_eq!(a, a);

    let b = MixedComponents {
        i: 31,
        s: "abc",
        f: || 14,
    };
    assert_eq!(a, b); // field f differs but is ignored

    let mut c = a.clone();
    c.i = 2;
    assert!(a != c);

    let mut d = a.clone();
    d.s = "def";
    assert!(a != d);
}
