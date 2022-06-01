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
struct Y<S, T> {
    _s: S,
    t: T,
}

#[test]
fn y() {
    let mut y = Y { _s: (), t: 1i32 };

    fn set(x: &mut i32) {
        *x = 2;
    }
    set(y.deref_mut());

    assert_eq!(y.t, 2);
}
