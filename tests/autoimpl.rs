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
fn test_has_copy(_: impl Copy) {}
fn test_has_partial_eq(_foo: impl PartialEq) {}
fn test_has_eq(_foo: impl Eq) {}

#[autoimpl(std::clone::Clone, core::fmt::Debug)]
struct Unit;

#[test]
fn unit() {
    test_has_clone(Unit);
    assert_eq!(format!("{:?}", Unit), "Unit");
}

#[autoimpl(Copy, Clone, Debug ignore self.1 where T: trait)]
struct Wrapper<T>(pub T, ());

#[test]
fn wrapper() {
    test_has_clone(Wrapper(0i32, ()));
    test_has_copy(Wrapper("foo", ()));
    assert_eq!(format!("{:?}", Wrapper((), ())), "Wrapper((), _)");
}

#[autoimpl(Clone)]
struct ContainsRef<'a, T: Clone> {
    a: &'a T,
}

#[test]
fn contains_ref() {
    let a = 1;
    let b = ContainsRef { a: &a };
    let c = b.clone();
    let _ = c.a;
}

#[autoimpl(Clone, Default where A: trait, B: trait)]
#[autoimpl(Debug ignore self.c where A: Debug)]
struct X<A, B: Debug, C> {
    a: A,
    b: B,
    c: PhantomData<C>,
}

#[autoimpl(Clone where A: trait, B: trait)]
#[autoimpl(Debug where A: Debug)]
enum MaybeX<A, B: Debug, C> {
    None,
    Some(X<A, B, C>),
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
    assert_eq!(format!("{x:?}"), "X { a: 1, b: \"abc\", .. }");

    let none = MaybeX::<(), (), ()>::None;
    assert_eq!(format!("{none:?}"), "MaybeX::None");
    test_has_clone(MaybeX::Some(x));
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

#[autoimpl(PartialEq, PartialOrd)]
#[derive(Clone, Copy, Debug)]
struct Pair(f32, f32);

#[test]
fn pair() {
    use core::cmp::Ordering;
    let a = Pair(123.0, 0.0);

    assert_eq!(a, Pair(123.0, 0.0));
    assert!(a != Pair(123.0, 0.1));
    assert!(a != Pair(122.0, 0.0));
    assert!(a != Pair(123.0, f32::NAN));

    assert!(a < Pair(123.0, 0.1));
    assert!(a > Pair(123.0, -0.1));
    assert!(a > Pair(122.0, 0.1));
    assert_eq!(a.partial_cmp(&Pair(123.0, 0.0)), Some(Ordering::Equal));
    assert_eq!(a.partial_cmp(&Pair(123.0, f32::NAN)), None);
}

#[autoimpl(Clone, Debug)]
#[autoimpl(PartialEq, Eq, PartialOrd, Ord, Hash ignore self._f)]
struct MixedComponents {
    i: i32,
    s: &'static str,
    _f: fn() -> i32,
}

#[test]
fn mixed_components() {
    use core::cmp::Ordering;

    let a = MixedComponents {
        i: 31,
        s: "abc",
        _f: || 9,
    };
    assert_eq!(a, a);
    assert_eq!(a.cmp(&a), Ordering::Equal);
    assert_eq!(a.partial_cmp(&a), Some(Ordering::Equal));
    let a_hash = xx_hash_64_0(&a);
    assert_eq!(a_hash, 877288650698020945);

    let b = MixedComponents {
        i: 31,
        s: "abc",
        _f: || 14,
    };
    assert_eq!(a, b); // field f differs but is ignored
    assert_eq!(a.cmp(&b), Ordering::Equal);
    assert_eq!(a.partial_cmp(&b), Some(Ordering::Equal));
    assert_eq!(xx_hash_64_0(&b), a_hash);

    let mut c = a.clone();
    c.i = 2;
    assert!(a != c);
    assert!(a > c);
    assert_eq!(Some(a.cmp(&c)), a.partial_cmp(&c));
    assert!(xx_hash_64_0(&c) != a_hash);

    let mut d = a.clone();
    d.s = "def";
    assert!(a != d);
    assert!(a < d);
    assert_eq!(Some(a.cmp(&d)), a.partial_cmp(&d));
    assert!(xx_hash_64_0(&d) != a_hash);
}

fn xx_hash_64_0(x: impl core::hash::Hash) -> u64 {
    let mut hasher = twox_hash::XxHash64::with_seed(0);
    x.hash(&mut hasher);
    core::hash::Hasher::finish(&hasher)
}

#[allow(unused)]
#[autoimpl(PartialEq, Eq ignore self.bar where S: trait)]
struct Foo<S, T: ?Sized> {
    foo: S,
    bar: u8,
    ptr: *const T,
}
#[test]
fn foo() {
    let x = || 1;
    let ptr = &x as *const _;
    test_has_partial_eq(Foo {
        foo: 1f32,
        bar: 0,
        ptr,
    });
    // Expected to fail:
    // test_has_eq(Foo { foo: 1f32, bar: 0, ptr });
    test_has_eq(Foo {
        foo: 1i32,
        bar: 0,
        ptr,
    });
}
