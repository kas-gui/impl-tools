use impl_tools::autoimpl;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::DerefMut;

fn test_has_clone(_: impl Clone) {}

#[autoimpl(Clone, Debug)]
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
    assert_eq!(format!("{x:?}"), "X { a: 1, b: \"abc\", .. }");
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

#[autoimpl(for<'a, T: trait> &'a mut T, Box<T>)]
trait Z {
    const A: i32;

    fn f(&self);
    fn g(&mut self, a: i32, b: &Self::B);

    type B;
}

impl Z for () {
    const A: i32 = 10;

    fn f(&self) {}
    fn g(&mut self, _: i32, _: &i32) {}

    type B = i32;
}

#[test]
fn z() {
    fn impls_z(mut z: impl Z<B = i32>) {
        z.f();
        z.g(1, &2);
    }

    impls_z(());
    impls_z(&mut ());
    impls_z(Box::new(()));
}

#[autoimpl(for<'a, V, T> &'a T, &'a mut T, Box<T> where T: trait + ?Sized)]
trait G<V>
where
    V: Debug,
{
    fn g(&self) -> V;
}

#[test]
fn g() {
    struct S;
    impl G<i32> for S {
        fn g(&self) -> i32 {
            123
        }
    }

    fn impls_g(g: impl G<i32>) {
        assert_eq!(g.g(), 123);
    }

    impls_g(S);
    impls_g(&S);
    impls_g(&&S);
    impls_g(&mut S);
    impls_g(&&mut S);
    impls_g(&S as &dyn G<i32>);
    impls_g(Box::new(S));
    impls_g(&mut &Box::new(S));
    impls_g(Box::new(S) as Box<dyn G<i32>>);
    impls_g(&mut (Box::new(S) as Box<dyn G<i32>>));
}
