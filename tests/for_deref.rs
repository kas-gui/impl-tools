//! Test #[autoimpl] for trait references

// Test no_std
#![no_std]
extern crate alloc;
use alloc::boxed::Box;

use core::fmt::Debug;
use impl_tools::autoimpl;

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

    fn s<X>(&self, f: impl Fn(V) -> X) -> X
    where
        Self: Sized,
    {
        f(self.g())
    }
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
        assert!(g.s(|x| x == 123));
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

#[cfg(rustc_1_65)]
#[autoimpl(for<A: trait + ?Sized> Box<A>)]
trait Gat {
    type T<X>;

    type R<'a, X>: core::ops::Deref<Target = Self::T<X>>
    where
        X: 'a;
}

#[cfg(rustc_1_65)]
#[test]
fn gat() {
    struct S;
    impl Gat for S {
        type T<X> = X;

        // Our MSRV doesn't support the preferred location!
        #[allow(deprecated_where_clause_location)]
        type R<'a, X>
        where
            X: 'a,
        = &'a X;
    }

    fn impls_gat(_: impl Gat) {}

    impls_gat(S);
    impls_gat(Box::new(S));
}
