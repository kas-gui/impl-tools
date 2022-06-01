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
