//! Test #[autoimpl] for trait references

// Test no_std
#![no_std]
extern crate alloc;
use alloc::boxed::Box;

use core::fmt::Debug;
use core::ops::Deref;
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

#[autoimpl(for<'a, T> &'a mut T where T: trait + ?Sized)]
pub trait BitRead2 {
    fn copy_to(&mut self, mut _n: u64) -> Result<(), core::convert::Infallible> {
        Ok(())
    }
}

#[autoimpl(for<'a, T> &'a T, &'a mut T, Box<T> where T: trait + ?Sized)]
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

#[allow(unused)]
#[autoimpl(for<T: trait + ?Sized> &T)]
trait H<X, Y: G<X>>
where
    X: Debug,
{
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

#[test]
fn custom_deref_target() {
    #[autoimpl(Deref<Target = T>, DerefMut using self.0)]
    struct BoxingWrapper<T: ?Sized>(Box<T>);

    #[autoimpl(for<T: trait + ?Sized> BoxingWrapper<T>)]
    trait Increment {
        fn increment(&mut self);
    }

    impl Increment for i32 {
        fn increment(&mut self) {
            *self += 1;
        }
    }

    let mut x = BoxingWrapper(Box::new(0));
    x.increment();
    assert_eq!(*x.0, 1);

    let mut y = 10;
    y.increment();
    let mut z = BoxingWrapper(Box::new(&mut y as &mut dyn Increment));
    z.increment();
    assert_eq!(y, 12);
}

#[allow(unused)]
#[autoimpl(for<T: trait + ?Sized> &T)]
trait Cfgs {
    #[cfg(test)]
    fn included(&self);

    #[cfg(mock_feature)]
    fn excluded(&self);
}

#[allow(unused)]
#[autoimpl(for<T: trait, U: Deref<Target = T> + Debug + ?Sized> U)]
trait SharedData<Key>: Debug {
    type Item;
    fn get(&self, key: &Key) -> Option<Self::Item>;
}
