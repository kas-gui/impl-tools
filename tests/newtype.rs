//! Test implementing traits over newtype wrappers

use impl_tools::autoimpl;

#[autoimpl(for<T: trait> &T, &mut T, Box<T>)]
trait Foo {
    fn success(&self) -> bool;
}

struct S;
impl Foo for S {
    fn success(&self) -> bool {
        true
    }
}

#[autoimpl(Deref, DerefMut using self.0)]
struct NewFoo<T: Foo>(T);

#[autoimpl(Deref, DerefMut using self.0)]
struct FooRef<'a>(&'a dyn Foo);

#[autoimpl(Deref, DerefMut using self.0)]
struct FooRefMut<'a>(&'a mut dyn Foo);

#[autoimpl(Deref, DerefMut using self.0)]
struct BoxFoo<T: Foo>(Box<T>);

#[test]
fn foo() {
    assert!(S.success());
    assert!(NewFoo(S).success());
    assert!(FooRef(&S).success());
    assert!(FooRefMut(&mut S).success());
    assert!(BoxFoo(Box::new(S)).success());
}
