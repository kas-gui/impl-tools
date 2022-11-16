//! Test implementing traits over newtype wrappers

use impl_tools::autoimpl;
use std::rc::Rc;
use std::sync::Arc;

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

#[test]
fn direct() {
    assert!(S.success());
}

#[test]
fn new_foo() {
    #[autoimpl(Deref, DerefMut using self.0)]
    struct NewType<T: Foo>(T);

    assert!(NewType(S).success());
}

#[test]
fn new_foo_ref() {
    #[autoimpl(Deref, DerefMut using self.0)]
    struct NewType<'a>(&'a dyn Foo);

    assert!(NewType(&S).success());
}

#[test]
fn new_foo_ref_mut() {
    #[autoimpl(Deref, DerefMut using self.0)]
    struct NewType<'a>(&'a mut dyn Foo);

    assert!(NewType(&mut S).success());
}

#[test]
fn new_foo_box() {
    #[autoimpl(Deref, DerefMut using self.0)]
    struct NewType<T: Foo>(Box<T>);

    assert!(NewType(Box::new(S)).success());
}

#[test]
fn new_foo_box_dyn() {
    #[autoimpl(Deref, DerefMut using self.0)]
    struct NewType(Box<dyn Foo>);

    assert!(NewType(Box::new(S)).success());
}

#[test]
fn new_foo_rc_dyn() {
    #[autoimpl(Deref, DerefMut using self.0)]
    struct NewType(Rc<dyn Foo>);

    assert!(NewType(Rc::new(S)).success());
}

#[test]
fn new_foo_arc_dyn() {
    #[autoimpl(Deref, DerefMut using self.0)]
    struct NewType(Arc<dyn Foo + Send + Sync>);

    assert!(NewType(Arc::new(S)).success());
}
