//! Test implementing traits over newtype wrappers

use std::rc::Rc;
use std::sync::Arc;

mod inner {
    use super::*;
    use impl_tools::autoimpl;

    #[autoimpl(for<T: trait + ?Sized> &T, &mut T, Box<T>, Rc<T>, Arc<T>)]
    // Optionally, we can also implement Foo directly for these types:
    // #[autoimpl(for<T: trait> NewFoo<T>, BoxFoo<T>)]
    pub trait Foo {
        fn is_true(&self) -> bool;
    }

    #[autoimpl(Deref<Target = T>, DerefMut using self.0)]
    pub struct NewFoo<T: Foo>(T);
    impl<T: Foo> NewFoo<T> {
        pub fn new(foo: T) -> Self {
            NewFoo(foo)
        }
    }

    #[autoimpl(Deref<Target = dyn Foo + 'a> using self.0)]
    pub struct FooRef<'a>(&'a dyn Foo);
    impl<'a> FooRef<'a> {
        pub fn new(foo: &'a dyn Foo) -> Self {
            FooRef(foo)
        }
    }

    #[autoimpl(Deref<Target = dyn Foo + 'a>, DerefMut using self.0)]
    pub struct FooRefMut<'a>(&'a mut dyn Foo);
    impl<'a> FooRefMut<'a> {
        pub fn new(foo: &'a mut dyn Foo) -> Self {
            FooRefMut(foo)
        }
    }

    #[autoimpl(Deref<Target = T>, DerefMut using self.0)]
    pub struct BoxFoo<T: Foo>(Box<T>);
    impl<T: Foo> BoxFoo<T> {
        pub fn new(foo: Box<T>) -> Self {
            BoxFoo(foo)
        }
    }

    #[autoimpl(Deref<Target = dyn Foo>, DerefMut using self.0)]
    pub struct BoxDynFoo(Box<dyn Foo>);
    impl BoxDynFoo {
        pub fn new(foo: Box<dyn Foo>) -> Self {
            BoxDynFoo(foo)
        }
    }

    #[autoimpl(Deref<Target = dyn Foo>, DerefMut using self.0)]
    pub struct RcDynFoo(Rc<dyn Foo>);
    impl RcDynFoo {
        pub fn new(foo: Rc<dyn Foo>) -> Self {
            RcDynFoo(foo)
        }
    }

    #[autoimpl(Deref<Target = dyn Foo>, DerefMut using self.0)]
    pub struct ArcDynFoo(Arc<dyn Foo>);
    impl ArcDynFoo {
        pub fn new(foo: Arc<dyn Foo>) -> Self {
            ArcDynFoo(foo)
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct V(bool);
impl inner::Foo for V {
    fn is_true(&self) -> bool {
        self.0
    }
}

#[test]
fn newtype() {
    use inner::*;

    let mut v = V(true);

    assert!(v.is_true());
    assert!(NewFoo::new(v).is_true());
    assert!(FooRef::new(&v).is_true());
    assert!(FooRefMut::new(&mut v).is_true());
    assert!(BoxFoo::new(Box::new(v)).is_true());
    assert!(BoxDynFoo::new(Box::new(v)).is_true());
    assert!(RcDynFoo::new(Rc::new(v)).is_true());
    assert!(ArcDynFoo::new(Arc::new(v)).is_true());
}
