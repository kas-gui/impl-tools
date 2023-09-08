// Test impl_scope!

use core::fmt::Debug;
use impl_tools::impl_scope;

impl_scope! {
    #[derive(Clone, Debug)]
    struct A<T: Clone + Debug>(T);

    impl Self {
        fn new(t: T) -> Self {
            A(t)
        }
    }
}

#[test]
fn a() {
    let a = A::new("abc");
    debug_assert_eq!(format!("{a:?}"), "A(\"abc\")");
}

impl_scope! {
    #[allow(unused)]
    #[derive(Clone, Debug)]
    struct B<T: Clone + Debug> {
        t: T,
    }

    impl Self {
        fn new(t: T) -> Self {
            B { t }
        }
    }
}

#[test]
fn b() {
    let b = B::new(123);
    debug_assert_eq!(format!("{b:?}"), "B { t: 123 }");
}
