// Test singleton!

use core::fmt::Debug;
use impl_tools::singleton;

#[test]
fn a() {
    let a = singleton! {
        #[derive(Clone, Debug)]
        struct<T: Clone + Debug>(T = "abc");

        impl Self {
            fn format(&self) -> String {
                format!("Anon({:?})", self.0)
            }
        }
    };

    debug_assert_eq!(a.format(), "Anon(\"abc\")");
}

#[test]
fn b() {
    let b = singleton! {
        #[derive(Clone, Debug)]
        struct<T: Clone + Debug> {
            t: T = 123,
        }

        impl Self {
            fn format(&self) -> String {
                format!("Anon {{ t: {:?} }}", self.t)
            }
        }
    };
    debug_assert_eq!(b.format(), "Anon { t: 123 }");
}
