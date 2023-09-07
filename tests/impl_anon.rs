// Test impl_anon!

use core::fmt::Debug;
use impl_tools::impl_anon;

#[test]
fn a() {
    let a = impl_anon! {
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
    let b = impl_anon! {
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
