//! Test that autoimpl can handle cfg on fields

#![allow(non_snake_case)]

use impl_tools::autoimpl;

#[autoimpl(Clone, Debug, Default where T: trait)]
#[derive(PartialEq, Eq)]
struct S<T> {
    a: T,
    #[cfg(unix)]
    b: String,
    #[cfg(not(unix))]
    b: Box<str>,
    #[cfg(feature = "feature1")]
    c: Vec<i32>,
}

impl<T> S<T> {
    fn new(a: T) -> Self {
        S {
            a,
            b: "hello world".to_string().into(),
            #[cfg(feature = "feature1")]
            c: vec![1, 1, 2, 3, 5, 8],
        }
    }
}

#[test]
fn test_clone_S() {
    let a = S::new(());
    assert_eq!(a.clone(), a);
    assert!(a != S::default());
}

#[test]
fn test_debug_S() {
    let s = S::new(42);
    #[cfg(not(feature = "feature1"))]
    let expected = "S { a: 42, b: \"hello world\" }";
    #[cfg(feature = "feature1")]
    let expected = "S { a: 42, b: \"hello world\", c: [1, 1, 2, 3, 5, 8] }";
    assert_eq!(format!("{s:?}"), expected);
}

#[autoimpl(Clone, Debug where T: trait)]
#[derive(PartialEq, Eq)]
enum E<T> {
    A(T),
    #[cfg(unix)]
    B(String),
    #[cfg(not(unix))]
    B(Box<str>),
    C {
        #[cfg(feature = "feature1")]
        nums: Vec<i32>,
    },
}

#[test]
fn test_clone_E() {
    let a = E::A(2.2);
    assert_eq!(a.clone(), a);

    let b = E::<()>::B("rain".to_string().into());
    assert_eq!(b.clone(), b);

    let c = E::<()>::C {
        #[cfg(feature = "feature1")]
        nums: vec![1, 2, 3],
    };
    assert_eq!(c.clone(), c);
}
