//! Test #[autoimpl] for trait implementations

// Test no_std
#![no_std]
extern crate alloc;
use alloc::format;

use impl_tools::autoimpl;

fn test_has_clone(_: impl Clone) {}
fn test_has_copy(_: impl Copy) {}

#[autoimpl(std::clone::Clone, core::fmt::Debug)]
enum Void {}

#[autoimpl(std::marker::Copy, std::clone::Clone, core::fmt::Debug)]
enum Variants {
    A,
    B(()),
    C(&'static str, i32),
    #[allow(unused)]
    D {
        x: i32,
        y: i32,
    },
}

#[test]
fn variants() {
    test_has_copy(Variants::A);
    test_has_clone(Variants::A);
    assert_eq!(format!("{:?}", Variants::A), "Variants::A");
    assert_eq!(format!("{:?}", Variants::B(())), "Variants::B(())");
    assert_eq!(
        format!("{:?}", Variants::C("abc", 3)),
        "Variants::C(\"abc\", 3)"
    );
    assert_eq!(
        format!("{:?}", Variants::D { x: 3, y: 2 }),
        "Variants::D { x: 3, y: 2 }"
    );
}

#[autoimpl(Copy, Clone, Debug where T: trait)]
enum MyOption<T> {
    None,
    Some(T),
}

#[test]
fn my_option() {
    test_has_clone(MyOption::<i32>::None);
    test_has_copy(MyOption::Some(1));
    assert_eq!(format!("{:?}", MyOption::Some(1)), "MyOption::Some(1)");
}
