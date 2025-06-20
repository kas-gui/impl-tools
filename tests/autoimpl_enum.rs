//! Test #[autoimpl] for trait implementations

// Test no_std
#![no_std]
extern crate alloc;
use alloc::format;

use impl_tools::{autoimpl, impl_default};

fn test_has_clone(_: impl Clone) {}
fn test_has_copy(_: impl Copy) {}

#[allow(unused)]
#[autoimpl(std::clone::Clone, core::fmt::Debug)]
#[autoimpl(std::cmp::PartialEq, std::cmp::Eq, core::hash::Hash)]
enum Void {}

#[autoimpl(std::marker::Copy, std::clone::Clone, core::fmt::Debug)]
#[autoimpl(PartialEq, Eq, Hash)]
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

#[autoimpl(std::clone::Clone)]
#[allow(unused)]
struct NotCopy(i32);

#[autoimpl(std::clone::Clone)]
#[allow(unused)]
enum CloneNotCopy {
    A { not_copy: NotCopy },
}

#[test]
fn variants() {
    test_has_copy(Variants::A);
    test_has_clone(Variants::A);
    test_has_clone(CloneNotCopy::A {
        not_copy: NotCopy(0),
    });
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
#[impl_default(MyOption::None)]
#[autoimpl(PartialEq, Eq where T: trait)]
enum MyOption<T> {
    None,
    Some(T),
}

#[test]
fn my_option() {
    test_has_clone(MyOption::<i32>::default());
    test_has_copy(MyOption::Some(1));
    assert_eq!(format!("{:?}", MyOption::Some(1)), "MyOption::Some(1)");
}
