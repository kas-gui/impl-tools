#![allow(unused)]
// Test no_std
#![no_std]
extern crate alloc;
use alloc::string::{String, ToString};

use impl_tools::{impl_default, impl_scope};

#[impl_default(LR::Left(0))]
#[derive(Debug, PartialEq)]
enum LR {
    Left(i32),
    Right(i32),
}

#[test]
fn lr() {
    assert_eq!(LR::default(), LR::Left(0));
}

#[impl_default(Single::A(Default::default()) where T: Default)]
enum Single<T> {
    A(T),
}

impl_scope! {
    #[impl_default(UnsafeNumber { as_u64: 0 })]
    union UnsafeNumber {
        as_u64: u64,
        as_f64: f64,
    }
}

#[test]
fn unsafe_number() {
    let un = UnsafeNumber::default();
    unsafe {
        let UnsafeNumber { as_f64: v } = un;
        assert_eq!(v, 0.0);
    }
}

impl_scope! {
    #[impl_default(where T: trait)]
    struct Person<T> {
        name: String = "Jane Doe".to_string(),
        age: u32 = 72,
        occupation: T,
    }
}

#[test]
fn person() {
    let person = Person::<String>::default();
    assert_eq!(person.name, "Jane Doe");
    assert_eq!(person.age, 72);
    assert_eq!(person.occupation, "");
}
