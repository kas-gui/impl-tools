#![allow(unused)]

#[impl_tools::autoimpl(Copy)]
union U {
    signed: i32,
    unsigned: u32,
}

fn main() {}
