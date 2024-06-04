impl_tools::impl_scope! {
    #[derive(Clone, Debug)]
    struct A<T: Clone + Debug>(T);

    impl () {
        fn new(t: T) -> A {
            A(t)
        }
    }
}

fn main() {}
