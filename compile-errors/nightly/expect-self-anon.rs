fn main() {
    let _ = impl_tools::impl_anon! {
        #[derive(Clone, Debug)]
        struct (i32 = 123);

        impl () {
            fn get(&self) -> i32 {
                self.0
            }
        }
    };
}
