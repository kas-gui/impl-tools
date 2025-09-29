use impl_tools::autoimpl;
use core::fmt::Debug;

#[autoimpl(for<'a, T> &'a T, &'a mut T, Box<T> where T: trait + ?Sized)]
trait G<V>
where
    V: Debug,
{
    fn g(&self) -> V;

    fn s<X>(&self, f: impl Fn(V) -> X) -> X
    where
        Self: Sized,
    {
        f(self.g())
    }
}

fn main() {
    compile_error!("Warnings verification");
}
