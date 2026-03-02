//! Test impl_for attribute
use impl_tools::split_impl;

struct Object<T> {
    name: String,
    mass: T,
}

#[split_impl(for<T: Copy + Into<f64>> Object<T>)]
trait Interface {
    /// Get the name
    #[inline]
    fn name(&self) -> &str {
        &self.name
    }

    /// Get the weight (Lb)
    fn weight(&self) -> f64 {
        let m: f64 = self.mass.into();
        m * 2.2046244201837775
    }
}

#[test]
fn test_stone() {
    let obj = Object {
        name: "stone".to_string(),
        mass: 6.350288,
    };

    let iface: &dyn Interface = &obj;
    assert_eq!(iface.name(), "stone");
    assert_eq!(iface.weight(), 14.0);
}

struct Brick;
impl Interface for Brick {
    #[inline]
    fn name(&self) -> &str {
        "brick"
    }

    #[inline]
    fn weight(&self) -> f64 {
        5.0
    }
}

#[test]
fn test_brick() {
    let iface: &dyn Interface = &Brick;
    assert_eq!(iface.name(), "brick");
    assert_eq!(iface.weight(), 5.0);
}
