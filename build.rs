extern crate autocfg;

fn main() {
    let ac = autocfg::new();
    ac.emit_rustc_version(1, 65);

    autocfg::rerun_path("build.rs");
}
