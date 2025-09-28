#[test]
fn compile_errors() {
    let t = trybuild::TestCases::new();
    t.compile_fail("compile-errors/common/*.rs");

    #[cfg(feature = "nightly-diagnostics")]
    t.compile_fail("compile-errors/nightly/*.rs");

    #[cfg(not(feature = "nightly-diagnostics"))]
    t.compile_fail("compile-errors/stable/*.rs");
}
