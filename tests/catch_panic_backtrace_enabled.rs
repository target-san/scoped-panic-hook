use scoped_panic_hook::catch_panic;
use std::backtrace::BacktraceStatus;
use std::env;

#[test]
fn catch_panic_backtrace_enabled() {
    unsafe {
        env::set_var("RUST_BACKTRACE", "1");
    }

    let panic = catch_panic(|| panic!("Oops!")).unwrap_err();
    assert_eq!(panic.backtrace().status(), BacktraceStatus::Captured);
}
