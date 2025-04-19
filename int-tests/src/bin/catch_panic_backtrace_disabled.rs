int_tests::bin_test! {
    test {
        use scoped_panic_hook::panic::{CaptureBacktrace, CatchPanicConfig, catch_panic_with_config};
        use std::backtrace::BacktraceStatus;
        use std::env;

        unsafe {
            env::set_var("RUST_BACKTRACE", "0");
            env::set_var("RUST_LIB_BACKTRACE", "0");
        }

        let panic = catch_panic_with_config(
            CatchPanicConfig {
                capture_backtrace: CaptureBacktrace::Yes,
            },
            || panic!("Oops!"),
        )
        .unwrap_err();
        assert_eq!(panic.backtrace().status(), BacktraceStatus::Disabled);

        let panic = catch_panic_with_config(
            CatchPanicConfig {
                capture_backtrace: CaptureBacktrace::Force,
            },
            || panic!("Oops!"),
        )
        .unwrap_err();
        assert_eq!(panic.backtrace().status(), BacktraceStatus::Captured);
    }
}
