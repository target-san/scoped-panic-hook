use std::panic::{PanicHookInfo, set_hook};
use std::sync::atomic::{AtomicUsize, Ordering};

use scoped_panic_hook::hook::{NextHook, catch_unwind_with_scoped_hook};

static GLOB_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn counter_hook(_info: &PanicHookInfo<'_>) {
    GLOB_COUNTER.fetch_add(1, Ordering::Relaxed);
}
/// If we return [`NextHook::PrevInstalledHook`] from hook,
/// previus installed hook should be actually invoked
/// 
/// Must be in separate binary because it overrides default hook
/// prior to scoped one takes effect
#[test]
fn hook_forwarding() {
    set_hook(Box::new(counter_hook));

    let mut counter = 0;

    let result = catch_unwind_with_scoped_hook(
        |_| {
            counter += 1;
            NextHook::PrevInstalledHook
        },
        || panic!("Oops!"),
    );
    assert!(result.is_err());

    assert_eq!(counter, 1);
    assert_eq!(GLOB_COUNTER.load(Ordering::Relaxed), 1);
}
