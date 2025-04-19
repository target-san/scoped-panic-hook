use std::cell::Cell;
use std::mem;
use std::panic::{PanicHookInfo, UnwindSafe, catch_unwind};
use std::sync::Once;
use std::thread;

/// Specifies whether to stop at currently executed scoped hook
/// or continue with other options
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NextHook {
    /// Don't call anything after hook which returned this value
    Break,
    /// Call hook which precedes scoped hooks; usually that's default hook
    PrevInstalledHook,
}
/// Initializes scoped hook infrastructure
///
/// Usually you don't need to call it explicitly, yet it's left accessible
/// in case crate user wants to perform initialization at well-defined point
pub fn init_scoped_hooks() {
    INIT_SCOPED_HOOKS.call_once(install_hook);
}

#[cfg(nightly)]
fn install_hook() {
    std::panic::update_hook(|prev, info| {
        if scoped_hook_fn(info) {
            prev(info);
        }
    });
}

#[cfg(not(nightly))]
fn install_hook() {
    let old_handler = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        if scoped_hook_fn(info) {
            old_handler(info);
        }
    }));
}
/// Executes `body` function wrapped in `catch_unwind` with specified scoped panic hook installed
///
/// # Parameters
/// * `hook` - panic hook function which receives panic info during `body`'s execution
/// * `body` - Actual payload closure.
///   The closure has same requirements as the parameter to [`std::panic::catch_unwind`].
///   In particular, if you want to assert that closure is unwind safe when type system
///   can't deduce it, you can use [`std::panic::AssertUnwindSafe`].
///   See [`std::panic::catch_unwind`] for details.
///
/// # Returns
/// Usual `catch_unwind` result with `Ok(...)` being return value from `body`
pub fn catch_unwind_with_scoped_hook<R>(
    mut hook: impl FnMut(&PanicHookInfo<'_>) -> NextHook,
    body: impl FnOnce() -> R + UnwindSafe,
) -> thread::Result<R> {
    init_scoped_hooks();

    let new_info = HookInfo::from_hook(&mut hook);
    let old_info = CURRENT_SCOPED_HOOK.replace(Some(new_info));
    let _restore = defer(|| CURRENT_SCOPED_HOOK.set(old_info));

    catch_unwind(body)
}

thread_local! {
    static CURRENT_SCOPED_HOOK: Cell<Option<HookInfo>> = const { Cell::new(None) };
}

static INIT_SCOPED_HOOKS: Once = Once::new();

fn scoped_hook_fn(info: &PanicHookInfo<'_>) -> bool {
    CURRENT_SCOPED_HOOK
        .get()
        .is_none_or(|hook| unsafe { hook.call_handler(info) } == NextHook::PrevInstalledHook)
}

type DynHookPtr<'a> = *mut (dyn FnMut(&PanicHookInfo<'_>) -> NextHook + 'a);
type StaticHookPtr = DynHookPtr<'static>;

#[derive(Copy, Clone)]
struct HookInfo {
    hook: StaticHookPtr,
}

impl HookInfo {
    fn from_hook(hook: &mut impl FnMut(&PanicHookInfo<'_>) -> NextHook) -> Self {
        Self {
            // transmute is required to erase lifetimes from actual closure type
            hook: unsafe {
                mem::transmute::<DynHookPtr<'_>, StaticHookPtr>(hook as DynHookPtr<'_>)
            },
        }
    }

    unsafe fn call_handler(&self, info: &PanicHookInfo<'_>) -> NextHook {
        unsafe { (*self.hook)(info) }
    }
}
/// Copy of `defer` from `defer` crate, to not introduce dependency
fn defer<F: FnOnce()>(f: F) -> impl Drop {
    struct Defer<F: FnOnce()>(mem::ManuallyDrop<F>);

    impl<F: FnOnce()> Drop for Defer<F> {
        fn drop(&mut self) {
            let f: F = unsafe { mem::ManuallyDrop::take(&mut self.0) };
            f();
        }
    }

    Defer(mem::ManuallyDrop::new(f))
}

#[cfg(test)]
mod tests {
    use crate::subprocess_test;

    use super::*;

    #[test]
    fn simple_ok() {
        let mut counter = 0;

        catch_unwind_with_scoped_hook(
            |_| {
                counter += 1;
                NextHook::Break
            },
            || (),
        )
        .unwrap();

        assert_eq!(counter, 0);
    }

    #[test]
    fn simple_panic() {
        let mut counter = 0;

        catch_unwind_with_scoped_hook(
            |_| {
                counter += 1;
                NextHook::Break
            },
            || panic!("Oops!"),
        )
        .unwrap_err();

        assert_eq!(counter, 1);
    }

    subprocess_test! {
        #[test]
        fn hook_forwarding() {
            use crate::hook::{NextHook, catch_unwind_with_scoped_hook};
            use std::panic::{PanicHookInfo, set_hook};
            use std::sync::atomic::{AtomicUsize, Ordering};
    
            static GLOB_COUNTER: AtomicUsize = AtomicUsize::new(0);
    
            fn counter_hook(_info: &PanicHookInfo<'_>) {
                GLOB_COUNTER.fetch_add(1, Ordering::Relaxed);
            }
            // If we return [`NextHook::PrevInstalledHook`] from hook,
            // previous installed hook should be actually invoked
            //
            // Must be in separate binary because it overrides default hook
            // prior to scoped one takes effect
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
    }
}
