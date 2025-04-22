use crate::hook::{NextHook, catch_unwind_with_scoped_hook};
use std::any::Any;
use std::backtrace::Backtrace;
use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display};
use std::io::Write;
use std::panic::{Location, PanicHookInfo, UnwindSafe};
/// Panic info gathered as one record
///
/// Contains panic's location, message or raw payload, and backtrace, if collected
#[derive(Debug)]
pub struct Panic {
    location: Option<OwnedLocation>,
    payload: Result<Message, RawPayload>,
    backtrace: Backtrace,
}
/// Backtrace display style
///
/// There's [`std::panic::BacktraceStyle`], but it's still experimental
#[derive(Copy, Clone, Debug)]
pub enum BacktraceStyle {
    Off,
    Short,
    Full,
}

type Message = Cow<'static, str>;

type RawPayload = Box<dyn Any + Send + 'static>;

const UNKNOWN_PANIC: &str = "<unknown panic>";

impl Panic {
    /// Panic location, if original [`std::panic::PanicHookInfo`] provided one
    pub fn location(&self) -> Option<&OwnedLocation> {
        self.location.as_ref()
    }
    /// Original panic message which was supplied to [`std::panic!`] macro
    ///
    /// If panic was raised using [`std::panic::panic_any`] with non-string payload,
    /// this function returns substitute message
    pub fn message(&self) -> &str {
        self.payload.as_ref().map_or(UNKNOWN_PANIC, |m| m.as_ref())
    }
    /// Raw panic payload if it wasn't recognized as string-like message and converted to it
    pub fn raw_payload(&self) -> Option<&RawPayload> {
        self.payload.as_ref().err()
    }
    /// Panic's backtrace gathered from within panic hook
    pub fn backtrace(&self) -> &Backtrace {
        &self.backtrace
    }
    /// Transforms panic info into raw panic payload.
    /// If it's a string-like message, it gets re-wrapped again.
    ///
    /// Useful when you need something like rethrowing panic
    pub fn into_raw_payload(self) -> RawPayload {
        match self.payload {
            Err(raw_payload) => raw_payload,
            Ok(Cow::Borrowed(str)) => Box::new(str) as RawPayload,
            Ok(Cow::Owned(str)) => Box::new(str) as RawPayload,
        }
    }
    /// Produces [`std::fmt::Display`]'able object
    /// which displays panic's message, location and short backtrace, if captured
    pub fn display_with_backtrace(&self) -> impl Display + '_ {
        self.display_with_backtrace_style(BacktraceStyle::Short)
    }

    /// Produces [`std::fmt::Display`]'able object
    /// which displays panic's message, location and backtrace, if captured, with specified style
    pub fn display_with_backtrace_style(&self, style: BacktraceStyle) -> impl Display + '_ {
        format_from_fn(move |f| self.fmt_panic(style, f))
    }

    fn fmt_panic(
        &self,
        backtrace_style: BacktraceStyle,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        if let Some(loc) = self.location() {
            write!(f, "Panic \"{}\" at {}", self.message(), loc)?;
        } else {
            write!(f, "Panic \"{}\"", self.message())?;
        }
        // NB: backtrace print style selection via `#` isn't well documented
        match backtrace_style {
            BacktraceStyle::Off => Ok(()),
            BacktraceStyle::Short => writeln!(f, "\nBacktrace: {}", self.backtrace),
            BacktraceStyle::Full => writeln!(f, "\nBacktrace: {:#}", self.backtrace),
        }
    }
}

impl Display for Panic {
    /// Displays panic's message and location
    ///
    /// To display full panic info including backtrace, use [`Panic::display_with_backtrace`]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_panic(BacktraceStyle::Off, f)
    }
}

impl Error for Panic {}
/// Determines how to capture backtrace when catching panic
#[derive(Copy, Clone, Debug, Default)]
pub enum CaptureBacktrace {
    /// Use [`std::backtrace::Backtrace::disabled`]
    No,
    /// Use [`std::backtrace::Backtrace::capture`]
    #[default]
    Yes,
    /// Use [`std::backtrace::Backtrace::force_capture`]
    Force,
}
/// Options for [`catch_panic_with_config`]
#[derive(Copy, Clone, Debug, Default)]
pub struct CatchPanicConfig {
    /// Whether backtrace should be captured
    pub capture_backtrace: CaptureBacktrace,
}
/// Runs provided closure and captures panic if one happens
///
/// # Limitations
///
/// Stable API doesn't allow to detect from panic hook that panic being handled will unwind.
/// So if you get unwindable panic, panic info will be swallowed. This will be fixed when
/// [#92988](https://github.com/rust-lang/rust/issues/92988) stabilizes
///
/// # Parameters
/// * `config` - options used when capturing panic info
/// * `body` - closure which should be run with capturing panics.
///   The closure has same requirements as the parameter to [`std::panic::catch_unwind`].
///   In particular, if you want to assert that closure is unwind safe when type system
///   can't deduce it, you can use [`std::panic::AssertUnwindSafe`].
///   See [`std::panic::catch_unwind`] for details.
///
/// # Returns
/// * `Ok(result)` - with `body1`'s return value if it completes without panic
/// * `Err(panic)` - if `body` panics in process, with `Panic` as error value
pub fn catch_panic_with_config<R>(
    config: CatchPanicConfig,
    body: impl FnOnce() -> R + UnwindSafe,
) -> Result<R, Panic> {
    let mut panic = None;

    match catch_unwind_with_scoped_hook(catch_panic_hook(config, &mut panic), body) {
        Ok(ok) => {
            debug_assert!(panic.is_none());
            Ok(ok)
        }
        Err(raw_payload) => {
            let panic = panic.expect("Panic info wasn't recorded");
            Err(Panic {
                payload: transform_payload(raw_payload),
                ..panic
            })
        }
    }
}

fn transform_payload(payload: RawPayload) -> Result<Message, RawPayload> {
    let payload = match payload.downcast::<&str>() {
        Ok(str) => return Ok((*str).into()),
        Err(p) => p,
    };

    let payload = match payload.downcast::<String>() {
        Ok(str) => return Ok((*str).into()),
        Err(p) => p,
    };

    Err(payload)
}
/// Constructs intermediate panic, some of its data will be reused
fn panic_from_hook_info(info: &PanicHookInfo<'_>, config: &CatchPanicConfig) -> Panic {
    let message = if let Some(str) = info.payload().downcast_ref::<&str>() {
        (*str).into()
    } else if let Some(str) = info.payload().downcast_ref::<String>() {
        str.clone().into()
    } else {
        UNKNOWN_PANIC.into()
    };

    Panic {
        location: info.location().map(Into::into),
        payload: Ok(message),
        backtrace: match config.capture_backtrace {
            CaptureBacktrace::No => Backtrace::disabled(),
            CaptureBacktrace::Yes => Backtrace::capture(),
            CaptureBacktrace::Force => Backtrace::force_capture(),
        },
    }
}

#[cfg(nightly)]
fn in_hook_backtrace_style() -> BacktraceStyle {
    match std::panic::get_backtrace_style() {
        None | Some(std::panic::BacktraceStyle::Off) => BacktraceStyle::Off,
        Some(std::panic::BacktraceStyle::Full) => BacktraceStyle::Full,
        // std enum is non-exhaustive
        Some(_) => BacktraceStyle::Short,
    }
}

#[cfg(not(nightly))]
fn in_hook_backtrace_style() -> BacktraceStyle {
    BacktraceStyle::Short
}

fn print_panic_in_hook(panic: &Panic) {
    use std::fmt::Write;

    let thread = std::thread::current();
    let name = thread.name().unwrap_or("<unnamed>");
    let mut msg = if let Some(loc) = panic.location() {
        format!(
            "\nthread '{name}' panicked at {loc}:\n{}",
            panic.message()
        )
    } else {
        format!(
            "\nthread '{name}' panicked:\n{}",
            panic.message()
        )
    };
    let _ = match in_hook_backtrace_style() {
        BacktraceStyle::Off => Ok(()),
        BacktraceStyle::Short => write!(msg, "\nstack backtrace:\n{}", panic.backtrace()),
        BacktraceStyle::Full => write!(msg, "\nstack backtrace:\n{:#}", panic.backtrace()),
    };
    let _ = std::io::stderr().lock().write_all(msg.as_bytes());
}

#[cfg(nightly)]
fn catch_panic_hook(
    config: CatchPanicConfig,
    panic: &mut Option<Panic>,
) -> impl FnMut(&PanicHookInfo<'_>) -> NextHook + '_ {
    move |info| {
        if !info.can_unwind() {
            // No-unwind, we don't expect to run this hook ever again,
            // so we print collected panic info, if any
            if let Some(panic) = panic {
                print_panic_in_hook(&*panic);
            }
            // Allow default or similar hook to display all the details
            return NextHook::PrevInstalledHook;
        }
        // We shouldn't ever fail this assert, but just in case
        assert!(panic.is_none(), "Panic was filled prior to unwind!");

        *panic = Some(panic_from_hook_info(info, &config));
        NextHook::Break
    }
}

#[cfg(not(nightly))]
fn catch_panic_hook(
    config: CatchPanicConfig,
    panic: &mut Option<Panic>,
) -> impl FnMut(&PanicHookInfo<'_>) -> NextHook + '_ {
    move |info| {
        // FIXME: check `info.can_unwind()` when https://github.com/rust-lang/rust/issues/92988 stabilizes;
        // otherwise we won't handle properly panics which initially don't unwind
        if let Some(panic) = panic {
            // In this case we assume we get non-unwinding panic in process of unwind.
            // As a result, we forward handling to previously installed hook,
            // so we'll see it described before program abort
            print_panic_in_hook(&*panic);
            NextHook::PrevInstalledHook
        } else {
            *panic = Some(panic_from_hook_info(info, &config));
            NextHook::Break
        }
    }
}
/// Runs provided closure and captures panic if one happens, along with its backtrace
///
/// # Limitations
///
/// Stable API doesn't allow to detect from panic hook that panic being handled will unwind.
/// So if you get unwindable panic, panic info will be swallowed. This will be fixed when
/// [#92988](https://github.com/rust-lang/rust/issues/92988) stabilizes.
/// Correct API is used on nightly builds, which resolves issue.
///
/// # Parameters
/// * `body` - closure which should be run with capturing panics.
///   The closure has same requirements as the parameter to [`std::panic::catch_unwind`].
///   In particular, if you want to assert that closure is unwind safe when type system
///   can't deduce it, you can use [`std::panic::AssertUnwindSafe`].
///   See [`std::panic::catch_unwind`] for details.
///
/// # Returns
/// * `Ok(result)` - with `body1`'s return value if it completes without panic
/// * `Err(panic)` - if `body` panics in process, with `Panic` as error value
pub fn catch_panic<R>(body: impl FnOnce() -> R + UnwindSafe) -> Result<R, Panic> {
    catch_panic_with_config(
        CatchPanicConfig {
            capture_backtrace: CaptureBacktrace::Yes,
        },
        body,
    )
}
/// Simple structure which mirrors [`std::panic::Location`],
/// although owns file name and is freely movable around
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OwnedLocation {
    file: String,
    line: u32,
    column: u32,
}

impl OwnedLocation {
    pub fn file(&self) -> &str {
        &self.file
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> u32 {
        self.column
    }
}

impl From<&'_ Location<'_>> for OwnedLocation {
    fn from(value: &Location<'_>) -> Self {
        Self {
            file: value.file().to_owned(),
            line: value.line(),
            column: value.column(),
        }
    }
}

impl Display for OwnedLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}
/// Creates type whose `Display` and `Debug` implementations
/// are provided with function `format_fn`
///
/// Replica of experimental `std::fmt::from_fn`
fn format_from_fn<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result>(
    format_fn: F,
) -> impl fmt::Debug + fmt::Display {
    struct FormatFromFn<F> {
        format_fn: F,
    }

    impl<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Debug for FormatFromFn<F> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            (self.format_fn)(f)
        }
    }

    impl<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Display for FormatFromFn<F> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            (self.format_fn)(f)
        }
    }

    FormatFromFn { format_fn }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::backtrace::BacktraceStatus;
    use std::env;
    use std::panic::panic_any;
    use std::path::Path;
    use subprocess_test::subprocess_test;

    #[test]
    fn simple_catch_panic() {
        let line = line!();
        let panic = catch_panic(|| panic!("Oops!")).unwrap_err();
        // NB: we test backtrace status separately
        assert!(panic.raw_payload().is_none());
        assert_eq!(panic.message(), "Oops!");

        let loc = panic.location().unwrap();
        assert!(Path::new(loc.file()).ends_with("panic.rs"));
        assert_eq!(loc.line(), line + 1);
        assert_eq!(loc.column(), 36);

        let payload = panic.into_raw_payload();
        assert_eq!(payload.downcast_ref::<&str>(), Some(&"Oops!"));
    }

    #[test]
    fn catch_panic_any() {
        let panic = catch_panic(|| panic_any("Oops!")).unwrap_err();
        assert_eq!(panic.message(), "Oops!");
        assert!(panic.raw_payload().is_none());

        let panic = catch_panic(|| panic_any(42usize)).unwrap_err();
        assert_eq!(panic.message(), "<unknown panic>");
        assert_eq!(
            panic.raw_payload().and_then(|p| p.downcast_ref::<usize>()),
            Some(&42usize)
        );
    }

    #[test]
    fn catch_panic_no_backtrace() {
        let panic = catch_panic_with_config(
            CatchPanicConfig {
                capture_backtrace: CaptureBacktrace::No,
            },
            || panic!("Oops!"),
        )
        .unwrap_err();
        assert_eq!(panic.backtrace().status(), BacktraceStatus::Disabled);
    }

    struct JustDie;

    impl Drop for JustDie {
        fn drop(&mut self) {
            panic!("Die!");
        }
    }

    fn find_panic_message<'a>(string: &'a str, module_path: &str, test_name: &str) -> (Option<usize>, &'a str) {
        todo!()
    }

    subprocess_test! {
        #[test]
        fn catch_panic_backtrace_disabled() {
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

        #[test]
        fn catch_panic_backtrace_enabled() {
            unsafe {
                env::set_var("RUST_BACKTRACE", "1");
            }

            let panic = catch_panic(|| panic!("Oops!")).unwrap_err();
            assert_eq!(panic.backtrace().status(), BacktraceStatus::Captured);
        }

        #[test]
        fn panic_in_drop() {
            let panic = catch_panic(|| {
                let _ = JustDie;
            }).unwrap_err();
            assert_eq!(panic.message(), "Die!");
        }

        #[test]
        fn panic_in_drop_on_unwind() {
            let _ = catch_panic(|| {
                let _keeper = JustDie;
                panic!("Cause unwind");
            });
        }
        verify |success, output| {
            assert!(!success, "Panic during unwind from within `catch_panic` should fail");
            println!("{output}");
        }
    }
}
