use crate::hook::{NextHook, catch_unwind_with_scoped_hook};
use std::any::Any;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display};
use std::panic::{Location, UnwindSafe};
/// Panic info gathered as one record
///
/// Contains panic's location, message or raw payload, and backtrace, if collected
#[derive(Debug)]
pub struct Panic {
    location: Option<OwnedLocation>,
    payload: Result<Message, RawPayload>,
    backtrace: Backtrace,
}

type Message = Cow<'static, str>;

type RawPayload = Box<dyn Any + Send + 'static>;

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
        if let Ok(msg) = &self.payload {
            msg.as_ref()
        } else {
            "<unknown panic>"
        }
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
    /// which displays panic's message, location and backtrace, if captured
    pub fn display_with_backtrace(&self) -> impl Display + '_ {
        format_from_fn(move |f| {
            write!(f, "{}", self)?;

            if self.backtrace.status() == BacktraceStatus::Captured {
                write!(f, "\nBacktrace: {}", self.backtrace)?;
            }

            Ok(())
        })
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

impl Display for Panic {
    /// Displays panic's message and location
    ///
    /// To display full panic info including backtrace, use [`Panic::display_with_backtrace`]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(loc) = self.location() {
            write!(f, "Panic \"{}\" at {}", self.message(), loc)
        } else {
            write!(f, "Panic \"{}\"", self.message())
        }
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
    let mut panic_bits = None;

    match catch_unwind_with_scoped_hook(
        |info| {
            // FIXME: check `info.can_unwind()` when https://github.com/rust-lang/rust/issues/92988 stabilizes;
            // otherwise we won't handle properly panics which initially don't unwind
            if panic_bits.is_none() {
                panic_bits = Some((
                    info.location().map(Into::into),
                    match config.capture_backtrace {
                        CaptureBacktrace::No => Backtrace::disabled(),
                        CaptureBacktrace::Yes => Backtrace::capture(),
                        CaptureBacktrace::Force => Backtrace::force_capture(),
                    },
                ));
                NextHook::Break
            } else {
                // In this case we assume we get non-unwinding panic in process of unwind.
                // As a result, we forward handling to previously installed hook,
                // so we'll see it described before program abort
                NextHook::PrevInstalledHook
            }
        },
        body,
    ) {
        Ok(ok) => {
            debug_assert!(panic_bits.is_none());
            Ok(ok)
        }
        Err(raw_payload) => {
            let (location, backtrace) = panic_bits.expect("Panic info wasn't recorded");
            Err(Panic {
                location,
                payload: transform_payload(raw_payload),
                backtrace,
            })
        }
    }
}
/// Runs provided closure and captures panic if one happens, along with its backtrace
///
/// # Limitations
///
/// Stable API doesn't allow to detect from panic hook that panic being handled will unwind.
/// So if you get unwindable panic, panic info will be swallowed. This will be fixed when
/// [#92988](https://github.com/rust-lang/rust/issues/92988) stabilizes
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
    use std::panic::panic_any;

    #[test]
    fn simple_catch_panic() {
        let panic = catch_panic(|| panic!("Oops!")).unwrap_err();
        // NB: we test backtrace status separately
        assert!(panic.raw_payload().is_none());
        assert_eq!(panic.message(), "Oops!");

        let loc = panic.location().unwrap();
        assert!(loc.file().ends_with("/panic.rs"));
        assert_eq!(loc.line(), 276);
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
}
