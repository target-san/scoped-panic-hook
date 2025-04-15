use crate::OwnedLocation;
use crate::hook::{NextHook, catch_unwind_with_scoped_hook};
use std::any::Any;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::borrow::Cow;
use std::fmt::{self, Display};
use std::panic::UnwindSafe;

#[derive(Debug)]
pub struct Panic {
    location: Option<OwnedLocation>,
    payload: Result<Message, RawPayload>,
    backtrace: Backtrace,
}

type Message = Cow<'static, str>;

type RawPayload = Box<dyn Any + Send + 'static>;

impl Panic {
    pub fn location(&self) -> Option<&OwnedLocation> {
        self.location.as_ref()
    }

    pub fn message(&self) -> &str {
        if let Ok(msg) = &self.payload {
            msg.as_ref()
        } else {
            "<unknown panic>"
        }
    }

    pub fn raw_payload(&self) -> Option<&RawPayload> {
        self.payload.as_ref().err()
    }

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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(loc) = self.location() {
            write!(f, "Panic \"{}\" at {}", self.message(), loc)
        } else {
            write!(f, "Panic \"{}\"", self.message())
        }
    }
}
/// Runs provided closure and captures any panics which may happen on the way
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
    let mut panic_bits = None;

    match catch_unwind_with_scoped_hook(
        |info| {
            // FIXME: check `info.can_unwind()` when https://github.com/rust-lang/rust/issues/92988 stabilizes;
            // otherwise we won't handle properly panics which initially don't unwind
            if panic_bits.is_none() {
                panic_bits = Some((info.location().map(Into::into), Backtrace::capture()));
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
