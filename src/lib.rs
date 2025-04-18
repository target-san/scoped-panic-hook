//! # Catching panics
//!
//! Just use
//!
//! ```rust
//! let result = scoped_panic_hook::catch_panic(|| panic!("Oopsie!"));
//!
//! if let Err(panic) = result {
//!     eprintln!("{}", panic.display_with_backtrace());
//! }
//! ```
//!
//! Any panic which happens inside closure supplied to [`catch_panic`]
//! will be caught
//!
//! # Using manual panic hooks
//!
//! In case you wnat to do something nontrivial, you can analyze panics manually
//!
//! ```rust
//! let mut counter = 0;
//!
//! let _ = scoped_panic_hook::hook::catch_unwind_with_scoped_hook(
//!     |_| { counter += 1; scoped_panic_hook::hook::NextHook::PrevInstalledHook },
//!     || panic!("Oopsie!")
//! );
//!
//! println!("Caught panics: {counter}");
//! ```

/// Raw API for setting up scoped panic hooks
pub mod hook;
/// Panic capture types and functions,
/// including some with finer tuned functionality
pub mod panic;

pub use panic::{Panic, catch_panic};
