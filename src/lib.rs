/// Raw API for setting up scoped panic hooks
pub mod hook;
/// Panic capture types and functions,
/// including some with finer tuned functionality
pub mod panic;

pub use panic::{Panic, catch_panic};
