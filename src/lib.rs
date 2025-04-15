/// Raw API for setting up scoped panic hooks
pub mod hook;
pub mod panic;

pub use panic::{Panic, catch_panic};
