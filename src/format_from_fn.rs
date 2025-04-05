use std::fmt::{self, Debug, Display, Formatter};
/// Creates type whose `Display` and `Debug` implementations
/// are provided with function `format_fn`
///
/// Replica of experimental `std::fmt::from_fn`
pub fn format_from_fn<F: Fn(&mut Formatter<'_>) -> fmt::Result>(format_fn: F) -> FormatFromFn<F> {
    FormatFromFn { format_fn }
}

pub struct FormatFromFn<F> {
    format_fn: F,
}

impl<F: Fn(&mut Formatter<'_>) -> fmt::Result> Debug for FormatFromFn<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self.format_fn)(f)
    }
}

impl<F: Fn(&mut Formatter<'_>) -> fmt::Result> Display for FormatFromFn<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self.format_fn)(f)
    }
}
