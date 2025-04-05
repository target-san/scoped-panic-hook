use std::{fmt::Display, panic::Location};
/// Simple structure which mirrors `std::panic::Location`,
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
