pub mod write;
pub mod read;
pub mod repr;

#[cfg(feature = "checksum")]
mod checksum;

use std::{error::Error, fmt::Display};

#[cfg(feature = "derive")]
pub use serry_derive::*;

pub use write::{SerryWrite, SerryOutput};
pub use read::{SerryRead, SerryInput};
pub use repr::SerrySized;
pub use byteorder::LittleEndian as Endian;

#[derive(Debug)]
enum SerryErrorInner {
    Io(std::io::Error),
    Custom(String)
}
#[derive(Debug)]
pub struct SerryError {
    inner: SerryErrorInner
}
impl SerryError {
    pub fn custom(msg: impl ToString) -> Self {
        Self {
            inner: SerryErrorInner::Custom(msg.to_string())
        }
    }
}
impl From<std::io::Error> for SerryError {
    fn from(value: std::io::Error) -> Self {
        Self {
            inner: SerryErrorInner::Io(value)
        }
    }
}
impl Display for SerryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            SerryErrorInner::Io(e) => Display::fmt(e, f),
            SerryErrorInner::Custom(c) => f.write_str(c.as_str())
        }
    }
}
impl Error for SerryError {}