pub mod write;
pub mod read;

mod repr;

#[cfg(feature = "checksum")]
mod checksum;

#[cfg(feature = "derive")]
pub use serry_derive::*;

pub use write::{SerryWrite, SerryOutput};
pub use read::{SerryRead, SerryInput};
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
