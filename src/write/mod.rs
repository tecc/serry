use std::io::Write;
use byteorder::{WriteBytesExt};
use crate::{Endian, SerryError};

mod builtin;

pub type WriteResult<T> = Result<T, SerryError>;

pub trait SerryWrite {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()>;
}

pub trait SerryOutput: Write + Sized {
    fn with_checksum(&mut self) -> ChecksumSerryOutput<Self> {
        ChecksumSerryOutput::new(self)
    }

    fn write_value<T>(&mut self, value: T) -> WriteResult<()> where T: SerryWrite {
        value.serry_write(self)
    }

    fn serry_str(&mut self, s: impl AsRef<str>) -> WriteResult<()> {
        let s = s.as_ref();
        let bytes = s.as_bytes();
        self.write_u64::<Endian>(bytes.len() as u64)?;
        self.write_all(bytes)?;
        Ok(())
    }
}

pub struct ChecksumSerryOutput<'a, T: ?Sized> {
    output: &'a T,
    buf: Vec<u8>,
}

impl<'a, T: ?Sized> ChecksumSerryOutput<'a, T> {
    fn new(output: &'a mut T) -> Self {
        Self {
            output,
            buf: Vec::new(),
        }
    }

    fn end(self) {}
}

impl<'a, T> Write for ChecksumSerryOutput<'a, T> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buf.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.buf.flush()
    }
}

impl<'a, T> SerryOutput for ChecksumSerryOutput<'a, T> {}