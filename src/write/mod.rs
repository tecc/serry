use crate::SerryError;
use std::io::Write;

mod builtin;
mod wrap;

pub use wrap::SerryOutputWrapper;

pub type WriteResult<T> = Result<T, SerryError>;

pub trait SerryWrite {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()>;
}

pub trait SerryOutput: Write {
    #[inline]
    fn write_value<T>(&mut self, value: T) -> WriteResult<()>
    where
        T: SerryWrite,
    {
        // So we can make sure it's always Sized
        let mut wrapper = wrap::SerryOutputWrapper(self);
        value.serry_write(&mut wrapper)
    }
}