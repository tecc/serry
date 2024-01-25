use crate::SerryError;
use std::io::Read;

mod builtin;
mod wrap;

pub use wrap::SerryInputWrapper;

pub type ReadResult<T> = Result<T, SerryError>;

pub trait SerryRead: Sized {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self>;
}

pub trait SerryInput: Read {
    fn read_value<T>(&mut self) -> ReadResult<T>
    where
        T: SerryRead,
    {
        let mut wrapper = SerryInputWrapper(self);
        T::serry_read(&mut wrapper)
    }
}
