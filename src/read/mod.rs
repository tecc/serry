use crate::SerryError;
use std::io::Read;

mod builtin;

pub type ReadResult<T> = Result<T, SerryError>;

pub trait SerryRead: Sized {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self>;
}

pub trait SerryInput: Read + Sized {
    fn read_value<T>(&mut self) -> ReadResult<T>
    where
        T: SerryRead,
    {
        T::serry_read(self)
    }
}
