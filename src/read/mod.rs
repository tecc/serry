use std::io::Read;
use crate::{SerryError};
#[cfg(feature = "checksum")]
use crate::checksum::Checksum;

pub type ReadResult<T> = Result<T, SerryError>;

pub trait SerryRead: Sized {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self>;
}

pub trait SerryInput: Read + Sized {
    #[cfg(feature = "checksum")]
    fn with_checksum(&mut self, checksum: Checksum) -> ChecksumSerryInput {
        todo!()
    }
    fn read_value<T>(&mut self) -> ReadResult<T> where T: SerryRead {
        T::serry_read(self)
    }
}



pub struct ChecksumSerryInput {

}