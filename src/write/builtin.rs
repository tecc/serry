use super::{SerryOutput, SerryWrite, WriteResult};
use std::fs::File;
use std::net::TcpStream;

// output impls
impl SerryOutput for Vec<u8> {}
impl SerryOutput for File {}
impl SerryOutput for TcpStream {}

// impl<T: SerryOutput> SerryOutput for &mut T {}

impl<T> SerryWrite for &T
where
    T: SerryWrite + ?Sized,
{
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        T::serry_write(self, output)
    }
}
