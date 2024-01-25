use crate::SerryInput;
use std::io::{IoSliceMut, Read};

pub struct SerryInputWrapper<'a, Impl: Read + ?Sized>(pub &'a mut Impl);

impl<'a, Impl> SerryInput for SerryInputWrapper<'a, Impl>
where
    Impl: Read + ?Sized,
{
    fn read_value<T>(&mut self) -> super::ReadResult<T>
    where
        T: crate::SerryRead,
    {
        T::serry_read(self)
    }
}

impl<'a, Impl> Read for SerryInputWrapper<'a, Impl>
where
    Impl: Read + ?Sized,
{
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }

    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> std::io::Result<usize> {
        self.0.read_vectored(bufs)
    }

    /*fn is_read_vectored(&self) -> bool {
        self.0.is_read_vectored()
    }*/

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        self.0.read_to_end(buf)
    }

    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
        self.0.read_to_string(buf)
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()> {
        self.0.read_exact(buf)
    }

    /*fn read_buf(&mut self, buf: BorrowedCursor<'_>) -> std::io::Result<()> {
        self.0.read_buf(buf)
    }

    fn read_buf_exact(&mut self, cursor: BorrowedCursor<'_>) -> std::io::Result<()> {
        self.0.read_buf_exact(cursor)
    }*/
}
