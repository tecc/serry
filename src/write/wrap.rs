use std::io::{Write};

use super::{SerryOutput, SerryWrite, WriteResult};

pub struct SerryOutputWrapper<'a, Impl: Write + ?Sized>(pub &'a mut Impl);
impl<'a, Impl> SerryOutput for SerryOutputWrapper<'a, Impl>
where
    Impl: Write + ?Sized,
{
    #[inline]
    fn write_value<T>(&mut self, value: T) -> WriteResult<()>
    where
        T: SerryWrite,
    {
        value.serry_write(self)
    }
}
impl<'a, Impl> Write for SerryOutputWrapper<'a, Impl>
where
    Impl: Write + ?Sized,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        self.0.write_vectored(bufs)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }

    /*fn is_write_vectored(&self) -> bool {
        self.0.is_write_vectored()
    }*/

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        self.0.write_all(buf)
    }

    /*fn write_all_vectored(&mut self, mut bufs: &mut [std::io::IoSlice<'_>]) -> std::io::Result<()> {
        self.0.write_all_vectored(bufs)
    }*/

    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        self.0.write_fmt(fmt)
    }
}
