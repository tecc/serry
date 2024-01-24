use crate::SerryError;
use std::io::Write;

mod builtin;

pub type WriteResult<T> = Result<T, SerryError>;

pub trait SerryWrite {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()>;
}

pub trait SerryOutput: Write + Sized {
    fn write_value<T>(&mut self, value: T) -> WriteResult<()>
    where
        T: SerryWrite,
    {
        value.serry_write(self)
    }

    /*fn serry_str(&mut self, s: impl AsRef<str>) -> WriteResult<()> {
        let s = s.as_ref();
        let bytes = s.as_bytes();
        self.write_u64::<Endian>(bytes.len() as u64)?;
        self.write_all(bytes)?;
        Ok(())
    }*/
}
