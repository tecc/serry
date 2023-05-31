
use super::{SerryOutput, SerryWrite, WriteResult};

// output impls
impl SerryOutput for Vec<u8> {}
// impl<T: SerryOutput> SerryOutput for &mut T {}

// write impls

impl<T> SerryWrite for [T] where T: SerryWrite {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        let length = self.len();
        output.write_value(length as u64)?;
        for item in self {
            item.serry_write(output)?;
        }
        Ok(())
    }
}
impl<T> SerryWrite for &T where T: SerryWrite + ?Sized {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        <T>::serry_write(self, output)
    }
}