
use super::{SerryOutput, SerryWrite, WriteResult};

// output impls
impl SerryOutput for Vec<u8> {}
// impl<T: SerryOutput> SerryOutput for &mut T {}

impl<T> SerryWrite for &T where T: SerryWrite + ?Sized {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        T::serry_write(self, output)
    }
}