use impl_trait_for_tuples::impl_for_tuples;

use crate::{SerryOutput, write::WriteResult, SerryWrite, SerryRead, read::ReadResult, SerryInput};

#[impl_for_tuples(1, 2)]
#[tuple_types_custom_trait_bound(SerryWrite)]
impl SerryWrite for Tuple {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        for_tuples!(#(output.write_value(&self.Tuple)?;)*);
        Ok(())
    }
}

#[impl_for_tuples(1, 2)]
#[tuple_types_custom_trait_bound(SerryRead)]
impl SerryRead for Tuple {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        Ok(for_tuples!((#(input.read_value()?),*)))
    }
}