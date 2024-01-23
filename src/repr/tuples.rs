use impl_trait_for_tuples::impl_for_tuples;

use crate::{
    read::ReadResult, write::WriteResult, SerryInput, SerryOutput, SerryRead, SerrySized,
    SerryWrite,
};

#[impl_for_tuples(2)]
#[tuple_types_custom_trait_bound(SerryWrite)]
impl SerryWrite for Tuple {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        for_tuples!(#(output.write_value(&self.Tuple)?;)*);
        Ok(())
    }
}

#[impl_for_tuples(2)]
#[tuple_types_custom_trait_bound(SerryRead)]
impl SerryRead for Tuple {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        Ok(for_tuples!((#(input.read_value()?),*)))
    }
}

#[impl_for_tuples(2)]
#[tuple_types_custom_trait_bound(SerrySized)]
impl SerrySized for Tuple {
    fn predict_size(&self) -> usize {
        let size = 0;
        for_tuples!(#(let size = size + self.Tuple.predict_size();)*);
        size
    }

    fn predict_constant_size() -> Option<usize> {
        let size = 0;

        for_tuples!(#(let size = size + match Tuple::predict_constant_size() {
            Some(v) => v,
            None => return None
        };)*);

        Some(size)
    }

    fn predict_constant_size_unchecked() -> usize {
        let size = 0;
        for_tuples!(#(let size = size + Tuple::predict_constant_size_unchecked();)*);
        size
    }
}
