use super::{
    Endian,
    write::{SerryOutput, SerryWrite, WriteResult},
    read::{SerryInput, SerryRead, ReadResult}
};
use byteorder::{WriteBytesExt, ReadBytesExt};

macro_rules! declare_int_impl_write {
    ($self:expr, $output:expr => with $write_fn:ident) => {{
        WriteBytesExt::$write_fn::<Endian>($output, *$self)?;
        Ok(())
    }};
    ($self:expr, $output:expr => none $write_fn:ident) => {{
        WriteBytesExt::$write_fn($output, *$self)?;
        Ok(())
    }}
}
macro_rules! declare_int_impl_read {
    ($input:expr => with $read_fn:ident) => {{
        Ok(ReadBytesExt::$read_fn::<Endian>($input)?)
    }};
    ($input:expr => none $read_fn:ident) => {{
        Ok(ReadBytesExt::$read_fn($input)?)
    }}
}
macro_rules! declare_int_impl {
    ($kind:ident $typename:ty) => {
        ::paste::paste!(
            impl SerryWrite for $typename {
                fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
                    declare_int_impl_write!(self, output => $kind [<write_ $typename>])
                }
            }

            impl SerryRead for $typename {
                fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
                    declare_int_impl_read!(input => $kind [<read_ $typename>])
                }
            }
        );
    }
}
macro_rules! declare_int_impls {
    () => {};
    ($typename:ty; $($remaining:tt)*) => {
        declare_int_impl!(with $typename);
        declare_int_impls!($($remaining)*);
    };
    (none $typename:ty; $($remaining:tt)*) => {
        declare_int_impl!(none $typename);
        declare_int_impls!($($remaining)*);
    };
}
declare_int_impls!(none u8; none i8; u16; i16; u32; i32; u64; i64; u128; i128;);

#[cfg(test)]
mod tests {
    use std::mem::transmute;
    use super::*;

    #[test]
    fn single_bytes() {
        let mut vec = Vec::new();
        for uint in u8::MIN..u8::MAX {
            vec.clear();
            vec.write_value(uint).expect("Could not write u8 to buffer");
            assert_eq!(vec.as_slice(), &[uint]);
        }
        for int in i8::MIN..i8::MAX {
            vec.clear();
            vec.write_value(int).expect("Could not write u8 to buffer");
            assert_eq!(vec.as_slice(), &[unsafe { transmute(int) }]);
        }
    }
}