use crate::{
    read::{ReadResult, SerryInput, SerryRead},
    write::{SerryOutput, SerryWrite, WriteResult},
    Endian,
};
use byteorder::{ReadBytesExt, WriteBytesExt};

macro_rules! declare_impl_write {
    ($self:expr, $output:expr => with $write_fn:ident) => {{
        WriteBytesExt::$write_fn::<Endian>($output, *$self)?;
        Ok(())
    }};
    ($self:expr, $output:expr => none $write_fn:ident) => {{
        WriteBytesExt::$write_fn($output, *$self)?;
        Ok(())
    }};
}
macro_rules! declare_impl_read {
    ($input:expr => with $read_fn:ident) => {{
        Ok(ReadBytesExt::$read_fn::<Endian>($input)?)
    }};
    ($input:expr => none $read_fn:ident) => {{
        Ok(ReadBytesExt::$read_fn($input)?)
    }};
}
macro_rules! declare_impl {
    ($kind:ident $typename:ty) => {
        ::paste::paste!(
            impl SerryWrite for $typename {
                fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
                    declare_impl_write!(self, output => $kind [<write_ $typename>])
                }
            }

            impl SerryRead for $typename {
                fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
                    declare_impl_read!(input => $kind [<read_ $typename>])
                }
            }
        );
    }
}
macro_rules! declare_impls {
    () => {};
    ($typename:ty; $($remaining:tt)*) => {
        declare_impl!(with $typename);
        declare_impls!($($remaining)*);
    };
    (none $typename:ty; $($remaining:tt)*) => {
        declare_impl!(none $typename);
        declare_impls!($($remaining)*);
    };
}
declare_impls!(none u8; none i8; u16; i16; u32; i32; u64; i64; u128; i128;);
declare_impls!(f32; f64;);

#[cfg(test)]
mod tests {
    use crate::SerryOutput;


    macro_rules! range {
        ($i:ident: => $($block:tt)*) => {};
        ($i:ident: $t:ty $(,$other:ty)* => $($block:tt)*) => {{
            let mut previous = <$t>::MIN;
            let step = std::mem::size_of::<$t>();
            let step = step - 1;
            let step: $t = if step > 0 { 0b1 << step * 8 } else { 1 };
            while let Some(i) = previous.checked_add(step) {
                previous = i;
                {
                    let $i: $t = i;
                    $($block)*
                }
            }
            // eprintln!(stringify!(- $t is okay));
            range!($i: $($other),* => $($block)*);
        }};
    }

    #[test]
    fn single_byte_integers() {
        let mut buf = Vec::new();
        range! { i: u8, i8 =>
            buf.clear();
            buf.write_value(i).expect("Could not write u8 to buffer");
            assert_eq!(buf.as_slice(), &i.to_le_bytes());
        }
    }

    #[test]
    fn unsigned_ints_larger_than_a_byte() {
        let mut buf = Vec::new();
        range! { i: u16, u32, u64, u128 =>
            buf.clear();
            buf.write_value(i).expect("Could not write");
            assert_eq!(buf.as_slice(), i.to_le_bytes())
        }
    }

    #[test]
    fn signed_ints_larger_than_a_byte() {
        let mut buf = Vec::new();
        range! { i: i16, i32, i64, i128 =>
            buf.clear();
            buf.write_value(i).expect("Could not write");
            assert_eq!(buf.as_slice(), i.to_le_bytes())
        }
    }
}
