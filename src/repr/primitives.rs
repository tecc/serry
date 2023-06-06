use crate::{
    read::{ReadResult, SerryInput, SerryRead},
    write::{SerryOutput, SerryWrite, WriteResult},
    repr::{SerrySized},
    Endian, SerryError,
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

            impl SerrySized for $typename {
                #[inline]
                fn predict_size(&self) -> usize {
                    Self::predict_constant_size_unchecked()
                }
                #[inline]
                fn predict_constant_size_unchecked() -> usize {
                    std::mem::size_of::<$typename>()
                }
                #[inline]
                fn predict_constant_size() -> Option<usize> {
                    Some(Self::predict_constant_size_unchecked())
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

const BOOL_FALSE: u8 = 0;
const BOOL_TRUE: u8 = 1;

impl SerryRead for bool {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        let value = input.read_value::<u8>()?;
        match value {
            BOOL_FALSE => Ok(false),
            BOOL_TRUE => Ok(true),
            _ => Err(SerryError::custom(format!("bool is invalid - {} is not {} (false) nor {} (true)", value, BOOL_FALSE, BOOL_TRUE)))
        }
    }
}

impl SerryWrite for bool {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        output.write_value(if *self { BOOL_TRUE } else { BOOL_FALSE })
    }
}
impl SerrySized for bool {
    #[inline]
    fn predict_size(&self) -> usize {
        Self::predict_constant_size_unchecked()
    }
    #[inline]
    fn predict_constant_size() -> Option<usize> {
        Some(Self::predict_constant_size_unchecked())
    }
    #[inline]
    fn predict_constant_size_unchecked() -> usize {
        std::mem::size_of::<u8>()
    }
}

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
impl<T> SerrySized for [T] where T: SerrySized {
    fn predict_size(&self) -> usize {
        let mut size = u64::predict_constant_size().unwrap();
        match T::predict_constant_size() {
            Some(v) => size += self.len() * v,
            None => {
                for value in self {
                    size += value.predict_size();
                }
            }
        }
        size
    }

    fn predict_constant_size() -> Option<usize> {
        None
    }
}
impl<T, const L: usize> SerrySized for [T; L] where T: SerrySized {
    fn predict_size(&self) -> usize {
        match Self::predict_constant_size() {
            Some(v) => v,
            None => {
                let mut size = u64::predict_constant_size_unchecked();
                for t in self {
                    size += t.predict_size();
                }
                size
            }
        }
    }

    fn predict_constant_size() -> Option<usize> {
        match T::predict_constant_size() {
            Some(v) => Some(u64::predict_constant_size_unchecked() + (v * L)),
            None => None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{SerryInput, SerryOutput};


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

    #[test]
    fn bools() {
        let mut buf = Vec::new();
        buf.write_value(true).expect("Could not write");
        let value: bool = buf.as_slice().read_value().expect("Could not read");
        assert!(value);
        buf.clear();
        buf.write_value(false).expect("Could not write");
        let value: bool = buf.as_slice().read_value().expect("Could not read");
        assert!(!value);
    }
}
