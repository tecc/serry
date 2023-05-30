use crate::{SerryError, SerryInput, SerryOutput, SerryRead, SerryWrite};
use crate::read::ReadResult;
use crate::write::WriteResult;

// vectors

impl<T> SerryWrite for Vec<T> where T: SerryWrite {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        self.as_slice().serry_write(output)
    }
}

impl<T> SerryRead for Vec<T> where T: SerryRead {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        let count = input.read_value::<u64>()? as usize;
        let mut vec: Vec<T> = Vec::new();
        vec.reserve_exact(count);

        for i in 0..count {
            vec[i] = T::serry_read(input)?;
        }

        Ok(vec)
    }
}

// strings

impl SerryWrite for String {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        output.write_value(self.as_bytes())
    }
}

impl SerryRead for String {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        let data = input.read_value()?;
        String::from_utf8(data).map_err(|v| SerryError::custom(v.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;
    use byteorder::ByteOrder;
    use crate::{Endian, SerryOutput};
    #[test]
    fn strings() {
        fn test_str(s: &str) {
            let mut length = [0u8; size_of::<u64>()];
            Endian::write_u64(&mut length, s.len() as u64);
            // LittleEndian::write_u64(&mut length, s.bytes().len() as u64)?;
            let mut out: Vec<u8> = Vec::new();

            out.serry_str(s).expect("Expected successful write");

            assert_eq!(&out[..length.len()], length);
            assert_eq!(&out[length.len()..], s.as_bytes());
        }
        test_str("Hello, world!");
        test_str(r#"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation
ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id
est laborum."#)
    }
}