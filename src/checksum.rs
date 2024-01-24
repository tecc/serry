use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Debug, Formatter};
use crate::write::WriteResult;
use crate::{SerryError, SerryInput, SerryOutput, SerryRead, SerrySized, SerryWrite};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use crate::read::ReadResult;

pub use digest::Digest;

pub struct Checksummed<T, D> {
    pub inner: T,
    __digest: PhantomData<D>,
}

impl<T, D> Checksummed<T, D> {
    pub const fn new(value: T) -> Self {
        Self {
            inner: value,
            __digest: PhantomData
        }
    }
    fn checksum_size() -> usize where D: Digest {
        <D as Digest>::output_size()
    }
}

impl<T, D> SerryWrite for Checksummed<T, D> where T: SerryWrite, D: Digest {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        let mut digest = D::new();
        let mut buf: Vec<u8> = Vec::new();
        buf.write_value(&self.inner)?;
        digest.update(&buf);
        let finalized = digest.finalize();
        output.write_value(&buf)?;
        output.write_value(finalized.as_slice())
    }
}

impl<T, D> SerryRead for Checksummed<T, D> where T: SerryRead, D: Digest {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        let value: Vec<u8> = input.read_value()?;
        let checksum: Vec<u8> = input.read_value()?;

        if checksum.len() != Self::checksum_size() {
            return Err(SerryError::custom("checksum size incorrect (possible algorithm mismatch)"));
        }

        let mut digest = D::new();
        digest.update(&value);
        let finalized = digest.finalize();
        if finalized.as_slice() != checksum {
            return Err(SerryError::custom("checksum incorrect"));
        }

        return Ok(Checksummed::new(T::serry_read(&mut value.as_slice())?));
    }
}

impl<T, D> SerrySized for Checksummed<T, D> where T: SerrySized, D: Digest {
    fn predict_size(&self) -> usize {
        self.inner.predict_size() + Self::checksum_size() + (u64::predict_constant_size_unchecked() * 2)
    }

    fn predict_constant_size() -> Option<usize> {
        T::predict_constant_size().map(|v| Self::checksum_size() + (u64::predict_constant_size_unchecked() * 2))
    }
}

impl<T, D> Deref for Checksummed<T, D> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T, D> DerefMut for Checksummed<T, D>  {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T, D> PartialEq<Self> for Checksummed<T, D> where T: Eq {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<T, D> PartialEq<T> for Checksummed<T, D> where T: Eq {
    fn eq(&self, other: &T) -> bool {
        self.inner.eq(other)
    }
}

impl<T, D> Eq for Checksummed<T, D> where T: Eq {}

impl<T, D> PartialOrd for Checksummed<T, D> where Checksummed<T, D>: PartialEq, T: PartialOrd {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}
impl<T, D> PartialOrd<T> for Checksummed<T, D> where Checksummed<T, D>: PartialEq<T>, T: PartialOrd {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.inner.partial_cmp(other)
    }
}

impl<T, D> Ord for Checksummed<T, D> where Checksummed<T, D>: PartialOrd, T: Ord {
    fn cmp(&self, other: &Self) -> Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<T, D> Debug for Checksummed<T, D> where T: Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}
impl<T, D> fmt::Display for Checksummed<T, D> where T: fmt::Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}


#[cfg(feature = "checksum-sha2")]
mod __sha2 {
    use sha2::Sha256;
    use sha2::Sha512;

    pub type ChecksummedSha256<T> = super::Checksummed<T, Sha256>;
    pub type ChecksummedSha512<T> = super::Checksummed<T, Sha512>;
}

#[cfg(feature = "checksum-sha2")]
pub use __sha2::*;

#[cfg(test)]
mod tests {
    use super::*;
    use rand::{Rng, RngCore};

    fn test_digest<D>() where D: Digest {
        let mut buf = Vec::new();
        macro_rules! value_test {
            ($type:ty, $value:expr) => {{
                let value: $type = $value;
                let checksummed = Checksummed::<$type, D>::new(value.clone());
                buf.write_value(&checksummed).expect("Failed to write value");
                if buf.len() != checksummed.predict_size() { panic!("Predicted size {} is different from written size {}", checksummed.predict_size(), buf.len()) }
                let read = Checksummed::<$type, D>::serry_read(&mut buf.as_slice()).expect("Failed to read value");
                assert_eq!(read, checksummed);
                assert_eq!(checksummed.inner, value);
                buf.clear();
            }};
        }


        let mut rng = rand::rngs::OsRng;

        value_test!(u8, rng.gen());
        value_test!(u8, rng.gen());
        value_test!(i8, rng.gen());
        value_test!(i8, rng.gen());

        value_test!(u16, rng.gen());
        value_test!(u16, rng.gen());
        value_test!(i16, rng.gen());
        value_test!(i16, rng.gen());

        value_test!(u32, rng.gen());
        value_test!(u32, rng.gen());
        value_test!(i32, rng.gen());
        value_test!(i32, rng.gen());

        value_test!(u64, rng.gen());
        value_test!(u64, rng.gen());
        value_test!(i64, rng.gen());
        value_test!(i64, rng.gen());

        value_test!(String, "Hello world!".into());
        value_test!(String, rng.next_u32().to_string());
        value_test!(String, rng.next_u64().to_string());
    }

    #[cfg(feature = "checksum-sha2")]
    mod sha2test {
        use sha2::{Sha224, Sha256, Sha512_224, Sha512_256, Sha384, Sha512};
        use super::test_digest;

        #[test]
        fn sha224() {
            test_digest::<Sha224>();
        }

        #[test]
        fn sha256() {
            test_digest::<Sha256>();
        }
        #[test]
        fn sha512_224() {
            test_digest::<Sha512_224>();
        }

        #[test]
        fn sha512_256() {
            test_digest::<Sha512_256>();
        }

        #[test]
        fn sha384() {
            test_digest::<Sha384>();
        }
        #[test]
        fn sha512() {
            test_digest::<Sha512>();
        }
    }
}