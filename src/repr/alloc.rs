use std::collections::HashMap;
use std::hash::Hash;

use crate::read::ReadResult;
use crate::write::WriteResult;
use crate::{SerryError, SerryInput, SerryOutput, SerryRead, SerrySized, SerryWrite};

// vectors

impl<T> SerryWrite for Vec<T>
where
    T: SerryWrite,
{
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        self.as_slice().serry_write(output)
    }
}

impl<T> SerryRead for Vec<T>
where
    T: SerryRead,
{
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        let count = input.read_value::<u64>()? as usize;
        let mut vec: Vec<T> = Vec::new();
        vec.reserve_exact(count);

        for _ in 0..count {
            // Ideally I'd prefer to use indexed accesses,
            // but that's not possible unless we initialise the vector with values first,
            // which requires that T implements Default (or something similar)
            vec.push(T::serry_read(input)?);
        }

        Ok(vec)
    }
}

impl<T> SerrySized for Vec<T>
where
    T: SerrySized,
{
    fn predict_size(&self) -> usize {
        let mut size = 0u64.predict_size();

        for el in self {
            size += el.predict_size();
        }

        size
    }

    fn predict_constant_size() -> Option<usize> {
        None
    }
}

// Hashmaps, as documented in the README, are represented as vectors of entries.
//
// Notice that the implementation does not literally pass it to the Vector implementation - for performance's sake,
// there is a custom implementation.
impl<K, V> SerryWrite for HashMap<K, V>
where
    K: SerryWrite,
    V: SerryWrite,
{
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        output.write_value(self.len() as u64)?;
        for entry in self {
            output.write_value(entry)?;
        }
        Ok(())
    }
}
// The read implementation is also slightly optimised to allocate less memory.
impl<K, V> SerryRead for HashMap<K, V>
where
    K: SerryRead + Hash + Eq,
    V: SerryRead,
{
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        let length = input.read_value::<u64>()? as usize;
        let mut map = HashMap::with_capacity(length);
        for _ in 0..length {
            let entry = input.read_value::<(K, V)>()?;
            map.insert(entry.0, entry.1);
        }
        Ok(map)
    }
}
impl<K, V> SerrySized for HashMap<K, V>
where
    K: SerrySized,
    V: SerrySized,
{
    fn predict_size(&self) -> usize {
        let mut size = u64::predict_constant_size_unchecked(); // size of the length prefix
        match (K::predict_constant_size(), V::predict_constant_size()) {
            (Some(k), Some(v)) => size += (k + v) * self.len(),
            _ => {
                for (key, value) in self {
                    size += key.predict_size() + value.predict_size();
                }
            }
        }
        size
    }

    fn predict_constant_size() -> Option<usize> {
        None
    }
}

// strings

impl SerryWrite for String {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        // We redirect to `str`s implementation to maintain correctness.
        self.as_str().serry_write(output)
    }
}

impl SerryRead for String {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        let data = input.read_value()?;
        String::from_utf8(data).map_err(|v| SerryError::custom(v.to_string()))
    }
}

impl SerrySized for String {
    fn predict_size(&self) -> usize {
        self.as_str().predict_size()
    }

    fn predict_constant_size() -> Option<usize> {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{Endian, SerryInput, SerryOutput, SerryRead};
    use byteorder::ByteOrder;
    use std::{collections::HashMap, mem::size_of};
    #[test]
    fn strings() {
        fn test_str(s: &str) {
            let mut length = [0u8; size_of::<u64>()];
            Endian::write_u64(&mut length, s.len() as u64);
            // LittleEndian::write_u64(&mut length, s.bytes().len() as u64)?;
            let mut out: Vec<u8> = Vec::new();

            out.write_value(s).expect("Expected successful write");

            assert_eq!(&out[..length.len()], length);
            assert_eq!(&out[length.len()..], s.as_bytes());

            let read = String::serry_read(&mut out.as_slice()).expect("Expected successful read");
            assert_eq!(read, s);
        }
        test_str("Hello, world!");
        test_str(
            r#"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation
ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id
est laborum."#,
        )
    }

    #[test]
    fn vectors() {
        let mut buf = Vec::new();
        type V = Vec<u128>;
        for n in 1..u8::MAX {
            buf.clear();
            let mut vec: V = vec![0u128; n as usize];
            buf.write_value(&vec)
                .expect("Could not write vector to buffer");

            let copy =
                V::serry_read(&mut buf.as_slice()).expect("Could not read vector from buffer");
            assert_eq!(vec, copy);

            for i in 0..vec.len() {
                vec[i] = i as u128;
            }

            buf.clear();
            buf.write_value(&vec)
                .expect("Could not write vector to buffer");
            let copy =
                V::serry_read(&mut buf.as_slice()).expect("Could not read vector from buffer");
            assert_eq!(vec, copy);
        }
    }

    #[test]
    fn hashmap() {
        let mut map = HashMap::new();
        for i in 0..u8::MAX {
            map.insert(i.to_string(), i as u128);
        }

        let mut buf = Vec::new();
        buf.write_value(&map)
            .expect("Could not write map to buffer");

        let copy = (&mut buf.as_slice())
            .read_value()
            .expect("Could not read map from buffer");
        assert_eq!(map, copy);

        let mut entries: Vec<_> = copy.into_iter().collect();
        let sort_fn = |v: &(String, u128)| v.1;
        entries.sort_by_key(sort_fn); // sort is required or else it fails because the ordering isn't correct
        let mut copy: Vec<_> = (&mut buf.as_slice())
            .read_value()
            .expect("Could not read list of entries from buffer");
        copy.sort_by_key(sort_fn);

        assert_eq!(entries, copy);
    }
}
