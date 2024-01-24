use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use byteorder::WriteBytesExt;
use crate::{SerryInput, SerryOutput, SerryRead, SerrySized, SerryWrite};
use crate::read::ReadResult;
use crate::write::WriteResult;

impl SerryWrite for OsStr {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        self.as_encoded_bytes().serry_write(output)
    }
}
impl SerrySized for OsStr {
    fn predict_size(&self) -> usize {
        self.as_encoded_bytes().predict_size()
    }

    fn predict_constant_size() -> Option<usize> {
        None
    }
}

impl SerryRead for OsString {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        Ok(Self::from(input.read_value::<Vec<u8>>())?)
    }
}
impl SerryWrite for OsString {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        self.as_os_str().serry_write(output)
    }
}
impl SerrySized for OsString {
    fn predict_size(&self) -> usize {
        self.as_os_str().predict_size()
    }

    fn predict_constant_size() -> Option<usize> {
        None
    }
}

impl SerryWrite for Path {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        self.as_os_str().serry_write(output)
    }
}
impl SerrySized for Path {
    fn predict_size(&self) -> usize {
        self.as_os_str().predict_size()
    }

    fn predict_constant_size() -> Option<usize> {
        None
    }
}

impl SerryRead for PathBuf {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        Ok(PathBuf::from(input.read_value::<OsString>()?))
    }
}
impl SerryWrite for PathBuf {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        self.as_path().serry_write(output)
    }
}
impl SerrySized for PathBuf {
    fn predict_size(&self) -> usize {
        self.as_path().predict_size()
    }

    fn predict_constant_size() -> Option<usize> {
        None
    }
}