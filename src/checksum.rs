use std::io::Write;
use sha2::{Sha256, Digest};
use sha2::digest::FixedOutput;
use crate::SerryOutput;
use crate::write::WriteResult;

compile_error!("The 'checksum' feature is not yet complete.");

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum Checksum {
    Sha256
}
impl Checksum {
    pub fn compute_sum(&self, data: &[u8]) -> Vec<u8> {
        let mut output = Vec::new();
        self.write_sum(&mut output, data).expect("Error whilst writing to vec");
        output
    }

    pub fn write_sum(&self, out: &mut impl Write, data: &[u8]) -> WriteResult<()> {
        match self {
            Self::Sha256 => {
                let mut sha256 = Sha256::new();
                sha256.update(data);
                let data = sha256.finalize_fixed();
                out.write_all(&data)?;
            }
        }
        Ok(())
    }
}

pub struct ChecksumSerryInput {
}

pub struct ChecksumSerryOutput<'a, T: SerryOutput> {
    underlying: &'a mut T
}
impl<'a, T> ChecksumSerryOutput<'a, T> where T: SerryOutput {
    fn new(underlying: &mut T) {
        todo!()
    }
}