use serry::{SerryRead, SerrySized, SerryWrite};

#[derive(SerryRead, SerryWrite, SerrySized)]
#[serry(version(3))]
struct Something {
    #[serry(version = 0)]
    name: String,
    #[serry(version = 1..2, default)]
    stuff: u32,
}

#[derive(SerryRead, SerryWrite, SerrySized)]
#[serry(discriminate_by(u16))]
enum VersionedEnum {
    #[serry(repr = 0)]
    V1 { field: String },
    #[serry(version(2))]
    V2 {
        #[serry(version = 1)]
        v1_field: u32,
        #[serry(version = 2)]
        v2_field: String,
        #[serry(version = 0)]
        aba: String,
    },
    #[serry(version(3))]
    Other(#[serry(version = 2)] u32),
}

fn main() {}
