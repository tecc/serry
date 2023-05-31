use serry::{SerryRead, SerryWrite};

#[derive(SerryRead, SerryWrite)]
#[serry(version(3))]
struct Something {
    #[serry(version = 0)]
    name: String,
    #[serry(version = 1..2, default)]
    stuff: u32
}

#[derive(SerryRead)]
enum VersionedEnum {
    #[serry(version)]
    V1 {
        
    },
    V2 {
        
    }
}

fn main() {

}