use serry_derive::{SerryRead, SerrySized, SerryTraits, SerryWrite};

#[derive(SerryRead, SerryWrite, SerrySized)]
#[serry(repr(u16))]
enum IntDiscriminated {
    VariantOne,
    VariantTwo,
}
#[derive(SerryTraits)]
#[serry(repr(&'static str))]
enum StringDiscriminated {
    VariantOne,
    VariantTwo,
}

fn main() {}
