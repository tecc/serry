use serry_derive::{SerryRead, SerrySized, SerryWrite};

#[derive(SerryRead, SerryWrite, SerrySized)]
#[serry(repr(u16))]
enum IntDiscriminated {
    VariantOne,
    VariantTwo
}
#[derive(SerryRead, SerryWrite, SerrySized)]
#[serry(repr(&'static str))]
enum StringDiscriminated {
    VariantOne,
    VariantTwo
}

fn main() {

}