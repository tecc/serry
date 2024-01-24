use serry::{Checksummed, ChecksummedSha256, ChecksummedSha512, SerryOutput};
use serry_derive::{SerryRead, SerryWrite};

#[derive(SerryWrite, SerryRead)]
struct Something {
    aaaa: String,
    // Serry will automatically add a checksum when writing and verify said checksum when reading
    sensitive: ChecksummedSha256<String>
}

fn main() {
    let mut value = Something {
        aaaa: "hello!".to_string(),
        sensitive: ChecksummedSha256::new("i'm checksummed!".into())
    };

    let mut output = Vec::new();
    // As previously stated, checksums are calculated when values are written.
    output.write_value(&value).expect("ooo it's written");

    // Checksummed (the base type for ChecksummedSha256 et al) implement
    // numerous traits (Deref, DerefMut, Eq*, Ord*, etc.) so that working with
    // it is as seamless as possible. (* if the wrapped type implements it)
    println!("{}", value.sensitive.trim());
    if value.sensitive.as_str() == "i'm checksummed!" {
        println!("oh wow value.sensitive is checksummed")
    }
    // The inner field is also public to allow end users to interact and set the value directly.
    value.sensitive.inner.make_ascii_uppercase();
    value.sensitive.inner = "hehehe i'm modified now".to_string();

    // You can use any algorithm with Checksummed that implements `digest::Digest`.
    let something: Checksummed<String, sha2::Sha512_224>;
    // Serry provides ChecksummedSha256 and ChecksummedSha512 (with the checksum-sha2 feature)
    // as builtins. More may be added in the future, but for now these two are the only ones available.
}