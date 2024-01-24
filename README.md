# Serry - the unstructured Serde

> NOTE: 
> 
> Serry's API and data representations are somewhat unstable. 
> They may be subject to breaking changes.

Serde is a great library, but can sometimes be undesirable - it demands you use an abstract but structured view of your data such that it may be serialised into virtually any format.

Serry is different - it is a serialisation and deserialisation library that explicitly _does **not** specify_ a format for its consumers - it leaves the specifics of serialisation and deserialisation up to each type to implement.
The only things Serry does is provide some common facilities and utilities to serialise data.

## Usage

The main traits used in Serry are the `SerryRead` and `SerryWrite` traits.
As the names suggest, those traits are responsible for the reading and writing of your data (respectively comparable to `Deserialize` and `Serialize`).
The `SerrySized` trait also exists as a utility.

### Derive macros

Serry has a `derive` feature very similar to Serde's equivalent.
It is configurable, although everything comes with a sensible default.

```rust
// Data is packed tightly in Serry, so there's no information describing
// "this is the `name` field" nor "this is the `password` field".
// By default, Serry will effectively "concatenate" the fields when serialising.
#[derive(SerryRead, SerryWrite, SerrySized)]
struct User {
    name: String,
    // Let's pretend this is hashed and secure and all that.
    password: String,
    tasks: Vec<(String, TaskState)> // Also, yes, some tuples are supported.
}

/// Enumerators are stored similarly to structs in that each variant 
/// is treated like one in practice.
/// The *only* difference is that they have a 'hidden field' of sorts;
/// that field being the discriminant. 
/// The discriminant is written before the variant contents. 
/// It is by default represented as a u16, although that can be changed with the repr attribute.
#[derive(SerryRead, SerryWrite, SerrySized)]
enum TaskState {
    NotYetStarted,
    Started {
        started_at: std::time::SystemTime 
    },
    /// If you wish to change the discriminant of a single variant,
    /// you can do so with the `repr` value.
    /// Any expression can be used as long as the expressions can be evaluated
    /// in a const context.
    #[serry(repr(8))]
    Completed {
        started_at: std::time::SystemTime,
        completed_at: std::time::SystemTime
    }
}
/// The discriminant type can be changed as well. 
/// Serry provides builtin support for integer types and strings as discriminants.
/// If strings are used, the discriminants default to 
/// using their respective variant names as the discriminants.
#[derive(SerryRead, SerryWrite, SerrySized)]
#[serry(repr(String))]
enum ThemePreference {
    Light,
    Dark,
    #[serry(repr("auto"))]
    Auto
}
```

### Implementing the traits manually

Manually implementing the traits can be benificial in some cases, but for 
most purposes derive macros are preferred due to the greater clarity they provide.

As an example, take Serry's `bool` implementation:
```rust 
const BOOL_FALSE: u8 = 0;
const BOOL_TRUE: u8 = 1;

impl SerryRead for bool {
    fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
        let value = input.read_value::<u8>()?;
        match value {
            BOOL_FALSE => Ok(false),
            BOOL_TRUE => Ok(true),
            _ => Err(SerryError::custom(format!(
                "bool is invalid - {} is not {} (false) nor {} (true)",
                value, BOOL_FALSE, BOOL_TRUE
            ))),
        }
    }
}
impl SerryWrite for bool {
    fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
        output.write_value(if *self { BOOL_TRUE } else { BOOL_FALSE })
    }
}
impl SerrySized for bool {
    #[inline]
    fn predict_size(&self) -> usize {
        Self::predict_constant_size_unchecked()
    }
    #[inline]
    fn predict_constant_size() -> Option<usize> {
        Some(Self::predict_constant_size_unchecked())
    }
    #[inline]
    fn predict_constant_size_unchecked() -> usize {
        std::mem::size_of::<u8>()
    }
}
```

`SerryRead` for `bool` is quite simple: it reads a single byte (`u8`) and checks if it is `0u8` (false) or `1u8` (true).

Really, just use the library. It can't hurt that much.

## Data representations

All Serry-provided representations attempt to be somewhat parseable by non-Rust programs.

### Integers

Integers are represented in little-endian (this may be subject to change, but probably will not).

### Vectors (for `T`)

Vectors begin with a `u64` which determines its length.
Then, Serry will read _n_ amount of `T`s into a vector. 

### HashMap (for `K` and `V`)

HashMaps are internally represented effectively as Vectors of `(K, V)`.
This means that you could interpret the encoded data as a list of entries or as a map of keys to values.

Note that Serry makes _no_ guarantee regarding the order of the elements. If a predictable order is desirable, please use vectors.

### Strings

Strings are effectively represented as Vectors of `u8`. They are encoded as UTF-8.
The only difference between using Vec<u8> and Strings is that `String` guarantees a valid UTF-8 string.

Similar implementations are used for `OsStr`, `Path`, and their owned counterparts;
the exact details _may_ vary based on the operating system.

## Licence

```txt
   Copyright (c) 2023 tecc

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
```