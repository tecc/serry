# Serry - the unstructured Serde

> NOTE: 
> 
> Serry's API and data representations are somewhat unstable. 
> They may be subject to breaking changes.

Serde is a great library, but can sometimes be undesirable - it demands you use an abstract but structured view of your data such that it may be serialised into virtually any format.

Serry is different - it is a serialisation and deserialisation library that explicitly _does **not** specify_ a format for its consumers - it leaves the specifics of serialisation and deserialisation up to each type to implement.
The only things Serry does is provide some common facilities and utilities to serialise data.

## Usage

### Implementing manually

The main traits used in Serry are the `SerryRead` and `SerryWrite` traits.
As the names suggest

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

Note that Serry makes _no_ guarantee of the order of the elements. If a predictable order is desirable, please use vectors.

### Strings

Strings are effectively represented as Vectors of `u8`. They are encoded as UTF-8.


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