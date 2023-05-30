# Serry - the unstructured Serde

Serde is a great library, but can sometimes be undesirable - it demands you use an abstract but structured view of your data such that it may be serialised into virtually any format.

Serry is different - it is a serialisation and deserialisation library that explicitly *does **not** specify* a format - it leaves the formatting details up to each type to implement.
The only things Serry does is provide some common facilities and utilities to serialise data.


## Data representations

### Integers

Integers are represented in little-endian (this may be subject to change, but probably will not).

### Vectors (for `T`)

Vectors begin with a `u64` which determines its length.
Then, Serry will read _n_ amount of `T`s into a vector. 

### HashMap (for `K` and `V`)

HashMaps are internally represented effectively as Vectors of `(K, V)`.
This means that you could interpret the encoded data as a list of entries or as a map of keys to values.

Note that Serry makes *no* guarantee of the order of the elements. If a predictable order is desirable, please use vectors.

### Strings

Strings are effectively represented as Vectors of `u8`.