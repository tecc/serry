use serry_derive::SerryTraits;
use std::collections::HashMap;
use std::hash::Hash;

// So we have this trait (IdTrait) that we use with another type.
// It has associated types, which is important.
trait Identifiable {
    type Id: Hash + Eq;
    fn id(&self) -> &Self::Id;
}

// Here we have the other type, IdMap, that takes some `T` that is `Identifiable`
// and works as a map from `T::Id` to `T`.
// Serry can't detect its usage now, sa we have to specify bounds manually.
// This is where the `bound` attribute comes in.
#[derive(SerryTraits)]
#[serry(bound(
    read = "T: SerryRead, T::Id: SerryRead",
    write = "T: SerryWrite, T::Id: SerryWrite",
    sized = "T: SerrySized, T::Id: SerrySized"
))]
struct IdMap<T>
where
    T: Identifiable,
{
    inner: HashMap<T::Id, T>,
}

// However, writing all three every time you need to specify the bounds can be exhausting.
// Serry provides a clean workaround - our favourite placeholder `_`!
#[derive(SerryTraits)]
#[serry(bound = "T: _, T::Id: _")]
struct IdMap2<T>
where
    T: Identifiable,
{
    inner: HashMap<T::Id, T>,
}

fn main() {}
