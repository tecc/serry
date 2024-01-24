mod alloc;
mod primitives;
mod tuples;
mod os;

pub trait SerrySized {
    fn predict_size(&self) -> usize;
    fn predict_constant_size_if_possible(&self) -> usize {
        Self::predict_constant_size().unwrap_or(self.predict_size())
    }
    fn predict_constant_size() -> Option<usize>;
    fn predict_constant_size_unchecked() -> usize {
        Self::predict_constant_size().unwrap()
    }
}

impl<T> SerrySized for &T
where
    T: SerrySized + ?Sized,
{
    fn predict_size(&self) -> usize {
        T::predict_size(self)
    }
    fn predict_constant_size() -> Option<usize> {
        T::predict_constant_size()
    }
}
