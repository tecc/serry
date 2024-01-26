use crate::{read::ReadResult, write::WriteResult, SerryInput, SerryOutput, SerryRead, SerrySized, SerryWrite};

macro_rules! ref_impl {
    ($type:ty, $construct:path) => {
        impl<T> SerryRead for $type where T: SerryRead {
            fn serry_read(input: &mut impl SerryInput) -> ReadResult<Self> {
                input.read_value().map($construct)
            }
        }
        impl<T> SerryWrite for $type where T: SerryWrite + ?Sized {
            fn serry_write(&self, output: &mut impl SerryOutput) -> WriteResult<()> {
                self.as_ref().serry_write(output)
            }
        }
        impl<T> SerrySized for $type where T: SerrySized + ?Sized {
            fn predict_size(&self) -> usize {
                self.as_ref().predict_size()
            }
        
            fn predict_constant_size() -> Option<usize> {
                T::predict_constant_size()
            }
        }
    };
}

ref_impl!(Box<T>, Box::new);
ref_impl!(std::rc::Rc<T>, std::rc::Rc::new);
ref_impl!(std::sync::Arc<T>, std::sync::Arc::new);