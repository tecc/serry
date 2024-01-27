use proc_macro2::TokenStream;
use syn::{parse_macro_input, DeriveInput, Error};

#[macro_use]
extern crate quote;

mod attr;
mod read;
mod sized;
mod util;
mod write;

#[proc_macro_derive(SerryRead, attributes(serry))]
pub fn derive_read(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    read::derive_read_impl(&item)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

#[proc_macro_derive(SerryWrite, attributes(serry))]
pub fn derive_write(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    write::derive_write_impl(&item)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

#[proc_macro_derive(SerrySized, attributes(serry))]
pub fn derive_sized(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    sized::derive_sized_impl(&item)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

#[proc_macro_derive(SerryTraits, attributes(serry))]
pub fn derive_all(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);

    let mut output = TokenStream::new();
    let mut extend = |result: Result<_, Error>| match result {
        Ok(content) => output.extend(content),
        Err(e) => output.extend(e.to_compile_error()),
    };

    extend(read::derive_read_impl(&item));
    extend(write::derive_write_impl(&item));
    extend(sized::derive_sized_impl(&item));

    output.into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::read::derive_read_impl;
    use crate::sized::derive_sized_impl;
    use crate::write::derive_write_impl;
    use quote::ToTokens;

    #[test]
    fn derive() {
        let check_validity = |input: TokenStream| {
            let _file = syn::parse_file(&input.to_string()).expect("Failed parse");
            // println!("{}", prettyplease::unparse(&file));
        };

        let derive_test = |content: &str| {
            let file = syn::parse_file(content).expect("Failed parse");
            let item = file.items.first().expect("First item required");
            let input = syn::parse2(item.to_token_stream())
                .expect("Could not parse first item to DeriveInput");
            check_validity(derive_read_impl(&input).expect("Could not derive SerryRead"));
            check_validity(derive_write_impl(&input).expect("Could not derive SerryWrite"));
            check_validity(derive_sized_impl(&input).expect("Could not derive SerrySized"));
        };

        derive_test(
            "
            #[derive(SerryRead, SerryWrite, SerrySized)]
            enum StringDiscriminated {
                VariantOne,
                VariantTwo,
            }
        ",
        );
        derive_test(
            "
            #[derive(SerryRead, SerryWrite, SerrySized)]
            #[serry(repr(&'static str))]
            enum StringDiscriminated {
                VariantOne,
                VariantTwo,
            }
        ",
        );
        derive_test(
            "
            #[derive(SerryRead, SerryWrite, SerrySized)]
            #[serry(repr(u16))]
            enum IntDiscriminated {
                VariantOne,
                VariantTwo,
            }
        ",
        );
        derive_test(
            "
            #[derive(SerryTraits)]
            struct ThingWithTypeParams<K, V: ?Sized> {
                map: HashMap<K, V>,
            }
        ",
        );
        derive_test(
            "
            #[derive(SerryTraits)]
            #[serry(bound(\"T: _, T::Id: _\"))]
            struct IdMap<T>
            where
                T: IdTrait,
            {
                inner: HashMap<T::Id, T>,
            }
            trait IdTrait {
                type Id: Hash + Eq;
                fn id(&self) -> &Self::Id;
            }
        ",
        );
    }
}
