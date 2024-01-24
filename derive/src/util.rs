use proc_macro2::Ident;
use quote::IdentFragment;
use std::fmt::Display;
use syn::{parse_quote, Type};

pub fn discriminant_name(str: impl IdentFragment) -> Ident {
    quote::format_ident!("__discriminant_{}", str)
}
pub fn default_discriminant_type() -> Type {
    parse_quote!(u16)
}

pub fn str_type() -> Type {
    parse_quote!(str)
}
pub fn str_ref_type() -> Type {
    parse_quote!(&str)
}
pub fn is_str_type(ty: &Type) -> bool {
    match ty {
        Type::Path(type_path) => {
            if let Some(ident) = type_path.path.get_ident() {
                let str = ident.to_string();
                match str.as_str() {
                    "str" => true,
                    "String" => true,
                    _ => false,
                }
            } else {
                false
            }
        }
        Type::Reference(reference) => is_str_type(reference.elem.as_ref()),
        _ => false,
    }
}
