mod field_name;
mod variants;
mod where_pred;

use proc_macro2::{Ident, Span};
use quote::IdentFragment;

use syn::{parse_quote, LitStr, Path, Type};

pub use field_name::*;
pub use variants::*;
pub use where_pred::*;

pub fn discriminant_name(str: impl IdentFragment) -> Ident {
    quote::format_ident!("__discriminant_{}", str)
}
pub fn default_discriminant_type() -> Type {
    parse_quote!(u16)
}

pub fn placeholder_ident() -> Ident {
    Ident::new("__SERRY_PLACEHOLDER", Span::call_site())
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

pub fn trait_read_path() -> Path {
    parse_quote!(::serry::read::SerryRead)
}
pub fn trait_write_path() -> Path {
    parse_quote!(::serry::write::SerryWrite)
}
pub fn trait_sized_path() -> Path {
    parse_quote!(::serry::repr::SerrySized)
}

pub fn error_type_path() -> Path {
    parse_quote!(::serry::SerryError)
}

pub fn parse_predicate(str: &LitStr) -> syn::Result<WherePredicates> {
    str.parse_with(WherePredicates::parse_terminated)
}
pub trait SynErrorExt: Sized {
    fn combined(self, other: Self) -> Self;
    fn maybe_combined(self, other: Option<Self>) -> Self;
}

impl SynErrorExt for syn::Error {
    fn combined(mut self, other: Self) -> Self {
        self.combine(other);
        self
    }

    fn maybe_combined(self, other: Option<Self>) -> Self {
        if let Some(other) = other {
            self.combined(other)
        } else {
            self
        }
    }
}
