use std::cmp::Ordering;
use syn::parse::{Parse, ParseStream};
use syn::{Error, Field, LitStr};

#[derive(Copy, Clone)]
pub enum FieldOrder {
    Alphabetical,
    AsSpecified,
}

impl FieldOrder {
    pub fn do_sort(&self) -> bool {
        match self {
            Self::AsSpecified => false,
            _ => true,
        }
    }

    pub fn cmp(&self, a: &Field, b: &Field) -> Ordering {
        match self {
            Self::AsSpecified => Ordering::Equal,
            Self::Alphabetical => a.ident.cmp(&b.ident),
        }
    }
}

impl Default for FieldOrder {
    fn default() -> Self {
        FieldOrder::AsSpecified
    }
}

impl Parse for FieldOrder {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let str: LitStr = input.parse()?;
        Ok(match str.value().to_lowercase().as_str() {
            "alphabetical" => Self::Alphabetical,
            "as_specified" => Self::AsSpecified,
            _ => {
                return Err(Error::new_spanned(
                    str,
                    "invalid field order - must be either 'alphabetical' or 'as_specified'",
                ))
            }
        })
    }
}
