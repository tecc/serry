use syn::parse::{Parse, ParseStream};
use syn::{LitInt, Token, Type};

pub struct RootVersionInfo {
    pub minimum_supported_version: usize,
    pub current_version: usize,
    pub version_type: Type,
}

#[derive(Default, Copy, Clone, Debug)]
pub struct VersionRange {
    pub since: usize,
    pub until: Option<usize>,
}

impl Parse for VersionRange {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let since: LitInt = input.parse()?;
        let since = since.base10_parse()?;

        let until = if input.peek(Token![..]) {
            let _ = input.parse::<Token![..]>();
            let until: LitInt = input.parse()?;
            Some(until.base10_parse()?)
        } else {
            None
        };

        Ok(Self { since, until })
    }
}
