use crate::attr::FieldOrder;
use proc_macro2::{Ident, Span, TokenStream};
use quote::ToTokens;
use syn::{Field, Fields, LitInt};

pub enum FieldName {
    Ident(Ident),
    Index(LitInt),
}
impl FieldName {
    pub fn output_ident(&self) -> Ident {
        let name = match &self {
            Self::Ident(ident) => ident.to_string(),
            Self::Index(int) => int.to_string(),
        };
        Ident::new(
            ["__field_", name.as_str()].join("").as_str(),
            Span::call_site(),
        )
    }
}
impl ToTokens for FieldName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self {
            Self::Ident(ident) => ident.to_tokens(tokens),
            Self::Index(index) => index.to_tokens(tokens),
        }
    }
}

pub fn output_fields<'a, I>(iter: I, unnamed: bool) -> TokenStream
where
    I: Iterator<Item = &'a FieldName>,
{
    if unnamed {
        let names = iter.map(FieldName::output_ident);
        quote!((#(#names),*))
    } else {
        let names = iter.map(|name| {
            let output = name.output_ident();
            quote!(#name: #output)
        });
        quote!({ #(#names),* })
    }
}

pub type ProcessedFields<'a> = Vec<(FieldName, &'a Field)>;
pub fn process_fields(fields: &Fields, field_order: FieldOrder) -> Option<ProcessedFields> {
    let fields: Vec<_> = match fields {
        Fields::Unit => return None,
        Fields::Named(named) => named.named.iter().collect(),
        Fields::Unnamed(unnamed) => unnamed.unnamed.iter().collect(),
    };

    let mut vec: Vec<(FieldName, &Field)> = vec![];
    for (i, field) in fields.into_iter().enumerate() {
        vec.push((
            match &field.ident {
                Some(ident) => FieldName::Ident(ident.clone()),
                None => FieldName::Index(LitInt::new(i.to_string().as_str(), Span::call_site())),
            },
            field,
        ));
    }

    if field_order.do_sort() {
        vec.sort_by(|a, b| field_order.cmp(a.1, b.1));
    }

    Some(vec)
}
