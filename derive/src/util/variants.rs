use crate::attr::{find_and_parse_serry_attr, SerryAttr, SerryAttrFields};
use crate::util;
use proc_macro2::Literal;
use quote::ToTokens;
use std::collections::HashSet;
use syn::{parse_quote, Error, Expr, Lit, Type, Variant};

pub struct AnnotatedVariant<'a> {
    pub variant: &'a Variant,
    pub attr: SerryAttr<'a>,
    pub discriminant: Expr,
}

pub fn enumerate_variants<'a, I>(
    discriminant_type: &Type,
    variants: I,
) -> Result<Vec<AnnotatedVariant<'a>>, Error>
where
    I: Iterator<Item = &'a Variant>,
{
    let mut reserved_values = HashSet::new();

    let mut preprocessed = Vec::new();
    for variant in variants {
        let attr = find_and_parse_serry_attr(&variant.attrs, SerryAttrFields::enum_variant())?;

        let discriminant = match &attr.discriminant_value {
            Some(value) => {
                if reserved_values.contains(value) {
                    return Err(Error::new_spanned(
                        value,
                        "multiple variants can not have the same value",
                    ));
                }
                reserved_values.insert(value.clone());
                Some(value.clone())
            }
            None => None,
        };

        preprocessed.push((variant, attr, discriminant))
    }

    let mut vec = Vec::new();
    let mut next = 0usize;

    for (variant, attr, discriminant) in preprocessed {
        let actual_discriminant = if let Some(discriminant) = discriminant {
            discriminant.clone()
        } else {
            // TODO: Handling of int types
            if util::is_str_type(discriminant_type) {
                let value = variant.ident.to_string();
                let expr: Expr = syn::parse2(Lit::new(Literal::string(&value)).to_token_stream())?;
                if reserved_values.contains(&expr) {
                    return Err(Error::new_spanned(
                        &variant.ident,
                        format!("Discriminator `{}` is already in use", value),
                    ));
                }
                expr
            } else {
                let value = loop {
                    let inner: Lit = Lit::new(Literal::usize_unsuffixed(next));
                    let expr = parse_quote!(#inner as #discriminant_type);
                    if reserved_values.contains(&expr) {
                        next += 1;
                        continue;
                    }
                    break expr;
                };
                next += 1;
                value
            }
        };

        vec.push(AnnotatedVariant {
            variant,
            attr,
            discriminant: actual_discriminant,
        })
    }

    Ok(vec)
}
