use proc_macro2::{Ident, Span, TokenStream};
use syn::{spanned::Spanned, Data, DeriveInput, Error, Field, Fields, Token, TypeReference};

use crate::{enumerate_variants, find_and_parse_serry_attr, SerryAttr, SerryAttrFields, find_and_parse_serry_attr_auto};

pub fn derive_write_impl(input: DeriveInput) -> Result<TokenStream, Error> {
    let root_attr = find_and_parse_serry_attr_auto(&input.attrs, &input.data)?;

    let ident = input.ident.clone();
    // let mut generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    fn serialise_fields(fields: &Fields, root_attr: &SerryAttr) -> TokenStream {
        let fields: Vec<_> = match fields {
            Fields::Unit => return quote!(),
            Fields::Named(named) => named.named.iter().collect(),
            Fields::Unnamed(unnamed) => unnamed.unnamed.iter().collect(),
        };

        let mut vec: Vec<(Ident, &Field)> = vec![];
        for (i, field) in fields.into_iter().enumerate() {
            vec.push((match &field.ident {
                Some(ident) => ident.clone(),
                None => Ident::new(i.to_string().as_str(), Span::call_site())
            }, field));
        }

        vec.sort_by(|a, b| a.0.cmp(&b.0));

        let vec: Vec<_> = vec
            .into_iter()
            .map(|(ident, field)| {
                let field_attr =
                    match find_and_parse_serry_attr(&field.attrs, SerryAttrFields::field()) {
                        Ok(attr) => attr,
                        Err(e) => return e.to_compile_error(),
                    };

                let version = match root_attr.version_with_range_of(&field_attr) {
                    Ok(version) => version,
                    Err(e) => return e.to_compile_error()
                };

                if let Some((root_info, field_range)) = version {
                    if field_range
                        .until
                        .map_or(false, |v| v < root_info.current_version)
                    {
                        return quote_spanned!(field.span() => const _: () = (););
                    }
                }
                let ty = TypeReference {
                    and_token: Token![&](Span::call_site()),
                    lifetime: None,
                    mutability: None,
                    elem: Box::new(field.ty.clone()),
                };
                quote_spanned! { field.span() =>
                    output.write_value::<#ty>(&self.#ident)?;
                }
            })
            .collect();

        (quote! {
            #(#vec)*
        })
        .into()
    }

    let mut field_serialise = TokenStream::new();

    if let Some(info) = &root_attr.version_info {
        match &input.data {
            Data::Enum(_) => {
                return Err(Error::new_spanned(&input, "Enums do not support versioning - versioning must be specified for each variant"));
            }
            _ => {}
        }

        let ty = &info.version_type;
        let version = info.current_version;
        field_serialise.extend(quote! {
            output.write_value::<#ty>(#version as #ty);
        })
    }

    match &input.data {
        Data::Struct(model) => field_serialise.extend(serialise_fields(&model.fields, &root_attr)),
        Data::Enum(model) => {
            let variants = enumerate_variants(model.variants.iter())?;
            let cases = variants.iter().map(|var| {
                let variant_ident = &var.variant.ident;
                quote!(Self::#variant_ident => )
            });
            field_serialise.extend(quote! {
                match self {
                    #(#cases),*,
                    _ => panic!("Unexpected enum variant")
                }
            })
        }
        // Data::Union(_) => return Err(Error::new_spanned(input, "unions are not supported")),
        _ => return Err(Error::new_spanned(input, "unsupported type - you can only derive SerryWrite for structs and enums")),
    }

    Ok(quote! {
        const _: () = {
            impl #impl_generics ::serry::write::SerryWrite for #ident #ty_generics #where_clause {
                fn serry_write(&self, output: &mut impl ::serry::write::SerryOutput) -> ::serry::write::WriteResult<()> {
                    #field_serialise
                    Ok(())
                }
            }
        };
    }.into())
}
