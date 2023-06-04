use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{spanned::Spanned, Data, DeriveInput, Error, Fields, Token, TypeReference};

use crate::{enumerate_variants, find_and_parse_serry_attr, SerryAttr, SerryAttrFields, find_and_parse_serry_attr_auto, FieldOrder, default_discriminant_type, FieldName, process_fields, create_pattern_match, ProcessedFields};

pub fn derive_write_impl(input: DeriveInput) -> Result<TokenStream, Error> {
    let root_attr = find_and_parse_serry_attr_auto(&input.attrs, &input.data)?;

    let ident = input.ident.clone();
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    fn serialise_fields<'a, T>(fields: &'a Fields, root_attr: &SerryAttr, field_order: FieldOrder, accessor: impl Fn(&FieldName) -> T)
        -> (TokenStream, Option<ProcessedFields<'a>>) where T: ToTokens {
        let processed_fields = match process_fields(fields, field_order) {
            Some(vec) => vec,
            None => return (quote!(), None)
        };

        let vec: Vec<_> = processed_fields
            .iter()
            .map(|(field_name, field)| {
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
                let output_ident = accessor(field_name);
                quote_spanned! { field.span() =>
                    output.write_value::<#ty>(#output_ident)?;
                }
            })
            .collect();

        (quote!(#(#vec)*), Some(processed_fields))
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
            output.write_value::<#ty>(#version as #ty)?;
        })
    }

    let field_order = root_attr.field_order.unwrap_or_default();
    match &input.data {
        Data::Struct(model) => {
            let accessor = |value: &FieldName| { quote!(&self.#value) };
            let serialise = serialise_fields(&model.fields, &root_attr, field_order, accessor);
            field_serialise.extend(serialise.0);
        },
        Data::Enum(model) => {
            let accessor = |v: &FieldName| { v.output_ident() };
            let variants = enumerate_variants(model.variants.iter())?;
            let discriminant_ty = root_attr.discriminant_type.clone().unwrap_or_else(default_discriminant_type);
            let cases = variants.iter().map(|var| {
                let variant_ident = &var.variant.ident;
                let discriminant = var.discriminant;
                let field_order = var.attr.field_order.unwrap_or(field_order);
                let (serialise, fields) = serialise_fields(&var.variant.fields, &var.attr, field_order, accessor);

                let params = if let Some(fields) = fields {
                    let unnamed = match var.variant.fields {
                        Fields::Unit => panic!("fields is unit but ProcessedFields is available?"),
                        Fields::Named(_) => false,
                        Fields::Unnamed(_) => true,
                    };
                    create_pattern_match(fields.iter().map(|v| &v.0), unnamed)
                } else {
                    quote!()
                };

                quote!(Self::#variant_ident #params => {
                    output.write_value::<#discriminant_ty>(#discriminant as #discriminant_ty)?;
                    #serialise
                })
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
            #[automatically_derived]
            impl #impl_generics ::serry::write::SerryWrite for #ident #ty_generics #where_clause {
                fn serry_write(&self, output: &mut impl ::serry::write::SerryOutput) -> ::serry::write::WriteResult<()> {
                    #field_serialise
                    Ok(())
                }
            }
        };
    }.into())
}
