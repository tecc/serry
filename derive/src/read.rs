use proc_macro2::{Ident, Span, TokenStream};
use quote::ToTokens;
use syn::{parse_quote, spanned::Spanned, Data, DeriveInput, Error, Expr, Field, Fields, Path};

use crate::{enumerate_variants, find_and_parse_serry_attr, find_and_parse_serry_attr_auto, process_fields, util, Extrapolate, FieldName, FieldOrder, SerryAttr, SerryAttrFields, create_where_clause};

fn version_ident() -> Ident {
    Ident::new("__version", Span::call_site())
}

pub fn derive_read_impl(input: &DeriveInput) -> Result<TokenStream, Error> {
    let root_attr = find_and_parse_serry_attr_auto(&input.attrs, &input.data)?;
    
    let trait_type = quote!(::serry::read::SerryRead);

    let ident = &input.ident;
    let (impl_generics, ty_generics, _) = input.generics.split_for_impl();

    let where_clause = create_where_clause(trait_type.to_token_stream(), &input.generics);

    fn deserialise_fields(
        fields: &Fields,
        root_attr: &SerryAttr,
        field_order: FieldOrder,
        target_type: &Path,
    ) -> TokenStream {
        fn deserialise_field(
            field: &Field,
            name: &FieldName,
            root_attr: &SerryAttr,
        ) -> TokenStream {
            let field_attr = match find_and_parse_serry_attr(&field.attrs, SerryAttrFields::field())
            {
                Ok(attr) => attr,
                Err(e) => return e.to_compile_error(),
            };

            let version = match root_attr.version_with_range_of(&field_attr) {
                Ok(version) => version,
                Err(e) => return e.to_compile_error(),
            };

            let ty = &field.ty;
            let output_ident = name.output_ident();
            if let Some((root_info, field_range)) = version {
                let version_id = version_ident();
                let version_ty = &root_info.version_type;
                let since = field_range.since;
                let until_clause = field_range.until
                    .map_or(TokenStream::new(), |until| {
                        quote_spanned!(field_attr.attr.span() => && #version_id <= (#until as #version_ty))
                    });

                // TODO: Make this use results
                let extrapolate = field_attr.extrapolate.as_ref().map_or(
                    quote!(
                        return Err(_Error::custom(format!(
                            "Cannot extrapolate field {} from structure with version {}",
                            stringify!(#name),
                            #version_id
                        )))
                    ),
                    |v| match v {
                        Extrapolate::Default => {
                            quote!(<#ty as ::core::default::Default>::default())
                        }
                        Extrapolate::Function(path) => {
                            quote!(#path())
                        }
                    },
                );
                quote_spanned! { field.span() =>
                    let #output_ident: #ty = if #version_id >= (#since as #version_ty) #until_clause {
                        input.read_value()?
                    } else {
                        #extrapolate
                    };
                }
            } else {
                quote_spanned! { field.span() =>
                    let #output_ident: #ty = input.read_value()?;
                }
            }
        }

        let vec: Vec<(FieldName, &Field)> = if let Some(vec) = process_fields(fields, field_order) {
            vec
        } else {
            return quote!(Ok(#target_type)).into_token_stream();
        };

        match fields {
            Fields::Unit => quote!(Ok(#target_type)).into_token_stream(),
            Fields::Named(_) => {
                let reading: Vec<_> = vec
                    .iter()
                    .map(|(name, field)| deserialise_field(field, &name, root_attr))
                    .collect();

                let identifiers = vec.iter().map(|v| {
                    let output_ident = v.0.output_ident();
                    let name = &v.0;
                    quote!(#name: #output_ident)
                });

                (quote! {
                    #(#reading)*;
                    Ok(#target_type {
                        #(#identifiers),*
                    })
                })
                .into()
            }
            Fields::Unnamed(_) => {
                let reading = vec
                    .iter()
                    .map(|data| deserialise_field(data.1, &data.0, root_attr));
                let props = vec.iter().map(|v| v.0.output_ident());

                quote! {
                    #(#reading);*
                    Ok(#target_type(#(#props),*))
                }
            }
        }
    }

    let mut read_body = TokenStream::new();
    let mut variant_discriminants = TokenStream::new();

    enum VersionStrategy {
        OnlyAllowSame,
        AnyWithinRange,
    }

    fn version_check(attr: &SerryAttr, strategy: VersionStrategy) -> TokenStream {
        let info = if let Some(info) = &attr.version_info {
            info
        } else {
            return quote!();
        };

        let version_id = version_ident();
        let ty = &info.version_type;
        let version = info.current_version;
        let minimum = info.minimum_supported_version;

        match strategy {
            // Enums are versioned differently if the current and minimum version are the same (which it will be for most cases)
            VersionStrategy::OnlyAllowSame => {
                quote! {
                    let #version_id: #ty = input.read_value()?;
                    if #version_id != (#version as #ty) {
                        return Err(_Error::custom(format!("unsupported version {} (up to version {} is supported)", #version_id, #version)));
                    }
                }
            }
            VersionStrategy::AnyWithinRange => {
                // Default versioning strategy - may be any version less than or equal to the current one
                let second_condidtion = if minimum != version {
                    quote!(&& #version_id <= (#minimum as #ty))
                } else {
                    quote!()
                };
                quote! {
                    let #version_id: #ty = input.read_value()?;
                    if #version_id > (#version as #ty) #second_condidtion {
                        return Err(_Error::custom(format!("unsupported version {} (up to version {} is supported)", #version_id, #version)));
                    }
                }
            }
        }
    }

    if let Some(info) = &root_attr.version_info {
        let version = info.current_version;
        let minimum = info.minimum_supported_version;

        read_body.extend(match &input.data {
            // Enums are versioned differently if the current and minimum version are the same (which it will be for most cases)
            Data::Enum(_) if minimum == version => {
                version_check(&root_attr, VersionStrategy::OnlyAllowSame)
            }
            _ => version_check(&root_attr, VersionStrategy::AnyWithinRange),
        })
    }

    let field_order = root_attr.field_order.unwrap_or_default();
    match &input.data {
        Data::Struct(model) => read_body.extend(deserialise_fields(
            &model.fields,
            &root_attr,
            field_order,
            &parse_quote!(Self),
        )),
        Data::Enum(model) => {
            let discriminant_type = root_attr
                .discriminant_type
                .clone()
                .unwrap_or_else(util::default_discriminant_type);
            let enumerated = enumerate_variants(&discriminant_type, model.variants.iter())?;

            let str_type = util::is_str_type(&discriminant_type);
            let read_discriminant_type = if str_type {
                parse_quote!(::serry::_internal::String)
            } else {
                discriminant_type.clone()
            };
            let enum_variant_ident = Ident::new("__variant", Span::call_site());
            /*let enum_variant_expr: Expr = if str_type {
                try_parse_quote!(#enum_variant_ident.as_str())?
            } else {
                try_parse_quote!(#enum_variant_ident)?
            };*/
            read_body.extend(
                quote!(let #enum_variant_ident: #read_discriminant_type = input.read_value()?;),
            );
            for v in enumerated.iter() {
                let variant = v.variant;
                let variant_name = &variant.ident;
                let discriminant = &v.discriminant;
                let discriminant_ident = util::discriminant_name(&variant.ident);
                let mut actual = TokenStream::new();
                let field_order = v.attr.field_order.unwrap_or(field_order);
                actual.extend(version_check(&v.attr, VersionStrategy::AnyWithinRange));
                actual.extend(deserialise_fields(
                    &variant.fields,
                    &v.attr,
                    field_order,
                    &parse_quote!(Self::#variant_name),
                ));
                variant_discriminants.extend(quote! {
                    #[allow(all)]
                    const #discriminant_ident: #discriminant_type = { #discriminant };
                });
                read_body.extend(quote! {
                    {
                        if #enum_variant_ident == #discriminant_ident {
                            return { #actual };
                        }
                    }
                });
            }
            read_body.extend(quote! {
                return Err(_Error::custom("unexpected variant")) // included for safety
            });
        }
        _ => {
            return Err(Error::new_spanned(
                input,
                "unsupported type - you can only derive SerryRead for structs and enums",
            ))
        }
    };

    Ok(quote! {
        const _: () = {
            use ::serry::SerryError as _Error;

            #variant_discriminants

            #[allow(all)]
            #[automatically_derived]
            impl #impl_generics #trait_type for #ident #ty_generics #where_clause {
                fn serry_read(input: &mut impl ::serry::read::SerryInput) -> ::serry::read::ReadResult<Self> {
                    #read_body
                }
            }
        };
    }.into())
}
