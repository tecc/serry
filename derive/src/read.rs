use proc_macro2::{Ident, Span, TokenStream};
use quote::ToTokens;
use syn::{spanned::Spanned, Data, DeriveInput, Error, Field, Fields, TypePath, parse_quote, Path};

use crate::{find_and_parse_serry_attr, Extrapolate, SerryAttr, SerryAttrFields, find_and_parse_serry_attr_auto, enumerate_variants, FieldOrder};

fn version_ident() -> Ident {
    Ident::new("__VERSION", Span::call_site())
}


pub fn derive_read_impl(input: DeriveInput) -> Result<TokenStream, Error> {
    let root_attr = find_and_parse_serry_attr_auto(&input.attrs, &input.data)?;

    let version_id = version_ident();

    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    fn deserialise_fields(fields: &Fields, root_attr: &SerryAttr, field_order: FieldOrder, target_type: &Path) -> TokenStream {
        fn deserialise_field(
            field: &Field,
            output_ident: &Ident,
            root_attr: &SerryAttr,
        ) -> TokenStream {
            let field_attr = match find_and_parse_serry_attr(&field.attrs, SerryAttrFields::field())
            {
                Ok(attr) => attr,
                Err(e) => return e.to_compile_error(),
            };

            let version = match root_attr.version_with_range_of(&field_attr) {
                Ok(version) => version,
                Err(e) => return e.to_compile_error()
            };

            let ty = &field.ty;
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
                    quote!(return Err(_Error::custom(format!("Cannot extrapolate field {} from structure with version {}", stringify!(#output_ident), #version_id)))),
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
        match fields {
            Fields::Unit => quote!().into_token_stream(),
            Fields::Named(named) => {
                let mut vec: Vec<(Ident, &Field)> = vec![];
                for field in &named.named {
                    vec.push((field.ident.clone().unwrap(), field));
                }

                if field_order.do_sort() {
                    // Avoid sorting if unnecessary because
                    vec.sort_by(|a, b| field_order.cmp(a.1, b.1));
                }

                let reading: Vec<_> = vec
                    .iter()
                    .map(|(ident, field)| deserialise_field(field, ident, root_attr))
                    .collect();

                let identifiers = vec.iter().map(|v| &v.0);

                (quote! {
                    #(#reading)*;
                    Ok(#target_type {
                        #(#identifiers),*
                    })
                })
                .into()
            }
            Fields::Unnamed(unnamed) => {
                let fields: Vec<_> = unnamed.unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, field)| {
                        let name = format!("__field_{}", i);
                        let ident = Ident::new(name.as_str(), Span::call_site());
                        (deserialise_field(field, &ident, root_attr), ident, field)
                    })
                    .collect();

                let reading = fields.iter().map(|data| &data.0);
                let props = fields.iter().map(|v| &v.1);

                quote! {
                    #(#reading);*
                    Ok(#target_type(#(#props),*))
                }
            }
        }
    }

    let mut output = TokenStream::new();

    enum VersionStrategy {
        OnlyAllowSame,
        AnyWithinRange
    }

    fn version_check(attr: &SerryAttr, strategy: VersionStrategy) -> TokenStream {
        let info = if let Some(info) = &attr.version_info { info } else { return quote!() };

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
            VersionStrategy::AnyWithinRange => { // Default versioning strategy - may be any version less than or equal to the current one
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
            },
        }
    }

    if let Some(info) = &root_attr.version_info {
        let version = info.current_version;
        let minimum = info.minimum_supported_version;

        output.extend(match &input.data {
            // Enums are versioned differently if the current and minimum version are the same (which it will be for most cases)
            Data::Enum(_) if minimum == version => version_check(&root_attr, VersionStrategy::OnlyAllowSame),
            _ => version_check(&root_attr, VersionStrategy::AnyWithinRange),
        })
    }

    let field_order = root_attr.field_order.unwrap_or_default();

    match &input.data {
        Data::Struct(model) => output.extend(deserialise_fields(&model.fields, &root_attr, field_order, &parse_quote!(Self))),
        Data::Enum(model) => {
            let enumerated = enumerate_variants(model.variants.iter())?;

            let discriminant_type = root_attr.discriminant_type.clone().unwrap_or_else(|| {
                parse_quote!(u16)
                // We default to u16 since most enums probably won't have more than 65536 variants
                // (and because 256 variants felt like a bit of low limit)
            });

            let enum_variant_ident = Ident::new("__variant", Span::call_site());
            output.extend(quote!(let #enum_variant_ident: #discriminant_type = input.read_value()?;));
            let variants = enumerated.iter().map(|v| {
                let discriminant = v.discriminant;
                let variant = v.variant;
                let variant_name = &variant.ident;
                let mut actual = TokenStream::new();
                actual.extend(version_check(&v.attr, VersionStrategy::AnyWithinRange));
                actual.extend(deserialise_fields(&variant.fields, &v.attr, field_order, &parse_quote!(Self::#variant_name)));
                quote!{
                    #discriminant => {
                        #actual
                    }
                }
            });
            output.extend(quote!{
                return match #enum_variant_ident {
                    #(#variants),*,
                    _ => Err(_Error::custom("unexpected variant")) // included for safety
                }
            });
        }
        _ => return Err(Error::new_spanned(input, "unsupported type - you can only derive SerryRead for structs and enums")),
    };

    Ok(quote! {
        const _: () = {
            use ::serry::SerryError as _Error;
            impl #impl_generics ::serry::read::SerryRead for #ident #ty_generics #where_clause {
                fn serry_read(input: &mut impl ::serry::read::SerryInput) -> ::serry::read::ReadResult<Self> {
                    #output
                }
            }
        };
    }.into())
}
