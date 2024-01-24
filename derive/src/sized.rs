use proc_macro2::{Ident, TokenStream};
use quote::ToTokens;
use syn::{parse_quote, spanned::Spanned, Data, DeriveInput, Error, Fields};

use crate::{
    create_pattern_match, enumerate_variants, find_and_parse_serry_attr,
    find_and_parse_serry_attr_auto, process_fields, util, FieldName, FieldOrder, ProcessedFields,
    SerryAttr, SerryAttrFields,
};

fn get_size_ident() -> Ident {
    return parse_quote!(__size);
}

pub fn derive_sized_impl(input: DeriveInput) -> Result<TokenStream, Error> {
    let root_attr = find_and_parse_serry_attr_auto(&input.attrs, &input.data)?;

    let size_ident = get_size_ident();
    let ident = input.ident.clone();
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    #[derive(Default)]
    struct Predict {
        on_self: TokenStream,
        constant: TokenStream,
        constant_unchecked: TokenStream,
    }
    impl Predict {
        fn all(v: impl ToTokens) -> Self {
            let stream = v.to_token_stream();
            Self {
                on_self: stream.clone(),
                constant: stream.clone(),
                constant_unchecked: stream,
            }
        }
        fn one(v: impl ToTokens) -> Self {
            Self {
                on_self: v.to_token_stream(),
                ..Self::default()
            }
        }
    }

    fn sized_fields<'a, T>(
        fields: &'a Fields,
        root_attr: &SerryAttr,
        field_order: FieldOrder,
        accessor: impl Fn(&FieldName) -> T,
    ) -> (Predict, Option<ProcessedFields<'a>>)
    where
        T: ToTokens,
    {
        let processed_fields = match process_fields(fields, field_order) {
            Some(vec) => vec,
            None => return (Predict::default(), None),
        };
        let size_ident = get_size_ident();

        let vec: Vec<Predict> = processed_fields
            .iter()
            .map(|(field_name, field)| {
                let field_attr =
                    match find_and_parse_serry_attr(&field.attrs, SerryAttrFields::field()) {
                        Ok(attr) => attr,
                        Err(e) => return Predict::one( e.to_compile_error()),
                    };

                let version = match root_attr.version_with_range_of(&field_attr) {
                    Ok(version) => version,
                    Err(e) => return Predict::one(e.to_compile_error())
                };

                if let Some((root_info, field_range)) = version {
                    if field_range
                        .until
                        .map_or(false, |v| v < root_info.current_version)
                    {
                        return Predict::default();
                    }
                }
                let ty = field.ty.clone();
                let output_ident = accessor(field_name);
                Predict {
                    on_self: quote_spanned! { field.span() =>
                        let #size_ident = #size_ident + ::serry::repr::SerrySized::predict_size(#output_ident);
                    },
                    constant: quote_spanned! { field.span() =>
                        let #size_ident = #size_ident + match <#ty as ::serry::repr::SerrySized>::predict_constant_size() {
                            Some(v) => v,
                            None => return None
                        };
                    },
                    constant_unchecked: quote_spanned!(field.span() => {
                        let #size_ident = #size_ident + <#ty as ::serry::repr::SerrySized>::predict_constant_size_unchecked();
                    }),
                }
            })
            .collect();

        let mut on_self = TokenStream::new();
        let mut constant = TokenStream::new();
        let mut constant_unchecked = TokenStream::new();
        for i in vec {
            on_self.extend(i.on_self);
            constant.extend(i.constant);
            constant_unchecked.extend(i.constant_unchecked);
        }

        (
            Predict {
                on_self,
                constant,
                constant_unchecked,
            },
            Some(processed_fields),
        )
    }

    let mut predict_on_self = TokenStream::new();
    let mut predict_constant = TokenStream::new();
    let mut predict_constant_unchecked = TokenStream::new();

    let mut variant_discriminants = TokenStream::new();

    fn add_version(
        attr: &SerryAttr,
        output: &mut [&mut TokenStream],
        input: Option<&DeriveInput>,
    ) -> Result<(), Error> {
        if let Some(info) = &attr.version_info {
            if let Some(input) = input {
                match input.data {
                    Data::Enum(_) => {
                        return Err(Error::new_spanned(input, "Enums do not support versioning - versioning must be specified for each variant"));
                    }
                    _ => {}
                }
            }

            let ty = &info.version_type;
            let size_ident = get_size_ident();
            for i in output {
                i.extend(quote! {
                    let #size_ident = #size_ident + <#ty as ::serry::repr::SerrySized>::predict_constant_size_unchecked();
                });
            }
        }
        Ok(())
    }

    add_version(
        &root_attr,
        &mut [
            &mut predict_on_self,
            &mut predict_constant,
            &mut predict_constant_unchecked,
        ],
        Some(&input),
    )?;

    let field_order = root_attr.field_order.unwrap_or_default();
    match &input.data {
        Data::Struct(model) => {
            let accessor = |value: &FieldName| quote!(&self.#value);
            let (predict, _) = sized_fields(&model.fields, &root_attr, field_order, accessor);
            predict_on_self.extend(predict.on_self);

            let constant = predict.constant;
            predict_constant = quote! {
                let #size_ident = 0usize;
                #predict_constant
                #constant

                Some(#size_ident)
            };
            let constant_unchecked = predict.constant_unchecked;
            predict_constant_unchecked = quote! {
                let #size_ident = 0usize;
                #predict_constant_unchecked
                #constant_unchecked

                #size_ident
            };
        }
        Data::Enum(model) => {
            let accessor = |v: &FieldName| v.output_ident();
            let discriminant_type = root_attr
                .discriminant_type
                .clone()
                .unwrap_or_else(util::default_discriminant_type);
            let variants = enumerate_variants(&discriminant_type, model.variants.iter())?;
            let cases = variants.iter().map(|var| {
                let variant_ident = &var.variant.ident;
                let discriminant = &var.discriminant;
                let discriminant_ident = util::discriminant_name(&variant_ident);
                let field_order = var.attr.field_order.unwrap_or(field_order);
                let (predict, fields) =
                    sized_fields(&var.variant.fields, &var.attr, field_order, accessor);

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

                let mut predict_size = TokenStream::new();
                add_version(&var.attr, &mut [&mut predict_size], None).unwrap();
                predict_size.extend(predict.on_self);

                variant_discriminants.extend(quote! {
                    #[allow(all)]
                    const #discriminant_ident: #discriminant_type = { #discriminant };
                });
                quote!(Self::#variant_ident #params => {
                    let #size_ident = 0usize;
                    let #size_ident = #size_ident + #discriminant_ident.predict_size();
                    #predict_size
                    #size_ident
                })
            });
            predict_on_self.extend(quote! {
                let #size_ident = #size_ident + match self {
                    #(#cases),*,
                    _ => panic!("Unexpected enum variant")
                };
            });
            predict_constant.extend(quote! { None });
            predict_constant_unchecked.extend(quote! { panic!("Enums cannot have a constant size determined automatically (yet)") })
        }
        // Data::Union(_) => return Err(Error::new_spanned(input, "unions are not supported")),
        _ => {
            return Err(Error::new_spanned(
                input,
                "unsupported type - you can only derive SerryWrite for structs and enums",
            ))
        }
    }

    Ok(quote! {
        const _: () = {
            #variant_discriminants

            #[allow(all)]
            #[automatically_derived]
            impl #impl_generics ::serry::repr::SerrySized for #ident #ty_generics #where_clause {
                fn predict_size(&self) -> usize {
                    let #size_ident = 0usize;
                    #predict_on_self

                    #size_ident
                }

                fn predict_constant_size() -> Option<usize> {
                    #predict_constant
                }

                fn predict_constant_size_unchecked() -> usize {
                    #predict_constant_unchecked
                }
            }
        };
    }
    .into())
}
