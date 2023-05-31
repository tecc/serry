use proc_macro2::{TokenStream, Ident, Span};
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_macro_input, DeriveInput, spanned::Spanned, WherePredicate, PredicateType, Data, Fields, Field, Attribute, Lit, LitInt, parenthesized, Token, Error, Path, Type, TypePath, parse_quote, TypeReference};
use syn::parse::{ParseBuffer, ParseStream};

struct RootVersionInfo {
    current_version: usize,
    version_type: Type,
}

#[derive(Default, Copy, Clone, Debug)]
struct VersionRange {
    since: usize,
    until: Option<usize>,
}

struct SerryAttr<'a> {
    version_info: Option<RootVersionInfo>,
    version_range: Option<VersionRange>,
    extrapolate: Option<Extrapolate>,
    attr: Option<&'a Attribute>,
}

enum Extrapolate {
    Default,
    Function(Path),
}

impl<'a> ToTokens for SerryAttr<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.attr.to_tokens(tokens)
    }
    fn to_token_stream(&self) -> TokenStream {
        self.attr.to_token_stream()
    }
    fn into_token_stream(self) -> TokenStream where Self: Sized {
        self.attr.into_token_stream()
    }
}

impl<'a> Default for SerryAttr<'a> {
    fn default() -> Self {
        Self {
            version_info: None,
            version_range: None,
            extrapolate: None,
            attr: None,
        }
    }
}

#[derive(Copy, Clone)]
struct SerryAttrFields {
    version: SerryAttrVersionField,
    extrapolate: bool,
}

impl Default for SerryAttrFields {
    fn default() -> Self {
        Self {
            version: SerryAttrVersionField::None,
            extrapolate: false,
        }
    }
}

impl SerryAttrFields {
    pub fn root() -> Self {
        Self {
            version: SerryAttrVersionField::Init,
            ..Self::default()
        }
    }
    pub fn field() -> Self {
        Self {
            version: SerryAttrVersionField::Range,
            extrapolate: true,
            ..Self::default()
        }
    }
}

#[derive(Copy, Clone)]
enum SerryAttrVersionField {
    None,
    Init,
    Range,
}

fn parse_serry_attr(attr: &Attribute, fields: SerryAttrFields) -> Result<SerryAttr, Error> {
    let mut version_info = None;
    let mut version_range = None;
    let mut extrapolate = None;
    attr.parse_nested_meta(|mut meta| {
        match &meta.path {
            path if path.is_ident("version") && version_range.is_none() && version_info.is_none() => match fields.version {
                SerryAttrVersionField::None => {
                    return Err(meta.error("unexpected attribute 'version'"));
                }
                SerryAttrVersionField::Init => {
                    let version_meta;
                    parenthesized!(version_meta in meta.input);
                    let value: syn::LitInt = version_meta.parse()?;

                    let ty = if version_meta.peek(Token![as]) {
                        version_meta.parse::<Token![as]>()?;
                        version_meta.parse()?
                    } else {
                        parse_quote!(u8)
                    };
                    version_info = Some(RootVersionInfo {
                        current_version: value.base10_parse()?,
                        version_type: ty,
                    });

                    Ok(())
                }
                SerryAttrVersionField::Range => {
                    let parse_version_set = |input: ParseStream| {
                        let since: LitInt = input.parse()?;
                        let since = since.base10_parse()?;

                        let until = if input.peek(Token![..]) {
                            let _ = input.parse::<Token![..]>();
                            let until: LitInt = input.parse()?;
                            Some(until.base10_parse()?)
                        } else { None };

                        Ok::<_, Error>(VersionRange {
                            since,
                            until,
                        })
                    };
                    version_range = Some(if meta.input.peek(Token![=]) {
                        let value = meta.value()?;
                        parse_version_set(value)?
                    } else {
                        let version_meta;
                        parenthesized!(version_meta in meta.input);
                        parse_version_set(&version_meta)?
                    });
                    Ok(())
                }
            },
            path if fields.extrapolate && path.is_ident("extrapolate") && extrapolate.is_none() => {
                let value = meta.value()?;
                extrapolate = Some(Extrapolate::Function(value.parse()?));
                Ok(())
            }
            path if fields.extrapolate && path.is_ident("default") && extrapolate.is_none() => {
                extrapolate = Some(Extrapolate::Default);
                Ok(())
            }
            other => return Err(meta.error(format_args!("unexpected attribute '{}'", other.to_token_stream())))
        }
    })?;
    Ok(SerryAttr {
        version_info,
        version_range,
        extrapolate,
        attr: Some(attr),
    })
}

fn find_and_parse_serry_attr(attrs: &Vec<Attribute>, fields: SerryAttrFields) -> Result<SerryAttr, Error> {
    let serry_attr: Vec<_> = attrs.iter().filter(|v| v.path().is_ident("serry")).collect();
    if serry_attr.len() > 1 {
        /*for i in 1..serry_attr.len() {
            let attr = serry_attr[i];
            errors.extend(quote_spanned!(attr.span() => compile_error!("Only one Serry attribute per item")));
        }*/
        return Err(syn::Error::new(attrs.first().map_or_else(Span::call_site, Attribute::span), "more than one serry attribute"));
    }
    let serry_attr = serry_attr.into_iter().nth(0);
    serry_attr.map(|v| parse_serry_attr(v, fields)).unwrap_or(Ok(SerryAttr::default()))
}

fn derive_write_impl(input: DeriveInput) -> Result<TokenStream, Error> {
    let root_attr = find_and_parse_serry_attr(&input.attrs, SerryAttrFields::root())?;

    let ident = input.ident.clone();
    let mut generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    fn serialise_fields(fields: Fields, root_attr: &SerryAttr) -> TokenStream {
        match fields {
            Fields::Unit => quote!().into_token_stream(),
            Fields::Named(named) => {
                let mut vec: Vec<(Ident, Field)> = vec![];
                for field in named.named {
                    vec.push((
                        field.ident.clone().unwrap(),
                        field
                    ));
                }

                vec.sort_by(|a, b| a.0.cmp(&b.0));

                let vec: Vec<_> = vec.into_iter()
                    .map(|(ident, field)| {
                        let field_attr = match find_and_parse_serry_attr(&field.attrs, SerryAttrFields::field()) {
                            Ok(attr) => attr,
                            Err(e) => return e.to_compile_error()
                        };

                        let version = match (&root_attr.version_info, &field_attr.version_range) {
                            (Some(info), Some(range)) => {
                                if let Some(until) = range.until {
                                    if info.current_version > until && field_attr.extrapolate.is_none() {
                                        return Error::new(field_attr.span(), "extrapolate is required if version has upper limit")
                                            .to_compile_error();
                                    }
                                }
                                Some((info, range))
                            }
                            (None, Some(_)) => return Error::new(field_attr.span(), "field has version range, but structure does not").to_compile_error(),
                            (Some(_), None) => return Error::new(field_attr.span(), "structure has versioning, but field does not").to_compile_error(),
                            (None, None) => None
                        };

                        if let Some((root_info, field_range)) = version {
                            if field_range.until.map_or(false, |v| v < root_info.current_version) {
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
                }).into()
            }
            Fields::Unnamed(unnamed) => {
                let fields = unnamed.unnamed;
                todo!()
            }
        }
    }

    let mut field_serialise = TokenStream::new();

    if let Some(info) = &root_attr.version_info {
        let ty = &info.version_type;
        let version = info.current_version;
        field_serialise.extend(quote! {
            output.write_value::<#ty>(#version as #ty);
        })
    }

    field_serialise.extend(match input.data {
        Data::Struct(model) => {
            serialise_fields(model.fields, &root_attr)
        }
        _ => todo!()
    });

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

#[proc_macro_derive(SerryWrite, attributes(serry))]
pub fn derive_write(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    match derive_write_impl(item) {
        Ok(output) => output,
        Err(e) => e.to_compile_error()
    }.into()
}

fn version_ident() -> Ident {
    Ident::new("__VERSION", Span::call_site())
}

fn derive_read_impl(input: DeriveInput) -> Result<TokenStream, syn::Error> {
    let root_attr = find_and_parse_serry_attr(&input.attrs, SerryAttrFields::root())?;

    let version_id = version_ident();

    let ident = input.ident.clone();
    let mut generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    fn deserialise_fields(fields: Fields, root_attr: &SerryAttr) -> TokenStream {
        fn deserialise_field(field: &Field, output_ident: &Ident, root_attr: &SerryAttr) -> TokenStream {
            let field_attr = match find_and_parse_serry_attr(&field.attrs, SerryAttrFields::field()) {
                Ok(attr) => attr,
                Err(e) => return e.to_compile_error()
            };

            let version = match (&root_attr.version_info, &field_attr.version_range) {
                (Some(info), Some(range)) => {
                    if let Some(until) = range.until {
                        if info.current_version > until && field_attr.extrapolate.is_none() {
                            return Error::new(field_attr.span(), "extrapolate is required if version has upper limit")
                                .to_compile_error();
                        }
                    }
                    Some((info, range))
                }
                (None, Some(_)) => return Error::new(field_attr.span(), "field has version range, but structure does not").to_compile_error(),
                (Some(_), None) => return Error::new(field_attr.span(), "structure has versioning, but field does not").to_compile_error(),
                (None, None) => None
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
                let extrapolate = field_attr.extrapolate.as_ref()
                    .map_or(quote!(panic!("Cannot extrapolate field");), |v| {
                        match v {
                            Extrapolate::Default => {
                                quote!(<#ty as ::core::default::Default>::default())
                            }
                            Extrapolate::Function(path) => {
                                quote!(#path())
                            }
                        }
                    });
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
                let mut vec: Vec<(Ident, Field)> = vec![];
                for field in named.named {
                    vec.push((
                        field.ident.clone().unwrap(),
                        field
                    ));
                }

                vec.sort_by(|a, b| a.0.cmp(&b.0));

                let reading: Vec<_> = vec.iter()
                    .map(|(ident, field)| deserialise_field(field, ident, root_attr))
                    .collect();

                let identifiers = vec.iter().map(|v| &v.0);

                (quote! {
                    #(#reading)*;
                    Ok(Self {
                        #(#identifiers),*
                    })
                }).into()
            }
            Fields::Unnamed(unnamed) => {
                let fields = unnamed.unnamed;

                let fields: Vec<_> = fields.iter()
                    .enumerate()
                    .map(|(i, field)| {
                        let name = format!("__field_{}", i);
                        let ident = Ident::new(name.as_str(), Span::call_site());
                        (deserialise_field(field, &ident, root_attr), ident, field)
                    })
                    .collect();

                let reading = fields.iter()
                    .map(|data| &data.0);
                let props = fields.iter().map(|v| &v.1);

                quote! {
                    #(#reading);*
                    Ok(Self(#(#props),*))
                }
            }
        }
    }

    let mut field_deserialise = TokenStream::new();

    if let Some(info) = &root_attr.version_info {
        let ty = &info.version_type;
        let version = info.current_version;
        field_deserialise.extend(quote! {
            let #version_id: #ty = input.read_value()?;
        })
    }

    field_deserialise.extend(match input.data {
        Data::Struct(model) => {
            deserialise_fields(model.fields, &root_attr)
        }
        Data::Enum(model) => {
            let open = quote!();
            todo!("Enum discriminators are not yet implemented");
        }
        other => todo!()
    });

    Ok(quote! {
        const _: () = {
            impl #impl_generics ::serry::read::SerryRead for #ident #ty_generics #where_clause {
                fn serry_read(input: &mut impl ::serry::read::SerryInput) -> ::serry::read::ReadResult<Self> {
                    #field_deserialise
                }
            }
        };
    }.into())
}

#[proc_macro_derive(SerryRead, attributes(serry))]
pub fn derive_read(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    match derive_read_impl(item) {
        Ok(output) => output,
        Err(e) => e.to_compile_error()
    }.into()
}