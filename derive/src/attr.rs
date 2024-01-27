pub mod bounds;
pub mod extrapolate;
pub mod field_order;
pub mod version;

pub use crate::attr::bounds::Bounds;
pub use crate::attr::extrapolate::Extrapolate;
pub use crate::attr::field_order::FieldOrder;
pub use crate::attr::version::{RootVersionInfo, VersionRange};
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::{parenthesized, parse_quote, Attribute, Data, Error, Expr, Token, Type};

pub struct SerryAttr<'a> {
    pub version_info: Option<RootVersionInfo>,
    pub version_range: Option<VersionRange>,
    pub extrapolate: Option<Extrapolate>,
    pub discriminant_value: Option<Expr>,
    pub discriminant_type: Option<Type>,
    pub field_order: Option<FieldOrder>,
    pub bounds: Bounds,
    pub attr: Option<&'a Attribute>,
}

impl<'a> SerryAttr<'a> {
    pub fn version_with_range_of<'all>(
        &'all self,
        field_attr: &'all SerryAttr,
    ) -> syn::Result<Option<(&'all RootVersionInfo, &'all VersionRange)>> {
        Ok(match (&self.version_info, &field_attr.version_range) {
            (Some(info), Some(range)) => {
                if let Some(until) = range.until {
                    if info.current_version > until && field_attr.extrapolate.is_none() {
                        return Err(Error::new(
                            field_attr.span(),
                            "extrapolate is required if version has upper limit",
                        ));
                    }
                }
                Some((info, range))
            }
            (None, Some(_)) => {
                return Err(Error::new(
                    field_attr.span(),
                    "field has version range, but structure does not",
                ));
            }
            (Some(_), None) => {
                return Err(Error::new(
                    field_attr.span(),
                    "structure has versioning, but field does not",
                ));
            }
            (None, None) => None,
        })
    }
}
impl<'a> ToTokens for SerryAttr<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.attr.to_tokens(tokens)
    }
    fn to_token_stream(&self) -> TokenStream {
        self.attr.to_token_stream()
    }
    fn into_token_stream(self) -> TokenStream
    where
        Self: Sized,
    {
        self.attr.into_token_stream()
    }
}

impl<'a> Default for SerryAttr<'a> {
    fn default() -> Self {
        Self {
            version_info: None,
            version_range: None,
            extrapolate: None,
            discriminant_value: None,
            discriminant_type: None,
            field_order: None,
            bounds: Bounds::default(),
            attr: None,
        }
    }
}

#[derive(Copy, Clone)]
pub struct SerryAttrFields {
    version: SerryAttrVersionField,
    extrapolate: bool,
    discriminate_by: bool,
    discriminator: bool,
    field_order: bool,
    bounds: bool,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum SerryAttrVersionField {
    None,
    Init,
    Range,
}

impl Default for SerryAttrFields {
    fn default() -> Self {
        Self {
            version: SerryAttrVersionField::None,
            extrapolate: false,
            discriminate_by: false,
            discriminator: false,
            field_order: false,
            bounds: false,
        }
    }
}

impl SerryAttrFields {
    pub fn struct_def() -> Self {
        Self {
            version: SerryAttrVersionField::Init,
            field_order: true,
            bounds: true,
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
    pub fn enum_def() -> Self {
        Self {
            // TODO: Right now enums don't support versioning in favour of being able to version each variant separately.
            discriminate_by: true,
            bounds: true,
            ..Self::default()
        }
    }
    pub fn enum_variant() -> Self {
        Self {
            version: SerryAttrVersionField::Init,
            discriminator: true,
            ..Self::default()
        }
    }
}

pub fn parse_serry_attr(attr: &Attribute, fields: SerryAttrFields) -> Result<SerryAttr, Error> {
    let mut version_info = None;
    let mut version_range = None;
    let mut extrapolate = None;
    let mut discriminant_type = None;
    let mut discriminant_value = None;
    let mut field_order = None;
    let mut bounds = None;
    attr.parse_nested_meta(|meta| match &meta.path {
        path if fields.version != SerryAttrVersionField::None
            && version_range.is_none()
            && version_info.is_none()
            && path.is_ident("version") =>
        {
            match fields.version {
                SerryAttrVersionField::None => panic!("Logical impossibility has occurred"),
                SerryAttrVersionField::Init => {
                    let version_meta;
                    parenthesized!(version_meta in meta.input);
                    let value: VersionRange = version_meta.parse()?;

                    let ty = if version_meta.peek(Token![as]) {
                        version_meta.parse::<Token![as]>()?;
                        version_meta.parse()?
                    } else {
                        parse_quote!(u8)
                    };

                    let current_version = value.until.unwrap_or(value.since);
                    let minimum_supported_version = value.since;

                    version_info = Some(RootVersionInfo {
                        minimum_supported_version,
                        current_version,
                        version_type: ty,
                    });

                    Ok(())
                }
                SerryAttrVersionField::Range => {
                    version_range = Some(if meta.input.peek(Token![=]) {
                        let value = meta.value()?;
                        value.parse()?
                    } else {
                        let version_meta;
                        parenthesized!(version_meta in meta.input);
                        version_meta.parse()?
                    });
                    Ok(())
                }
            }
        }
        path if fields.extrapolate && extrapolate.is_none() && path.is_ident("extrapolate") => {
            let value = meta.value()?;
            extrapolate = Some(Extrapolate::Function(value.parse()?));
            Ok(())
        }
        path if fields.extrapolate && extrapolate.is_none() && path.is_ident("default") => {
            extrapolate = Some(Extrapolate::Default);
            Ok(())
        }
        path if fields.discriminate_by
            && discriminant_type.is_none()
            && (path.is_ident("discriminate_by") || path.is_ident("repr")) =>
        {
            let value;
            parenthesized!(value in meta.input);

            let ty = value.parse()?;
            discriminant_type = Some(ty);

            Ok(())
        }
        path if fields.discriminator
            && discriminant_value.is_none()
            && (path.is_ident("discriminant") || path.is_ident("repr")) =>
        {
            let value = meta.value()?;

            let type_path = value.parse()?;
            discriminant_value = Some(type_path);

            Ok(())
        }
        path if fields.field_order && field_order.is_none() && path.is_ident("field_order") => {
            let value = meta.value()?;
            field_order = Some(value.parse()?);

            Ok(())
        }
        path if fields.bounds && bounds.is_none() && path.is_ident("bound") => {
            if meta.input.lookahead1().peek(Token![=]) {
                let value = meta.value()?;
                bounds = Some(value.parse()?);
            } else {
                let parse_buffer;
                parenthesized!(parse_buffer in meta.input);
                bounds = Some(parse_buffer.parse()?);
            }
            Ok(())
        }
        other => Err(meta.error(format_args!(
            "unexpected attribute '{}'",
            other.to_token_stream()
        ))),
    })?;
    Ok(SerryAttr {
        version_info,
        version_range,
        extrapolate,
        discriminant_type,
        discriminant_value,
        field_order,
        bounds: bounds.unwrap_or_default(),
        attr: Some(attr),
    })
}

pub fn find_and_parse_serry_attr_auto<'a>(
    attrs: &'a Vec<Attribute>,
    type_data: &'_ Data,
) -> Result<SerryAttr<'a>, Error> {
    find_and_parse_serry_attr(
        attrs,
        match type_data {
            Data::Struct(_) => SerryAttrFields::struct_def(),
            Data::Enum(_) => SerryAttrFields::enum_def(),
            _ => {
                return Err(Error::new(
                    Span::call_site(),
                    "cannot derive for types other than structs and enums",
                ));
            }
        },
    )
}
pub fn find_and_parse_serry_attr(
    attrs: &Vec<Attribute>,
    fields: SerryAttrFields,
) -> Result<SerryAttr, Error> {
    let serry_attr: Vec<_> = attrs
        .iter()
        .filter(|v| v.path().is_ident("serry"))
        .collect();
    if serry_attr.len() > 1 {
        /*for i in 1..serry_attr.len() {
            let attr = serry_attr[i];
            errors.extend(quote_spanned!(attr.span() => compile_error!("Only one Serry attribute per item")));
        }*/
        return Err(Error::new(
            attrs.first().map_or_else(Span::call_site, Attribute::span),
            "more than one serry attribute",
        ));
    }
    let serry_attr = serry_attr.into_iter().nth(0);
    serry_attr
        .map(|v| parse_serry_attr(v, fields))
        .unwrap_or(Ok(SerryAttr::default()))
}
