use std::cmp::Ordering;
use std::collections::HashSet;

use crate::util::{parse_predicate, SynErrorExt};
use fancy_regex::Regex;
use proc_macro2::TokenTree::Punct;
use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::ToTokens;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Where;
use syn::{
    parenthesized, parse_macro_input, parse_quote, spanned::Spanned, Attribute, Data, DeriveInput,
    Error, Expr, ExprLit, Generics, Lit, LitInt, MetaList, MetaNameValue, Path, Token, Type,
    TypeParamBound, TypePath, WhereClause, WherePredicate,
};
use syn::{Field, Fields, LitStr, Variant};

#[macro_use]
extern crate quote;

mod read;
mod sized;
mod util;
mod write;

#[proc_macro_derive(SerryRead, attributes(serry))]
pub fn derive_read(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    match read::derive_read_impl(&item) {
        Ok(output) => output,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

#[proc_macro_derive(SerryWrite, attributes(serry))]
pub fn derive_write(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    match write::derive_write_impl(&item) {
        Ok(output) => output,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

#[proc_macro_derive(SerrySized, attributes(serry))]
pub fn derive_sized(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    sized::derive_sized_impl(&item)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

#[proc_macro_derive(SerryTraits, attributes(serry))]
pub fn derive_all(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);

    let mut output = TokenStream::new();
    let mut extend = |result: Result<_, Error>| match result {
        Ok(content) => output.extend(content),
        Err(e) => output.extend(e.to_compile_error()),
    };

    extend(read::derive_read_impl(&item));
    extend(write::derive_write_impl(&item));
    extend(sized::derive_sized_impl(&item));

    output.into()
}

type ProcessedFields<'a> = Vec<(FieldName, &'a Field)>;
fn process_fields(fields: &Fields, field_order: FieldOrder) -> Option<ProcessedFields> {
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

fn create_pattern_match<'a, I>(iter: I, unnamed: bool) -> TokenStream
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

struct RootVersionInfo {
    minimum_supported_version: usize,
    current_version: usize,
    version_type: Type,
}

#[derive(Default, Copy, Clone, Debug)]
struct VersionRange {
    since: usize,
    until: Option<usize>,
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

struct SerryAttr<'a> {
    version_info: Option<RootVersionInfo>,
    version_range: Option<VersionRange>,
    extrapolate: Option<Extrapolate>,
    discriminant_value: Option<Expr>,
    discriminant_type: Option<Type>,
    field_order: Option<FieldOrder>,
    bounds: Bounds,
    attr: Option<&'a Attribute>,
}

impl<'a> SerryAttr<'a> {
    fn version_with_range_of<'all>(
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

#[derive(Copy, Clone)]
enum FieldOrder {
    Alphabetical,
    AsSpecified,
}

impl Default for FieldOrder {
    fn default() -> Self {
        FieldOrder::AsSpecified
    }
}

impl FieldOrder {
    fn do_sort(&self) -> bool {
        match self {
            Self::AsSpecified => false,
            _ => true,
        }
    }

    fn cmp(&self, a: &Field, b: &Field) -> Ordering {
        match self {
            Self::AsSpecified => Ordering::Equal,
            Self::Alphabetical => a.ident.cmp(&b.ident),
        }
    }
}

impl Parse for FieldOrder {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let str: LitStr = input.parse()?;
        Ok(match str.value().to_lowercase().as_str() {
            "alphabetical" => Self::Alphabetical,
            "as_specified" => Self::AsSpecified,
            _ => {
                return Err(Error::new_spanned(
                    str,
                    "invalid field order - must be either 'alphabetical' or 'as_specified'",
                ))
            }
        })
    }
}

enum Extrapolate {
    Default,
    Function(Path),
}

type WherePredicates = Punctuated<WherePredicate, Token![,]>;
#[derive(Default)]
struct Bounds {
    read: Option<WherePredicates>,
    write: Option<WherePredicates>,
    sized: Option<WherePredicates>,
}
impl Bounds {
    pub fn parse_list(input: ParseStream) -> syn::Result<Self> {
        let list: Punctuated<MetaNameValue, Token![,]> = Punctuated::parse_terminated(input)?;

        let mut read = None;
        let mut write = None;
        let mut sized = None;

        let mut error = None;
        for pair in list.iter() {
            macro_rules! match_pair {
                ($($output:ident($ident:literal)),*) => {
                    match &pair.path {
                        $(path if $output.is_none() && path.is_ident($ident) => {
                            let str: LitStr = match syn::parse2(pair.value.to_token_stream()) {
                                Ok(value) => value,
                                Err(e) => {
                                    error = Some(e.maybe_combined(error));
                                    continue;
                                },
                            };
                            let predicate = match parse_predicate(&str) {
                                Ok(value) => value,
                                Err(e) => {
                                    error = Some(e.maybe_combined(error));
                                    continue;
                                }
                            };
                            $output = Some(predicate);
                        }),*,
                        path => {
                            let e = Error::new(
                                pair.span(),
                                format!(
                                    "unknown or duplicate key '{}'",
                                    path.to_token_stream().to_string()
                                ),
                            );
                            error = Some(e.maybe_combined(error));
                            continue;
                        }
                    }
                };
            }

            match_pair!(read("read"), write("write"), sized("sized"));
        }

        if let Some(error) = error {
            return Err(error);
        }

        Ok(Self { read, write, sized })
    }

    pub fn parse_any(input: ParseStream) -> syn::Result<Self> {
        // I know the implementation is ugly with regexs and all, but it's better than nothing.
        // It's a workaround for WherePredicate not allowing keywords, which is kind of annoying.
        let predicate_str: LitStr = input.parse()?;
        let predicate_str = predicate_str.value();
        let regex = Regex::new(r"(?<!\w)_(?!\w)").unwrap();

        let placeholder = util::placeholder_ident();
        let placeholder_str = placeholder.to_string();

        let predicate_replaced = regex.replace_all(&predicate_str, &placeholder_str);
        let predicate_str = LitStr::new(&predicate_replaced, predicate_str.span());
        let predicates: WherePredicates = parse_predicate(&predicate_str)?;

        let mut read: WherePredicates = WherePredicates::new();
        let mut write: WherePredicates = WherePredicates::new();
        let mut sized: WherePredicates = WherePredicates::new();
        for mut pair in predicates.into_pairs() {
            let predicate = pair.value_mut();

            let placeholder = util::placeholder_ident();

            let read_pred: WherePredicate;
            let write_pred: WherePredicate;
            let sized_pred: WherePredicate;
            match predicate {
                WherePredicate::Type(ty_predicate) => {
                    let mut placeholders = Vec::new();
                    for bound in ty_predicate.bounds.iter_mut() {
                        match bound {
                            TypeParamBound::Trait(tr) => {
                                if tr.path.is_ident(&placeholder) {
                                    placeholders.push(std::ptr::addr_of_mut!(tr.path));
                                }
                            }
                            _ => {}
                        }
                    }
                    macro_rules! set_placeholder {
                        ($new:expr) => {
                            unsafe {
                                let new = $new;
                                for placeholder in placeholders.iter() {
                                    **placeholder = new.clone();
                                }
                            }
                        };
                    }
                    // This might be A Very Bad Thing To Do. But it should work. Hopefully.
                    set_placeholder!(util::trait_read_path());
                    read_pred = predicate.clone();
                    set_placeholder!(util::trait_write_path());
                    write_pred = predicate.clone();
                    set_placeholder!(util::trait_sized_path());
                    sized_pred = predicate.clone();
                }
                _ => {
                    read_pred = predicate.clone();
                    write_pred = predicate.clone();
                    sized_pred = predicate.clone();
                }
            }

            read.push_value(read_pred);
            write.push_value(write_pred);
            sized.push_value(sized_pred);

            if let Some(comma) = pair.punct() {
                read.push_punct(comma.clone());
                write.push_punct(comma.clone());
                sized.push_punct(comma.clone());
            }
        }

        Ok(Self {
            read: Some(read),
            write: Some(write),
            sized: Some(sized),
        })
    }
}
impl Parse for Bounds {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(Ident::peek_any) {
            Self::parse_list(input)
        } else {
            Self::parse_any(input)
        }
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
struct SerryAttrFields {
    version: SerryAttrVersionField,
    extrapolate: bool,
    discriminate_by: bool,
    discriminator: bool,
    field_order: bool,
    bounds: bool,
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

#[derive(Copy, Clone, Eq, PartialEq)]
enum SerryAttrVersionField {
    None,
    Init,
    Range,
}

fn parse_serry_attr(attr: &Attribute, fields: SerryAttrFields) -> Result<SerryAttr, Error> {
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

fn find_and_parse_serry_attr(
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

fn find_and_parse_serry_attr_auto<'a>(
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

struct AnnotatedVariant<'a> {
    pub variant: &'a Variant,
    pub attr: SerryAttr<'a>,
    pub discriminant: Expr,
}

fn enumerate_variants<'a, I>(
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

enum FieldName {
    Ident(Ident),
    Index(LitInt),
}
impl ToTokens for FieldName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self {
            Self::Ident(ident) => ident.to_tokens(tokens),
            Self::Index(index) => index.to_tokens(tokens),
        }
    }
}
impl FieldName {
    fn output_ident(&self) -> Ident {
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

fn create_where_clause(
    bounds: &Option<WherePredicates>,
    required_traits: impl ToTokens,
    generics: &Generics,
) -> Option<WhereClause> {
    if let Some(bounds) = bounds {
        combine_where_clause(bounds, generics)
    } else {
        generate_where_clause(required_traits.to_token_stream(), generics)
    }
}
fn generate_where_clause(required_traits: TokenStream, generics: &Generics) -> Option<WhereClause> {
    let mut custom_bounds: Vec<WherePredicate> = Vec::new();

    for generic in generics.type_params() {
        let name = &generic.ident;
        custom_bounds.push(parse_quote!(#name: #required_traits));
    }

    let mut clause = generics.where_clause.clone();
    if custom_bounds.len() < 1 {
        return clause;
    }
    if let Some(clause) = &mut clause {
        clause.predicates.extend(custom_bounds.into_iter());
    } else {
        clause = Some(parse_quote!(where #(#custom_bounds),*));
    }
    clause
}
fn combine_where_clause(bounds: &WherePredicates, generics: &Generics) -> Option<WhereClause> {
    let mut custom_bounds: Vec<WherePredicate> = Vec::new();

    for predicate in bounds {
        custom_bounds.push(predicate.clone());
    }

    let mut clause = generics.where_clause.clone();
    if custom_bounds.len() < 1 {
        return clause;
    }
    if let Some(clause) = &mut clause {
        clause.predicates.extend(custom_bounds.into_iter());
    } else {
        clause = Some(parse_quote!(where #(#custom_bounds),*));
    }
    clause
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::read::derive_read_impl;
    use crate::sized::derive_sized_impl;
    use crate::write::derive_write_impl;
    use syn::{parse_quote, File};

    #[test]
    fn derive() {
        let check_validity = |input: TokenStream| {
            let file = syn::parse_file(&input.to_string()).expect("Failed parse");
            println!("{}", prettyplease::unparse(&file));
        };

        let derive_test = |content: &str| {
            let file = syn::parse_file(content).expect("Failed parse");
            let item = file.items.first().expect("First item required");
            let input =
                syn::parse2(item.to_token_stream()).expect("Could not parse file to DeriveInput");
            check_validity(derive_read_impl(&input).expect("Could not derive SerryRead"));
            check_validity(derive_write_impl(&input).expect("Could not derive SerryWrite"));
            check_validity(derive_sized_impl(&input).expect("Could not derive SerrySized"));
        };

        derive_test(
            "
            #[derive(SerryRead, SerryWrite, SerrySized)]
            enum StringDiscriminated {
                VariantOne,
                VariantTwo,
            }
        ",
        );
        derive_test(
            "
            #[derive(SerryRead, SerryWrite, SerrySized)]
            #[serry(repr(&'static str))]
            enum StringDiscriminated {
                VariantOne,
                VariantTwo,
            }
        ",
        );
        derive_test(
            "
            #[derive(SerryRead, SerryWrite, SerrySized)]
            #[serry(repr(u16))]
            enum IntDiscriminated {
                VariantOne,
                VariantTwo,
            }
        ",
        );
        derive_test(
            "
            #[derive(SerryTraits)]
            struct ThingWithTypeParams<K, V: ?Sized> {
                map: HashMap<K, V>,
            }
        ",
        );
        derive_test(
            "
            #[derive(SerryTraits)]
            #[serry(bound(\"T: _, T::Id: _\"))]
            struct IdMap<T>
            where
                T: IdTrait,
            {
                inner: HashMap<T::Id, T>,
            }
            trait IdTrait {
                type Id: Hash + Eq;
                fn id(&self) -> &Self::Id;
            }
        ",
        );
    }
}
