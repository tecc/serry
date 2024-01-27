use crate::util;
use crate::util::{parse_predicate, SynErrorExt, WherePredicates};
use fancy_regex::Regex;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::Error;
use syn::{spanned::Spanned, LitStr, MetaNameValue, Token, TypeParamBound, WherePredicate};

#[derive(Default)]
pub struct Bounds {
    pub read: Option<WherePredicates>,
    pub write: Option<WherePredicates>,
    pub sized: Option<WherePredicates>,
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
        let predicate_str_original: LitStr = input.parse()?;
        let predicate_str = predicate_str_original.value();
        let regex = Regex::new(r"(?<!\w)_(?!\w)").unwrap();

        let placeholder = util::placeholder_ident();
        let placeholder_str = placeholder.to_string();

        let predicate_replaced = regex.replace_all(&predicate_str, &placeholder_str);
        let predicate_str = LitStr::new(&predicate_replaced, predicate_str_original.span());
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
