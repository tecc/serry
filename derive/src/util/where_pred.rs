use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::{parse_quote, Generics, Token, WhereClause, WherePredicate};

pub type WherePredicates = Punctuated<WherePredicate, Token![,]>;
pub fn create_where_clause(
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
pub fn generate_where_clause(
    required_traits: TokenStream,
    generics: &Generics,
) -> Option<WhereClause> {
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
pub fn combine_where_clause(bounds: &WherePredicates, generics: &Generics) -> Option<WhereClause> {
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
