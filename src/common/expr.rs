use quote::{quote, ToTokens};
use syn::{parse_macro_input, parse_quote, spanned::Spanned, Attribute, Expr, Lit, LitBool, LitStr, Meta, MetaNameValue, Token, Type, WherePredicate};
use syn::parse::{ParseStream, Parser};
use syn::punctuated::Punctuated;
use crate::common::ident_bool::meta_name_value_2_bool;
use crate::common::where_predicates_bool::WherePredicates;
use super::path::path_to_string;

const INT_TYPES: [&str; 12] =
    ["u8", "u16", "u32", "u64", "u128", "usize", "i8", "i16", "i32", "i64", "i128", "isize"];

const FLOAT_TYPES: [&str; 2] = ["f32", "f64"];

#[inline]
pub(crate) fn meta_2_expr(meta: &Meta) -> syn::Result<Expr> {
    match &meta {
        Meta::NameValue(name_value) => Ok(name_value.value.clone()),
        Meta::List(list) => list.parse_args::<Expr>(),
        Meta::Path(path) => Err(syn::Error::new(
            path.span(),
            format!("expected `{path} = Expr` or `{path}(Expr)`", path = path_to_string(path)),
        )),
    }
}



#[inline]
pub(crate) fn meta_name_value_2_attrs(name_value: &MetaNameValue) -> syn::Result<Vec<Attribute>> {
    if let Expr::Lit(lit) = &name_value.value {
        if let Lit::Str(b) = &lit.lit {
            return b.parse_with(|input: ParseStream<'_>| {
                let mut attrs = vec![];

                while !input.is_empty() {
                    attrs.extend(input.call(Attribute::parse_outer)?);
                    if !input.is_empty() {
                        let _: Token![,] = input.parse()?;
                    }
                }

                Ok(attrs)
            });
        }
    }

    Err(syn::Error::new(
        name_value.value.span(),
        format!("expected `{path} = false`", path = path_to_string(&name_value.path)),
    ))
}

#[inline]
pub(crate) fn meta_2_attrs(meta: &Meta) -> syn::Result<Vec<Attribute>> {
    match &meta {
        Meta::NameValue(name_value) => meta_name_value_2_attrs(name_value),
        Meta::List(list) => {
            list.parse_args_with(|input: ParseStream<'_>| {
                let mut attrs = vec![];

                while !input.is_empty() {
                    attrs.extend(input.call(Attribute::parse_outer)?);
                    if !input.is_empty() {
                        let _: Token![,] = input.parse()?;
                    }
                }

                Ok(attrs)
            })
        }
        Meta::Path(path) => Err(syn::Error::new(
            path.span(),
            format!("expected `{path} = false` or `{path}(false)`", path = path_to_string(path)),
        )),
    }
}

#[inline]
pub(crate) fn auto_adjust_expr(expr: Expr, ty: Option<&Type>) -> Expr {
    match &expr {
        Expr::Lit(lit) => {
            match &lit.lit {
                Lit::Int(lit) => {
                    if let Some(Type::Path(ty)) = ty {
                        let ty_string = ty.into_token_stream().to_string();

                        if lit.suffix() == ty_string || INT_TYPES.contains(&ty_string.as_str()) {
                            // don't call into
                            return expr;
                        }
                    }
                }
                Lit::Float(lit) => {
                    if let Some(Type::Path(ty)) = ty {
                        let ty_string = ty.into_token_stream().to_string();

                        if lit.suffix() == ty_string || FLOAT_TYPES.contains(&ty_string.as_str()) {
                            // don't call into
                            return expr;
                        }
                    }
                }
                Lit::Str(_) => {
                    if let Some(Type::Reference(ty)) = ty {
                        let ty_string = ty.elem.clone().into_token_stream().to_string();

                        if ty_string == "str" {
                            // don't call into
                            return expr;
                        }
                    }
                }
                Lit::Bool(_) => {
                    if let Some(Type::Path(ty)) = ty {
                        let ty_string = ty.into_token_stream().to_string();

                        if ty_string == "bool" {
                            // don't call into
                            return expr;
                        }
                    }
                }
                Lit::Char(_) => {
                    if let Some(Type::Path(ty)) = ty {
                        let ty_string = ty.into_token_stream().to_string();

                        if ty_string == "char" {
                            // don't call into
                            return expr;
                        }
                    }
                }
                Lit::Byte(_) => {
                    if let Some(Type::Path(ty)) = ty {
                        let ty_string = ty.into_token_stream().to_string();

                        if ty_string == "u8" {
                            // don't call into
                            return expr;
                        }
                    }
                }
                Lit::ByteStr(_) => {
                    if let Some(Type::Reference(ty)) = ty {
                        if let Type::Array(ty) = ty.elem.as_ref() {
                            if let Type::Path(ty) = ty.elem.as_ref() {
                                let ty_string = ty.into_token_stream().to_string();

                                if ty_string == "u8" {
                                    // don't call into
                                    return expr;
                                }
                            }
                        }
                    }
                }
                _ => (),
            }

            syn::parse2(quote!(::core::convert::Into::into(#expr))).unwrap()
        }
        _ => expr,
    }
}
