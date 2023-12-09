use std::collections::HashSet;

use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident, Meta, Token, Type, TypeParamBound,
};

pub(crate) struct TypeWithPunctuatedMeta {
    pub(crate) ty:   Type,
    pub(crate) list: Punctuated<Meta, Token![,]>,
}

impl Parse for TypeWithPunctuatedMeta {
    #[inline]
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty = input.parse::<Type>()?;

        if input.is_empty() {
            return Ok(Self {
                ty,
                list: Punctuated::new(),
            });
        }

        input.parse::<Token![,]>()?;

        let list = input.parse_terminated(Meta::parse, Token![,])?;

        Ok(Self {
            ty,
            list,
        })
    }
}

/// recursive (dereference, de_ptr)
#[inline]
pub(crate) fn find_idents_in_type<'a>(
    set: &mut HashSet<&'a Ident>,
    ty: &'a Type,
    recursive: Option<(bool, bool)>,
) {
    match ty {
        Type::Array(ty) => {
            if recursive.is_some() {
                find_idents_in_type(set, ty.elem.as_ref(), recursive);
            }
        },
        Type::Group(ty) => {
            if recursive.is_some() {
                find_idents_in_type(set, ty.elem.as_ref(), recursive);
            }
        },
        Type::ImplTrait(ty) => {
            if recursive.is_some() {
                for b in &ty.bounds {
                    if let TypeParamBound::Trait(ty) = b {
                        if let Some(ty) = ty.path.get_ident() {
                            set.insert(ty);
                        }
                    }
                }
            }
        },
        Type::Macro(ty) => {
            if recursive.is_some() {
                if let Some(ty) = ty.mac.path.get_ident() {
                    set.insert(ty);
                }
            }
        },
        Type::Paren(ty) => {
            if recursive.is_some() {
                find_idents_in_type(set, ty.elem.as_ref(), recursive);
            }
        },
        Type::Path(ty) => {
            if let Some(ty) = ty.path.get_ident() {
                set.insert(ty);
            }
        },
        Type::Ptr(ty) => {
            if let Some((_, de_ptr)) = recursive {
                if de_ptr {
                    find_idents_in_type(set, ty.elem.as_ref(), recursive);
                }
            }
        },
        Type::Reference(ty) => {
            if let Some((dereference, _)) = recursive {
                if dereference {
                    find_idents_in_type(set, ty.elem.as_ref(), recursive);
                }
            }
        },
        Type::Slice(ty) => {
            if recursive.is_some() {
                find_idents_in_type(set, ty.elem.as_ref(), recursive);
            }
        },
        Type::TraitObject(ty) => {
            if recursive.is_some() {
                for b in &ty.bounds {
                    if let TypeParamBound::Trait(ty) = b {
                        if let Some(ty) = ty.path.get_ident() {
                            set.insert(ty);
                        }
                    }
                }
            }
        },
        Type::Tuple(ty) => {
            if recursive.is_some() {
                for ty in &ty.elems {
                    find_idents_in_type(set, ty, recursive)
                }
            }
        },
        _ => (),
    }
}

#[inline]
pub(crate) fn find_derivable_idents_in_type<'a>(set: &mut HashSet<&'a Ident>, ty: &'a Type) {
    match ty {
        Type::Array(ty) => find_derivable_idents_in_type(set, ty.elem.as_ref()),
        Type::Group(ty) => find_derivable_idents_in_type(set, ty.elem.as_ref()),
        Type::ImplTrait(ty) => {
            for b in &ty.bounds {
                if let TypeParamBound::Trait(ty) = b {
                    if let Some(ty) = ty.path.get_ident() {
                        set.insert(ty);
                    }
                }
            }
        },
        Type::Macro(ty) => {
            if let Some(ty) = ty.mac.path.get_ident() {
                set.insert(ty);
            }
        },
        Type::Paren(ty) => find_derivable_idents_in_type(set, ty.elem.as_ref()),
        Type::Path(ty) => {
            if let Some(ty) = ty.path.get_ident() {
                set.insert(ty);
            }
        },
        Type::Ptr(_) => (),
        Type::Reference(_) => (),
        Type::Slice(ty) => find_derivable_idents_in_type(set, ty.elem.as_ref()),
        Type::TraitObject(ty) => {
            for b in &ty.bounds {
                if let TypeParamBound::Trait(ty) = b {
                    if let Some(ty) = ty.path.get_ident() {
                        set.insert(ty);
                    }
                }
            }
        },
        Type::Tuple(ty) => {
            for ty in &ty.elems {
                find_derivable_idents_in_type(set, ty)
            }
        },
        _ => (),
    }
}

#[inline]
pub(crate) fn dereference(ty: &Type) -> &Type {
    if let Type::Reference(ty) = ty {
        dereference(ty.elem.as_ref())
    } else {
        ty
    }
}

#[inline]
pub(crate) fn dereference_changed(ty: &Type) -> (&Type, bool) {
    if let Type::Reference(ty) = ty {
        (dereference(ty.elem.as_ref()), true)
    } else {
        (ty, false)
    }
}
