use proc_macro::TokenStream;
use quote::quote;
use std::collections::{HashMap, HashSet};
use syn::{punctuated::Punctuated, token::Comma, visit::Visit, *};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

struct VisitHelper {
    pub clause: HashMap<String, HashSet<TypePath>>,
}

impl<'ast> Visit<'ast> for VisitHelper {
    fn visit_type_path(&mut self, node: &'ast TypePath) {
        let segments = &node.path.segments;
        if segments.len() >= 2 {
            let first = segments.first().unwrap();
            let ident_name = first.ident.to_string();
            if !self.clause.contains_key(&ident_name) {
                self.clause.insert(ident_name.clone(), HashSet::new());
            }
            self.clause
                .get_mut(&ident_name)
                .unwrap()
                .insert(node.clone());
        }

        visit::visit_type_path(self, node);
    }
}

impl VisitHelper {
    fn extend_where_clause(&mut self, where_clause: &mut WhereClause) {
        for set in self.clause.values() {
            for value in set.iter() {
                where_clause.predicates.push(parse_quote! {
                    #value: core::fmt::Debug
                });
            }
        }
    }
}

fn do_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &st.ident;
    let struct_name_literal = struct_ident.to_string();
    let fields = parse_data_fields(st)?;

    let fields_stuffing = generate_fields_stuffing(fields)?;
    let mut helper = VisitHelper {
        clause: HashMap::new(),
    };
    helper.visit_derive_input(st);

    let (mut phantom, non_phantom) = generics_in_phantom(fields, &helper);
    let hatch_attrs = parse_hatch_attr(&st.attrs, &mut phantom);
    let mut generics = add_trait_bounds(st.generics.clone(), &phantom, &non_phantom);
    generics.make_where_clause();

    helper.extend_where_clause(generics.where_clause.as_mut().unwrap());
    for hatch in hatch_attrs {
        let where_clause = generics.where_clause.as_mut().unwrap();
        where_clause.predicates.push(parse_str(&hatch)?);
    }

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let ret = quote! {
        impl#impl_generics ::core::fmt::Debug for #struct_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                f.debug_struct(#struct_name_literal)#fields_stuffing.finish()
            }
        }
    };
    Ok(ret)
}

fn generics_in_phantom(
    fields: &Punctuated<Field, Comma>,
    helper: &VisitHelper,
) -> (Vec<String>, Vec<String>) {
    let mut phantom = Vec::new();
    helper.clause.keys().for_each(|key| {
        phantom.push(key.clone());
    });
    let mut non_phantom = Vec::new();
    fields.iter().for_each(|field| {
        let ty = &field.ty;
        let segments = match ty {
            Type::Path(TypePath {
                path: Path { segments, .. },
                ..
            }) => segments,
            _ => return,
        };

        let (ident, args) = match segments.first() {
            Some(PathSegment {
                ident,
                arguments:
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
                ..
            }) => (ident, args),
            Some(PathSegment { ident, .. }) => {
                let name = ident.to_string();
                if !helper.clause.contains_key(&name) {
                    non_phantom.push(ident.to_string());
                }
                return;
            }
            _ => return,
        };

        if ident.to_string().as_str() != "PhantomData" {
            return;
        }

        let path_seg = match args.first() {
            Some(GenericArgument::Type(Type::Path(TypePath {
                path: Path {
                    segments: path_seg, ..
                },
                ..
            }))) => path_seg,
            _ => return,
        };

        let generics_ident = match path_seg.first() {
            Some(PathSegment { ident, .. }) => ident,
            _ => return,
        };

        phantom.push(generics_ident.to_string());
    });
    (phantom, non_phantom)
}

fn add_trait_bounds(
    mut generics: Generics,
    phantom: &Vec<String>,
    non_phantom: &Vec<String>,
) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            if phantom.contains(&type_param.ident.to_string())
                && !non_phantom.contains(&type_param.ident.to_string())
            {
                continue;
            }
            type_param.bounds.push(parse_quote!(::core::fmt::Debug));
        }
    }
    generics
}

fn generate_fields_stuffing(
    fields: &Punctuated<Field, Comma>,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut stuff = proc_macro2::TokenStream::new();
    for field in fields {
        let ident = field.ident.as_ref().unwrap();
        let ident_literal = ident.to_string();
        match parse_attributes(&field.attrs) {
            Some(format_str) => {
                stuff.extend(quote! {
                    .field(#ident_literal, &format_args!(#format_str, self.#ident))
                });
            }
            None => {
                stuff.extend(quote! {
                    .field(#ident_literal, &self.#ident)
                });
            }
        };
    }

    Ok(stuff)
}

fn parse_hatch_attr(attrs: &Vec<Attribute>, set: &mut Vec<String>) -> Vec<String> {
    let mut ret = Vec::new();
    attrs.iter().for_each(|attr| {
        let (segments, nested) = match attr.parse_meta() {
            Ok(Meta::List(MetaList {
                path: Path { segments, .. },
                nested,
                ..
            })) => (segments, nested),
            _ => return,
        };

        let segment = match segments.first() {
            Some(segment) => segment,
            None => return,
        };

        if segment.ident.to_string().as_str() != "debug" {
            return;
        }

        let lit_str = match nested.first() {
            Some(NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                lit: Lit::Str(lit_str),
                ..
            }))) => lit_str,
            _ => return,
        };
        set.push(lit_str.value().to_string().as_str()[0..1].into());

        ret.push(lit_str.value());
    });
    ret
}

fn parse_attributes(attrs: &Vec<Attribute>) -> Option<String> {
    let ret = None;
    for attr in attrs {
        let (segments, lit_str) = match attr.parse_meta() {
            Ok(Meta::NameValue(MetaNameValue {
                path: Path { segments, .. },
                lit: Lit::Str(lit_str),
                ..
            })) => (segments, lit_str),
            _ => return ret,
        };

        let segment = match segments.first() {
            Some(segment) => segment,
            None => return ret,
        };

        if segment.ident.to_string().as_str() != "debug" {
            return ret;
        }

        return Some(lit_str.value());
    }
    ret
}

fn parse_data_fields(st: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    let ret = Err(syn::Error::new_spanned(
        st,
        "Still not supported".to_string(),
    ));
    let fields = match &st.data {
        Data::Struct(DataStruct { fields, .. }) => fields,
        _ => return ret,
    };
    match fields {
        Fields::Named(FieldsNamed {
            brace_token: _,
            named,
        }) => Ok(named),
        _ => ret,
    }
}
