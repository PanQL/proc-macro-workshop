use proc_macro::TokenStream;
use quote::{quote};
use syn::{
    *, spanned::Spanned, token::Comma, punctuated::Punctuated,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

enum InnerType{
    Atom(Type),
    OptionInner(Type),
    VecInner(Type),
}

fn check_and_fetch_attr_for_vec(attrs: &Vec<Attribute>) -> syn::Result<Option<String>> {
    let mut ret = Ok(None);
    attrs.iter().for_each(|attr| {
        match attr.parse_meta() {
            Ok(Meta::List(MetaList{
                path: Path {
                    segments,
                    ..
                },
                nested,
                ..
            })) => {
                if let Some(segment) = segments.first() {
                    if segment.ident.to_string().as_str() == "builder" {
                        match nested.first() {
                            Some(NestedMeta::Meta(Meta::NameValue(MetaNameValue{
                                path: Path {
                                    segments: path_seg,
                                    ..
                                },
                                lit: Lit::Str(lit_str),
                                ..
                            }))) => {
                                match path_seg.first() {
                                    Some(PathSegment{
                                        ident,
                                        ..
                                    }) => {
                                        if ident.to_string().as_str() == "each" {
                                            ret = Ok(Some(lit_str.value()));
                                        } else {
                                            if let Ok(list) = attr.parse_meta() {
                                                ret = Err(syn::Error::new_spanned(list, "expected `builder(each = \"...\")`".to_string()));
                                            }
                                        }
                                    },
                                    _ => {}
                                }
                            },
                            _ => {}
                        }
                    }
                }
            },
            _ => {}
        }
    });
    ret
}

fn parse_inner_type(ty: &Type) -> InnerType {
    match ty {
        Type::Path(TypePath{qself: _, path: Path{
            segments,
            ..
        }}) => {
            match segments.first() {
                Some(PathSegment{
                    ident, 
                    arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args,
                        ..
                    }),
                }) => {
                    match args.first() {
                        Some(GenericArgument::Type(new_ty)) => {
                            match ident.to_string().as_str() {
                                "Option" => {
                                    InnerType::OptionInner(new_ty.clone())
                                },
                                "Vec" => {
                                    InnerType::VecInner(new_ty.clone())
                                },
                                _ => InnerType::Atom(ty.clone())
                            }
                        },
                        _ => InnerType::Atom(ty.clone()),
                    }
                },
                _ => InnerType::Atom(ty.clone()),
            }
        }
        _ => InnerType::Atom(ty.clone()),
    }
}

fn do_expand(st: &DeriveInput)  -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_literal = st.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());

    let struct_ident = &st.ident;
    let data_fields = parse_data_fields(st)?;
    let mut builder_fields = proc_macro2::TokenStream::new();
    let mut builder_fn_fields = proc_macro2::TokenStream::new();
    let mut builder_impl_func = proc_macro2::TokenStream::new();
    let mut build_fn_fields = proc_macro2::TokenStream::new();

    for field in data_fields {
        let attrs = &field.attrs;
        let field_attr = check_and_fetch_attr_for_vec(attrs)?;
        let ident = &field.ident;
        let ty = &field.ty;
        match (parse_inner_type(ty), field_attr) {
            (InnerType::OptionInner(new_ty), _) => {
                builder_fields.extend(quote!{
                    #ident: core::option::Option<#new_ty>,
                });
                builder_fn_fields.extend(quote!{
                    #ident: core::option::Option::None,
                });
                builder_impl_func.extend(quote!{
                    pub fn #ident(&mut self, #ident: #new_ty) -> &mut Self {
                        self.#ident = core::option::Option::Some(#ident);
                        self
                    }
                });
                build_fn_fields.extend(quote!{
                    #ident: self.#ident.take(),
                });
            },
            (InnerType::VecInner(new_ty), Some(interface_name)) => {
                builder_fields.extend(quote!{
                    #ident: std::vec::Vec<#new_ty>,
                });
                builder_fn_fields.extend(quote!{
                    #ident: std::vec::Vec::new(),
                });
                let ident_func = Ident::new(&interface_name, ident.span());
                builder_impl_func.extend(quote!{
                    pub fn #ident_func(&mut self, #ident: #new_ty) -> &mut Self {
                        self.#ident.push(#ident);
                        self
                    }
                });
                build_fn_fields.extend(quote!{
                    #ident: {
                        let mut res = std::vec::Vec::new();
                        res.append(&mut self.#ident);
                        res
                    },
                });
            },
            _ => {
                builder_fields.extend(quote!{
                    #ident: core::option::Option<#ty>,
                });
                builder_fn_fields.extend(quote!{
                    #ident: core::option::Option::None,
                });
                builder_impl_func.extend(quote!{
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = core::option::Option::Some(#ident);
                        self
                    }
                });
                build_fn_fields.extend(quote!{
                    #ident: self.#ident.take().unwrap(),
                });
            }
        }
    }


    let ret = quote! {
        pub struct #builder_name_ident {
            #builder_fields
        }
        impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #builder_fn_fields
                }
            }
        }
        impl #builder_name_ident {
            #builder_impl_func
            pub fn build(&mut self) -> core::option::Option<#struct_ident> {
                core::option::Option::Some(#struct_ident {
                    #build_fn_fields
                })
            }
        }
    };

    Ok(ret)
}

fn parse_data_fields(st: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    match &st.data {
        Data::Struct(DataStruct {struct_token: _, fields, semi_token: _}) => {
            match fields {
                Fields::Named(FieldsNamed{brace_token: _, named}) => {
                    Ok(named)
                }
                _ => {
                    Err(syn::Error::new_spanned(st, "Still not supported".to_string()))
                }
            }
        },
        _ => {
            Err(syn::Error::new_spanned(st, "Still not supported".to_string()))
        }
    }
}

