use proc_macro::TokenStream;
use syn::{
    self, DeriveInput, parse_macro_input, spanned::Spanned,
    Fields, DataStruct, Data, FieldsNamed, punctuated::Punctuated,
    Field, token::Comma,
};
use quote::{quote};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(st: &DeriveInput)  -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_literal = st.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());

    let struct_ident = &st.ident;
    let data_fields = parse_data_fields(st)?;
    let mut builder_fields = proc_macro2::TokenStream::new();
    let mut build_fn_body = proc_macro2::TokenStream::new();
    let mut builder_impl_func = proc_macro2::TokenStream::new();
    data_fields.iter().for_each(|field|{
        let ident = &field.ident;
        let ty = &field.ty;
        builder_fields.extend(quote!{
            #ident: Option<#ty>,
        });
        build_fn_body.extend(quote!{
            #ident: None,
        });
        builder_impl_func.extend(quote!{
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        });
    });

    let ret = quote! {
        pub struct #builder_name_ident {
            #builder_fields
        }
        impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #build_fn_body
                }
            }
        }
        impl #builder_name_ident {
            #builder_impl_func
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

