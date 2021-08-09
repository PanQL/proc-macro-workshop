#![feature(extend_one)]
use proc_macro::{TokenStream};
use proc_macro2::{TokenTree, Group, Literal};
use syn::{Token, parse_macro_input, Ident, LitInt};
use syn::parse::{Parse, ParseStream, Result};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    eprintln!("{:?}", input);
    let mut seq_generator = parse_macro_input!(input as SeqGenerator);
    //eprintln!("{:#?}", seq_generator);

    match seq_generator.generate() {
        Ok(stream) => stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[derive(Debug)]
struct SeqGenerator{
    replacement: Ident,
    range_left: LitInt,
    range_right: LitInt,
    template: proc_macro2::Group,
    result: proc_macro2::TokenStream,
}

impl Parse for SeqGenerator {
    fn parse(input: ParseStream) -> Result<Self> {
        let replacement: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let range_left: LitInt = input.parse()?;
        input.parse::<Token![.]>()?;
        input.parse::<Token![.]>()?;
        let range_right: LitInt = input.parse()?;
        let group: proc_macro2::Group = input.parse()?;
        Ok(SeqGenerator {
            replacement,
            range_left,
            range_right,
            template: group.clone(),
            result: proc_macro2::TokenStream::new(),
        })
    }
}

impl SeqGenerator {
    fn generate(&mut self) -> Result<proc_macro2::TokenStream> {
        let left  = self.range_left.base10_parse::<u64>()?;
        let right  = self.range_right.base10_parse::<u64>()?;

        for number in left..right {
            self.result.extend(self.apply_group(&self.template, number).stream());
        }
        //eprintln!("{:?}", self.result);
        Ok(self.result.clone())
    }

    fn apply_group(&self, group: &proc_macro2::Group, current_num: u64) -> Group {
        let mut new_stream = proc_macro2::TokenStream::new();
        let delimiter = group.delimiter();
        group.stream().into_iter().for_each(|tt|{
            match tt {
                TokenTree::Ident(ident) => {
                    if ident.eq(&self.replacement) {
                        let literal = Literal::u64_unsuffixed(current_num);
                        //let new_ident = proc_macro2::Ident::new(&format!("{}", current_num), ident.span());
                        new_stream.extend_one(TokenTree::Literal(literal));
                    } else {
                        new_stream.extend_one(TokenTree::Ident(ident.clone()));
                    }
                },
                TokenTree::Group(sub_group) => {
                    let new_sub_group = self.apply_group(&sub_group, current_num);
                    new_stream.extend_one(TokenTree::Group(new_sub_group));
                },
                token => new_stream.extend_one(token),
            }
        });
        proc_macro2::Group::new(delimiter, new_stream)
    }
}
