#![feature(extend_one)]
use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenTree};
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, Ident, LitInt, Token};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let mut seq_generator = parse_macro_input!(input as SeqGenerator);

    match seq_generator.generate() {
        Ok(stream) => stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[derive(Debug)]
struct SeqGenerator {
    replacement: Ident,
    left: u64,
    right: u64,
    template: Group,
}

impl Parse for SeqGenerator {
    fn parse(input: ParseStream) -> Result<Self> {
        let replacement: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let left = input.parse::<LitInt>()?.base10_parse::<u64>()?;
        input.parse::<Token![.]>()?;
        input.parse::<Token![.]>()?;
        let right = if let Ok(_) = input.parse::<Token![=]>() {
            input.parse::<LitInt>()?.base10_parse::<u64>()? + 1
        } else {
            input.parse::<LitInt>()?.base10_parse::<u64>()?
        };
        let group: Group = input.parse()?;
        Ok(SeqGenerator {
            replacement,
            left,
            right,
            template: group.clone(),
        })
    }
}

impl SeqGenerator {
    fn generate(&mut self) -> Result<proc_macro2::TokenStream> {
        let mut result = proc_macro2::TokenStream::new();
        let middle = self.expand_hashtag_parentheses(&self.template);
        if self.check_group_iteration(&middle) {
            let stream_template = middle.stream().into_iter().collect();
            for num in self.left..self.right {
                result.extend(self.apply_token_stream(&stream_template, num));
            }
        } else {
            result.extend(middle.stream());
        }
        Ok(result)
    }

    fn expand_hashtag_parentheses(&self, group: &Group) -> Group {
        let delimiter = group.delimiter();
        let mut new_stream: Vec<TokenTree> = Vec::new();
        let mut stream_iter = group.stream().into_iter();
        loop {
            let tt = match stream_iter.next() {
                Some(tt) => tt,
                None => break,
            };
            match tt {
                TokenTree::Group(sub_group) => {
                    let new_sub_group = self.expand_hashtag_parentheses(&sub_group);
                    new_stream.push(TokenTree::Group(new_sub_group));
                }
                TokenTree::Punct(punct) => {
                    let punct_backup = TokenTree::Punct(punct.clone());
                    match punct.as_char() {
                        '#' => {
                            let middle = stream_iter.next().unwrap();
                            match middle {
                                TokenTree::Group(sub_group) => {
                                    if sub_group.delimiter() == Delimiter::Parenthesis {
                                        let _ = stream_iter.next();
                                        let sub_template = sub_group.stream().into_iter().collect();
                                        for num in self.left..self.right {
                                            new_stream.append(
                                                &mut self.apply_token_stream(&sub_template, num),
                                            );
                                        }
                                    } else {
                                        new_stream.push(punct_backup);
                                        new_stream.push(TokenTree::Group(sub_group));
                                    }
                                }
                                token => {
                                    new_stream.push(punct_backup);
                                    new_stream.push(token.clone());
                                }
                            }
                        }
                        _ => new_stream.push(punct_backup),
                    }
                }
                token => new_stream.push(token),
            }
        }
        let mut result = proc_macro2::TokenStream::new();
        result.extend(new_stream);
        Group::new(delimiter, result)
    }

    fn apply_token_stream(&self, stream: &Vec<TokenTree>, num: u64) -> Vec<TokenTree> {
        let mut new_stream: Vec<TokenTree> = Vec::new();
        let mut stream_iter = stream.iter();
        loop {
            let tt = match stream_iter.next() {
                Some(tt) => tt,
                None => break,
            };
            match tt {
                TokenTree::Group(sub_group) => {
                    let sub_stream = sub_group.stream().into_iter().collect();
                    let new_sub_stream = self.apply_token_stream(&sub_stream, num);
                    let mut new_token_stream = proc_macro2::TokenStream::new();
                    new_token_stream.extend(new_sub_stream);
                    let new_sub_group = Group::new(sub_group.delimiter(), new_token_stream);
                    new_stream.push(TokenTree::Group(new_sub_group));
                }
                TokenTree::Punct(punct) => match punct.as_char() {
                    '#' => {
                        let middle = stream_iter.next().unwrap();
                        match middle {
                            TokenTree::Ident(ident) => {
                                let last_token = new_stream.pop();
                                match last_token {
                                    Some(TokenTree::Ident(last_ident)) => {
                                        if ident.eq(&self.replacement) {
                                            let new_name =
                                                format!("{}{}", last_ident.to_string(), num);
                                            let new_ident =
                                                Ident::new(&new_name, last_ident.span());
                                            new_stream.push(TokenTree::Ident(new_ident));
                                        } else {
                                            new_stream.push(TokenTree::Ident(last_ident));
                                            new_stream.push(TokenTree::Ident(ident.clone()));
                                        }
                                    }
                                    token => {
                                        if let Some(last) = token {
                                            new_stream.push(last);
                                        }
                                        if ident.eq(&self.replacement) {
                                            let literal = Literal::u64_unsuffixed(num);
                                            new_stream.push(TokenTree::Literal(literal));
                                        } else {
                                            new_stream.push(TokenTree::Ident(ident.clone()));
                                        }
                                    }
                                }
                            }
                            token => new_stream.push(token.clone()),
                        }
                    }
                    _ => new_stream.push(TokenTree::Punct(punct.clone())),
                },
                TokenTree::Ident(ident) => {
                    if ident.eq(&self.replacement) {
                        let literal = Literal::u64_unsuffixed(num);
                        new_stream.push(TokenTree::Literal(literal));
                    } else {
                        new_stream.push(TokenTree::Ident(ident.clone()));
                    }
                }
                token => new_stream.push(token.clone()),
            }
        }
        new_stream
    }

    fn check_group_iteration(&self, stream: &Group) -> bool {
        stream.stream().into_iter().any(|token| match token {
            TokenTree::Ident(ident) => ident.eq(&self.replacement),
            TokenTree::Group(group) => self.check_group_iteration(&group),
            _ => false,
        })
    }
}
