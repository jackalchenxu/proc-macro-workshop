use std::ops::{Range, RangeInclusive};

use proc_macro2::{Delimiter, Group, Ident, Punct, TokenStream, TokenTree};
use quote::quote;
use syn::{
    buffer::TokenBuffer,
    parse::{Parse, ParseStream},
    parse_macro_input, LitInt,
};

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq = parse_macro_input!(input as Seq);
    let expand_mode = {
        if search_repeat_tag(&seq.body) {
            ExpandMode::ExpandAsterik
        } else {
            ExpandMode::ExpandInPlace
        }
    };
    seq.call_body(expand_mode)
}

#[derive(Debug)]
struct Seq {
    ident: Ident,
    // start: isize,
    // end: isize,
    range: MyRange,
    body: TokenStream,
}

#[derive(PartialEq)]
enum ExpandMode {
    ExpandInPlace,
    ExpandAsterik,
}

#[derive(Debug, Clone)]
enum MyRange {
    NonInclusive(Range<isize>),
    Inclusive(RangeInclusive<isize>),
}
impl Iterator for MyRange {
    type Item = isize;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MyRange::NonInclusive(n) => n.next(),
            MyRange::Inclusive(i) => i.next(),
        }
    }
}
impl From<Range<isize>> for MyRange {
    fn from(r: Range<isize>) -> Self {
        MyRange::NonInclusive(r)
    }
}
impl From<RangeInclusive<isize>> for MyRange {
    fn from(r: RangeInclusive<isize>) -> Self {
        MyRange::Inclusive(r)
    }
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        input.parse::<syn::Token!(in)>()?;

        let start: LitInt = input.parse()?;
        input.parse::<syn::Token!(..)>()?;

        let range = {
            if input.peek(syn::Token![=]) {
                let _: Punct = input.parse()?;
                let end: LitInt = input.parse()?;

                MyRange::from(RangeInclusive::new(
                    start.base10_parse::<u32>().unwrap() as isize,
                    end.base10_parse::<u32>().unwrap() as isize,
                ))
            } else {
                let end: LitInt = input.parse()?;

                MyRange::from(Range {
                    start: start.base10_parse::<u32>().unwrap() as isize,
                    end: end.base10_parse::<u32>().unwrap() as isize,
                })
            }
        };

        let body_buf;
        syn::braced!(body_buf in input);
        let body = body_buf.parse::<TokenStream>()?;

        Ok(Seq { ident, range, body })
    }
}

impl Seq {
    fn call_body(&self, expand_mode: ExpandMode) -> proc_macro::TokenStream {
        // eprintln!("function body:{:#?}", self.body);
        let mut ret = TokenStream::new();

        if expand_mode == ExpandMode::ExpandInPlace {
            for n in self.range.clone() {
                ret.extend(self.expand(&self.body, n));
            }
        } else {
            ret.extend(self.expand_range(&self.body, self.range.clone()));
        }

        ret.into()
    }

    fn expand(&self, ts: &TokenStream, n: isize) -> TokenStream {
        let buf = ts.clone().into_iter().collect::<Vec<_>>();

        let mut ret = TokenStream::new();

        let mut index: usize = 0;
        while index < buf.len() {
            let tree_node = &buf[index];
            match tree_node {
                TokenTree::Group(g) => {
                    let new_stream = self.expand(&g.stream(), n);
                    let wrap_in_group = Group::new(g.delimiter(), new_stream);
                    ret.extend(quote!(#wrap_in_group));
                    index += 1;
                    continue;
                }
                TokenTree::Ident(id) => {
                    if let Some(TokenTree::Punct(p)) = &buf.get(index + 1) {
                        if p.as_char() == '~' {
                            if let Some(TokenTree::Ident(match_id)) = &buf.get(index + 2) {
                                if match_id == &self.ident {
                                    let func_name = Ident::new(&format!("{}{}", id, n), id.span());
                                    ret.extend(quote!(#func_name));
                                    index += 3;
                                    continue;
                                }
                            }
                        }
                    }

                    if id == &self.ident {
                        // let new_ident = LitInt::new(&format!("{}", n), proc_macro2::Span::call_site());
                        let new_ident = proc_macro2::Literal::i64_unsuffixed(n as i64);
                        ret.extend(quote!(#new_ident));
                        index += 1;
                        continue;
                    }
                }
                _ => {}
            }

            ret.extend(quote!(#tree_node));
            index += 1;
        }

        ret
    }

    fn expand_range(&self, ts: &TokenStream, range: MyRange) -> TokenStream {
        let buf = ts.clone().into_iter().collect::<Vec<_>>();
        // let buffer = TokenBuffer::new2(ts.clone());

        let mut ret = TokenStream::new();

        let mut index: usize = 0;
        while index < buf.len() {
            let tree_node = &buf[index];
            match tree_node {
                TokenTree::Group(g) => {
                    let new_stream = self.expand_range(&g.stream(), range.clone());
                    let wrap_in_group = Group::new(g.delimiter(), new_stream);
                    ret.extend(quote!(#wrap_in_group));
                    index += 1;
                    continue;
                }
                TokenTree::Punct(p) => {
                    if p.as_char() == '#' {
                        if let TokenTree::Group(gg) = &buf[index + 1] {
                            if gg.delimiter() == Delimiter::Parenthesis {
                                if let TokenTree::Punct(asterik) = &buf[index + 2] {
                                    if asterik.as_char() == '*' {
                                        index += 3;
                                        // replace "~N" in the group into repeat loop
                                        for n in range.clone() {
                                            ret.extend(self.replace_quote_n(gg.stream(), n));
                                            // ret.extend(self.replace_quote_n(gg.stream(), n));
                                        }
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                }
                _ => {}
            }

            ret.extend(quote!(#tree_node));
            index += 1;
        }

        ret
    }

    /// replace ID~N into ID1 alike
    fn replace_quote_n(&self, ts: TokenStream, n: isize) -> TokenStream {
        let buf = ts.into_iter().collect::<Vec<_>>();
        let mut ret = TokenStream::new();

        let mut index: usize = 0;
        while index < buf.len() {
            match &buf[index] {
                TokenTree::Ident(id) => {
                    // replace ident N to n
                    if id == &self.ident {
                        let replaced = proc_macro2::Literal::i64_unsuffixed(n as i64);
                        ret.extend(quote!(#replaced));
                        index += 1;
                        continue;
                    }

                    // replace ~N to n
                    if let TokenTree::Punct(p) = &buf[index + 1] {
                        if p.as_char() == '~' {
                            if let TokenTree::Ident(match_n) = &buf[index + 2] {
                                if match_n == &self.ident {
                                    let field_name = Ident::new(&format!("{}{}", id, n), id.span());
                                    ret.extend(quote!(#field_name));
                                    index += 3;
                                    continue;
                                }
                            }
                        }
                    }
                }
                TokenTree::Group(g) => {
                    let new_stream = self.replace_quote_n(g.stream(), n);
                    let wrap_in_group = Group::new(g.delimiter(), new_stream);
                    ret.extend(quote!(#wrap_in_group));
                    index += 1;
                    continue;
                }
                _ => {}
            }

            let a = &buf[index];
            ret.extend(quote!(#a));
            index += 1;
        }

        ret
    }
}

/// search #()*
/// return true if found the pattern;  else return false
fn search_repeat_tag(ts: &TokenStream) -> bool {
    let buffer = TokenBuffer::new2(ts.clone());
    let mut cursor = buffer.begin();

    while let Some((token, cur)) = cursor.token_tree() {
        match token {
            TokenTree::Punct(p) if p.as_char() == '#' => {
                if let Some((TokenTree::Group(g), c_star)) = cur.token_tree() {
                    if g.delimiter() == proc_macro2::Delimiter::Parenthesis {
                        match c_star.punct() {
                            Some((p, _)) if p.as_char() == '*' => {
                                return true;
                            }
                            _ => {}
                        }
                    }
                }
            }
            TokenTree::Group(g) => {
                if search_repeat_tag(&g.stream()) {
                    return true;
                }
            }
            _ => {}
        }

        cursor = cur;
    }

    false
}
