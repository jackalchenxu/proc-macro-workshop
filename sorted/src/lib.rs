use std::cmp::Ordering;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Colon2;
use syn::visit_mut::{self, VisitMut};
use syn::ItemFn;
use syn::PathSegment;
use syn::{parse_macro_input, ExprMatch, Item, ItemEnum};

#[proc_macro_attribute]
pub fn sorted(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // eprintln!("input:{:#?}", input);
    let p = parse_macro_input!(input as Item);
    match p {
        Item::Enum(sorted) => {
            // eprintln!("Item:{:#?}", sorted);
            match expand_enum_def(&sorted) {
                Ok(ts) => ts.into(),
                Err(e) => e.to_compile_error().into(),
            }
        }
        _ => syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected enum or match expression",
        )
        .to_compile_error()
        .into(),
    }
}

#[proc_macro_attribute]
pub fn check(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // eprintln!("input:{:#?}", input);
    let mut func = parse_macro_input!(input as ItemFn);

    // returned token stream should contain function body
    // with #[sorted] attribute removed
    let mut t = TokenStream::new();
    if let Err(e) = check_match_sort_in_fn(&mut func) {
        // if any error, put it into returned tokenstream
        t.extend(e.to_compile_error());
    }
    t.extend(func.to_token_stream());

    t.into()
}

/// check match items sorted in function definition
/// return ok(()) if items are sorted, input ItemFn are already handled
/// return syn::Error if not sorted
fn check_match_sort_in_fn(func: &mut ItemFn) -> syn::Result<()> {
    let mut sort_check = SortCheck::default();

    for stmt in func.block.stmts.iter_mut() {
        if let syn::Stmt::Expr(syn::Expr::Match(m)) = stmt {
            sort_check.visit_expr_match_mut(m);
        }
    }

    if sort_check.err.is_none() {
        syn::Result::Ok(())
    } else {
        syn::Result::Err(sort_check.err.unwrap())
    }
}

/// expand sort macro on enum definition, and check if sorted
/// return Ok(TokenStream_expanded) or Err(syn::Error error message)
fn expand_enum_def(item_enum: &ItemEnum) -> syn::Result<TokenStream> {
    let mut v = vec![];

    for items in item_enum.variants.iter() {
        v.push(items.ident.clone());
    }

    let mut vv = v.clone();
    vv.sort();

    for (a, b) in v.iter().zip(vv.iter()) {
        if a != b {
            return Err(syn::Error::new(
                b.span(),
                format!("{b} should sort before {a}"),
            ));
        }
    }

    Ok(item_enum.to_token_stream())
}

#[derive(Default)]
struct SortCheck {
    err: Option<syn::Error>,
}

impl VisitMut for SortCheck {
    fn visit_expr_match_mut(&mut self, node: &mut ExprMatch) {
        // if fucntion body contains attribute: sorted
        if node.attrs.iter().any(|n| {
            n.path
                .segments
                .iter()
                .any(|segment| segment.ident == "sorted")
        }) {
            // remove the "sorted" attribute from function body
            // it will cause syntax error
            node.attrs.retain(|n| {
                n.path
                    .segments
                    .iter()
                    .all(|segment| segment.ident != "sorted")
            });

            let mut v = vec![];
            for arm in node.arms.iter() {
                match &arm.pat {
                    syn::Pat::TupleStruct(t) => {
                        let b = concat_path_string(&t.path.segments);

                        if v.last().is_some() {
                            let a: &String = v.last().unwrap();
                            if a.cmp(&b) == Ordering::Greater && self.err.is_none() {
                                self.err = Some(syn::Error::new_spanned(
                                    t.path.clone(),
                                    format!("{b} should sort before {a}"),
                                ));
                            }
                        }
                        v.push(b);
                    }
                    syn::Pat::Ident(id) => {
                        let b = id.ident.to_string();
                        if v.last().is_some() {
                            let a: &String = v.last().unwrap();
                            if a.cmp(&b) == Ordering::Greater && self.err.is_none() {
                                self.err = Some(syn::Error::new_spanned(
                                    id.clone(),
                                    format!("{b} should sort before {a}"),
                                ));
                            }
                        }
                        v.push(b);
                    }
                    syn::Pat::Wild(w) => {
                        if arm != node.arms.last().unwrap() {
                            self.err = Some(syn::Error::new_spanned(
                                w.clone(),
                                "_ should be the last one of the match",
                            ));
                        }
                    }
                    _ => {
                        // eprintln!("else:{:#?}", arm.pat);
                        if self.err.is_none() {
                            self.err =
                                Some(syn::Error::new(arm.pat.span(), "unsupported by #[sorted]"));
                        }
                    }
                }
            }
        }

        visit_mut::visit_expr_match_mut(self, node);
    }
}

/// get concated path string like "Error::Io" from Punctuated<PathSegment, Colon2>
fn concat_path_string(segments: &Punctuated<PathSegment, Colon2>) -> String {
    let mut v = vec![];

    for segment in segments {
        v.push(format!("{}", segment.ident));
    }

    v.join("::")
}
