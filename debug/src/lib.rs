use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::visit::{self, Visit};
use syn::{parse_macro_input, DeriveInput, Ident, LitStr};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match do_expand(&input) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[derive(Debug)]
struct FieldInfo {
    f_name: Ident,
    output_format: Option<LitStr>,
}

fn do_expand(input: &DeriveInput) -> syn::Result<TokenStream> {
    let struct_name = &input.ident;
    let attrs = &input.attrs;

    let hatch_escape = get_attribute_debug_bound(attrs)?;

    let fields_info = get_fields_info(&input.data)?;

    let field_generics: Vec<syn::Path> = get_struct_field_types(input)
        .iter()
        .map(|ty| ty.path.clone())
        .collect();

    let debug_impl = generate_debug_trait(
        struct_name,
        &fields_info,
        &input.generics,
        &field_generics,
        hatch_escape,
    )?;

    let mut token_steam = TokenStream::new();
    token_steam.extend(debug_impl);

    Ok(token_steam)
}

/// get field_name and inet attribute of "debug" output format
fn get_fields_info(data: &syn::Data) -> syn::Result<Vec<FieldInfo>> {
    let mut v = vec![];

    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = data
    {
        for name in named.iter() {
            let mut output_format = None;
            for field_attr in name.attrs.iter() {
                if let Ok(syn::Meta::NameValue(m)) = field_attr.parse_meta() {
                    if m.path.is_ident("debug") {
                        if let syn::Lit::Str(debug_format) = m.lit {
                            output_format = Some(debug_format);
                            break;
                        }
                    } else {
                        return Err(syn::Error::new_spanned(m, "expected `debug(..)`"));
                    }
                }
            }

            v.push(FieldInfo {
                f_name: name.ident.clone().expect("in get_struct_field"),
                output_format,
            });
        }
    }

    Ok(v)
}

// impl std::fmt::Debug trait for
//
// pub struct Field {
//     name: &'static str,
//     bitmask: u16,
// }
//
// generate code as below:
//
// impl std::fmt::Debug for Field {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//       f.debug_struct("Field").field("name", &self.name).field("bitmask", self.bitmask).finish()
// }
fn generate_debug_trait(
    struct_name: &Ident,
    field_infos: &[FieldInfo],
    input_generics: &syn::Generics,
    field_generics: &[syn::Path],
    hatch_escape: Option<String>,
) -> syn::Result<TokenStream> {
    let struct_name_str = format!("{}", struct_name);
    let mut struct_fmt_str = String::new();
    let mut field_value: Vec<_> = vec![];

    let struct_debug_begin: String = format!("{} {{{{ ", struct_name_str);
    struct_fmt_str.push_str(&struct_debug_begin);

    let mut v = vec![];
    for field_info in field_infos.iter() {
        let field_name = &field_info.f_name;
        let attr_info = &field_info.output_format;
        let format_str = {
            if attr_info.is_some() {
                attr_info.clone().unwrap().value()
            } else {
                "{:?}".to_owned()
            }
        };

        v.push(format!("{}: {}", field_name, format_str));

        field_value.push(quote!(self.#field_name));
    }
    struct_fmt_str.push_str(&v.join(", "));

    let struct_debug_end = " }}";
    struct_fmt_str.push_str(struct_debug_end);

    let token_stream = {
        // struct generics handle
        let mut generics_params = input_generics.clone();

        // here we just care for type parameter of struct generics
        if generics_params.lt_token.is_none() || generics_params.params.is_empty() {
            // we don't need to do anything
        } else {
            // add struct generic trait bound
            let (impl_generics, _, _) = input_generics.split_for_impl();
            // eprintln!("impl_generics:{:#?}", impl_generics);

            let bounds = syn::parse::<syn::Generics>(impl_generics.to_token_stream().into())
                .expect("impl_generics into Generics");
            for bound in bounds.params.iter() {
                if let syn::GenericParam::Type(tp) = bound {
                    if !tp.bounds.is_empty() {
                        let struct_trait_bound =
                            syn::parse::<syn::WherePredicate>(bound.to_token_stream().into())
                                .expect("add struct generic trait bound");
                        generics_params
                            .make_where_clause()
                            .predicates
                            .push(struct_trait_bound);
                    }
                }
            }

            // if escape hatch provided, skip field trait
            if let Some(h) = hatch_escape {
                generics_params.make_where_clause().predicates.push(
                    syn::parse_str::<syn::WherePredicate>(&h)
                        .expect("parse attribute_debug_bound error!"),
                );
            } else {
                // add field contrait for Debug
                for fg in field_generics.iter() {
                    let field_type = fg.into_token_stream();
                    generics_params.make_where_clause().predicates.push(
                        syn::parse_str::<syn::WherePredicate>(&format!(
                            "{}: std::fmt::Debug",
                            field_type
                        ))
                        .expect("add where predicate"),
                    );
                }
            }
        }

        let (_, type_generics, where_clause) = generics_params.split_for_impl();

        quote!(
            impl #type_generics std::fmt::Debug for #struct_name #type_generics #where_clause {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, #struct_fmt_str, #(#field_value),*)
                }
            }
        )
    };

    Ok(token_stream)
}

// Only get inner type "T::Value" out of wrapped like: "Option<Box<T::Value>>
struct TypePathVisitor {
    v: Vec<syn::TypePath>,
}
impl Visit<'_> for TypePathVisitor {
    fn visit_type_path<'ast>(&mut self, node: &'_ syn::TypePath) {
        if node
            .path
            .segments
            .pairs()
            .any(|a| a.value().ident != "PhantomData")
        {
            if node.path.segments[0].arguments == syn::PathArguments::None && !self.v.contains(node)
            {
                self.v.push(node.clone());
            }

            visit::visit_type_path(self, node);
        }
    }
}

fn get_struct_field_types(st: &syn::DeriveInput) -> Vec<syn::TypePath> {
    let mut visitor = TypePathVisitor { v: vec![] };
    visitor.visit_derive_input(st);
    visitor.v
}

/// only handle attribute as below:
/// #[debug(bound="...")]
fn get_attribute_debug_bound(attrs: &[syn::Attribute]) -> syn::Result<Option<String>> {
    for attr in attrs.iter() {
        if let syn::Meta::List(syn::MetaList { path, nested, .. }) = attr.parse_meta()? {
            if path.segments[0].ident == "debug" {
                if let syn::NestedMeta::Meta(syn::Meta::NameValue(mv)) = nested.first().unwrap() {
                    if mv.path.segments[0].ident == "bound" {
                        return match &mv.lit {
                            syn::Lit::Str(litstr) => Ok(Some(litstr.value())),
                            _ => Err(syn::Error::new(
                                mv.lit.span(),
                                "expect LitStr instead of other type",
                            )),
                        };
                    }
                }
            }
        }
    }

    Ok(None)
}
