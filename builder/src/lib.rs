use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Colon2, Data, DeriveInput,
    Field, Ident, MetaNameValue, PathSegment, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match do_expand(&input) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// Reference pattern
//  #[derive(Builder)]               <--derive宏
/// pub struct Command {
///     executable: String,
///     #[builder(each = "arg")]      <-- inert属性builder，只能依附于derive Builder内被使用
///     args: Vec<String>,
///     #[builder(each = "env")]
///     env: Vec<String>,
///     current_dir: Option<String>,
/// }
#[derive(Debug)]
struct FieldInfo {
    f_name: Ident,
    f_type: TypePath,
    inner_type: Option<Ident>,
    optional: bool,
    each_ident: Option<Ident>,
}

fn do_expand(input: &DeriveInput) -> syn::Result<TokenStream> {
    let struct_name = &input.ident;
    let data = &input.data;

    let field_infos = get_fields_info(data)?;

    let mut code_generated = TokenStream::new();
    let builder_def = generate_builder_def(struct_name, &field_infos)?;
    let builder_impl = generate_builder_impl(struct_name, &field_infos)?;
    let setters = generate_setters(struct_name, &field_infos)?;
    let attribute_setters_fn = generate_attribute_setters(struct_name, &field_infos)?;
    let build_fn = generate_build(struct_name, &field_infos)?;

    code_generated.extend(builder_def);
    code_generated.extend(builder_impl);
    code_generated.extend(setters);
    code_generated.extend(attribute_setters_fn);
    code_generated.extend(build_fn);
    Ok(code_generated)
}

/// get field info from struct
fn get_fields_info(data: &Data) -> syn::Result<Vec<FieldInfo>> {
    let mut v = vec![];
    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = data {
        for field in fields {
            if let syn::Type::Path(ty) = &field.ty {
                let inner_type = get_inner_type(ty.path.segments.first().unwrap());
                let optional = is_wrapped_by_option(&ty.path.segments);
                let each_ident = get_field_attr(field)?;

                let f = FieldInfo {
                    f_name: field.ident.clone().expect("field_name cannot be empty"),
                    f_type: ty.clone(),
                    inner_type,
                    optional,
                    each_ident,
                };

                v.push(f);
            } else {
                return Err(syn::Error::new(
                    field.span(),
                    "only parse TypePath for field type",
                ));
            }
        }
    } else {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "only parse Builder for struct",
        ));
    }

    Ok(v)
}

/// make CommandBuilder definition
/// struct CommandBuilder {
///    executable: Option<String>,
///    args: Vec<String>,
///    current_dir: Option<String>,   //current_dir is optional in Command
/// }
fn generate_builder_def(struct_id: &Ident, field_infos: &[FieldInfo]) -> syn::Result<TokenStream> {
    let builder_name: Ident = Ident::new(&format!("{}Builder", struct_id), struct_id.span());

    let builder_def: Vec<_> = field_infos
        .iter()
        .map(|field_info| {
            let fname = &field_info.f_name;
            let ftype = &field_info.f_type;

            if field_info.optional || field_info.each_ident.is_some() {
                quote!(#fname: #ftype)
            } else {
                quote!(#fname: std::option::Option<#ftype>)
            }
        })
        .collect();

    let token_stream = quote!(
        pub struct #builder_name {
           #(#builder_def),*
        }
    );

    Ok(token_stream)
}

/// make builder() fucntion
/// impl CommandBuilder {
///    pub fn builder() -> CommandBuilder {
///       CommandBuilder {
///          field: Option or vec![],
///       }
///    }
/// }
fn generate_builder_impl(struct_id: &Ident, field_infos: &[FieldInfo]) -> syn::Result<TokenStream> {
    let builder_name: Ident = Ident::new(&format!("{}Builder", struct_id), struct_id.span());
    let mut field_set_impl: Vec<_> = vec![];

    for field_info in field_infos.iter() {
        let field_name = &field_info.f_name;
        let attr_ident = &field_info.each_ident;

        if attr_ident.is_some() {
            field_set_impl.push(quote!(
                #field_name: vec![]
            ));
        } else {
            field_set_impl.push(quote!(
                #field_name: std::option::Option::None
            ));
        }
    }
    // eprintln!("field_set_impl:{:#?}", field_set_impl);

    let token_stream = quote!(
        impl #struct_id {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#field_set_impl),*
                }
            }
        }
    );

    Ok(token_stream)
}

/// make every field setter function
/// impl CommandBuilder {
///    fn executable(&mut self, executable: String) -> &mut Self {
///       self.executable = Some(executable);
///       self
///    }
/// }
fn generate_setters(struct_id: &Ident, field_infos: &[FieldInfo]) -> syn::Result<TokenStream> {
    let builder_name: Ident = Ident::new(&format!("{}Builder", struct_id), struct_id.span());

    let setter_impl: Vec<_> = field_infos
        .iter()
        .map(|field_info| {
            let fname = &field_info.f_name;
            let ftype = &field_info.f_type;
            let attr_decorated = &field_info.each_ident;
            if attr_decorated.is_some() {
                if attr_decorated.as_ref() != Some(fname) {
                    quote!(
                        fn #fname(&mut self, value:#ftype) -> &mut Self {
                            self.#fname = value;
                            self
                        }
                    )
                } else {
                    quote!()
                }
            } else if field_info.optional {
                let inner_type = get_inner_type(
                    ftype
                        .path
                        .segments
                        .first()
                        .expect("should contains at least one path"),
                )
                .unwrap();
                quote!(
                    fn #fname(&mut self, value:#inner_type) -> &mut Self {
                        self.#fname = std::option::Option::Some(value);
                        self
                    }
                )
            } else {
                quote!(
                    fn #fname(&mut self, value:#ftype) -> &mut Self {
                        self.#fname = std::option::Option::Some(value);
                        self
                    }
                )
            }
        })
        .collect();

    let token_stream = quote!(
        impl #builder_name {
            #(#setter_impl)*
        }
    );

    Ok(token_stream)
}

///impl CommandBuilder {
///   fn arg(&mut self, value: String) -> &mut Self {
///      self.args.push(value);
///       self
///   }
///   fn env(&mut self, value: String) -> &mut Self {
///      self.env.push(value);
///      self
///   }
///}
fn generate_attribute_setters(
    struct_id: &Ident,
    field_infos: &[FieldInfo],
) -> syn::Result<TokenStream> {
    let builder_name: Ident = Ident::new(&format!("{}Builder", struct_id), struct_id.span());
    let mut attr_setter_impl = vec![];

    for field_info in field_infos
        .iter()
        .filter(|field_info| field_info.each_ident.is_some())
    {
        let fname = &field_info.f_name;
        let inner_type = &field_info
            .inner_type
            .clone()
            .expect("should contain inner_type");
        let attr_name = &field_info
            .each_ident
            .clone()
            .expect("should contains \"each\" attribute");

        attr_setter_impl.push(quote!(
            fn #attr_name(&mut self, value: #inner_type) -> &mut Self {
                self.#fname.push(value);
                self
            }
        ));
    }

    let token_stream = quote!(
        impl #builder_name {
            #(#attr_setter_impl)*
        }
    );

    Ok(token_stream)
}

//     impl CommandBuilder {
//         pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
//             if self.executable.is_none() { return Err(Error(...)) }
//             Ok(Command {
//                 executable: self.executable;
//             })
//         }
//     }
fn generate_build(struct_id: &Ident, field_infos: &[FieldInfo]) -> syn::Result<TokenStream> {
    let builder_name: Ident = Ident::new(&format!("{}Builder", struct_id), struct_id.span());
    let mut field_check = vec![];
    let mut build_struct = vec![];

    for field_info in field_infos.iter() {
        let field_name = field_info.f_name.clone();

        if !field_info.optional && field_info.each_ident.is_none() {
            field_check.push(quote!(
                if self.#field_name.is_none() {
                    let err_str = format!("lack of {} value", stringify!(#field_name));
                    return std::result::Result::Err(std::boxed::Box::<dyn std::error::Error>::from(err_str));
                }
            ));
        }

        if field_info.optional || field_info.each_ident.is_some() {
            build_struct.push(quote!(
                #field_name: self.#field_name.clone()
            ));
        } else {
            build_struct.push(quote!(
                #field_name: self.#field_name.clone().unwrap()
            ));
        }
    }

    let token_stream = quote!(
        impl #builder_name {
            pub fn build(&mut self) -> std::result::Result<#struct_id, std::boxed::Box<dyn std::error::Error>> {
                #(#field_check)*

                Ok(#struct_id {
                    #(#build_struct),*
                })
            }
        }
    );

    Ok(token_stream)
}

// PathSegment {
//     ident: Ident {
//         ident: "String",
//         span: #0 bytes(86..92),
//     },
//     arguments: None,
// }
// or
// PathSegment {
//     ident: Ident {
//         ident: "Option",
//         span: #0 bytes(156..162),
//     },
//     arguments: AngleBracketed(
//         AngleBracketedGenericArguments {
//             colon2_token: None,
//             lt_token: Lt,
//             args: [
//                 Type(
//                     Path(
//                         TypePath {
//                             qself: None,
//                             path: Path {
//                                 leading_colon: None,
//                                 segments: [
//                                     PathSegment {
//                                         ident: Ident {
//                                             ident: "String",
//                                             span: #0 bytes(163..169),
//                                         },
//                                         arguments: None,
//                                     },
//                                 ],
//                             },
//                         },
//                     ),
//                 ),
//             ],
//             gt_token: Gt,
//         },
//     ),
// }
fn get_inner_type(path: &PathSegment) -> Option<Ident> {
    if path.arguments == syn::PathArguments::None {
        Some(path.ident.clone())
    } else {
        if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
            args,
            ..
        }) = &path.arguments
        {
            let l = args.last().unwrap();
            if let syn::GenericArgument::Type(syn::Type::Path(p)) = l {
                let full_path = &p.path.segments;
                let l = full_path.last().unwrap();
                return Some(l.ident.clone());
            }
        }
        None
    }
}

/// check if type wrapped by Option
/// Option<..> or std::option::Option<...>
fn is_wrapped_by_option(path: &Punctuated<PathSegment, Colon2>) -> bool {
    path.first()
        .expect("should contain at least one type")
        .ident
        == "Option"
}

fn get_field_attr(field: &Field) -> syn::Result<Option<Ident>> {
    for field_attr in field.attrs.iter() {
        if let Ok(syn::Meta::List(m)) = field_attr.parse_meta() {
            if m.path.is_ident("builder") {
                if let std::option::Option::Some(syn::NestedMeta::Meta(syn::Meta::NameValue(
                    MetaNameValue {
                        lit: syn::Lit::Str(s),
                        path,
                        ..
                    },
                ))) = m.nested.first()
                {
                    if path.segments.first().unwrap().ident == "each" {
                        return Ok(Some(Ident::new(&s.value(), s.span())));
                    } else {
                        return Err(syn::Error::new_spanned(
                            m,
                            "expected `builder(each = \"...\")`",
                        ));
                    }
                }
            }
        }
    }

    Ok(None)
}
