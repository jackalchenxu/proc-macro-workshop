use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Fields, Ident, ItemStruct, Type};

#[proc_macro_attribute]
pub fn bitfield(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let ts = parse_macro_input!(input as ItemStruct);

    let mut token_stream = TokenStream::new();

    match expand(&ts) {
        Ok(ret) => {
            token_stream.extend(ret);
            token_stream.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(BitfieldSpecifier)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match build_spec(&input) {
        Ok(token_stream) => {
            // eprintln!("#######\n{}", token_stream);
            token_stream.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
}

fn build_spec(input: &DeriveInput) -> syn::Result<TokenStream> {
    match &input.data {
        syn::Data::Enum(enum_input) => {
            let name = &input.ident;
            let cnt = enum_input.variants.iter().count();
            if !cnt.is_power_of_two() {
                return Err(syn::Error::new(
                    proc_macro2::Span::call_site(),
                    "BitfieldSpecifier expected a number of variants which is a power of 2",
                ));
            }
            let width = (cnt as f32).log2() as usize;
            let fields: Vec<Ident> = enum_input
                .variants
                .iter()
                .map(|v| v.ident.clone())
                .collect();

            let name_str = format!("{}", name);

            Ok(quote!(
                impl Specifier for #name {
                    const BITS: usize = #width;
                    type GetterType = #name;
                    type SetterType = #name;

                    #[inline]
                    fn from_u64(val: u64) -> Self::GetterType {
                        match val{
                            #(x if x == Self::#fields as u64 => Self::#fields),*,
                            _ => {
                                panic!("error when convert {} from u64 to {}", val, #name_str);
                            }
                        }
                    }

                    #[inline]
                    fn into_u64(val: Self::SetterType) -> u64 {
                        val as u64
                    }
                }

                impl #name {
                    pub fn _check_discriminant_in_range(){
                        // let inbound:bool = #(#fields)&*&(2.pow(#cnt));
                        let _: ::bitfield::CheckDiscriminantInRange<[u8; (( #(Self::#fields as usize) |* ) & (#cnt))]>;
                    }
                }
            ))
        }
        _ => Err(syn::Error::new_spanned(
            input,
            "only parse enum for BitfieldSpecifier",
        )),
    }
}

fn expand(ts: &ItemStruct) -> syn::Result<TokenStream> {
    let struct_name = &ts.ident;
    let field_type = get_field_type(&ts.fields)?;

    let total_bits = quote!(
        #(
            <#field_type as ::bitfield::Specifier>::BITS as usize
        )+*
    );

    let struct_def = gen_struct_def(struct_name, &total_bits);

    let mut checks = vec![];
    checks.push(quote!(
        let _: ::bitfield::CheckTotalSizeIsMultipleOfEightBits<[u8; (#total_bits) % 8]>;
    ));
    checks.extend(parse_bits_attr(&ts.fields));

    // make struct impl template, add new() fn
    let struct_new_impl = quote!(
        fn new() -> Self {
            #(#checks)*
            Self { data: [0u8; (#total_bits) / 8] }
        }
    );

    let fields = get_field_info(&ts.fields)?;

    let helper_fn = gen_helper(&fields)?;
    let getter = gen_getters(&fields)?;
    let setter = gen_setters(&fields)?;

    let token_stream = quote!(
        #struct_def
        impl #struct_name {
            #struct_new_impl
            #helper_fn
            #getter
            #setter
        }
    );

    Ok(token_stream)
}

// make struct definition
fn gen_struct_def(struct_name: &Ident, total_bits: &TokenStream) -> TokenStream {
    quote!(
        pub struct #struct_name {
            data: [u8; (#total_bits) / 8],
        }
    )
}

/// generate helper definition and functions for getter/setter
fn gen_helper(fields: &[FieldInfo]) -> syn::Result<TokenStream> {
    let mut const_names = vec![];
    let mut offset_value = vec![];
    for i in 0..fields.len() {
        const_names.push(
            syn::parse_str::<Ident>(&format!("{}_OFFSET", fields[i].f_name).to_uppercase())
                .expect("parse Field_name_OFFSET"),
        );
        if i == 0 {
            offset_value.push(quote!(0));
        } else {
            let ty = &fields[i - 1].f_type;
            let prev_offset = &const_names[i - 1];
            offset_value.push(quote!(<#ty as Specifier>::BITS + Self::#prev_offset));
        }
    }

    let gen_const_offset = quote!(
        #(
            const #const_names : usize = #offset_value;
        )*
    );

    let get_set_bit_fn = quote!(
        //获取self data值的offset位的bit
        fn get_bit(&self, offset: usize) -> bool {
            let byte_index = offset / 8;
            let bit_index = offset % 8;
            (((1 << bit_index) & self.data[byte_index]) >> bit_index) == 1
        }

        //设置self data值的offset位的bit
        fn set_bit(&mut self, offset: usize, set_val: bool) {
            let byte_index = offset / 8;
            let bit_index = offset % 8;
            self.data[byte_index] |= (set_val as u8) << bit_index
        }
    );

    Ok(quote!(
        #gen_const_offset
        #get_set_bit_fn
    ))
}

// generate blocks:
// impl MyFourBytes {
//   get_a(&self) -> u8 { .... }
// }
fn gen_getters(fields: &[FieldInfo]) -> syn::Result<TokenStream> {
    let mut impl_items = vec![];

    for field in fields.iter() {
        let f_name = &field.f_name;
        let f_type = &field.f_type;
        let fn_name = syn::parse_str::<Ident>(&format!("get_{f_name}")).expect("parse fn_name");
        let offset = syn::parse_str::<Ident>(&format!("{f_name}_OFFSET").to_uppercase())
            .expect("parse Field_name_OFFSET");

        impl_items.push(quote!(
            fn #fn_name(&self) -> <#f_type as Specifier>::GetterType {
                let mut val = 0;
                for i in 0..<#f_type as Specifier>::BITS {
                    val = ((self.get_bit(Self::#offset + i) as u64) << i) + val;
                }
                <#f_type as Specifier>::from_u64(val)
            }
        ));
    }

    Ok(quote!(
        #(#impl_items)*
    ))
}

// generate blocks:
// impl MyFourBytes {
//   set_c(&mut self, val: u8) { ...  }
//}
fn gen_setters(fields: &[FieldInfo]) -> syn::Result<TokenStream> {
    let mut impl_items = vec![];

    for field in fields.iter() {
        let f_name = &field.f_name;
        let f_type = &field.f_type;
        let fn_name = syn::parse_str::<Ident>(&format!("set_{f_name}")).expect("parse fn_name");
        let offset = syn::parse_str::<Ident>(&format!("{f_name}_OFFSET").to_uppercase())
            .expect("parse Field_name_OFFSET");

        impl_items.push(quote!(
            fn #fn_name(&mut self, set_val: <#f_type as Specifier>::SetterType) {
                for i in 0..<#f_type as Specifier>::BITS {
                    let mask = 1 << i;
                    let bit = mask == (mask & <#f_type as Specifier>::into_u64(set_val));
                    self.set_bit(Self::#offset + i, bit);
                }
            }
        ));
    }

    Ok(quote!(
        #(#impl_items)*
    ))
}

/// store field info like as:
/// f_name: b
/// f_type: B3
struct FieldInfo {
    f_name: Ident,
    f_type: Type,
}

fn get_field_info(fields: &Fields) -> syn::Result<Vec<FieldInfo>> {
    let mut v = vec![];

    match fields {
        syn::Fields::Named(fs) => {
            for field in fs.named.iter() {
                let inner_type = field.ty.clone();
                if field.ident.is_some() {
                    v.push(FieldInfo {
                        f_name: field.ident.clone().expect("should contain field name"),
                        f_type: inner_type,
                    });
                }
            }
        }
        _ => {
            return Err(syn::Error::new_spanned(
                fields,
                "only support bitfield on struct definition",
            ));
        }
    }

    Ok(v)
}

fn get_field_type(fields: &Fields) -> syn::Result<Vec<Type>> {
    let mut v = vec![];

    match fields {
        syn::Fields::Named(fs) => {
            for field in fs.named.iter() {
                let inner_type = field.ty.clone();
                v.push(inner_type);
            }
        }
        _ => {
            return Err(syn::Error::new_spanned(
                fields,
                "only support bitfield on struct definition",
            ));
        }
    }

    Ok(v)
}

fn parse_bits_attr(fields: &Fields) -> Vec<TokenStream> {
    let mut v = vec![];

    if let syn::Fields::Named(fs) = fields {
        for field in fs.named.iter() {
            for attr in field.attrs.iter() {
                if let Ok(syn::Meta::NameValue(res)) = syn::Attribute::parse_meta(attr) {
                    if res.path.segments.len() == 1
                        && res.path.segments.first().unwrap().ident == "bits"
                    {
                        if let syn::Lit::Int(bits) = res.lit {
                            let field_type = &field.ty;
                            v.push(quote!(
                                let _: ::bitfield::CheckOccupiedAsItsDeclare<[u8; (<#field_type as Specifier>::BITS == #bits) as usize]>;
                            ));
                        }
                    }
                }
            }
        }
    }

    v
}

#[proc_macro]
pub fn define_bit_field_specifiers(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut code = TokenStream::new();

    for width in 0_usize..=64_usize {
        let span = Span::call_site();
        let long_name = Ident::new(&format!("BitField{}", width), span);
        let short_name = Ident::new(&format!("B{}", width), span);

        let default_field_type = if width <= 8 {
            quote!(u8)
        } else if width <= 16 {
            quote!(u16)
        } else if width <= 32 {
            quote!(u32)
        } else {
            quote!(u64)
        };

        code.extend(quote! {
            pub struct #long_name;
            pub use self::#long_name as #short_name;

            impl Specifier for #long_name {
                const BITS: usize = #width;
                type SetterType = #default_field_type;
                type GetterType = #default_field_type;

                #[inline]
                fn from_u64(val: u64) -> Self::GetterType {
                    val as Self::GetterType
                }

                #[inline]
                fn into_u64(val: Self::SetterType) -> u64 {
                    val as u64
                }
            }
        });
    }

    code.into()
}
