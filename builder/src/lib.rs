use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Comma, DataStruct,
    DeriveInput, Field, Fields, FieldsNamed,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match do_expand(&input) {
        Ok(expanded) => expanded.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let builder_struct_def = generate_builder_struct_def(input)?;
    let struct_impl_def = generate_struct_impl_def(input)?;
    let builder_impl_def = generate_builder_impl_def(input)?;
    Ok(quote! {
        #builder_struct_def
        #struct_impl_def
        #builder_impl_def
    })
}
fn generate_builder_struct_def(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;
    let builder_ident = syn::Ident::new(&format!("{struct_ident}Builder"), input.span());
    let fields = get_fields_from_derive_input(input)?;

    let mut declare_clause = Vec::<proc_macro2::TokenStream>::new();
    for i in 0..fields.len() {
        let ident = &fields[i].ident;
        let original_type = &fields[i].ty;
        let option_inner_type = get_inner_type(original_type, "Option");
        let vec_inner_type = get_inner_type(original_type, "Vec");
        if let Some(ty) = option_inner_type {
            declare_clause.push(quote! {
                #ident: std::option::Option<#ty>
            })
        } else if let Some(_) = vec_inner_type {
            declare_clause.push(quote! {
                #ident: #original_type
            })
        } else {
            declare_clause.push(quote! {
                #ident: std::option::Option<#original_type>
            })
        }
    }

    Ok(quote! {
        pub struct #builder_ident {
            #(#declare_clause),*
        }
    })
}
fn generate_struct_impl_def(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;
    let builder_ident = syn::Ident::new(&format!("{struct_ident}Builder"), input.span());
    let fields = get_fields_from_derive_input(input)?;
    let mut bind_clause = Vec::<proc_macro2::TokenStream>::new();
    for i in 0..fields.len() {
        let ident = &fields[i].ident;
        let original_type = &fields[i].ty;
        let vec_inner_type = get_inner_type(original_type, "Vec");
        if let Some(_) = vec_inner_type {
            bind_clause.push(quote! {
                #ident: std::vec::Vec::new()
            })
        } else {
            bind_clause.push(quote! {
                #ident: std::option::Option::None
            })
        }
    }
    Ok(quote! {
        impl #struct_ident {
            pub fn builder() -> #builder_ident{
                #builder_ident {
                    #(#bind_clause),*
                }
            }
        }
    })
}

fn generate_builder_impl_def(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;
    let builder_ident = syn::Ident::new(&format!("{struct_ident}Builder"), input.span());
    let fields = get_fields_from_derive_input(input)?;

    let mut setter_clause = Vec::<proc_macro2::TokenStream>::new();
    let mut checker_clause = Vec::<proc_macro2::TokenStream>::new();
    let mut bind_clause = Vec::<proc_macro2::TokenStream>::new();
    for i in 0..fields.len() {
        let ident = &fields[i].ident;
        let original_type = &fields[i].ty;
        let custom_ident = get_attr_args(&fields[i].attrs, "each")?;
        let option_inner_type = get_inner_type(original_type, "Option");
        let vec_inner_type = get_inner_type(original_type, "Vec");

        if custom_ident.is_some() && vec_inner_type.is_none() {
            return syn::Result::Err(syn::Error::new(
                fields[i].span(),
                "`each` can only be defined on Vec",
            ));
        }

        if let Some(ty) = option_inner_type {
            setter_clause.push(quote! {
                fn #ident(&mut self, #ident:#ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            });
            bind_clause.push(quote! {
                #ident: self.#ident.clone()
            })
        } else if let Some(ty) = vec_inner_type {
            if custom_ident.is_some() {
                setter_clause.push(quote! {
                    fn #custom_ident(&mut self, #ident:#ty) -> &mut Self {
                        self.#ident.push(#ident);
                        self
                    }
                });
            }
            if *ident != custom_ident {
                setter_clause.push(quote! {
                    fn #ident(&mut self, #ident:#original_type) -> &mut Self{
                        self.#ident = #ident;
                        self
                    }
                })
            }
            bind_clause.push(quote! {
                #ident: self.#ident.clone()
            })
        } else {
            setter_clause.push(quote! {
                fn #ident(&mut self, #ident:#original_type) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            });
            checker_clause.push(quote! {
                if self.#ident.is_none(){
                    let err = format!("{} is missing", stringify!(#ident));
                    return std::result::Result::Err(err.into());
                }
            });
            bind_clause.push(quote! {
                #ident: self.#ident.clone().unwrap()
            })
        }
    }

    Ok(quote! {
        impl #builder_ident {
            #(#setter_clause)*

            pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
                #(#checker_clause)*

                std::result::Result::Ok(
                    #struct_ident {
                        #(#bind_clause,)*
                    }
                )
            }
        }
    })
}

fn get_fields_from_derive_input(input: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    if let syn::Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        Ok(named)
    } else {
        Err(syn::Error::new_spanned(input, "Must define on struct"))
    }
}

fn get_inner_type<'a>(ty: &'a syn::Type, outer_type: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = ty
    {
        if segments[0].ident.to_string() == outer_type {
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                ref args,
                ..
            }) = segments[0].arguments
            {
                if let syn::GenericArgument::Type(ref inner_type) = args[0] {
                    return Some(inner_type);
                }
            }
        }
    }
    None
}

fn get_attr_args(attrs: &Vec<syn::Attribute>, arg_name: &str) -> syn::Result<Option<syn::Ident>> {
    for attr in attrs {
        if attr.path.is_ident("builder") {
            if let Ok(syn::Meta::NameValue(value)) = attr.parse_args() {
                if value.path.is_ident(arg_name) {
                    if let syn::Lit::Str(val) = value.lit {
                        return Ok(Some(syn::Ident::new(&val.value(), attr.span())));
                    }
                } else {
                    return Err(syn::Error::new_spanned(
                        attr.parse_meta()?,
                        r#"expected `builder(each = "...")`"#,
                    ));
                }
            }
        }
    }
    Ok(None)
}
