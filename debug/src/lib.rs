use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{
    parse_quote, parse_str, punctuated::Punctuated, token::Comma, visit::Visit, DeriveInput, Field,
    GenericArgument, Path, Type, TypePath,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ref input = syn::parse_macro_input!(input as DeriveInput);
    match do_expand(input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;
    let struct_name = struct_ident.to_string();
    let fields = get_fields_from_derive_input(input)?;

    // generate body
    let mut fmt_body = quote! {
        f.debug_struct(#struct_name)
    };
    for i in 0..fields.len() {
        let ident = fields[i].ident.as_ref().unwrap();
        let name = ident.to_string();
        let debug_format = get_name_value_from_field_attr(&fields[i], "debug")?;
        if let Some(format) = debug_format {
            fmt_body.extend(quote! {
                .field(#name, &std::format_args!(#format, self.#ident))
            })
        } else {
            fmt_body.extend(quote! {
                .field(#name, &self.#ident)
            })
        }
    }
    fmt_body.extend(quote! {
        .finish()
    });

    // generate bounds
    let mut generics = input.generics.clone();
    // using escape hatch
    if let Some(hatch) = get_hatch_from_attr(input) {
        generics
            .make_where_clause()
            .predicates
            .push(parse_str(&hatch)?);
    } else {
        // the types that are directly relied on
        let need_debug_idents = fields.iter().fold(HashSet::new(), |mut ret, field| {
            if let Type::Path(TypePath { ref path, .. }) = field.ty {
                if let Some(ident) = path.get_ident() {
                    ret.insert(ident);
                }
            }
            ret
        });

        // the types that are in the phantom data
        let idents_in_phantom_data = fields.iter().fold(HashSet::new(), |mut ret, field| {
            if let Type::Path(TypePath { ref path, .. }) = field.ty {
                if path.segments[0].ident == "PhantomData" {
                    if let Some(&Type::Path(TypePath { ref path, .. })) =
                        get_inner_type_from_path(&path)
                    {
                        if let Some(ident) = path.get_ident() {
                            ret.insert(ident);
                        }
                    }
                }
            }
            ret
        });

        let associated_types = get_generic_associated_types(input);

        for g in generics.params.iter_mut() {
            if let syn::GenericParam::Type(t) = g {
                // directly rely on this Type
                if need_debug_idents.contains(&t.ident) {
                    t.bounds.push(parse_quote!(std::fmt::Debug));
                } else if idents_in_phantom_data.contains(&t.ident)
                    || associated_types.contains_key(&t.ident)
                {
                    continue;
                } else {
                    t.bounds.push(parse_quote! {std::fmt::Debug})
                }
            }
        }
        for (_, types) in associated_types {
            for ty in types {
                generics
                    .make_where_clause()
                    .predicates
                    .push(parse_quote! {#ty:std::fmt::Debug});
            }
        }
    }

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics std::fmt::Debug for #struct_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #fmt_body
            }
        }
    })
}

fn get_name_value_from_field_attr(f: &Field, name: &str) -> syn::Result<Option<String>> {
    for attr in &f.attrs {
        if attr.path.is_ident(name) {
            if let syn::Meta::NameValue(syn::MetaNameValue {
                lit: syn::Lit::Str(ref v),
                ..
            }) = attr.parse_meta()?
            {
                return Ok(Some(v.value()));
            }
        }
    }
    Ok(None)
}

fn get_fields_from_derive_input(input: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(
        input,
        "Must be defined on a struct",
    ))
}

fn get_inner_type_from_path(path: &Path) -> Option<&Type> {
    if path.segments.len() < 1 {
        return None;
    }
    if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        ref args,
        ..
    }) = path.segments[0].arguments
    {
        if let GenericArgument::Type(ty) = args.first().unwrap() {
            return Some(ty);
        }
    }
    None
}

fn get_generic_associated_types(input: &DeriveInput) -> HashMap<Ident, Vec<TypePath>> {
    #[derive(Default)]
    struct TypePathVisitor {
        generic_type_name: HashSet<String>,
        associated_types: HashMap<Ident, Vec<TypePath>>,
    }
    impl<'ast> Visit<'ast> for TypePathVisitor {
        fn visit_type_path(&mut self, i: &'ast TypePath) {
            if self
                .generic_type_name
                .contains(&i.path.segments[0].ident.to_string())
                && i.path.segments.len() > 1
            {
                self.associated_types
                    .entry(i.path.segments[0].ident.clone())
                    .or_insert_with(|| vec![i.to_owned()]);
            }
            syn::visit::visit_type_path(self, i);
        }
    }
    let mut visitor = TypePathVisitor::default();
    for generic_type in input.generics.type_params() {
        visitor
            .generic_type_name
            .insert(generic_type.ident.to_string());
    }

    visitor.visit_derive_input(input);
    visitor.associated_types
}

fn get_hatch_from_attr(input: &DeriveInput) -> Option<String> {
    for attr in &input.attrs {
        if attr.path.is_ident("debug") {
            if let Ok(syn::Meta::List(syn::MetaList { ref nested, .. })) = attr.parse_meta() {
                for meta in nested {
                    if let syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                        ref path,
                        ref lit,
                        ..
                    })) = meta
                    {
                        if path.is_ident("bound") {
                            if let syn::Lit::Str(lit) = lit {
                                return Some(lit.value());
                            }
                        }
                    }
                }
            }
        }
    }
    None
}
