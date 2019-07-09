#![recursion_limit = "128"]
extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Ident, Type};
use quote::quote;

#[derive(Debug)]
enum FieldType<'a> {
    Optionized(&'a Type),
    Extending(Option<(bool, proc_macro2::TokenStream)>),
    Plain,
}

#[derive(Debug)]
struct BuilderField<'a> {
    name: &'a Option<Ident>,
    field_type: FieldType<'a>,
    ty: &'a Type,
}

fn builder_field<'a>(name: &'a Option<Ident>, field_type: FieldType<'a>, ty: &'a Type) -> BuilderField<'a> {
    BuilderField { name: name, field_type: field_type, ty: ty }
}

impl BuilderField<'_> {
    fn struct_field(&self) -> proc_macro2::TokenStream {
        let name = self.name;
        let ty = self.ty;
        match self.field_type {
            FieldType::Optionized(_) => quote! {#name: #ty},
            FieldType::Extending(_) => quote! {#name: #ty},
            FieldType::Plain => quote! {#name: std::option::Option<#ty>}
        }
    }
    fn methods(&self) -> proc_macro2::TokenStream {
        let name = self.name;
        let ty = self.ty;
        match &self.field_type {
            FieldType::Optionized(option_ty) => quote! {
             fn #name(&mut self, #name: #option_ty) -> &mut Self {
             self.#name = std::option::Option::Some(#name);
             self
             }
         },
            FieldType::Extending(None) => quote! {
                 fn #name(&mut self, #name: #ty) -> &mut Self {
                 self.#name.push(#name);
                 self
                }
             },
            FieldType::Extending(Some ((false, extend_method))) => quote! {
                 fn #name(&mut self, #name: #ty) -> &mut Self {
                 self.#name = #name;
                 self
                }
                #extend_method
             },
            FieldType::Extending(Some ((true, extend_method))) => quote! {
                #extend_method
             },
            FieldType::Plain => quote! {
             fn #name(&mut self, #name: #ty) -> &mut Self {
             self.#name = std::option::Option::Some(#name);
             self
             }
         }
        }
    }
    fn setters(&self) -> proc_macro2::TokenStream {
        let name = self.name;
        match self.field_type {
            FieldType::Plain => quote! {#name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?},
            _ => quote! {#name: self.#name.clone()},
        }
    }
    fn empties(&self) -> proc_macro2::TokenStream {
        let name = self.name;
        match self.field_type {
            FieldType::Extending(_) => quote! {#name: std::vec::Vec::new()},
            _ => quote! {#name: std::option::Option::None},
        }

    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let builder_name = format!("{}Builder", name);
    let builder_ident = syn::Ident::new(&builder_name, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
                                              fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }), ..
                                          }) = ast.data {
        named
    } else {
        unimplemented!();
    };

    let mut builder_fields = Vec::new();

    fields.iter().for_each(|f| {
        let ty = &f.ty;
        let name = &f.ident;
        if let Some(option_ty) = ty_is_option(ty) {
            builder_fields.push(builder_field(name, FieldType::Optionized(option_ty), ty));
        } else if builder_of(f).is_some() {
            let extentable =  extend_method(&f);
            builder_fields.push(builder_field(name, FieldType::Extending(extentable), ty));
        } else {
            builder_fields.push(builder_field(name, FieldType::Plain, ty));
        }
    });

    //eprintln!("fields before {:#?}", builder_fields);

    let build_empty = builder_fields.iter().map(|f| {
        f.empties()
    });

    let methods = builder_fields.iter().map(|f| {
        f.methods()
    });

    let setters = builder_fields.iter().map(|f| {
        f.setters()
    });

    let struct_fields = builder_fields.iter().map(|f| {
        f.struct_field()
    });

    let expanded = quote! {
        pub struct #builder_ident {
             #(#struct_fields,)*
        }

        impl #builder_ident {
            #(#methods)*


             pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                 std::result::Result::Ok(#name {
                    #(#setters,)*
                 })
             }
        }

        impl #name {
             pub fn builder() -> #builder_ident {
                 #builder_ident {
                         #(#build_empty,)*
                 }
             }
         }
    };
    expanded.into()
}

fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
            return Some(attr);
        }
    }
    None
}

fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = f.ident.as_ref().unwrap();
    let g = builder_of(f)?;

    let meta = match g.parse_meta() {
        Ok(syn::Meta::List(mut nvs)) => {
            // list here is .. in #[builder(..)]
            assert_eq!(nvs.ident, "builder");
            if nvs.nested.len() != 1 {
                return mk_err(nvs);
            }

            // nvs.nested[0] here is (hopefully): each = "foo"
            match nvs.nested.pop().unwrap().into_value() {
                syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                    if nv.ident != "each" {
                        return mk_err(nvs);
                    }
                    nv
                }
                meta => {
                    // nvs.nested[0] was not k = v
                    return mk_err(meta);
                }
            }
        }
        Ok(meta) => {
            // inside of #[] there was either just an identifier (`#[builder]`) or a key-value
            // mapping (`#[builder = "foo"]`), neither of which are okay.
            return mk_err(meta);
        }
        Err(e) => {
            return Some((false, e.to_compile_error()));
        }
    };

    match meta.lit {
        syn::Lit::Str(s) => {
            let arg = syn::Ident::new(&s.value(), s.span());
            let inner_ty = ty_is_extended("Vec", &f.ty).unwrap();
            let method = quote! {
                pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                    self.#name.push(#arg);
                    self
                }
            };
            Some((&arg == name, method))
        }
        lit => panic!("expected string, found {:?}", lit),
    }
}

fn ty_is_extended<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty.value() {
                return Some(t);
            }
        }
    }
    None
}

fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
    Some((
        false,
        syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
    ))
}

fn ty_is_option(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != "Option" {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty.value() {
                return Some(t);
            }
        }
    }
    None
}