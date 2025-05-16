use std::sync::atomic;

use proc_macro::{Span, TokenStream};
use quote::ToTokens;
use syn::{Attribute, ItemFn, ReturnType};

static IDX: atomic::AtomicUsize = atomic::AtomicUsize::new(0);

fn no_inline_attr() -> Attribute {
    syn::parse_quote! {
        #[inline(never)]
    }
}

fn dereference_type(t: &syn::Type) -> &syn::Type {
    match t {
        syn::Type::Reference(r) => dereference_type(&r.elem),
        others => others,
    }
}

fn insert_eq_function(item_fn: &mut ItemFn, user_eq_name: &str, function_name: &str) {
    let eq_function_name = syn::Ident::new(function_name, Span::call_site().into());
    let user_eq_name: syn::ExprPath =
        syn::parse_str(user_eq_name).expect("expected ExprPath, e.g. i32::eq");

    let return_type = match &item_fn.sig.output {
        ReturnType::Type(_, t) => t,
        _ => panic!("Function must have a return type"),
    };

    item_fn.sig.inputs.iter().for_each(|arg| {
        let is_ref = match arg {
            syn::FnArg::Typed(p) => matches!(&*p.ty, syn::Type::Reference(_)),
            syn::FnArg::Receiver(p) => p.reference.is_some(),
        };
        assert!(is_ref, "arg: {:?} must be a reference", arg.to_token_stream().to_string());
    });

    let return_type = dereference_type(return_type);
    item_fn.block.stmts.insert(
        0,
        syn::parse_quote! {
                // eq function
                #[inline(never)]
                #[unsafe(no_mangle)]
                pub fn #eq_function_name(x : & #return_type , y: & #return_type) -> bool {
                    return #user_eq_name(&x, &y);
                }
        },
    );
}

fn insert_identifier_function(item_fn: &mut ItemFn, function_name: &str) {
    let identifier_function_name = syn::Ident::new(function_name, Span::call_site().into());
    item_fn.block.stmts.insert(
        0,
        syn::parse_quote! {
        extern "C" {
            // identifier function
            pub fn #identifier_function_name();
        }
        },
    );
}
fn insert_identifier_call(item_fn: &mut ItemFn, function_name: &str) {
    let identifier_function_name = syn::Ident::new(function_name, Span::call_site().into());
    item_fn.block.stmts.insert(
        0,
        syn::parse_quote! {
            {
                // Call the identifier function so that we can find function to
                // harden
                unsafe {#identifier_function_name()}
            };
        },
    );
}

fn insert_type_check(item_fn: &mut ItemFn, eq_fn: &str) {
    let eq_fn = syn::Ident::new(eq_fn, Span::call_site().into());
    let ReturnType::Type(_, return_type) = &item_fn.sig.output else {
        panic!()
    };
    let item_fn_return_type = dereference_type(&return_type);

    item_fn.block.stmts.insert(
        0,
        syn::parse_quote! {
        {
            // this function is never called, thus not a problem
            #[allow(improper_ctypes)]
            // unused function to check types
            #[allow(dead_code)]
            fn type_check() -> bool {
                extern "C" {fn type_check_fn_mock() -> #item_fn_return_type;}
                let x = unsafe { type_check_fn_mock() };
                return #eq_fn (&x, &x);
            }
        }
        },
    );
}
#[proc_macro_attribute]
pub fn harden_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut item_fn = syn::parse_macro_input!(item as syn::ItemFn);
    let user_eq_name = attr.to_string();
    let function_name_prefix = format!(
        "internal_firv2_{}_",
        IDX.fetch_add(1, atomic::Ordering::SeqCst)
    );
    item_fn.attrs.push(no_inline_attr());
    let identifier_function_name = format!("{function_name_prefix}identifier");
    let eq_fn = format!("{function_name_prefix}eq");

    insert_identifier_function(&mut item_fn, &identifier_function_name);
    insert_eq_function(&mut item_fn, &user_eq_name, &eq_fn);
    insert_type_check(&mut item_fn, &eq_fn);
    insert_identifier_call(&mut item_fn, &identifier_function_name);

    item_fn.to_token_stream().into()
}
