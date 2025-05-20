use std::collections::HashSet;
use std::sync::atomic;

use proc_macro::{ Span, TokenStream};
use quote::ToTokens;
use syn::visit_mut::{self, VisitMut};

static IDX: atomic::AtomicUsize = atomic::AtomicUsize::new(0);

struct HardenVariable {
    requested: HashSet<String>,
    hardened: HashSet<String>,
}
impl HardenVariable {
    fn new(requested: Vec<String>) -> Self {
        Self {
            requested: requested.into_iter().collect(),
            hardened: HashSet::new(),
        }
    }
    fn check(self) -> Result<(), HashSet<String>> {
        let not_hardened: HashSet<String> =
            self.requested.difference(&self.hardened).cloned().collect();
        if not_hardened.is_empty() {
            Ok(())
        } else {
            Err(not_hardened)
        }
    }
}

// TODO: Replace this with https://docs.rs/syn/latest/syn/meta/fn.parser.html
fn read_from_attr(attrs: TokenStream) -> Vec<String> {
    attrs
        .into_iter()
        .filter(|attr| matches!(attr, proc_macro::TokenTree::Ident(_)))
        .map(|attr| attr.to_string())
        .collect::<Vec<_>>()
}

fn harden_init(stmt_init: &mut syn::LocalInit) {
    let new_init = stmt_init.expr.to_token_stream();
    let function_name = syn::Ident::new(
        format!(
            "cfip_harden_var_{}",
            IDX.fetch_add(1, atomic::Ordering::SeqCst)
        )
        .as_str(),
        Span::call_site().into(),
    );

    *stmt_init = syn::LocalInit {
        expr: Box::<syn::Expr>::new(
            syn::parse(
                quote::quote! {
                    {
                        #[inline(never)]
                        #[unsafe(no_mangle)]
                        pub fn #function_name<T>(dummy: T) -> T {
                            use core::hint::black_box;
                            return black_box(dummy);
                        }
                        #function_name(#new_init)
                    }
                }
                .into(),
            )
            .expect("Failed to insert hidden_call block"),
        ),
        ..stmt_init.clone()
    };
}
impl VisitMut for HardenVariable {
    fn visit_local_mut(&mut self, stmt: &mut syn::Local) {
        let syn::Pat::Ident(ref pattern) = stmt.pat else {
            return;
        };
        let harden_var_name = if self.requested.contains(&pattern.ident.to_string()) {
            println!("Harden variable: {}", pattern.ident);
            self.hardened.insert(pattern.ident.to_string());
            &pattern.ident
        } else {
            return;
        };
        let Some(ref mut original_init) = stmt.init else {
            panic!(
                "Failed to harden variable: {:?}, because it was not initialized.",
                harden_var_name
            );
        };

        harden_init(original_init);

        visit_mut::visit_local_mut(self, stmt);
    }
}

#[proc_macro_attribute]
pub fn harden_vars_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut harden = HardenVariable::new(read_from_attr(attr));
    let mut item_fn = syn::parse_macro_input!(item as syn::ItemFn);
    harden.visit_item_fn_mut(&mut item_fn);
    harden.check().unwrap_or_else(|not_hardened| {
        panic!("Failed harden requested variables: {:?}.", not_hardened)
    });
    item_fn.to_token_stream().into()
}
