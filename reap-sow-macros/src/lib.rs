use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Expr, parse_macro_input, parse_quote,
    visit_mut::{VisitMut, visit_expr_mut},
};

struct Sow;

impl VisitMut for Sow {
    fn visit_expr_mut(&mut self, node: &mut syn::Expr) {
        if let Expr::Call(expr) = &node {
            let arg = expr.args.first().unwrap();
            if let Expr::Path(p) = &*expr.func {
                if p.path.is_ident("sow") {
                    *node = parse_quote!({ field.push(#arg); #arg });
                }
            }
        }

        visit_expr_mut(self, node);
    }
}

#[proc_macro]
pub fn reap(input: TokenStream) -> TokenStream {
    let mut ast: Expr = parse_macro_input!(input);

    Sow.visit_expr_mut(&mut ast);

    let expanded = quote! {
        {
            use std::any::Any;
            let mut field: Vec<u64> = Vec::new();
            (#ast, field)
        }
    };

    TokenStream::from(expanded)
}
