use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Expr, parse_macro_input, parse_quote,
    visit_mut::{VisitMut, visit_expr_mut},
};

#[derive(Default)]
struct Sow {
    pub exprs: Vec<Expr>,
}

impl VisitMut for Sow {
    fn visit_expr_mut(&mut self, node: &mut syn::Expr) {
        if let Expr::Call(expr) = &node {
            if expr.args.len() == 1 {
                let arg = &expr.args[0];
                if let Expr::Path(p) = &*expr.func {
                    if p.path.is_ident("sow") {
                        let n = syn::Index::from(self.exprs.len());
                        self.exprs.push(arg.clone());
                        *node = parse_quote!({ field.#n });
                    }
                }
            }
        }

        visit_expr_mut(self, node);
    }
}

#[proc_macro]
pub fn reap(input: TokenStream) -> TokenStream {
    let mut ast: Expr = parse_macro_input!(input);

    let mut sow = Sow::default();
    sow.visit_expr_mut(&mut ast);
    let exprs = sow.exprs;

    let expanded = quote! {
        {
            let field = ( #( #exprs ),* , );
            (#ast, field)
        }
    };

    TokenStream::from(expanded)
}
