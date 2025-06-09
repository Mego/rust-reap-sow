use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use quote::{ToTokens, format_ident, quote};
use syn::{
    Expr, Lit, Token,
    parse::Parse,
    parse_macro_input, parse_quote,
    visit_mut::{VisitMut, visit_expr_mut},
};

#[derive(Default)]
struct Sow {
    pub exprs: HashMap<Option<String>, Vec<Expr>>,
    pub tags: HashSet<String>,
}

impl VisitMut for Sow {
    fn visit_expr_mut(&mut self, node: &mut syn::Expr) {
        if let Expr::Call(expr) = &node {
            let mut tag = None;
            if expr.args.len() >= 1 && expr.args.len() <= 2 {
                if expr.args.len() == 2 {
                    match &expr.args[1] {
                        Expr::Lit(tag_expr) => {
                            let tag_value = if let Lit::Str(tag_expr) = &tag_expr.lit {
                                tag_expr.value()
                            } else {
                                tag_expr.lit.to_token_stream().to_string()
                            };
                            tag = Some(tag_value.clone());
                            self.tags.insert(tag_value);
                        }
                        Expr::Path(p) => {
                            if !p.path.is_ident("None") {
                                panic!("sow tag must be literal, None, or _");
                            }
                        }
                        Expr::Infer(_) => {}
                        _ => {
                            panic!("sow tag must be literal, None, or _");
                        }
                    }
                }
                let arg = &expr.args[0];
                if let Expr::Path(p) = &*expr.func {
                    if p.path.is_ident("sow") {
                        let tag_exprs = self.exprs.entry(tag.clone()).or_default();
                        let n = syn::Index::from(tag_exprs.len());
                        tag_exprs.push(arg.clone());
                        let ident = format_ident!(
                            "field{}",
                            tag.map(|s| format!("_{s}")).unwrap_or("".to_owned())
                        );
                        *node = parse_quote!({ #ident.#n });
                    }
                }
            }
        }

        visit_expr_mut(self, node);
    }
}

struct MacroInput {
    expr: Expr,
    comma: Option<Token![,]>,
    pat: Option<Expr>,
}

impl Parse for MacroInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut parsed = Self {
            expr: input.parse()?,
            comma: None,
            pat: None,
        };
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![,]) {
            parsed.comma = Some(input.parse()?);
            parsed.pat = Some(input.parse()?);
        }
        Ok(parsed)
    }
}

fn parse_tag_pattern(pat: &Expr) -> Option<String> {
    match pat {
        Expr::Lit(tag_expr) => {
            let tag_value = if let Lit::Str(tag_expr) = &tag_expr.lit {
                tag_expr.value()
            } else {
                tag_expr.lit.to_token_stream().to_string()
            };
            Some(tag_value)
        }
        Expr::Path(p) => {
            if !p.path.is_ident("None") {
                panic!("sow tag must be literal, None, or _");
            }
            None
        }
        Expr::Infer(_) => None,
        _ => {
            panic!("sow tag must be literal, None, or _");
        }
    }
}

#[proc_macro]
pub fn reap(input: TokenStream) -> TokenStream {
    let mut ast: MacroInput = parse_macro_input!(input);

    let filter_patterns: Option<Vec<_>> = if let Some(pat) = ast.pat {
        match &pat {
            Expr::Array(arr) => arr.elems.iter().map(parse_tag_pattern).collect(),
            _ => vec![parse_tag_pattern(&pat)],
        }
    } else {
        vec![None]
    }
    .into_iter()
    .collect();

    let mut sow = Sow::default();
    sow.visit_expr_mut(&mut ast.expr);
    let default_tag_exprs = sow.exprs.remove(&None).unwrap_or_default();
    let field = if default_tag_exprs.len() > 0 {
        quote! { #( #default_tag_exprs ),* , }
    } else {
        quote! {}
    };
    let tagged_fields = sow
        .exprs
        .into_iter()
        .filter_map(|(k, v)| k.map(|s| (s, v)))
        .map(|(tag, exprs)| {
            let field_ident = format_ident!("field_{tag}");
            quote! { let #field_ident = ( #( #exprs ),* , ); }
        })
        .collect::<Vec<_>>();
    let mut fields: Vec<_> = sow
        .tags
        .into_iter()
        .filter(|tag| {
            filter_patterns
                .as_ref()
                .is_none_or(|pats| pats.contains(tag))
        })
        .map(|tag| {
            let field_ident = format_ident!("field_{tag}");
            quote! { (#tag, #field_ident) }
        })
        .collect();
    if filter_patterns.is_none() {
        fields = [quote! { field }]
            .into_iter()
            .chain(fields.into_iter())
            .collect();
    }
    let expr = ast.expr;

    let expanded = quote! {
        {
            let field = ( #field );
            #( #tagged_fields )*
            (#expr, #( #fields ),* )
        }
    };

    TokenStream::from(expanded)
}
