//! <https://ocaml.org/manual/lex.html#sss:keywords>

use inflections::Inflect;
use proc_macro2::{TokenStream, TokenTree};
use quote::{format_ident, quote, quote_spanned};

const KW_STRING: &str = r#"
      and         as          assert      asr         begin       class
      constraint  do          done        downto      else        end
      exception   external    false       for         fun         function
      functor     if          in          include     inherit     initializer
      land        lazy        let         lor         lsl         lsr
      lxor        match       method      mod         module      mutable
      new         nonrec      object      of          open        or
      private     rec         sig         struct      then        to
      true        try         type        val         virtual     when
      while       with
"#;

const KW_SYMBOLS: &str = r#"
    !=    #     &     &&    '     (     )     *     +     ,     -
    -.    ->    .     ..    .~    :     ::    :=    :>    ;     ;;
    <     <-    =     >     >]    >}    ?     [     [<    [>    [|
    ]     _     `     {     {<    |     |]    ||    }     ~
"#;

fn name_symbols(symbols: &str) -> String {
    symbols
        .chars()
        .map(|c| match c {
            '!' => "Not",
            '=' => "Eq",
            '#' => "Hash",
            '&' => "Amp",
            '\'' => "Apostrophe",
            '(' => "LParen",
            ')' => "RParen",
            '*' => "Asterisk",
            '+' => "Plus",
            ',' => "Comma",
            '-' => "Minus",
            '.' => "Dot",
            '>' => "Gt",
            '~' => "Tilde",
            ':' => "Colon",
            ';' => "SColon",
            '<' => "Lt",
            ']' => "RSquare",
            '}' => "RCurly",
            '?' => "Int",
            '[' => "LSquare",
            '|' => "Pipe",
            '_' => "Underscore",
            '`' => "Bt",
            '{' => "LCurly",
            _ => unimplemented!(),
        })
        .collect::<String>()
}

fn get_keywords() -> Vec<(String, String, bool)> {
    let mut keywords = KW_STRING
        .split_ascii_whitespace()
        .zip(std::iter::repeat(true))
        .chain(
            KW_SYMBOLS
                .split_ascii_whitespace()
                .zip(std::iter::repeat(false)),
        )
        .map(|(kw, is_alpha)| {
            let variant_name = if is_alpha {
                kw.to_string().to_title_case()
            } else {
                name_symbols(kw)
            };

            (kw.to_string(), variant_name, is_alpha)
        })
        .collect::<Vec<_>>();

    keywords.sort_by_key(|(k, _, _)| k.len());

    keywords
}

#[proc_macro]
pub fn generate_keywords(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);

    let mut input = input.into_iter();
    let name = input.next().expect("missing argument");
    let span = name.span();
    assert!(input.next().is_none(), "too many argument");

    match name {
        TokenTree::Ident(ident) => {
            let kw = get_keywords();
            let keywords = kw.iter().map(|(kw, variant, _)| {
                let kw = if kw == "`" { "`\u{200b}`\u{200b}`" } else { kw };

                let doc = format!("OCaml's `{}` keyword", kw);
                let variant = format_ident!("{}", variant);
                quote! {
                    #[doc=#doc]
                    #variant,
                }
            });

            let alphabetic =
                kw.iter()
                    .filter(|(_, _, is_alpha)| *is_alpha)
                    .map(|(_, variant, _)| {
                        let variant = format_ident!("{}", variant);
                        quote!(| Self::#variant)
                    });

            quote_spanned! {span=>
                #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
                pub enum #ident {
                    #(#keywords)*
                }

                impl #ident {
                    fn is_alphabetic(self) -> bool {
                        match self {
                            #(#alphabetic)* => true,
                            _ => false,
                        }
                    }
                }
            }
        }
        _ => panic!("expected identifier as only argument"),
    }
    .into()
}
