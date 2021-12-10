//! <https://ocaml.org/manual/lex.html#sss:keywords>

use inflections::Inflect;
use proc_macro2::{Ident, TokenStream, TokenTree};
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

fn get_keywords() -> Vec<(String, Ident, bool)> {
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

            (kw.to_string(), format_ident!("{}", variant_name), is_alpha)
        })
        .collect::<Vec<_>>();

    keywords.sort_by_key(|(k, _, _)| k.len());

    keywords
}

#[proc_macro_attribute]
pub fn generate_keywords(
    params: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    assert_eq!(params.into_iter().count(), 0, "no parameters expected");

    let input = TokenStream::from(input);
    let mut input = input.into_iter().peekable();

    if matches!(input.peek(), Some(&TokenTree::Ident(ref ident)) if ident.to_string() == "pub") {
        input.next();
    }

    assert!(
        matches!(input.next(), Some(TokenTree::Ident(ident)) if ident == format_ident!("enum")),
        "expected enum",
    );

    let name = input.next().expect("missing argument");
    let span = name.span();

    assert_eq!(input.count(), 1, "too many arguments");

    match name {
        TokenTree::Ident(ident) => {
            let kw = get_keywords();
            let keywords = kw.iter().map(|(kw, variant, _)| {
                let kw = if kw == "`" { "`\u{200b}`\u{200b}`" } else { kw };

                let doc = format!("OCaml's `{}` keyword", kw);
                quote! {
                    #[doc=#doc]
                    #variant,
                }
            });

            let to_str = kw.iter().map(|(kw, variant, _)| {
                quote! {
                    Self::#variant => #kw,
                }
            });

            let alts = kw
                .iter()
                .rev()
                .map(|(_, variant, _)| {
                    quote! {
                        Self::#variant.parser(),
                    }
                })
                .collect::<Vec<_>>();

            let alts = alts.chunks(21).map(|chunk| {
                quote! {
                    ::nom::branch::alt((
                        #(#chunk)*
                    )),
                }
            });

            let alphabetic = kw
                .iter()
                .filter(|(_, _, is_alpha)| *is_alpha)
                .map(|(_, variant, _)| quote!(| Self::#variant));

            let ident = quote_spanned! {span=> #ident };

            quote! {
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

                    pub fn to_str(self) -> &'static str {
                        match self {
                            #(#to_str)*
                        }
                    }

                    /// Creates a [nom][::nom] parser for a specific keyword
                    pub fn parser<Input, Error: ::nom::error::ParseError<Input>>(
                        self,
                    ) -> impl Fn(Input) -> ::nom::IResult<Input, Self, Error>
                        where Input: ::nom::InputTake + ::nom::Compare<&'static str> + Clone,
                    {
                        move |input: Input| {
                            ::nom::combinator::value(
                                self,
                                ::nom::bytes::complete::tag(self.to_str()),
                            )(input)
                        }
                    }

                    /// Parses any keyword using [nom][::nom]
                    pub fn parse<Input, Error: ::nom::error::ParseError<Input>>(
                        input: Input,
                    ) -> ::nom::IResult<Input, Self, Error>
                        where Input: ::nom::InputTake + ::nom::Compare<&'static str> + Clone,
                    {
                        ::nom::branch::alt((
                            #(#alts)*
                        ))(input)
                    }
                }
            }
        }
        _ => panic!("expected identifier as only argument"),
    }
    .into()
}
