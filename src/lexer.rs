use crate::*;
use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_until};
use nom::character::complete::{anychar, char, multispace0, none_of, one_of};
use nom::combinator::{map, map_res, opt, recognize, value};
use nom::multi::{count, fold_many0, many0, many1};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{IResult, Offset, Slice};

const DIGITS_HEX: &str = "0123456789abcdefABCDEF";
const DIGITS_OCTAL: &str = "01234567";
const DIGITS_BINARY: &str = "01";
const DIGITS_DECIMAL: &str = "0123456789";

const ILLEGAL_CHAR_CHARS: &str = "\n\r\'\x0C";
const ILLEGAL_STRING_CHARS: &str = "\n\r\"\x0C";

/// <https://ocaml.org/manual/lex.html#letter>
pub fn parse_letter(input: &str) -> IResult<&str, char> {
    one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")(input)
}

pub fn parse_digit(input: &str) -> IResult<&str, char> {
    one_of("0123456789")(input)
}

/// <https://ocaml.org/manual/lex.html#ident>
pub fn parse_identifier(input: &str) -> IResult<&str, Identifier> {
    map(
        recognize(tuple((
            alt((parse_letter, char('_'))),
            many0(alt((parse_letter, parse_digit, char('_'), char('\'')))),
        ))),
        |consumed| Identifier {
            inner: consumed.to_string(),
        },
    )(input)
}

pub fn parse_escape_sequence_number(input: &str) -> IResult<&str, u8> {
    alt((
        map_res(recognize(count(parse_digit, 3)), |n| {
            u8::from_str_radix(n, 10)
        }),
        map_res(
            recognize(preceded(tag("x"), count(one_of(DIGITS_HEX), 2))),
            |n| u8::from_str_radix(n, 16),
        ),
        map_res(
            recognize(preceded(
                tag("o"),
                tuple((one_of("0123"), one_of(DIGITS_HEX), one_of(DIGITS_HEX))),
            )),
            |n| u8::from_str_radix(n, 8),
        ),
    ))(input)
}

pub fn parse_escape_sequence(input: &str) -> IResult<&str, u8> {
    preceded(
        char('\\'),
        alt((
            value(b'\\', char('\\')),
            value(b'"', char('"')),
            value(b'\'', char('\'')),
            value(b'\n', char('n')),
            value(b'\n', char('\n')),
            value(b'\r', char('r')),
            value(b'\t', char('t')),
            value(b'\x08', char('b')),
            value(b' ', char(' ')),
            parse_escape_sequence_number,
        )),
    )(input)
}

fn parse_integer_literal_radix<'a, R, P: FnMut(&'a str) -> IResult<&'a str, R>>(
    mut prefix: P,
    radix: u32,
    digits: &'static str,
) -> impl FnMut(&'a str) -> IResult<&'a str, i64> {
    move |input| {
        map_res(
            tuple((
                |i| prefix(i),
                recognize(tuple((
                    one_of(digits),
                    many0(alt((one_of(digits), char('_')))),
                ))),
            )),
            |(_, consumed)| {
                let num_without_underscores = consumed.replace("_", "");
                i64::from_str_radix(&num_without_underscores, radix)
            },
        )(input)
    }
}

pub fn parse_integer_literal_suffix(input: &str) -> IResult<&str, IntType> {
    alt((
        value(IntType::Int32, char('l')),
        value(IntType::Int64, char('L')),
        value(IntType::IntSize, char('n')),
        value(IntType::Int, take(0usize)),
    ))(input)
}

/// <https://ocaml.org/manual/lex.html#integer-literal>
pub fn parse_integer_literal(input: &str) -> IResult<&str, Literal> {
    map(
        tuple((
            opt(char('-')),
            alt((
                parse_integer_literal_radix(alt((tag("0x"), tag("0X"))), 16, DIGITS_HEX),
                parse_integer_literal_radix(alt((tag("0o"), tag("0O"))), 8, DIGITS_OCTAL),
                parse_integer_literal_radix(alt((tag("0b"), tag("0B"))), 2, DIGITS_BINARY),
                parse_integer_literal_radix(take(0usize), 10, DIGITS_DECIMAL),
            )),
            parse_integer_literal_suffix,
        )),
        |(negative, num, int_type)| {
            let sign = if negative.is_none() { 1 } else { -1 };

            Literal::Int(num * sign, int_type)
        },
    )(input)
}

/// <https://ocaml.org/manual/lex.html#char-literal>
pub fn parse_char_literal(input: &str) -> IResult<&str, Literal> {
    map(
        delimited(
            char('\''),
            alt((
                parse_escape_sequence,
                map_res(none_of(ILLEGAL_CHAR_CHARS), |char: char| {
                    if !char.is_ascii() {
                        Err("char is not valid ascii")
                    } else {
                        Ok(char as u8)
                    }
                }),
            )),
            char('\''),
        ),
        Literal::Char,
    )(input)
}

pub fn parse_string_literal_quotes(input: &str) -> IResult<&str, String> {
    fn parse_unicode_sequence_code(input: &str) -> IResult<&str, u32> {
        map_res(recognize(many1(one_of(DIGITS_HEX))), |hex| {
            u32::from_str_radix(hex, 16)
        })(input)
    }

    fn parse_unicode_sequence(input: &str) -> IResult<&str, char> {
        map_res(
            delimited(tag("\\u{"), parse_unicode_sequence_code, char('}')),
            |code| char::from_u32(code).ok_or("cannot convert code to char"),
        )(input)
    }

    delimited(
        char('"'),
        fold_many0(
            alt((
                map(parse_escape_sequence, |u| char::from_u32(u as _).unwrap()),
                parse_unicode_sequence,
                none_of(ILLEGAL_STRING_CHARS),
            )),
            String::new,
            |mut acc, c| {
                acc.push(c);
                acc
            },
        ),
        char('"'),
    )(input)
}

pub fn parse_string_literal_curly_braces(input: &str) -> IResult<&str, String> {
    fn parse_inner(input: &str) -> IResult<&str, String> {
        let (rest, id) = recognize(many0(one_of("abcdefghijklmnopqrstuvwxyz_")))(input)?;
        let id = String::with_capacity(id.len() + 1) + "|" + id;

        let r = map(
            delimited(char('|'), take_until(id.as_str()), tag(id.as_str())),
            |s: &str| s.to_string(),
        )(rest);

        std::mem::drop(id);

        r
    }

    delimited(char('{'), parse_inner, char('}'))(input)
}

/// <https://ocaml.org/manual/lex.html#string-literal>
pub fn parse_string_literal(input: &str) -> IResult<&str, Literal> {
    map(
        alt((
            parse_string_literal_quotes,
            parse_string_literal_curly_braces,
        )),
        Literal::String,
    )(input)
}

pub fn parse_literal(input: &str) -> IResult<&str, Literal> {
    alt((
        parse_integer_literal,
        // TODO float
        parse_char_literal,
        parse_string_literal,
    ))(input)
}

pub fn parse_token(input: &str) -> IResult<&str, Token> {
    alt((
        map(Keyword::parse, Token::Keyword),
        map(parse_identifier, Token::Identifier),
        // TODO label
        map(parse_literal, Token::Literal),
    ))(input)
}

pub fn parse_comment(base_input: &str) -> IResult<&str, String> {
    let (mut input, _) = tag("(*")(base_input)?;

    let mut depth = 1isize;

    loop {
        #[rustfmt::skip]
        let (rest, depth_offset) = alt((
            value(1, tag("(*")),
            value(-1, tag("*)")),
            value(0, anychar),
        ))(input)?;

        depth += depth_offset;

        if depth == 0 {
            let index = base_input.offset(&rest);
            let consumed = base_input.slice(..index);
            break Ok((rest, consumed.to_string()));
        } else {
            input = rest;
        }
    }
}

pub enum TokenOrComment {
    Token(Token),
    Comment(String),
}

impl TokenOrComment {
    pub fn token(self) -> Option<Token> {
        match self {
            Self::Token(token) => Some(token),
            Self::Comment(_) => None,
        }
    }
}

pub fn parse_token_or_comment(input: &str) -> IResult<&str, TokenOrComment> {
    alt((
        map(parse_comment, TokenOrComment::Comment),
        map(parse_token, TokenOrComment::Token),
    ))(input)
}

pub fn parse_tokens(input: &str) -> IResult<&str, Vec<Token>> {
    map(
        preceded(
            multispace0,
            many0(terminated(parse_token_or_comment, multispace0)),
        ),
        |token_or_comments| {
            token_or_comments
                .into_iter()
                .filter_map(TokenOrComment::token)
                .collect()
        },
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_parses {
        ($parser:ident($input:expr), $expected:expr $(,)?) => {
            assert_eq!(
                ::nom::combinator::all_consuming($parser)($input).unwrap().1,
                $expected,
            );
        };
    }

    #[test]
    fn test_keyword() {
        let parse_keyword = Keyword::parse::<&str, ()>;

        assert_parses!(parse_keyword("fun"), Keyword::Fun);
        assert_parses!(parse_keyword("!="), Keyword::NotEq);
        assert_parses!(parse_keyword("."), Keyword::Dot);
        assert_parses!(parse_keyword(".."), Keyword::DotDot);
        assert_parses!(parse_keyword("as"), Keyword::As);
        assert_parses!(parse_keyword("asr"), Keyword::Asr);
    }

    #[test]
    fn test_integer_literal() {
        assert_parses!(parse_literal("012"), Literal::Int(12, IntType::Int));
        assert_parses!(parse_literal("-0b11"), Literal::Int(-3, IntType::Int));
        assert_parses!(
            parse_literal("0x12_34L"),
            Literal::Int(0x1234, IntType::Int64),
        );
    }

    #[test]
    fn test_char_literal() {
        assert_parses!(parse_literal("'x'"), Literal::Char(b'x'));
        assert_parses!(parse_literal(r#"'\n'"#), Literal::Char(b'\n'));
        assert_parses!(parse_literal(r#"'\\'"#), Literal::Char(b'\\'));
        assert_parses!(parse_literal(r#"'\000'"#), Literal::Char(b'\0'));
        assert_parses!(parse_literal("'\t'"), Literal::Char(b'\t'));
    }

    #[test]
    fn test_string_literal() {
        assert_parses!(
            parse_literal("\"Hello world\""),
            Literal::String("Hello world".into()),
        );
        assert_parses!(
            parse_literal("\"Hello\\\nworld\""),
            Literal::String("Hello\nworld".into()),
        );
        assert_parses!(
            parse_literal(r#""AB\u{0043}\n""#),
            Literal::String("ABC\n".into()),
        );
        assert_parses!(
            parse_literal(r#"{hello|Hello{{|World|}}\n|hello}"#),
            Literal::String(r#"Hello{{|World|}}\n"#.into()),
        );
    }

    #[test]
    fn test_parse_comment() {
        const INPUT: &str = "(* This code was commented (* but there's a sub-comment *) *)";

        assert_parses!(parse_comment(INPUT), String::from(INPUT));
    }

    #[test]
    fn test_parse_tokens() {
        const INPUT: &str = r#"
(* Prints "Hello world" *)
let _ = print_endline "Hello world"
"#;

        assert_parses!(
            parse_tokens(INPUT),
            vec![
                Token::Keyword(Keyword::Let),
                Token::Keyword(Keyword::Underscore),
                Token::Keyword(Keyword::Eq),
                Token::Identifier(Identifier::new(String::from("print_endline"))),
                Token::Literal(Literal::String(String::from("Hello world")))
            ],
        );
    }
}
