use crate::*;
use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_until};
use nom::character::complete::{char, multispace0, none_of, one_of};
use nom::combinator::{consumed, map, opt, value};
use nom::multi::{count, fold_many0, many0, many1};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

const DIGITS_HEX: &str = "0123456789abcdefABCDEF";
const DIGITS_OCTAL: &str = "01234567";
const DIGITS_BINARY: &str = "01";
const DIGITS_DECIMAL: &str = "0123456789";

const ILLEGAL_STRING_CHARS: &str = " \t\n\r\"\'\x0C";

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
        consumed(tuple((
            alt((parse_letter, char('_'))),
            many0(alt((parse_letter, parse_digit, char('_'), char('\'')))),
        ))),
        |(consumed, _)| Identifier {
            inner: consumed.to_string(),
        },
    )(input)
}

pub fn parse_escape_sequence_number(input: &str) -> IResult<&str, u8> {
    alt((
        map(consumed(count(parse_digit, 3)), |(n, _)| {
            u8::from_str_radix(n, 10).unwrap()
        }),
        map(
            consumed(preceded(tag("x"), count(one_of(DIGITS_HEX), 2))),
            |(n, _)| u8::from_str_radix(n, 16).unwrap(),
        ),
        map(
            consumed(preceded(
                tag("o"),
                tuple((one_of("0123"), one_of(DIGITS_HEX), one_of(DIGITS_HEX))),
            )),
            |(n, _)| u8::from_str_radix(n, 8).unwrap(),
        ),
    ))(input)
}

// TODO remove unwraps

pub fn parse_escape_sequence(input: &str) -> IResult<&str, u8> {
    preceded(
        char('\\'),
        alt((
            value(b'\\', char('\\')),
            value(b'"', char('"')),
            value(b'\'', char('\'')),
            value(b'\n', char('n')),
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
        map(
            tuple((
                |i| prefix(i),
                consumed(tuple((
                    one_of(digits),
                    many0(alt((one_of(digits), char('_')))),
                ))),
            )),
            |(_, (consumed, _))| {
                let num_without_underscores = consumed.replace("_", "");
                i64::from_str_radix(&num_without_underscores, radix).unwrap() // TODO unwrap
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
                map(none_of(ILLEGAL_STRING_CHARS), |char: char| {
                    if !char.is_ascii() {
                        todo!("char not ascii")
                    } else {
                        char as u8
                    }
                }),
            )),
            char('\''),
        ),
        Literal::Char,
    )(input)
}

pub fn parse_string_literal_quotes(input: &str) -> IResult<&str, String> {
    fn parse_unicode_sequence(input: &str) -> IResult<&str, char> {
        map(
            delimited(tag("\\u{"), consumed(many1(one_of(DIGITS_HEX))), char('}')),
            |(hex, _)| char::from_u32(u32::from_str_radix(hex, 16).unwrap()).unwrap(),
        )(input)
    }

    // TODO newline

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
        let (rest, (id, _)) = consumed(many0(one_of("abcdefghijklmnopqrstuvwxyz_")))(input)?;
        let id = String::with_capacity(id.len() + 1) + "|" + id;

        let r = map(
            delimited(char('|'), take_until(id.as_str()), tag(id.as_str())),
            |s: &str| s.to_string(),
        )(rest);

        id;

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

pub fn parse_tokens(input: &str) -> IResult<&str, Vec<Token>> {
    // TODO handle comments and test

    preceded(multispace0, many0(terminated(parse_token, multispace0)))(input)
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
    }

    #[test]
    fn test_string_literal() {
        assert_parses!(
            parse_literal(r#""AB\u{0043}\n""#),
            Literal::String("ABC\n".into()),
        );
        assert_parses!(
            parse_literal(r#"{hello|Hello{{|World|}}\n|hello}"#),
            Literal::String(r#"Hello{{|World|}}\n"#.into()),
        );
    }
}
