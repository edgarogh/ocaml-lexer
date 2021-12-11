//! <https://ocaml.org/manual/lex.html>

use keyword_generator::generate_keywords;

pub mod lexer;

/// <https://ocaml.org/manual/lex.html#sss:lex:identifiers>
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Identifier {
    inner: String,
}

impl Identifier {
    pub fn new(inner: String) -> Self {
        Self { inner }
    }

    /// <https://ocaml.org/manual/lex.html#capitalized-ident>
    pub fn is_capitalized(&self) -> bool {
        self.inner.starts_with(|c: char| c.is_ascii_uppercase())
    }

    /// <https://ocaml.org/manual/lex.html#lowercase-ident>
    pub fn is_lowercase(&self) -> bool {
        !self.is_capitalized()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IntType {
    Int,
    Int32,
    Int64,
    IntSize,
}

impl Default for IntType {
    fn default() -> Self {
        Self::Int
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    /// <https://ocaml.org/manual/lex.html#sss:integer-literals>
    Int(i64, IntType),

    /// <https://ocaml.org/manual/lex.html#sss:floating-point-literals>
    Float(f64),

    /// <https://ocaml.org/manual/lex.html#sss:character-literals>
    Char(u8),

    /// <https://ocaml.org/manual/lex.html#sss:stringliterals>
    String(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Label {
    /// Must be lowercase
    name: Identifier,

    is_optional: bool,
}

#[generate_keywords]
pub enum Keyword {}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(Identifier),
    Literal(Literal),
    Label(Label),
    Keyword(Keyword),
}

impl Token {
    fn requires_whitespace_around(&self) -> bool {
        match self {
            Self::Identifier(_) => true,
            Self::Literal(lit) => !matches!(lit, Literal::String(_)),
            Self::Label(_) => false,
            Self::Keyword(_) => false,
        }
    }
}
