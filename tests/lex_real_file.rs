#[test]
fn lex() {
    let source = include_str!("main.ml");
    let tokens = ocaml_lexer::parse_tokens(source).unwrap();
    assert_eq!(tokens.1.len(), 541);
}
