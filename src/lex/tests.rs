use super::*;

fn collect_tokens(input: &str) -> Vec<(TokenKind, std::ops::Range<usize>)> {
    let mut input = Input::new(input, 1729);
    let mut tokens = vec![];
    loop {
        let before = input.text;
        match input.get_token() {
            Ok(Token { kind: token, span }) => {
                let done = token == TokenKind::End;
                assert_eq!(span.0, 1729);
                tokens.push((token, span.1));
                if done {
                    return tokens;
                }
            }
            Err(_) => {
                panic!("get_token found unexpected character at: {:?}", before);
            }
        }
    }
}

#[test]
    #[rustfmt::skip]
    fn basic() {
        use VectorSize::*;

        assert_eq!(collect_tokens(""), vec![(TokenKind::End, 0..0)]);
        assert_eq!(collect_tokens("+-{}()<>"), 
                   vec![
                       (TokenKind::Symbol('+'), 0..1),
                       (TokenKind::Symbol('-'), 1..2),
                       (TokenKind::Symbol('{'), 2..3),
                       (TokenKind::Symbol('}'), 3..4),
                       (TokenKind::Symbol('('), 4..5),
                       (TokenKind::Symbol(')'), 5..6),
                       (TokenKind::Symbol('<'), 6..7),
                       (TokenKind::Symbol('>'), 7..8),
                       (TokenKind::End, 8..8)
                   ]);

        assert_eq!(collect_tokens("module..dispatch check  group\tbinding array\n init"),
                   vec![
                        (TokenKind::Module,   0..6),
                        (TokenKind::Range,    6..8),
                        (TokenKind::Dispatch, 8..16),
                        (TokenKind::Check,    17..22),
                        (TokenKind::Group,    24..29),
                        (TokenKind::Binding,  30..37),
                        (TokenKind::Array,    38..43),
                        (TokenKind::Init,     45..49),
                        (TokenKind::End,      49..49),
                   ]);

        assert_eq!(collect_tokens("       f32 // comment\n\
                                   // comment at beginning of line\n\
                                   i32\n\
                                   u32 bool        "),
                   vec![
                       (TokenKind::F32, 7..10),
                       (TokenKind::I32, 54..57),
                       (TokenKind::U32, 58..61),
                       (TokenKind::Bool, 62..66),
                       (TokenKind::End, 74..74),
                   ]);

        assert_eq!(collect_tokens("vec2 vec3 vec4"),
                   vec![
                       (TokenKind::Vec(Vec2), 0..4),
                       (TokenKind::Vec(Vec3), 5..9),
                       (TokenKind::Vec(Vec4), 10..14),
                       (TokenKind::End, 14..14)
                   ]);

        assert_eq!(collect_tokens("mat2x2 mat2x3 mat2x4 \
                                   mat3x2 mat3x3 mat3x4 \
                                   mat4x2 mat4x3 mat4x4"),
                   vec![
                       (TokenKind::Mat { columns: Vec2, rows: Vec2 }, 0..6),
                       (TokenKind::Mat { columns: Vec2, rows: Vec3 }, 7..13),
                       (TokenKind::Mat { columns: Vec2, rows: Vec4 }, 14..20),
                       (TokenKind::Mat { columns: Vec3, rows: Vec2 }, 21..27),
                       (TokenKind::Mat { columns: Vec3, rows: Vec3 }, 28..34),
                       (TokenKind::Mat { columns: Vec3, rows: Vec4 }, 35..41),
                       (TokenKind::Mat { columns: Vec4, rows: Vec2 }, 42..48),
                       (TokenKind::Mat { columns: Vec4, rows: Vec3 }, 49..55),
                       (TokenKind::Mat { columns: Vec4, rows: Vec4 }, 56..62),
                       (TokenKind::End, 62..62)
                   ]);

        assert_eq!(collect_tokens("// comment until end\n"),
                   vec![(TokenKind::End, 21..21)]);

        assert_eq!(collect_tokens("// comment no newline"),
                   vec![(TokenKind::End, 21..21)]);

        assert_eq!(collect_tokens("mat2x3 4 5 6 groo:s"),
                   vec![
                       (TokenKind::Mat { columns: Vec2, rows: Vec3 }, 0..6),
                       (TokenKind::Number(4.0), 7..8),
                       (TokenKind::Number(5.0), 9..10),
                       (TokenKind::Number(6.0), 11..12),
                       (TokenKind::Ident("groo"), 13..17),
                       (TokenKind::Symbol(':'), 17..18),
                       (TokenKind::Ident("s"), 18..19),
                       (TokenKind::End, 19..19),
                   ]);
    }

fn check_number(input: &str) -> (f64, std::ops::Range<usize>) {
    let mut input = Input::new(input, 1728);
    match input.get_token() {
        Ok(Token {
            kind: TokenKind::Number(n),
            span,
            ..
        }) => (n, span.1),
        other => panic!("Unexpected result in check_number test: {:?}", other),
    }
}

#[test]
fn numbers() {
    assert_eq!(check_number("  0  "), (0.0, 2..3));
    assert_eq!(check_number("  9  "), (9.0, 2..3));
    assert_eq!(check_number("  10  "), (10.0, 2..4));
    assert_eq!(check_number("  99  "), (99.0, 2..4));
    assert_eq!(check_number("  4294967295  "), (4294967295.0, 2..12));
    assert_eq!(check_number("   10.125  "), (10.125, 3..9));
    assert_eq!(check_number("   1e4  "), (10000.0, 3..6));
    assert_eq!(check_number("   1.2e4  "), (12000.0, 3..8));
    let mut input = Input::new("   1e1000  ", 13);
    assert_eq!(
        input.get_token(),
        Err(TokenError {
            kind: TokenErrorKind::NumberOutOfRange,
            span: (13, 3..9)
        })
    );
}

#[test]
fn indentation() {
    use CodeBlockPart::*;

    let input = Input::new("  two spaces\n    four spaces indentation\nnone", 1729);
    assert_eq!(input.indentation(&input.text[0..], Introducing), Ok(2));
    assert_eq!(input.indentation(&input.text[2..], Introducing), Ok(0));
    assert_eq!(input.indentation(&input.text[13..], Introducing), Ok(4));
    assert_eq!(input.indentation(&input.text[41..], Introducing), Ok(0));

    let input = Input::new("none", 1000);
    assert_eq!(input.indentation(&input.text[0..], Introducing), Ok(0));

    let input = Input::new("  \t  tab\n", 729);
    assert_eq!(
        input.indentation(&input.text[0..], Body),
        Err(TokenError {
            span: (729, 0..5),
            kind: TokenErrorKind::TabInCodeBlock {
                tab: (729, 2..3),
                part: CodeBlockPart::Body,
            }
        })
    );
}

fn code(text: &str, map: impl IntoIterator<Item = (usize, std::ops::Range<usize>)>) -> TokenKind {
    TokenKind::CodeBlock(CodeBlock {
        text: text.to_string(),
        map: map.into_iter().collect(),
    })
}

#[test]
fn code_block() {
    fn check(block: &str) -> TokenResult {
        let quote_start = block.find(r#"""""#).unwrap();
        let mut input = Input::new(block, 1);
        input.rest = &input.text[quote_start..];
        input.get_source_block(&input.text[quote_start + 3..])
    }

    assert_eq!(
        check(
            r#"  boo """ junk  
   body
"#
        ),
        Err(TokenError {
            span: (1, 6..16),
            kind: TokenErrorKind::JunkAfterCodeBlockStart((1, 10..14))
        })
    );

    assert_eq!(
        check(" \t boo \"\"\" \n   body\n"),
        Err(TokenError {
            span: (1, 0..3),
            kind: TokenErrorKind::TabInCodeBlock {
                tab: (1, 1..2),
                part: CodeBlockPart::Introducing,
            }
        })
    );

    assert_eq!(
        check(
            r#"
shader """
   code
termination
"#
        ),
        Ok(Token {
            span: (1, 8..20),
            kind: code("code\n", [(0, 15..19)]),
        })
    );

    assert_eq!(
        check(
            r#"
    shader """
     code
    termination
"#
        ),
        Ok(Token {
            span: (1, 12..26),
            kind: code("code\n", [(0, 21..25)]),
        })
    );

    assert_eq!(
        check(r#"   shader """"#),
        Ok(Token {
            span: (1, 10..13),
            kind: code("", []),
        })
    );

    assert_eq!(
        check(
            r#"   shader """
"#
        ),
        Ok(Token {
            span: (1, 10..14),
            kind: code("", []),
        })
    );

    assert_eq!(
        check(
            r#""""

"#
        ),
        Ok(Token {
            span: (1, 0..5),
            kind: code("", []),
        })
    );

    assert_eq!(
        check(
            r#"
   """
   terminator

"#
        ),
        Ok(Token {
            span: (1, 4..8),
            kind: code("", []),
        })
    );

    assert_eq!(
        check(
            "
    bleah \"\"\"\r
        code
      stuff

       various
    end
"
        ),
        Ok(Token {
            span: (1, 11..57),
            kind: code(
                "  code\nstuff\n\n various\n",
                [(0, 22..28), (7, 35..40), (14, 48..56)]
            ),
        })
    );

    assert_eq!(
        check(
            r#"
    bleah """

        code
      stuff

       various


    end
"#
        ),
        Ok(Token {
            span: (1, 11..59),
            kind: code(
                "  code\nstuff\n\n various\n",
                [(0, 22..28), (7, 35..40), (14, 48..56)]
            )
        })
    );
}
