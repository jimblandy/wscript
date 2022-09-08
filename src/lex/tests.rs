use super::*;

fn collect_tokens(input: &str) -> Vec<(Token, std::ops::Range<usize>)> {
    let mut input = Input::new(input, 1729);
    let mut tokens = vec![];
    loop {
        let before = input.text;
        match input.get_token() {
            Ok(TokenOk { token, span }) => {
                let done = token == Token::End;
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

        assert_eq!(collect_tokens(""), vec![(Token::End, 0..0)]);
        assert_eq!(collect_tokens("+-{}()<>"), 
                   vec![
                       (Token::Symbol('+'), 0..1),
                       (Token::Symbol('-'), 1..2),
                       (Token::Symbol('{'), 2..3),
                       (Token::Symbol('}'), 3..4),
                       (Token::Symbol('('), 4..5),
                       (Token::Symbol(')'), 5..6),
                       (Token::Symbol('<'), 6..7),
                       (Token::Symbol('>'), 7..8),
                       (Token::End, 8..8)
                   ]);

        assert_eq!(collect_tokens("buffer..dispatch check  group\tbinding array\n"),
                   vec![
                        (Token::Buffer,   0..6),
                        (Token::Range,    6..8),
                        (Token::Dispatch, 8..16),
                        (Token::Check,    17..22),
                        (Token::Group,    24..29),
                        (Token::Binding,  30..37),
                        (Token::Array,    38..43),
                        (Token::End,      44..44),
                   ]);

        assert_eq!(collect_tokens("       f32 // comment\n\
                                   // comment at beginning of line\n\
                                   i32\n\
                                   u32 bool        "),
                   vec![
                       (Token::F32, 7..10),
                       (Token::I32, 54..57),
                       (Token::U32, 58..61),
                       (Token::Bool, 62..66),
                       (Token::End, 74..74),
                   ]);

        assert_eq!(collect_tokens("vec2 vec3 vec4"),
                   vec![
                       (Token::Vec(Vec2), 0..4),
                       (Token::Vec(Vec3), 5..9),
                       (Token::Vec(Vec4), 10..14),
                       (Token::End, 14..14)
                   ]);

        assert_eq!(collect_tokens("mat2x2 mat2x3 mat2x4 \
                                   mat3x2 mat3x3 mat3x4 \
                                   mat4x2 mat4x3 mat4x4"),
                   vec![
                       (Token::Mat { columns: Vec2, rows: Vec2 }, 0..6),
                       (Token::Mat { columns: Vec2, rows: Vec3 }, 7..13),
                       (Token::Mat { columns: Vec2, rows: Vec4 }, 14..20),
                       (Token::Mat { columns: Vec3, rows: Vec2 }, 21..27),
                       (Token::Mat { columns: Vec3, rows: Vec3 }, 28..34),
                       (Token::Mat { columns: Vec3, rows: Vec4 }, 35..41),
                       (Token::Mat { columns: Vec4, rows: Vec2 }, 42..48),
                       (Token::Mat { columns: Vec4, rows: Vec3 }, 49..55),
                       (Token::Mat { columns: Vec4, rows: Vec4 }, 56..62),
                       (Token::End, 62..62)
                   ]);

        assert_eq!(collect_tokens("// comment until end\n"),
                   vec![(Token::End, 21..21)]);

        assert_eq!(collect_tokens("// comment no newline"),
                   vec![(Token::End, 21..21)]);

        let mut input = Input::new("slurve", 1789);
        assert_eq!(input.get_token(),
                   Err(TokenError {
                       kind: TokenErrorKind::UnrecognizedWord, 
                       span: (1789, 0..6),
                   }));

        assert_eq!(collect_tokens("mat2x3 4 5 6"),
                   vec![
                       (Token::Mat { columns: Vec2, rows: Vec3 }, 0..6),
                       (Token::Number(4.0), 7..8),
                       (Token::Number(5.0), 9..10),
                       (Token::Number(6.0), 11..12),
                       (Token::End, 12..12),
                   ]);
    }

fn check_number(input: &str) -> (f64, std::ops::Range<usize>) {
    let mut input = Input::new(input, 1728);
    match input.get_token() {
        Ok(TokenOk {
            token: Token::Number(n),
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
        Ok(TokenOk {
            span: (1, 8..20),
            token: Token::Code("code\n".to_string()),
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
        Ok(TokenOk {
            span: (1, 12..26),
            token: Token::Code("code\n".to_string()),
        })
    );

    assert_eq!(
        check(r#"   shader """"#),
        Ok(TokenOk {
            span: (1, 10..13),
            token: Token::Code("".to_string()),
        })
    );

    assert_eq!(
        check(
            r#"   shader """
"#
        ),
        Ok(TokenOk {
            span: (1, 10..14),
            token: Token::Code("".to_string()),
        })
    );

    assert_eq!(
        check(
            r#""""

"#
        ),
        Ok(TokenOk {
            span: (1, 0..5),
            token: Token::Code("".to_string()),
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
        Ok(TokenOk {
            span: (1, 11..57),
            token: Token::Code("  code\nstuff\n\n various\n".to_string()),
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
        Ok(TokenOk {
            span: (1, 11..59),
            token: Token::Code("  code\nstuff\n\n various\n".to_string()),
        })
    );
}
