//! Tokenizing wgpu-script.

use unicode_xid::UnicodeXID;

use super::ast::{Span, VectorSize};

#[derive(Debug, PartialEq)]
pub enum Token {
    End,
    Symbol(char),
    Literal(f64),

    /// The '..' operator.
    Range,

    /// Triple-quoted, indented source code.
    Source(String),

    Buffer,
    Dispatch,
    Check,

    Group,
    Binding,

    Array,
    F32,
    I32,
    U32,
    Bool,
    Vec(VectorSize),
    Mat {
        columns: VectorSize,
        rows: VectorSize,
    },
}

#[derive(Debug, PartialEq)]
pub struct TokenOk<'a> {
    token: Token,
    span: Span,
    rest: &'a str,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TokenError {
    kind: TokenErrorKind,
    span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenErrorKind {
    UnrecognizedWord,
}

pub fn get_token(input: &str, whole_length: usize) -> Result<TokenOk, TokenError> {
    let mut rest = input.trim_start();
    let mut token_start;

    let (token, rest) = loop {
        token_start = whole_length - rest.len();
        if rest.is_empty() {
            break (Token::End, rest);
        } else if let Some(comment) = rest.strip_prefix("//") {
            if let Some((_text, end)) = comment.split_once('\n') {
                rest = end;
            } else {
                rest = "";
            }
        } else if let Some(rest) = rest.strip_prefix("..") {
            break (Token::Range, rest);
        } else if rest.starts_with(|ch: char| ch.is_ascii_digit()) {
            break get_number(rest)?;
        } else if rest.starts_with(|ch: char| ch.is_xid_start()) {
            return get_ident(rest, token_start);
        } else {
            let mut chars = rest.chars();
            let first = chars.next().unwrap();
            break (Token::Symbol(first), chars.as_str());
        }
    };

    let token_end = whole_length - rest.len();

    Ok(TokenOk {
        token,
        span: token_start..token_end,
        rest,
    })
}

fn get_number(input: &str) -> Result<(Token, &str), TokenError> {
    todo!()
}

fn get_ident(input: &str, start: usize) -> Result<TokenOk, TokenError> {
    let rest = input.trim_start_matches(|ch: char| ch.is_xid_continue());
    let len = input.len() - rest.len();
    let ident = &input[..len];
    let span = start..start + len;

    use VectorSize::*;
    let token = match ident {
        "buffer" => Token::Buffer,
        "dispatch" => Token::Dispatch,
        "check" => Token::Check,
        "group" => Token::Group,
        "binding" => Token::Binding,
        "array" => Token::Array,
        "f32" => Token::F32,
        "i32" => Token::I32,
        "u32" => Token::U32,
        "bool" => Token::Bool,
        "vec2" => Token::Vec(Vec2),
        "vec3" => Token::Vec(Vec3),
        "vec4" => Token::Vec(Vec4),
        "mat2x2" => Token::Mat {
            columns: Vec2,
            rows: Vec2,
        },
        "mat2x3" => Token::Mat {
            columns: Vec2,
            rows: Vec3,
        },
        "mat2x4" => Token::Mat {
            columns: Vec2,
            rows: Vec4,
        },
        "mat3x2" => Token::Mat {
            columns: Vec3,
            rows: Vec2,
        },
        "mat3x3" => Token::Mat {
            columns: Vec3,
            rows: Vec3,
        },
        "mat3x4" => Token::Mat {
            columns: Vec3,
            rows: Vec4,
        },
        "mat4x2" => Token::Mat {
            columns: Vec4,
            rows: Vec2,
        },
        "mat4x3" => Token::Mat {
            columns: Vec4,
            rows: Vec3,
        },
        "mat4x4" => Token::Mat {
            columns: Vec4,
            rows: Vec4,
        },
        _ => {
            return Err(TokenError {
                kind: TokenErrorKind::UnrecognizedWord,
                span,
            })
        }
    };

    Ok(TokenOk { token, span, rest })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_tokens(mut input: &str) -> Vec<(Token, Span)> {
        let whole_length = input.len();
        let mut tokens = vec![];
        loop {
            let before = input;
            match get_token(input, whole_length) {
                Ok(TokenOk { token, span, rest }) => {
                    let done = token == Token::End;
                    tokens.push((token, span));
                    if done {
                        return tokens;
                    }
                    input = rest;
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

        assert_eq!(get_token("slurve", 6),
                   Err(TokenError {
                       kind: TokenErrorKind::UnrecognizedWord, 
                       span: 0..6,
                   }));
    }
}
