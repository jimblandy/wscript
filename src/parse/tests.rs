//! Tests for parsing.

use super::*;
use crate::ast;
use std::{fmt, ops};

// Adapted from the standard library.
macro_rules! assert_matches {
    ($left:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        match $left {
            $( $pattern )|+ $( if $guard )? => {}
            ref left_val => {
                assert_matches_failed(
                    left_val,
                    stringify!($($pattern)|+ $(if $guard)?),
                );
            }
        }
    };
}

macro_rules! sp {
    ( $start:literal .. $end:literal ) => {
        (
            1729,
            ops::Range {
                start: $start,
                end: $end,
            },
        )
    };
}

#[track_caller]
fn assert_matches_failed<T: fmt::Debug + ?Sized>(left: &T, right: &str) -> ! {
    panic!(
        r#"match assertion failed:
   left: `{:#?}`
  right: `{}`"#,
        left, right
    );
}

fn parse_type(source: &str) -> ast::Type {
    let mut context = Context::new(source, 1729).unwrap();
    let result = context.take_if_type();
    if result.is_ok() {
        assert_eq!(context.peek().kind, TokenKind::End);
    }
    result.unwrap().unwrap()
}

#[test]
fn scalar_types() {
    #[track_caller]
    fn test(source: &str, expected: ast::ScalarKind) {
        let expected_span = (1729, 1..source.len() - 1);
        assert_matches!(
            parse_type(source),
                        ast::Type {
                            span: actual_span,
                            kind: ast::TypeKind::Scalar(actual_kind)
                        }
                        if actual_span == expected_span && actual_kind == expected
        );
    }

    test(" f32 ", ast::ScalarKind::F32);
    test(" i32 ", ast::ScalarKind::I32);
    test(" u32 ", ast::ScalarKind::U32);
    test(" bool ", ast::ScalarKind::Bool);
}

#[test]
fn vector_types() {
    use ast::ScalarKind as Sk;
    use ast::VectorSize as Vs;

    #[track_caller]
    fn test(
        source: &str,
        expected_size: Vs,
        expected_kind: Sk,
        expected_component_span: ops::Range<usize>,
    ) {
        let expected_span = (1729, 1..source.len() - 1);
        assert_matches!(parse_type(source),
                        ast::Type {
                            span: actual_span,
                            kind: ast::TypeKind::Vector {
                                size: actual_size,
                                component: actual_kind,
                                component_span: actual_component_span,
                            }
                        }
                        if actual_span == expected_span
                        && actual_size == expected_size
                        && actual_kind == expected_kind
                        && actual_component_span == (1729, expected_component_span));
    }

    test(" vec2<f32> ", Vs::Vec2, Sk::F32, 6..9);
    test(" vec3 < i32 > ", Vs::Vec3, Sk::I32, 8..11);
    test(" vec4  <  u32 > ", Vs::Vec4, Sk::U32, 10..13);
    test(" vec2   <   bool   > ", Vs::Vec2, Sk::Bool, 12..16);
}

fn parse_expr(source: &str) -> Result<ast::Expression, error::ParseError> {
    let mut context = Context::new(source, 1729).unwrap();
    let result = context.parse_expr();
    if result.is_ok() {
        assert_eq!(context.peek().kind, TokenKind::End);
    }
    result
}

#[test]
fn expr_primary() {
    use ast::ExpressionKind as Ek;
    use error::ParseErrorKind as Pk;

    assert_matches!(parse_expr("  10.125   "),
                    Ok(ast::Expression {
                        kind: Ek::Literal(lit),
                        span: (1729, span),
                    })
                    if span == (2..8) && lit == 10.125);

    assert_matches!(parse_expr(" buffer   "),
                    Err(error::ParseError {
                        span: (1729, span),
                        kind: Pk::UnexpectedToken { .. }
                    })
                    if span == (1..7));

    assert_matches!(parse_expr("  (1000000000)   "),
                    Ok(ast::Expression {
                        kind: Ek::Literal(lit),
                        span: (1729, span),
                    })
                    if span == (3..13) && lit == 1000000000.0);

    assert_matches!(parse_expr("  (1000000000   "),
                    Err(error::ParseError {
                        kind: Pk::MissingCloseParen { opening },
                        span: (1729, span),
                    })
                    if span == (16..16) && opening == (1729, 2..3));
}

#[test]
fn expr_binary() {
    use ast::ExpressionKind as Ek;
    use error::ParseErrorKind as Pk;

    #[track_caller]
    fn is_literal(expr: &ast::Expression, span: ops::Range<usize>, n: f64) -> bool {
        matches!(*expr,
                 ast::Expression {
                     span: (1729, ref actual_span),
                     kind: Ek::Literal(actual_n)
                 }
                 if actual_span == &span && actual_n == n)
    }

    assert_matches!(
        parse_expr("1 + 2 * 3"),
        Ok(ast::Expression {
            span: (1729, whole),
            kind: Ek::Binary { left, op: ast::BinaryOp::Add, op_span, ref right },
        })
            if (whole == (0..9)
                && op_span == (1729, 2..3)
                && is_literal(&left, 0..1, 1.0)
                && matches!(**right,
                            ast::Expression {
                                ref span,
                                kind: Ek::Binary {
                                    ref left,
                                    op: ast::BinaryOp::Multiply,
                                    ref op_span,
                                    ref right
                                }
                            }
                            if (*span == (1729, 4..9)
                                && *op_span == (1729, 6..7)
                                && is_literal(left, 4..5, 2.0)
                                && is_literal(right, 8..9, 3.0))))
    );

    assert_matches!(parse_expr("1 * 2 + 3"),
                    Ok(ast::Expression {
                        span: (1729, whole),
                        kind: Ek::Binary { left: _, op: ast::BinaryOp::Add, op_span, right: _ },
                    })
                    if whole == (0..9) && op_span == (1729, 6..7));

    assert_matches!(
        parse_expr("1 * 2 + dispatch"),
        Err(error::ParseError {
            span: sp!(8..16),
            kind: Pk::UnexpectedToken { .. }
        })
    );
}

fn parse_statement(source: &str) -> Result<ast::Statement, error::ParseError> {
    let mut context = Context::new(source, 1729).unwrap();
    let result = context.parse_statement();
    if result.is_ok() {
        assert_eq!(context.peek().kind, TokenKind::End);
    }
    result
}

#[test]
fn parse_init() {
    assert_matches!(
        parse_statement("init foo = 1000"),
        Ok(ast::Statement {
            span: sp!(0..15),
            kind: ast::StatementKind::Init {
                buffer: ast::BufferId { span: sp!(5..8), kind: ast::BufferIdKind::Name(id) },
                value: ast::Expression { span: sp!(11..15), kind: ast::ExpressionKind::Literal(lit) },
            }
        })
            if id == "foo" && lit == 1000.0
    );
}

#[test]
fn parse_module() {
    assert_matches!(
        parse_statement(r#"  module """

    var<uniform> buf: array<i32>;

    fn f() -> i32 { return buf[0]; }

"#),
        Ok(ast::Statement {
            span: sp!(2..87),
            kind: ast::StatementKind::Module {
                ref wgsl
            }
        })
            if wgsl.span == sp!(9..87) && wgsl.text == "var<uniform> buf: array<i32>;\n\nfn f() -> i32 { return buf[0]; }\n"
    );
}

#[test]
fn parse_workgroup_count() {
    fn parse(source: &str) -> Result<ast::WorkgroupCount, error::ParseError> {
        let mut context = Context::new(source, 1729).unwrap();
        let command_span = match context.next() {
            Ok(Token {
                span,
                kind: TokenKind::Ident(_),
            }) => span,
            _ => panic!("input must start with an identifier"),
        };
        let result = context.parse_workgroup_count(&command_span);
        if result.is_ok() {
            assert_eq!(context.peek().kind, TokenKind::End);
        }
        result
    }

    assert_matches!(
        parse(" bleah  (20)"),
        Ok(ast::WorkgroupCount {
            size: (20, 1, 1),
            span: sp!(8..12)
        })
    );

    assert_matches!(
        parse(" bleah  (20,30)"),
        Ok(ast::WorkgroupCount {
            size: (20, 30, 1),
            span: sp!(8..15)
        })
    );

    assert_matches!(
        parse(" bleah  (20,30,40)"),
        Ok(ast::WorkgroupCount {
            size: (20, 30, 40),
            span: sp!(8..18)
        })
    );
}

#[test]
fn parse_dispatch() {
    assert_matches!(
        parse_statement("   dispatch bleah (2,3)   "),
        Ok(ast::Statement {
            span: sp!(3..23),
            kind: ast::StatementKind::Dispatch {
                entry_point: ast::EntryPoint { name, span: sp!(12..17) },
                count: ast::WorkgroupCount { .. }
            }
        })
        if name == "bleah"
    );
}
