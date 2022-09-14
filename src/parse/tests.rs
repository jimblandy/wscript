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
        assert_matches!(parse_type(source),
                        ast::Type {
                            span: actual_span,
                            kind: ast::TypeKind::Scalar(actual_kind)
                        }
                        if actual_span == expected_span && actual_kind == expected);
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
