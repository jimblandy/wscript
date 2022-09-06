//! Tokenizing wgpu-script.

use unicode_xid::UnicodeXID;

use super::ast::{Span, VectorSize};

use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum Token {
    End,
    Symbol(char),
    Literal(f64),

    /// The '..' operator.
    Range,

    /// Triple-quoted, indented source code.
    Code(String),

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

type TokenResult = std::result::Result<TokenOk, TokenError>;

#[derive(Debug, PartialEq)]
pub struct TokenOk {
    pub token: Token,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TokenError {
    pub kind: TokenErrorKind,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenErrorKind {
    UnrecognizedWord,
    NumberOutOfRange,

    /// There were non-space characters following the `"""` that
    /// starts a code block.
    ///
    /// `TokenError::span` refers to the text starting with the `"""`
    /// and extending to the end of the line. The given `Span` refers
    /// to the non-space character.
    JunkAfterCodeBlockStart(Span),

    /// There was a tab character in the indentation of a line that
    /// starts a code block.
    ///
    /// `TokenError::span` refers to the entire indentation run.
    TabInCodeBlock {
        /// The location of the tab itself.
        tab: Span,

        /// Which part of the code block the bad indentation occurs in.
        part: CodeBlockPart,
    },
}

/// Different parts of a code block, for use in errors.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CodeBlockPart {
    /// The introducing line, containing the `"""`.
    Introducing,

    /// The body or terminating line.
    ///
    /// (When reporting errors like `TabInCodeBlock`, it is impossible
    /// to distinguish body lines from the terminating line, since the
    /// problem is that we can't clearly determine the indentation.)
    Body,
}

#[derive(Debug)]
pub struct Input<'i> {
    /// The full text of the input.
    text: &'i str,

    /// The tail of `text` that remains to be processed.
    rest: &'i str,

    /// The id of the source from which the input is drawn.
    source_id: usize,
}

impl<'i> Input<'i> {
    pub fn new(text: &'i str, source_id: usize) -> Input {
        Input {
            text,
            rest: text,
            source_id,
        }
    }

    pub fn get_token(&mut self) -> TokenResult {
        let whole_len = self.text.len();
        let mut token_start;

        self.rest = self.rest.trim_start();
        let token = loop {
            token_start = whole_len - self.rest.len();
            if self.rest.is_empty() {
                // End of input.
                break Token::End;
            } else if let Some(comment) = self.rest.strip_prefix("//") {
                // Comment until end of line.
                if let Some((_text, end)) = comment.split_once('\n') {
                    self.rest = end;
                } else {
                    self.rest = "";
                }
            } else if let Some(rest) = self.rest.strip_prefix("..") {
                // Range operator.
                self.rest = rest;
                break Token::Range;
            } else if let Some(rest) = self.rest.strip_prefix(r#"""""#) {
                return self.get_source_block(rest);
            } else if self.rest.starts_with(|ch: char| ch.is_ascii_digit()) {
                return self.get_number();
            } else if self.rest.starts_with(|ch: char| ch.is_xid_start()) {
                return self.get_ident();
            } else {
                // Single-character symbol operator.
                let mut chars = self.rest.chars();
                let first = chars.next().unwrap();
                self.rest = chars.as_str();
                break Token::Symbol(first);
            }
        };

        let token_end = whole_len - self.rest.len();

        Ok(TokenOk {
            token,
            span: (self.source_id, token_start..token_end),
        })
    }

    /// Return the offset of the start of `rest` within `self.text`.
    ///
    /// `rest` must be a complete tail of `self.text`.
    #[inline]
    fn offset(&self, rest: &str) -> usize {
        self.text.len() - rest.len()
    }

    /// Accept `new_rest` as the new substring of `self.text` remaining to process.
    ///
    /// Return the substring that this update advances over, and a
    /// `Span` describing its position in `self.text`.
    fn advance_to(&mut self, new_rest: &'i str) -> (&str, Span) {
        let span = self.offset(self.rest)..self.offset(new_rest);
        let text = &self.text[span.clone()];
        self.rest = new_rest;
        (text, (self.source_id, span))
    }

    fn get_number(&mut self) -> TokenResult {
        let mut rest = self.rest.trim_start_matches(|ch: char| ch.is_ascii_digit());
        if let Some(fraction) = rest.strip_prefix('.') {
            rest = fraction.trim_start_matches(|ch: char| ch.is_ascii_digit());
        }

        if let Some(mut exponent) = rest.strip_prefix('e') {
            if let Some(after_sign) = exponent.strip_prefix(&['+', '-'][..]) {
                exponent = after_sign;
            }

            rest = exponent.trim_start_matches(|ch: char| ch.is_ascii_digit());
        }

        let (number_text, span) = self.advance_to(rest);

        // Parsing the floating-point value should always succeed,
        // given that we checked the syntax above. Values that are out
        // of range return infinity.
        let n = f64::from_str(number_text).unwrap();
        if n.is_infinite() {
            Err(TokenError {
                kind: TokenErrorKind::NumberOutOfRange,
                span,
            })
        } else {
            Ok(TokenOk {
                token: Token::Literal(n),
                span,
            })
        }
    }

    fn get_ident(&mut self) -> TokenResult {
        let rest = self
            .rest
            .trim_start_matches(|ch: char| ch.is_xid_continue());
        let (ident, span) = self.advance_to(rest);

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

        Ok(TokenOk { token, span })
    }

/// Parse a source block.
///
/// When this is called, `self.rest` points to the introducing `"""`,
/// and `rest` is the text following it.
///
/// A source block starts with `"""`, and includes all following
/// lines that are more indented than the line containing the
/// `"""`. Blank lines do not terminate the block. There is no
/// closing `"""`. Leading and trailing blank lines and common
/// indentation are removed. No indentation may include tabs,
/// including that of the line containing the introducing `"""`.
///
/// For example:
///
/// ```text
/// dispatch """
///
///     @group(0) @binding(0)
///     var<storage> buf: array<u32, 4096>;
///
///     @compute
///     @workgroup_size(64)
///     fn add_one(@builtin(global_invocation_id) i: i32) {
///         buf[i] += 1;
///     }
///
/// check: 1 .. 4097
/// ```
///
/// The code block here includes the eight lines:
///
/// ```text
/// @group(0) @binding(0)
/// var<storage> buf: array<u32, 4096>;
///
/// @compute
/// @workgroup_size(64)
/// fn add_one(@builtin(global_invocation_id) i: i32) {
///     buf[i] += 1;
/// }
/// ```
///
/// It does not including the leading and trailing blank lines.
/// The blank line in the midst of the code does not need to have
/// leading spaces to avoid ending the block.
    fn get_source_block(&mut self, after_quote: &'i str) -> TokenResult {
        // Make sure the rest of the line after the `"""` is just whitespace.
        let mut body = if let Some((quote_to_eol, after)) = after_quote.split_once('\n') {
            let non_space = quote_to_eol.trim_start();
            if !non_space.is_empty() {
                let full_span = self.offset(self.rest)..self.offset(after) - 1;
                let non_space_end = quote_to_eol.trim_end();
                let after_quote_offset = self.offset(after_quote);
                let non_space_span = after_quote_offset + (quote_to_eol.len() - non_space.len())
                    ..after_quote_offset + non_space_end.len();
                return Err(TokenError {
                    span: (self.source_id, full_span),
                    kind: TokenErrorKind::JunkAfterCodeBlockStart((self.source_id, non_space_span)),
                });
            }
            after
        } else {
            ""
        };

        // Find the indentation of the line that introduces the code block.
        let introducing_indent = {
            let before_quote = &self.text[..self.offset(self.rest)];
            let introducing_line = if let Some((before, _)) = before_quote.rsplit_once('\n') {
                &self.text[self.offset(before) + 1..]
            } else {
                self.text
            };
            self.indentation(introducing_line, CodeBlockPart::Introducing)?
        };

        let mut code = String::new();
        loop {
            // This catches end-of-input, too.
            if self.indentation(body, CodeBlockPart::Body)? <= introducing_indent {
                // blank lines (including lines containing only whitespace)
                // don't terminate code blocks.
                if !body.trim().is_empty() {
                    break;
                }
            }

            let line;
            if let Some((next, rest)) = body.split_once('\n') {
                line = next;
                body = rest;
            } else {
                line = body;
                body = "";
            }
                    
            let line = line.strip_suffix('\r').unwrap_or(line);

            // We know this is a character boundary, because
            // `self.indentation` checks that the indentation is
            // all single-byte characters.
            code.push_str(&line[introducing_indent..]);
        }

        let (_, span) = self.advance_to(body);

        Ok(TokenOk {
            span,
            token: Token::Code(code),
        })
    }

    /// Count the indentation of `line`, which is a tail of `self.text`.
    ///
    /// The `part` indicates which part of the code block `line`
    /// represents, for the sake of generating errors.
    fn indentation(&self, line: &'i str, part: CodeBlockPart) -> Result<usize, TokenError> {
        let trimmed = line.trim_start_matches(' ');

        // Refuse to deal with tabs altogether.
        if trimmed.starts_with('\t') {
            let after_all_indentation = line.trim_start();
            let indentation_span = self.offset(line)..self.offset(after_all_indentation);
            let tab_offset = self.offset(trimmed);
            return Err(TokenError {
                span: (self.source_id, indentation_span),
                kind: TokenErrorKind::TabInCodeBlock {
                    tab: (self.source_id, tab_offset..tab_offset + 1),
                    part,
                },
            });
        }

        Ok(line.len() - trimmed.len())
    }
}

impl TokenError {
    pub fn build_report(&self, builder: &mut crate::error::ReportBuilder) {
        use ariadne::Label;
        match self.kind {
            TokenErrorKind::UnrecognizedWord => {
                builder.set_message("word is not a recognized part of the wscript vocabulary");
                builder.add_label(Label::new(self.span.clone()).with_message("unrecognized word"));
            }
            TokenErrorKind::JunkAfterCodeBlockStart(ref junk) => {
                builder.set_message(
                    r#"non-whitespace characters following a `"""` code block introducer"#,
                );
                builder.add_label(
                    Label::new(self.span.clone()).with_message("code block is introduced here"),
                );
                builder.add_label(
                    Label::new(junk.clone()).with_message("this character isn't allowed here "),
                );
                builder.set_help(
                    r#"The `"""` that starts a code block must not have anything else following it on the line."#
                );
            }
            TokenErrorKind::NumberOutOfRange => {
                builder.add_label(
                    Label::new(self.span.clone()).with_message("number too large to represent"),
                );
                builder.set_help(
                    "wscript numbers must be representable as a 64-bit IEEE double value.",
                );
            }
            TokenErrorKind::TabInCodeBlock { ref tab, part } => {
                builder.add_label(Label::new(self.span.clone()).with_message(format!(
                    "tab character in indentation of code block's {}",
                    match part {
                        CodeBlockPart::Introducing => "introducing line",
                        CodeBlockPart::Body => "body",
                    }
                )));
                builder.add_label(
                    Label::new(tab.clone()).with_message("this tab character is not allowed"),
                );
                builder.set_help(
                    "Tabs have no well-defined width, so the lines that begin and end code\n\
                     blocks, as well as the lines that make up its content, must be indented\n\
                     with spaces only.",
                );
            }
        };
    }
}

#[cfg(test)]
mod tests {
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
                       (Token::Literal(4.0), 7..8),
                       (Token::Literal(5.0), 9..10),
                       (Token::Literal(6.0), 11..12),
                       (Token::End, 12..12),
                   ]);
    }

    fn check_number(input: &str) -> (f64, std::ops::Range<usize>) {
        let mut input = Input::new(input, 1728);
        match input.get_token() {
            Ok(TokenOk {
                token: Token::Literal(n),
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
        fn check(block: &str, quote_start: usize) -> TokenResult {
            let mut input = Input::new(block, 1);
            input.rest = &input.text[quote_start..];
            input.get_source_block(&input.text[quote_start + 3..])
        }

        assert_eq!(
            check("  boo \"\"\" junk  \n   body\n", 6),
            Err(TokenError {
                span: (1, 6..16),
                kind: TokenErrorKind::JunkAfterCodeBlockStart((1, 10..14))
            })
        );

        assert_eq!(
            check(" \t boo \"\"\" \n   body\n", 7),
            Err(TokenError {
                span: (1, 0..3),
                kind: TokenErrorKind::TabInCodeBlock {
                    tab: (1, 1..2),
                    part: CodeBlockPart::Introducing,
                }
            })
        );

        // carriage return before eol on introducing line
    }
}
