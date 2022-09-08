//! Tokenizing wgpu-script.

use unicode_xid::UnicodeXID;

use super::ast::{Span, VectorSize};

use std::str::FromStr;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum Token {
    End,
    Symbol(char),
    Number(f64),

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
                token: Token::Number(n),
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
                &self.text[before.len() + 1..]
            } else {
                self.text
            };
            self.indentation(introducing_line, CodeBlockPart::Introducing)?
        };

        let mut lines = vec![];
        let mut common_indentation = usize::MAX;
        while !body.is_empty() {
            let indentation = self.indentation(body, CodeBlockPart::Body)?;

            // Find the extent of the next line.
            let (line, next) = body.split_once('\n').unwrap_or((body, ""));
            let line = line.strip_suffix('\r').unwrap_or(line);

            // Don't trim the front - that's indentation, some of which we want
            // to preserve. Trimming from the end is equally effective in
            // detecting blank lines.
            let line = line.trim_end();

            // blank lines (including lines containing only whitespace) don't
            // terminate code blocks, and don't affect the common indentation.
            if !line.is_empty() {
                if indentation <= introducing_indent {
                    break;
                }
                common_indentation = std::cmp::min(common_indentation, indentation);
            }

            lines.push(line);
            body = next;
        }

        let (_, span) = self.advance_to(body);

        // Drop leading and trailing blank lines.
        {
            let leading_blanks = lines
                .iter()
                .position(|line| !line.is_empty())
                .unwrap_or(lines.len());
            lines.drain(..leading_blanks);

            let until_trailing_blanks = lines
                .iter()
                .rposition(|line| !line.is_empty())
                .map(|n| n + 1)
                .unwrap_or(0);
            lines.truncate(until_trailing_blanks);
        }

        // Now that we know the common indentation, we can build the text.
        let mut code = String::new();
        for line in lines {
            if line.len() > common_indentation {
                // We know this is a character boundary, because we required
                // all indentation to be ASCII spaces only.
                code.push_str(&line[common_indentation..]);
            }
            code.push('\n');
        }

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
    pub fn build_report(&self, builder: &mut crate::error::ReportBuilder) -> &'static str {
        use ariadne::Label;
        match self.kind {
            TokenErrorKind::UnrecognizedWord => {
                builder.set_message("word is not a recognized part of the wscript vocabulary");
                "unrecognized word"
            }
            TokenErrorKind::JunkAfterCodeBlockStart(ref junk) => {
                builder.set_message(
                    r#"non-whitespace characters following a `"""` code block introducer"#,
                );
                builder.add_label(
                    Label::new(junk.clone()).with_message("this character isn't allowed here "),
                );
                builder.set_help(
                    r#"The `"""` that starts a code block must not have anything else following it on the line."#
                );
                "code block is introduced here"
            }
            TokenErrorKind::NumberOutOfRange => {
                builder.set_help(
                    "wscript numbers must be representable as a 64-bit IEEE double value.",
                );
                "number too large to represent"
            }
            TokenErrorKind::TabInCodeBlock { ref tab, part } => {
                builder.add_label(
                    Label::new(tab.clone()).with_message("this tab character is not allowed"),
                );
                builder.set_help(
                    "Tabs have no well-defined width, so the lines that begin and end code\n\
                     blocks, as well as the lines that make up its content, must be indented\n\
                     with spaces only.",
                );

                match part {
                    CodeBlockPart::Introducing => "tab character in indentation of line introducing code block",
                    CodeBlockPart::Body => "tab character in indentation of the body of the code block",
                }                    
            }
        }
    }
}
