//! Tokenizing wgpu-script.

use unicode_xid::UnicodeXID;

use super::ast::{Span, VectorSize};

use std::str::FromStr;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub struct Token<'s> {
    pub kind: TokenKind<'s>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind<'s> {
    End,
    Symbol(char),
    Number(f64),
    Ident(&'s str),

    /// The '..' operator.
    Range,

    /// Triple-quoted, indented source code.
    CodeBlock {
        /// The code represented. Common indentation and leading and trailing
        /// blank lines are removed.
        text: String,

        /// A mapping from positions in `text` to input positions.
        ///
        /// See [`ast::CodeBlock`] for details.
        ///
        /// [`ast::CodeBlock`]: crate::ast::CodeBlock
        map: Vec<(usize, std::ops::Range<usize>)>,
    },

    Module,
    Init,
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

type TokenResult<'s> = std::result::Result<Token<'s>, TokenError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TokenError {
    pub kind: TokenErrorKind,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenErrorKind {
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

/// Positions in which brackets could occur.
///
/// This isn't really used in tokenizing itself, but it's useful when
/// talking about tokens.
#[derive(Clone, Copy, Debug)]
pub enum BracketPosition {
    Open,
    Close,
}

#[derive(Debug)]
pub struct Input<'s> {
    /// The full text of the input.
    text: &'s str,

    /// The tail of `text` that remains to be processed.
    rest: &'s str,

    /// The id of the source from which the input is drawn.
    source_id: usize,
}

impl<'s> Input<'s> {
    pub fn new(text: &'s str, source_id: usize) -> Input {
        Input {
            text,
            rest: text,
            source_id,
        }
    }

    pub fn get_token(&mut self) -> TokenResult<'s> {
        let whole_len = self.text.len();
        let mut token_start;

        self.rest = self.rest.trim_start();
        let token = loop {
            token_start = whole_len - self.rest.len();
            if self.rest.is_empty() {
                // End of input.
                break TokenKind::End;
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
                break TokenKind::Range;
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
                break TokenKind::Symbol(first);
            }
        };

        let token_end = whole_len - self.rest.len();

        Ok(Token {
            kind: token,
            span: (self.source_id, token_start..token_end),
        })
    }

    /// Return the offset of the start of `rest` within `self.text`.
    ///
    /// `rest` must be a complete tail of `self.text`. This restriction lets us
    /// use the slices' lengths, rather than their pointers, to compute the
    /// offset. Subtracting pointers would work, but that's undefined behavior
    /// unless the pointers being subtracted both refer to parts of the same
    /// object. Since we can't ensure statically that `rest` is a subslice of
    /// `self.text`, this would have to become an `unsafe` function if we tried
    /// to use pointer subtraction, and that'd be a pain.
    #[inline]
    fn offset(&self, rest: &str) -> usize {
        self.text.len() - rest.len()
    }

    /// Accept `new_rest` as the new substring of `self.text` remaining to process.
    ///
    /// Return the substring that this update advances over, and a
    /// `Span` describing its position in `self.text`.
    fn advance_to(&mut self, new_rest: &'s str) -> (&'s str, Span) {
        let span = self.offset(self.rest)..self.offset(new_rest);
        let text = &self.text[span.clone()];
        self.rest = new_rest;
        (text, (self.source_id, span))
    }

    fn get_number(&mut self) -> TokenResult<'s> {
        let mut rest = self.rest.trim_start_matches(|ch: char| ch.is_ascii_digit());
        if !rest.starts_with("..") {
            if let Some(fraction) = rest.strip_prefix('.') {
                rest = fraction.trim_start_matches(|ch: char| ch.is_ascii_digit());
            }
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
            Ok(Token {
                kind: TokenKind::Number(n),
                span,
            })
        }
    }

    fn get_ident(&mut self) -> TokenResult<'s> {
        let rest = self
            .rest
            .trim_start_matches(|ch: char| ch.is_xid_continue());
        let (ident, span) = self.advance_to(rest);

        use VectorSize::*;
        let token = match ident {
            "module" => TokenKind::Module,
            "init" => TokenKind::Init,
            "dispatch" => TokenKind::Dispatch,
            "check" => TokenKind::Check,
            "group" => TokenKind::Group,
            "binding" => TokenKind::Binding,
            "array" => TokenKind::Array,
            "f32" => TokenKind::F32,
            "i32" => TokenKind::I32,
            "u32" => TokenKind::U32,
            "bool" => TokenKind::Bool,
            "vec2" => TokenKind::Vec(Vec2),
            "vec3" => TokenKind::Vec(Vec3),
            "vec4" => TokenKind::Vec(Vec4),
            "mat2x2" => TokenKind::Mat {
                columns: Vec2,
                rows: Vec2,
            },
            "mat2x3" => TokenKind::Mat {
                columns: Vec2,
                rows: Vec3,
            },
            "mat2x4" => TokenKind::Mat {
                columns: Vec2,
                rows: Vec4,
            },
            "mat3x2" => TokenKind::Mat {
                columns: Vec3,
                rows: Vec2,
            },
            "mat3x3" => TokenKind::Mat {
                columns: Vec3,
                rows: Vec3,
            },
            "mat3x4" => TokenKind::Mat {
                columns: Vec3,
                rows: Vec4,
            },
            "mat4x2" => TokenKind::Mat {
                columns: Vec4,
                rows: Vec2,
            },
            "mat4x3" => TokenKind::Mat {
                columns: Vec4,
                rows: Vec3,
            },
            "mat4x4" => TokenKind::Mat {
                columns: Vec4,
                rows: Vec4,
            },
            _ => TokenKind::Ident(ident),
        };

        Ok(Token { kind: token, span })
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
    fn get_source_block(&mut self, after_quote: &'s str) -> TokenResult<'s> {
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

            // This line's starting position in the input, including all indentation.
            let pos = self.offset(body);

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

            lines.push((line, pos));
            body = next;
        }

        let (_, span) = self.advance_to(body);

        // Drop leading and trailing blank lines.
        {
            let leading_blanks = lines
                .iter()
                .position(|&(line, _pos)| !line.is_empty())
                .unwrap_or(lines.len());
            lines.drain(..leading_blanks);

            let until_trailing_blanks = lines
                .iter()
                .rposition(|&(line, _pos)| !line.is_empty())
                .map(|n| n + 1)
                .unwrap_or(0);
            lines.truncate(until_trailing_blanks);
        }

        // Now that we know the common indentation, we can build the text.
        let mut text = String::new();
        let mut map = vec![];
        for (line, line_pos) in lines {
            if line.len() > common_indentation {
                // Note the position in text at which we're about to add new text.
                let text_pos = text.len();

                // We know this is a character boundary, because we required
                // all indentation to be ASCII spaces only.
                text.push_str(&line[common_indentation..]);

                // Record which section of the input this part of `text`
                // corresponds to.
                map.push((
                    text_pos,
                    line_pos + common_indentation..line_pos + line.len(),
                ));
            }
            text.push('\n');
        }

        Ok(Token {
            span,
            kind: TokenKind::CodeBlock { text, map },
        })
    }

    /// Count the indentation of `line`, which is a tail of `self.text`.
    ///
    /// The `part` indicates which part of the code block `line`
    /// represents, for the sake of generating errors.
    fn indentation(&self, line: &'s str, part: CodeBlockPart) -> Result<usize, TokenError> {
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

impl<'s> TokenKind<'s> {
    pub fn description(&self) -> String {
        use VectorSize as Vs;
        let s = match *self {
            TokenKind::End => "end of file",
            TokenKind::Symbol(_) => "a symbol",
            TokenKind::Number(_) => "a number",
            TokenKind::Ident(ident) => return ident.to_string(),
            TokenKind::Range => "a range",
            TokenKind::CodeBlock { .. } => "a code block",
            TokenKind::Module => "module",
            TokenKind::Init => "init",
            TokenKind::Dispatch => "dispatch",
            TokenKind::Check => "check",
            TokenKind::Group => "group",
            TokenKind::Binding => "binding",
            TokenKind::Array => "array",
            TokenKind::F32 => "f32",
            TokenKind::I32 => "i32",
            TokenKind::U32 => "u32",
            TokenKind::Bool => "bool",
            TokenKind::Vec(Vs::Vec2) => "vec2",
            TokenKind::Vec(Vs::Vec3) => "vec3",
            TokenKind::Vec(Vs::Vec4) => "vec4",
            TokenKind::Mat {
                columns: Vs::Vec2,
                rows: Vs::Vec2,
            } => "mat2x2",
            TokenKind::Mat {
                columns: Vs::Vec3,
                rows: Vs::Vec2,
            } => "mat3x2",
            TokenKind::Mat {
                columns: Vs::Vec4,
                rows: Vs::Vec2,
            } => "mat4x2",
            TokenKind::Mat {
                columns: Vs::Vec2,
                rows: Vs::Vec3,
            } => "mat2x3",
            TokenKind::Mat {
                columns: Vs::Vec3,
                rows: Vs::Vec3,
            } => "mat3x3",
            TokenKind::Mat {
                columns: Vs::Vec4,
                rows: Vs::Vec3,
            } => "mat4x3",
            TokenKind::Mat {
                columns: Vs::Vec2,
                rows: Vs::Vec4,
            } => "mat2x4",
            TokenKind::Mat {
                columns: Vs::Vec3,
                rows: Vs::Vec4,
            } => "mat3x4",
            TokenKind::Mat {
                columns: Vs::Vec4,
                rows: Vs::Vec4,
            } => "mat4x4",
        };

        s.to_string()
    }
}

impl BracketPosition {
    pub fn angle_token(self) -> TokenKind<'static> {
        TokenKind::Symbol(self.angle_char())
    }

    pub fn angle_char(self) -> char {
        match self {
            BracketPosition::Open => '<',
            BracketPosition::Close => '>',
        }
    }

    pub fn angle_description(self) -> &'static str {
        match self {
            BracketPosition::Open => "opening '<'",
            BracketPosition::Close => "closing '>'",
        }
    }
}
