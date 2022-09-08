//! Parsing errors.

use crate::{error, ast::Span, lex::TokenError};

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    LexError(TokenError),
    ExpectedStatement,
    ExpectedGroupAttrParameter,
    ExpectedBindingAttrParameter,
    ExpectedBufferAttributeOrType,
    ExpectedInteger {
        unsigned: bool,
        message: &'static str,
        help: &'static str,
    },
}

impl ParseError {
    pub fn report(&self) -> error::Report {
        use ariadne::{Report, ReportKind};

        let (source_id, range) = self.span.clone();
        let mut builder = Report::build(ReportKind::Error, source_id, range.start);
        let label = match self.kind {
            ParseErrorKind::LexError(ref lex_error) => {
                lex_error.build_report(&mut builder)
            }
            ParseErrorKind::ExpectedStatement => {
                builder.set_message("Unexpected token at start of statement");
                builder.set_help("Statements begin with a command like `buffer`, `dispatch`, or `check`.");
                "expected statement here"
            }
            ParseErrorKind::ExpectedGroupAttrParameter => {
                builder.set_message("Unexpected token in `@group` attribute");
                builder.set_help("Specify a binding group with an attribute like `@group(3)`.");
                "value of `@group` attribute"
            }
            ParseErrorKind::ExpectedBindingAttrParameter => {
                builder.set_message("Unexpected token in `@binding` attribute");
                builder.set_help("Specify a binding index with an attribute like `@binding(6)`.");
                "value of `@binding` attribute"
            }
            ParseErrorKind::ExpectedInteger { unsigned: positive, message, help } => {
                builder.set_message(message);
                builder.set_help(help);
                if positive {
                    "expected a positive integer or zero here"
                } else {
                    "expected an integer here"
                }
            }
            ParseErrorKind::ExpectedBufferAttributeOrType => {
                builder.set_message("In buffer statement, expected an attribute or a colon for the type");
                builder.set_help("A buffer statement has the form `buffer ATTRIBUTES : TYPE = VALUE`.");
                "unexpected symbol"
            }
        };

        builder.add_label(
            ariadne::Label::new(self.span.clone()).with_message(label),
        );

        
        builder.finish()
    }
}

impl From<TokenError> for ParseError {
    fn from(token_error: TokenError) -> Self {
        let span = token_error.span.clone();
        ParseError {
            kind: ParseErrorKind::LexError(token_error),
            span,
        }
    }
}
