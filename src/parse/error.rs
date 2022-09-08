//! Parsing errors.

use crate::{ast::Span, error, lex::TokenError};

use std::borrow::Cow;

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ParseErrorKind {
    DuplicateAttribute {
        attr: Attribute,
        prior: Span,
    },
    ExpectedStatement,
    ExpectedAttrParameter(Attribute),
    ExpectedBufferAttributeOrType,
    ExpectedInteger {
        unsigned: bool,
        message: Cow<'static, str>,
        help: Cow<'static, str>,
    },
    LexError(TokenError),
}

#[derive(Clone, Copy, Debug)]
pub enum Attribute {
    Buffer(BufferAttribute),
}

impl Attribute {
    pub fn owner(&self) -> &'static str {
        match *self {
            Attribute::Buffer(_) => "buffer",
        }
    }

    pub fn token(&self) -> &'static str {
        match *self {
            Attribute::Buffer(attr) => attr.token(),
        }
    }

    pub fn description(&self) -> &'static str {
        match *self {
            Attribute::Buffer(attr) => attr.description(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BufferAttribute {
    Group,
    Binding,
}

impl BufferAttribute {
    fn token(&self) -> &'static str {
        match *self {
            BufferAttribute::Group => "group",
            BufferAttribute::Binding => "binding",
        }
    }

    fn description(&self) -> &'static str {
        match *self {
            BufferAttribute::Group => "binding group",
            BufferAttribute::Binding => "binding index",
        }
    }
}

impl ParseError {
    pub fn report(&self) -> error::Report {
        use ariadne::{Report, ReportKind};
        use Attribute as At;

        let (source_id, range) = self.span.clone();
        let mut builder = Report::build(ReportKind::Error, source_id, range.start);
        let label: Cow<'static, str> = match self.kind {
            ParseErrorKind::LexError(ref lex_error) => lex_error.build_report(&mut builder).into(),
            ParseErrorKind::ExpectedStatement => {
                builder.set_message("Unexpected token at start of statement");
                builder.set_help(
                    "Statements begin with a command like `buffer`, `dispatch`, or `check`.",
                );
                "expected statement here".into()
            }
            ParseErrorKind::ExpectedAttrParameter(At::Buffer(attr)) => {
                builder.set_message(format!("Unexpected token in `@{}` attribute", attr.token()));
                builder.set_help(format!(
                    "Specify a {} with an attribute like `@{}(3)`.",
                    attr.description(),
                    attr.token()
                ));
                format!("value of `@{}` attribute", attr.token()).into()
            }
            ParseErrorKind::ExpectedInteger {
                unsigned: positive,
                ref message,
                ref help,
            } => {
                builder.set_message(message);
                builder.set_help(help);
                if positive {
                    "expected a positive integer or zero here".into()
                } else {
                    "expected an integer here".into()
                }
            }
            ParseErrorKind::ExpectedBufferAttributeOrType => {
                builder.set_message(
                    "In buffer statement, expected an attribute or a colon for the type",
                );
                builder.set_help(
                    "A buffer statement has the form `buffer ATTRIBUTES : TYPE = VALUE`.",
                );
                "unexpected symbol".into()
            }
            ParseErrorKind::DuplicateAttribute { attr, ref prior } => {
                builder.set_message(format!(
                    "{} attribute `@{}` appears more than once",
                    attr.owner(),
                    attr.token()
                ));
                builder.add_label(ariadne::Label::new(prior.clone()).with_message(format!(
                    "this is the first occurrence of the `@{}` attribute",
                    attr.token()
                )));
                "this attribute is redundant".into()
            }
        };

        builder.add_label(ariadne::Label::new(self.span.clone()).with_message(label));

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
