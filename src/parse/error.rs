//! Parsing errors.

use crate::lex::{BracketPosition, TokenError};
use crate::{ast::Span, error};
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
    ExpectedAttrParameter(Attribute),
    ExpectedBufferAttributeOrType,
    ExpectedInteger {
        unsigned: bool,
        message: Cow<'static, str>,
        help: Cow<'static, str>,
    },
    ExpectedStatement,
    ExpectedTypeParameterBracket {
        constructor: &'static str,
        constructor_span: Span,
        position: BracketPosition,
    },
    ExpectedTypeParameter(&'static str),
    LexError(TokenError),
    MissingAttr(Attribute),
    TypeMatrixF32 {
        parameter: Span,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum Attribute {
    Buffer(BufferAttribute),
}

#[derive(Clone, Copy, Debug)]
pub enum BufferAttribute {
    Group,
    Binding,
}

impl ParseError {
    pub fn report(&self) -> error::Report {
        use ariadne::{Report, ReportKind};
        use Attribute as At;

        let (source_id, range) = self.span.clone();
        let mut builder = Report::build(ReportKind::Error, source_id, range.start);
        let label: Cow<'static, str> = match self.kind {
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
            ParseErrorKind::ExpectedAttrParameter(At::Buffer(attr)) => {
                builder.set_message(format!("Unexpected token in `@{}` attribute", attr.token()));
                builder.set_help(format!(
                    "Specify a {} with an attribute like `@{}(3)`.",
                    attr.description(),
                    attr.token()
                ));
                format!("value of `@{}` attribute", attr.token()).into()
            }
            ParseErrorKind::ExpectedBufferAttributeOrType => {
                builder.set_message(
                    "In `buffer` statement, expected an attribute, or a colon for the type",
                );
                builder.set_help(
                    "A `buffer` statement has the form: `buffer ATTRIBUTES : TYPE = VALUE`.\n\
                     For example: `buffer @group(0) @binding(3): u32 = 1729`",
                );
                "unexpected symbol".into()
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
            ParseErrorKind::ExpectedStatement => {
                builder.set_message("Unexpected token at start of statement");
                builder.set_help(
                    "Statements begin with a command like `buffer`, `dispatch`, or `check`.",
                );
                "expected statement here".into()
            }
            ParseErrorKind::LexError(ref lex_error) => lex_error.build_report(&mut builder).into(),
            ParseErrorKind::MissingAttr(attribute) => {
                builder.set_message(format!(
                    "{} is missing required `@{}` attribute",
                    attribute.owner(),
                    attribute.token(),
                ));
                "`{}` statement with missing attribute".into()
            }
            ParseErrorKind::ExpectedTypeParameterBracket {
                constructor,
                position,
                ref constructor_span,
            } => {
                let description = position.angle_description();
                builder.set_message(format!(
                    "Expected {} for `{}` type parameter",
                    description, constructor
                ));
                builder.add_label(
                    ariadne::Label::new(constructor_span.clone()).with_message("type constructor"),
                );
                builder.set_help(format!(
                    "Type parameters are surrounded by `<` and `>` characters, like `{}<f32>`.",
                    constructor
                ));
                format!("expected {} bracket here", description).into()
            }
            ParseErrorKind::ExpectedTypeParameter(constructor) => {
                builder.set_message(format!(
                    "Expected type parameter after '{}' type constructor",
                    constructor
                ));
                builder.set_help(format!(
                    "A well-formed {} type takes a type parameter, like: `{}<f32>`.",
                    constructor, constructor
                ));
                "expected type parameter here".into()
            }
            ParseErrorKind::TypeMatrixF32 { ref parameter } => {
                builder.set_message("Only matrices of `f32` components are supported.");
                builder.add_label(
                    ariadne::Label::new(parameter.clone())
                        .with_message("type parameter must be `f32`"),
                );
                "matrix types must be of the form `matCxR<f32>`".into()
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

impl TokenError {
    pub fn build_report(&self, builder: &mut crate::error::ReportBuilder) -> &'static str {
        use ariadne::Label;
        use crate::lex::TokenErrorKind as Tek;

        match self.kind {
            Tek::UnrecognizedWord => {
                builder.set_message("word is not a recognized part of the wscript vocabulary");
                "unrecognized word"
            }
            Tek::JunkAfterCodeBlockStart(ref junk) => {
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
            Tek::NumberOutOfRange => {
                builder.set_help(
                    "wscript numbers must be representable as a 64-bit IEEE double value.",
                );
                "number too large to represent"
            }
            Tek::TabInCodeBlock { ref tab, part } => {
                builder.add_label(
                    Label::new(tab.clone()).with_message("this tab character is not allowed"),
                );
                builder.set_help(
                    "Tabs have no well-defined width, so the lines that begin and end code\n\
                     blocks, as well as the lines that make up its content, must be indented\n\
                     with spaces only.",
                );

                match part {
                    crate::lex::CodeBlockPart::Introducing => {
                        "tab character in indentation of line introducing code block"
                    }
                    crate::lex::CodeBlockPart::Body => {
                        "tab character in indentation of the body of the code block"
                    }
                }
            }
        }
    }
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
