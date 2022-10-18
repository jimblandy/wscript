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
    ExpectedBufferId,
    ExpectedInitValue,
    ExpectedCheckValue,
    ExpectedEntryPointName,
    ExpectedInteger {
        unsigned: bool,
        message: Cow<'static, str>,
        help: Cow<'static, str>,
    },
    ExpectedModuleCode,
    ExpectedStatement,
    ExpectedType {
        thing: &'static str,
        introducing_span: Span,
        help: &'static str,
    },
    ExpectedTypeParameterBracket {
        constructor: String,
        constructor_span: Span,
        position: BracketPosition,
    },
    ExpectedScalarType {
        constructor: String,
        constructor_span: Span,
    },
    ExpectedWorkgroupCount {
        command_span: Span,
    },
    LexError(TokenError),
    MissingAttr(Attribute),
    MissingCloseParen {
        opening: Span,
    },
    TypeMatrixF32 {
        parameter: Span,
    },
    UnexpectedToken {
        place: &'static str,
        expected: &'static str,
    },
    WorkgroupCountTooLong,
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
        self.report_with_config(ariadne::Config::default())
    }

    pub fn report_with_config(&self, config: ariadne::Config) -> error::Report {
        use ariadne::{Report, ReportKind};
        use Attribute as At;

        let (source_id, range) = self.span.clone();
        let mut builder =
            Report::build(ReportKind::Error, source_id, range.start).with_config(config);
        let b = &mut builder;

        fn label<M: ToString>(b: &mut crate::error::ReportBuilder, span: &Span, message: M) {
            b.add_label(ariadne::Label::new(span.clone()).with_message(message));
        }

        let main_label: Cow<'static, str> = match self.kind {
            ParseErrorKind::DuplicateAttribute { attr, ref prior } => {
                b.set_message(format!(
                    "{} attribute `@{}` appears more than once",
                    attr.owner(),
                    attr.token()
                ));
                label(
                    b,
                    prior,
                    format!(
                        "this is the first occurrence of the `@{}` attribute",
                        attr.token()
                    ),
                );
                "this attribute is redundant".into()
            }
            ParseErrorKind::ExpectedAttrParameter(At::Buffer(attr)) => {
                b.set_message(format!("Unexpected token in `@{}` attribute", attr.token()));
                b.set_help(format!(
                    "Specify a {} with an attribute like `@{}(3)`.",
                    attr.description(),
                    attr.token()
                ));
                format!("value of `@{}` attribute", attr.token()).into()
            }
            ParseErrorKind::ExpectedBufferId => {
                b.set_message("Expected buffer name");
                b.set_help("Supply the name of a global variable in the current shader module.");
                "a buffer name is expected here".into()
            }
            ParseErrorKind::ExpectedInitValue => {
                b.set_message("Expected value to initialize buffer");
                b.set_help("An `init` statement has the form `init BUFFER = VALUE`.");
                "an `=` symbol is expected here, before an initial value expression".into()
            }
            ParseErrorKind::ExpectedCheckValue => {
                b.set_message("Expected value of expected buffer contents");
                b.set_help("A `check` statement has the form `check BUFFER = VALUE`.");
                "an `=` symbol is expected here, before an expected-value expression".into()
            }
            ParseErrorKind::ExpectedEntryPointName => {
                b.set_message("Expected name of shader entry point");
                b.set_help(
                    "A `dispatch` command starts with the name of the entry point to invoke.",
                );
                "entry point name expected here".into()
            }
            ParseErrorKind::ExpectedType {
                thing,
                ref introducing_span,
                help,
            } => {
                b.set_message("Expected type in {}");
                label(b, introducing_span, format!("this {} lacks a type", thing));
                b.set_help(help);
                "expected a type here".into()
            }
            ParseErrorKind::ExpectedInteger {
                unsigned: positive,
                ref message,
                ref help,
            } => {
                b.set_message(message);
                b.set_help(help);
                if positive {
                    "expected a positive integer or zero here".into()
                } else {
                    "expected an integer here".into()
                }
            }
            ParseErrorKind::ExpectedModuleCode => {
                b.set_message("Expected WGSL code for `module` statement");
                b.set_help("Supply indented WGSL code for the shader module.");
                "A triple-quoted code block is expected here".into()
            }
            ParseErrorKind::ExpectedStatement => {
                b.set_message("Unexpected token at start of statement");
                b.set_help(
                    "Statements begin with a command like `module`, `init`, `dispatch`, or `check`.",
                );
                "expected statement here".into()
            }
            ParseErrorKind::LexError(ref lex_error) => lex_error.build_report(b).into(),
            ParseErrorKind::MissingAttr(attribute) => {
                b.set_message(format!(
                    "{} is missing required `@{}` attribute",
                    attribute.owner(),
                    attribute.token(),
                ));
                "`{}` statement with missing attribute".into()
            }
            ParseErrorKind::MissingCloseParen { ref opening } => {
                b.set_message("expected closing parenthesis".to_string());
                label(b, opening, "this opening parenthesis is unmatched");
                "expected closing parenthesis here".into()
            }
            ParseErrorKind::ExpectedTypeParameterBracket {
                ref constructor,
                position,
                ref constructor_span,
            } => {
                let description = position.angle_description();
                b.set_message(format!(
                    "Expected {} for `{}` type parameter",
                    description, constructor
                ));
                label(b, constructor_span, "type constructor");
                b.set_help(format!(
                    "Type parameters are surrounded by `<` and `>` characters, like `{}<f32>`.",
                    constructor
                ));
                format!("expected {} bracket here", description).into()
            }
            ParseErrorKind::ExpectedScalarType {
                ref constructor,
                ref constructor_span,
            } => {
                b.set_message(format!(
                    "'{}' type constructor applied to non-scalar type",
                    constructor
                ));
                label(b, constructor_span, "type constructor");
                b.set_help(format!(
                    "The components of a `{}` must have a scalar type, like `f32` or `bool`.",
                    constructor
                ));
                "expected scalar type here".into()
            }
            ParseErrorKind::ExpectedWorkgroupCount { command_span: _ } => {
                b.set_message("Expected workgroup count for `dispatch` command");
                b.set_help(
                    "A workgroup count is a parenthesized list of one to three dimensions: \
                            `(10)`, `(16,32)`, `(64,64,64)`",
                );
                "expected workgroup count".into()
            }
            ParseErrorKind::TypeMatrixF32 { ref parameter } => {
                b.set_message("Only matrices of `f32` components are supported.");
                label(b, parameter, "type parameter must be `f32`");
                "matrix types must be of the form `matCxR<f32>`".into()
            }
            ParseErrorKind::UnexpectedToken { place, expected } => {
                b.set_message(format!("Unexpected token {}", place));
                format!("expected {} here", expected).into()
            }
            ParseErrorKind::WorkgroupCountTooLong => {
                b.set_message("Too many dimensions in workgroup count");
                b.set_help(
                    "A workgroup count has one to three dimensions, \
                                  like `(10)` or `(10,20,30)`.",
                );
                "Count should end here".into()
            }
        };

        label(b, &self.span, main_label);

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
        use crate::lex::TokenErrorKind as Tek;
        use ariadne::Label;

        match self.kind {
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
