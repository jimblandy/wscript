//! Abstract syntax tree for wgpu-script.
#![allow(dead_code, unreachable_code)]

use std::{fmt, ops::Range};

/// A span of wscript source code, as a byte range.
pub type Span = (usize, Range<usize>);

pub type Program = Vec<Statement>;

/// A wscript statement.
#[derive(Debug)]
pub struct Statement {
    /// Information specific to a particular kind of statement.
    pub kind: StatementKind,

    /// The statement's position within the script.
    pub span: Span,
}

#[derive(Debug)]
pub enum StatementKind {
    /// Set a module.
    ///
    /// ```ignore
    /// "module" Code
    /// ```
    Module {
        /// WGSL source code for the compute shader.
        wgsl: std::sync::Arc<CodeBlock>,
    },

    /// Initialize a buffer.
    ///
    /// ```ignore
    /// "init" Ident "=" Expression
    /// ```
    Init {
        /// The buffer to initialize.
        buffer: BufferId,

        /// The value stored in the buffer, of the given type
        value: Expression,
    },

    /// Run a compute shader.
    ///
    /// ```ignore
    /// "dispatch" Ident WorkgroupCount
    /// ```
    Dispatch {
        /// Name of the entry point to invoke.
        entry_point: EntryPoint,

        /// Number of workgroups to dispatch
        count: WorkgroupCount,
    },

    /// Check the contents of a buffer against expected values.
    ///
    /// ```ignore
    /// "check" Ident "=" Expression
    /// ```
    Check {
        /// The buffer whose contents we should check.
        buffer: BufferId,

        /// The value we expect to find there.
        value: Expression,
    },
}

/// A way to identify a particular buffer.
#[derive(Clone, Debug)]
pub struct BufferId {
    pub kind: BufferIdKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum BufferIdKind {
    /// The name of the variable bound to this buffer in the shader.
    Name(String),

    /// Binding group and index.
    Binding(naga::ResourceBinding),
}

#[derive(Debug)]
pub struct EntryPoint {
    pub name: String,
    pub span: Span,
}

/// A workgroup size or count.
#[derive(Debug)]
pub struct WorkgroupCount {
    pub size: (usize, usize, usize),
    pub span: Span,
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Literal(f64),
    Sequence(Vec<Expression>),
    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        op_span: Span,
        right: Box<Expression>,
    },
    Nullary(Nullary),
    Vec(VectorSize),
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Negate,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Range,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

#[derive(Clone, Copy, Debug)]
pub enum Nullary {
    XHat,
    YHat,
    ZHat,
    Identity,
}

/// The type of a value in a buffer.
#[derive(Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeKind {
    /// A scalar type.
    Scalar(ScalarKind),

    /// A vector type.
    Vector {
        size: VectorSize,
        component: ScalarKind,

        /// The span of the component type.
        component_span: Span,
    },

    /// A matrix type (elements are always `f32`).
    Matrix {
        columns: VectorSize,
        rows: VectorSize,
    },

    /// An array type.
    Array {
        /// The type of the elements of this array.
        element_type: Box<Type>,

        /// The number of elements in the array, if given.
        length: Option<usize>,
    },
}

/// A scalar type kind.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScalarKind {
    I32,
    U32,
    F32,
    Bool,
}

/// The number of components in a vector.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VectorSize {
    Vec2 = 2,
    Vec3 = 3,
    Vec4 = 4,
}

/// The contents of a `"""` code block.
#[derive(Debug, PartialEq)]
pub struct CodeBlock {
    /// Position of this WGSL source code in the script.
    pub span: Span,

    /// The code represented. Common indentation and leading and trailing blank lines are removed.
    pub text: String,

    /// A mapping from positions in `text` to the corresponding byte ranges in
    /// the wscript source, so that we can find script spans corresponding to
    /// Naga spans.
    ///
    /// Sorted by increasing start position. Input byte ranges do not overlap,
    /// and are never empty.
    ///
    /// Since we trim trailing whitespace and normalize line endings, the newlines
    /// in `text` are not covered by these ranges.
    pub map: Vec<(usize, Range<usize>)>,
}

impl CodeBlock {
    /// Find the wscript span corresponding to a byte range in `self.text`.
    ///
    /// Given a byte range in `self.text`, return the corresponding
    /// byte range in the surrounding wscript.
    pub fn span_from_text_range(&self, text_span: Range<usize>) -> Range<usize> {
        let map = |pos| {
            match self.map.binary_search_by_key(&pos, |&(start, _)| start) {
                Ok(exact_index) => self.map[exact_index].1.start,
                Err(insertion_index) => match self.map.get(insertion_index - 1) {
                    None => {
                        // This implies that `insertion_index` must be 0, which in
                        // turn implies that `text_span.start` lies before any of
                        // the ranges in `self.map`. Call it the start of the text.
                        assert_eq!(insertion_index, 0);
                        self.span.1.start
                    }
                    // The only characters in `self.text` not covered by `map`'s
                    // spans are newlines, so even if `text_span.start` falls at the
                    // end of the range, or one byte afterwards, it's still probably
                    // fine to just return a position relative to range.start.
                    Some(&(start, ref range)) => range.start + (pos - start),
                },
            }
        };

        map(text_span.start)..map(text_span.end)
    }
}

pub fn join_spans(left: &Span, right: &Span) -> Span {
    assert_eq!(left.0, right.0);
    (left.0, left.1.start..right.1.end)
}

impl Type {
    pub fn is_scalar(&self) -> Option<ScalarKind> {
        match *self {
            Type {
                kind: TypeKind::Scalar(kind),
                ..
            } => Some(kind),
            _ => None,
        }
    }
}

impl fmt::Display for BufferIdKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            BufferIdKind::Name(ref id) => {
                write!(f, "{:?}", id)
            }
            BufferIdKind::Binding(ref naga) => {
                write!(f, "@group({}) @binding({})", naga.group, naga.binding)
            }
        }
    }
}
