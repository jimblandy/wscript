//! Abstract syntax tree for wgpu-script.

/// A span of wscript source code, as a byte range.
pub type Span = (usize, std::ops::Range<usize>);

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
    /// Create a buffer.
    Buffer {
        binding: Binding,

        /// The type of the data stored in the buffer
        ty: Type,

        /// The value stored in the buffer, of the given type
        value: Expression,
    },

    /// Run a compute shader.
    Dispatch {
        /// WGSL source code for the compute shader.
        wgsl: Wgsl,
    },

    /// Check the contents of a buffer against expected values.
    Check {
        binding: Option<(u32, u32)>,
        value: Expression,
    },
}

#[derive(Debug)]
pub struct Binding {
    pub group: (u32, Span),
    pub binding: (u32, Span),
}

#[derive(Debug)]
pub struct Wgsl {
    /// WGSL source text, with all common indentation removed.
    pub text: String,

    /// Indentation depth, in columns
    pub indentation: usize,

    /// Position of this WGSL source code in the script.
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

#[derive(Debug)]
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

#[derive(Debug)]
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
