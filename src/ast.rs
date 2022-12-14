//! Abstract syntax tree for wgpu-script.
#![allow(dead_code)]

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
    /// Set a module.
    Module {
        /// WGSL source code for the compute shader.
        wgsl: Wgsl,
    },

    /// Initialize a buffer.
    Init {
        /// The buffer to initialize.
        buffer: BufferId,

        /// The value stored in the buffer, of the given type
        value: Expression,
    },

    /// Run a compute shader.
    Dispatch {
        /// Name of the entry point to invoke.
        entry_point: EntryPoint,

        /// Number of workgroups to dispatch
        count: WorkgroupCount,
    },

    /// Check the contents of a buffer against expected values.
    Check {
        /// The buffer whose contents we should check.
        buffer: BufferId,

        /// The value we expect to find there.
        value: Expression,
    },
}

/// A way to identify a particular buffer
#[derive(Debug)]
pub enum BufferId {
    /// The name of the variable bound to this buffer in the shader.
    Name { id: String, span: Span },

    /// Binding group and index.
    Binding { naga: naga::ResourceBinding, span: Span },
}

#[derive(Debug)]
pub struct EntryPoint {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct Wgsl {
    /// WGSL source text, with all common indentation removed.
    pub text: String,

    /// Position of this WGSL source code in the script.
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
