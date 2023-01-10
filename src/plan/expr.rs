//! Planning expression evaluation.

/// An execution plan for a wscript expression.
///
/// An expression plan is a closure that evaluates a wscript
/// expression. On success, it produces a value of type `T`. It may
/// fail, returning a `run::Error`. It may use and modify the state of
/// a `Context`.
///
/// [`Context`]: run::Context
pub type ExpressionPlan<T> = dyn Fn(&mut run::Context) -> Result<T, run::Error>;

