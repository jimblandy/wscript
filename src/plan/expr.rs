//! Planning expression evaluation.
#![allow(unused_imports)]

use crate::{ast, run};
use super::{Module, Planner};
use super::error::{Error, ErrorKind, Result};

/// A source of data to initialize a GPU buffer, or check its contents.
pub trait ByteSource {
    /// Return the remaining number of bytes this plan will generate.
    fn len(&self) -> usize;

    /// Fill `buf` with the next `buf.len()` bytes from this source.
    fn fill(&mut self, buf: &mut [u8]) -> run::Result<()>;

    /// Check `buf` against the next `buf.len()` bytes from this source.
    fn check(&mut self, buf: &[u8]) -> run::Result<Comparison>;
}

/// A plan that can construct a [`ByteSource`]
pub type BytesPlan =
    dyn Fn(&mut run::Context) -> run::Result<Box<dyn ByteSource + 'static>> + 'static;

pub enum Comparison {
    /// This plan's bytes have the given length, and the corresponding
    /// prefix of the input matches.
    Matches { len: usize },

    /// This plan's bytes do not match the input, at the given byte offset.
    Mismatch { pos: usize },
}

impl Planner {
    pub fn plan_expression(
        &mut self,
        expr: &ast::Expression,
        module: &Module,
        expected_type: naga::Handle<naga::Type>,
    ) -> Result<Box<BytesPlan>> {
        todo!()
    }
}
