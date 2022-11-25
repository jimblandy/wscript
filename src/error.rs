//! Error reporting.

use crate::ast;

use indexmap::IndexMap;

use std::path::PathBuf;
use std::result::Result as StdResult;
use std::{fmt, io};

pub type Report = ariadne::Report<ast::Span>;
pub type ReportBuilder = ariadne::ReportBuilder<ast::Span>;

#[derive(Default)]
pub struct Cache {
    sources: IndexMap<PathBuf, ariadne::Source>,
}

impl Cache {
    pub fn insert(&mut self, path: PathBuf, source: &str) -> usize {
        self.sources
            .insert_full(path, ariadne::Source::from(source))
            .0
    }
}

impl ariadne::Cache<usize> for Cache {
    fn fetch(&mut self, &id: &usize) -> StdResult<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        match self.sources.get_index(id) {
            Some((_path, source)) => Ok(source),
            None => Err(Box::new(id)),
        }
    }

    fn display<'a>(&self, &id: &'a usize) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.sources
            .get_index(id)
            .map(|(path, _source)| -> Box<dyn std::fmt::Display + 'a> {
                Box::new(path.display().to_string())
            })
    }
}

impl fmt::Debug for Cache {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.sources.keys()).finish()
    }
}

/// Values that can be written to an output stream, given a [`Cache`].
pub trait AriadneReport {
    /// Produce an `ariadne::Report` using the given configuration.
    fn write_with_config<W>(
        &self,
        stream: W,
        cache: &mut Cache,
        config: ariadne::Config,
    ) -> io::Result<()>
    where
        W: io::Write;

    /// Produce an `ariadne::Report` using a default configuration.
    fn write<W>(&self, stream: W, cache: &mut Cache) -> io::Result<()>
    where
        W: io::Write,
    {
        self.write_with_config(stream, cache, ariadne::Config::default())
    }
}
