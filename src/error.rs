//! Error reporting.

use crate::ast;

use indexmap::IndexMap;
use anyhow::anyhow;

use std::fmt;
use std::path::{self, PathBuf};

pub type Report = ariadne::Report<ast::Span>;
pub type ReportBuilder = ariadne::ReportBuilder<ast::Span>;

#[derive(Default)]
pub struct Cache {
    sources: IndexMap<PathBuf, ariadne::Source>,
}

impl Cache {
    pub fn insert(&mut self, path: PathBuf, source: &str) -> usize {
        self.sources.insert_full(path, ariadne::Source::from(source)).0
    }
}

impl ariadne::Cache<usize> for Cache {
    fn fetch(&mut self, &id: &usize) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        match self.sources.get_index(id) {
            Some((_path, source)) => Ok(source),
            None => Err(Box::new(id))
        }
    }

    fn display<'a>(&self, &id: &'a usize) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.sources.get_index(id).map(|(path, _source)| -> Box<dyn std::fmt::Display + 'a> {
            Box::new(path.display().to_string())
        })
    }
}

impl fmt::Debug for Cache {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.sources.keys()).finish()
    }
}
