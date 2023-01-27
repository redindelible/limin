use std::path::Path;
use typed_arena::Arena;
use crate::source::Source;

pub struct Compiler {
    sources: Arena<Source>
}

impl Compiler {
    pub fn add_file(&self, file: &Path) -> Option<&Source> {
        Some(self.sources.alloc(Source::from_file(file)?))
    }
}