extern crate dunce;

use std::ops::{Range, Add};
use std::path::{Path, PathBuf};

pub struct Source {
    path: PathBuf,
    pub text: String,
    line_starts: Vec<usize>
}

#[derive(Copy, Clone)]
pub enum Loc<'com> {
    Inline {
        source: &'com Source,
        idx: usize,
        len: usize
    },
    Multiline {
        source: &'com Source,
        idx: usize,
    },
    EndOfFile {
        source: &'com Source
    }
}

impl<'com> Loc<'com> {
    pub fn new_inline(source: &'com Source, range: &Range<usize>) -> Loc<'com> {
        Loc::Inline { source, idx: range.start, len: range.len() }
    }

    pub fn new_eof(source: &'com Source) -> Loc<'com> {
        Loc::EndOfFile { source }
    }

    pub fn new_multiline(source: &'com Source, idx: usize) -> Loc<'com> {
        Loc::Multiline { source, idx }
    }

    fn source(&self) -> &'com Source {
        match *self {
            Loc::Inline { source, .. } => source,
            Loc::Multiline { source, ..} => source,
            Loc::EndOfFile { source } => source
        }
    }

    fn line(&self) -> Line {
        match *self {
            Loc::Inline { source, idx, .. } => source.get_line(idx),
            Loc::Multiline { source, idx } => source.get_line(idx),
            Loc::EndOfFile { source } => source.get_last_line()
        }
    }

    fn idx(&self) -> usize {
        match *self {
            Loc::Inline { idx, .. } => idx,
            Loc::Multiline { idx, ..} => idx,
            Loc::EndOfFile { source } => {
                let line = source.get_last_line();
                line.line_start + line.line.len()
            }
        }
    }
}

impl<'com> Add for &Loc<'com> {
    type Output = Loc<'com>;

    fn add(self, rhs: Self) -> Self::Output {
        if !std::ptr::eq(self.source(), rhs.source()) {
            panic!("sources must be the same")
        }
        match (self, rhs) {
            (Loc::EndOfFile { source }, _) | (_, Loc::EndOfFile { source }) => Loc::EndOfFile { source },
            (Loc::Multiline { source, idx}, other) | (other, Loc::Multiline { source, idx }) => {
                Loc::Multiline { source, idx: std::cmp::min(*idx, other.idx())}
            }
            (Loc::Inline { idx: idx1, len: len1, .. }, Loc::Inline { idx: idx2, len: len2, .. }) => {
                let (a_line, b_line) = (self.line(), rhs.line());
                let idx = *std::cmp::min(idx1, idx2);
                if a_line.line_no != b_line.line_no {
                    Loc::Multiline { source: self.source(), idx }
                } else {
                    Loc::Inline { source: self.source(), idx, len: std::cmp::max(idx1 + len1, idx2 + len2) - idx}
                }
            }
        }
    }
}

impl<'com> Add for Loc<'com> {
    type Output = Loc<'com>;

    fn add(self, rhs: Self) -> Self {
        &self + &rhs
    }
}

#[derive(Debug)]
pub struct RenderedLoc {
    pub source: String,
    pub line: String,
    pub line_no: usize,
    pub range: Range<usize>,
    pub continues: bool
}

struct Line<'a> {
    line_no: usize,
    line_start: usize,
    line: &'a str
}

impl Source {
    pub fn from_file(file: &Path) -> Option<Source> {
        let path = dunce::canonicalize(file).ok()?;
        let text = std::fs::read_to_string(&path).ok()?;
        let mut line_starts = vec![0];
        for (idx, chr) in text.char_indices() {
            if chr == '\n' {
                line_starts.push(idx+1);
            }
        }
        return Some(Source { path, text, line_starts });
    }

    pub fn from_text(text: &str) -> Source {
        let path = PathBuf::from("<string>");
        let text = text.to_string();
        let mut line_starts = vec![0];
        for (idx, chr) in text.char_indices() {
            if chr == '\n' {
                line_starts.push(idx+1);
            }
        };
        Source { path, text, line_starts }
    }

    fn get_line(&self, idx: usize) -> Line {
        let line_no;
        let line_start;
        let line_end;
        match self.line_starts.binary_search(&idx) {
            Ok(line_start_idx) => {
                line_no = line_start_idx;
                line_start = self.line_starts[line_start_idx];
                line_end = match self.line_starts.get(line_start_idx+1) {
                    Some(end) => end - 1,
                    None => self.text.len(),
                };
            },
            Err(line_end_idx) => {
                line_no = line_end_idx-1;
                line_start = self.line_starts[line_end_idx-1];
                line_end = match self.line_starts.get(line_end_idx) {
                    Some(end) => end - 1,
                    None => self.text.len(),
                };
            }
        };
        Line { line_no, line_start, line: &self.text[line_start..line_end] }
    }

    fn get_last_line(&self) -> Line {
        let line_no = self.line_starts.len() - 1;
        let line_start = *self.line_starts.last().unwrap();
        let line_end = self.text.len();
        Line { line_no, line_start, line: &self.text[line_start..line_end] }
    }
}

impl<'com> Loc<'com> {
    pub fn render(&self) -> RenderedLoc {
        match *self {
            Loc::Inline { source, idx, len } => {
                let Line { line_no, line_start, line } = source.get_line(idx);
                let range = (idx - line_start)..(idx - line_start)+len;
                RenderedLoc { source: source.path.to_string_lossy().to_string(), line: line.to_owned(), line_no, range, continues: false }
            },
            Loc::Multiline { source, idx} => {
                let Line { line_no, line_start, line } = source.get_line(idx);
                let range = (idx - line_start)..line.len();
                RenderedLoc { source: source.path.to_string_lossy().to_string(), line: line.to_owned(), line_no, range, continues: true }
            },
            Loc::EndOfFile { source } => {
                let Line { line_no, line, .. } = source.get_last_line();
                let range = line.len()..line.len()+1;
                RenderedLoc { source: source.path.to_string_lossy().to_string(), line: line.to_owned(), line_no, range, continues: false }
            }
        }
    }
}