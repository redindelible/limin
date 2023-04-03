use std::cmp::{min, max};
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::io;
use std::io::Read;
use std::ops::Add;
use std::path::Path;

pub struct Source {
    pub path: String,
    pub text: String,
    line_starts: Box<[usize]>
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Location<'a> {
    pub source: &'a Source,
    pub start: usize,
    pub len: usize
}

pub trait HasLoc<'a> {
    fn loc(&self) -> Location<'a>;
}

#[derive(Eq, PartialEq, Debug)]
pub struct RenderedLocation {
    pub line: String,
    pub line_no: usize,
    pub line_idx: usize,
    pub len: usize,
    pub is_multiline: bool,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct LineInfo<'a> {
    text: &'a str,
    line_no: usize,
    line_start: usize
}

impl Source {
    pub fn from_file(path: &Path) -> io::Result<Source> {
        let name = dunce::canonicalize(path)?;
        let mut f = File::open(path)?;
        let mut s = String::new();
        f.read_to_string(&mut s)?;
        Ok(Source::from_text_owned(&name.to_string_lossy(), s))
    }

    fn from_text_owned(name: &str, text: String) -> Source {
        let mut line_starts = vec![0];
        for (i, c) in text.char_indices() {
            if c == '\n' {
                line_starts.push(i+1);
            }
        }
        line_starts.push(text.len()+1);
        Source { path: name.to_owned(), text, line_starts: line_starts.into_boxed_slice() }
    }

    pub fn from_text(name: &str, text: &str) -> Source {
        Source::from_text_owned(name, text.to_owned())
    }

    fn get_line_on(&self, idx: usize) -> LineInfo {
        let line_no = self.line_starts.binary_search(&idx).map_or_else(|x| x - 1, |x| x);
        let line_start = self.line_starts[line_no];
        let line_end = self.line_starts[line_no+1];
        let text = &self.text[line_start..line_end-1];
        LineInfo { text, line_no, line_start }
    }

    pub fn eof(&self) -> Location {
        let end = self.line_starts.last().unwrap()-1;
        Location { source: self, start: end, len: 1 }
    }
}

impl PartialEq for Source {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path && self.text == other.text
    }
}
impl Eq for Source { }


impl Location<'_> {
    pub fn render(&self) -> RenderedLocation {
        let line = self.source.get_line_on(self.start);
        RenderedLocation {
            line: line.text.to_owned(),
            line_no: line.line_no,
            line_idx: self.start - line.line_start,
            len: self.len.min(line.text.len() - (self.start - line.line_start)),
            is_multiline: line.text.len() - (self.start - line.line_start) < self.len
        }
    }
}

impl<'s> Add for Location<'s> {
    type Output = Location<'s>;

    fn add(self, rhs: Self) -> Self::Output {
        if rhs.source != self.source { panic!("sources must be the same") };
        let start = min(self.start, rhs.start);
        let end = max(self.start + self.len, rhs.start + rhs.len);
        Location { source: self.source, start, len: end - start}
    }
}

impl Debug for Location<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Location(source: {:?}, {}, {})", self.source.path, self.start, self.len)
    }
}

#[cfg(test)]
mod test {
    use crate::source::{Location, RenderedLocation, Source};

    #[test]
    fn test_line_starts_empty() {
        let s = Source::from_text("test","");
        assert_eq!(s.line_starts, vec![0, 1].into());
    }

    #[test]
    fn test_line_starts_one_line() {
        let s = Source::from_text("test","a");
        assert_eq!(s.line_starts, vec![0, 2].into());
    }

    #[test]
    fn test_line_starts_empty_line() {
        let s = Source::from_text("test","\n");
        assert_eq!(s.line_starts, vec![0, 1, 2].into());
    }

    #[test]
    fn test_line_starts_1() {
        let s = Source::from_text("test","a\na");
        assert_eq!(s.line_starts, vec![0, 2, 4].into());
    }

    #[test]
    fn test_line_starts_empty_lines() {
        let s = Source::from_text("test","a\n\n be");
        assert_eq!(s.line_starts, vec![0, 2, 3, 7].into());
    }

    #[test]
    fn test_get_line_empty() {
        let s = Source::from_text("test", "");
        let line_info = s.get_line_on(0);
        assert_eq!(line_info.text, "");
        assert_eq!(line_info.line_no, 0);
        assert_eq!(line_info.line_start, 0);
    }

    #[test]
    fn test_get_line_one() {
        let s = Source::from_text("test", "a");
        let line_info = s.get_line_on(0);
        assert_eq!(line_info.text, "a");
        assert_eq!(line_info.line_no, 0);
        assert_eq!(line_info.line_start, 0);
    }

    #[test]
    fn test_get_line_new_empty() {
        let s = Source::from_text("test", "\n");
        let line_info = s.get_line_on(0);
        assert_eq!(line_info.text, "");
        assert_eq!(line_info.line_no, 0);
        assert_eq!(line_info.line_start, 0);

        let line_info = s.get_line_on(1);
        assert_eq!(line_info.text, "");
        assert_eq!(line_info.line_no, 1);
        assert_eq!(line_info.line_start, 1);
    }

    #[test]
    fn test_get_line_longer() {
        let s = Source::from_text("test", "ba");
        let line_info = s.get_line_on(0);
        assert_eq!(line_info.text, "ba");
        assert_eq!(line_info.line_no, 0);
        assert_eq!(line_info.line_start, 0);

        assert_eq!(s.get_line_on(0), s.get_line_on(1));
    }

    #[test]
    fn test_get_line_and_half() {
        let s = Source::from_text("test", "\nb");
        let line_info = s.get_line_on(1);
        assert_eq!(line_info.text, "b");
        assert_eq!(line_info.line_no, 1);
        assert_eq!(line_info.line_start, 1);
    }

    #[test]
    fn test_get_line_two() {
        let s = Source::from_text("test", "a\nb");
        let line_info = s.get_line_on(0);
        assert_eq!(line_info.text, "a");
        assert_eq!(line_info.line_no, 0);
        assert_eq!(line_info.line_start, 0);

        let line_info = s.get_line_on(2);
        assert_eq!(line_info.text, "b");
        assert_eq!(line_info.line_no, 1);
        assert_eq!(line_info.line_start, 2);

        assert_eq!(s.get_line_on(0), s.get_line_on(1));
    }

    #[test]
    fn test_get_line_three() {
        let s = Source::from_text("test", "a\nb\n");
        let line_info = s.get_line_on(0);
        assert_eq!(line_info.text, "a");
        assert_eq!(line_info.line_no, 0);
        assert_eq!(line_info.line_start, 0);

        assert_eq!(s.get_line_on(0), s.get_line_on(1));

        let line_info = s.get_line_on(2);
        assert_eq!(line_info.text, "b");
        assert_eq!(line_info.line_no, 1);
        assert_eq!(line_info.line_start, 2);

        assert_eq!(s.get_line_on(2), s.get_line_on(3));
    }

    #[test]
    fn test_eof_empty() {
        let s = Source::from_text("test", "");
        assert_eq!(s.eof().start, 0);
        assert_eq!(s.eof().len, 1);
    }

    #[test]
    fn test_eof_newline() {
        let s = Source::from_text("test", "\n");
        assert_eq!(s.eof().start, 1);
        assert_eq!(s.eof().len, 1);
    }

    #[test]
    fn test_eof_char() {
        let s = Source::from_text("test", "g");
        assert_eq!(s.eof().start, 1);
        assert_eq!(s.eof().len, 1);
    }

    #[test]
    fn test_eof_long() {
        let s = Source::from_text("test", "gasda\nawaqer\na");
        assert_eq!(s.eof().start, 14);
        assert_eq!(s.eof().len, 1);
    }

    #[test]
    fn test_render_short() {
        let s = Source::from_text("test", "a");
        let loc = Location { source: &s, start: 0, len: 1 };
        let r = loc.render();
        assert_eq!(r, RenderedLocation { line: "a".into(), line_no: 0, line_idx: 0, len: 1, is_multiline: false});
    }

    #[test]
    fn test_render_longer() {
        let s = Source::from_text("test", "abad");
        let loc = Location { source: &s, start: 1, len: 3 };
        let r = loc.render();
        assert_eq!(r, RenderedLocation { line: "abad".into(), line_no: 0, line_idx: 1, len: 3, is_multiline: false});
    }

    #[test]
    fn test_render_second_line() {
        let s = Source::from_text("test", "\na");
        let loc = Location { source: &s, start: 1, len: 1 };
        let r = loc.render();
        assert_eq!(r, RenderedLocation { line: "a".into(), line_no: 1, line_idx: 0, len: 1, is_multiline: false});
    }

    #[test]
    fn test_render_long_line() {
        let s = Source::from_text("test", "\naba");
        let loc = Location { source: &s, start: 2, len: 2 };
        let r = loc.render();
        assert_eq!(r, RenderedLocation { line: "aba".into(), line_no: 1, line_idx: 1, len: 2, is_multiline: false});
    }

    #[test]
    fn test_render_multiline() {
        let s = Source::from_text("test", "asda\naba");
        let loc = Location { source: &s, start: 0, len: 6 };
        let r = loc.render();
        assert_eq!(r, RenderedLocation { line: "asda".into(), line_no: 0, line_idx: 0, len: 4, is_multiline: true});
    }

    #[test]
    fn test_render_multiline_edgecase() {
        let s = Source::from_text("test", "asda\naba");
        let loc = Location { source: &s, start: 0, len: 5 };
        let r = loc.render();
        assert_eq!(r, RenderedLocation { line: "asda".into(), line_no: 0, line_idx: 0, len: 4, is_multiline: true});
    }

    #[test]
    fn test_render_multiline_edgecase_2() {
        let s = Source::from_text("test", "asda\naba");
        let loc = Location { source: &s, start: 0, len: 4 };
        let r = loc.render();
        assert_eq!(r, RenderedLocation { line: "asda".into(), line_no: 0, line_idx: 0, len: 4, is_multiline: false});
    }
}