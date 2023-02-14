pub struct Source {
    pub name: String,
    pub text: String,
    line_starts: Box<[usize]>
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Location<'a> {
    pub source: &'a Source,
    pub start: usize,
    pub len: usize
}

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
    pub fn from_text(name: &str, text: &str) -> Source {
        let mut line_starts = vec![0];
        for (i, c) in text.char_indices() {
            if c == '\n' {
                line_starts.push(i+1);
            }
        }
        line_starts.push(text.len()+1);
        Source { name: name.to_owned(), text: text.to_owned(), line_starts: line_starts.into_boxed_slice() }
    }

    fn get_line_on(&self, idx: usize) -> LineInfo {
        let line_no = self.line_starts.binary_search(&idx).map_or_else(|x| x, |x| x);
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
        self.name == other.name && self.text == other.text
    }
}
impl Eq for Source { }


// impl Location<'_> {
//     pub fn render() -> RenderedLocation {
//
//     }
// }

#[cfg(test)]
mod test {
    use crate::source::Source;

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
    fn test_get_line_two() {
        let s = Source::from_text("test", "a\nb");
        let line_info = s.get_line_on(0);
        assert_eq!(line_info.text, "a");
        assert_eq!(line_info.line_no, 0);
        assert_eq!(line_info.line_start, 0);

        let line_info = s.get_line_on(1);
        assert_eq!(line_info.text, "b");
        assert_eq!(line_info.line_no, 1);
        assert_eq!(line_info.line_start, 2);

        assert_eq!(s.get_line_on(1), s.get_line_on(2));
    }

    #[test]
    fn test_get_line_three() {
        let s = Source::from_text("test", "a\nb\n");
        let line_info = s.get_line_on(0);
        assert_eq!(line_info.text, "a");
        assert_eq!(line_info.line_no, 0);
        assert_eq!(line_info.line_start, 0);

        let line_info = s.get_line_on(1);
        assert_eq!(line_info.text, "b");
        assert_eq!(line_info.line_no, 1);
        assert_eq!(line_info.line_start, 2);

        assert_eq!(s.get_line_on(1), s.get_line_on(2));

        let line_info = s.get_line_on(3);
        assert_eq!(line_info.text, "");
        assert_eq!(line_info.line_no, 2);
        assert_eq!(line_info.line_start, 4);
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
}