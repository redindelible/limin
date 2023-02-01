struct Source {
    name: String,
    text: String,
    line_starts: Box<[usize]>
}

struct Location<'a> {
    pub source: &'a Source,
    pub start: usize,
    pub len: usize
}

impl Source {
    fn from_text(name: &str, text: &str) -> Source {
        let mut line_starts = vec![0];
        for (i, c) in text.char_indices() {
            if c == '\n' {
                line_starts.push(i+1);
            }
        }
        line_starts.push(text.len()+1);
        Source { name: name.to_owned(), text: text.to_owned(), line_starts: line_starts.into_boxed_slice() }
    }

    fn get_line_on(&self, idx: usize) -> &str {
        let starts_idx = self.line_starts.binary_search(&idx).map_or_else(|x| x, |x| x);
        let line_start = self.line_starts[starts_idx];
        let line_end = self.line_starts[starts_idx+1];
        &self.text[line_start..line_end-1]
    }
}


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
        assert_eq!(s.get_line_on(0), "");
    }

    #[test]
    fn test_get_line_one() {
        let s = Source::from_text("test", "a");
        assert_eq!(s.get_line_on(0), "a");
    }

    #[test]
    fn test_get_line_new_empty() {
        let s = Source::from_text("test", "\n");
        assert_eq!(s.get_line_on(0), "");
        assert_eq!(s.get_line_on(1), "");
    }

    #[test]
    fn test_get_line_two() {
        let s = Source::from_text("test", "a\nb");
        assert_eq!(s.get_line_on(0), "a");
        assert_eq!(s.get_line_on(1), "b");
        assert_eq!(s.get_line_on(2), "b");
    }

    #[test]
    fn test_get_line_three() {
        let s = Source::from_text("test", "a\nb\n");
        assert_eq!(s.get_line_on(0), "a");
        assert_eq!(s.get_line_on(1), "b");
        assert_eq!(s.get_line_on(2), "b");
        assert_eq!(s.get_line_on(3), "");
    }
}