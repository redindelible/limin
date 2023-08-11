use std::{fmt, io};
use std::io::stderr;
use crate::source::Location;

struct WriterHelper<W: io::Write>(W);

impl<W: io::Write> fmt::Write for WriterHelper<W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
}

pub trait Message {
    fn write_into<W: fmt::Write>(&self, to: &mut W) -> fmt::Result;

    fn render_to_string(&self) -> String {
        let mut s = String::new();
        self.write_into(&mut s).unwrap();
        s
    }

    fn render_to_stderr(&self) {
        self.write_into(&mut WriterHelper(stderr())).unwrap();
    }

    fn show_location<W: fmt::Write>(loc: &Location, to: &mut W) -> fmt::Result {
        let rendered_loc = loc.render();

        writeln!(to, "In '{}':", loc.source.path)?;
        writeln!(to, "{: >4} | {}", rendered_loc.line_no + 1, &rendered_loc.line)?;
        writeln!(to,   "       {}{}", " ".repeat(rendered_loc.line_idx), "^".repeat(rendered_loc.len))
    }

    fn show_note_location<W: fmt::Write>(loc: &Location, to: &mut W) -> fmt::Result {
        let rendered_loc = loc.render();

        writeln!(to, " | In '{}':", loc.source.path)?;
        writeln!(to, " | {: >4} | {}", rendered_loc.line_no + 1, &rendered_loc.line)?;
        writeln!(to,   " |        {}{}", " ".repeat(rendered_loc.line_idx), "^".repeat(rendered_loc.len))
    }
}

impl<T> Message for [T] where T: Message {
    fn write_into<W: fmt::Write>(&self, to: &mut W) -> fmt::Result {
        for msg in self {
            msg.write_into(to)?;
            writeln!(to, "\n")?;
        }
        Ok(())
    }
}