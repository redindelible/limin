use std::fmt::{Debug, Display, Formatter};
use crate::source::{Loc, RenderedLoc};

pub struct CompilerMessage {
    level: MessageLevel,
    msg: String,
    loc: Option<RenderedLoc>,
    indent: usize,
    sub_messages: Vec<CompilerMessage>
}

#[derive(Debug)]
pub enum MessageLevel {
    Error,
    Warning,
    Note
}

impl CompilerMessage {
    pub fn new_located(level: MessageLevel, msg: String, loc: Loc) -> CompilerMessage {
        CompilerMessage { level, msg, loc: Some(loc.render()), indent: 0, sub_messages: Vec::new() }
    }

    pub fn new_no_loc(level: MessageLevel, msg: String) -> CompilerMessage {
        CompilerMessage { level, msg, loc: None, indent: 0, sub_messages: Vec::new() }
    }

    pub fn add_message(&mut self, mut message: CompilerMessage) {
        message.indent = self.indent + 1;
        self.sub_messages.push(message);
    }

    pub fn show_messages(msgs: Vec<CompilerMessage>) {
        for msg in msgs {
            eprintln!("{}", msg);
        }
    }
}

impl Display for CompilerMessage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}{: >5}: {}", " |  ".repeat(self.indent), match self.level {
            MessageLevel::Error => "Error",
            MessageLevel::Warning => "Warn",
            MessageLevel::Note => "Note"
        }, self.msg)?;
        match &self.loc {
            Some(loc) => {
                writeln!(f, "{}{: >4} | {}", " |  ".repeat(self.indent), loc.line_no, loc.line)?;
                writeln!(f, "{}       {}{}", " |  ".repeat(self.indent), " ".repeat(loc.range.start), "^".repeat(loc.range.len()))
            }
            None => {
                Ok(())
            }
        }?;

        for sub_message in &self.sub_messages {
            write!(f, "{}", sub_message)?
        }
        Ok(())
    }
}

impl Debug for CompilerMessage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CompilerMessage({:?}, {}, {:?})", self.level, self.msg, self.sub_messages)?;
        Ok(())
    }
}