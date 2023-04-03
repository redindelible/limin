use crate::source::Location;

pub trait Message {
    fn render(&self);

    fn show_location(loc: Location) {
        let rendered_loc = loc.render();

        eprintln!("In '{}':", loc.source.path);
        eprintln!("{: >4} | {}", rendered_loc.line_no + 1, &rendered_loc.line);
        eprintln!(  "       {}{}", " ".repeat(rendered_loc.line_idx), "^".repeat(rendered_loc.len));
    }
}
