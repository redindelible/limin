

pub fn map_join<I: IntoIterator, T: ToString, F: FnMut(I::Item) -> T>(it: I, closure: F) -> String {
    it.into_iter().map(closure).map(|t| t.to_string()).collect::<Vec<String>>().join(", ")
}