#![allow(dead_code)]

use std::ops::RangeFrom;
use slotmap::{new_key_type, SlotMap};

pub fn map_join<I: IntoIterator, T: ToString, F: FnMut(I::Item) -> T>(it: I, closure: F) -> String {
    it.into_iter().map(closure).map(|t| t.to_string()).collect::<Vec<String>>().join(", ")
}

pub fn join_with<I: IntoIterator>(it: I, sep: &str) -> String where I::Item: ToString {
    it.into_iter().map(|t| t.to_string()).collect::<Vec<String>>().join(sep)
}

pub fn map_join_with<I: IntoIterator, T: ToString, F: FnMut(I::Item) -> T>(it: I, closure: F, sep: &str) -> String {
    it.into_iter().map(closure).map(|t| t.to_string()).collect::<Vec<String>>().join(sep)
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct Counter {
    count: usize
}

impl Counter {
    pub fn new(start: usize) -> Counter {
        Counter { count: start }
    }

    pub fn next(&mut self) -> usize {
        let value = self.count;
        self.count += 1;
        value
    }
}

impl From<RangeFrom<usize>> for Counter {
    fn from(value: RangeFrom<usize>) -> Self {
        Counter { count: value.start }
    }
}

new_key_type! {
    pub struct StackKey;
}

struct StackItem<Ann> {
    parent: Option<StackKey>,
    ann: Ann
}

pub struct StackTrace<Ann> {
    stack_tree: SlotMap<StackKey, StackItem<Ann>>,
    curr: Option<StackKey>
}

impl<Ann> StackTrace<Ann> {
    pub fn new() -> Self {
        StackTrace {
            stack_tree: SlotMap::with_key(),
            curr: None
        }
    }

    pub fn top(&self) -> Option<StackKey> {
        self.curr
    }

    pub fn push(&mut self, ann: Ann) -> StackKey {
        let key = self.stack_tree.insert(StackItem { parent: self.curr, ann });
        self.curr = Some(key);
        key
    }

    pub fn pop(&mut self) -> StackKey {
        if let Some(curr) = self.curr {
            self.curr = self.stack_tree[curr].parent;
            curr
        } else {
            panic!()
        }
    }

    pub fn pop_to(&mut self, to: Option<StackKey>) {
        if let Some(to) = to {
            while self.curr.unwrap() != to {
                self.pop();
            }
        } else {
            while self.curr.is_some() {
                self.pop();
            }
        }
    }

    pub fn ann(&self, key: StackKey) -> &Ann {
        &self.stack_tree[key].ann
    }

    pub fn ann_mut(&mut self, key: StackKey) -> &mut Ann {
        &mut self.stack_tree[key].ann
    }

    pub fn stack(&self, key: Option<StackKey>) -> Iter<Ann> {
        Iter { trace: self, curr: key }
    }

    pub fn stack_mut(&mut self, key: Option<StackKey>) -> IterMut<Ann> {
        IterMut { trace: self, curr: key }
    }
}

pub struct Iter<'a, Ann> {
    trace: &'a StackTrace<Ann>,
    curr: Option<StackKey>
}

impl<'a, Ann> Iterator for Iter<'a, Ann> {
    type Item = (StackKey, &'a Ann);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(curr) = self.curr {
            self.curr = self.trace.stack_tree[curr].parent;
            let item = &self.trace.stack_tree[curr].ann;
            Some((curr, item))
        } else {
            None
        }
    }
}

pub struct IterMut<'a, Ann> {
    trace: &'a mut StackTrace<Ann>,
    curr: Option<StackKey>
}

impl<'a, Ann> Iterator for IterMut<'a, Ann> {
    type Item = (StackKey, &'a mut Ann);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(curr) = self.curr {
            self.curr = self.trace.stack_tree[curr].parent;
            let item = &mut self.trace.stack_tree[curr].ann as *mut Ann;
            let item = unsafe {
                &mut *item
            };
            Some((curr, item))
        } else {
            None
        }
    }
}