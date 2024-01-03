#![allow(dead_code)]

use std::fmt::Debug;
use std::hash::Hash;
use std::ops::RangeFrom;
use slotmap::{new_key_type, SlotMap};

pub fn pluralize(text: impl AsRef<str>, count: u64) -> String {
    if count == 1 {
        format!("1 {}", text.as_ref())
    } else {
        format!("{} {}s", count, text.as_ref())
    }
}

pub fn map_join<I: IntoIterator, T: ToString, F: FnMut(I::Item) -> T>(it: I, closure: F) -> String {
    it.into_iter().map(closure).map(|t| t.to_string()).collect::<Vec<String>>().join(", ")
}

pub fn join_with<I: IntoIterator>(it: I, sep: &str) -> String where I::Item: ToString {
    it.into_iter().map(|t| t.to_string()).collect::<Vec<String>>().join(sep)
}

pub fn map_join_with<I: IntoIterator, T: ToString, F: FnMut(I::Item) -> T>(it: I, closure: F, sep: &str) -> String {
    it.into_iter().map(closure).map(|t| t.to_string()).collect::<Vec<String>>().join(sep)
}

// impl<A, T: FromIterator<A> + IntoIterator<Item=A>> IntoIterTools<A> for T{
//
// }

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

pub use key_map::*;

mod key_map {
    use indexmap::IndexMap;
    use std::hash::Hash;
    use std::fmt::Debug;
    use std::ops::{Index, IndexMut};

    pub trait KeyMapKey : Copy + Clone + Hash + Eq + Debug + Default {
        fn next(&self) -> Self;
    }

    macro_rules! declare_key_type {
        { $($vis:vis struct $name:ident;)* }  => {
            $(
                #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
                #[repr(transparent)]
                $vis struct $name(u64);

                impl $crate::util::KeyMapKey for $name {
                    #[must_use]
                    fn next(&self) -> Self { Self(self.0 + 1) }
                }
            )*
        };
    }

    pub(crate) use declare_key_type;

    #[derive(Clone)]
    pub struct KeyMap<K: KeyMapKey, T> {
        next_key: K,
        items: IndexMap<K, T>
    }

    impl<K: KeyMapKey, T> KeyMap<K, T> {
        pub fn new() -> Self {
            KeyMap { next_key: K::default(), items: IndexMap::new() }
        }

        pub fn add(&mut self, value: T) -> K {
            let key = self.next_key;
            self.next_key = key.next();
            self.items.insert(key, value);
            key
        }

        pub fn contains_key(&self, key: K) -> bool {
            self.items.contains_key(&key)
        }

        pub fn insert(&mut self, key: K, value: T) -> Option<T> {
            self.items.insert(key, value)
        }

        pub fn reserve(&mut self) -> K {
            let key = self.next_key;
            self.next_key = key.next();
            key
        }

        pub fn get(&self, key: K) -> Option<&T> {
            self.items.get(&key)
        }

        pub fn values(&self) -> impl Iterator<Item=&T> { self.items.values() }

        pub fn iter(&self) -> impl Iterator<Item=(K, &T)> {
            self.items.iter().map(|(k, t)| (*k, t))
        }

        pub fn map<O>(self, mut f: impl FnMut(T) -> O) -> KeyMap<K, O> {
            let mut new_items = IndexMap::with_capacity(self.items.len());
            for (key, value) in self.items {
                new_items.insert(key, f(value));
            }
            KeyMap { next_key: self.next_key, items: new_items }
        }
    }

    impl<K: KeyMapKey, T> Index<K> for KeyMap<K, T> {
        type Output = T;

        fn index(&self, index: K) -> &T {
            &self.items[&index]
        }
    }

    impl<K: KeyMapKey, T> IndexMut<K> for KeyMap<K, T> {
        fn index_mut(&mut self, index: K) -> &mut T {
            self.items.get_mut(&index).unwrap()
        }
    }


    impl<K: KeyMapKey, T> IntoIterator for KeyMap<K, T> {
        type Item = (K, T);
        type IntoIter = <IndexMap<K, T> as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            self.items.into_iter()
        }
    }

    impl<'a, K: KeyMapKey, T> IntoIterator for &'a KeyMap<K, T> {
        type Item = (K, &'a T);
        type IntoIter = Iter<'a, K, T>;

        fn into_iter(self) -> Self::IntoIter {
            Iter { items: self.items.iter() }
        }
    }

    pub struct Iter<'a, K: KeyMapKey, T> {
        items: <&'a IndexMap<K, T> as IntoIterator>::IntoIter
    }

    impl<'a, K: KeyMapKey, T> Iterator for Iter<'a, K, T> {
        type Item = (K, &'a T);

        fn next(&mut self) -> Option<(K, &'a T)> {
            self.items.next().map(|(k, t)| (*k, t))
        }
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