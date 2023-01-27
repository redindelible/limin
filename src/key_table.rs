use std::hash::{Hash, Hasher};
use std::ops::{Index, IndexMut};

pub struct KeyTable<T> {
    items: Vec<T>
}

pub struct Key<T> {
    key: usize,
    phantom: std::marker::PhantomData<T>
}

impl<T> Hash for Key<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.hash(state)
    }
}

impl<T> PartialEq for Key<T> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}
impl<T> Eq for Key<T> { }

impl<T> Clone for Key<T> {
    fn clone(&self) -> Self {
        Key { ..*self }
    }
}

impl<T> Copy for Key<T> { }

impl<T> KeyTable<T> {
    pub fn new() -> Self {
        KeyTable {
            items: Vec::new()
        }
    }

    pub fn insert(&mut self, item: T) -> Key<T> {
        let key = self.items.len();
        self.items.push(item);
        Key { key, phantom: std::marker::PhantomData }
    }

    pub fn get(&mut self, key: Key<T>) -> Option<&T> {
        self.items.get(key.key)
    }

    pub fn get_mut(&mut self, key: Key<T>) -> Option<&mut T> {
        self.items.get_mut(key.key)
    }
}

impl<T> Index<Key<T>> for KeyTable<T> {
    type Output = T;

    fn index(&self, index: Key<T>) -> &Self::Output {
        &self.items[index.key]
    }
}

impl<T> IndexMut<Key<T>> for KeyTable<T> {
    fn index_mut(&mut self, index: Key<T>) -> &mut Self::Output {
        &mut self.items[index.key]
    }
}