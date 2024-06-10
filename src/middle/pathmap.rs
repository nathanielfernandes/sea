use std::hash::Hash;

use indexmap::IndexMap;
use smallvec::SmallVec;

// typically we won't need more than 4 parts in a path
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Path<K: Copy + Hash + Eq>(pub SmallVec<[K; 4]>);

impl<K: Copy + Hash + Eq> Path<K> {
    pub fn new() -> Self {
        Self(SmallVec::new())
    }

    pub fn push(&mut self, key: K) {
        self.0.push(key);
    }

    pub fn pop(&mut self) -> Option<K> {
        self.0.pop()
    }

    pub fn last(&self) -> Option<&K> {
        self.0.last()
    }

    pub fn from(key: &[K]) -> Self {
        Self(SmallVec::from_slice(key))
    }

    pub fn from_iter(iter: impl IntoIterator<Item = K>) -> Self {
        Self(SmallVec::from_iter(iter))
    }
}

pub struct PathMap<K: Copy + Hash + Eq, V> {
    map: IndexMap<Path<K>, V>,
}

impl<K: Copy + Hash + Eq, V> PathMap<K, V> {
    pub fn new() -> Self {
        Self {
            map: IndexMap::new(),
        }
    }

    pub fn insert(&mut self, path: Path<K>, value: V) {
        self.map.insert(path, value);
    }

    pub fn get(&self, path: &Path<K>) -> Option<&V> {
        self.map.get(path)
    }

    pub fn get_all_for(&self, path: &Path<K>) -> Vec<Path<K>> {
        self.map
            .keys()
            .filter(|k| k.0.starts_with(&path.0))
            .map(|k| Path(k.0.clone()))
            .collect()
    }

    pub fn get_qualified_paths(&self, key: K) -> Vec<Path<K>> {
        self.map
            .keys()
            .filter(|k| k.0.contains(&key))
            .map(|k| Path(k.0.clone()))
            .collect()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.map.values()
    }
}

#[test]
fn test() {
    let mut map = PathMap::<&'static str, &'static str>::new();

    map.insert(Path::from(&["a", "b", "c"]), "yabba");
    map.insert(Path::from(&["soup", "b", "c"]), "yabba");
    map.insert(Path::from(&["a", "b", "goop"]), "yabba");

    assert_eq!(map.get(&Path::from(&["a", "b", "c"])), Some(&"yabba"));

    assert_eq!(
        map.get_all_for(&Path::from(&["a", "b"])),
        vec![
            Path::from(&["a", "b", "c"]),
            Path::from(&["a", "b", "goop"])
        ]
    );

    assert_eq!(
        map.get_qualified_paths("c"),
        vec![
            Path::from(&["a", "b", "c"]),
            Path::from(&["soup", "b", "c"])
        ]
    );
}
