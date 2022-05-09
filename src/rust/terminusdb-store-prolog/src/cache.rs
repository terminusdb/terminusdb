use std::sync::{Arc, Weak};
use dashmap::DashMap;
use dashmap::mapref::entry::Entry;
use std::cmp::Eq;
use std::hash::Hash;
use std::ops::Deref;

#[derive(Clone)]
pub struct Cache<Key:Eq+Hash+Clone, Cached> {
    map: Arc<DashMap<Key, Weak<ObjectInCache<Key, Cached>>>>
}

pub struct ObjectInCache<Key:Eq+Hash+Clone, Cached> {
    key: Key,
    object: Cached,
    cache: Arc<DashMap<Key, Weak<Self>>>
}

impl<Key:Eq+Hash+Clone, Cached> Cache<Key, Cached> {
    pub fn new() -> Self {
        Self {
            map: Arc::new(DashMap::new())
        }
    }

    pub fn insert(&self, key: Key, object: Cached) -> Arc<ObjectInCache<Key, Cached>> {
        let entry = self.map.entry(key.clone());
        match entry {
            Entry::Occupied(mut o) => {
                // encountered weak pointer in cache. It may not be upgradeable though
                if let Some(arc) = o.get().upgrade() {
                    arc
                }
                else {
                    let arc = Arc::new(ObjectInCache::new(key, object, self.clone()));
                    o.insert(Arc::downgrade(&arc));

                    arc
                }
            },
            Entry::Vacant(v) => {
                let arc = Arc::new(ObjectInCache::new(key, object, self.clone()));
                v.insert(Arc::downgrade(&arc));

                arc
            }
        }
    }

    pub fn get(&self, key: &Key) -> Option<Arc<ObjectInCache<Key, Cached>>> {
        self.map.get(key)
            .and_then(|weak| weak.upgrade())
    }
}

impl<Key:Eq+Hash+Clone, Cached> ObjectInCache<Key, Cached> {
    fn new(key: Key, object: Cached, cache: &Cache<Key, Cached>) -> Self {
        Self {
            key,
            object,
            cache: cache.map.clone()
        }
    }
}

impl<Key:Eq+Hash+Clone, Cached> Drop for ObjectInCache<Key, Cached> {
    fn drop(&mut self) {
        if let Entry::Occupied(o) = self.cache.entry(self.key.clone()) {
            if o.get().strong_count() == 0 {
                o.remove_entry();
            }
        }
    }
}

impl<Key:Eq+Hash+Clone, Cached> Deref for ObjectInCache<Key, Cached> {
    type Target = Cached;

    fn deref(&self) -> &Self::Target {
        &self.object
    }
}
