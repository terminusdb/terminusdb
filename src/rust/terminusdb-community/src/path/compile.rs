use std::collections::HashSet;
use std::rc::Rc;

use super::iterator::*;
use super::parse::*;

use crate::graphql::frame::Prefixes;
use crate::terminus_store::layer::*;
use crate::terminus_store::store::sync::SyncStoreLayer;

fn compile_path(
    g: &SyncStoreLayer,
    prefixes: Prefixes,
    path: Path,
    mut iter: ClonableIterator<u64>,
) -> ClonableIterator<u64> {
    match path {
        Path::Seq(vec) => {
            for sub_path in vec {
                iter = compile_path(g, prefixes.clone(), sub_path, iter);
            }
            iter
        }
        Path::Choice(vec) => {
            let branch = iter.clone();
            let g = g.clone();
            let result = vec
                .into_iter()
                .map(move |sub_path| compile_path(&g, prefixes.clone(), sub_path, branch.clone()));
            ClonableIterator::from(result.flatten())
        }
        Path::Positive(p) => match p {
            Pred::Any => {
                let g = g.clone();
                ClonableIterator::from(iter.flat_map(move |object| {
                    CachedClonableIterator::new(g.triples_s(object).map(|t| t.object))
                }))
            }
            Pred::Named(pred) => {
                let pred = prefixes.expand_schema(&pred);
                if let Some(p_id) = g.predicate_id(&pred) {
                    let g = g.clone();
                    ClonableIterator::from(iter.flat_map(move |object| {
                        CachedClonableIterator::new(g.triples_sp(object, p_id).map(|t| t.object))
                    }))
                } else {
                    ClonableIterator::from(std::iter::empty())
                }
            }
        },
        Path::Negative(p) => match p {
            Pred::Any => {
                let g = g.clone();
                ClonableIterator::from(iter.flat_map(move |object| {
                    CachedClonableIterator::new(g.triples_o(object).map(|t| t.subject))
                }))
            }
            Pred::Named(pred) => {
                let pred = prefixes.expand_schema(&pred);
                if let Some(p_id) = g.predicate_id(&pred) {
                    let g = g.clone();
                    ClonableIterator::from(iter.flat_map(move |object| {
                        CachedClonableIterator::new(
                            g.triples_o(object)
                                .filter(move |t| t.predicate == p_id)
                                .map(|t| t.subject),
                        )
                    }))
                } else {
                    ClonableIterator::from(std::iter::empty())
                }
            }
        },
        Path::Plus(sub_path) => compile_many(g, prefixes, sub_path, iter, 1, None),
        Path::Star(sub_path) => compile_many(g, prefixes, sub_path, iter, 0, None),
        Path::Times(sub_path, n, m) => compile_many(g, prefixes, sub_path, iter, n, Some(m)),
    }
}

#[derive(Clone)]
struct ManySearchIterator {
    graph: SyncStoreLayer,
    prefixes: Prefixes,
    start: usize,
    stop: Option<usize>,
    iterator: ClonableIterator<u64>,
    current: usize,
    visited: HashSet<u64>,
    openset: Vec<u64>,
    pattern: Rc<Path>,
}

impl Iterator for ManySearchIterator {
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        loop {
            if self.stop.map_or(false, |x| self.current >= x) {
                return None;
            }
            let result = self.iterator.next();
            if let Some(id) = result {
                if !self.visited.insert(id) {
                    continue;
                }

                self.openset.push(id);
                if self.current >= self.start {
                    return Some(id);
                }
            } else if self.openset.is_empty() {
                return None;
            } else {
                self.current += 1;
                let mut openset = Vec::new();
                std::mem::swap(&mut openset, &mut self.openset);
                let next_elements = ClonableIterator::from(openset.into_iter());
                self.iterator = compile_path(
                    &self.graph,
                    self.prefixes.clone(),
                    (*self.pattern).clone(),
                    next_elements,
                );
            }
        }
    }
}

fn compile_many(
    g: &SyncStoreLayer,
    prefixes: Prefixes,
    path: Rc<Path>,
    iterator: ClonableIterator<u64>,
    start: usize,
    stop: Option<usize>,
) -> ClonableIterator<u64> {
    if Some(0) == stop {
        return iterator;
    } else {
        ClonableIterator::from(ManySearchIterator {
            graph: g.clone(),
            prefixes: prefixes.clone(),
            start,
            stop,
            current: 0,
            iterator,
            visited: HashSet::new(),
            openset: Vec::new(),
            pattern: path,
        })
    }
}

#[cfg(test)]
mod tests {
    use terminusdb_store_prolog::terminus_store::{open_memory_store, open_sync_memory_store};

    use super::*;

    #[test]
    fn clonable() {
        let v = vec![1, 2, 3, 4, 5];
        let i = v.into_iter();
        let b: ClonableIterator<usize> = ClonableIterator::from(i);

        let b2 = b.clone();
        let b3 = b2.clone();

        let collected: Vec<_> = b.collect();
        assert_eq!(collected, vec![1, 2, 3, 4, 5]);
        let collected2: Vec<_> = b2.collect();
        assert_eq!(collected2, vec![1, 2, 3, 4, 5]);
        let collected3: Vec<_> = b3.collect();
        assert_eq!(collected3, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn flatten() {
        let v = vec![1, 2, 3, 4, 5];
        let i = v.into_iter();
        let b: ClonableIterator<usize> = ClonableIterator::from(i);

        let my_iter = b.flat_map(|x| {
            let mut vec = Vec::new();
            let mut i = x;
            while i > 0 {
                vec.push(i);
                i -= 1;
            }
            vec.into_iter()
        });

        let res: Vec<_> = my_iter.collect();
        assert_eq!(vec![1, 2, 1, 3, 2, 1, 4, 3, 2, 1, 5, 4, 3, 2, 1], res);
    }

    #[test]
    fn asdfasd() {
        let store = open_sync_memory_store();
        let builder = store.create_base_layer().unwrap();
        builder
            .add_string_triple(StringTriple::new_node(
                "http://base/a",
                "http://schema#b",
                "http://base/c",
            ))
            .unwrap();
        builder
            .add_string_triple(StringTriple::new_node(
                "http://base/c",
                "http://schema#b",
                "http://base/d",
            ))
            .unwrap();

        let layer = builder.commit().unwrap();
        let prefixes = Prefixes {
            kind: "@context".to_string(),
            base: "http://base/".to_string(),
            schema: "http://schema#".to_string(),
            documentation: None,
            extra_prefixes: Default::default(),
        };

        let p = path("b*").unwrap().1;
        let id = layer.object_node_id("http://base/a").unwrap();
        let path_iter = compile_path(
            &layer,
            prefixes,
            p,
            ClonableIterator::from(vec![id].into_iter()),
        );

        let result: Vec<_> = path_iter.collect();

        panic!("{result:?}");
    }
}
