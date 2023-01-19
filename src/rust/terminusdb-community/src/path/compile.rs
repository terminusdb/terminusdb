use std::collections::HashSet;
use std::rc::Rc;

use itertools::Itertools;

use super::iterator::*;
use super::parse::*;

use crate::consts::RDF_TYPE;
use crate::graphql::frame::{AllFrames, Prefixes};
use crate::graphql::query::predicate_value_filter;
use crate::terminus_store::layer::*;
use crate::terminus_store::store::sync::SyncStoreLayer;

pub fn path_to_class<'a, 'b>(
    path_string: &'b str,
    g: &'a SyncStoreLayer,
    to_class: &'a str,
    all_frames: &'a AllFrames,
    zero_iter: ClonableIterator<'a, u64>,
) -> ClonableIterator<'a, u64> {
    let path = parse_path(path_string)
        .expect("Did not give a valid path")
        .1;
    let expanded_type_name = all_frames.fully_qualified_class_name(to_class);
    let iter = compile_path(g, all_frames.context.clone(), path, zero_iter);
    ClonableIterator::new(
        predicate_value_filter(g, RDF_TYPE, &ObjectType::Node(expanded_type_name), iter).dedup(),
    )
}

fn compile_path<'a>(
    g: &'a SyncStoreLayer,
    prefixes: Prefixes,
    path: Path,
    mut iter: ClonableIterator<'a, u64>,
) -> ClonableIterator<'a, u64> {
    match path {
        Path::Seq(vec) => {
            for sub_path in vec {
                iter = compile_path(g, prefixes.clone(), sub_path, iter);
            }
            iter
        }
        Path::Choice(vec) => {
            let branch = iter.clone();
            let result = vec
                .into_iter()
                .map(move |sub_path| compile_path(g, prefixes.clone(), sub_path, branch.clone()));
            ClonableIterator::new(result.flatten())
        }
        Path::Positive(p) => match p {
            Pred::Any => ClonableIterator::new(iter.flat_map(move |object| {
                CachedClonableIterator::new(g.triples_s(object).map(|t| t.object))
            })),
            Pred::Named(pred) => {
                let pred = prefixes.expand_schema(&pred);
                if let Some(p_id) = g.predicate_id(&pred) {
                    ClonableIterator::new(iter.flat_map(move |object| {
                        CachedClonableIterator::new(g.triples_sp(object, p_id).map(|t| t.object))
                    }))
                } else {
                    ClonableIterator::new(std::iter::empty())
                }
            }
        },
        Path::Negative(p) => match p {
            Pred::Any => ClonableIterator::new(iter.flat_map(move |object| {
                CachedClonableIterator::new(g.triples_o(object).map(|t| t.subject))
            })),
            Pred::Named(pred) => {
                let pred = prefixes.expand_schema(&pred);
                if let Some(p_id) = g.predicate_id(&pred) {
                    ClonableIterator::new(iter.flat_map(move |object| {
                        CachedClonableIterator::new(
                            g.triples_o(object)
                                .filter(move |t| t.predicate == p_id)
                                .map(|t| t.subject),
                        )
                    }))
                } else {
                    ClonableIterator::new(std::iter::empty())
                }
            }
        },
        Path::Plus(sub_path) => compile_many(g, prefixes, sub_path, iter, 1, None),
        Path::Star(sub_path) => compile_many(g, prefixes, sub_path, iter, 0, None),
        Path::Times(sub_path, n, m) => compile_many(g, prefixes, sub_path, iter, n, Some(m)),
    }
}

#[derive(Clone)]
struct ManySearchIterator<'a> {
    graph: &'a SyncStoreLayer,
    prefixes: Prefixes,
    start: usize,
    stop: Option<usize>,
    iterator: ClonableIterator<'a, u64>,
    current: usize,
    visited: HashSet<u64>,
    openset: Vec<u64>,
    pattern: Rc<Path>,
}

impl<'a> Iterator for ManySearchIterator<'a> {
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
                let next_elements = ClonableIterator::new(openset.into_iter());
                self.iterator = compile_path(
                    self.graph,
                    self.prefixes.clone(),
                    (*self.pattern).clone(),
                    next_elements,
                );
            }
        }
    }
}

fn compile_many<'a>(
    g: &'a SyncStoreLayer,
    prefixes: Prefixes,
    path: Rc<Path>,
    iterator: ClonableIterator<'a, u64>,
    start: usize,
    stop: Option<usize>,
) -> ClonableIterator<'a, u64> {
    if Some(0) == stop {
        iterator
    } else {
        ClonableIterator::new(ManySearchIterator {
            graph: g,
            prefixes,
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
    use terminusdb_store_prolog::terminus_store::open_sync_memory_store;

    use super::*;

    #[test]
    fn clonable() {
        let v = vec![1, 2, 3, 4, 5];
        let i = v.into_iter();
        let b: ClonableIterator<usize> = ClonableIterator::new(i);

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
        let b: ClonableIterator<usize> = ClonableIterator::new(i);

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
            .add_value_triple(ValueTriple::new_node(
                "http://base/a",
                "http://schema#b",
                "http://base/c",
            ))
            .unwrap();
        builder
            .add_value_triple(ValueTriple::new_node(
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
            metadata: None,
            extra_prefixes: Default::default(),
        };

        let p = parse_path("b*").unwrap().1;
        let id = layer.object_node_id("http://base/a").unwrap();
        let path_iter = compile_path(
            &layer,
            prefixes,
            p,
            ClonableIterator::new(vec![id].into_iter()),
        );

        let result: Vec<_> = path_iter
            .map(|object| layer.id_object_node(object))
            .flatten()
            .collect();

        assert_eq!(
            result,
            vec!["http://base/a", "http://base/c", "http://base/d"]
        );
    }

    #[test]
    fn cycle() {
        let store = open_sync_memory_store();
        let builder = store.create_base_layer().unwrap();
        builder
            .add_value_triple(ValueTriple::new_node(
                "http://base/a",
                "http://schema#b",
                "http://base/c",
            ))
            .unwrap();
        builder
            .add_value_triple(ValueTriple::new_node(
                "http://base/c",
                "http://schema#b",
                "http://base/d",
            ))
            .unwrap();
        builder
            .add_value_triple(ValueTriple::new_node(
                "http://base/d",
                "http://schema#b",
                "http://base/a",
            ))
            .unwrap();
        builder
            .add_value_triple(ValueTriple::new_node(
                "http://base/a",
                "http://schema#e",
                "http://base/z",
            ))
            .unwrap();

        let layer = builder.commit().unwrap();
        let prefixes = Prefixes {
            kind: "@context".to_string(),
            base: "http://base/".to_string(),
            schema: "http://schema#".to_string(),
            documentation: None,
            metadata: None,
            extra_prefixes: Default::default(),
        };

        let p = parse_path("b*,e").unwrap().1;
        let id = layer.object_node_id("http://base/a").unwrap();
        let path_iter = compile_path(
            &layer,
            prefixes,
            p,
            ClonableIterator::new(vec![id].into_iter()),
        );

        let result: Vec<_> = path_iter
            .map(|object| layer.id_object_node(object))
            .flatten()
            .collect();

        assert_eq!(result, vec!["http://base/z".to_string()]);
    }

    #[test]
    fn backwards() {
        let store = open_sync_memory_store();
        let builder = store.create_base_layer().unwrap();
        builder
            .add_value_triple(ValueTriple::new_node(
                "http://base/a",
                "http://schema#b",
                "http://base/c",
            ))
            .unwrap();
        builder
            .add_value_triple(ValueTriple::new_node(
                "http://base/o",
                "http://schema#e",
                "http://base/c",
            ))
            .unwrap();
        builder
            .add_value_triple(ValueTriple::new_node(
                "http://base/o",
                "http://schema#b",
                "http://base/q",
            ))
            .unwrap();
        builder
            .add_value_triple(ValueTriple::new_node(
                "http://base/q",
                "http://schema#b",
                "http://base/r",
            ))
            .unwrap();

        let layer = builder.commit().unwrap();
        let prefixes = Prefixes {
            kind: "@context".to_string(),
            base: "http://base/".to_string(),
            schema: "http://schema#".to_string(),
            documentation: None,
            metadata: None,
            extra_prefixes: Default::default(),
        };

        let p = parse_path("b,<e,b*").unwrap().1;
        let id = layer.object_node_id("http://base/a").unwrap();
        let path_iter = compile_path(
            &layer,
            prefixes,
            p,
            ClonableIterator::new(vec![id].into_iter()),
        );

        let result: Vec<_> = path_iter
            .map(|object| layer.id_object_node(object))
            .flatten()
            .collect();

        assert_eq!(
            result,
            vec![
                "http://base/o".to_string(),
                "http://base/q".to_string(),
                "http://base/r".to_string()
            ]
        );
    }
}
