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
    mut iter: ClonableIterator<IdTriple>,
) -> ClonableIterator<IdTriple> {
    match path {
        Path::Seq(vec) => {
            for sub_path in vec {
                iter = compile_path(g, prefixes.clone(), sub_path, iter)
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
                ClonableIterator::from(
                    iter.flat_map(move |t| CachedClonableIterator::new(g.triples_s(t.object))),
                )
            }
            Pred::Named(pred) => {
                let pred = prefixes.expand_schema(&pred);
                if let Some(p_id) = g.predicate_id(&pred) {
                    let g = g.clone();
                    ClonableIterator::from(iter.flat_map(move |t| {
                        CachedClonableIterator::new(g.triples_sp(t.object, p_id))
                    }))
                } else {
                    ClonableIterator::from(std::iter::empty())
                }
            }
        },
        Path::Negative(p) => match p {
            Pred::Any => {
                let g = g.clone();
                ClonableIterator::from(
                    iter.flat_map(move |t| CachedClonableIterator::new(g.triples_o(t.object))),
                )
            }
            Pred::Named(pred) => {
                let pred = prefixes.expand_schema(&pred);
                if let Some(p_id) = g.predicate_id(&pred) {
                    let g = g.clone();
                    ClonableIterator::from(iter.flat_map(move |t| {
                        CachedClonableIterator::new(
                            g.triples_o(t.object).filter(move |t| t.predicate == p_id),
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
    iterator: ClonableIterator<IdTriple>,
    current: usize,
    visited: HashSet<IdTriple>,
    openset: Vec<IdTriple>,
    pattern: Rc<Path>,
}

impl Iterator for ManySearchIterator {
    type Item = IdTriple;

    fn next(&mut self) -> Option<IdTriple> {
        loop {
            if self.stop.map_or(false, |x| self.current >= x) {
                return None;
            }
            let result = self.iterator.next();
            if let Some(idtriple) = result {
                if !self.visited.insert(idtriple) {
                    continue;
                }

                self.openset.push(idtriple);
                if self.current >= self.start {
                    return Some(idtriple);
                }
            }else{
                self.current += 1;
                let mut openset = Vec::new();
                std::mem::swap(&mut openset, &mut self.openset);
                let next_elements = ClonableIterator::from(openset.into_iter());
                self.iterator = compile_path(&self.graph, self.prefixes.clone(), (*self.pattern).clone(), next_elements);
            }
        }
    }
}

fn compile_many(
    g: &SyncStoreLayer,
    prefixes: Prefixes,
    path: Rc<Path>,
    iterator: ClonableIterator<IdTriple>,
    start: usize,
    stop: Option<usize>,
) -> ClonableIterator<IdTriple> {
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
}
