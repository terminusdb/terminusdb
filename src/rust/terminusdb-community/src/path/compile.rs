use super::iterator::*;
use super::parse::*;

use crate::graphql::frame::Prefixes;
use crate::terminus_store::layer::*;
use crate::terminus_store::store::sync::SyncStoreLayer;

fn compile_path(
    g: &SyncStoreLayer,
    prefixes: &Prefixes,
    path: Path,
    iter: ClonableIterator<IdTriple>,
) -> ClonableIterator<IdTriple> {
    let mut iter = iter;
    match path {
        Path::Seq(vec) => {
            for sub_path in vec {
                iter = compile_path(g, prefixes, sub_path, iter)
            }
            iter
        }
        Path::Choice(vec) => {
            let branch = iter.clone();
            let result = vec
                .into_iter()
                .map(move |sub_path| compile_path(g, prefixes, sub_path, branch.clone()));
            ClonableIterator::from(result.flatten())
        }
        Path::Positive(p) => match p {
            Pred::Any => ClonableIterator::from(iter.flat_map(|t| g.triples_s(t.object))),
            Pred::Named(pred) => {
                let pred = prefixes.expand_schema(&pred);
                if let Some(p_id) = g.predicate_id(&pred) {
                    ClonableIterator::from(iter.flat_map(|t| g.triples_sp(t.object, p_id)))
                } else {
                    ClonableIterator::from(std::iter::empty())
                }
            }
        },
        Path::Negative(p) => match p {
            Pred::Any => iter.flat_map(|t| g.triples_o(t.object)),
            Pred::Named(pred) => {
                let pred = prefixes.expand_schema(&pred);
                if let Some(p_id) = g.predicate_id(&pred) {
                    iter.flat_map(|t| g.triples_o(t.object).filter(|t| t.p == p_id))
                } else {
                    ClonableIterator::from(std::iter::empty())
                }
            }
        },
        Path::Plus(_) => todo!(),
        Path::Star(_) => todo!(),
        Path::Times(_, _, _) => todo!(),
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
            let vec = Vec::new();
            let mut i = x;
            while i > 0 {
                vec.push(i);
                i = -1;
            }
            vec.iter()
        });

        let res: Vec<_> = my_iter.collect();
        eprintln!("{res:?}");
    }
}
