use super::parse::*;

use terminusdb_store_prolog::terminus_store::IdTriple;

trait ClonableIterator {
    type Item;

    fn clone_boxed(&self) -> Box<dyn ClonableIterator<Item = Self::Item>>;
    fn iter(&self) -> Box<dyn Iterator<Item = Self::Item>>;
}

struct ConcreteClonableIterator<T: 'static, I: Iterator<Item = T> + Clone + 'static> {
    iterator: I,
}

impl<T: 'static, I: Iterator<Item = T> + Clone + 'static> ClonableIterator
    for ConcreteClonableIterator<T, I>
{
    type Item = T;

    fn clone_boxed(&self) -> Box<dyn ClonableIterator<Item = Self::Item>> {
        Box::new(Self {
            iterator: self.iterator.clone(),
        })
    }

    fn iter(&self) -> Box<dyn Iterator<Item = Self::Item>> {
        Box::new(self.iterator.clone())
    }
}

impl<T: 'static, I: Iterator<Item = T> + Clone + 'static> From<I>
    for ConcreteClonableIterator<T, I>
{
    fn from(iterator: I) -> Self {
        ConcreteClonableIterator { iterator }
    }
}

impl<'a, T: 'static, I: Iterator<Item = T> + Clone + 'static> From<&'a I>
    for ConcreteClonableIterator<T, I>
{
    fn from(iterator: &'a I) -> Self {
        ConcreteClonableIterator {
            iterator: iterator.clone(),
        }
    }
}

// i: m a -> f: (a -> m b) -> (m a -> m b)

//

fn compile_path(
    path: Path,
    iter: Box<dyn ClonableIterator<Item = IdTriple>>,
) -> Box<dyn ClonableIterator<Item = IdTriple>> {
    let mut iter = iter;
    match path {
        Path::Seq(vec) => {
            for sub_path in vec {
                iter = compile_path(sub_path, iter)
            }
            iter
        }
        Path::Choice(vec) => {
            let branch = iter.clone_boxed();
            let result = vec
                .into_iter()
                .map(move |sub_path| compile_path(sub_path, branch.clone_boxed()));
            Box::new(result.flatten().into())
        }
        Path::Positive(p) => todo!(),
        Path::Negative(_) => todo!(),
        Path::Plus(_) => todo!(),
        Path::Star(_) => todo!(),
        Path::Times(_, _, _) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blah() {
        let v = vec![1, 2, 3, 4, 5];
        let i = v.into_iter();
        let concrete: ConcreteClonableIterator<_, _> = i.into();
        let b: Box<dyn ClonableIterator<Item = usize>> = Box::new(concrete);

        let b2 = b.clone_boxed();

        let collected: Vec<_> = b2.iter().collect();
        eprintln!("{:?}", collected);
        let collected2: Vec<_> = b2.iter().collect();
        eprintln!("{:?}", collected2);

        let b3 = b2.clone_boxed();
        let collected3: Vec<_> = b3.iter().collect();
        eprintln!("{:?}", collected3);

        panic!("asdfas");
    }
}
