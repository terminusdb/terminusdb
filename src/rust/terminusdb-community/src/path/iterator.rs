trait InnerClonableIterator {
    type Item;

    fn clone_boxed(&self) -> Box<dyn InnerClonableIterator<Item = Self::Item>>;
    fn iter(&self) -> Box<dyn Iterator<Item = Self::Item>>;
    fn next_impl(&mut self) -> Option<Self::Item>;
}

pub struct ConcreteClonableIterator<T: 'static, I: Iterator<Item = T> + Clone + 'static> {
    iterator: I,
}

impl<T: 'static, I: Iterator<Item = T> + Clone + 'static> InnerClonableIterator
    for ConcreteClonableIterator<T, I>
{
    type Item = T;

    fn clone_boxed(&self) -> Box<dyn InnerClonableIterator<Item = Self::Item>> {
        Box::new(Self {
            iterator: self.iterator.clone(),
        })
    }

    fn iter(&self) -> Box<dyn Iterator<Item = Self::Item>> {
        Box::new(self.iterator.clone())
    }

    fn next_impl(&mut self) -> Option<T> {
        self.next()
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

impl<T, I: Iterator<Item = T> + Clone> Iterator for ConcreteClonableIterator<T, I> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.iterator.next()
    }
}

pub struct ClonableIterator<T> {
    i: Box<dyn InnerClonableIterator<Item = T>>,
}

impl<T: 'static> ClonableIterator<T> {
    pub fn from<I: Iterator<Item = T> + Clone + 'static>(iterator: I) -> Self {
        let concrete = ConcreteClonableIterator { iterator };

        Self {
            i: Box::new(concrete),
        }
    }
}

impl<T> Clone for ClonableIterator<T> {
    fn clone(&self) -> Self {
        Self {
            i: self.i.clone_boxed(),
        }
    }
}

impl<T> Iterator for ClonableIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.i.next_impl()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clone_it() {
        let v = vec![1, 2, 3, 4, 5];
        let i = v.into_iter();
        let concrete: ConcreteClonableIterator<_, _> = i.into();
        let b: Box<dyn InnerClonableIterator<Item = usize>> = Box::new(concrete);

        let b2 = b.clone_boxed();

        let collected: Vec<_> = b2.iter().collect();
        assert_eq!(collected, vec![1, 2, 3, 4, 5]);
        let collected2: Vec<_> = b2.iter().collect();
        assert_eq!(collected2, vec![1, 2, 3, 4, 5]);

        let b3 = b2.clone_boxed();
        let collected3: Vec<_> = b3.iter().collect();
        assert_eq!(collected, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn clone_and_resume() {
        let v1 = vec![3, 0, 5];
        let v2 = vec![1, 8, 6];

        let i1 = ClonableIterator::from(v1.into_iter());
        let i2 = ClonableIterator::from(v2.into_iter());

        let ci1 = vec![i1.clone(), i2.clone()].into_iter().flatten();
        let ci2 = vec![i1.clone(), i2.clone()].into_iter().flatten();
        let ci3 = ci1.clone();
        let mut ci4 = ci1.clone();

        let r1: Vec<_> = ci1.collect();
        let r2: Vec<_> = ci2.collect();
        let r3: Vec<_> = ci3.collect();
        assert_eq!(r1, vec![3, 0, 5, 1, 8, 6]);
        assert_eq!(r2, vec![3, 0, 5, 1, 8, 6]);
        assert_eq!(r3, vec![3, 0, 5, 1, 8, 6]);

        ci4.next().unwrap();

        let ci5 = ci4.clone();
        let r5: Vec<_> = ci5.collect();
        assert_eq!(r5, vec![0, 5, 1, 8, 6]);
    }
}
