trait ClonableIterator {
    type Item;

    fn clone_boxed(&self) -> Box<dyn ClonableIterator<Item=Self::Item>>;
    fn iter(&self) -> Box<dyn Iterator<Item=Self::Item>>;
    fn next_impl(&mut self) -> Option<Self::Item>;
}

struct ConcreteClonableIterator<T:'static,I:Iterator<Item=T>+Clone+'static> {
    iterator: I
}

impl<T:'static,I:Iterator<Item=T>+Clone+'static> ClonableIterator for ConcreteClonableIterator<T,I> {
    type Item = T;

    fn clone_boxed(&self) -> Box<dyn ClonableIterator<Item=Self::Item>> {
        Box::new(Self {
            iterator: self.iterator.clone()
        })
    }

    fn iter(&self) -> Box<dyn Iterator<Item=Self::Item>> {
        Box::new(self.iterator.clone())
    }

    fn next_impl(&mut self) -> Option<T> {
        self.next()
    }
}

impl<T:'static,I:Iterator<Item=T>+Clone+'static> From<I> for ConcreteClonableIterator<T,I> {
    fn from(iterator: I) -> Self {
        ConcreteClonableIterator { iterator }
    }
}

impl<'a, T:'static,I:Iterator<Item=T>+Clone+'static> From<&'a I> for ConcreteClonableIterator<T,I> {
    fn from(iterator: &'a I) -> Self {
        ConcreteClonableIterator { iterator: iterator.clone() }
    }
}

impl<T, I:Iterator<Item=T>+Clone> Iterator for ConcreteClonableIterator<T,I> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.iterator.next()
    }
}

struct WrappedClonableIterator<T> {
    i: Box<dyn ClonableIterator<Item=T>>
}

impl<T:'static> WrappedClonableIterator<T> {
    pub fn from<I:Iterator<Item=T>+Clone+'static>(iterator: I) -> Self {
        let concrete = ConcreteClonableIterator { iterator };

        Self {
            i: Box::new(concrete)
        }
    }
}

impl<T> Clone for WrappedClonableIterator<T> {
    fn clone(&self) -> Self {
        Self {
            i: self.i.clone_boxed()
        }
    }
}

impl<T> Iterator for WrappedClonableIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.i.next_impl()
    }
}

/*
struct ClonableFlatten<T> {
    iterators: Vec<Box<dyn Iterator<Item=T>+'static>>
}

impl<T> Clone for ClonableFlatten<T> {
    fn clone(&self) -> Self {
        Self {
            current: 
            iterators: self.iterators.iter().map(|i|i.clone_boxed()).collect()
        }
    }
}

impl<T> ClonableFlatten<T> {
    pub fn new(mut iterators: Vec<Box<dyn ClonableIterator<Item=T>>>) -> Self {
        iterators.reverse();

        Self { iterators }
    }
}

impl<T> Iterator for ClonableFlatten<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        while let Some(it) = self.iterators.last_mut() {
            if let Some(next) = it.next() {
                return Some(next);
            }
            else {
                self.iterators.pop();
            }
        }

        None
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blah() {
        let v = vec![1,2,3,4,5];
        let i = v.into_iter();
        let concrete: ConcreteClonableIterator<_,_> = i.into();
        let b: Box<dyn ClonableIterator<Item=usize>> = Box::new(concrete);

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

    #[test]
    fn blah2() {
        let v1 = vec![3,0,5];
        let v2 = vec![1,8,6];

        let i1 = WrappedClonableIterator::from(v1.into_iter());
        let i2 = WrappedClonableIterator::from(v2.into_iter());

        let ci1 = vec![i1.clone(),i2.clone()].into_iter().flatten();
        let ci2 = vec![i1.clone(),i2.clone()].into_iter().flatten();
        let ci3 = ci1.clone();
        let mut ci4 = ci1.clone();

        let r1: Vec<_> = ci1.collect();
        let r2: Vec<_> = ci2.collect();
        let r3: Vec<_> = ci3.collect();

        eprintln!("{r1:?}");
        eprintln!("{r2:?}");
        eprintln!("{r3:?}");


        ci4.next().unwrap();

        let ci5 = ci4.clone();
        let r5: Vec<_> = ci5.collect();
        eprintln!("{r5:?}");


        panic!("asdfads");
    }
}
