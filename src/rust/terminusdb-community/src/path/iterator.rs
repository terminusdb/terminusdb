use std::cell::RefCell;
use std::rc::Rc;

trait InnerClonableIterator<'a> {
    type Item;

    fn clone_boxed(&self) -> Box<dyn InnerClonableIterator<'a, Item = Self::Item> + 'a>;
    fn iter(&self) -> Box<dyn Iterator<Item = Self::Item> + 'a>;
    fn next_impl(&mut self) -> Option<Self::Item>;
}

pub struct ConcreteClonableIterator<'a, T, I: Iterator<Item = T> + Clone + 'a> {
    _a: std::marker::PhantomData<&'a bool>,
    iterator: I,
}

impl<'a, T: 'a, I: Iterator<Item = T> + Clone + 'a> InnerClonableIterator<'a>
    for ConcreteClonableIterator<'a, T, I>
{
    type Item = T;

    fn clone_boxed(&self) -> Box<dyn InnerClonableIterator<'a, Item = Self::Item> + 'a> {
        Box::new(Self {
            _a: Default::default(),
            iterator: self.iterator.clone(),
        })
    }

    fn iter(&self) -> Box<dyn Iterator<Item = Self::Item> + 'a> {
        Box::new(self.iterator.clone())
    }

    fn next_impl(&mut self) -> Option<T> {
        self.next()
    }
}

impl<'a, T: 'a, I: Iterator<Item = T> + Clone + 'a> From<I> for ConcreteClonableIterator<'a, T, I> {
    fn from(iterator: I) -> Self {
        ConcreteClonableIterator {
            _a: Default::default(),
            iterator,
        }
    }
}

impl<'a, T: 'a, I: Iterator<Item = T> + Clone + 'a> From<&'a I>
    for ConcreteClonableIterator<'a, T, I>
{
    fn from(iterator: &'a I) -> Self {
        ConcreteClonableIterator {
            _a: Default::default(),
            iterator: iterator.clone(),
        }
    }
}

impl<'a, T: 'a, I: Iterator<Item = T> + Clone> Iterator for ConcreteClonableIterator<'a, T, I> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.iterator.next()
    }
}

pub struct ClonableIterator<'a, T> {
    i: Box<dyn InnerClonableIterator<'a, Item = T> + 'a>,
}

impl<'a, T: 'a> ClonableIterator<'a, T> {
    pub fn new<I: Iterator<Item = T> + Clone + 'a>(iterator: I) -> Self {
        let concrete = ConcreteClonableIterator {
            _a: Default::default(),
            iterator,
        };

        Self {
            i: Box::new(concrete),
        }
    }
}

impl<'a, T> Clone for ClonableIterator<'a, T> {
    fn clone(&self) -> Self {
        Self {
            i: self.i.clone_boxed(),
        }
    }
}

impl<'a, T> Iterator for ClonableIterator<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.i.next_impl()
    }
}

struct CachedClonableIteratorState<'a, T: Clone> {
    i: Box<dyn Iterator<Item = T> + 'a>,
    cache: Vec<T>,
    done: bool,
}

impl<'a, T: Clone> CachedClonableIteratorState<'a, T> {
    fn next(&mut self, current_pos: usize) -> Option<T> {
        if self.done {
            return None;
        }

        if current_pos < self.cache.len() {
            println!("getting from cache");
            Some(self.cache[current_pos].clone())
        } else {
            if let Some(next) = self.i.next() {
                println!("getting fresh value");
                self.cache.push(next.clone());

                Some(next)
            } else {
                println!("done");
                self.done = true;
                None
            }
        }
    }
}

#[derive(Clone)]
pub struct CachedClonableIterator<'a, T: Clone + 'a> {
    pos: usize,
    state: Rc<RefCell<CachedClonableIteratorState<'a, T>>>,
}

impl<'a, T: Clone + 'a> CachedClonableIterator<'a, T> {
    pub fn new(i: impl Iterator<Item = T> + 'a) -> Self {
        let state = CachedClonableIteratorState {
            i: Box::new(i),
            cache: Vec::new(),
            done: false,
        };

        Self {
            pos: 0,
            state: Rc::new(RefCell::new(state)),
        }
    }
}

impl<'a, T: Clone + 'a> Iterator for CachedClonableIterator<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        let mut borrow = self.state.borrow_mut();
        if let Some(result) = borrow.next(self.pos) {
            self.pos += 1;
            Some(result)
        } else {
            None
        }
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
        assert_eq!(collected3, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn clone_and_resume() {
        let v1 = vec![3, 0, 5];
        let v2 = vec![1, 8, 6];

        let i1 = ClonableIterator::new(v1.into_iter());
        let i2 = ClonableIterator::new(v2.into_iter());

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

    #[test]
    fn cached_iterator() {
        let i = (0..5000).into_iter();

        let mut cached = CachedClonableIterator::new(i);
        assert_eq!(
            vec![0, 1, 2, 3, 4],
            cached.clone().take(5).collect::<Vec<_>>()
        );
        assert_eq!(0, cached.next().unwrap());

        let mut cached2 = cached.clone();
        let mut cached3 = cached.clone();
        assert_eq!(1, cached2.next().unwrap());
        assert_eq!(1, cached3.next().unwrap());
    }
}
