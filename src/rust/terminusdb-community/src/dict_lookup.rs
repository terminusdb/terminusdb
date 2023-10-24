#![allow(unused)]
use std::mem::MaybeUninit;

use lazy_init::Lazy;
use terminusdb_store_prolog::terminus_store::Layer;

pub enum WhichDict {
    Node,
    Predicate,
}

pub struct SingleDictLookup<'a, L: Layer> {
    layer: L,
    s: &'a str,
    which: WhichDict,
    result: Lazy<Option<u64>>,
}

impl<'a, L: Layer> SingleDictLookup<'a, L> {
    pub fn new(layer: L, s: &'a str, which: WhichDict) -> Self {
        Self {
            layer,
            s,
            which,
            result: Lazy::new(),
        }
    }

    pub fn new_node(layer: L, s: &'a str) -> Self {
        Self::new(layer, s, WhichDict::Node)
    }

    pub fn new_predicate(layer: L, s: &'a str) -> Self {
        Self::new(layer, s, WhichDict::Predicate)
    }

    fn lookup(&self) -> Option<u64> {
        match self.which {
            WhichDict::Node => self.layer.subject_id(self.s),
            WhichDict::Predicate => self.layer.predicate_id(self.s),
        }
    }

    pub fn get(&self) -> Option<u64> {
        self.result
            .get_or_create(|| self.lookup())
            .as_ref()
            .copied()
    }
}

#[allow(clippy::forget_non_drop)]
pub fn lookup_nodes<'a, L: Layer + Clone, const N: usize>(
    layer: L,
    strings: [&'a str; N],
) -> [SingleDictLookup<'a, L>; N] {
    let mut result: [MaybeUninit<SingleDictLookup<'a, L>>; N] =
        unsafe { MaybeUninit::uninit().assume_init() };

    for i in 0..strings.len() {
        result[i].write(SingleDictLookup::new_node(layer.clone(), strings[i]));
    }

    // It would be nicer if we could do a transmute here, as
    // transmute ensures that the conversion converts between
    // types of the same size, but it seems like this doesn't work
    // yet with const generic arrays. We do a pointer cast
    // instead.
    let magic = result.as_ptr() as *const [SingleDictLookup<'a, L>; N];
    std::mem::forget(result);

    unsafe { magic.read() }
}

#[allow(clippy::forget_non_drop)]
pub fn lookup_predicates<'a, L: Layer + Clone, const N: usize>(
    layer: L,
    strings: [&'a str; N],
) -> [SingleDictLookup<'a, L>; N] {
    let mut result: [MaybeUninit<SingleDictLookup<'a, L>>; N] =
        unsafe { MaybeUninit::uninit().assume_init() };

    for i in 0..strings.len() {
        result[i].write(SingleDictLookup::new_predicate(layer.clone(), strings[i]));
    }

    // It would be nicer if we could do a transmute here, as
    // transmute ensures that the conversion converts between
    // types of the same size, but it seems like this doesn't work
    // yet with const generic arrays. We do a pointer cast
    // instead.
    let magic = result.as_ptr() as *const [SingleDictLookup<'a, L>; N];
    std::mem::forget(result);

    unsafe { magic.read() }
}
