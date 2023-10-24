#![allow(unused)]
use std::mem::MaybeUninit;

use lazy_init::Lazy;
use terminusdb_store_prolog::terminus_store::{store::sync::SyncStoreLayer, Layer};

use crate::consts::{SYS_ARRAY, SYS_VALUE};

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

macro_rules! generate_lookup_elem {
    (node, $layer:ident, $str:expr) => {
        SingleDictLookup::new_node($layer.clone(), $str)
    };
    (pred, $layer:ident, $str:expr) => {
        SingleDictLookup::new_predicate($layer.clone(), $str)
    };
}

macro_rules! generate_lookup_type {
    ($struct_name:ident {$($node_name:ident : $node_type:ident $node_str:expr),* $(,)?}) => {
        struct $struct_name<L: Layer+Clone> {
            $($node_name : SingleDictLookup<'static, L>),*,
        }

        impl<L: Layer+Clone> $struct_name<L> {
            fn new(layer: L) -> Self {
                Self {
                    $($node_name: generate_lookup_elem!($node_type, layer, $node_str)),*
                }
            }
            $(fn $node_name(&self) -> Option<u64> {
                self.$node_name.get()
            })*
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    // this is not really a test, but the build will fail if the macro is wrong
    generate_lookup_type! {
        Foo {
            array_type: node SYS_ARRAY,
            value_pred: pred SYS_VALUE,
        }
    }
}
