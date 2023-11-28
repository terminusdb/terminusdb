use crate::swipl::prelude::*;
use terminusdb_store_prolog::{
    builder::WrappedBuilder,
    layer::*,
    terminus_store::store::sync::{SyncStoreLayer, SyncStoreLayerBuilder},
};

use crate::swipl::atom;

pub fn transaction_instance_layer<C: QueryableContextType>(
    context: &Context<C>,
    transaction_term: &Term,
) -> PrologResult<Option<SyncStoreLayer>> {
    let instance_atom = atom!("instance_objects");
    let read_atom = atom!("read");

    let frame = context.open_frame();
    let list_term = frame.new_term_ref();
    transaction_term.get_dict_key_term(&instance_atom, &list_term)?;

    if let Some(item) = frame.term_list_iter(&list_term).next() {
        let layer: Option<WrappedLayer> = attempt_opt(item.get_dict_key(&read_atom))?;
        Ok(layer.map(|l| l.0))
    } else {
        Ok(None)
    }
}

pub fn transaction_schema_layer<C: QueryableContextType>(
    context: &Context<C>,
    transaction_term: &Term,
) -> PrologResult<Option<SyncStoreLayer>> {
    let schema_atom = atom!("schema_objects");
    let read_atom = atom!("read");

    let frame = context.open_frame();
    let list_term = frame.new_term_ref();
    transaction_term.get_dict_key_term(&schema_atom, &list_term)?;

    if let Some(item) = frame.term_list_iter(&list_term).next() {
        let layer: Option<WrappedLayer> = attempt_opt(item.get_dict_key(&read_atom))?;
        Ok(layer.map(|l| l.0))
    } else {
        Ok(None)
    }
}

pub fn transaction_instance_builder<C: QueryableContextType>(
    context: &Context<C>,
    transaction_term: &Term,
) -> PrologResult<Option<SyncStoreLayerBuilder>> {
    let instance_atom = atom!("instance_objects");
    let write_atom = atom!("write");

    let frame = context.open_frame();
    let list_term = frame.new_term_ref();
    transaction_term.get_dict_key_term(&instance_atom, &list_term)?;

    if let Some(item) = frame.term_list_iter(&list_term).next() {
        let layer: Option<WrappedBuilder> = attempt_opt(item.get_dict_key(&write_atom))?;
        Ok(layer.map(|l| l.0))
    } else {
        Ok(None)
    }
}
