use crate::builder::*;
use crate::layer::*;
use crate::named_graph::*;
use std::io::{self, Cursor};
use swipl::prelude::*;
use terminus_store::storage::archive::DirectoryArchiveBackend;
use terminus_store::storage::archive::LruArchiveBackend;
use terminus_store::storage::archive::LruMetadataArchiveBackend;
use terminus_store::storage::CachedLayerStore;
use terminus_store::storage::LockingHashMapLayerCache;
use terminus_store::storage::{
    archive::ArchiveLayerStore, name_to_string, pack_layer_parents, string_to_name, PackError,
};
use terminus_store::store::{sync::*, Store};

use terminusdb_grpc_labelstore_client::GrpcLabelStore;

predicates! {
    pub semidet fn open_memory_store(_context, term) {
        let store = open_sync_memory_store();
        term.unify(&WrappedStore(store))
    }

    pub semidet fn open_directory_store(_context, dir_term, out_term) {
        let dir: PrologText = dir_term.get_ex()?;
        let store = open_sync_directory_store(&*dir);
        out_term.unify(&WrappedStore(store))
    }

    pub semidet fn open_raw_archive_store(_context, dir_term, out_term) {
        let dir: PrologText = dir_term.get_ex()?;
        let store = open_sync_raw_archive_store(&*dir);
        out_term.unify(&WrappedStore(store))
    }

    pub semidet fn open_archive_store(_context, dir_term, cache_size_term, out_term) {
        let dir: PrologText = dir_term.get_ex()?;
        let cache_size: usize = cache_size_term.get_ex::<u64>()? as usize;
        let store = open_sync_archive_store(&*dir, cache_size);
        out_term.unify(&WrappedStore(store))
    }

    pub semidet fn open_grpc_store(context, dir_term, address_term, initial_pool_term, cache_size_term, out_term) {
        let dir: PrologText = dir_term.get_ex()?;
        let address: PrologText = address_term.get_ex()?;
        let pool_size: u64 = initial_pool_term.get_ex()?;
        let cache_size: usize = cache_size_term.get_ex::<u64>()? as usize;
        let directory_layer_backend = DirectoryArchiveBackend::new((&*dir).into());
        let layer_backend = LruArchiveBackend::new(directory_layer_backend.clone(), cache_size);
        let layer_metadata_backend = LruMetadataArchiveBackend::new(directory_layer_backend, layer_backend.clone());
        let layer_store = CachedLayerStore::new(ArchiveLayerStore::new(layer_metadata_backend, layer_backend), LockingHashMapLayerCache::new());

        let label_store = context.try_or_die_generic(task_sync(GrpcLabelStore::new(address.to_string(), pool_size as usize)))?;

        let store = SyncStore::wrap(Store::new(label_store, layer_store));

        out_term.unify(&WrappedStore(store))
    }

    pub semidet fn open_write(context, store_or_graph_or_layer_term, builder_term) {
        let builder;
        if let Some(store) = attempt_opt(store_or_graph_or_layer_term.get::<WrappedStore>())? {
            builder = context.try_or_die(store.create_base_layer())?;
        }
        else if let Some(graph) = attempt_opt(store_or_graph_or_layer_term.get::<WrappedNamedGraph>())? {
            if let Some(layer) = context.try_or_die(graph.head())? {
                builder = context.try_or_die(layer.open_write())?;
            }
            else {
                return context.raise_exception(&term!{context: error(cannot_open_named_graph_without_base_layer, _)}?);
            }
        }
        else {
            let layer: WrappedLayer = store_or_graph_or_layer_term.get_ex()?;
            builder = context.try_or_die(layer.open_write())?;
        }

        builder_term.unify(WrappedBuilder(builder))
    }

    pub semidet fn pack_export(context, store_term, layer_ids_term, pack_term) {
        let store: WrappedStore = store_term.get_ex()?;
        let layer_id_strings_list: Vec<String> = layer_ids_term.get_ex()?;
        let mut layer_ids_list = Vec::with_capacity(layer_id_strings_list.len());
        for layer_id_string in layer_id_strings_list {
            let layer_id = context.try_or_die(string_to_name(&layer_id_string))?;
            layer_ids_list.push(layer_id);
        }

        let result = context.try_or_die(store.export_layers(
            Box::new(layer_ids_list.into_iter())))?;

        pack_term.unify(result.as_slice())
    }

    pub semidet fn pack_layerids_and_parents(context, pack_term, layer_parents_term) {
        let pack: Vec<u8> = pack_term.get_ex()?;
        let layer_parent_map = context.try_or_die(pack_layer_parents(Cursor::new(pack))
                                                  .map_err(|e| {
                                                      // todo we're mapping to io error here for ease but should be something better
                                                      match e {
                                                          PackError::Io(e) => e,
                                                          PackError::LayerNotFound => io::Error::new(io::ErrorKind::NotFound, "a layer from the pack was not found"),
                                                          PackError::Utf8Error(e) => io::Error::new(io::ErrorKind::InvalidData, format!("{:?}", e))
                                                      }
                                                  }))?;

        let pair_functor = Functor::new("-", 2);
        let none_atom = Atom::new("none");
        let some_functor = Functor::new("some", 1);

        let mut result_terms = Vec::with_capacity(layer_parent_map.len());
        for (layer, parent) in layer_parent_map {
            let term = context.new_term_ref();
            term.unify(pair_functor)?;
            term.unify_arg(1, name_to_string(layer))?;
            match parent {
                Some(parent) => {
                    let parent_term = context.new_term_ref();
                    parent_term.unify(some_functor)?;
                    parent_term.unify_arg(1, name_to_string(parent))?;
                    term.unify_arg(2, &parent_term)?;
                },
                None => {
                    term.unify_arg(2, &none_atom)?;
                }
            }

            result_terms.push(term);
        }

        layer_parents_term.unify(result_terms.as_slice())
    }

    pub semidet fn pack_import(context, store_term, layer_ids_term, pack_term) {
        let store: WrappedStore = store_term.get_ex()?;

        let layer_id_strings: Vec<String> = layer_ids_term.get_ex()?;
        let mut layer_ids = Vec::with_capacity(layer_id_strings.len());
        for layer_id_string in layer_id_strings {
            let name = context.try_or_die(string_to_name(&layer_id_string))?;
            layer_ids.push(name);
        }

        let pack: Vec<u8> = pack_term.get_ex()?;

        context.try_or_die(store.import_layers(pack.as_slice(), Box::new(layer_ids.into_iter())))
    }

    pub semidet fn merge_base_layers(context, store_term, layer_ids_term, output_id_term) {
        let store: WrappedStore = store_term.get_ex()?;

        let layer_id_strings: Vec<String> = layer_ids_term.get_ex()?;
        let mut layer_ids = Vec::with_capacity(layer_id_strings.len());
        for layer_id_string in layer_id_strings {
            let name = context.try_or_die(string_to_name(&layer_id_string))?;
            layer_ids.push(name);
        }

        let result = context.try_or_die(store.merge_base_layers(&layer_ids))?;
        let result_string = name_to_string(result);

        output_id_term.unify(result_string)
    }
}

wrapped_clone_blob!("store", pub WrappedStore, SyncStore, defaults);
