use crate::builder::*;
use crate::layer::*;
use crate::named_graph::*;
use std::io::{self, Cursor};
use std::path::PathBuf;
use swipl::prelude::*;
use terminus_store::storage::archive::DirectoryArchiveBackend;
use terminus_store::storage::archive::LruArchiveBackend;
use terminus_store::storage::CachedLayerStore;
use terminus_store::storage::LockingHashMapLayerCache;
use terminus_store::storage::{
    archive::ArchiveLayerStore, name_to_string, pack_layer_parents, string_to_name, PackError,
};
use terminus_store::store::{sync::*, Store};

use terminusdb_grpc_labelstore_client::GrpcLabelStore;

/// Get the current process resident set size (RSS) in bytes.
/// Returns None if the value cannot be determined on this platform.
#[cfg(target_os = "macos")]
fn get_process_rss_bytes() -> Option<usize> {
    #[repr(C)]
    struct MachTaskBasicInfo {
        suspend_count: i32,
        virtual_size: u64,
        resident_size: u64,
        user_time: [u32; 2],
        system_time: [u32; 2],
        policy: i32,
    }

    const MACH_TASK_BASIC_INFO: u32 = 20;
    const MACH_TASK_BASIC_INFO_COUNT: u32 =
        (std::mem::size_of::<MachTaskBasicInfo>() / std::mem::size_of::<u32>()) as u32;

    extern "C" {
        fn mach_task_self() -> u32;
        fn task_info(
            target_task: u32,
            flavor: u32,
            task_info_out: *mut MachTaskBasicInfo,
            task_info_count: *mut u32,
        ) -> i32;
    }

    unsafe {
        let mut info: MachTaskBasicInfo = std::mem::zeroed();
        let mut count = MACH_TASK_BASIC_INFO_COUNT;
        let result = task_info(mach_task_self(), MACH_TASK_BASIC_INFO, &mut info, &mut count);
        if result == 0 {
            Some(info.resident_size as usize)
        } else {
            None
        }
    }
}

/// Get the current process resident set size (RSS) in bytes.
/// Returns None if the value cannot be determined on this platform.
#[cfg(target_os = "linux")]
fn get_process_rss_bytes() -> Option<usize> {
    let status = std::fs::read_to_string("/proc/self/status").ok()?;
    for line in status.lines() {
        if line.starts_with("VmRSS:") {
            let kb_str = line.split_whitespace().nth(1)?;
            let kb: usize = kb_str.parse().ok()?;
            return Some(kb * 1024);
        }
    }
    None
}

/// Get the current process resident set size (RSS) in bytes.
/// Returns None on unsupported platforms.
#[cfg(not(any(target_os = "macos", target_os = "linux")))]
fn get_process_rss_bytes() -> Option<usize> {
    None
}

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
        let layer_backend = LruArchiveBackend::new(directory_layer_backend.clone(), directory_layer_backend, cache_size);
        let lru_ref = layer_backend.clone();
        let layer_store = CachedLayerStore::new(ArchiveLayerStore::new(layer_backend.clone(), layer_backend), LockingHashMapLayerCache::new());

        let label_store = context.try_or_die_generic(task_sync(GrpcLabelStore::new(address.to_string(), pool_size as usize)))?;

        let store = SyncStore::wrap(Store::new(label_store, layer_store).with_lru_used_bytes(move || lru_ref.used_bytes()));

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

    pub semidet fn merge_base_layers(context, store_term, temp_dir_term, layer_ids_term, output_id_term) {
        let store: WrappedStore = store_term.get_ex()?;
        let temp_dir: PrologText = temp_dir_term.get_ex()?;
        let temp_dir_path: PathBuf = (&*temp_dir).into();

        let layer_id_strings: Vec<String> = layer_ids_term.get_ex()?;
        let mut layer_ids = Vec::with_capacity(layer_id_strings.len());
        for layer_id_string in layer_id_strings {
            let name = context.try_or_die(string_to_name(&layer_id_string))?;
            layer_ids.push(name);
        }

        let result = context.try_or_die(store.merge_base_layers(&layer_ids, &temp_dir_path))?;
        let result_string = name_to_string(result);

        output_id_term.unify(result_string)
    }

    /// Get layer cache statistics: (total_entries, live_entries, dead_entries)
    /// Dead entries are stale weak references that should be cleaned up.
    pub semidet fn layer_cache_stats(_context, store_term, total_term, live_term, dead_term) {
        let store: WrappedStore = store_term.get_ex()?;
        let (total, live, dead) = store.layer_cache_stats();
        total_term.unify(total as u64)?;
        live_term.unify(live as u64)?;
        dead_term.unify(dead as u64)
    }

    /// Get bytes of backing data for layer cache entries: (total, live, dead).
    pub semidet fn layer_cache_memory_bytes(_context, store_term, total_term, live_term, dead_term) {
        let store: WrappedStore = store_term.get_ex()?;
        let (total, live, dead) = store.layer_cache_memory_bytes();
        total_term.unify(total as u64)?;
        live_term.unify(live as u64)?;
        dead_term.unify(dead as u64)
    }

    /// Get current LRU archive cache usage in bytes.
    /// Fails if no LRU backend is configured (e.g. memory store).
    pub semidet fn lru_cache_used_bytes(_context, store_term, bytes_term) {
        let store: WrappedStore = store_term.get_ex()?;
        match store.lru_cache_used_bytes() {
            Some(bytes) => bytes_term.unify(bytes as u64),
            None => Err(PrologError::Failure),
        }
    }

    /// Remove stale (dead) weak references from the layer cache.
    /// Returns the number of entries removed.
    pub semidet fn cleanup_layer_cache(_context, store_term, removed_term) {
        let store: WrappedStore = store_term.get_ex()?;
        let removed = store.cleanup_layer_cache();
        removed_term.unify(removed as u64)
    }

    /// Invalidate a specific layer from the cache, forcing reload from disk on next access.
    /// This is useful after rollup to ensure the rolled-up version is loaded.
    pub semidet fn invalidate_layer_cache_entry(context, store_term, layer_id_term) {
        let store: WrappedStore = store_term.get_ex()?;
        let layer_id_string: PrologText = layer_id_term.get_ex()?;
        let layer_id = context.try_or_die(string_to_name(&layer_id_string))?;
        store.invalidate_layer(layer_id);
        Ok(())
    }

    /// Get the current process resident set size (RSS) in bytes.
    /// Fails if the value cannot be determined on this platform.
    pub semidet fn process_rss_bytes(_context, bytes_term) {
        match get_process_rss_bytes() {
            Some(bytes) => bytes_term.unify(bytes as u64),
            None => Err(PrologError::Failure),
        }
    }
}

wrapped_clone_blob!("store", pub WrappedStore, SyncStore, defaults);
