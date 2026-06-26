use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::terminus_store::layer::*;
use crate::terminus_store::storage::name_to_string;
use lazy_static::lazy_static;
use swipl::prelude::*;
use terminusdb_store_prolog::layer::WrappedLayer;
use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;

use crate::consts::SysIds;
use super::DocumentContext;

const XSD_PREFIX: &str = "http://www.w3.org/2001/XMLSchema#";

/// Check if `name` is already a fully qualified URI with a protocol scheme.
/// Mirrors Prolog `uri_has_protocol/1` using the standard scheme syntax:
/// `ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )` followed by `://`.
///
/// This is intentionally a hand-rolled byte scan rather than a regex. The
/// predicate is on the hot path for every document key and value, so we want
/// to avoid regex compilation overhead and keep the common case (no protocol)
/// as a single fast scan. Approx 4x faster than regex on typical input.
fn has_protocol(name: &str) -> bool {
    let bytes = name.as_bytes();
    if let Some(i) = name.find("://") {
        if i == 0 {
            return false;
        }
        if !bytes[0].is_ascii_alphabetic() {
            return false;
        }
        for j in 1..i {
            let b = bytes[j];
            if !(b.is_ascii_alphabetic()
                || b.is_ascii_digit()
                || b == b'+'
                || b == b'-'
                || b == b'.')
            {
                return false;
            }
        }
        true
    } else {
        false
    }
}

/// A flat-vector prefix map.  For the small sets of prefixes typical in
/// TerminusDB (global defaults plus a handful of custom prefixes), linear
/// scan beats `HashMap` on lookup and has better cache locality.
pub type PrefixMap = Vec<(String, String)>;

const TERM_PTR_CACHE_CAPACITY: usize = 1024;

lazy_static! {
    /// Fast-path cache keyed by the Prolog term pointer of the context
    /// dictionary.  SWI-Prolog reuses term addresses after deallocation, so we
    /// store a cheap fingerprint alongside the cached map and verify it on every
    /// lookup.  The cache is bounded and cleared on overflow to avoid unbounded
    /// growth.
    static ref TERM_PTR_CACHE: Mutex<HashMap<usize, (u64, Arc<PrefixMap>)>> =
        Mutex::new(HashMap::with_capacity(TERM_PTR_CACHE_CAPACITY));
}

/// Default global prefixes used in TerminusDB schemas and documents.
fn default_prefixes() -> HashMap<String, String> {
    let mut prefixes = HashMap::new();
    prefixes.insert("sys".to_string(), "http://terminusdb.com/schema/sys#".to_string());
    prefixes.insert("xdd".to_string(), "http://terminusdb.com/schema/xdd#".to_string());
    prefixes.insert("vio".to_string(), "http://terminusdb.com/schema/vio#".to_string());
    prefixes.insert("woql".to_string(), "http://terminusdb.com/schema/woql#".to_string());
    prefixes.insert("xsd".to_string(), XSD_PREFIX.to_string());
    prefixes.insert("rdf".to_string(), "http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string());
    prefixes.insert("rdfs".to_string(), "http://www.w3.org/2000/01/rdf-schema#".to_string());
    prefixes.insert("owl".to_string(), "http://www.w3.org/2002/07/owl#".to_string());
    prefixes.insert("api".to_string(), "http://terminusdb.com/schema/api#".to_string());
    prefixes.insert("json".to_string(), "http://terminusdb.com/schema/json#".to_string());
    prefixes
}

/// Build a prefix expansion map from the default global prefixes plus the
/// schema context (`@base`, `@schema`, and any custom prefix pairs) using a
/// layer and its cached system ids.  This is the shared implementation used by
/// both the `DocumentContext` path and the layer fast path.
fn build_prefix_map_for_layer<L: Layer + Clone>(
    schema: &L,
    schema_sys: &SysIds<L>,
) -> HashMap<String, String> {
    let mut prefixes = default_prefixes();

    if let (Some(context_id), Some(base_id), Some(schema_id)) = (
        schema_sys.tdb_context(),
        schema_sys.base(),
        schema_sys.schema(),
    ) {
        if let Some(base_triple) = schema.single_triple_sp(context_id, base_id) {
            if let Some(ObjectType::Value(v)) = schema.id_object(base_triple.object) {
                prefixes.insert("@base".to_string(), v.as_val::<String, String>());
            }
        }
        if let Some(schema_triple) = schema.single_triple_sp(context_id, schema_id) {
            if let Some(ObjectType::Value(v)) = schema.id_object(schema_triple.object) {
                prefixes.insert("@schema".to_string(), v.as_val::<String, String>());
            }
        }
        if let Some(prefix_pair_id) = schema_sys.prefix_pair() {
            if let (Some(prefix_id), Some(url_id)) = (schema_sys.prefix(), schema_sys.url()) {
                for t in schema.triples_sp(context_id, prefix_pair_id) {
                    let contraction_id = schema.single_triple_sp(t.object, prefix_id);
                    let expansion_id = schema.single_triple_sp(t.object, url_id);
                    if let (Some(contraction), Some(expansion)) =
                        (contraction_id, expansion_id)
                    {
                        if let (
                            Some(ObjectType::Value(contraction_v)),
                            Some(ObjectType::Value(expansion_v)),
                        ) = (
                            schema.id_object(contraction.object),
                            schema.id_object(expansion.object),
                        ) {
                            let contraction_s = contraction_v.as_val::<String, String>();
                            let expansion_s = expansion_v.as_val::<String, String>();
                            prefixes.insert(contraction_s, expansion_s);
                        }
                    }
                }
            }
        }
    }

    prefixes
}

/// Build a prefix expansion map from a `DocumentContext`.
pub fn build_prefix_map<L: Layer + Clone>(doc_context: &DocumentContext<L>) -> HashMap<String, String> {
    match doc_context.schema.as_ref() {
        Some(schema) => build_prefix_map_for_layer(schema, &doc_context.schema_sys),
        None => default_prefixes(),
    }
}

/// Build a prefix expansion map directly from a `SyncStoreLayer`.
fn build_prefix_map_from_sync_layer(layer: &SyncStoreLayer) -> HashMap<String, String> {
    let schema_sys = Arc::new(SysIds::new(Some(layer.clone())));
    build_prefix_map_for_layer(layer, &schema_sys)
}

const LAYER_PREFIX_MAP_CACHE_CAPACITY: usize = 1024;

lazy_static! {
    /// Lifecycle-bound cache for schema-layer prefix maps.  Layer names are
    /// content-addressed, so the layer name is a stable key for the cached map.
    /// This cache is kept separately from the dict cache because the layer path
    /// does not require any Prolog term iteration or fingerprint checks.
    static ref LAYER_PREFIX_MAP_CACHE: Mutex<HashMap<String, Arc<PrefixMap>>> =
        Mutex::new(HashMap::with_capacity(LAYER_PREFIX_MAP_CACHE_CAPACITY));
}

/// Build or retrieve the cached prefix map for a schema layer.  The first call
/// for a given layer builds the map from the layer triples; subsequent calls
/// reuse the cached flat vector.  This is the fast path for the common case
/// where the caller has a schema layer rather than a custom context dict.
fn get_layer_prefix_map(layer: &SyncStoreLayer) -> Arc<PrefixMap> {
    let name = name_to_string(layer.name());
    {
        let cache = LAYER_PREFIX_MAP_CACHE.lock().unwrap();
        if let Some(cached) = cache.get(&name) {
            return cached.clone();
        }
    }

    let hashmap = build_prefix_map_from_sync_layer(layer);
    let mut map: PrefixMap = Vec::with_capacity(hashmap.len());
    for (k, v) in hashmap {
        map.push((k, v));
    }
    let map = Arc::new(map);

    let mut cache = LAYER_PREFIX_MAP_CACHE.lock().unwrap();
    if cache.len() >= LAYER_PREFIX_MAP_CACHE_CAPACITY {
        cache.clear();
    }
    cache.insert(name, map.clone());
    map
}

/// Expand a prefixed name using a prefix map.  Mirrors Prolog `prefix_expand`.
///
/// Returns the expanded string together with a flag indicating whether the
/// result should be an atom in Prolog.
///
/// - Pass-through cases (any URI with a protocol scheme, and bare `@...`)
///   preserve the input type.
/// - Prefixed names and base+vocab concatenation are always atoms, matching
///   Prolog's `atom_concat/3` and `atomic_list_concat/3` behavior.
///
/// Returns `Ok(None)` for empty keys, and `Err(name)` when a prefixed name
/// uses an unknown prefix so that the Prolog caller can throw
/// `error(key_has_unknown_prefix(Name), _)`.
fn prefix_expand_raw(
    prefixes: &PrefixMap,
    name: &str,
    input_is_atom: bool,
) -> Result<Option<(String, bool)>, String> {
    prefix_expand_with_base(prefixes, None, name, input_is_atom)
}

/// Expand a schema name.  Mirrors Prolog `prefix_expand_schema`: temporarily
/// treats `@base` as `@schema` so that unprefixed class/property names resolve
/// into the schema namespace.
fn prefix_expand_schema_raw(
    prefixes: &PrefixMap,
    name: &str,
    input_is_atom: bool,
) -> Result<Option<(String, bool)>, String> {
    let schema_base = prefixes
        .iter()
        .find(|(k, _)| k == "@schema")
        .map(|(_, v)| v.as_str());
    prefix_expand_with_base(prefixes, schema_base, name, input_is_atom)
}

/// Expand a schema name using a `HashMap` prefix map.
fn hashmap_prefix_expand_schema_raw(
    prefixes: &HashMap<String, String>,
    name: &str,
    input_is_atom: bool,
) -> Result<Option<(String, bool)>, String> {
    let schema_base = prefixes.get("@schema").map(|s| s.as_str());
    hashmap_prefix_expand_with_base(prefixes, schema_base, name, input_is_atom)
}

/// Check whether `name` has the shape of a prefixed name (`prefix:suffix`).
/// Mirrors the Prolog regex used in `uri_has_prefix_unsafe/2`:
///
///     ^(?<prefix>(\p{L}|@)((\p{Xwd}|-|\.)*(\p{Xwd}|-))?):(?<suffix>.*)$
///
/// Only names that look like a prefix reference are treated as one; names such
/// as `Node/prefix:node/...` (with a `/` before the colon) are not prefixed.
fn split_prefixed_name(name: &str) -> Option<(&str, &str)> {
    let idx = name.find(':')?;
    let prefix = &name[..idx];
    let suffix = &name[idx + 1..];
    if prefix.is_empty() {
        return None;
    }
    let mut chars = prefix.chars();
    let first = chars.next().unwrap();
    if !first.is_alphabetic() && first != '@' {
        return None;
    }
    // The prefix body may contain letters, digits, '-', '_', and '.', but the
    // last character before the colon must be alphanumeric, '-' or '_' (not '.').
    if let Some(last) = prefix.chars().last() {
        if last == '.' {
            return None;
        }
        if !last.is_alphanumeric() && last != '-' && last != '_' {
            return None;
        }
    }
    // Intermediate characters must be alphanumeric, '-', '_', or '.'.
    for c in chars {
        if !c.is_alphanumeric() && c != '-' && c != '_' && c != '.' {
            return None;
        }
    }
    Some((prefix, suffix))
}

/// Internal helper that avoids cloning the prefix map for schema expansion.
/// `base_override` is used by the schema path so that `@base` resolves to
/// `@schema` without allocating a new `HashMap`.
fn prefix_expand_with_base(
    prefixes: &PrefixMap,
    base_override: Option<&str>,
    name: &str,
    input_is_atom: bool,
) -> Result<Option<(String, bool)>, String> {
    if has_protocol(name) {
        return Ok(Some((name.to_string(), input_is_atom)));
    }
    if name.is_empty() {
        return Ok(None);
    }

    if let Some((prefix, suffix)) = split_prefixed_name(name) {
        for (k, v) in prefixes {
            if k.as_str() == prefix {
                let mut result = String::with_capacity(v.len() + suffix.len());
                result.push_str(v);
                result.push_str(suffix);
                return Ok(Some((result, true)));
            }
        }
        Err(name.to_string())
    } else if name.starts_with('@') {
        // Bare @-keywords (e.g. '@type', '@base') are returned unchanged.
        Ok(Some((name.to_string(), input_is_atom)))
    } else {
        let base = base_override.unwrap_or_else(|| {
            for (k, v) in prefixes {
                if k.as_str() == "@base" {
                    return v.as_str();
                }
            }
            ""
        });
        let mut vocab = "";
        for (k, v) in prefixes {
            if k.as_str() == "@vocab" {
                vocab = v.as_str();
                break;
            }
        }
        let mut result = String::with_capacity(base.len() + vocab.len() + name.len());
        result.push_str(base);
        result.push_str(vocab);
        result.push_str(name);
        Ok(Some((result, true)))
    }
}

/// Lookup a prefix in a `HashMap`-backed prefix map.  Used by the
/// `DocumentContext` lifecycle-bound cache and the internal Rust elaboration path.
fn hashmap_prefix_expand_with_base(
    prefixes: &HashMap<String, String>,
    base_override: Option<&str>,
    name: &str,
    input_is_atom: bool,
) -> Result<Option<(String, bool)>, String> {
    if has_protocol(name) {
        return Ok(Some((name.to_string(), input_is_atom)));
    }
    if name.is_empty() {
        return Ok(None);
    }

    if let Some((prefix, suffix)) = split_prefixed_name(name) {
        match prefixes.get(prefix) {
            Some(expanded) => Ok(Some((format!("{}{}", expanded, suffix), true))),
            None => Err(name.to_string()),
        }
    } else if name.starts_with('@') {
        // Bare @-keywords (e.g. '@type', '@base') are returned unchanged.
        Ok(Some((name.to_string(), input_is_atom)))
    } else {
        let base = base_override
            .or_else(|| prefixes.get("@base").map(|s| s.as_str()))
            .unwrap_or_default();
        let vocab = prefixes.get("@vocab").map(|s| s.as_str()).unwrap_or_default();
        Ok(Some((format!("{}{}{}", base, vocab, name), true)))
    }
}

/// Read a Prolog atom or string term as a Rust String.
fn term_to_string(term: &Term) -> PrologResult<String> {
    if term.is_atom() {
        term.get_atom_name(|opt| opt.map(|s| s.to_owned()).unwrap_or_default())
    } else {
        term.get_str(|opt| opt.map(|s| s.to_owned()).unwrap_or_default())
    }
}

/// Fast 64-bit hash mixer (SplitMix64).  Used for the context-dict fingerprint
/// so that key/value hashes are combined without sorting.
#[inline]
fn mix64(x: u64) -> u64 {
    let mut x = x.wrapping_add(0x9e3779b97f4a7c15);
    x = (x ^ (x >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94d049bb133111eb);
    x ^ (x >> 31)
}

/// Hash the content of a Prolog term without allocating a String.  Atoms and
/// strings in SWI-Prolog are both backed by interned atoms, so we hash the atom
/// pointer when possible; otherwise we fall back to a fast FNV-1a hash of the
/// contents.
fn hash_term(term: &Term) -> u64 {
    const FNV_OFFSET: u64 = 0xcbf29ce484222325;
    const FNV_PRIME: u64 = 0x100000001b3;

    #[inline]
    fn hash_bytes(s: &str) -> u64 {
        let mut h = FNV_OFFSET;
        for b in s.bytes() {
            h ^= b as u64;
            h = h.wrapping_mul(FNV_PRIME);
        }
        h
    }

    if let Ok(atom) = term.get::<Atom>() {
        atom.atom_ptr() as u64
    } else if term.is_atom() {
        term.get_atom_name(|opt| opt.map(hash_bytes).unwrap_or(FNV_OFFSET))
            .unwrap_or(FNV_OFFSET)
    } else {
        term.get_str(|opt| opt.map(hash_bytes).unwrap_or(FNV_OFFSET))
            .unwrap_or(FNV_OFFSET)
    }
}

/// Compute a cheap order-independent fingerprint of a Prolog context dictionary.
/// The fingerprint mixes the atom pointers of keys and values (or a content hash
/// fallback for non-atom terms) using XOR so the iteration order does not matter.
fn compute_fingerprint<'a, C: QueryableContextType>(
    context: &'a Context<'a, C>,
    dict_term: &Term<'a>,
) -> PrologResult<u64> {
    let mut fp: u64 = 0;
    let mut count: usize = 0;
    for (key, val_term) in context.dict_entries(dict_term) {
        let key_hash = match key {
            Key::Atom(ref a) => a.atom_ptr() as u64,
            Key::Int(i) => i as u64,
        };
        let val_hash = if let Ok(atom) = val_term.get::<Atom>() {
            atom.atom_ptr() as u64
        } else {
            hash_term(&val_term)
        };
        fp ^= mix64(key_hash) ^ mix64(val_hash);
        count += 1;
    }
    fp ^= mix64(count as u64);
    Ok(fp)
}

/// Parse a Prolog context dictionary into a prefix map.  A term-pointer cache
/// avoids rebuilding the map when the same dict is reused, and a cheap
/// fingerprint detects term-address reuse without requiring a full content hash.
fn parse_prefix_context<'a, C: QueryableContextType>(
    context: &'a Context<'a, C>,
    dict_term: &Term<'a>,
) -> PrologResult<(u64, Arc<PrefixMap>)> {
    let term_ptr = dict_term.term_ptr() as usize;

    // Fast path: term-pointer cache lookup with fingerprint verification.
    {
        let cache = TERM_PTR_CACHE.lock().unwrap();
        if let Some((fp, map)) = cache.get(&term_ptr) {
            let current_fp = compute_fingerprint(context, dict_term)?;
            if current_fp == *fp {
                return Ok((*fp, map.clone()));
            }
        }
    }

    // Build the map and fingerprint in a single dict iteration.
    let mut map: smallvec::SmallVec<[(String, String); 32]> =
        smallvec::SmallVec::new();
    let mut fp: u64 = 0;
    let mut count: usize = 0;
    for (key, val_term) in context.dict_entries(dict_term) {
        let key_str = match key {
            Key::Atom(ref a) => a.to_string(),
            Key::Int(i) => i.to_string(),
        };
        let key_hash = match key {
            Key::Atom(ref a) => a.atom_ptr() as u64,
            Key::Int(i) => i as u64,
        };
        let val_hash = if let Ok(atom) = val_term.get::<Atom>() {
            atom.atom_ptr() as u64
        } else {
            hash_term(&val_term)
        };
        fp ^= mix64(key_hash) ^ mix64(val_hash);
        count += 1;

        let val_str = term_to_string(&val_term)?;
        map.push((key_str, val_str));
    }
    fp ^= mix64(count as u64);

    let map: PrefixMap = map.into_vec();
    let map = Arc::new(map);

    let mut cache = TERM_PTR_CACHE.lock().unwrap();
    if cache.len() >= TERM_PTR_CACHE_CAPACITY {
        cache.clear();
    }
    cache.insert(term_ptr, (fp, map.clone()));
    Ok((fp, map))
}

/// Unify an expanded prefix result with the output term.  The `output_is_atom`
/// flag is computed by `prefix_expand_raw` to match Prolog's
/// `atomic_list_concat` behavior.
fn unify_expanded(
    expanded: &str,
    output_is_atom: bool,
    expanded_term: &Term,
) -> PrologResult<()> {
    if output_is_atom {
        let expanded_atom = Atom::new(expanded);
        expanded_term.unify(expanded_atom)
    } else {
        expanded_term.unify(expanded)
    }
}

/// Expand a name by borrowing its string contents directly from the Prolog
/// term, avoiding an intermediate `String` allocation.  The `expand` callback
/// receives the name and a flag indicating whether the input is an atom.
///
/// The callback returns `Ok(Some(...))` on success, `Ok(None)` for failure
/// cases such as an empty key, and `Err(name)` when the prefix is unknown so
/// that the Prolog exception `error(key_has_unknown_prefix(name), _)` can be
/// raised.
fn expand_with_name<C: QueryableContextType>(
    context: &Context<C>,
    name_term: &Term,
    expanded_term: &Term,
    expand: impl Fn(&str, bool) -> Result<Option<(String, bool)>, String>,
) -> PrologResult<()> {
    let input_is_atom = name_term.is_atom();
    let name = if input_is_atom {
        name_term.get_atom_name(|opt| opt.unwrap_or("").to_string())?
    } else {
        name_term.get_str(|opt| opt.unwrap_or("").to_string())?
    };
    match expand(&name, input_is_atom) {
        Ok(Some((expanded, output_is_atom))) => {
            unify_expanded(&expanded, output_is_atom, expanded_term)
        }
        Ok(None) => Err(PrologError::Failure),
        Err(unknown_name) => {
            let name_atom = Atom::new(&unknown_name);
            let error_term = term! {context: error(key_has_unknown_prefix(#name_atom), _)}?;
            context.raise_exception(&error_term)
        }
    }
}

/// Convenience wrapper used by the Rust elaboration path for class names and
/// property keys.  Uses the `DocumentContext` lifecycle-bound prefix map and
/// then expands as a schema name.
///
/// Returns `Ok(expanded)` on success, or `Err(name)` when the prefix is unknown
/// so callers can include the offending name in their error messages instead of
/// discarding the diagnostic.
pub fn expand_prefixed_name<L: Layer + Clone>(
    doc_context: &DocumentContext<L>,
    name: &str,
) -> Result<String, String> {
    let prefixes = doc_context.prefix_map();
    match hashmap_prefix_expand_schema_raw(prefixes, name, true) {
        Ok(Some((expanded, _))) => Ok(expanded),
        Ok(None) => Err(name.to_string()),
        Err(unknown_name) => Err(unknown_name),
    }
}

pub fn register() {
    register_rust_expand_prefix();
    register_rust_expand_prefix_schema();
    register_rust_expand_prefix_layer();
    register_rust_expand_prefix_schema_layer();
}

predicates! {
    #[module("$doc")]
    pub semidet fn rust_expand_prefix(context, context_term, name_term, expanded_term) {
        let (_, map) = parse_prefix_context(context, context_term)?;
        expand_with_name(context, name_term, expanded_term, |name, input_is_atom| {
            prefix_expand_raw(map.as_ref(), name, input_is_atom)
        })
    }

    #[module("$doc")]
    pub semidet fn rust_expand_prefix_schema(context, context_term, name_term, expanded_term) {
        let (_, map) = parse_prefix_context(context, context_term)?;
        expand_with_name(context, name_term, expanded_term, |name, input_is_atom| {
            prefix_expand_schema_raw(map.as_ref(), name, input_is_atom)
        })
    }

    #[module("$doc")]
    pub semidet fn rust_expand_prefix_layer(context, layer_term, name_term, expanded_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let map = get_layer_prefix_map(&layer);
        expand_with_name(context, name_term, expanded_term, |name, input_is_atom| {
            prefix_expand_raw(map.as_ref(), name, input_is_atom)
        })
    }

    #[module("$doc")]
    pub semidet fn rust_expand_prefix_schema_layer(context, layer_term, name_term, expanded_term) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let map = get_layer_prefix_map(&layer);
        expand_with_name(context, name_term, expanded_term, |name, input_is_atom| {
            prefix_expand_schema_raw(map.as_ref(), name, input_is_atom)
        })
    }
}
