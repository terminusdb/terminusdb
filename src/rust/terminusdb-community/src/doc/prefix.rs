use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::num::NonZeroUsize;
use std::sync::{Arc, Mutex};

use crate::terminus_store::layer::*;
use lazy_static::lazy_static;
use lru::LruCache;
use swipl::prelude::*;

use super::DocumentContext;

const XSD_PREFIX: &str = "http://www.w3.org/2001/XMLSchema#";
const PREFIX_MAP_CACHE_CAPACITY: usize = 1024;

lazy_static! {
    /// Global LRU cache of prefix maps built from Prolog context dictionaries.
    /// The key is a deterministic hash of the sorted (key, value) pairs.
    /// The cache is bounded to prevent unbounded growth across long-running
    /// processes that encounter many different schemas.
    static ref PREFIX_MAP_CACHE: Mutex<LruCache<u64, Arc<HashMap<String, String>>>> =
        Mutex::new(LruCache::new(NonZeroUsize::new(PREFIX_MAP_CACHE_CAPACITY).unwrap()));
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
/// schema context (`@base`, `@schema`, and any custom prefix pairs).
pub fn build_prefix_map<L: Layer + Clone>(doc_context: &DocumentContext<L>) -> HashMap<String, String> {
    let mut prefixes = default_prefixes();

    if let Some(schema) = doc_context.schema.as_ref() {
        if let (Some(context_id), Some(base_id), Some(schema_id)) = (
            doc_context.schema_sys.tdb_context(),
            doc_context.schema_sys.base(),
            doc_context.schema_sys.schema(),
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
            if let Some(prefix_pair_id) = doc_context.schema_sys.prefix_pair() {
                if let (Some(prefix_id), Some(url_id)) =
                    (doc_context.schema_sys.prefix(), doc_context.schema_sys.url())
                {
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
    }

    prefixes
}

/// Expand a prefixed name using a prefix map.  Mirrors Prolog `prefix_expand`.
///
/// Returns the expanded string together with a flag indicating whether the
/// result should be an atom in Prolog.
///
/// - Pass-through cases (`http://...`, `https://...`, `terminusdb://...`,
///   bare `@...`) preserve the input type.
/// - Prefixed names and base+vocab concatenation are always atoms, matching
///   Prolog's `atom_concat/3` and `atomic_list_concat/3` behavior.
fn prefix_expand_raw(
    prefixes: &HashMap<String, String>,
    name: &str,
    input_is_atom: bool,
) -> Option<(String, bool)> {
    if name.starts_with("http://") || name.starts_with("https://") || name.starts_with("terminusdb://") {
        return Some((name.to_string(), input_is_atom));
    }
    if name.is_empty() {
        return None;
    }

    if let Some(idx) = name.find(':') {
        let prefix = &name[..idx];
        let suffix = &name[idx + 1..];
        let expanded = prefixes.get(prefix)?;
        Some((format!("{}{}", expanded, suffix), true))
    } else if name.starts_with('@') {
        // Bare @-keywords (e.g. '@type', '@base') are returned unchanged.
        Some((name.to_string(), input_is_atom))
    } else {
        let base = prefixes.get("@base").cloned().unwrap_or_default();
        let vocab = prefixes.get("@vocab").cloned().unwrap_or_default();
        Some((format!("{}{}{}", base, vocab, name), true))
    }
}

/// Expand a schema name.  Mirrors Prolog `prefix_expand_schema`: temporarily
/// treats `@base` as `@schema` so that unprefixed class/property names resolve
/// into the schema namespace.
fn prefix_expand_schema_raw(
    prefixes: &HashMap<String, String>,
    name: &str,
    input_is_atom: bool,
) -> Option<(String, bool)> {
    let mut schema_context = prefixes.clone();
    if let Some(schema) = prefixes.get("@schema") {
        schema_context.insert("@base".to_string(), schema.clone());
    }
    prefix_expand_raw(&schema_context, name, input_is_atom)
}

/// Read a Prolog atom or string term as a Rust String.
fn term_to_string(term: &Term) -> PrologResult<String> {
    if term.is_atom() {
        term.get_atom_name(|opt| opt.map(|s| s.to_owned()).unwrap_or_default())
    } else {
        term.get_str(|opt| opt.map(|s| s.to_owned()).unwrap_or_default())
    }
}

/// Parse a Prolog context dictionary into a sorted prefix map and compute a
/// deterministic hash for caching.  The dictionary is expected to already
/// contain all relevant prefixes (global defaults plus schema-specific ones).
fn parse_prefix_context<'a, C: QueryableContextType>(
    context: &'a Context<'a, C>,
    dict_term: &Term<'a>,
) -> PrologResult<(u64, Arc<HashMap<String, String>>)> {
    let mut pairs: Vec<(String, String)> = Vec::new();
    for (key, val_term) in context.dict_entries(dict_term) {
        let key_str = match key {
            Key::Atom(a) => a.to_string(),
            Key::Int(i) => i.to_string(),
        };
        let val_str = term_to_string(&val_term)?;
        pairs.push((key_str, val_str));
    }
    pairs.sort();

    let mut hasher = DefaultHasher::new();
    for (k, v) in &pairs {
        k.hash(&mut hasher);
        v.hash(&mut hasher);
    }
    let hash = hasher.finish();

    let mut cache = PREFIX_MAP_CACHE.lock().unwrap();
    if let Some(cached) = cache.get(&hash) {
        return Ok((hash, cached.clone()));
    }

    let mut map = HashMap::with_capacity(pairs.len());
    for (k, v) in pairs {
        map.insert(k, v);
    }
    let map = Arc::new(map);
    cache.put(hash, map.clone());
    Ok((hash, map))
}

/// Expand a name using a Prolog context dictionary.  Caches the resulting
/// prefix map for the lifetime of the current process.
fn prefix_expand<'a, C: QueryableContextType>(
    context: &'a Context<'a, C>,
    dict_term: &Term<'a>,
    name: &str,
    input_is_atom: bool,
) -> PrologResult<Option<(String, bool)>> {
    let (_, prefixes) = parse_prefix_context(context, dict_term)?;
    Ok(prefix_expand_raw(&prefixes, name, input_is_atom))
}

/// Expand a schema name using a Prolog context dictionary.
fn prefix_expand_schema<'a, C: QueryableContextType>(
    context: &'a Context<'a, C>,
    dict_term: &Term<'a>,
    name: &str,
    input_is_atom: bool,
) -> PrologResult<Option<(String, bool)>> {
    let (_, prefixes) = parse_prefix_context(context, dict_term)?;
    Ok(prefix_expand_schema_raw(&prefixes, name, input_is_atom))
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

/// Convenience wrapper used by the Rust elaboration path for class names and
/// property keys.  Uses the `DocumentContext` lifecycle-bound prefix map and
/// then expands as a schema name.
pub fn expand_prefixed_name<L: Layer + Clone>(
    doc_context: &DocumentContext<L>,
    name: &str,
) -> Option<String> {
    let prefixes = doc_context.prefix_map();
    prefix_expand_schema_raw(prefixes, name, true)
        .map(|(expanded, _)| expanded)
}

pub fn register() {
    register_rust_expand_prefix();
    register_rust_expand_prefix_schema();
}

predicates! {
    #[module("$doc")]
    pub semidet fn rust_expand_prefix(context, context_term, name_term, expanded_term) {
        let name = term_to_string(name_term)?;
        let input_is_atom = name_term.is_atom();
        let (expanded, output_is_atom) = prefix_expand(context, context_term, &name, input_is_atom)
            .map_err(|_| PrologError::Failure)?
            .ok_or(PrologError::Failure)?;
        unify_expanded(&expanded, output_is_atom, expanded_term)
    }

    #[module("$doc")]
    pub semidet fn rust_expand_prefix_schema(context, context_term, name_term, expanded_term) {
        let name = term_to_string(name_term)?;
        let input_is_atom = name_term.is_atom();
        let (expanded, output_is_atom) =
            prefix_expand_schema(context, context_term, &name, input_is_atom)
                .map_err(|_| PrologError::Failure)?
                .ok_or(PrologError::Failure)?;
        unify_expanded(&expanded, output_is_atom, expanded_term)
    }
}
