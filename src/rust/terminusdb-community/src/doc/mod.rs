mod delete;

use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::io::Write;
use std::iter::Peekable;
use std::sync::{mpsc, Arc};

use crate::terminus_store::store::sync::*;
use crate::terminus_store::*;

use lazy_init::Lazy;
use serde_json::map;
use serde_json::{Map, Value};
use thiserror::Error;

use super::consts::*;
use super::prefix::PrefixContracter;
use super::schema::*;
use super::value::*;

use lazy_static::lazy_static;
use rayon::prelude::*;
use swipl::prelude::*;

pub struct DocumentContext<L: Layer + Clone> {
    schema: Option<L>,
    layer: Option<L>,
    prefixes: Lazy<PrefixContracter>,
    default_prefixes: Lazy<PrefixContracter>,
    types: HashSet<u64>,
    subtypes: HashMap<String, HashSet<u64>>,
    document_types: HashSet<u64>,
    unfoldables: HashSet<u64>,
    enums: HashMap<u64, String>,
    set_pairs: HashSet<(u64, u64)>,
    value_hashes: HashSet<u64>,
    sys_index_ids: Vec<u64>,

    rdf: Arc<RdfIds<L>>,
    sys: Arc<SysIds<L>>,
    schema_sys: Arc<SysIds<L>>,
}

impl<L: Layer + Clone> DocumentContext<L> {
    pub fn new(schema: L, instance: Option<L>) -> DocumentContext<L> {
        let schema_rdf = RdfIds::new(Some(schema.clone()));
        let schema_sys = Arc::new(SysIds::new(Some(schema.clone())));
        let schema_query_context = SchemaQueryContext::new(&schema, &schema_rdf, &schema_sys);

        let types: HashSet<u64>;
        let mut subtypes: HashMap<String, HashSet<u64>>;
        let mut document_types: HashSet<u64>;
        let unfoldables: HashSet<u64>;
        let mut enums: HashMap<u64, String>;
        let mut set_pairs: HashSet<(u64, u64)>;
        let mut value_hashes: HashSet<u64>;
        let sys_index_ids: Vec<u64>;

        let sys = Arc::new(SysIds::new(instance.clone()));
        let rdf = Arc::new(RdfIds::new(instance.clone()));

        if let Some(ref instance) = instance {
            let schema_document_type_ids = schema_query_context.get_document_type_ids_from_schema();
            let schema_type_ids = schema_query_context.get_type_ids_from_schema();
            let schema_subtype_ids = schema_query_context.get_subtype_ids();
            let schema_unfoldable_ids = schema_query_context.get_unfoldable_ids_from_schema();
            let schema_enum_ids = schema_query_context.get_enum_ids_from_schema();

            document_types = schema_query_context
                .schema_to_instance_types(instance, schema_document_type_ids)
                .collect();
            types = schema_query_context
                .schema_to_instance_types(instance, schema_type_ids)
                .collect();

            unfoldables = schema_query_context
                .schema_to_instance_types(instance, schema_unfoldable_ids)
                .collect();

            subtypes = HashMap::new();
            for (schema_sup, schema_subs) in schema_subtype_ids {
                let sup_name = schema.id_subject(schema_sup).unwrap();

                let subs: HashSet<u64> = schema_subs
                    .into_iter()
                    .filter_map(|schema_sub| {
                        schema_query_context.translate_subject_id(instance, schema_sub)
                    })
                    .collect();

                subtypes.insert(sup_name, subs);
            }

            enums = HashMap::new();
            for schema_enum_id in schema_enum_ids {
                let enum_expanded = schema.id_object_node(schema_enum_id).unwrap();
                if let Some(instance_enum_id) = instance.object_node_id(&enum_expanded) {
                    // an enum node is its type concatenated with a / followed by an uri encoded string corresponding to its value
                    let pos = enum_expanded.rfind('/').unwrap();
                    let encoded = &enum_expanded[pos + 1..];

                    let decoded = urlencoding::decode(encoded).unwrap().to_string();
                    enums.insert(instance_enum_id, decoded);
                }
            }

            let schema_set_pairs = schema_query_context.get_set_pairs_from_schema();
            set_pairs = HashSet::new();
            for (schema_type_id, schema_predicate_id) in schema_set_pairs {
                if let Some(type_id) =
                    schema_query_context.translate_subject_id(instance, schema_type_id)
                {
                    if let Some(predicate_id) =
                        schema_query_context.translate_predicate_id(instance, schema_predicate_id)
                    {
                        set_pairs.insert((type_id, predicate_id));
                    }
                }
            }

            value_hashes = HashSet::new();
            let schema_value_hashes = schema_query_context.get_valuehash_type_ids();
            for schema_type_id in schema_value_hashes {
                if let Some(type_id) =
                    schema_query_context.translate_subject_id(instance, schema_type_id)
                {
                    value_hashes.insert(type_id);
                }
            }

            if let Some(sys_json_document_type_id) = sys.json_document() {
                document_types.insert(sys_json_document_type_id);
            }

            sys_index_ids = retrieve_all_index_ids(instance);
        } else {
            types = HashSet::with_capacity(0);
            subtypes = HashMap::with_capacity(0);
            document_types = HashSet::with_capacity(0);
            unfoldables = HashSet::with_capacity(0);
            enums = HashMap::with_capacity(0);
            set_pairs = HashSet::with_capacity(0);
            value_hashes = HashSet::with_capacity(0);
            sys_index_ids = Vec::with_capacity(0);
        }

        DocumentContext {
            layer: instance,
            schema: Some(schema),

            prefixes: Lazy::new(),
            default_prefixes: Lazy::new(),
            types,
            subtypes,
            document_types,
            unfoldables,
            enums,
            set_pairs,
            value_hashes,
            sys_index_ids,

            rdf,
            sys,
            schema_sys,
        }
    }

    pub fn new_json(instance: Option<L>) -> DocumentContext<L> {
        Self {
            layer: instance.clone(),
            schema: None,

            default_prefixes: Lazy::new(),
            prefixes: Lazy::new(),
            types: HashSet::with_capacity(0),
            subtypes: HashMap::with_capacity(0),
            document_types: HashSet::with_capacity(0),
            unfoldables: HashSet::with_capacity(0),
            enums: HashMap::with_capacity(0),
            set_pairs: HashSet::with_capacity(0),
            value_hashes: HashSet::with_capacity(0),
            sys_index_ids: Vec::with_capacity(0),

            rdf: Arc::new(RdfIds::new(instance.clone())),
            sys: Arc::new(SysIds::new(instance)),
            schema_sys: Arc::new(SysIds::new(None)),
        }
    }

    fn layer(&self) -> &L {
        self.layer.as_ref().unwrap()
    }

    fn prefixes(&self, compress: bool) -> &PrefixContracter {
        if compress {
            self.prefixes.get_or_create(|| match self.schema.as_ref() {
                Some(schema) => prefix_contracter_from_schema_layer(schema, &self.schema_sys),
                None => PrefixContracter::default(),
            })
        } else {
            self.default_prefixes
                .get_or_create(PrefixContracter::default)
        }
    }

    pub fn get_document(
        &self,
        iri: &str,
        compress: bool,
        unfold: bool,
    ) -> Result<Option<Map<String, Value>>, DocRetrievalError> {
        match self.layer.as_ref().and_then(|layer| {
            layer
                .subject_id(iri)
                .map(|id| self.get_id_document(id, compress, unfold))
        }) {
            Some(Ok(x)) => Ok(x),
            Some(Err(e)) => Err(e),
            None => Ok(None),
        }
    }

    fn get_field<'a, 'b>(
        &'a self,
        object: u64,
        compress: bool,
        unfold: bool,
    ) -> Result<Value, StackEntry<'a, L>> {
        if let Some(val) = self.enums.get(&object) {
            Ok(Value::String(val.clone()))
        } else if Some(object) == self.rdf.nil() {
            Ok(Value::Array(vec![]))
        } else {
            match self.get_doc_stub(object, true, compress, unfold) {
                // it's not a terminator so we will need to descend into it. That is, we would need to descend into it if there were any children, so let's check.
                Ok((doc, type_id, fields, json)) => Err(StackEntry::Document {
                    id: object,
                    doc,
                    type_id,
                    fields: Some(fields),
                    json,
                }),
                Err(v) => Ok(v),
            }
        }
    }

    fn add_field(&self, obj: &mut Map<String, Value>, key: &str, value: Value, is_set: bool) {
        // add a field, but if the field is already there, make it a collection
        match obj.entry(key) {
            map::Entry::Vacant(e) => {
                if is_set {
                    e.insert(Value::Array(vec![value]));
                } else {
                    e.insert(value);
                }
            }
            map::Entry::Occupied(mut e) => {
                let v = e.get_mut();
                match v {
                    Value::Array(a) => {
                        a.push(value);
                    }
                    _ => {
                        let mut a = Vec::new();
                        let mut old_v = Value::Null;
                        std::mem::swap(&mut old_v, v);
                        a.push(old_v);
                        a.push(value);
                        *v = Value::Array(a);
                    }
                }
            }
        }
    }

    fn get_list_iter(&self, id: u64) -> Peekable<RdfListIterator<L>> {
        RdfListIterator {
            layer: self.layer(),
            cur: id,
            rdf_first_id: self.rdf.first(),
            rdf_rest_id: self.rdf.rest(),
            rdf_nil_id: self.rdf.nil(),
        }
        .peekable()
    }

    fn get_array_iter(&self, stack_entry: &mut StackEntry<L>) -> ArrayIterator<L> {
        if let StackEntry::Document { fields, .. } = stack_entry {
            let mut it = None;
            std::mem::swap(&mut it, fields);
            let mut it = it.unwrap();
            let t = it.peek().unwrap();
            let subject = t.subject;
            let predicate = t.predicate;
            return ArrayIterator {
                layer: self.layer(),
                it,
                subject,
                predicate,
                last_index: None,
                sys_index_ids: &self.sys_index_ids,
                sys_value_id: self.sys.value(),
            };
        } else {
            panic!("array is not a field of a document");
        }
    }

    fn get_doc_stub(
        &self,
        id: u64,
        terminate: bool,
        compress: bool,
        unfold: bool,
    ) -> Result<
        (
            Map<String, Value>,
            Option<u64>,
            Peekable<Box<dyn Iterator<Item = IdTriple> + Send>>,
            bool,
        ),
        Value,
    > {
        let id_name = self.layer().id_object(id).unwrap();
        if let Some(v) = id_name.value_ref() {
            // this is not actually a document but a value
            return Err(value_to_json(v));
        }

        // we know id_name is properly a node
        let id_name = id_name.node().unwrap();
        let id_name_contracted = self
            .prefixes(compress)
            .instance_contract(&id_name)
            .to_string();

        let rdf_type_id = self.rdf.type_();
        let mut fields = (Box::new(
            self.layer()
                .triples_s(id)
                .filter(move |f| Some(f.predicate) != rdf_type_id),
        ) as Box<dyn Iterator<Item = IdTriple> + Send>)
            .peekable();

        let mut type_id = None;
        let mut type_name_contracted: Option<String> = None;
        let mut json = false;
        if let Some(rdf_type_id) = rdf_type_id {
            if let Some(t) = self.layer().single_triple_sp(id, rdf_type_id) {
                if terminate
                    && (!unfold
                        || (self.document_types.contains(&t.object)
                            && !self.unfoldables.contains(&t.object)))
                {
                    return Err(Value::String(id_name_contracted));
                }

                type_id = Some(t.object);
                if Some(t.object) == self.sys.json_document() || Some(t.object) == self.sys.json() {
                    json = true;
                }

                if !json {
                    // json objects are special. We don't care about their type names.
                    // for other types, we do care, so convert to string format.
                    let type_name = self.layer().id_object_node(t.object).unwrap();
                    type_name_contracted = Some(
                        self.prefixes(compress)
                            .schema_contract(&type_name)
                            .to_string(),
                    );
                }
            }
        }

        if type_id.is_none()
            && (fields.peek().is_none()
                || fields.peek().map(|x| x.predicate) == self.sys.foreign_type_predicate())
        {
            // we're actually dealing with a raw id here
            Err(Value::String(id_name_contracted))
        } else {
            let mut result = Map::new();

            if type_id.is_none() || type_id != self.sys.json() {
                // we only care about the id for non-json types, and for the top level json documents. Json (sub)documents should not include an id.
                result.insert("@id".to_string(), Value::String(id_name_contracted));
            }
            if let Some(tn) = type_name_contracted {
                // since we only contract types for non-json types, this will only add the @type for actual defined types, not the json types.
                result.insert("@type".to_string(), Value::String(tn));
            }

            Ok((result, type_id, fields, json))
        }
    }

    pub fn get_id_document(
        &self,
        id: u64,
        compress: bool,
        unfold: bool,
    ) -> Result<Option<Map<String, Value>>, DocRetrievalError> {
        if self.layer.is_none() {
            panic!("expected id to point at document: {}", id);
        }

        let mut stack = Vec::new();

        if let Ok((doc, type_id, fields, json)) = self.get_doc_stub(id, false, compress, unfold) {
            stack.push(StackEntry::Document {
                id,
                doc,
                type_id,
                fields: Some(fields),
                json,
            });
        } else {
            // Something is wrong. We expected to be looking up a document, but instead we got a single value.
            // The only way this could have happened is because we're looking up something that doesn't exist.
            return Ok(None);
        }

        let mut visited: Vec<u64> = Vec::new();
        visited.push(id);

        lazy_static! {
            static ref LIMIT: usize = {
                std::env::var("TERMINUSDB_DOC_WORK_LIMIT")
                    .ok()
                    .and_then(|s| s.parse::<usize>().ok())
                    .unwrap_or(500_000)
            };
        }
        let mut work = 0;
        loop {
            work += 1;
            if work >= *LIMIT {
                break;
            }
            let cur = stack.last_mut().unwrap();
            let is_json = cur.is_json();
            if let Some(next_obj) = cur.peek() {
                // let's first see if this object is one of the expected types
                if is_json || cur.is_document() {
                    if let Some(rdf_type_id) = self.rdf.type_() {
                        if let Some(t) = self.layer().single_triple_sp(next_obj, rdf_type_id) {
                            if Some(t.object) == self.sys.array() {
                                let array_iter = self.get_array_iter(cur);
                                stack.push(StackEntry::Array(ArrayStackEntry {
                                    collect: Vec::new(),
                                    entries: array_iter,
                                }));
                                continue;
                            } else if Some(t.object) == self.rdf.list() {
                                let list_iter = self.get_list_iter(next_obj);
                                stack.push(StackEntry::List {
                                    collect: Vec::new(),
                                    entries: list_iter,
                                    json: is_json,
                                });
                                continue;
                            }
                        }
                    }
                }

                // it's not one of the special types, treat it as an ordinary field.
                match self.get_field(next_obj, compress, unfold) {
                    Ok(val) => {
                        cur.integrate_value(self, val, compress);
                    }
                    Err(entry) => {
                        // this is a bit of a silly setup.
                        // we pretty much know for sure that get_field will have returned the stub of a subdocument by now. We could either use the documnet id contained in that stub, or we could use the next_obj id that we already had.
                        // I chose to use document_id() here just in case a future refactor suddenly changes what kind of things come back as field entries here.
                        if entry
                            .document_id()
                            .map(|id| visited.contains(&id))
                            .unwrap_or(false)
                        {
                            // This entry is already visited. We only have to provide a name.
                            cur.integrate_value(
                                self,
                                entry.document_iri().unwrap().clone(),
                                compress,
                            );
                        } else {
                            // We need to iterate deeper, so add it to the stack without iterating past the field.
                            visited.push(next_obj);
                            stack.push(entry);
                        }
                    }
                }
            } else {
                // done!
                visited.pop();
                let cur = stack.pop().unwrap();
                if let Some(parent) = stack.last_mut() {
                    parent.integrate(self, cur, compress);
                } else {
                    // we're done, this was the root, time to return!
                    match cur {
                        StackEntry::Document { doc, .. } => return Ok(Some(doc)),
                        _ => panic!("unexpected element at stack top"),
                    }
                }
            }
        }

        // if we are here, it is because doc retrieval got too expensive
        let iri = if let Some(StackEntry::Document { doc, .. }) = stack.first() {
            doc.get("@id")
                .map(|j| j.to_string())
                .unwrap_or_else(|| "unknown".to_string())
        } else {
            "unknown".to_string()
        };
        Err(DocRetrievalError::LimitExceeded(iri))
    }

    fn get_subtypes_for(&self, type_name: &str) -> Vec<u64> {
        let mut types: Vec<u64> = Vec::new();
        if let Some(type_id) = self.layer().object_node_id(type_name) {
            // always at least search for the type itself
            types.push(type_id);
        }
        if let Some(subtypes) = self.subtypes.get(type_name) {
            types.extend(subtypes.iter().cloned());
        }

        types
    }

    fn id_document_exists(&self, id: u64) -> bool {
        self.rdf
            .type_()
            .map(|rdf_type_id| self.layer().single_triple_sp(id, rdf_type_id).is_some())
            .unwrap_or(false)
    }

    pub fn document_exists(&self, iri: &str) -> bool {
        match self.layer.as_ref() {
            None => false,
            Some(l) => match l.subject_id(iri) {
                Some(id) => self.id_document_exists(id),
                None => false,
            },
        }
    }
}

#[derive(Error, Debug)]
pub enum DocRetrievalError {
    #[error("Limit exceeded while trying to retrieve document with IRI {0}")]
    LimitExceeded(String),
}

impl IntoPrologException for DocRetrievalError {
    fn into_prolog_exception<'a, T: QueryableContextType>(
        self,
        context: &'a Context<'_, T>,
    ) -> PrologResult<Term<'a>> {
        match self {
            DocRetrievalError::LimitExceeded(iri) => {
                term! {context: error(doc_retrieval_limit_exceeded(#iri), _)}
            }
        }
    }
}

pub fn retrieve_all_index_ids<L: Layer>(instance: &L) -> Vec<u64> {
    let mut sys_index_ids = Vec::new();
    let mut index_str = SYS_INDEX.to_string();
    let orig_len = index_str.len();
    let mut ix = 1;
    loop {
        if let Some(index_id) = instance.predicate_id(&index_str) {
            sys_index_ids.push(index_id);
            ix += 1;
            let ix_s = ix.to_string();
            index_str.truncate(orig_len);
            index_str.push_str(&ix_s);
        } else {
            break;
        }
    }

    sys_index_ids
}

enum StackEntry<'a, L: Layer> {
    Document {
        id: u64,
        doc: Map<String, Value>,
        type_id: Option<u64>,
        fields: Option<Peekable<Box<dyn Iterator<Item = IdTriple> + Send>>>,
        json: bool,
    },
    List {
        collect: Vec<Value>,
        entries: Peekable<RdfListIterator<'a, L>>,
        json: bool,
    },
    Array(ArrayStackEntry<'a, L>),
}

impl<'a, L: Layer> StackEntry<'a, L> {
    fn is_document(&self) -> bool {
        matches!(self, Self::Document { .. })
    }

    fn is_json(&self) -> bool {
        match self {
            Self::Document { json, .. } => *json,
            Self::List { json, .. } => *json,
            _ => false,
        }
    }

    fn document_id(&self) -> Option<u64> {
        match self {
            Self::Document { id, .. } => Some(*id),
            _ => None,
        }
    }
    fn document_iri(&self) -> Option<&Value> {
        match self {
            Self::Document { doc, .. } => doc.get("@id"),
            _ => None,
        }
    }
}

struct ArrayStackEntry<'a, L: Layer> {
    collect: Vec<(Vec<usize>, Value)>,
    entries: ArrayIterator<'a, L>,
}

pub struct ArrayIterator<'a, L: Layer> {
    pub layer: &'a L,
    pub it: Peekable<Box<dyn Iterator<Item = IdTriple> + Send>>,
    pub subject: u64,
    pub predicate: u64,
    pub last_index: Option<Vec<usize>>,
    pub sys_index_ids: &'a [u64],
    pub sys_value_id: Option<u64>,
}

impl<'a, L: Layer> Iterator for ArrayIterator<'a, L> {
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        if let Some(t) = self.it.peek() {
            if t.subject == self.subject && t.predicate == self.predicate {
                let mut indexes = Vec::new();
                for index_id in self.sys_index_ids {
                    if let Some(index_triple) = self.layer.single_triple_sp(t.object, *index_id) {
                        let index_value = self.layer.id_object_value(index_triple.object).unwrap();
                        let index = value_to_array_index(&index_value);
                        indexes.push(index);
                    } else {
                        // no more indexes to come
                        break;
                    }
                }
                indexes.reverse();

                let value_id = self
                    .layer
                    .single_triple_sp(t.object, *self.sys_value_id.as_ref().unwrap())
                    .expect("expected value property on array element")
                    .object;

                // now that we know for sure that we got everything and this is indeed an array cell, let's actually move the field iterator.
                self.it.next().unwrap();
                self.last_index = Some(indexes);
                return Some(value_id);
            }
        }

        None
    }
}

impl<'a, L: Layer + Clone> StackEntry<'a, L> {
    fn peek(&mut self) -> Option<u64> {
        match self {
            Self::Document { fields, .. } => fields.as_mut().unwrap().peek().map(|t| t.object),
            Self::List { entries, .. } => entries.peek().copied(),
            Self::Array(a) => a.entries.next(),
        }
    }

    fn into_value(self) -> Value {
        match self {
            Self::Document { doc, .. } => Value::Object(doc),
            Self::List { collect, .. } => Value::Array(collect),
            Self::Array { .. } => panic!("cannot directly turn array into a value"),
        }
    }

    fn integrate(
        &mut self,
        context: &DocumentContext<L>,
        child: StackEntry<'a, L>,
        compress: bool,
    ) {
        match child {
            StackEntry::Array(a) => self.integrate_array(context, a, compress),
            _ => self.integrate_value(context, child.into_value(), compress),
        }
    }

    fn integrate_array(
        &mut self,
        context: &DocumentContext<L>,
        array: ArrayStackEntry<'a, L>,
        compress: bool,
    ) {
        // self has to be an object, let's make sure of that.
        match self {
            Self::Document { doc, fields, .. } => {
                *fields = Some(array.entries.it);

                let value = collect_array(array.collect);

                let p_name = context
                    .layer()
                    .id_predicate(array.entries.predicate)
                    .unwrap();
                let p_name_contracted = context
                    .prefixes(compress)
                    .schema_contract(&p_name)
                    .to_string();
                context.add_field(doc, &p_name_contracted, value, false);
            }
            _ => panic!("unexpected parent type of array"),
        }
    }

    fn integrate_value(&mut self, context: &DocumentContext<L>, value: Value, compress: bool) {
        match self {
            Self::Document {
                doc,
                fields,
                type_id,
                ..
            } => {
                // we previously peeked a field and decided we needed to recurse deeper.
                // this is the time to pop it.
                let t = fields.as_mut().unwrap().next().unwrap();
                let is_set = type_id
                    .map(|type_id| context.set_pairs.contains(&(type_id, t.predicate)))
                    .unwrap_or(false);
                let p_name = context.layer().id_predicate(t.predicate).unwrap();
                let p_name_contracted = context
                    .prefixes(compress)
                    .schema_contract(&p_name)
                    .to_string();
                context.add_field(doc, &p_name_contracted, value, is_set);
            }
            Self::List {
                collect, entries, ..
            } => {
                // We previously peeked a list entry and decided we needed to recurse deeper.
                // this is the time to pop it.
                let _elt = entries.next().unwrap();
                collect.push(value);
            }
            Self::Array(a) => {
                let mut index = None;
                std::mem::swap(&mut index, &mut a.entries.last_index);

                a.collect.push((index.unwrap(), value));
            }
        }
    }
}

fn collect_array(mut elements: Vec<(Vec<usize>, Value)>) -> Value {
    elements.sort_by(|(i1, _), (i2, _)| i1.cmp(i2));

    let dimensions = elements[0].0.len();
    let mut collect: Vec<Vec<Value>> = Vec::with_capacity(dimensions);
    collect.resize_with(dimensions, Vec::new);

    for (index, value) in elements {
        assert!(
            index.len() == dimensions,
            "array elemenet did not have expected amount of dimensions"
        );

        // match indexes
        for d in 0..dimensions {
            let expected = collect[d].len();
            if expected < index[d] {
                // any less significant dimension will need to be gathered up, provided anything was collected to begin with.
                for n in (d + 1..dimensions).rev() {
                    if collect[n].len() != 0 {
                        let mut x = Vec::new();
                        std::mem::swap(&mut x, &mut collect[n]);
                        collect[n - 1].push(Value::Array(x));
                    }
                }

                // Then, we need to add null values until we're at the expected index.
                // Note that we might have just incremented by one due to the collect.
                collect[d].resize(index[d], Value::Null);
            }
        }

        // add the value
        collect[dimensions - 1].push(value);
    }

    // Finally, gather up everything
    for d in (0..dimensions - 1).rev() {
        let x = collect.pop().unwrap();
        if !x.is_empty() {
            collect[d].push(Value::Array(x));
        }
    }

    Value::Array(collect.pop().unwrap())
}

wrapped_arc_blob!(
    "GetDocumentContext",
    DocumentContextBlob,
    DocumentContext<SyncStoreLayer>,
    defaults
);

fn map_to_writer<W: Write>(
    writer: W,
    m: Map<String, Value>,
    pretty: bool,
) -> serde_json::Result<()> {
    if pretty {
        serde_json::to_writer_pretty(writer, &Value::Object(m))
    } else {
        serde_json::to_writer(writer, &Value::Object(m))
    }
}

fn print_document<C: QueryableContextType>(
    context: &Context<C>,
    stream: &mut WritablePrologStream,
    doc: Map<String, Value>,
    as_list: bool,
    minimized: bool,
    stream_started: &mut bool,
) -> PrologResult<()> {
    if as_list && *stream_started {
        context.try_or_die_generic(stream.write_all(b",\n"))?;
    }
    *stream_started = true;

    context.try_or_die_generic(map_to_writer(&mut *stream, doc, !minimized))?;

    if !as_list {
        context.try_or_die(stream.write_all(b"\n"))?;
    }

    context.try_or_die(stream.flush())?;

    Ok(())
}

fn print_documents_of_types<
    'a,
    C: QueryableContextType,
    L: Layer + Clone,
    I: IntoIterator<Item = &'a u64>,
>(
    context: &Context<C>,
    doc_context: &DocumentContext<L>,
    stream_term: &Term,
    skip_term: &Term,
    count_term: &Term,
    as_list_term: &Term,
    types: I,
    compress: bool,
    unfold: bool,
    minimize: bool,
) -> PrologResult<()> {
    let mut stream: WritablePrologStream = stream_term.get_ex()?;
    let mut skip: u64 = skip_term.get_ex()?;
    let mut count: Option<u64> = attempt_opt(count_term.get())?;
    let as_list: bool = as_list_term.get_ex()?;
    let mut started = false;

    for typ in types {
        for t in doc_context.layer().triples_o(*typ) {
            if skip != 0 {
                // skip
                skip -= 1;
                continue;
            }
            if let Some(count) = count.as_mut() {
                if *count == 0 {
                    break;
                }
                *count -= 1;
            }

            if Some(t.predicate) != doc_context.rdf.type_() {
                continue;
            }

            let map = context
                .try_or_die(doc_context.get_id_document(t.subject, compress, unfold))?
                .expect("expected document lookup by type to succeed as ids were prefetched");
            print_document(context, &mut stream, map, as_list, minimize, &mut started)?;
        }
    }

    Ok(())
}

fn par_print_documents_of_types<C: QueryableContextType, L: Layer + Clone + 'static>(
    context: &Context<C>,
    doc_context: &Arc<DocumentContext<L>>,
    stream_term: &Term,
    skip_term: &Term,
    count_term: &Term,
    as_list_term: &Term,
    types: Vec<u64>,
    compress: bool,
    unfold: bool,
    minimize: bool,
) -> PrologResult<()> {
    let mut stream: WritablePrologStream = stream_term.get_ex()?;

    let skip: u64 = skip_term.get_ex()?;
    let count: Option<u64> = attempt_opt(count_term.get())?;
    let as_list: bool = as_list_term.get_ex()?;

    let channel_size = rayon::current_num_threads() * 2;
    let (sender, receiver) = mpsc::sync_channel(channel_size);

    // cloning here cause we need to use the doc context from two places.
    // Since we are dealing with an arc, the clone is cheap.
    let doc_context: Arc<_> = doc_context.clone();
    let doc_context2: Arc<_> = doc_context.clone();

    let rdf_type_id = doc_context.rdf.type_();
    let iter = types
        .into_iter()
        .flat_map(move |typ| doc_context.layer().triples_o(typ))
        .filter(move |t| Some(t.predicate) == rdf_type_id)
        .skip(skip as usize);
    let iter = if let Some(count) = count {
        itertools::Either::Left(iter.take(count as usize))
    } else {
        itertools::Either::Right(iter)
    };

    rayon::spawn(move || {
        let _result = iter
            .enumerate()
            .par_bridge()
            .try_for_each_with(sender, |sender, (ix, t)| {
                let map = doc_context2.get_id_document(t.subject, compress, unfold);
                sender.send((ix, map)) // failure will kill the task
            });
    });

    let mut started = false;

    let mut result = BinaryHeap::new();
    let mut cur = 0;
    while let Ok((ix, map)) = receiver.recv() {
        let map = context
            .try_or_die(map)?
            .expect("expected parallel document lookup by type to succeed as ids were prefetched");
        if ix == cur {
            print_document(context, &mut stream, map, as_list, minimize, &mut started)?;

            cur += 1;
            while result
                .peek()
                .map(|HeapEntry { index, .. }| index == &cur)
                .unwrap_or(false)
            {
                let HeapEntry {
                    index: _index,
                    value,
                } = result.pop().unwrap();

                print_document(context, &mut stream, value, as_list, minimize, &mut started)?;

                cur += 1;
            }
        } else {
            result.push(HeapEntry {
                index: ix,
                value: map,
            });
        }
    }

    assert!(result.is_empty());

    Ok(())
}

fn print_documents_by_id<C: QueryableContextType, L: Layer + Clone + 'static>(
    context: &Context<C>,
    doc_context: &Arc<DocumentContext<L>>,
    stream_term: &Term,
    skip_term: &Term,
    count_term: &Term,
    as_list_term: &Term,
    iris_term: &Term,
    compress: bool,
    unfold: bool,
    minimize: bool,
) -> PrologResult<()> {
    let mut stream: WritablePrologStream = stream_term.get_ex()?;

    let mut skip: u64 = skip_term.get_ex()?;
    let mut count: Option<u64> = attempt_opt(count_term.get())?;
    let as_list: bool = as_list_term.get_ex()?;

    let mut started = false;
    for iri_term in context.term_list_iter(iris_term) {
        let iri: PrologText = iri_term.get_ex()?;
        if let Some(doc) = context.try_or_die(doc_context.get_document(&iri, compress, unfold))? {
            if skip > 0 {
                skip -= 1;
                continue;
            }
            if let Some(count) = count.as_mut() {
                if *count == 0 {
                    break;
                }
                *count -= 1;
            }
            print_document(context, &mut stream, doc, as_list, minimize, &mut started)?;
        }
    }

    Ok(())
}

fn par_print_documents_by_id<C: QueryableContextType, L: Layer + Clone + 'static>(
    context: &Context<C>,
    doc_context: &Arc<DocumentContext<L>>,
    stream_term: &Term,
    skip_term: &Term,
    count_term: &Term,
    as_list_term: &Term,
    iris_term: &Term,
    compress: bool,
    unfold: bool,
    minimize: bool,
) -> PrologResult<()> {
    let mut stream: WritablePrologStream = stream_term.get_ex()?;

    let skip: u64 = skip_term.get_ex()?;
    let count: Option<u64> = attempt_opt(count_term.get())?;
    let as_list: bool = as_list_term.get_ex()?;

    let channel_size = rayon::current_num_threads() * 2;
    let (sender, receiver) = mpsc::sync_channel(channel_size);

    let doc_context2 = doc_context.clone();
    let doc_context3 = doc_context.clone();
    // for the par version, we pre-scan for existence to handle skip and count more efficiently
    let iris: Vec<String> = context
        .term_list_iter(iris_term)
        .map(|iri_term| iri_term.get_ex::<PrologText>().map(|t| t.to_string()))
        .collect::<Result<_, _>>()?;
    let iter = iris
        .into_iter()
        .filter(move |iri| doc_context3.document_exists(&iri))
        .skip(skip as usize);
    let iter = match count {
        Some(count) => itertools::Either::Left(iter.take(count as usize)),
        None => itertools::Either::Right(iter),
    };
    rayon::spawn(move || {
        let _result =
            iter.enumerate()
                .par_bridge()
                .try_for_each_with(sender, |sender, (ix, iri)| {
                    let map = doc_context2.get_document(&iri, compress, unfold);
                    sender.send((ix, map)) // failure will kill the task
                });
    });

    let mut started = false;
    let mut result = BinaryHeap::new();
    let mut cur = 0;
    while let Ok((ix, map)) = receiver.recv() {
        let map = context
            .try_or_die(map)?
            .expect("expected document to exist");
        if ix == cur {
            print_document(context, &mut stream, map, as_list, minimize, &mut started)?;

            cur += 1;
            while result
                .peek()
                .map(|HeapEntry { index, .. }| index == &cur)
                .unwrap_or(false)
            {
                let HeapEntry {
                    index: _index,
                    value,
                } = result.pop().unwrap();

                print_document(context, &mut stream, value, as_list, minimize, &mut started)?;

                cur += 1;
            }
        } else {
            result.push(HeapEntry {
                index: ix,
                value: map,
            });
        }
    }

    assert!(result.is_empty());
    Ok(())
}

use super::types::*;
predicates! {
    #[module("$doc")]
    semidet fn get_document_context(context, transaction_term, context_term) {
        let schema_layer = transaction_schema_layer(context, transaction_term)?.unwrap();
        let instance_layer = transaction_instance_layer(context, transaction_term)?;

        let get_context = DocumentContext::new(schema_layer, instance_layer);

        context_term.unify(DocumentContextBlob(Arc::new(get_context)))
    }

    #[module("$doc")]
    semidet fn print_document_json(context, stream_term, get_context_term, doc_name_term, as_list_term, compress_term, unfold_term, minimize_term, started_term) {
        let mut stream: WritablePrologStream = stream_term.get_ex()?;
        if !doc_name_term.is_string() && !doc_name_term.is_atom() {
            return fail();
        }

        let doc_context: DocumentContextBlob = get_context_term.get()?;
        let s: PrologText = doc_name_term.get()?;
        let as_list: bool = as_list_term.get()?;
        let compress: bool = compress_term.get()?;
        let unfold: bool = unfold_term.get()?;
        let minimize: bool = minimize_term.get()?;
        let started: bool = started_term.get()?;

        if let Some(result) = context.try_or_die(doc_context.get_document(&s, compress, unfold))? {
            if as_list && started {
                context.try_or_die_generic(stream.write_all(b",\n"))?;
            }
            context.try_or_die_generic(map_to_writer(&mut stream, result, !minimize))?;
            if !as_list {
                context.try_or_die(stream.write_all(b"\n"))?;
            }

            Ok(())
        }
        else {
            fail()
        }
    }

    #[module("$doc")]
    semidet fn print_all_documents_json_by_type(context, stream_term, get_context_term, type_term, skip_term, count_term, as_list_term, compress_term, unfold_term, minimize_term) {
        let doc_context: DocumentContextBlob = get_context_term.get()?;
        if doc_context.layer.is_none() {
            return Ok(());
        }

        let type_name: PrologText = type_term.get()?;
        let types = doc_context.get_subtypes_for(&type_name);
        let compress: bool = compress_term.get()?;
        let unfold: bool = unfold_term.get()?;
        let minimize: bool = minimize_term.get()?;

        if types.is_empty() {
            // no type found that is subsumed by the given type, so we're done
            return Ok(())
        }

        print_documents_of_types(context, &doc_context, stream_term, skip_term, count_term, as_list_term, types.as_slice(), compress, unfold, minimize)
    }

    #[module("$doc")]
    semidet fn par_print_all_documents_json_by_type(context, stream_term, get_context_term, type_term, skip_term, count_term, as_list_term, compress_term, unfold_term, minimize_term) {
        let doc_context: DocumentContextBlob = get_context_term.get()?;
        if doc_context.layer.is_none() {
            return Ok(());
        }

        let type_name: PrologText = type_term.get()?;
        let types = doc_context.get_subtypes_for(&type_name);
        let compress: bool = compress_term.get()?;
        let unfold: bool = unfold_term.get()?;
        let minimize: bool = minimize_term.get()?;

        if types.is_empty() {
            // no type found that is subsumed by the given type, so we're done
            return Ok(())
        }

        par_print_documents_of_types(context, &doc_context.0, stream_term, skip_term, count_term, as_list_term, types, compress, unfold, minimize)
    }

    #[module("$doc")]
    semidet fn print_all_documents_json(context, stream_term, get_context_term, skip_term, count_term, as_list_term, compress_term, unfold_term, minimize_term) {
        let doc_context: DocumentContextBlob = get_context_term.get()?;
        if doc_context.layer.is_none() {
            return Ok(());
        }
        let compress: bool = compress_term.get()?;
        let unfold: bool = unfold_term.get()?;
        let minimize: bool = minimize_term.get()?;

        let mut types: Vec<u64> = match unfold {
            true => doc_context.document_types.iter().cloned().collect(),
            false => doc_context.types.iter().cloned().collect()
        };
        types.sort();

        print_documents_of_types(context, &doc_context, stream_term, skip_term, count_term, as_list_term, types.iter(), compress, unfold, minimize)
    }

    #[module("$doc")]
    semidet fn par_print_all_documents_json(context, stream_term, get_context_term, skip_term, count_term, as_list_term, compress_term, unfold_term, minimize_term) {
        let doc_context: DocumentContextBlob = get_context_term.get()?;
        if doc_context.layer.is_none() {
            return Ok(());
        }
        let compress: bool = compress_term.get()?;
        let unfold: bool = unfold_term.get()?;
        let minimize: bool = minimize_term.get()?;

        // We either iterate over the document types, or if unfold is false, we iterate over all types
        let mut types: Vec<u64> = match unfold {
            true => doc_context.document_types.iter().cloned().collect(),
            false => doc_context.types.iter().cloned().collect()
        };
        types.sort();

        par_print_documents_of_types(context, &doc_context.0, stream_term, skip_term, count_term, as_list_term, types, compress, unfold, minimize)
    }

    #[module("$doc")]
    semidet fn print_documents_json_by_id(context, stream_term, get_context_term, ids_term, skip_term, count_term, as_list_term, compress_term, unfold_term, minimize_term) {
        let doc_context: DocumentContextBlob = get_context_term.get()?;
        if doc_context.layer.is_none() {
            return Ok(());
        }
        let compress: bool = compress_term.get()?;
        let unfold: bool = unfold_term.get()?;
        let minimize: bool = minimize_term.get()?;

        print_documents_by_id(context, &doc_context.0, stream_term, skip_term, count_term, as_list_term, ids_term, compress, unfold, minimize)
    }

    #[module("$doc")]
    semidet fn par_print_documents_json_by_id(context, stream_term, get_context_term, ids_term, skip_term, count_term, as_list_term, compress_term, unfold_term, minimize_term) {
        let doc_context: DocumentContextBlob = get_context_term.get()?;
        if doc_context.layer.is_none() {
            return Ok(());
        }
        let compress: bool = compress_term.get()?;
        let unfold: bool = unfold_term.get()?;
        let minimize: bool = minimize_term.get()?;

        par_print_documents_by_id(context, &doc_context.0, stream_term, skip_term, count_term, as_list_term, ids_term, compress, unfold, minimize)
    }
}

struct HeapEntry {
    index: usize,
    value: Map<String, Value>,
}

impl PartialEq for HeapEntry {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for HeapEntry {}

impl PartialOrd for HeapEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HeapEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index).reverse()
    }
}

pub fn register() {
    register_get_document_context();
    register_print_document_json();
    register_print_all_documents_json();
    register_par_print_all_documents_json();
    register_print_all_documents_json_by_type();
    register_par_print_all_documents_json_by_type();
    register_print_documents_json_by_id();
    register_par_print_documents_json_by_id();

    delete::register();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_dim_array() {
        let collected = vec![
            (vec![0], Value::Bool(true)),
            (vec![1], Value::Bool(false)),
            (vec![2], Value::Bool(true)),
        ];

        let json = collect_array(collected);
        assert_eq!(
            Value::Array(vec![
                Value::Bool(true),
                Value::Bool(false),
                Value::Bool(true),
            ]),
            json
        );
    }

    #[test]
    fn single_dim_array_start_offset() {
        let collected = vec![
            (vec![3], Value::Bool(true)),
            (vec![4], Value::Bool(false)),
            (vec![5], Value::Bool(true)),
        ];

        let json = collect_array(collected);
        assert_eq!(
            Value::Array(vec![
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Bool(true),
                Value::Bool(false),
                Value::Bool(true),
            ]),
            json
        );
    }

    #[test]
    fn single_dim_array_holes() {
        let collected = vec![
            (vec![0], Value::Bool(true)),
            (vec![3], Value::Bool(false)),
            (vec![5], Value::Bool(true)),
        ];

        let json = collect_array(collected);
        assert_eq!(
            Value::Array(vec![
                Value::Bool(true),
                Value::Null,
                Value::Null,
                Value::Bool(false),
                Value::Null,
                Value::Bool(true),
            ]),
            json
        );
    }

    #[test]
    fn double_dim_array() {
        let collected = vec![
            (vec![0, 0], Value::Bool(true)),
            (vec![0, 1], Value::Bool(false)),
            (vec![0, 2], Value::Bool(true)),
            (vec![1, 0], Value::Bool(false)),
            (vec![1, 1], Value::Bool(true)),
            (vec![1, 2], Value::Bool(false)),
        ];

        let json = collect_array(collected);
        assert_eq!(
            Value::Array(vec![
                Value::Array(vec![
                    Value::Bool(true),
                    Value::Bool(false),
                    Value::Bool(true),
                ]),
                Value::Array(vec![
                    Value::Bool(false),
                    Value::Bool(true),
                    Value::Bool(false)
                ])
            ]),
            json
        );
    }

    #[test]
    fn double_dim_array_offset() {
        let collected = vec![
            (vec![2, 3], Value::Bool(true)),
            (vec![2, 4], Value::Bool(false)),
            (vec![2, 5], Value::Bool(true)),
            (vec![3, 1], Value::Bool(false)),
            (vec![3, 2], Value::Bool(true)),
            (vec![3, 3], Value::Bool(false)),
        ];

        let json = collect_array(collected);
        assert_eq!(
            Value::Array(vec![
                Value::Null,
                Value::Null,
                Value::Array(vec![
                    Value::Null,
                    Value::Null,
                    Value::Null,
                    Value::Bool(true),
                    Value::Bool(false),
                    Value::Bool(true),
                ]),
                Value::Array(vec![
                    Value::Null,
                    Value::Bool(false),
                    Value::Bool(true),
                    Value::Bool(false)
                ])
            ]),
            json
        );
    }

    #[test]
    fn double_dim_array_holes() {
        let collected = vec![
            (vec![0, 0], Value::Bool(true)),
            (vec![0, 3], Value::Bool(false)),
            (vec![0, 5], Value::Bool(true)),
            (vec![3, 0], Value::Bool(false)),
            (vec![3, 2], Value::Bool(true)),
            (vec![3, 4], Value::Bool(false)),
            (vec![6, 0], Value::Bool(true)),
        ];

        let json = collect_array(collected);
        assert_eq!(
            Value::Array(vec![
                Value::Array(vec![
                    Value::Bool(true),
                    Value::Null,
                    Value::Null,
                    Value::Bool(false),
                    Value::Null,
                    Value::Bool(true),
                ]),
                Value::Null,
                Value::Null,
                Value::Array(vec![
                    Value::Bool(false),
                    Value::Null,
                    Value::Bool(true),
                    Value::Null,
                    Value::Bool(false),
                ]),
                Value::Null,
                Value::Null,
                Value::Array(vec![Value::Bool(true),]),
            ]),
            json
        );
    }
}
