use std::collections::{HashMap, HashSet};
use std::iter::Peekable;
use std::sync::Arc;

use crate::terminus_store::store::sync::*;
use crate::terminus_store::*;

use serde_json::map;
use serde_json::{Map, Value};
use urlencoding;

use super::consts::*;
use super::prefix::PrefixContracter;
use super::schema::*;
use super::value::*;

use swipl::prelude::*;

pub struct GetDocumentContext<L: Layer> {
    layer: L,
    prefixes: PrefixContracter,
    terminators: HashSet<u64>,
    enums: HashMap<u64, String>,
    set_pairs: HashSet<(u64, u64)>,
    rdf_type_id: Option<u64>,
    rdf_first_id: Option<u64>,
    rdf_rest_id: Option<u64>,
    rdf_nil_id: Option<u64>,
    rdf_list_id: Option<u64>,
    sys_index_ids: Vec<u64>,
    sys_array_id: Option<u64>,
    sys_value_id: Option<u64>,
}

impl<L: Layer> GetDocumentContext<L> {
    #[inline(never)]
    pub fn new<SL: Layer>(schema: &SL, instance: L) -> GetDocumentContext<L> {
        let schema_type_ids = get_document_type_ids_from_schema(schema);
        let terminators: HashSet<u64> =
            schema_to_instance_types(schema, &instance, schema_type_ids).collect();

        let schema_enum_ids = get_enum_ids_from_schema(schema);
        let mut enums = HashMap::new();
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

        let prefixes = prefix_contracter_from_schema_layer(schema);

        let schema_set_pairs = get_set_pairs_from_schema(schema);
        let mut set_pairs = HashSet::new();
        for (schema_type_id, schema_predicate_id) in schema_set_pairs {
            if let Some(type_id) = translate_subject_id(schema, &instance, schema_type_id) {
                if let Some(predicate_id) =
                    translate_predicate_id(schema, &instance, schema_predicate_id)
                {
                    set_pairs.insert((type_id, predicate_id));
                }
            }
        }

        let rdf_type_id = instance.predicate_id(RDF_TYPE);
        let rdf_first_id = instance.predicate_id(RDF_FIRST);
        let rdf_rest_id = instance.predicate_id(RDF_REST);
        let rdf_nil_id = instance.object_node_id(RDF_NIL);
        let rdf_list_id = instance.object_node_id(RDF_LIST);

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

        let sys_array_id = instance.object_node_id(SYS_ARRAY);
        let sys_value_id = instance.predicate_id(SYS_VALUE);

        GetDocumentContext {
            layer: instance,
            prefixes,
            terminators,
            enums,
            set_pairs,
            rdf_type_id,
            rdf_first_id,
            rdf_rest_id,
            rdf_nil_id,
            rdf_list_id,
            sys_index_ids,
            sys_array_id,
            sys_value_id,
        }
    }

    #[inline(never)]
    pub fn get_document(&self, iri: &str) -> Option<Map<String, Value>> {
        if let Some(id) = self.layer.subject_id(iri) {
            Some(self.get_id_document(id))
        } else {
            None
        }
    }

    #[inline(never)]
    fn get_field(&self, object: u64) -> Result<Value, StackEntry<L>> {
        if let Some(val) = self.enums.get(&object) {
            Ok(Value::String(val.clone()))
        } else if Some(object) == self.rdf_nil_id {
            Ok(Value::Array(vec![]))
        } else if self.layer.id_object_is_node(object).unwrap() {
            // this might not be a terminator, but we won't know until trying to get a list or doc stub
            if let Some(list_iter) = self.get_list_iter(object) {
                Err(StackEntry::List {
                    collect: Vec::new(),
                    entries: list_iter,
                })
            } else {
                match self.get_doc_stub(object, true) {
                    // it's not a terminator so we will need to descend into it. That is, we would need to descend into it if there were any children, so let's check.
                    Ok((doc, type_id, fields)) => Err(StackEntry::Document {
                        doc,
                        type_id,
                        fields: Some(fields),
                    }),
                    // it turns out it is a terminator after all, so we just use the id as a direct value
                    Err(s) => Ok(Value::String(s)),
                }
            }
        } else {
            // This is a terminator, so we're gonna directly get the field
            match self.layer.id_object(object).unwrap() {
                ObjectType::Node(n) => {
                    let n_contracted = self.prefixes.instance_contract(&n).to_string();
                    Ok(Value::String(n_contracted))
                }
                ObjectType::Value(v) => Ok(value_string_to_json(&v)),
            }
        }
    }

    #[inline(never)]
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
                let mut v = e.get_mut();
                match v {
                    Value::Array(a) => {
                        a.push(value);
                    }
                    _ => {
                        let mut a = Vec::new();
                        let mut old_v = Value::Null;
                        std::mem::swap(&mut old_v, &mut v);
                        a.push(old_v);
                        a.push(value);
                        *v = Value::Array(a);
                    }
                }
            }
        }
    }

    #[inline(never)]
    fn get_list_iter(&self, id: u64) -> Option<Peekable<RdfListIterator<L>>> {
        if let Some(rdf_type_id) = self.rdf_type_id {
            if let Some(t) = self.layer.triples_sp(id, rdf_type_id).next() {
                if Some(t.object) == self.rdf_list_id {
                    return Some(
                        RdfListIterator {
                            layer: &self.layer,
                            cur: id,
                            rdf_first_id: self.rdf_first_id,
                            rdf_rest_id: self.rdf_rest_id,
                            rdf_nil_id: self.rdf_nil_id,
                        }
                        .peekable(),
                    );
                }
            }
        }
        None
    }

    #[inline(never)]
    fn get_array_iter(&self, id: u64, stack_entry: &mut StackEntry<L>) -> Option<ArrayIterator<L>> {
        if let StackEntry::Document { fields, .. } = stack_entry {
            if let Some(rdf_type_id) = self.rdf_type_id {
                if let Some(t) = self.layer.triples_sp(id, rdf_type_id).next() {
                    if Some(t.object) == self.sys_array_id {
                        let mut it = None;
                        std::mem::swap(&mut it, fields);
                        let mut it = it.unwrap();
                        let t = it.peek().unwrap();
                        let subject = t.subject;
                        let predicate = t.predicate;
                        return Some(ArrayIterator {
                            layer: &self.layer,
                            it,
                            subject,
                            predicate,
                            last_index: None,
                            sys_index_ids: &self.sys_index_ids,
                            sys_value_id: self.sys_value_id,
                        });
                    }
                }
            }
        }

        None
    }

    #[inline(never)]
    fn get_doc_stub(
        &self,
        id: u64,
        terminate: bool,
    ) -> Result<
        (
            Map<String, Value>,
            Option<u64>,
            Peekable<Box<dyn Iterator<Item = IdTriple> + Send>>,
        ),
        String,
    > {
        let id_name = self.layer.id_subject(id).unwrap();
        let id_name_contracted = self.prefixes.instance_contract(&id_name).to_string();

        let rdf_type_id = self.rdf_type_id;
        let mut fields = (Box::new(
            self.layer
                .triples_s(id)
                .filter(move |f| Some(f.predicate) != rdf_type_id),
        ) as Box<dyn Iterator<Item = IdTriple> + Send>)
            .peekable();

        let mut type_id = None;
        let mut type_name_contracted: Option<String> = None;
        if let Some(rdf_type_id) = self.rdf_type_id {
            if let Some(t) = self.layer.triples_sp(id, rdf_type_id).next() {
                if terminate && self.terminators.contains(&t.object) {
                    return Err(id_name_contracted);
                }
                type_id = Some(t.object);
                let type_name = self.layer.id_object_node(t.object).unwrap();
                type_name_contracted = Some(self.prefixes.schema_contract(&type_name).to_string());
            }
        }

        if type_name_contracted.is_none() && fields.peek().is_none() {
            // we're actually dealing with a raw id here
            Err(id_name_contracted)
        } else {
            let mut result = Map::new();
            result.insert("@id".to_string(), Value::String(id_name_contracted));
            if let Some(tn) = type_name_contracted {
                result.insert("@type".to_string(), Value::String(tn));
            }

            Ok((result, type_id, fields))
        }
    }

    #[inline(never)]
    pub fn get_id_document(&self, id: u64) -> Map<String, Value> {
        let mut stack = Vec::new();

        let (doc, type_id, fields) = self.get_doc_stub(id, false).unwrap();
        stack.push(StackEntry::Document {
            doc,
            type_id,
            fields: Some(fields),
        });

        loop {
            let cur = stack.last_mut().unwrap();
            if let Some(next_obj) = cur.peek() {
                if let Some(array_iter) = self.get_array_iter(next_obj, cur) {
                    stack.push(StackEntry::Array(ArrayStackEntry {
                        collect: Vec::new(),
                        entries: array_iter,
                    }));
                } else {
                    match self.get_field(next_obj) {
                        Ok(val) => {
                            cur.integrate_value(self, val);
                        }
                        Err(entry) => {
                            // We need to iterate deeper, so add it to the stack without iterating past the field.
                            stack.push(entry);
                        }
                    }
                }
            } else {
                // done!
                let cur = stack.pop().unwrap();
                if let Some(parent) = stack.last_mut() {
                    parent.integrate(self, cur);
                } else {
                    // we're done, this was the root, time to return!
                    match cur {
                        StackEntry::Document { doc, .. } => return doc,
                        _ => panic!("unexpected element at stack top"),
                    }
                }
            }
        }
    }
}

enum StackEntry<'a, L: Layer> {
    Document {
        doc: Map<String, Value>,
        type_id: Option<u64>,
        fields: Option<Peekable<Box<dyn Iterator<Item = IdTriple> + Send>>>,
    },
    List {
        collect: Vec<Value>,
        entries: Peekable<RdfListIterator<'a, L>>,
    },
    Array(ArrayStackEntry<'a, L>),
}

struct ArrayStackEntry<'a, L: Layer> {
    collect: Vec<(Vec<usize>, Value)>,
    entries: ArrayIterator<'a, L>,
}

struct ArrayIterator<'a, L: Layer> {
    layer: &'a L,
    it: Peekable<Box<dyn Iterator<Item = IdTriple> + Send>>,
    subject: u64,
    predicate: u64,
    last_index: Option<Vec<usize>>,
    sys_index_ids: &'a [u64],
    sys_value_id: Option<u64>,
}

impl<'a, L: Layer> Iterator for ArrayIterator<'a, L> {
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        if let Some(t) = self.it.peek() {
            if t.subject == self.subject && t.predicate == self.predicate {
                let mut indexes = Vec::new();
                for index_id in self.sys_index_ids {
                    if let Some(index_triple) = self.layer.triples_sp(t.object, *index_id).next() {
                        let index_value = self.layer.id_object_value(index_triple.object).unwrap();
                        let index = value_string_to_usize(&index_value);
                        indexes.push(index);
                    } else {
                        // no more indexes to come
                        break;
                    }
                }
                indexes.reverse();

                let value_id = self
                    .layer
                    .triples_sp(t.object, *self.sys_value_id.as_ref().unwrap())
                    .next()
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

impl<'a, L: Layer> StackEntry<'a, L> {
    #[inline(never)]
    fn peek(&mut self) -> Option<u64> {
        match self {
            Self::Document { fields, .. } => fields.as_mut().unwrap().peek().map(|t| t.object),
            Self::List { entries, .. } => entries.peek().map(|x| *x),
            Self::Array(a) => a.entries.next(),
        }
    }

    #[inline(never)]
    fn into_value(self) -> Value {
        match self {
            Self::Document { doc, .. } => Value::Object(doc),
            Self::List { collect, .. } => Value::Array(collect),
            Self::Array { .. } => panic!("cannot directly turn array into a value"),
        }
    }

    #[inline(never)]
    fn integrate(&mut self, context: &GetDocumentContext<L>, child: StackEntry<'a, L>) {
        match child {
            StackEntry::Array(a) => self.integrate_array(context, a),
            _ => self.integrate_value(context, child.into_value()),
        }
    }

    #[inline(never)]
    fn integrate_array(&mut self, context: &GetDocumentContext<L>, array: ArrayStackEntry<'a, L>) {
        // self has to be an object, let's make sure of that.
        match self {
            Self::Document { doc, fields, .. } => {
                *fields = Some(array.entries.it);

                let value = collect_array(array.collect);

                let p_name = context.layer.id_predicate(array.entries.predicate).unwrap();
                let p_name_contracted = context.prefixes.schema_contract(&p_name).to_string();
                context.add_field(doc, &p_name_contracted, value, false);
            }
            _ => panic!("unexpected parent type of array"),
        }
    }

    #[inline(never)]
    fn integrate_value(&mut self, context: &GetDocumentContext<L>, value: Value) {
        match self {
            Self::Document {
                doc,
                fields,
                type_id,
            } => {
                // we previously peeked a field and decided we needed to recurse deeper.
                // this is the time to pop it.
                let t = fields.as_mut().unwrap().next().unwrap();
                let is_set = type_id
                    .map(|type_id| context.set_pairs.contains(&(type_id, t.predicate)))
                    .unwrap_or(false);
                let p_name = context.layer.id_predicate(t.predicate).unwrap();
                let p_name_contracted = context.prefixes.schema_contract(&p_name).to_string();
                context.add_field(doc, &p_name_contracted, value, is_set);
            }
            Self::List { collect, entries } => {
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

#[inline(never)]
fn collect_array(mut elements: Vec<(Vec<usize>, Value)>) -> Value {
    elements.sort_by(|(i1, _), (i2, _)| i1.cmp(i2));

    let dimensions = elements[0].0.len();
    let mut collect: Vec<Vec<Value>> = Vec::with_capacity(dimensions);
    collect.resize_with(dimensions, || Vec::new());

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
        if x.len() != 0 {
            collect[d].push(Value::Array(x));
        }
    }

    Value::Array(collect.pop().unwrap())
}

wrapped_arc_blob!(
    "GetDocumentContext",
    GetDocumentContextBlob,
    GetDocumentContext<SyncStoreLayer>,
    defaults
);

#[inline(never)]
fn map_to_string(m: Map<String, Value>) -> String {
    Value::Object(m).to_string()
}

#[inline(never)]
fn unify_json_string(term: &Term, s: String) -> PrologResult<()> {
    term.unify(s)
}

use super::types::*;
predicates! {
    #[module("$moo")]
    semidet fn get_document_context(context, transaction_term, context_term) {
        let schema_layer = transaction_schema_layer(context, transaction_term)?.unwrap();
        let instance_layer = transaction_instance_layer(context, transaction_term)?.unwrap();

        let get_context = GetDocumentContext::new(&schema_layer, instance_layer);

        context_term.unify(GetDocumentContextBlob(Arc::new(get_context)))
    }

    #[module("$moo")]
    semidet fn get_document_json_string(_context, get_context_term, doc_name_term, doc_json_string_term) {
        if !doc_name_term.is_string() && !doc_name_term.is_atom() {
            return fail();
        }

        let context: GetDocumentContextBlob = get_context_term.get()?;
        let s: PrologText = doc_name_term.get()?;
        if let Some(result) = context.get_document(&s) {
            unify_json_string(&doc_json_string_term, map_to_string(result))
        }
        else {
            fail()
        }
    }

    #[module("$moo")]
    semidet fn print_prefix_tree(_context, get_context_term) {
        let get_context: GetDocumentContextBlob = get_context_term.get()?;

        println!("{:?}", get_context.prefixes);

        Ok(())
    }

    #[module("$moo")]
    semidet fn prefix_schema_contract(_context, get_context_term, expanded_term, contracted_term) {
        let expanded: String = expanded_term.get()?;
        let get_context: GetDocumentContextBlob = get_context_term.get()?;

        let contracted = get_context.prefixes.schema_contract(&expanded);

        contracted_term.unify(&*contracted)
    }

    #[module("$moo")]
    semidet fn prefix_instance_contract(_context, get_context_term, expanded_term, contracted_term) {
        let expanded: String = expanded_term.get()?;
        let get_context: GetDocumentContextBlob = get_context_term.get()?;

        let contracted = get_context.prefixes.instance_contract(&expanded);

        contracted_term.unify(&*contracted)
    }
}

pub fn register() {
    register_get_document_context();
    register_get_document_json_string();
    register_prefix_schema_contract();
    register_prefix_instance_contract();
    register_print_prefix_tree();
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
