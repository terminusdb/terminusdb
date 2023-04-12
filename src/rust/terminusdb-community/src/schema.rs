use crate::terminus_store::layer::*;
use lazy_init::Lazy;

use std::collections::{HashMap, HashSet};

use super::consts::*;
use super::prefix::*;

pub struct SchemaQueryContext<'a, L: Layer> {
    layer: &'a L,
    inheritance_graph: Lazy<InheritanceGraph>,
    reverse_inheritance_graph: Lazy<ReverseInheritanceGraph>,
}

impl<'a, L: Layer> SchemaQueryContext<'a, L> {
    pub fn new(layer: &'a L) -> Self {
        Self {
            layer,
            inheritance_graph: Lazy::new(),
            reverse_inheritance_graph: Lazy::new(),
        }
    }

    fn get_inheritance_graph_(&self) -> InheritanceGraph {
        if let Some(inherits_id) = self.layer.predicate_id(SYS_INHERITS) {
            let mut result = HashMap::new();
            for triple in self.layer.triples_p(inherits_id) {
                let entry = result.entry(triple.subject).or_insert_with(HashSet::new);
                entry.insert(triple.object);
            }

            InheritanceGraph(result)
        } else {
            InheritanceGraph(HashMap::with_capacity(0))
        }
    }

    pub fn get_inheritance_graph(&self) -> &InheritanceGraph {
        self.inheritance_graph
            .get_or_create(|| self.get_inheritance_graph_())
    }

    fn get_reverse_inheritance_graph_(&self) -> ReverseInheritanceGraph {
        if let Some(inherits_id) = self.layer.predicate_id(SYS_INHERITS) {
            let mut result = HashMap::new();
            for triple in self.layer.triples_p(inherits_id) {
                let entry = result.entry(triple.object).or_insert_with(HashSet::new);
                entry.insert(triple.subject);
            }

            ReverseInheritanceGraph(result)
        } else {
            ReverseInheritanceGraph(HashMap::with_capacity(0))
        }
    }

    pub fn get_reverse_inheritance_graph(&self) -> &ReverseInheritanceGraph {
        self.reverse_inheritance_graph
            .get_or_create(|| self.get_reverse_inheritance_graph_())
    }

    fn get_subdocument_ids_from_schema(&self) -> HashSet<u64> {
        let mut result = HashSet::new();
        let inheritance = self.get_reverse_inheritance_graph();
        let mut work: Vec<_> = get_direct_subdocument_ids_from_schema(self.layer).collect();
        while let Some(cur) = work.pop() {
            if !result.insert(cur) {
                // we already found this type.
                continue;
            }

            if let Some(children) = inheritance.get(&cur) {
                work.extend(children);
            }
        }

        result
    }

    pub fn get_unfoldable_ids_from_schema(&self) -> HashSet<u64> {
        let mut result = HashSet::new();
        let inheritance = self.get_reverse_inheritance_graph();
        let mut work: Vec<_> = get_direct_unfoldable_ids_from_schema(self.layer).collect();
        while let Some(cur) = work.pop() {
            if !result.insert(cur) {
                // we already found this type.
                continue;
            }

            if let Some(children) = inheritance.get(&cur) {
                work.extend(children);
            }
        }

        result
    }

    pub fn get_set_pairs_from_schema(&'_ self) -> impl Iterator<Item = (u64, u64)> + '_ {
        let sys_set_id = self.layer.object_node_id(SYS_SET);
        if sys_set_id.is_none() {
            return itertools::Either::Left(std::iter::empty());
        }
        let sys_set_id = sys_set_id.unwrap();

        let rdf_type_id = self.layer.predicate_id(RDF_TYPE);
        if rdf_type_id.is_none() {
            return itertools::Either::Left(std::iter::empty());
        }
        let rdf_type_id = rdf_type_id.unwrap();

        let inheritance_graph = self.get_reverse_inheritance_graph();
        itertools::Either::Right(
            self.layer
                .triples_o(sys_set_id)
                .filter(move |t| t.predicate == rdf_type_id)
                .flat_map(move |t| {
                    let set_origin = self
                        .layer
                        .triples_o(t.subject)
                        .next()
                        .expect("Expected set to be connected");

                    let mut subjects: Vec<u64> = vec![set_origin.subject];
                    let predicate = set_origin.predicate;
                    let mut r = Vec::new();

                    while let Some(s) = subjects.pop() {
                        r.push((s, predicate));
                        if let Some(children) = inheritance_graph.get(&s) {
                            subjects.extend(children.iter());
                        }
                    }

                    r.into_iter()
                }),
        )
    }

    pub fn get_document_type_ids_from_schema(&self) -> impl Iterator<Item = u64> {
        let subdocument_ids = self.get_subdocument_ids_from_schema();

        self.get_type_ids_from_schema()
            .filter(move |t| !subdocument_ids.contains(t))
    }

    pub fn get_enum_ids_from_schema(&self) -> HashSet<u64> {
        let mut result = HashSet::new();
        let sys_enum_id = self.layer.object_node_id(SYS_ENUM);
        let rdf_type_id = self.layer.predicate_id(RDF_TYPE);
        let rdf_first_id = self.layer.predicate_id(RDF_FIRST);
        let rdf_rest_id = self.layer.predicate_id(RDF_REST);
        let rdf_nil_id = self.layer.object_node_id(RDF_NIL);

        if sys_enum_id.is_none() || rdf_type_id.is_none() {
            return result;
        }
        let sys_enum_id = sys_enum_id.unwrap();
        let rdf_type_id = rdf_type_id.unwrap();

        for t in self.layer.triples_o(sys_enum_id) {
            if t.predicate == rdf_type_id {
                // we found an enum type!
                // it is going to have a value field pointing at a list of possible enum values
                let sys_value_id = self.layer.predicate_id(SYS_VALUE).unwrap();
                let value_list_id = self
                    .layer
                    .single_triple_sp(t.subject, sys_value_id)
                    .unwrap()
                    .object;

                result.extend(RdfListIterator {
                    rdf_first_id,
                    rdf_rest_id,
                    rdf_nil_id,
                    layer: self.layer,
                    cur: value_list_id,
                });
            }
        }

        result
    }

    pub fn get_type_ids_from_schema(&self) -> impl Iterator<Item = u64> {
        let type_id_opt = self.layer.predicate_id(RDF_TYPE);
        if type_id_opt.is_none() {
            // no rdf:type? then there cannot be any type definitions.
            return itertools::Either::Left(std::iter::empty());
        }

        let type_id = type_id_opt.unwrap();
        let class_id = self.layer.object_node_id(SYS_CLASS);
        let tagged_union_id = self.layer.object_node_id(SYS_TAGGED_UNION);
        let foreign_id = self.layer.object_node_id(SYS_FOREIGN);

        itertools::Either::Right(
            self.layer
                .triples_p(type_id)
                .filter(move |t| {
                    Some(t.object) == class_id
                        || Some(t.object) == tagged_union_id
                        || Some(t.object) == foreign_id
                })
                .map(|t| t.subject),
        )
    }

    pub fn get_subtype_ids(&self) -> HashMap<u64, HashSet<u64>> {
        let inheritance = self.get_inheritance_graph();
        let mut result = HashMap::new();

        for (sub, supers) in inheritance.iter() {
            let mut visited = HashSet::new();
            let mut supers: Vec<&u64> = supers.iter().collect();
            while let Some(sup) = supers.pop() {
                if !visited.contains(&sup) {
                    visited.insert(sup);
                    let entry = result.entry(*sup).or_insert_with(HashSet::new);
                    entry.insert(*sub);
                    if let Some(more_supers) = inheritance.get(sup) {
                        supers.extend(more_supers.iter());
                    }
                }
            }
        }

        result
    }

    pub fn translate_subject_id<L2: Layer>(&self, layer2: &L2, id: u64) -> Option<u64> {
        let subject = self.layer.id_subject(id).unwrap();
        layer2.subject_id(&subject)
    }

    pub fn translate_predicate_id<L2: Layer>(&self, layer2: &L2, id: u64) -> Option<u64> {
        let predicate = self.layer.id_predicate(id).unwrap();
        layer2.predicate_id(&predicate)
    }

    #[allow(dead_code)]
    pub fn translate_object_id<L2: Layer>(&self, layer2: &L2, id: u64) -> Option<u64> {
        let object = self.layer.id_object(id).unwrap();
        match object {
            ObjectType::Node(n) => layer2.object_node_id(&n),
            ObjectType::Value(v) => layer2.object_value_id(&v),
        }
    }

    pub fn schema_to_instance_types<'b, L2: 'b + Layer, I: 'b + IntoIterator<Item = u64>>(
        &'b self,
        instance_layer: &'b L2,
        type_iter: I,
    ) -> impl Iterator<Item = u64> + 'b {
        type_iter
            .into_iter()
            .filter_map(move |t| self.translate_subject_id(instance_layer, t))
    }
}

fn get_direct_subdocument_ids_from_schema<L: Layer>(layer: &L) -> impl Iterator<Item = u64> {
    if let Some(subdocument_id) = layer.predicate_id(SYS_SUBDOCUMENT) {
        itertools::Either::Left(layer.triples_p(subdocument_id).map(|t| t.subject))
    } else {
        itertools::Either::Right(std::iter::empty())
    }
}

fn get_direct_unfoldable_ids_from_schema<L: Layer>(layer: &L) -> impl Iterator<Item = u64> {
    if let Some(unfoldable_id) = layer.predicate_id(SYS_UNFOLDABLE) {
        itertools::Either::Left(layer.triples_p(unfoldable_id).map(|t| t.subject))
    } else {
        itertools::Either::Right(std::iter::empty())
    }
}

pub struct RdfListIterator<'a, L: Layer> {
    pub layer: &'a L,
    pub cur: u64,
    pub rdf_first_id: Option<u64>,
    pub rdf_rest_id: Option<u64>,
    pub rdf_nil_id: Option<u64>,
}

impl<'a, L: Layer> Iterator for RdfListIterator<'a, L> {
    type Item = u64;
    fn next(&mut self) -> Option<u64> {
        if Some(self.cur) == self.rdf_nil_id {
            None
        } else {
            let rdf_first_id = self
                .rdf_first_id
                .expect("expected cons cell to have a first but id not found");
            let rdf_rest_id = self
                .rdf_rest_id
                .expect("expected cons cell to have a rest but id not found");
            let first_id = self
                .layer
                .single_triple_sp(self.cur, rdf_first_id)
                .expect("expected cons cell to have a first");
            let rest_id = self
                .layer
                .single_triple_sp(self.cur, rdf_rest_id)
                .expect("expected cons cell to have a rest");

            self.cur = rest_id.object;
            Some(first_id.object)
        }
    }
}

#[derive(Debug)]
pub struct InheritanceGraph(HashMap<u64, HashSet<u64>>);
#[derive(Debug)]
pub struct ReverseInheritanceGraph(HashMap<u64, HashSet<u64>>);

impl std::ops::Deref for InheritanceGraph {
    type Target = HashMap<u64, HashSet<u64>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::Deref for ReverseInheritanceGraph {
    type Target = HashMap<u64, HashSet<u64>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn prefix_contracter_from_schema_layer<L: Layer>(schema: &L) -> PrefixContracter {
    let context_id = schema.subject_id(TDB_CONTEXT);
    let base_id = schema.predicate_id(SYS_BASE);
    let schema_id = schema.predicate_id(SYS_SCHEMA);
    let prefix_pair_id = schema.predicate_id(SYS_PREFIX_PAIR);
    let prefix_id = schema.predicate_id(SYS_PREFIX);
    let url_id = schema.predicate_id(SYS_URL);

    let mut prefixes: Vec<Prefix> = Vec::new();

    if let (Some(context_id), Some(base_id), Some(schema_id)) = (context_id, base_id, schema_id) {
        let base_expansion_id = schema.single_triple_sp(context_id, base_id).unwrap().object;
        let schema_expansion_id = schema
            .single_triple_sp(context_id, schema_id)
            .unwrap()
            .object;

        if let ObjectType::Value(base_expansion) = schema.id_object(base_expansion_id).unwrap() {
            let base_expansion_sub = base_expansion.as_val::<String, String>();
            prefixes.push(Prefix::base(&base_expansion_sub));
        } else {
            panic!("unexpected node type for base");
        }
        if let ObjectType::Value(schema_expansion) = schema.id_object(schema_expansion_id).unwrap()
        {
            let schema_expansion_sub = schema_expansion.as_val::<String, String>();
            prefixes.push(Prefix::schema(&schema_expansion_sub));
        } else {
            panic!("unexpected node type for schema");
        }

        if let Some(prefix_pair_id) = prefix_pair_id {
            // these next 2 will exist if any prefix is found
            let prefix_id = prefix_id.unwrap();
            let url_id = url_id.unwrap();

            for t in schema.triples_sp(context_id, prefix_pair_id) {
                let contraction_id = schema.single_triple_sp(t.object, prefix_id).unwrap().object;
                let expansion_id = schema.single_triple_sp(t.object, url_id).unwrap().object;

                if let (ObjectType::Value(contraction), ObjectType::Value(expansion)) = (
                    schema.id_object(contraction_id).unwrap(),
                    schema.id_object(expansion_id).unwrap(),
                ) {
                    let contraction_sub = contraction.as_val::<String, String>();
                    let expansion_sub = expansion.as_val::<String, String>();
                    prefixes.push(Prefix::other(&contraction_sub, &expansion_sub));
                }
            }
        }

        PrefixContracter::new(prefixes)
    } else {
        // TODO proper error
        panic!("invalid schema");
    }
}
