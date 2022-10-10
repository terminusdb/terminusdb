use std::sync::Arc;

use juniper::meta::{DeprecationStatus, EnumValue, Field};
use juniper::{
    DefaultScalarValue, FromInputValue, GraphQLEnum, GraphQLType, GraphQLValue, InputValue,
    Registry, Value, ID,
};
use swipl::prelude::*;
use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;
use terminusdb_store_prolog::terminus_store::{IdTriple, Layer};

use crate::consts::{RDF_FIRST, RDF_NIL, RDF_REST, SYS_VALUE};
use crate::doc::{retrieve_all_index_ids, ArrayIterator};
use crate::schema::RdfListIterator;
use crate::types::{transaction_instance_layer, transaction_schema_layer};
use crate::value::{
    enum_node_to_value, type_is_bool, type_is_float, type_is_integer, value_string_to_graphql,
};

use super::frame::*;
use super::query::run_filter_query;
use super::top::System;

macro_rules! execute_prolog {
    ($context:ident, $body:block) => {{
        let result: PrologResult<_> = { $body };

        result_to_string_result($context, result).map_err(|e| match e {
            PrologStringError::Failure => FieldError::new("prolog call failed", Value::Null),
            PrologStringError::Exception(e) => FieldError::new(e, Value::Null),
        })
    }};
}

pub struct SystemInfo {
    pub user: Atom,
    pub system: SyncStoreLayer,
    pub commit: Option<SyncStoreLayer>,
    pub meta: Option<SyncStoreLayer>,
}

pub struct TerminusContext<'a, C: QueryableContextType> {
    pub context: &'a Context<'a, C>,
    pub system_info: SystemInfo,
    pub schema: SyncStoreLayer,
    pub instance: Option<SyncStoreLayer>,
}

impl<'a, C: QueryableContextType> TerminusContext<'a, C> {
    pub fn new(
        context: &'a Context<'a, C>,
        _auth_term: &Term,
        system_term: &Term,
        meta_term: &Term,
        commit_term: &Term,
        transaction_term: &Term,
    ) -> PrologResult<TerminusContext<'a, C>> {
        let user_: Atom = Atom::new("terminusdb://system/data/User/admin"); //auth_term.get_ex()?;
        let user;
        if user_ == atom!("anonymous") {
            user = atom!("terminusdb://system/data/User/anonymous");
        } else {
            user = user_;
        }
        let system =
            transaction_instance_layer(context, system_term)?.expect("system layer not found");
        let meta = if meta_term.unify(atomable("none")).is_ok() {
            None
        } else {
            transaction_instance_layer(context, meta_term).expect("Missing meta layer")
        };
        let commit = if commit_term.unify(atomable("none")).is_ok() {
            None
        } else {
            transaction_instance_layer(context, commit_term).expect("Missing commit layer")
        };

        let schema =
            transaction_schema_layer(context, transaction_term)?.expect("missing schema layer");
        let instance = transaction_instance_layer(context, transaction_term)?;

        Ok(TerminusContext {
            system_info: SystemInfo {
                user,
                system,
                meta,
                commit,
            },
            context,
            schema,
            instance,
        })
    }
}

pub struct TerminusTypeCollection<'a, C: QueryableContextType> {
    _c: std::marker::PhantomData<&'a Context<'a, C>>,
}

impl<'a, C: QueryableContextType> TerminusTypeCollection<'a, C> {
    pub fn new() -> Self {
        Self {
            _c: Default::default(),
        }
    }
}

fn add_arguments<'r>(
    info: &(String, Arc<AllFrames>),
    registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    mut field: Field<'r, DefaultScalarValue>,
    class_definition: &ClassDefinition,
    all_frames: &AllFrames,
) -> Field<'r, DefaultScalarValue> {
    field = field.argument(registry.arg::<Option<ID>>("id", &()));
    field = field.argument(
        registry
            .arg::<Option<i32>>("offset", &())
            .description("skip N elements"),
    );
    field = field.argument(
        registry
            .arg::<Option<i32>>("limit", &())
            .description("limit results to N elements"),
    );
    field = field.argument(
        registry
            .arg::<Option<TerminusOrderBy>>(
                "orderBy",
                &(
                    format!("{}_Ordering", info.0),
                    info.0.to_string(),
                    info.1.clone(),
                ),
            )
            .description("order by the given fields"),
    );
    for (name, f) in class_definition.fields().iter() {
        if is_reserved_argument_name(name) {
            // these are special. we're generating them differently
            continue;
        }
        if let Some(t) = f.base_type() {
            if type_is_bool(t) {
                field = field.argument(registry.arg::<Option<bool>>(name, &()));
            } else if type_is_integer(t) {
                field = field.argument(registry.arg::<Option<i32>>(name, &()));
            } else if type_is_float(t) {
                field = field.argument(registry.arg::<Option<f64>>(name, &()));
            } else {
                field = field.argument(registry.arg::<Option<String>>(name, &()));
            }
        } else if let Some(t) = f.enum_type(all_frames) {
            field = field.argument(
                registry.arg::<Option<TerminusEnum>>(name, &(t.to_string(), info.1.clone())),
            );
        }
    }

    field
}

pub fn is_reserved_argument_name(name: &String) -> bool {
    name == "offset" || name == "limit" || name == "orderBy" || is_reserved_field_name(name)
}

pub fn is_reserved_field_name(name: &String) -> bool {
    name == "id"
}

impl<'a, C: QueryableContextType + 'a> GraphQLType for TerminusTypeCollection<'a, C> {
    fn name(_info: &Self::TypeInfo) -> Option<&str> {
        Some("Query")
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let mut fields: Vec<_> = info
            .frames
            .iter()
            .map(|(name, typedef)| {
                if typedef.kind() == TypeKind::Enum {
                    registry.field::<TerminusEnum>(name, &(name.to_owned(), info.clone()))
                } else if let TypeDefinition::Class(c) = typedef {
                    let field = registry
                        .field::<Vec<TerminusType<'a, C>>>(name, &(name.to_owned(), info.clone()));

                    add_arguments(&(name.to_owned(), info.clone()), registry, field, c, info)
                } else {
                    panic!("unexpected type kind");
                }
            })
            .collect();

        fields.push(registry.field::<System>("_system", &()));
        registry
            .build_object_type::<TerminusTypeCollection<'a, C>>(info, &fields)
            .into_meta()
    }
}

impl<'a, C: QueryableContextType> GraphQLValue for TerminusTypeCollection<'a, C> {
    type Context = TerminusContext<'a, C>;

    type TypeInfo = Arc<AllFrames>;

    fn type_name<'i>(&self, _info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some("TerminusTypeCollection")
    }

    fn resolve_field(
        &self,
        info: &Self::TypeInfo,
        field_name: &str,
        arguments: &juniper::Arguments<DefaultScalarValue>,
        executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult<DefaultScalarValue> {
        /*
        let get_object_iterator = || {
            let instance = executor.context().instance.as_ref()?;
            let rdf_type_id = instance.predicate_id(RDF_TYPE)?;
            let type_name_expanded = info.context.expand_schema(field_name);
            let type_name_id = instance.subject_id(&type_name_expanded)?;

            Some(instance.triples_o(type_name_id)
                 .filter(move |t|t.predicate == rdf_type_id)
                 .map(|t|TerminusType::new(t.subject)))
        };

        let objects: Vec<_> = get_object_iterator().into_iter()
            .flatten().collect();
        */
        if field_name == "_system" {
            executor.resolve_with_ctx(&(), &System {})
        } else {
            let objects = match executor.context().instance.as_ref() {
                Some(instance) => {
                    run_filter_query(instance, &info.context, arguments, field_name, info, None)
                        .into_iter()
                        .map(|id| TerminusType::new(id))
                        .collect()
                }
                None => vec![],
            };

            executor.resolve(&(field_name.to_owned(), info.clone()), &objects)
        }
    }
}

pub struct TerminusType<'a, C: QueryableContextType> {
    id: u64,
    _x: std::marker::PhantomData<Context<'a, C>>,
}

impl<'a, C: QueryableContextType + 'a> TerminusType<'a, C> {
    fn new(id: u64) -> Self {
        Self {
            id,
            _x: Default::default(),
        }
    }

    fn register_field<'r, T: GraphQLType>(
        registry: &mut Registry<'r, DefaultScalarValue>,
        field_name: &str,
        type_info: &T::TypeInfo,
        kind: FieldKind,
    ) -> Field<'r, DefaultScalarValue> {
        match kind {
            FieldKind::Required => registry.field::<T>(field_name, type_info),
            FieldKind::Optional => registry.field::<Option<T>>(field_name, type_info),
            FieldKind::Array => registry.field::<Vec<Option<T>>>(field_name, type_info),
            _ => registry.field::<Vec<T>>(field_name, type_info),
        }
    }

    fn generate_class_type<'r>(
        d: &ClassDefinition,
        info: &<Self as GraphQLValue>::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let frames = &info.1;
        let mut fields: Vec<_> = d
            .fields()
            .iter()
            .filter_map(|(field_name, field_definition)| {
                if is_reserved_field_name(field_name) {
                    return None;
                }
                Some(
                    if let Some(document_type) = field_definition.document_type(frames) {
                        let field = Self::register_field::<TerminusType<'a, C>>(
                            registry,
                            field_name,
                            &(document_type.to_owned(), frames.clone()),
                            field_definition.kind(),
                        );

                        if field_definition.kind().is_collection() {
                            let class_definition =
                                info.1.frames[document_type].as_class_definition();
                            add_arguments(info, registry, field, class_definition, &info.1)
                        } else {
                            field
                        }
                    } else if let Some(base_type) = field_definition.base_type() {
                        if type_is_bool(base_type) {
                            Self::register_field::<bool>(
                                registry,
                                field_name,
                                &(),
                                field_definition.kind(),
                            )
                        } else if type_is_integer(base_type) {
                            Self::register_field::<i32>(
                                registry,
                                field_name,
                                &(),
                                field_definition.kind(),
                            )
                        } else if type_is_float(base_type) {
                            Self::register_field::<f64>(
                                registry,
                                field_name,
                                &(),
                                field_definition.kind(),
                            )
                        } else {
                            // asssume stringy
                            Self::register_field::<String>(
                                registry,
                                field_name,
                                &(),
                                field_definition.kind(),
                            )
                        }
                    } else if let Some(enum_type) = field_definition.enum_type(frames) {
                        Self::register_field::<TerminusEnum>(
                            registry,
                            field_name,
                            &(enum_type.to_owned(), frames.clone()),
                            field_definition.kind(),
                        )
                    } else {
                        panic!("No known range for field {:?}", field_definition)
                    },
                )
            })
            .collect();
        fields.push(registry.field::<ID>("id", &()));

        registry
            .build_object_type::<TerminusType<'a, C>>(info, &fields)
            .into_meta()
    }

    /*
    fn resolve_class_field(definition: &ClassDefinition,
                           field_name: &str,
                           instance: &SyncStoreLayer,
                           info: &(String, Arc<AllFrames>),
                           executor: &juniper::Executor<TerminusContext<'a,C>, DefaultScalarValue>) -> juniper::ExecutionResult {
        let field = &definition.fields[field_name];
        match field.kind() {
            FieldKind::Required => {
                let object_id = dbg!(instance.single_triple_sp(self.id, field_id))?.object;
                if let Some(doc_type) = field.document_type() {
                    executor.resolve(&(doc_type.to_string(), info.1.clone()), &TerminusType::new(object_id))
                }
                else {
                    let instance = executor.context().instance.as_ref().unwrap();
                    let val = instance.id_object_value(object_id).unwrap();
                    Ok(value_string_to_graphql(&val))
                }
            },
            _ => todo!()
        }
    }
    */
}

pub struct TerminusEnum {
    pub value: String,
}

impl GraphQLType for TerminusEnum {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.0)
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        if let TypeDefinition::Enum(e) = &info.1.frames[&info.0] {
            let values: Vec<_> = e
                .values
                .iter()
                .map(|v| -> EnumValue {
                    EnumValue {
                        name: v.to_string(),
                        description: None,
                        deprecation_status: DeprecationStatus::Current,
                    }
                })
                .collect();

            registry
                .build_enum_type::<TerminusEnum>(info, &values)
                .into_meta()
        } else {
            panic!("tried to build meta for enum but this is not an enum");
        }
    }
}

impl FromInputValue for TerminusEnum {
    fn from_input_value(v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        match v {
            InputValue::Enum(value) => Some(Self {
                value: value.to_owned(),
            }),
            _ => None,
        }
    }
}

impl GraphQLValue for TerminusEnum {
    type Context = ();

    type TypeInfo = (String, Arc<AllFrames>);

    fn type_name<'i>(&self, _info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some("TerminusEnum")
    }
}

impl<'a, C: QueryableContextType + 'a> GraphQLType for TerminusType<'a, C> {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.0)
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let (name, frames) = info;
        let frame = &frames.frames[name];
        match frame {
            TypeDefinition::Class(d) => Self::generate_class_type(d, info, registry),
            TypeDefinition::Enum(_) => panic!("no enum expected here"),
            _ => todo!(),
        }
    }
}

impl<'a, C: QueryableContextType + 'a> GraphQLValue for TerminusType<'a, C> {
    type Context = TerminusContext<'a, C>;

    type TypeInfo = (String, Arc<AllFrames>);

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.0)
    }

    fn resolve_field(
        &self,
        info: &Self::TypeInfo,
        field_name: &str,
        arguments: &juniper::Arguments,
        executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult {
        let get_info = || {
            // TODO: should this really be with a `?`? having an id,
            // we should always have had this instance layer at some
            // point. not having it here would be a weird bug.
            let instance = executor.context().instance.as_ref()?;
            if field_name == "id" {
                return Some(Ok(Value::Scalar(DefaultScalarValue::String(
                    instance.id_subject(self.id)?,
                ))));
            }

            let field_name_expanded = info.1.context.expand_schema(field_name);
            let field_id = instance.predicate_id(&field_name_expanded)?;

            let allframes = &info.1;
            let frame = &allframes.frames[&info.0];
            let doc_type;
            let enum_type;
            let kind;
            match frame {
                TypeDefinition::Class(c) => {
                    let field = &c.resolve_field(&field_name.to_string()); //fields[field_name];
                    doc_type = field.document_type(allframes);
                    enum_type = field.enum_type(allframes);
                    kind = field.kind();
                    //self.resolve_class_field(c, field_id )
                }
                _ => panic!("expected only a class at this level"),
            }

            match kind {
                FieldKind::Required => {
                    let object_id = instance.single_triple_sp(self.id, field_id)?.object;
                    if let Some(doc_type) = doc_type {
                        Some(executor.resolve(
                            &(doc_type.to_string(), info.1.clone()),
                            &TerminusType::new(object_id),
                        ))
                    } else if let Some(enum_type) = enum_type {
                        let enum_uri = instance.id_object_node(object_id).unwrap();
                        Some(Ok(enum_node_to_value(&enum_type, &enum_uri)))
                    } else {
                        let val = instance.id_object_value(object_id).unwrap();
                        Some(Ok(value_string_to_graphql(&val)))
                    }
                }
                FieldKind::Optional => {
                    let object_id = instance
                        .single_triple_sp(self.id, field_id)
                        .map(|t| t.object);
                    match object_id {
                        Some(object_id) => {
                            if let Some(doc_type) = doc_type {
                                Some(executor.resolve(
                                    &(doc_type.to_string(), info.1.clone()),
                                    &TerminusType::new(object_id),
                                ))
                            } else {
                                let val = instance.id_object_value(object_id).unwrap();
                                Some(Ok(value_string_to_graphql(&val)))
                            }
                        }
                        None => Some(Ok(Value::Null)),
                    }
                }
                FieldKind::Set => {
                    let object_ids = instance.triples_sp(self.id, field_id).map(|t| t.object);
                    collect_into_graphql_list(
                        doc_type, executor, info, arguments, object_ids, instance,
                    )
                }
                FieldKind::Cardinality => {
                    // pretty much a set actually
                    let object_ids = instance.triples_sp(self.id, field_id).map(|t| t.object);
                    collect_into_graphql_list(
                        doc_type, executor, info, arguments, object_ids, instance,
                    )
                }
                FieldKind::List => {
                    let list_id = instance
                        .single_triple_sp(self.id, field_id)
                        .expect("list element expected but not found")
                        .object;
                    let object_ids = RdfListIterator {
                        layer: instance,
                        cur: list_id,
                        rdf_first_id: instance.predicate_id(RDF_FIRST),
                        rdf_rest_id: instance.predicate_id(RDF_REST),
                        rdf_nil_id: instance.subject_id(RDF_NIL),
                    };
                    collect_into_graphql_list(
                        doc_type, executor, info, arguments, object_ids, instance,
                    )
                }
                FieldKind::Array => {
                    let array_element_ids: Box<dyn Iterator<Item = IdTriple> + Send> =
                        Box::new(instance.triples_sp(self.id, field_id));
                    let sys_index_ids = retrieve_all_index_ids(instance);
                    let array_iterator = SimpleArrayIterator(ArrayIterator {
                        layer: instance,
                        it: array_element_ids.peekable(),
                        subject: self.id,
                        predicate: field_id,
                        last_index: None,
                        sys_index_ids: &sys_index_ids,
                        sys_value_id: instance.predicate_id(SYS_VALUE),
                    });

                    let mut elements: Vec<_> = array_iterator.collect();
                    elements.sort();
                    let elements_iterator = elements.into_iter().map(|(_, elt)| elt);
                    collect_into_graphql_list(
                        doc_type,
                        executor,
                        info,
                        arguments,
                        elements_iterator,
                        instance,
                    )
                }
            }
        };
        let x = get_info();
        match x {
            Some(r) => r,
            None => Ok(Value::Null),
        }
        //Ok(Value::List(vec![Value::Scalar(DefaultScalarValue::String("cow!".to_string())),
        //                    Value::Scalar(DefaultScalarValue::String("duck!".to_string()))]))
    }
}

struct SimpleArrayIterator<'a, L: Layer>(ArrayIterator<'a, L>);

impl<'a, L: Layer> Iterator for SimpleArrayIterator<'a, L> {
    type Item = (Vec<usize>, u64);

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.0.next();
        match result {
            None => None,
            Some(element) => {
                let mut index = None;
                std::mem::swap(&mut index, &mut self.0.last_index);

                Some((index.unwrap(), element))
            }
        }
    }
}

fn collect_into_graphql_list<C: QueryableContextType>(
    doc_type: Option<&String>,
    executor: &juniper::Executor<TerminusContext<C>>,
    info: &(String, Arc<AllFrames>),
    arguments: &juniper::Arguments,
    object_ids: impl Iterator<Item = u64>,
    instance: &SyncStoreLayer,
) -> Option<Result<Value, juniper::FieldError>> {
    if let Some(doc_type) = doc_type {
        let object_ids = match executor.context().instance.as_ref() {
            Some(instance) => run_filter_query(
                instance,
                &info.1.context,
                arguments,
                doc_type,
                &info.1,
                Some(Box::new(object_ids)),
            ),
            None => vec![],
        };
        let subdocs: Vec<_> = object_ids.into_iter().map(TerminusType::new).collect();
        Some(executor.resolve(&(doc_type.to_string(), info.1.clone()), &subdocs))
    } else {
        let vals: Vec<_> = object_ids
            .map(|o| {
                let val = instance.id_object_value(o).unwrap();
                value_string_to_graphql(&val)
            })
            .collect();
        Some(Ok(Value::List(vals)))
    }
}

#[derive(GraphQLEnum)]
pub enum TerminusOrdering {
    Asc,
    Desc,
}

struct TerminusOrderBy {
    fields: Vec<(String, TerminusOrdering)>,
}

impl FromInputValue for TerminusOrderBy {
    fn from_input_value(_v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        todo!()
    }
}

impl GraphQLType for TerminusOrderBy {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.0)
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let frames = &info.2;
        if let TypeDefinition::Class(d) = &frames.frames[&info.1] {
            let arguments: Vec<_> = d
                .fields
                .iter()
                .filter_map(|(field_name, field_definition)| {
                    if let Some(_) = field_definition.base_type() {
                        Some(registry.arg::<Option<TerminusOrdering>>(field_name, &()))
                    } else {
                        None
                    }
                })
                .collect();

            registry
                .build_input_object_type::<TerminusOrderBy>(info, &arguments)
                .into_meta()
        } else {
            panic!("shouldn't be here");
        }
    }
}

impl GraphQLValue for TerminusOrderBy {
    type Context = ();

    type TypeInfo = (String, String, Arc<AllFrames>);

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.0)
    }

    fn resolve_field(
        &self,
        _info: &Self::TypeInfo,
        _field_name: &str,
        _arguments: &juniper::Arguments<DefaultScalarValue>,
        _executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult<DefaultScalarValue> {
        panic!("GraphQLValue::resolve_field() must be implemented by objects and interfaces");
    }
}
