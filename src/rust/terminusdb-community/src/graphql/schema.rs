use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;

use juniper::meta::{DeprecationStatus, EnumValue, Field};
use juniper::{
    graphql_value, DefaultScalarValue, FromInputValue, GraphQLEnum, GraphQLType, GraphQLValue,
    InputValue, Registry, Value, ID,
};
use lazy_init::Lazy;
use swipl::prelude::*;
use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;
use terminusdb_store_prolog::terminus_store::structure::TypedDictEntry;
use terminusdb_store_prolog::terminus_store::{IdTriple, Layer, ObjectType};

use crate::consts::{RDF_FIRST, RDF_NIL, RDF_REST, RDF_TYPE, SYS_VALUE};
use crate::doc::{retrieve_all_index_ids, ArrayIterator, GetDocumentContext};
use crate::path::iterator::{CachedClonableIterator, ClonableIterator};
use crate::schema::RdfListIterator;
use crate::types::{transaction_instance_layer, transaction_schema_layer};
use crate::value::{
    enum_node_to_value, type_is_big_integer, type_is_bool, type_is_datetime, type_is_decimal,
    type_is_float, type_is_json, type_is_small_integer, value_to_graphql,
};

use super::filter::{FilterInputObject, FilterInputObjectTypeInfo};
use super::frame::*;
use super::naming::{ordering_name, path_field_to_class};
use super::query::run_filter_query;

pub enum NodeOrValue {
    Node(IriName),
    #[allow(dead_code)]
    Value(TypedDictEntry),
}

#[derive(Clone)]
pub struct SystemInfo {
    pub user: Atom,
    pub system: SyncStoreLayer,
    pub commit: Option<SyncStoreLayer>,
    pub meta: Option<SyncStoreLayer>,
}

#[derive(Clone)]
pub struct TerminusContext<'a> {
    pub context: Rc<GenericQueryableContext<'a>>,
    pub system_transaction_term: Term<'a>,
    pub transaction_term: Term<'a>,
    pub author_term: Term<'a>,
    pub message_term: Term<'a>,
    pub system_info: SystemInfo,
    pub schema: SyncStoreLayer,
    pub instance: Option<SyncStoreLayer>,
    pub type_collection: TerminusTypeCollectionInfo,
    pub document_context: Arc<Lazy<GetDocumentContext<SyncStoreLayer>>>,
}

impl<'a> TerminusContext<'a> {
    pub fn new(
        context: GenericQueryableContext<'a>,
        auth_term: &Term,
        system_term: &'a Term,
        meta_term: &Term,
        commit_term: &Term,
        transaction_term: &'a Term,
        author_term: &'a Term,
        message_term: &'a Term,
        type_collection: TerminusTypeCollectionInfo,
    ) -> PrologResult<TerminusContext<'a>> {
        let user: Atom = auth_term.get_ex()?;
        let system =
            transaction_instance_layer(&context, system_term)?.expect("system layer not found");
        let meta = if meta_term.unify(atomable("none")).is_ok() {
            None
        } else {
            transaction_instance_layer(&context, meta_term).expect("Missing meta layer")
        };
        let commit = if commit_term.unify(atomable("none")).is_ok() {
            None
        } else {
            transaction_instance_layer(&context, commit_term).expect("Missing commit layer")
        };

        let schema =
            transaction_schema_layer(&context, transaction_term)?.expect("missing schema layer");
        let instance = transaction_instance_layer(&context, transaction_term)?;

        let context = Rc::new(context);

        Ok(TerminusContext {
            system_info: SystemInfo {
                user,
                system,
                meta,
                commit,
            },
            system_transaction_term: system_term.clone(),
            transaction_term: transaction_term.clone(),
            author_term: author_term.clone(),
            message_term: message_term.clone(),
            context,
            schema,
            instance,
            type_collection,
            document_context: Arc::new(Lazy::new()),
        })
    }

    pub fn document_context(&self) -> &GetDocumentContext<SyncStoreLayer> {
        self.document_context
            .get_or_create(|| GetDocumentContext::new(self.schema.clone(), self.instance.clone()))
    }
}

pub struct TerminusTypeCollection;

pub struct TerminusOrderingInfo {
    ordering_name: GraphQLName<'static>,
    type_name: GraphQLName<'static>,
    allframes: Arc<AllFrames>,
}

impl TerminusOrderingInfo {
    fn new(type_name: &GraphQLName, allframes: &Arc<AllFrames>) -> Self {
        Self {
            ordering_name: ordering_name(&type_name),
            type_name: type_name.as_static(),
            allframes: allframes.clone(),
        }
    }
}

fn add_arguments<'r>(
    info: &TerminusTypeInfo,
    registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    mut field: Field<'r, DefaultScalarValue>,
    class_definition: &ClassDefinition,
) -> Field<'r, DefaultScalarValue> {
    field = field.argument(registry.arg::<Option<ID>>("id", &()));
    field = field.argument(registry.arg::<Option<Vec<ID>>>("ids", &()));
    field = field.argument(registry.arg::<Option<bool>>("include_children", &()));
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
    field = field.argument(registry.arg::<Option<FilterInputObject>>(
        "filter",
        &FilterInputObjectTypeInfo::new(&info.class, &info.allframes),
    ));
    if must_generate_ordering(class_definition) {
        field = field.argument(
            registry
                .arg::<Option<TerminusOrderBy>>(
                    "orderBy",
                    &TerminusOrderingInfo::new(&info.class, &info.allframes),
                )
                .description("order by the given fields"),
        );
    }

    field
}

fn must_generate_ordering(class_definition: &ClassDefinition) -> bool {
    for (_, field) in class_definition.fields.iter() {
        if field.base_type().is_some() {
            return true;
        }
    }

    false
}

impl GraphQLType for TerminusTypeCollection {
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
            .allframes
            .frames
            .iter()
            .filter_map(|(name, typedef)| {
                if let TypeDefinition::Class(c) = typedef {
                    let newinfo = TerminusTypeInfo {
                        class: name.as_static(),
                        allframes: info.allframes.clone(),
                    };
                    let field = registry.field::<Vec<TerminusType>>(name.as_str(), &newinfo);

                    Some(add_arguments(&newinfo, registry, field, c))
                } else {
                    None
                }
            })
            .collect();
        let restriction_fields: Vec<_> = info
            .allframes
            .restrictions
            .iter()
            .map(|(name, restrictiondef)| {
                let newinfo = TerminusTypeInfo {
                    class: restrictiondef.on.to_owned(),
                    allframes: info.allframes.clone(),
                };
                let field = registry.field::<Vec<TerminusType>>(name.as_str(), &newinfo);
                let class_def;
                if let TypeDefinition::Class(c) = info
                    .allframes
                    .frames
                    .get(&restrictiondef.on)
                    .expect("Restriction not on known class")
                {
                    class_def = c;
                } else {
                    panic!("Restriction not on a class");
                }

                add_arguments(&newinfo, registry, field, class_def)
            })
            .collect();

        fields.extend(restriction_fields);

        fields.extend(standard_collection_operators(registry));

        /*
        fields.push(registry.field::<System>("_system", &()));
        */
        registry
            .build_object_type::<TerminusTypeCollection>(info, &fields)
            .into_meta()
    }
}

fn standard_collection_operators<'r>(
    registry: &mut juniper::Registry<'r, DefaultScalarValue>,
) -> impl Iterator<Item = Field<'r, DefaultScalarValue>> {
    vec![registry
        .field::<GraphQLJSON>("_getDocument", &())
        .argument(registry.arg::<String>("id", &()))]
    .into_iter()
}

fn standard_type_operators<'r>(
    registry: &mut juniper::Registry<'r, DefaultScalarValue>,
) -> impl Iterator<Item = Field<'r, DefaultScalarValue>> {
    vec![
        registry.field::<ID>("_id", &()),
        registry.field::<ID>("_type", &()),
        registry.field::<GraphQLJSON>("_json", &()),
    ]
    .into_iter()
}

#[derive(Clone)]
#[clone_blob("terminus_type_collection_info", defaults)]
pub struct TerminusTypeCollectionInfo {
    pub allframes: Arc<AllFrames>,
}

pub fn result_to_execution_result<C: QueryableContextType, T>(
    context: &Context<C>,
    result: PrologResult<T>,
) -> Result<T, juniper::FieldError> {
    result_to_string_result(context, result).map_err(|e| match e {
        PrologStringError::Failure => juniper::FieldError::new("prolog call failed", Value::Null),
        PrologStringError::Exception(e) => juniper::FieldError::new(e, Value::Null),
    })
}

fn pl_ids_from_restriction(
    context: &TerminusContext,
    restriction: &RestrictionDefinition,
) -> PrologResult<Vec<u64>> {
    let mut result = Vec::new();
    let prolog_context = &context.context;
    let frame = prolog_context.open_frame();
    let [restriction_term, id_term, reason_term] = frame.new_term_refs();
    restriction_term.unify(restriction.original_id.as_str())?;
    let open_call = frame.open(
        pred!("query:ids_for_restriction/4"),
        [
            &context.transaction_term,
            &restriction_term,
            &id_term,
            &reason_term,
        ],
    );
    while attempt_opt(open_call.next_solution())?.is_some() {
        let id: u64 = id_term.get_ex()?;
        result.push(id);
    }

    Ok(result)
}

fn ids_from_restriction(
    context: &TerminusContext,
    restriction: &RestrictionDefinition,
) -> Result<Vec<u64>, juniper::FieldError> {
    let result = pl_ids_from_restriction(&context, restriction).map(|mut r| {
        r.sort();
        r.dedup();

        r
    });
    result_to_execution_result(&context.context, result)
}

fn pl_id_matches_restriction(
    context: &TerminusContext,
    restriction: &ShortName,
    id: u64,
) -> PrologResult<Option<String>> {
    let prolog_context = &context.context;
    let frame = prolog_context.open_frame();
    let [restriction_term, id_term, reason_term] = frame.new_term_refs();
    restriction_term.unify(restriction.as_str())?;
    id_term.unify(id)?;
    let open_call = frame.open(
        pred!("query:ids_for_restriction/4"),
        [
            &context.transaction_term,
            &restriction_term,
            &id_term,
            &reason_term,
        ],
    );
    if attempt_opt(open_call.next_solution())?.is_some() {
        let reason: String = reason_term.get_ex()?;
        Ok(Some(reason))
    } else {
        Ok(None)
    }
}

pub fn id_matches_restriction(
    context: &TerminusContext,
    restriction: &ShortName,
    id: u64,
) -> Result<Option<String>, juniper::FieldError> {
    let result = pl_id_matches_restriction(context, restriction, id);
    result_to_execution_result(&context.context, result)
}

impl GraphQLValue for TerminusTypeCollection {
    type Context = TerminusContext<'static>;

    type TypeInfo = TerminusTypeCollectionInfo;

    fn type_name<'i>(&self, _info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some("TerminusTypeCollection")
    }

    fn resolve_field(
        &self,
        info: &Self::TypeInfo,
        resolve_field_name: &str,
        arguments: &juniper::Arguments<DefaultScalarValue>,
        executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult<DefaultScalarValue> {
        let field_name = GraphQLName(resolve_field_name.into());
        match resolve_field_name {
            "_getDocument" => {
                let context = executor.context();
                let document_context = context.document_context();
                let id: NodeVariety = node_variety(arguments.get("id").unwrap());
                let expanded_id = context
                    .type_collection
                    .allframes
                    .context
                    .expand_instance(&id);
                let doc = document_context.get_document(&expanded_id, true, true)?;
                match doc {
                    Some(doc) => {
                        let json_string =
                            serde_json::to_string_pretty(&serde_json::Value::Object(doc)).unwrap();

                        Ok(Value::Scalar(DefaultScalarValue::String(json_string)))
                    }
                    None => Err("No such document".into()),
                }
            }
            _ => {
                let zero_iter;
                let type_name;
                if let Some(restriction) = info.allframes.restrictions.get(&field_name) {
                    // This is a restriction. We're gonna have to call into prolog to get an iri list and turn it into an iterator over ids to use as a zero iter
                    type_name = &restriction.on;
                    let id_list = ids_from_restriction(executor.context(), restriction)?;
                    zero_iter = Some(ClonableIterator::new(id_list.into_iter()));
                } else {
                    type_name = &field_name;
                    zero_iter = None;
                }
                let objects = match executor.context().instance.as_ref() {
                    Some(instance) => run_filter_query(
                        executor.context(),
                        instance,
                        &info.allframes.context,
                        arguments,
                        &type_name,
                        &info.allframes,
                        zero_iter,
                    )
                    .into_iter()
                    .map(|id| TerminusType::new(id))
                    .collect(),
                    None => vec![],
                };

                executor.resolve(
                    &TerminusTypeInfo {
                        class: type_name.as_static(),
                        allframes: info.allframes.clone(),
                    },
                    &objects,
                )
            }
        }
    }
}

pub struct TerminusTypeInfo {
    class: GraphQLName<'static>,
    allframes: Arc<AllFrames>,
}

pub struct TerminusType {
    id: u64,
}

impl TerminusType {
    fn new(id: u64) -> Self {
        Self { id }
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
        class_name: &str,
        d: &ClassDefinition,
        info: &<Self as GraphQLValue>::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let frames = &info.allframes;
        let class_name = GraphQLName(class_name.into());
        let mut fields: Vec<_> = d
            .fields()
            .iter()
            .map(|(field_name, field_definition)| {
                if let Some(document_type) = field_definition.document_type(frames) {
                    let field = Self::register_field::<TerminusType>(
                        registry,
                        field_name,
                        &TerminusTypeInfo {
                            class: document_type.as_static(),
                            allframes: frames.clone(),
                        },
                        field_definition.kind(),
                    );

                    if field_definition.kind().is_collection() {
                        let class_definition =
                            info.allframes.frames[document_type].as_class_definition();
                        let new_info = TerminusTypeInfo {
                            class: document_type.as_static(),
                            allframes: info.allframes.clone(),
                        };
                        add_arguments(&new_info, registry, field, class_definition)
                    } else {
                        field
                    }
                } else if let Some(base_type) = field_definition.base_type() {
                    if type_is_bool(base_type) {
                        Self::register_field::<bool>(
                            registry,
                            field_name.as_str(),
                            &(),
                            field_definition.kind(),
                        )
                    } else if type_is_small_integer(base_type) {
                        Self::register_field::<i32>(
                            registry,
                            field_name.as_str(),
                            &(),
                            field_definition.kind(),
                        )
                    } else if type_is_big_integer(base_type) {
                        Self::register_field::<BigInt>(
                            registry,
                            field_name.as_str(),
                            &(),
                            field_definition.kind(),
                        )
                    } else if type_is_float(base_type) {
                        Self::register_field::<f64>(
                            registry,
                            field_name.as_str(),
                            &(),
                            field_definition.kind(),
                        )
                    } else if type_is_datetime(base_type) {
                        Self::register_field::<DateTime>(
                            registry,
                            field_name.as_str(),
                            &(),
                            field_definition.kind(),
                        )
                    } else if type_is_decimal(base_type) {
                        Self::register_field::<BigFloat>(
                            registry,
                            field_name.as_str(),
                            &(),
                            field_definition.kind(),
                        )
                    } else if type_is_json(base_type) {
                        Self::register_field::<GraphQLJSON>(
                            registry,
                            field_name.as_str(),
                            &(),
                            field_definition.kind(),
                        )
                    } else {
                        // assume stringy
                        Self::register_field::<String>(
                            registry,
                            field_name.as_str(),
                            &(),
                            field_definition.kind(),
                        )
                    }
                } else if let Some(enum_type) = field_definition.enum_type(frames) {
                    Self::register_field::<TerminusEnum>(
                        registry,
                        field_name,
                        &(enum_type.as_static(), frames.clone()),
                        field_definition.kind(),
                    )
                } else {
                    Self::register_field::<ID>(registry, field_name, &(), field_definition.kind())
                }
            })
            .collect();

        let mut inverted_fields: Vec<_> = Vec::new();
        let database_class_name = &info.class;
        if let Some(inverted_type) = &frames.inverted.classes.get(database_class_name) {
            for (field_name, ifd) in inverted_type.domain.iter() {
                let class = &ifd.class;
                if !info.allframes.frames[class].is_document_type() {
                    continue;
                }
                let class_definition = info.allframes.frames[class].as_class_definition();
                let new_info = TerminusTypeInfo {
                    class: class.as_static(),
                    allframes: frames.clone(),
                };
                let field = Self::register_field::<TerminusType>(
                    registry,
                    field_name,
                    &new_info,
                    FieldKind::Set,
                );
                let field = add_arguments(&new_info, registry, field, class_definition);

                inverted_fields.push(field);
            }
        }
        fields.append(&mut inverted_fields);

        let mut path_fields: Vec<_> = Vec::new();
        for class in frames.frames.keys() {
            if !info.allframes.frames[class].is_document_type() {
                continue;
            }
            let field_name = format!("_path_to_{class}");
            let class_definition = info.allframes.frames[class].as_class_definition();
            let new_info = TerminusTypeInfo {
                class: class.as_static(),
                allframes: frames.clone(),
            };
            let field = Self::register_field::<TerminusType>(
                registry,
                &field_name,
                &new_info,
                FieldKind::Set,
            );
            let field = add_arguments(&new_info, registry, field, class_definition);
            let field = field.argument(registry.arg::<String>("path", &()));
            path_fields.push(field);
        }

        fields.append(&mut path_fields);

        let mut applicable_restrictions = Vec::new();
        for restriction in frames.restrictions.values() {
            if restriction.on == class_name {
                applicable_restrictions.push(restriction.id.to_owned());
            }
        }

        if !applicable_restrictions.is_empty() {
            let enum_name = GraphQLName(format!("{class_name}_Restriction").into());
            let type_info = GeneratedEnumTypeInfo {
                name: enum_name.as_static(),
                values: applicable_restrictions,
            };

            let restriction_field = registry
                .field::<Option<GraphQLJSON>>("_restriction", &())
                .argument(registry.arg::<GeneratedEnum>("name", &type_info));

            fields.push(restriction_field);
        }

        fields.extend(standard_type_operators(registry));

        registry
            .build_object_type::<TerminusType>(info, &fields)
            .into_meta()
    }
}

impl GraphQLType for TerminusType {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.class)
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let class = &info.class;
        let allframes = &info.allframes;
        let frame = &allframes.frames[class];
        match frame {
            TypeDefinition::Class(d) => Self::generate_class_type(class, d, info, registry),
            TypeDefinition::Enum(_) => panic!("no enum expected here"),
        }
    }
}

fn rewind_rdf_list(instance: &dyn Layer, cons_id: u64) -> Option<u64> {
    if let Some(rdf_rest) = instance.predicate_id(RDF_REST) {
        let mut cons = Some(cons_id);
        while let Some(id) = cons {
            let res = instance
                .triples_o(id)
                .filter(|t| t.predicate == rdf_rest)
                .map(|t| t.subject)
                .next();
            if res.is_none() {
                return cons;
            } else {
                cons = res
            }
        }
        cons
    } else {
        None
    }
}

fn subject_has_type(instance: &dyn Layer, subject_id: u64, class: &str) -> bool {
    if let Some(rdf_type_id) = instance.predicate_id(RDF_TYPE) {
        if let Some(class_id) = instance.object_node_id(class) {
            instance.triple_exists(subject_id, rdf_type_id, class_id)
        } else {
            false
        }
    } else {
        false
    }
}

impl GraphQLValue for TerminusType {
    type Context = TerminusContext<'static>;

    type TypeInfo = TerminusTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.class)
    }

    fn resolve_field(
        &self,
        info: &Self::TypeInfo,
        field_name: &str,
        arguments: &juniper::Arguments,
        executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult {
        let field_name = GraphQLName(field_name.into());
        let get_info = || {
            // TODO: should this really be with a `?`? having an id,
            // we should always have had this instance layer at some
            // point. not having it here would be a weird bug.
            let instance = executor.context().instance.as_ref()?;
            if &*field_name == "_id" {
                return Some(Ok(Value::Scalar(DefaultScalarValue::String(
                    instance.id_subject(self.id)?,
                ))));
            }
            if &*field_name == "_json" {
                let document_context = executor.context().document_context();
                let doc = document_context.get_id_document(self.id, true, true);
                match doc {
                    Ok(doc) => {
                        let json_string =
                            serde_json::to_string_pretty(&serde_json::Value::Object(doc)).unwrap();

                        return Some(Ok(Value::Scalar(DefaultScalarValue::String(json_string))));
                    }
                    Err(e) => return Some(Err(e.into())),
                }
            }

            let allframes = &info.allframes;
            let class = &info.class;

            if &*field_name == "_type" {
                let ty = instance
                    .predicate_id(RDF_TYPE)
                    .and_then(|pid| instance.single_triple_sp(self.id, pid))
                    .and_then(|t| instance.id_object_node(t.object))
                    .map(|ty| {
                        let small_ty = allframes.iri_to_graphql_name(&IriName(ty));
                        Ok(Value::Scalar(DefaultScalarValue::String(
                            small_ty.to_string(),
                        )))
                    });
                return ty;
            }

            eprintln!("{field_name}");
            if let Some(reverse_link) = allframes.reverse_link(class, &field_name) {
                eprintln!("{:?}", reverse_link);
                let property = &reverse_link.property;
                let graphql_domain = &reverse_link.class;
                let kind = &reverse_link.kind;
                // TODO: We need to check that the domain uri is correct
                let property_expanded = allframes.context.expand_schema(property);
                let domain_uri = allframes.context.expand_schema(graphql_domain);
                let field_id = instance.predicate_id(&property_expanded)?;
                // List and array are special since they are *deep* objects
                match kind {
                    FieldKind::List => {
                        let instance1 = instance.clone();
                        let instance2 = instance.clone();
                        let instance3 = instance.clone();
                        let object_ids = instance
                            .triples_o(self.id)
                            .flat_map(move |t| {
                                instance1
                                    .predicate_id(RDF_FIRST)
                                    .filter(|rdf_first| t.predicate == *rdf_first)
                                    .map(|_| t.subject)
                            })
                            .flat_map(move |cons| rewind_rdf_list(&instance2, cons))
                            .flat_map(move |o| {
                                instance3
                                    .triples_o(o)
                                    .filter(|t| {
                                        t.predicate == field_id
                                            && subject_has_type(&instance3, t.subject, &domain_uri)
                                    })
                                    .map(|t| t.subject)
                                    .next()
                            });
                        collect_into_graphql_list(
                            Some(&graphql_domain),
                            None,
                            false,
                            executor,
                            info,
                            arguments,
                            ClonableIterator::new(CachedClonableIterator::new(object_ids)),
                            instance,
                        )
                    }
                    FieldKind::Array => {
                        let instance1 = instance.clone();
                        let instance2 = instance.clone();
                        let instance3 = instance.clone();
                        let object_ids = instance
                            .triples_o(self.id)
                            .flat_map(move |t| {
                                instance1
                                    .predicate_id(SYS_VALUE)
                                    .filter(|sys_value| t.predicate == *sys_value)
                                    .map(|_| t.subject)
                            })
                            .flat_map(move |o| {
                                instance2
                                    .triples_o(o)
                                    .filter(|t| {
                                        t.predicate == field_id
                                            && subject_has_type(&instance3, t.subject, &domain_uri)
                                    })
                                    .map(|t| t.subject)
                                    .next()
                            });
                        collect_into_graphql_list(
                            Some(&graphql_domain),
                            None,
                            false,
                            executor,
                            info,
                            arguments,
                            ClonableIterator::new(CachedClonableIterator::new(object_ids)),
                            instance,
                        )
                    }
                    _ => {
                        let instance1 = instance.clone();
                        let object_ids = instance
                            .triples_o(self.id)
                            .filter(move |t| {
                                t.predicate == field_id
                                    && subject_has_type(&instance1, t.subject, &domain_uri)
                            })
                            .map(|t| t.subject);
                        collect_into_graphql_list(
                            Some(&graphql_domain),
                            None,
                            false,
                            executor,
                            info,
                            arguments,
                            ClonableIterator::new(CachedClonableIterator::new(object_ids)),
                            instance,
                        )
                    }
                }
            } else if let Some(class) = path_field_to_class(&field_name) {
                let ids = vec![self.id].into_iter();
                collect_into_graphql_list(
                    Some(&class),
                    None,
                    false,
                    executor,
                    info,
                    arguments,
                    ClonableIterator::new(CachedClonableIterator::new(ids)),
                    instance,
                )
            } else if &*field_name == "_restriction" {
                // fetch argument
                let restriction_enum_value: GeneratedEnum = arguments.get("name")?;
                let original_restriction = &allframes
                    .class_renaming
                    .get_by_left(&restriction_enum_value.value)
                    .unwrap();

                let result =
                    id_matches_restriction(executor.context(), original_restriction, self.id);

                match result {
                    Ok(Some(reason)) => Some(Ok(graphql_value!(reason))),
                    Ok(None) => Some(Ok(graphql_value!(None))),
                    Err(e) => Some(Err(e.into())),
                }
            } else {
                let frame = &allframes.frames[&info.class];
                let field_name_expanded: &IriName;
                let doc_type;
                let enum_type;
                let kind;
                let is_json;
                match frame {
                    TypeDefinition::Class(c) => {
                        let field = &c.resolve_field(&field_name);
                        field_name_expanded =
                            c.graphql_to_iri_name(&allframes.context, &field_name);

                        doc_type = field.document_type(allframes);
                        enum_type = field.enum_type(allframes);
                        kind = field.kind();
                        is_json = field.is_json_type();
                    }
                    _ => panic!("expected only a class at this level"),
                }
                let field_id_opt = instance.predicate_id(&*field_name_expanded);
                if field_id_opt.is_none() {
                    match kind {
                        FieldKind::Array
                        | FieldKind::List
                        | FieldKind::Cardinality
                        | FieldKind::Set => return Some(Ok(graphql_value!([]))),
                        _ => return None,
                    }
                };
                let field_id = field_id_opt.unwrap();
                match kind {
                    FieldKind::Required => {
                        let object_id = instance.single_triple_sp(self.id, field_id)?.object;
                        extract_fragment(
                            executor, info, instance, object_id, doc_type, enum_type, is_json,
                        )
                    }
                    FieldKind::Optional => {
                        let object_id = instance
                            .single_triple_sp(self.id, field_id)
                            .map(|t| t.object);
                        match object_id {
                            Some(object_id) => extract_fragment(
                                executor, info, instance, object_id, doc_type, enum_type, is_json,
                            ),
                            None => Some(Ok(Value::Null)),
                        }
                    }
                    FieldKind::Set => {
                        let object_ids = ClonableIterator::new(CachedClonableIterator::new(
                            instance.triples_sp(self.id, field_id).map(|t| t.object),
                        ));
                        collect_into_graphql_list(
                            doc_type, enum_type, is_json, executor, info, arguments, object_ids,
                            instance,
                        )
                    }
                    FieldKind::Cardinality => {
                        // pretty much a set actually
                        let object_ids = ClonableIterator::new(CachedClonableIterator::new(
                            instance.triples_sp(self.id, field_id).map(|t| t.object),
                        ));
                        collect_into_graphql_list(
                            doc_type, enum_type, is_json, executor, info, arguments, object_ids,
                            instance,
                        )
                    }
                    FieldKind::List => {
                        let list_id = instance
                            .single_triple_sp(self.id, field_id)
                            .expect("list element expected but not found")
                            .object;
                        let object_ids =
                            ClonableIterator::new(CachedClonableIterator::new(RdfListIterator {
                                layer: instance,
                                cur: list_id,
                                rdf_first_id: instance.predicate_id(RDF_FIRST),
                                rdf_rest_id: instance.predicate_id(RDF_REST),
                                rdf_nil_id: instance.subject_id(RDF_NIL),
                            }));
                        collect_into_graphql_list(
                            doc_type, enum_type, is_json, executor, info, arguments, object_ids,
                            instance,
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
                        let elements_iterator = ClonableIterator::new(CachedClonableIterator::new(
                            elements.into_iter().map(|(_, elt)| elt),
                        ));
                        collect_into_graphql_list(
                            doc_type,
                            enum_type,
                            is_json,
                            executor,
                            info,
                            arguments,
                            elements_iterator,
                            instance,
                        )
                    }
                }
            }
        };

        let x: Option<Result<_, juniper::FieldError<_>>> = get_info();
        match x {
            Some(r) => r,
            None => Ok(Value::Null),
        }
    }
}

fn extract_fragment(
    executor: &juniper::Executor<TerminusContext<'static>, DefaultScalarValue>,
    info: &TerminusTypeInfo,
    instance: &SyncStoreLayer,
    object_id: u64,
    doc_type: Option<&GraphQLName<'_>>,
    enum_type: Option<&GraphQLName<'_>>,
    is_json: bool,
) -> Option<Result<juniper::Value, juniper::FieldError>> {
    if let Some(doc_type) = doc_type {
        Some(executor.resolve(
            &TerminusTypeInfo {
                class: doc_type.as_static(),
                allframes: info.allframes.clone(),
            },
            &TerminusType::new(object_id),
        ))
    } else if let Some(enum_type) = enum_type {
        let value = extract_enum_fragment(info, instance, object_id, enum_type);
        Some(Ok(value))
    } else if is_json {
        let val = extract_json_fragment(instance, object_id);
        Some(val)
    } else {
        let obj = instance.id_object(object_id)?;
        match obj {
            ObjectType::Node(n) => Some(Ok(n.into())),
            ObjectType::Value(v) => Some(Ok(value_to_graphql(&v))),
        }
    }
}

pub fn enum_type_and_node_to_iri_name(enum_type: &IriName, enum_uri: &IriName) -> IriName {
    IriName(enum_node_to_value(enum_type, enum_uri))
}

fn extract_enum_fragment(
    info: &TerminusTypeInfo,
    instance: &SyncStoreLayer,
    object_id: u64,
    enum_type: &GraphQLName<'_>,
) -> juniper::Value {
    let enum_uri = IriName(instance.id_object_node(object_id).unwrap());
    let qualified_enum_type = info.allframes.graphql_to_iri_name(enum_type);
    let enum_value = enum_type_and_node_to_iri_name(&qualified_enum_type, &enum_uri);
    let enum_definition = info.allframes.frames[enum_type].as_enum_definition();
    juniper::Value::Scalar(DefaultScalarValue::String(
        enum_definition.name_value(&enum_value).to_string(),
    ))
}

fn extract_json_fragment(
    instance: &SyncStoreLayer,
    object_id: u64,
) -> Result<juniper::Value, juniper::FieldError> {
    // TODO this should really not just recreate a context, but it's cheap enough since it is schema independent.
    let context = GetDocumentContext::new_json(Some(instance.clone()));
    let doc = context.get_id_document(object_id, true, true)?;
    let json = serde_json::Value::Object(doc);
    Ok(juniper::Value::Scalar(DefaultScalarValue::String(
        json.to_string(),
    )))
}

/// An enum type that is generated dynamically
pub struct GeneratedEnumTypeInfo {
    pub name: GraphQLName<'static>,
    pub values: Vec<GraphQLName<'static>>,
}

/// An enum value that is generated dynamically
pub struct GeneratedEnum {
    pub value: GraphQLName<'static>,
}

impl GraphQLValue for GeneratedEnum {
    type Context = ();

    type TypeInfo = GeneratedEnumTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.name)
    }
}

impl GraphQLType for GeneratedEnum {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.name)
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let values: Vec<_> = info
            .values
            .iter()
            .map(|v| EnumValue {
                name: v.to_string(),
                description: None,
                deprecation_status: DeprecationStatus::Current,
            })
            .collect();
        registry
            .build_enum_type::<GeneratedEnum>(info, &values)
            .into_meta()
    }
}

impl FromInputValue for GeneratedEnum {
    fn from_input_value(v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        match v {
            InputValue::Enum(value) => Some(Self {
                value: GraphQLName(Cow::Owned(value.into())),
            }),
            InputValue::Scalar(DefaultScalarValue::String(value)) => Some(Self {
                value: GraphQLName(Cow::Owned(value.into())),
            }),
            _ => None,
        }
    }
}

pub struct TerminusEnum {
    pub value: GraphQLName<'static>,
}

impl<'a> GraphQLType for TerminusEnum {
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
                value: GraphQLName(value.to_string().into()),
            }),
            InputValue::Scalar(DefaultScalarValue::String(value)) => Some(Self {
                value: GraphQLName(value.to_string().into()),
            }),
            _ => None,
        }
    }
}

impl GraphQLValue for TerminusEnum {
    type Context = ();

    type TypeInfo = (GraphQLName<'static>, Arc<AllFrames>);

    fn type_name<'i>(&self, _info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some("TerminusEnum")
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

fn collect_into_graphql_list<'a>(
    doc_type: Option<&'a GraphQLName<'a>>,
    enum_type: Option<&'a GraphQLName<'a>>,
    is_json: bool,
    executor: &'a juniper::Executor<TerminusContext<'static>>,
    info: &'a TerminusTypeInfo,
    arguments: &'a juniper::Arguments,
    object_ids: ClonableIterator<'a, u64>,
    instance: &'a SyncStoreLayer,
) -> Option<Result<Value, juniper::FieldError>> {
    if let Some(doc_type) = doc_type {
        let object_ids = match executor.context().instance.as_ref() {
            Some(instance) => run_filter_query(
                executor.context(),
                instance,
                &info.allframes.context,
                arguments,
                doc_type,
                &info.allframes,
                Some(object_ids),
            ),
            None => vec![],
        };
        let subdocs: Vec<_> = object_ids.into_iter().map(TerminusType::new).collect();
        Some(executor.resolve(
            &TerminusTypeInfo {
                class: doc_type.as_static(),
                allframes: info.allframes.clone(),
            },
            &subdocs,
        ))
    } else if let Some(enum_type) = enum_type {
        let vals: Vec<_> = object_ids
            .map(|o| extract_enum_fragment(info, instance, o, enum_type))
            .collect();
        Some(Ok(Value::List(vals)))
    } else if is_json {
        let mut vals: Vec<_> = Vec::new();
        for o in object_ids {
            let fragment = extract_json_fragment(instance, o);
            if fragment.is_err() {
                return Some(fragment);
            }
            let fragment = fragment.unwrap();
            vals.push(fragment);
        }
        Some(Ok(Value::List(vals)))
    } else {
        let vals: Vec<_> = object_ids
            .map(|o| match instance.id_object(o).unwrap() {
                ObjectType::Node(n) => graphql_value!(n),
                ObjectType::Value(v) => value_to_graphql(&v),
            })
            .collect();
        Some(Ok(Value::List(vals)))
    }
}

#[derive(GraphQLEnum, Clone, Copy)]
pub enum TerminusOrdering {
    Asc,
    Desc,
}

pub struct TerminusOrderBy {
    pub fields: Vec<(GraphQLName<'static>, TerminusOrdering)>,
}

impl FromInputValue for TerminusOrderBy {
    fn from_input_value(v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        if let InputValue::Object(o) = v {
            let fields: Vec<_> = o
                .iter()
                .map(|(k, v)| {
                    (
                        GraphQLName(Cow::Owned(k.item.to_owned())),
                        TerminusOrdering::from_input_value(&v.item).unwrap(),
                    )
                })
                .collect();

            Some(Self { fields })
        } else {
            None
        }
    }
}

impl GraphQLType for TerminusOrderBy {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.ordering_name)
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let frames = &info.allframes;
        if let TypeDefinition::Class(d) = &frames.frames[&info.type_name] {
            let arguments: Vec<_> = d
                .fields
                .iter()
                .filter_map(|(field_name, field_definition)| {
                    if field_definition.base_type().is_some() {
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

    type TypeInfo = TerminusOrderingInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.ordering_name)
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

#[derive(Debug, Clone)]
pub struct BigInt(pub String);

#[juniper::graphql_scalar(
    name = "BigInt",
    description = "The `BigInt` scalar type represents non-fractional signed whole numeric values."
)]
impl<S> GraphQLScalar for BigInt
where
    S: juniper::ScalarValue,
{
    fn resolve(&self) -> juniper::Value {
        juniper::Value::scalar(self.0.to_owned())
    }

    fn from_input_value(value: &juniper::InputValue) -> Option<Self> {
        value.as_string_value().map(|s| Self(s.to_owned()))
    }

    fn from_str<'a>(value: juniper::ScalarToken<'a>) -> juniper::ParseScalarResult<'a, S> {
        <String as juniper::ParseScalarValue<S>>::from_str(value)
    }
}

#[derive(Debug, Clone)]
pub struct GraphQLJSON(pub String);

#[juniper::graphql_scalar(name = "JSON", description = "An arbitrary JSON value.")]
impl<S> GraphQLScalar for GraphQLJSON
where
    S: juniper::ScalarValue,
{
    fn resolve(&self) -> juniper::Value {
        juniper::Value::scalar(self.0.to_owned())
    }

    fn from_input_value(value: &juniper::InputValue) -> Option<Self> {
        value.as_string_value().map(|s| Self(s.to_owned()))
    }

    fn from_str<'a>(value: juniper::ScalarToken<'a>) -> juniper::ParseScalarResult<'a, S> {
        <String as juniper::ParseScalarValue<S>>::from_str(value)
    }
}

#[derive(Debug, Clone)]
pub struct DateTime(pub String);

#[juniper::graphql_scalar(
    name = "DateTime",
    description = "The `DateTime` scalar type represents a date encoded as a string using the RFC 3339 profile of the ISO 8601 standard for representation of dates and times using the Gregorian calendar."
)]
impl<S> GraphQLScalar for DateTime
where
    S: juniper::ScalarValue,
{
    fn resolve(&self) -> juniper::Value {
        juniper::Value::scalar(self.0.to_owned())
    }

    fn from_input_value(value: &juniper::InputValue) -> Option<Self> {
        value.as_string_value().map(|s| Self(s.to_owned()))
    }

    fn from_str<'a>(value: juniper::ScalarToken<'a>) -> juniper::ParseScalarResult<'a, S> {
        <String as juniper::ParseScalarValue<S>>::from_str(value)
    }
}

#[derive(Debug, Clone)]
pub struct BigFloat(pub String);

#[juniper::graphql_scalar(
    name = "BigFloat",
    description = "The `BigFloat` scalar type represents an arbitrary precision decimal."
)]
impl<S> GraphQLScalar for BigFloat
where
    S: juniper::ScalarValue,
{
    fn resolve(&self) -> juniper::Value {
        juniper::Value::scalar(self.0.to_owned())
    }

    fn from_input_value(value: &juniper::InputValue) -> Option<Self> {
        value.as_string_value().map(|s| Self(s.to_owned()))
    }

    fn from_str<'a>(value: juniper::ScalarToken<'a>) -> juniper::ParseScalarResult<'a, S> {
        <String as juniper::ParseScalarValue<S>>::from_str(value)
    }
}

#[derive(GraphQLEnum, Clone, Copy, Debug, Eq, PartialEq)]
pub enum GraphType {
    InstanceGraph,
    SchemaGraph,
}
