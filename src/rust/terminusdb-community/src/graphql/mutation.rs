use std::sync::Arc;

use juniper::{
    meta::{Argument, Field, InputObjectMeta},
    DefaultScalarValue, FromInputValue, GraphQLType, GraphQLValue, InputValue, Registry, ID,
};
use swipl::{
    atom, pred,
    prelude::GenericQueryableContext,
    result::{attempt, PrologResult},
    term::Term,
};

use crate::{
    graphql::{
        naming::{delete_class_name, update_class_name},
        schema::GraphQLJSON,
    },
    value::base_type_kind,
};

use super::{
    frame::{AllFrames, ClassDefinition, FieldDefinition, FieldKind, GraphQLName, TypeDefinition},
    naming::{input_type_name, insert_class_name},
    schema::{
        result_to_execution_result, BigFloat, BigInt, DateTime, GraphType, TerminusContext,
        TerminusEnum, TerminusType, TerminusTypeCollectionInfo, TerminusTypeInfo,
    },
};

pub struct TerminusMutationRoot;

impl GraphQLType for TerminusMutationRoot {
    fn name(_info: &Self::TypeInfo) -> Option<&str> {
        Some("TerminusMutation")
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let mut mutation_fields: Vec<_> = info
            .allframes
            .frames
            .iter()
            .flat_map(|(name, typedef)| {
                if let TypeDefinition::Class(_) = typedef {
                    let insert_field =
                        generate_insert_field(registry, info.allframes.clone(), name);

                    // this vec will also get update and delete
                    vec![insert_field]
                } else {
                    Vec::with_capacity(0)
                }
            })
            .collect();

        let insert_documents_field = registry
            .field::<Vec<ID>>("_insertDocuments", &())
            .argument(registry.arg::<GraphQLJSON>("json", &()))
            .argument(registry.arg::<Option<GraphType>>("graph_type", &()))
            .argument(registry.arg::<Option<bool>>("raw_json", &()));
        let replace_documents_field = registry
            .field::<Vec<ID>>("_replaceDocuments", &())
            .argument(registry.arg::<GraphQLJSON>("json", &()))
            .argument(registry.arg::<Option<GraphType>>("graph_type", &()))
            .argument(registry.arg::<Option<bool>>("raw_json", &()))
            .argument(registry.arg::<Option<bool>>("create", &()));
        let delete_documents_field = registry
            .field::<Vec<ID>>("_deleteDocuments", &())
            .argument(registry.arg::<Vec<ID>>("ids", &()))
            .argument(registry.arg::<Option<GraphType>>("graph_type", &()));
        let commit_info_field = registry
            .field::<bool>("_commitInfo", &())
            .argument(registry.arg::<Option<String>>("author", &()))
            .argument(registry.arg::<Option<String>>("message", &()));

        mutation_fields.extend([
            insert_documents_field,
            replace_documents_field,
            delete_documents_field,
            commit_info_field,
        ]);
        registry
            .build_object_type::<TerminusMutationRoot>(info, &mutation_fields)
            .into_meta()
    }
}

pub struct InsertTypeInputObject {
    pub edges: Vec<(juniper::Spanning<String>, juniper::Spanning<InputValue>)>,
}

impl GraphQLType for InsertTypeInputObject {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(info.input_type_name.as_str())
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        if let Some(TypeDefinition::Class(d)) = &info.allframes.frames.get(&info.type_name) {
            let id_arg = registry.arg::<Option<ID>>("_id", &());
            let mut args: Vec<_> = d
                .fields()
                .iter()
                .map(
                    |(field_name, field_definition)| -> juniper::meta::Argument<DefaultScalarValue> {
                        create_insert_arg_from_field_definition(
                            registry,
                            info.allframes.clone(),
                            field_name,
                            field_definition,
                        )
                    },
                )
                .collect();

            args.push(id_arg);
            registry
                .build_input_object_type::<InsertTypeInputObject>(info, &args)
                .into_meta()
        } else {
            panic!(
                "The class {} was not found and so no insertion arguments could be constructed.",
                &info.type_name
            )
        }
    }
}

impl FromInputValue for InsertTypeInputObject {
    fn from_input_value(v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        match v {
            InputValue::Object(o) => Some(Self { edges: o.clone() }),
            _ => None,
        }
    }
}

pub struct InputObjectTypeInfo {
    pub input_type_name: GraphQLName<'static>,
    pub type_name: GraphQLName<'static>,
    pub allframes: Arc<AllFrames>,
}

pub enum MutationMode {
    Insert,
    Delete,
    Update,
}

impl InputObjectTypeInfo {
    pub fn new(
        type_name: &GraphQLName,
        all_frames: &Arc<AllFrames>,
        mutation_mode: MutationMode,
    ) -> Self {
        Self {
            input_type_name: input_type_name(type_name, mutation_mode),
            type_name: type_name.as_static(),
            allframes: all_frames.clone(),
        }
    }
}

impl GraphQLValue for InsertTypeInputObject {
    type Context = TerminusContext<'static>;
    type TypeInfo = InputObjectTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(info.input_type_name.as_str())
    }
}

fn generate_insert_field<'r>(
    registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    allframes: Arc<AllFrames>,
    name: &GraphQLName,
) -> Field<'r, DefaultScalarValue> {
    let insert_field_name = insert_class_name(name);
    let info = TerminusTypeInfo {
        class: name.as_static(),
        allframes: allframes.clone(),
    };
    let mut field = registry.field::<Vec<TerminusType>>(insert_field_name.as_str(), &info);
    if let Some(TypeDefinition::Class(d)) = &info.allframes.frames.get(&info.class) {
        let id_arg = registry.arg::<Option<ID>>("_id", &());
        let new_info = InputObjectTypeInfo::new(&info.class, &allframes, MutationMode::Insert);
        let doc_arg = registry.arg::<InsertTypeInputObject>("doc", &new_info);
        field = field.argument(id_arg);
        field = field.argument(doc_arg);

        field
    } else {
        panic!(
            "The class {} was not found and so no insertion arguments could be constructed.",
            &info.class
        )
    }
}

fn create_insert_arg_from_field_definition<'a, 'r>(
    registry: &mut Registry<'r>,
    allframes: Arc<AllFrames>,
    field_name: &'a GraphQLName,
    field_definition: &'a FieldDefinition,
) -> Argument<'r, DefaultScalarValue> {
    match field_definition {
        FieldDefinition::Required(c) => match c {
            super::frame::BaseOrDerived::Base(base_type) => match base_type_kind(base_type) {
                crate::value::BaseTypeKind::String => {
                    registry.arg::<String>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::SmallInteger => {
                    registry.arg::<i32>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::BigIntger => {
                    registry.arg::<BigInt>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::Boolean => {
                    registry.arg::<bool>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::DateTime => {
                    registry.arg::<DateTime>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::Float => registry.arg::<f64>(field_name.as_str(), &()),
                crate::value::BaseTypeKind::Decimal => {
                    registry.arg::<BigFloat>(field_name.as_str(), &())
                }
            },
            super::frame::BaseOrDerived::Derived(c) => {
                if let Some(class_def) = allframes.frames.get(c) {
                    match class_def {
                        TypeDefinition::Class(_) => {
                            let new_info =
                                InputObjectTypeInfo::new(c, &allframes, MutationMode::Insert);
                            registry.arg::<InsertTypeInputObject>(field_name.as_str(), &new_info)
                        }
                        TypeDefinition::Enum(_) => registry.arg::<TerminusEnum>(
                            field_name.as_str(),
                            &(c.clone(), allframes.clone()),
                        ),
                    }
                } else {
                    panic!("No class named {}", c)
                }
            }
        },
        FieldDefinition::Optional(c) => match c {
            super::frame::BaseOrDerived::Base(base_type) => match base_type_kind(base_type) {
                crate::value::BaseTypeKind::String => {
                    registry.arg::<Option<String>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::SmallInteger => {
                    registry.arg::<Option<i32>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::BigIntger => {
                    registry.arg::<Option<BigInt>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::Boolean => {
                    registry.arg::<Option<bool>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::DateTime => {
                    registry.arg::<Option<DateTime>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::Float => {
                    registry.arg::<Option<f64>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::Decimal => {
                    registry.arg::<Option<BigFloat>>(field_name.as_str(), &())
                }
            },
            super::frame::BaseOrDerived::Derived(c) => {
                if let Some(class_def) = allframes.frames.get(c) {
                    match class_def {
                        TypeDefinition::Class(_) => {
                            let new_info =
                                InputObjectTypeInfo::new(c, &allframes, MutationMode::Insert);
                            registry.arg::<Option<InsertTypeInputObject>>(
                                field_name.as_str(),
                                &new_info,
                            )
                        }
                        TypeDefinition::Enum(_) => registry.arg::<Option<TerminusEnum>>(
                            field_name.as_str(),
                            &(c.clone(), allframes.clone()),
                        ),
                    }
                } else {
                    panic!("No class named {}", c)
                }
            }
        },
        FieldDefinition::Set(class)
        | FieldDefinition::List(class)
        | FieldDefinition::Array { class, .. }
        | FieldDefinition::Cardinality { class, .. } => match class {
            super::frame::BaseOrDerived::Base(base_type) => match base_type_kind(base_type) {
                crate::value::BaseTypeKind::String => {
                    registry.arg::<Vec<String>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::SmallInteger => {
                    registry.arg::<Vec<i32>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::BigIntger => {
                    registry.arg::<Vec<BigInt>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::Boolean => {
                    registry.arg::<Vec<bool>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::DateTime => {
                    registry.arg::<Vec<DateTime>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::Float => {
                    registry.arg::<Vec<f64>>(field_name.as_str(), &())
                }
                crate::value::BaseTypeKind::Decimal => {
                    registry.arg::<Vec<BigFloat>>(field_name.as_str(), &())
                }
            },
            super::frame::BaseOrDerived::Derived(c) => {
                if let Some(class_def) = allframes.frames.get(c) {
                    match class_def {
                        TypeDefinition::Class(_) => {
                            let new_info =
                                InputObjectTypeInfo::new(c, &allframes, MutationMode::Insert);
                            registry
                                .arg::<Vec<InsertTypeInputObject>>(field_name.as_str(), &new_info)
                        }
                        TypeDefinition::Enum(_) => registry.arg::<Vec<TerminusEnum>>(
                            field_name.as_str(),
                            &(c.clone(), allframes.clone()),
                        ),
                    }
                } else {
                    panic!("No class named {}", c)
                }
            }
        },
    }
}

fn add_update_arguments<'r>(
    info: &TerminusTypeInfo,
    registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    mut field: Field<'r, DefaultScalarValue>,
    class_definition: &ClassDefinition,
) -> Field<'r, DefaultScalarValue> {
    todo!()
}

fn add_delete_arguments<'r>(
    info: &TerminusTypeInfo,
    registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    mut field: Field<'r, DefaultScalarValue>,
    class_definition: &ClassDefinition,
) -> Field<'r, DefaultScalarValue> {
    todo!()
}

fn check_write_auth(
    executor: &juniper::Executor<TerminusContext<'static>, DefaultScalarValue>,
) -> PrologResult<bool> {
    let prolog_context = &executor.context().context;
    let system_transaction_term = &executor.context().system_transaction_term;
    let transaction_term = &executor.context().transaction_term;
    let auth = &executor.context().system_info.user;
    let auth_term = prolog_context.new_term_ref();
    auth_term.unify(auth)?;
    Ok(attempt(prolog_context.call_once(
        pred!("capabilities:auth_instance_write_access/3"),
        [system_transaction_term, &auth_term, transaction_term],
    ))?)
}

impl GraphQLValue for TerminusMutationRoot {
    type Context = TerminusContext<'static>;
    type TypeInfo = TerminusTypeCollectionInfo;

    fn type_name<'i>(&self, _info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some("TerminusMutation")
    }

    fn resolve_field(
        &self,
        _info: &Self::TypeInfo,
        field_name: &str,
        arguments: &juniper::Arguments<DefaultScalarValue>,
        executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult<DefaultScalarValue> {
        let prolog_context = &executor.context().context;
        let passes_write_auth =
            result_to_execution_result(prolog_context, check_write_auth(executor))?;
        if !passes_write_auth {
            return Err("Not authorized for write".into());
        };
        match field_name {
            "_insertDocuments" => {
                let json = arguments.get::<String>("json");
                if json.is_none() {
                    return Err("no documents specified".into());
                }
                let json = json.unwrap();
                let graph_type = arguments
                    .get::<String>("graph_type")
                    .unwrap_or("InstanceGraph".to_string());
                let raw_json = arguments.get::<bool>("raw_json").unwrap_or(false);
                result_to_execution_result(
                    prolog_context,
                    self.call_insert_doc(
                        prolog_context,
                        &executor.context().transaction_term,
                        &json,
                        &graph_type,
                        raw_json,
                    ),
                )
            }
            "_deleteDocuments" => {
                let ids = arguments.get::<Vec<String>>("ids");
                if ids.is_none() {
                    return Err("no documents specified".into());
                }
                let ids = ids.unwrap();
                let graph_type = arguments
                    .get::<String>("graph_type")
                    .unwrap_or("InstanceGraph".to_string());
                result_to_execution_result(
                    prolog_context,
                    self.call_delete_doc(
                        prolog_context,
                        &executor.context().transaction_term,
                        &ids,
                        &graph_type,
                    ),
                )
            }
            "_replaceDocuments" => {
                let json = arguments.get::<String>("json");
                if json.is_none() {
                    return Err("no documents specified".into());
                }
                let json = json.unwrap();
                let graph_type = arguments
                    .get::<String>("graph_type")
                    .unwrap_or("InstanceGraph".to_string());
                let raw_json = arguments.get::<bool>("raw_json").unwrap_or(false);
                let create = arguments.get::<bool>("create").unwrap_or(false);
                result_to_execution_result(
                    prolog_context,
                    self.call_replace_doc(
                        prolog_context,
                        &executor.context().transaction_term,
                        &json,
                        &graph_type,
                        raw_json,
                        create,
                    ),
                )
            }
            "_commitInfo" => {
                if let Some(author) = arguments.get::<String>("author") {
                    result_to_execution_result(
                        prolog_context,
                        executor.context().author_term.unify(author),
                    )?;
                }
                if let Some(message) = arguments.get::<String>("message") {
                    result_to_execution_result(
                        prolog_context,
                        executor.context().message_term.unify(message),
                    )?;
                }

                Ok(true.into())
            }
            _ => Err("uknown field".into()),
        }
    }
}

impl TerminusMutationRoot {
    fn call_insert_doc(
        &self,
        context: &GenericQueryableContext<'static>,
        transaction_term: &Term,
        json: &str,
        graph_type: &str,
        raw_json: bool,
    ) -> PrologResult<juniper::Value> {
        let frame = context.open_frame();
        let [string_term, graph_type_term, raw_json_term, full_replace_term, doc_merge_term, ids_term] =
            frame.new_term_refs();
        let graph_type_atom = if graph_type == "SchemaGraph" {
            atom!("schema")
        } else {
            atom!("instance")
        };
        string_term.put(json)?;
        graph_type_term.put(&graph_type_atom)?;
        raw_json_term.put(&raw_json)?;
        full_replace_term.put(&false)?;
        doc_merge_term.put(&false)?;

        let insert_doc = pred!("api_document:api_insert_documents_core_string/7");
        frame.call_once(
            insert_doc,
            [
                transaction_term,
                &string_term,
                &graph_type_term,
                &raw_json_term,
                &full_replace_term,
                &doc_merge_term,
                &ids_term,
            ],
        )?;
        let ids: Vec<String> = ids_term.get_ex()?;
        frame.close();
        Ok(juniper::Value::List(
            ids.into_iter().map(|id| id.to_string().into()).collect(),
        ))
    }

    fn call_delete_doc(
        &self,
        context: &GenericQueryableContext<'static>,
        transaction_term: &Term,
        ids: &[String],
        graph_type: &str,
    ) -> PrologResult<juniper::Value> {
        let frame = context.open_frame();
        let [graph_type_term, ids_term] = frame.new_term_refs();

        let graph_type_atom = if graph_type == "SchemaGraph" {
            atom!("schema")
        } else {
            atom!("instance")
        };

        ids_term.unify(ids)?;
        graph_type_term.put(&graph_type_atom)?;

        let delete_doc = pred!("api_document:api_delete_documents_by_ids/3");
        frame.call_once(delete_doc, [transaction_term, &graph_type_term, &ids_term])?;

        frame.close();
        Ok(juniper::Value::List(
            ids.iter().map(|id| id.to_string().into()).collect(),
        ))
    }

    fn call_replace_doc(
        &self,
        context: &GenericQueryableContext<'static>,
        transaction_term: &Term,
        json: &str,
        graph_type: &str,
        raw_json: bool,
        create: bool,
    ) -> PrologResult<juniper::Value> {
        let frame = context.open_frame();
        let [string_term, graph_type_term, raw_json_term, ids_term, create_term] =
            frame.new_term_refs();

        let graph_type_atom = if graph_type == "SchemaGraph" {
            atom!("schema")
        } else {
            atom!("instance")
        };

        string_term.put(json)?;
        graph_type_term.put(&graph_type_atom)?;
        raw_json_term.put(&raw_json)?;
        create_term.put(&create)?;

        let replace_doc = pred!("api_document:api_replace_documents_core_string/6");
        frame.call_once(
            replace_doc,
            [
                transaction_term,
                &string_term,
                &graph_type_term,
                &raw_json_term,
                &create_term,
                &ids_term,
            ],
        )?;

        let ids: Vec<String> = ids_term.get_ex()?;
        frame.close();
        Ok(juniper::Value::List(
            ids.into_iter().map(|id| id.to_string().into()).collect(),
        ))
    }
}

pub struct TerminusTypeMutation {
    id: u64,
}

impl GraphQLType for TerminusTypeMutation {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(info.class.as_str())
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

impl TerminusTypeMutation {
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
        class_name: &GraphQLName,
        d: &ClassDefinition,
        info: &<Self as GraphQLValue>::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        todo!()
    }
}

impl GraphQLValue for TerminusTypeMutation {
    type Context = TerminusContext<'static>;

    type TypeInfo = TerminusTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(info.class.as_str())
    }

    fn resolve_field(
        &self,
        info: &Self::TypeInfo,
        field_name: &str,
        arguments: &juniper::Arguments,
        executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult {
        todo!()
    }
}
