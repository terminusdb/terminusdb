use juniper::{meta::Field, DefaultScalarValue, GraphQLType, GraphQLValue, Registry, ID};
use swipl::{
    atom, pred,
    prelude::GenericQueryableContext,
    result::{attempt, PrologResult},
    term::Term,
};

use crate::graphql::{
    naming::{delete_class_name, update_class_name},
    schema::GraphQLJSON,
};

use super::{
    frame::{ClassDefinition, FieldKind, GraphQLName, TypeDefinition},
    naming::insert_class_name,
    schema::{
        result_to_execution_result, GraphType, TerminusContext, TerminusType,
        TerminusTypeCollectionInfo, TerminusTypeInfo,
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
                if let TypeDefinition::Class(c) = typedef {
                    let newinfo = TerminusTypeInfo {
                        class: name.as_static(),
                        allframes: info.allframes.clone(),
                    };
                    let insert_field_name = insert_class_name(name);
                    let insert_field =
                        registry.field::<Vec<TerminusType>>(insert_field_name.as_str(), &newinfo);
                    add_insert_arguments(&newinfo, registry, insert_field, &c);

                    let update_field_name = update_class_name(name);
                    let update_field =
                        registry.field::<Vec<TerminusType>>(update_field_name.as_str(), &newinfo);
                    add_update_arguments(&newinfo, registry, update_field, &c);

                    let delete_field_name = delete_class_name(name);
                    let delete_field =
                        registry.field::<Vec<TerminusType>>(delete_field_name.as_str(), &newinfo);
                    add_delete_arguments(&newinfo, registry, delete_field, &c);

                    vec![insert_field, update_field, delete_field]
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
            .build_object_type::<TerminusMutationRoot>(&info, &mutation_fields)
            .into_meta()
    }
}

fn add_insert_arguments<'r>(
    info: &TerminusTypeInfo,
    registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    mut field: Field<'r, DefaultScalarValue>,
    class_definition: &ClassDefinition,
) -> Field<'r, DefaultScalarValue> {
    todo!()
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
        todo!();
        let frames = &info.allframes;
        let mut fields: Vec<_> = d
            .fields()
            .iter()
            .map(|(field_name, field_definition)| {
                if let Some(document_type) = field_definition.document_type(frames) {
                    let field = Self::register_field::<TerminusTypeMutation>(
                        registry,
                        field_name.as_str(),
                        &TerminusTypeMutationInfo {
                            class: document_type.as_static(),
                            allframes: frames.clone(),
                        },
                        field_definition.kind(),
                    );

                    if field_definition.kind().is_collection() {
                        let class_definition =
                            info.allframes.frames[document_type].as_class_definition();
                        let new_info = TerminusTypeMutationInfo {
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
                        field_name.as_str(),
                        &(enum_type.as_static(), frames.clone()),
                        field_definition.kind(),
                    )
                } else {
                    Self::register_field::<ID>(
                        registry,
                        field_name.as_str(),
                        &(),
                        field_definition.kind(),
                    )
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
                let new_info = TerminusTypeMutationInfo {
                    class: class.as_static(),
                    allframes: frames.clone(),
                };
                let field = Self::register_field::<TerminusTypeMutation>(
                    registry,
                    field_name.as_str(),
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
            let field_name = path_to_class_name(class);
            let class_definition = info.allframes.frames[class].as_class_definition();
            let new_info = TerminusTypeMutationInfo {
                class: class.as_static(),
                allframes: frames.clone(),
            };
            let field = Self::register_field::<TerminusTypeMutation>(
                registry,
                field_name.as_str(),
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
            if restriction.on == *class_name {
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
            .build_object_type::<TerminusTypeMutation>(info, &fields)
            .into_meta()
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
