use juniper::{DefaultScalarValue, GraphQLType, GraphQLValue, ID};
use swipl::{
    atom, pred,
    prelude::{Atom, GenericQueryableContext},
    result::{attempt, PrologResult},
    term::Term,
};

use crate::graphql::schema::GraphQLJSON;

use super::schema::{result_to_execution_result, GraphType, TerminusContext};

pub struct TerminusMutationRoot;

impl GraphQLType for TerminusMutationRoot {
    fn name(_info: &Self::TypeInfo) -> Option<&str> {
        Some("TerminusMutation")
    }

    fn meta<'r>(
        _info: &Self::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
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

        registry
            .build_object_type::<TerminusMutationRoot>(
                &(),
                &[
                    insert_documents_field,
                    replace_documents_field,
                    delete_documents_field,
                    commit_info_field,
                ],
            )
            .into_meta()
    }
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
    type TypeInfo = ();

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
        eprintln!("{:?}", ids);
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
