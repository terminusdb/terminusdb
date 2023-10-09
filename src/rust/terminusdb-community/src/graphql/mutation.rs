use juniper::{DefaultScalarValue, GraphQLType, GraphQLValue, ID};
use swipl::{
    atom, pred,
    prelude::{Atom, Context, QueryableContextType},
    result::{attempt, PrologResult},
    term::Term,
};

use crate::graphql::schema::GraphQLJSON;

use super::schema::{result_to_execution_result, TerminusContext};

pub struct TerminusMutationRoot<'a, C: QueryableContextType> {
    _c: std::marker::PhantomData<&'a Context<'a, C>>,
}

impl<'a, C: QueryableContextType> TerminusMutationRoot<'a, C> {
    pub fn new() -> Self {
        Self {
            _c: Default::default(),
        }
    }
}

impl<'a, C: QueryableContextType> GraphQLType for TerminusMutationRoot<'a, C> {
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
            .argument(registry.arg::<GraphQLJSON>("json", &()));
        let commit_info_field = registry
            .field::<bool>("_commitInfo", &())
            .argument(registry.arg::<Option<String>>("author", &()))
            .argument(registry.arg::<Option<String>>("message", &()));

        registry
            .build_object_type::<TerminusMutationRoot<'a, C>>(
                &(),
                &[insert_documents_field, commit_info_field],
            )
            .into_meta()
    }
}

fn check_write_auth<C: QueryableContextType>(
    executor: &juniper::Executor<TerminusContext<'_, C>, DefaultScalarValue>,
) -> PrologResult<bool> {
    let prolog_context = executor.context().context;
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

impl<'a, C: QueryableContextType> GraphQLValue for TerminusMutationRoot<'a, C> {
    type Context = TerminusContext<'a, C>;
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
        let prolog_context = executor.context().context;
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
                result_to_execution_result(
                    prolog_context,
                    self.call_insert_doc(
                        prolog_context,
                        &executor.context().transaction_term,
                        &json,
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

impl<'a, C: QueryableContextType> TerminusMutationRoot<'a, C> {
    fn call_insert_doc(
        &self,
        context: &Context<'a, C>,
        transaction_term: &Term,
        json: &str,
    ) -> PrologResult<juniper::Value> {
        let frame = context.open_frame();
        let [string_term, graph_type_term, raw_json_term, full_replace_term, doc_merge_term, ids_term] =
            frame.new_term_refs();

        string_term.put(json)?;
        graph_type_term.put(&atom!("instance"))?;
        raw_json_term.put(&false)?;
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

        let ids: Vec<Atom> = ids_term.get_ex()?;
        frame.close();
        Ok(juniper::Value::List(
            ids.into_iter().map(|id| id.to_string().into()).collect(),
        ))
    }
}
