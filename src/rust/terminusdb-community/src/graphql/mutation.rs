use juniper::{DefaultScalarValue, GraphQLType, GraphQLValue, ID};
use swipl::{
    atom, pred,
    prelude::{Atom, Context, QueryableContextType},
    result::PrologResult,
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
        let field = registry
            .field::<Vec<ID>>("_insertDocuments", &())
            .argument(registry.arg::<GraphQLJSON>("json", &()));

        registry
            .build_object_type::<TerminusMutationRoot<'a, C>>(&(), &[field])
            .into_meta()
    }
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
        match field_name {
            "_insertDocuments" => {
                let json = arguments.get::<String>("json");
                if json.is_none() {
                    return Err("no documents specified".into());
                }
                let json = json.unwrap();
                let prolog_context = executor.context().context;
                result_to_execution_result(
                    prolog_context,
                    self.call_insert_doc(
                        executor.context().context,
                        &executor.context().transaction_term,
                        &json,
                    ),
                )
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
