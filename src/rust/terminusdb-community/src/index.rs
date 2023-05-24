use std::{collections::HashMap, pin::Pin, sync::Arc};

use crate::{
    graphql::{
        schema::TerminusTypeCollectionInfo, type_collection_from_term, GraphQLExecutionContext,
    },
    template::handlebars_from_term,
};
use handlebars::Handlebars;
use juniper::{
    parser::parse_document_source,
    validation::{visit_all_rules, ValidatorContext},
    DefaultScalarValue, Definition, InputValue,
};
use swipl::prelude::*;

#[arc_blob("embedding_context", defaults)]
struct EmbeddingContext {
    templates: Handlebars<'static>,
    types: TerminusTypeCollectionInfo,
    queries: HashMap<String, (Box<String>, Vec<Definition<'static, DefaultScalarValue>>)>,
}

impl EmbeddingContext {
    pub fn new<'a, C: QueryableContextType>(
        context: &Context<'a, C>,
        system_term: &Term,
        transaction_term: &Term,
        templates_term: &Term,
        queries_term: &Term,
        frames_term: &Term,
    ) -> PrologResult<Self> {
        let templates = handlebars_from_term(context, templates_term)?;
        let types = type_collection_from_term(context, frames_term)?;

        let mut queries = HashMap::new();
        let none_term = context.new_term_ref();
        none_term.unify(atom!("none"))?;
        let execution_context = GraphQLExecutionContext::new_from_context_terms(
            types.clone(),
            context,
            &none_term,
            &system_term,
            &none_term,
            &none_term,
            &transaction_term,
        )?;

        for type_tuple_term in context.term_list_iter(queries_term) {
            // TODO seriously error handle here
            let [type_name_term, query_term] = context.compound_terms(&type_tuple_term)?;
            let type_name: PrologText = type_name_term.get_ex()?;
            let source: Box<String> = Box::new(query_term.get_ex()?);
            let document =
                parse_document_source(&source, &execution_context.root_node.schema).unwrap();
            let mut ctx = ValidatorContext::new(&execution_context.root_node.schema, &document);
            visit_all_rules(&mut ctx, &document);

            let errors = ctx.into_errors();
            assert!(errors.is_empty());
            // since the document makes use of the pinned string this should be ok, as long as we never expose the static lifetime outside the struct.
            let lifetime_erased_document = unsafe { std::mem::transmute(document) };
            queries.insert(type_name.to_string(), (source, lifetime_erased_document));
        }

        Ok(Self {
            templates,
            types,
            queries,
        })
    }

    fn get_query_document<'a>(
        &'a self,
        t: &str,
    ) -> Option<&'a Vec<Definition<'a, DefaultScalarValue>>> {
        // this accessor makes sure that we only return query documents with a lifetime that will definitely not outlive the query source
        self.queries.get(t).map(|x| &x.1)
    }
}

predicates! {
    #[module("$index")]
    semidet fn embedding_context(context, system_term, transaction_term, templates_term, queries_term, frames_term, embedding_context_term) {
        let embedding_context = EmbeddingContext::new(context, system_term, transaction_term, templates_term, queries_term, frames_term)?;

        embedding_context_term.unify(Arc::new(embedding_context))
    }

    #[module("$index")]
    semidet fn embedding_string_for(context, system_term, transaction_term, embedding_context_term, type_term, iri_term, output_term) {
        let type_name: PrologText = type_term.get_ex()?;
        let embedding_context: Arc<EmbeddingContext> = embedding_context_term.get_ex()?;
        if !embedding_context.templates.has_template(&type_name) {
            return Err(PrologError::Failure);
        }
        let iri: PrologText = iri_term.get_ex()?;

        let none_term = context.new_term_ref();
        none_term.unify(atom!("none"))?;
        let execution_context = GraphQLExecutionContext::new_from_context_terms(embedding_context.types.clone(), context, &none_term, &system_term, &none_term, &none_term, &transaction_term)?;
        let document = embedding_context.get_query_document(&type_name).unwrap();
        let id_value: InputValue<DefaultScalarValue> = InputValue::scalar(&*iri.as_str());
        let mut parameters = HashMap::with_capacity(1);
        parameters.insert("id".to_string(), id_value);

        let (docs, errs) = execution_context.execute_query_document(document, &parameters).unwrap();
        assert!(errs.is_empty());
        let (_, docs) = docs.into_object().unwrap().into_iter().next().unwrap();
        let docs = docs.as_list_value().unwrap();
        assert!(!docs.is_empty());

        let single_doc = &docs[0];
        match embedding_context.templates.render(&*type_name, single_doc) {
            Ok(result) => {
                output_term.unify(result)
            }
            Err(e) => {
                let msg = e.to_string();
                let line = e.line_no.unwrap_or(0) as u64;
                let column = e.column_no.unwrap_or(0) as u64;
                context.raise_exception(&term!{context: error(handlebars_render_error(#msg, #line, #column))}?)
            }
        }
    }
}

pub fn register() {
    register_embedding_context();
    register_embedding_string_for();
}
