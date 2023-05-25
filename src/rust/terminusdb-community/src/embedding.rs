use std::{collections::HashMap, io::Write, sync::Arc};

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
    DefaultScalarValue, Definition, InputValue, Value,
};
use serde::Serialize;
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
            let document = parse_document_source(&source, &execution_context.root_node.schema);
            if document.is_err() {
                return context.raise_exception(
                    &term! {context: error(embedding_query_parsing_failed(#&*type_name),_)}?,
                );
            }
            let document = document.unwrap();
            let mut ctx = ValidatorContext::new(&execution_context.root_node.schema, &document);
            visit_all_rules(&mut ctx, &document);

            let errors = ctx.into_errors();
            if !errors.is_empty() {
                return context.raise_exception(
                    &term! {context: error(embedding_query_validation_failed(#&*type_name),_)}?,
                );
            }
            // since the document makes use of the boxed string this should be ok, as long as we never expose the static lifetime outside the struct and make sure to drop the document before the source.
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

    fn embedding_doc_for<'a, C: QueryableContextType>(
        &self,
        context: &Context<'a, C>,
        system_term: &Term,
        transaction_term: &Term,
        type_name: &str,
        iri: &str,
    ) -> PrologResult<Value<DefaultScalarValue>> {
        let none_term = context.new_term_ref();
        none_term.unify(atom!("none"))?;
        let execution_context = GraphQLExecutionContext::new_from_context_terms(
            self.types.clone(),
            context,
            &none_term,
            system_term,
            &none_term,
            &none_term,
            transaction_term,
        )?;
        let document = self.get_query_document(&type_name);
        if document.is_none() {
            return context.raise_exception(
                &term! {context: error(no_embedding_query_for_type(#&*type_name),_)}?,
            );
        }
        let document = document.unwrap();
        let id_value: InputValue<DefaultScalarValue> = InputValue::scalar(iri);
        let mut parameters = HashMap::with_capacity(1);
        parameters.insert("id".to_string(), id_value);

        let result = execution_context.execute_query_document(document, &parameters);
        if result.is_err() {
            let error = result.err().unwrap().to_string();
            return context.raise_exception(
                &term! {context: error(graphql_error_for_embedding(#type_name, #iri, #error),_)}?,
            );
        }
        let (docs, errs) = result.unwrap();
        if !errs.is_empty() {
            let error_string = format!("{:?}", errs);
            return context.raise_exception(&term!{context: error(graphql_error_for_embedding(#type_name, #iri, #error_string),_)}?);
        }
        let (_, docs) = docs
            .into_object()
            .expect("graphql result was expected to be an object")
            .into_iter()
            .next()
            .expect("graphql result object was expected to have one field");
        let docs: Vec<Value<DefaultScalarValue>> = match docs {
            Value::List(v) => v,
            _ => panic!("graphql result field was expected to be a list"),
        };
        if docs.is_empty() {
            return context.raise_exception(
                &term! {context: error(no_result_for_embedding(#type_name, #iri),_)}?,
            );
        }

        let first = docs.into_iter().next().unwrap();
        Ok(first)
    }
    fn embedding_string_for<'a, C: QueryableContextType>(
        &self,
        context: &Context<'a, C>,
        system_term: &Term,
        transaction_term: &Term,
        type_name: &str,
        iri: &str,
    ) -> PrologResult<String> {
        let doc =
            self.embedding_doc_for(context, system_term, transaction_term, &*type_name, &*iri)?;

        if self.templates.has_template(&type_name) {
            match self.templates.render(&*type_name, &doc) {
                Ok(result) => Ok(result),
                Err(e) => {
                    let msg = e.to_string();
                    let line = e.line_no.unwrap_or(0) as u64;
                    let column = e.column_no.unwrap_or(0) as u64;
                    context.raise_exception(&term!{context: error(handlebars_render_error_for_embedding(#&*type_name, #&*iri, #msg, #line, #column))}?)
                }
            }
        } else {
            let result = serde_json::to_string(&doc)
                .expect("Couldn't turn a graphql result document into a json string");
            Ok(result)
        }
    }
}

impl Drop for EmbeddingContext {
    fn drop(&mut self) {
        // out of an abundance of caution, we should make sure that
        // the documents get dropped before their source strings do,
        // as the documments contain borrows to this source even
        // though its lifetime was erased.
        for (_source, doc) in self.queries.drain() {
            std::mem::drop(doc);
        }
    }
}

#[derive(Serialize)]
enum IndexOperationType {
    Inserted,
    Changed,
    Deleted,
    Error,
}

impl IndexOperationType {
    fn from_atom(a: &Atom) -> Self {
        if a == &atom!("Inserted") {
            Self::Inserted
        } else if a == &atom!("Changed") {
            Self::Changed
        } else {
            Self::Deleted
        }
    }
}

#[derive(Serialize)]
struct IndexOperation<'a> {
    id: &'a str,
    op: IndexOperationType,
    #[serde(skip_serializing_if = "Option::is_none")]
    string: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    message: Option<String>,
}

predicates! {
    #[module("$embedding")]
    semidet fn embedding_context(context, system_term, transaction_term, templates_term, queries_term, frames_term, embedding_context_term) {
        let embedding_context = EmbeddingContext::new(context, system_term, transaction_term, templates_term, queries_term, frames_term)?;

        embedding_context_term.unify(Arc::new(embedding_context))
    }

    #[module("$embedding")]
    semidet fn embedding_string_for(context, system_term, transaction_term, embedding_context_term, type_term, iri_term, output_term) {
        let type_name: PrologText = type_term.get_ex()?;
        let embedding_context: Arc<EmbeddingContext> = embedding_context_term.get_ex()?;
        let iri: PrologText = iri_term.get_ex()?;

        let result = embedding_context.embedding_string_for(context, system_term, transaction_term, &*type_name, &*iri)?;

        output_term.unify(result)
    }

    #[module("$embedding")]
    semidet fn write_op_for(context, stream_term, system_term, transaction_term, embedding_context_term, type_term, iri_term, op_term) {
        let mut stream: WritablePrologStream = stream_term.get_ex()?;
        let mut op = IndexOperationType::from_atom(&op_term.get_ex()?);
        let type_name: PrologText = type_term.get_ex()?;
        let embedding_context: Arc<EmbeddingContext> = embedding_context_term.get_ex()?;
        let iri: PrologText = iri_term.get_ex()?;
        let mut message = None;
        let string = match op {
            IndexOperationType::Inserted | IndexOperationType::Changed => {
                match embedding_context.embedding_string_for(context, system_term, transaction_term, &*type_name, &*iri) {
                    Ok(result) => Some(result),
                    Err(_e) => {
                        // janky error handling
                        // TODO actually embed reason for failure. should probably trickle down here without being converted into a prolog error right away.
                        op = IndexOperationType::Error;
                        message = Some(format!("Failed to process embedding operation for id {}", &*iri));

                        None
                    }
                }
            },
            IndexOperationType::Deleted => {
                None
            },
            IndexOperationType::Error =>  {
                message = Some(format!("Failed to retrieve embedding for id {}",&*iri));
                None
            }
        };

        let result = IndexOperation {
            id: &*iri,
            op,
            string,
            message
        };

        context.try_or_die_generic(serde_json::to_writer(&mut stream, &result))?;
        context.try_or_die_generic(stream.write_all(b"\n"))
    }
}

pub fn register() {
    register_embedding_context();
    register_embedding_string_for();
    register_write_op_for();
}
