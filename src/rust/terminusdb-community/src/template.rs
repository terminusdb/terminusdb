use handlebars::Handlebars;
use serde_json::Value;
use swipl::prelude::*;

use std::sync::Arc;

predicates! {
    #[module("handlebars")]
    semidet fn handlebars_context(context, template_list_term, output_term) {
        let mut handlebars = Handlebars::new();

        for template_pair_term in context.term_list_iter(template_list_term) {
            let [type_name_term, template_term] = context.compound_terms(&template_pair_term)?;
            let type_name: PrologText = type_name_term.get_ex()?;
            let template: PrologText = template_term.get_ex()?;
            if let Err(e) = handlebars.register_template_string(&*type_name, &*template) {
                let msg = e.to_string();
                let line = e.line_no.unwrap_or(0) as u64;
                let column = e.column_no.unwrap_or(0) as u64;

                context.raise_exception(&term!{context: error(handlebars_template_error(#msg, #line, #column))}?)?;
            }
        }

        output_term.unify(WrappedHandlebars(Arc::new(handlebars)))
    }
    #[module("handlebars")]
    semidet fn handlebars_render_template(context, handlebars_term, type_name_term, json_string_term, output_term) {
        let handlebars: WrappedHandlebars = handlebars_term.get_ex()?;
        let type_name: PrologText = type_name_term.get_ex()?;
        let json_string: PrologText = json_string_term.get_ex()?;

        let result: Result<Value,_> = serde_json::from_str(&*json_string);
        if let Some(e) = result.as_ref().err() {
            let msg = e.to_string();
            context.raise_exception(&term!{context: error(handlebars_json_parse_error(#msg))}?)?;
        }
        let value = result.unwrap();

        match handlebars.render(&*type_name, &value) {
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

wrapped_arc_blob!(
    "handlebars_context",
    WrappedHandlebars,
    Handlebars<'static>,
    defaults
);

pub fn register() {
    register_handlebars_context();
    register_handlebars_render_template();
}
