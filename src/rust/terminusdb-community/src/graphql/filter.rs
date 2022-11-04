use std::sync::Arc;

use juniper::{
    meta::Field, DefaultScalarValue, FromInputValue, GraphQLInputObject, GraphQLType, GraphQLValue,
    InputValue, Registry,
};

use crate::value::{base_type_kind, BaseTypeKind};

use super::frame::{AllFrames, ClassDefinition, KeyDefinition, TypeDefinition};

fn generate_filter_argument<'r, T: GraphQLType>(
    registry: &mut Registry<'r, DefaultScalarValue>,
    definition: &ClassDefinition,
    all_frames: &AllFrames,
) {
    todo!()
}

pub struct FilterInputObject;

pub struct FilterInputObjectTypeInfo {
    filter_type_name: String,
    type_name: String,
    frames: Arc<AllFrames>,
}

impl FilterInputObjectTypeInfo {
    pub fn new(type_name: &str, all_frames: &Arc<AllFrames>) -> Self {
        Self {
            filter_type_name: format!("{type_name}_Filter"),
            type_name: type_name.to_string(),
            frames: all_frames.clone(),
        }
    }
}

impl GraphQLType for FilterInputObject {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.filter_type_name)
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        if let TypeDefinition::Class(d) = &info.frames.frames[&info.type_name] {
            let args: Vec<_> = d
                .fields()
                .iter()
                .filter_map(|(name, field_definition)| {
                    if let Some(base_type) = field_definition.base_type() {
                        Some(registry.arg::<StringFilterInputObject>(name, &()))
                    } else {
                        None
                    }
                })
                .collect();
            registry
                .build_input_object_type::<FilterInputObject>(info, &args)
                .into_meta()
        } else {
            panic!("expected type of kind class but found something else");
        }
    }
}

impl FromInputValue for FilterInputObject {
    fn from_input_value(v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        match v {
            InputValue::Object(o) => {
                todo!()
            }
            _ => None,
        }
    }
}

impl GraphQLValue for FilterInputObject {
    type Context = ();

    type TypeInfo = FilterInputObjectTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.filter_type_name)
    }
}

#[derive(GraphQLInputObject)]
struct StringFilterInputObject {
    #[allow(non_snake_case)]
    GT: Option<String>,
}

/*

struct FilterLexicalBaseFieldInputObject;

impl FilterLexicalBaseFieldInputObject {
    fn comparator_fields<'r, T>(
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> Vec<Field<'r, T>> {
        todo!();
    }
}

impl GraphQLType for FilterLexicalBaseFieldInputObject {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.input_object_type_name)
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let base_type = info.type_name;
        let kind = base_type_kind(base_type);
        let mut arguments : Vec<_> =
            match kind {
                BaseTypeKind::String => {
                    registry.arg::<Option<
                }
                _ => todo!();
            }
        registry.build_input_object_type::<FilterLexicalBaseFieldInputObject>(info.filter_type_name, &arguments)
        todo!()
    }
}

impl GraphQLValue for FilterLexicalBaseFieldInputObject {
    type Context = ();

    type TypeInfo = FilterLexicalBaseFieldInputObjectTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.input_object_type_name)
    }
}

struct FilterLexicalBaseFieldInputObjectTypeInfo {
    input_object_type_name: String,
    base_type_name: String,
}

impl FilterLexicalBaseFieldInputObjectTypeInfo {
    fn new(base_type_name: &str) -> Self {
        Self {
            input_object_type_name: format!("{}_BasetypeFilter", base_type_name),
            base_type_name: base_type_name.to_string(),
        }
    }
}
*/
