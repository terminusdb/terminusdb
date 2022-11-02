use std::sync::Arc;

use juniper::{GraphQLType, Registry, DefaultScalarValue, GraphQLValue, meta::Field};

use super::frame::{ClassDefinition, AllFrames, TypeDefinition};

fn generate_filter_argument<'r, T: GraphQLType>(registry: &mut Registry<'r, DefaultScalarValue>, definition: &ClassDefinition, all_frames: &AllFrames) {
    for (name, f) in definition.fields.iter() {

    }
}


struct FilterInputObject;

impl GraphQLType for FilterInputObject {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.filter_type_name)
    }

    fn meta<'r>(info: &Self::TypeInfo, registry: &mut Registry<'r, DefaultScalarValue>) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r {
        if let TypeDefinition::Class(d) = &info.frames.frames[&info.type_name] {
            for (name, field_definition) in d.fields.iter() {
            }
            todo!();
        }
        else {
            panic!("expected type of kind class but found something else");
        }
        todo!()
    }
}

impl GraphQLValue for FilterInputObject {
    type Context = ();

    type TypeInfo = FilterInputObjectTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.filter_type_name)
    }
}

struct FilterInputObjectTypeInfo {
    filter_type_name: String,
    type_name: String,
    frames: Arc<AllFrames>
}

struct FilterLexicalBaseFieldInputObject;

impl FilterLexicalBaseFieldInputObject {
    fn comparator_fields<'r, T>(registry: &mut Registry<'r, DefaultScalarValue>) -> Vec<Field<'r, T>> {
        todo!();
    }
}

impl GraphQLType for FilterLexicalBaseFieldInputObject {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.input_object_type_name)
    }

    fn meta<'r>(info: &Self::TypeInfo, registry: &mut Registry<'r, DefaultScalarValue>) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r {

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
            base_type_name: base_type_name.to_string()
        }
    }
}
