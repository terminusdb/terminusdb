use std::marker::PhantomData;
use std::sync::Arc;

use juniper::meta::{DeprecationStatus, EnumValue};
use juniper::{DefaultScalarValue, GraphQLType, GraphQLValue, ScalarValue, FromInputValue, InputValue};

use super::frame::*;
use super::top::Info;

pub struct TerminusTypeCollection;
impl GraphQLType for TerminusTypeCollection {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some("Query")
    }

    fn meta<'r>(info: &Self::TypeInfo, registry: &mut juniper::Registry<'r, DefaultScalarValue>) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r {

        let fields: Vec<_> = info.frames.iter().map(|(name, typedef)| {
            if typedef.kind() == TypeKind::Enum {
                registry.field::<TerminusEnum>(name, &(name.to_owned(), info.clone()))
            }
            else{
                registry.field::<TerminusType>(name, &(name.to_owned(), info.clone()))
            }
        }).collect();
        
        registry.build_object_type::<TerminusTypeCollection>(info, &fields).into_meta()
    }
}

impl GraphQLValue for TerminusTypeCollection {
    type Context = Info;

    type TypeInfo = Arc<AllFrames>;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        todo!()
    }
}

pub struct TerminusType; 
impl TerminusType {
    fn generate_class_type<'r>(d: &ClassDefinition, info: &<Self as GraphQLValue>::TypeInfo, registry: &mut juniper::Registry<'r, DefaultScalarValue>) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r {
        let frames = &info.1;
        let fields: Vec<_> = d.fields.iter()
            .map(|(field_name, field_definition)| {
                if let Some(document_type) = field_definition.document_type() {
                    match field_definition.kind() {
                        FieldKind::Required => registry.field::<TerminusType>(field_name, &(document_type.to_owned(), frames.clone())),
                        FieldKind::Optional => registry.field::<Option<TerminusType>>(field_name, &(document_type.to_owned(), frames.clone())),
                        FieldKind::Array => registry.field::<Vec<Option<TerminusType>>>(field_name, &(document_type.to_owned(), frames.clone())),
                        _ => registry.field::<Vec<TerminusType>>(field_name, &(document_type.to_owned(), frames.clone())),
                    }
                }
                else {
                    registry.field::<&String>(field_name, &())
                }
            })
            .collect();

        registry.build_object_type::<TerminusType>(info, &fields).into_meta()
    }
}

struct TerminusEnum {
    value: String
}

impl GraphQLType for TerminusEnum {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.0)
    }

    fn meta<'r>(info: &Self::TypeInfo, registry: &mut juniper::Registry<'r, DefaultScalarValue>) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r {
        if let TypeDefinition::Enum(e) = &info.1.frames[&info.0] {
            let values: Vec<_> = e.values.iter().map(|v| EnumValue {
                name: v.to_string(),
                description: None,
                deprecation_status: DeprecationStatus::Current
            }).collect();

            registry.build_enum_type::<TerminusEnum>(info, &values).into_meta()
        }
        else {
            panic!("tried to build meta for enum but this is not an enum");
        }
    }
}

impl FromInputValue for TerminusEnum {
    fn from_input_value(v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        match v {
            InputValue::Enum(value) => Some(Self {
                value: value.to_owned()
            }),
            _ => None
        }
    }
}

impl GraphQLValue for TerminusEnum {
    type Context = Info;

    type TypeInfo = (String, Arc<AllFrames>);

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        todo!()
    }
}

impl GraphQLType for TerminusType {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.0)
    }

    fn meta<'r>(info: &Self::TypeInfo, registry: &mut juniper::Registry<'r, DefaultScalarValue>) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r {
        let (name, frames) = info;
        let frame = &frames.frames[name];
        match frame {
            TypeDefinition::Class(d) => Self::generate_class_type(d, info, registry),
            TypeDefinition::Enum(e) => panic!("no enum expected here"),
            _ => todo!()
        }
    }
}

impl GraphQLValue for TerminusType {
    type Context = Info;

    type TypeInfo = (String, Arc<AllFrames>);

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        todo!()
    }

    fn resolve_field(
        &self,
        _info: &Self::TypeInfo,
        _field_name: &str,
        _arguments: &juniper::Arguments,
        _executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult {
        panic!("GraphQLValue::resolve_field() must be implemented by objects and interfaces");
    }
}
