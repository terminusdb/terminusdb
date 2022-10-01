use std::marker::PhantomData;
use std::sync::Arc;

use juniper::{DefaultScalarValue, GraphQLType, GraphQLValue, ScalarValue};

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

        let fields: Vec<_> = info.frames.iter().map(|(name, _typedef)| {
            registry.field::<TerminusType>(name, &(name.to_owned(), info.clone()))
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

impl GraphQLType for TerminusType {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.0)
    }

    fn meta<'r>(info: &Self::TypeInfo, registry: &mut juniper::Registry<'r, DefaultScalarValue>) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r {
        let (name, frames) = info;
        let frame = &frames.frames[name];
        if let TypeDefinition::Class(d) = frame {
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
        } else {
            todo!()
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
