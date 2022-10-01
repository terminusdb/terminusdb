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
                .map(|(field_name, _field_definition)| registry.field::<&String>(field_name, &()))
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
