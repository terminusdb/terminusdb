use std::marker::PhantomData;
use std::sync::Arc;

use juniper::meta::{DeprecationStatus, EnumValue};
use juniper::{DefaultScalarValue, GraphQLType, GraphQLValue, ScalarValue, FromInputValue, InputValue, Value, FieldError};
use swipl::prelude::*;
use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;

use crate::types::{transaction_schema_layer, transaction_instance_layer};

use super::frame::*;
use super::top::Info;

macro_rules! execute_prolog {
    ($context:ident, $body:block) => {
        {
            let result: PrologResult<()> = {
                $body
            };

            result_to_string_result($context, result)
                .map_err(|e| match e {
                    PrologStringError::Failure => FieldError::new(
                        "prolog call failed",
                        Value::Null),
                    PrologStringError::Exception(e) => FieldError::new(
                        e,
                        Value::Null)
                })
        }
    }
}
    

pub struct TerminusContext<'a, C: QueryableContextType> {
    pub schema: SyncStoreLayer,
    pub instance: Option<SyncStoreLayer>,
    pub context: &'a Context<'a, C>
}


impl<'a, C: QueryableContextType> TerminusContext<'a, C> {
    pub fn new(
        context: &'a Context<'a, C>,
        transaction_term: &Term
    ) -> PrologResult<TerminusContext<'a,C>> {
        let schema = transaction_schema_layer(context, transaction_term)?.expect("missing schema layer");
        let instance = transaction_instance_layer(context, transaction_term)?;

        Ok(TerminusContext { context, schema, instance})
    }
}

pub struct TerminusTypeCollection<'a,C:QueryableContextType> {
    _c: std::marker::PhantomData<&'a Context<'a, C>>
}

impl<'a, C:QueryableContextType> TerminusTypeCollection<'a, C> {
    pub fn new() -> Self {
        Self {
            _c: Default::default()
        }
    }
}

impl<'a,C:QueryableContextType+'a> GraphQLType for TerminusTypeCollection<'a,C> {
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
                registry.field::<Vec<TerminusType>>(name, &(name.to_owned(), info.clone()))
            }
        }).collect();
        
        registry.build_object_type::<TerminusTypeCollection<'a,C>>(info, &fields).into_meta()
    }
}

impl<'a, C:QueryableContextType> GraphQLValue for TerminusTypeCollection<'a,C> {
    type Context = TerminusContext<'a, C>;

    type TypeInfo = Arc<AllFrames>;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some("TerminusTypeCollection")
    }

    fn resolve_field(
        &self,
        info: &Self::TypeInfo,
        field_name: &str,
        _arguments: &juniper::Arguments<DefaultScalarValue>,
        executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult<DefaultScalarValue> {
        //let instance = executor.context().instance;
        //let field_id = 
        //executor.resolve(&(field_name.to_string(), info.clone()), &vec![TerminusType,TerminusType])
        let context = executor.context().context;
        let result;
        execute_prolog!(context, {
            let frame = context.open_frame();
            let x_term = frame.new_term_ref();
            let term = term!{frame: format(string(#&x_term), "hello, ~q, ~q", [world])}?;
            frame.call_term_once(&term)?;
            result = x_term.get::<String>()?;

            Ok(())
        })?;

        Ok(Value::Scalar(DefaultScalarValue::String(result)))
    }
}

pub struct TerminusType;
//{
//    id: u64
//} 
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

pub struct TerminusEnum {
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
        Some(&info.0)
    }

    fn resolve_field(
        &self,
        _info: &Self::TypeInfo,
        _field_name: &str,
        _arguments: &juniper::Arguments,
        _executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult {
        //Ok(Value::Scalar(DefaultScalarValue::String("duck!".to_string())))
        Ok(Value::List(vec![Value::Scalar(DefaultScalarValue::String("cow!".to_string())),
                            Value::Scalar(DefaultScalarValue::String("duck!".to_string()))]))
    }
}
