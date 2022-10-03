use std::marker::PhantomData;
use std::sync::Arc;

use juniper::meta::{DeprecationStatus, EnumValue};
use juniper::{DefaultScalarValue, GraphQLType, GraphQLValue, ScalarValue, FromInputValue, InputValue, Value, FieldError};
use swipl::prelude::*;
use terminusdb_store_prolog::terminus_store::{Layer, ObjectType};
use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;

use crate::consts::RDF_TYPE;
use crate::types::{transaction_schema_layer, transaction_instance_layer};

use super::frame::*;
use super::top::Info;

macro_rules! execute_prolog {
    ($context:ident, $body:block) => {
        {
            let result: PrologResult<_> = {
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
    pub context: &'a Context<'a, C>,
}


impl<'a, C: QueryableContextType> TerminusContext<'a, C> {
    pub fn new(
        context: &'a Context<'a, C>,
        transaction_term: &Term,
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
                registry.field::<Vec<TerminusType<'a,C>>>(name, &(name.to_owned(), info.clone()))
            }
        }).collect();
        
        registry.build_object_type::<TerminusTypeCollection<'a,C>>(info, &fields).into_meta()
    }
}

impl<'a, C:QueryableContextType> GraphQLValue for TerminusTypeCollection<'a,C> {
    type Context = TerminusContext<'a, C>;

    type TypeInfo = Arc<AllFrames>;

    fn type_name<'i>(&self, _info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some("TerminusTypeCollection")
    }

    fn resolve_field(
        &self,
        info: &Self::TypeInfo,
        field_name: &str,
        _arguments: &juniper::Arguments<DefaultScalarValue>,
        executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult<DefaultScalarValue> {
        let get_type_iterator = || {
            let instance = executor.context().instance.as_ref()?;
            let rdf_type_id = instance.predicate_id(RDF_TYPE)?;
            let type_name_expanded = info.context.expand_schema(field_name);
            let type_name_id = instance.subject_id(&type_name_expanded)?;

            Some(instance.triples_o(type_name_id)
                 .filter(move |t|t.predicate == rdf_type_id)
                 .map(|t|TerminusType::new(t.subject)))
        };

        let types: Vec<_> = get_type_iterator().into_iter()
            .flatten().collect();
        executor.resolve(&(field_name.to_owned(), info.clone()),
                         &types)

        /*
        //let instance = executor.context().instance;
        //let field_id = 
        //executor.resolve(&(field_name.to_string(), info.clone()), &vec![TerminusType,TerminusType])
        let context = executor.context();
        let _: Option<()> = {
            let instance = context.instance?;
            let type_name_expanded = context.prefixes.expand_schema(field_name);
            let type_name_i = context.instance.predicate_id(&RDF_TYPE)?;
            context.instance.
        };

        Ok(Value::Scalar(DefaultScalarValue::String(result)))
         */
    }
}

pub struct TerminusType<'a, C:QueryableContextType> {
    id: u64,
    _x: std::marker::PhantomData<Context<'a, C>>
}

impl<'a, C:QueryableContextType+'a> TerminusType<'a, C> {
    fn new(id: u64) -> Self {
        Self { id, _x: Default::default() }
    }

    fn generate_class_type<'r>(d: &ClassDefinition, info: &<Self as GraphQLValue>::TypeInfo, registry: &mut juniper::Registry<'r, DefaultScalarValue>) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r {
        let frames = &info.1;
        let fields: Vec<_> = d.fields.iter()
            .map(|(field_name, field_definition)| {
                if let Some(document_type) = field_definition.document_type() {
                    match field_definition.kind() {
                        FieldKind::Required => registry.field::<TerminusType<'a,C>>(field_name, &(document_type.to_owned(), frames.clone())),
                        FieldKind::Optional => registry.field::<Option<TerminusType<'a,C>>>(field_name, &(document_type.to_owned(), frames.clone())),
                        FieldKind::Array => registry.field::<Vec<Option<TerminusType<'a,C>>>>(field_name, &(document_type.to_owned(), frames.clone())),
                        _ => registry.field::<Vec<TerminusType<'a,C>>>(field_name, &(document_type.to_owned(), frames.clone())),
                    }
                }
                else {
                    registry.field::<&String>(field_name, &())
                }
            })
            .collect();

        registry.build_object_type::<TerminusType<'a,C>>(info, &fields).into_meta()
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
    type Context = ();

    type TypeInfo = (String, Arc<AllFrames>);

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        todo!()
    }
}

impl<'a,C:QueryableContextType+'a> GraphQLType for TerminusType<'a,C> {
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

impl<'a,C:QueryableContextType+'a> GraphQLValue for TerminusType<'a,C> {
    type Context = TerminusContext<'a, C>;

    type TypeInfo = (String, Arc<AllFrames>);

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.0)
    }

    fn resolve_field(
        &self,
        info: &Self::TypeInfo,
        field_name: &str,
        _arguments: &juniper::Arguments,
        executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult {
        let get_info = || {
            let instance = executor.context().instance.as_ref()?;
            let field_name_expanded = dbg!(info.1.context.expand_schema(field_name));
            let field_id = dbg!(instance.predicate_id(&field_name_expanded))?;

            let frame = dbg!(&info.1.frames[&info.0]);
            let doc_type;
            match frame {
                TypeDefinition::Class(c) => {
                    let field = &c.fields[field_name];
                    doc_type = field.document_type();
                },
                _ => {doc_type = None}
            }

            let object_id = dbg!(instance.single_triple_sp(self.id, field_id))?.object;

            Some((doc_type, object_id))
        };
        let x = get_info();
        eprintln!("x: {:?}", x);
        match x {
           Some((Some(doc_type), id)) => {
               executor.resolve(&(doc_type.to_string(), info.1.clone()), &TerminusType::new(id))
            },
            Some((None, id)) => Ok(Value::Scalar(DefaultScalarValue::String(format!("{:?}", id)))),
           None => Ok(Value::Null)
        }
        //Ok(Value::List(vec![Value::Scalar(DefaultScalarValue::String("cow!".to_string())),
        //                    Value::Scalar(DefaultScalarValue::String("duck!".to_string()))]))
    }
}
