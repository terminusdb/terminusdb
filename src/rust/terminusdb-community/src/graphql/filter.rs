use std::sync::Arc;

use juniper::{
    meta::Field, DefaultScalarValue, FromInputValue, GraphQLInputObject, GraphQLType, GraphQLValue,
    InputValue, Registry,
};

use crate::value::{base_type_kind, BaseTypeKind};

use super::{
    frame::{AllFrames, ClassDefinition, KeyDefinition, TypeDefinition},
    schema::{BigInt, DateTime},
};

fn generate_filter_argument<'r, T: GraphQLType>(
    registry: &mut Registry<'r, DefaultScalarValue>,
    definition: &ClassDefinition,
    all_frames: &AllFrames,
) {
    todo!()
}

pub struct FilterInputObject {
    edges: Vec<(juniper::Spanning<String>, juniper::Spanning<InputValue>)>,
}

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
                    let kind = field_definition.kind();
                    if kind.is_collection() {
                        let c = field_definition.range();
                        Some(registry.arg::<CollectionFilterInputObject>(
                            name,
                            &CollectionFilterInputObjectTypeInfo::new(&c, &info.frames),
                        ))
                    } else if let Some(base_type) = field_definition.base_type() {
                        let kind = base_type_kind(base_type);
                        match kind {
                            BaseTypeKind::String => {
                                Some(registry.arg::<StringFilterInputObject>(name, &()))
                            }
                            BaseTypeKind::SmallInteger => {
                                Some(registry.arg::<SmallIntegerFilterInputObject>(name, &()))
                            }
                            BaseTypeKind::BigIntger => {
                                Some(registry.arg::<BigIntFilterInputObject>(name, &()))
                            }
                            BaseTypeKind::Boolean => {
                                Some(registry.arg::<BooleanFilterInputObject>(name, &()))
                            }
                            BaseTypeKind::Float => {
                                Some(registry.arg::<FloatFilterInputObject>(name, &()))
                            }
                            BaseTypeKind::DateTime => {
                                Some(registry.arg::<DateTimeFilterInputObject>(name, &()))
                            }
                        }
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
            InputValue::Object(o) => Some(Self { edges: o.clone() }),
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

pub struct CollectionFilterInputObject {
    filter: Vec<(juniper::Spanning<String>, juniper::Spanning<InputValue>)>,
}

pub struct CollectionFilterInputObjectTypeInfo {
    filter_type_name: String,
    type_name: String,
    frames: Arc<AllFrames>,
}

impl CollectionFilterInputObjectTypeInfo {
    pub fn new(type_name: &str, all_frames: &Arc<AllFrames>) -> Self {
        Self {
            filter_type_name: format!("{type_name}_Collection_Filter"),
            type_name: type_name.to_string(),
            frames: all_frames.clone(),
        }
    }
}

impl GraphQLType for CollectionFilterInputObject {
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
        let mut args: Vec<_> = Vec::with_capacity(2);
        args.push(registry.arg::<Option<FilterInputObject>>(
            "someHave",
            &FilterInputObjectTypeInfo::new(&info.type_name, &info.frames),
        ));
        args.push(registry.arg::<Option<FilterInputObject>>(
            "allHave",
            &FilterInputObjectTypeInfo::new(&info.type_name, &info.frames),
        ));
        registry
            .build_input_object_type::<CollectionFilterInputObject>(info, &args)
            .into_meta()
    }
}

impl FromInputValue for CollectionFilterInputObject {
    fn from_input_value(v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        match v {
            InputValue::Object(o) => Some(Self { filter: o.clone() }),
            _ => None,
        }
    }
}

impl GraphQLValue for CollectionFilterInputObject {
    type Context = ();

    type TypeInfo = CollectionFilterInputObjectTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.filter_type_name)
    }
}

#[derive(GraphQLInputObject)]
#[allow(non_snake_case)]
struct StringFilterInputObject {
    eq: Option<String>,
    lt: Option<String>,
    le: Option<String>,
    gt: Option<String>,
    ge: Option<String>,
    startsWith: Option<String>,
    allOfTerms: Option<Vec<String>>,
    anyOfTerms: Option<Vec<String>>,
}

#[derive(GraphQLInputObject)]
struct BigIntFilterInputObject {
    eq: Option<BigInt>,
    lt: Option<BigInt>,
    le: Option<BigInt>,
    gt: Option<BigInt>,
    ge: Option<BigInt>,
}

#[derive(GraphQLInputObject)]
struct SmallIntegerFilterInputObject {
    eq: Option<i32>,
    lt: Option<i32>,
    le: Option<i32>,
    gt: Option<i32>,
    ge: Option<i32>,
}

#[derive(GraphQLInputObject)]
struct FloatFilterInputObject {
    eq: Option<f64>,
    lt: Option<f64>,
    le: Option<f64>,
    gt: Option<f64>,
    ge: Option<f64>,
}

#[derive(GraphQLInputObject)]
struct BooleanFilterInputObject {
    #[allow(non_snake_case)]
    eq: Option<bool>,
}

#[derive(GraphQLInputObject)]
struct DateTimeFilterInputObject {
    eq: Option<DateTime>,
    lt: Option<DateTime>,
    le: Option<DateTime>,
    gt: Option<DateTime>,
    ge: Option<DateTime>,
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
