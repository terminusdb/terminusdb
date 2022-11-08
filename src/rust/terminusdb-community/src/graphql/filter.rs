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
    pub edges: Vec<(juniper::Spanning<String>, juniper::Spanning<InputValue>)>,
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
                        Some(registry.arg::<Option<CollectionFilterInputObject>>(
                            name,
                            &CollectionFilterInputObjectTypeInfo::new(&c, &info.frames),
                        ))
                    } else if let Some(base_type) = field_definition.base_type() {
                        let kind = base_type_kind(base_type);
                        match kind {
                            BaseTypeKind::String => {
                                Some(registry.arg::<Option<StringFilterInputObject>>(name, &()))
                            }
                            BaseTypeKind::SmallInteger => Some(registry.arg::<Option<
                                SmallIntegerFilterInputObject,
                            >>(
                                name, &()
                            )),
                            BaseTypeKind::BigIntger => {
                                Some(registry.arg::<Option<BigIntFilterInputObject>>(name, &()))
                            }
                            BaseTypeKind::Boolean => {
                                Some(registry.arg::<Option<BooleanFilterInputObject>>(name, &()))
                            }
                            BaseTypeKind::Float => {
                                Some(registry.arg::<Option<FloatFilterInputObject>>(name, &()))
                            }
                            BaseTypeKind::DateTime => {
                                Some(registry.arg::<Option<DateTimeFilterInputObject>>(name, &()))
                            }
                        }
                    } else if let Some(enum_type) = field_definition.enum_type() {
                        Some(registry.arg::<Option<EnumFilterInputObject>>(
                            name,
                            &FilterInputObjectTypeInfo::new(&enum_type, &info.frames),
                        ))
                    } else {
                        let c = field_definition.range();
                        Some(registry.arg::<Option<FilterInputObject>>(
                            name,
                            &FilterInputObjectTypeInfo::new(&c, &info.frames),
                        ))
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
    pub edges: Vec<(juniper::Spanning<String>, juniper::Spanning<InputValue>)>,
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
            InputValue::Object(o) => Some(Self { edges: o.clone() }),
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

pub struct EnumFilterInputObject {
    pub enum_value: String,
}

pub struct EnumFilterInputObjectTypeInfo {
    filter_type_name: String,
    type_name: String,
    frames: Arc<AllFrames>,
}

impl EnumFilterInputObjectTypeInfo {
    pub fn new(type_name: &str, all_frames: &Arc<AllFrames>) -> Self {
        Self {
            filter_type_name: format!("{type_name}_Enum_Filter"),
            type_name: type_name.to_string(),
            frames: all_frames.clone(),
        }
    }
}

impl GraphQLType for EnumFilterInputObject {
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
            .build_input_object_type::<EnumFilterInputObject>(info, &args)
            .into_meta()
    }
}

impl FromInputValue for EnumFilterInputObject {
    fn from_input_value(v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        match v {
            InputValue::Object(o) => Some(Self { edges: o.clone() }),
            _ => None,
        }
    }
}

impl GraphQLValue for EnumFilterInputObject {
    type Context = ();

    type TypeInfo = EnumFilterInputObjectTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.filter_type_name)
    }
}

#[derive(GraphQLInputObject)]
#[allow(non_snake_case)]
pub struct StringFilterInputObject {
    pub eq: Option<String>,
    pub ne: Option<String>,
    pub lt: Option<String>,
    pub le: Option<String>,
    pub gt: Option<String>,
    pub ge: Option<String>,
    pub startsWith: Option<String>,
    pub allOfTerms: Option<Vec<String>>,
    pub anyOfTerms: Option<Vec<String>>,
}

#[derive(GraphQLInputObject)]
pub struct BigIntFilterInputObject {
    pub eq: Option<BigInt>,
    pub ne: Option<BigInt>,
    pub lt: Option<BigInt>,
    pub le: Option<BigInt>,
    pub gt: Option<BigInt>,
    pub ge: Option<BigInt>,
}

#[derive(GraphQLInputObject)]
pub struct SmallIntegerFilterInputObject {
    pub eq: Option<i32>,
    pub ne: Option<i32>,
    pub lt: Option<i32>,
    pub le: Option<i32>,
    pub gt: Option<i32>,
    pub ge: Option<i32>,
}

#[derive(GraphQLInputObject)]
pub struct FloatFilterInputObject {
    pub eq: Option<f64>,
    pub ne: Option<f64>,
    pub lt: Option<f64>,
    pub le: Option<f64>,
    pub gt: Option<f64>,
    pub ge: Option<f64>,
}

#[derive(GraphQLInputObject)]
pub struct BooleanFilterInputObject {
    pub eq: Option<bool>,
    pub ne: Option<bool>,
}

#[derive(GraphQLInputObject)]
pub struct DateTimeFilterInputObject {
    pub eq: Option<DateTime>,
    pub ne: Option<DateTime>,
    pub lt: Option<DateTime>,
    pub le: Option<DateTime>,
    pub gt: Option<DateTime>,
    pub ge: Option<DateTime>,
}
