use std::sync::Arc;

use juniper::{
    DefaultScalarValue, FromInputValue, GraphQLInputObject, GraphQLType, GraphQLValue, InputValue,
    Registry, ID,
};

use crate::value::{base_type_kind, BaseTypeKind};

use super::{
    frame::{AllFrames, BaseOrDerived, GraphQLName, TypeDefinition},
    naming::{collection_filter_name, enum_filter_name, filter_name, restriction_name},
    query::EnumOperation,
    schema::{BigFloat, BigInt, DateTime, GeneratedEnum, GeneratedEnumTypeInfo, TerminusEnum},
};

pub struct FilterInputObject {
    pub edges: Vec<(juniper::Spanning<String>, juniper::Spanning<InputValue>)>,
}

pub struct FilterInputObjectTypeInfo {
    pub filter_type_name: GraphQLName<'static>,
    pub type_name: GraphQLName<'static>,
    pub allframes: Arc<AllFrames>,
}

impl FilterInputObjectTypeInfo {
    pub fn new(type_name: &GraphQLName, all_frames: &Arc<AllFrames>) -> Self {
        Self {
            filter_type_name: filter_name(type_name),
            type_name: type_name.as_static(),
            allframes: all_frames.clone(),
        }
    }
}

impl GraphQLType for FilterInputObject {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(info.filter_type_name.as_str())
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        if let Some(TypeDefinition::Class(d)) = &info.allframes.frames.get(&info.type_name) {
            let mut args: Vec<_> = d
                .fields()
                .iter()
                .map(
                    |(name, field_definition)| -> juniper::meta::Argument<DefaultScalarValue> {
                        let kind = field_definition.kind();
                        if kind.is_collection() {
                            match field_definition.range() {
                                BaseOrDerived::Base(base_type) => {
                                    let kind = base_type_kind(base_type);
                                    match kind {
                                        BaseTypeKind::String => registry.arg::<Option<
                                            CollectionStringFilterInputObject,
                                        >>(
                                            name.as_str(), &()
                                        ),
                                        BaseTypeKind::SmallInteger => registry.arg::<Option<
                                            CollectionIntFilterInputObject,
                                        >>(
                                            name.as_str(), &()
                                        ),
                                        BaseTypeKind::BigIntger => registry.arg::<Option<
                                            CollectionBigIntFilterInputObject,
                                        >>(
                                            name.as_str(), &()
                                        ),
                                        BaseTypeKind::Boolean => registry.arg::<Option<
                                            CollectionBooleanFilterInputObject,
                                        >>(
                                            name.as_str(), &()
                                        ),
                                        BaseTypeKind::Float => registry.arg::<Option<
                                            CollectionFloatFilterInputObject,
                                        >>(
                                            name.as_str(), &()
                                        ),
                                        BaseTypeKind::Decimal => registry.arg::<Option<
                                            CollectionBigFloatFilterInputObject,
                                        >>(
                                            name.as_str(), &()
                                        ),
                                        BaseTypeKind::DateTime => registry.arg::<Option<
                                            CollectionDateTimeFilterInputObject,
                                        >>(
                                            name.as_str(), &()
                                        ),
                                    }
                                }
                                BaseOrDerived::Derived(c) => {
                                    // is this foreign?
                                    if info.allframes.is_foreign(c) {
                                        registry.arg::<Option<CollectionIdFilterInputObject>>(
                                            name.as_str(),
                                            &(),
                                        )
                                    } else {
                                        registry.arg::<Option<CollectionFilterInputObject>>(
                                            name.as_str(),
                                            &CollectionFilterInputObjectTypeInfo::new(
                                                c,
                                                &info.allframes,
                                            ),
                                        )
                                    }
                                }
                            }
                        } else if let Some(base_type) = field_definition.base_type() {
                            let kind = base_type_kind(base_type);
                            match kind {
                                BaseTypeKind::String => registry
                                    .arg::<Option<StringFilterInputObject>>(name.as_str(), &()),
                                BaseTypeKind::SmallInteger => {
                                    registry.arg::<Option<IntFilterInputObject>>(name.as_str(), &())
                                }
                                BaseTypeKind::BigIntger => registry
                                    .arg::<Option<BigIntFilterInputObject>>(name.as_str(), &()),
                                BaseTypeKind::Boolean => registry
                                    .arg::<Option<BooleanFilterInputObject>>(name.as_str(), &()),
                                BaseTypeKind::Float => registry
                                    .arg::<Option<FloatFilterInputObject>>(name.as_str(), &()),
                                BaseTypeKind::Decimal => registry
                                    .arg::<Option<BigFloatFilterInputObject>>(name.as_str(), &()),
                                BaseTypeKind::DateTime => registry
                                    .arg::<Option<DateTimeFilterInputObject>>(name.as_str(), &()),
                            }
                        } else if let Some(enum_type) = field_definition.enum_type(&info.allframes)
                        {
                            registry.arg::<Option<EnumFilterInputObject>>(
                                name.as_str(),
                                &EnumFilterInputObjectTypeInfo::new(enum_type, &info.allframes),
                            )
                        } else {
                            match field_definition.range() {
                                BaseOrDerived::Base(_) => {
                                    panic!("This branch should be unreachable - not a base type")
                                }
                                BaseOrDerived::Derived(c) => {
                                    if info.allframes.is_foreign(c) {
                                        registry
                                            .arg::<Option<IdFilterInputObject>>(name.as_str(), &())
                                    } else {
                                        registry.arg::<Option<FilterInputObject>>(
                                            name.as_str(),
                                            &FilterInputObjectTypeInfo::new(c, &info.allframes),
                                        )
                                    }
                                }
                            }
                        }
                    },
                )
                .collect();

            let mut applicable_restrictions = Vec::new();
            for restriction in info.allframes.restrictions.values() {
                if restriction.on == info.type_name {
                    applicable_restrictions.push(restriction.id.to_owned());
                }
            }

            if !applicable_restrictions.is_empty() {
                let enum_name = restriction_name(&info.type_name);
                let type_info = GeneratedEnumTypeInfo {
                    name: enum_name,
                    values: applicable_restrictions,
                };

                args.push(registry.arg::<Option<GeneratedEnum>>("_restriction", &type_info));
            }

            args.push(registry.arg::<Option<ID>>("_id", &()));
            args.push(registry.arg::<Option<Vec<ID>>>("_ids", &()));

            args.push(registry.arg::<Option<Vec<FilterInputObject>>>(
                "_and",
                &FilterInputObjectTypeInfo::new(&info.type_name, &info.allframes),
            ));

            args.push(registry.arg::<Option<Vec<FilterInputObject>>>(
                "_or",
                &FilterInputObjectTypeInfo::new(&info.type_name, &info.allframes),
            ));

            args.push(registry.arg::<Option<FilterInputObject>>(
                "_not",
                &FilterInputObjectTypeInfo::new(&info.type_name, &info.allframes),
            ));

            registry
                .build_input_object_type::<FilterInputObject>(&info, &args)
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
        Some(&info.filter_type_name.as_str())
    }
}

pub struct CollectionFilterInputObject {
    pub edges: Vec<(juniper::Spanning<String>, juniper::Spanning<InputValue>)>,
}

pub struct CollectionFilterInputObjectTypeInfo {
    filter_type_name: GraphQLName<'static>,
    type_name: GraphQLName<'static>,
    frames: Arc<AllFrames>,
}

impl CollectionFilterInputObjectTypeInfo {
    pub fn new<'a>(type_name: &GraphQLName<'a>, all_frames: &Arc<AllFrames>) -> Self {
        Self {
            filter_type_name: collection_filter_name(&type_name),
            type_name: type_name.as_static(),
            frames: all_frames.clone(),
        }
    }
}

impl GraphQLType for CollectionFilterInputObject {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(info.filter_type_name.as_str())
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let mut args: Vec<_> = Vec::with_capacity(2);
        let type_definition = &info.frames.frames[&info.type_name];
        match type_definition {
            TypeDefinition::Class(_) => {
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
            TypeDefinition::Enum(_) => {
                args.push(registry.arg::<Option<EnumFilterInputObject>>(
                    "someHave",
                    &EnumFilterInputObjectTypeInfo::new(&info.type_name, &info.frames),
                ));
                args.push(registry.arg::<Option<EnumFilterInputObject>>(
                    "allHave",
                    &EnumFilterInputObjectTypeInfo::new(&info.type_name, &info.frames),
                ));
                registry
                    .build_input_object_type::<CollectionFilterInputObject>(info, &args)
                    .into_meta()
            }
        }
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
        Some(info.filter_type_name.as_str())
    }
}

pub struct EnumFilterInputObject {
    pub op: EnumOperation,
    pub enum_value: TerminusEnum,
}

pub struct EnumFilterInputObjectTypeInfo {
    filter_type_name: GraphQLName<'static>,
    type_name: GraphQLName<'static>,
    frames: Arc<AllFrames>,
}

impl EnumFilterInputObjectTypeInfo {
    pub fn new(type_name: &GraphQLName, all_frames: &Arc<AllFrames>) -> Self {
        Self {
            filter_type_name: enum_filter_name(type_name),
            type_name: type_name.as_static(),
            frames: all_frames.clone(),
        }
    }
}

impl GraphQLType for EnumFilterInputObject {
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(&info.filter_type_name.as_str())
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let mut args: Vec<_> = Vec::with_capacity(2);
        let type_info = (info.type_name.clone(), info.frames.clone());
        args.push(registry.arg::<Option<TerminusEnum>>("eq", &type_info));
        args.push(registry.arg::<Option<TerminusEnum>>("ne", &type_info));
        registry
            .build_input_object_type::<EnumFilterInputObject>(info, &args)
            .into_meta()
    }
}

impl FromInputValue for EnumFilterInputObject {
    fn from_input_value(v: &InputValue<DefaultScalarValue>) -> Option<Self> {
        match v {
            InputValue::Object(o) => {
                if let Some((key, val)) = o.first() {
                    let op = match key.item.as_ref() {
                        "eq" => EnumOperation::Eq,
                        "ne" => EnumOperation::Ne,
                        unknown => panic!("unknown enum operation {unknown}"),
                    };

                    let enum_value = TerminusEnum::from_input_value(&val.item);
                    enum_value.map(|e| Self { op, enum_value: e })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl GraphQLValue for EnumFilterInputObject {
    type Context = ();

    type TypeInfo = EnumFilterInputObjectTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some(&info.filter_type_name.as_str())
    }
}

#[derive(GraphQLInputObject)]
#[graphql(name = "CollectionStringFilter")]
#[allow(non_snake_case)]
pub struct CollectionStringFilterInputObject {
    pub someHave: Option<StringFilterInputObject>,
    pub allHave: Option<StringFilterInputObject>,
}

#[derive(GraphQLInputObject)]
#[allow(non_snake_case)]
#[graphql(name = "StringFilter")]
pub struct StringFilterInputObject {
    pub eq: Option<String>,
    pub ne: Option<String>,
    pub lt: Option<String>,
    pub le: Option<String>,
    pub gt: Option<String>,
    pub ge: Option<String>,
    pub regex: Option<String>,
    pub startsWith: Option<String>,
    pub allOfTerms: Option<Vec<String>>,
    pub anyOfTerms: Option<Vec<String>>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "CollectionBigIntFilter")]
#[allow(non_snake_case)]
pub struct CollectionBigIntFilterInputObject {
    pub someHave: Option<BigIntFilterInputObject>,
    pub allHave: Option<BigIntFilterInputObject>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "BigIntFilter")]
pub struct BigIntFilterInputObject {
    pub eq: Option<BigInt>,
    pub ne: Option<BigInt>,
    pub lt: Option<BigInt>,
    pub le: Option<BigInt>,
    pub gt: Option<BigInt>,
    pub ge: Option<BigInt>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "CollectionIntFilter")]
#[allow(non_snake_case)]
pub struct CollectionIntFilterInputObject {
    pub someHave: Option<IntFilterInputObject>,
    pub allHave: Option<IntFilterInputObject>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "IntFilter")]
pub struct IntFilterInputObject {
    pub eq: Option<i32>,
    pub ne: Option<i32>,
    pub lt: Option<i32>,
    pub le: Option<i32>,
    pub gt: Option<i32>,
    pub ge: Option<i32>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "CollectionBigFloatFilter")]
#[allow(non_snake_case)]
pub struct CollectionBigFloatFilterInputObject {
    pub someHave: Option<BigFloatFilterInputObject>,
    pub allHave: Option<BigFloatFilterInputObject>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "BigFloatFilter")]
pub struct BigFloatFilterInputObject {
    pub eq: Option<BigFloat>,
    pub ne: Option<BigFloat>,
    pub lt: Option<BigFloat>,
    pub le: Option<BigFloat>,
    pub gt: Option<BigFloat>,
    pub ge: Option<BigFloat>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "CollectionFloatFilter")]
#[allow(non_snake_case)]
pub struct CollectionFloatFilterInputObject {
    pub someHave: Option<FloatFilterInputObject>,
    pub allHave: Option<FloatFilterInputObject>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "FloatFilter")]
pub struct FloatFilterInputObject {
    pub eq: Option<f64>,
    pub ne: Option<f64>,
    pub lt: Option<f64>,
    pub le: Option<f64>,
    pub gt: Option<f64>,
    pub ge: Option<f64>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "CollectionBooleanFilter")]
#[allow(non_snake_case)]
pub struct CollectionBooleanFilterInputObject {
    pub someHave: Option<BooleanFilterInputObject>,
    pub allHave: Option<BooleanFilterInputObject>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "BooleanFilter")]
pub struct BooleanFilterInputObject {
    pub eq: Option<bool>,
    pub ne: Option<bool>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "CollectionDateTimeFilter")]
#[allow(non_snake_case)]
pub struct CollectionDateTimeFilterInputObject {
    pub someHave: Option<DateTimeFilterInputObject>,
    pub allHave: Option<DateTimeFilterInputObject>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "DateTimeFilter")]
pub struct DateTimeFilterInputObject {
    pub eq: Option<DateTime>,
    pub ne: Option<DateTime>,
    pub lt: Option<DateTime>,
    pub le: Option<DateTime>,
    pub gt: Option<DateTime>,
    pub ge: Option<DateTime>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "IdFilter")]
pub struct IdFilterInputObject {
    #[graphql(name = "_id")]
    pub id: Option<ID>,
    #[graphql(name = "_ids")]
    pub ids: Option<Vec<ID>>,
}

#[derive(GraphQLInputObject)]
#[graphql(name = "CollectionIdFilterInputObject")]
#[allow(non_snake_case)]
pub struct CollectionIdFilterInputObject {
    pub someHave: Option<IdFilterInputObject>,
    pub allHave: Option<IdFilterInputObject>,
}
