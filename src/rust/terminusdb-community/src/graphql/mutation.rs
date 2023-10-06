use juniper::{DefaultScalarValue, GraphQLType, GraphQLValue, ID};
use swipl::prelude::{Context, QueryableContextType};

use crate::graphql::schema::GraphQLJSON;

use super::schema::TerminusContext;

pub struct TerminusMutationRoot<'a, C: QueryableContextType> {
    _c: std::marker::PhantomData<&'a Context<'a, C>>,
}

impl<'a, C: QueryableContextType> TerminusMutationRoot<'a, C> {
    pub fn new() -> Self {
        Self {
            _c: Default::default(),
        }
    }
}

impl<'a, C: QueryableContextType> GraphQLType for TerminusMutationRoot<'a, C> {
    fn name(_info: &Self::TypeInfo) -> Option<&str> {
        Some("TerminusMutation")
    }

    fn meta<'r>(
        _info: &Self::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let field = registry
            .field::<Vec<ID>>("insertDocuments", &())
            .argument(registry.arg::<GraphQLJSON>("json", &()));

        registry
            .build_object_type::<TerminusMutationRoot<'a, C>>(&(), &[field])
            .into_meta()
    }
}

impl<'a, C: QueryableContextType> GraphQLValue for TerminusMutationRoot<'a, C> {
    type Context = TerminusContext<'a, C>;
    type TypeInfo = ();

    fn type_name<'i>(&self, _info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some("TerminusMutation")
    }
}
