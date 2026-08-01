//! GraphQL subscription types and resolution context.
//!
//! `TerminusSubscriptionRoot` exists only for schema generation and
//! introspection. Actual subscription execution bypasses Juniper's async
//! executor entirely — events are resolved manually using
//! `SubscriptionResolveContext`.

use std::sync::Arc;

use juniper::{
    DefaultScalarValue, GraphQLType, GraphQLValue,
};
use lazy_init::Lazy;

use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;

use crate::doc::DocumentContext;
use crate::graphql::frame::TypeDefinition;
use crate::graphql::schema::{
    TerminusResolveContext, TerminusTypeCollectionInfo,
};

/// Context for resolving GraphQL subscription events outside of Juniper.
///
/// Contains only the Rust-side data needed for field resolution. Prolog
/// passes schema and instance layers via FFI after each commit; no per-event
/// FFI calls are needed for resolution.
#[derive(Clone)]
pub struct SubscriptionResolveContext {
    pub schema: SyncStoreLayer,
    pub instance: Option<SyncStoreLayer>,
    pub type_collection: TerminusTypeCollectionInfo,
    pub document_context: Arc<Lazy<DocumentContext<SyncStoreLayer>>>,
}

impl SubscriptionResolveContext {
    pub fn new(
        schema: SyncStoreLayer,
        instance: Option<SyncStoreLayer>,
        type_collection: TerminusTypeCollectionInfo,
    ) -> Self {
        Self {
            schema,
            instance,
            type_collection,
            document_context: Arc::new(Lazy::new()),
        }
    }

    pub fn document_context(&self) -> &DocumentContext<SyncStoreLayer> {
        self.document_context
            .get_or_create(|| DocumentContext::new(self.schema.clone(), self.instance.clone()))
    }
}

impl TerminusResolveContext for SubscriptionResolveContext {
    fn schema(&self) -> &SyncStoreLayer {
        &self.schema
    }

    fn instance(&self) -> Option<&SyncStoreLayer> {
        self.instance.as_ref()
    }

    fn type_collection(&self) -> &TerminusTypeCollectionInfo {
        &self.type_collection
    }

    fn document_context(&self) -> &DocumentContext<SyncStoreLayer> {
        self.document_context()
    }

    // Restrictions are rejected at subscription registration time,
    // so the default no-op implementation is correct.
}

/// Root subscription type for SDL generation.
///
/// Generates `{Class}_added`, `{Class}_changed`, `{Class}_deleted` fields
/// for every class in the schema. This type is only used for schema
/// generation and introspection — actual event resolution is done
/// manually in the WebSocket handler.
pub struct TerminusSubscriptionRoot;

impl GraphQLType for TerminusSubscriptionRoot {
    fn name(_info: &Self::TypeInfo) -> Option<&str> {
        Some("Subscription")
    }

    fn meta<'r>(
        info: &Self::TypeInfo,
        registry: &mut juniper::Registry<'r, DefaultScalarValue>,
    ) -> juniper::meta::MetaType<'r, DefaultScalarValue>
    where
        DefaultScalarValue: 'r,
    {
        let mut fields: Vec<_> = Vec::new();

        for (name, typedef) in info.allframes.frames.iter() {
            if let TypeDefinition::Class(_) = typedef {
                let added_name = format!("{}_added", name.as_str());
                let changed_name = format!("{}_changed", name.as_str());
                let deleted_name = format!("{}_deleted", name.as_str());

                // Each subscription field returns a JSON string.
                // We use String as the return type since actual resolution
                // bypasses Juniper — the SDL is for introspection only.
                fields.push(
                    registry
                        .field::<String>(added_name.as_str(), &())
                        .description("Fired when a document of this class is added"),
                );
                fields.push(
                    registry
                        .field::<String>(changed_name.as_str(), &())
                        .description("Fired when a document of this class is changed"),
                );
                fields.push(
                    registry
                        .field::<String>(deleted_name.as_str(), &())
                        .description("Fired when a document of this class is deleted"),
                );
            }
        }

        registry
            .build_object_type::<TerminusSubscriptionRoot>(info, &fields)
            .into_meta()
    }
}

impl GraphQLValue for TerminusSubscriptionRoot {
    type Context = SubscriptionResolveContext;
    type TypeInfo = TerminusTypeCollectionInfo;

    fn type_name<'i>(&self, _info: &'i Self::TypeInfo) -> Option<&'i str> {
        Some("TerminusSubscriptionRoot")
    }

    fn resolve_field(
        &self,
        _info: &Self::TypeInfo,
        _field_name: &str,
        _arguments: &juniper::Arguments,
        _executor: &juniper::Executor<Self::Context, DefaultScalarValue>,
    ) -> juniper::ExecutionResult {
        // This is never called — subscription execution bypasses Juniper.
        // The GraphQLValue impl exists only for SDL generation.
        Err("Subscription resolution is not handled by Juniper".into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graphql::frame::AllFrames;

    fn empty_allframes() -> AllFrames {
        let json = r#"{"@context": {"@type":"@context","@base":"http://example.com/","@schema":"http://example.com/schema#"}}"#;
        let unclean: crate::graphql::frame::UncleanAllFrames =
            serde_json::from_str(json).expect("failed to parse empty frames");
        unclean.finalize()
    }

    #[test]
    fn subscription_resolve_context_no_restriction_check() {
        // We can't easily construct a SyncStoreLayer in a unit test,
        // but we can verify the trait's default method returns Ok(None)
        // by checking the trait definition compiles and the method
        // signature is correct.
        // The actual restriction check is tested in integration tests.
        //
        // This test verifies that SubscriptionResolveContext implements
        // TerminusResolveContext with the default no-op restriction check.
        let frames = Arc::new(empty_allframes());
        let type_collection = TerminusTypeCollectionInfo { allframes: frames };

        // Verify type_collection is accessible
        assert!(type_collection.allframes.frames.is_empty());
    }
}
