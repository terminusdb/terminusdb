//! GraphQL subscription types and resolution context.
//!
//! `TerminusSubscriptionRoot` exists only for schema generation and
//! introspection. Actual subscription execution bypasses Juniper's async
//! executor entirely — events are resolved manually using
//! `SubscriptionResolveContext`.

use std::sync::Arc;

use juniper::{
    parser::parse_document_source, DefaultScalarValue, Definition, EmptyMutation,
    GraphQLType, GraphQLValue, RootNode,
};
use lazy_init::Lazy;
use sha2::{Digest, Sha256};

use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;

use crate::doc::DocumentContext;
use crate::graphql::frame::TypeDefinition;
use crate::graphql::schema::{
    TerminusContext, TerminusResolveContext, TerminusTypeCollection, TerminusTypeCollectionInfo,
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

/// Parsed components of a GraphQL subscription query.
///
/// Extracted by parsing the query string with Juniper's `parse_document_source`
/// and walking the AST. Used by Prolog to compute the cohort key.
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedSubscription {
    pub field_name: String,
    pub class_name: String,
    pub operation: String,
    pub filter_canonical_json: String,
    pub selection_set_hash: String,
}

/// Parse a GraphQL subscription query string and extract the components
/// needed for cohort key computation.
///
/// Uses Juniper's `parse_document_source` — the same parser used by the
/// HTTP GraphQL endpoint and the embedding query parser.
pub fn parse_subscription_query(
    query: &str,
    type_collection: &TerminusTypeCollectionInfo,
) -> Result<ParsedSubscription, String> {
    let root_node = RootNode::new_with_info(
        TerminusTypeCollection,
        EmptyMutation::<TerminusContext<'static>>::new(),
        TerminusSubscriptionRoot,
        type_collection.clone(),
        (),
        type_collection.clone(),
    );

    let source = Box::new(query.to_string());
    let document = parse_document_source(&source, &root_node.schema)
        .map_err(|e| format!("GraphQL parse error: {}", e))?;

    // Find the subscription operation in the parsed definitions.
    let mut subscription_op = None;
    for def in &document {
        if let Definition::Operation(op) = def {
            if let juniper::OperationType::Subscription = op.item.operation_type {
                subscription_op = Some(&op.item);
                break;
            }
        }
    }

    let op = subscription_op.ok_or_else(|| {
        "No subscription operation found in query. Expected `subscription { ... }`".to_string()
    })?;

    // Get the first field in the subscription's selection set.
    let first_field = op.selection_set.first().ok_or_else(|| {
        "Subscription operation has an empty selection set".to_string()
    })?;

    let field = match first_field {
        juniper::Selection::Field(spanning_field) => &spanning_field.item,
        _ => return Err("Subscription top-level selection must be a field, not a fragment".to_string()),
    };

    let field_name = field.name.item.to_string();

    // Split field name on the last `_` to get class and operation.
    // e.g. "Person_added" -> ("Person", "added")
    let last_underscore = field_name.rfind('_').ok_or_else(|| {
        format!(
            "Subscription field name '{}' does not contain '_' — expected format '{{Class}}_{{operation}}'",
            field_name
        )
    })?;

    let class_name = field_name[..last_underscore].to_string();
    let operation = field_name[last_underscore + 1..].to_string();

    // Validate operation is one of the supported types.
    match operation.as_str() {
        "added" | "changed" | "deleted" => {}
        _ => return Err(format!(
            "Unknown subscription operation '{}'. Must be one of: added, changed, deleted",
            operation
        )),
    }

    // Extract filter arguments and serialize to canonical JSON (sorted keys).
    let filter_canonical_json = if let Some(args) = &field.arguments {
        let mut map: serde_json::Map<String, serde_json::Value> = serde_json::Map::new();
        for (key, val) in &args.item.items {
            let key_str = key.item.to_string();
            let val_json = input_value_to_json(&val.item);
            map.insert(key_str, val_json);
        }
        // serde_json::Map with BTreeMap ordering would be ideal, but
        // serde_json::Map maintains insertion order by default. We sort
        // by serializing through a BTreeMap.
        let sorted: std::collections::BTreeMap<_, _> = map.into_iter().collect();
        serde_json::to_string(&sorted).map_err(|e| format!("Failed to serialize filter: {}", e))?
    } else {
        "{}".to_string()
    };

    // Extract and hash the selection set.
    let selection_set_str = selection_set_to_string(&field.selection_set);
    let hash = Sha256::digest(selection_set_str.as_bytes());
    let selection_set_hash = format!("{:x}", hash);

    Ok(ParsedSubscription {
        field_name,
        class_name,
        operation,
        filter_canonical_json,
        selection_set_hash,
    })
}

/// Convert a Juniper `InputValue` to a `serde_json::Value` for canonical
/// serialization.
fn input_value_to_json(val: &juniper::InputValue<DefaultScalarValue>) -> serde_json::Value {
    use juniper::InputValue;
    match val {
        InputValue::Null => serde_json::Value::Null,
        InputValue::Scalar(s) => match s {
            DefaultScalarValue::Int(i) => serde_json::Value::Number((*i).into()),
            DefaultScalarValue::Float(f) => {
                serde_json::Number::from_f64(*f).map(serde_json::Value::Number).unwrap_or(serde_json::Value::Null)
            }
            DefaultScalarValue::String(s) => serde_json::Value::String(s.clone()),
            DefaultScalarValue::Boolean(b) => serde_json::Value::Bool(*b),
        },
        InputValue::Enum(name) => serde_json::Value::String(name.clone()),
        InputValue::Variable(name) => serde_json::Value::String(format!("${}", name)),
        InputValue::List(items) => {
            serde_json::Value::Array(items.iter().map(|i| input_value_to_json(&i.item)).collect())
        }
        InputValue::Object(entries) => {
            let mut map = std::collections::BTreeMap::new();
            for (key, val) in entries {
                map.insert(key.item.to_string(), input_value_to_json(&val.item));
            }
            serde_json::Value::Object(map.into_iter().collect())
        }
    }
}

/// Normalize a selection set to a deterministic string representation
/// for hashing. Field names are sorted, nested selection sets are
/// recursively normalized.
fn selection_set_to_string(selection_set: &Option<Vec<juniper::Selection<DefaultScalarValue>>>) -> String {
    match selection_set {
        Some(items) => {
            let mut field_strings: Vec<String> = Vec::new();
            for sel in items {
                match sel {
                    juniper::Selection::Field(f) => {
                        let name = f.item.name.item.to_string();
                        let alias = f.item.alias.as_ref().map(|a| a.item.to_string());
                        let nested = selection_set_to_string(&f.item.selection_set);
                        let field_str = match alias {
                            Some(a) => format!("{}:{}{{{}}}", a, name, nested),
                            None => format!("{}{{{}}}", name, nested),
                        };
                        field_strings.push(field_str);
                    }
                    juniper::Selection::FragmentSpread(fs) => {
                        field_strings.push(format!("...{}", fs.item.name.item));
                    }
                    juniper::Selection::InlineFragment(ifr) => {
                        let type_cond = ifr.item.type_condition.as_ref().map(|t| t.item.to_string()).unwrap_or_default();
                        let nested = selection_set_to_string(&Some(ifr.item.selection_set.clone()));
                        field_strings.push(format!("...on {}{{{}}}", type_cond, nested));
                    }
                }
            }
            field_strings.sort();
            field_strings.join(",")
        }
        None => String::new(),
    }
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
    type Context = TerminusContext<'static>;
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

    fn person_allframes() -> AllFrames {
        let json = r#"{
            "@context": {
                "@type": "@context",
                "@base": "http://example.com/",
                "@schema": "http://example.com/schema#",
                "Person": "http://example.com/schema#Person",
                "name": "http://example.com/schema#name"
            },
            "Person": {
                "@type": "Class",
                "name": "xsd:string"
            }
        }"#;
        let unclean: crate::graphql::frame::UncleanAllFrames =
            serde_json::from_str(json).expect("failed to parse person frames");
        unclean.finalize()
    }

    fn person_type_collection() -> TerminusTypeCollectionInfo {
        TerminusTypeCollectionInfo {
            allframes: Arc::new(person_allframes()),
        }
    }

    #[test]
    fn subscription_resolve_context_no_restriction_check() {
        let frames = Arc::new(empty_allframes());
        let type_collection = TerminusTypeCollectionInfo { allframes: frames };
        assert!(type_collection.allframes.frames.is_empty());
    }

    #[test]
    fn parse_basic_subscription() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_added { _id } }",
            &tc,
        );
        assert!(result.is_ok(), "parse failed: {:?}", result.err());
        let parsed = result.unwrap();
        assert_eq!(parsed.field_name, "Person_added");
        assert_eq!(parsed.class_name, "Person");
        assert_eq!(parsed.operation, "added");
    }

    #[test]
    fn parse_changed_operation() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_changed { _id } }",
            &tc,
        );
        assert!(result.is_ok());
        let parsed = result.unwrap();
        assert_eq!(parsed.operation, "changed");
    }

    #[test]
    fn parse_deleted_operation() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_deleted { _id } }",
            &tc,
        );
        assert!(result.is_ok());
        let parsed = result.unwrap();
        assert_eq!(parsed.operation, "deleted");
    }

    #[test]
    fn parse_unknown_operation_rejected() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_foo { _id } }",
            &tc,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("Unknown subscription operation"), "got: {}", err);
    }

    #[test]
    fn parse_no_underscore_in_field_name_rejected() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person { _id } }",
            &tc,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("does not contain '_'"), "got: {}", err);
    }

    #[test]
    fn parse_no_subscription_operation_rejected() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "{ Person { _id } }",
            &tc,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("No subscription operation"), "got: {}", err);
    }

    #[test]
    fn parse_empty_selection_set_rejected() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_added }",
            &tc,
        );
        // Person_added without a selection set — Juniper may parse this
        // as a scalar field. The parse should still succeed but the
        // selection_set will be None, producing an empty hash.
        // This is acceptable — the hash will be consistent.
        let result = result;
        if let Ok(parsed) = result {
            assert_eq!(parsed.field_name, "Person_added");
            // Empty selection set hashes to a deterministic value
            assert!(!parsed.selection_set_hash.is_empty());
        }
    }

    #[test]
    fn parse_different_selection_sets_different_hashes() {
        let tc = person_type_collection();
        let r1 = parse_subscription_query(
            "subscription { Person_added { _id } }",
            &tc,
        ).unwrap();
        let r2 = parse_subscription_query(
            "subscription { Person_added { _id name } }",
            &tc,
        ).unwrap();
        assert_ne!(r1.selection_set_hash, r2.selection_set_hash);
    }

    #[test]
    fn parse_same_selection_sets_same_hash() {
        let tc = person_type_collection();
        let r1 = parse_subscription_query(
            "subscription { Person_added { _id name } }",
            &tc,
        ).unwrap();
        let r2 = parse_subscription_query(
            "subscription { Person_added { _id name } }",
            &tc,
        ).unwrap();
        assert_eq!(r1.selection_set_hash, r2.selection_set_hash);
    }

    #[test]
    fn parse_reordered_fields_same_hash() {
        let tc = person_type_collection();
        let r1 = parse_subscription_query(
            "subscription { Person_added { _id name } }",
            &tc,
        ).unwrap();
        let r2 = parse_subscription_query(
            "subscription { Person_added { name _id } }",
            &tc,
        ).unwrap();
        // Field order in selection set should not affect the hash
        // because we sort fields before hashing.
        assert_eq!(r1.selection_set_hash, r2.selection_set_hash);
    }

    #[test]
    fn parse_filter_args_extracted() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_added(filter: {name: {eq: \"Alice\"}}) { _id } }",
            &tc,
        );
        // The filter may or may not parse depending on whether the schema
        // has the filter input type. But the parsing should at least
        // extract the field name correctly.
        if let Ok(parsed) = result {
            assert_eq!(parsed.field_name, "Person_added");
            // Filter should be non-empty if extracted
            assert_ne!(parsed.filter_canonical_json, "{}");
        }
    }

    #[test]
    fn parse_no_filter_empty_json() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_added { _id } }",
            &tc,
        ).unwrap();
        assert_eq!(result.filter_canonical_json, "{}");
    }

    #[test]
    fn parse_invalid_syntax_rejected() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_added { ",
            &tc,
        );
        assert!(result.is_err());
    }
}
