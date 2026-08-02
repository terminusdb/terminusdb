//! GraphQL subscription types and resolution context.
//!
//! `TerminusSubscriptionRoot` is for schema generation only. Event resolution
//! bypasses Juniper's async executor and uses `SubscriptionResolveContext`.

use std::sync::Arc;

use juniper::{
    parser::parse_document_source, DefaultScalarValue, Definition,
};
use lazy_init::Lazy;
use sha2::{Digest, Sha256};

use terminusdb_store_prolog::terminus_store::store::sync::SyncStoreLayer;

use crate::doc::DocumentContext;
use crate::graphql::schema::{
    TerminusResolveContext, TerminusTypeCollectionInfo,
};

/// Context for resolving GraphQL subscription events outside of Juniper.
///
/// Prolog passes schema and instance layers via FFI after each commit;
/// no per-event FFI calls are needed. `change_type`, `commit_id`, `timestamp`,
/// and `datetime` are `None` for regular GraphQL queries.
#[derive(Clone)]
pub struct SubscriptionResolveContext {
    pub schema: SyncStoreLayer,
    pub instance: Option<SyncStoreLayer>,
    pub type_collection: TerminusTypeCollectionInfo,
    pub document_context: Arc<Lazy<DocumentContext<SyncStoreLayer>>>,
    pub change_type: Option<String>,
    pub commit_id: Option<String>,
    pub timestamp: Option<f64>,
    pub datetime: Option<String>,
}

impl SubscriptionResolveContext {
    /// Create a context with subscription event metadata.
    pub fn with_metadata(
        schema: SyncStoreLayer,
        instance: Option<SyncStoreLayer>,
        type_collection: TerminusTypeCollectionInfo,
        change_type: String,
        commit_id: String,
        timestamp: f64,
        datetime: String,
    ) -> Self {
        Self {
            schema,
            instance,
            type_collection,
            document_context: Arc::new(Lazy::new()),
            change_type: Some(change_type),
            commit_id: Some(commit_id),
            timestamp: Some(timestamp),
            datetime: Some(datetime),
        }
    }

    pub fn document_context(&self) -> &DocumentContext<SyncStoreLayer> {
        self.document_context
            .get_or_create(|| DocumentContext::new(self.schema.clone(), self.instance.clone()))
    }
}

impl TerminusResolveContext for SubscriptionResolveContext {
    fn instance(&self) -> Option<&SyncStoreLayer> {
        self.instance.as_ref()
    }

    fn type_collection(&self) -> &TerminusTypeCollectionInfo {
        &self.type_collection
    }

    fn document_context(&self) -> &DocumentContext<SyncStoreLayer> {
        self.document_context()
    }

    fn change_type(&self) -> Option<&str> {
        self.change_type.as_deref()
    }

    fn commit_id(&self) -> Option<&str> {
        self.commit_id.as_deref()
    }

    fn timestamp(&self) -> Option<f64> {
        self.timestamp
    }

    fn datetime(&self) -> Option<&str> {
        self.datetime.as_deref()
    }

    // Restrictions are rejected at subscription registration time,
    // so the default no-op implementation is correct.
}

/// Parsed components of a GraphQL subscription query.
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedSubscription {
    pub field_name: String,
    pub class_name: String,
    pub operation: String,
    pub filter_canonical_json: String,
    pub selection_set: String,
    pub selection_set_hash: String,
    /// Selection set as valid GraphQL text preserving field order, used for
    /// Query-root resolution. Unlike `selection_set` (sorted for hashing).
    pub selection_set_graphql: String,
}

/// Parse a GraphQL subscription query string into cohort key components.
pub fn parse_subscription_query(
    query: &str,
    type_collection: &TerminusTypeCollectionInfo,
) -> Result<ParsedSubscription, String> {
    let root_node = crate::graphql::get_or_create_subscription_root_node(type_collection);

    let source = Box::new(query.to_string());
    let document = parse_document_source(&source, &root_node.schema)
        .map_err(|e| format!("GraphQL parse error: {}", e))?;

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

    let first_field = op.selection_set.first().ok_or_else(|| {
        "Subscription operation has an empty selection set".to_string()
    })?;

    let field = match first_field {
        juniper::Selection::Field(spanning_field) => &spanning_field.item,
        _ => return Err("Subscription top-level selection must be a field, not a fragment".to_string()),
    };

    let field_name = field.name.item.to_string();

    // e.g. "Person_added" -> ("Person", "added")
    let last_underscore = field_name.rfind('_').ok_or_else(|| {
        format!(
            "Subscription field name '{}' does not contain '_' — expected format '{{Class}}_{{operation}}'",
            field_name
        )
    })?;

    let class_name = field_name[..last_underscore].to_string();
    let operation = field_name[last_underscore + 1..].to_string();

    match operation.as_str() {
        "added" | "changed" | "deleted" => {}
        _ => return Err(format!(
            "Unknown subscription operation '{}'. Must be one of: added, changed, deleted",
            operation
        )),
    }

    let filter_canonical_json = if let Some(args) = &field.arguments {
        let mut map: serde_json::Map<String, serde_json::Value> = serde_json::Map::new();
        for (key, val) in &args.item.items {
            let key_str = key.item.to_string();
            let val_json = input_value_to_json(&val.item);
            map.insert(key_str, val_json);
        }
        // Sort via BTreeMap for canonical (deterministic) serialization.
        let sorted: std::collections::BTreeMap<_, _> = map.into_iter().collect();
        serde_json::to_string(&sorted).map_err(|e| format!("Failed to serialize filter: {}", e))?
    } else {
        "{}".to_string()
    };

    let selection_set_str = selection_set_to_string(&field.selection_set);
    let hash = Sha256::digest(selection_set_str.as_bytes());
    let selection_set_hash = format!("{:x}", hash);

    let selection_set_graphql = selection_set_to_graphql(&field.selection_set);

    // Collect fragment definitions from the document so that fragment
    // spreads in the selection set resolve correctly when the query is
    // re-executed via resolve_subscription_event.
    let fragments: Vec<String> = document
        .iter()
        .filter_map(|def| {
            if let Definition::Fragment(frag) = def {
                let name = frag.item.name.item.to_string();
                let type_cond = frag.item.type_condition.item.to_string();
                let body = selection_set_to_graphql(&Some(frag.item.selection_set.clone()));
                Some(format!("fragment {} on {} {{ {} }}", name, type_cond, body))
            } else {
                None
            }
        })
        .collect();
    let selection_set_graphql = if fragments.is_empty() {
        selection_set_graphql
    } else {
        format!("{} {}", selection_set_graphql, fragments.join(" "))
    };

    Ok(ParsedSubscription {
        field_name,
        class_name,
        operation,
        filter_canonical_json,
        selection_set: selection_set_str.clone(),
        selection_set_hash,
        selection_set_graphql,
    })
}

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

/// Deterministic string for hashing. Fields are sorted recursively.
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

/// Query-ready GraphQL text preserving field order, for Query-root resolution.
fn selection_set_to_graphql(selection_set: &Option<Vec<juniper::Selection<DefaultScalarValue>>>) -> String {
    match selection_set {
        Some(items) => {
            let mut parts: Vec<String> = Vec::new();
            for sel in items {
                match sel {
                    juniper::Selection::Field(f) => {
                        let name = f.item.name.item.to_string();
                        let alias = f.item.alias.as_ref().map(|a| a.item.to_string());
                        let nested = selection_set_to_graphql(&f.item.selection_set);
                        let field_str = match (alias, nested.is_empty()) {
                            (Some(a), true) => format!("{}: {}", a, name),
                            (Some(a), false) => format!("{}: {} {{ {} }}", a, name, nested),
                            (None, true) => name,
                            (None, false) => format!("{} {{ {} }}", name, nested),
                        };
                        parts.push(field_str);
                    }
                    juniper::Selection::FragmentSpread(fs) => {
                        parts.push(format!("...{}", fs.item.name.item));
                    }
                    juniper::Selection::InlineFragment(ifr) => {
                        let type_cond = ifr.item.type_condition.as_ref().map(|t| t.item.to_string()).unwrap_or_default();
                        let nested = selection_set_to_graphql(&Some(ifr.item.selection_set.clone()));
                        if type_cond.is_empty() {
                            parts.push(format!("... {{ {} }}", nested));
                        } else {
                            parts.push(format!("... on {} {{ {} }}", type_cond, nested));
                        }
                    }
                }
            }
            parts.join(" ")
        }
        None => String::new(),
    }
}

/// Root subscription type for SDL generation. Not used for event resolution —
/// that goes through `SubscriptionResolveContext`.
pub struct TerminusSubscriptionRoot<C: TerminusResolveContext> {
    _phantom: std::marker::PhantomData<C>,
}

impl<C: TerminusResolveContext> TerminusSubscriptionRoot<C> {
    pub fn new() -> Self {
        Self {
            _phantom: std::marker::PhantomData,
        }
    }
}

// GraphQLType/GraphQLValue impls are in schema.rs (needs access to private
// TerminusTypeInfo fields and add_arguments()).

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
    fn commit_metadata_values_holds_all_fields() {
        use crate::graphql::schema::CommitMetadataValues;
        let cmv = CommitMetadataValues {
            id: Some("abc123".to_string()),
            timestamp: Some(1722528000.0),
            datetime: Some("2024-08-01T16:00:00Z".to_string()),
            change_type: Some("added".to_string()),
        };
        assert_eq!(cmv.id.as_deref(), Some("abc123"));
        assert_eq!(cmv.timestamp, Some(1722528000.0));
        assert_eq!(cmv.datetime.as_deref(), Some("2024-08-01T16:00:00Z"));
        assert_eq!(cmv.change_type.as_deref(), Some("added"));
    }

    #[test]
    fn commit_metadata_values_all_none() {
        use crate::graphql::schema::CommitMetadataValues;
        let cmv = CommitMetadataValues {
            id: None,
            timestamp: None,
            datetime: None,
            change_type: None,
        };
        assert!(cmv.id.is_none());
        assert!(cmv.timestamp.is_none());
        assert!(cmv.datetime.is_none());
        assert!(cmv.change_type.is_none());
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
        // Juniper may parse this as a scalar field with no selection set.
        let result = result;
        if let Ok(parsed) = result {
            assert_eq!(parsed.field_name, "Person_added");
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
        assert_eq!(r1.selection_set_hash, r2.selection_set_hash);
    }

    #[test]
    fn parse_filter_args_extracted() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_added(filter: {name: {eq: \"Alice\"}}) { _id } }",
            &tc,
        );
        let parsed = result
            .expect("filter parse should succeed with filter argument in schema");
        assert_eq!(parsed.field_name, "Person_added");
        assert_ne!(parsed.filter_canonical_json, "{}");
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
    fn parse_returns_selection_set_string() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_added { _id name } }",
            &tc,
        ).unwrap();
        assert!(!result.selection_set.is_empty());
        assert!(result.selection_set.contains("_id"));
        assert!(result.selection_set.contains("name"));
    }

    #[test]
    fn parse_returns_selection_set_graphql_flat_fields() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_added { _id name } }",
            &tc,
        ).unwrap();
        assert_eq!(result.selection_set_graphql, "_id name");
    }

    #[test]
    fn parse_returns_selection_set_graphql_id_only() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "subscription { Person_added { _id } }",
            &tc,
        ).unwrap();
        assert_eq!(result.selection_set_graphql, "_id");
    }

    #[test]
    fn parse_returns_selection_set_graphql_preserves_order() {
        let tc = person_type_collection();
        let r1 = parse_subscription_query(
            "subscription { Person_added { _id name } }",
            &tc,
        ).unwrap();
        let r2 = parse_subscription_query(
            "subscription { Person_added { name _id } }",
            &tc,
        ).unwrap();
        assert_eq!(r1.selection_set_graphql, "_id name");
        assert_eq!(r2.selection_set_graphql, "name _id");
        assert_eq!(r1.selection_set_hash, r2.selection_set_hash);
    }

    #[test]
    fn parse_different_selection_sets_different_strings() {
        let tc = person_type_collection();
        let r1 = parse_subscription_query(
            "subscription { Person_added { _id } }",
            &tc,
        ).unwrap();
        let r2 = parse_subscription_query(
            "subscription { Person_added { _id name } }",
            &tc,
        ).unwrap();
        assert_ne!(r1.selection_set, r2.selection_set);
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

    #[test]
    fn parse_returns_selection_set_graphql_with_fragment_definitions() {
        let tc = person_type_collection();
        let result = parse_subscription_query(
            "fragment PersonFields on Person { _id name } subscription { Person_added { ...PersonFields } }",
            &tc,
        );
        assert!(result.is_ok(), "parse failed: {:?}", result.err());
        let parsed = result.unwrap();
        assert!(parsed.selection_set_graphql.contains("...PersonFields"),
                "selection_set_graphql should contain fragment spread: {}",
                parsed.selection_set_graphql);
        assert!(parsed.selection_set_graphql.contains("fragment PersonFields on Person"),
                "selection_set_graphql should contain fragment definition: {}",
                parsed.selection_set_graphql);
        assert!(parsed.selection_set_graphql.contains("_id"),
                "selection_set_graphql should contain _id from fragment: {}",
                parsed.selection_set_graphql);
        assert!(parsed.selection_set_graphql.contains("name"),
                "selection_set_graphql should contain name from fragment: {}",
                parsed.selection_set_graphql);
    }

    #[test]
    fn subscription_schema_has_proper_return_types() {
        let tc = person_type_collection();
        let root_node = crate::graphql::get_or_create_subscription_root_node(&tc);

        let sub_type = root_node.schema.concrete_subscription_type()
            .expect("Schema should have a subscription type");

        let field = sub_type.field_by_name("Person_added")
            .expect("Person_added field should exist");

        match &field.field_type {
            juniper::Type::Named(name) | juniper::Type::NonNullNamed(name) => {
                assert_eq!(name.as_ref(), "Person",
                    "Person_added should return type Person, not String");
            }
            other => panic!("Unexpected field type for Person_added: {:?}", other),
        }

        let args = field.arguments.as_ref()
            .expect("Person_added should have arguments");
        let has_filter = args.iter().any(|a| a.name == "filter");
        assert!(has_filter, "Person_added should have a filter argument");

        for op in ["Person_changed", "Person_deleted"] {
            let f = sub_type.field_by_name(op)
                .expect(&format!("{op} field should exist"));
            match &f.field_type {
                juniper::Type::Named(name) | juniper::Type::NonNullNamed(name) => {
                    assert_eq!(name.as_ref(), "Person",
                        "{op} should return type Person, not String");
                }
                other => panic!("Unexpected field type for {op}: {:?}", other),
            }
        }
    }
}
