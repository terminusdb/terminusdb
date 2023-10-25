pub const TDB_CONTEXT: &str = "terminusdb://context";

pub const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
pub const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
pub const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
pub const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
pub const RDF_LIST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#List";
generate_lookup_type! {
    RdfIds {
        type_: pred RDF_TYPE,
        first: pred RDF_FIRST,
        rest: pred RDF_REST,
        nil: node RDF_NIL,
        list: node RDF_LIST,
    }
}
pub const SYS_CLASS: &str = "http://terminusdb.com/schema/sys#Class";
pub const SYS_TAGGED_UNION: &str = "http://terminusdb.com/schema/sys#TaggedUnion";
pub const SYS_FOREIGN: &str = "http://terminusdb.com/schema/sys#Foreign";
pub const SYS_ENUM: &str = "http://terminusdb.com/schema/sys#Enum";
pub const SYS_SET: &str = "http://terminusdb.com/schema/sys#Set";
pub const SYS_SUBDOCUMENT: &str = "http://terminusdb.com/schema/sys#subdocument";
pub const SYS_UNFOLDABLE: &str = "http://terminusdb.com/schema/sys#unfoldable";
pub const SYS_INHERITS: &str = "http://terminusdb.com/schema/sys#inherits";
pub const SYS_BASE: &str = "http://terminusdb.com/schema/sys#base";
pub const SYS_SCHEMA: &str = "http://terminusdb.com/schema/sys#schema";
pub const SYS_PREFIX_PAIR: &str = "http://terminusdb.com/schema/sys#prefix_pair";
pub const SYS_PREFIX: &str = "http://terminusdb.com/schema/sys#prefix";
pub const SYS_URL: &str = "http://terminusdb.com/schema/sys#url";
pub const SYS_INDEX: &str = "http://terminusdb.com/schema/sys#index";
pub const SYS_VALUE: &str = "http://terminusdb.com/schema/sys#value";
pub const SYS_ARRAY: &str = "http://terminusdb.com/schema/sys#Array";
pub const SYS_JSON: &str = "http://terminusdb.com/schema/sys#JSON";
pub const SYS_JSON_DOCUMENT: &str = "http://terminusdb.com/schema/sys#JSONDocument";
pub const SYS_FOREIGN_TYPE_PREDICATE_ID: &str = "http://terminusdb.com/schema/sys#foreign_type";
generate_lookup_type! {
    SysIds {
        class: node SYS_CLASS,
        tagged_union: node SYS_TAGGED_UNION,
        foreign: node SYS_FOREIGN,
        enum_: node SYS_ENUM,
        set: node SYS_SET,
        subdocument: pred SYS_SUBDOCUMENT,
        unfoldable: pred SYS_UNFOLDABLE,
        inherits: pred SYS_INHERITS,
        base: pred SYS_BASE,
        schema: pred SYS_SCHEMA,
        prefix_pair: pred SYS_PREFIX_PAIR,
        prefix: pred SYS_PREFIX,
        url: pred SYS_URL,
        index: pred SYS_INDEX,
        value: pred SYS_VALUE,
        array: node SYS_ARRAY,
        json: node SYS_JSON,
        json_document: node SYS_JSON_DOCUMENT,
        foreign_type_predicate: pred SYS_FOREIGN_TYPE_PREDICATE_ID,
    }
}

pub const SYS_JSON_PREFIX: &str = "http://terminusdb.com/schema/json#";
