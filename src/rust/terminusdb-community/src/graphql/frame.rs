use serde::{self, Deserialize};
use std::collections::BTreeMap;

#[derive(Deserialize, PartialEq, Debug)]
pub struct SchemaDocumentation {
    #[serde(rename = "@title")]
    pub title: Option<String>,
    #[serde(rename = "@description")]
    pub description: Option<String>,
    #[serde(rename = "@authors")]
    pub authors: Option<Vec<String>>,
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct Prefixes {
    #[serde(rename = "@type")]
    pub kind: String,
    #[serde(rename = "@base")]
    pub base: String,
    #[serde(rename = "@schema")]
    pub schema: String,
    #[serde(rename = "@documentation")]
    pub documentation: Option<SchemaDocumentation>,
    #[serde(flatten)]
    pub extra_prefixes: BTreeMap<String, String>,
}

impl Prefixes {
    pub fn expand(&self, s: &str) -> String {
        // this is dumb but will work for now
        format!("{}{}", self.base, s)
    }
    pub fn expand_schema(&self, s: &str) -> String {
        // this is dumb but will work for now
        format!("{}{}", self.schema, s)
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum StructuralPropertyDocumentationRecord {
    OnlyPropertyLabel(String),
    PropertyCommentLabel {
        #[serde(rename = "@label")]
        label: Option<String>,
        #[serde(rename = "@comment")]
        comment: Option<String>,
    },
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(from = "StructuralPropertyDocumentationRecord")]
pub struct PropertyDocumentationRecord {
    pub label: Option<String>,
    pub comment: Option<String>,
}

impl From<StructuralPropertyDocumentationRecord> for PropertyDocumentationRecord {
    fn from(f: StructuralPropertyDocumentationRecord) -> Self {
        match f {
            StructuralPropertyDocumentationRecord::OnlyPropertyLabel(s) => {
                PropertyDocumentationRecord {
                    label: Some(s),
                    comment: None,
                }
            }
            StructuralPropertyDocumentationRecord::PropertyCommentLabel { label, comment } => {
                PropertyDocumentationRecord { label, comment }
            }
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct PropertyDocumentation {
    #[serde(flatten)]
    pub records: BTreeMap<String, PropertyDocumentationRecord>,
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct ClassDocumentationDefinition {
    #[serde(rename = "@label")]
    pub label: Option<String>,
    #[serde(rename = "@comment")]
    pub comment: Option<String>,
    #[serde(rename = "@properties")]
    pub properties: Option<PropertyDocumentation>,
}

#[inline]
fn default_dimensionality() -> usize {
    1
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
enum ComplexFieldDefinition {
    Optional {
        #[serde(rename = "@class")]
        class: String,
    },
    Set {
        #[serde(rename = "@class")]
        class: String,
    },
    Array {
        #[serde(rename = "@class")]
        class: String,
        #[serde(default = "default_dimensionality")]
        dimensions: usize,
    },
    List {
        #[serde(rename = "@class")]
        class: String,
    },
    Cardinality {
        #[serde(rename = "@class")]
        class: String,
        min: Option<usize>,
        max: Option<usize>,
    },
}

impl From<StructuralFieldDefinition> for FieldDefinition {
    fn from(f: StructuralFieldDefinition) -> Self {
        match f {
            StructuralFieldDefinition::SimpleField(s) => FieldDefinition::Required(s),
            StructuralFieldDefinition::ContainerField(c) => match c {
                ComplexFieldDefinition::Optional { class } => FieldDefinition::Optional(class),
                ComplexFieldDefinition::Set { class } => FieldDefinition::Set(class),
                ComplexFieldDefinition::List { class } => FieldDefinition::List(class),
                ComplexFieldDefinition::Array { class, dimensions } => {
                    FieldDefinition::Array { class, dimensions }
                }
                ComplexFieldDefinition::Cardinality { class, min, max } => {
                    FieldDefinition::Cardinality { class, min, max }
                }
            },
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(untagged)]
enum StructuralFieldDefinition {
    SimpleField(String),
    ContainerField(ComplexFieldDefinition),
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(from = "StructuralFieldDefinition")]
pub enum FieldDefinition {
    Required(String),
    Optional(String),
    Set(String),
    List(String),
    Array {
        class: String,
        dimensions: usize,
    },
    Cardinality {
        class: String,
        min: Option<usize>,
        max: Option<usize>,
    },
}

#[derive(PartialEq)]
pub enum FieldKind {
    Required,
    Optional,
    Set,
    List,
    Array,
    Cardinality,
}

pub fn is_base_type(s: &str) -> bool {
    // TODO this is not good enough
    s.starts_with("xsd:")
}

impl FieldDefinition {
    fn range(&self) -> &String {
        match self {
            Self::Required(c) => c,
            Self::Optional(c) => c,
            Self::Set(c) => c,
            Self::List(c) => c,
            Self::Array { class, .. } => class,
            Self::Cardinality { class, .. } => class,
        }
    }

    pub fn base_type(&self) -> Option<&String> {
        let range = self.range();
        if is_base_type(range) {
            Some(range)
        } else {
            None
        }
    }

    pub fn document_type<'a>(&'a self, allframes: &'a AllFrames) -> Option<&'a String> {
        if self.base_type().is_some() {
            None
        } else {
            let range = self.range();
            allframes.document_type(range)
        }
    }

    pub fn enum_type<'a>(&'a self, allframes: &'a AllFrames) -> Option<&'a String> {
        if self.base_type().is_some() {
            None
        } else {
            let range = self.range();
            allframes.enum_type(range)
        }
    }

    pub fn kind(&self) -> FieldKind {
        match self {
            Self::Required(_) => FieldKind::Required,
            Self::Optional(_) => FieldKind::Optional,
            Self::Set(_) => FieldKind::Set,
            Self::List(_) => FieldKind::List,
            Self::Array { .. } => FieldKind::Array,
            Self::Cardinality { .. } => FieldKind::Cardinality,
        }
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
pub enum KeyDefinition {
    Random,
    Lexical {
        #[serde(rename = "@fields")]
        fields: Vec<String>,
    },
    Hash {
        #[serde(rename = "@fields")]
        fields: Vec<String>,
    },
    ValueHash,
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct OneOf {
    #[serde(flatten)]
    choices: BTreeMap<String, FieldDefinition>,
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct ClassDefinition {
    #[serde(rename = "@documentation")]
    pub documentation: Option<ClassDocumentationDefinition>,
    #[serde(rename = "@key")]
    pub key: Option<KeyDefinition>,
    #[serde(rename = "@subdocument")]
    pub is_subdocument: Option<Vec<()>>,
    #[serde(rename = "@abstract")]
    pub is_abstract: Option<Vec<()>>,
    #[serde(rename = "@oneOf")]
    pub one_of: Option<Vec<OneOf>>,
    #[serde(flatten)]
    pub fields: BTreeMap<String, FieldDefinition>,
}

#[derive(Deserialize, PartialEq, Debug)]
pub struct EnumDocumentationDefinition {}

#[derive(Deserialize, PartialEq, Debug)]
pub struct EnumDefinition {
    //#[serde(rename = "@type")]
    //pub kind: String,
    #[serde(rename = "@documentation")]
    pub documentation: Option<EnumDocumentationDefinition>,
    #[serde(rename = "@values")]
    pub values: Vec<String>,
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(tag = "@type")]
pub enum TypeDefinition {
    Class(ClassDefinition),
    Enum(EnumDefinition),
}

#[derive(PartialEq)]
pub enum TypeKind {
    Class,
    Enum,
}

impl TypeDefinition {
    pub fn is_document_type(&self) -> bool {
        match self {
            Self::Class(_) => true,
            _ => false,
        }
    }
    pub fn is_enum_type(&self) -> bool {
        match self {
            Self::Enum(_) => true,
            _ => false,
        }
    }

    pub fn kind(&self) -> TypeKind {
        match self {
            Self::Class(_) => TypeKind::Class,
            Self::Enum(_) => TypeKind::Enum,
        }
    }

    pub(crate) fn as_class_definition(&self) -> &ClassDefinition {
        match self {
            Self::Class(c) => &c,
            _ => panic!("tried to unwrap non-class definition as class definition"),
        }
    }
}

impl FieldKind {
    pub fn is_collection(&self) -> bool {
        match self {
            Self::Set => true,
            Self::Array => true,
            Self::List => true,
            Self::Cardinality => true,
            _ => false,
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct AllFrames {
    #[serde(rename = "@context")]
    pub context: Prefixes,
    #[serde(flatten)]
    pub frames: BTreeMap<String, TypeDefinition>,
}

impl AllFrames {
    fn document_type<'a>(&self, s: &'a String) -> Option<&'a String> {
        if self.frames[s].is_document_type() {
            Some(s)
        } else {
            None
        }
    }

    fn enum_type<'a>(&self, s: &'a String) -> Option<&'a String> {
        if self.frames[s].is_enum_type() {
            Some(s)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use swipl::prelude::*;
    #[test]
    fn deserialize_context() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
_{'@base': "http://some_base/",
  '@schema': "http://some_schema#",
  '@type' : 'Context',
  a: "http://extra_prefix/a",
  b: "http://extra_prefix/b"
}
            "#;
        let term = context.term_from_string(term).unwrap();

        let prefixes: Prefixes = context.deserialize_from_term(&term).unwrap();

        assert_eq!(
            Prefixes {
                kind: "Context".to_string(),
                documentation: None,
                base: "http://some_base/".to_string(),
                schema: "http://some_schema#".to_string(),
                extra_prefixes: BTreeMap::from([
                    ("a".to_string(), "http://extra_prefix/a".to_string()),
                    ("b".to_string(), "http://extra_prefix/b".to_string())
                ])
            },
            prefixes
        );
    }

    #[test]
    fn deserialize_field_definition() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
_{'@type': "Array", '@class': 'Something'}
"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let typedef: FieldDefinition = context.deserialize_from_term(&term).unwrap();

        assert_eq!(
            FieldDefinition::Array {
                class: "Something".to_string(),
                dimensions: 1
            },
            typedef
        );
    }

    #[test]
    fn deserialize_key_definition() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
_{'@type': "Lexical", '@fields': ["foo", "bar"]}
"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let typedef: KeyDefinition = context.deserialize_from_term(&term).unwrap();

        assert_eq!(
            KeyDefinition::Lexical {
                fields: vec!["foo".to_string(), "bar".to_string()]
            },
            typedef
        );
    }

    #[test]
    fn deserialize_oneof() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"json{'@oneOf':[json{a:'xsd:string',b:'xsd:integer'},
                           json{c:'xsd:string',d:'xsd:integer'}],
                           '@type':'Class'}"#;

        let term = unwrap_result(&context, context.term_from_string(term));
        let typedef: TypeDefinition = context.deserialize_from_term(&term).unwrap();
        assert_eq!(
            TypeDefinition::Class(ClassDefinition {
                documentation: None,
                key: None,
                is_subdocument: None,
                is_abstract: None,
                one_of: Some(vec![
                    OneOf {
                        choices: BTreeMap::from([
                            (
                                "a".to_string(),
                                FieldDefinition::Required("xsd:string".to_string())
                            ),
                            (
                                "b".to_string(),
                                FieldDefinition::Required("xsd:integer".to_string())
                            )
                        ])
                    },
                    OneOf {
                        choices: BTreeMap::from([
                            (
                                "c".to_string(),
                                FieldDefinition::Required("xsd:string".to_string())
                            ),
                            (
                                "d".to_string(),
                                FieldDefinition::Required("xsd:integer".to_string())
                            )
                        ])
                    }
                ]),
                fields: BTreeMap::from([]),
            }),
            typedef
        );
    }

    #[test]
    fn deserialize_all_frames() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
json{'@context':_27018{'@base':"terminusdb:///data/",
                       '@schema':"terminusdb:///schema#",
                       '@type':'Context'},
      'Test':json{'@type':'Class',bar:'xsd:string',foo:'xsd:integer'}}
"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let frames: AllFrames = context.deserialize_from_term(&term).unwrap();

        panic!("{:?}", frames);
    }

    #[test]
    fn deserialize_action_frame() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
json{ '@documentation':json{ '@comment':"The exhaustive list of actions which are available to roles."
					  },
		     '@type':'Enum',
		     '@values':[ create_database,
				 delete_database,
				 class_frame,
				 clone,
				 fetch,
				 push,
				 branch,
				 rebase,
				 instance_read_access,
				 instance_write_access,
				 schema_read_access,
				 schema_write_access,
				 meta_read_access,
				 meta_write_access,
				 commit_read_access,
				 commit_write_access,
				 manage_capabilities
			       ]
		   }

"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let typedef: TypeDefinition = dbg!(context.deserialize_from_term(&term)).unwrap();

        assert_eq!(
            TypeDefinition::Enum(EnumDefinition {
                documentation: Some(EnumDocumentationDefinition {}),
                values: vec![
                    "create_database".to_string(),
                    "delete_database".to_string(),
                    "class_frame".to_string(),
                    "clone".to_string(),
                    "fetch".to_string(),
                    "push".to_string(),
                    "branch".to_string(),
                    "rebase".to_string(),
                    "instance_read_access".to_string(),
                    "instance_write_access".to_string(),
                    "schema_read_access".to_string(),
                    "schema_write_access".to_string(),
                    "meta_read_access".to_string(),
                    "meta_write_access".to_string(),
                    "commit_read_access".to_string(),
                    "commit_write_access".to_string(),
                    "manage_capabilities".to_string()
                ]
            }),
            typedef
        );
    }

    #[test]
    fn deserialize_system_frame() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"
json{ '@context':_{ '@base':"terminusdb://system/data/",
		    '@documentation':json{ '@authors':[ "Gavin Mendel-Gleason",
							"Matthijs van Otterdijk"
						      ],
					   '@description':"This is the System schema in which resides all information regarding capabilities, users, organizations, databases and available actions.",
					   '@title':"The System schema"
					 },
		    '@schema':"http://terminusdb.com/schema/system#",
		    '@type':'Context',
		    xsd:"http://www.w3.org/2001/XMLSchema#"
		  },
      'Action':json{ '@documentation':json{ '@comment':"The exhaustive list of actions which are available to roles."
					  },
		     '@type':'Enum',
		     '@values':[ create_database,
				 delete_database,
				 class_frame,
				 clone,
				 fetch,
				 push,
				 branch,
				 rebase,
				 instance_read_access,
				 instance_write_access,
				 schema_read_access,
				 schema_write_access,
				 meta_read_access,
				 meta_write_access,
				 commit_read_access,
				 commit_write_access,
				 manage_capabilities
			       ]
		   },
      'Capability':json{ '@documentation':json{ '@comment':"A capability is a set of roles combined with a rescource over which those roles hold.",
						'@properties':json{ role:"The set of roles the capability has access to.",
								    scope:"The resource over which the role holds."
								  }
					      },
			 '@key':json{'@type':"Random"},
			 '@type':'Class',
			 role:json{'@class':'Role','@type':'Set'},
			 scope:'Resource'
		       },
      'Database':json{ '@documentation':json{ '@comment':"A database.",
					      '@properties':json{ name:"The name of the resource."
								}
					    },
		       '@type':'Class',
		       name:'xsd:string'
		     },
      'DatabaseState':json{ '@documentation':json{ '@comment':"The current system transaction state of a database. Only the 'finalized' state is a consistent state, all others indicate that database construction failed."
						 },
			    '@type':'Enum',
			    '@values':[creating,deleting,finalized]
			  },
      'Organization':json{ '@documentation':json{ '@comment':"An organisation.",
						  '@properties':json{ child:"The set of organizations which are children of the current organization.",
								      database:"The set of databases controlled by the organization.",
								      name:"The name of the organization."
								    }
						},
			   '@key':json{'@fields':[name],'@type':"Lexical"},
			   '@type':'Class',
			   child:json{'@class':'Organization','@type':'Set'},
			   database:json{'@class':'Database','@type':'Set'},
			   name:'xsd:string'
			 },
      'Resource':json{ '@documentation':json{ '@comment':"A named resource.",
					      '@properties':json{ name:"The name of the resource."
								}
					    },
		       '@type':'Class',
		       name:'xsd:string'
		     },
      'Role':json{ '@documentation':json{ '@comment':"Roles are named collections of actions which can be provided to a capability.",
					  '@properties':json{ action:"The set of actions associated with the role.",
							      name:"The name of the role."
							    }
					},
		   '@type':'Class',
		   action:json{'@class':'Action','@type':'Set'},
		   name:'xsd:string'
		 },
      'SystemDatabase':json{ '@documentation':json{ '@comment':"The special system database.",
						    '@properties':json{ name:"The name of the resource."
								      }
						  },
			     '@type':'Class',
			     name:'xsd:string'
			   },
      'User':json{ '@documentation':json{ '@comment':"A database user.",
					  '@properties':json{ capability:"A set of capabilities which the user has access to.",
							      key_hash:"An optional key hash for authentication.",
							      name:"The users name."
							    }
					},
		   '@key':json{'@fields':[name],'@type':"Lexical"},
		   '@type':'Class',
		   capability:json{'@class':'Capability','@type':'Set'},
		   key_hash:json{'@class':'xsd:string','@type':'Optional'},
		   name:'xsd:string'
		 },
      'UserDatabase':json{ '@documentation':json{ '@comment':"A normal user database.",
						  '@properties':json{ comment:"A comment associated with the database.",
								      creation_date:"The time of creation of the database.",
								      label:"The label name of the database.",
								      name:"The name of the resource.",
								      state:"The system transaction state of the database."
								    }
						},
			   '@key':json{'@type':"Random"},
			   '@type':'Class',
			   comment:'xsd:string',
			   creation_date:'xsd:dateTime',
			   label:'xsd:string',
			   name:'xsd:string',
			   state:'DatabaseState'
			 }
    }
"#;

        let term = unwrap_result(&context, context.term_from_string(term));
        let frames: AllFrames = context.deserialize_from_term(&term).unwrap();
        // at least it parses!
    }

    #[test]
    fn deserialize_woql_frames() {
        let engine = Engine::new();
        let activation = engine.activate();
        let context: Context<_> = activation.into();

        let term = r#"json{'@context':_48112{'@base':"terminusdb://woql/data/",'@documentation':json{'@authors':["Gavin Mendel-Gleason"],'@description':"This is the WOQL schema. It gives a complete specification of the syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic correctness, helps to prevent errors and detect conflicts in merge of queries, and allows the storage and retrieval of queries so that queries can be associated with data products.",'@title':"WOQL schema"},'@schema':"http://terminusdb.com/schema/woql#",'@type':'Context',xsd:"http://www.w3.org/2001/XMLSchema#"},
'AddData':json{'@documentation':json{'@comment':"Add an edge with a data value.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A data value or variable which is the target or object of the graph edge. The variable must be bound.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge. The variable must be bound.",subject:"A URI or variable which is the source or subject of the graph edge. The variable must be bound."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'DataValue',predicate:'NodeValue',subject:'NodeValue'},
'AddLink':json{'@documentation':json{'@comment':"Add an edge which links between nodes in the graph.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A URI or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'NodeValue',predicate:'NodeValue',subject:'NodeValue'},
'AddTriple':json{'@documentation':json{'@comment':"Specify an edge to add to the graph.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A URI, datatype or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'Value',predicate:'NodeValue',subject:'NodeValue'},
'AddedData':json{'@documentation':json{'@comment':"Specify an edge pattern with data value which was added in *this* commit*.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A datatype or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'DataValue',predicate:'NodeValue',subject:'NodeValue'},
'AddedLink':json{'@documentation':json{'@comment':"Specify an edge pattern which links between nodes at *this* commit.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A URI or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'NodeValue',predicate:'NodeValue',subject:'NodeValue'},
'AddedTriple':json{'@documentation':json{'@comment':"Specify an edge pattern which was *added* at *this commit*.",'@properties':json{graph:"An optional graph (either 'instance' or 'schema')",object:"A URI, datatype or variable which is the target or object of the graph edge.",predicate:"A URI or variable which is the edge-label or predicate of the graph edge.",subject:"A URI or variable which is the source or subject of the graph edge."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',graph:json{'@class':'xsd:string','@type':'Optional'},object:'Value',predicate:'NodeValue',subject:'NodeValue'},
'And':json{'@documentation':json{'@comment':"A conjunction of queries which must all have a solution.",'@properties':json{and:"List of queries which must hold."}},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class',and:json{'@class':'Query','@type':'List'}},
'ArithmeticExpression':json{'@abstract':[],'@documentation':json{'@comment':"An abstract class specifying the AST super-class of all arithemtic expressions."},'@key':json{'@type':"ValueHash"},'@subdocument':[],'@type':'Class'}}"#;
        let term = unwrap_result(&context, context.term_from_string(term));
        let frames: AllFrames = context.deserialize_from_term(&term).unwrap();
        panic!("{frames:?}")
        // at least it parses!
    }
}
