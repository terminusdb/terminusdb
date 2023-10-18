use juniper::{graphql_interface, graphql_object, GraphQLEnum};
use swipl::prelude::Atom;
use terminusdb_store_prolog::terminus_store::{store::sync::SyncStoreLayer, Layer};

use crate::value::value_to_json;

pub struct SystemData {
    pub user: Atom,
    pub system: SyncStoreLayer,
}
impl juniper::Context for SystemData {}

#[derive(Default)]
pub struct SystemRoot;

#[graphql_object(context = SystemData)]
#[no_async]
impl SystemRoot {
    /// Get the user that is currently logged in.
    fn user(#[graphql(context)] _info: &SystemData) -> User {
        User
    }
}

#[derive(Default)]
pub struct User;

#[graphql_object(context = SystemData)]
/// The user that is currently logged in.
impl User {
    fn id(#[graphql(context)] info: &SystemData) -> String {
        info.user.to_string()
    }

    /// The name of the user that is currently logged in.
    fn name(#[graphql(context)] info: &SystemData) -> String {
        let user_id = info
            .system
            .subject_id(&info.user.to_string())
            .unwrap_or_else(|| panic!("can't make user id from {}", info.user.to_string()));
        required_object_string(
            &info.system,
            user_id,
            "http://terminusdb.com/schema/system#name",
        )
    }

    fn capability(#[graphql(context)] info: &SystemData) -> Vec<Capability> {
        let user_id = info
            .system
            .subject_id(&info.user.to_string())
            .unwrap_or_else(|| panic!("can't make user id from {}", info.user.to_string()));
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#capability")
            .expect("can't find capability predicate");

        info.system
            .triples_sp(user_id, predicate_id)
            .map(|triple| Capability { id: triple.object })
            .collect()
    }
}

pub struct Capability {
    id: u64,
}

#[graphql_object(context = SystemData)]
impl Capability {
    fn id(&self, #[graphql(context)] info: &SystemData) -> String {
        info.system
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    fn role(&self, #[graphql(context)] info: &SystemData) -> Vec<Role> {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#role")
            .expect("can't find name predicate");

        info.system
            .triples_sp(self.id, predicate_id)
            .map(|triple| Role { id: triple.object })
            .collect()
    }

    fn scope(&self, #[graphql(context)] info: &SystemData) -> ResourceValue {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#scope")
            .expect("can't find name predicate");
        let scope_id = info
            .system
            .single_triple_sp(self.id, predicate_id)
            .expect("can't find capability triple")
            .object;

        // And get the type
        let predicate_id = info
            .system
            .predicate_id("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
            .expect("can't find rdf:type predicate");
        let type_id = info
            .system
            .single_triple_sp(self.id, predicate_id)
            .expect("can't find type triple")
            .object;
        let db_type_id = info
            .system
            .subject_id("http://terminusdb.com/schema/system#UserDatabase");

        if db_type_id == Some(type_id) {
            Database { id: scope_id }.into()
        } else {
            Organization { id: scope_id }.into()
        }
    }
}

pub struct Role {
    id: u64,
}

#[graphql_interface(for = [Database, Organization])]
pub trait Resource {
    #[graphql(ignore)]
    fn get_id(&self) -> u64;
    fn id(&self, #[graphql(context)] info: &SystemData) -> String {
        info.system
            .id_subject(self.get_id())
            .expect("can't make u64 into id")
    }
    fn name(&self, #[graphql(context)] info: &SystemData) -> String {
        required_object_string(
            &info.system,
            self.get_id(),
            "http://terminusdb.com/schema/system#name",
        )
    }
}
#[derive(Clone)]
pub struct Database {
    id: u64,
}

#[graphql_object(context = SystemData, impl = ResourceValue)]
impl Database {
    fn id(&self, #[graphql(context)] info: &SystemData) -> String {
        <Self as Resource>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &SystemData) -> String {
        <Self as Resource>::name(self, info)
    }

    fn label(&self, #[graphql(context)] info: &SystemData) -> Option<String> {
        maybe_object_string(
            &info.system,
            self.get_id(),
            "http://terminusdb.com/schema/system#label",
        )
    }

    fn comment(&self, #[graphql(context)] info: &SystemData) -> Option<String> {
        maybe_object_string(
            &info.system,
            self.get_id(),
            "http://terminusdb.com/schema/system#comment",
        )
    }
}

#[graphql_interface]
impl Resource for Database {
    fn get_id(&self) -> u64 {
        self.id
    }
}

#[derive(Clone)]
pub struct Organization {
    id: u64,
}

#[graphql_object(context = SystemData, impl = ResourceValue)]
impl Organization {
    fn id(&self, #[graphql(context)] info: &SystemData) -> String {
        <Self as Resource>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &SystemData) -> String {
        <Self as Resource>::name(self, info)
    }

    fn database(&self, #[graphql(context)] info: &SystemData) -> Vec<Database> {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#database")
            .expect("can't find 'database' predicate");
        info.system
            .triples_sp(self.id, predicate_id)
            .map(|triple| Database { id: triple.object })
            .collect()
    }

    fn child(&self, #[graphql(context)] info: &SystemData) -> Vec<Organization> {
        // if no children, empty.
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#child")
            .expect("can't find child' predicate");
        info.system
            .triples_sp(self.id, predicate_id)
            .map(|triple| Organization { id: triple.object })
            .collect()
    }
}

#[graphql_interface]
impl Resource for Organization {
    fn get_id(&self) -> u64 {
        self.id
    }
}

#[graphql_object(context = SystemData)]
/// The user that is currently logged in.
impl Role {
    fn id(&self, #[graphql(context)] info: &SystemData) -> String {
        info.system
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    fn name(&self, #[graphql(context)] info: &SystemData) -> String {
        required_object_string(
            &info.system,
            self.id,
            "http://terminusdb.com/schema/system#name",
        )
    }

    fn action(&self, #[graphql(context)] info: &SystemData) -> Vec<Action> {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#action")
            .expect("can't find 'database' predicate");
        info.system
            .triples_sp(self.id, predicate_id)
            .map(|triple| triple.object)
            .map(|o| {
                info.system
                    .id_subject(o)
                    .expect("Invalid Action in database (corrupted)")
            })
            .map(|action| action_enum(&action))
            .collect()
    }
}

fn required_object_string(db: &SyncStoreLayer, id: u64, prop: &str) -> String {
    let predicate_id = db
        .predicate_id(prop)
        .unwrap_or_else(|| panic!("can't find {} predicate", prop));
    let name_id = db
        .single_triple_sp(id, predicate_id)
        .unwrap_or_else(|| panic!("can't find triple for {}", prop))
        .object;
    let name_unprocessed = db
        .id_object(name_id)
        .unwrap_or_else(|| panic!("no object for id {}", id))
        .value()
        .expect("returned object was not a value");

    name_unprocessed.as_val::<String, String>()
}

#[derive(GraphQLEnum, Clone, Copy, Debug, Eq, PartialEq)]
pub enum Action {
    #[graphql(name = "create_database")]
    CreateDatabase,
    #[graphql(name = "delete_database")]
    DeleteDatabase,
    #[graphql(name = "class_frame")]
    ClassFrame,
    #[graphql(name = "clone")]
    Clone,
    #[graphql(name = "fetch")]
    Fetch,
    #[graphql(name = "push")]
    Push,
    #[graphql(name = "branch")]
    Branch,
    #[graphql(name = "rebase")]
    Rebase,
    #[graphql(name = "instance_read_access")]
    InstanceReadAccess,
    #[graphql(name = "instance_write_access")]
    InstanceWriteAccess,
    #[graphql(name = "schema_read_access")]
    SchemaReadAccess,
    #[graphql(name = "schema_write_access")]
    SchemaWriteAccess,
    #[graphql(name = "meta_read_access")]
    MetaReadAccess,
    #[graphql(name = "meta_write_access")]
    MetaWriteAccess,
    #[graphql(name = "commit_read_access")]
    CommitReadAccess,
    #[graphql(name = "commit_write_access")]
    CommitWriteAccess,
    #[graphql(name = "manage_capabilities")]
    ManageCapabilities,
}

fn action_enum(action: &str) -> Action {
    if action == "http://terminusdb.com/schema/system#Action/create_database" {
        Action::CreateDatabase
    } else if action == "http://terminusdb.com/schema/system#Action/delete_database" {
        Action::DeleteDatabase
    } else if action == "http://terminusdb.com/schema/system#Action/class_frame" {
        Action::ClassFrame
    } else if action == "http://terminusdb.com/schema/system#Action/clone" {
        Action::Clone
    } else if action == "http://terminusdb.com/schema/system#Action/fetch" {
        Action::Fetch
    } else if action == "http://terminusdb.com/schema/system#Action/push" {
        Action::Push
    } else if action == "http://terminusdb.com/schema/system#Action/branch" {
        Action::Branch
    } else if action == "http://terminusdb.com/schema/system#Action/rebase" {
        Action::Rebase
    } else if action == "http://terminusdb.com/schema/system#Action/instance_read_access" {
        Action::InstanceReadAccess
    } else if action == "http://terminusdb.com/schema/system#Action/instance_write_access" {
        Action::InstanceWriteAccess
    } else if action == "http://terminusdb.com/schema/system#Action/schema_read_access" {
        Action::SchemaReadAccess
    } else if action == "http://terminusdb.com/schema/system#Action/schema_write_access" {
        Action::SchemaWriteAccess
    } else if action == "http://terminusdb.com/schema/system#Action/meta_read_access" {
        Action::MetaReadAccess
    } else if action == "http://terminusdb.com/schema/system#Action/meta_write_access" {
        Action::MetaWriteAccess
    } else if action == "http://terminusdb.com/schema/system#Action/commit_read_access" {
        Action::CommitReadAccess
    } else if action == "http://terminusdb.com/schema/system#Action/commit_write_access" {
        Action::CommitWriteAccess
    } else if action == "http://terminusdb.com/schema/system#Action/manage_capabilities" {
        Action::ManageCapabilities
    } else {
        panic!("This is not good!")
    }
}

fn maybe_object_string(db: &SyncStoreLayer, id: u64, prop: &str) -> Option<String> {
    db.predicate_id(prop)
        .and_then(|p| db.single_triple_sp(id, p))
        .and_then(|t| db.id_object(t.object))
        .and_then(|o| o.value())
        .as_ref()
        .map(value_to_json)
        .and_then(|j| j.as_str().map(|s| s.to_string()))
}
