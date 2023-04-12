use crate::terminus_store::store::sync::*;
use crate::terminus_store::structure::*;
use crate::terminus_store::Layer as TSLayer;
use crate::value::*;
use juniper::FromContext;
use juniper::{self, graphql_interface, graphql_object, GraphQLEnum};
use swipl::prelude::*;

use super::schema::SystemInfo;
use super::schema::TerminusContext;
impl juniper::Context for SystemInfo {}

impl<'a, C: QueryableContextType> FromContext<TerminusContext<'a, C>> for SystemInfo {
    fn from<'b>(value: &'b TerminusContext<'a, C>) -> &'b SystemInfo {
        &value.system_info
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

fn required_object_float(db: &SyncStoreLayer, id: u64, prop: &str) -> f64 {
    let predicate_id = db
        .predicate_id(prop)
        .unwrap_or_else(|| panic!("can't find {} predicate", prop));
    let name_id = db
        .single_triple_sp(id, predicate_id)
        .unwrap_or_else(|| panic!("can't find triple for {}", prop))
        .object;
    let f_unprocessed = db
        .id_object(name_id)
        .unwrap_or_else(|| panic!("no object for id {}", id))
        .value()
        .expect("returned object was not a value");

    f_unprocessed.as_val::<f64, f64>()
}

fn has_string_value(db: &SyncStoreLayer, id: u64, prop: &str, obj: &str) -> bool {
    let res = (|| {
        let predicate_id = db.predicate_id(prop)?;
        let object = String::make_entry(&obj);
        let object_id = db.object_value_id(&object)?;
        db.triple_exists(id, predicate_id, object_id).then_some(())
    })();
    res.is_some()
}

#[graphql_interface(for = [Database, Organization], context = SystemInfo)]
pub trait Resource {
    #[graphql(ignore)]
    fn get_id(&self) -> u64;
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        info.system
            .id_subject(self.get_id())
            .expect("can't make u64 into id")
    }
    fn name(&self, #[graphql(context)] info: &SystemInfo) -> String {
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

#[graphql_object(context = SystemInfo, impl = ResourceValue)]
impl Database {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as Resource>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as Resource>::name(self, info)
    }

    fn label(&self, #[graphql(context)] info: &SystemInfo) -> Option<String> {
        maybe_object_string(
            &info.system,
            self.get_id(),
            "http://terminusdb.com/schema/system#label",
        )
    }

    fn comment(&self, #[graphql(context)] info: &SystemInfo) -> Option<String> {
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

#[graphql_object(context = SystemInfo, impl = ResourceValue)]
impl Organization {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as Resource>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as Resource>::name(self, info)
    }

    fn database(&self, #[graphql(context)] info: &SystemInfo) -> Vec<Database> {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#database")
            .expect("can't find 'database' predicate");
        info.system
            .triples_sp(self.id, predicate_id)
            .map(|triple| Database { id: triple.object })
            .collect()
    }

    fn child(&self, #[graphql(context)] info: &SystemInfo) -> Vec<Organization> {
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

pub struct Capability {
    id: u64,
}

#[graphql_object(context = SystemInfo)]
impl Capability {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        info.system
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    fn role(&self, #[graphql(context)] info: &SystemInfo) -> Vec<Role> {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#role")
            .expect("can't find name predicate");

        info.system
            .triples_sp(self.id, predicate_id)
            .map(|triple| Role { id: triple.object })
            .collect()
    }

    fn scope(&self, #[graphql(context)] info: &SystemInfo) -> ResourceValue {
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

pub struct Role {
    id: u64,
}

#[graphql_object(context = SystemInfo)]
/// The user that is currently logged in.
impl Role {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        info.system
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    fn name(&self, #[graphql(context)] info: &SystemInfo) -> String {
        required_object_string(
            &info.system,
            self.id,
            "http://terminusdb.com/schema/system#name",
        )
    }

    fn action(&self, #[graphql(context)] info: &SystemInfo) -> Vec<Action> {
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

pub struct User;

#[graphql_object(context = SystemInfo)]
/// The user that is currently logged in.
impl User {
    fn id(#[graphql(context)] info: &SystemInfo) -> String {
        info.user.to_string()
    }

    /// The name of the user that is currently logged in.
    fn name(#[graphql(context)] info: &SystemInfo) -> String {
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

    fn capability(#[graphql(context)] info: &SystemInfo) -> Vec<Capability> {
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

#[graphql_interface(for = [Local, Remote], context = SystemInfo)]
pub trait Repository {
    #[graphql(ignore)]
    fn get_id(&self) -> u64;
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        info.meta
            .as_ref()
            .expect("No meta graph availble")
            .id_subject(self.get_id())
            .expect("can't make u64 into id")
    }
    fn name(&self, #[graphql(context)] info: &SystemInfo) -> String {
        required_object_string(
            info.meta.as_ref().expect("Missing meta graph"),
            self.get_id(),
            "http://terminusdb.com/schema/repository#name",
        )
    }

    fn head(&self, #[graphql(context)] info: &SystemInfo) -> Layer {
        let predicate_id = info
            .meta
            .as_ref()
            .expect("Missing meta graph")
            .predicate_id("http://terminusdb.com/schema/repository#head")
            .expect("can't find http://terminusdb.com/schema/repository#head predicate");
        let head_id = info
            .meta
            .as_ref()
            .expect("Missing meta graph")
            .single_triple_sp(self.get_id(), predicate_id)
            .expect("Repo has no head: broken repo graph")
            .object;
        Layer {
            layer_type: LayerType::Meta,
            id: head_id,
        }
    }
}

#[derive(Clone)]
pub struct Local {
    id: u64,
}

#[graphql_object(context = SystemInfo, impl = RepositoryValue)]
impl Local {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as Repository>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as Repository>::name(self, info)
    }

    fn head(&self, #[graphql(context)] info: &SystemInfo) -> Layer {
        <Self as Repository>::head(self, info)
    }
}

#[graphql_interface]
impl Repository for Local {
    fn get_id(&self) -> u64 {
        self.id
    }
}

#[derive(Clone)]
pub struct Remote {
    id: u64,
}

#[graphql_object(context = SystemInfo, impl = RepositoryValue)]
impl Remote {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as Repository>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as Repository>::name(self, info)
    }

    fn head(&self, #[graphql(context)] info: &SystemInfo) -> Layer {
        <Self as Repository>::head(self, info)
    }
}

#[graphql_interface]
impl Repository for Remote {
    fn get_id(&self) -> u64 {
        self.id
    }
}

pub enum LayerType {
    #[allow(unused)]
    Commit,
    Meta,
}

pub struct Layer {
    layer_type: LayerType,
    id: u64,
}

#[graphql_object(context = SystemInfo)]
/// The user that is currently logged in.
impl Layer {
    fn id(#[graphql(context)] info: &SystemInfo) -> String {
        match self.layer_type {
            LayerType::Commit => info
                .commit
                .as_ref()
                .expect("We have no commit graph")
                .id_subject(self.id)
                .expect("can't make u64 into id"),
            LayerType::Meta => info
                .meta
                .as_ref()
                .expect("We have no commit graph")
                .id_subject(self.id)
                .expect("can't make u64 into id"),
        }
    }
    fn identifier(#[graphql(context)] info: &SystemInfo) -> String {
        match self.layer_type {
            LayerType::Commit => required_object_string(
                info.commit.as_ref().expect("We have no commit graph"),
                self.id,
                "http://terminusdb.com/schema/layer#identifier",
            ),
            LayerType::Meta => required_object_string(
                info.meta.as_ref().expect("We have no commit graph"),
                self.id,
                "http://terminusdb.com/schema/layer#identifier",
            ),
        }
    }
}

fn id_to_commit(id: u64, info: &SystemInfo) -> AbstractCommitValue {
    let rdf_type_id = info
        .commit
        .as_ref()
        .expect("Missing meta graph")
        .predicate_id("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        .expect("Can't find rdf:type predicate");
    let maybe_commit_type = info
        .commit
        .as_ref()
        .expect("Missing meta graph")
        .subject_id("http://terminusdb.com/schema/ref#Commit");
    let maybe_initial_commit_type = info
        .commit
        .as_ref()
        .expect("Missing meta graph")
        .subject_id("http://terminusdb.com/schema/ref#InitialCommit");
    let maybe_invalid_commit_type = info
        .commit
        .as_ref()
        .expect("Missing meta graph")
        .subject_id("http://terminusdb.com/schema/ref#InvalidCommit");
    let maybe_valid_commit_type = info
        .commit
        .as_ref()
        .expect("Missing meta graph")
        .subject_id("http://terminusdb.com/schema/ref#ValidCommit");

    let type_id = info
        .commit
        .as_ref()
        .expect("Missing meta graph")
        .single_triple_sp(id, rdf_type_id)
        .expect("No type for commit object!")
        .object;

    if Some(type_id) == maybe_commit_type {
        Commit { id }.into()
    } else if Some(type_id) == maybe_invalid_commit_type {
        InvalidCommit { id }.into()
    } else if Some(type_id) == maybe_valid_commit_type {
        ValidCommit { id }.into()
    } else if Some(type_id) == maybe_initial_commit_type {
        InitialCommit { id }.into()
    } else {
        panic!("Unable to find a commit type!")
    }
}

fn head(id: u64, info: &SystemInfo) -> Option<AbstractCommitValue> {
    let predicate_id = info
        .commit
        .as_ref()
        .expect("Missing meta graph")
        .predicate_id("http://terminusdb.com/schema/ref#head")
        .expect("can't find http://terminusdb.com/schema/ref#head predicate");

    // We need the correct type of commit here!
    info.commit
        .as_ref()
        .expect("Missing meta graph")
        .single_triple_sp(id, predicate_id)
        .map(|t| id_to_commit(t.object, info))
}

pub struct Branch {
    id: u64,
}

#[graphql_object(context = SystemInfo)]
impl Branch {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        info.commit
            .as_ref()
            .expect("No meta graph availble")
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    fn name(&self, #[graphql(context)] info: &SystemInfo) -> String {
        required_object_string(
            info.commit.as_ref().expect("Missing meta graph"),
            self.id,
            "http://terminusdb.com/schema/ref#name",
        )
    }

    fn log(
        &self,
        offset: Option<i32>,
        limit: Option<i32>,
        #[graphql(context)] info: &SystemInfo,
    ) -> Vec<AbstractCommitValue> {
        let start = offset.unwrap_or(0);
        let count = limit.unwrap_or(-1);
        let mut vec = if limit.is_some() {
            Vec::with_capacity(count as usize)
        } else {
            Vec::new()
        };
        let mut maybe_commit = head(self.id, info);
        let mut i = 0;
        let mut j = 0;
        while j != count && maybe_commit.is_some() {
            let commit = maybe_commit.unwrap();
            let id = commit.get_id();
            i += 1;
            if i > start {
                // Need to do author filter here..
                j += 1;
                vec.push(commit);
            }
            maybe_commit = (|| {
                let predicate_id = info
                    .commit
                    .as_ref()
                    .expect("Missing commit graph")
                    .predicate_id("http://terminusdb.com/schema/ref#parent")?;
                info.commit
                    .as_ref()
                    .expect("Missing commit graph")
                    .single_triple_sp(id, predicate_id)
                    .map(|t| id_to_commit(t.object, info))
            })();
        }
        vec
    }

    fn head(&self, #[graphql(context)] info: &SystemInfo) -> Option<AbstractCommitValue> {
        head(self.id, info)
    }
}

#[graphql_interface(for = [Commit, InitialCommit, ValidCommit, InvalidCommit], context = SystemInfo)]
pub trait AbstractCommit {
    #[graphql(ignore)]
    fn get_id(&self) -> u64;
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        info.commit
            .as_ref()
            .expect("No commit graph")
            .id_subject(self.get_id())
            .expect("can't make u64 into id")
    }

    fn author(&self, #[graphql(context)] info: &SystemInfo) -> String {
        required_object_string(
            info.commit.as_ref().expect("Missing commit graph"),
            self.get_id(),
            "http://terminusdb.com/schema/ref#author",
        )
    }

    fn message(&self, #[graphql(context)] info: &SystemInfo) -> String {
        required_object_string(
            info.commit.as_ref().expect("Missing commit graph"),
            self.get_id(),
            "http://terminusdb.com/schema/ref#message",
        )
    }

    fn timestamp(&self, #[graphql(context)] info: &SystemInfo) -> f64 {
        required_object_float(
            info.commit.as_ref().expect("Missing commit graph"),
            self.get_id(),
            "http://terminusdb.com/schema/ref#timestamp",
        )
    }

    fn parent(&self, #[graphql(context)] info: &SystemInfo) -> Option<AbstractCommitValue> {
        let predicate_id = info
            .commit
            .as_ref()
            .expect("Missing meta graph")
            .predicate_id("http://terminusdb.com/schema/ref#parent")
            .expect("can't find http://terminusdb.com/schema/ref#parent predicate");

        // We need the correct type of commit here!
        info.commit
            .as_ref()
            .expect("Missing meta graph")
            .single_triple_sp(self.get_id(), predicate_id)
            .map(|t| id_to_commit(t.object, info))
    }
}

#[derive(Clone)]
pub struct Commit {
    id: u64,
}

#[graphql_object(context = SystemInfo, impl = AbstractCommitValue)]
impl Commit {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::id(self, info)
    }

    fn author(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::author(self, info)
    }

    fn message(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::message(self, info)
    }

    fn timestamp(&self, #[graphql(context)] info: &SystemInfo) -> f64 {
        <Self as AbstractCommit>::timestamp(self, info)
    }

    fn parent(&self, #[graphql(context)] info: &SystemInfo) -> Option<AbstractCommitValue> {
        <Self as AbstractCommit>::parent(self, info)
    }
}

#[graphql_interface]
impl AbstractCommit for Commit {
    fn get_id(&self) -> u64 {
        self.id
    }
}

#[derive(Clone)]
pub struct InitialCommit {
    id: u64,
}

#[graphql_object(context = SystemInfo, impl = AbstractCommitValue)]
impl InitialCommit {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::id(self, info)
    }

    fn author(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::author(self, info)
    }

    fn message(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::message(self, info)
    }

    fn timestamp(&self, #[graphql(context)] info: &SystemInfo) -> f64 {
        <Self as AbstractCommit>::timestamp(self, info)
    }

    fn parent(&self, #[graphql(context)] info: &SystemInfo) -> Option<AbstractCommitValue> {
        <Self as AbstractCommit>::parent(self, info)
    }
}

#[graphql_interface]
impl AbstractCommit for InitialCommit {
    fn get_id(&self) -> u64 {
        self.id
    }
}

#[derive(Clone)]
pub struct ValidCommit {
    id: u64,
}

#[graphql_object(context = SystemInfo, impl = AbstractCommitValue)]
impl ValidCommit {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::id(self, info)
    }

    fn author(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::author(self, info)
    }

    fn message(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::message(self, info)
    }

    fn timestamp(&self, #[graphql(context)] info: &SystemInfo) -> f64 {
        <Self as AbstractCommit>::timestamp(self, info)
    }

    fn parent(&self, #[graphql(context)] info: &SystemInfo) -> Option<AbstractCommitValue> {
        <Self as AbstractCommit>::parent(self, info)
    }
}

#[graphql_interface]
impl AbstractCommit for ValidCommit {
    fn get_id(&self) -> u64 {
        self.id
    }
}

#[derive(Clone)]
pub struct InvalidCommit {
    id: u64,
}

#[graphql_object(context = SystemInfo, impl = AbstractCommitValue)]
impl InvalidCommit {
    fn id(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::id(self, info)
    }

    fn author(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::author(self, info)
    }

    fn message(&self, #[graphql(context)] info: &SystemInfo) -> String {
        <Self as AbstractCommit>::message(self, info)
    }

    fn timestamp(&self, #[graphql(context)] info: &SystemInfo) -> f64 {
        <Self as AbstractCommit>::timestamp(self, info)
    }

    fn parent(&self, #[graphql(context)] info: &SystemInfo) -> Option<AbstractCommitValue> {
        <Self as AbstractCommit>::parent(self, info)
    }
}

#[graphql_interface]
impl AbstractCommit for InvalidCommit {
    fn get_id(&self) -> u64 {
        self.id
    }
}

pub struct System;

#[graphql_object(context = SystemInfo)]
#[no_async]
impl System {
    /// Get the user that is currently logged in.
    fn user(#[graphql(context)] _info: &SystemInfo) -> User {
        User
    }
    fn repository(
        name: Option<String>,
        #[graphql(context)] info: &SystemInfo,
    ) -> Vec<RepositoryValue> {
        if info.meta.is_some() {
            // And get the type
            let predicate_id: u64 = info
                .meta
                .as_ref()
                .unwrap()
                .predicate_id("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                .expect("can't find rdf:type predicate");
            let mut local_objs: Vec<RepositoryValue> = info
                .meta
                .as_ref()
                .unwrap()
                .subject_id("http://terminusdb.com/schema/repository#Local")
                .map(|local_id| {
                    info.meta
                        .as_ref()
                        .unwrap()
                        .triples_o(local_id)
                        .filter(move |t| t.predicate == predicate_id)
                        .filter(|t| {
                            name.is_none()
                                || has_string_value(
                                    info.meta.as_ref().unwrap(),
                                    t.subject,
                                    "http://terminusdb.com/schema/repository#name",
                                    name.as_ref().unwrap(),
                                )
                        })
                        .map(|t| Local { id: t.subject }.into())
                        .collect::<Vec<RepositoryValue>>()
                })
                .into_iter()
                .flatten()
                .collect();
            let mut remote_objs: Vec<RepositoryValue> = info
                .meta
                .as_ref()
                .unwrap()
                .subject_id("http://terminusdb.com/schema/repository#Remote")
                .map(|remote_id| {
                    info.meta
                        .as_ref()
                        .unwrap()
                        .triples_o(remote_id)
                        .filter(move |t| t.predicate == predicate_id)
                        .filter(|t| {
                            name.is_none()
                                || has_string_value(
                                    info.meta.as_ref().unwrap(),
                                    t.subject,
                                    "http://terminusdb.com/schema/repository#name",
                                    name.as_ref().unwrap(),
                                )
                        })
                        .map(|t| Local { id: t.subject }.into())
                        .collect::<Vec<RepositoryValue>>()
                })
                .into_iter()
                .flatten()
                .collect();
            local_objs.append(&mut remote_objs);
            local_objs
        } else {
            Vec::new()
        }
    }

    fn branch(name: Option<String>, #[graphql(context)] info: &SystemInfo) -> Vec<Branch> {
        if info.commit.is_some() {
            // And get the type
            let predicate_id: u64 = info
                .commit
                .as_ref()
                .unwrap()
                .predicate_id("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                .expect("can't find rdf:type predicate");
            info.commit
                .as_ref()
                .unwrap()
                .subject_id("http://terminusdb.com/schema/ref#Branch")
                .map(|branch_id| {
                    info.commit
                        .as_ref()
                        .unwrap()
                        .triples_o(branch_id)
                        .filter(move |t| t.predicate == predicate_id)
                        .filter(|t| {
                            name.is_none()
                                || has_string_value(
                                    info.commit.as_ref().unwrap(),
                                    t.subject,
                                    "http://terminusdb.com/schema/ref#name",
                                    name.as_ref().unwrap(),
                                )
                        })
                        .map(|t| Branch { id: t.subject })
                        .collect::<Vec<Branch>>()
                })
                .into_iter()
                .flatten()
                .collect()
        } else {
            Vec::new()
        }
    }
}
