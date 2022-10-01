use crate::graphql::schema::TerminusType;
use crate::terminus_store::store::sync::*;
use crate::terminus_store::Layer as TSLayer;
use crate::types::*;
use crate::value::*;
use juniper::{self, graphql_object, graphql_interface, GraphQLObject, GraphQLEnum};
use swipl::prelude::*;

use super::frame::AllFrames;

pub struct Info {
    system: SyncStoreLayer,
    meta : Option<SyncStoreLayer>,
    commit : Option<SyncStoreLayer>,
    branch: Option<SyncStoreLayer>,
    schema: Option<SyncStoreLayer>,
    user: Atom,
    frames: Option<AllFrames>
}

impl juniper::Context for Info {}

impl Info {
    pub fn new<C: QueryableContextType>(
        context: &Context<C>,
        system_term: &Term,
        meta_term: &Term,
        commit_term: &Term,
        branch_term: &Term,
        auth_term: &Term,
    ) -> PrologResult<Info> {
        let user_: Atom = Atom::new("terminusdb://system/data/User/admin"); //auth_term.get_ex()?;
        let user;
        if user_ == atom!("anonymous") {
            user = atom!("terminusdb://system/data/User/anonymous");
        } else {
            user = user_;
        }
        let system = transaction_instance_layer(context, system_term)?.expect("system layer not found");
        let meta = if meta_term.unify(atomable("none")).is_ok() {
            None
        }else{
            transaction_instance_layer(context, meta_term).expect("Missing meta layer")
        };
        let commit = if commit_term.unify(atomable("none")).is_ok() {
            None
        }else{
            transaction_instance_layer(context, commit_term).expect("Missing commit layer")
        };
        let branch = if branch_term.unify(atomable("none")).is_ok() {
            None
        }else{
            transaction_instance_layer(context, branch_term).expect("Missing branch layer")
        };
        Ok(Info { system, meta, commit, branch, schema: None, user, frames: None })
    }
}

fn maybe_object_string(db : &SyncStoreLayer, id : u64, prop : &str) -> Option<String> {
    db
        .predicate_id(prop)
        .and_then(|p| db
                  .single_triple_sp(id, p)
        )
        .and_then(|t| db
                  .id_object(t.object))
        .and_then(|o| o.value())
        .map(move |v|
             value_string_to_json(&v))
        .and_then(|j| j.as_str().clone().map(|s| s.to_string()))
}

fn required_object_string(db : &SyncStoreLayer, id: u64, prop : &str) -> String{
    let predicate_id = db
        .predicate_id(prop)
        .expect(&format!("can't find {} predicate", prop));
    let name_id = db
        .single_triple_sp(id, predicate_id)
        .expect(&format!("can't find triple for {}", prop))
        .object;
    let name_unprocessed = db
        .id_object(name_id)
        .expect(&format!("no object for id {}", id))
        .value()
        .expect("returned object was no value");

    let name_json = value_string_to_json(&name_unprocessed);
    let name = name_json.as_str().unwrap();

    name.to_string()
}

#[graphql_interface(for = [Database, Organization], context = Info)]
pub trait Resource {
    #[graphql(ignore)]
    fn get_id(&self) -> u64;
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        info
            .system
            .id_subject(self.get_id())
            .expect("can't make u64 into id")
    }
    fn name(&self,#[graphql(context)] info: &Info) -> String {
        required_object_string(&info.system, self.get_id(), "http://terminusdb.com/schema/system#name")
    }
}

#[derive(Clone)]
pub struct Database {
    id : u64
}

#[graphql_object(context = Info, impl = ResourceValue)]
impl Database {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        <Self as Resource>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &Info) -> String {
        <Self as Resource>::name(self, info)
    }

    fn label(&self, #[graphql(context)] info: &Info) -> Option<String> {
        maybe_object_string(&info.system, self.get_id(), "http://terminusdb.com/schema/system#label")
    }

    fn comment(&self, #[graphql(context)] info: &Info) -> Option<String> {
        maybe_object_string(&info.system, self.get_id(), "http://terminusdb.com/schema/system#comment")
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
    id : u64
}

#[graphql_object(context = Info, impl = ResourceValue)]
impl Organization {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        <Self as Resource>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &Info) -> String {
        <Self as Resource>::name(self, info)
    }

    fn database(&self, #[graphql(context)] info: &Info) -> Vec<Database> {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#database")
            .expect("can't find 'database' predicate");
        info
            .system
            .triples_sp(self.id, predicate_id)
            .map(|triple| Database{ id: triple.object })
            .collect()
    }

    fn child(&self, #[graphql(context)] info: &Info) -> Vec<Organization> {
        // if no children, empty.
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#child")
            .expect("can't find child' predicate");
        info
            .system
            .triples_sp(self.id, predicate_id)
            .map(|triple| Organization{ id: triple.object })
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
    id : u64
}

#[graphql_object(context = Info)]
impl Capability {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        info
            .system
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    fn role(&self, #[graphql(context)] info: &Info) -> Vec<Role> {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#role")
            .expect("can't find name predicate");

        info
            .system
            .triples_sp(self.id, predicate_id)
            .map(|triple| Role{ id: triple.object })
            .collect()
    }

    fn scope(&self, #[graphql(context)] info: &Info) -> ResourceValue {
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
            Database{ id : scope_id }.into()
        }else{
            Organization{ id : scope_id }.into()
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
    ManageCapabilities
}

fn action_enum(action : &str) -> Action {
    if action == "http://terminusdb.com/schema/system#Action/create_database" {
        Action::CreateDatabase
    }else if action == "http://terminusdb.com/schema/system#Action/delete_database" {
        Action::DeleteDatabase
    }else if action == "http://terminusdb.com/schema/system#Action/class_frame" {
        Action::ClassFrame
    }else if action == "http://terminusdb.com/schema/system#Action/clone"{
        Action::Clone
    }else if action == "http://terminusdb.com/schema/system#Action/fetch"{
        Action::Fetch
    }else if action == "http://terminusdb.com/schema/system#Action/push"{
        Action::Push
    }else if action == "http://terminusdb.com/schema/system#Action/branch"{
        Action::Branch
    }else if action == "http://terminusdb.com/schema/system#Action/rebase"{
        Action::Rebase
    }else if action == "http://terminusdb.com/schema/system#Action/instance_read_access"{
        Action::InstanceReadAccess
    }else if action == "http://terminusdb.com/schema/system#Action/instance_write_access"{
        Action::InstanceWriteAccess
    }else if action == "http://terminusdb.com/schema/system#Action/schema_read_access"{
        Action::SchemaReadAccess
    }else if action == "http://terminusdb.com/schema/system#Action/schema_write_access"{
        Action::SchemaWriteAccess
    }else if action == "http://terminusdb.com/schema/system#Action/meta_read_access"{
        Action::MetaReadAccess
    }else if action == "http://terminusdb.com/schema/system#Action/meta_write_access"{
        Action::MetaWriteAccess
    }else if action == "http://terminusdb.com/schema/system#Action/commit_read_access"{
        Action::CommitReadAccess
    }else if action == "http://terminusdb.com/schema/system#Action/commit_write_access"{
        Action::CommitWriteAccess
    }else if action == "http://terminusdb.com/schema/system#Action/manage_capabilities"{
        Action::ManageCapabilities
    }else{
        panic!("This is not good!")
    }
}

pub struct Role{
    id : u64
}

#[graphql_object(context = Info)]
/// The user that is currently logged in.
impl Role {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        info
            .system
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    fn name(&self,#[graphql(context)] info: &Info) -> String {
        required_object_string(&info.system, self.id, "http://terminusdb.com/schema/system#name")
    }

    fn action(&self,#[graphql(context)] info: &Info) -> Vec<Action> {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#action")
            .expect("can't find 'database' predicate");
        info
            .system
            .triples_sp(self.id, predicate_id)
            .map(|triple| triple.object)
            .map(|o| info
                 .system
                 .id_subject(o)
                 .expect("Invalid Action in database (corrupted)"))
            .map(|action| action_enum(&action))
            .collect()
    }
}

pub struct User;

#[graphql_object(context = Info)]
/// The user that is currently logged in.
impl User {
    fn id(#[graphql(context)] info: &Info) -> String {
        info.user.to_string()
    }

    /// The name of the user that is currently logged in.
    fn name(#[graphql(context)] info: &Info) -> String {
        let user_id = info
            .system
            .subject_id(&info.user.to_string())
            .expect(&format!(
                "can't make user id from {}",
                info.user.to_string()
            ));
        required_object_string(&info.system, user_id, "http://terminusdb.com/schema/system#name")
    }

    fn capability(#[graphql(context)] info: &Info) -> Vec<Capability> {
        let user_id = info
            .system
            .subject_id(&info.user.to_string())
            .expect(&format!(
                "can't make user id from {}",
                info.user.to_string()
            ));
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#capability")
            .expect("can't find capability predicate");

        info
            .system
            .triples_sp(user_id, predicate_id)
            .map(|triple| Capability{ id: triple.object })
            .collect()
    }
}

#[graphql_interface(for = [Local, Remote], context = Info)]
pub trait Repository {
    #[graphql(ignore)]
    fn get_id(&self) -> u64;
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        info
            .meta
            .as_ref()
            .expect("No meta graph availble")
            .id_subject(self.get_id())
            .expect("can't make u64 into id")
    }
    fn name(&self,#[graphql(context)] info: &Info) -> String {
        required_object_string(&info.meta.as_ref().expect("Missing meta graph"), self.get_id(), "http://terminusdb.com/schema/repository#name")
    }

    fn head(&self,#[graphql(context)] info: &Info) -> Layer {
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
        Layer{
            layer_type : LayerType::Meta,
            id : head_id
        }
    }
}

#[derive(Clone)]
pub struct Local {
    id : u64
}

#[graphql_object(context = Info, impl = RepositoryValue)]
impl Local {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        <Self as Repository>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &Info) -> String {
        <Self as Repository>::name(self, info)
    }

    fn head(&self, #[graphql(context)] info: &Info) -> Layer {
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
    id : u64
}

#[graphql_object(context = Info, impl = RepositoryValue)]
impl Remote {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        <Self as Repository>::id(self, info)
    }

    fn name(&self, #[graphql(context)] info: &Info) -> String {
        <Self as Repository>::name(self, info)
    }

    fn head(&self, #[graphql(context)] info: &Info) -> Layer {
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
    Commit,
    Meta,
}

pub struct Layer {
    layer_type : LayerType,
    id : u64,
}

#[graphql_object(context = Info)]
/// The user that is currently logged in.
impl Layer {
    fn id(#[graphql(context)] info: &Info) -> String {
        match self.layer_type {
            LayerType::Commit => {
                info
                    .commit
                    .as_ref()
                    .expect("We have no commit graph")
                    .id_subject(self.id)
                    .expect("can't make u64 into id")
            }
            LayerType::Meta => {
                info
                    .meta
                    .as_ref()
                    .expect("We have no commit graph")
                    .id_subject(self.id)
                    .expect("can't make u64 into id")
            }
        }
    }
    fn identifier(#[graphql(context)] info: &Info) -> String {
        match self.layer_type {
            LayerType::Commit => {
                required_object_string(&info.commit.as_ref().expect("We have no commit graph"), self.id, "http://terminusdb.com/schema/layer#identifier")
            }
            LayerType::Meta => {
                required_object_string(&info.meta.as_ref().expect("We have no commit graph"), self.id, "http://terminusdb.com/schema/layer#identifier")
            }
        }
    }
}

pub struct Branch {
    id : u64
}

#[graphql_object(context = Info)]
impl Branch {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        info
            .commit
            .as_ref()
            .expect("No meta graph availble")
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    fn name(&self,#[graphql(context)] info: &Info) -> String {
        required_object_string(&info.commit.as_ref().expect("Missing meta graph"), self.id, "http://terminusdb.com/schema/ref#name")
    }

    fn head(&self,#[graphql(context)] info: &Info) -> Option<AbstractCommitValue> {
        let predicate_id = info
            .commit
            .as_ref()
            .expect("Missing meta graph")
            .predicate_id("http://terminusdb.com/schema/ref#head")
            .expect("can't find http://terminusdb.com/schema/ref#head predicate");
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

        // We need the correct type of commit here!
        info
            .commit
            .as_ref()
            .expect("Missing meta graph")
            .single_triple_sp(self.id, predicate_id)
            .map(|t| {
                let type_id = info
                    .commit
                    .as_ref()
                    .expect("Missing meta graph")
                    .single_triple_sp(t.object, rdf_type_id)
                    .expect("No type for commit object!")
                    .object;
                if Some(type_id) == maybe_commit_type {
                    Commit{
                        id : t.object
                    }.into()
                } else if Some(type_id) == maybe_invalid_commit_type {
                    InvalidCommit{
                        id : t.object
                    }.into()
                } else if Some(type_id) == maybe_valid_commit_type {
                    ValidCommit{
                        id : t.object
                    }.into()
                } else if Some(type_id) == maybe_initial_commit_type {
                    InitialCommit{
                        id : t.object
                    }.into()
                } else {
                    panic!("Unable to find a commit type!")
                }
            })
    }
}

#[graphql_interface(for = [Commit, InitialCommit, ValidCommit, InvalidCommit], context = Info)]
pub trait AbstractCommit {
    #[graphql(ignore)]
    fn get_id(&self) -> u64;
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        info
            .commit
            .as_ref()
            .expect("No commit graph")
            .id_subject(self.get_id())
            .expect("can't make u64 into id")
    }

    fn author(&self,#[graphql(context)] info: &Info) -> String {
        required_object_string(&info.commit.as_ref().expect("Missing commit graph"), self.get_id(), "http://terminusdb.com/schema/ref#author")
    }

    fn message(&self,#[graphql(context)] info: &Info) -> String {
        required_object_string(&info.commit.as_ref().expect("Missing commit graph"), self.get_id(), "http://terminusdb.com/schema/ref#message")
    }

    fn parent(&self,#[graphql(context)] info: &Info) -> Option<AbstractCommitValue> {
        let predicate_id = info
            .commit
            .as_ref()
            .expect("Missing meta graph")
            .predicate_id("http://terminusdb.com/schema/ref#parent")
            .expect("can't find http://terminusdb.com/schema/ref#parent predicate");
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

        // We need the correct type of commit here!
        info
            .commit
            .as_ref()
            .expect("Missing meta graph")
            .single_triple_sp(self.get_id(), predicate_id)
            .map(|t| {
                let type_id = info
                    .commit
                    .as_ref()
                    .expect("Missing meta graph")
                    .single_triple_sp(t.object, rdf_type_id)
                    .expect("No type for commit object!")
                    .object;
                if Some(type_id) == maybe_commit_type {
                    Commit{
                        id : t.object
                    }.into()
                } else if Some(type_id) == maybe_invalid_commit_type {
                    InvalidCommit{
                        id : t.object
                    }.into()
                } else if Some(type_id) == maybe_valid_commit_type {
                    ValidCommit{
                        id : t.object
                    }.into()
                } else if Some(type_id) == maybe_initial_commit_type {
                    InitialCommit{
                        id : t.object
                    }.into()
                } else {
                    panic!("Unable to find a commit type!")
                }
            })
    }
}

#[derive(Clone)]
pub struct Commit {
    id : u64
}

#[graphql_object(context = Info, impl = AbstractCommitValue)]
impl Commit {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::id(self, info)
    }

    fn author(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::author(self, info)
    }

    fn message(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::message(self, info)
    }

    fn parent(&self, #[graphql(context)] info: &Info) -> Option<AbstractCommitValue> {
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
    id : u64
}

#[graphql_object(context = Info, impl = AbstractCommitValue)]
impl InitialCommit {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::id(self, info)
    }

    fn author(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::author(self, info)
    }

    fn message(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::message(self, info)
    }

    fn parent(&self, #[graphql(context)] info: &Info) -> Option<AbstractCommitValue> {
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
    id : u64
}

#[graphql_object(context = Info, impl = AbstractCommitValue)]
impl ValidCommit {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::id(self, info)
    }

    fn author(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::author(self, info)
    }

    fn message(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::message(self, info)
    }

    fn parent(&self, #[graphql(context)] info: &Info) -> Option<AbstractCommitValue> {
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
    id : u64
}

#[graphql_object(context = Info, impl = AbstractCommitValue)]
impl InvalidCommit {
    fn id(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::id(self, info)
    }

    fn author(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::author(self, info)
    }

    fn message(&self, #[graphql(context)] info: &Info) -> String {
        <Self as AbstractCommit>::message(self, info)
    }

    fn parent(&self, #[graphql(context)] info: &Info) -> Option<AbstractCommitValue> {
        <Self as AbstractCommit>::parent(self, info)
    }
}

#[graphql_interface]
impl AbstractCommit for InvalidCommit {
    fn get_id(&self) -> u64 {
        self.id
    }
}

pub struct Query;

#[graphql_object(context = Info)]
#[no_async]
impl Query {
    /// Get the user that is currently logged in.
    fn user(#[graphql(context)] _info: &Info) -> User {
        User
    }
    fn repository(#[graphql(context)] info: &Info) -> Vec<RepositoryValue> {
        if let Some(_) = info.meta {
            // And get the type
            let predicate_id : u64 = info
                .meta
                .as_ref()
                .unwrap()
                .predicate_id("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                .expect("can't find rdf:type predicate");
            let mut local_objs : Vec<RepositoryValue> = info
                .meta
                .as_ref()
                .unwrap()
                .subject_id("http://terminusdb.com/schema/repository#Local")
                .map(|local_id| info
                     .meta
                     .as_ref()
                     .unwrap()
                     .triples_o(local_id)
                     .filter(move |t| t.predicate == predicate_id)
                     .map(|t| Local{ id: t.subject }.into())
                     .collect::<Vec<RepositoryValue>>()
                )
                .into_iter()
                .flatten()
                .collect();
            let mut remote_objs : Vec<RepositoryValue> = info
                .meta
                .as_ref()
                .unwrap()
                .subject_id("http://terminusdb.com/schema/repository#Remote")
                .map(|remote_id| info
                     .meta
                     .as_ref()
                     .unwrap()
                     .triples_o(remote_id)
                     .filter(move |t| t.predicate == predicate_id)
                     .map(|t| Local{ id: t.subject }.into())
                     .collect::<Vec<RepositoryValue>>()
                )
                .into_iter()
                .flatten()
                .collect();
            local_objs.append(&mut remote_objs);
            local_objs
        }else{
            Vec::new()
        }
    }

    fn branch(#[graphql(context)] info: &Info) -> Vec<Branch> {
        if let Some(_) = info.commit {
            // And get the type
            let predicate_id : u64 = info
                .commit
                .as_ref()
                .unwrap()
                .predicate_id("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                .expect("can't find rdf:type predicate");
            info
                .commit
                .as_ref()
                .unwrap()
                .subject_id("http://terminusdb.com/schema/ref#Branch")
                .map(|branch_id| info
                     .commit
                     .as_ref()
                     .unwrap()
                     .triples_o(branch_id)
                     .filter(move |t| t.predicate == predicate_id)
                     .map(|t| Branch{ id: t.subject })
                     .collect::<Vec<Branch>>()
                )
                .into_iter()
                .flatten()
                .collect()
        }else{
            Vec::new()
        }
    }
}
