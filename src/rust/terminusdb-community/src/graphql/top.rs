use crate::terminus_store::store::sync::*;
use crate::terminus_store::Layer;
use crate::types::*;
use crate::value::*;
use juniper::{self, graphql_object, graphql_interface, GraphQLObject, GraphQLEnum};
use swipl::prelude::*;

pub struct Info {
    system: SyncStoreLayer,
    user: Atom,
}

impl juniper::Context for Info {}

impl Info {
    pub fn new<C: QueryableContextType>(
        context: &Context<C>,
        db_term: &Term,
        auth_term: &Term,
    ) -> PrologResult<Info> {
        let user_: Atom = Atom::new("terminusdb://system/data/User/admin"); //auth_term.get_ex()?;
        let user;
        if user_ == atom!("anonymous") {
            user = atom!("terminusdb://system/data/User/anonymous");
        } else {
            user = user_;
        }
        let system = transaction_instance_layer(context, db_term)?.expect("system layer not found");

        Ok(Info { system, user })
    }
}

fn maybe_object_string(info : &Info, id : u64, prop : &str) -> Option<String> {
    info
        .system
        .predicate_id(prop)
        .and_then(|p| info
                  .system
                  .single_triple_sp(id, p)
        )
        .and_then(|t| info
                  .system
                  .id_object(t.object))
        .and_then(|o| o.value())
        .map(move |v|
             value_string_to_json(&v))
        .and_then(|j| j.as_str().clone().map(|s| s.to_string()))
}

fn required_object_string(info : &Info, id: u64, prop : &str) -> String{
    let predicate_id = info
        .system
        .predicate_id(prop)
        .expect("can't find name predicate");
    let name_id = info
        .system
        .single_triple_sp(id, predicate_id)
        .expect(&format!("can't find triple for {}", prop))
        .object;
    let name_unprocessed = info
        .system
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
        required_object_string(info, self.get_id(), "http://terminusdb.com/schema/system#name")
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
        maybe_object_string(info, self.get_id(), "http://terminusdb.com/schema/system#label")
    }

    fn comment(&self, #[graphql(context)] info: &Info) -> Option<String> {
        maybe_object_string(info, self.get_id(), "http://terminusdb.com/schema/system#comment")
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
        required_object_string(info, self.id, "http://terminusdb.com/schema/system#name")
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
        required_object_string(info, user_id, "http://terminusdb.com/schema/system#name")
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

pub struct Query;

#[graphql_object(context = Info)]
impl Query {
    /// Get the user that is currently logged in.
    fn user(#[graphql(context)] _info: &Info) -> User {
        User
    }
}
