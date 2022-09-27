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


#[graphql_interface(for = [Database, Organization], context = Info)]
pub trait Resource {
    fn id(&self) -> String;
    fn name(&self) -> String;
}

#[derive(Clone)]
pub struct Database {
    id : u64
}

#[graphql_object(context = Info, impl = ResourceValue)]
impl Database {
    pub fn id(&self, #[graphql(context)] info: &Info) -> String {
        info
            .system
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    pub fn name(&self, #[graphql(context)] info: &Info) -> String {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#name")
            .expect("can't find name predicate");
        let name_id = info
            .system
            .single_triple_sp(self.id, predicate_id)
            .expect("can't find triple")
            .object;
        let name_unprocessed = info
            .system
            .id_object(name_id)
            .expect("no object for id")
            .value()
            .expect("returned object was no value");

        let name_json = value_string_to_json(&name_unprocessed);
        let name = name_json.as_str().unwrap();

        name.to_string()
    }
}

#[derive(Clone)]
pub struct Organization {
    id : u64
}

#[graphql_object(context = Info, impl = ResourceValue)]
impl Organization {
    pub fn id(&self, #[graphql(context)] info: &Info) -> String {
        info
            .system
            .id_subject(self.id)
            .expect("can't make u64 into id")
    }

    pub fn name(&self, #[graphql(context)] info: &Info) -> String {
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#name")
            .expect("can't find name predicate");
        let name_id = info
            .system
            .single_triple_sp(self.id, predicate_id)
            .expect("can't find triple")
            .object;
        let name_unprocessed = info
            .system
            .id_object(name_id)
            .expect("no object for id")
            .value()
            .expect("returned object was no value");

        let name_json = value_string_to_json(&name_unprocessed);
        let name = name_json.as_str().unwrap();

        name.to_string()
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
            .subject_id("http://terminusdb.com/schema/system#Database");

        if db_type_id == Some(type_id) {
            Database{ id : scope_id }.into()
        }else{
            Organization{ id : scope_id }.into()
        }
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
        let predicate_id = info
            .system
            .predicate_id("http://terminusdb.com/schema/system#name")
            .expect("can't find name predicate");
        let name_id = info
            .system
            .single_triple_sp(user_id, predicate_id)
            .expect("can't find triple")
            .object;
        let name_unprocessed = info
            .system
            .id_object(name_id)
            .expect("no object for id")
            .value()
            .expect("returned object was no value");

        let name_json = value_string_to_json(&name_unprocessed);
        let name = name_json.as_str().unwrap();

        name.to_string()
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
