use crate::terminus_store::store::sync::*;
use crate::terminus_store::Layer;
use crate::types::*;
use crate::value::*;
use juniper::{self, graphql_object, GraphQLObject};
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
        let user_: Atom = auth_term.get_ex()?;
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

pub struct User;

#[graphql_object(context = Info)]
/// The user that is currently logged in.
impl User {
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
}

pub struct Query;

#[graphql_object(context = Info)]
impl Query {
    /// Get the user that is currently logged in.
    fn user(#[graphql(context)] info: &Info) -> User {
        User
    }
}
