use juniper::graphql_object;
use swipl::prelude::GenericQueryableContext;

struct MainContext {
    _prolog: GenericQueryableContext<'static>,
}
impl juniper::Context for MainContext {}

/*
#[derive(Default)]
struct MainRoot;

#[graphql_object(context = MainContext)]
#[no_async]
impl MainRoot {
    fn user(#[graphql(context)] _context: &MainContext) -> User {
        User
    }
}
*/

#[derive(Default)]
struct User;

#[graphql_object(context = MainContext, noasync)]
impl User {
    fn id() -> &'static str {
        "hello"
    }
}
