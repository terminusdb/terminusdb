:- module(user_management,
          [
              add_user/4,
              agent_name_uri/3,
              agent_name_exists/2
          ]).

:- use_module(core(util)).
:- use_module(core(api)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).

/** <module> User
 *
 * User management. This is required for testing the capabilities system,
 * but may later be exposed for a user API.
 */

/*
 * agent_name_uri(Askable, Name, User_URI) is semidet.
 */
agent_name_uri(Askable, Name, User_URI) :-
    once(ask(Askable,
             t(User_URI, terminus:agent_name, Name^^xsd:string)
            )).

agent_name_exists(Askable, Name) :-
    agent_name_uri(Askable, Name, _URI).

/*
 * add_user
 */
add_user(Nick, Email, Pass, User_URI) :-
    create_context(terminus_descriptor{},
                   commit_info{
                       author: "add_user/4",
                       message: "Non-existent"
                   }, TerminusDB),

    (   agent_name_exists(TerminusDB, Nick)
    ->  throw(error(user_already_exists(Nick)))
    ;   true),

    with_transaction(
        TerminusDB,
        (
            crypto_password_hash(Pass,Hash),
            ask(TerminusDB,
                (   idgen(doc:'User',[Nick^^xsd:string], User_URI),
                    idgen(doc:'Capability',[Nick^^xsd:string], Capability_URI),
                    idgen(doc:'Access',["terminus"^^xsd:string, "create_database"^^xsd:string], Access_URI),
                    t(Server_URI, terminus:resource_name, "terminus"^^xsd:string),
                    insert(User_URI,rdf:type, terminus:'User'),
                    insert(User_URI,terminus:email, Email^^xsd:string),
                    insert(User_URI,terminus:agent_name, Nick^^xsd:string),
                    insert(User_URI,terminus:agent_key_hash, Hash^^xsd:string),
                    insert(User_URI,terminus:authority, Capability_URI),
                    insert(Capability_URI, rdf:type, terminus:'Capability'),
                    insert(Capability_URI, terminus:access, Access_URI),
                    insert(Access_URI, rdf:type, terminus:'Access'),
                    insert(Access_URI, terminus:authority_scope, Server_URI),
                    insert(Access_URI, terminus:action, terminus:create_database)
                )
               )
        ),
        _Meta_Data
    ).

:- begin_tests(user_management).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).

test(add_user, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    add_user("Gavin", "gavin@terminusdb.com", "here.i.am", URI),

    agent_name_uri(terminus_descriptor{}, "Gavin", URI),

    once(ask(terminus_descriptor{},
             t(URI, terminus:email, "gavin@terminusdb.com"^^xsd:string))).

:- end_tests(user_management).
