:- module(user_management,
          [
              add_user/4,
              agent_name_uri/3,
              agent_name_exists/2,
              delete_user/1
          ]).

:- use_module(core(util)).
:- use_module(core(api)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(capabilities).

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
             t(User_URI, system:agent_name, Name^^xsd:string)
            )).

agent_name_exists(Askable, Name) :-
    agent_name_uri(Askable, Name, _).

/*
 * add_user
 */
add_user(Nick, Email, Pass, User_URI) :-
    create_context(system_descriptor{},
                   commit_info{
                       author: "add_user/4",
                       message: "internal system operation"
                   }, SystemDB),

    (   agent_name_exists(SystemDB, Nick)
    ->  throw(error(user_already_exists(Nick)))
    ;   true),

    (   organization_name_exists(SystemDB, Nick)
    ->  throw(error(organization_already_exists(Nick)))
    ;   true),

    with_transaction(
        SystemDB,
        (
            crypto_password_hash(Pass,Hash),
            ask(SystemDB,
                (   random_idgen(doc:'User',[Nick^^xsd:string], User_URI),
                    random_idgen(doc:'Role',["founder"^^xsd:string], Role_URI),
                    random_idgen(doc:'Capability',[Nick^^xsd:string], Capability_URI),
                    random_idgen(doc:'Organization',[Nick^^xsd:string], Organization_URI),
                    insert(User_URI, rdf:type, system:'User'),
                    insert(User_URI, system:user_identifier, Email^^xsd:string),
                    insert(User_URI, system:agent_name, Nick^^xsd:string),
                    insert(User_URI, system:user_key_hash, Hash^^xsd:string),
                    insert(User_URI, system:role, Role_URI),
                    insert(Role_URI, rdf:type, system:'Role'),
                    insert(Role_URI, system:capability, Capability_URI),
                    insert(Capability_URI, rdf:type, system:'Capability'),
                    insert(Capability_URI, system:capability_scope, Organization_URI),
                    insert(Capability_URI, system:action, system:create_database),
                    insert(Capability_URI, system:action, system:manage_capabilities),
                    insert(Capability_URI, system:action, system:delete_database),
                    insert(Capability_URI, system:action, system:class_frame),
                    insert(Capability_URI, system:action, system:clone),
                    insert(Capability_URI, system:action, system:fetch),
                    insert(Capability_URI, system:action, system:push),
                    insert(Capability_URI, system:action, system:branch),
                    insert(Capability_URI, system:action, system:rebase),
                    insert(Capability_URI, system:action, system:meta_read_access),
                    insert(Capability_URI, system:action, system:commit_read_access),
                    insert(Capability_URI, system:action, system:instance_read_access),
                    insert(Capability_URI, system:action, system:instance_write_access),
                    insert(Capability_URI, system:action, system:schema_read_access),
                    insert(Capability_URI, system:action, system:schema_write_access),
                    insert(Capability_URI, system:action, system:manage_capabilities),
                    insert(Organization_URI, rdf:type, system:'Organization'),
                    insert(Organization_URI, system:organization_name, Nick^^xsd:string),
                    insert(doc:admin_organization, system:resource_includes, Organization_URI)
                )
               )
        ),
        _Meta_Data
    ).

/*
 * delete_user(+User_ID) is semidet.
 */
delete_user(User_URI) :-
    create_context(system_descriptor{}, Context),
    with_transaction(
        Context,
        ask(Context,
            (   t(User_URI, rdf:type, system:'User'),
                delete_object(User_URI)
            )),
        _).

:- begin_tests(user_management).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).

test(add_user, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    add_user("Gavin", "gavin@terminusdb.com", "here.i.am", URI),

    agent_name_uri(system_descriptor{}, "Gavin", URI),

    once(ask(system_descriptor{},
             t(URI, system:user_identifier, "gavin@terminusdb.com"^^xsd:string))).


test(test_user_ownership, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    Name = "Gavin",
    add_user(Name, "gavin@terminusdb.com", "here.i.am", User_URI),

    create_db_without_schema(Name, "test"),

    open_descriptor(system_descriptor{}, System),

    resolve_absolute_string_descriptor("Gavin/test", Descriptor),

    create_context(Descriptor, Context1),
    Commit_Info = commit_info{ author: "Gavin",
                               message : "Testing" },
    merge_dictionaries(
        query_context{
            commit_info : Commit_Info,
            files : Files,
            system: System,
            update_guard : _Guard0,
            authorization : User_URI
        }, Context1, Context1_0),

    with_transaction(
        Context1_0,
        ask(Context1_0,
            insert(a, b, c)),
        _),

    create_context(Descriptor, Context2),
    Commit_Info = commit_info{ author: "Gavin",
                               message : "Testing" },

    merge_dictionaries(
        query_context{
            commit_info : Commit_Info,
            files : Files,
            system: System,
            update_guard : _Guard1,
            authorization : User_URI
        }, Context2, _Context2_0),

    once(ask(Descriptor, t(a,b,c))).


:- end_tests(user_management).
