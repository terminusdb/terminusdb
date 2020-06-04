:- module(user_management,
          [
              add_user/4,
              agent_name_uri/3,
              agent_name_exists/2,
              make_user_own_database/2,
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

/*
 * delete_user(+User_ID) is semidet.
 */
delete_user(User_URI) :-
    create_context(terminus_descriptor{}, Context),
    with_transaction(
        Context,
        ask(Context,
            (   t(User_URI, rdf:type, terminus:'User'),
                delete_object(User_URI)
            )),
        _).

make_user_own_database(User_Name, Database_Name) :-
    create_context(terminus_descriptor{},
                   commit_info{ author: "Terminus System",
                                message: "Creating TerminusDB"},
                   TerminusDB),
    with_transaction(
        TerminusDB,
        (
            username_user_id(TerminusDB, User_Name, User_ID),
            user_id_auth_id(TerminusDB, User_ID, Auth_ID),
            % writeq(User_ID),nl,
            % findall(DB-Name,
            %         ask(TerminusDB,
            %             (   t(DB, rdf:type, terminus:'Database'),
            %                 t(DB, terminus:resource_name, Name^^(xsd:string))
            %             )),
            %         Names),
            % writeq(Names),

            ask(TerminusDB,
                (   t(DB_URI, rdf:type, terminus:'Database'),
                    t(DB_URI, terminus:resource_name, Database_Name^^(xsd:string))
                )),

            % writeq(DB_URI),nl,

            ask(TerminusDB,
                (
                    idgen(doc:'Access', [
                              delete_database^^(xsd:string),
                              class_frame^^(xsd:string),
                              clone^^(xsd:string),
                              fetch^^(xsd:string),
                              push^^(xsd:string),
                              branch^^(xsd:string),
                              rebase^^(xsd:string),
                              meta_read_access^^(xsd:string),
                              commit_read_access^^(xsd:string),
                              instance_read_access^^(xsd:string),
                              instance_write_access^^(xsd:string),
                              schema_read_access^^(xsd:string),
                              schema_write_access^^(xsd:string),
                              manage^^(xsd:string),
                              Database_Name^^(xsd:string)], Access_URI),
                    insert(Auth_ID, terminus:access, Access_URI),
                    insert(Access_URI, rdf:type, terminus:'Access'),
                    insert(Access_URI, terminus:action, terminus:delete_database),
                    insert(Access_URI, terminus:action, terminus:class_frame),
                    insert(Access_URI, terminus:action, terminus:clone),
                    insert(Access_URI, terminus:action, terminus:fetch),
                    insert(Access_URI, terminus:action, terminus:push),
                    insert(Access_URI, terminus:action, terminus:branch),
                    insert(Access_URI, terminus:action, terminus:rebase),
                    insert(Access_URI, terminus:action, terminus:meta_read_access),
                    insert(Access_URI, terminus:action, terminus:commit_read_access),
                    insert(Access_URI, terminus:action, terminus:instance_read_access),
                    insert(Access_URI, terminus:action, terminus:instance_write_access),
                    insert(Access_URI, terminus:action, terminus:schema_read_access),
                    insert(Access_URI, terminus:action, terminus:schema_write_access),
                    insert(Access_URI, terminus:action, terminus:manage),
                    insert(Access_URI, terminus:authority_scope, DB_URI)
                ))
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


test(test_user_ownership, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    Name = "Gavin",
    add_user(Name, "gavin@terminusdb.com", "here.i.am", User_URI),
    user_database_name(Name, 'test', Database_Name),

    create_db_without_schema(Database_Name, 'test', 'a test'),

    make_user_own_database(Name, Database_Name),

    open_descriptor(terminus_descriptor{}, TerminusDB),
    once(user_id_auth_id(TerminusDB, User_URI, Auth_ID)),
    resolve_absolute_string_descriptor("Gavin/test", Descriptor),

    create_context(Descriptor, Context1),
    Commit_Info = commit_info{ author: "Gavin",
                               message : "Testing" },
    merge_dictionaries(
        query_context{
            commit_info : Commit_Info,
            files : Files,
            terminus: TerminusDB,
            update_guard : _Guard0,
            authorization : Auth_ID
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
            terminus: TerminusDB,
            update_guard : _Guard1,
            authorization : Auth_ID
        }, Context2, _Context2_0),


    once(ask(Descriptor, t(a,b,c))).


test(test_no_user_ownership, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    true.

:- end_tests(user_management).
