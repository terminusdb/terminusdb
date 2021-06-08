:- module(user_management,
          [
              add_user/3,
              add_user/4,
              delete_user/1,
              delete_user/2,
              delete_user_transaction/3,
              delete_organization/1,
              delete_organization/2,
              delete_organization_transaction/3,
              update_user/2,
              update_user/3,
              update_user_transaction/4,
              add_organization/2,
              add_organization/3,
              add_organization_transaction/3,

              add_user_organization_transaction/4,

              add_role/6,
              update_role/6,
              update_role_transaction/6,

              get_role/4,
              exists_role/4
          ]).

:- use_module(core(util)).
:- use_module(core(api)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(capabilities).
:- use_module(core(document)).

/** <module> User
 *
 * User management. This is required for testing the capabilities system,
 * but may later be exposed for a user API.
 */

delete_organization_transaction(System_DB, Auth, Name) :-
    do_or_die(is_super_user(Auth, _{}),
              error(delete_organization_requires_superuser,_)),

    Commit_Info = commit_info{ author: "delete_organization_transaction/3",
                               message: "internal system operation"
                             },
    askable_context(System_DB, System_DB, Auth, Commit_Info, Ctx),

    with_transaction(Ctx,
                     delete_organization(Ctx, Name),
                     _).

delete_organization(Name) :-
    create_context(system_descriptor{}, Context),
    with_transaction(
        Context,
        delete_organization(Context,Name),
        _).

delete_organization(Context,Name) :-
    organization_name_uri(Context, Name, Organization_Uri),
    delete_object(Context, Organization_Uri).

add_user_organization_transaction(System_DB, Auth, Nick, Org) :-
    do_or_die(is_super_user(Auth, _{}),
              error(organization_creation_requires_superuser,_)),

    Commit_Info = commit_info{
                      author: "add_user_organization_transaction/3",
                      message: "internal system operation"
                  },
    askable_context(System_DB, System_DB, Auth, Commit_Info, Ctx),

    with_transaction(Ctx,
                     add_user_organization(Ctx, Nick, Org, _Uri),
                     _).

add_user_organization(Context, Nick, Org, Organization_URI) :-

    do_or_die(agent_name_exists(Context, Nick),
              error(agent_name_does_not_exist(Nick))),

    (   organization_name_exists(Context, Org)
    ->  throw(error(organization_already_exists(Org),_))
    ;   true),

    user_name_uri(Context, Nick, User_URI),

    add_organization(Context, Org, Organization_URI),

    get_document(Context, User_URI, User_Document),
    Capabilities = (User_Document.capabilities),
    New_Capabilities = [_{ 'scope' : Organization_URI,
                           'role': ["admin_role"]}
                        |Capabilities],
    New_User_Document = (User_Document.put(capabilities, New_Capabilities)),
    update_document(Context, New_User_Document).

add_organization_transaction(System_DB, Auth, Name) :-
    do_or_die(is_super_user(Auth, _{}),
              error(organization_creation_requires_superuser,_)),

    Commit_Info = commit_info{ author: "add_organization_transaction/3",
                               message: "internal system operation"
                             },
    askable_context(System_DB, System_DB, Auth, Commit_Info, Ctx),

    with_transaction(Ctx,
                     add_organization(Ctx, Name, _),
                     _).

add_organization(Name, Organization_URI) :-
    create_context(system_descriptor{}, Context),
    with_transaction(Context,
                     add_organization(Context, Name, Organization_URI),
                     _).

add_organization(Context, Name, Organization_URI) :-

    (   organization_name_exists(Context, Name)
    ->  throw(error(organization_already_exists(Name),_))
    ;   true),

    insert_document(Context, _{
                                 '@type': 'Organization',
                                 'name': Name
                             }, Organization_URI).


user_managed_resource(_Askable, User_Uri, _Resource_Uri) :-
    is_super_user(User_Uri),
    !.
user_managed_resource(Askable, User_Uri, Resource_Uri) :-
    auth_action_scope(Askable, User_Uri, system:manage_capabilities, Resource_Uri).

add_user(Nick, Pass_Opt, User_URI) :-
    create_context(system_descriptor{},
                   commit_info{
                       author: "add_user/4",
                       message: "internal system operation"
                   }, SystemDB),
    with_transaction(SystemDB,
                     add_user(SystemDB, Nick, Pass_Opt, User_URI),
                     _).

add_user(SystemDB, Nick, Pass_Opt, User_URI) :-

    (   agent_name_exists(SystemDB, Nick)
    ->  throw(error(user_already_exists(Nick), _))
    ;   true),

    add_organization(SystemDB, Nick, Organization_URI),
    User_Document = _{
                        '@type': 'User',
                        'name': Nick,
                        'capability': [
                            { 'scope': Organization_URI,
                              'role': [ "admin_role" ]
                            }
                        ]
                    },
    (   some(Pass) = Pass_Opt
    ->  crypto_password_hash(Pass,Hash),
        Final_User_Document = (User_Document.put(key_hash, Hash))
    ;   Final_User_Document = User_Document),

    insert_document(SystemDB, Final_User_Document, User_URI).

update_user_transaction(SystemDB, Auth, Name, Document) :-
    do_or_die(is_super_user(Auth, _{}),
              error(user_update_requires_superuser,_)),

    Commit_Info = commit_info{ author: "user_handler/2",
                               message: "internal system operation"
                             },

    askable_context(SystemDB, SystemDB, Auth, Commit_Info, Ctx),
    with_transaction(Ctx,
                     update_user(Ctx, Name, Document),
                     _),
    !.
update_user_transaction(_SystemDB, _Auth, Name, Document) :-
    throw(error(user_update_failed_without_error(Name,Document),_)).

update_user(Name, Document) :-
    create_context(system_descriptor{},
                   commit_info{
                       author: "update_user/2",
                       message: "internal system operation"
                   }, SystemDB),
    with_transaction(
        SystemDB,
        update_user(SystemDB, Name, Document),
        _).

update_user(SystemDB, Name, Document) :-
    % Agent already exists so update
    user_name_uri(SystemDB, Name, User_Uri),
    !,

    (   get_dict(password, Document, Password)
    ->  crypto_password_hash(Password,Hash),
        ask(SystemDB,
            (   t(User_Uri, system:user_key_hash, Old_Hash^^xsd:string),
                delete(User_Uri, system:user_key_hash, Old_Hash^^xsd:string),
                insert(User_Uri, system:user_key_hash, Hash^^xsd:string)))
    ;   true).
update_user(SystemDB, Name, Document) :-
    do_or_die((_{ user_identifier : ID,
                  agent_name : Name,
                  comment : Comment} :< Document),
              error(malformed_update_user_document(Document,[user_identifier,
                                                             agent_name,
                                                             comment
                                                            ]),
                    _)),

    (   _{ password : Password } :< Document
    ->  Password_Option = some(Password)
    ;   Password_Option = none),

    add_user(SystemDB, Name, ID, Comment, Password_Option, _).

delete_user_transaction(SystemDB, Auth, Name) :-
    do_or_die(is_super_user(Auth, _{}),
              error(user_update_requires_superuser,_)),

    Commit_Info = commit_info{ author: "delete_user_transaction/3",
                               message: "internal system operation"
                             },

    askable_context(SystemDB, SystemDB, Auth, Commit_Info, Ctx),
    with_transaction(Ctx,
                     delete_user(Ctx, Name),
                     _),
    !.
delete_user_transaction(_SystemDB, _Auth, Name) :-
    throw(error(user_delete_failed_without_error(Name),_)).


/*
 * delete_user(+User_ID) is semidet.
 */
delete_user(User_Name) :-
    create_context(system_descriptor{}, Context),
    with_transaction(
        Context,
        delete_user(Context, User_Name),
        _).

delete_user(Context, User_Name) :-
    user_name_uri(Context, User_Name, User_URI),
    ask(Context,
        (   t(User_URI, rdf:type, system:'User'),
            delete_object(User_URI)
        )).

exists_role(Askable, User, Organization, Resource_Name_Option) :-
    user_name_uri(Askable, User, User_URI),

    (   Resource_Name_Option = none
    ->  organization_name_uri(Askable, Organization, Resource_URI)
    ;   Resource_Name_Option = some(Resource_Name),
        organization_database_name_uri(Askable, Organization, Resource_Name, Resource_URI)),

    once(
        ask(Askable,
            (   t(User_URI, capability, Capability_URI),
                t(Capability_URI, role, Role_URI),
                t(Role_URI, rdf:type, system:'Role'),
                t(Capability_URI, scope, Resource_URI)
            ))).

add_role(Context, Auth_ID, User, Organization, Resource_Name_Option, Actions) :-
    user_name_uri(Context, User, User_URI),

    (   Resource_Name_Option = none
    ->  organization_name_uri(Context, Organization, Resource_URI)
    ;   Resource_Name_Option = some(Resource_Name),
        organization_database_name_uri(Context, Organization, Resource_Name, Resource_URI)),

    do_or_die(user_managed_resource(Context, Auth_ID, Resource_URI),
              error(no_manage_capability(Organization,Resource_Name), _)),

    forall(
        ask(Context,
            (   random_idgen(doc:'Role',["founder"^^xsd:string], Role_URI),
                random_idgen(doc:'Capability',[User^^xsd:string], Capability_URI),
                insert(User_URI, system:role, Role_URI),
                insert(Role_URI, rdf:type, system:'Role'),
                insert(Role_URI, system:capability, Capability_URI),
                insert(Capability_URI, system:direct_capability_scope, Resource_URI),
                insert(Capability_URI, rdf:type, system:'Capability'),
                member(Action, Actions),
                insert(Capability_URI, system:action, Action)
            )),
        true).


get_role(Askable, Auth_ID, Document, Response) :-
    (   get_dict(agent_name, Document, Agent)
    ->  Agent_Var = Agent^^xsd:string
    ;   Agent_Var = v('Agent')),

    (   get_dict(database_name, Document, Database_Name)
    ->  Database_Name_Var = Database_Name^^xsd:string
    ;   Database_Name_Var = v('Database_Name')),

    (   get_dict(organization_name, Document, Organization_Name)
    ->  Organization_Var = Organization_Name^^xsd:string
    ;   Organization_Var = v('Organization')),

    create_context(Askable, Query_Context),

    Query = (select(
                 [v('Owner_Role_Obj'),
                  v('Control_Role_ID'),
                  v('Database_ID'),
                  v('Agent'),
                  v('Organization')],
                 (   t(Auth_ID, system:role, v('Control_Role_ID')),
                     t(v('Control_Role_ID'), system:capability, v('Control_Capability_ID')),
                     t(v('Control_Capability_ID'), system:capability_scope, v('Organization_ID')),
                     t(v('Control_Capability_ID'), system:action, system:manage_capabilities),
                     t(v('Organization_ID'), system:organization_name, Organization_Var),
                     t(v('Organization_ID'), system:resource_includes, v('Database_ID')),
                     t(v('Database_ID'), rdf:type, system:'Database'),
                     t(v('Database_ID'), system:resource_name, Database_Name_Var),
                     t(v('Capability_ID'), system:capability_scope, v('Database_ID')),
                     t(v('Owner_Role_ID'), system:capability, v('Capability_ID')),
                     t(v('Owner_ID'), system:role, v('Owner_Role_ID')),
                     t(v('Owner_ID'), system:agent_name, Agent_Var),
                     read_object(v('Owner_Role_ID'), 3, v('Owner_Role_Obj'))
                 ))),
    run_context_ast_jsonld_response(Query_Context,Query,Response).

update_role_transaction(System_DB, Auth, Agents, Organization, Database_Name, Actions) :-
    Commit_Info = commit_info{ author: "add_organization_transaction/3",
                               message: "internal system operation"
                             },
    askable_context(System_DB, System_DB, Auth, Commit_Info, Ctx),

    % NOTE: This should be replaced by appropriate document API expansion globally
    default_prefixes(Prefixes),
    maplist({Prefixes}/[Action,Action_URI]>>
            prefix_expand(Action, Prefixes, Action_URI),
            Actions,
            Action_Uris),

    with_transaction(Ctx,
                     update_role(System_DB, Auth, Agents, Organization, Database_Name, Action_Uris),
                     _).


update_role(Context, Auth_ID, Users, Organization, Resource_Name_Option, Actions) :-
    exists_role(Context, User, Organization, Resource_Name_Option),
    !,

    findall(User_URI,
            (   member(User, Users),
                user_name_uri(Context, User, User_URI)),
            User_URIs),

    (   Resource_Name_Option = none
    ->  organization_name_uri(Context, Organization, Resource_URI)
    ;   Resource_Name_Option = some(Resource_Name),
        organization_database_name_uri(Context, Organization, Resource_Name, Resource_URI)),

    do_or_die(user_managed_resource(Context, Auth_ID, Resource_URI),
              error(no_manage_capability(Organization,Resource_Name), _)),

    forall(ask(Context,
               (   member(User_URI, User_URIs),
                   t(User_URI, system:role, Role_URI),
                   t(Role_URI, system:capability, Capability_URI),
                   t(Capability_URI, system:direct_capability_scope, Resource_URI),
                   t(Capability_URI, rdf:type, system:'Capability'),
                   t(Capability_URI, system:action, Action),
                   delete(Capability_URI, system:action, Action)
               )),
           true),

    forall(ask(Context,
               (   member(User_URI, User_URIs),
                   t(User_URI, system:role, Role_URI),
                   t(Role_URI, system:capability, Capability_URI),
                   t(Capability_URI, system:direct_capability_scope, Resource_URI),
                   t(Capability_URI, rdf:type, system:'Capability'),
                   member(Action, Actions),
                   insert(Capability_URI, system:action, Action)
               )),
           true).
update_role(Context, Auth_ID, Users, Organization, Resource_Name_Option, Actions) :-
    forall(member(User, Users),
           add_role(Context, Auth_ID, User, Organization, Resource_Name_Option, Actions)).

delete_role(Context, Auth_ID, User, Organization, Resource_Name_Option) :-
    exists_role(Context, User, Organization, Resource_Name_Option),
    !,

    user_name_uri(Context, User, User_URI),

    (   Resource_Name_Option = none
    ->  organization_name_uri(Context, Organization, Resource_URI)
    ;   Resource_Name_Option = some(Resource_Name),
        organization_database_name_uri(Context, Organization, Resource_Name, Resource_URI)),

    do_or_die(user_managed_resource(Context, Auth_ID, Resource_URI),
              error(no_manage_capability(Organization,Resource_Name), _)),

    forall(ask(Context,
               (
                   t(User_URI, system:role, Role_URI),
                   t(Role_URI, system:capability, Capability_URI),
                   t(Capability_URI, system:direct_capability_scope, Resource_URI),
                   t(Capability_URI, rdf:type, system:'Capability'),
                   t(Capability_URI, system:action, Action),

                   delete(Role_URI, system:capability, Capability_URI),
                   delete(Capability_URI, system:direct_capability_scope, Resource_URI),
                   delete(Capability_URI, rdf:type, system:'Capability'),
                   delete(Capability_URI, system:action, Action),
                   delete(Capability_URI, system:action, Action)
               )),
           true).


:- begin_tests(user_management).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).

test(add_user, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    add_user("Gavin", some('password'), URI),

    agent_name_uri(system_descriptor{}, "Gavin", URI),

    once(ask(system_descriptor{},
             t(URI, '@schema':name, "Gavin"^^xsd:string))).


test(user_ownership, [
         setup(setup_temp_store(State)),
         cleanup((* print_all_triples(system_descriptor{}),
                  teardown_temp_store(State)))
     ]) :-

    Name = "Gavin",
    add_user(Name, some('password'), User_URI),

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

test(user_update, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    Name = "Gavin",
    add_user(Name, "gavin@datachemist.com", "here.i.am", some("password"), User_URI),

    Agent_Name = "Bill",
    Password = "my_pass_is_strong",
    Comment = "Never again bill",
    Identifier = "gavin@terminusdb.com",

    Document =
    _{ agent_name : Agent_Name,
       password : Password,
       user_identifier : Identifier,
       comment : Comment },

    update_user(Name, Document),

    %findall(P-Q, ask(system_descriptor{}, t(User_URI, P, Q)), PQs),
    %writeq(PQs),
    %nl,
    once(ask(system_descriptor{},
             (   t(User_URI, system:agent_name, Agent_Name^^xsd:string),
                 t(User_URI, system:user_key_hash, Hash^^xsd:string),
                 t(User_URI, rdfs:comment, Comment@en),
                 t(User_URI, system:user_identifier, Identifier^^xsd:string)),
             [compress_prefixes(false)]
            )),
    atom_string(Hash_Atom, Hash),
    crypto_password_hash(Password, Hash_Atom).

test(organization_creation, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Admin),
    add_organization_transaction(System_DB, Admin,"testing_organization"),

    organization_name_exists(system_descriptor{}, "testing_organization").

test(organization_delete, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    add_organization("testing_organization", _),

    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Admin),

    delete_organization_transaction(System_DB, Admin, "testing_organization"),

    \+ organization_name_exists(system_descriptor{}, "testing_organization").


test(get_roles, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    Document = _{},
    Name = "Gavin",
    add_user(Name, "gavin@terminusdb.com", "here.i.am", some('password'), User_URI),

    create_db_without_schema(Name, "test"),

    get_role(system_descriptor{}, User_URI, Document, Response),
    Bindings = (Response.bindings),

    forall((member(Binding_Set, Bindings),
            (Name = Binding_Set.'Organization'.'@value')
           ),
           member(Name, ["Gavin"])
          ).

test(get_loads_of_roles, [
         blocked('takes too long, adds nothing'),
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    Document = _{},
    add_user("Gavin", "gavin@terminusdb.com", "here.i.am", some('password'), User_URI),

    create_db_without_schema("Gavin", "test1"),
    create_db_without_schema("Gavin", "test2"),
    create_db_without_schema("Gavin", "test3"),
    create_db_without_schema("Gavin", "test4"),
    create_db_without_schema("Gavin", "test6"),
    create_db_without_schema("Gavin", "test7"),
    create_db_without_schema("Gavin", "test8"),
    create_db_without_schema("Gavin", "test9"),
    create_db_without_schema("Gavin", "test10"),
    create_db_without_schema("Gavin", "test12"),
    create_db_without_schema("Gavin", "test13"),
    create_db_without_schema("Gavin", "test14"),
    create_db_without_schema("Gavin", "test15"),
    create_db_without_schema("Gavin", "test16"),
    create_db_without_schema("Gavin", "test17"),
    create_db_without_schema("Gavin", "test18"),
    create_db_without_schema("Gavin", "test19"),
    create_db_without_schema("Gavin", "test20"),
    create_db_without_schema("Gavin", "test21"),
    create_db_without_schema("Gavin", "test22"),
    create_db_without_schema("Gavin", "test23"),
    create_db_without_schema("Gavin", "test24"),
    create_db_without_schema("Gavin", "test25"),
    create_db_without_schema("Gavin", "test26"),
    create_db_without_schema("Gavin", "test27"),
    create_db_without_schema("Gavin", "test28"),
    create_db_without_schema("Gavin", "test29"),
    create_db_without_schema("Gavin", "test30"),

    get_role(system_descriptor{}, User_URI, Document, Response),

    Bindings = (Response.bindings),

    forall((member(Binding_Set, Bindings),
            (Name = Binding_Set.'Organization'.'@value')
           ),
           member(Name, ["Gavin"])
          ).

test(add_role, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-


    create_db_without_schema("admin", "flurp"),
    create_context(system_descriptor{}, commit_info{ message : "http://foo", author : "bar"}, Context),

    with_transaction(
        Context,
        add_role(Context,
                 doc:admin,
                 "admin",
                 "admin",
                 some("flurp"),
                 [system:inference_write_access]
                ),
        _),

    Document =
    _{  agent_name : "admin"
    },
    get_role(system_descriptor{}, doc:admin, Document, Response),
    Bindings = (Response.bindings),

    once((member(Elt,Bindings),
          (Elt.'Owner_Role_Obj'.'system:capability') = Cap,
          (Cap.'system:capability_scope'.'system:resource_name'.'@value') = "flurp",
          (Cap.'system:action'.'@id') = 'system:inference_write_access')).


test(update_role, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-


    create_db_without_schema("admin", "flurp"),
    create_context(system_descriptor{}, commit_info{ message : "http://foo", author : "bar"}, Context),

    with_transaction(
        Context,
        add_role(Context,
                 doc:admin,
                 "admin",
                 "admin",
                 some("flurp"),
                 [system:inference_write_access]
                ),
        _),

    create_context(system_descriptor{}, commit_info{ message : "http://foo", author : "bar"}, Context2),

    with_transaction(
        Context2,
        update_role(Context2,
                    doc:admin,
                    ["admin"],
                    "admin",
                    some("flurp"),
                    [system:inference_read_access]
                   ),
        _),

    Document =
    _{  agent_name : "admin"
    },

    get_role(system_descriptor{}, doc:admin, Document, Response),
    Bindings = (Response.bindings),

    once((member(Elt,Bindings),
          (Elt.'Owner_Role_Obj'.'system:capability') = Cap,
          (Cap.'system:capability_scope'.'system:resource_name'.'@value') = "flurp",
          (Cap.'system:action'.'@id') = 'system:inference_read_access')),

    \+ (member(Elt,Bindings),
        (Elt.'Owner_Role_Obj'.'system:capability') = Cap,
        (Cap.'system:capability_scope'.'system:resource_name'.'@value') = "flurp",
        (Cap.'system:action'.'@id') = 'system:inference_write_access').


test(update_role_unexpanded_keys, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    create_db_without_schema("admin", "flurp"),
    create_context(system_descriptor{}, commit_info{ message : "http://foo", author : "bar"}, Context),

    with_transaction(
        Context,
        add_role(Context,
                 doc:admin,
                 "admin",
                 "admin",
                 some("flurp"),
                 [system:inference_write_access]
                ),
        _),

    create_context(system_descriptor{}, commit_info{ message : "http://foo", author : "bar"}, Context2),

    update_role_transaction(Context2,
                            doc:admin,
                            ["admin"],
                            "admin",
                            some("flurp"),
                            ["system:inference_read_access",
                             "system:push"]
                           ),
    % No error is sufficient
    true.

test(add_role_no_capability, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State)),
         error(no_manage_capability("admin","flurp"), _)
     ]) :-

    Name = "Gavin",
    add_user(Name, "gavin@terminusdb.com", "here.i.am", some('password'), User_URI),

    create_db_without_schema("admin", "flurp"),
    create_context(system_descriptor{},
                   commit_info{ message : "http://foo", author : "bar"},
                   Context),

    with_transaction(
        Context,
        add_role(Context,
                 User_URI,
                 "admin",
                 "admin",
                 some("flurp"),
                 [system:inference_write_access]
                ),
        _).

test(update_role_no_capability, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State)),
         error(no_manage_capability("Kevin","flurp"),_)
     ]) :-

    add_user("Kevin", "kevin@terminusdb.com", "here.i.am", some('password'), _Kevin_URI),
    add_user("Gavin", "gavin@terminusdb.com", "here.i.am", some('password'), Gavin_URI),

    create_db_without_schema("Kevin", "flurp"),
    create_context(system_descriptor{}, commit_info{ message : "http://foo", author : "bar"}, Context),

    with_transaction(
        Context,
        update_role(Context,
                 Gavin_URI,
                 ["Kevin"],
                 "Kevin",
                 some("flurp"),
                 [system:inference_write_access]
                ),
        _).

test(update_organization_role, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-


    create_db_without_schema("admin", "flurp"),
    create_context(system_descriptor{}, commit_info{ message : "http://foo", author : "bar"}, Context),

    with_transaction(
        Context,
        add_role(Context,
                 doc:admin,
                 "admin",
                 "admin",
                 none,
                 [system:manage_capabilities]
                ),
        _),

    create_context(system_descriptor{}, commit_info{ message : "http://foo", author : "bar"}, Context2),

    with_transaction(
        Context2,
        update_role(Context2,
                    doc:admin,
                    ["admin"],
                    "admin",
                    some("flurp"),
                    [system:manage_capabilities,system:inference_read_access]
                   ),
        _),

    Document =
    _{  agent_name : "admin"
    },

    get_role(system_descriptor{}, doc:admin, Document, Response),
    Bindings = (Response.bindings),

    once((member(Elt,Bindings),
          (Elt.'Owner_Role_Obj'.'system:capability') = Cap,
          (Cap.'system:capability_scope'.'system:resource_name'.'@value') = "flurp",
          (Cap.'system:action') =
          [_{'@id':'system:inference_read_access',
             '@type':'system:DBAction'},
           _{'@id':'system:manage_capabilities','@type':'system:DBAction'}])).

test(add_user_organization, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    add_user("Kevin", "kevin@terminusdb.com", "here.i.am", some('password'), _Kevin_URI),

    create_db_without_schema("Kevin", "flurp"),

    super_user_authority(Admin),
    add_user_organization_transaction(
        system_descriptor{},
        Admin,
        "Kevin",
        "Fantasy"),

    agent_name_uri(system_descriptor{}, "Kevin", User_Uri),
    organization_name_uri(system_descriptor{}, "Fantasy", Organization_Uri),
    once(user_managed_resource(system_descriptor{}, User_Uri, Organization_Uri)).


test(bad_add_user_document, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State)),
         error(malformed_update_user_document(_660{agent_name:'Gavin',comment:'some comment',user_piedentifier:gavin},[user_identifier,agent_name,comment]),_)
     ]) :-

    User_Document = _{ user_piedentifier : gavin,
                       agent_name : 'Gavin',
                       comment : 'some comment'},

    super_user_authority(Auth),
    update_user_transaction(system_descriptor{}, Auth, gavin, User_Document).

:- end_tests(user_management).
