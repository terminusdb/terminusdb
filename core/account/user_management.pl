:- module(user_management,
          [
              add_user/5,
              add_user/6,
              agent_name_uri/3,
              agent_name_exists/2,
              delete_user/1,
              delete_user/2,
              delete_organization/1,
              delete_organization/2,
              update_user/2,
              update_user/3,
              update_organization/2,
              update_organization/3,
              add_organization/2,
              add_organization/3,

              add_role/6,
              update_role/6,
              get_role/4,
              exists_role/4
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

delete_organization(Name) :-
    create_context(system_descriptor{}, Context),
    with_transaction(
        Context,
        delete_organization(Context,Name),
        _).

delete_organization(Context,Name) :-
    organization_name_uri(Context, Name, Organization_Uri),
    ask(Context,
        (   t(Organization_Uri, rdf:type, system:'Organization'),
            delete_object(Organization_Uri)
        )).

add_organization(Name, Organization_URI) :-
    create_context(system_descriptor{}, Context),
    with_transaction(Context,
                     add_organization(Context, Name, Organization_URI),
                     _).

add_organization(Context, Name, Organization_URI) :-

    (   organization_name_exists(Context, Name)
    ->  throw(error(organization_already_exists(Name)))
    ;   true),

    ask(Context,
        (
            random_idgen(doc:'Organization',[Name^^xsd:string], Organization_URI),
            insert(Organization_URI, rdf:type, system:'Organization'),
            insert(Organization_URI, system:organization_name, Name^^xsd:string),
            insert(doc:admin_organization, system:resource_includes, Organization_URI)
        ),
       [compress_prefixes(false)]).

user_managed_resource(Askable, User_Uri, Resource_Uri) :-
    ask(Askable,
        (   t(User_Uri, system:role, Role_Uri),
            t(Role_Uri, rdf:type, system:'Role'),
            t(Role_Uri, system:capability, Capability_Uri),
            t(Capability_Uri, system:capability_scope, Resource_Uri),
            t(Capability_Uri, rdf:type, system:'Capability'),
            t(Capability_Uri, system:action, system:manage_capabilities)
        )).

update_organization(Name, New_Name) :-
    create_context(system_descriptor{}, Context),
    with_transaction(
        Context,
        update_organization(Context,Name,New_Name),
        _).

update_organization(Context, Name, New_Name) :-
    ask(Context,
        (   t(Organization_URI, system:organization_name, Name^^xsd:string),
            delete(Organization_URI, system:organization_name, Name^^xsd:string),
            insert(Organization_URI, system:organization_name, New_Name^^xsd:string)
        ),
       [compress_prefixes(false)]).


add_user(Nick, Email, Comment, Pass_Opt, User_URI) :-
    create_context(system_descriptor{},
                   commit_info{
                       author: "add_user/4",
                       message: "internal system operation"
                   }, SystemDB),
    with_transaction(SystemDB,
                     add_user(SystemDB, Nick, Email, Comment, Pass_Opt, User_URI),
                     _).

add_user(SystemDB, Nick, Email, Comment, Pass_Opt, User_URI) :-

    (   agent_name_exists(SystemDB, Nick)
    ->  throw(error(user_already_exists(Nick), _))
    ;   true),

    add_organization(SystemDB, Nick, Organization_URI),
    ask(SystemDB,
        (   random_idgen(doc:'User',[Nick^^xsd:string], User_URI),
            random_idgen(doc:'Role',["founder"^^xsd:string], Role_URI),
            random_idgen(doc:'Capability',[Nick^^xsd:string], Capability_URI),
            insert(User_URI, rdf:type, system:'User'),
            insert(User_URI, rdfs:comment, Comment@en),
            insert(User_URI, system:agent_name, Nick^^xsd:string),
            insert(User_URI, system:user_identifier, Email^^xsd:string),
            insert(User_URI, system:role, Role_URI),
            insert(Role_URI, rdf:type, system:'Role'),
            insert(Role_URI, system:capability, Capability_URI),
            insert(Capability_URI, rdf:type, system:'Capability'),
            insert(Capability_URI, system:direct_capability_scope, Organization_URI),
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
            insert(Capability_URI, system:action, system:manage_capabilities)
        ),
        [compress_prefixes(false)]
       ),

    (   Pass_Opt = some(Pass)
    ->  crypto_password_hash(Pass,Hash),
        ask(SystemDB,
            insert(User_URI, system:user_key_hash, Hash^^xsd:string)
           )
    ;   true).

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
    agent_name_uri(SystemDB, Name, User_Uri),
    !,

    (   get_dict(user_identifier, Document, Identifier)
    ->  ask(SystemDB,
            (   t(User_Uri, system:user_identifier, Old_ID^^xsd:string),
                delete(User_Uri, system:user_identifier, Old_ID^^xsd:string),
                insert(User_Uri, system:user_identifier, Identifier^^xsd:string)))
    ;   true),

    (   get_dict(agent_name, Document, Agent_Name)
    ->  ask(SystemDB,
            (   t(User_Uri, system:agent_name, Old_Name^^xsd:string),
                delete(User_Uri, system:agent_name, Old_Name^^xsd:string),
                insert(User_Uri, system:agent_name, Agent_Name^^xsd:string)))
    ;   true),

    (   get_dict(comment, Document, Comment)
    ->  ask(SystemDB,
            (   t(User_Uri, rdfs:comment, Old_Comment@en),
                delete(User_Uri, rdfs:comment, Old_Comment@en),
                insert(User_Uri, rdfs:comment, Comment@en)))
    ;   true),

    (   get_dict(password, Document, Password)
    ->  crypto_password_hash(Password,Hash),
        ask(SystemDB,
            (   t(User_Uri, system:user_key_hash, Old_Hash^^xsd:string),
                delete(User_Uri, system:user_key_hash, Old_Hash^^xsd:string),
                insert(User_Uri, system:user_key_hash, Hash^^xsd:string)))
    ;   true).
update_user(SystemDB, Name, Document) :-
    _{ user_identifier : ID,
       agent_name : Name,
       comment : Comment,
       password : Pass } :< Document,
    add_user(SystemDB, Name, ID, Comment, Pass, _).

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

exists_role(Askable, User, Organization, Resource_Name) :-
    user_name_uri(Askable, User, User_URI),
    organization_database_name_uri(Askable, Organization, Resource_Name, Resource_URI),
    once(
        ask(Askable,
            (   t(User_URI, system:role, Role_URI),
                t(Role_URI, rdf:type, system:'Role'),
                t(Role_URI, system:capability, Capability_URI),
                t(Capability_URI, system:direct_capability_scope, Resource_URI)
            ))).

add_role(Context, Auth_ID, User, Organization, Resource_Name, Actions) :-
    user_name_uri(Context, User, User_URI),
    organization_database_name_uri(Context, Organization, Resource_Name, Resource_URI),

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
                 (   (v('Auth_ID') = (Auth_ID)),
                     t(v('Auth_ID'), system:role, v('Control_Role_ID')),
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

update_role(Context, Auth_ID, Users, Organization, Resource_Name, Actions) :-
    exists_role(Context, User, Organization, Resource_Name),
    !,

    findall(User_URI,
            (   member(User, Users),
                user_name_uri(Context, User, User_URI)),
            User_URIs),

    organization_database_name_uri(Context, Organization, Resource_Name, Resource_URI),

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
update_role(Context, Auth_ID, Users, Organization, Resource_Name, Actions) :-
    forall(member(User, Users),
           add_role(Context, Auth_ID, User, Organization, Resource_Name, Actions)).

delete_role(Context, Auth_ID, User, Organization, Resource_Name) :-
    exists_role(Context, User, Organization, Resource_Name),
    !,

    user_name_uri(Context, User, User_URI),
    organization_database_name_uri(Context, Organization, Resource_Name, Resource_URI),

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

    add_user("Gavin", "gavin@terminusdb.com", "here.i.am",  some('password'), URI),

    agent_name_uri(system_descriptor{}, "Gavin", URI),

    once(ask(system_descriptor{},
             t(URI, system:user_identifier, "gavin@terminusdb.com"^^xsd:string))).


test(user_ownership, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    Name = "Gavin",
    add_user(Name, "gavin@terminusdb.com", "here.i.am", some('password'), User_URI),

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

    add_organization("testing_organization", _),

    organization_name_exists(system_descriptor{}, "testing_organization").

test(organization_update, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    add_organization("testing_organization", _),

    update_organization("testing_organization", "new_organization"),

    organization_name_exists(system_descriptor{}, "new_organization").

test(organization_delete, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    add_organization("testing_organization", _),

    delete_organization("testing_organization"),

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
                 "flurp",
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
                 "flurp",
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
                    "flurp",
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
                 "flurp",
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
                 "flurp",
                 [system:inference_write_access]
                ),
        _).


:- end_tests(user_management).
