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

              user_organizations/3
          ]).

:- use_module(core(util)).
:- use_module(core(api)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(capabilities).
:- use_module(core(document)).

:- use_module(library(crypto)).
:- use_module(library(lists)).
:- use_module(library(plunit)).

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
    do_or_die(organization_name_uri(Context, Name, Organization_Uri),
              error(unknown_organization(Name),_)),
    delete_document(Context, Organization_Uri).

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
    error_on_excluded_organization(Org),

    do_or_die(agent_name_exists(Context, Nick),
              error(unknown_user(Nick), _)),

    (   organization_name_exists(Context, Org)
    ->  throw(error(organization_already_exists(Org),_))
    ;   true),

    user_name_uri(Context, Nick, User_URI),

    add_organization(Context, Org, Organization_URI),

    get_document(Context, User_URI, User_Document),
    Capabilities = (User_Document.capability),
    New_Capabilities = [_{ 'scope' : Organization_URI,
                           'role': ["Role/admin"]}
                        |Capabilities],
    New_User_Document = (User_Document.put(capability, New_Capabilities)),
    replace_document(Context, New_User_Document, _).

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
    error_on_excluded_organization(Org),

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
    auth_action_scope(Askable, User_Uri, '@schema':'Action/manage_capabilities', Resource_Uri).

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
    User_Document = json{
                        '@type': 'User',
                        'name': Nick,
                        'capability': [
                            json{ 'scope': Organization_URI,
                                  'role': [ "Role/admin" ]
                                }
                        ]
                    },
    (   some(Pass) = Pass_Opt
    ->  crypto_password_hash(Pass,Hash),
        atom_string(Hash,Hash_String),
        Final_User_Document = (User_Document.put(key_hash, Hash_String))
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
    throw(error(user_cannot_update_as_they_dont_exist(Name,Document),_)).

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

    do_or_die(
        (   get_dict(password, Document, Password),
            crypto_password_hash(Password,Hash),
            ask(SystemDB,
                (   t(User_Uri, key_hash, Old_Hash^^xsd:string),
                    delete(User_Uri, key_hash, Old_Hash^^xsd:string),
                    insert(User_Uri, key_hash, Hash^^xsd:string)))
        ),
        error(user_update_requires_superuser,_)).

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
        (   t(User_URI, rdf:type, '@schema':'User'),
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
                t(Role_URI, rdf:type, '@system':'Role'),
                t(Capability_URI, scope, Resource_URI)
            ))).

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
    add_user(Name, some("password"), User_URI),

    Password = "my_pass_is_strong",

    Document =
    _{ password : Password },

    update_user(Name, Document),

    once(ask(system_descriptor{},
             (   t(User_URI, name, Name^^xsd:string),
                 t(User_URI, key_hash, Hash^^xsd:string))
            )),

    atom_string(Hash_Atom, Hash),

    crypto_password_hash(Password,Hash_Atom).

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

test(add_user_organization, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    add_user("Kevin", some('password'), _Kevin_URI),

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
         error(user_cannot_update_as_they_dont_exist(gavin,_{agent_name:'Gavin'}),_)
     ]) :-

    User_Document = _{ agent_name : 'Gavin' },

    super_user_authority(Auth),
    update_user_transaction(system_descriptor{}, Auth, gavin, User_Document).

:- end_tests(user_management).
