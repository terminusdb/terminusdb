:- module(capabilities,[
              username_user_id/3,
              user_key_user_id/4,
              username_auth/3,
              get_user/3,
              auth_action_scope/4,
              assert_auth_action_scope/4,
              assert_read_access/1,
              assert_read_access/2,
              assert_read_access/3,
              assert_read_access/4,
              assert_write_access/1,
              assert_write_access/2,
              assert_write_access/3,
              assert_write_access/4,
              authorisation_object/3,
              user_accessible_database/3,
              user_accessible_database/5,
              check_descriptor_auth/4,
              is_super_user/1,
              is_super_user/2,
              resolve_descriptor_auth/6
          ]).

/** <module> Capabilities
 *
 * Capability system for access control.
 *
 * We will eventually integrate a rich ontological model which
 * enables fine grained permission access to the database.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(document)).

:- use_module(config(terminus_config),[]).

:- use_module(library(crypto)).
:- use_module(library(lists)).
:- use_module(library(plunit)).

/**
 * username_user_id(+DB, +Username, -User_ID) is semidet.
 *
 * Username user association - goes only one way
 */
username_user_id(DB, Username, User_ID) :-
    ask(DB,
        (
            isa(User_ID,'User'),
            t(User_ID, name, Username^^xsd:string)
        ),
        [compress_prefixes(false)]
       ).


/**
 * key_user(+DB, +Username, +Key, -User_ID) is semidet.
 *
 * Key user association - goes only one way
 */
user_key_user_id(DB, Username, Key, User_ID) :-
    coerce_literal_string(Key, K),
    coerce_literal_string(Username, U),
    ask(DB,
        (
            t(User_ID, name, U^^xsd:string),
            isa(User_ID, 'User'),
            t(User_ID, key_hash, Hash^^xsd:string)
        ),
        [compress_prefixes(false)]
       ),
    atom_string(Hash_Atom, Hash),
    crypto_password_hash(K, Hash_Atom).

/**
 * get_user(+User_ID, -User) is det.
 *
 * Gets back a full user object which includes all authorities
 */
get_user(Database, User_ID, User) :-
    get_document(Database,User_ID,User).

/**
 * user_key_auth(DB, Key, Auth_URI) is det.
 *
 * Give a capabilities JSON object corresponding to the capabilities
 * of the key supplied by searching the core permissions database.
 */
user_key_auth(DB, Username, Key, User_ID) :-
    user_key_user_id(DB, Username, Key, User_ID).

/**
 * username_auth(DB, Key,Auth) is det.
 *
 * Give a capabilities JSON object corresponding to the capabilities
 * of the username supplied by searching the core permissions database.
 */
username_auth(DB, Username, User_ID) :-
    username_user_id(DB, Username, User_ID).

/*
 * auth_action_scope(Auth,Action,Scope) is nondet.
 *
 * Does Auth object have capability Action on scope Scope.
 *
 * This needs to implement some of the logical character of scope subsumption.
 */
auth_action_scope(_, Auth, _, _) :-
    is_super_user(Auth),
    !.
auth_action_scope(DB, Auth, Action, Scope_Iri) :-
    ground(Auth),
    ask(DB,
        (
            t(Auth, capability, Capability),
            t(Capability, role, Role),
            t(Role, action, Action),
            t(Capability, scope, Intermediate_Scope_Iri),
            path(Intermediate_Scope_Iri, (star((p(child);p(database)))), Scope_Iri, _)
        )
       ).
auth_action_scope(DB, _Auth, Action, Scope_Iri) :-
    % For access to everything that the anonymous user has...
    % This does not allow transitive scoping to work as written, but relies on the
    % API setting up a capability for each database marked public
    ask(DB,
        (   t(Capability, scope, Scope_Iri),
            t('User/anonymous', capability, Capability),
            t(Capability, role, Role),
            t(Role, action, Action)
        )
       ).

/*
 * resource_user_path(Askable,Resource,User,Path) is nondet.
 *
 * resource_includes+
 * Note: Get the full path to a resource.

resource_user_path(Askable,Resource,User,Path) :-
    askable_no_inference(Aksable,Context),
    ask(Context,
        (
            t())).

 */


/**
 * assert_write_access(Graph_Descriptor,Context,Context) is det + error.
 *
 * Temporarily set the write_graph for permissions check.
 */
assert_write_access(G, Context, Context) :-
    New_Context = Context.put(write_graph,G),
    assert_write_access(New_Context, _).

write_type_access(instance,'@schema':'Action/instance_write_access').
write_type_access(schema,'@schema':'Action/schema_write_access').

is_super_user(Auth) :-
    Prefixes = _{ '@base' : 'terminusdb://system/data/' },
    is_super_user(Auth, Prefixes).

is_super_user(Auth,Prefixes) :-
    super_user_authority(URI),
    uri_eq(Auth, URI, Prefixes).

require_super_user(Context) :-
    % This allows us to shortcut looking in the database,
    % avoiding infinite regression
    is_super_user(Context.authorization, Context.prefixes),
    !.
require_super_user(Context) :-
    throw(error(not_super_user(Context))).

/**
 * assert_write_access(Context, Context) is det + error.
 *
 * Throws an error when access is forbidden
 *
 * Simply copies the context so it can be used as assert_write_access//0.
 */
assert_write_access(Context, Context) :-
    assert_write_access(Context).

assert_write_access(Context) :-
    is_super_user(Context.authorization, Context.prefixes),
    % This probably makes all super user checks redundant.
    !.
assert_write_access(Context) :-
    database_descriptor{
        organization_name: Organization_Name,
        database_name: Database_Name
    } :< Context.default_collection,
    !,
    Auth = (Context.authorization),
    DB = (Context.system),
    organization_database_name_uri(DB, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(DB, Auth, '@schema':'Action/meta_write_access', Scope_Iri).
assert_write_access(Context) :-
    repository_descriptor{
        database_descriptor :
        database_descriptor{
            organization_name: Organization_Name,
            database_name : Database_Name
        },
        repository_name : _Repo
    } :< Context.default_collection,
    !,
    Auth = (Context.authorization),
    DB = (Context.system),
    organization_database_name_uri(DB, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(DB, Auth, '@schema':'Action/commit_write_access', Scope_Iri).
assert_write_access(Context) :-
    branch_descriptor{
        repository_descriptor :
        repository_descriptor{
            database_descriptor :
            database_descriptor{
                organization_name: Organization_Name,
                database_name : Database_Name
            },
            repository_name : _Repo
        },
        branch_name : _Branch_Name
    }:< Context.default_collection,
    !,
    Auth = (Context.authorization),
    DB = (Context.system),
    WG = (Context.write_graph),
    write_type_access(WG.type,Access),
    organization_database_name_uri(DB, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(DB, Auth, Access, Scope_Iri).
assert_write_access(Context) :-
    throw(error(write_access_malformed_context(Context))).

/* Associated with the resource pre-pass checking */
assert_write_access(_System, Auth, _Collection, _Filter) :-
    is_super_user(Auth),
    % This probably makes all super user checks redundant.
    !.
assert_write_access(System, Auth, Collection, _Filter) :-
    database_descriptor{
        organization_name: Organization_Name,
        database_name: Database_Name
    } :< Collection,
    !,
    organization_database_name_uri(System, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(System, Auth, '@schema':'Action/meta_write_access', Scope_Iri).
assert_write_access(System, Auth, Collection, _Filter) :-
    repository_descriptor{
        database_descriptor :
        database_descriptor{
            organization_name: Organization_Name,
            database_name : Database_Name
        },
        repository_name : _Repo
    } :< Collection,
    !,
    organization_database_name_uri(System, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(System, Auth, '@schema':'Action/commit_write_access', Scope_Iri).
assert_write_access(System, Auth, Collection, Filter) :-
    branch_descriptor{
        repository_descriptor :
        repository_descriptor{
            database_descriptor :
            database_descriptor{
                organization_name: Organization_Name,
                database_name : Database_Name
            },
            repository_name : _Repo
        },
        branch_name : _Branch_Name
    }:< Collection,
    !,
    write_type_access(Filter.type,Access),
    organization_database_name_uri(System, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(System, Auth, Access, Scope_Iri).
assert_write_access(_System, _Auth, Collection, _Filter) :-
    throw(error(write_access_malformed_collection(Collection))).

/**
 * assert_read_access(Filter,Context,Context) is det + error.
 *
 * Temporarily set the filter for permissions check.
 */
assert_read_access(Filter,Context,Context) :-
    New_Context = Context.put(filter,Filter),
    assert_read_access(New_Context, _).

read_type_access(instance,'@schema':'Action/instance_read_access').
read_type_access(schema,'@schema':'Action/schema_read_access').

filter_types(type_filter{types:Types}, Types).
filter_types(type_name_filter{type : Type}, [Type]).

/**
 * assert_read_access(Context, Context) is det + error.
 *
 * Throws an error when access is forbidden
 *
 * Simply copies the context so it can be used as assert_read_access//0.
 */
assert_read_access(Context, Context) :-
    assert_read_access(Context).

assert_read_access(Context) :-
    is_super_user(Context.authorization, Context.prefixes),
    % This probably makes all super user checks redundant.
    !.
assert_read_access(Context) :-
    system_descriptor{} :< Context.default_collection,
    !,
    Auth = (Context.authorization),
    DB = (Context.system),
    assert_auth_action_scope(DB, Auth, '@schema':'Action/meta_read_access', 'system').
assert_read_access(Context) :-
    database_descriptor{
        organization_name: Organization_Name,
        database_name : Database_Name
    } :< Context.default_collection,
    !,
    Auth = (Context.authorization),
    DB = (Context.system),
    organization_database_name_uri(DB, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(DB, Auth, '@schema':'Action/meta_read_access', Scope_Iri).
assert_read_access(Context) :-
    repository_descriptor{
        database_descriptor :
        database_descriptor{
        organization_name: Organization_Name,
        database_name : Database_Name
        },
        repository_name : _Repo
    } :< Context.default_collection,
    !,
    Auth = (Context.authorization),
    DB = (Context.system),
    organization_database_name_uri(DB, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(DB, Auth, '@schema':'Action/commit_read_access', Scope_Iri).
assert_read_access(Context) :-
    branch_descriptor{
        repository_descriptor :
        repository_descriptor{
            database_descriptor :
            database_descriptor{
                organization_name: Organization_Name,
                database_name : Database_Name
            },
            repository_name : _Repo
        },
        branch_name : _Branch_Name
    } :< Context.default_collection,
    !,
    Auth = (Context.authorization),
    DB = (Context.system),
    Filter = (Context.filter),
    filter_types(Filter,Types),
    organization_database_name_uri(DB, Organization_Name, Database_Name, Scope_Iri),
    forall(member(Type,Types),
           (   read_type_access(Type,Access),
               assert_auth_action_scope(DB, Auth, Access, Scope_Iri))).
assert_read_access(Context) :-
    commit_descriptor{
        repository_descriptor:
        repository_descriptor{
            database_descriptor:
            database_descriptor{
                organization_name: Organization_Name,
                database_name : Database_Name
            },
            repository_name : _Repo
        },
        commit_id : _ID
    } :< Context.default_collection,
    !,
    Auth = (Context.authorization),
    DB = (Context.system),
    Filter = (Context.filter),
    filter_types(Filter,Types),
    organization_database_name_uri(DB, Organization_Name, Database_Name, Scope_Iri),
    forall(member(Type,Types),
           (   read_type_access(Type,Access),
               assert_auth_action_scope(DB, Auth, Access, Scope_Iri))).
assert_read_access(Context) :-
    throw(error(read_access_malformed_context(Context))).

assert_read_access(_System, Auth, _Collection, _Filter) :-
    is_super_user(Auth),
    % This probably makes all super user checks redundant.
    !.
assert_read_access(System, Auth, Collection, _Filter) :-
    database_descriptor{
        organization_name: Organization_Name,
        database_name : Database_Name
    } :< Collection,
    !,
    organization_database_name_uri(System, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(System, Auth, '@schema':'Action/meta_read_access', Scope_Iri).
assert_read_access(System, Auth, Collection, _Filter) :-
    repository_descriptor{
        database_descriptor :
        database_descriptor{
        organization_name: Organization_Name,
        database_name : Database_Name
        },
        repository_name : _Repo
    } :< Collection,
    !,
    organization_database_name_uri(System, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(System, Auth, '@schema':'Action/commit_read_access', Scope_Iri).
assert_read_access(System, Auth, Collection, Filter) :-
    branch_descriptor{
        repository_descriptor :
        repository_descriptor{
            database_descriptor :
            database_descriptor{
                organization_name: Organization_Name,
                database_name : Database_Name
            },
            repository_name : _Repo
        },
        branch_name : _Branch_Name
    } :< Collection,
    !,
    filter_types(Filter,Types),
    organization_database_name_uri(System, Organization_Name, Database_Name, Scope_Iri),
    forall(member(Type,Types),
           (   read_type_access(Type,Access),
               assert_auth_action_scope(System, Auth, Access, Scope_Iri))).
assert_read_access(System, Auth, Collection, Filter) :-
    commit_descriptor{
        repository_descriptor:
        repository_descriptor{
            database_descriptor:
            database_descriptor{
                organization_name: Organization_Name,
                database_name : Database_Name
            },
            repository_name : _Repo
        },
        commit_id : _ID
    } :< Collection,
    !,
    filter_types(Filter,Types),
    organization_database_name_uri(System, Organization_Name, Database_Name, Scope_Iri),
    forall(member(Type,Types),
           (   read_type_access(Type,Access),
               assert_auth_action_scope(System, Auth, Access, Scope_Iri))).
assert_read_access(_System, _Auth, Collection, _Filter) :-
    throw(error(read_access_malformed_collection(Collection))).


/**
 * assert_auth_action_scope(DB, Auth, Action, Scope) is det + error
 *
 * Determinise auth_action_scope/4 by throwing an error on failure
 */
assert_auth_action_scope(DB, Auth, Action, Scope) :-
    (   auth_action_scope(DB, Auth, Action, Scope)
    ->  true
    ;   throw(error(access_not_authorised(Auth,Action,Scope), _))).

/**
 * authorisation_capabilities(DB,Auth_ID,Capabilities) is det.
 *
 * Finds all capabilities
 */
authorisation_object(DB, Auth_ID, Capabilities) :-
    findall(
        Capability,
        ask(DB,
            (  t(Auth_ID, capability, Capability_ID), % Some action to look at...
               get_document(Capability_ID, Capability)
            )
           ),
        Capabilities
    ).

/**
 * user_accessible_database(DB,User_ID,Database) is det.
 *
 * Finds all database objects accessible to a user.
 */
user_accessible_database(DB, User_ID, Database) :-
    ask(DB,
        (   t(User_ID, capability, Capability_ID),
            path(Capability_ID, (star(p(scope)),p(database)), Database_ID),
            isa(Database_ID, 'UserDatabase'),
            get_document(Database_ID, Database)
        )).

/**
 * user_accessible_database(DB,User_ID,Org,Name,Database) is det.
 *
 * Finds a database object accessible to a user.
 */
user_accessible_database(DB, User_ID, Org, Name, Database) :-
    (   ground(Org)
    ->  atom_string(Org, Org_S)
    ;   true),
    (   ground(Name)
    ->  atom_string(Name, Name_S)
    ;   true),
    ask(DB,
        (   t(User_ID, capability, Capability_ID),
            path(Capability_ID, (star(p(scope)),p(database)), Database_ID),
            isa(Database_ID, 'UserDatabase'),
            t(Database_ID, name, Name_S^^xsd:string),
            t(Org_Id, database, Database_ID),
            t(Org_Id, name, Org_S^^xsd:string),
            get_document(Database_ID, Database)
        )),
    (   var(Org)
    ->  atom_string(Org, Org_S)
    ;   true),
    (   var(Name)
    ->  atom_string(Name, Name_S)
    ;   true
    ).


check_descriptor_auth_(system_descriptor{},Action,Auth,System_DB) :-
    assert_auth_action_scope(System_DB,Auth,Action,system).
check_descriptor_auth_(database_descriptor{ database_name : Database,
                                            organization_name : Organization},
                       Action, Auth, System_DB) :-
    do_or_die(organization_database_name_uri(System_DB, Organization, Database, URI),
              error(unknown_database(Organization, Database),_)),

    assert_auth_action_scope(System_DB,Auth,Action, URI).
check_descriptor_auth_(repository_descriptor{ database_descriptor : DB,
                                              repository_name : _ }, Action, Auth, System_DB) :-
    check_descriptor_auth_(DB, Action, Auth, System_DB).
check_descriptor_auth_(branch_descriptor{ repository_descriptor : Repo,
                                          branch_name : _ }, Action, Auth, System_DB) :-
    check_descriptor_auth_(Repo, Action, Auth, System_DB).
check_descriptor_auth_(commit_descriptor{ repository_descriptor : Repo,
                                          commit_id : _ }, Action, Auth, System_DB) :-
    check_descriptor_auth_(Repo, Action, Auth, System_DB).

check_descriptor_auth(System_DB, Descriptor, Action, Auth) :-
    check_descriptor_auth_(Descriptor, Action, Auth, System_DB).

document_auth_action_type(Descriptor_Type, Graph_Type_String, ReadWrite_String, Action) :-
    atom_string(Graph_Type, Graph_Type_String),
    atom_string(ReadWrite, ReadWrite_String),

    document_auth_action_type_(Descriptor_Type, Graph_Type, ReadWrite, Action).

document_auth_action_type_(system_descriptor, _, _, '@schema':'Action/manage_capabilities').
document_auth_action_type_(database_descriptor, _, read, '@schema':'Action/meta_read_access').
document_auth_action_type_(database_descriptor, instance, write, '@schema':'Action/meta_write_access').
document_auth_action_type_(repository_descriptor, _, read, '@schema':'Action/commit_read_access').
document_auth_action_type_(repository_descriptor, instance, write, '@schema':'Action/commit_write_access').
document_auth_action_type_(branch_descriptor, instance, read, '@schema':'Action/instance_read_access').
document_auth_action_type_(branch_descriptor, instance, write, '@schema':'Action/instance_write_access').
document_auth_action_type_(branch_descriptor, schema, read, '@schema':'Action/schema_read_access').
document_auth_action_type_(branch_descriptor, schema, write, '@schema':'Action/schema_write_access').
document_auth_action_type_(commit_descriptor, instance, read, '@schema':'Action/instance_read_access').
document_auth_action_type_(commit_descriptor, schema, read, '@schema':'Action/schema_read_access').

resolve_descriptor_auth(ReadWrite, SystemDB, Auth, Path, Graph_Type, Descriptor) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path), _)),
    Descriptor_Type{} :< Descriptor,
    do_or_die(document_auth_action_type(Descriptor_Type, Graph_Type, ReadWrite, Action),
              error(document_access_impossible(Descriptor, Graph_Type, ReadWrite), _)),
    check_descriptor_auth(SystemDB, Descriptor, Action, Auth).


:- begin_tests(capabilities).

:- use_module(core(util/test_utils)).
:- use_module(core(account/user_management)).

test(admin_has_access_to_all_dbs, [
         setup((setup_temp_store(State),
                add_user("Gavin", some('password'), _),
                create_db_without_schema("Gavin", "test1"))
               ),
         cleanup(teardown_temp_store(State))
     ]) :-

    super_user_authority(Auth),
    resolve_absolute_string_descriptor("Gavin/test1", GavinTestDB),
    check_descriptor_auth(system_descriptor{}, GavinTestDB, system:meta_write_access, Auth),
    check_descriptor_auth(system_descriptor{}, GavinTestDB, system:commit_write_access, Auth).

:- end_tests(capabilities).
