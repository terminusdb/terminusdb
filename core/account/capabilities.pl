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
              assert_write_access/1,
              assert_write_access/2,
              assert_write_access/3,
              authorisation_object/3,
              user_object/3,
              super_user_authority/1,
              check_descriptor_auth/4,
              is_super_user/2
          ]).

/** <module> Capabilities
 *
 * Capability system for access control.
 *
 * We will eventually integrate a rich ontological model which
 * enables fine grained permission access to the database.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).


:- use_module(config(terminus_config),[]).

:- use_module(library(crypto)).

/**
 * username_user_id(+DB, +Username, -User_ID) is semidet.
 *
 * Username user association - goes only one way
 */
username_user_id(DB, Username, User_ID) :-
    ask(DB,
        (
            t(User_ID, rdf:type, system:'User'),
            t(User_ID, system:agent_name, Username^^xsd:string)
        )
       ).


/**
 * key_user(+DB, +Username, +Key, -User_ID) is semidet.
 *
 * Key user association - goes only one way
 */
user_key_user_id(DB, Username, Key, User_ID) :-
    coerce_literal_string(Key, K),
    ask(DB,
        (
            t(User_ID, rdf:type, system:'User'),
            t(User_ID, system:agent_name, Username^^xsd:string),
            t(User_ID, system:user_key_hash, Hash^^xsd:string)
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
    document_jsonld(Database,User_ID,3,User).


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
auth_action_scope(DB, Auth, Action, Scope_Iri) :-
    ground(Auth),
    ask(DB,
        (
            t(Auth, system:role, Role),
            t(Role, system:capability, Capability),
            t(Capability, system:action, Action),
            t(Capability, system:capability_scope, Scope_Iri)
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

write_type_access(instance,system:instance_write_access).
write_type_access(schema,system:schema_write_access).
write_type_access(inference,system:inference_write_access).

is_super_user(Auth,Prefixes) :-
    super_user_authority(URI),
    prefixed_to_uri(Auth, Prefixes, URI).

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
    assert_auth_action_scope(DB, Auth, system:meta_write_access, Scope_Iri).
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
    assert_auth_action_scope(DB, Auth, system:commit_write_access, Scope_Iri).
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

/**
 * assert_read_access(Filter,Context,Context) is det + error.
 *
 * Temporarily set the filter for permissions check.
 */
assert_read_access(Filter,Context,Context) :-
    New_Context = Context.put(filter,Filter),
    assert_read_access(New_Context, _).

read_type_access(instance,system:instance_read_access).
read_type_access(schema,system:schema_read_access).
read_type_access(inference,system:inference_read_access).

filter_types(type_filter{types:Types}, Types).
filter_types(type_name_filter{type : Type, names : _}, [Type]).

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
    database_descriptor{
        organization_name: Organization_Name,
        database_name : Database_Name
    } :< Context.default_collection,
    !,
    Auth = (Context.authorization),
    DB = (Context.system),
    organization_database_name_uri(DB, Organization_Name, Database_Name, Scope_Iri),
    assert_auth_action_scope(DB, Auth, system:meta_read_access, Scope_Iri).
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
    assert_auth_action_scope(DB, Auth, system:commit_read_access, Scope_Iri).
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

/**
 * assert_auth_action_scope(DB, Auth, Action, Scope) is det + error
 *
 * Determinise auth_action_scope/4 by throwing an error on failure
 */
assert_auth_action_scope(DB, Auth, Action, Scope) :-
    (   auth_action_scope(DB, Auth, Action, Scope)
    ->  true
    ;   throw(error(access_not_authorised(Auth,Action,Scope)))).

/**
 * authorisation_object(DB,Auth_ID,Auth_Obj) is det.
 *
 * Finds all database objects accessible to a user.
 */
authorisation_object(DB, Auth_ID, Auth_Obj) :-
    once(ask(DB,
             (  t(Auth_ID, system:action, _), % Some action to look at...
                read_object(Auth_ID, 2, Auth_Obj)
             )
            )).

/**
 * user_object(DB,User_ID,User_Obj) is det.
 *
 * Finds all database objects accessible to a user.
 */
user_object(DB, User_ID, User_Obj) :-
    once(ask(DB,
             (  t(User_ID, rdf:type, system:'User'), % Some action to look at...
                read_object(User_ID, 4, User_Obj)
             )
            )).


check_descriptor_auth_(system_descriptor{},Action,Auth,Terminus) :-
    assert_auth_action_scope(Terminus,Auth,Action,doc:system).
check_descriptor_auth_(database_descriptor{ database_name : Database,
                                            organization_name : Organization},
                       Action, Auth, Terminus) :-
    organization_database_name_uri(Terminus, Organization, Database, URI),
    assert_auth_action_scope(Terminus,Auth,Action, URI).
check_descriptor_auth_(repository_descriptor{ database_descriptor : DB,
                                              repository_name : _ }, Action, Auth, Terminus) :-
    check_descriptor_auth_(DB, Action, Auth, Terminus).
check_descriptor_auth_(branch_descriptor{ repository_descriptor : Repo,
                                          branch_name : _ }, Action, Auth, Terminus) :-
    check_descriptor_auth_(Repo, Action, Auth, Terminus).
check_descriptor_auth_(commit_descriptor{ repository_descriptor : Repo,
                                          commit_id : _ }, Action, Auth, Terminus) :-
    check_descriptor_auth_(Repo, Action, Auth, Terminus).

check_descriptor_auth(Terminus, Descriptor, Action, Auth) :-
    check_descriptor_auth_(Descriptor, Action, Auth, Terminus).
