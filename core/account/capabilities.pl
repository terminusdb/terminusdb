:- module(capabilities,[
              user_key_auth/4,
              username_user_id/3,
              user_key_user_id/4,
              user_id_auth_id/3,
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
              write_cors_headers/2,
              authorisation_object/3,
              user_object/3,
              super_user_authority/1,
              check_descriptor_auth/4
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
            t(User_ID, rdf:type, terminus:'User'),
            t(User_ID, terminus:agent_name, Username^^xsd:string)
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
            t(User_ID, rdf:type, terminus:'User'),
            t(User_ID, terminus:agent_name, Username^^xsd:string),
            t(User_ID, terminus:agent_key_hash, Hash^^xsd:string)
        )
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
user_key_auth(DB, Username, Key, Auth_ID) :-
    user_key_user_id(DB, Username, Key, User_ID),
    user_id_auth_id(DB, User_ID, Auth_ID).

/**
 * username_auth(DB, Key,Auth) is det.
 *
 * Give a capabilities JSON object corresponding to the capabilities
 * of the username supplied by searching the core permissions database.
 */
username_auth(DB, Username, Auth) :-
    username_user_id(DB, Username, User_ID),
    user_id_auth_id(DB, User_ID, Auth).

/*
 * user_id_auth_id(+DB, +User_ID, -Auth_id) is semidet.
 *
 * Sould return the auth object
 */
user_id_auth_id(DB, User_ID, Auth_ID) :-
    ask(DB,
        (
            t( User_ID , rdf:type , terminus:'User' ),
            t( User_ID , terminus:authority, Auth_ID )
        )
       ).

/*
 * auth_action_scope(Auth,Action,Scope) is nondet.
 *
 * Does Auth object have capability Action on scope Scope.
 *
 * This needs to implement some of the logical character of scope subsumption.
 */
auth_action_scope(DB, Auth, Action, Resource_Name) :-
    ask(DB,
        (
            t(Auth, terminus:access, Access),
            t(Access, terminus:action, Action),
            t(Access, terminus:authority_scope, Scope),
            t(Scope, terminus:resource_name, Resource_Name ^^ (xsd:string))
        )
       ).

/**
 * assert_write_access(Graph_Descriptor,Context,Context) is det + error.
 *
 * Temporarily set the write_graph for permissions check.
 */
assert_write_access(G, Context, Context) :-
    New_Context = Context.put(write_graph,G),
    assert_write_access(New_Context, _).

write_type_access(instance,terminus:instance_write_access).
write_type_access(schema,terminus:schema_write_access).
write_type_access(inference,terminus:inference_write_access).

super_user_authority('terminus:///terminus/document/access_all_areas').

require_super_user(Context) :-
    % This allows us to shortcut looking in the database,
    % avoiding infinite regression
    %prefixed_to_uri(Context.authorization, Context.prefixes, Auth),
    Context.authorization = Auth,
    (   Auth = doc:access_all_areas
    ;   Auth = 'terminus:///terminus/document/access_all_areas'),
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
    terminus_descriptor{} :< Context.default_collection,
    !,
    % This allows us to shortcut looking in the database,
    % avoiding infinite regression
    require_super_user(Context).
assert_write_access(Context) :-
    database_descriptor{ database_name : Name
                       } :< Context.default_collection,
    !,
    Auth = Context.authorization,
    DB = Context.terminus,
    assert_auth_action_scope(DB, Auth, terminus:meta_write_access, Name).
assert_write_access(Context) :-
    repository_descriptor{
        database_descriptor :
        database_descriptor{
            database_name : Name
        },
        repository_name : _Repo
    } :< Context.default_collection,
    !,
    Auth = Context.authorization,
    DB = Context.terminus,
    assert_auth_action_scope(DB, Auth, terminus:commit_write_access, Name).
assert_write_access(Context) :-
    branch_descriptor{
        repository_descriptor :
        repository_descriptor{
            database_descriptor :
            database_descriptor{
                database_name : Name
            },
            repository_name : _Repo
        },
        branch_name : _Branch_Name
    }:< Context.default_collection,
    !,
    Auth = Context.authorization,
    DB = Context.terminus,
    WG = Context.write_graph,
    write_type_access(WG.type,Access),
    assert_auth_action_scope(DB, Auth, Access, Name).
assert_write_access(Context) :-
    id_descriptor{
        id: _ID
    } :< Context.default_collection,
    !,
    require_super_user(Context).
assert_write_access(Context) :-
    label_descriptor{
        label: _Label
    } :< Context.default_collection,
    !,
    require_super_user(Context).
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

read_type_access(instance,terminus:instance_read_access).
read_type_access(schema,terminus:schema_read_access).
read_type_access(inference,terminus:inference_read_access).

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
    terminus_descriptor{} :< Context.default_collection,
    !,
    require_super_user(Context).
assert_read_access(Context) :-
    database_descriptor{ database_name : Name
                       } :< Context.default_collection,
    !,
    Auth = Context.authorization,
    DB = Context.terminus,
    assert_auth_action_scope(DB, Auth, terminus:meta_read_access, Name).
assert_read_access(Context) :-
    repository_descriptor{
        database_descriptor :
        database_descriptor{
            database_name : Name
        },
        repository_name : _Repo
    } :< Context.default_collection,
    !,
    Auth = Context.authorization,
    DB = Context.terminus,
    assert_auth_action_scope(DB, Auth, terminus:commit_read_access, Name).
assert_read_access(Context) :-
    branch_descriptor{
        repository_descriptor :
        repository_descriptor{
            database_descriptor :
            database_descriptor{
                database_name : Name
            },
            repository_name : _Repo
        },
        branch_name : _Branch_Name
    } :< Context.default_collection,
    !,
    Auth = Context.authorization,
    DB = Context.terminus,
    Filter = Context.filter,
    filter_types(Filter,Types),
    forall(member(Type,Types),
           (   read_type_access(Type,Access),
               assert_auth_action_scope(DB, Auth, Access, Name))).
assert_read_access(Context) :-
    id_descriptor{
        id: _ID
    } :< Context.default_collection,
    !,
    Auth = Context.authorization,
    DB = Context.terminus,
    % Equivalent to trying to read terminus?
    Filter = Context.filter,
    filter_types(Filter,Types),
    forall(member(Type,Types),
           (   read_type_access(Type,Access),
               assert_auth_action_scope(DB, Auth, Access, "terminus"))).
assert_read_access(Context) :-
    label_descriptor{
        label: _Label
    } :< Context.default_collection,
    !,
    Auth = Context.authorization,
    DB = Context.terminus,
    % Equivalent to trying to read terminus?
    Filter = Context.filter,
    filter_types(Filter,Types),
    forall(member(Type,Types),
           (   read_type_access(Type,Access),
               assert_auth_action_scope(DB, Auth, Access, "terminus"))).
assert_read_access(Context) :-
    commit_descriptor{
        repository_descriptor:
        repository_descriptor{
            database_descriptor:
            database_descriptor{
                database_name : DB_Name
            },
            repository_name : _Repo
        },
        commit_id : _ID
    } :< Context.default_collection,
    !,
    Auth = Context.authorization,
    DB = Context.terminus,
    Filter = Context.filter,
    filter_types(Filter,Types),
    forall(member(Type,Types),
           (   read_type_access(Type,Access),
               assert_auth_action_scope(DB, Auth, Access, DB_Name))).
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
             (  t(Auth_ID, terminus:action, _), % Some action to look at...
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
             (  t(User_ID, rdf:type, terminus:'User'), % Some action to look at...
                read_object(User_ID, 3, User_Obj)
             )
            )).


check_descriptor_auth_(terminus_descriptor{},Action,Auth,Terminus) :-
    assert_auth_action_scope(Terminus,Auth,Action,"terminus").
check_descriptor_auth_(database_descriptor{ database_name : Name }, Action, Auth, Terminus) :-
    assert_auth_action_scope(Terminus,Auth,Action,Name).
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

/*
 * write_cors_headers(Resource_Name) is det.
 *
 * Writes cors headers associated with Resource_URI
 */
write_cors_headers(Resource_Name, DB) :-
    % delete the object
    findall(Origin,
            ask(DB,
                (   t(Internal_Resource_URI, terminus:resource_name, Resource_Name^^(xsd:string)),
                    t(Internal_Resource_URI, terminus:allow_origin, Origin^^(xsd:string))
                )),
            Origins),
    sort(Origins,Unique_Origins),
    current_output(Out),
    format(Out,'Access-Control-Allow-Methods: GET, POST, DELETE, OPTIONS\n',[]),
    format(Out,'Access-Control-Allow-Credentials: true\n',[]),
    format(Out,'Access-Control-Max-Age: 1728000\n',[]),
    format(Out,'Access-Control-Allow-Headers: Authorization, Authorization-Remote, Accept, Accept-Encoding, Accept-Language, Host, Origin, Referer, Content-Type, Content-Length, Content-Range, Content-Disposition, Content-Description\n',[]),
    format(Out,'Access-Control-Allow-Origin: ',[]),
    write_domains(Unique_Origins, Out),
    format(Out,'\n',[]).

write_domains([], _).
write_domains([Domain| Domains], Out) :-
    write(Out, Domain),
    (   Domains == []
    ->  true
    ;   write(' '),
        write_domains(Domains, Out)
    ).
