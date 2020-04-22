:- module(capabilities,[
              user_key_auth/4,
              username_user_id/3,
              user_key_user_id/4,
              user_id_auth_id/3,
              username_auth/3,
              get_user/3,
              auth_action_scope/4,
              write_cors_headers/2,
              authorisation_object/3,
              user_object/3
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
 * user_name_user_id(+DB, +Username, -User_ID) is semidet.
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
            t(Auth, terminus:action, Action),
            t(Auth, terminus:authority_scope, Scope),
            t(Scope, terminus:resource_name, Resource_Name ^^ (xsd:string))
        )
       ).

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
    format(Out,'Access-Control-Allow-Headers: Authorization, Accept, Accept-Encoding, Accept-Language, Host, Origin, Referer, Content-Type, Content-Length, Content-Range, Content-Disposition, Content-Description\n',[]),
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
