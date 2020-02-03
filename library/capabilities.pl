:- module(capabilities,[
              key_auth/3,
              key_user/3,
              get_user/2,
              user_action/2,
              auth_action_scope/4,
              add_database_resource/3,
              delete_database_resource/1,
              write_cors_headers/2
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

:- use_module(config(config),[]).
:- use_module(library(crypto)).
:- use_module(utils).
:- use_module(file_utils).
:- use_module(triplestore).
:- use_module(frame).
:- use_module(jsonld).
:- use_module(database).
:- use_module(database_utils).
:- use_module(sdk).

:- op(2, xfx, ^^).
:- op(1050, xfx, =>).

/**
 * root_user_id(Root_User_ID : uri) is det.
 */
root_user_id(Root) :-
    config:public_server_url(Server),
    atomic_list_concat([Server,'/terminus/document/admin'],Root).

/**
 * key_user(+Key,-User) is det.
 *
 * Key user association - goes only one way
 */
key_user(Key, DB, User_ID) :-
    coerce_literal_string(Key,K),
    ask(DB,
        select([User_ID],
		       (
			       t( User_ID , rdf/type , terminus/'User' ),
			       t( User_ID , terminus/user_key_hash, Hash^^_ )
		       )
	          )
       ),
    atom_string(Hash_Atom, Hash),
    crypto_password_hash(K, Hash_Atom).

/**
 * get_user(+User_ID, -User) is det.
 *
 * Gets back a full user object which includes all authorities
 */
get_user(User_ID, User) :-
    terminus_database(Database),
    terminus_context(Ctx),

    document_jsonld(User_ID,Ctx,Database,3,User).


/**
 * key_auth(Key,Auth) is det.
 *
 * Give a capabilities JSON object corresponding to the capabilities
 * of the key supplied by searching the core permissions database.
 */
key_auth(Key, DB, Auth) :-
    key_user(Key,DB,User_ID),

    terminus_database(Database),
    terminus_context(Ctx),

    user_auth_id(User_ID, DB, Auth_ID),

    document_jsonld(Auth_ID,Ctx,Database,Auth).

/*
 * user_auth_id(User,Auth_id) is semidet.
 *
 * Maybe should return the auth object - as soon as we have
 * obj embedded in woql.
 */
user_auth_id(User_ID, DB, Auth_ID) :-
    ask(DB,
        select([Auth_ID],
		       (
			       t( User_ID , rdf/type , terminus/'User' ),
			       t( User_ID , terminus/authority, Auth_ID )
		       )
	          )
       ).

/*
 * user_action(+User,-Action) is nondet.
 */
user_action(User,Action) :-
    terminus_database_name(Collection),
    connect(Collection,DB),
    ask(DB,
        select([Action],
		       (
			       t( User , rdf/type , terminus/'User' ),
			       t( User , terminus/authority, Auth ),
			       t( Auth , terminus/action, Action)
		       )
	          )
       ).

/*
 * auth_action_scope(Auth,Action,Scope) is nondet.
 *
 * Does Auth object have capability Action on scope Scope.
 *
 * This needs to implement some of the logical character of scope subsumption.
 */
auth_action_scope(Auth, DB, Action, Resource_ID) :-
    ask(DB,
	    where(
            (
                t(Auth, terminus/action, Action),
                t(Auth, terminus/authority_scope, Scope),
                t(Scope, terminus/id, Resource_ID ^^ (xsd/anyURI))
            )
        )
	   ).

    % make comparison late..
    %atom_string(Resource_ID,Resource_ID_String).

/*
 * add_database_resource(DB,URI,Doc) is det.
 *
 * Adds a database resource object to the capability instance database for the purpose of
 * authority reference.
 *
 * DB is the name of the database, URI is its identifier and Doc is a document which
 * describes all of its metadata properties.
 */
add_database_resource(DB_Name,URI,Doc) :-
    expand(Doc,DocX),
    /* Don't create database resources if they already exist */
    (   database_exists(URI)
    ->  throw(http_reply(method_not_allowed(
                             _{'@type' : 'vio:DatabaseCreateError',
                               'vio:database_name' : _{'@value' : URI,
                                                       '@type' : 'xdd:url'},
                               'vio:message' : 'Database exists'})))
    ;   true),

    /* This check is required to cary out appropriate auth restriction */
    (   get_key_document('@type', DocX, 'terminus:Database')
    ->  true
    ;   format(atom(MSG),'Unable to create database metadata due to capabilities authorised.',[]),
        throw(http_reply(method_not_allowed(
                             _{'@type' : 'vio:DatabaseCreateError',
                               'vio:database_name' : _{'@value' : URI,
                                                       '@type' : 'xdd:url'},
                               'vio:message' : MSG})))),

    /* Extend Doc with default databases */
    extend_database_defaults(URI, DocX, Ext),

    terminus_database_name(Collection),
    connect(Collection, DB),
    ask(DB,
	    (
            true
        =>
            insert(doc/server, terminus/resource_includes, doc/DB_Name),
            insert(doc/DB_Name, terminus/id, URI^^(xsd/anyURI)),
            update_object(doc/DB_Name,Ext)
        )
       ).


/*
 * delete_database_resource(URI) is det.
 *
 * Deletes a database resource object to the capability instance database for the purpose of
 * removing the authority reference.
 */
delete_database_resource(URI) :-
    % hmmm... this is going to be tricky... We need to delete all references to the object.
    % but are those references then going to be "naked" having no other reference?
    %
    % Supposing we have only one scope for an auth, do we delete the auth?
    terminus_database_name(Collection),
    connect(Collection, DB),
    % delete the object
    ask(DB,
        (   t(DB_URI, terminus/id, URI^^(xsd/anyURI)),
            (
                where(
                    (
                        t(DB_URI, terminus/id, URI^^(xsd/anyURI)),
                        t(DB_URI, rdf/type, terminus/'Database')
                    ))
            =>
                delete_object(DB_URI))
        )).

/*
 * write_cors_headers(Resource_URI) is det.
 *
 * Writes cors headers associated with Resource_URI
 */
write_cors_headers(Resource_URI, DB) :-
    % delete the object
    findall(Origin,
            ask(DB,
                where(
                    (   t(Internal_Resource_URI, terminus/id, Resource_URI^^(xsd/anyURI)),
                        t(Internal_Resource_URI, terminus/allow_origin, Origin^^(xsd/string))
                    )
                )),
            Origins),
    current_output(Out),
    format(Out,'Access-Control-Allow-Methods: GET, POST, DELETE, OPTIONS\n',[]),
    format(Out,'Access-Control-Allow-Credentials: true\n',[]),
    format(Out,'Access-Control-Max-Age: 1728000\n',[]),
    format(Out,'Access-Control-Allow-Headers: Authorization, Accept, Accept-Encoding, Accept-Language, Host, Origin, Referer, Content-Type, Content-Length, Content-Range, Content-Disposition, Content-Description\n',[]),
    format(Out,'Access-Control-Allow-Origin: ',[]),
    write_domains(Origins, Out),
    format(Out,'\n',[]).

write_domains([], _).
write_domains([Domain| Domains], Out) :-
    write(Out, Domain),
    (   Domains == []
    ->  true
    ;   write(' '),
        write_domains(Domains, Out)
    ).
