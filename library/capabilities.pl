:- module(capabilities,[
              key_auth/2
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
 *  This file is part of RegulumDB.                                      *
 *                                                                       *
 *  RegulumDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  RegulumDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with RegulumDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

:- use_module(config(config),[]).
:- use_module(library(utils)).
:- use_module(library(file_utils)).
:- use_module(library(triplestore)).
:- use_module(library(frame)).
:- use_module(library(json_ld)).
:- use_module(library(collection)).

capability_collection('http://localhost/capability').

/** 
 * capability_context(Context : dictionary) is det.
 * 
 * JSON-LD capability access context. 
 */ 
capability_context(_{
                       doc : 'https://localhost/masterdb/candidate/', 
                       reg : 'https://regulumdb.com/ontology/regulum#'
                   }).

/** 
 * root_user_id(Root_User_ID : uri) is det.
 */
root_user_id('https://localhost/masterdb/candidate/admin').

/** 
 * key_user(Key,User) is det.
 * 
 * Key user association
 */ 
key_user(Key, Root_User_ID) :-
    config:root_password_hash(Key),
    !,
    root_user_id(Root_User_ID).

/** 
 * key_auth(Key,Auth) is det. 
 *  
 * Give a capabilities JSON object corresponding to the capabilities
 * of the key supplied by searching the core permissions database.
 */ 
key_auth(Key, Auth) :-
    key_user(Key,User_ID),

    capability_collection(C),
    make_collection_graph(C,Graph),
    capability_context(Ctx),

    user_auth_id(User_ID, Auth_ID),
    
    entity_jsonld(Auth_ID,Ctx,Graph,Auth).

/* 
 * user_auth_id(User,Auth_id) is det.
 * 
 * Maybe should return the auth object - as soon as we have 
 * obj embedded in woql.
 */
user_auth_id(User_ID, Auth_ID) :-
    capability_collection(Collection),
    connect(Collection,DB),
    ask(DB, 
        select([Auth_ID], 
		       (
			       t( User_ID , rdf/type , reg/'User' ), 
			       t( User_ID , reg/authority, Auth_ID )
		       )
	          )
       ).

user_capability(User,Action) :-
    capability_collection(Collection),
    connect(Collection,DB),
    ask(DB, 
        select([User, Action], 
		       (
			       t( User , rdf/type , reg/'User' ), 
			       t( User , reg/authority, Auth ), 
			       t( Auth , reg/action, Action)
		       )
	          )
       ).

