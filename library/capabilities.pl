:- module(capabilities,[key_capabilities/2]).
                 
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

capability_database('capability/').

/** 
 * capability_context(Context : dictionary) is det.
 * 
 * JSON-LD capability access context. 
 */ 
capability_context(_{
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
key_user(Key, Root_User) :-
    config:root_password_hash(Key),
    !,
    root_user_id(Root_User_ID),
    entity_object(Root_User).

/** 
 * key_capabilities(Key,Capabilities) is det. 
 *  
 * Give a capabilities JSON object corresponding to the capabilities
 * of the key supplied by searching the core permissions database.
 */ 
key_capabilities(Key, Capabilities) :-
    key_user(Key,User),
    user_access(User, Access),
    capability_context(Capability_Context),
    Capabilities = _{
        '@context' : Capability_Context,               
        'reg:server_key' : Key,
        'reg:user' : User,
        'reg:access' : Access
    },
    compress(Capabilities,Capability_Context,CC).
                 
/* 
 * user_capabilities(User,Access_Object) is det.
 */
user_capabilities(User, Accesses) :-
    setof(Access,
          user_capability(User,Access),
          Accesses).

user_capability(User,Action) :-
    connect('http://localhost/capability',DB),
    ask(DB, 
        select([User, Action], 
		       (
			       t( User , rdf/type , reg/'User' ), 
			       t( User , reg/authority, Auth ), 
			       t( Auth , reg/action, Action)
		       )
	          )
       ).

