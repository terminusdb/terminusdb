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
:- use_module(library(triplestore)).

capability_database('capability/').

/** 
 * capability_context(Context : dictionary) is det.
 * 
 * JSON-LD capability access context. 
 */ 
capability_context(_{
                       cap : 'https://regulumdb.com/ontology/capability#'
                   }).

/** 
 * root_user_id(Root_User_ID : uri) is det.
 */
root_user_id(Root_User_ID) :-
    config:server_name(Server),
    capability_database(Cap_DB),
    interpolate([Server,Cap_DB,user/root],Root_User_ID).

/** 
 * key_user(Key,User) is det.
 * 
 * Key user association
 */ 
key_user(Key, Root_User) :-
    config:root_password_hash(Key),
    !,
    root_user_id(Root_User_ID),
    Root_User = _{ 'cap:id' : Root_User_ID,
                   'cap:name' : root }.

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
        'cap:server_key' : Key,
        'cap:user' : User,
        'cap:access' : Access
    }.
                 
/* 
 * user_access(User,Access) is det.
 */
user_access(Root_User, Access) :-
    % Root User is special
    root_user_id(Root_User_ID),
    get_dict('cap:id', Root_User, Root_User_ID),
    !,

    collections(DBs),
    config:server_name(Server_Name),
    Server_Capability = (
        Server_Name - [_{
                           action : create, 
                           class : 'Database'},
                       _{
                           action : list, 
                           class : 'Database'}]
    ),
    findall([DB - Access],
            (   member(DB,DBs),
                db_meta_data(DB,Meta_Data),
                put_dict(_{
                             capabilities : [ createdb, connect ]
                         },
                         Meta_Data,
                         Access)
            ),
            Objs),
    dict_pairs(Access, _, [Server_Capability|Objs]).
user_access(_User, _Access) :-
    % Query other users here. 
    fail.

