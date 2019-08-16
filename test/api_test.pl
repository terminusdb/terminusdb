:- module(api_test,[
              run_api_tests/0
          ]).
                 
/** <module> Capabilities
 * 
 * Tests of the HTTP API interface
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
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

:- use_module(library(database_utils)).

/* 
 * run_api_tests is det. 
 *
 * Run all structured tests of the API
 */ 
run_api_tests :-
    try(run_db_create_test).

try(Goal) :- 
    (   call(Goal)
    ->  true
    ;   format(atom(Msg),'Could not run ~s', [Goal]),
        throw(error(Msg))).

run_db_create_test :-
    % create DB
    config:server_name(Server_Name),
    config:server_port(Port),
    % Need to set the user key correctly here or we will get a spurious error...
    Payload = '{"terminus:document": {"@type":"terminus:Database", "rdfs:label":{"@language":"en","@value":"asdsda"}, "rdfs:comment":{"@language":"en","@value":"dasd"}, "terminus:allow_origin":{"@type":"xsd:string","@value":"*"}, "@id":"http://localhost/test"}, "@context":{"rdfs":"http://www.w3.org/2000/01/rdf-schema#", "terminus":"https://datachemist.net/ontology/terminus#"}, "@type":"terminus:APIUpdate", "terminus:user_key":"root"}',
    atomic_list_concat(['curl -d \'',Payload,'\' -H "Content-Type: application/json" -X POST ',Server_Name,':',Port,'/test'], Cmd),
    
    format('~nRunning command: "~s"~n',[Cmd]),        
    shell(Cmd),

    % cleanup
    atomic_list_concat([Server_Name,'/test'],DB_URI),
    ignore(delete_db(DB_URI)).

