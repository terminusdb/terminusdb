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

:- use_module(library(utils)).
:- use_module(library(database_utils)).
:- use_module(library(api)).

/* 
 * run_api_tests is det. 
 *
 * Run all structured tests of the API
 */ 
run_api_tests :-
    try(run_db_create_test),
    try(run_db_delete_test),
    try(run_get_filled_frame_test),
    try(run_woql_test).

try(Goal) :- 
    (   call(Goal)
    ->  true
    ;   format('~n*************************************~nFAIL! Could not successfully run ~s~n', [Goal])).

run_db_create_test :-
    % create DB
    config:server_name(Server_Name),
    config:server_port(Port),
    atomic_list_concat([Server_Name,'/terminus_qa_test'], DB_URI),
    catch(
        api:try_delete_db(DB_URI),
        _,
        true),

    % Need to set the user key correctly here or we will get a spurious error...
    Payload = '{"terminus:document": {"@type":"terminus:Database", "rdfs:label":{"@language":"en","@value":"asdsda"}, "rdfs:comment":{"@language":"en","@value":"dasd"}, "terminus:schema":{"@type":"xsd:string","@value":"schema"}, "terminus:document":{"@type":"xsd:string","@value":"document"}, "terminus:allow_origin":{"@type":"xsd:string","@value":"*"}, "@id":"http://localhost/terminus_qa_test"}, "@context":{"rdfs":"http://www.w3.org/2000/01/rdf-schema#", "terminus":"https://datachemist.net/ontology/terminus#"}, "@type":"terminus:APIUpdate", "terminus:user_key":"root"}',
    atomic_list_concat(['curl -d \'',Payload,'\' -H "Content-Type: application/json" -X POST ',Server_Name,':',Port,'/terminus_qa_test'], Cmd),
    
    format('~nRunning command: "~s"~n',[Cmd]),        
    shell(Cmd),

    % cleanup
    delete_db(DB_URI).


run_db_delete_test :-
    % create DB
    config:server_name(Server_Name),
    config:server_port(Port),
    atomic_list_concat([Server_Name,'/terminus_qa_test'], DB_URI),

    % in case test already exists...
    catch(
        api:try_delete_db(DB_URI),
        _,
        true),
    
    Doc = _17614{'@id':"http://localhost/terminus_qa_test",
                 '@type':"terminus:Database",
                 'rdfs:comment':_17542{'@language':"en",
                                       '@value':"dasd"},
                 'rdfs:label':_17494{'@language':"en",
                                     '@value':"asdsda"},
                 'terminus:allow_origin':_17590{'@type':"xsd:string",
                                                '@value':"*"}},
    
    api:try_create_db(terminus_qa_test,DB_URI,Doc),
    
    % Need to set the user key correctly here or we will get a spurious error...
    atomic_list_concat(['curl -X DELETE ',Server_Name,':',Port,'/terminus_qa_test?terminus:user_key=root'], Cmd),
    
    format('~nRunning command: "~s"~n',[Cmd]),        
    shell(Cmd).

run_get_filled_frame_test :-
    % create DB
    config:server_name(Server_Name),
    config:server_port(Port),
    atomic_list_concat(['curl -X GET \'',Server_Name,':',Port,'/terminus/document/terminus?terminus%3Aencoding=terminus%3Aframe&terminus%3Auser_key=root\''], Cmd),
    shell(Cmd).

run_get_doc_test :-
    % create DB
    config:server_name(Server_Name),
    config:server_port(Port),
    atomic_list_concat(['curl -X GET \'',Server_Name,':',Port,'/terminus/document/terminus?terminus%3Auser_key=root\''], Cmd),
    shell(Cmd).

run_woql_test :-
    config:server_name(Server_Name),
    config:server_port(Port),
    atomic_list_concat(['curl -X GET \'',Server_Name,':',Port,'/terminus/woql?terminus%3Aquery=prefixes(%5B%20s%3D%27http%3A%2F%2F195.201.12.87%3A6363%2Fterminus%2Fschema%23%27%2Cdg%3D%27http%3A%2F%2F195.201.12.87%3A6363%2Fterminus%2Fschema%27%2Cdoc%3D%27http%3A%2F%2F195.201.12.87%3A6363%2Fterminus%2Fdocument%2F%27%2Cdb%3D%27http%3A%2F%2F195.201.12.87%3A6363%2Fterminus%2F%27%2Cg%3D%27http%3A%2F%2F195.201.12.87%3A6363%2F%27%2Crdf%3D%27http%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%27%2Crdfs%3D%27http%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%27%2Cxdd%3D%27https%3A%2F%2Fdatachemist.net%2Fontology%2Fxdd%23%27%2Cxsd%3D%27http%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema%23%27%2Cdcog%3D%27https%3A%2F%2Fdatachemist.net%2Fontology%2Fdcog%23%27%2Cowl%3D%27http%3A%2F%2Fwww.w3.org%2F2002%2F07%2Fowl%23%27%2Cdcogbox%3D%27https%3A%2F%2Fdatachemist.net%2Fontology%2Fdcogbox%23%27%5D%2C%20from(g%2F%27terminus%27%2Cselect(%5Bv(%27Class%27)%2C%20v(%27Label%27)%2C%20v(%27Comment%27)%2C%20v(%27Abstract%27)%5D%2C(t(v(%27Class%27)%2C%20rdf%2Ftype%2C%20owl%2F%27Class%27%2C%20dg%2Fschema)%2C%20not(t(v(%27Class%27)%2C%20dcog%2Ftag%2C%20dcog%2Fabstract%2C%20dg%2Fschema))%2C%20opt(t(v(%27Class%27)%2C%20rdfs%2Flabel%2C%20v(%27Label%27)%2C%20dg%2Fschema))%2C%20opt(t(v(%27Class%27)%2C%20rdfs%2Fcomment%2C%20v(%27Comment%27)%2C%20dg%2Fschema))%2C%20opt(t(v(%27Class%27)%2C%20dcog%2Ftag%2C%20v(%27Abstract%27)%2C%20dg%2Fschema))))))&terminus%3Auser_key=root\''], Cmd),
    shell(Cmd).


    
