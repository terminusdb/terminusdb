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
    try(run_woql_test),
    try(run_schema_get_test).

try(Goal) :- 
    (   call(Goal)
    ->  true
    ;   format('~n*************************************~nFAIL! Could not successfully run ~s~n', [Goal])).

run_db_create_test :-
    % create DB
    config:server(Server),
    atomic_list_concat([Server,'/terminus_qa_test'], DB_URI),
    catch(
        api:try_delete_db(DB_URI),
        _,
        true),

    % Need to set the user key correctly here or we will get a spurious error...
    atomic_list_concat(['{"terminus:document": {"@type":"terminus:Database", "rdfs:label":{"@language":"en","@value":"asdsda"}, "rdfs:comment":{"@language":"en","@value":"dasd"}, "terminus:schema":{"@type":"xsd:string","@value":"schema"}, "terminus:document":{"@type":"xsd:string","@value":"document"}, "terminus:allow_origin":{"@type":"xsd:string","@value":"*"}, "@id":"',Server,'/terminus_qa_test"}, "@context":{"rdfs":"http://www.w3.org/2000/01/rdf-schema#", "terminus":"https://datachemist.net/ontology/terminus#"}, "@type":"terminus:APIUpdate", "terminus:user_key":"root"}'], Payload),
    
    atomic_list_concat(['curl -d \'',Payload,'\' -H "Content-Type: application/json" -X POST ',Server,'/terminus_qa_test'], Cmd),
    
    format('~nRunning command: "~s"~n',[Cmd]),        
    shell(Cmd),

    % cleanup
    delete_db(DB_URI).


run_db_delete_test :-
    % create DB
    config:server(Server),

    atomic_list_concat([Server,'/terminus_qa_test'], DB_URI),

    % in case test already exists...
    catch(
        api:try_delete_db(DB_URI),
        _,
        true),

    format(string(DBID), '~s', [DB_URI]),
    
    Doc = _17614{'@id': DBID,
                 '@type':"terminus:Database",
                 'rdfs:comment':_17542{'@language':"en",
                                       '@value':"dasd"},
                 'rdfs:label':_17494{'@language':"en",
                                     '@value':"asdsda"},
                 'terminus:allow_origin':_17590{'@type':"xsd:string",
                                                '@value':"*"}},
    
    api:try_create_db(terminus_qa_test,DB_URI,Doc),
    
    % Need to set the user key correctly here or we will get a spurious error...
    atomic_list_concat(['curl -X DELETE ',Server,'/terminus_qa_test?terminus:user_key=root'], Cmd),
    
    format('~nRunning command: "~s"~n',[Cmd]),        
    shell(Cmd).

run_get_filled_frame_test :-
    % create DB
    config:server(Server),
    atomic_list_concat(['curl -X GET \'',Server,'/terminus/document/terminus?terminus%3Aencoding=terminus%3Aframe&terminus%3Auser_key=root\''], Cmd),
    shell(Cmd).

run_get_doc_test :-
    % create DB
    config:server(Server),
    atomic_list_concat(['curl -X GET \'',Server,'/terminus/document/terminus?terminus%3Auser_key=root\''], Cmd),
    shell(Cmd).

run_woql_test :-
    config:server(Server),
    atomic_list_concat(
        ['prefixes([ s=\'',Server,'/terminus/schema#\',
                     doc=\'',Server,'/terminus/document/\',
                     db=\'',Server,'/terminus/\',
                     g=\'',Server,'/\',
                     rdf=\'http://www.w3.org/1999/02/22-rdf-syntax-ns#\',
                     rdfs=\'http://www.w3.org/2000/01/rdf-schema#\',
                     xdd=\'https://datachemist.net/ontology/xdd#\',
                     xsd=\'http://www.w3.org/2001/XMLSchema#\',
                     dcog=\'https://datachemist.net/ontology/dcog#\',
                     owl=\'http://www.w3.org/2002/07/owl#\',
                     dcogbox=\'https://datachemist.net/ontology/dcogbox#\'], 
          from(g/\'terminus\',
             select([v(\'Class\'), v(\'Label\'), v(\'Comment\'), v(\'Abstract\')],
                (t(v(\'Class\'), rdf/type, owl/\'Class\', schema), 
                 not(t(v(\'Class\'), dcog/tag, dcog/abstract, schema)), 
                 opt(t(v(\'Class\'), rdfs/label, v(\'Label\'), schema)), 
                 opt(t(v(\'Class\'), rdfs/comment, v(\'Comment\'), schema)), 
                 opt(t(v(\'Class\'), dcog/tag, v(\'Abstract\'), schema))))))'],Query),
    www_form_encode(Query,Encoded),
    
    atomic_list_concat(['curl -X GET \'',Server,'/terminus/woql?terminus%3Aquery=',Encoded,'&terminus%3Auser_key=root\''], Cmd),
    shell(Cmd).


run_schema_get_test :-
    config:server(Server),
    atomic_list_concat(['curl \'',Server,'/terminus/schema?terminus%3Auser_key=root&terminus%3Aencoding=terminus%3Aturtle\''], Cmd),
    shell(Cmd).
