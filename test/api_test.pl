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
:- use_module(library(file_utils)).
:- use_module(library(database_utils)).
:- use_module(library(api)).
:- use_module(library(http/json)).


/* 
 * run_api_tests is det. 
 *
 * Run all structured tests of the API
 */ 
run_api_tests :-
    try(run_connect_test),
    % ( Creates DB for other tests
    try(run_db_create_test),
    try(run_schema_update_test),
    try(run_schema_get_test),
    try(run_db_delete_test),
    %   grouped )
    try(run_get_filled_frame_test),
    try(run_woql_test).

try(Goal) :- 
    (   call(Goal)
    ->  true
    ;   format('~n*************************************~nFAIL! Could not successfully run ~s~n', [Goal])).

run_connect_test :-
    config:server(Server),

    atomic_list_concat(['curl -X GET ',Server,'/?terminus:user_key=root'], Cmd),
    shell(Cmd).

run_db_create_test :-
    % create DB
    config:server(Server),
    atomic_list_concat([Server,'/terminus_qa_test'], DB_URI),
    catch(
        api:try_delete_db(DB_URI),
        _,
        true),

    Doc = _{'@context': _{
                            rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                            terminus:"https://datachemist.net/ontology/terminus#"
                        },
            '@type':"terminus:APIUpdate",
            'terminus:document' : _{'@id': DB_URI, '@type':"terminus:Database",
                                    'rdfs:comment':_{'@language':"en", '@value':"dasd"},
                                    'rdfs:label':_{'@language':"en", '@value':"asdsda"},
                                    'terminus:allow_origin':_{'@type':"xsd:string", '@value':"*"},
                                    'terminus:document':_{'@type':"xsd:string", '@value':"document"},
                                    'terminus:schema':_{'@type':"xsd:string", '@value':"schema"}
                                   },
            'terminus:user_key':"root"},
    
    with_output_to(
        string(Payload),
        json_write(current_output, Doc, [])        
    ),

    atomic_list_concat(['curl -d \'',Payload,'\' -H "Content-Type: application/json" -X POST ',Server,'/terminus_qa_test'], Cmd),
    
    format('~nRunning command: "~s"~n',[Cmd]),        
    shell(Cmd).

run_schema_update_test :-
    config:server(Server),

    terminus_path(Path),
    interpolate([Path, '/test/geo.ttl'], TTL_File),

    read_file_to_string(TTL_File, String, []),
    Doc = _{'terminus:turtle': _{'@value': String, '@type' : "xsd:string"},
            'terminus:schema' : _{'@value': "schema", '@type' : "xsd:string"},
            'terminus:user_key' : _{'@value': "root", '@type' : "xsd:string"}
           },
    
    with_output_to(
        string(Payload),
        json_write(current_output, Doc, [])        
    ),

    atomic_list_concat([Server,'/terminus_qa_test/schema'], URI),

    Args = ['-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    
    format('~nRunning command: curl "~q"~n',[Args]),        

    % process_create avoids shell escaping complexities. 
    process_create(path(curl), Args,
                   [ stdout(std),
                     stderr(null),
                     process(PID)
                   ]),
    process_wait(PID,Status),
    (   Status=killed(Signal)
    ->  interpolate(["curl killed with signal ",Signal], M),
        throw(error(M))
    ;   true),

    % We need to test the response here! It should be a vio witness list.
    * close(_Result).
    
run_schema_get_test :-
    config:server(Server),
    atomic_list_concat(['curl \'',Server,'/terminus_qa_test/schema?terminus%3Auser_key=root&terminus%3Aencoding=terminus%3Aturtle\''], Cmd),
    shell(Cmd).

run_db_delete_test :-
    % create DB
    config:server(Server),

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
