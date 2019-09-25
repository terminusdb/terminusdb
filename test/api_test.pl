:- module(api_test,[
              run_api_tests/0
          ]).
                 
/** <module> API Test
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

:- use_module(test(test_utils)).
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
    try(run_doc_update_test),
    try(run_doc_delete_test),
    try(run_doc_get_missing_test),
    try(run_db_delete_test),
    %   grouped )
    try(run_doc_get_test),
    try(run_get_filled_frame_test),
    try(run_woql_test).

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
                                    'terminus:instance':_{'@type':"xsd:string", '@value':"document"},
                                    'terminus:schema':_{'@type':"xsd:string", '@value':"schema"}
                                   },
            'terminus:user_key':"root"},
    
    with_output_to(
        string(Payload),
        json_write_dict(current_output, Doc, [])        
    ),

    atomic_list_concat(['curl -d \'',Payload,'\' -H "Content-Type: application/json" -X POST ',Server,'/terminus_qa_test'], Cmd),
    
    format('~nRunning command: "~s"~n',[Cmd]),        
    shell(Cmd).

run_schema_update_test :-
    config:server(Server),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/terminus.owl.ttl'], TTL_File),

    read_file_to_string(TTL_File, String, []),
    Doc = _{
              'terminus:turtle': _{'@value': String, '@type' : "xsd:string"},
              'terminus:schema' : _{'@value': "schema", '@type' : "xsd:string"},
              'terminus:user_key' : _{'@value': "root", '@type' : "xsd:string"}
           },

    with_output_to(
        string(Payload),
        json_write_dict(current_output, Doc, [])        
    ),

    atomic_list_concat([Server,'/terminus_qa_test/schema'], URI),

    Args = ['-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    
    format('~nRunning command: curl -X POST ~s...~n',[URI]),

    % process_create avoids shell escaping complexities. 
    process_create(path(curl), Args,
                   [ stdout(pipe(Out)),
                     stderr(null),
                     process(PID)
                   ]),
    process_wait(PID,Status),
    (   Status=killed(Signal)
    ->  interpolate(["curl killed with signal ",Signal], M),
        throw(error(M))
    ;   true),

    json_read_dict(Out, Term),
    (   Term = _{'terminus:status' : "terminus:success"}
    ->  true
    ;   json_write_dict(current_output,Term,[]),
        fail),
    
    close(Out).
    
run_schema_get_test :-
    config:server(Server),
    atomic_list_concat(['curl \'',Server,'/terminus_qa_test/schema?terminus%3Auser_key=root&terminus%3Aencoding=terminus%3Aturtle\''], Cmd),
    shell(Cmd).

run_doc_get_test :-
    % create DB
    config:server(Server),
    atomic_list_concat(['curl -X GET \'',Server,'/terminus/document/admin?terminus%3Auser_key=root\''], Cmd),
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

run_doc_update_test :-
    % create DB
    config:server(Server),

    Doc = _{'@type':"terminus:APIUpdate",
            'terminus:user_key':"root",
            'terminus:document' :
            _{'@context':_{dcog:"https://datachemist.net/ontology/dcog#",
                           dcogbox:"https://datachemist.net/ontology/dcogbox#",
                           doc:"http://localhost:6363/terminus/document/",
                           ex:"http://example.org/",
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           rvo:"https://datachemist.net/ontology/rvo#",
                           scm:"http://localhost:6363/terminus/schema/",
                           terminus:"https://datachemist.net/ontology/terminus#",
                           xdd:"https://datachemist.net/ontology/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:admin",
              '@type':"terminus:User",
              'rdfs:comment':_{'@language':"en", '@value':"This is a fake super user"},
              'rdfs:label':_{'@language':"en", '@value':"Server Admin User"}
             }
           },

    with_output_to(
        string(Payload),
        json_write(current_output, Doc, [])
    ),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),
        
    Args = ['-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    
    format('~nRunning command: curl -X POST ~s...~n',[URI]),        

    % process_create avoids shell escaping complexities. 
    process_create(path(curl), Args,
                   [ stdout(pipe(Out)),
                     stderr(null),
                     process(PID)
                   ]),
    
    process_wait(PID,Status),
    
    (   Status=killed(Signal)
    ->  interpolate(["curl killed with signal ",Signal], M),
        throw(error(M))
    ;   true),

    json_read_dict(Out, Term),

    (   Term = _{'terminus:status' : "terminus:success"}
    ->  true
    ;   json_write_dict(current_output,Term,[]),
        fail).

run_doc_delete_test :-
    % create DB
    config:server(Server),
    atomic_list_concat(['curl -X DELETE \'',Server,'/terminus_qa_test/document/admin?terminus%3Auser_key=root\''], Cmd),
    shell(Cmd).

run_doc_get_missing_test :-
    % create DB
    config:server(Server),
    atomic_list_concat(['curl -X GET \'',Server,'/terminus_qa_test/document/admin?terminus%3Auser_key=root\''], Cmd),
    \+ shell(Cmd).

run_get_doc_test :-
    % create DB
    config:server(Server),
    atomic_list_concat(['curl -X GET \'',Server,'/terminus/document/terminus?terminus%3Auser_key=root\''], Cmd),
    shell(Cmd).

run_woql_test :-
    config:server(Server),
    atomic_list_concat([Server,'/terminus/document/'], Document),
    atomic_list_concat([Server,'/terminus/schema#'], Schema),
    atomic_list_concat([Server,'/terminus/'], Terminus),
    atomic_list_concat([Server,'/'], G),

    Query = 
    _{'@context' : _{s : Schema,
                     doc : Document,
                     db : Terminus,
                     e : "",
                     g : G},
      from: ["g:terminus",
             _{select: [
                   "v:Class", "v:Label", "v:Comment", "v:Abstract", 
                   _{and: [
                         _{triple: ["v:Class", "rdf:type", "owl:Class", "e:schema"]},
                         _{not: [_{triple: ["v:Class", "dcog:tag", "dcog:abstract", "e:schema"]}]},
                         _{opt: [_{triple: ["v:Class", "rdfs:label", "v:Label", "e:schema"]}]},
                         _{opt: [_{triple: ["v:Class", "rdfs:comment", "v:Comment", "e:schema"]}]},
                         _{opt: [_{triple: ["v:Class", "dcog:tag", "v:Abstract", "e:schema"]}]}
                     ]
                    }
               ]
              }
            ]
     },
    
    with_output_to(
        string(Payload),
        json_write(current_output, Query, [])
    ),

    www_form_encode(Payload,Encoded),
    
    atomic_list_concat(['curl -X GET \'',Server,'/terminus/woql?terminus%3Aquery=',Encoded,'&terminus%3Auser_key=root\''], Cmd),
    shell(Cmd).
