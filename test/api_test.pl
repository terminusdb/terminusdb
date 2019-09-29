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
    try(run_bad_auth_test),
    % TERMINUS_QA_TEST (
    %   Creates DB for other tests
    try(run_db_create_test),
    try(run_schema_update_test),
    try(run_schema_get_test),
    try(run_doc_get_test),
    try(run_doc_update_test),
    try(run_doc_update_get_test),
    try(run_doc_update_update_test),
    try(run_doc_update_update_get_test), 
    try(run_doc_delete_test),
    try(run_doc_get_missing_test),
    %     INSTANCE_CHECKING (
    try(run_bad_comment_update_test),
    %     ) INSTANCE CHECKING
    try(run_db_delete_test),
    % ) TERMINUS_QA_TEST
    try(run_db_delete_nonexistent_test),
    try(run_doc_get_test),
    try(run_get_filled_frame_test),
    try(run_woql_test),
    try(run_woql_empty_error_test),
    try(run_woql_syntax_error_test).

/****************************************************************
 * Basic API tests
 ***************************************************************/

run_connect_test :-
    config:server(Server),

    Args = ['--user', ':root','-X','GET', Server],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'@type':"terminus:User"} :< Term.

run_bad_auth_test :-
    config:server(Server),

    Args = ['--user', ':flute','-X','GET', Server],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'terminus:status':"terminus:failure"} :< Term.

run_db_create_test :-
    % create DB
    config:server(Server),
    atomic_list_concat([Server,'/terminus_qa_test'], DB_URI),
    catch(
        api:try_delete_db(DB_URI),
        _,
        true),

    atomic_list_concat([DB_URI,'/document'], Document),
    atomic_list_concat([DB_URI,'/schema'], Schema),
    
    Doc = _{'@context': _{
                            rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                            terminus:"http://terminusdb.com/schema/terminus#"
                        },
            '@type':"terminus:APIUpdate",
            'terminus:document' : _{'@id': DB_URI, '@type':"terminus:Database",
                                    'rdfs:comment':_{'@language':"en", '@value':"dasd"},
                                    'rdfs:label':_{'@language':"en", '@value':"asdsda"},
                                    'terminus:allow_origin':_{'@type':"xsd:string", '@value':"*"},
                                    'terminus:instance':_{'@type':"xsd:string", '@value': Document},
                                    'terminus:schema':_{'@type':"xsd:string", '@value': Schema}
                                   },
            'terminus:user_key':"root"},

    with_output_to(
        string(Payload),
        json_write_dict(current_output, Doc, [])        
    ), 

    atomic_list_concat([Server,'/terminus_qa_test'], URI),
    Args = ['--user',':root','-d',Payload,'-H','Content-Type: application/json','-X','POST',URI],
    report_curl_command(Args),
    curl_json(Args,Term),

    nl,json_write_dict(current_output,Term,[]),
    Term = _{'terminus:status' : "terminus:success"}.

run_schema_update_test :-
    config:server(Server),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/terminus.owl.ttl'], TTL_File),
    
    atomic_list_concat([Server,'/terminus_qa_test/schema'], URI),

    read_file_to_string(TTL_File, String, []),
    Doc = _{
              'terminus:turtle': _{'@value': String, '@type' : "xsd:string"},
              'terminus:schema' : _{'@value': URI, '@type' : "xsd:string"},
              'terminus:user_key' : _{'@value': "root", '@type' : "xsd:string"}
           },

    with_output_to(
        string(Payload),
        json_write_dict(current_output, Doc, [])        
    ),


    Args = ['--user',':root','-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    Term = _{'terminus:status' : "terminus:success"}.
    
run_schema_get_test :-
    config:server(Server),
    atomic_list_concat([Server,'/terminus_qa/schema'],Schema),
    www_form_encode(Schema,S),
    atomic_list_concat([Server,'/terminus_qa_test/schema?terminus%3Aencoding=terminus%3Aturtle&terminus&3Aturtle&terminus&3Aschema=',S],URI),
    
    Args = ['--user',':root',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
    string(Term).

run_doc_get_test :-
    config:server(Server),
    atomic_list_concat([Server,'/terminus/document/admin'], URI),
    Args = ['--user', ':root','-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    json_write_dict(current_output,Term,[]),
    _{'@type' : "terminus:User"} :< Term.

run_db_delete_test :-
    config:server(Server),
    atomic_list_concat([Server,'/terminus_qa_test'],URI),
    Args = ['--user', ':root', '-X','DELETE', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
    _{'terminus:status' : "terminus:success"} :< Term.

run_get_filled_frame_test :-
    config:server(Server),
    atomic_list_concat([Server,'/terminus/document/terminus?terminus%3Aencoding=terminus%3Aframe'],
                       URI),
    Args = ['--user', ':root', '-X','GET', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    % This should not return a bare frame...
    is_list(Term).

run_doc_update_test :-
    % create DB
    config:server(Server),

    interpolate([Server,'/terminus_qa_test/document/'], Doc_Base),
    interpolate([Server,'/terminus_qa_test/schema/'], Scm_Base),

    Doc = _{'@type':"terminus:APIUpdate",
            'terminus:user_key':"root",
            'terminus:document' :
            _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc: Doc_Base,
                           ex:"http://example.org/",
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm: Scm_Base,
                           terminus:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
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
        
    Args = ['--user', ':root', '-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
    
    _{'terminus:status' : "terminus:success"} :< Term.

run_doc_update_get_test :-
    config:server(Server),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),
        
    Args = ['--user', ':root','-X','GET','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
    
    _{'@id':"doc:admin"} :< Term.

run_doc_update_update_test :-
    % create DB
    config:server(Server),

    interpolate([Server,'/terminus_qa_test/document/'], Doc_Base),
    interpolate([Server,'/terminus_qa_test/schema/'], Scm_Base),

    Doc = _{'@type':"terminus:APIUpdate",
            'terminus:user_key':"root",
            'terminus:document' :
            _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc: Doc_Base,
                           ex:"http://example.org/",
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm: Scm_Base,
                           terminus:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:admin",
              '@type':"terminus:User",
              'rdfs:comment':_{'@language':"en",
                               '@value':"This is a fake super user who has been changed"},
              'rdfs:label':_{'@language':"en",
                             '@value':"Server Admin User"}
             }
           },

    with_output_to(
        string(Payload),
        json_write(current_output, Doc, [])
    ),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),
        
    Args = ['--user', ':root','-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
        
    Term = _{'terminus:status' : "terminus:success"}.

run_doc_update_update_get_test :-
    config:server(Server),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),
        
    Args = ['--user', ':root','-X','GET','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{ 'rdfs:comment' :
       _{'@language':"en",
         '@value' : "This is a fake super user who has been changed"}} :< Term.


run_db_delete_nonexistent_test :-
    config:server(Server),

    % Need to set the user key correctly here or we will get a spurious error...
    atomic_list_concat([Server,'/dOeS_nOt_ExIsT'], URI),

    Args = ['--user', ':root', '-X','DELETE',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    % This should not be a bare error
    _{code : 500} :< Term.

run_doc_delete_test :-
        
    config:server(Server),

    % Need to set the user key correctly here or we will get a spurious error...
    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user', ':root','-X','DELETE',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
    
    _{'terminus:status' : "terminus:success"} :< Term.

run_doc_get_missing_test :-
    % create DB
    config:server(Server),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),
    
    Args = ['--user',':root','-X','GET','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'terminus:status':"terminus:failure"} :< Term.

/****************************************************************
 * Woql Tests
 ***************************************************************/

run_woql_test :-
    config:server(Server),
    atomic_list_concat([Server,'/terminus/document/'], Document),
    atomic_list_concat([Server,'/terminus/schema#'], Schema),
    atomic_list_concat([Server,'/terminus/'], Terminus),
    atomic_list_concat([Server,'/'], S),

    Query = 
    _{'@context' : _{scm : Schema,
                     doc : Document,
                     db : Terminus,
                     s : S},
      from: ["s:terminus",
             _{select: [
                   "v:Class", "v:Label", "v:Comment", "v:Abstract", 
                   _{and: [
                         _{quad: ["v:Class", "rdf:type", "owl:Class", "db:schema"]},
                         _{not: [_{quad: ["v:Class", "tcs:tag", "tcs:abstract", "db:schema"]}]},
                         _{opt: [_{quad: ["v:Class", "rdfs:label", "v:Label", "db:schema"]}]},
                         _{opt: [_{quad: ["v:Class", "rdfs:comment", "v:Comment", "db:schema"]}]},
                         _{opt: [_{quad: ["v:Class", "tcs:tag", "v:Abstract", "db:schema"]}]}
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
    atomic_list_concat([Server,'/terminus/woql?terminus%3Aquery=',Encoded], URI),
        
    Args = ['--user', ':root','-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
    
    (   _{'bindings' : L} :< Term
    ->  length(L, N),
        N >= 8
    ;   fail).

run_woql_empty_error_test :-
    config:server(Server),
    
    atomic_list_concat([Server,'/terminus/woql'], URI),
        
    Args = ['--user', ':root','-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
    
    _{'terminus:status':"terminus:failure"} :< Term.
    
run_woql_syntax_error_test :-
    config:server(Server),
    atomic_list_concat([Server,'/terminus/document/'], Document),
    atomic_list_concat([Server,'/terminus/schema#'], Schema),
    atomic_list_concat([Server,'/terminus/'], Terminus),
    atomic_list_concat([Server,'/'], S),

    Query = 
    _{'@context' : _{scm : Schema,
                     doc : Document,
                     db : Terminus,
                     e : "",
                     s : S},
      from: ["s:terminus",
             _{select: [
                   "v:Class", "v:Label", "v:Comment", "v:Abstract", 
                   _{fand: [
                         _{quad: ["v:Class", "rdf:type", "owl:Class", "db:schema"]},
                         _{not: [_{quad: ["v:Class", "tcs:tag", "tcs:abstract", "db:schema"]}]},
                         _{opt: [_{quad: ["v:Class", "rdfs:label", "v:Label", "db:schema"]}]},
                         _{opt: [_{quad: ["v:Class", "rdfs:comment", "v:Comment", "db:schema"]}]},
                         _{opt: [_{quad: ["v:Class", "tcs:tag", "v:Abstract", "db:schema"]}]}
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
    atomic_list_concat([Server,'/terminus/woql?terminus%3Aquery=',Encoded], URI),
        
    Args = ['--user', ':root','-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
    
    _{'@type':"vio:WOQLSyntaxError"} :< Term.

/****************************************************************
 * Instance Checking Tests
 ***************************************************************/

/* 
 * To run this we need to create a 'terminus_qa_test' database first...
 */
run_bad_comment_update_test :-
    % create DB
    config:server(Server),

    interpolate([Server,'/terminus_qa_test/document/'], Doc_Base),
    interpolate([Server,'/terminus_qa_test/schema/'], Scm_Base),

    Doc = _{'@type':"terminus:APIUpdate",
            'terminus:document' :
            _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc: Doc_Base,
                           ex:"http://example.org/",
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm: Scm_Base,
                           terminus:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:bad_admin",
              '@type':"terminus:User",
              'rdfs:comment':_{'@type':"xsd:integer",
                               '@value': 3},
              'rdfs:label':_{'@language':"en",
                             '@value':"A badly designed admin user"}
             }
           },

    with_output_to(
        string(Payload),
        json_write(current_output, Doc, [])
    ),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),
        
    Args = ['--user', ':root','-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],

    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
        
    _{'terminus:status':"terminus:failure",
      'terminus:witnesses': _W} :< Term.

run_bad_property_update_test :-
    % create DB
    config:server(Server),

    interpolate([Server,'/terminus_qa_test/document/'], Doc_Base),
    interpolate([Server,'/terminus_qa_test/schema/'], Scm_Base),

    Doc = _{'@type':"terminus:APIUpdate",
            'terminus:document' :
            _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc: Doc_Base,
                           ex:"http://example.org/",
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm: Scm_Base,
                           terminus:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:bad_admin",
              '@type':"terminus:User",
              'terminus:shmerminus':_{'@id' : 'doc:berminus'},
              'rdfs:comment':_{'@type':"xsd:integer",
                               '@value': 3},
              'rdfs:label':_{'@language':"en",
                             '@value':"A badly designed admin user"}
             }
           },

    with_output_to(
        string(Payload),
        json_write(current_output, Doc, [])
    ),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),
        
    Args = ['--user', ':root','-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],

    report_curl_command(Args),
    curl_json(Args,Term),    
    nl,json_write_dict(current_output,Term,[]),
        
    _{'terminus:status':"terminus:failure",
      'terminus:witnesses': _W} :< Term.

