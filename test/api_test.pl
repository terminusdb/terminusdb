:- module(api_test,[
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

:- begin_tests(api_test).

:- use_module(core(util/test_utils)).
:- use_module(core(util/utils)).
:- use_module(core(util/file_utils)).

:- use_module(core(triple/database_utils)).

:- use_module(server(routes)).

:- use_module(library(http/json)).



/****************************************************************
 * Basic API tests
 ***************************************************************/

auth(Auth) :-
    admin_pass(Pass),
    atomic_list_concat(['admin', ':',Pass],Auth).

test(run_connect_test) :-
    config:server(Server),
    auth(Auth),

    Args = ['--user', Auth,'-X','GET', Server],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'@type':"system:User"} :< Term.

test(run_bad_auth_test) :-
    config:server(Server),

    Args = ['--user', ':flute','-X','GET', Server],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status':"system:failure"} :< Term.

test(run_db_create_test) :-
    % create DB
    config:server(Server),
    auth(Auth),

    atomic_list_concat([Server,'/terminus_qa_test'], DB_URI),
    catch(
        api:try_delete_db('terminus_qa_test'),
        _,
        true),

    atomic_list_concat([DB_URI,'/document'], Document),
    atomic_list_concat([DB_URI,'/schema'], Schema),

    Doc = _{'@context': _{
                            rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                            system:"http://terminusdb.com/schema/terminus#"
                        },
            '@type':"system:APIUpdate",
            'system:base_uri' : "https://terminushub.com/",
            'system:document' : _{'@id': DB_URI, '@type':"system:Database",
                                    'rdfs:comment':_{'@language':"en", '@value':"dasd"},
                                    'rdfs:label':_{'@language':"en", '@value':"asdsda"},
                                    'system:allow_origin':_{'@type':"xsd:string", '@value':"*"},
                                    'system:instance':_{'@type':"xsd:string", '@value': Document},
                                    'system:schema':_{'@type':"xsd:string", '@value': Schema}
                                   },
            'system:user_key':"root"},

    with_output_to(
        string(Payload),
        json_write_dict(current_output, Doc, [])
    ),

    atomic_list_concat([Server,'/db/terminus_qa_test'], URI),
    Args = ['--user',Auth,'-d',Payload,'-H','Content-Type: application/json','-X','POST',URI],
    report_curl_command(Args),
    curl_json(Args,Term),

    nl,json_write_dict(current_output,Term,[]),
    Term = _{'system:status' : "system:success"}.

test(run_schema_update_test) :-
    config:server(Server),
    auth(Auth),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/terminus.owl.ttl'], TTL_File),

    atomic_list_concat([Server,'/terminus_qa_test/schema'], URI),

    read_file_to_string(TTL_File, String, []),
    Doc = _{
              'system:turtle': _{'@value': String, '@type' : "xsd:string"},
              'system:schema' : _{'@value': URI, '@type' : "xsd:string"}
           },

    with_output_to(
        string(Payload),
        json_write_dict(current_output, Doc, [])
    ),


    Args = ['--user',Auth,'-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    Term = _{'system:status' : "system:success"}.

test(run_schema_get_test) :-
    config:server(Server),
    auth(Auth),
    atomic_list_concat([Server,'/terminus_qa/schema'],Schema),
    www_form_encode(Schema,S),
    atomic_list_concat([Server,'/terminus_qa_test/schema?terminus%3Aencoding=terminus%3Aturtle&terminus&3Aturtle&terminus&3Aschema=',S],URI),

    Args = ['--user',Auth,URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,write(Term),
    string(Term).

test(run_doc_get_test) :-
    config:server(Server),
    auth(Auth),
    atomic_list_concat([Server,'/terminus/document/admin'], URI),
    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    json_write_dict(current_output,Term,[]),
    _{'@type' : "system:User"} :< Term.

test(run_db_delete_test) :-
    config:server(Server),
    auth(Auth),
    atomic_list_concat([Server,'/db/terminus_qa_test'],URI),
    Args = ['--user', Auth, '-X','DELETE', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),
    _{'system:status' : "system:success"} :< Term.

test(run_get_filled_frame_test) :-
    config:server(Server),
    auth(Auth),
    atomic_list_concat([Server,'/terminus/document/terminus?terminus%3Aencoding=terminus%3Aframe'],
                       URI),
    Args = ['--user', Auth, '-X','GET', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    % This should not return a bare frame...
    is_list(Term).

update_document(Doc) :-
    config:server(Server),

    format(string(Doc_Base), "~s~s", [Server,"/terminus_qa_test/document/"]),
    format(string(Scm_Base), "~s~s", [Server,"/terminus_qa_test/schema#"]),

    Doc = _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc: Doc_Base,
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm: Scm_Base,
                           system:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:admin",
              '@type':"system:User",
              'rdfs:comment':_{'@language':"en", '@value':"This is a fake super user"},
              'rdfs:label':_{'@language':"en", '@value':"Server Admin User"}
             }.

test(run_doc_update_test) :-
    % create DB
    config:server(Server),
    auth(Auth),

    test_update_document(Doc),

    with_output_to(
        string(Payload),
        json_write(current_output, _{'@type':"system:APIUpdate",
                                     'system:document' : Doc}, [])
    ),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user', Auth, '-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status' : "system:success"} :< Term.

test(run_doc_update_get_test) :-
    config:server(Server),
    auth(Auth),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user', Auth,'-X','GET','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    test_update_document(Doc),

    % original document simulates
    (   Term = Doc
    ->  true
    ;   format('~n~nNot structured as: ~n', []),
        json_write_dict(current_output,Doc,[]),
        false
    ).

test_update_document(Doc) :-
    config:server(Server),

    format(string(Doc_Base), "~s~s", [Server,"/terminus_qa_test/document/"]),
    format(string(Scm_Base), "~s~s", [Server,"/terminus_qa_test/schema#"]),

    Doc = _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc: Doc_Base,
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm: Scm_Base,
                           system:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:admin",
              '@type':"system:User",
              'rdfs:comment':_{'@language':"en",
                               '@value':"This is a fake super user who has been changed"},
              'rdfs:label':_{'@language':"en",
                             '@value':"Server Admin User"}
             }.


test(run_doc_update_update_test) :-
    % create DB
    config:server(Server),
    auth(Auth),

    test_update_update_document(Doc),

    with_output_to(
        string(Payload),
        json_write(current_output, _{'@type':"system:APIUpdate",
                                     'system:document' : Doc}, [])
    ),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user', Auth,'-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    Term = _{'system:status' : "system:success"}.

test(run_doc_update_update_get_test) :-
    config:server(Server),
    auth(Auth),

    test_update_update_document(Doc),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user', Auth,'-X','GET','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    (   Term = Doc
    ->  true
    ;   format('~n~nNot structured as: ~n', []),
        json_write_dict(current_output,Doc,[]),
        false
    ).

test(run_db_delete_nonexistent_test) :-
    config:server(Server),
    auth(Auth),

    % Need to set the user key correctly here or we will get a spurious error...
    atomic_list_concat([Server,'/db/dOeS_nOt_ExIsT'], URI),

    Args = ['--user', Auth, '-X','DELETE',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    % This should not be a bare error
    _{'system:status':"system:failure"} :< Term.

test(run_doc_delete_test) :-

    config:server(Server),
    auth(Auth),

    % Need to set the user key correctly here or we will get a spurious error...
    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user', Auth,'-X','DELETE',URI],

    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status' : "system:success"} :< Term.

test(run_doc_get_missing_test) :-
    % create DB
    config:server(Server),
    auth(Auth),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user',Auth,'-X','GET','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status':"system:failure"} :< Term.

/****************************************************************
 * Woql Tests
 ***************************************************************/

test(run_woql_test) :-
    config:server(Server),
    auth(Auth),
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

    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    (   _{'bindings' : L} :< Term
    ->  length(L, N),
        N >= 8
    ;   fail).


test(run_woql_external_file_test) :-
    config:server(Server),
    auth(Auth),

    Query = _{'@context':_{'rdf':"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           'rdfs':"http://www.w3.org/2000/01/rdf-schema#",
                           'xsd':"http://www.w3.org/2001/XMLSchema#",
                           'owl':"http://www.w3.org/2002/07/owl#",
                           'tcs':"http://terminusdb.com/schema/tcs#",
                           'xdd':"http://terminusdb.com/schema/xdd#",
                           'v':"http://terminusdb.com/woql/variable/",
                           'terminus':"http://terminusdb.com/schema/terminus#",
                           'vio':"http://terminusdb.com/schema/vio#",
                           'doc':"http://localhost:6363/terminus/document/",
                           'scm':"http://localhost:6363/terminus/schema#",
                           'db':"http://localhost:6363/terminus/"},
              'get':[[_{'as':[_{'@value':"councillor_a"},"v:Rep_A"]},
                      _{'as':[_{'@value':"councillor_b"},"v:Rep_B"]},
                      _{'as':[_{'@value':"party_a"},"v:Party_A"]},
                      _{'as':[_{'@value':"party_b"},"v:Party_B"]},
                      _{'as':[_{'@value':"distance"},"v:Distance"]}],
                     _{'remote':["https://terminusdb.com/t/data/council/weighted_similarity.csv"]}
                    ]},

    with_output_to(
        string(Payload),
        json_write(current_output, Query, [])
    ),

    www_form_encode(Payload, Encoded),
    atomic_list_concat([Server,'/terminus/woql', '?terminus%3Aquery=', Encoded], URI),
    Args = ['-s', '--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]).

test(run_woql_empty_error_test) :-
    config:server(Server),
    auth(Auth),

    atomic_list_concat([Server,'/terminus/woql'], URI),

    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status':"system:failure"} :< Term.

test(run_woql_syntax_error_test) :-
    config:server(Server),
    auth(Auth),
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

    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'@type':"vio:WOQLSyntaxError"} :< Term.

% Requires terminus_qa_test!
test(run_woql_update_test) :-
    config:server(Server),
    auth(Auth),
    atomic_list_concat([Server,'/terminus_qa_test/document/'], Doc_Base),
    atomic_list_concat([Server,'/terminus_qa_test/schema#'], Scm_Base),
    atomic_list_concat([Server,'/terminus_qa_test/'], QA),
    atomic_list_concat([Server,'/'], S),

    Document = _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                              tbs:"http://terminusdb.com/schema/tbs#",
                              doc: Doc_Base,
                              ex:"http://example.org/",
                              owl:"http://www.w3.org/2002/07/owl#",
                              rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                              rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                              vio:"http://terminusdb.com/schema/vio#",
                              scm: Scm_Base,
                              system:"http://terminusdb.com/schema/terminus#",
                              xdd:"http://terminusdb.com/schema/xdd#",
                              xsd:"http://www.w3.org/2001/XMLSchema#"},
                 '@id':"doc:admin",
                 '@type':"system:User",
                 'rdfs:comment':_{'@language':"en", '@value':"A WOQL updated superuser"},
                 'rdfs:label':_{'@language':"en", '@value':"Server Admin User"}
                },
    Query =
    _{'@context' : _{scm : Scm_Base,
                     doc : Doc_Base,
                     db : QA,
                     s : S},
      from: ["s:terminus_qa_test",
             _{into: ["db:document",
                      _{when: [_{true: []},
                               _{update: [Document]}
                              ]}
                     ]}
            ]},

    with_output_to(
        string(Payload),
        json_write(current_output, Query, [])
    ),

    www_form_encode(Payload,Encoded),
    atomic_list_concat([Server,'/terminus_qa_test/woql?terminus%3Aquery=',Encoded], URI),

    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'bindings' : [ _{} ]} :< Term.

% Requires terminus_qa_test!
test(run_woql_verify_update_test) :-
    config:server(Server),
    auth(Auth),
    atomic_list_concat([Server,'/terminus_qa_test/document/'], Document),
    atomic_list_concat([Server,'/terminus_qa_test/schema#'], Schema),
    atomic_list_concat([Server,'/terminus_qa_test/'], QA),
    atomic_list_concat([Server,'/'], S),

    Query =
    _{'@context' : _{scm : Schema,
                     doc : Document,
                     db : QA,
                     s : S},
      from: ["s:terminus_qa_test",
             _{triple: [ "doc:admin", "rdfs:comment", "v:comment"]}
            ]},

    with_output_to(
        string(Payload),
        json_write(current_output, Query, [])
    ),

    www_form_encode(Payload,Encoded),
    atomic_list_concat([Server,'/terminus_qa_test/woql?terminus%3Aquery=',Encoded], URI),

    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),


    _{'bindings' : [ _{
                         'http://terminusdb.com/woql/variable/comment':
                         _{'@language':"en",'@value':"A WOQL updated superuser"}}
                   ]}  :< Term.


test(run_woql_csv_test) :-
    config:server(Server),
    auth(Auth),
    terminus_path(Path),
    atomic_list_concat([Server,'/terminus/document/'], Document),
    atomic_list_concat([Server,'/terminus/schema#'], Schema),
    atomic_list_concat([Server,'/terminus/'], Terminus),
    atomic_list_concat([Server,'/'], S),
    atomic_list_concat([Path,'/test/0CE.csv'],CSV),
    Query =
    _{'@context' : _{scm : Schema,
                     doc : Document,
                     db : Terminus,
                     s : S},
      limit : [2,
               _{get : [ [_{as : [_{'@value' : "Polity_nam"}, "v:Polity_Name"]},
                          _{as : [_{'@value' : "fid"},        "v:Polygon"]}],
                         _{file : [CSV]}
                       ]}
              ]},

    with_output_to(
        string(Payload),
        json_write(current_output, Query, [])
    ),

    www_form_encode(Payload,Encoded),
    atomic_list_concat([Server,'/terminus/woql?terminus%3Aquery=',Encoded], URI),

    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{bindings : [
          _{
              'http://terminusdb.com/woql/variable/Polity_Name' :
              _{'@type' : "http://www.w3.org/2001/XMLSchema#string",
                '@value' : "ItRomPr"}
              ,
              'http://terminusdb.com/woql/variable/Polygon':
              _{'@type' : "http://www.w3.org/2001/XMLSchema#decimal",
                '@value' : 1}
          },
          _{
              'http://terminusdb.com/woql/variable/Polity_Name':
              _{'@type' : "http://www.w3.org/2001/XMLSchema#string",
                '@value' : "ItRomPr"}
              ,
              'http://terminusdb.com/woql/variable/Polygon':
              _{'@type' : "http://www.w3.org/2001/XMLSchema#decimal",
                '@value' : 1}
          }
      ]} :< Term.

test(run_woql_instantiation_test) :-
    config:server(Server),
    auth(Auth),

    Query = _{trim: ["v:Something_to_Trim","v:Output"]},

    with_output_to(
        string(Payload),
        json_write(current_output, Query, [])
    ),

    www_form_encode(Payload,Encoded),
    atomic_list_concat([Server,'/terminus/woql?terminus%3Aquery=',Encoded], URI),

    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    % test here.
    _{
        'system:message' : _,
        'system:status' :"system:failure"
    } :< Term.

test(run_woql_re_test) :-
    config:server(Server),
    auth(Auth),

    Query =  _{re: [_{'@value': ".*/candidate/(.*)",'@type': "xsd:string"},
                    "/candidate/asdfadsf",
                    _{list: ["v:AllDI","v:IDURL_Extension"]}]},

    with_output_to(
        string(Payload),
        json_write(current_output, Query, [])
    ),

    www_form_encode(Payload,Encoded),
    atomic_list_concat([Server,'/terminus/woql?terminus%3Aquery=',Encoded], URI),

    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{bindings: [
          _{'http://terminusdb.com/woql/variable/AllDI':
            _{'@type':"http://www.w3.org/2001/XMLSchema#string",
	          '@value':"http://terminusdb.com/woql#/candidate/asdfadsf"},
            'http://terminusdb.com/woql/variable/IDURL_Extension':
            _{'@type':"http://www.w3.org/2001/XMLSchema#string",
	          '@value':"asdfadsf"}}
      ]} :< Term.

test(run_woql_typecast_test) :-
    config:server(Server),
    auth(Auth),

    Query =  _{and : [_{typecast: [_{'@value': "[1,2]",'@type': "xsd:string"},
                                   "http://terminusdb.com/schema/xdd#integerRange",
                                   "v:Res1"]},
                      _{typecast: [_{'@value': "[1.1,2.2]",'@type': "xsd:string"},
                                   "http://terminusdb.com/schema/xdd#decimalRange",
                                   "v:Res2"]}]},

    with_output_to(
        string(Payload),
        json_write(current_output, Query, [])
    ),

    www_form_encode(Payload,Encoded),
    atomic_list_concat([Server,'/terminus/woql?terminus%3Aquery=',Encoded], URI),

    Args = ['--user', Auth,'-X','GET',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{bindings:[_{'http://terminusdb.com/woql/variable/Res1':
                  _{'@type':"http://terminusdb.com/schema/xdd#integerRange",'@value':"[1,2]"},
                  'http://terminusdb.com/woql/variable/Res2':
                  _{'@type':"http://terminusdb.com/schema/xdd#decimalRange",'@value':"[1.1,2.2]"}}
               ]} :< Term.


test(run_woql_file_upload ) :-
    config:server(Server),
    auth(Auth),

    atomic_list_concat([Server,'/terminus/woql'], URI),

    % Expand into a get from post.
    Query =  _{get : [ [_{as : [_{'@value' : "col 2"}, "v:Col_2"]}],
                       _{post : ["my_json_file", _{type : "panda_json"}]}
                     ]},

    with_output_to(
        string(Payload),
        json_write(current_output, Query, [])
    ),


    atomic_list_concat(['system:query=',Payload,';type=application/json'], Q),

    atomic_list_concat(['my_json_file=@"test/simple.json"'], Upload),

    Args = ['--user', Auth,'-F',Q,'-F',Upload,'-X','POST',URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{bindings:
      [_{'http://terminusdb.com/woql/variable/Col_2':
         _{'@type':"http://www.w3.org/2001/XMLSchema#string",
	       '@value':"b"
          }
        },
       _{'http://terminusdb.com/woql/variable/Col_2':
         _{'@type':"http://www.w3.org/2001/XMLSchema#string",
	       '@value':"d"
          }
        }
      ]} :< Term.

/****************************************************************
 * Instance Checking Tests
 ***************************************************************/

/*
 * To run this we need to create a 'terminus_qa_test' database first...
 */
test(run_bad_comment_update_test) :-
    % create DB
    config:server(Server),
    auth(Auth),

    interpolate([Server,'/terminus_qa_test/document/'], Doc_Base),
    interpolate([Server,'/terminus_qa_test/schema/'], Scm_Base),

    Doc = _{'@type':"system:APIUpdate",
            'system:document' :
            _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc: Doc_Base,
                           ex:"http://example.org/",
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm: Scm_Base,
                           system:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:bad_admin",
              '@type':"system:User",
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

    Args = ['--user', Auth,'-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],

    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status':"system:failure",
      'system:witnesses': _W} :< Term.

test(run_bad_property_update_test) :-
    % create DB
    config:server(Server),
    auth(Auth),

    interpolate([Server,'/terminus_qa_test/document/'], Doc_Base),
    interpolate([Server,'/terminus_qa_test/schema/'], Scm_Base),

    Doc = _{'@type':"system:APIUpdate",
            'system:document' :
            _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc: Doc_Base,
                           ex:"http://example.org/",
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm: Scm_Base,
                           system:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:bad_admin",
              '@type':"system:User",
              'system:shmerminus':_{'@id' : 'doc:berminus'},
              'rdfs:comment':_{'@language':"en",
                               '@value': "doinky doink"},
              'rdfs:label':_{'@language':"en",
                             '@value':"A badly designed admin user"}
             }
           },

    with_output_to(
        string(Payload),
        json_write(current_output, Doc, [])
    ),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user', Auth,'-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],

    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status':"system:failure",
      'system:witnesses': _W} :< Term.

/*************************************
 * Datatypes schema instance checking.
 *
 * Need to load a fresh database for this with run_db_create_test/0
 * followed by run_db_delete_test/0
 */
test(run_schema_datatypes_update_test) :-
    config:server(Server),
    auth(Auth),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/examples/datatypes.owl.ttl'], TTL_File),

    atomic_list_concat([Server,'/terminus_qa_test/schema'], URI),

    read_file_to_string(TTL_File, String, []),
    Doc = _{
              'system:turtle': _{'@value': String, '@type' : "xsd:string"},
              'system:schema' : _{'@value': URI, '@type' : "xsd:string"}
           },

    with_output_to(
        string(Payload),
        json_write_dict(current_output, Doc, [])
    ),


    Args = ['--user',Auth,'-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    Term = _{'system:status' : "system:success"}.

test(run_bad_doc_datatype_update_test) :-
    config:server(Server),
    auth(Auth),

    interpolate([Server,'/terminus_qa_test/document/'], Doc_Base),
    interpolate([Server,'/terminus_qa_test/schema/'], Scm_Base),

    Doc = _{'@type':"system:APIUpdate",
            'system:document' :
            _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc:Doc_Base,
                           ex:"http://example.org/",
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm:Scm_Base,
                           datatypes:"http://terminusdb.com/schema/datatypes#",
                           system:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:bad_data",
              '@type':"datatypes:DatatypeHolder",
              'rdfs:comment':_{'@language':"en",
                               '@value': "A very bad document"},
              'rdfs:label':_{'@language':"en",
                             '@value':"Bad Doc"},
              'datatypes:coord':_{'@type' : "xdd:coordinate",
                                  '@value' : "fdas"},
              'datatypes:coordline':_{'@type' : "xdd:coordinatePolyline",
                                      '@value' : "fdas"},
              'datatypes:coordpoly':_{'@type' : "xdd:coordinatePolygon",
                                      '@value' : "goop"},
              'datatypes:dateRange':_{'@type' : "xdd:dateRange",
                                      '@value' : "1-2"},
              'datatypes:gYearRange':_{'@type' : "xdd:gYearRange",
                                       '@value' : "a-c"},
              'datatypes:integerRange':_{'@type' : "xdd:integerRange",
                                         '@value' : "[1,k]"},
              'datatypes:decimalRange':_{'@type' : "xdd:decimalRange",
                                         '@value' : "asdf"},
              'datatypes:email':_{'@type' : "xdd:email",
                                  '@value' : 2},
              'datatypes:html':_{'@type' : "xdd:html",
                                 '@value' : 1},
              'datatypes:url':_{'@type' : "xdd:url",
                                '@value' : 3},
              'datatypes:boolean':_{'@type' : "xsd:boolean",
                                    '@value' : "asdf"},
              'datatypes:decimal':_{'@type' : "xsd:decimal",
                                    '@value' : "asdf"},
              'datatypes:double':_{'@type' : "xsd:double",
                                   '@value' : "asdf"},
              'datatypes:float':_{'@type' : "xsd:float",
                                  '@value' : "a float wat?"},
              'datatypes:time':_{'@type' : "xsd:time",
                                 '@value' : "that time again"},
              'datatypes:date':_{'@type' : "xsd:date",
                                 '@value' : "the date"},
              'datatypes:dateTime':_{'@type' : "xsd:dateTime",
                                     '@value' : "the dateTime"},
              'datatypes:dateTimeStamp':_{'@type' : "xsd:dateTimeStamp",
                                          '@value' : "the dateTimeStamp"},
              'datatypes:gYearMonth':_{'@type' : "xsd:gYearMonth",
                                       '@value' : "Yar"},
              'datatypes:gMonth':_{'@type' : "xsd:gMonth",
                                   '@value' : "ggggMaonth"},
              'datatypes:json':_{'@type' : "xdd:json",
                                 '@value' : "{"}
             }
           },

    with_output_to(
        string(Payload),
        json_write(current_output, Doc, [])
    ),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user', Auth,'-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],

    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status':"system:failure",
      'system:witnesses': W} :< Term,

    length(W,18).

test(run_good_doc_datatype_update_test) :-
    config:server(Server),
    auth(Auth),

    interpolate([Server,'/terminus_qa_test/document/'], Doc_Base),
    interpolate([Server,'/terminus_qa_test/schema/'], Scm_Base),

    Doc = _{'@type':"system:APIUpdate",
            'system:document' :
            _{'@context':_{tcs:"http://terminusdb.com/schema/tcs#",
                           tbs:"http://terminusdb.com/schema/tbs#",
                           doc:Doc_Base,
                           ex:"http://example.org/",
                           owl:"http://www.w3.org/2002/07/owl#",
                           rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                           rdfs:"http://www.w3.org/2000/01/rdf-schema#",
                           vio:"http://terminusdb.com/schema/vio#",
                           scm:Scm_Base,
                           datatypes:"http://terminusdb.com/schema/datatypes#",
                           system:"http://terminusdb.com/schema/terminus#",
                           xdd:"http://terminusdb.com/schema/xdd#",
                           xsd:"http://www.w3.org/2001/XMLSchema#"},
              '@id':"doc:bad_data",
              '@type':"datatypes:DatatypeHolder",
              'rdfs:comment':_{'@language':"en",
                               '@value': "A healthy document"},
              'rdfs:label':_{'@language':"en",
                             '@value':"Healthy Document"},
              'datatypes:coord':_{'@type' : "xdd:coordinate",
                                  '@value' : "[ 32.0 , 41.0 ]"},
              'datatypes:coordline':_{'@type' : "xdd:coordinatePolyline",
                                      '@value' : "[ [ 32.0 , 41.0 ] ]"},
              'datatypes:coordpoly':_{'@type' : "xdd:coordinatePolygon",
                                      '@value' : "[ [ 32.0 , 41.0 ] ]"},
              'datatypes:dateRange':_{'@type' : "xdd:dateRange",
                                      '@value' : "[ 2019-08-02 , 2019-08-03 ]"},
              'datatypes:gYearRange':_{'@type' : "xdd:gYearRange",
                                       '@value' : "[ 2017 , 2018 ]"},
              'datatypes:integerRange':_{'@type' : "xdd:integerRange",
                                         '@value' : "[ 1 , 12]"},
              'datatypes:decimalRange':_{'@type' : "xdd:decimalRange",
                                         '@value' : "[ 1.0 , 33 ]"},
              'datatypes:email':_{'@type' : "xdd:email",
                                  '@value' : "gavin@datachemist.com"},
              'datatypes:html':_{'@type' : "xdd:html",
                                 '@value' : "<html></html>"},
              'datatypes:url':_{'@type' : "xdd:url",
                                '@value' : "http://terminusdb.com"},
              'datatypes:boolean':_{'@type' : "xsd:boolean",
                                    '@value' : true},
              'datatypes:decimal':_{'@type' : "xsd:decimal",
                                    '@value' : "12.5"},
              'datatypes:double':_{'@type' : "xsd:double",
                                   '@value' : "NAN" },
              'datatypes:float':_{'@type' : "xsd:float",
                                  '@value' : "12.2343E-7"},
              'datatypes:time':_{'@type' : "xsd:time",
                                 '@value' : "12:34:00"},
              'datatypes:date':_{'@type' : "xsd:date",
                                 '@value' : "2019-05-29"},
              'datatypes:dateTime':_{'@type' : "xsd:dateTime",
                                     '@value' : "2019-05-29T12:34:00"},
              'datatypes:dateTimeStamp':_{'@type' : "xsd:dateTimeStamp",
                                          '@value' : "3243"},
              'datatypes:gYearMonth':_{'@type' : "xsd:gYearMonth",
                                       '@value' : "2018-05"},
              'datatypes:gMonth':_{'@type' : "xsd:gMonth",
                                   '@value' : "--03"},
              'datatypes:json':_{'@type' : "xdd:json",
                                 '@value' : "{\"asdf\" : \"fdas\"}"}
             }
           },

    with_output_to(
        string(Payload),
        json_write(current_output, Doc, [])
    ),

    atomic_list_concat([Server,'/terminus_qa_test/document/admin'], URI),

    Args = ['--user', Auth,'-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],

    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status':"system:success"} :< Term.
    % need to check witnesses but we throw an error at insert leaving only witness!

test(run_console) :-
    config:server(Server),
    atomic_list_concat([Server,'/console'], URI),
    status_200(URI).

% TODO: need an order by test here.
test(run_woql_order_by) :-
    true.

test(run_db_metadata_test, [blocked("Blocked for some reason")] ) :-
    config:server(Server),
    auth(Auth),

    atomic_list_concat([Server,'/terminus/metadata'], URI),

    Args = ['--user', Auth,'-X','GET','-H','Content-Type: application/json', URI],
    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{ '@type' : "system:DatabaseMetadata"} :< Term.

test(run_message_test) :-
    config:server(Server),
    auth(Auth),

    atomic_list_concat([Server,'/message'], URI),

    Message = _{'@type':"system:APIUpdate",
                'system:message' : _{some : "Payload"}
               },

    with_output_to(
        string(Payload),
        json_write(current_output, Message, [])
    ),

    Args = ['--user', Auth,'-d',Payload,'-X','POST','-H','Content-Type: application/json', URI],

    report_curl_command(Args),
    curl_json(Args,Term),
    nl,json_write_dict(current_output,Term,[]),

    _{'system:status' : "system:success"} :< Term.

:- end_tests(api_test).
