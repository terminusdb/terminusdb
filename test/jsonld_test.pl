:- module(jsonld_test,[
              run_jsonld_tests/0
          ]).
                 
/** <module> JSON-LD Test
 * 
 * Tests of the JSON-LD libraries. The JSON-LD "@context" manipulation is 
 * relatively complex we need to have some sanity checks to make sure that 
 * things don't go too haywire. This is also complex because there are 
 * numerous equivalent forms. 
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
:- use_module(library(jsonld)).

run_jsonld_tests :-
    try(run_woql_expand).

run_woql_expand :- 
    Doc = _{'@context' : 
            _{'@version': 1.1,
              '@vocab': "http://terminusdb.com/woql#", 
              'rdf' : "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
              'rdfs' : "http://www.w3.org/2000/01/rdf-schema#",
              'dcog' : "https://datachemist.net/ontology/dcog#",
              'v' : 'http://terminusdb.com/woql/variable/',
              'select' : _{'@type' : "@id"},
              'from' : _{'@type' : "@id"}, 
              'and' : _{'@type' : "@id"},
              'triple' : _{'@type' : "@id"},   
              'eq' : _{'@type' : "@id" }, 
              'sub' : _{'@type' : "@id" },
              'opt' : _{'@type' : "@id"}
             },
            'select' : [["v:arg1"],
                        _{'and' : [_{'triple': ["v:a", "v:b", "v:c"]},
                                   _{'triple' :["v:d", "v:e", "v:f"]}]}]
           },
    % We better hope this is canonical!
    expand(Doc,Expanded),
    % remove the context for simplicity (and because it may be expanded)
    select_dict(_{'@context' : _Ctx}, Expanded, Query),
    format('~n~q', [Query]),
    Query = _.


    
