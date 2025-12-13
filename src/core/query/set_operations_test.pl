/** <module> Set Operations Tests
 *
 * Tests for WOQL set operations: set_difference, set_intersection,
 * set_union, set_member, and list_to_set.
 *
 * These operations use Prolog's ordsets library for O(n log n) performance.
 */

:- begin_tests(set_operations, []).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(yall)).
:- use_module(core(util/test_utils)).
:- use_module(core(api)).
:- use_module(core(query)).
:- use_module(core(query/query_response), [run_context_ast_jsonld_response/5]).
:- use_module(core(query/json_woql), [json_woql/2]).
:- use_module(core(triple)).
:- use_module(core(transaction)).

% Define assertion/1 locally to satisfy linter (plunit provides it at runtime)
:- if(\+ current_predicate(assertion/1)).
assertion(Goal) :- call(Goal).
:- endif.

% Helper for running queries in set operation tests
query_test_response_set(Descriptor, Query, Response) :-
    create_context(Descriptor,
                   commit_info{author: "set test", message: "testing"},
                   Context),
    json_woql(Query, AST),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, Response).

/*
 * Test: set_difference basic operation
 */
test(set_difference_basic, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListA'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 3}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 4}}
                    ]}},
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListB'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 4}}
                    ]}},
                  _{'@type': 'SetDifference',
                    'list_a': _{'@type': 'Value', 'variable': 'ListA'},
                    'list_b': _{'@type': 'Value', 'variable': 'ListB'},
                    'result': _{'@type': 'Value', 'variable': 'Diff'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    get_dict('Diff', Res, Diff),
    maplist([json{'@type':'xsd:integer','@value':V},V]>>true, Diff, DiffValues),
    assertion(DiffValues == [1, 3]).

/*
 * Test: set_intersection basic operation
 */
test(set_intersection_basic, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListA'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 3}}
                    ]}},
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListB'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 3}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 4}}
                    ]}},
                  _{'@type': 'SetIntersection',
                    'list_a': _{'@type': 'Value', 'variable': 'ListA'},
                    'list_b': _{'@type': 'Value', 'variable': 'ListB'},
                    'result': _{'@type': 'Value', 'variable': 'Common'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    get_dict('Common', Res, Common),
    maplist([json{'@type':'xsd:integer','@value':V},V]>>true, Common, CommonValues),
    assertion(CommonValues == [2, 3]).

/*
 * Test: set_union basic operation
 */
test(set_union_basic, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListA'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}}
                    ]}},
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListB'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 3}}
                    ]}},
                  _{'@type': 'SetUnion',
                    'list_a': _{'@type': 'Value', 'variable': 'ListA'},
                    'list_b': _{'@type': 'Value', 'variable': 'ListB'},
                    'result': _{'@type': 'Value', 'variable': 'All'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    get_dict('All', Res, All),
    maplist([json{'@type':'xsd:integer','@value':V},V]>>true, All, AllValues),
    assertion(AllValues == [1, 2, 3]).

/*
 * Test: set_member succeeds for element in set
 */
test(set_member_success, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'MySet'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 3}}
                    ]}},
                  _{'@type': 'SetMember',
                    'element': _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                    'set': _{'@type': 'Value', 'variable': 'MySet'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    Bindings = JSON.bindings,
    assertion(length(Bindings, 1)).

/*
 * Test: set_member fails for non-member
 */
test(set_member_failure, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'MySet'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 3}}
                    ]}},
                  _{'@type': 'SetMember',
                    'element': _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 5}},
                    'set': _{'@type': 'Value', 'variable': 'MySet'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    Bindings = JSON.bindings,
    assertion(length(Bindings, 0)).

/*
 * Test: list_to_set removes duplicates and sorts
 */
test(list_to_set_basic, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'MyList'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 3}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}}
                    ]}},
                  _{'@type': 'ListToSet',
                    'list': _{'@type': 'Value', 'variable': 'MyList'},
                    'set': _{'@type': 'Value', 'variable': 'MySet'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    get_dict('MySet', Res, MySet),
    maplist([json{'@type':'xsd:integer','@value':V},V]>>true, MySet, MySetValues),
    assertion(MySetValues == [1, 2, 3]).

/*
 * Test: set_difference with empty result
 */
test(set_difference_empty_result, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListA'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}}
                    ]}},
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListB'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 3}}
                    ]}},
                  _{'@type': 'SetDifference',
                    'list_a': _{'@type': 'Value', 'variable': 'ListA'},
                    'list_b': _{'@type': 'Value', 'variable': 'ListB'},
                    'result': _{'@type': 'Value', 'variable': 'Diff'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    get_dict('Diff', Res, Diff),
    assertion(Diff == []).

/*
 * Test: Incommensurable types - different types are distinct elements
 */
test(incommensurable_types_set_difference, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListA'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 2}}
                    ]}},
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListB'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:decimal', '@value': 1}},
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:decimal', '@value': 2}}
                    ]}},
                  _{'@type': 'SetDifference',
                    'list_a': _{'@type': 'Value', 'variable': 'ListA'},
                    'list_b': _{'@type': 'Value', 'variable': 'ListB'},
                    'result': _{'@type': 'Value', 'variable': 'Diff'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    get_dict('Diff', Res, Diff),
    % Both integers remain because decimals are different types
    length(Diff, 2).

/*
 * Test: Incommensurable types - intersection is empty for different types
 */
test(incommensurable_types_set_intersection, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListA'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}}
                    ]}},
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListB'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:decimal', '@value': 1}}
                    ]}},
                  _{'@type': 'SetIntersection',
                    'list_a': _{'@type': 'Value', 'variable': 'ListA'},
                    'list_b': _{'@type': 'Value', 'variable': 'ListB'},
                    'result': _{'@type': 'Value', 'variable': 'Common'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    get_dict('Common', Res, Common),
    % No common elements because types differ
    assertion(Common == []).

/*
 * Test: Incommensurable types - union contains both type variants
 */
test(incommensurable_types_set_union, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'And',
              'and': [
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListA'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:integer', '@value': 1}}
                    ]}},
                  _{'@type': 'Equals',
                    'left': _{'@type': 'Value', 'variable': 'ListB'},
                    'right': _{'@type': 'Value', 'list': [
                        _{'@type': 'Value', 'data': _{'@type': 'xsd:decimal', '@value': 1}}
                    ]}},
                  _{'@type': 'SetUnion',
                    'list_a': _{'@type': 'Value', 'variable': 'ListA'},
                    'list_b': _{'@type': 'Value', 'variable': 'ListB'},
                    'result': _{'@type': 'Value', 'variable': 'All'}}
              ]},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_set(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    get_dict('All', Res, All),
    % Both elements present because types differ
    length(All, 2).

:- end_tests(set_operations).
