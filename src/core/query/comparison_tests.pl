:- module(comparison_tests, []).

/** <module> Comparison Tests for Less and Greater
 *
 * This test suite specifically tests the less (<) and greater (>) operators
 * with focus on type inconsistency issues between different representations
 * of the same XSD type (e.g., xsd:decimal vs 'http://www.w3.org/2001/XMLSchema#decimal')
 *
 * Issue #2225: Type inconsistency in comparison operators
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(woql_compile).
:- use_module(global_prefixes).
:- use_module(core(util/test_utils)).
:- use_module(core(api)).

:- begin_tests(woql_comparison_operators, [concurrent(true)]).

/*
 * Unit tests for woql_less/2 and woql_greater/2 predicates
 * These tests expose the type inconsistency bug where comparing
 * the same value with different type representations yields incorrect results
 */

% Basic functionality tests - These should pass
test(woql_less_basic_decimal, []) :-
    % 5 < 10 should be true
    woql_compile:woql_less(5^^'http://www.w3.org/2001/XMLSchema#decimal',
                           10^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_greater_basic_decimal, []) :-
    % 10 > 5 should be true
    woql_compile:woql_greater(10^^'http://www.w3.org/2001/XMLSchema#decimal',
                 5^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_less_basic_integer, []) :-
    % 5 < 10 should be true for integers
    woql_compile:woql_less(5^^'http://www.w3.org/2001/XMLSchema#integer',
              10^^'http://www.w3.org/2001/XMLSchema#integer').

% ========================================
% Smoke Tests: Basic comparisons with different numeric types
% ========================================

test(woql_greater_integer, []) :-
    % 10 > 5 for integers
    woql_compile:woql_greater(10^^'http://www.w3.org/2001/XMLSchema#integer',
                 5^^'http://www.w3.org/2001/XMLSchema#integer').

test(woql_less_integer, []) :-
    % 5 < 10 for integers
    woql_compile:woql_less(5^^'http://www.w3.org/2001/XMLSchema#integer',
              10^^'http://www.w3.org/2001/XMLSchema#integer').

test(woql_greater_decimal, []) :-
    % 40.1 > 21.1 for decimals
    woql_compile:woql_greater(40.1^^'http://www.w3.org/2001/XMLSchema#decimal',
                 21.1^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_less_decimal, []) :-
    % 21.1 < 40.1 for decimals
    woql_compile:woql_less(21.1^^'http://www.w3.org/2001/XMLSchema#decimal',
              40.1^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_greater_float, []) :-
    % 40.1 > 21.1 for floats
    woql_compile:woql_greater(40.1^^'http://www.w3.org/2001/XMLSchema#float',
                 21.1^^'http://www.w3.org/2001/XMLSchema#float').

test(woql_less_float, []) :-
    % 21.1 < 40.1 for floats
    woql_compile:woql_less(21.1^^'http://www.w3.org/2001/XMLSchema#float',
              40.1^^'http://www.w3.org/2001/XMLSchema#float').

test(woql_greater_double, []) :-
    % 40.1 > 21.1 for doubles
    woql_compile:woql_greater(40.1^^'http://www.w3.org/2001/XMLSchema#double',
                 21.1^^'http://www.w3.org/2001/XMLSchema#double').

test(woql_less_double, []) :-
    % 21.1 < 40.1 for doubles
    woql_compile:woql_less(21.1^^'http://www.w3.org/2001/XMLSchema#double',
              40.1^^'http://www.w3.org/2001/XMLSchema#double').

% Cross-type comparisons (decimal with integer)
test(woql_greater_decimal_vs_integer, []) :-
    % 40.1 (decimal) > 33 (integer)
    woql_compile:woql_greater(40.1^^'http://www.w3.org/2001/XMLSchema#decimal',
                 33^^'http://www.w3.org/2001/XMLSchema#integer').

test(woql_less_integer_vs_decimal, []) :-
    % 21 (integer) < 40.1 (decimal)
    woql_compile:woql_less(21^^'http://www.w3.org/2001/XMLSchema#integer',
              40.1^^'http://www.w3.org/2001/XMLSchema#decimal').

% Mixed short/full URI representations
test(woql_greater_float_short_vs_full, []) :-
    % 40.1 > 21.1 with mixed type representations
    woql_compile:woql_greater(40.1^^xsd:float,
                 21.1^^'http://www.w3.org/2001/XMLSchema#float').

test(woql_less_double_full_vs_short, []) :-
    % 21.1 < 40.1 with mixed type representations
    woql_compile:woql_less(21.1^^'http://www.w3.org/2001/XMLSchema#double',
              40.1^^xsd:double).

% Equal values should fail comparisons - These should pass (by failing the comparison)
test(woql_less_equal_fails, [fail]) :-
    % 33 < 33 should be false
    woql_compile:woql_less(33^^'http://www.w3.org/2001/XMLSchema#decimal',
              33^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_greater_equal_fails, [fail]) :-
    % 33 > 33 should be false
    woql_compile:woql_greater(33^^'http://www.w3.org/2001/XMLSchema#decimal',
                 33^^'http://www.w3.org/2001/XMLSchema#decimal').

% CRITICAL: Equal values across different numeric types
% These match the user's WOQL query scenario
test(woql_greater_33_float_vs_33_decimal, [fail]) :-
    % 33.0 (float) should NOT be > 33 (decimal)
    woql_compile:woql_greater(33.0^^'http://www.w3.org/2001/XMLSchema#float',
                 33^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_greater_33_double_vs_33_decimal, [fail]) :-
    % 33.0 (double) should NOT be > 33 (decimal)
    woql_compile:woql_greater(33.0^^'http://www.w3.org/2001/XMLSchema#double',
                 33^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_less_33_float_vs_33_decimal, [fail]) :-
    % 33.0 (float) should NOT be < 33 (decimal)
    woql_compile:woql_less(33.0^^'http://www.w3.org/2001/XMLSchema#float',
              33^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_less_33_double_vs_33_decimal, [fail]) :-
    % 33.0 (double) should NOT be < 33 (decimal)
    woql_compile:woql_less(33.0^^'http://www.w3.org/2001/XMLSchema#double',
              33^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_greater_33_float_vs_33_integer, [fail]) :-
    % 33.0 (float) should NOT be > 33 (integer)
    woql_compile:woql_greater(33.0^^'http://www.w3.org/2001/XMLSchema#float',
                 33^^'http://www.w3.org/2001/XMLSchema#integer').

test(woql_less_33_double_vs_33_integer, [fail]) :-
    % 33.0 (double) should NOT be < 33 (integer)
    woql_compile:woql_less(33.0^^'http://www.w3.org/2001/XMLSchema#double',
              33^^'http://www.w3.org/2001/XMLSchema#integer').

% Type inconsistency tests - These should now PASS after fix
test(woql_less_type_inconsistency_short_vs_full, [fail]) :-
    % 33 < 33 should be false regardless of type representation
    woql_compile:woql_less(33^^xsd:decimal,
                           33^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_greater_type_inconsistency_short_vs_full, [fail]) :-
    % 33 > 33 should be false regardless of type representation
    % This was the BUG - now fixed!
    woql_compile:woql_greater(33^^xsd:decimal,
                              33^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_less_type_inconsistency_full_vs_short, [fail]) :-
    % 33 < 33 should be false regardless of type representation
    woql_compile:woql_less(33^^'http://www.w3.org/2001/XMLSchema#decimal',
                           33^^xsd:decimal).

test(woql_greater_type_inconsistency_full_vs_short, [fail]) :-
    % 33 > 33 should be false regardless of type representation
    woql_compile:woql_greater(33^^'http://www.w3.org/2001/XMLSchema#decimal',
                              33^^xsd:decimal).

% Integer type inconsistency
test(woql_less_type_inconsistency_integer, [fail]) :-
    woql_compile:woql_less(42^^xsd:integer,
                           42^^'http://www.w3.org/2001/XMLSchema#integer').

test(woql_greater_type_inconsistency_integer, [fail]) :-
    woql_compile:woql_greater(42^^xsd:integer,
                              42^^'http://www.w3.org/2001/XMLSchema#integer').

% Different value comparisons with type inconsistency
test(woql_less_different_values_type_inconsistency, []) :-
    % 5 < 10 should be true, but type inconsistency might affect this
    woql_compile:woql_less(5^^xsd:decimal,
              10^^'http://www.w3.org/2001/XMLSchema#decimal').

test(woql_greater_different_values_type_inconsistency, []) :-
    % 10 > 5 should be true, but type inconsistency might affect this
    woql_compile:woql_greater(10^^xsd:decimal,
                 5^^'http://www.w3.org/2001/XMLSchema#decimal').

% DateTime tests - different code path
test(woql_less_datetime, []) :-
    % DateTime comparisons use a different clause
    woql_compile:woql_less('2023-01-01T00:00:00Z'^^'http://www.w3.org/2001/XMLSchema#dateTime',
              '2023-12-31T23:59:59Z'^^'http://www.w3.org/2001/XMLSchema#dateTime').

test(woql_greater_datetime, []) :-
    woql_compile:woql_greater('2023-12-31T23:59:59Z'^^'http://www.w3.org/2001/XMLSchema#dateTime',
                 '2023-01-01T00:00:00Z'^^'http://www.w3.org/2001/XMLSchema#dateTime').

test(woql_less_datetime_equal, [fail]) :-
    % Equal datetimes should fail less comparison
    woql_compile:woql_less('2023-01-01T00:00:00Z'^^'http://www.w3.org/2001/XMLSchema#dateTime',
              '2023-01-01T00:00:00Z'^^'http://www.w3.org/2001/XMLSchema#dateTime').

test(woql_greater_datetime_equal, [fail]) :-
    % Equal datetimes should fail greater comparison
    woql_compile:woql_greater('2023-01-01T00:00:00Z'^^'http://www.w3.org/2001/XMLSchema#dateTime',
                 '2023-01-01T00:00:00Z'^^'http://www.w3.org/2001/XMLSchema#dateTime').

% String comparison through fallback clause
test(woql_less_string_comparison, []) :-
    % Strings should use structural comparison (fallback clause)
    woql_compile:woql_less("abc"^^'http://www.w3.org/2001/XMLSchema#string',
              "xyz"^^'http://www.w3.org/2001/XMLSchema#string').

test(woql_greater_string_comparison, []) :-
    woql_compile:woql_greater("xyz"^^'http://www.w3.org/2001/XMLSchema#string',
                 "abc"^^'http://www.w3.org/2001/XMLSchema#string').

:- end_tests(woql_comparison_operators).

/*
 * Integration tests using JSON-LD queries
 * These test the full pipeline from JSON to execution
 */
:- begin_tests(comparison_json_integration, [concurrent(true)]).

:- use_module(json_woql).
:- use_module(ask,[ask/2,create_context/2, create_context/3]).
:- use_module(query_response, [run_context_ast_jsonld_response/5]).
:- use_module(core(transaction)).

test(less_than_json_basic, [
    setup((setup_temp_store(State),
           create_db_without_schema(admin,test))),
    cleanup(teardown_temp_store(State))
]) :-
    % Basic less than should work
    Query = _{ '@type' : "Less",
               left : _{'@type' : "DataValue", 'data' : 5},
               right : _{'@type' : "DataValue", 'data' : 10}
             },
    make_branch_descriptor('admin', 'test', Descriptor),
    create_context(Descriptor, commit_info{ author : "test", message: "less test"}, Context),
    json_woql(Query, AST),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, JSON),
    [_] = (JSON.bindings).

test(greater_than_json_basic, [
    setup((setup_temp_store(State),
           create_db_without_schema(admin,test))),
    cleanup(teardown_temp_store(State))
]) :-
    % Basic greater than should work
    Query = _{ '@type' : "Greater",
               left : _{'@type' : "DataValue", 'data' : 10},
               right : _{'@type' : "DataValue", 'data' : 5}
             },
    make_branch_descriptor('admin', 'test', Descriptor),
    create_context(Descriptor, commit_info{ author : "test", message: "greater test"}, Context),
    json_woql(Query, AST),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, JSON),
    [_] = (JSON.bindings).

test(less_than_equal_fails_json, [
    setup((setup_temp_store(State),
           create_db_without_schema(admin,test))),
    cleanup(teardown_temp_store(State))
]) :-
    % 33 < 33 should fail (no results)
    Query = _{ '@type' : "Less",
               left : _{'@type' : "DataValue", 'data' : 33},
               right : _{'@type' : "DataValue", 'data' : 33}
             },
    make_branch_descriptor('admin', 'test', Descriptor),
    create_context(Descriptor, commit_info{ author : "test", message: "less equal test"}, Context),
    json_woql(Query, AST),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, JSON),
    [] = (JSON.bindings).

test(greater_than_equal_fails_json, [
    setup((setup_temp_store(State),
           create_db_without_schema(admin,test))),
    cleanup(teardown_temp_store(State))
]) :-
    % 33 > 33 should fail (no results)
    % This currently fails due to the bug
    Query = _{ '@type' : "Greater",
               left : _{'@type' : "DataValue", 'data' : 33},
               right : _{'@type' : "DataValue", 'data' : 33}
             },
    make_branch_descriptor('admin', 'test', Descriptor),
    create_context(Descriptor, commit_info{ author : "test", message: "greater equal test"}, Context),
    json_woql(Query, AST),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, JSON),
    [] = (JSON.bindings).

test(less_than_with_explicit_types_json, [
    setup((setup_temp_store(State),
           create_db_without_schema(admin,test))),
    cleanup(teardown_temp_store(State))
]) :-
    % Explicitly specify xsd:decimal for both sides
    Query = _{ '@type' : "Less",
               left : _{'@type' : "DataValue", 
                       'data' : _{'@type': 'xsd:decimal', '@value': 33}},
               right : _{'@type' : "DataValue",
                        'data' : _{'@type': 'xsd:decimal', '@value': 33}}
             },
    make_branch_descriptor('admin', 'test', Descriptor),
    create_context(Descriptor, commit_info{ author : "test", message: "less explicit test"}, Context),
    json_woql(Query, AST),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, JSON),
    [] = (JSON.bindings).

test(greater_than_with_explicit_types_json, [
    setup((setup_temp_store(State),
           create_db_without_schema(admin,test))),
    cleanup(teardown_temp_store(State))
]) :-
    % Explicitly specify xsd:decimal for both sides
    % This will still fail because of how types are expanded
    Query = _{ '@type' : "Greater",
               left : _{'@type' : "DataValue", 
                       'data' : _{'@type': 'xsd:decimal', '@value': 33}},
               right : _{'@type' : "DataValue",
                        'data' : _{'@type': 'xsd:decimal', '@value': 33}}
             },
    make_branch_descriptor('admin', 'test', Descriptor),
    create_context(Descriptor, commit_info{ author : "test", message: "greater explicit test"}, Context),
    json_woql(Query, AST),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, JSON),
    [] = (JSON.bindings).

:- end_tests(comparison_json_integration).
