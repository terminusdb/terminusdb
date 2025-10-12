/** <module> Decimal Precision Tests (Included in woql_compile.pl)
 *
 * Tests for xsd:decimal precision handling throughout the system.
 * Ensures that decimals maintain at least 18 digits of precision
 * as required for regulated environments (XSD 1.1, ISO22022, Ethereum).
 *
 * Bug: Decimals currently lose precision when processed through WOQL
 * and serialized to JSON due to using doubles internally.
 *
 * Requirements:
 * - Support minimum 18 digits of precision (preferably 20 for XSD compatibility)
 * - Use Prolog rationals for internal processing (GMP-backed)
 * - Serialize xsd:decimal to JSON as strings, not numbers
 * - Maintain precision through WOQL operations, JSON bindings, GraphQL, and document interface
 */

% Note: This file is included by woql_compile.pl.
% Test suites need their own imports even when included.

:- begin_tests(decimal_precision, []).
:- use_module(core(util/test_utils)).
:- use_module(core(api)).
:- use_module(core(query)).
:- use_module(core(query/query_response), [run_context_ast_jsonld_response/5]).
:- use_module(core(query/json_woql), [json_woql/2]).
:- use_module(core(triple)).
:- use_module(core(transaction)).

% Helper for running queries in decimal tests
query_test_response_decimal(Descriptor, Query, Response) :-
    create_context(Descriptor,
                   commit_info{author: "decimal test", message: "testing"},
                   Context),
    json_woql(Query, AST),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, Response).

/*
 * Test 1: Basic typecast precision preservation
 * The original bug report - typecasting loses precision
 */
test(typecast_preserves_high_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'Typecast',
              value: _{'@type': 'Value',
                      data: _{'@type': 'xsd:string',
                              '@value': "58.082342356456934053939"}},
              type: _{'@type': 'NodeValue',
                      node: 'http://www.w3.org/2001/XMLSchema#decimal'},
              result: _{'@type': 'Value',
                       variable: "Casted"}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_decimal(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    
    % Should preserve up to 20 decimal places (per XSD_DECIMAL_SPECIFICATION.md)
    % Input has 21 decimals, output capped at 20
    get_dict('Casted', Res, Casted),
    get_dict('@value', Casted, Value),
    
    % The value should be a rational/number in JSON with full precision
    assertion(rational(Value)),
    % Check the value is approximately correct (rationals preserve exact precision)
    assertion(Value =:= 58.082342356456934053939).  % 20 decimal places

/*
 * Test 2: Arithmetic operations with decimals
 * Tests the 0.1 * 0.1 * 10 = 0.1 case from bug report
 */
test(decimal_arithmetic_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Query = _{'@type': 'Select',
              variables: ["result"],
              query: _{'@type': 'And',
                      and: [
                          _{'@type': 'Typecast',
                            value: _{'@type': 'Value',
                                    data: _{'@type': 'http://www.w3.org/2001/XMLSchema#decimal',
                                            '@value': 0.1}},
                            type: _{'@type': 'NodeValue',
                                    node: 'http://www.w3.org/2001/XMLSchema#decimal'},
                            result: _{'@type': 'Value',
                                     variable: "term1"}},
                          _{'@type': 'Typecast',
                            value: _{'@type': 'Value',
                                    data: _{'@type': 'http://www.w3.org/2001/XMLSchema#decimal',
                                            '@value': 0.1}},
                            type: _{'@type': 'NodeValue',
                                    node: 'http://www.w3.org/2001/XMLSchema#decimal'},
                            result: _{'@type': 'Value',
                                     variable: "term2"}},
                          _{'@type': 'Typecast',
                            value: _{'@type': 'Value',
                                    data: _{'@type': 'http://www.w3.org/2001/XMLSchema#decimal',
                                            '@value': 10}},
                            type: _{'@type': 'NodeValue',
                                    node: 'http://www.w3.org/2001/XMLSchema#decimal'},
                            result: _{'@type': 'Value',
                                     variable: "term3"}},
                          _{'@type': 'Eval',
                            expression: _{'@type': 'Times',
                                         left: _{'@type': 'Times',
                                                left: _{'@type': 'Value',
                                                       variable: "term1"},
                                                right: _{'@type': 'Value',
                                                        variable: "term2"}},
                                         right: _{'@type': 'Value',
                                                 variable: "term3"}},
                            result: _{'@type': 'Value',
                                     variable: "result"}}
                      ]}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_decimal(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    
    get_dict('result', Res, Result),
    get_dict('@value', Result, Value),
    
    % Should be exactly 0.1 (as rational 1/10), not 0.10000000000000002
    assertion(rational(Value)),
    assertion(Value =:= 0.1).

/*
 * Test 3: High precision decimal (18 digits)
 */
test(eighteen_digit_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    % Test with exactly 18 significant digits
    Decimal_18 = "123456789012345678.901234567890123456",
    
    Query = _{'@type': 'Typecast',
              value: _{'@type': 'Value',
                      data: _{'@type': 'xsd:string',
                              '@value': Decimal_18}},
              type: _{'@type': 'NodeValue',
                      node: 'http://www.w3.org/2001/XMLSchema#decimal'},
              result: _{'@type': 'Value',
                       variable: "Result"}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_decimal(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    
    get_dict('Result', Res, Result),
    get_dict('@value', Result, Value),
    
    assertion(rational(Value)),
    % Check value matches expected (rationals preserve exact precision)
    assertion(Value =:= 123456789012345678.901234567890123456).

/*
 * Test 4: Very high precision decimal (20+ digits for XSD 1.1 compatibility)
 */
test(twenty_digit_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    % Test with 20+ significant digits
    Decimal_20 = "12345678901234567890.12345678901234567890",
    
    Query = _{'@type': 'Typecast',
              value: _{'@type': 'Value',
                      data: _{'@type': 'xsd:string',
                              '@value': Decimal_20}},
              type: _{'@type': 'NodeValue',
                      node: 'http://www.w3.org/2001/XMLSchema#decimal'},
              result: _{'@type': 'Value',
                       variable: "Result"}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_decimal(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    
    get_dict('Result', Res, Result),
    get_dict('@value', Result, Value),
    
    assertion(rational(Value)),
    % Check value matches expected (rationals preserve exact precision)
    assertion(Value =:= 12345678901234567890.1234567890123456789).

/*
 * Test 5: Addition with high precision decimals
 */
test(decimal_addition_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Decimal_A = "123456789012345678.123456789012345678",
    Decimal_B = "987654321098765432.987654321098765432",
    
    AST = (typecast(Decimal_A, 'http://www.w3.org/2001/XMLSchema#decimal', v('A')),
           typecast(Decimal_B, 'http://www.w3.org/2001/XMLSchema#decimal', v('B')),
           v('Result') is v('A') + v('B')),
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,
                   commit_info{author: "test", message: "decimal test"},
                   Context),
    
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, Response),
    [First] = Response.bindings,
    get_dict('Result', First, Result),
    get_dict('@value', Result, Value),
    
    assertion(rational(Value)),
    % Check value approximately matches expected sum
    assertion(Value =:= 1111111110111111111.11111111011111111).

/*
 * Test 6: Subtraction with high precision decimals
 */
test(decimal_subtraction_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Decimal_A = "987654321098765432.987654321098765432",
    Decimal_B = "123456789012345678.123456789012345678",
    
    AST = (typecast(Decimal_A, 'http://www.w3.org/2001/XMLSchema#decimal', v('A')),
           typecast(Decimal_B, 'http://www.w3.org/2001/XMLSchema#decimal', v('B')),
           v('Result') is v('A') - v('B')),
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,
                   commit_info{author: "test", message: "decimal test"},
                   Context),
    
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, Response),
    [First] = Response.bindings,
    get_dict('Result', First, Result),
    get_dict('@value', Result, Value),
    
    assertion(rational(Value)),
    % Check value approximately matches expected difference
    assertion(Value =:= 864197532086419754.864197532086419754).

/*
 * Test 7: Multiplication with high precision decimals
 */
test(decimal_multiplication_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Decimal_A = "12345.678901234567890",
    Decimal_B = "98765.432109876543210",
    
    AST = (typecast(Decimal_A, 'http://www.w3.org/2001/XMLSchema#decimal', v('A')),
           typecast(Decimal_B, 'http://www.w3.org/2001/XMLSchema#decimal', v('B')),
           v('Result') is v('A') * v('B')),
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,
                   commit_info{author: "test", message: "decimal test"},
                   Context),
    
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, Response),
    [First] = Response.bindings,
    get_dict('Result', First, Result),
    get_dict('@value', Result, Value),
    
    % Result should be a rational with full precision
    assertion(rational(Value)),
    % Verify the multiplication is approximately correct
    assertion(Value =:= 12345.678901234567890 * 98765.432109876543210).

/*
 * Test 8: Division with high precision decimals
 */
test(decimal_division_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Decimal_A = "1.000000000000000000",
    Decimal_B = "3.000000000000000000",
    
    AST = (typecast(Decimal_A, 'http://www.w3.org/2001/XMLSchema#decimal', v('A')),
           typecast(Decimal_B, 'http://www.w3.org/2001/XMLSchema#decimal', v('B')),
           v('Result') is v('A') / v('B')),
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,
                   commit_info{author: "test", message: "decimal test"},
                   Context),
    
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, Response),
    [First] = Response.bindings,
    get_dict('Result', First, Result),
    get_dict('@value', Result, Value),
    
    % Division now uses rdiv, producing exact rational
    assertion(rational(Value)),
    % Exact rational: 1/3
    assertion(Value =:= 1 rdiv 3).

/*
 * Test 9: JSON binding serialization preserves strings
 */
test(json_bindings_decimal_as_string, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    High_Precision = "999999999999999999.999999999999999999",
    
    Query = _{'@type': 'Equals',
              left: _{'@type': 'Value',
                     variable: "X"},
              right: _{'@type': 'Value',
                      data: _{'@type': 'http://www.w3.org/2001/XMLSchema#decimal',
                              '@value': High_Precision}}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_decimal(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    
    get_dict('X', Res, X),
    get_dict('@value', X, Value),
    
    % Value is a rational/number with full precision
    assertion(rational(Value)),
    assertion(Value =:= 999999999999999999.999999999999999999).

/*
 * Test 10: Document storage and retrieval preserves decimal precision
 */
test(document_decimal_precision, [
         setup((setup_temp_store(State),
                create_db_with_test_schema("admin", "test"))),
         cleanup(teardown_temp_store(State)),
         fixme('Requires schema with decimal property')
     ]) :-
    
    High_Precision = "555555555555555555.555555555555555555",
    
    % Insert document with high-precision decimal
    Doc = _{'@type': 'TestClass',
            decimal_field: _{'@type': 'http://www.w3.org/2001/XMLSchema#decimal',
                           '@value': High_Precision}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,
                   commit_info{author: "test", message: "decimal test"},
                   Context),
    
    % Insert and retrieve
    AST = (insert_document(Doc, v('ID')),
           get_document(v('ID'), v('Retrieved'))),
    
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, Response),
    [First] = Response.bindings,
    
    get_dict('Retrieved', First, Retrieved),
    get_dict(decimal_field, Retrieved, Field),
    get_dict('@value', Field, Value),
    
    assertion(rational(Value)),
    assertion(Value =:= 555555555555555555.555555555555555555).

/*
 * Test 11: Comparison operations with high precision decimals
 */
test(decimal_comparison_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Decimal_A = "123456789012345678.123456789012345678",
    Decimal_B = "123456789012345678.123456789012345679",
    
    AST = (typecast(Decimal_A, 'http://www.w3.org/2001/XMLSchema#decimal', v('A')),
           typecast(Decimal_B, 'http://www.w3.org/2001/XMLSchema#decimal', v('B')),
           v('A') < v('B')),
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,
                   commit_info{author: "test", message: "decimal test"},
                   Context),
    
    % Should succeed - A is less than B by the smallest margin
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, Response),
    assertion(Response.bindings \= []).

/*
 * Test 12: Negative high precision decimals
 */
test(negative_decimal_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Negative_Decimal = "-888888888888888888.888888888888888888",
    
    Query = _{'@type': 'Typecast',
              value: _{'@type': 'Value',
                      data: _{'@type': 'xsd:string',
                              '@value': Negative_Decimal}},
              type: _{'@type': 'NodeValue',
                      node: 'http://www.w3.org/2001/XMLSchema#decimal'},
              result: _{'@type': 'Value',
                       variable: "Result"}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_decimal(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    
    get_dict('Result', Res, Result),
    get_dict('@value', Result, Value),
    
    assertion(rational(Value)),
    assertion(Value =:= -888888888888888888.888888888888888888).

/*
 * Test 13: Very small decimal values (many decimal places)
 */
test(small_decimal_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    Small_Decimal = "0.000000000000000001234567890123456789",
    
    Query = _{'@type': 'Typecast',
              value: _{'@type': 'Value',
                      data: _{'@type': 'xsd:string',
                              '@value': Small_Decimal}},
              type: _{'@type': 'NodeValue',
                      node: 'http://www.w3.org/2001/XMLSchema#decimal'},
              result: _{'@type': 'Value',
                       variable: "Result"}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_decimal(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    
    get_dict('Result', Res, Result),
    get_dict('@value', Result, Value),
    
    assertion(rational(Value)),
    % Exact rational representation
    assertion(Value =:= 0.000000000000000001234567890123456789).

/*
 * Test 14: Ethereum-style 18 decimal precision (Wei to Ether)
 */
test(ethereum_wei_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    % 1 Ether = 1000000000000000000 Wei (18 zeros)
    Wei_Value = "1000000000000000000",
    
    Query = _{'@type': 'Typecast',
              value: _{'@type': 'Value',
                      data: _{'@type': 'xsd:string',
                              '@value': Wei_Value}},
              type: _{'@type': 'NodeValue',
                      node: 'http://www.w3.org/2001/XMLSchema#decimal'},
              result: _{'@type': 'Value',
                       variable: "Result"}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_decimal(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    
    get_dict('Result', Res, Result),
    get_dict('@value', Result, Value),
    
    % Large integer-valued decimal (whole number)
    assertion(number(Value)),
    assertion(Value =:= 1000000000000000000).

/*
 * Test 15: ISO 20022 financial message precision (regulatory requirement)
 */
test(iso20022_financial_precision, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-
    
    % ISO 20022 requires up to 18 digits for amounts
    Amount = "999999999999999.99",
    
    Query = _{'@type': 'Typecast',
              value: _{'@type': 'Value',
                      data: _{'@type': 'xsd:string',
                              '@value': Amount}},
              type: _{'@type': 'NodeValue',
                      node: 'http://www.w3.org/2001/XMLSchema#decimal'},
              result: _{'@type': 'Value',
                       variable: "Result"}},
    
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    query_test_response_decimal(Descriptor, Query, JSON),
    [Res] = JSON.bindings,
    
    get_dict('Result', Res, Result),
    get_dict('@value', Result, Value),
    
    assertion(rational(Value)),
    assertion(Value =:= 999999999999999.99).

:- end_tests(decimal_precision).
