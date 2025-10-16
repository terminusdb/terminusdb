:- module(graphql_numeric_serialization_test, []).

/**
 * GraphQL Numeric Serialization Tests
 * 
 * Verifies that GraphQL responses serialize xsd:decimal and xsd:integer types
 * as native JSON numbers (not strings) while preserving arbitrary precision.
 * 
 * JSON RFC 8259 supports arbitrary precision numbers. The Juniper GraphQL
 * library should output numeric values as JSON numbers, not quoted strings.
 */

:- begin_tests(graphql_numeric_serialization, []).
:- use_module(core(util/test_utils)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(core(query/resolve_query_resource), [resolve_absolute_string_descriptor/2]).
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

% Define assertion/1 locally to satisfy linter (plunit provides it at runtime)
:- if(\+ current_predicate(assertion/1)).
assertion(Goal) :- call(Goal).
:- endif.

test(graphql_integer_as_json_number, [
    setup((setup_temp_store(State),
           create_db_with_empty_schema('admin','testdb'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Create a schema with integer field
    Schema = [
        _{'@type': 'Class',
          '@id': 'Product',
          quantity: 'xsd:integer'}
    ],
    
    with_test_transaction(
        Descriptor,
        C1,
        insert_schema_document(C1, Schema)
    ),
    
    % Insert a document with integer value
    Doc = _{'@type': 'Product',
            '@id': 'Product/test1',
            quantity: 42},
    
    with_test_transaction(
        Descriptor,
        C2,
        insert_document(C2, Doc, _Id)
    ),
    
    % Query via GraphQL
    GraphQL_Query = "{
        Product {
            quantity
        }
    }",
    
    resolve_absolute_string_descriptor("admin/testdb", GraphQL_Descriptor),
    Path = "admin/testdb",
    
    api_test_graphql_request(GraphQL_Descriptor, Path, GraphQL_Query, JSON_String),
    
    % Parse JSON response
    atom_json_dict(JSON_String, Response, []),
    
    % Verify the quantity is a JSON number, not a string
    get_dict(data, Response, Data),
    get_dict('Product', Data, Products),
    [Product|_] = Products,
    get_dict(quantity, Product, Quantity),
    
    % Should be a number, not a string
    assertion(number(Quantity)),
    assertion(Quantity =:= 42).

test(graphql_decimal_as_json_number, [
    setup((setup_temp_store(State),
           create_db_with_empty_schema('admin','testdb'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Create a schema with decimal field
    Schema = [
        _{'@type': 'Class',
          '@id': 'Product',
          price: 'xsd:decimal'}
    ],
    
    with_test_transaction(
        Descriptor,
        C1,
        insert_schema_document(C1, Schema)
    ),
    
    % Insert a document with decimal value
    Doc = _{'@type': 'Product',
            '@id': 'Product/test2',
            price: '19.99'^^'http://www.w3.org/2001/XMLSchema#decimal'},
    
    with_test_transaction(
        Descriptor,
        C2,
        insert_document(C2, Doc, _Id)
    ),
    
    % Query via GraphQL
    GraphQL_Query = "{
        Product {
            price
        }
    }",
    
    resolve_absolute_string_descriptor("admin/testdb", GraphQL_Descriptor),
    Path = "admin/testdb",
    
    api_test_graphql_request(GraphQL_Descriptor, Path, GraphQL_Query, JSON_String),
    
    % Parse JSON response and verify raw JSON text contains number not string
    % Should contain: "price":19.99  NOT "price":"19.99"
    sub_string(JSON_String, _, _, _, '"price":19.99'),
    \+ sub_string(JSON_String, _, _, _, '"price":"19.99"').

test(graphql_big_integer_as_json_number, [
    setup((setup_temp_store(State),
           create_db_with_empty_schema('admin','testdb'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Create a schema with integer field
    Schema = [
        _{'@type': 'Class',
          '@id': 'LargeNumber',
          value: 'xsd:integer'}
    ],
    
    with_test_transaction(
        Descriptor,
        C1,
        insert_schema_document(C1, Schema)
    ),
    
    % Insert a document with large integer (beyond JavaScript MAX_SAFE_INTEGER)
    BigInt = '9007199254740993',  % 2^53 + 1
    Doc = _{'@type': 'LargeNumber',
            '@id': 'LargeNumber/test1',
            value: BigInt^^'http://www.w3.org/2001/XMLSchema#integer'},
    
    with_test_transaction(
        Descriptor,
        C2,
        insert_document(C2, Doc, _Id)
    ),
    
    % Query via GraphQL
    GraphQL_Query = "{
        LargeNumber {
            value
        }
    }",
    
    resolve_absolute_string_descriptor("admin/testdb", GraphQL_Descriptor),
    Path = "admin/testdb",
    
    api_test_graphql_request(GraphQL_Descriptor, Path, GraphQL_Query, JSON_String),
    
    % Verify JSON contains number without quotes
    % Should contain: "value":9007199254740993  NOT "value":"9007199254740993"
    atom_concat('"value":', BigInt, Expected),
    sub_string(JSON_String, _, _, _, Expected),
    
    % Verify it's not quoted as a string
    atom_concat('"value":"', BigInt, NotExpected),
    \+ sub_string(JSON_String, _, _, _, NotExpected).

test(graphql_high_precision_decimal_as_json_number, [
    setup((setup_temp_store(State),
           create_db_with_empty_schema('admin','testdb'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Create a schema with decimal field
    Schema = [
        _{'@type': 'Class',
          '@id': 'Measurement',
          value: 'xsd:decimal'}
    ],
    
    with_test_transaction(
        Descriptor,
        C1,
        insert_schema_document(C1, Schema)
    ),
    
    % Insert a document with 20-digit precision decimal
    HighPrecision = '1.23456789012345678901',
    Doc = _{'@type': 'Measurement',
            '@id': 'Measurement/test1',
            value: HighPrecision^^'http://www.w3.org/2001/XMLSchema#decimal'},
    
    with_test_transaction(
        Descriptor,
        C2,
        insert_document(C2, Doc, _Id)
    ),
    
    % Query via GraphQL
    GraphQL_Query = "{
        Measurement {
            value
        }
    }",
    
    resolve_absolute_string_descriptor("admin/testdb", GraphQL_Descriptor),
    Path = "admin/testdb",
    
    api_test_graphql_request(GraphQL_Descriptor, Path, GraphQL_Query, JSON_String),
    
    % Verify JSON contains the full precision number without quotes
    atom_concat('"value":', HighPrecision, Expected),
    sub_string(JSON_String, _, _, _, Expected),
    
    % Verify it's not quoted as a string
    atom_concat('"value":"', HighPrecision, NotExpected),
    \+ sub_string(JSON_String, _, _, _, NotExpected).

test(graphql_multiple_numeric_types, [
    setup((setup_temp_store(State),
           create_db_with_empty_schema('admin','testdb'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Create a schema with multiple numeric fields
    Schema = [
        _{'@type': 'Class',
          '@id': 'Financial',
          intValue: 'xsd:int',
          longValue: 'xsd:long',
          decimalValue: 'xsd:decimal',
          unsignedLongValue: 'xsd:unsignedLong'}
    ],
    
    with_test_transaction(
        Descriptor,
        C1,
        insert_schema_document(C1, Schema)
    ),
    
    % Insert a document with multiple numeric types
    Doc = _{'@type': 'Financial',
            '@id': 'Financial/test1',
            intValue: 42,
            longValue: '9999999999'^^'http://www.w3.org/2001/XMLSchema#long',
            decimalValue: '123.456'^^'http://www.w3.org/2001/XMLSchema#decimal',
            unsignedLongValue: '18446744073709551615'^^'http://www.w3.org/2001/XMLSchema#unsignedLong'},
    
    with_test_transaction(
        Descriptor,
        C2,
        insert_document(C2, Doc, _Id)
    ),
    
    % Query via GraphQL
    GraphQL_Query = "{
        Financial {
            intValue
            longValue
            decimalValue
            unsignedLongValue
        }
    }",
    
    resolve_absolute_string_descriptor("admin/testdb", GraphQL_Descriptor),
    Path = "admin/testdb",
    
    api_test_graphql_request(GraphQL_Descriptor, Path, GraphQL_Query, JSON_String),
    
    % All should be JSON numbers, not strings
    sub_string(JSON_String, _, _, _, '"intValue":42'),
    sub_string(JSON_String, _, _, _, '"longValue":9999999999'),
    sub_string(JSON_String, _, _, _, '"decimalValue":123.456'),
    sub_string(JSON_String, _, _, _, '"unsignedLongValue":18446744073709551615'),
    
    % Verify none are quoted as strings
    \+ sub_string(JSON_String, _, _, _, '"intValue":"42"'),
    \+ sub_string(JSON_String, _, _, _, '"longValue":"9999999999"'),
    \+ sub_string(JSON_String, _, _, _, '"decimalValue":"123.456"'),
    \+ sub_string(JSON_String, _, _, _, '"unsignedLongValue":"18446744073709551615"').

test(graphql_negative_and_zero_numbers, [
    setup((setup_temp_store(State),
           create_db_with_empty_schema('admin','testdb'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Create a schema with numeric fields
    Schema = [
        _{'@type': 'Class',
          '@id': 'Numbers',
          zeroInt: 'xsd:integer',
          negativeInt: 'xsd:integer',
          zeroDecimal: 'xsd:decimal',
          negativeDecimal: 'xsd:decimal'}
    ],
    
    with_test_transaction(
        Descriptor,
        C1,
        insert_schema_document(C1, Schema)
    ),
    
    % Insert a document with edge cases
    Doc = _{'@type': 'Numbers',
            '@id': 'Numbers/test1',
            zeroInt: 0,
            negativeInt: -42,
            zeroDecimal: '0.0'^^'http://www.w3.org/2001/XMLSchema#decimal',
            negativeDecimal: '-19.99'^^'http://www.w3.org/2001/XMLSchema#decimal'},
    
    with_test_transaction(
        Descriptor,
        C2,
        insert_document(C2, Doc, _Id)
    ),
    
    % Query via GraphQL
    GraphQL_Query = "{
        Numbers {
            zeroInt
            negativeInt
            zeroDecimal
            negativeDecimal
        }
    }",
    
    resolve_absolute_string_descriptor("admin/testdb", GraphQL_Descriptor),
    Path = "admin/testdb",
    
    api_test_graphql_request(GraphQL_Descriptor, Path, GraphQL_Query, JSON_String),
    
    % All should be JSON numbers
    sub_string(JSON_String, _, _, _, '"zeroInt":0'),
    sub_string(JSON_String, _, _, _, '"negativeInt":-42'),
    sub_string(JSON_String, _, _, _, '"zeroDecimal":0'),
    sub_string(JSON_String, _, _, _, '"negativeDecimal":-19.99').

test(graphql_positiveInteger_as_json_number, [
    setup((setup_temp_store(State),
           create_db_with_empty_schema('admin','testdb'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Create a schema with positiveInteger field
    Schema = [
        _{'@type': 'Class',
          '@id': 'Counter',
          count: 'xsd:positiveInteger'}
    ],
    
    with_test_transaction(
        Descriptor,
        C1,
        insert_schema_document(C1, Schema)
    ),
    
    % Insert a document with positiveInteger value
    Doc = _{'@type': 'Counter',
            '@id': 'Counter/test1',
            count: '999999999999999999'^^'http://www.w3.org/2001/XMLSchema#positiveInteger'},
    
    with_test_transaction(
        Descriptor,
        C2,
        insert_document(C2, Doc, _Id)
    ),
    
    % Query via GraphQL
    GraphQL_Query = "{
        Counter {
            count
        }
    }",
    
    resolve_absolute_string_descriptor("admin/testdb", GraphQL_Descriptor),
    Path = "admin/testdb",
    
    api_test_graphql_request(GraphQL_Descriptor, Path, GraphQL_Query, JSON_String),
    
    % Verify JSON contains number without quotes
    sub_string(JSON_String, _, _, _, '"count":999999999999999999'),
    \+ sub_string(JSON_String, _, _, _, '"count":"999999999999999999"').

% Helper predicate to make GraphQL requests for testing
% This simulates the HTTP request flow through the GraphQL API
api_test_graphql_request(Descriptor, Path, Query, JSON_Response) :-
    % This would need to call the actual GraphQL handler
    % For now, we'll use a placeholder that needs to be implemented
    format(atom(JSON_Request), '{"query": "~w"}', [Query]),
    atom_string(JSON_Request, JSON_Request_String),
    
    % Call the GraphQL API
    open_descriptor(Descriptor, Transaction),
    System = system_descriptor{},
    
    % Create a temporary stream with the query
    setup_call_cleanup(
        open_string(JSON_Request_String, Input),
        (
            string_length(JSON_Request_String, ContentLength),
            catch(
                '$graphql':handle_request(
                    post,
                    _GraphQL_Context,
                    System,
                    none, % meta
                    none, % commit
                    Transaction,
                    'admin', % auth
                    ContentLength,
                    Input,
                    JSON_Response,
                    _IsError,
                    _Author,
                    _Message
                ),
                Error,
                (format('GraphQL Error: ~w~n', [Error]), fail)
            )
        ),
        close(Input)
    ).

:- end_tests(graphql_numeric_serialization).
