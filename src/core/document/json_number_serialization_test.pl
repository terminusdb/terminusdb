:- module(json_number_serialization_test, []).

:- use_module(core(util/test_utils)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/json)).

/**
 * JSON Number Serialization Tests
 * 
 * Verifies that xsd:decimal and xsd:integer types serialize to proper JSON numbers
 * (not strings) while preserving arbitrary precision.
 * 
 * JSON RFC 8259 supports arbitrary precision numbers - the limitation is in 
 * JavaScript's JSON.parse(), not the JSON format itself.
 */

:- begin_tests(json_number_serialization, []).

test(decimal_serializes_as_json_number, [
    setup((setup_temp_store(State),
           create_db_without_schema('admin','test'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Insert a document with a decimal value
    Doc = _{'@type': 'Product',
            '@id': 'Product/test1',
            price: '19.99'^^'http://www.w3.org/2001/XMLSchema#decimal'},
    
    with_test_transaction(
        _Descriptor,
        C1,
        insert_document(C1, Doc, Id)
    ),
    
    % Retrieve and check JSON serialization
    with_test_transaction(
        _Descriptor2,
        C2,
        (   get_document(C2, Id, Retrieved),
            % Convert to JSON text
            with_output_to(string(JsonText), json_write(current_output, Retrieved)),
            
            % Verify the price appears as a JSON number, not a string
            % Should be: "price":19.99  not "price":"19.99"
            sub_string(JsonText, _, _, _, '"price":19.99'),
            \+ sub_string(JsonText, _, _, _, '"price":"19.99"')
        )
    ).

test(integer_serializes_as_json_number, [
    setup((setup_temp_store(State),
           create_db_without_schema('admin','test'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Insert a document with an integer value
    Doc = _{'@type': 'Product',
            '@id': 'Product/test2',
            quantity: '42'^^'http://www.w3.org/2001/XMLSchema#integer'},
    
    with_test_transaction(
        _Descriptor,
        C1,
        insert_document(C1, Doc, Id)
    ),
    
    % Retrieve and check JSON serialization
    with_test_transaction(
        _Descriptor2,
        C2,
        (   get_document(C2, Id, Retrieved),
            % Convert to JSON text
            with_output_to(string(JsonText), json_write(current_output, Retrieved)),
            
            % Verify the quantity appears as a JSON number, not a string
            % Should be: "quantity":42  not "quantity":"42"
            sub_string(JsonText, _, _, _, '"quantity":42'),
            \+ sub_string(JsonText, _, _, _, '"quantity":"42"')
        )
    ).

test(big_integer_serializes_as_json_number, [
    setup((setup_temp_store(State),
           create_db_without_schema('admin','test'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Insert a document with a large integer (beyond JavaScript MAX_SAFE_INTEGER)
    BigInt = '9007199254740993',  % 2^53 + 1, beyond JavaScript safe range
    Doc = _{'@type': 'Product',
            '@id': 'Product/test3',
            bigValue: BigInt^^'http://www.w3.org/2001/XMLSchema#integer'},
    
    with_test_transaction(
        _Descriptor,
        C1,
        insert_document(C1, Doc, Id)
    ),
    
    % Retrieve and check JSON serialization
    with_test_transaction(
        _Descriptor2,
        C2,
        (   get_document(C2, Id, Retrieved),
            % Convert to JSON text
            with_output_to(string(JsonText), json_write(current_output, Retrieved)),
            
            % Verify it's a JSON number with full precision
            atom_concat('"bigValue":', BigInt, Expected),
            sub_string(JsonText, _, _, _, Expected),
            % Verify it's not quoted as a string
            atom_concat('"bigValue":"', BigInt, NotExpected),
            \+ sub_string(JsonText, _, _, _, NotExpected)
        )
    ).

test(high_precision_decimal_serializes_as_json_number, [
    setup((setup_temp_store(State),
           create_db_without_schema('admin','test'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Insert a document with 20-digit precision decimal
    HighPrecision = '1.23456789012345678901',
    Doc = _{'@type': 'Product',
            '@id': 'Product/test4',
            preciseValue: HighPrecision^^'http://www.w3.org/2001/XMLSchema#decimal'},
    
    with_test_transaction(
        _Descriptor,
        C1,
        insert_document(C1, Doc, Id)
    ),
    
    % Retrieve and check JSON serialization
    with_test_transaction(
        _Descriptor2,
        C2,
        (   get_document(C2, Id, Retrieved),
            % Convert to JSON text
            with_output_to(string(JsonText), json_write(current_output, Retrieved)),
            
            % Verify it's a JSON number with 20-digit precision
            atom_concat('"preciseValue":', HighPrecision, Expected),
            sub_string(JsonText, _, _, _, Expected),
            % Verify it's not quoted as a string
            atom_concat('"preciseValue":"', HighPrecision, NotExpected),
            \+ sub_string(JsonText, _, _, _, NotExpected)
        )
    ).

test(rational_decimal_serializes_as_json_number_with_20_digits, [
    setup((setup_temp_store(State),
           create_db_without_schema('admin','test'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Insert a document with a rational (1/3) stored internally as rational
    Doc = _{'@type': 'Product',
            '@id': 'Product/test5',
            rational: '0.33333333333333333333'^^'http://www.w3.org/2001/XMLSchema#decimal'},
    
    with_test_transaction(
        _Descriptor,
        C1,
        insert_document(C1, Doc, Id)
    ),
    
    % Retrieve and check JSON serialization
    with_test_transaction(
        _Descriptor2,
        C2,
        (   get_document(C2, Id, Retrieved),
            % Convert to JSON text
            with_output_to(string(JsonText), json_write(current_output, Retrieved)),
            
            % Verify it's a JSON number with 20 digits (not IEEE 754 rounded)
            sub_string(JsonText, _, _, _, '"rational":0.33333333333333333333'),
            % Verify it's not the IEEE 754 representation
            \+ sub_string(JsonText, _, _, _, '"rational":0.3333333333333333'),
            % Verify it's not a string
            \+ sub_string(JsonText, _, _, _, '"rational":"0.33333333333333333333"')
        )
    ).

test(multiple_numeric_types_in_same_document, [
    setup((setup_temp_store(State),
           create_db_without_schema('admin','test'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Insert a document with multiple numeric types
    Doc = _{'@type': 'Product',
            '@id': 'Product/test6',
            intValue: '42'^^'http://www.w3.org/2001/XMLSchema#integer',
            decimalValue: '19.99'^^'http://www.w3.org/2001/XMLSchema#decimal',
            bigInt: '999999999999999999999'^^'http://www.w3.org/2001/XMLSchema#integer',
            highPrecision: '3.14159265358979323846'^^'http://www.w3.org/2001/XMLSchema#decimal'},
    
    with_test_transaction(
        _Descriptor,
        C1,
        insert_document(C1, Doc, Id)
    ),
    
    % Retrieve and check JSON serialization
    with_test_transaction(
        _Descriptor2,
        C2,
        (   get_document(C2, Id, Retrieved),
            % Convert to JSON text
            with_output_to(string(JsonText), json_write(current_output, Retrieved)),
            
            % All should be JSON numbers, not strings
            sub_string(JsonText, _, _, _, '"intValue":42'),
            sub_string(JsonText, _, _, _, '"decimalValue":19.99'),
            sub_string(JsonText, _, _, _, '"bigInt":999999999999999999999'),
            sub_string(JsonText, _, _, _, '"highPrecision":3.14159265358979323846'),
            
            % Verify none are quoted as strings
            \+ sub_string(JsonText, _, _, _, '"intValue":"42"'),
            \+ sub_string(JsonText, _, _, _, '"decimalValue":"19.99"'),
            \+ sub_string(JsonText, _, _, _, '"bigInt":"999999999999999999999"'),
            \+ sub_string(JsonText, _, _, _, '"highPrecision":"3.14159265358979323846"')
        )
    ).

test(zero_and_negative_numbers_serialize_correctly, [
    setup((setup_temp_store(State),
           create_db_without_schema('admin','test'))),
    cleanup(teardown_temp_store(State))
]) :-
    % Test edge cases: zero and negative numbers
    Doc = _{'@type': 'Product',
            '@id': 'Product/test7',
            zeroInt: '0'^^'http://www.w3.org/2001/XMLSchema#integer',
            zeroDecimal: '0.0'^^'http://www.w3.org/2001/XMLSchema#decimal',
            negativeInt: '-42'^^'http://www.w3.org/2001/XMLSchema#integer',
            negativeDecimal: '-19.99'^^'http://www.w3.org/2001/XMLSchema#decimal'},
    
    with_test_transaction(
        _Descriptor,
        C1,
        insert_document(C1, Doc, Id)
    ),
    
    % Retrieve and check JSON serialization
    with_test_transaction(
        _Descriptor2,
        C2,
        (   get_document(C2, Id, Retrieved),
            % Convert to JSON text
            with_output_to(string(JsonText), json_write(current_output, Retrieved)),
            
            % All should be JSON numbers
            sub_string(JsonText, _, _, _, '"zeroInt":0'),
            sub_string(JsonText, _, _, _, '"zeroDecimal":0'),
            sub_string(JsonText, _, _, _, '"negativeInt":-42'),
            sub_string(JsonText, _, _, _, '"negativeDecimal":-19.99')
        )
    ).

:- end_tests(json_number_serialization).
