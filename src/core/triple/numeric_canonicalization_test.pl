/** <module> Numeric Canonicalization Tests
 *
 * Tests for numeric canonicalization across all input types and interfaces.
 * Ensures that mathematically equivalent inputs produce identical internal
 * representations and outputs.
 *
 * Canonical Forms (XSD 1.1 Specification):
 * - xsd:decimal (whole): No decimal point (e.g., "2" not "2.0")
 * - xsd:decimal (fractional): Leading zero (e.g., "0.1" not ".1")
 * - xsd:integer: No decimal point (e.g., "2" not "2.0")
 * - xsd:float/double: Decimal point preserved (e.g., "2.0")
 */

:- begin_tests(numeric_canonicalization).
:- use_module(core(triple/casting)).
:- use_module(core(triple/literals)).

/*
 * Test 1: xsd:decimal whole number canonicalization
 * Inputs: 2, 2.0, 2.00 should all produce canonical 2 rdiv 1
 */
test(decimal_whole_number_equivalence, []) :-
    % Test various input forms
    typecast("2"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    typecast("2.0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R2^^_),
    typecast("2.00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R3^^_),
    
    % All should produce the same rational
    assertion(rational(R1)),
    assertion(rational(R2)),
    assertion(rational(R3)),
    
    % All should equal 2 rdiv 1 (simplified to 2)
    assertion(R1 =:= 2),
    assertion(R2 =:= 2),
    assertion(R3 =:= 2),
    
    % Exact structural equality
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 2: xsd:decimal fractional canonicalization with leading zero
 * Inputs: .1, 0.1, 0.10 should all produce canonical 1 rdiv 10
 */
test(decimal_leading_zero_canonicalization, []) :-
    % Test leading zero normalization
    typecast(".1"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    typecast("0.1"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R2^^_),
    typecast("0.10"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R3^^_),
    
    % All should produce the same rational
    assertion(rational(R1)),
    assertion(rational(R2)),
    assertion(rational(R3)),
    
    % All should equal 1 rdiv 10
    assertion(R1 =:= 0.1),
    assertion(R2 =:= 0.1),
    assertion(R3 =:= 0.1),
    
    % Exact structural equality
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 3: xsd:integer canonicalization
 * Inputs: 2, 2.0 should both produce integer 2
 */
test(integer_canonicalization, []) :-
    % Integer from string without decimal
    typecast("2"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#integer', [], R1^^_),
    % Integer from string with decimal (must be whole number)
    typecast("2.0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#integer', [], R2^^_),
    
    % Both should produce integer 2
    assertion(integer(R1)),
    assertion(integer(R2)),
    assertion(R1 =:= 2),
    assertion(R2 =:= 2),
    assertion(R1 = R2).

/*
 * Test 4: Negative number canonicalization
 * Inputs: -2, -2.0, -2.00 should all produce -2 rdiv 1
 */
test(negative_decimal_canonicalization, []) :-
    typecast("-2"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    typecast("-2.0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R2^^_),
    typecast("-2.00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R3^^_),
    
    assertion(rational(R1)),
    assertion(rational(R2)),
    assertion(rational(R3)),
    assertion(R1 =:= -2),
    assertion(R2 =:= -2),
    assertion(R3 =:= -2),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 5: Negative fractional with leading zero
 * Inputs: -.1, -0.1, -0.10 should all produce -1 rdiv 10
 */
test(negative_fractional_leading_zero, []) :-
    typecast("-.1"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    typecast("-0.1"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R2^^_),
    typecast("-0.10"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R3^^_),
    
    assertion(rational(R1)),
    assertion(rational(R2)),
    assertion(rational(R3)),
    assertion(R1 =:= -0.1),
    assertion(R2 =:= -0.1),
    assertion(R3 =:= -0.1),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 6: Trailing zeros are normalized by rational simplification
 * Inputs: 1.10, 1.100, 1.1000 should all produce 11 rdiv 10
 */
test(trailing_zeros_normalized, []) :-
    typecast("1.1"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    typecast("1.10"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R2^^_),
    typecast("1.100"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R3^^_),
    
    assertion(rational(R1)),
    assertion(rational(R2)),
    assertion(rational(R3)),
    assertion(R1 =:= 1.1),
    assertion(R2 =:= 1.1),
    assertion(R3 =:= 1.1),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 7: High precision inputs maintain precision
 * Different string lengths but mathematically equal should be equivalent
 */
test(high_precision_equivalence, []) :-
    % Same value with different trailing zero counts
    typecast("0.33333333333333333333"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    typecast("0.333333333333333333330"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R2^^_),
    
    assertion(rational(R1)),
    assertion(rational(R2)),
    % Should be mathematically equal
    assertion(R1 =:= R2).

/*
 * Test 8: String decimal parsing for literals.pl
 * Verify literals.pl version matches casting.pl version
 */
test(string_decimal_to_rational_leading_zero, []) :-
    % Test the low-level function directly
    string_decimal_to_rational(".1", R1),
    string_decimal_to_rational("0.1", R2),
    string_decimal_to_rational("0.10", R3),
    
    assertion(rational(R1)),
    assertion(rational(R2)),
    assertion(rational(R3)),
    assertion(R1 =:= 0.1),
    assertion(R2 =:= 0.1),
    assertion(R3 =:= 0.1),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 9: String decimal parsing for whole numbers
 * Verify whole number variations are equivalent
 */
test(string_decimal_to_rational_whole_numbers, []) :-
    string_decimal_to_rational("2", R1),
    string_decimal_to_rational("2.0", R2),
    string_decimal_to_rational("2.00", R3),
    
    assertion(R1 =:= 2),
    assertion(R2 =:= 2),
    assertion(R3 =:= 2),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 10: Arithmetic with canonicalized inputs
 * Operations on equivalent inputs should produce identical results
 */
test(arithmetic_with_canonical_inputs, []) :-
    % Parse equivalent decimals
    typecast("0.1"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], A1^^_),
    typecast(".1"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], A2^^_),
    typecast("0.10"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], A3^^_),
    
    % All arithmetic results should be identical
    R1 is A1 + A1,
    R2 is A2 + A2,
    R3 is A3 + A3,
    
    assertion(R1 =:= 0.2),
    assertion(R2 =:= 0.2),
    assertion(R3 =:= 0.2),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 11: Rational to string round-trip preserves canonicalization
 * Converting to string and back should maintain canonical form
 */
test(rational_to_string_roundtrip, []) :-
    % Start with canonical input
    typecast("0.1"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    
    % Convert to string
    decimal_precision(Precision),
    rational_to_decimal_string(R1, Str, Precision),
    
    % Parse back
    string_decimal_to_rational(Str, R2),
    
    % Should be mathematically equal
    assertion(R1 =:= R2).

/*
 * Test 12: Very small decimals with leading zeros
 * Inputs: .001, 0.001, 0.0010 should all be equivalent
 */
test(small_decimal_leading_zeros, []) :-
    typecast(".001"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    typecast("0.001"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R2^^_),
    typecast("0.0010"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R3^^_),
    
    assertion(R1 =:= 0.001),
    assertion(R2 =:= 0.001),
    assertion(R3 =:= 0.001),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 13: Zero representations are canonical
 * Inputs: 0, 0.0, 0.00, .0 should all be zero
 */
test(zero_canonicalization, []) :-
    typecast("0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    typecast("0.0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R2^^_),
    typecast("0.00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R3^^_),
    typecast(".0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R4^^_),
    
    assertion(R1 =:= 0),
    assertion(R2 =:= 0),
    assertion(R3 =:= 0),
    assertion(R4 =:= 0),
    assertion(R1 = R2),
    assertion(R2 = R3),
    assertion(R3 = R4).

/*
 * Test 14: Large whole numbers with decimal points
 * Inputs: 1000000, 1000000.0, 1000000.00 should be equivalent
 */
test(large_whole_number_canonicalization, []) :-
    typecast("1000000"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R1^^_),
    typecast("1000000.0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R2^^_),
    typecast("1000000.00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], R3^^_),
    
    assertion(R1 =:= 1000000),
    assertion(R2 =:= 1000000),
    assertion(R3 =:= 1000000),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 15: Comparison operations use canonical forms
 * Equivalent inputs should compare as equal
 */
test(comparison_with_canonical_inputs, []) :-
    typecast("0.5"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], A^^_),
    typecast("0.50"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], B^^_),
    
    % Should be equal
    assertion(A =:= B),
    assertion(A = B),
    
    % Comparisons should work correctly
    assertion(\+ (A < B)),
    assertion(\+ (A > B)),
    assertion(A =< B),
    assertion(A >= B).

/*
 * Test 16: xsd:double canonicalization (THE ORIGINAL BUG)
 * Inputs: 33, 33.0, 33.00 should all produce the same float
 * This tests the fix for: https://github.com/terminusdb/terminusdb/issues/...
 */
test(double_whole_number_equivalence, []) :-
    % Test various input forms
    typecast("33"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], R1^^T1),
    typecast("33.0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], R2^^T2),
    typecast("33.00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], R3^^T3),
    
    % All should be the same type
    assertion(T1 = 'http://www.w3.org/2001/XMLSchema#double'),
    assertion(T2 = 'http://www.w3.org/2001/XMLSchema#double'),
    assertion(T3 = 'http://www.w3.org/2001/XMLSchema#double'),
    
    % All should produce the same float
    assertion(float(R1)),
    assertion(float(R2)),
    assertion(float(R3)),
    
    % All should equal 33.0
    assertion(R1 =:= 33.0),
    assertion(R2 =:= 33.0),
    assertion(R3 =:= 33.0),
    
    % Exact structural equality (critical for the bug fix)
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 17: xsd:double zero canonicalization
 * Inputs: 0, 0.0, 0.00 should all produce the same float zero
 */
test(double_zero_canonicalization, []) :-
    typecast("0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], R1^^_),
    typecast("0.0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], R2^^_),
    typecast("0.00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], R3^^_),
    
    assertion(float(R1)),
    assertion(float(R2)),
    assertion(float(R3)),
    assertion(R1 =:= 0.0),
    assertion(R2 =:= 0.0),
    assertion(R3 =:= 0.0),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 18: xsd:double with fractional part
 * Inputs: 2.5, 2.50, 2.500 should all be equivalent
 */
test(double_fractional_canonicalization, []) :-
    typecast("2.5"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], R1^^_),
    typecast("2.50"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], R2^^_),
    typecast("2.500"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], R3^^_),
    
    assertion(float(R1)),
    assertion(float(R2)),
    assertion(float(R3)),
    assertion(R1 =:= 2.5),
    assertion(R2 =:= 2.5),
    assertion(R3 =:= 2.5),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 19: xsd:float canonicalization
 * Inputs: 33, 33.0, 33.00 should all produce the same float
 */
test(float_whole_number_equivalence, []) :-
    typecast("33"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#float', [], R1^^T1),
    typecast("33.0"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#float', [], R2^^T2),
    typecast("33.00"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#float', [], R3^^T3),
    
    assertion(T1 = 'http://www.w3.org/2001/XMLSchema#float'),
    assertion(T2 = 'http://www.w3.org/2001/XMLSchema#float'),
    assertion(T3 = 'http://www.w3.org/2001/XMLSchema#float'),
    
    assertion(float(R1)),
    assertion(float(R2)),
    assertion(float(R3)),
    assertion(R1 =:= 33.0),
    assertion(R2 =:= 33.0),
    assertion(R3 =:= 33.0),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 20: xsd:float with fractional part
 * Inputs: 1.5, 1.50, 1.500 should all be equivalent
 */
test(float_fractional_canonicalization, []) :-
    typecast("1.5"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#float', [], R1^^_),
    typecast("1.50"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#float', [], R2^^_),
    typecast("1.500"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#float', [], R3^^_),
    
    assertion(float(R1)),
    assertion(float(R2)),
    assertion(float(R3)),
    assertion(R1 =:= 1.5),
    assertion(R2 =:= 1.5),
    assertion(R3 =:= 1.5),
    assertion(R1 = R2),
    assertion(R2 = R3).

/*
 * Test 21: xsd:double vs xsd:decimal (different types should NOT be equal)
 * 33 as double should NOT structurally equal 33 as decimal
 */
test(double_vs_decimal_different_types, []) :-
    typecast("33"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#double', [], Double^^DType),
    typecast("33"^^'http://www.w3.org/2001/XMLSchema#string',
             'http://www.w3.org/2001/XMLSchema#decimal', [], Decimal^^DecType),
    
    % Should be numerically equal
    assertion(Double =:= Decimal),
    
    % But structurally different (float vs rational)
    assertion(Double \= Decimal),
    assertion(DType \= DecType).

:- end_tests(numeric_canonicalization).
