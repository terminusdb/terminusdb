# JSON Serialization Rules for Numeric Types

**Version:** 3.1  
**Date:** 2025-10-18  
**Status:** Normative Specification  
**Last Updated:** 2025-10-18 (Added String Serialization section)

---

## Overview

This document specifies the canonical JSON serialization behavior for all XSD numeric types in TerminusDB. These rules apply to:
- **Document retrieval** (`get_document/3`)
- **GraphQL query results**
- **WOQL query bindings**
- **Document insertion and updates**

**Key Principle**: All numeric inputs are canonicalized at the input parsing stage according to their XSD type, ensuring consistent representation and mathematical equivalence across all interfaces.

---

## Numeric Type Hierarchy

TerminusDB strictly follows the XML Schema Datatypes hierarchy:

```
Numeric Types
├── xsd:decimal (Rational arithmetic, arbitrary precision)
│   ├── xsd:integer (GMP arbitrary precision)
│   │   ├── xsd:nonNegativeInteger
│   │   │   ├── xsd:positiveInteger
│   │   │   └── xsd:unsignedLong
│   │   ├── xsd:nonPositiveInteger
│   │   │   └── xsd:negativeInteger
│   │   └── xsd:long
│   │       └── xsd:int
│   │           └── xsd:short
│   │               └── xsd:byte
│
└── IEEE 754 Floating Point (NOT derived from xsd:decimal)
    ├── xsd:float (32-bit)
    └── xsd:double (64-bit)
```

**Important**: `xsd:float` and `xsd:double` are **not** subtypes of `xsd:decimal`. They use different internal representations and arithmetic semantics.

---

## Canonicalization Rules

### Input Canonicalization

All numeric inputs are canonicalized according to their target XSD type:

| Input Examples | Target Type | Canonical Internal | Canonical Output |
|----------------|-------------|-------------------|------------------|
| `2`, `2.0`, `2.00` | `xsd:decimal` | `2 rdiv 1` | `2` |
| `.1`, `0.1`, `0.10` | `xsd:decimal` | `1 rdiv 10` | `0.1` |
| `2`, `2.0` | `xsd:integer` | `2` (GMP int) | `2` |
| `2`, `2.0` | `xsd:float` | `2.0` (IEEE 754) | `2.0` |
| `2`, `2.0` | `xsd:double` | `2.0` (IEEE 754) | `2.0` |

### Canonical Form Rules

**xsd:decimal (whole numbers)**:
- **Canonical**: No decimal point (e.g., `2`)
- **Non-canonical**: With decimal point (e.g., `2.0`, `2.00`)
- **Rule**: Inputs `2`, `2.0`, `2.00` all canonicalize to `2`

**xsd:decimal (fractional)**:
- **Canonical**: Leading zero before decimal point (e.g., `0.1`)
- **Non-canonical**: No leading zero (e.g., `.1`)
- **Rule**: Input `.1` canonicalizes to `0.1`

**xsd:integer**:
- **Canonical**: No decimal point (e.g., `2`)
- **Non-canonical**: With decimal point (e.g., `2.0`)
- **Rule**: Input `2.0` must represent a whole number, stored as `2`

**xsd:float / xsd:double**:
- **Canonical**: Decimal point preserved (e.g., `2.0`)
- **Note**: Whole numbers maintain `.0` for float/double types
- **Rule**: Input `2` becomes `2.0` for float/double types

---

## Core Serialization Rules

### Rule: ALL Numeric Types → JSON Numbers

**ALL XSD numeric types serialize as JSON numbers with appropriate precision handling.**

| XSD Type | JSON Serialization | Precision | Example |
|----------|-------------------|-----------|---------|
| `xsd:decimal` | Number (20 digits) | Arbitrary precision via rationals | `{"value": 0.333333333333333333}` |
| `xsd:integer` | Number | Arbitrary precision integers | `{"value": 9007199254740993}` |
| `xsd:float` | Number | IEEE 754 single (7 digits) | `{"value": 12.34}` |
| `xsd:double` | Number | IEEE 754 double (15-17 digits) | `{"value": 12.34}` |
| `xsd:long` | Number | 64-bit integer | `{"value": 9223372036854775807}` |
| `xsd:int` | Number | 32-bit integer | `{"value": 2147483647}` |

### Key Points

1. **xsd:decimal uses rationals internally** - Provides exact arbitrary-precision arithmetic
2. **JSON output via `json:json_write_hook/4`** - Rationals formatted with 20-digit precision
3. **Integers preserved exactly** - No loss of precision for large integers
4. **Floats use IEEE 754** - Native floating-point representation

---

## Precision Handling

### xsd:decimal (Rationals)

**Internal Representation**: Prolog rationals (`1 rdiv 3`)  
**Storage**: Decimal strings in Rust layer (20-digit precision)  
**JSON Output**: JSON numbers with up to 20 significant digits

```json
{
  "@type": "xsd:decimal",
  "@value": 0.33333333333333333333
}
```

**Key Feature**: Trailing zeros are normalized (e.g., `0.10000...` → `0.1`)

### xsd:integer (Arbitrary Precision)

**Internal Representation**: Prolog integers (GMP-backed, unlimited)  
**Storage**: Native integers in Rust  
**JSON Output**: JSON numbers (exact, even > MAX_SAFE_INTEGER)

```json
{
  "@type": "xsd:integer",
  "@value": 12345678901234567890
}
```

**Warning**: JavaScript clients must use BigInt or string parsing for integers > 2^53-1

### xsd:float / xsd:double

**Internal Representation**: Prolog floats (IEEE 754 double precision)  
**Storage**: Native floats in Rust  
**JSON Output**: JSON numbers

```json
{
  "@type": "xsd:double",
  "@value": 3.141592653589793
}
```

**Normalization**: Integer-valued floats normalized (e.g., `2012.0` → `2012`)

---

## Implementation Architecture

### Layer 1: Storage (Rust ↔ Prolog)

**File**: `src/core/triple/literals.pl`

```prolog
% ground_object_storage/2 - Prolog → Rust storage
ground_object_storage(Val^^Type, value(StorageVal,Type)) :-
    rational(Val),
    rational(Val, Num, Den),
    (   Den =:= 1
    ->  StorageVal = Num  % Integer rationals → integers
    ;   (   Type = 'xsd:decimal'
        ->  % Rationals → decimal strings (20 digits)
            decimal_precision(Precision),  % Precision = 20
            rational_to_decimal_string(Val, StorageVal, Precision)
        ;   % Float types → floats
            StorageVal is float(Val)
        )
    ).
```

**Storage Format**:
- `xsd:decimal` rationals → `PrologText` strings (e.g., `"0.33333333333333333333"`)
- `xsd:integer` → Native integers
- `xsd:float`/`double` → Native floats

### Layer 2: Retrieval (Rust → Prolog)

**File**: `src/core/triple/literals.pl`

```prolog
% storage_object/2 - Rust storage → Prolog
storage_object(value(Val,T),Rational^^T) :-
    (   T = 'xsd:decimal'),
    (   string(Val)
    ->  % New format: stored as decimal string
        string_decimal_to_rational(Val, Rational)  % Parse to rational
    ;   number(Val)
    ->  % Legacy format: stored as float
        format(string(S), "~w", [Val]),
        string_decimal_to_rational(S, Rational)  % Convert via string
    ).
```

**Retrieval Behavior**:
- Decimal strings → Converted to rationals
- Legacy floats → Converted to rationals (with precision loss)
- Integers → Used directly

### Layer 3: WOQL/Document Response

**File**: `src/core/query/jsonld.pl`

```prolog
% term_jsonld/3 - Internal representation → JSON structure
term_jsonld(D^^T,Prefixes,json{'@type' : TC, '@value' : V}) :-
    (   T = 'xsd:integer'
    ->  V = D  % Integers as-is
    ;   T = 'xsd:float' ; T = 'xsd:double'
    ->  (   float(D), D =:= floor(D)
        ->  V is truncate(D)  % Normalize 2012.0 → 2012
        ;   V = D
        )
    ;   T = 'xsd:decimal'
    ->  (   number(D), D =:= floor(D)
        ->  V is truncate(D)  % Normalize whole number rationals
        ;   V = D  % Keep rational
        )
    ),
    compress_dict_uri(T, Prefixes, TC).
```

**Key Behavior**: Rationals stay as rationals in Prolog dict structure

### Layer 4: JSON Serialization

**File**: `src/core/document/json.pl`

```prolog
% json:json_write_hook/4 - Prolog rational → JSON number output
json:json_write_hook(Term, Stream, _State, _Options) :-
    rational(Term),
    \+ integer(Term),
    !,
    Precision = 20,  % 20-digit precision
    format(string(FormatStr), '~~~wf', [Precision]),
    format(string(S), FormatStr, [Term]),
    normalize_decimal(S, Normalized),  % Remove trailing zeros
    format(Stream, '~w', [Normalized]).
```

**Output**:
- `1 rdiv 3` → `0.33333333333333333333` (20 digits)
- `1 rdiv 10` → `0.1` (trailing zeros removed)
- Integers → Pass through unchanged

---

## Examples

### Document API

**Schema:**
```json
{
  "@type": "Class",
  "@id": "Product",
  "price": "xsd:decimal",
  "quantity": "xsd:integer",
  "weight": "xsd:double"
}
```

**Insert:**
```json
{
  "@type": "Product",
  "price": "19.99",
  "quantity": 42,
  "weight": 2.5
}
```

**Retrieve:**
```json
{
  "@id": "Product/...",
  "@type": "Product",
  "price": 19.99,      // JSON number (exact rational)
  "quantity": 42,      // JSON number (integer)
  "weight": 2.5        // JSON number (float)
}
```

### WOQL Query

**Query:**
```javascript
WOQL.triple("v:Product", "price", "v:Price")
```

**Result:**
```json
{
  "bindings": [{
    "Price": {
      "@type": "xsd:decimal",
      "@value": 19.99    // JSON number with rational precision
    }
  }]
}
```

### High-Precision Arithmetic

**Query:**
```javascript
WOQL.eval(
  WOQL.div(1, 3),  // Uses rdiv internally for rational division
  "v:Result"
)
```

**Result:**
```json
{
  "bindings": [{
    "Result": {
      "@type": "xsd:decimal",
      "@value": 0.33333333333333333333  // 20-digit precision
    }
  }]
}
```

---

## Client-Side Handling

### JavaScript Clients

**Integers > MAX_SAFE_INTEGER:**
```javascript
// ❌ Incorrect: Loses precision
const id = doc.id;  // 9007199254740993 → 9007199254740992

// ✅ Correct: Use BigInt
const id = BigInt(doc.id);  // Preserves exact value

// ✅ Alternative: Custom JSON parser
const data = JSON.parse(jsonString, (key, value) => {
  if (typeof value === 'number' && Math.abs(value) > Number.MAX_SAFE_INTEGER) {
    return BigInt(value);
  }
  return value;
});
```

**Decimals:**
```javascript
// Native JSON parsing works for most cases
const price = doc.price;  // 19.99 as number

// For financial calculations, use decimal.js or similar
import Decimal from 'decimal.js';
const price = new Decimal(doc.price);  // Arbitrary precision

const data = JSON.parse(jsonString, (key, value) => {
  if (typeof value === 'number') {
    return new Decimal(value);
  }
  return value;
});
```

---

## String Serialization (Typecast to xsd:string)

### Overview

When numeric types are typecast to `xsd:string`, the serialization follows type-specific rules to maintain type semantics and ensure proper round-tripping.

### Typecast Rules by Type

**File**: `src/core/triple/casting.pl` (lines 406-442)

| Source Type | String Format | Example | Rule |
|-------------|--------------|---------|------|
| `xsd:double` | Always with decimal point | `33` → `"33.0"` | Whole numbers get `.0` suffix |
| `xsd:float` | Always with decimal point | `33` → `"33.0"` | Whole numbers get `.0` suffix |
| `xsd:decimal` | Minimal form | `33` → `"33"` | No trailing zeros |
| `xsd:integer` | No decimal point | `33` → `"33"` | Integer form |

### Implementation Details

#### xsd:double and xsd:float to String

```prolog
%%% xsd:double => xsd:string
typecast_switch('http://www.w3.org/2001/XMLSchema#string', 
                'http://www.w3.org/2001/XMLSchema#double', 
                Val, _, S^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    (   number(Val)
    ->  % CRITICAL: Ensure doubles always have decimal point
        % xsd:double values must have .0 for whole numbers (IEEE 754 semantics)
        (   (float(Val) ; integer(Val)),
            Val =:= floor(Val)
        ->  % Whole number: add .0 suffix
            Truncated is truncate(Val),
            format(string(S), "~w.0", [Truncated])
        ;   % Fractional float: use normal representation
            format(string(S), "~w", [Val])
        )
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#double'),_))).
```

**Rationale**: 
- IEEE 754 types represent **real numbers**, not integers
- String representation should reflect floating-point nature
- Enables type distinction in string form (`"33.0"` vs `"33"`)
- Ensures proper round-tripping: `xsd:double → string → xsd:double`

### Numeric JSON Input Handling

**Problem Fixed** (2025-10-18): Numeric JSON values for xsd:double/float types were losing decimal notation.

#### Before Fix

```json
{
  "@type": "Typecast",
  "value": { "@type": "xsd:double", "@value": 33 },
  "type": "xsd:string",
  "result": { "variable": "Result" }
}
```

**Output (Bug)**: `{ "Result": "33" }` ❌ Missing decimal point

#### After Fix

**Output (Fixed)**: `{ "Result": "33.0" }` ✅ Correct

### JSON Input Processing

**Files Modified**:
1. `src/core/query/json_woql.pl` - WOQL JSON parsing
2. `src/core/document/json.pl` - Document JSON parsing

```prolog
% json_woql.pl - Convert integer JSON values to floats for xsd:double/float
json_value_cast_type(V,Type,WOQL) :-
    ...
    (   integer(V)
    ->  % CRITICAL FIX: Convert integers to floats for xsd:double/xsd:float types
        (   is_float_or_double_type(TE)
        ->  FloatV is float(V),
            typecast(FloatV^^TE, TE, [], Val)
        ;   typecast(V^^xsd:decimal, TE, [], Val)
        )
    ...
```

```prolog
% json.pl - Convert integer JSON values to floats in document parsing
json_type_to_value_type(J, T, X, T) :-
    number(J),
    !,
    % CRITICAL FIX: Convert integers to floats for xsd:double/xsd:float types
    (   integer(J),
        (   T = 'http://www.w3.org/2001/XMLSchema#float'
        ;   T = 'http://www.w3.org/2001/XMLSchema#double'
        ;   T = 'xsd:float'
        ;   T = 'xsd:double'
        )
    ->  X is float(J)  % Convert integer to float
    ;   X = J          % Keep as-is for other numeric types
    ).
```

### Examples

#### Example 1: Typecast with String Values

```json
{
  "@type": "And",
  "and": [
    {
      "@type": "Equals",
      "left": { "variable": "Val" },
      "right": { "data": { "@type": "xsd:double", "@value": "33" } }
    },
    {
      "@type": "Typecast",
      "value": { "variable": "Val" },
      "type": { "node": "xsd:string" },
      "result": { "variable": "AsString" }
    }
  ]
}
```

**Result**:
```json
{
  "bindings": [{
    "AsString": { "@type": "xsd:string", "@value": "33.0" },
    "Val": { "@type": "xsd:double", "@value": 33 }
  }]
}
```

#### Example 2: Typecast with Numeric Values

```json
{
  "@type": "And",
  "and": [
    {
      "@type": "Equals",
      "left": { "variable": "Val" },
      "right": { "data": { "@type": "xsd:double", "@value": 33 } }
    },
    {
      "@type": "Typecast",
      "value": { "variable": "Val" },
      "type": { "node": "xsd:string" },
      "result": { "variable": "AsString" }
    }
  ]
}
```

**Result** (After Fix):
```json
{
  "bindings": [{
    "AsString": { "@type": "xsd:string", "@value": "33.0" },
    "Val": { "@type": "xsd:double", "@value": 33 }
  }]
}
```

#### Example 3: All Numeric Types

```json
// Input: { "@value": 33 } for each type
// String typecast results:

{ "@type": "xsd:double",  "@value": 33 } → "33.0"  // With decimal
{ "@type": "xsd:float",   "@value": 33 } → "33.0"  // With decimal  
{ "@type": "xsd:decimal", "@value": 33 } → "33"    // Minimal form
{ "@type": "xsd:integer", "@value": 33 } → "33"    // Integer form
```

#### Example 4: Fractional Values

```json
// Fractional values work correctly for all types
{ "@type": "xsd:double",  "@value": 33.5 } → "33.5"
{ "@type": "xsd:float",   "@value": 33.5 } → "33.5"
{ "@type": "xsd:decimal", "@value": 33.5 } → "33.5"
```

### Testing

**Test File**: `tests/test/float-double-string-representation.js`

**Coverage**: 8 comprehensive tests
- ✅ xsd:double with string value → `"33.0"`
- ✅ xsd:float with string value → `"33.0"`
- ✅ xsd:double with numeric value → `"33.0"` (Fixed 2025-10-18)
- ✅ xsd:float with numeric value → `"33.0"` (Fixed 2025-10-18)
- ✅ xsd:decimal → `"33"` (minimal form)
- ✅ xsd:integer → `"33"` (no decimal)
- ✅ Fractional values preserved
- ✅ Edge cases (zero values)

```bash
# Run string serialization tests
npx mocha tests/test/float-double-string-representation.js --timeout 10000
```

### Design Rationale

**Why Always Add `.0` for xsd:double/xsd:float?**

1. **Type Semantics**: Float/double represent **real numbers**, not integers
2. **IEEE 754 Compliance**: String form should reflect floating-point nature  
3. **Type Distinction**: `"33.0"` clearly indicates float vs `"33"` for integer
4. **Round-Tripping**: Ensures `xsd:double → string → xsd:double` preserves type
5. **XSD Specification**: Lexical representation distinguishes float from integer types

**Why Not for xsd:decimal?**

- `xsd:decimal` can represent both integers and non-integers
- Both `"33"` and `"33.0"` are valid canonical forms
- Minimal form `"33"` is more concise and equally correct
- Decimal arithmetic uses rationals, not floats

### Client Usage

#### JavaScript

```javascript
// Query with numeric value (works correctly after fix)
const query = {
  "@type": "Typecast",
  value: { "@type": "xsd:double", "@value": 33 },  // Numeric value
  type: "xsd:string",
  result: { variable: "Result" }
};

// Result: { "Result": "33.0" } ✅
```

#### GraphQL

```graphql
query {
  typecast(value: 33.0, type: "xsd:double", asType: "xsd:string")
}
```

**Result**: `"33.0"`

---

## Arithmetic Operations

### WOQL Operators

| Operator | Internal | Precision | Example Result |
|----------|----------|-----------|----------------|
| `Plus` | `+` | Rational | `0.1 + 0.2 = 0.3` (exact) |
| `Minus` | `-` | Rational | `1 - 0.9 = 0.1` (exact) |
| `Times` | `*` | Rational | `0.1 * 3 = 0.3` (exact) |
| `Divide` | `rdiv` | Rational | `1 / 3 = 0.33333...` (20 digits) |
| `Div` | `div` | Integer | `7 div 3 = 2` |
| `Exp` | `**` | Rational | `2 ** 10 = 1024` |
| `Floor` | `floor` | Integer | `floor(3.7) = 3` |

**Critical**: Division (`/`) is automatically replaced with `rdiv` for rational precision

---

## Testing

### Test Coverage

✅ **Prolog Tests**: 14 tests in `decimal_precision_test.pl`  
✅ **JavaScript Tests**: 43 tests in `decimal-precision.js`  
✅ **Cross-Interface Tests**: Document API ↔ GraphQL ↔ WOQL consistency

### Key Test Scenarios

**Test 1: 20-Digit Precision**
```prolog
test(twenty_digit_precision) :-
    Query = typecast("12345678901234567890.1234567890123456789"^^xsd:string,
                     xsd:decimal, "v:Result"),
    run_query(Query, Bindings),
    Result = Bindings.'Result',
    assertion(rational(Result)),
    assertion(Result =:= 12345678901234567890.1234567890123456789).
```

**Test 2: Rational Division**
```prolog
test(rational_division) :-
    Query = eval(1 / 3, "v:Result"),
    run_query(Query, JSON),
    JSON.bindings[0].'Result'.'@value' = 0.33333333333333333333.
```

**Test 3: Large Integer**
```prolog
test(large_integer) :-
    Query = typecast(9007199254740993^^xsd:integer, xsd:integer, "v:Result"),
    run_query(Query, JSON),
    JSON.bindings[0].'Result'.'@value' = 9007199254740993.  % Exact
```

---

## Migration Guide

This version represents the **current behavior**. Previous versions had different serialization rules.

### From Version 1.0 (Strings for Precision Types)

**Not applicable** - Version 1.0 documentation was incorrect. The system has always used JSON numbers.

### Best Practices

1. **Use `xsd:decimal` for financial data** - Exact rational arithmetic
2. **Use `xsd:integer` for IDs** - Arbitrary precision integers
3. **Use `xsd:double` for measurements** - IEEE 754 floats
4. **Client-side**: Use BigInt for large integers, Decimal.js for financial math

---

## FAQ

**Q: Are decimals stored as strings or numbers?**  
A: Internally as rationals, stored as decimal strings in Rust, serialized as JSON numbers with 20-digit precision.

**Q: Can I use integers larger than MAX_SAFE_INTEGER?**  
A: Yes, but JavaScript clients must use BigInt or custom parsing.

**Q: How many digits of precision for xsd:decimal?**  
A: 20 significant digits in JSON output. Internal rationals have unlimited precision.

**Q: What about backwards compatibility?**  
A: This documents the current behavior. The system has always used JSON numbers for numeric types.

**Q: Why 20 digits?**  
A: Matches XSD 1.1 decimal precision requirements and handles most financial/scientific use cases.

**Q: What happens to trailing zeros in decimal values?**  
A: Trailing zeros after the decimal point may be normalized away per JSON specification. For example, `0.12345678901234567890` may become `0.1234567890123456789`. This is **correct behavior** - both representations are mathematically equal and have the same precision. The significant digits are preserved (20 digits), but trailing zeros are semantically redundant in JSON numbers. Use values like `0.01234567890123456789` (no trailing zero) when testing to avoid confusion and to ensure consistent results across different JSON parsers.

**Example:**
```json
// Input (stored)
{ "value": "0.12345678901234567890" }

// Output (may be normalized)
{ "value": 0.1234567890123456789 }  // Trailing zero removed, precision maintained
```

---

## References

- **XSD 1.1 Part 2**: https://www.w3.org/TR/xmlschema11-2/
- **JSON Specification (RFC 8259)**: https://datatracker.ietf.org/doc/html/rfc8259
- **Prolog Rationals**: https://www.swi-prolog.org/pldoc/man?section=rational
- **IEEE 754**: https://en.wikipedia.org/wiki/IEEE_754
- **JavaScript BigInt**: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- **Decimal.js**: https://github.com/MikeMcl/decimal.js/

---

## Implementation Files

- **Storage Layer**: `src/core/triple/literals.pl` (ground_object_storage, storage_object)
- **Type Casting**: `src/core/triple/casting.pl` (typecast_switch/5, decimal_precision/1)
- **JSON Serialization**: `src/core/document/json.pl` (json:json_write_hook/4, json_type_to_value_type/4)
- **WOQL JSON Parsing**: `src/core/query/json_woql.pl` (json_value_cast_type/3)
- **WOQL Response**: `src/core/query/jsonld.pl` (term_jsonld/3)
- **Arithmetic**: `src/core/query/woql_compile.pl` (compile_arith/3)
- **Tests**: 
  - `src/core/query/decimal_precision_test.pl` - Prolog precision tests
  - `tests/test/decimal-precision.js` - JavaScript precision tests
  - `tests/test/float-double-string-representation.js` - String serialization tests (8 tests)
