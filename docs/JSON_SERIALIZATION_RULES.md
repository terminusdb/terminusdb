# JSON Serialization Rules for Numeric Types

**Version:** 2.0  
**Date:** 2025-10-12  
**Status:** Normative Specification

---

## Overview

This document specifies the canonical JSON serialization behavior for all XSD numeric types in TerminusDB. These rules apply to:
- **Document retrieval** (`get_document/3`)
- **GraphQL query results**
- **WOQL query bindings**

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
```

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
- **JSON Serialization**: `src/core/document/json.pl` (json:json_write_hook/4)
- **WOQL Response**: `src/core/query/jsonld.pl` (term_jsonld/3)
- **Arithmetic**: `src/core/query/woql_compile.pl` (compile_arith/3)
- **Precision Constant**: `src/core/triple/casting.pl` (decimal_precision/1)
- **Tests**: `src/core/query/decimal_precision_test.pl`, `tests/test/decimal-precision.js`
