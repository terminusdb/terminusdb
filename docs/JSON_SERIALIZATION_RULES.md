# JSON Serialization Rules for Numeric Types

**Version:** 1.0  
**Date:** 2025-10-11  
**Status:** Normative Specification

---

## Overview

This document specifies the canonical JSON serialization behavior for all XSD numeric types in TerminusDB. These rules apply to:
- **Document retrieval** (`get_document/3`)
- **GraphQL query results**
- **WOQL query bindings**

---

## Core Serialization Rules

### Rule 1: Precision Types → JSON Strings

**ALL numeric XSD types except `xsd:float` and `xsd:double` MUST serialize as JSON strings.**

| XSD Type | JSON Serialization | Example |
|----------|-------------------|---------|
| `xsd:decimal` | String | `{"value": "12.34"}` |
| `xsd:integer` | String | `{"value": "42"}` |
| `xsd:nonNegativeInteger` | String | `{"value": "123"}` |
| `xsd:positiveInteger` | String | `{"value": "1"}` |
| `xsd:negativeInteger` | String | `{"value": "-5"}` |
| `xsd:nonPositiveInteger` | String | `{"value": "-10"}` |
| `xsd:long` | String | `{"value": "9223372036854775807"}` |
| `xsd:int` | String | `{"value": "2147483647"}` |
| `xsd:short` | String | `{"value": "32767"}` |
| `xsd:byte` | String | `{"value": "127"}` |
| `xsd:unsignedLong` | String | `{"value": "18446744073709551615"}` |
| `xsd:unsignedInt` | String | `{"value": "4294967295"}` |
| `xsd:unsignedShort` | String | `{"value": "65535"}` |
| `xsd:unsignedByte` | String | `{"value": "255"}` |

### Rule 2: Float Types → JSON Numbers

**ONLY `xsd:float` and `xsd:double` serialize as JSON numbers.**

| XSD Type | JSON Serialization | Example |
|----------|-------------------|---------|
| `xsd:float` | Number | `{"value": 12.34}` |
| `xsd:double` | Number | `{"value": 12.34}` |

---

## Rationale

### Why Strings for Precision Types?

**1. Precision Preservation**
- JavaScript `Number` is IEEE 754 float64 (15-17 digits precision)
- Large integers > 2^53-1 (9007199254740991) lose precision in JavaScript
- Example: `9007199254740993` becomes `9007199254740992` in JavaScript

**2. Type Safety**
- TerminusDB's typing system recognizes string-encoded numbers
- Schema validation works correctly with string values
- Prevents silent data corruption

**3. Consistency**
- All precision-critical types use the same serialization strategy
- Predictable behavior across all integer and decimal types

**4. Industry Standard**
- JSON doesn't have native integer types (only IEEE 754 numbers)
- Many APIs serialize IDs and large numbers as strings (Twitter, GitHub, etc.)

### Why Numbers for Float Types?

**1. IEEE 754 Alignment**
- `xsd:float` and `xsd:double` are defined as IEEE 754 types
- Native JSON numbers are IEEE 754
- Perfect semantic match

**2. Performance**
- Client-side numeric operations work directly
- No string parsing overhead

**3. Compatibility**
- Most JavaScript libraries expect floats as numbers
- Charting, graphing, and math libraries work seamlessly

---

## Examples

### Document Retrieval

**Schema:**
```json
{
  "@type": "Class",
  "@id": "Product",
  "@key": {"@type": "Lexical", "@fields": ["sku"]},
  "sku": "xsd:integer",
  "price": "xsd:decimal",
  "weight": "xsd:float",
  "rating": "xsd:double"
}
```

**Inserted Document:**
```json
{
  "@type": "Product",
  "sku": "123456789012345678",
  "price": "19.99",
  "weight": 2.5,
  "rating": 4.7
}
```

**Retrieved Document (`get_document/3`):**
```json
{
  "@id": "Product/123456789012345678",
  "@type": "Product",
  "sku": "123456789012345678",     // String (xsd:integer)
  "price": "19.99",                 // String (xsd:decimal)
  "weight": 2.5,                    // Number (xsd:float)
  "rating": 4.7                     // Number (xsd:double)
}
```

### WOQL Query Bindings

**Query:**
```javascript
WOQL.triple("v:Product", "price", "v:Price")
```

**Result:**
```json
{
  "bindings": [
    {
      "Product": {"@type": "@id", "@value": "Product/123456789012345678"},
      "Price": {"@type": "xsd:decimal", "@value": "19.99"}  // String
    }
  ]
}
```

### GraphQL Results

**Query:**
```graphql
query {
  Product {
    sku
    price
    weight
    rating
  }
}
```

**Result:**
```json
{
  "data": {
    "Product": [
      {
        "sku": "123456789012345678",  // String
        "price": "19.99",             // String
        "weight": 2.5,                // Number
        "rating": 4.7                 // Number
      }
    ]
  }
}
```

---

## Workarounds

### Scenario: Need JSON Numbers for Precision Types

If your application **requires** native JSON numbers but needs integer/decimal fields:

#### Option 1: Use Float Types in Schema (Recommended)

**Trade-off**: Lose precision beyond ~15 digits, gain native JSON numbers

```json
{
  "@type": "Class",
  "@id": "Coordinate",
  "latitude": "xsd:double",   // Will serialize as number
  "longitude": "xsd:double"   // Will serialize as number
}
```

**Result:**
```json
{
  "latitude": 41.2008,   // Number (not "41.2008")
  "longitude": -73.9254  // Number (not "-73.9254")
}
```

#### Option 2: Typecast at Query Time

**Trade-off**: Extra query complexity, per-query conversion

```javascript
WOQL.and(
  WOQL.triple("v:Product", "price", "v:PriceDecimal"),
  WOQL.typecast("v:PriceDecimal", "xsd:float", "v:PriceFloat")
)
```

**Result:**
```json
{
  "bindings": [{
    "PriceDecimal": {"@type": "xsd:decimal", "@value": "19.99"},  // String
    "PriceFloat": {"@type": "xsd:float", "@value": 19.99}         // Number
  }]
}
```

#### Option 3: Client-Side Parsing

**Trade-off**: No schema change, parsing overhead on client

```javascript
// JavaScript client code
const price = parseFloat(product.price);  // "19.99" → 19.99
const sku = parseInt(product.sku);        // "123456789012345678" → number
```

**Warning**: Large integers may lose precision!

---

## Implementation Details

### Serialization Functions

**Location**: `src/core/query/jsonld.pl`

#### `value_jsonld/2` (Documents)

```prolog
value_jsonld(D^^T, json{'@type': T, '@value': V}) :-
    % Rule: Only xsd:float and xsd:double as JSON numbers
    (   (   T = 'http://www.w3.org/2001/XMLSchema#float'
        ;   T = 'http://www.w3.org/2001/XMLSchema#double')
    ->  % Float types: serialize as number
        V = D
    ;   % All other numeric types: serialize as string
        typecast(D^^T, 'http://www.w3.org/2001/XMLSchema#string', [], V^^_)
    ).
```

#### `term_jsonld/3` (WOQL Bindings)

```prolog
term_jsonld(D^^T, Prefixes, json{'@type': TC, '@value': V}) :-
    % Same rule applies to WOQL bindings
    (   (   T = 'http://www.w3.org/2001/XMLSchema#float'
        ;   T = 'http://www.w3.org/2001/XMLSchema#double')
    ->  V = D
    ;   typecast(D^^T, 'http://www.w3.org/2001/XMLSchema#string', [], V^^_)
    ),
    compress_dict_uri(T, Prefixes, TC).
```

---

## Testing Requirements

### Test Coverage Matrix

| Test Type | Coverage | Status |
|-----------|----------|--------|
| Document insertion & retrieval | All numeric types | ✅ Required |
| WOQL query bindings | All numeric types | ✅ Required |
| GraphQL results | All numeric types | ✅ Required |
| Large integers (>MAX_SAFE_INTEGER) | String serialization | ✅ Required |
| Float/double types | Number serialization | ✅ Required |
| Typecast workarounds | Decimal→Float conversion | ✅ Required |

### Key Test Scenarios

**Test 1: Integer Types Serialize as Strings**
```prolog
test(integer_as_string) :-
    Schema = _{'@type': "Class", '@id': "Test", value: "xsd:integer"},
    Document = _{'@type': "Test", value: 42},
    insert_document(Context, Document, Id),
    get_document(Descriptor, Id, Retrieved),
    Retrieved.value = "42".  % String, not 42
```

**Test 2: Decimal Types Serialize as Strings**
```prolog
test(decimal_as_string) :-
    Schema = _{'@type': "Class", '@id': "Test", value: "xsd:decimal"},
    Document = _{'@type': "Test", value: "19.99"},
    insert_document(Context, Document, Id),
    get_document(Descriptor, Id, Retrieved),
    Retrieved.value = "19.99".  % String, not 19.99
```

**Test 3: Float Types Serialize as Numbers**
```prolog
test(float_as_number) :-
    Schema = _{'@type': "Class", '@id': "Test", value: "xsd:float"},
    Document = _{'@type': "Test", value: 12.5},
    insert_document(Context, Document, Id),
    get_document(Descriptor, Id, Retrieved),
    Retrieved.value = 12.5.  % Number, not "12.5"
```

**Test 4: Large Integers Protected**
```prolog
test(large_integer_as_string) :-
    Schema = _{'@type': "Class", '@id': "Test", id: "xsd:long"},
    Document = _{'@type': "Test", id: "9007199254740993"},  % > MAX_SAFE_INTEGER
    insert_document(Context, Document, Id),
    get_document(Descriptor, Id, Retrieved),
    Retrieved.id = "9007199254740993".  % String preserves exact value
```

---

## Migration Guide

### For Existing Applications

If you have existing schemas using `xsd:decimal` or `xsd:integer` and expect JSON numbers:

**Before (Old Behavior):**
```json
{
  "price": 19.99,    // JSON number
  "quantity": 5      // JSON number
}
```

**After (New Behavior):**
```json
{
  "price": "19.99",  // JSON string
  "quantity": "5"    // JSON string
}
```

**Migration Options:**

1. **Update client code** to parse strings:
   ```javascript
   const price = parseFloat(doc.price);
   const quantity = parseInt(doc.quantity);
   ```

2. **Change schema types** to `xsd:float`/`xsd:double`:
   ```json
   {
     "@id": "Product",
     "price": "xsd:double",      // Changed from xsd:decimal
     "quantity": "xsd:integer"   // Keep as string or change to xsd:int
   }
   ```

3. **Use typecast in queries** for backward compatibility

---

## FAQ

**Q: Why not serialize all numbers as JSON numbers?**  
A: JSON numbers are IEEE 754 floats, which lose precision for:
- Large integers > 2^53-1
- Decimals requiring >15 digits precision
- Financial calculations requiring exactness

**Q: Why are `xsd:float` and `xsd:double` exceptions?**  
A: These types are explicitly defined as IEEE 754 in XSD spec, matching JSON's number type exactly.

**Q: Will this break my existing application?**  
A: Only if you're relying on `xsd:decimal` or `xsd:integer` being JSON numbers. Use the migration guide.

**Q: How do I get numbers for chart libraries?**  
A: Use `xsd:float` or `xsd:double` types in your schema, or parse strings client-side.

**Q: What about backwards compatibility?**  
A: This is a breaking change for precision types. Version your API or update clients.

---

## References

- XSD 1.1 Part 2: https://www.w3.org/TR/xmlschema11-2/
- JSON Specification (RFC 8259): https://datatracker.ietf.org/doc/html/rfc8259
- JavaScript MAX_SAFE_INTEGER: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER
- IEEE 754: https://en.wikipedia.org/wiki/IEEE_754
