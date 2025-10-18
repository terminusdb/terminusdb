# Numeric Canonicalization Quick Reference

**Quick guide for developers working with numeric types in TerminusDB**

---

## Canonical Forms Cheat Sheet

| Type | Input Examples | Canonical | Output |
|------|---------------|-----------|--------|
| **xsd:decimal (whole)** | `2`, `2.0`, `2.00` | `2` | `2` |
| **xsd:decimal (frac)** | `.1`, `0.1`, `0.10` | `0.1` | `0.1` |
| **xsd:integer** | `2`, `2.0` | `2` | `2` |
| **xsd:float** | `2`, `2.0` | `2.0` | `2.0` |
| **xsd:double** | `2`, `2.0` | `2.0` | `2.0` |

---

## Key Rules

### Rule 1: Decimal Whole Numbers
❌ **Wrong**: `"2.0"`  
✅ **Correct**: `"2"`  
💡 **Auto-fixed**: System canonicalizes `"2.0"` → `"2"`

### Rule 2: Decimal Fractional Numbers
❌ **Wrong**: `".1"`  
✅ **Correct**: `"0.1"`  
💡 **Auto-fixed**: System canonicalizes `".1"` → `"0.1"`

### Rule 3: Float/Double Whole Numbers
✅ **Correct**: `2.0` (decimal point preserved)  
💡 **Note**: Float/double maintain `.0` for whole numbers

### Rule 4: Default Type
💡 **No type specified** → defaults to `xsd:decimal` for precision

---

## Common Patterns

### Pattern 1: Document Insertion
```javascript
// Implicit decimal (recommended for financial data)
await client.insertDocument({
  "@type": "Product",
  "price": 19.99  // → xsd:decimal by default
})

// Explicit type (use for float/double)
await client.insertDocument({
  "@type": "Sensor",
  "temperature": {"@type": "xsd:double", "@value": 98.6}
})
```

### Pattern 2: WOQL Arithmetic
```javascript
// All equivalent (all canonicalize to 2 rdiv 1)
WOQL.plus(2, 3)
WOQL.plus("2.0", "3.0")
WOQL.plus(
  {"@type": "xsd:decimal", "@value": "2.00"},
  {"@type": "xsd:decimal", "@value": "3.000"}
)
```

### Pattern 3: Type Casting
```prolog
% Decimal to decimal (canonicalizes)
typecast("2.0"^^xsd:string, xsd:decimal, [], R^^_).
% R = 2^^xsd:decimal (rational: 2 rdiv 1)

% String to integer (validates whole number)
typecast("2.0"^^xsd:string, xsd:integer, [], R^^_).
% R = 2^^xsd:integer

% Decimal to float (requires explicit cast)
typecast(2^^xsd:decimal, xsd:float, [], R^^_).
% R = 2.0^^xsd:float
```

### Pattern 4: Mixed-Type Operations
```prolog
% ERROR: Cannot mix types
Result is (1.5^^xsd:decimal) + (2.5^^xsd:double).

% CORRECT: Explicit typecast
typecast(1.5^^xsd:decimal, xsd:double, [], A^^xsd:double),
Result is A + (2.5^^xsd:double).
% Result = 4.0^^xsd:double
```

---

## Testing Equivalence

### Verify Canonical Equivalence
```prolog
% All these should be equivalent
typecast("2"^^xsd:string, xsd:decimal, [], R1^^_),
typecast("2.0"^^xsd:string, xsd:decimal, [], R2^^_),
typecast("2.00"^^xsd:string, xsd:decimal, [], R3^^_),

assertion(R1 = R2),  % Structural equality
assertion(R2 = R3),
assertion(R1 =:= 2). % Mathematical equality
```

---

## Type Hierarchy

```
xsd:decimal (rationals)
  ├── xsd:integer
  │   ├── xsd:long
  │   ├── xsd:int
  │   ├── xsd:short
  │   └── xsd:byte
  
IEEE 754 (separate from decimal!)
  ├── xsd:float (32-bit)
  └── xsd:double (64-bit)
```

**Important**: `xsd:float` and `xsd:double` are **NOT** subtypes of `xsd:decimal`

---

## Common Mistakes

### Mistake 1: Assuming Floats are Decimals
❌ **Wrong**:
```javascript
const value = 19.99;  // Assumed to be float
```
✅ **Correct**:
```javascript
// Defaults to xsd:decimal (exact precision)
const value = 19.99;

// Or explicit for float
const value = {"@type": "xsd:double", "@value": 19.99};
```

### Mistake 2: Using `.1` Instead of `0.1`
❌ **Wrong** (non-canonical):
```json
{"taxRate": ".1"}
```
✅ **Correct** (canonical):
```json
{"taxRate": "0.1"}
```
💡 **Note**: System auto-canonicalizes, but best to use canonical form

### Mistake 3: Mixing Types Without Casting
❌ **Wrong**:
```prolog
Result is (1.5^^xsd:decimal) + (2.5^^xsd:double).
```
✅ **Correct**:
```prolog
typecast(2.5^^xsd:double, xsd:decimal, [], B^^xsd:decimal),
Result is (1.5^^xsd:decimal) + B.
```

---

## When to Use Each Type

### Use `xsd:decimal`
✅ Financial calculations (money, prices, rates)  
✅ Exact arithmetic requirements  
✅ Regulatory compliance (ISO 20022, blockchain)  
✅ Default for precision preservation

### Use `xsd:integer`
✅ Whole numbers only  
✅ IDs, counts, quantities  
✅ Better performance than general decimal

### Use `xsd:float` / `xsd:double`
✅ Approximate measurements (sensor data)  
✅ Scientific computing (where approximation is acceptable)  
✅ Graphics/visualization  
✅ Performance-critical operations

---

## Debugging Tips

### Check Internal Representation
```prolog
% View rational representation
?- typecast("0.1"^^xsd:string, xsd:decimal, [], R^^_), write(R).
% Output: 1 rdiv 10

% Check if canonicalized correctly
?- typecast(".1"^^xsd:string, xsd:decimal, [], R1^^_),
   typecast("0.1"^^xsd:string, xsd:decimal, [], R2^^_),
   R1 = R2.
% Should succeed
```

### Verify Equivalence
```prolog
% Test mathematical equivalence
?- R1 =:= R2.

% Test structural equivalence
?- R1 = R2.
```

### Check Storage Format
```prolog
% Whole numbers stored as integers
ground_object_storage(2^^xsd:decimal, Storage).
% Storage = value(2, 'xsd:decimal')

% Fractional stored as strings
ground_object_storage((1 rdiv 10)^^xsd:decimal, Storage).
% Storage = value("0.1", 'xsd:decimal')
```

---

## Performance Notes

### Canonicalization Overhead
- **Leading zero normalization**: O(1)
- **Rational simplification**: O(log n)
- **Total impact**: <1% of processing time

### Optimization Tips
1. Use canonical forms in input when possible
2. Cache frequently used rational values
3. Use `xsd:integer` for whole numbers when applicable
4. Consider `xsd:double` for performance-critical approximate calculations

---

## Test Files

### Unit Tests
- `src/core/triple/numeric_canonicalization_test.pl` - Canonicalization tests
- `src/core/query/decimal_precision_test.pl` - Precision tests

### Integration Tests
- `tests/test/decimal-precision.js` - JavaScript integration tests
- Document API tests
- WOQL arithmetic tests
- GraphQL tests

---

## Additional Resources

- **Architecture**: `docs/ARBITRARY_PRECISION_DECIMALS_ARCHITECTURE.md`
- **Serialization**: `docs/JSON_SERIALIZATION_RULES.md`
- **Implementation**: `docs/NUMERIC_CANONICALIZATION_IMPLEMENTATION.md`
- **XSD Spec**: https://www.w3.org/TR/xmlschema11-2/#decimal

---

**Remember**: The system automatically canonicalizes inputs, so non-canonical forms are accepted but internally converted to canonical representation.
