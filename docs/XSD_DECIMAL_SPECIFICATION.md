# XSD Decimal Precision Specification

**Version:** 1.0  
**Date:** 2025-10-11  
**Status:** Implementation Specification

## Overview

This document specifies how TerminusDB handles `xsd:decimal` values to ensure mathematical precision, XSD 1.1 compliance, and data integrity for applications requiring high-precision arithmetic (financial systems, scientific computing, blockchain, etc.).

## Requirements

### Primary Goals
1. **No Precision Loss**: Decimal values must not lose precision during WOQL query processing
2. **XSD 1.1 Compliance**: Support XSD 1.1 decimal requirements (minimum 18 digits, recommended 20+)
3. **Deterministic Operations**: Arithmetic operations must be deterministic and exact within precision limits
4. **JSON Serialization**: Decimals serialize as strings in JSON to avoid JavaScript floating-point issues

### Use Cases
- **Financial Systems**: ISO 20022 messages (up to 18 digits)
- **Blockchain/Crypto**: Ethereum Wei/Ether conversions (18 decimal places)
- **Scientific Computing**: High-precision measurements
- **Regulatory Compliance**: Auditable calculations with exact arithmetic

## Internal Representation

### Rational Numbers (GMP-backed)

**Implementation:**
- All `xsd:decimal` values are stored internally as **rational numbers** (`Numerator rdiv Denominator`)
- Uses SWI-Prolog's GMP (GNU Multiple Precision) library for arbitrary-precision arithmetic
- Rationals are exact - no rounding errors during computation

**Example:**
```prolog
"0.1" → 1 rdiv 10                    % Exact representation
"58.082342356456934053939" → 58082342356456934053939 rdiv 1000000000000000000000
```

### Why Rationals?

| Approach | Precision | Speed | Memory | Use Case |
|----------|-----------|-------|---------|----------|
| **Float64** | ~15 digits | Fast | 8 bytes | ❌ Insufficient |
| **Decimal128** | 34 digits | Medium | 16 bytes | ✅ Good but limited |
| **Rational (GMP)** | Unlimited | Slower | Variable | ✅✅ Best for exactness |

TerminusDB uses rationals because:
- **Exact arithmetic**: `0.1 + 0.2 = 0.3` (not `0.30000000000000004`)
- **Preserves input precision**: If user inputs 20 digits, all 20 are preserved
- **Composable operations**: Multiple operations don't accumulate errors

## Precision Limits

### Input Precision

**Rule:** Accept decimals with **up to 100 decimal places** for input/storage

**Rationale:**
- Handles any reasonable real-world use case
- Prevents abuse (e.g., million-digit decimals causing DoS)
- 100 digits = 332 bits of precision (GMP handles efficiently)

**Examples:**
```
✅ "123.456" (3 decimals)
✅ "58.082342356456934053939" (21 decimals) 
✅ "0.000000000000000001234567890123456789" (36 decimals)
✅ 100 decimal places
❌ 101+ decimal places (error: exceeds maximum precision)
```

### Output Precision (Materialization)

**Rule:** When converting `xsd:decimal` to `xsd:string` (JSON serialization), preserve **up to 20 significant decimal places**

**Behavior:**
1. **Infer original precision from denominator** (if power of 10)
2. **Cap at 20 decimal places maximum**
3. **Preserve trailing zeros** from original input (within 20-digit limit)
4. **Trim only excess zeros** beyond original precision

**Algorithm:**
```prolog
% If denominator = 10^N where N ≤ 20:
%   Use N decimal places
% Else if N > 20:
%   Use 20 decimal places
% Else:
%   Compute up to 20 decimal places
```

**Examples:**

| Input String | Rational | Output (JSON) | Decimals |
|--------------|----------|---------------|----------|
| `"0.1"` | `1 rdiv 10` | `"0.1"` | 1 |
| `"0.10"` | `1 rdiv 10` | `"0.1"` | 1 * |
| `"123.4567890123456789"` | `1234567890123456789 rdiv 10000000000000000` | `"123.4567890123456789"` | 16 |
| `"1.12345678901234567890"` | `112345678901234567890 rdiv 100000000000000000000` | `"1.1234567890123456789"` | 19 ** |
| `"999999999999999999.999999999999999999"` | ... | `"999999999999999999.99999999999999999"` | 20 (capped) |

\* Trailing zero information lost during rational conversion  
\** 20th zero trimmed because it exceeds cap

### Arithmetic Operations

**Rule:** All arithmetic operations use **exact rational arithmetic** internally

**Supported Operations:**
- Addition: `a + b`
- Subtraction: `a - b`
- Multiplication: `a * b`
- Division: `a / b` (maintains rational, may increase denominator)
- Comparisons: `<`, `>`, `=`, `≤`, `≥` (exact)

**Precision Behavior:**
```prolog
% Example: 0.1 + 0.2
(1 rdiv 10) + (2 rdiv 10) = 3 rdiv 10  % Exact: 0.3

% Example: 1/3 + 1/3
(1 rdiv 3) + (1 rdiv 3) = 2 rdiv 3    % Exact: 0.666... (repeating)
% When materialized → "0.66666666666666666667" (20 digits, rounded)
```

**Rounding:** Only applied at materialization (string conversion), never during computation

## Equality and Comparison

### Exact Equality

**Rule:** Two decimals are equal if and only if their rational representations are mathematically equal

```prolog
"0.1" == "0.10"        % TRUE  (both = 1 rdiv 10)
"0.333333" == "1/3"    % FALSE (0.333333 ≠ exact 1/3)
```

### Comparison Operators

All comparisons use **exact rational arithmetic**:

```prolog
"0.1" < "0.2"                      % TRUE
"999999999999999999.99" < "1e18"  % FALSE (equal)
```

### Tolerance-based Comparison

For use cases requiring approximate equality (e.g., floating-point compatibility):

```prolog
decimal_approximately_equal(A, B, Tolerance) :-
    Diff is abs(A - B),
    Diff < Tolerance.

**Rationale:**
- JavaScript `Number` is float64 (only 15 digits precision)
- Prevents silent precision loss in client applications
- Standard practice for high-precision systems
**Example:**
```json
{
  "@type": "xsd:decimal",
  "@value": "58.082342356456934053939"
}
```

**NOT:**
```json
{
  "@type": "xsd:decimal",
  "@value": 58.082342356456934  ❌ Loses precision!
}
```

## Edge Cases

### Repeating Decimals

**Behavior:** Rationals that don't terminate (e.g., 1/3) are truncated at 20 decimal places

```prolog
Input:  1 rdiv 3
Output: "0.33333333333333333333" (20 threes)
```

### Very Large Numbers

**Rule:** Support up to **18 integer digits + 20 decimal digits**

```prolog
✅ "999999999999999999.99999999999999999999" (18 + 20)
✅ "1e18" (scientific notation → decimal)
❌ "1e100" (error: integer part too large)
```

### Trailing Zeros

**Behavior:** Preserve semantically significant trailing zeros up to 20 decimals

```prolog
"100.00" → "100"      % Trailing zeros trimmed after decimal point
"0.10"   → "0.1"      % Single trailing zero trimmed
"1.00000000000000000000" → "1"  % All zeros trimmed
```

### Zero Values

```prolog
"0"      → 0       → "0"
"0.0"    → 0       → "0"
"-0.0"   → 0       → "0"  (negative zero normalized)
```

### Special Values

**Not Supported:**
- `NaN` (Not a Number)
- `+Infinity` / `-Infinity`
- Scientific notation in output (converted to decimal)

Use `xsd:double` for these cases.

## XSD 1.1 Compliance

### Standards Conformance

| XSD 1.1 Requirement | TerminusDB Implementation | Status |
|---------------------|---------------------------|--------|
| Arbitrary precision | ✅ Rationals (GMP) | Compliant |
| Min 18 digits | ✅ 20 digits output | Exceeds |
| Exact equality | ✅ Rational comparison | Compliant |
| Canonical form | ✅ No trailing zeros (output) | Compliant |
| Value space | ✅ All rationals | Compliant |

### Canonical Lexical Representation

Per XSD 1.1 spec, canonical form has:
- No leading zeros (except "0")
- No trailing zeros in fractional part
- Explicit decimal point only if fractional part exists

TerminusDB follows these rules for output.

## Implementation Notes

### Type Casting

```prolog
% String to Decimal (parsing)
typecast("58.082342356456934053939"^^xsd:string, xsd:decimal, Result)
→ Result = 58082342356456934053939 rdiv 1000000000000000000000

% Decimal to String (serialization)
typecast(Rational^^xsd:decimal, xsd:string, Result)
→ Result = "58.082342356456934053939"^^xsd:string
```

### Performance Considerations

**Rational Arithmetic Overhead:**
- ~2-10x slower than float operations
- Negligible for queries processing <1M values
- Memory: ~32 bytes per value (vs 8 for float)

**Optimization Strategies:**
- Cache string representations after first conversion
- Use integers when denominator = 1
- Consider float for non-critical aggregations with `xsd:double`

## Migration Path

### Existing Databases

**Scenario:** Upgrading from float-based decimals

**Strategy:**
1. Scan all `xsd:decimal` values
2. Re-parse from original strings if available
3. Flag values with precision loss warnings
4. Allow opt-in migration with data validation

### Client Libraries

**JavaScript/TypeScript:**
```javascript
// Before (precision loss)
const value = 58.082342356456934053939;  // ❌ Truncated

// After (preserved)
const value = "58.082342356456934053939";  // ✅ String
```

**Python:**
```python
from decimal import Decimal

# Use Python's Decimal type
value = Decimal("58.082342356456934053939")  # ✅
```

## Testing Requirements

All implementations must pass:

1. **Precision preservation tests** (15 test cases)
2. **Arithmetic accuracy tests** (0.1 + 0.2 = 0.3)
3. **Round-trip tests** (string → rational → string)
4. **Equality tests** (mathematical equivalence)
5. **Edge case tests** (very large, very small, special values)
6. **XSD compliance tests** (canonical forms)

See `src/core/query/decimal_precision_test.pl` for reference implementation.

## References

- **XSD 1.1 Part 2:** [W3C Recommendation](https://www.w3.org/TR/xmlschema11-2/#decimal)
- **ISO 20022:** Financial services message standard
- **IEEE 754-2008:** Floating-point arithmetic standard (for comparison)
- **GMP Library:** [GNU Multiple Precision](https://gmplib.org/)
- **SWI-Prolog Rationals:** [Documentation](https://www.swi-prolog.org/pldoc/man?section=rational)

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-10-11 | Initial specification |

---

**Approved by:** TerminusDB Engineering Team  
**Implementation Status:** In Progress (v11.2.0)
