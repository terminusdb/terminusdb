# TerminusDB Error Handling Standards

## Overview

This document specifies the standardized error handling, logging, and response formats for TerminusDB.

## Error Response Structure

All HTTP error responses MUST follow this structure:

### Top-Level Response Structure

```json
{
  "@type": "api:ErrorResponseType",
  "api:status": "api:failure|api:server_error",
  "api:error": { /* Structured error object */ },
  "api:message": "Human-readable error message",
  "api:request_id": "uuid-v4-string",
  "api:trace_id": "trace-id-if-available"  // Optional
}
```

### Response Type Variants

1. **`api:WoqlErrorResponse`** - WOQL query errors
2. **`api:FrameErrorResponse`** - Frame/schema errors
3. **`api:InsertDocumentErrorResponse`** - Document insert errors
4. **`api:DeleteDocumentErrorResponse`** - Document delete errors
5. **`api:ReplaceDocumentErrorResponse`** - Document replace errors
6. **`api:ErrorResponse`** - Generic/unhandled errors

## Error Object Structure

The `api:error` field contains structured, machine-readable error information:

```json
{
  "@type": "api:ErrorTypeName",
  "api:field1": "value1",
  "api:field2": "value2"
}
```

### Common Error Types

#### Validation Errors
- **`api:MissingParameter`** - Required parameter missing
  - Fields: `api:parameter` (name of missing parameter)
  
- **`api:BadParameterType`** - Parameter has wrong type
  - Fields: `api:parameter`, `api:expected_type`, `api:provided_value`

- **`api:BadFieldValue`** - Field has invalid value
  - Fields: `api:field`, `api:value`, `api:document`

#### Type/Cast Errors
- **`api:BadCast`** - Value cannot be cast to type
  - Fields: `api:value`, `api:type`

- **`api:IncompatibleNumericComparison`** - Comparing incompatible numeric types
  - Fields: `api:left`, `api:right`

#### Schema Errors
- **`api:SchemaCheckFailure`** - Schema validation failed
  - Fields: `api:witnesses` (array of validation failures)

- **`vio:WOQLSyntaxError`** - WOQL query syntax error
  - Fields: `vio:path`, `vio:query`

#### Resource Errors
- **`api:UnknownDatabase`** - Database does not exist
  - Fields: `api:database_name`

- **`api:BranchDoesNotExist`** - Branch does not exist
  - Fields: `api:absolute_descriptor`

#### Document Errors
- **`api:SubmittedDocumentIdDoesNotHaveExpectedPrefix`** - Document ID has wrong prefix
  - Fields: `api:submitted_id`, `api:prefix`, `api:document`

- **`api:SubmittedIdDoesNotMatchGeneratedId`** - ID mismatch
  - Fields: `api:submitted_id`, `api:generated_id`

#### Server Errors
- **`api:InternalServerError`** - Unhandled server error
  - No additional fields (details in logs)

## Field Naming Rules

### MUST Use These Names

| Field | Description | Type |
|-------|-------------|------|
| `@type` | Error type identifier | String (IRI) |
| `api:status` | Status indicator | `api:failure` or `api:server_error` |
| `api:error` | Structured error object | Object |
| `api:message` | Human-readable message | String |
| `api:request_id` | Unique request identifier | String (UUID v4) |
| `api:trace_id` | Distributed trace ID | String (32-char hex) |

### Field Value Rules

1. **`api:status`**:
   - `api:failure` - Client error (4xx HTTP status)
   - `api:server_error` - Server error (5xx HTTP status)

2. **`api:message`**:
   - MUST be human-readable
   - MUST NOT contain stack traces
   - MUST be composed from structured error data
   - MAY contain formatting (newlines, etc.)

3. **`api:request_id`**:
   - MUST be UUID v4 format
   - MUST be unique per request
   - MUST be included in error logs

4. **`api:trace_id`**:
   - MUST be 32-character hex string (from W3C traceparent)
   - MUST be included if `traceparent` header present
   - MAY be omitted if no trace context

## HTTP Status Codes

Map error types to HTTP status codes:

| Error Type | HTTP Status |
|------------|-------------|
| Missing/invalid parameters | 400 Bad Request |
| Authentication failure | 401 Unauthorized |
| Authorization failure | 403 Forbidden |
| Resource not found | 404 Not Found |
| Schema validation failure | 400 Bad Request |
| Type casting error | 400 Bad Request |
| Internal server error | 500 Internal Server Error |

## Logging Standards

### Server-Side Logging

All errors MUST be logged server-side with full context:

```json
{
  "severity": "ERROR",
  "request_id": "uuid-v4",
  "operation_id": "operation-or-trace-id",
  "error_type": "api:ErrorTypeName",
  "error": "error_term_from_prolog",
  "stack_trace": "full_stack_trace",
  "timestamp": "iso8601-timestamp",
  "http": {
    "method": "GET",
    "path": "/api/woql/admin/test",
    "remote_ip": "127.0.0.1",
    "user_agent": "curl/7.0"
  }
}
```

### Log Levels

- **ERROR**: All unhandled exceptions and 5xx errors
- **WARNING**: Validation failures, 4xx errors
- **INFO**: Successful requests
- **DEBUG**: Detailed execution traces

### What to Log

**MUST Log**:
- Request ID (UUID v4)
- Operation ID (if provided)
- Trace ID (if available)
- Error type
- Error message
- Stack trace (server-side only)
- HTTP method, path, status
- Timestamp

**MUST NOT Log in HTTP Response**:
- Full stack traces (security risk)
- Internal file paths
- Sensitive data (tokens, passwords)

## Distributed Tracing

### W3C Trace Context Support

TerminusDB supports W3C Trace Context standard for distributed tracing:

**Request Headers**:
- `traceparent: 00-<trace-id>-<span-id>-<flags>`
- `x-request-id: <uuid>` (alternative)
- `x-operation-id: <custom-id>` (TerminusDB-specific)

**Response Headers**:
- `X-Request-ID: <uuid>`
- `X-Operation-ID: <operation-id>`

**Priority Order**:
1. `x-operation-id` (highest priority)
2. `traceparent` (W3C standard)
3. `x-request-id` (common alternative)
4. Generated UUID (if none provided)

### Trace ID Extraction

From `traceparent` header:
```
traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                 This 32-char hex is the trace_id
```

Extracted as `api:trace_id` in response.

## Error vs. Message Semantics

### The `api:error` Field

**Purpose**: Machine-readable, structured error data

**Contains**:
- Error type (`@type`)
- Specific fields relevant to the error
- Raw values without formatting

**Example**:
```json
"api:error": {
  "@type": "api:BadCast",
  "api:value": "1.2",
  "api:type": "xsd:integer"
}
```

### The `api:message` Field

**Purpose**: Human-readable explanation

**Contains**:
- Composed sentence describing the error
- May include context from `api:error` fields
- Formatted for display to end users

**Example**:
```json
"api:message": "The value '1.2' could not be cast as 'xsd:integer'"
```

### Rules for Composition

1. **NO DUPLICATION**: `api:message` should compose from `api:error` data, not duplicate it
2. **CLEAR SEPARATION**: Machine-readable data in `api:error`, human text in `api:message`
3. **CONSISTENCY**: Same error type should produce consistent message format

**Bad Example** (duplication):
```json
{
  "api:error": {
    "@type": "api:BadCast",
    "api:value": "Long explanation text...",  // ❌ Wrong
    "api:type": "1.2"
  },
  "api:message": "The value 'Long explanation text...' could not be cast as '1.2'"  // ❌ Duplicated
}
```

**Good Example** (clean separation):
```json
{
  "api:error": {
    "@type": "api:BadCast",
    "api:value": "1.2",  // ✅ Just the value
    "api:type": "xsd:integer"  // ✅ Just the type
  },
  "api:message": "The value '1.2' could not be cast as 'xsd:integer'"  // ✅ Composed from fields
}
```

## Implementation Guidelines

### For Prolog Error Handlers

1. Throw structured errors:
   ```prolog
   throw(error(error_type(Field1, Field2), Context))
   ```

2. Map to JSON in `api_error.pl`:
   ```prolog
   api_error_jsonld_(API, error(error_type(Val, Type), _), JSON) :-
       format(string(Msg), "Description: ~w as ~w", [Val, Type]),
       JSON = _{'@type': 'api:SomeErrorResponse',
                'api:status': 'api:failure',
                'api:error': _{'@type': 'api:ErrorType',
                               'api:field1': Val,
                               'api:field2': Type},
                'api:message': Msg}.
   ```

3. Add request ID in `routes.pl`:
   - Done automatically by `api_error_http_reply/3`

### For New Error Types

1. Define error term in appropriate module
2. Add handler in `src/core/api/api_error.pl`
3. Follow field naming conventions
4. Compose clear `api:message` from structured data
5. Document in error schema

## Debug Mode

When `TERMINUSDB_EXPOSE_STACK_TRACES=true`:

**Adds `api:debug` field**:
```json
{
  "@type": "api:ErrorResponse",
  "api:error": { /* ... */ },
  "api:message": "Processing error: ...",
  "api:request_id": "uuid",
  "api:debug": "error(...)\nStack trace:\n  at line 1234..."  // ⚠️ Only in debug mode
}
```

**MUST NOT** be enabled in production (security risk).

## Client-Side Error Handling

Clients SHOULD:
1. Check `api:status` for failure type
2. Parse `api:error.@type` for error handling logic
3. Display `api:message` to users
4. Log `api:request_id` for support tickets
5. Use `api:trace_id` for distributed tracing

Clients SHOULD NOT:
- Parse `api:message` for error type detection
- Display raw `api:error` fields to users
- Assume specific message formats

## Schema Location

Error schema definition: `src/core/api/api_error.json` (TODO: Create)

This should define:
- All error types (`@type` values)
- Required fields per error type
- Field value constraints
- Examples

## Testing Requirements

All error responses MUST:
- Include `@type` field
- Include `api:status` field
- Include `api:error` object with `@type`
- Include `api:message` string
- Include `api:request_id` UUID
- Map to correct HTTP status code
- NOT include stack traces (unless debug mode)

## Migration Notes

Existing code may use various error formats. When updating:
1. ✅ Add `api:request_id` (non-breaking)
2. ✅ Add `api:trace_id` if traceparent present (non-breaking)
3. ✅ Keep all existing fields unchanged
4. ❌ Do NOT change existing `@type` values
5. ❌ Do NOT remove existing fields
6. ❌ Do NOT change message formats (may break client parsing)

## References

- W3C Trace Context: https://www.w3.org/TR/trace-context/
- JSON-LD: https://www.w3.org/TR/json-ld11/
- Error implementation: `src/core/api/api_error.pl`
- Trace context: `src/core/util/trace_context.pl`
