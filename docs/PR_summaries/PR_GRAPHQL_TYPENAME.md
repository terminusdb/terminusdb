# Fix GraphQL `__typename` and fragment resolution on dynamically generated types

## Summary

Fixes terminusdb/terminusdb#2550. Selecting `__typename` on any document type — for example `query { Tree { name __typename } }` — crashed in the GraphQL endpoint with `panic("GraphQLValue::concrete_type_name() must be implemented by unions, interfaces and objects")` and turned the whole request into an HTTP 500.

The per-database GraphQL schema is built dynamically through hand-rolled `juniper::GraphQLValue` implementations rather than juniper's derive macros. Juniper calls `concrete_type_name()` whenever a selection set asks for `__typename` or evaluates a fragment type condition, and none of the dynamic object types implemented it. The trait's default implementation is the panic the issue reporter hit. It is "halfway implemented" only in the sense that the static system schema, which does use the macros, already resolved `__typename` correctly.

## Changes

`concrete_type_name` and `resolve_into_type` are now implemented for the three object types in the dynamic schema:

- `TerminusType` (every generated document type). `concrete_type_name` resolves the instance's actual `rdf:type` through the layer and maps it back to its GraphQL name, so a `Dog` returned by an `Animal` query reports `__typename: "Dog"` — the concrete type, as the spec requires. A missing instance layer or an unmapped `rdf:type` is an invariant violation, not a fallback case: it panics, matching the existing `_type` resolver, which uses the same lookup. Reporting the declared class for a document of another type would hand GraphQL clients a wrong cache key.
- `TerminusTypeCollection` (the `Query` root) and `TerminusMutationRoot` (the `TerminusMutation` root), which return their registered schema names.

`resolve_into_type` matters because implementing `concrete_type_name` exposes the next juniper code path: fragment evaluation calls it with the concrete type name, and the default implementation panics whenever that differs from the declared type. A named fragment `... on Parent` spread over subsumed `Child` instances now resolves instead of crashing. It is implemented on `TerminusType` and `TerminusMutationRoot`; juniper's own `RootNode` handles the query root, so `TerminusTypeCollection` only needs `concrete_type_name`.

The `rdf:type` lookup (`predicate_id` → `single_triple_sp` → `id_object_node`) is shared between `concrete_type_name` and the `_type` field resolver via a `type_iri` helper on `TerminusType`, keeping their semantics identical.

## Testing

New cases in `tests/test/graphql.js`, driven by the issue's schema shape (a concrete `Tree`, an abstract `Animal`, and `Dog`/`Cat`-style subclasses — `Dog` here since `Cat` was already taken in the shared test schema):

- `__typename` on a concrete class query returns the class name.
- `__typename` on an abstract class query returns the concrete subclass (`Animal` → `Dog`).
- `__typename` under plain subsumption returns `Parent` for `Parent` documents and `Child` for `Child` documents.
- `__typename` on the query root returns `Query`.
- An inline fragment on the declared type and a named fragment spread over a subsumed type both resolve their fields.
- Inline fragments on the two roots, `{ ... on Query { __typename } }` and `mutation { ... on TerminusMutation { __typename } }`, exercise the root `concrete_type_name` implementations — the only paths that reach them.

The fragment and root tests use `fetchPolicy: 'no-cache'` because Apollo resolves root `__typename` and fragment type conditions client-side in `InMemoryCache`, which would skip the server path under test. All 58 tests in the file pass, and each new case panics on the unpatched build.
