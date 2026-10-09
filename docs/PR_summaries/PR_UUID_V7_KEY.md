# UuidV7 key strategy: generated UUIDs, arbitrary IRIs

Enterprise data arrives with identifiers already minted — UUIDs, GUIDs, and
instance IRIs from traditional ontologies like Linked Art. `Random` refused
them all: `idgen_check_base` forces a submitted `@id` to sit under the class
`@base`, so loading `https://linked.art/example/object/47` was impossible
without rewriting the IRI first.

`UuidV7` relaxes exactly that constraint. Supply any `@id` and it is used
as-is, expanded through the prefix context like any other IRI. Omit it and
the class `@base` gets a fresh RFC 9562 UUID v7 suffix — time-ordered, so it
plays nicely with TerminusDB's front-coded storage. Duplicate handling stays
where it always was: the document endpoint.

The strategy works everywhere a normal key does — top-level classes,
`@subdocument` classes, and `TaggedUnion` members.

## Changes

Engine:

- `utils:uuid_v7/1` foreign predicate in `terminusdb-community`, backed by
  the `uuid` crate (`~1.10`, `v7` feature, locked at 1.10.0) and registered
  in `install()` alongside `random_base64`.
- `idgen_uuid_v7/2,3` in `document/json`, re-exported through `core(document)`.
- `uuid_v7(Base)` key descriptor: `json_idgen_`, `json_idgen_schema_`,
  `json_schema_elaborate_key`, `key_descriptor_json`, subdocument key
  allow-list, `is_key/1` and `schema_key_descriptor_` in `schema.pl`.
- Supplied `@id` goes through `prefix_expand/3` and skips
  `idgen_check_base`; no `@id` generates `<base><uuid-v7>` via
  `idgen_uuid_v7`. `migration.pl` `change_key` and the `api_error.pl` key
  message updated.
- WOQL predicate `UuidV7` mirroring `RandomKey`: `definition.pl` entry and
  `operator/1`, `json_woql.pl` AST mapping, `find_resources`/`compile_wf`
  clauses in `woql_compile.pl`, `UuidV7` class in `woql.json`.
- GraphQL: `UuidV7` in `UncleanKeyDefinition`/`KeyDefinition` in `frame.rs`.

Clients:

- JavaScript: `WOQL.idgen_uuid_v7(base, uri)` emitting
  `{"@type":"UuidV7", base, uri}`, plus `WOQLQuery.prototype.idgen_uuid_v7`.
- Python: `WOQLQuery.idgen_uuid_v7` and a `UuidV7Key` schema class.
  `UuidV7Key` bypasses `_check_and_fix_custom_id` — that helper prepends
  `Class/` and URL-quotes, which would mangle an arbitrary IRI.

## Verification

```bash
# Engine
make test SUITE='[json,document_id_generation]'   # 130 pass

# Engine integration (test server on :6363)
npx mocha tests/test/woql-idgen.js tests/test/schema-check-coverage.js  # 66 pass

# JS client
npm run woql-test                                    # 94 pass
npx jest integration_tests/woql_uuid_v7_idgen.test.ts # 5 pass

# Python client
python -m pytest terminusdb_client/tests/test_woql_idgen_uuid_v7.py \
  terminusdb_client/tests/test_schema_overall.py \
  terminusdb_client/tests/test_woql_core.py          # 151 pass
```

Covered end to end: UUID v7 format/version/variant bits, arbitrary external
IRIs (`https://linked.art/example/object/47`), off-base relative IRIs,
prefixed name expansion, schema write/read round-trip, subdocuments, and
lexicographic time-ordering of generated IDs.
