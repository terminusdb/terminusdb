# TerminusDB Plugin API Contract

This document defines the philosophy, stability guarantees, and
contractual obligations of the TerminusDB plugin API. It is a
timeless reference for plugin authors and core maintainers alike.

Specific API references, predicate signatures, and usage examples
live in separate documentation. This document is about principles.

## Purpose

TerminusDB is a transactional document graph database. Plugins
extend it — adding HTTP endpoints, reacting to commits, integrating
external search engines, embedding vector stores, or registering
native predicates for performance-critical work.

The plugin API exists to make these extensions straightforward to
build, safe to ship, and stable across releases.

## Design Principles

### Capabilities before conventions

The API provides unopinionated building blocks — capabilities —
that plugins compose into opinionated solutions. A capability is a
generic primitive with no assumptions about what the plugin does
with it. A convention is a pre-configured composition that works
out of the box but can be replaced.

Plugins that want ready-made behavior use the conventions. Plugins
that need something different fall back to the capabilities and
build their own. Neither path is second-class.

### Minimal surface, maximal clarity

Every exported predicate is part of the public contract. We export
only what plugins need, not everything that is internally available.
A small, well-documented surface is easier to maintain, easier to
learn, and easier to keep stable.

If a predicate is not in the public API, it is internal. Internal
predicates may change or disappear without notice. Plugin authors
who reach past the public API accept that their plugin may break.

### Additive evolution

Within a major version, the API only grows. New predicates, new
options, and new hooks may be added at any time. Existing
predicates are not removed, renamed, or given incompatible
signatures.

When a predicate becomes obsolete, it is marked deprecated but
continues to function until the next major version. Deprecation is
documented, not silent.

### Threat model

Plugins execute in-process with full access to the server's
memory, database files, network, and filesystem. There is no
isolation boundary between a plugin and the server.

- **Malicious plugin**: can read or corrupt any data the server
  can access, exfiltrate credentials, spawn processes, and crash
  the server at will.
- **Buggy plugin**: can leak memory, hold locks that block the
  commit pipeline, trigger stack overflows, or leave external
  resources in an inconsistent state.
- **Compromised supply chain**: a trusted plugin updated with
  malicious code inherits all capabilities of the original.

Operators are responsible for vetting plugins before deployment.
The metadata requirements in this contract exist to make
attribution and auditing possible, not to prevent abuse. A
sandboxed execution model is a future possibility but is not part
of the current contract.

Ensure your plugin software supply chain requirements and system threat models are properly considered prior to extending TerminusDB with plugins.

### Fail loud, never silent

Plugins are guests in the server process. When something goes
wrong — a missing configuration, a failed HTTP request to an
external service, a type mismatch — the plugin must surface the
error, not swallow it. Silent failures make debugging impossible
and erode trust in the system.

Errors should be logged with enough context to diagnose the
problem. Where appropriate, errors should propagate to the caller
as a structured HTTP error response. Use the error reporting
facilities provided by the maintained API rather than building
custom error formats, for consistency across plugins.

## Stability Tiers

The plugin API is organized into tiers with distinct stability
guarantees.

### Tier 1 — Maintained API

The recommended interface for plugin development. Predicates in
this tier carry a stability promise: they must not change in
incompatible ways within a major version — an incompatible
change is a bug. Variants with additional arguments may be added;
existing arguments will not be removed or reordered.

Tier 1 covers:

- **HTTP infrastructure** — route registration, CORS handling,
  authentication wrappers
- **Path and descriptor utilities** — converting between
  human-readable database paths and internal descriptors
- **Query value encoding** — percent-encoding for HTTP parameters
- **Change detection** — enumerating document insertions, updates,
  and deletions between commits
- **Schema metadata access** — discovering types that carry plugin
  extensibility metadata
- **NDJSON streaming** — chunked transfer encoding helpers for
  streaming large document sets over HTTP

### Tier 2 — Raw API

A bare-bones interface for plugins with specific latency or
protocol requirements that the maintained API does not serve well.
Predicates in this tier register directly with the webserver
without CORS handling, authentication, or error formatting.

Tier 2 carries no stability guarantee. The surface may change
between minor releases. Plugins using this tier are responsible
for all cross-cutting concerns and must be prepared to update
their code when the API changes.

### Hooks

Multifile predicates that the server calls at well-defined
lifecycle points. Hooks are part of the maintained API and carry
the same stability promise as Tier 1.

The following hooks are available:

- **Pre-commit** — fires before a commit is finalized; plugins can
  validate or reject. Rejection is signaled by throwing an
  exception; the commit is aborted and the exception propagates to
  the caller as a structured error.
- **Post-commit** — fires after a commit is written; plugins can
  trigger asynchronous work such as search index pushes
- **Post-delete-database** — fires after a database is deleted;
  plugins can clean up external resources

New hooks may be added within a major version. Existing hook
signatures will not change.

### Native predicate registration

Plugins may ship native code (Rust, C, Go) as shared objects in
the plugins directory. The loader discovers and loads them
automatically at startup. Native predicates register through the
SWI-Prolog foreign language interface, either directly via
`swipl-rs` (Rust) or through the C bridge API (C/Go).

The C bridge API (`tdb_plugin_api`) is a Tier 2 surface. Its
function signatures and type definitions may evolve. Rust plugins
using `swipl-rs` directly depend on the `swipl-rs` version
contract, not on a TerminusDB-specific API.

Native predicates may only add new predicates and may never change
internal behavior. The intent is to provide functionality to
plugins for enabling algorithms and functionality to be used.

## Contract Obligations

### For the core team

1. **Do not break maintained API consumers.** A plugin written
   against Tier 1 of a given major version must continue to work
   without source changes through all minor and patch releases of
   that major version.

2. **Document every exported predicate.** If a predicate is
   exported but undocumented, it is not part of the contract.
   Documentation includes purpose, arguments, determinism, errors
   thrown, and stability tier.

3. **Deprecate before removing.** A predicate marked deprecated in
   minor version N is removed in major version N+1 at the earliest.
   The deprecation notice must point to the replacement, if one
   exists.

4. **Keep internal modules internal.** The file and folder
   structure under `src/` is not part of the public API. Plugin
   authors who import directly from internal modules accept the
   risk of breakage. The core team may reorganize internals freely
   within minor versions.

5. **Version the API.** A `plugin_api_version/1` predicate is
   available for plugins to check compatibility at load time. The
   version is incremented when the API changes incompatibly.

### For plugin authors

1. **Use the maintained API.** Import from the public API modules,
   not from internal server or core modules. If the maintained API
   lacks something you need, open an issue rather than reaching
   past it.

2. **Gate on configuration.** A plugin should be inert when its
   configuration is absent. If no endpoint is configured, no hooks
   fire, no routes return functional results, and the server
   behaves as if the plugin were not loaded. The primary source of
configuration should be a namespaced section in schema context
metadata for a branch.

3. **Handle errors explicitly.** Catch exceptions in hooks and
   background workers. Log them with context. Never let an
   unhandled exception propagate from a plugin into the server's
   request handling or commit pipeline.

4. **Do not block the commit pipeline.** Post-commit hooks that
   perform slow work (network calls, indexing) must do so
   asynchronously — spawn a detached thread or enqueue work.
   The commit must not wait for the plugin to finish. Test it.

5. **Clean up on deletion.** If a plugin creates external resources
   (search domains, index tables, cache entries), it must register
   a post-delete-database hook to clean them up when databases
   are removed.

6. **Register routes at load time.** HTTP routes are registered
   during plugin loading, before the server accepts connections.
   Do not register routes lazily at request time.

7. **State your stability tier.** Document whether your plugin
   targets Tier 1 or Tier 2. If Tier 2, note that the plugin may
   break between server versions.

## Naming and Metadata

Every plugin must declare metadata that identifies it and its
provenance. This metadata is read at load time and made available
to a server inspector for listing loaded plugins, diagnosing
configuration issues, and attributing behavior to the correct
source.

### Required metadata

| Field | Description |
|---|---|
| `plugin_name` | Unique, stable identifier for the plugin. Lowercase, hyphen-separated, no spaces. Used in log messages, route prefixes, and inspector output. |
| `author` | Person or organization responsible for the plugin. |
| `copyright` | Copyright holder and year, e.g. `Acme Corp 2024`. |
| `license` | SPDX identifier or full license name, e.g. `Apache-2.0`, `MIT`, `Proprietary`. |
| `description` | One-sentence summary of what the plugin does. |

A plugin that omits any required field must be rejected at load
time with a clear error message naming the missing field.

### Optional metadata

| Field | Description |
|---|---|
| `version` | Plugin version string, following semantic versioning. |
| `homepage` | URL for documentation or source repository. |
| `min_api_version` | Minimum plugin API version required by this plugin. The loader may refuse to load a plugin whose declared minimum exceeds the server's current API version. |
| `stability_tier` | Which API tier the plugin targets (`1` or `2`). Defaults to `1`. |

### Naming conventions

- The `plugin_name` must not collide with another loaded plugin.
  The loader rejects duplicate names at load time.
- HTTP routes registered by a plugin should be prefixed with the
  plugin name to avoid collisions with core routes and other
  plugins, e.g. `/api/v1/ext/<plugin_name>/...`.
- Log messages emitted by a plugin should be prefixed with the
  plugin name for traceability.
- Native predicates registered by a plugin should be placed in a
  module named after the plugin, not in the shared `$rustnative`
  module, to avoid collisions between plugins.

### Server inspector

The server exposes the declared metadata of all loaded plugins
through an introspection endpoint. This allows operators and
administrators to verify which plugins are active, who wrote them,
under what license, and what they claim to do. The inspector does
not expose internal plugin state or configuration values — only
the declared metadata fields.

## Extensibility Model

TerminusDB plugins extend a transactional document graph database.
The extension surface reflects this.

### Schema-driven extensibility

Plugins read plugin-specific configuration from the schema itself.
A schema class may carry metadata that a plugin interprets —
embedding parameters, indexing rules, webhook URLs, or any other
plugin-defined annotation. This means the schema is the
configuration surface; no separate plugin configuration files are
needed for per-type behavior.

The maintained API provides a predicate for discovering schema
types that carry metadata. Plugins define which metadata keys they
recognize and how to interpret them.

### Reactive extensibility

Plugins react to changes in the database. The maintained API
provides a predicate for enumerating document changes between
commits — which documents were inserted, changed, or deleted. This
is the foundation for search index synchronization, audit logging,
webhook dispatch, and any other reactive behavior.

### HTTP extensibility

Plugins register HTTP routes that become part of the server's API
surface. Routes are registered with CORS handling and
authentication by default. Plugins that need raw access to the
HTTP protocol can bypass these wrappers using the raw API tier.

### Native extensibility

Plugins that need performance beyond what Prolog provides can ship
native code. Rust plugins compile to shared objects and register
predicates through `swipl-rs`. C and Go plugins link against the
bridge crate and register predicates through a C API. The loader
discovers both Prolog and native plugins automatically.

## What Is Not Part of the Contract

- The internal file and folder structure of `src/`
- The implementation of any predicate — only its signature and
  documented behavior are contractual
- The module names of internal modules (a predicate may move from
  one internal module to another without breaking the public API,
  as long as the public re-export path remains stable)
- The build system internals (Makefile structure, Cargo
  configuration) — these are documented separately and may change
  to accommodate build tooling evolution
- The wire format of internal protocols between TerminusDB and its
  shipped plugins (e.g. the vectorlink push protocol) — these are
  internal to each plugin and versioned independently

## Versioning

The plugin API follows semantic versioning, decoupled from the
server application version:

- **Major**: Breaking changes to the maintained API. Existing
  plugins may need source changes.
- **Minor**: Additive changes — new predicates, new hooks, new
  optional arguments. Existing plugins work without modification.
- **Patch**: Bug fixes and documentation improvements. No new API
  surface.

The raw API (Tier 2) and the C bridge API may change in minor
versions. Plugins depending on these tiers should pin to a
specific server version range.

Until the initial 1.0.0 API version, expect contracts to break as
we work out the plugin architecture in the community. The
stability promises in this document take effect at version 1.0.0;
0.x releases are exempt.
