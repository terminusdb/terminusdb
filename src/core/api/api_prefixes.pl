/**
 * Prefix Management API
 *
 * This module provides atomic CRUD operations for individual prefixes.
 * Prefixes are namespace mappings stored in the context document
 * (`terminusdb://context`) within the schema graph.
 *
 * DESIGN DECISIONS:
 *
 * 1. Permission Model:
 *    All prefix operations use `instance_read_access` and `instance_write_access`
 *    rather than `schema_read_access` / `schema_write_access`. This is consistent
 *    with the existing `update_prefixes/5` predicate and treats prefix/context
 *    management as instance-level metadata rather than schema definition changes.
 *    This allows users with instance write permissions to manage prefixes without
 *    requiring full schema modification rights.
 *
 * 2. Atomicity:
 *    Individual prefix operations (add, update, upsert, delete) use direct
 *    triple-level operations (`insert/5`, `delete/5`) within a single transaction.
 *    All existence checks happen INSIDE the transaction to prevent TOCTOU
 *    (time-of-check-time-of-use) race conditions.
 *
 * 3. Triple Storage:
 *    Prefixes are stored as `sys:Prefix` subdocuments linked from
 *    `terminusdb://context` via `sys:prefix_pair`. Each prefix has:
 *    - `sys:prefix` (xsd:string) - the prefix name
 *    - `sys:url` (xsd:string) - the IRI the prefix expands to
 *    Prefix IDs are deterministically generated using `idgen_hash/3`.
 *
 * 4. Reserved Prefixes:
 *    Prefixes starting with `@` (e.g., `@base`, `@schema`) are reserved
 *    and cannot be modified through this API.
 *
 * 5. IRI Validation:
 *    All URIs must have a valid scheme (e.g., http://, https://).
 *    Invalid IRIs are rejected with an `invalid_iri` error.
 */
:- module(api_prefixes, [
              get_prefixes/4,
              update_prefixes/5,
              get_prefix/5,
              add_prefix/7,
              update_prefix/7,
              upsert_prefix/7,
              delete_prefix/6
          ]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(document)).
:- use_module(library(uri), [uri_components/2]).
:- use_module(library(apply), [exclude/3]).
:- use_module(core(util/xsd_parser), [ncname//0]).

:- use_module(library(plunit)).

% Validate prefix name follows ASCII-only XML NCName rules (from xsd_parser)
% Pattern: [A-Za-z_][A-Za-z0-9_.-]* (can start with underscore, cannot contain colon)
% Rejects any prefix starting with '@' - reserved for JSON-LD keywords (@context, @id, @type, @base, etc.)
% NOTE: Currently ASCII-only. URL decoding in routes.pl allows future Unicode support.
valid_prefix_name(Prefix_Name) :-
    (atom(Prefix_Name) -> atom_codes(Prefix_Name, Codes) ; string_codes(Prefix_Name, Codes)),
    phrase(ncname, Codes, []),
    !.  % Commit once validated - no backtracking needed

% Validate that a string is a valid IRI (must have a scheme like http://)
valid_iri(IRI) :-
    (   atom(IRI) -> atom_string(IRI, IRI_String) ; IRI_String = IRI ),
    uri_components(IRI_String, uri_components(Scheme, _Authority, _Path, _Search, _Fragment)),
    nonvar(Scheme),
    Scheme \= ''.

get_prefixes(Path, System_DB, Auth, JSON) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_read_access', Auth),

    database_prefixes(Descriptor,JSON).

update_prefixes(Path, System_DB, Auth, Commit_Info, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_write_access', Auth),

    create_context(Descriptor, Commit_Info, Context),
    with_transaction(
        Context,
        insert_context_document(Context, Document),
        _).

get_prefix(Path, System_DB, Auth, Prefix_Name, Prefix_URI) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_read_access', Auth),

    database_prefixes(Descriptor, Prefixes),
    do_or_die(
        get_dict(Prefix_Name, Prefixes, Prefix_URI),
        error(prefix_not_found(Prefix_Name), _)).

% Generate deterministic prefix pair ID using hash of name and URI
generate_prefix_id(Prefix_Name, Prefix_URI, Prefix_Id) :-
    (   atom(Prefix_Name) -> atom_string(Name_Str, Prefix_Name) ; Name_Str = Prefix_Name ),
    (   atom(Prefix_URI) -> atom_string(URI_Str, Prefix_URI) ; URI_Str = Prefix_URI ),
    idgen_hash('terminusdb://Prefix_Pair/',
               [json{'@value': Name_Str}, json{'@value': URI_Str}],
               Prefix_Id).

% Find existing prefix by name in schema (returns Prefix_Id and URI if found)
find_prefix_by_name(Schema, Prefix_Name, Prefix_Id, Prefix_URI) :-
    % Normalize prefix name to string for comparison
    (   atom(Prefix_Name) -> atom_string(Prefix_Name, Name_Str) ; Name_Str = Prefix_Name ),
    global_prefix_expand(sys:prefix_pair, Prefix_Pair_Pred),
    global_prefix_expand(sys:prefix, Prefix_Pred),
    global_prefix_expand(sys:url, URL_Pred),
    global_prefix_expand(xsd:string, XSD_String),
    xrdf(Schema, 'terminusdb://context', Prefix_Pair_Pred, Prefix_Id),
    xrdf(Schema, Prefix_Id, Prefix_Pred, Name_Str^^XSD_String),
    xrdf(Schema, Prefix_Id, URL_Pred, URI_Str^^XSD_String),
    Prefix_URI = URI_Str.

% Insert prefix triples atomically
insert_prefix_triples(Schema, Prefix_Name, Prefix_URI) :-
    (   atom(Prefix_Name) -> atom_string(Name_Str, Prefix_Name) ; Name_Str = Prefix_Name ),
    (   atom(Prefix_URI) -> atom_string(URI_Str, Prefix_URI) ; URI_Str = Prefix_URI ),
    generate_prefix_id(Name_Str, URI_Str, Prefix_Id),
    global_prefix_expand(sys:prefix_pair, Prefix_Pair_Pred),
    global_prefix_expand(sys:prefix, Prefix_Pred),
    global_prefix_expand(sys:url, URL_Pred),
    global_prefix_expand(sys:'Prefix', Prefix_Type),
    global_prefix_expand(rdf:type, Rdf_Type),
    global_prefix_expand(xsd:string, XSD_String),
    % Insert link from context to prefix
    insert(Schema, 'terminusdb://context', Prefix_Pair_Pred, Prefix_Id, _),
    % Insert prefix type
    insert(Schema, Prefix_Id, Rdf_Type, Prefix_Type, _),
    % Insert prefix name
    insert(Schema, Prefix_Id, Prefix_Pred, Name_Str^^XSD_String, _),
    % Insert prefix URL
    insert(Schema, Prefix_Id, URL_Pred, URI_Str^^XSD_String, _).

% Delete prefix triples atomically (Schema is the RWO, SchemaList is [Schema] for xrdf)
delete_prefix_triples(Schema, SchemaList, Prefix_Id) :-
    global_prefix_expand(sys:prefix_pair, Prefix_Pair_Pred),
    global_prefix_expand(sys:prefix, Prefix_Pred),
    global_prefix_expand(sys:url, URL_Pred),
    global_prefix_expand(sys:'Prefix', Prefix_Type),
    global_prefix_expand(rdf:type, Rdf_Type),
    % Get current values for deletion
    xrdf(SchemaList, Prefix_Id, Prefix_Pred, Name_Value),
    xrdf(SchemaList, Prefix_Id, URL_Pred, URL_Value),
    % Delete all prefix triples
    delete(Schema, 'terminusdb://context', Prefix_Pair_Pred, Prefix_Id, _),
    delete(Schema, Prefix_Id, Rdf_Type, Prefix_Type, _),
    delete(Schema, Prefix_Id, Prefix_Pred, Name_Value, _),
    delete(Schema, Prefix_Id, URL_Pred, URL_Value, _).

% Check that context exists in schema (within transaction)
check_context_exists_in_schema(Schema) :-
    global_prefix_expand(rdf:type, Rdf_Type),
    global_prefix_expand(sys:'Context', Context_Type),
    do_or_die(
        xrdf(Schema, 'terminusdb://context', Rdf_Type, Context_Type),
        error(schema_not_initialized, _)).

% Add a new prefix (fails if prefix already exists) - ATOMIC
add_prefix(Path, System_DB, Auth, Commit_Info, Prefix_Name, Prefix_URI, Result_Prefixes) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_write_access', Auth),

    % Validate prefix name is not reserved
    do_or_die(
        valid_prefix_name(Prefix_Name),
        error(reserved_prefix(Prefix_Name), _)),

    % Validate IRI format
    do_or_die(
        valid_iri(Prefix_URI),
        error(invalid_iri(Prefix_URI), _)),

    create_context(Descriptor, Commit_Info, Context),
    with_transaction(
        Context,
        (   % All checks inside transaction for atomicity
            query_default_collection(Context, Transaction),
            database_schema(Transaction, [Schema]),
            check_context_exists_in_schema([Schema]),
            % Check prefix doesn't already exist
            do_or_die(
                \+ find_prefix_by_name([Schema], Prefix_Name, _, _),
                error(prefix_already_exists(Prefix_Name), _)),
            % Insert new prefix triples
            insert_prefix_triples(Schema, Prefix_Name, Prefix_URI)
        ),
        _),
    % Return updated prefixes
    database_prefixes(Descriptor, Result_Prefixes).

% Update an existing prefix (fails if prefix doesn't exist) - ATOMIC
update_prefix(Path, System_DB, Auth, Commit_Info, Prefix_Name, Prefix_URI, Result_Prefixes) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_write_access', Auth),

    % Validate prefix name is not reserved
    do_or_die(
        valid_prefix_name(Prefix_Name),
        error(reserved_prefix(Prefix_Name), _)),

    % Validate IRI format
    do_or_die(
        valid_iri(Prefix_URI),
        error(invalid_iri(Prefix_URI), _)),

    create_context(Descriptor, Commit_Info, Context),
    with_transaction(
        Context,
        (   % All checks inside transaction for atomicity
            query_default_collection(Context, Transaction),
            database_schema(Transaction, [Schema]),
            check_context_exists_in_schema([Schema]),
            % Check prefix exists and get its ID
            do_or_die(
                find_prefix_by_name([Schema], Prefix_Name, Old_Prefix_Id, _),
                error(prefix_not_found(Prefix_Name), _)),
            % Delete old prefix triples
            delete_prefix_triples(Schema, [Schema], Old_Prefix_Id),
            % Insert new prefix triples with updated URI
            insert_prefix_triples(Schema, Prefix_Name, Prefix_URI)
        ),
        _),
    % Return updated prefixes
    database_prefixes(Descriptor, Result_Prefixes).

% Upsert a prefix (create if not exists, update if exists) - ATOMIC
upsert_prefix(Path, System_DB, Auth, Commit_Info, Prefix_Name, Prefix_URI, Result_Prefixes) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_write_access', Auth),

    % Validate prefix name is not reserved
    do_or_die(
        valid_prefix_name(Prefix_Name),
        error(reserved_prefix(Prefix_Name), _)),

    % Validate IRI format
    do_or_die(
        valid_iri(Prefix_URI),
        error(invalid_iri(Prefix_URI), _)),

    create_context(Descriptor, Commit_Info, Context),
    with_transaction(
        Context,
        (   % All checks inside transaction for atomicity
            query_default_collection(Context, Transaction),
            database_schema(Transaction, [Schema]),
            check_context_exists_in_schema([Schema]),
            % Check if prefix exists
            (   find_prefix_by_name([Schema], Prefix_Name, Old_Prefix_Id, _)
            ->  % Delete old prefix triples
                delete_prefix_triples(Schema, [Schema], Old_Prefix_Id)
            ;   true
            ),
            % Insert new prefix triples
            insert_prefix_triples(Schema, Prefix_Name, Prefix_URI)
        ),
        _),
    % Return updated prefixes
    database_prefixes(Descriptor, Result_Prefixes).

% Delete a prefix - ATOMIC
delete_prefix(Path, System_DB, Auth, Commit_Info, Prefix_Name, Result_Prefixes) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_write_access', Auth),

    % Validate prefix name is not reserved (can't delete @base, @schema, etc.)
    do_or_die(
        valid_prefix_name(Prefix_Name),
        error(reserved_prefix(Prefix_Name), _)),

    create_context(Descriptor, Commit_Info, Context),
    with_transaction(
        Context,
        (   % All checks inside transaction for atomicity
            query_default_collection(Context, Transaction),
            database_schema(Transaction, [Schema]),
            check_context_exists_in_schema([Schema]),
            % Check prefix exists and get its ID
            do_or_die(
                find_prefix_by_name([Schema], Prefix_Name, Prefix_Id, _),
                error(prefix_not_found(Prefix_Name), _)),
            % Delete prefix triples
            delete_prefix_triples(Schema, [Schema], Prefix_Id)
        ),
        _),
    % Return updated prefixes
    database_prefixes(Descriptor, Result_Prefixes).

:- begin_tests(api_prefixes).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

test(get_prefixes,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    Path = "admin/testdb",

    super_user_authority(Auth),
    get_prefixes(Path,system_descriptor{},Auth,Prefixes),

    _{'@base':"http://somewhere.for.now/document/",
      '@schema':"http://somewhere.for.now/schema#",
      '@type':'Context'} :< Prefixes.


test(get_prefixes_auth_failure,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(access_not_authorised('terminusdb://system/data/User/Doug',
                                  '@schema':'Action/instance_read_access',
                                  _),
            _)
     ]) :-

    add_user("Doug", some("password"), User_URI),
    Path = "admin/testdb",
    get_prefixes(Path,system_descriptor{},User_URI,Prefixes),

    writeq(Prefixes).

test(update_prefixes_auth_failure,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(access_not_authorised('terminusdb://system/data/User/Doug',
                                  '@schema':'Action/instance_write_access',
                                  _),
            _)
     ]) :-

    add_user("Doug", some("password"), User_URI),
    Path = "admin/testdb",
    Commit_Info = commit_info{author: "me", message: "prefix it"},
    update_prefixes(Path, system_descriptor{}, User_URI, Commit_Info, _{}).

test(get_prefix,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    get_prefix(Path, system_descriptor{}, Auth, '@base', Base),
    atom_string(Base, "http://somewhere.for.now/document/").

test(get_prefix_not_found,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(prefix_not_found(nonexistent), _)
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    get_prefix(Path, system_descriptor{}, Auth, nonexistent, _).

test(add_prefix,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "add prefix"},
    add_prefix(Path, system_descriptor{}, Auth, Commit_Info, myprefix, "http://example.org/myprefix/", Result),
    get_dict(myprefix, Result, URI),
    atom_string(URI, "http://example.org/myprefix/").

test(add_prefix_already_exists,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(prefix_already_exists(_), _)
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "add prefix"},
    add_prefix(Path, system_descriptor{}, Auth, Commit_Info, myprefix, "http://example.org/myprefix/", _),
    % Try to add the same prefix again
    add_prefix(Path, system_descriptor{}, Auth, Commit_Info, myprefix, "http://example.org/other/", _).

test(add_prefix_reserved,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(reserved_prefix(_), _)
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "add prefix"},
    add_prefix(Path, system_descriptor{}, Auth, Commit_Info, '@custom', "http://example.org/", _).

test(add_prefix_invalid_iri,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(invalid_iri(_), _)
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "add prefix"},
    add_prefix(Path, system_descriptor{}, Auth, Commit_Info, myprefix, "not a valid iri", _).

test(valid_prefix_name_basic,
     []) :-
    valid_prefix_name(schema),
    valid_prefix_name("ex"),
    valid_prefix_name('v1'),
    valid_prefix_name("my_prefix"),
    valid_prefix_name('foo-bar').

test(valid_prefix_name_with_dot,
     []) :-
    valid_prefix_name('v1.0'),
    valid_prefix_name("schema.org"),
    valid_prefix_name('a.b.c').

test(invalid_prefix_name_start_digit,
     [fail]) :-
    valid_prefix_name('1bad').

test(invalid_prefix_name_start_hyphen,
     [fail]) :-
    valid_prefix_name('-bad').

test(valid_prefix_name_start_underscore,
     []) :-
    valid_prefix_name('_prefix').

test(valid_prefix_name_end_dot,
     []) :-
    valid_prefix_name('valid.').

test(invalid_prefix_name_colon,
     [fail]) :-
    valid_prefix_name('bad:name').

test(invalid_prefix_name_slash,
     [fail]) :-
    valid_prefix_name('bad/name').

test(invalid_prefix_name_space,
     [fail]) :-
    valid_prefix_name('bad name').

test(invalid_prefix_name_empty,
     [fail]) :-
    valid_prefix_name('').

test(invalid_prefix_name_reserved,
     [fail]) :-
    valid_prefix_name('@base').

% Edge case tests: Unicode boundaries
test(invalid_unicode_latin_extended,
     [fail]) :-
    valid_prefix_name('café').  % Latin-1 Supplement U+00E9

test(invalid_unicode_cyrillic,
     [fail]) :-
    valid_prefix_name('имя').  % Cyrillic

test(invalid_unicode_cjk,
     [fail]) :-
    valid_prefix_name('名前').  % Japanese

test(invalid_unicode_emoji,
     [fail]) :-
    valid_prefix_name('🚀').  % Emoji

% Edge case tests: Special ASCII characters
test(valid_ascii_all_letters,
     []) :-
    valid_prefix_name('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz').

test(valid_ascii_all_allowed_chars,
     []) :-
    valid_prefix_name('aZ09_.-').

test(invalid_start_period,
     [fail]) :-
    valid_prefix_name('.bad').

test(invalid_multiple_colons,
     [fail]) :-
    valid_prefix_name('a:b:c').

test(invalid_unicode_combining_mark,
     [fail]) :-
    valid_prefix_name('a\x0301\').  % Combining acute accent U+0301

% Edge case: Single underscore is valid in NCName
test(valid_single_underscore,
     []) :-
    valid_prefix_name('_').

% Edge case: Maximum length reasonable prefix
test(valid_very_long_name,
     []) :-
    atom_chars(LongName, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                          a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                          a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]),
    valid_prefix_name(LongName).

test(update_prefix,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "update prefix"},
    % First add a prefix
    add_prefix(Path, system_descriptor{}, Auth, Commit_Info, myprefix, "http://example.org/old/", _),
    % Then update it
    update_prefix(Path, system_descriptor{}, Auth, Commit_Info, myprefix, "http://example.org/new/", Result),
    get_dict(myprefix, Result, URI),
    atom_string(URI, "http://example.org/new/").

test(update_prefix_not_found,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(prefix_not_found(_), _)
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "update prefix"},
    update_prefix(Path, system_descriptor{}, Auth, Commit_Info, nonexistent, "http://example.org/", _).

test(upsert_prefix_create,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "upsert prefix"},
    % Upsert creates if not exists
    upsert_prefix(Path, system_descriptor{}, Auth, Commit_Info, newprefix, "http://example.org/new/", Result),
    get_dict(newprefix, Result, URI),
    atom_string(URI, "http://example.org/new/").

test(upsert_prefix_update,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "upsert prefix"},
    % First create
    upsert_prefix(Path, system_descriptor{}, Auth, Commit_Info, myprefix, "http://example.org/old/", _),
    % Then upsert updates
    upsert_prefix(Path, system_descriptor{}, Auth, Commit_Info, myprefix, "http://example.org/updated/", Result),
    get_dict(myprefix, Result, URI),
    atom_string(URI, "http://example.org/updated/").

test(delete_prefix,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "delete prefix"},
    % First add a prefix
    add_prefix(Path, system_descriptor{}, Auth, Commit_Info, todelete, "http://example.org/delete/", _),
    % Then delete it
    delete_prefix(Path, system_descriptor{}, Auth, Commit_Info, todelete, Result),
    \+ get_dict(todelete, Result, _).

test(delete_prefix_not_found,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(prefix_not_found(_), _)
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "delete prefix"},
    delete_prefix(Path, system_descriptor{}, Auth, Commit_Info, nonexistent, _).

test(delete_prefix_reserved,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(reserved_prefix(_), _)
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "delete prefix"},
    delete_prefix(Path, system_descriptor{}, Auth, Commit_Info, '@base', _).

test(add_prefix_with_emoji_iri,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    Path = "admin/testdb",
    super_user_authority(Auth),
    Commit_Info = commit_info{author: "test", message: "add emoji prefix"},
    % IRI with emoji - valid UTF-8 in IRI
    add_prefix(Path, system_descriptor{}, Auth, Commit_Info, emoji, "http://example.org/🚀/", Result),
    get_dict(emoji, Result, URI),
    atom_string(URI, "http://example.org/🚀/").

:- end_tests(api_prefixes).
