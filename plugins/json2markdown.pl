:- module(json2markdown, [
    clean_json2markdown_env/0,
    json_to_markdown/2
]).

/** <module> json2markdown plugin — default embedding for JSONDocument

Provides a fallback embedding string for documents that have no
schema-defined embedding query+template. Converts the JSON document
dict to a Markdown representation inspired by 
https://github.com/memochou1993/json2markdown

Original Copyright (c) 2024 memochou1993. MIT Licensed.
This plugin Copyright (c) 2026 Philippe Höij. MIT Licensed.

Activation:
  - Always active for sys#JSONDocument types.
  - Optional env var TERMINUSDB_JSON2MARKDOWN_GRAPHSPECS can restrict
    to specific data products (comma-separated graphspecs, e.g.
    "admin/testdb,org/products").
  - Optional env var TERMINUSDB_JSON2MARKDOWN_TYPES can extend to
    additional type IRIs (comma-separated).

The /4 hook (with graphspec) is tried first, then the /3 hook (without).
*/

:- use_module(core(plugin_api)).
:- use_module(core(plugins)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(dicts)).
:- use_module(core(triple/casting), [decimal_precision/1, rational_to_decimal_string/3]).

% ==========================================================================
% Config
% ==========================================================================

json2markdown_graphspecs(GraphSpecs) :-
    plugin_env('TERMINUSDB_JSON2MARKDOWN_GRAPHSPECS', Raw),
    !,
    split_comma_separated(Raw, GraphSpecs).
json2markdown_graphspecs([]).

json2markdown_extra_types(Types) :-
    plugin_env('TERMINUSDB_JSON2MARKDOWN_TYPES', Raw),
    !,
    split_comma_separated(Raw, Types).
json2markdown_extra_types([]).

clean_json2markdown_env :-
    abolish_plugin_env('TERMINUSDB_JSON2MARKDOWN_GRAPHSPECS'),
    abolish_plugin_env('TERMINUSDB_JSON2MARKDOWN_TYPES'),
    unsetenv('TERMINUSDB_JSON2MARKDOWN_GRAPHSPECS'),
    unsetenv('TERMINUSDB_JSON2MARKDOWN_TYPES').

split_comma_separated(Raw, Items) :-
    (   atom(Raw) -> atom_string(Raw, Str)
    ;   string(Raw) -> Str = Raw
    ;   Str = Raw
    ),
    atomic_list_concat(Parts, ',', Str),
    findall(Item,
            ( member(P, Parts),
              atom_string(PA, P),
              normalize_space(atom(CA), PA),
              CA \= '',
              atom_string(Item, CA)
            ),
            Items).

% ==========================================================================
% Document matching
%
% This plugin is a catch-all fallback for raw JSON documents (those stored
% with raw_json=true). It matches any document whose @id starts with
% "JSONDocument/", regardless of what @type the JSON payload declares.
%
% To create a plugin that matches on a specific @type instead, comment out
% the matches_document/1 clause below and uncomment the matches_type/1
% clause with the desired type IRI:
% ==========================================================================

json_document_id_prefix("JSONDocument/").
json_document_id_prefix_full("terminusdb:///data/JSONDocument/").

matches_document(Document) :-
    json_document_id_prefix(Prefix),
    get_dict('@id', Document, Id),
    id_to_string(Id, IdStr),
    (   sub_string(IdStr, 0, _, _, Prefix)
    ->  true
    ;   json_document_id_prefix_full(FullPrefix),
        sub_string(IdStr, 0, _, _, FullPrefix)
    ->  true
    ;   sub_string(IdStr, _, _, _, Prefix)
    ).

id_to_string(Id, IdStr) :-
    (   atom(Id) -> atom_string(Id, IdStr)
    ;   string(Id) -> IdStr = Id
    ;   term_string(Id, IdStr)
    ).

% To match on a specific @type instead, comment out matches_document/1
% above and uncomment this:
% matches_type('http://example.com/schema#MyType').
% matches_type(Type_IRI) :-
%     json2markdown_extra_types(Types),
%     member(Type_IRI, Types).

matches_graphspec(GraphSpec) :-
    json2markdown_graphspecs(GraphSpecs),
    (   GraphSpecs = []
    ->  true
    ;   member(GraphSpec, GraphSpecs)
    ).

% ==========================================================================
% Multifile hooks
% ==========================================================================

:- multifile plugins:embedding_for_type/4.
:- multifile plugins:embedding_for_type/3.

plugins:embedding_for_type(GraphSpec, _Type_IRI, Document, Markdown) :-
    matches_graphspec(GraphSpec),
    matches_document(Document),
    !,
    json_to_markdown(Document, Markdown).

plugins:embedding_for_type(_Type_IRI, Document, Markdown) :-
    json2markdown_graphspecs([]),
    matches_document(Document),
    !,
    json_to_markdown(Document, Markdown).

% ==========================================================================
% JSON to Markdown conversion
%
% Follows the json2markdown approach:
%   - Dict keys become headings
%   - String/number/boolean values become paragraph text
%   - List values become bullet lists
%   - Nested dicts recurse with increased heading level
%   - @id and @type are rendered as regular fields
% ==========================================================================

json_to_markdown(Document, Markdown) :-
    with_output_to(string(Raw), render_document(Document, 1)),
    strip_trailing_whitespace(Raw, Cleaned),
    string_concat("\n", Cleaned, WithLead),
    string_concat(WithLead, "\n", Markdown).

strip_trailing_whitespace(Str, Cleaned) :-
    atomic_list_concat(Lines, '\n', Str),
    strip_leading_blanks(Lines, AfterLead),
    reverse(AfterLead, Reversed),
    strip_leading_blanks(Reversed, ReversedTrimmed),
    reverse(ReversedTrimmed, Trimmed),
    findall(Line,
            ( member(L, Trimmed),
              ( atom_string(LA, L),
                normalize_space(atom(CA), LA),
                CA \= ''
              -> atom_string(Line, CA)
              ; Line = L
              )
            ),
            NonEmpty),
    atomic_list_concat(NonEmpty, '\n', Cleaned).

strip_leading_blanks([], []).
strip_leading_blanks([''|Rest], Trimmed) :-
    !,
    strip_leading_blanks(Rest, Trimmed).
strip_leading_blanks(Lines, Lines).

render_document(Dict, Level) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, _, Pairs),
    partition_at_pairs(Pairs, AtPairs, ContentPairs),
    render_pairs(ContentPairs, Level),
    render_at_suffix(AtPairs).

render_document(Value, _Level) :-
    render_scalar(Value),
    nl.

partition_at_pairs(Pairs, AtPairs, ContentPairs) :-
    partition([K-_]>>(atom(K), sub_atom(K, 0, 1, _, '@')), Pairs, AtPairs, ContentPairs).

render_at_suffix([]).
render_at_suffix(AtPairs) :-
    forall(member(K-V, AtPairs),
           format("-- ~w ~w~n", [K, V])).

render_pairs([], _Level).
render_pairs([Key-Value | Rest], Level) :-
    pretty_key(Key, PrettyKey),
    render_pair(PrettyKey, Value, Level),
    render_pairs(Rest, Level).

render_pair(Key, Value, Level) :-
    is_dict(Value),
    !,
    heading(Level, Key),
    NextLevel is Level + 1,
    render_document(Value, NextLevel),
    nl.
render_pair(Key, Value, Level) :-
    is_list(Value),
    !,
    heading(Level, Key),
    NextLevel is Level + 1,
    render_list(Value, NextLevel),
    nl.
render_pair(Key, Value, Level) :-
    heading(Level, Key),
    render_scalar(Value),
    nl,
    nl.

render_list([], _Level).
render_list([Item | Rest], Level) :-
    (   is_dict(Item)
    ->  format("- ~n", []),
        render_document_indented(Item, Level)
    ;   is_list(Item)
    ->  format("- ~n", []),
        NextLevel is Level + 1,
        render_list(Item, NextLevel)
    ;   scalar_to_string(Item, ItemStr),
        format("- ~w~n", [ItemStr])
    ),
    render_list(Rest, Level).

render_document_indented(Dict, Level) :-
    dict_pairs(Dict, _, Pairs),
    render_pairs_indented(Pairs, Level).

render_pairs_indented([], _Level).
render_pairs_indented([Key-Value | Rest], Level) :-
    pretty_key(Key, PrettyKey),
    (   is_dict(Value)
    ->  indent(Level),
        heading_inline(Level, PrettyKey),
        NextLevel is Level + 1,
        render_document_indented(Value, NextLevel)
    ;   is_list(Value)
    ->  indent(Level),
        heading_inline(Level, PrettyKey),
        NextLevel is Level + 1,
        render_list_indented(Value, NextLevel)
    ;   indent(Level),
        format("**~w**: ", [PrettyKey]),
        render_scalar(Value),
        nl
    ),
    render_pairs_indented(Rest, Level).

render_list_indented([], _Level).
render_list_indented([Item | Rest], Level) :-
    indent(Level),
    (   is_dict(Item)
    ->  format("- ~n", []),
        NextLevel is Level + 1,
        render_pairs_indented(Item, NextLevel)
    ;   is_list(Item)
    ->  format("- ~n", []),
        NextLevel is Level + 1,
        render_list_indented(Item, NextLevel)
    ;   scalar_to_string(Item, ItemStr),
        format("- ~w~n", [ItemStr])
    ),
    render_list_indented(Rest, Level).

render_scalar(Value) :-
    (   rational(Value), \+ integer(Value)
    ->  decimal_precision(Precision),
        rational_to_decimal_string(Value, DecimalStr, Precision),
        format("~w", [DecimalStr])
    ;   atom(Value) -> format("~w", [Value])
    ;   string(Value) -> format("~w", [Value])
    ;   number(Value) -> format("~w", [Value])
    ;   true -> format("~w", [Value])
    ).

scalar_to_string(Value, Str) :-
    (   rational(Value), \+ integer(Value)
    ->  decimal_precision(Precision),
        rational_to_decimal_string(Value, Str, Precision)
    ;   atom(Value) -> atom_string(Value, Str)
    ;   string(Value) -> Str = Value
    ;   number(Value) -> number_string(Value, Str)
    ;   term_string(Value, Str)
    ).

heading(Level, Key) :-
    (   Level =< 6
    ->  hashes(Level, Hashes),
       format("~w ~w~n~n", [Hashes, Key])
    ;   format("**~w**~n~n", [Key])
    ).

heading_inline(Level, Key) :-
    (   Level =< 6
    ->  hashes(Level, Hashes),
       format("~w ~w~n", [Hashes, Key])
    ;   format("**~w**~n", [Key])
    ).

%% pretty_key(+Key, -PrettyKey) is det.
%%
%%  Replaces underscores with spaces and capitalizes the first letter
%%  of each word if it starts with a lowercase letter. Words starting
%%  with a number or uppercase letter are left unchanged.
pretty_key(Key, PrettyKey) :-
    (   atom(Key) -> atom_string(Key, Str)
    ;   string(Key) -> Str = Key
    ),
    atomic_list_concat(Words, '_', Str),
    maplist(capitalize_word, Words, PrettyWords),
    atomic_list_concat(PrettyWords, ' ', PrettyAtom),
    atom_string(PrettyKey, PrettyAtom).

capitalize_word(Word, Capitalized) :-
    (   atom(Word) -> atom_string(Word, WS)
    ;   string(Word) -> WS = Word
    ),
    string_chars(WS, Chars),
    (   Chars = []
    ->  Capitalized = ''
    ;   Chars = [First|Rest],
        (   char_type(First, lower)
        ->  string_upper(First, Upper),
            string_chars(RestS, Rest),
            string_concat(Upper, RestS, Capitalized)
        ;   Capitalized = WS
        )
    ).

hashes(1, '#').
hashes(2, '##').
hashes(3, '###').
hashes(4, '####').
hashes(5, '#####').
hashes(6, '######').

indent(Level) :-
    Level > 0,
    Indent is Level - 1,
    forall(between(1, Indent, _), write('  ')).

:- begin_tests(json2markdown_unit).

test(simple_string_value) :-
    Doc = json{title: "Hello"},
    json_to_markdown(Doc, MD),
    once(sub_string(MD, _, _, _, "Hello")).

test(simple_number_value) :-
    Doc = json{count: 42},
    json_to_markdown(Doc, MD),
    once(sub_string(MD, _, _, _, "42")).

test(list_of_strings) :-
    Doc = json{tags: ["alpha", "beta"]},
    json_to_markdown(Doc, MD),
    once(sub_string(MD, _, _, _, "alpha")),
    once(sub_string(MD, _, _, _, "beta")).

test(nested_dict) :-
    Doc = json{meta: json{author: "Alice", year: 2024}},
    json_to_markdown(Doc, MD),
    once(sub_string(MD, _, _, _, "Alice")),
    once(sub_string(MD, _, _, _, "2024")).

test(heading_level_increases) :-
    Doc = json{outer: json{inner: "deep"}},
    json_to_markdown(Doc, MD),
    once(sub_string(MD, _, _, _, "deep")).

test(multiple_keys) :-
    Doc = json{a: "first", b: "second"},
    json_to_markdown(Doc, MD),
    once(sub_string(MD, _, _, _, "first")),
    once(sub_string(MD, _, _, _, "second")).

test(empty_dict) :-
    Doc = json{},
    once(( json_to_markdown(Doc, MD),
           normalize_space(atom(Norm), MD),
           Norm == '' )).

test(boolean_value) :-
    Doc = json{active: @(true)},
    catch(json_to_markdown(Doc, _MD), _, fail).

test(jsondocument_id_matches) :-
    matches_document(_{'@id': "JSONDocument/abc123"}).

test(non_jsondocument_id_does_not_match) :-
    \+ matches_document(_{'@id': "Entity/abc123"}).

test(jsondocument_id_matches_with_type) :-
    matches_document(_{'@id': "JSONDocument/abc123", '@type': "Entity"}).

test(pretty_key_simple) :-
    pretty_key(title, PK),
    sub_string(PK, _, _, _, "Title").

test(pretty_key_underscore) :-
    pretty_key('first_name', PK),
    sub_string(PK, _, _, _, "First Name").

test(pretty_key_leaves_numbers) :-
    pretty_key('3rd_option', PK),
    sub_string(PK, _, _, _, "3rd Option").

test(pretty_key_leaves_uppercase) :-
    pretty_key('URL', PK),
    sub_string(PK, _, _, _, "URL").

test(pretty_key_multiple_underscores) :-
    pretty_key('user_id_number', PK),
    sub_string(PK, _, _, _, "User Id Number").

test(rational_scalar_value) :-
    Doc = json{depth: 313r10},
    json_to_markdown(Doc, MD),
    \+ sub_string(MD, _, _, _, "313r10"),
    sub_string(MD, _, _, _, "31.3").

test(rational_in_list) :-
    Doc = json{values: [181r5, 471r5]},
    json_to_markdown(Doc, MD),
    \+ sub_string(MD, _, _, _, "181r5"),
    \+ sub_string(MD, _, _, _, "471r5"),
    sub_string(MD, _, _, _, "36.2"),
    sub_string(MD, _, _, _, "94.2").

test(rational_in_nested_dict) :-
    Doc = json{size: json{width: 313r10, height: 471r5}},
    json_to_markdown(Doc, MD),
    \+ sub_string(MD, _, _, _, "r10"),
    \+ sub_string(MD, _, _, _, "r5").

:- end_tests(json2markdown_unit).
