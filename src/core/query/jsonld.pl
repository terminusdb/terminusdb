:- module(jsonld, [
              expand/2,
              expand/3,
              expand_context/2,
              prefix_expand/3,
              compress/3,
              term_jsonld/2,
              term_jsonld/3,
              value_jsonld/2,
              jsonld_id/2,
              jsonld_type/2,
              get_key_document/4,
              compress_dict_uri/3,
              compress_uri/4,
              context_prefix_expand/3,
              has_at/1,
              json_dict_create/2
          ]).

/** <module> JSON-LD
 *
 * Definitions for translating and manipulating JSON_LD
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(core(util)).
:- reexport(core(util/syntax)).

:- use_module(core(triple)).
:- use_module(core(transaction)).

:- use_module(library(pcre)).
:- use_module(library(pairs)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(dicts)).
:- use_module(library(plunit)).

% Currently a bug in groundedness checking.
%:- use_module(library(mavis)).

% sgml for xsd dates.
%:- use_module(library(sgml), [xsd_time_string/3]).

% efficiency
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

/**
 * expand(+JSON_LD, -JSON) is det.
 *
 * Expands from JSON_LD prefixed format to fully expanded form.
 */
expand(JSON_LD, JSON) :-
    expand(JSON_LD, json{}, JSON).

/**
 * expand(+JSON_LD, +Context:dict, -JSON) is det.
 *
 * Expands from JSON_LD prefixed format to fully expanded form.
 */
expand(JSON_LD, Context, JSON) :-
    is_dict(JSON_LD),
    % Law of recursion: Something must be getting smaller...
    % This "something" is the removal of the context
    % from the object to be expanded.
    select_dict(json{'@context' : New_Context}, JSON_LD, JSON_Ctx_Free),
    !,
    expand_context(New_Context,Context_Expanded),
    (   is_dict(Context)
    ->  merge_dictionaries(Context_Expanded,Context,Local_Context)
    ;   Local_Context = Context_Expanded),
    expand(JSON_Ctx_Free, Local_Context, JSON_Ex),
    put_dict('@context',JSON_Ex,Local_Context,JSON).
expand(json{'@type' : Type, '@value' : Value}, Context, JSON) :-
    !,
    prefix_expand(Type,Context,TypeX),
    JSON = json{'@type' : TypeX, '@value' : Value}.
expand(json{'@language' : Lang, '@value' : Value}, _Context, JSON) :-
    !,
    JSON = json{'@language' : Lang, '@value' : Value}.
expand(JSON_LD, Context, JSON) :-
    is_dict(JSON_LD),
    !,
    dict_keys(JSON_LD,Keys),
    setof(Key-Value,
          expand_dict_keys(JSON_LD, Context, Keys, Key, Value),
          Data),
    dict_pairs(JSON,json,Data).
expand(JSON_LD, Context, JSON) :-
    is_list(JSON_LD),
    !,
    maplist({Context}/[JL,J]>>expand(JL,Context,J),JSON_LD,JSON).
expand(JSON, _, JSON) :-
    atom(JSON),
    !.
expand(JSON, _, JSON) :-
    string(JSON).

expand_dict_keys(JSON_LD, Context, Keys, Key, Value) :-
    member(K,Keys),
    get_dict(K,JSON_LD,V),

    (   member(K,['@id','@type'])
    ->  prefix_expand(V,Context,Value),
        Key = K
    ;   expand_key(K,Context,Key,Key_Context),
        expand_value(V,Key_Context,Context,Value)
    ).

expand_value(V,Key_Ctx,Ctx,Value) :-
    is_list(V),
    !,
    maplist({Key_Ctx,Ctx}/[V1,V2]>>expand_value(V1,Key_Ctx,Ctx,V2),
            V,Value).
expand_value(V,Key_Ctx,Ctx,Value) :-
    string(V),
    !,
    (   json{'@type' : "@id"} = Key_Ctx
    %   Type ID distribution
    ->  prefix_expand(V, Ctx, Expanded),
        Value = json{'@id' : Expanded}
    ;   json{'@type' : "@id", '@id' : ID} = Key_Ctx
    %   Fixed ID distribution
    ->  Value = json{'@id' : ID}
    ;   json{'@type' : Type} = Key_Ctx
    ->  prefix_expand(Type,Ctx,EType),
        Value = json{'@value' : V,
                  '@type' : EType}
    ;   json{'@language' : Lang} = Key_Ctx
    ->  Value = json{'@value' : V,
                  '@language' : Lang}
    ;   json{} = Key_Ctx
    ->  V = Value
    ;   throw(error('Invalid key context in expand_value/4'))
    ).
expand_value(V,Key_Ctx,Ctx,Value) :-
    is_dict(V),
    !,
    (   json{'@type' : "@id", '@id' : _} :< Key_Ctx
    ->  expand(V,Ctx,Value)
    ;   json{'@type' : "@id"} :< Key_Ctx
    ->  expand(V,Ctx,Value)
    ;   is_dict(Key_Ctx)
    ->  merge_dictionaries(V,Key_Ctx,V2),
        expand(V2,Ctx,Value)
    ;   throw(error('Invalid key context in expand_value/4'))
    ).
expand_value(V,_Key_Ctx,_Ctx,V) :-
    number(V),
    !.
expand_value(V, _, _, V) :-
    var(V), % For dictionary matching in WOQL
    !.
expand_value(true,_Key_Ctx,_Ctx,true) :-
    !.
expand_value(null,_Key_Ctx,_Ctx,null) :-
    !.
%expand_value(V,_Key_Ctx,_Ctx,V) :-
%    atom(V),
%    !.
expand_value(V,Key_Ctx,_Ctx,_Value) :-
    throw(error(unknown_key_context(Key_Ctx,V))).

has_at(K) :-
    re_match('^@.*',K).

context_prefix_expand(K,Context,Key) :-
    %   Already qualified
    (   uri_has_protocol(K)
    ->  K = Key
    %   Prefixed URI
    ;   uri_has_prefix_unsafe(K, Groups)
    ->  atom_string(Prefix, Groups.prefix),
        writeq(Context),
        (   get_dict(Prefix, Context, Expanded)
        ->  atom_concat(Expanded,(Groups.suffix),Key)
        ;   throw(error(key_has_unknown_prefix(K),_)))
    %   Keyword
    ;   is_at(K)
    ->  K = Key
    %   This is a value expansion
    ;   get_dict(K,Context,Obj),
        is_dict(Obj)
    ->  (   get_dict('@base', Context, Base)
        ->  true
        ;   Base = ''),
        (   get_dict('@vocab', Context, Vocab)
        ->  true
        ;   Vocab = ''),
        atomic_list_concat([Base,Vocab,K],Key)
    %   Pass on by...
    ;   K = Key
    ).

% For document templating
prefix_expand(K,_Context,K2) :-
    var(K),
    K = K2,
    !.
prefix_expand('',_,_) :-
    throw(error(empty_key, _)).
prefix_expand("",_,_) :-
    throw(error(empty_key, _)).
prefix_expand(K,Context,Key) :-
    %   Is already qualified
    (   uri_has_protocol(K)
    ->  K = Key
    %   Is prefixed (but does not check for protocol)
    ;   uri_has_prefix_unsafe(K, Groups)
    ->  atom_string(Prefix, Groups.prefix),
        (   get_dict(Prefix,Context,Expanded)
        ->  atom_concat(Expanded,(Groups.suffix),Key)
        ;   throw(error(key_has_unknown_prefix(K), _)))
    ;   is_at(K)
    ->  K = Key
    ;   (   get_dict('@base', Context, Base)
        ->  true
        ;   Base = ''),
        (   get_dict('@vocab', Context, Vocab)
        ->  true
        ;   Vocab = ''),
        atomic_list_concat([Base,Vocab,K],Key)
    ).

/*
 * expand_context(+Context,-Context_Expanded) is det.
 *
 * Expand all prefixes in the context for other elements of the context
 */
expand_context(Context,Context_Expanded) :-
    is_dict(Context),
    !,
    dict_pairs(Context,_,Pairs),
    maplist({Context}/[K-V,Key-V]>>context_prefix_expand(K,Context,Key), Pairs, Expanded_Pairs),
    dict_create(Context_Expanded, _, Expanded_Pairs).
expand_context(Context_URI,Context_Expanded) :-
    atomic(Context_URI),
    atomic_list_concat(['(http(s?)://[^/]*)?/api/prefixes/(?P<path>.*)'], Pattern),
    pcre:re_matchsub(Pattern, Context_URI, Match, []),

    Path = (Match.path),

    do_or_die(
        query:resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    collection_descriptor_prefixes(Descriptor, Context_Expanded).

/*
 * expand_key(+K,+Context,-Key,-Value) is det.
 *
 * Need to expand value from context information in the event
 * that typing/language information is contained there.
 */
expand_key(K,Context,Key,Value) :-
    prefix_expand(K,Context,Key_Candidate),
    (   get_dict(Key_Candidate,Context,R)
    ->  (   is_dict(R)
        ->  Key = Key_Candidate,
            Value = R
        ;   atom_string(Key,R),
            Value = json{})
    ;   Key = Key_Candidate,
        Value = json{}).

:- begin_tests(jsonld_expand).
:- use_module(core(util/test_utils)).
:- use_module(config(terminus_config)).

test(expand_inner, [])
:-
    Context = json{doc : 'http://outer_document/'},
    Document = json{'@context' : json{ doc : 'http://inner_document/'},
                 'test' : json{'@id' : 'doc:test'}},
    expand(Document, Context, JSON_LD),

    json{ test:json{'@id':'http://inner_document/test'}} :< JSON_LD.


test(expand_path, [])
:-
    server(Server),
    atomic_list_concat([Server, '/api/prefixes/_system'], Context),
    Document = json{'@context' : Context,
                    'test' : json{'@id' : 'test'}},
    expand(Document, Context, JSON_LD),
    Result = (JSON_LD.'@context'),

    json{ '@base':'terminusdb:///system/data/'} :< Result.

test(expand_prefix_with_colon, []) :-
    Prefixes = _{'@base': "terminusdb:///data/",
                 '@schema': "terminusdb:///schema#",
                 'a': "http://example.org/a/"},

    prefix_expand("@base:foo:bar", Prefixes, 'terminusdb:///data/foo:bar'),
    prefix_expand("@schema:foo:bar", Prefixes, 'terminusdb:///schema#foo:bar'),
    prefix_expand("a:foo:bar", Prefixes, 'http://example.org/a/foo:bar').

:- end_tests(jsonld_expand).

/*
 * compress_uri(URI, Key, Prefix, Compressed) is det.
 *
 * Take a URI and a context and compress uris such that
 * Ctx = { foo : 'http://example.com/' }
 * URI = 'http://example.com/bar'
 * compresses to => foo:bar
 */
compress_uri(URI, '@base', Prefix, Rest) :-
    !,
    sub_atom(URI, _, Length, After, Prefix),
    sub_atom(URI, Length, After, _, Rest).
compress_uri(_URI, Key, _Prefix, _Rest) :-
    is_at(Key),
    Key \= '@schema',
    !,
    fail.
compress_uri(URI, Key, Prefix, Comp) :-
    sub_atom(URI, _, Length, After, Prefix),
    sub_atom(URI, Length, After, _, Rest),
    atomic_list_concat([Key,':',Rest], Comp).

compress_pairs_uri(URI, Pairs, Folded_URI) :-
    (   member(Prefix-Expanded, Pairs),
        compress_uri(URI, Prefix, Expanded, Folded_URI)
    ->  true
    ;   URI = Folded_URI).

compress_dict_uri(URI, Dict, Folded_URI) :-
    dict_pairs(Dict, _, Pairs),
    exclude([_-B]>>is_dict(B), Pairs, Filtered),
    compress_pairs_uri(URI, Filtered, Folded_URI).

is_at(Key) :-
    sub_string(Key,0,1,_,"@").

/*
 * compress(JSON,Context,JSON_LD) is det.
 *
 * Replace expanded URIs using Context.
 */
compress(JSON,Context,JSON_LD) :-
    dict_pairs(Context, _, Pairs),
    compress_aux(JSON,Pairs,JSON_Pre),

    extend_with_context(JSON_Pre,Context,JSON_LD).

extend_with_context(JSON_Pre,Context,JSON_LD) :-
    is_dict(JSON_Pre),
    !,
    put_dict(json{'@context' : Context}, JSON_Pre, JSON_LD).
extend_with_context(JSON_Pre,Context,JSON_LD) :-
    is_list(JSON_Pre),

    maplist({Context}/[JSON,JSON_Out]>>extend_with_context(JSON,Context,JSON_Out),
            JSON_Pre,
            JSON_LD).

compress_aux(JSON,Ctx_Pairs,JSON_LD) :-
    is_dict(JSON),
    !,
    dict_pairs(JSON, _, JSON_Pairs),
    maplist({Ctx_Pairs}/[Key-Value,Folded_Key-Folded_Value]>>(
                (   Key = '@type'
                ->  Folded_Key = Key,
                    compress_pairs_uri(Value, Ctx_Pairs, Folded_Value)
                ;   Key = '@id'
                ->  Folded_Key = Key,
                    compress_pairs_uri(Value, Ctx_Pairs, Folded_Value)
                ;   compress_pairs_uri(Key, Ctx_Pairs, Folded_Key),
                    compress_aux(Value, Ctx_Pairs, Folded_Value))
            ),
            JSON_Pairs,
            Folded_JSON_Pairs),
    dict_create(JSON_LD, _,Folded_JSON_Pairs).
compress_aux(JSON,Ctx_Pairs,JSON_LD) :-
    is_list(JSON),
    !,
    maplist({Ctx_Pairs}/[Obj,Transformed]>>compress_aux(Obj,Ctx_Pairs,Transformed), JSON, JSON_LD).
compress_aux(JSON,_Ctx_Pairs,JSON) :-
    (   string(JSON)
    ->  true
    ;   atom(JSON)),
    !.
compress_aux(JSON,_Ctx_Pairs,JSON) :-
    number(JSON),
    !.

:- begin_tests(jsonld_compress).
:- use_module(core(util/test_utils)).


% Note: This needs to treat "base", "vocab", as well.
test(compress_prefix, [])
:-
    Context = json{ ex : "http://example.com/document/",
                 scm : "http://example.com/schema#"},
    Document = json{ '@type' : "http://example.com/schema#Fact",
                  'http://example.com/schema#your_face' :
                  json{ '@id' : "http://example.com/document/is_ugly" }},
    compress(Document, Context, Compressed),

    json{'@type':'scm:Fact','scm:your_face':json{'@id':'ex:is_ugly'}} :< Compressed.

test(compress_base, [])
:-
    Context = json{ '@base' : "http://example.com/document/",
                    scm : "http://example.com/schema#"},
    Document = json{ '@type' : "http://example.com/schema#Fact",
                     'http://example.com/schema#your_face' :
                     json{ '@id' : "http://example.com/document/is_ugly" }},
    compress(Document, Context, Compressed),

    json{'@type':'scm:Fact','scm:your_face':json{'@id':'is_ugly'}} :< Compressed.

:- end_tests(jsonld_compress).

/*
 *
 */
value_jsonld(D^^T,json{'@type' : T, '@value' : V}) :-
    (   compound(D)
    ->  typecast(D^^T, 'http://www.w3.org/2001/XMLSchema#string',
                 [], V^^_)
    ;   D=V),
    !.
value_jsonld(D@L,json{'@language' : L, '@value' : D}) :-
    !.
value_jsonld(D,D) :-
    atom(D),
    !.
value_jsonld(List,JSON_List) :-
    is_list(List),
    maplist(value_jsonld,List,JSON_List).

/*
 * term_jsonld(Term,JSON) is det.
 *
 * expand a prolog internal json representation to dicts.
 */
term_jsonld(Term,JSON) :-
    is_list(Term),
    maplist([A=B,A-JSON_B]>>term_jsonld(B,JSON_B), Term, JSON_List),
    % We are a dictionary not a list.
    !,
    dict_pairs(JSON, _, JSON_List).
term_jsonld(Term,JSON) :-
    is_list(Term),
    !,
    maplist([Obj,JSON]>>term_jsonld(Obj,JSON), Term, JSON).
term_jsonld(Term,JSON) :-
    value_jsonld(Term,JSON).

/* With prefix compression */
term_jsonld(Var,_,null) :-
    var(Var), % Tranform unbound variables to null
    !.
term_jsonld(D^^T,Prefixes,json{'@type' : TC, '@value' : V}) :-
    (   compound(D) % check if not bool, number, atom, string
    ->  typecast(D^^T, 'http://www.w3.org/2001/XMLSchema#string',
                 [], V^^_)
    ;   D=V),
    !,
    compress_dict_uri(T, Prefixes, TC).
term_jsonld(D@L,_,json{'@language' : L, '@value' : D}) :-
    !.
term_jsonld(Term,Prefixes,JSON) :-
    is_list(Term),
    !,
    maplist({Prefixes}/[Obj,JSON]>>term_jsonld(Obj,Prefixes,JSON), Term, JSON).
term_jsonld(JSON,Prefixes,JSON_Compressed) :-
    is_dict(JSON),
    !,
    dict_pairs(JSON, _, Pairs),
    maplist({Prefixes}/[Key-Value,Compressed_Key-Compressed_Value]>>(
                term_jsonld(Key,Prefixes,Compressed_Key),
                term_jsonld(Value,Prefixes,Compressed_Value)
            ), Pairs, New_Pairs),
    dict_create(JSON_Compressed, _, New_Pairs).
term_jsonld(URI,Prefixes,URI_Compressed) :-
    compress_dict_uri(URI, Prefixes, URI_Compressed).

/*
 * Get the ID a json objct
 *
 */
jsonld_id(Obj,ID) :-
    is_dict(Obj),
    !,
    get_dict('@id',Obj,ID).


/*
 * Get the type a json objct
 *
 */
jsonld_type(Obj,Type) :-
    is_dict(Obj),
    !,
    get_dict('@type',Obj,Type).

/*
 * get_key_document(Key,Ctx,Document,Value)
 */
get_key_document(Key,Ctx,Document,Value) :-
    is_dict(Document),
    % Law of recursion: Something must be getting smaller...
    % This "something" is the removal of the context
    % from the object to be expanded.
    select_dict(json{'@context' : New_Ctx}, Document, Document_Ctx_Free),
    !,
    expand_context(New_Ctx,Ctx_Expanded),
    merge_dictionaries(Ctx,Ctx_Expanded,Local_Ctx),
    get_key_document(Key, Local_Ctx, Document_Ctx_Free, Value).
get_key_document(Key,Ctx,Document,Value) :-
    prefix_expand(Key,Ctx,KX),
    expand(Document,Ctx,Document_Ex),
    get_dict(KX,Document_Ex,Value1),

    % Transform the value to canonical form
    % if it is set so we can do comparison
    (   memberchk(Key,['@type', '@id']),
        (   atom(Value)
        ;   string(Value))
    %   We are a value that could require expansion
    ->  prefix_expand(Value,Ctx,Value2),
        atom_string(V,Value2),
        atom_string(V,Value1)
    %   May want to do dictionary expansion here
    %   to get canonical comparitives.
    %   We probably need a Dict1 ~ Dict2 comparison
    %   operator which uses an expanded canonical form
    %   modulo adornments (like '@context').
    ;   Value1 = Value).

/*
 * Convert JSON literals (null, false, true) to strings when found in the values
 * of keywords.
 */
stringify_json_literals(Key-Val_In, Key-Val_Out) :-
    is_at(Key),
    atom(Val_In),
    memberchk(Val_In, [null, false, true]),
    !,
    atom_string(Val_In, Val_Out).
stringify_json_literals(Key-Val, Key-Val).

/*
 * Build a JSON dict safely.
 *
 * Use this instead of dict_create to avoid problems converting between atoms
 * and strings.
 */
json_dict_create(JSON, Pairs) :-
    maplist(stringify_json_literals, Pairs, Pairs_Fixed),
    dict_create(JSON, json, Pairs_Fixed).
