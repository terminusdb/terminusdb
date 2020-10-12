:- module(jsonld, [
              expand/2,
              expand/3,
              expand_context/2,
              prefix_expand/3,
              compress/3,
              term_jsonld/2,
              jsonld_triples/3,
              jsonld_id/2,
              jsonld_type/2,
              get_key_document/4,
              compress_dict_uri/3
          ]).

/** <module> JSON-LD
 *
 * Definitions for translating and manipulating JSON_LD
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(core(util)).
:- reexport(core(util/syntax)).

:- use_module(core(triple)).
:- use_module(core(transaction)).

:- use_module(library(pcre)).
:- use_module(library(pairs)).
:- use_module(library(http/json)).

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
    expand(JSON_LD, _{}, JSON).

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
    select_dict(_{'@context' : New_Context}, JSON_LD, JSON_Ctx_Free),
    !,
    expand_context(New_Context,Context_Expanded),
    (   is_dict(Context)
    ->  merge_dictionaries(Context_Expanded,Context,Local_Context)
    ;   Local_Context = Context_Expanded),
    expand(JSON_Ctx_Free, Local_Context, JSON_Ex),
    put_dict('@context',JSON_Ex,Local_Context,JSON).
expand(_{'@type' : Type, '@value' : Value}, Context, JSON) :-
    !,
    prefix_expand(Type,Context,TypeX),
    JSON = _{'@type' : TypeX, '@value' : Value}.
expand(_{'@language' : Lang, '@value' : Value}, _Context, JSON) :-
    !,
    JSON = _{'@language' : Lang, '@value' : Value}.
expand(JSON_LD, Context, JSON) :-
    is_dict(JSON_LD),
    % \+ select_dict(_{'@context' : New_Context}, JSON_LD, JSON_Ctx_Free),
    !,
    dict_keys(JSON_LD,Keys),
    findall(Key-Value,
            (
                member(K,Keys),
                get_dict(K,JSON_LD,V),

                (   member(K,['@id','@type'])
                ->  prefix_expand(V,Context,Value),
                    Key = K
                ;   expand_key(K,Context,Key,Key_Context),
                    expand_value(V,Key_Context,Context,Value)
                )
            ),
            Data),
    dict_create(JSON,_,Data).
expand(JSON_LD, Context, JSON) :-
    is_list(JSON_LD),
    !,
    maplist({Context}/[JL,J]>>expand(JL,Context,J),JSON_LD,JSON).
expand(JSON, _, JSON) :-
    atom(JSON),
    !.
expand(JSON, _, JSON) :-
    string(JSON).

expand_value(V,Key_Ctx,Ctx,Value) :-
    is_list(V),
    !,
    maplist({Key_Ctx,Ctx}/[V1,V2]>>expand_value(V1,Key_Ctx,Ctx,V2),
            V,Value).
expand_value(V,Key_Ctx,Ctx,Value) :-
    string(V),
    !,
    (   _{'@type' : "@id"} = Key_Ctx
    %   Type ID distribution
    ->  prefix_expand(V, Ctx, Expanded),
        Value = _{'@id' : Expanded}
    ;   _{'@type' : "@id", '@id' : ID} = Key_Ctx
    %   Fixed ID distribution
    ->  Value = _{'@id' : ID}
    ;   _{'@type' : Type} = Key_Ctx
    ->  prefix_expand(Type,Ctx,EType),
        Value = _{'@value' : V,
                  '@type' : EType}
    ;   _{'@language' : Lang} = Key_Ctx
    ->  Value = _{'@value' : V,
                  '@language' : Lang}
    ;   _{} = Key_Ctx
    ->  V = Value
    ;   throw(error('Invalid key context in expand_value/4'))
    ).
expand_value(V,Key_Ctx,Ctx,Value) :-
    is_dict(V),
    !,
    (   _{'@type' : "@id", '@id' : _} :< Key_Ctx
    ->  expand(V,Ctx,Value)
    ;   _{'@type' : "@id"} :< Key_Ctx
    ->  expand(V,Ctx,Value)
    ;   is_dict(Key_Ctx)
    ->  merge_dictionaries(V,Key_Ctx,V2),
        expand(V2,Ctx,Value)
    ;   throw(error('Invalid key context in expand_value/4'))
    ).
expand_value(V,_Key_Ctx,_Ctx,V) :-
    number(V),
    !.
expand_value(true,_Key_Ctx,_Ctx,true) :-
    !.
%expand_value(V,_Key_Ctx,_Ctx,V) :-
%    atom(V),
%    !.
expand_value(V,Key_Ctx,_Ctx,_Value) :-
    format(atom(M),'Unknown key context ~q for value ~q', [Key_Ctx,V]),
    throw(error(M)).

has_at(K) :-
    re_match('^@.*',K).

context_prefix_expand(K,Context,Key) :-
    %   Already qualified
    (   uri_has_protocol(K)
    ->  K = Key
    %   Prefixed URI
    ;   uri_has_prefix(K)
    ->  split_atom(K,':',[Prefix,Suffix]),
        (   get_dict(Prefix,Context,Expanded)
        ->  atom_concat(Expanded,Suffix,Key)
        ;   throw(error(key_has_unknown_prefix(K),_)))
    %   Keyword
    ;   has_at(K)
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

prefix_expand(K,Context,Key) :-
    %   Is already qualified
    (   uri_has_protocol(K)
    ->  K = Key
    %   Is prefixed
    ;   uri_has_prefix(K)
    ->  split_atom(K,':',[Prefix,Suffix]),
        (   get_dict(Prefix,Context,Expanded)
        ->  atom_concat(Expanded,Suffix,Key)
        ;   throw(error(key_has_unknown_prefix(K), _)))
    ;   has_at(K)
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
            Value = _{})
    ;   Key = Key_Candidate,
        Value = _{}).

:- begin_tests(jsonld_expand).
:- use_module(core(util/test_utils)).

test(expand_inner, [])
:-
    Context = _{doc : 'http://outer_document/'},
    Document = _{'@context' : _{ doc : 'http://inner_document/'},
                 'test' : _{'@id' : 'doc:test'}},
    expand(Document, Context, JSON_LD),

    _{ test:_{'@id':'http://inner_document/test'}} :< JSON_LD.


test(expand_path, [])
:-
    server(Server),
    atomic_list_concat([Server, '/api/prefixes/_system'], Context),
    Document = _{'@context' : Context,
                 'test' : _{'@id' : 'doc:test'}},
    expand(Document, Context, JSON_LD),
    Result = (JSON_LD.'@context'),
    _{ doc:'terminusdb:///system/data/'} :< Result.

:- end_tests(jsonld_expand).

/*
 * compress_uri(URI, Key, Prefix, Compressed) is det.
 *
 * Take a URI and a context and compress uris such that
 * Ctx = { foo : 'http://example.com/' }
 * URI = 'http://example.com/bar'
 * compresses to => foo:bar
 */
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
    compress_pairs_uri(URI, Pairs, Folded_URI).

is_at(Key) :-
    sub_string(Key,0,1,_,"@").

/*
 * compress(JSON,Context,JSON_LD) is det.
 *
 * Replace expanded URIs using Context.
 */
compress(JSON,Context,JSON_LD) :-
    dict_pairs(Context, _, Pairs),
    include([A-B]>>(\+ is_at(A), % exclude @type, @vocab, etc. from expansions
                    (   atom(B)
                    ->  true
                    ;   string(B))),
            Pairs, Valid_Pairs),
    compress_aux(JSON,Valid_Pairs,JSON_Pre),

    extend_with_context(JSON_Pre,Context,JSON_LD).

extend_with_context(JSON_Pre,Context,JSON_LD) :-
    is_dict(JSON_Pre),
    !,
    merge_dictionaries(_{'@context' : Context}, JSON_Pre, JSON_LD).
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
compress_aux(time(H, M, S),_Ctx_Pairs, Atom) :-
    format(atom(Atom),'~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+', [H,M,S]).
compress_aux(date(Y, M, D),_Ctx_Pairs, Atom) :-
    format(atom(Atom),'~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+', [Y,M,D]).
compress_aux(date(Y,M,D,HH,MM,SS,_Z,_ZH,_ZM),_Ctx_Pairs, Atom) :-
    format(atom(Atom),'~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+',
           [Y,M,D,HH,MM,SS]).
compress_aux(date_time(Y,M,D,HH,MM,SS),_Ctx_Pairs, Atom) :-
    format(atom(Atom),'~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+',
           [Y,M,D,HH,MM,SS]).
compress_aux(month_day(M,D),_Ctx_Pairs, Atom) :-
    format(atom(Atom),'~|~`0t~d~2+-~|~`0t~d~2+', [M,D]).
compress_aux(year_month(Y,M),_Ctx_Pairs, Atom) :-
    format(atom(Atom),'~|~`0t~d~4+-~|~`0t~d~2+', [Y,M]).

:- begin_tests(jsonld_compress).
:- use_module(core(util/test_utils)).


% Note: This needs to treat "base", "vocab", as well.
test(compress_prefix, [])
:-
    Context = _{ ex : "http://example.com/document/",
                 scm : "http://example.com/schema#"},
    Document = _{ '@type' : "http://example.com/schema#Fact",
                  'http://example.com/schema#your_face' :
                  _{ '@id' : "http://example.com/document/is_ugly" }},
    compress(Document, Context, Compressed),

    _{'@type':'scm:Fact','scm:your_face':_{'@id':'ex:is_ugly'}} :< Compressed.

:- end_tests(jsonld_compress).


/*

date(2017, 9, 29, 0, 0, 0, 0, 0, 0)

date(Y,M,D)
xsd:dateTime	date_time(Y,M,D,HH,MM,SS) (2,3)
xsd:gDay	integer
xsd:gMonth	integer
xsd:gMonthDay	month_day(M,D)
xsd:gYear	integer
xsd:gYearMonth	year_month(Y,M)

compress_aux(time(H, M, S),_Ctx_Pairs, Atom) :-
    format(atom(Atom),'~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+', [H,M,S]).
compress_aux(time(H, M, S),_Ctx_Pairs, Atom) :-
    format(atom(Atom),'~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+', [H,M,S]).
compress_aux(time(H, M, S),_Ctx_Pairs, Atom) :-
    format(atom(Atom),'~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+', [H,M,S]).
*/

/*
 * term_jsonld(Term,JSON) is det.
 *
 * expand a prolog internal json representation to dicts.
 */
term_jsonld(date(Y, M, D, HH, MM, SS, _Z, _ZH, _ZM)^^'http://www.w3.org/2001/XMLSchema#dateTime',
            _{'@type' : 'http://www.w3.org/2001/XMLSchema#dateTime', '@value' : Atom}) :-
    !,
    % Really should put the time zone in properly.
    format(atom(Atom),'~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+',
           [Y,M,D,HH,MM,SS]).
term_jsonld(D^^T,_{'@type' : T, '@value' : D}) :-
    !.
term_jsonld(D@L,_{'@language' : L, '@value' : D}) :-
    !.
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
term_jsonld(JSON,JSON).

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

/* Debug
 * This should not exist.... We should already be expanded.
 *
 * jsonld_predicate_value(P,Val,Ctx,Expanded_Value) is det.
 *
 * Look up the predicate in the context to see if we should expand our information
 * about the value.
 */
jsonld_predicate_value(P,Val,Ctx,Expanded_Value) :-
    get_dict(P,Ctx,Expanded),

    (   is_dict(Expanded)
    ->  (   is_dict(Val)
        ->  merge_dictionaries(Expanded,Val,Expanded_Value)
        ;   is_list(Val)
        % definitely do something here which isn't this!!
        ->  true
        ;   get_dict('@type', Expanded, '@id')
        ->  (   \+ atom(Val)
            ->  throw(error(malformed_jsonld_id(Val),_))),
            (   get_dict('@id', Expanded, Type)
            ->  Expanded_Value = _{'@id' : Val,
                                   '@type' : Type}
            ;   Expanded_Value = _{'@id' : Val})
        ;   merge_dictionaries(Expanded, _{'@value' : Val}, Expanded_Value))
    ;   Val = Expanded_Value).
jsonld_predicate_value(_P,Val,_Ctx,Val).

/*
 * jsonld_triples(+Dict,+Ctx,-Triples) is det.
 *
 * Return the triples associated with a JSON-LD structure.
 */
jsonld_triples(JSON, Ctx, Triples) :-
    get_dict_default('@context', JSON, Internal,_{}),
    merge_dictionaries(Ctx, Internal, New_Ctx),
    expand_context(New_Ctx,New_Expanded),
    expand(JSON,New_Expanded,JSON_Ex),
    jsonld_triples_aux(JSON_Ex, New_Ctx, Triples_Unsorted),
    sort(Triples_Unsorted,Triples).

/*
 * jsonld_triples_aux(Dict, Ctx, Tuples) is det.
 *
 * Create a list of triples which provide a representation for the objects
 * in the graph.
 */
jsonld_triples_aux(Dict, Ctx, Triples) :-
    is_dict(Dict),
    !,

    (   get_dict('@id', Dict, ID)
    ->  jsonld_id_triples(ID,Dict,Ctx,Triples)
    ;   dict_pairs(Dict, _, JSON_Pairs),
        maplist({Ctx}/[ID-PV,Triples]>>(
                    (   memberchk(ID,['@context','@id'])
                    ->  Triples = []
                    ;   jsonld_id_triples(ID,PV,Ctx,Triples))
                ),
                JSON_Pairs, Triples_List),
        append(Triples_List,Triples)).
% what could this be? A list?
jsonld_triples_aux(List, Ctx, Triples) :-
    is_list(List),
    !,

    maplist({Ctx}/[Obj,Ts]>>
                jsonld_triples_aux(Obj,Ctx,Ts),
            List,
            Ts_List),
    append(Ts_List, Triples).

/*
 * jsonld_id_triples(ID,PV,Ctx,Triples) is det.
 *
 * We have the id and are looking for the edge and values.
 */
jsonld_id_triples(ID,PV,Ctx,Triples) :-
    is_dict(PV),
    !,

    dict_pairs(PV, _, JSON_Pairs),
    maplist({ID,Ctx}/[P-V,Triples]>>(
                (   memberchk(P,['@context','@id'])
                ->  Triples = []
                ;   P = '@type'
                ->  Pred = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
                    Triples = [(ID,Pred,V)]
                ;   P = Pred,
                    %jsonld_predicate_value(P,V,Ctx,EV),
                    json_value_triples(ID,Pred,V,Ctx,Triples)
                )
            ),
            JSON_Pairs, Triples_List),

    append(Triples_List, Triples).

json_value_triples(ID,Pred,V,Ctx,Triples) :-
    (   is_dict(V)
    ->  (   V = _{'@type' : Type,
                  '@value' : Data
                 }
        ->  atom_string(Atom_Type,Type),
            Triples = [(ID,Pred,Data^^Atom_Type)]
        ;   V = _{'@language' : Lang,
                  '@value' : Data}
        ->  atom_string(Atom_Lang,Lang),
            Triples = [(ID,Pred,Data@Atom_Lang)]
        ;   jsonld_id(V,Val),
            jsonld_triples_aux(V,Ctx,Rest),
            Triples = [(ID,Pred,Val)|Rest])
    ;   is_list(V)
    ->  maplist({ID,Pred,Ctx}/[V,Triples]>>(
                    json_value_triples(ID,Pred,V,Ctx,Triples)
                ), V, Triples_List),
        append(Triples_List, Triples)
    ;   string(V)
    ->  atom_string(A,V),
        Triples = [(ID,Pred,A)]
    ;   atom(V)
    ->  Triples = [(ID,Pred,V)]
    ;   Triples = [(ID,Pred,V)]).

/*
 * get_key_document(Key,Ctx,Document,Value)
 */
get_key_document(Key,Ctx,Document,Value) :-
    is_dict(Document),
    % Law of recursion: Something must be getting smaller...
    % This "something" is the removal of the context
    % from the object to be expanded.
    select_dict(_{'@context' : New_Ctx}, Document, Document_Ctx_Free),
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
