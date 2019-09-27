:- module(jsonld, [
              expand/2,
              expand/3,
              expand_context/2,
              prefix_expand/3,
              compress/3,
              term_jsonld/2,
              jsonld_triples/3,
              jsonld_triples/4,
              jsonld_id/2,
              get_key_document/3,
              get_key_document/4
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
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
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

:- use_module(library(pairs)).
:- use_module(library(utils)).
:- use_module(library(http/json)).
% Currently a bug in groundedness checking.
%:- use_module(library(mavis)).
:- use_module(library(database)).

       
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
    merge_dictionaries(Context,Context_Expanded,Local_Context),
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
expand_value(_V,Key_Ctx,_Ctx,_Value) :-
    format(atom(M),'Unknown key context ~q', [Key_Ctx]),
    throw(error(M)).

has_protocol(K) :-
    re_match('^[^:/]+://.*',K).

has_prefix(K) :-
    \+ has_protocol(K),
    re_match('^[^:]*:[^:]*',K).

has_at(K) :-
    re_match('^@.*',K).

context_prefix_expand(K,Context,Key) :-
    %   Already qualified
    (   has_protocol(K)
    ->  K = Key
    %   Prefixed URI
    ;   has_prefix(K)
    ->  split_atom(K,':',[Prefix,Suffix]),
        (   get_dict(Prefix,Context,Expanded)
        ->  atom_concat(Expanded,Suffix,Key)
        ;   format(atom(M), 'Key has unkown prefix: ~q', [K]),
            throw(syntax_error(M)))
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
    (   has_protocol(K)
    ->  K = Key
    %   Is prefixed
    ;   has_prefix(K)
    ->  split_atom(K,':',[Prefix,Suffix]),
        (   get_dict(Prefix,Context,Expanded)
        ->  atom_concat(Expanded,Suffix,Key)
        ;   format(atom(M), 'Key has unknown prefix: ~q', [K]),
            throw(syntax_error(M)))
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
    dict_pairs(Context,_,Pairs),
    maplist({Context}/[K-V,Key-V]>>context_prefix_expand(K,Context,Key), Pairs, Expanded_Pairs),
    dict_create(Context_Expanded, _, Expanded_Pairs).

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
        ;   string_to_atom(R,Key),
            Value = _{})
    ;   Key = Key_Candidate,
        Value = _{}).

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

/* 
 * compress(JSON,Context,JSON_LD) is det.
 * 
 * Replace expanded URIs using Context.
 */
compress(JSON,Context,JSON_LD) :-
    dict_pairs(Context, _, Pairs),
    include([_A-B]>>atom(B), Pairs, Valid_Pairs),
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
                compress_pairs_uri(Key, Ctx_Pairs, Folded_Key),
                compress_aux(Value, Ctx_Pairs, Folded_Value)
            ),
            JSON_Pairs,
            Folded_JSON_Pairs),
    dict_create(JSON_LD, _,Folded_JSON_Pairs).
compress_aux(JSON,Ctx_Pairs,JSON_LD) :-
    is_list(JSON),
    !,
    maplist({Ctx_Pairs}/[Obj,Transformed]>>compress_aux(Obj,Ctx_Pairs,Transformed), JSON, JSON_LD).
compress_aux(JSON,_Ctx_Pairs,JSON) :-
    string(JSON),
    !.
compress_aux(JSON,_Ctx_Pairs,JSON) :-
    number(JSON),
    !.
compress_aux(URI,Ctx_Pairs,Folded_URI) :-
    atom(URI),
    compress_pairs_uri(URI,Ctx_Pairs,Folded_URI).
compress_aux(time(H, M, S),_Ctx_Pairs, _{'@value' : Atom, '@type' : 'xsd:time'}) :-
    format(atom(Atom),'~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+', [H,M,S]).

/* 
 * term_jsonld(Term,JSON) is det.
 * 
 * expand a prolog internal json representation to dicts. 
 */
term_jsonld(literal(type(T,D)),_{'@type' : T, '@value' : D}).
term_jsonld(literal(lang(L,D)),_{'@language' : L, '@value' : D}).
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
This should be more explicit and separate....

jsonld_id(_Obj,Database,ID) :-
    database_name(Database,Collection),
    interpolate([Collection,'/document'],Base),
    gensym(Base,ID).
  */

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
            ->  format(atom(Msg),'Not a well formed JSON id: ~q~n', [Val]),
                throw(syntax_error(Msg))),
            (   get_dict('@id', Expanded, Type)
            ->  Expanded_Value = _{'@id' : Val,
                                   '@type' : Type}
            ;   Expanded_Value = _{'@id' : Val})
        ;   merge_dictionaries(Expanded, _{'@value' : Val}, Expanded_Value))
    ;   Val = Expanded_Value).
jsonld_predicate_value(_P,Val,_Ctx,Val).
    
/* 
 * jsonld_triples(+Dict,+Database,-Triples) is det. 
 * 
 * Return the triples associated with a JSON-LD structure.
 */
jsonld_triples(JSON, Database, Triples) :-
    jsonld_triples(JSON, _{}, Database, Triples).

jsonld_triples(JSON, Ctx, Database, Triples) :-
    get_dict_default('@context', JSON, Internal,_{}),
    merge_dictionaries(Ctx, Internal, New_Ctx),
    expand_context(New_Ctx,New_Expanded),
    expand(JSON,New_Expanded,JSON_Ex),
    jsonld_triples_aux(JSON_Ex, New_Ctx, Database, Triples).
    
/* 
 * jsonld_triples_aux(Dict, Ctx, Database, Tuples) is det.
 * 
 * Create the n-triple format of the given json triples 
 * suitable for use with delete/5 or insert/5
 */
jsonld_triples_aux(Dict, Ctx, Database, Triples) :-
    is_dict(Dict),
    !,

    (   get_dict('@id', Dict, ID)
    ->  jsonld_id_triples(ID,Dict,Ctx,Database,Triples)
    ;   dict_pairs(Dict, _, JSON_Pairs),
        maplist({Database,Ctx}/[ID-PV,Triples]>>(
                    (   memberchk(ID,['@context','@id'])
                    ->  Triples = []
                    ;   jsonld_id_triples(ID,PV,Ctx,Database,Triples))
                ),
                JSON_Pairs, Triples_List),
        append(Triples_List,Triples)).
% what could this be? A list?
jsonld_triples_aux(List, Ctx, Database, Triples) :-
    is_list(List),
    !,
    
    maplist({Ctx,Database}/[Obj,Ts]>>
                jsonld_triples_aux(Obj,Ctx,Database,Ts),
            List,
            Ts_List),
    append(Ts_List, Triples).
    
/* 
 * jsonld_id_triples(ID,PV,Ctx,Database,Triples) is det. 
 *
 * We have the id and are looking for the edge and values. 
 */
jsonld_id_triples(ID,PV,Ctx,Database,Triples) :-
    is_dict(PV),
    !,    

    dict_pairs(PV, _, JSON_Pairs),
    maplist({ID,Ctx,Database}/[P-V,Triples]>>(
                database_name(Database,C),
                database_instance(Database,G),

                * format('Incoming P-V: ~q~n',[P-V]),
                (   memberchk(P,['@context','@id'])
                ->  Triples = []
                ;   P = '@type'
                ->  Pred = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
                    Triples = [(C,G,ID,Pred,V)]
                ;   P = Pred,
                    %jsonld_predicate_value(P,V,Ctx,EV),
                    json_value_triples(C,G,ID,Pred,V,Ctx,Database,Triples)
                )                
            ),
            JSON_Pairs, Triples_List),
    
    append(Triples_List, Triples).
 
json_value_triples(C,G,ID,Pred,V,Ctx,Database,Triples) :-
    (   is_dict(V)
    ->  (   V = _{'@type' : Type,
                  '@value' : Data
                 }
        ->  Triples = [(C,G,ID,Pred,literal(type(Type,Data)))]
        ;   V = _{'@language' : Lang,
                  '@value' : Data}
        ->  Triples = [(C,G,ID,Pred,literal(lang(Lang,Data)))]
        ;   jsonld_id(V,Val),
            jsonld_triples_aux(V,Ctx,Database,Rest),
            Triples = [(C,G,ID,Pred,Val)|Rest])        
    ;   is_list(V)
    ->  maplist({C,G,ID,Pred,Ctx,Database}/[V,Triples]>>(
                    json_value_triples(C,G,ID,Pred,V,Ctx,Database,Triples)
                ), V, Triples_List),
        append(Triples_List, Triples)
    ;   string(V)        
    ->  atom_string(A,V),
        Triples = [(C,G,ID,Pred,literal(type('http://www.w3.org/2001/XMLSchema#string',A)))]
    ;   atom(V)        
    ->  Triples = [(C,G,ID,Pred,literal(type('http://www.w3.org/2001/XMLSchema#string',V)))]
    ;   Triples = [(C,G,ID,Pred,V)]).


/* 
 * get_key_document(Key,Document,Value) is det
 * 
 * Looks up the value of a key in a document
 */ 
get_key_document(Key,Document,Value) :-
    get_global_jsonld_context(Ctx),
    get_key_document(Key,Ctx,Document,Value).
    
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
    (   member(Key,['@type', '@id']),
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

