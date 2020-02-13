:- module(woql_compile,[
              compile_query/3,
              compile_query/4,
              run_query/2,
              run_query/3,
              is_new/2,
              empty_ctx/1,
              empty_ctx/2,
              active_graphs/2
          ]).

/** <module> WOQL Compile
 *
 * Core compiler for the WOQL query language.
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

:- use_module(database).
:- use_module(woql_term).
:- use_module(utils).
:- use_module(triplestore, [
                  xrdf/5,
                  insert/5,
                  delete/5
              ]).
%:- use_module(schema).
:- use_module(relationships, [
                  relationship_source_property/3,
                  relationship_target_property/3
              ]).
:- use_module(inference).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(solution_sequences)).

:- use_module(temp_graph).

:- use_module(jsonld).
:- use_module(json_woql).

:- use_module(frame, [
                  update_object/3,
                  delete_object/2
              ]).

% We may need to patch this in again...
%:- use_module(query, [enrich_graph_fragment/5]).

:- use_module(validate_schema, [datatype_property/2,
                                object_property/2,
                            basetype_subsumption_of/2]).
:- use_module(casting, [typecast/4,hash/3,idgen/3]).
:- use_module(prefixes, [get_collection_jsonld_context/2, woql_context/1]).

:- use_module(speculative_parse, [guess_date/2]).

% This should really not be used... it is too low level - Gavin
%:- use_module(journaling, [write_triple/5]).

:- use_module(remote_file, [
                  copy_remote/4
              ]).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

% is this actually needed?
:- op(2, xfx, :).
:- op(600, xfx, <:).
:- op(600, xfx, as).
:- op(2, xfx, :=).
:- op(1050, xfx, =>).
:- op(2, xfx, @).
:- op(2, xfx, ^^).

merge_output_graphs(OGs1,OGs2,OGs3) :-
    merge_output_graphs_aux(OGs1,OGs2,[],OGs3).

merge_output_graphs_aux([G=g(O1,_H1-T1,_FH1-FT1)|OGs1],
                        OGs2,
                        Seen,
                        [G=g(Output,Head-_,Fringe_Head-_)|OGs3]) :-
    member(G=g(O2,_H2-T2,_FH2-FT2),OGs2),
    !,
    Output=O1,
    Output=O2,
    Head=T1,
    Head=T2,
    Fringe_Head=FT1,
    Fringe_Head=FT2,
    merge_output_graphs_aux(OGs1, OGs2,[G|Seen],OGs3).
merge_output_graphs_aux([G=g(OG,H-T,FH-FT)|Rest],
                        OGs1,
                        Seen,
                        [G=g(OG,H-T,FH-FT)|OGs]) :-
    % \+ member(G=_,OGs1),
    merge_output_graphs_aux(Rest, OGs1, Seen, OGs).
merge_output_graphs_aux([], OGs2, Seen, OGs3) :-
    exclude([G=_]>>(member(G,Seen)), OGs2, OGs3).

/*
 * merge_output_bindings(Bindings0,Bindings1,Bindings_Merged) is det.
 *
 * merge bindings such that co-bindings get the same binding variables.
 */
merge_output_bindings(A, B, C) :-
    union(A,B,C).

merge_graph(Database_List0,Database_List1,Database_List2) :-
    % in both
    convlist({Database_List1}/[Name=graph(T0,G0,F0),
                            Name=graph(T2,G2,F2)]>>(
                member(Name=graph(T1,G1,F1),Database_List1),
                append(T0,T1,T2),
                append(G0,G1,G2),
                append(F0,F1,F2)),
             Database_List0,Database_List_Same),
    % in first
    convlist({Database_List1}/[Name=graph(T0,G0,F0),
                            Name=graph(T0,G0,F0)]>>(
                 \+ member(Name=_,Database_List1)),
             Database_List0,Database_List_Left),
    % in second
    convlist({Database_List0}/[Name=graph(T0,G0,F0),
                            Name=graph(T0,G0,F0)]>>(
                 \+ member(Name=_,Database_List0)),
             Database_List1,Database_List_Right),
    append([Database_List_Same,
            Database_List_Left,
            Database_List_Right],
           Database_List2).


merge_graphs_aux([],G,G).
merge_graphs_aux([G0|Gs],G1,Out) :-
    merge_graph(G0,G1,Result),
    merge_graphs_aux(Gs,Result,Out).

merge_graphs(Gs,G) :-
    merge_graphs_aux(Gs,[],G).

/*
 * merge(S0,S1,SM) is det.
 *
 * We need to merge multiple states into a signal state for output.
 *
 * we use S0 as the prototype
 */
merge(S0,S1,SM) :-
    member(output_graphs=OGs0,S0),
    member(output_graphs=OGs1,S1),
    merge_output_graphs(OGs0,OGs1,OGs),

    member(bindings=B0,S0),
    member(bindings=B1,S1),
    merge_output_bindings(B0,B1,Bindings),

    member(prefixes=Prefixes,S0),
    member(database=Database,S0),
    member(write_graph=Write_Graph,S0),
    member(definitions=Definitions,S0),
    member(bound=Bound,S1),
    member(used=Used,S1),
    member(module=M,S1),
    member(collection=C,S1),
    SM=[output_graphs=OGs,
        current_output_graph=default,
        definitions=Definitions,
        prefixes=Prefixes,
       database=Database,
        write_graph=Write_Graph,
        collection=C,
        used=Used,
        bound=Bound,
        module=M,
        bindings=Bindings].

merge_all_aux([],S,S).
merge_all_aux([S1|Ss],S0,S) :-
    merge(S0,S1,S2),
    merge_all_aux(Ss,S2,S).

merge_all([S0|Ss],S) :-
    merge_all_aux(Ss,S0,S).

/*
 * elt(Elt,Set) is det.
 */
elt(Elt,Set) :-
    (   member(Elt, Set)
    ->  true
    ;   format(atom(M), 'Element (~q) does not appear in set (~q)', [Elt,Set]),
        throw(error(M))
    ).

is_new(Elt,TDB) :-
    (   var(TDB)
    ->  true
    ;   TDB = []
    ->  true
    ;   TDB=[Elt|_Rest]
    ->  false
    ;   TDB=[_|Rest],
        is_new(Elt,Rest)).

/* Monadic selection */
update(C0,C1,S0,S1) :-
    select(C0,S0,C1,S1).

view(C0,S0,S0) :-
    elt(C0,S0).

peak(S0,S0,S0).

return(S0,_,S0).

/*
 * Ctx is a context object which has some of the following attributes:
 * [attribute=Value]
 *
   prefixes = [P=foo, Q=bar]  % which prefixes are expanded
   bindings = [v=bar]         % bindings for variables
   bound = N                  % Current number of bindings

 */
empty_ctx([output_graphs=[default=g(_G,_H-_T,_F-_FT)], % fresh vars
           current_output_graph=default,
           write_graph=_,
           prefixes=[rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#',
                     rdfs='http://www.w3.org/2000/01/rdf-schema#'],
           definitions=[],
           collection='https://example.org',
           database=_,
           files=[],
           used=_Used,
           bound=3,
           module=M,
           bindings=[]]) :-
    gensym(module_,M).

empty_ctx(S0,S7) :-
    empty_ctx(S),
    elt(prefixes=Prefixes,S0),
    elt(database=Database,S0),
    elt(write_graph=Write_Graph,S0),
    elt(module=M,S0),
    elt(definitions=D,S0),
    elt(collection=C,S0),
    elt(files=Files,S0),
    select(prefixes=_,S,
           prefixes=Prefixes,S1),
    select(database=_,S1,
          database=Database,S2),
    select(module=_,S2,
           module=M,S3),
    select(definitions=_,S3,
           definitions=D,S4),
    select(collection=_,S4,
           collection=C,S5),
    select(write_graph=_,S5,
           write_graph=Write_Graph,S6),
    select(files=_,S6,
           files=Files,S7).

empty_ctx(Prefixes,S0,S7) :-
    empty_ctx(S),
    elt(database=Database,S0),
    elt(write_graph=Write_Graph,S0),
    elt(module=M,S0),
    elt(definitions=D,S0),
    elt(collection=C,S0),
    elt(files=Files,S0),
    select(prefixes=_,S,
           prefixes=Prefixes,S1),
    select(database=_,S1,
          database=Database,S2),
    select(module=_,S2,
           module=M,S3),
    select(definitions=_,S3,
           definitions=D,S4),
    select(collection=_,S4,
           collection=C,S5),
    select(write_graph=_,S5,
           write_graph=Write_Graph,S6),
    select(files=_,S6,
           files=Files,S7).

/*
 * compile_representation(S,T,V) is det.
 *
 * Gives the internal representation of some type T from some string S.
 */
compile_representation(String,'http://www.w3.org/2001/XMLSchema#dateTime',Date) :-
    !,
    guess_date(String,Date).
compile_representation(String,Type,literal(type(Type,String))).

resolve(ignore,_Something) -->
    !,
    [].
resolve(ID / Suf,U) -->
    !,
    resolve(ID,ID_res),
    {
        atomic_list_concat([ID_res,Suf],U)
    }.
resolve(v(X),Xe) -->
    !,
    update(bindings=B0,
           bindings=B1),
    {
        (   member(X=Xe,B0)
        ->  B1=B0
        ;   B1=[X=Xe|B0])
    }.
resolve(X,Xe) -->
    view(prefixes=Prefixes),
    {
        atom(X),
        (   member(X=URI,Prefixes)
        ->  Xe=URI
        ;   X=Xe)
    }.
resolve(X,Xe) -->
    {
        is_dict(X),
        !,
        expand(X,XEx), % also need to use the prefixes here.
        jsonld_id(XEx,XI)
    },
    resolve(XI,Xe).
resolve(X@L,literal(lang(LE,XS))) -->
    resolve(X,XE),
    {
        (   ground(XE),
            atom(XE)
        ->  atom_string(XE,XS)
        ;   XE = XS),
        !
    },
    resolve(L,LE).
resolve(X^^T,Lit) -->
    resolve(X,XE),
    resolve(T,TE),
    {
        (   ground(XE)
        ->  (   atom(XE)
            ->  atom_string(XE,XS)
            ;   XE=XS),
            compile_representation(XS,TE,Lit)
        ;   Lit = literal(type(TE,XE))),
        !
    }.
resolve(L,Le) -->
    {
        is_list(L),
        !
    },
    mapm(resolve,L,Le).
resolve(X,X) -->
    {
        string(X)
    },
    !.
resolve(X,X) -->
    {
        atom(X)
    },
    !.
resolve(X,X) -->
    {
        number(X)
    },
    !.

resolve_possible_object(_,Y,Ye,true) -->
    resolve(Y,Ye),
    {   \+ \+ Ye = literal(_)  },
    !.
resolve_possible_object(P,Y,Ye,T) -->
    view(database=Database),
    resolve(Y,Yi),
    {
        % We need to do this at query time since the schema may be lifted...
        T = (    validate_schema:datatype_property(P,Database)
            ->  (   validate_schema:range(P,R,Database)
                ->  Ye = literal(type(R,Yi))
                ;   Ye = Yi
                )
            ;   Ye = Yi)
    }.


/*
 * compile_query(+Term:any,-Prog:any,-Ctx_Out:context) is det.
 */
compile_query(Term, Prog, Ctx_Out) :-
    empty_ctx(Ctx_In),
    compile_query(Term,Prog,Ctx_In,Ctx_Out).

compile_query(Term, Prog, Ctx_In, Ctx_Out) :-
    (   compile_wf(Term, Prog, Ctx_In, Ctx_Out)
    ->  true
    ;   format(atom(M), 'Failure to compile term ~q', [Term]),
        throw(compilation_error(M))).

assert_program([]).
assert_program([Def|Remainder]) :-
    assertz(Def),
    assert_program(Remainder).

retract_program([]).
retract_program([def(Name,Args,_Body)|Remainder]) :-
    length(Args,N),
    abolish(woql_compile:(Name/N)),
    retract_program(Remainder).

bookend_graphs(Databases) :-
    maplist([_=g(L,L-[],_-[])]>>true, Databases).

nonground_elts([A|Rest_In],[A|Rest_Out]) :-
    \+ ground(A),
    !,
    nonground_elts(Rest_In,Rest_Out).
nonground_elts([_|Rest_In],Rest_Out) :-
    nonground_elts(Rest_In,Rest_Out).
nonground_elts([],[]).

/*
 * enrich_graphs(Databases,Database,Enriched) is det.
 *
 * DDD enrich_graph_fragment currently unimplemented...
 */
/*
enrich_graphs(Databases,Database,Enriched) :-
    convlist({Database}/[G=g(L,H-[],F-[]),
                      G=Result]>>(
                 % just set it to something
                 % if unbound
                 ignore(H=[]),
                 ignore(F=[]),
                 ignore(L=[]),
                 \+ L=[], % don't include if empty.
                 enrich_graph_fragment([],H,F,Database,Result)
             ),
             Databases,
             Enriched).
*/

/*
 * run_query(JSON_In, JSON_Out) is det.
 *
 * Runs a WOQL query in JSON-LD WOQL syntax
 * with pre-specified context.
 */
run_query(JSON_In, JSON_Out) :-
    empty_ctx(CCTX),
    run_query(JSON_In,CCTX,JSON_Out).

:- use_module(library(http/http_log)).


/*
 * run_query(JSON_In, CCTX, JSON_Out) is det.
 *
 * Runs a WOQL query in JSON-LD WOQL syntax
 * with pre-specified context, with compile
 * context CCTX.
 */
run_query(JSON_In,CCTX,JSON_Out) :-
    woql_context(Ctx),
    memberchk(database=Database,CCTX),
    database_name(Database, Name),
    get_collection_jsonld_context(Name,Ctx_Database),
    merge_dictionaries(Ctx,Ctx_Database,Ctx_Total),
    http_log('Ctx: ~q~n',[Ctx]),
    json_woql(JSON_In, Ctx_Total, Query),
    * http_log('Query: ~q~n',[Query]),
    run_term(Query,CCTX,JSON_Out).

run_term(Query,JSON) :-
    empty_ctx(Ctx),
    run_term(Query,Ctx,JSON).

run_term(Query,Ctx_In,JSON) :-
    debug(terminus(woql_compile(run_term)), 'Query: ~q',[Query]),
    compile_query(Query,Prog,Ctx_In,Ctx_Out),
    debug(terminus(woql_compile(run_term)), 'Program: ~q',[Prog]),
    debug(terminus(woql_compile(run_term)), 'Ctx: ~q',[Ctx_Out]),
    elt(definitions=Definitions,Ctx_Out),
    elt(database=_Database,Ctx_Out),
    debug(terminus(woql_compile(run_term)),'We are here -1',[]),
    assert_program(Definitions),
    debug(terminus(woql_compile(run_term)),'We are here -0.5',[]),
    findall((B-OGs),
            (   elt(output_graphs=OGs,Ctx_Out),
                % sets head to graph start and tail to the empty list.
                bookend_graphs(OGs),
                catch(
                    call(Prog),
                    error(instantiation_error, C),
                    % We need to find the offending unbound culprit here
                    woql_compile:report_instantiation_error(Prog,C,Ctx_Out)
                ),
                elt(bindings=B,Ctx_Out)
            ),
            BGs),

    zip(Bindings,Database_List_List,BGs),

    merge_graphs(Database_List_List,Database_List),

    maplist([B0,B1]>>patch_bindings(B0,B1),Bindings,Patched_Bindings),
    % maplist({Database}/[OGs,Gs]>>enrich_graphs(OGs,Database,Gs),Database_List,E_Databases),
    Database_List= E_Databases,

    debug(terminus(woql_compile(run_term)), 'We are here 1',[]),

    term_jsonld([bindings=Patched_Bindings,graphs=E_Databases],JSON),
    ignore(retract_program(Definitions)).

get_varname(Var,[X=Y|_Rest],Name) :-
    Y == Var,
    !,
    Name = X.
get_varname(Var,[_|Rest],Name) :-
    get_varname(Var,Rest,Name).

guess_varnames([],[]).
guess_varnames([X=Y|Rest],[X|Names]) :-
    var(Y),
    !,
    guess_varnames(Rest,Names).
guess_varnames([_|Rest],Names) :-
    guess_varnames(Rest,Names).

report_instantiation_error(_Prog,context(Pred,Var),Ctx) :-
    memberchk(bindings=B,Ctx),
    get_varname(Var,B,Name),
    !,
    format(string(MSG), "The variable: ~q is unbound while being proceed in the AST operator ~q, but must be instantiated", [Name,Pred]),
    throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                          'terminus:message' : MSG}))).
report_instantiation_error(_Prog,context(Pred,_),Ctx) :-
    memberchk(bindings=B,Ctx),
    guess_varnames(B,Names),
    format(string(MSG), "The variables: ~q are unbound, one of which was a problem while being proceed in the AST operator ~q, which but must be instantiated", [Names,Pred]),
    throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                          'terminus:message' : MSG}))).

/*
run_term(Query,Ctx_In,JSON) :-
    compile_query(Query,Prog,Ctx_In,Ctx_Out),
    elt(definitions=Definitions,Ctx_Out),
    elt(database=_Database,Ctx_Out),
    assert_program(Definitions),
    findall((B,Gs),
            (   elt(output_graphs=OGs,Ctx_Out),
                % sets head to graph start and tail to the empty list.
                bookend_graphs(OGs),
                call(Prog),
                elt(bindings=B1,Ctx_Out),
                patch_bindings(B1,B),
                % enrich_graphs(OGs,Database,Gs)
                OGs = Gs
            ),
            BGs),

    zip(Bindings,Databases,BGs),

    term_jsonld([bindings=Bindings,graphs=Databases],JSON),
    ignore(retract_program(Definitions)).
*/

literal_string(literal(type(_,Val)), Val).
literal_string(literal(lang(_,Val)), Val).

not_literal(X) :-
    nonvar(X),
    X = literal(_),
    !,
    false.
not_literal(_).

expand(A/B,U) -->
    !,
    view(prefixes=NS),
    {
        elt(A=URI,NS),
        atom_concat(URI,B,U)
    }.
expand(C,C) --> [], {atom(C)}.

patch_binding(X,Y) :-
    (   var(X)
    ->  Y=unknown
    ;   (   \+ \+ (X = literal(type(A,B)),
                   (var(A) ; var(B)))
        ->  Y = unknown
        ;   X = Y)
    ;   X=Y).

patch_bindings([],[]).
patch_bindings([V=X|B0],[V=Y|B1]) :-
    patch_binding(X,Y),
    patch_bindings(B0,B1).

/* For now, no class expressions.

We should wrap URIs so this is deterministic without cut.

*/
compile_node(X:C,XE,Goals) -->
    !,
    resolve(X,XE),
    expand(C,CE),
    view(database=G),
    {
        database_instance(G,I),
        Goals = [xrdf(G,I,XE,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',D),
                 once(schema:subsumption_of(D,CE,G))
                ]
    }.
compile_node(X,XE,[]) -->
    %\+ X = _:_,
    resolve(X,XE).

compile_node_or_lit(PE,X:C,XE,XGoals) -->
    !,
    view(database=G),
    (   { datatype_property(PE,G) }
    ->  resolve(X,XE),
        { XGoals=[] }
    ;   { object_property(PE,G) }
    ->  compile_node(X:C,XE,XGoals)
    ;   { format(atom(M), 'Unknown property ~q in graph ~q~n', [PE,G]),
          throw(syntax_error(M)) }
    ).
compile_node_or_lit(_,X,XE,[]) -->
    %\+ X = _:_,
    resolve(X,XE).

/* currently the same as compile node */
compile_relation(X:_C,XE,Class,Goals) -->
    resolve(X,XE),
    %expand(C,CE),
    resolve(Class,ClassE),
    view(database=G),
    {
        (   X=ignore
        ->  Goals=[]
        ;   database_instance(G,I),
            Goals = [xrdf(G,I,XE,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',D),
                     once(schema:subsumption_of(D,ClassE,G))
                    ]
        )
    }.

as_vars([],[]).
as_vars([as(_X,Y)|Rest],[Y|Vars]) :-
    as_vars(Rest,Vars).
as_vars([as(_X,Y,_T)|Rest],[Y|Vars]) :-
    as_vars(Rest,Vars).

position_vars([],[]).
position_vars([v(V)|Rest],[v(V)|Vars]) :-
    position_vars(Rest,Vars).

/* indexing_list(Spec,Header,Values,Bindings,Result) is det.
 *
 * A fold over Spec into Result
 */
indexing_as_list([],_,_,_,[]).
indexing_as_list([As_Clause|Rest],Header,Values,Bindings,[Term|Result]) :-
    (   As_Clause = as(N,v(V))
    ->  Type = none
    ;   As_Clause = as(N,v(V),Type)),
    member(V=Xe,Bindings),
    Term = (   nth1(Idx,Header,N)
           ->  (   nth1(Idx,Values,Value)
               ->  (   Type = none
                   ->  Value = Xe
                   ;   typecast(Value,Type,[],Xe))
               ;   format(string(Msg),"Too few values in get: ~q with header: ~q and values: ~q giving index: ~q creating prolog: ~q",[N,Header,Values,Idx, nth1(Idx,Values,Value)]),
                   throw(error(syntax_error(Msg)))
               )
           ;   format(string(Msg),"No such indexed name in get: ~q with header: ~q and values: ~q giving",[N,Header,Values]),
               throw(error(syntax_error(Msg)))
           ),
    indexing_as_list(Rest,Header,Values,Bindings,Result).

indexing_position_list([],_,_,[]).
indexing_position_list([v(V)|Rest],N,Values,Bindings,[Term|Result]) :-
    member(V=Xe,Bindings),
    Term = (   nth0(N,Values,Xe)
           ->  true
           ;   format(string(Msg),"No such index in get: ~q for values: ~q",[N,Values]),
               throw(error(syntax_error(Msg)))
           ),
    M is N+1,
    indexing_position_list(Rest,M,Values,Bindings,Result).

indexing_term(Spec,Header,Values,Bindings,Indexing_Term) :-
    (   indexing_as_list(Spec,Header,Values,Bindings,Indexing_List)
    ;   indexing_position_list(Spec,0,Values,Bindings,Indexing_List),
        Header=false),
    list_conjunction(Indexing_List,Indexing_Term).

/*
 * woql_equal(AE,BE) is det.
 */
woql_equal(AE,BE) :-
    nonvar(AE),
    nonvar(BE),
    % Probably strictly should check subsumption
    AE = literal(type(_T1,Y)),
    BE = literal(type(_T2,Y)),
    !.
woql_equal(AE,BE) :-
    AE=BE.

/*
 * woql_less(AE,BE) is det.
 *
 * TODO: May need other cases.
 */
woql_less(literal(type('http://www.w3.org/2001/XMLSchema#dateTime',X)),
          literal(type('http://www.w3.org/2001/XMLSchema#dateTime',Y))) :-
    !,
    X @< Y.
woql_less(literal(type(T1,X)),
          literal(type(T2,Y))) :-
    basetype_subsumption_of(T1,'http://www.w3.org/2001/XMLSchema#decimal'),
    basetype_subsumption_of(T2,'http://www.w3.org/2001/XMLSchema#decimal'),
    !,
    X < Y.
woql_less(AE,BE) :-
    % dodgy - should switch on type
    compare((<),AE,BE).

/*
 * woql_greater(AE,BE) is det.
 *
 * TODO: May need other cases.
 */
woql_greater(literal(type('http://www.w3.org/2001/XMLSchema#dateTime',X)),
             literal(type('http://www.w3.org/2001/XMLSchema#dateTime',Y))) :-
    !,
    X @> Y.
woql_greater(literal(type(T1,X)),
             literal(type(T2,Y))) :-
    basetype_subsumption_of(T1,'http://www.w3.org/2001/XMLSchema#decimal'),
    basetype_subsumption_of(T2,'http://www.w3.org/2001/XMLSchema#decimal'),
    !,
    X > Y.
woql_greater(AE,BE) :-
    % dodgy - should switch on type
    compare((>),AE,BE).

/*
 * term_literal(Value, Value_Cast) is det.
 *
 * Casts a bare object from prolog to a typed object
 */
term_literal(Term, Term) :-
    var(Term),
    !.
term_literal(Term, literal(type('http://www.w3.org/2001/XMLSchema#string', String))) :-
    atom(Term),
    !,
    atom_string(Term,String).
term_literal(Term, literal(type('http://www.w3.org/2001/XMLSchema#string', Term))) :-
    string(Term),
    !.
term_literal(Term, literal(type('http://www.w3.org/2001/XMLSchema#decimal', Term))) :-
    number(Term).


/*
 * csv_term(Path,Has_Header,Header,Indexing,Prog,Options) is det.
 *
 * Create a program term Prog for a csv with Header and column reference strategy
 * Indexing.
 */
csv_term(Path,true,Header,Values,Indexing_Term,Prog,Options) :-
    Prog = (
        % header row only
        csv_read_file_row(Path, Header_Row, [line(1)|Options]),
        Header_Row =.. [_|Header]
    ->  csv_read_file_row(Path, Value_Row, [line(Line)|Options]),
        Line > 1,
        Value_Row =.. [_|Pre_Values],
        maplist(term_literal,Pre_Values,Values),
        Indexing_Term
    ),
    !.
csv_term(Path,false,_,Values,Indexing_Term,Prog,Options) :-
    Prog = (
        csv_read_file_row(Path, Value_Row, Options),
        Value_Row =.. [_|Pre_Values],
        maplist(term_literal,Pre_Values,Values),
        Indexing_Term
    ),
    !.
csv_term(Path,Has_Header,Header,Values,Indexing_Term,Prog,Options) :-
    format(atom(M),'Unknown csv processing options for "get" processing: ~q~n',
           [csv_term(Path,Has_Header,Header,Values,Indexing_Term,Prog,Options)]),
    throw(error(M)).

json_term(Path,Header,Values,Indexing_Term,Prog,_New_Options) :-
    setup_call_cleanup(
        open(Path,read,In),
        json_read_dict(In,Dict,[]),
        close(In)
    ),
    get_dict(columns,Dict,Pre_Header),
    maplist([Str,Atom]>>atom_string(Atom,Str),Pre_Header,Header),
    get_dict(data,Dict,Rows),
    Prog = (
        member(Row,Rows),
        maplist(term_literal,Row,Values),
        Indexing_Term
    ).


/*
 * bool_convert(+Bool_Id,-Bool) is det.
 * bool_convert(-Bool_Id,+Bool) is nondet.
 *
 * Converts a boolean representation from json.
 */
bool_convert(true,true).
bool_convert("true",true).
bool_convert(1,true).
bool_convert("false",false).
bool_convert(false,false).
bool_convert(0,false).

/*
 * convert_csv_options(+Options, -CSV_Options) is det.
 *
 * We need the various parsing options etc. to be implemented here
 * by converting from URI terms to proper CSV library terms.
 */
convert_csv_options(Options,CSV_Options) :-
    (   memberchk('http://terminusdb.com/woql#separator'(A),Options)
    ->  atom_codes(A,[C]),
        CSV_Options1 = [separator(C)]
    ;   CSV_Options1 = []),

    (   memberchk('http://terminusdb.com/woql#convert'(Bool_Str),Options)
    ->  bool_convert(Bool_Str,Bool),
        CSV_Options2 = [convert(Bool)]
    ;   CSV_Options2 = CSV_Options1),

    CSV_Options = CSV_Options2.

/*
 * turtle_term(Path,Values,Prog,Options) is det.
 *
 * Create a program term Prog for a csv with Header and column reference strategy
 * Indexing.
 */
turtle_term(Path,Vars,Prog,Options) :-
    Prog = (turtle:rdf_read_turtle(Path, Triples, [encoding(utf8)|Options]),
            member(Triple,Triples),
            literals:normalise_triple(Triple, rdf(X,P,Y)),
            Vars = [X,P,Y]).

compile_wf(update_object(Doc),frame:update_object(Doc,Database)) -->
    view(database=Database).
compile_wf(update_object(X,Doc),frame:update_object(URI,Doc,Database)) -->
    view(database=Database),
    resolve(X,URI).
compile_wf(delete_object(X),frame:delete_object(URI,Database)) -->
    view(database=Database),
    resolve(X,URI).
compile_wf(delete(WG,X,P,Y),delete(DB,WG,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(database=DB).
compile_wf(insert(WG,X,P,Y),insert(DB,WG,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(database=DB).
compile_wf(delete(X,P,Y),delete(DB,WG,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(database=DB),
    view(write_graph=[WG]).
compile_wf(insert(X,P,Y),insert(DB,WG,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(database=DB),
    view(write_graph=[WG]).
compile_wf(X:C,Goal) -->
    compile_node(X:C,_,Goals),
    { list_conjunction(Goals,Goal) }.
compile_wf(A=B,woql_equal(AE,BE)) -->
    resolve(A,AE),
    resolve(B,BE).
compile_wf(A<B,woql_less(AE,BE)) -->
    resolve(A,AE),
    resolve(B,BE).
compile_wf(A>B,woql_greater(AE,BE)) -->
    resolve(A,AE),
    resolve(B,BE).
compile_wf(like(A,B,F), Goal) -->
    resolve(A,AE),
    resolve(B,BE),
    {
        Goal = (freeze(AE,
                       freeze(BE,
                              (   literal_string(AE,AS),
                                  literal_string(BE,BS),
                                  isub(AS, BS, true, F)))))
    }.
compile_wf(A << B,schema:subsumption_of(AE,BE,G)) -->
    resolve(A,AE),
    resolve(B,BE),
    view(database=G).
compile_wf(opt(P), ignore(Goal)) -->
    compile_wf(P,Goal).
compile_wf(let(P,Args,Def,Query),Goal) -->
    % Get the compilation module
    view(module=M),

    % get previous definitions
    view(definitions=Old_Definitions),

    % *** compile the predicate definition ***
    empty_ctx,
    % Bind formal parameters
    mapm(resolve,Args,Rargs),

    % Get the initial output graph and Tail variable.
    view(current_output_graph=G),
    view(output_graphs=OGS0),
    { elt(G=g(OG,_-T,FH-FT),OGS0) },

    % Get upper bound, and update the Used var.
    update(bound=_,
           bound=Bound),
    update(used=Used,
           used=New_Used),

    % *** compile the body ***
    compile_wf(Def,Body),

    % Get the tail of the final output graph
    view(output_graphs=OGS1),
    { elt(G=g(_,HN-_,_),OGS1) },

    % *** compile the goal. ****
    empty_ctx,
    % set fuel consumption at zero
    update(used=_,
           used=0),
    compile_wf(Query,Goal),
    update(definitions=_,
           definitions=[Predicate|Old_Definitions]),
    {
        % *** Construct the program ***
        % M: Module name,
        % P: predicate_name,
        % Used: amount of fuel used
        % Bound: Max fuel
        % OG: the entire graph so far (including unbound tail)
        % H-T: Difference list of graph
        % FH-FT: Difference list of fringe
        % Rargs: Actual predicate arguments
        Predicate_Head =.. [P, Used, Bound, OG, Head-Tail, Fringe-Fringe_Tail | Rargs],
        Pre_Logic = (New_Used is Used + 1,
                     (   New_Used > Bound
                     % Push the head and tail into fringe
                     ->  Fringe-Fringe_Tail = T-HN,
                         % Set the incoming tail to head,
                         % i.e. we're doing nothing for it.
                         Head=Tail
                     % HN is the next head - i.e. our tail
                     ;   Head-Tail = T-HN,
                         Fringe-Fringe_Tail=FH-FT
                     )),
        % Meta logical test feels very procedural...
        Post_Logic = (   var(FH) % Empty fringe means Tail is the head.
                     ->  FH=FT
                     ;   true),
        Predicate = (M:Predicate_Head :- Pre_Logic, Body, Post_Logic)
    }.
compile_wf(p(P,Args),Goal) -->
    mapm(resolve, Args, RArgs),
    view(used=Used),
    view(bound=Bound),
    view(module=M),
    view(current_output_graph=G),
    update(output_graphs=Gs0,
           output_graphs=Gs1),
    {
        select(G=g(OG,_-T0,FH-FT),Gs0,
               G=g(OG,T0-T1,FH-FT),Gs1),
        debug(terminus(woql_compile(compile_wf)), 'Predicate: ~q-~q',[T0,T1]),

        Pred =.. [P,Used,Bound,OG,T0-T1,FH-FT|RArgs],
        Goal = (   Used > Bound
               ->  false
               ;   M:Pred)
    }.
compile_wf(t(X,P,Y),Goal) -->
    compile_node(X,XE,XGoals),
    resolve(P,PE),
    compile_node_or_lit(PE,Y,YE,YGoals),
    view(database=G),
    %view(current_output_graph=OG),
    %update(output_graphs=OGS1,
    %        output_graphs=OGS2),
    {
        Search=inference:inferredEdge(XE,PE,YE,G),
        %select(OG=g(Full_G,_-T0,FH-FT),OGS1,
        %       OG=g(Full_G,T0-T1,FH-FT),OGS2),
        append([[not_literal(XE),not_literal(PE),Search],XGoals,YGoals],
               GoalList),
        list_conjunction(GoalList,Goal)
    }.
compile_wf(t(X,P,Y,G),Goal) -->
    compile_node(X,XE,XGoals),
    resolve(P,PE),
    compile_node_or_lit(PE,Y,YE,YGoals),
    resolve(G,GE),
    view(database=Database),
    %view(current_output_graph=OG),
    %update(output_graphs=OGS1,
    %        output_graphs=OGS2),
    {
        %select(OG=g(Full_G,_-T0,FH-FT),OGS1,
        %       OG=g(Full_G,T0-T1,FH-FT),OGS2),
        (   database_instance(Database,L),
            member(GE,L)
        ->  Search=inference:inferredEdge(XE,PE,YE,Database)
        ;   Search=xrdf(Database,[GE],XE,PE,YE)),

        append([[not_literal(XE),not_literal(PE),Search],XGoals,YGoals],
               GoalList),
        list_conjunction(GoalList,Goal)
    }.
compile_wf(r(X,R,Y),Goal) -->
    compile_node(X,XE,XGoals),
    compile_relation(R,RE,RClass,RGoals),
    expand(RClass,RClassID),
    compile_node(Y,YE,YGoals),
    view(database=G),
    view(current_output_graph=OG),
    update(output_graphs=OGS1,
           output_graphs=OGS2),
    {
        database_instance(G,I),
        database_name(G,C),

        select(OG=g(Full_G,_-T0,FH-FT),OGS1,
               OG=g(Full_G,T0-T1,FH-FT),OGS2),

        (   relationship_source_property(RClassID,P,G)
        ->  true
        ;   format(atom(M), 'No relationship source property ~q in ~q', [RClassID,G]),
            throw(error(M))),

        (   relationship_target_property(RClassID,Q,G)
        ->  true
        ;   format(atom(M), 'No relationship target property ~q in ~q', [RClassID,G]),
            throw(error(M))),

        append([[xrdf(G,I,RE,P,XE),
                 xrdf(G,I,RE,Q,YE),
                 is_new(triple(C,I,XE,RE,YE,'==>'),Full_G),
                 T0=[triple(C,I,XE,RE,YE,'==>')|T1]
                ],XGoals,RGoals,YGoals],
               GoalList),

        list_conjunction(GoalList,Goal)
    }.
compile_wf(r(X,R,Y,G),Goal) -->
    compile_node(X,XE,XGoals),
    compile_relation(R,RE,RClass,RGoals),
    expand(RClass,RClassID),
    compile_node(Y,YE,YGoals),
    %view(database=G),
    update(output_graphs=OGS1,
           output_graphs=OGS2),
    view(current_output_graph=OG),
    {
        make_database_from_database_name(G,Database),
        database_name(Database,C),
        database_instance(Database,I),

        select(OG=g(Full_G,_-T0,FH-FT),OGS1,
               OG=g(Full_G,T0-T1,FH-FT),OGS2),

        relationship_source_property(RClassID,P,Database),
        relationship_target_property(RClassID,Q,Database),

        debug(terminus(woql_compile(compile_wf)), 'Relation: ~q-~q',[T0,T1]),

        append([[xrdf(C,I,RE,P,XE),
                 xrdf(C,I,RE,Q,YE,I),
                 is_new(triple(C,I,XE,RE,YE,'==>'),Full_G),
                 T0=[triple(C,I,XE,RE,YE,'==>')|T1]
                ],XGoals,RGoals,YGoals],
               GoalList),
        list_conjunction(GoalList,Goal)
    }.
compile_wf((A;B),(ProgA;ProgB)) -->
    peak(S0),
    compile_wf(A,ProgA),
    peak(S1),
    return(S0),
    compile_wf(B,ProgB),
    peak(S2),
    {
        debug(terminus(woql_compile(compile_wf)), 'Before disjunctions(0)', []),
        debug(terminus(woql_compile(compile_wf)), 'Program: ~q',[(ProgA;ProgB)]),
        %elt(current_output_graph=G,S2),
        %elt(output_graphs=OGs,S2),
        %elt(G=g(_,T0-T1,_),OGs),
        % debug(terminus(woql_compile(compile_wf)), 'After disjunctions (2): ~q-~q',[T0,T1]),
        %format('***************~nProgram: ~n~q~n',[(ProgA;ProgB)]),
        merge(S1,S2,S3)
        %format('***************~nAfter merge: ~n~q-~q~n',[T0,T1]),
        %format('***************~nProgram: ~n~q~n',[(ProgA;ProgB)])
    },
    return(S3).
compile_wf((A,B),(ProgA,ProgB)) -->
    compile_wf(A,ProgA),
    compile_wf(B,ProgB),
    {
        debug(terminus(woql_compile(compile_wf)), 'Conjunctive Program: ~q',[(ProgA,ProgB)])
    }.
compile_wf((A => B),Goal) -->
    view(database=Database),
    compile_wf(A,ProgA),
    update(database=Database,
           database=UpdateDB),
    % This second one should be simpler, to reflect that only writes are allowed on the right.
    compile_wf(B,ProgB),
    % This definitely needs to be a collection of all actual graphs written to...
    % should be easy to extract from B
    view(write_graph=[WG]),
    {
        debug(terminus(woql_compile(compile_wf)), 'Database: ~q', [Database]),
        active_graphs(B,Active_Graphs),
        get_dict(schema,Database,Schemata),
        debug(terminus(woql_compile(compile_wf)), 'Schemata: ~q', [Schemata]),
        get_dict(write,Active_Graphs,AWGs),
        debug(terminus(woql_compile(compile_wf)), 'AWGs: ~q', [AWGs]),
        union([WG],AWGs,WGs),
        debug(terminus(woql_compile(compile_wf)), 'Write Graphs: ~q', [WGs]),
        intersection(WGs,Schemata,Changed_Schemata),
        debug(terminus(woql_compile(compile_wf)), 'Write Schemata: ~q', [Changed_Schemata]),
        % We want to choose schema free transactions whenever possible.
        (   Changed_Schemata = []
        ->  Goal = (
                validate:instance_transaction(
                             Database,
                             UpdateDB,
                             WGs,
                             woql_compile:(
                                 forall(ProgA,
                                        ProgB)
                             ),
                             Witnesses),
                (   Witnesses = []
                ->  true
                ;   throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                                          'terminus:witnesses' : Witnesses})))
                )
            )
        ;   Goal = (
                validate:instance_schema_transaction(
                             Database,
                             UpdateDB,
                             WGs,
                             woql_compile:(
                                 forall(ProgA,
                                        ProgB)
                             ),
                             Witnesses),
                (   Witnesses = []
                ->  true
                ;   throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                                          'terminus:witnesses' : Witnesses})))
                )
            )
        )
    },
    update(database=_,
           database=Database).
compile_wf(select(VL,P), Prog) -->
    compile_wf(P, Prog),
    restrict(VL).
compile_wf(all(P), Prog) -->
    % This needs to merge only bindings and graphs
    % and using tail append
    % bindings probably also need to be difference lists
    view(bindings=Old_Bindings),
    debug_wf('Got here'),
    compile_wf(P, Single),
    debug_wf('Got here 0.5'),
    view(current_output_graph=G),
    debug_wf('Got here 1'),
    update(output_graphs=OGs0,
           output_graphs=OGs1),
    debug_wf('Got here 2'),
    update(bindings=_,
           bindings=Old_Bindings),
    debug_wf('Got here 3'),
    {
        % These are the variables which occur in
        % our goal... OG, OH...
        select(G=g(OG,OH-OT,OFH-OFT),OGs0,
               G=New_G,OGs1),
        debug(terminus(woql_compile(compile_wf)), 'Program: ~q',[Single]),
        debug(terminus(woql_compile(compile_wf)), 'Program: ~q',[OFT]),
        Prog=once(% one aggregate is enough
                 sfoldr(
                     % fold predicate
                     [g(G0,H0-T0,FH0-FT0),
                      g(_,H1-T1,FH1-FT1),
                      g(G2,H2-T2,FH2-FT2)]>>(
                         debug(terminus(woql_compile(compile_wf)), 'incoming: ~q~n',[FH0-FT0]),
                         debug(terminus(woql_compile(compile_wf)), 'update: ~q~n',[FH1-FT1]),
                         G0=G2,
                         H0=H2,   T0=H1, T2=T1,
                         FH0=FH2, FT0=FH1, FT2=FT1
                     ),
                     % generator
                     [g(OG,OH-OT,OFH-OFT)]>>(OG=OH,Single),
                     g(Head,Head-Head,Fringe-Fringe),
                     New_G
                 ))
    }.
compile_wf(from(G,P),Goal) -->
    resolve(G,GName),
    { make_database_from_database_name(GName,Database) },
    update(database=Old_Database,
           database=Database),
    compile_wf(P, Goal),
    update(database=_,
           database=Old_Database).
compile_wf(depth(N,P), Prog) -->
    update(bound=D,
           bound=N),
    compile_wf(P, Prog),
    update(bound=_,
           bound=D).
compile_wf(prefixes(NS,S), Prog) -->
    debug_wf('DO YOU HEAR ME ~q', [NS]),
    update(prefixes=NS_Old,
           prefixes=NS_New),
    { append(NS, NS_Old, NS_New) },
    compile_wf(S, Prog),
    update(prefixes=_,
           prefixes=NS_Old).
compile_wf(with(GN,GS,Q), (Program, Sub_Query)) -->
    resolve(GN,GName),
    update(database=Old_Database,
           database=Database),
    view(files=Files),
    % TODO: Extend with optiosn for various file types.
    { file_spec_path_options(GS, Files, Path, _{}, Options),
      extend_database_with_temp_graph(GName,Path,Options,Program,Old_Database,Database)
    },
    compile_wf(Q,Sub_Query),
    update(database=_,
           database=Old_Database).
compile_wf(get(Spec,File_Spec), Prog) -->
    {
        Default = _{
                      'http://terminusdb.com/woql#header' : "true",
                      'http://terminusdb.com/woql#type' : "csv"},

        (   as_vars(Spec,Vars),
            Has_Header = true
        ;   position_vars(Spec,Vars),
            Has_Header = false
        )
    },

    % Make sure all variables are given bindings
    mapm(resolve,Vars,BVars),
    view(bindings=Bindings),
    view(files=Files),
    {
        file_spec_path_options(File_Spec, Files, Path, Default, New_Options),
        convert_csv_options(New_Options,CSV_Options),

        (   memberchk('http://terminusdb.com/woql#type'("csv"),New_Options)
        ->  indexing_term(Spec,Header,Values,Bindings,Indexing_Term),
            csv_term(Path,Has_Header,Header,Values,Indexing_Term,Prog,New_Options)
        ;   memberchk('http://terminusdb.com/woql#type'("turtle"),New_Options),
            Has_Header = false
        ->  turtle_term(Path,BVars,Prog,CSV_Options)
        ;   memberchk('http://terminusdb.com/woql#type'("panda_json"),New_Options)
        ->  indexing_term(Spec,Header,Values,Bindings,Indexing_Term),
            json_term(Path,Header,Values,Indexing_Term,Prog,New_Options)
        ;   format(atom(M), 'Unknown file type for "get" processing: ~q', [File_Spec]),
            throw(error(M)))
    }.
compile_wf(put(Spec,Query,File_Spec), Prog) -->
    {
        maplist([Name as Var,Var,Name]>>(true), Spec, Vars, Names)
    },
    % Make sure all variables are bound
    mapm(resolve,Vars,VarsE),
    compile_wf(Query,Compiled_Query),
    {

        (   File_Spec = file(CSV_Path,Options)
        ;   File_Spec = file(CSV_Path),
            Options = []),

        Header_Row =.. [row|Names],

        Prog = setup_call_cleanup(
                   open(CSV_Path, write, Out),
                   (
                       csv_write_stream(Out,[Header_Row], Options),
                       forall(
                           (
                               Compiled_Query,
                               maplist([Value,Data]>>(
                                           (   Value=literal(lang(_,Data))
                                           ->  true
                                           ;   Value=literal(type(_,Data))
                                           ->  true
                                           ;   Data=Value)
                                       ),
                                       VarsE, Row_Data),
                               Row_Term =.. [row|Row_Data]
                           ),
                           csv_write_stream(Out,[Row_Term],Options)
                       )
                   ),
                   close(Out)
               )
    }.
compile_wf(where(P), Prog) -->
    compile_wf(P, Prog).
compile_wf(typecast(Val,Type,_Hints,Cast),
           (typecast(ValE, TypeE, [], CastE))) -->
    resolve(Val,ValE),
    resolve(Type,TypeE),
    resolve(Cast,CastE).
compile_wf(hash(Base,Args,Id),hash(BaseE,ArgsE,IdE)) -->
    resolve(Base, BaseE),
    mapm(resolve,Args,ArgsE),
    resolve(Id,IdE).
compile_wf(idgen(Base,Args,Id),(literal_list(ArgsE,ArgsL),idgen(BaseE,ArgsL,IdE))) -->
    resolve(Base, BaseE),
    mapm(resolve,Args,ArgsE),
    resolve(Id,IdE).
compile_wf(start(N,S),offset(N,Prog)) -->
    compile_wf(S, Prog).
compile_wf(limit(N,S),limit(N,Prog)) -->
    compile_wf(S, Prog).
compile_wf(asc(X),asc(XE)) -->
    resolve(X,XE).
compile_wf(order_by(L,S),order_by(LSpec,Prog)) -->
    mapm(compile_wf, L, LSpec),
    compile_wf(S, Prog).
compile_wf(into(G,S),Goal) -->
    % swap in new graph
    resolve(G,GE),
    update(write_graph=OG,
           write_graph=[GE]),
    compile_wf(S,Goal),
    % swap old graph back in
    update(write_graph=_,
           write_graph=OG).
compile_wf(limit(N,S),limit(N,Prog)) -->
    compile_wf(S, Prog).
compile_wf(not(P),not(Q)) -->
    compile_wf(P, Q).
compile_wf(concat(L,A),(literal_list(LE,LL),
                        utils:interpolate_string(LL,AE_raw),
                        AE = literal(type('http://www.w3.org/2001/XMLSchema#string',AE_raw)))) -->
    resolve(L,LE),
    resolve(A,AE).
compile_wf(trim(S,A),(literally(SE,SL),
                      atom_string(SL,SS),
                      trim(SS,X),
                      AE = literal(type('http://www.w3.org/2001/XMLSchema#string',X)))) -->
    resolve(S,SE),
    resolve(A,AE).
compile_wf(pad(S,C,N,V),(literally(SE,SL),
                         literally(CE,CL),
                         literally(NE,NL),
                         pad(SL,CL,NL,VE_raw),
                         VE = literal(type('http://www.w3.org/2001/XMLSchema#string',VE_raw)))) -->
    resolve(S,SE),
    resolve(C,CE),
    resolve(N,NE),
    resolve(V,VE).
compile_wf(sub_string(S,B,L,A,Sub),(literally(SE,SL),
                                    literally(BE,BL),
                                    literally(LE,LL),
                                    literally(AE,AL),
                                    literally(SubE,SubL),
                                    sub_string(SL,BL,LL,AL,SubL),
                                    unliterally(SL,SE),
                                    unliterally(BL,BE),
                                    unliterally(LL,LE),
                                    unliterally(AL,AE),
                                    unliterally(SubL,SubE)
                                   )) -->
    resolve(S,SE),
    resolve(B,BE),
    resolve(L,LE),
    resolve(A,AE),
    resolve(Sub,SubE).
compile_wf(re(P,S,L),(literally(PE,PL),
                      literally(SE,SL),
                      literal_list(LE,LL),
                      utils:re(PL,SL,LL),
                      unliterally(PL,PE),
                      unliterally(SL,SE),
                      unliterally_list(LL,LE)
                     )) -->
    resolve(P,PE),
    resolve(S,SE),
    resolve(L,LE).
compile_wf(split(S,P,L),(literally(SE,SL),
                         literally(PE,PL),
                         literal_list(LE,LL),
                         utils:pattern_string_split(PL,SL,LL),
                         unliterally(SL,SE),
                         unliterally(PL,PE),
                         unliterally_list(LL,LE)
                        )) -->
    resolve(S,SE),
    resolve(P,PE),
    resolve(L,LE).
compile_wf(upper(S,A),(literally(SE,SL),string_upper(SL,AE))) -->
    resolve(S,SE),
    resolve(A,AE).
compile_wf(lower(S,A),(literally(SE,SL),string_lower(SL,AE))) -->
    resolve(S,SE),
    resolve(A,AE).
compile_wf(format(X,A,L),format(atom(XE),A,LE)) -->
    resolve(X,XE),
    mapm(resolve,L,LE).
compile_wf(X is Arith, (Pre_Term,
                        XA is ArithE,
                        XE = literal(type('http://www.w3.org/2001/XMLSchema#decimal',XA)))) -->
    resolve(X,XE),
    compile_arith(Arith,Pre_Term,ArithE).
compile_wf(group_by(WGroup,WTemplate,WQuery,WAcc),group_by(Group,Template,Query,Acc)) -->
    resolve(WGroup,Group),
    resolve(WTemplate,Template),
    compile_wf(WQuery, Query),
    resolve(WAcc,Acc).
compile_wf(length(L,N),(length(LE,Num),
                        NE = literal(type('http://www.w3.org/2001/XMLSchema#decimal', Num)))) -->
    resolve(L,LE),
    resolve(N,NE).
compile_wf(member(X,Y),member(XE,YE)) -->
    resolve(X,XE),
    resolve(Y,YE).
compile_wf(join(X,S,Y),(literal_list(XE,XL),
                        literally(SE,SL),
                        literally(YE,YL),
                        utils:join(XL,SL,YE),
                        unliterally_list(XL,XE),
                        unliterally(SL,SE),
                        unliterally(YL,YE))) -->
    resolve(X,XE),
    resolve(S,SE),
    resolve(Y,YE).
compile_wf(sum(X,Y),(literal_list(XE,XL),
                     literally(YE,YL),
                     sumlist(XL,YL),
                     unliterally_list(XL,XE),
                     unliterally(YL,YE))) -->
    resolve(X,XE),
    resolve(Y,YE).
compile_wf(true,true) -->
    [].
compile_wf(Q,_) -->
    {
        format(atom(M), 'Unable to compile AST query ~q', [Q]),
        throw(syntax_error(M))
    }.

debug_wf(Lit) -->
    { debug(terminus(woql_compile(compile_wf)), '~w', [Lit]) },
    [].

debug_wf(Fmt, Args) -->
    { debug(terminus(woql_compile(compile_wf)), Fmt, Args) },
    [].


/*
 * file_spec_path_options(File_Spec,Path,Default, Options) is semidet.
 *
 * Converts a file spec into a referenceable file path which can be opened as a stream.
 */
file_spec_path_options(File_Spec,_Files,Path,Default,New_Options) :-
    (   File_Spec = file(Path,Options)
    ;   File_Spec = file(Path),
        Options = []),
    merge_options(Options,Default,New_Options).
file_spec_path_options(File_Spec,_Files,Path,Default,New_Options) :-
    (   File_Spec = remote(URI,Options)
    ;   File_Spec = remote(URI),
        Options = []),
    merge_options(Options,Default,New_Options),
    copy_remote(URI,URI,Path,New_Options).
file_spec_path_options(File_Spec,Files,Path,Default,New_Options) :-
    (   File_Spec = post(Name,Options)
    ;   File_Spec = post(Name),
        Options = []),
    atom_string(Name_Atom,Name),
    merge_options(Options,Default,New_Options),
    memberchk(Name_Atom=file(_Original,Path), Files).

literal_list(X, _X) :-
    var(X),
    !.
literal_list([],[]).
literal_list([H|T],[HL|TL]) :-
    literally(H,HL),
    literal_list(T,TL).

literally(X, _X) :-
    var(X),
    !.
literally(literal(T), L) :-
    % this adds choice points - is this avoidable?
    var(T),
    !,
    (   T = lang(_,L)
    ;   T = type(_,L)).
literally(literal(type(_,L)), L) :-
    !.
literally(literal(lang(_,L)), L) :-
    !.
literally(X^^_T, X) :-
    !.
literally(X@_L, X) :-
    !.
literally(X, X) :-
    (   atom(X)
    ->  true
    ;   string(X)
    ->  true
    ;   number(X)
    ).

unliterally_list([],[]).
unliterally_list([H|T],[HL|TL]) :-
    unliterally(H,HL),
    unliterally_list(T,TL).

unliterally(X,Y) :-
    var(Y),
    !,
    Y = literal(type('http://www.w3.org/2001/XMLSchema#string',X)).
unliterally(X,Y) :-
    string(X),
    !,
    (   (   Y = literal(type(Type,X))
        ;   Y = X^^Type),
        (   var(Type)
        ->  Type = 'http://www.w3.org/2001/XMLSchema#string'
        ;   % subsumption test here.
            true)
    ;   (   Y = literal(lang(Lang,X))
        ;   Y = X@Lang),
        (   var(Lang)
        ->  Lang = en
        ;   true)
    ).
unliterally(X,Y) :-
    atom(X),
    atom(Y),
    !,
    X = Y.
unliterally(X,Y) :-
    number(X),
    !,
    (   (   Y = literal(type(Type,X))
        ;   Y = X^^Type),
        (   var(Type)
        ->  Type = 'http://www.w3.org/2001/XMLSchema#decimal'
        ;   % subsumption test here.
            true)
    ;   (   Y = literal(lang(Lang,X))
        ;   Y = X@Lang),
        (   var(Lang)
        ->  Lang = en
        ;   true)
    ).



compile_arith(Exp,Pre_Term,ExpE) -->
    {
        Exp =.. [Functor|Args],
        % lazily snarf everything named...
        % probably need to add stuff here.
        member(Functor, ['*','-','+','div','/','floor', '**'])
    },
    !,
    mapm(compile_arith,Args,Pre_Terms,ArgsE),
    {
        ExpE =.. [Functor|ArgsE],
        list_conjunction(Pre_Terms,Pre_Term)
    }.
compile_arith(Exp,literally(ExpE,ExpL),ExpL) -->
    resolve(Exp,ExpE).

restrict(VL) -->
    update(bindings=B0,
           bindings=B1),
    {
        include({VL}/[X=_]>>member(v(X),VL), B0, B1)
    }.

% Could be a single fold, but then we always get a conjunction with true
list_conjunction([],true).
list_conjunction(L,Goal) :-
    L = [_|_],
    reverse(L,R),
    R = [A|Rest],
    foldl([X,Y,(X,Y)]>>true, Rest, A, Goal).

/*
 * active_graphs(Term,Dict:dict) is det.
 *
 * What graphs are currently active in a given query.
 *
 * NOTE: This can be used by capabilities assessment to determine what
 * capabilities are necessary.
 *
 * Dict has the form:
 * _{read : [Graphs], write : [Graphs]}
 */
active_graphs(Term, Dict) :-
    % lazy
    Term =.. [Functor|Args],
    (   Functor = insert,
        Args = [G,_,_,_]
    ->  Dict = _{read:[],write:[G]}
    ;   Functor = delete,
        Args = [G,_,_,_]
    ->  Dict = _{read:[],write:[G]}
    ;   Functor = update_object,
        Args = [_,_]
    ->  Dict = _{read:[],write:[]}
    ;   Functor = update_object,
        Args = [_]
    ->  Dict = _{read:[],write:[]}
    ;   Functor = t,
        Args = [_,_,_,G]
    ->  Dict = _{read:[G],write:[]}
    ;   Functor = r,
        Args = [_,_,_,G]
    ->  Dict = _{read:[G],write:[]}
    ;   Functor = from,
        Args = [G,P]
    ->  active_graphs(P,G_Sub),
        Dict = _{read : [G|G_Sub.get(read) ],
                 write : G_Sub.get(write)}
    ;   Functor = into,
        Args = [G,P]
    ->  active_graphs(P,G_Sub),
        Dict = _{read : G_Sub.get(read),
                 write : [G|G_Sub.get(write)]}
    ;   Functor = '/'
    ->  Dict = _{read:[],write:[]}
    ;   maplist(active_graphs,Args,Results),
        merge_active_graphs(Results,Dict)
    ).

merge_active_graphs_aux([],D,D).
merge_active_graphs_aux([D1|Rest],D2,D) :-
    R1 = D1.get(read),
    W1 = D1.get(write),
    R2 = D2.get(read),
    W2 = D2.get(write),
    append(R1,R2,RI),
    sort(RI,R3),
    append(W1,W2,WI),
    sort(WI,W3),
    D3 = _{read:R3,write:W3},
    merge_active_graphs_aux(Rest,D3,D).

merge_active_graphs(Results,Dictionary) :-
    merge_active_graphs_aux(Results,_{read:[],write:[]},Dictionary).
