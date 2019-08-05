:- module(woql_compile,[
              compile_query/3,
              compile_query/4,
              run_query/2,
              is_new/2,
              empty_ctx/1,
              empty_ctx/2
          ]).

/** <module> WOQL Compile 
 * 
 * Core compiler for the WOQL query language. 
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                      *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(library(collection)).
:- use_module(library(woql_term)).
:- use_module(library(utils), except([elt/2])).
:- use_module(library(triplestore), [xrdf/5,with_output_graph/2,sync_from_journals/2]).
:- use_module(library(schema), [subsumptionOf/3]).
:- use_module(library(relationships), [
                  relationship_source_property/3,
                  relationship_target_property/3
              ]).
:- use_module(library(inference)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(solution_sequences)).

% We may need to patch this in again...
%:- use_module(query, [enrich_graph_fragment/5]).

:- use_module(validate_schema, [datatypeProperty/2, objectProperty/2]).
:- use_module(casting, [typecast/4,hash/3]).

% This should really not be used... it is too low level - Gavin
%:- use_module(journaling, [write_triple/5]).

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

merge_graph(Graph_List0,Graph_List1,Graph_List2) :-
    % in both
    convlist({Graph_List1}/[Name=graph(T0,G0,F0),
                            Name=graph(T2,G2,F2)]>>(
                member(Name=graph(T1,G1,F1),Graph_List1),
                append(T0,T1,T2),
                append(G0,G1,G2),
                append(F0,F1,F2)),
             Graph_List0,Graph_List_Same),
    % in first
    convlist({Graph_List1}/[Name=graph(T0,G0,F0),
                            Name=graph(T0,G0,F0)]>>(
                 \+ member(Name=_,Graph_List1)),
             Graph_List0,Graph_List_Left),
    % in second
    convlist({Graph_List0}/[Name=graph(T0,G0,F0),
                            Name=graph(T0,G0,F0)]>>(
                 \+ member(Name=_,Graph_List0)),
             Graph_List1,Graph_List_Right),
    append([Graph_List_Same,
            Graph_List_Left,
            Graph_List_Right],
           Graph_List2).
        

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
    member(graph=Graph,S0),
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
        graph=Graph,
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
         
/* (already imported)
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
           graph=_,
           used=_Used,
           bound=3,
           module=M,
           bindings=[]]) :-
    gensym(module_,M).

empty_ctx(S0,S6) :-
    empty_ctx(S),
    elt(prefixes=Prefixes,S0),
    elt(graph=Graph,S0),
    elt(graph=Write_Graph,S0),
    elt(module=M,S0),
    elt(definitions=D,S0),
    elt(collection=C,S0),
    select(prefixes=_,S,
           prefixes=Prefixes,S1),
    select(graph=_,S1,
           graph=Graph,S2),
    select(module=_,S2,
           module=M,S3),
    select(definitions=_,S3,
           definitions=D,S4),
    select(collection=_,S4,
           collection=C,S5),
    select(write_graph=_,S5,
           write_graph=Write_Graph,S6).

empty_ctx(Prefixes,S0,S6) :-
    empty_ctx(S),
    elt(graph=Graph,S0),
    elt(write_graph=Write_Graph,S0),
    elt(module=M,S0),
    elt(definitions=D,S0),
    elt(collection=C,S0),
    select(prefixes=_,S,
           prefixes=Prefixes,S1),
    select(graph=_,S1,
           graph=Graph,S2),
    select(module=_,S2,
           module=M,S3),
    select(definitions=_,S3,
           definitions=D,S4),
    select(collection=_,S4,
           collection=C,S5),
    select(write_graph=_,S5,
           write_graph=Write_Graph,S6).

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
        (   once(member(X=URI,Prefixes))
        ->  Xe=URI
        ;   X=Xe)
    }.
resolve(X,literal(type('http://www.w3.org/2001/XMLSchema#integer',X))) -->
    [],
    {
        integer(X)
    }.
resolve(X,literal(type('http://www.w3.org/2001/XMLSchema#decimal',X))) -->
    [],
    {
        number(X)
    }.
resolve(X,literal(type('http://www.w3.org/2001/XMLSchema#string',Y))) -->
    [],
    {
        string(X),
        string_to_atom(Y,X)
    }.
resolve(X@L,literal(lang(LE,XE))) -->
    resolve(X,XE),
    resolve(L,LE).
resolve(X^^T,literal(type(TE,XE))) -->
    resolve(X,XE),
    resolve(T,TE).

/* 
 * compile_query(+Term:any,-Prog:any,-Ctx_Out:context) is det.
 */
compile_query(Term, Prog, Ctx_Out) :-
    empty_ctx(Ctx_In),
    compile_query(Term,Prog,Ctx_In,Ctx_Out).

compile_query(Term, Prog, Ctx_In, Ctx_Out) :-
    (   wf(Term)
    ->  (   compile_wf(Term, Prog, Ctx_In, Ctx_Out)
        ->  true
        ;   format(atom(M), 'Failure to compile term ~q', [Term]),
            throw(compilation_error(M)))        
    ;   throw(syntax_error(Term))
    ).

assert_program([]).
assert_program([Def|Remainder]) :-
    assert(Def),
    assert_program(Remainder).

retract_program([]).
retract_program([def(Name,Args,_Body)|Remainder]) :-
    length(Args,N),
    abolish(woql_compile:Name/N),
    retract_program(Remainder).

bookend_graphs(Graphs) :-
    maplist([_=g(L,L-[],_-[])]>>true, Graphs).

nonground_elts([A|Rest_In],[A|Rest_Out]) :-
    \+ ground(A),
    !,
    nonground_elts(Rest_In,Rest_Out).
nonground_elts([_|Rest_In],Rest_Out) :-
    nonground_elts(Rest_In,Rest_Out).
nonground_elts([],[]).

/* 
 * enrich_graphs(Graphs,Graph,Enriched) is det.
 * 
 * DDD enrich_graph_fragment currently unimplemented...
 */
enrich_graphs(Graphs,Graph,Enriched) :-
    convlist({Graph}/[G=g(L,H-[],F-[]),
                      G=Result]>>(
                 % just set it to something
                 % if unbound
                 ignore(H=[]),
                 ignore(F=[]),
                 ignore(L=[]),
                 \+ L=[], % don't include if empty.
                 enrich_graph_fragment([],H,F,Graph,Result)
             ),
             Graphs,
             Enriched).

run_query(Atom, JSON) :-
    read_term_from_atom(Atom,Query,[]),
    run_term(Query,JSON).

run_term(Query,JSON) :-
    (   \+ ground(Query)
    ->  format(atom(M), 'Arguments are insufficiently instantiated for Query ~q', [Query]),
        throw(instantiation_error(M))
    ;   true),
    compile_query(Query,Prog,Ctx_Out),
    %format('~n***********~nProgram: ~q~n',[Prog]),
    %format('~n***********~nCtx: ~q~n',[Ctx_Out]),
    elt(definitions=Definitions,Ctx_Out),
    elt(graph=Graph,Ctx_Out),
    assert_program(Definitions),
    findall((B,OGs),
            (   elt(output_graphs=OGs,Ctx_Out),
                % sets head to graph start and tail to the empty list.
                bookend_graphs(OGs),
                call(Prog),
                elt(bindings=B,Ctx_Out)
            ),
            BGs),

    zip(Bindings,Graph_List_List,BGs),
    merge_graphs(Graph_List_List,Graph_List),
    
    maplist([B0,B1]>>patch_bindings(B0,B1),Bindings,Patched_Bindings),
    maplist({Graph}/[OGs,Gs]>>enrich_graphs(OGs,Graph,Gs),Graph_List,E_Graphs),

    jsonify([bindings=Patched_Bindings,graphs=E_Graphs],JSON),
    ignore(retract_program(Definitions)).
 
run_term(Query,JSON) :-
    compile_query(Query,Prog,Ctx_Out),
    %format('~n***********~nProgram: ~q~n',[Prog]),
    %format('~n***********~nCtx: ~q~n',[Ctx_Out]),
    elt(definitions=Definitions,Ctx_Out),
    elt(graph=Graph,Ctx_Out),
    assert_program(Definitions),
    findall((B,Gs),
            (   elt(output_graphs=OGs,Ctx_Out),
                % sets head to graph start and tail to the empty list.
                bookend_graphs(OGs),
                call(Prog),
                elt(bindings=B1,Ctx_Out),
                patch_bindings(B1,B),
                enrich_graphs(OGs,Graph,Gs)
            ),
            BGs),

    zip(Bindings,Graphs,BGs),
    
    jsonify([bindings=Bindings,graphs=Graphs],JSON),
    ignore(retract_program(Definitions)).

literal_string(literal(type(_,Val)), Val).
literal_string(literal(lang(_,Val)), Val).

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
    ;   (   \+ \+ (X = literal(type(X,Y)),
                   (var(X) ; var(Y)))
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
    view(graph=G),
    {
        graph_collection(G,C),
        graph_instance(G,I),
        Goals = [xrdf(C,I,XE,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',D),
                 once(subsumptionOf(D,CE,G))
                ]
    }.
compile_node(X,XE,[]) -->
    %\+ X = _:_,
    resolve(X,XE).

compile_node_or_lit(PE,X:C,XE,XGoals) -->
    !,
    view(graph=G),
    (   { datatypeProperty(PE,G) }
    ->  resolve(X,XE),
        { XGoals=[] }
    ;   { objectProperty(PE,G) }
    ->  compile_node(X:C,XE,XGoals)
    ;   { format(atom(M), 'Unknown property ~q in graph ~q~n', [PE,G]),
          throw(syntax_error(M)) }
    ).
compile_node_or_lit(_,X,XE,[]) -->
    %\+ X = _:_,
    resolve(X,XE).

/* currently the same as compile node */
compile_relation(X:C,XE,Class,Goals) -->
    resolve(X,XE),
    %expand(C,CE),
    resolve(Class,ClassE),
    view(graph=G),
    {
        (   X=ignore
        ->  Goals=[]
        ;   graph_collection(G,C),
            graph_instance(G,I),
            Goals = [xrdf(C,I,XE,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',D),
                     once(subsumptionOf(D,ClassE,G))
                    ]
        )
    }.

compile_wf(update_object(X,Doc),update_object(URI,Doc,Graph)) -->
    view(graph=Graph),
    resolve(X,URI).
compile_wf(delete_object(X),delete_object(URI,Graph)) -->
    view(graph=Graph),
    resolve(X,URI).
compile_wf(delete(WG,X,P,Y),delete(WC,WG,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(collection=WC).
compile_wf(insert(WG,X,P,Y),insert(WC,WG,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(collection=WC).
compile_wf(delete(X,P,Y),delete(WC,WG,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(collection=WC),
    view(write_graph=WG).
compile_wf(insert(X,P,Y),insert(WC,WG,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(collection=WC),
    view(write_graph=WG).
compile_wf(X:C,Goal) -->
    compile_node(X:C,_,Goals),
    { list_conjunction(Goals,Goal) }.
compile_wf(A=B,AE=BE) -->
    resolve(A,AE),
    resolve(B,BE).
compile_wf(A<B,AE<BE) -->
    resolve(A,AE),
    resolve(B,BE).
compile_wf(A>B,AE>BE) -->
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
compile_wf(A << B,subsumptionOf(AE,BE,G)) -->
    resolve(A,AE),
    resolve(B,BE),
    view(graph=G).
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
        %format('***************~nPredicate: ~n~q-~q~n',[T0,T1]),

        Pred =.. [P,Used,Bound,OG,T0-T1,FH-FT|RArgs],
        Goal = (   Used > Bound
               ->  false
               ;   M:Pred)
    }.
compile_wf(t(X,P,Y),Goal) -->
    compile_node(X,XE,XGoals),
    resolve(P,PE),
    compile_node_or_lit(PE,Y,YE,YGoals),
    view(graph=G),
    %view(current_output_graph=OG),
    %update(output_graphs=OGS1,
    %        output_graphs=OGS2),
    {
        graph_collection(G,C),            
        graph_instance(G,I),
        (   I = main
        ->  Search=inferredEdge(XE,PE,YE,G)
        ;   Search=xrdf(C,I,XE,PE,YE)),
        %select(OG=g(Full_G,_-T0,FH-FT),OGS1,
        %       OG=g(Full_G,T0-T1,FH-FT),OGS2),
        append([[Search],XGoals,YGoals],
               GoalList),
        list_conjunction(GoalList,Goal)
    }.
compile_wf(t(X,P,Y,G),Goal) -->
    compile_node(X,XE,XGoals),
    resolve(P,PE),
    compile_node_or_lit(PE,Y,YE,YGoals),
    resolve(G,GE),
    view(graph=Graph),
    %view(current_output_graph=OG),
    %update(output_graphs=OGS1,
    %        output_graphs=OGS2),
    {        
        %select(OG=g(Full_G,_-T0,FH-FT),OGS1,
        %       OG=g(Full_G,T0-T1,FH-FT),OGS2),
        graph_collection(Graph,C),
        (   GE = main
        ->  Search=inferredEdge(XE,PE,YE,Graph)
        ;   Search=xrdf(C,GE,XE,PE,YE)),
        
        append([[Search],XGoals,YGoals],
               GoalList),
        list_conjunction(GoalList,Goal)
    }.
compile_wf(r(X,R,Y),Goal) -->
    compile_node(X,XE,XGoals),
    compile_relation(R,RE,RClass,RGoals),
    expand(RClass,RClassID),    
    compile_node(Y,YE,YGoals),
    view(graph=G),
    view(current_output_graph=OG),
    update(output_graphs=OGS1,
           output_graphs=OGS2),
    {
        graph_collection(G,C),                
        graph_instance(G,I),

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

        append([[xrdf(C,I,RE,P,XE),
                 xrdf(C,I,RE,Q,YE),
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
    %view(graph=G),
    update(output_graphs=OGS1,
           output_graphs=OGS2),
    view(current_output_graph=OG),
    {
        make_collection_graph(G,Graph),
        graph_collection(Graph,C),
        graph_instance(Graph,I),

        select(OG=g(Full_G,_-T0,FH-FT),OGS1,
               OG=g(Full_G,T0-T1,FH-FT),OGS2),

        relationship_source_property(RClassID,P,Graph),
        relationship_target_property(RClassID,Q,Graph),

        %format('***************~nRelation:~n~q-~q~n',[T0,T1]),

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
        %format('***************~nBefore disjunctions(0)~n'),
        %format('***************~nProgram: ~n~q~n',[(ProgA;ProgB)]),
        %elt(current_output_graph=G,S2),
        %elt(output_graphs=OGs,S2),
        %elt(G=g(_,T0-T1,_),OGs),
        %format('***************~nAfter disjunctions (2): ~n~q-~q~n',[T0,T1]),
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
        %format('***************~nConjunctive Program: ~n~q~n',[(ProgA,ProgB)])
    }.
compile_wf((A => B),Goal) -->
    compile_wf(A,ProgA),
    % This second one should be simpler, to reflect that only writes are allowed on the right. 
    compile_wf(B,ProgB),
    view(collection=C),
    % This definitely needs to be a collection of all actual graphs written to...
    % should be easy to extract from B
    view(write_graph=WG),
    {
        %format('***************~nImplicative Program: ~n~q~n',[(ProgA,ProgB)])
        Goal = (
            with_transction(
                [collection(C),graphs([WG])],
                exhaust(
                    (   ProgA,
                        ProgB
                    ))
            )
        )
    }.
compile_wf(select(VL,P), Prog) -->
    compile_wf(P, Prog),
    restrict(VL).
compile_wf(all(P), Prog) -->
    % This needs to merge only bindings and graphs
    % and using tail append
    % bindings probably also need to be difference lists
    view(bindings=Old_Bindings),
    %{ format('******************~nGot here~n') },
    compile_wf(P, Single),
    %{ format('******************~nGot here 0.5~n') },
    view(current_output_graph=G),
    %{ format('******************~nGot here 1~n') },

    update(output_graphs=OGs0,
           output_graphs=OGs1),
    %{ format('******************~nGot here 2~n') },
    
    update(bindings=_,
           bindings=Old_Bindings),
    {   %format('******************~nGot here 3~n'),
        % These are the variables which occur in
        % our goal... OG, OH...
        select(G=g(OG,OH-OT,OFH-OFT),OGs0,
               G=New_G,OGs1),
        %format('******************~nProgram: ~q~n',[Single]),
        %format('******************~nProgram: ~q~n',[OFT]),
        Prog=once(% one aggregate is enough
                 sfoldr(
                     % fold predicate
                     [g(G0,H0-T0,FH0-FT0),
                      g(_,H1-T1,FH1-FT1),
                      g(G2,H2-T2,FH2-FT2)]>>(
                         %format('********incoming: ~q~n',[FH0-FT0]),
                         %format('********update: ~q~n',[FH1-FT1]),
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
    { make_collection_graph(GName,Graph) },
    update(graph=Old_Graph,
           graph=Graph),
    compile_wf(P, Goal),
    update(graph=_,
           graph=Old_Graph).
compile_wf(depth(N,P), Prog) -->
    update(bound=D,
           bound=N),
    compile_wf(P, Prog),
    update(bound=_,
           bound=D).
compile_wf(prefixes(NS,S), Prog) -->
    %{ format('~n~nDO YOU HEAR ME~n~n~q~n', [NS]) },
    update(prefixes=NS_Old,
           prefixes=NS_New),
    { append(NS, NS_Old, NS_New) },
    compile_wf(S, Prog),
    update(prefixes=_,
           prefixes=NS_Old).
compile_wf(get(Spec,File_Spec), Prog) -->
    {
        maplist([_ as Var,Var]>>(true), Spec, Vars)
    },
    % Make sure all variables are bound
    mapm(resolve,Vars,_),
    view(bindings=B),
    {

        (   File_Spec = file(CSV_Path,Options)
        ;   File_Spec = file(CSV_Path),
            Options = []),

        % Index each named column in the spec
        foldl({Names,Values,B}/
              [N as v(V),R,[Term|R]]>>(
                  member(V=Xe,B),
                  Term = 
                      (   nth1(Idx,Names,N),
                          nth1(Idx,Values,Xe)
                      ->  true
                      ;   throw(error('No such indexed name in get'))
                      )
              ), Spec, [], Indexing_List),
    
        list_conjunction(Indexing_List,Indexing),
        
        Prog = (
            % header row only
            csv_read_file_row(CSV_Path, Header_Row, [line(1)|Options]),
            Header_Row =.. [_|Names]
        ->  csv_read_file_row(CSV_Path, Value_Row, [line(Line)|Options]),
            Line > 1,
            Value_Row =.. [_|Values],
            Indexing
        )
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
compile_wf(typecast(Val,Type,_Hints,Cast), Prog) -->
    resolve(Val,ValE),
    resolve(Type,TypeE),
    resolve(Cast,CastE),
    {
        Prog = typecast(ValE, TypeE, [], CastE)
    }.
compile_wf(hash(Base,Args,Id), Prog) -->
    resolve(Base, BaseE),
    mapm(resolve,Args,ArgsE),
    resolve(Id,IdE),
    {
        Prog = hash(BaseE,ArgsE,IdE)
    }.
compile_wf(start(N,S),offset(N,Prog)) -->
    compile_wf(S, Prog).
compile_wf(limit(N,S),limit(N,Prog)) -->
    compile_wf(S, Prog).
compile_wf(into(G,S),Goal) -->
    % swap in new graph
    resolve(G,GE),
    update(write_graph=OG,
           write_graph=GE),
    compile_wf(S,Goal),
    % swap old graph back in
    update(write_graph=_,
           write_graph=OG).
compile_wf(limit(N,S),limit(N,Prog)) -->
    compile_wf(S, Prog).
compile_wf(not(P),not(Q)) -->
    compile_wf(P, Q).
compile_wf(concat(L,A),atomic_list_concat(LE,AE)) -->
    mapm(resolve,L,LE),
    resolve(A,AE).
compile_wf(trim(S,A),(trim(SE,X),atom_string(AE,X))) -->
    resolve(S,SE),
    resolve(A,AE).
compile_wf(format(X,A,L),format(atom(XE,A,LE))) -->
    resolve(X,XE),
    mapm(resolve,L,LE).
compile_wf(X is Arith, XE is ArithE) -->
    resolve(X,XE),
    compile_arith(Arith,ArithE).
compile_wf(true,true) -->
    [].

compile_arith(Exp,ExpE) -->
    {
        Exp =.. [Functor|Args],
        % lazily snarf everything named...
        % probably need to add stuff here.
        member(Functor, ['*','-','div','/'])
    },
    !,
    mapm(compile_arith,Args,ArgsE),
    {
        ExpE =.. [Functor|ArgsE]
    }.
compile_arith(Exp,ExpE) -->
    resolve(Exp,ExpE).
    
restrict(VL) -->
    update(bindings=B0,
           bindings=B1),
    {
        include( {VL}/[X=_]>>(member(v(X),VL)), B0, B1)
    }.

% Could be a single fold, but then we always get a conjunction with true
list_conjunction([],true).
list_conjunction(L,Goal) :-
    reverse(L,R),
    R = [A|Rest],
    foldl( [X,Y,(X,Y)]>>(true), Rest, A, Goal).
