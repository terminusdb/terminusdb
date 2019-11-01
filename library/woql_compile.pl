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
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
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

:- use_module(library(database)).
:- use_module(library(woql_term)).
:- use_module(library(utils), except([elt/2])).
:- use_module(library(triplestore), [
                  xrdf/5,
                  insert/5,
                  delete/5
              ]).
:- use_module(library(schema)).
:- use_module(library(relationships), [
                  relationship_source_property/3,
                  relationship_target_property/3
              ]).
:- use_module(library(inference)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(solution_sequences)).

:- use_module(library(jsonld)).
:- use_module(library(json_woql)).

:- use_module(library(frame), [
                  update_object/3,
                  delete_object/2
              ]).
 
% We may need to patch this in again...
%:- use_module(query, [enrich_graph_fragment/5]).

:- use_module(library(validate_schema), [datatypeProperty/2, objectProperty/2]).
:- use_module(library(casting), [typecast/4,hash/3]).

% This should really not be used... it is too low level - Gavin
%:- use_module(journaling, [write_triple/5]).

:- use_module(library(remote_file), [
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
          database=_,
           used=_Used,
           bound=3,
           module=M,
           bindings=[]]) :-
    gensym(module_,M).

empty_ctx(S0,S6) :-
    empty_ctx(S),
    elt(prefixes=Prefixes,S0),
    elt(database=Database,S0),
    elt(write_graph=Write_Graph,S0),
    elt(module=M,S0),
    elt(definitions=D,S0),
    elt(collection=C,S0),
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
           write_graph=Write_Graph,S6).

empty_ctx(Prefixes,S0,S6) :-
    empty_ctx(S),
    elt(database=Database,S0),
    elt(write_graph=Write_Graph,S0),
    elt(module=M,S0),
    elt(definitions=D,S0),
    elt(collection=C,S0),
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
resolve(X,literal(type('http://www.w3.org/2001/XMLSchema#integer',X))) -->
    [],
    {
        integer(X),
        !
    }.
resolve(X,literal(type('http://www.w3.org/2001/XMLSchema#decimal',X))) -->
    [],
    {
        number(X),
        !
    }.
resolve(X,X) -->
    [],
    {
        string(X),
        !
    }.
resolve(X@L,literal(lang(LE,XS))) -->
    resolve(X,XE),
    {
        (   ground(XE),
            atom(XE)
        ->  atom_string(XE,XS)
        ;   XE = XS)
    },
    resolve(L,LE).
resolve(X^^T,literal(type(TE,XS))) -->
    resolve(X,XE),
    {
        (   ground(XE),
            atom(XE)
        ->  atom_string(XE,XS)
        ;   XE = XS)
    },
    resolve(T,TE).

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
    abolish(woql_compile:Name/N),
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
 * Runs a WOQL query in JSON-LD WOQL syntax.
 */ 
run_query(JSON_In, JSON_Out) :-
    woql_context(Ctx),
    json_woql(JSON_In, Ctx, Query),
    run_term(Query,JSON_Out).

:- use_module(library(http/http_log)).

run_term(Query,JSON) :-
    /*
    (   \+ ground(Query)
    ->  format(atom(M), 'Arguments are insufficiently instantiated for Query ~q', [Query]),
        throw(instantiation_error(M))
    ;   true),
    */
    * format('~n***********~nQuery: ~q~n',[Query]),
    compile_query(Query,Prog,Ctx_Out),
    * format('~n***********~nProgram: ~q~n',[Prog]),
    * format('~n***********~nCtx: ~q~n',[Ctx_Out]),
    elt(definitions=Definitions,Ctx_Out),
    elt(database=_Database,Ctx_Out),

    * http_log_stream(Log),
    * format(Log,'~n~nWe are here -1 ~n~n',[]),
    
    assert_program(Definitions),

    * format(Log,'~n~nWe are here -0.5 ~n~n',[]),

    findall((B-OGs),
            (   elt(output_graphs=OGs,Ctx_Out),
                % sets head to graph start and tail to the empty list.
                bookend_graphs(OGs),
                call(Prog),
                elt(bindings=B,Ctx_Out)
            ),
            BGs),

    zip(Bindings,Database_List_List,BGs),

    merge_graphs(Database_List_List,Database_List),

    maplist([B0,B1]>>patch_bindings(B0,B1),Bindings,Patched_Bindings),
    % maplist({Database}/[OGs,Gs]>>enrich_graphs(OGs,Database,Gs),Database_List,E_Databases),
    Database_List= E_Databases,
    
    * format(Log,'~n~nWe are here 1 ~n~n',[]),
    
    term_jsonld([bindings=Patched_Bindings,graphs=E_Databases],JSON),
    ignore(retract_program(Definitions)).
 
run_term(Query,JSON) :-
    compile_query(Query,Prog,Ctx_Out),
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
    view(database=G),
    {
        database_instance(G,I),
        Goals = [xrdf(G,I,XE,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',D),
                 once(schema:subsumptionOf(D,CE,G))
                ]
    }.
compile_node(X,XE,[]) -->
    %\+ X = _:_,
    resolve(X,XE).

compile_node_or_lit(PE,X:C,XE,XGoals) -->
    !,
    view(database=G),
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
                     once(schema:subsumptionOf(D,ClassE,G))
                    ]
        )
    }.

compile_wf(update_object(Doc),frame:update_object(Doc,Database)) -->
    view(database=Database).
compile_wf(update_object(X,Doc),frame:update_object(URI,Doc,Database)) -->
    view(database=Database),
    resolve(X,URI).
compile_wf(delete_object(X),frame:delete_object(URI,Database)) -->
    view(database=Database),
    resolve(X,URI).
compile_wf(delete([WG],X,P,Y),delete(DB,WG,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(database=DB).
compile_wf(insert([WG],X,P,Y),insert(DB,WG,XE,PE,YE)) -->
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
compile_wf(A << B,schema:subsumptionOf(AE,BE,G)) -->
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
    view(database=G),
    %view(current_output_graph=OG),
    %update(output_graphs=OGS1,
    %        output_graphs=OGS2),
    {
        Search=inference:inferredEdge(XE,PE,YE,G),
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
        
        append([[Search],XGoals,YGoals],
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
        Goal = (
            validate:document_transaction(
                         Database,
                         UpdateDB,
                         WG,
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

        (   (   File_Spec = file(CSV_Path,Options)
            ;   File_Spec = file(CSV_Path),
                Options = []),
            dict_options(_DictOptions,Options)
        ;   (   File_Spec = http(URI,Options)
            ;   File_Spec = http(URI),
                Options = []),
            dict_options(DictOptions,Options),
            copy_remote(URI,URI,CSV_Path,DictOptions)
        ),

        % Index each named column in the spec
        foldl({Names,Values,B}/
              [N as v(V),R,[Term|R]]>>(
                  member(V=Xe,B),
                  Term = 
                      (   nth1(Idx,Names,N),
                          nth1(Idx,Values,Xe)
                      ->  true
                      ;   format(atom(Msg),'No such indexed name in get: ~q with header: ~q and values: ~q',[N,Names,Values]),
                          throw(http_error(syntax_error(Msg)))
                      )
              ), Spec, [], Indexing_List),
    
        list_conjunction(Indexing_List,Indexing),

        % This should know about non-header
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
compile_wf(order_by(L,S),order_by(LE,Prog)) -->
    mapm(resolve,L,LE),
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
compile_wf(concat(L,A),atomic_list_concat(LLE,AE)) -->
    mapm(resolve,L,LE),
    {
        maplist(literally,LE, LLE)
    },
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
compile_wf(group_by(WGroup,WTemplate,WQuery,WAcc),group_by(Group,Template,Query,Acc)) -->
    resolve(WGroup,Group),
    mapm(resolve,WTemplate,Template),
    compile_wf(WQuery, Query),
    resolve(WAcc,Acc).
compile_wf(member(X,Y),member(XE,YE)) -->
    mapm(resolve,X,XE),
    resolve(Y,YE).
compile_wf(true,true) -->
    [].

literally(literal(type(_,L)), L).
literally(literal(lang(_,L)), L).
literally(X, X) :-
    (   var(X)
    ;   atom(X)
    ;   string(X)).

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
        include({VL}/[X=_]>>member(v(X),VL), B0, B1)
    }.

% Could be a single fold, but then we always get a conjunction with true
list_conjunction([],true).
list_conjunction(L,Goal) :-
    L = [_|_],
    reverse(L,R),
    R = [A|Rest],
    foldl([X,Y,(X,Y)]>>true, Rest, A, Goal).
