:- module(woql_compile,[
              lookup/3,
              compile_query/3,
              compile_query/4,
              run_query/2,
              run_query/3,
              empty_ctx/1,
              empty_ctx/2,
              active_graphs/2,
              descriptor_ctx/2
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
:- use_module(utils).
:- use_module(triplestore, [
                  xrdf/4,
                  insert/4,
                  delete/4
              ]).
%:- use_module(schema).
:- use_module(inference).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(solution_sequences)).
:- use_module(library(http/http_log)).

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

:- use_module(global_prefixes, [default_prefixes/1]).

:- use_module(resolve_query_resource).

:- use_module(speculative_parse, [guess_date/2]).

:- use_module(descriptor).

% This should really not be used... it is too low level - Gavin
%:- use_module(journaling, [write_triple/5]).

:- use_module(remote_file, [
                  copy_remote/4
              ]).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

% Get op precedence
:- reexport(syntax).

/*
 * Ctx is a context object which is used in WOQL queries to
 * keep track of state.
 *
 *******
 * TODO: This is complicated, punt to later
 *
 * store_id --> store_id{ descriptor : graph_descriptor,
 *                        id : integer }
 * store_ids = list(store_id)
 *
 * woql_var ---> woql_var{ prolog_var : var,
 *                         store_ids : store_ids }
 ******
 *
 * woql_var ---> var % currently snarfing prolog unification
 *
 * var_binding ---> var_binding{ woql_var : woql_var,
 *                               var_name : atom }
 * var_bindings = list(var_binding)
 *
 * query_context ---> query_context{ <default_output_graph : graph_descriptor>,
 *                                   <default_collection : collection_descriptor>,
 *                                   <prefixes : context>,
 *                                   transaction_objects : list(query_object),
 *                                   bindings : list(var_binding),
 *                                   selected : list(var_binding)
 *                                }
 */

/*******
 * Mondic DCG management
 *
 * We use DCG's to simplify tracking the state of the WOQL query compiler.
 */

get(Key,Value,Set) :-
    Value = Set.Key.

/* Monadic selection */
update(Key,C0,C1,S0,S1) :-
    C0 = S0.Key,
    S1 = S0.put(Key, C1).

view(Key,C0,S0,S0) :-
    C0 = S0.Key.

swap(Key,C0,C1,S0,S1) :-
    C0 = S0.Key,
    C1 = S1.Key.

put(Key, C0, S0, S1) :-
    S1 = S0.put(Key, C0).

peek(S0,S0,S0).

return(S0,_,S0).

/*
 * merge(S0,S1,SM) is det.
 *
 * We need to merge multiple states into a signal state for output.
 *
 * we use S0 as the "merge in set"
 */
merge(S0) -->
    {
        S0.get(bindings, B0)
    },

    view(bindings,B1),

    {
        merge_output_bindings(B0,B1,Bindings)
    },

    put(bindings,Bindings).

/*
 * empty_ctx(Ctx) is det.
 *
 * Empty Ctx to start with.
 */
empty_ctx(query_context{
              bindings: [],
              selected: [],
              prefixes: Prefixes
          }) :-
    default_prefixes(Prefixes).

/*
 * prototype_empty_ctx(S0,S1) is det.
 *
 * updates a context, keeping only global info
 */
empty_ctx -->
    view(prefixes,Prefixes),
    view(transaction_objects,Transaction_Objects),
    view(files,Files),

    { empty_ctx(S0)
    },
    return(S0),

    put(prefixes,Prefixes),
    put(transaction_objects,Transaction_Objects),
    put(files,Files).

empty_ctx(Prefixes) -->
    empty_ctx,
    put(prefixes, Prefixes).


descriptor_ctx(Collection_Descriptor,New_Ctx) :-
    descriptor_query(Collection_Descriptor, Query_Object),
    empty_ctx(Ctx),
    New_Ctx = Ctx.put(_{transaction_objects : [Query_Object],
                        current_collection : Collection_Descriptor}).

/******************************
 * Binding management utilities
 ******************************/

/* Lookup a variable by name */
lookup(Var_Name,Prolog_Var,[Record|_B0]) :-
    var_same(Var_Name,Record,Prolog_Var),
    !.
lookup(Var_Name,Prolog_Var,[_Record|B0]) :-
    lookup(Var_Name,Prolog_Var,B0).

lookup_or_extend(Var_Name, Prolog_Var) -->
    update(bindings,B0,B1),
    {
        (   lookup(Var_Name, Prolog_Var, B0)
        ->  B1=B0
        ;   B1=[var_binding{
                    woql_var : Prolog_Var,
                    var_name : Var_Name}
                |B0])
    }.

resolve_prefix(Pre,Suf,URL) -->
    view(prefixes,Prefixes),
    {
        (   Full_Prefix = Prefixes.get(Pre)
        ->  true
        ;   format(atom(M), 'Unresolvable prefix ~q', [Pre:Suf]),
            throw(error(syntax_error,M))),
        atomic_list_concat([Full_Prefix,Suf],URL)
    }.

/*
 * resolve(ID,Resolution, S0, S1) is det.
 *
 * TODO: This needs a good going over. Way too much duplication of effort.
 */
resolve(ignore,_Something) -->
    !,
    [].
resolve(ID:Suf,U) -->
    !,
    resolve_prefix(ID,Suf,U).
resolve(v(Var_Name),Var) -->
    !,
    lookup_or_extend(Var_Name,Var).
resolve(X,Xe) -->
    {
        is_dict(X),
        !,
        expand(X,XEx), % also need to use the prefixes here.
        jsonld_id(XEx,XI)
    },
    resolve(XI,Xe).
resolve(X@L,XS@LE) -->
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
        ;   Lit = XE^^TE),
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
% Questionable! How can this be a number!
resolve(X,X) -->
    {
        throw(error('How did we get here?', X))
    }.


/*
 * compile_representation(S,T,V) is det.
 *
 * Gives the internal representation of some type T from some string S.
 */

compile_representation(String,Type,String^^Type) :-
    var(Type),
    !.
compile_representation(String,'http://www.w3.org/2001/XMLSchema#dateTime',Date) :-
    !,
    guess_date(String,Date).
compile_representation(String,Type,String^^Type).

var_same(Var_Name,
         var_binding{
             woql_var : Prolog_Var,
             var_name : Var_Name},
         Prolog_Var).

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

/*
 * run_query(JSON_In, JSON_Out) is det.
 *
 * Runs a WOQL query in JSON-LD WOQL syntax
 * with default context.
 */
run_query(JSON_In, JSON_Out) :-
    empty_ctx(CCTX),
    run_query(JSON_In,CCTX,JSON_Out).

/*
 * jsonld_to_ast_and_context(+JSON_LD, -AST, -Ctx).
 */
jsonld_to_ast_and_context(JSON_In, AST, CCTX) :-
    empty_ctx(CCTX),
    CCTX.prefixs = Prefixes_Database,
    woql_context(Ctx),
    merge_dictionaries(Ctx,Prefixes_Database,Ctx_Total),
    json_woql(JSON_In, Ctx_Total, AST).

/*
 * run_query(JSON_In, CCTX, JSON_Out) is det.
 *
 * Runs a WOQL query in JSON-LD WOQL syntax
 * with pre-specified context, with compile
 * context CCTX.
 */
run_query(JSON_In,CCTX,JSON_Out) :-
    json_to_ast_and_context(JSON_In,CCTX,AST),
    run_term(AST,CCTX,JSON_Out).

run_term(Query,JSON) :-
    empty_ctx(Ctx),
    run_term(Query,Ctx,JSON).

run_term(Query,Ctx_In,JSON) :-
    debug(terminus(woql_compile(run_term)), 'Query: ~q',[Query]),
    compile_query(Query,Prog,Ctx_In,Ctx_Out),

    % TODO: This should probably be a forall with a write to the output stream
    findall(B,
            (   catch(
                    call(Prog),
                    error(instantiation_error, C),
                    % We need to find the offending unbound culprit here
                    woql_compile:report_instantiation_error(Prog,C,Ctx_Out)
                ),
                B = Ctx_Out.bindings
            ),
            Bindings),

    maplist([B0,B1]>>patch_bindings(B0,B1),Bindings,Patched_Bindings),

    JSON = _{bindings : Patched_Bindings}.

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

literal_string(Val^^_, Val).
literal_string(Val@_, Val).

not_literal(X) :-
    nonvar(X),
    X = _V^^_T,
    !,
    false.
not_literal(X) :-
    nonvar(X),
    X = _V@_T,
    !,
    false.
not_literal(_).

/* TODO: Needs fixed */
patch_binding(X,Y) :-
    (   var(X)
    ->  Y=unknown
    ;   (   \+ \+ (X = B^^A,
                   (var(A) ; var(B)))
        ->  Y = unknown
        ;   X = Y)
    ;   X=Y).

patch_bindings([],[]).
patch_bindings([V=X|B0],[V=Y|B1]) :-
    patch_binding(X,Y),
    patch_bindings(B0,B1).

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
    lookup(V,Xe,Bindings),
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
    lookup(V,Xe,Bindings),
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
    % TODO: Lang!!! Foo@Bar
    AE = Y^^_T1,
    BE = Y^^_T2,
    !.
woql_equal(AE,BE) :-
    AE=BE.

/*
 * woql_less(AE,BE) is det.
 *
 * TODO: May need other cases.
 */
woql_less(X^^'http://www.w3.org/2001/XMLSchema#dateTime',
          Y^^'http://www.w3.org/2001/XMLSchema#dateTime') :-
    !,
    X @< Y.
woql_less(X^^T1,Y^^T2) :-
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
woql_greater(X^^'http://www.w3.org/2001/XMLSchema#dateTime',
             Y^^'http://www.w3.org/2001/XMLSchema#dateTime') :-
    !,
    X @> Y.
woql_greater(X^^T1,
             Y^^T2) :-
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
term_literal(Term,  String^^'http://www.w3.org/2001/XMLSchema#string') :-
    atom(Term),
    !,
    atom_string(Term,String).
term_literal(Term,  Term^^'http://www.w3.org/2001/XMLSchema#string') :-
    string(Term),
    !.
term_literal(Term,  Term^^'http://www.w3.org/2001/XMLSchema#decimal') :-
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
    view(default_collection,Database).
compile_wf(update_object(X,Doc),frame:update_object(URI,Doc,Database)) -->
    view(default_collection,Database),
    resolve(X,URI).
compile_wf(delete_object(X),frame:delete_object(URI,Database)) -->
    view(default_collection,Database),
    resolve(X,URI).
% TODO: Need to translate the reference WG to a read-write object.
compile_wf(delete(X,P,Y,G),delete(Read_Write_Objects,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(transaction_objects,Transaction_Objects),
    {
        resolve_filter(G,Filter),
        filter_transaction_objects_read_write_objects(Filter, Transaction_Objects, Read_Write_Objects)
    }.
% TODO: Need to translate the reference WG to a read-write object.
compile_wf(insert(X,P,Y,G),insert(Read_Write_Object,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(default_collection,DB),
    view(transaction_objects,Transaction_Objects),
    {
        resolve_filter(G,Filter),
        filter_transaction_objects_read_write_objects(Filter, Transaction_Objects, Read_Write_Objects),
        (   Read_Write_Objects = [Read_Write_Object]
        ->  true
        ;   format(atom(M), 'You must resolve to a single graph to insert. Graph Descriptor: ~q', G),
            throw(syntax_error(M,context(compile_wf//2,insert/4)))
        )
    }.
compile_wf(delete(X,P,Y),delete(Read_Write_Object,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(write_graph,Graph_Descriptor),
    view(transaction_objects, Transaction_Objects),
    {
       graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor, Transaction_Objects, Read_Write_Object)
    }.
compile_wf(insert(X,P,Y),insert(Read_Write_Object,XE,PE,YE)) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(write_graph,Graph_Descriptor),
    view(transaction_objects, Transaction_Objects),
    {
       graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor, Transaction_Objects, Read_Write_Object)
    }.
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
    view(default_collection,G).
compile_wf(opt(P), ignore(Goal)) -->
    compile_wf(P,Goal).
compile_wf(t(X,P,Y),Goal) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(default_collection, Collection_Descriptor),
    view(transaction_objects, Transaction_Objects),
    view(filter, Filter),
    {
        collection_descriptor_transaction_object(Collection_Descriptor,Transaction_Objects,
                                                 Transaction_Object),
        filter_transaction_object_goal(Transaction_Object, Filter, Search_Clause),
        Goal = (not_literal(XE),not_literal(PE),Search_Clause)
    }.
compile_wf(t(X,P,Y,G),Goal) -->
    resolve(X,XE),
    resolve(P,PE),
    resolve(Y,YE),
    view(default_collection, Collection_Descriptor),
    view(transaction_objects,Transaction_Objects),
    {
        collection_descriptor_transaction_object(Collection_Descriptor,Transaction_Objects,
                                                 Transaction_Object),
        resolve_filter(G,Filter),
        filter_transaction_object_goal(Transaction_Object, Filter, Search_Clause),
        Goal = (not_literal(XE),not_literal(PE),Search_Clause)
    }.
compile_wf((A;B),(ProgA;ProgB)) -->
    peek(S0),
    compile_wf(A,ProgA),
    peek(S1),
    return(S0),
    compile_wf(B,ProgB),
    merge(S1). % merges S1 back in to current state.
compile_wf((A,B),(ProgA,ProgB)) -->
    compile_wf(A,ProgA),
    compile_wf(B,ProgB),
    {
        debug(terminus(woql_compile(compile_wf)), 'Conjunctive Program: ~q',[(ProgA,ProgB)])
    }.
compile_wf(when(A,B),forall(ProgA,ProgB)) -->
    compile_wf(A,ProgA),
    compile_wf(B,ProgB).
compile_wf(select(VL,P), Prog) -->
    compile_wf(P, Prog),
    restrict(VL).
compile_wf(using(Collection_URI,P),Goal) -->
    { resolve_query_resource(Collection_URI, Default_Collection) },
    update(default_collection,Old_Default_Collection,Default_Collection),
    compile_wf(P, Goal),
    update(default_collection,_,Old_Default_Collectcion).
compile_wf(from(Filter_String,P),Goal) -->
    { resolve_filter(Filter_String,Filter) },
    update(filter,Old_Default_Filter,Filter),
    compile_wf(P, Goal),
    update(filter,_,Old_Default_Filter).
compile_wf(prefixes(NS,S), Prog) -->
    % Need to convert the datatype of prefixes here.
    debug_wf('DO YOU HEAR ME ~q', [NS]),
    update(prefixes,NS_Old,NS_New),
    { append(NS, NS_Old, NS_New) },
    compile_wf(S, Prog),
    update(prefixes,_,NS_Old).
compile_wf(with(GN,GS,Q), (Program, Sub_Query)) -->
    resolve(GN,GName),
    update(default_collection,Old_Default_Collection,Default_Collection),
    view(files,Files),
    % TODO: Extend with options for various file types.
    { file_spec_path_options(GS, Files, Path, _{}, Options),
      extend_database_with_temp_graph(GName,Path,Options,Program,Old_Default_Collection,Default_Collection)
    },
    compile_wf(Q,Sub_Query),
    update(default_collection,_,Old_Default_Collection).
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
    view(bindings,Bindings),
    view(files,Files),
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
                                           (   Value=Data@_
                                           ->  true
                                           ;   Value=Data^^_
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
compile_wf(random_idgen(Base,Args,Id),(literal_list(ArgsE,ArgsL),
                                       random_idgen(BaseE,ArgsL,IdE))) -->
    resolve(Base, BaseE),
    mapm(resolve,Args,ArgsE),
    resolve(Id,IdE).
compile_wf(idgen(Base,Args,Id),(literal_list(ArgsE,ArgsL),
                                idgen(BaseE,ArgsL,IdE))) -->
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
    % TODO: Resolve G to descriptor
    % swap in new graph
    {
        resolve_filter(G,Filter),
        (   Filter = type_name_filter{ type : Type, name : [Name]}
        ->  true % should do something
        ;   format(atom(M), 'Unresolvable write filter: ~q', [G]),
            throw(syntax_error(M,context(compile_wf//2, into/2)))
        )
    },
    update(write_graph,OG,G),
    compile_wf(S,Goal),
    % swap old graph back in
    update(write_graph,_,OG).
compile_wf(limit(N,S),limit(N,Prog)) -->
    compile_wf(S, Prog).
compile_wf(not(P),not(Q)) -->
    compile_wf(P, Q).
compile_wf(concat(L,A),(literal_list(LE,LL),
                        utils:interpolate_string(LL,AE_raw),
                        AE = AE_raw^^'http://www.w3.org/2001/XMLSchema#string')) -->
    resolve(L,LE),
    resolve(A,AE).
compile_wf(trim(S,A),(literally(SE,SL),
                      atom_string(SL,SS),
                      trim(SS,X),
                      AE = X^^'http://www.w3.org/2001/XMLSchema#string')) -->
    resolve(S,SE),
    resolve(A,AE).
compile_wf(pad(S,C,N,V),(literally(SE,SL),
                         literally(CE,CL),
                         literally(NE,NL),
                         pad(SL,CL,NL,VE_raw),
                         VE = VE_raw^^'http://www.w3.org/2001/XMLSchema#string')) -->
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
    % TODO: You can execute an arbitrary goal!!!!
    resolve(X,XE),
    mapm(resolve,L,LE).
compile_wf(X is Arith, (Pre_Term,
                        XA is ArithE,
                        XE = XA^^'http://www.w3.org/2001/XMLSchema#decimal')) -->
    resolve(X,XE),
    compile_arith(Arith,Pre_Term,ArithE).
compile_wf(group_by(WGroup,WTemplate,WQuery,WAcc),group_by(Group,Template,Query,Acc)) -->
    resolve(WGroup,Group),
    resolve(WTemplate,Template),
    compile_wf(WQuery, Query),
    resolve(WAcc,Acc).
compile_wf(length(L,N),(length(LE,Num),
                        NE =  Num^^'http://www.w3.org/2001/XMLSchema#decimal')) -->
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
compile_wf(timestamp_now(X), get_time(Timestamp)) -->
    resolve(X,XE),
    {
        XE = Timestamp^^(xsd:decimal)
    }.
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
literally(X^^_T, X) :-
    !.
literally(X@_L, X) :-
    !.
literally(X, X) :-
    % How can this happen?
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
    Y = X^^'http://www.w3.org/2001/XMLSchema#string'.
unliterally(X,Y) :-
    string(X),
    !,
    (   Y = X^^Type,
        (   var(Type)
        ->  Type = 'http://www.w3.org/2001/XMLSchema#string'
        ;   % subsumption test here.
            true)
    ->  true
    ;   Y = X@Lang,
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
    (   Y = X^^Type,
        (   var(Type)
        ->  Type = 'http://www.w3.org/2001/XMLSchema#decimal'
        ;   % subsumption test here.
            true)
    ;   Y = X@Lang,
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
    update(bindings,B0,B1),
    {
        include({VL}/[Record]>>lookup(Record.var_name,_,VL), B0, B1)
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
 * _{read : [Graph_Descriptor], write : [Graph_Descriptor]}
 */
active_graphs(Term, Dict) :-
    active_graphs_(Term,Pre_Dict),
    maplist(resolve_graph_resource,Pre_Dict.read, Reads),
    maplist(resolve_graph_resource,Pre_Dict.write, Writes),
    Dict = _{read : Reads, write : Writes}.

active_graphs_(Term, Dict) :-
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
    ->  active_graphs_(P,G_Sub),
        Dict = _{read : [G|G_Sub.get(read) ],
                 write : G_Sub.get(write)}
    ;   Functor = into,
        Args = [G,P]
    ->  active_graphs_(P,G_Sub),
        Dict = _{read : G_Sub.get(read),
                 write : [G|G_Sub.get(write)]}
    ;   Functor = '/'
    ->  Dict = _{read:[],write:[]}
    ;   maplist(active_graphs_,Args,Results),
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

filter_transaction_objects_read_write_objects(type_filter{ types : Types}, Transaction_Object, Read_Write_Objects) :-
    (   memberchk(instance,Types)
    ->  Instance_Objects = Transaction_Object.instance_objects
    ;   Instance_Objects = []),
    (   memberchk(schema,Types)
    ->  Schema_Objects = Transaction_Object.schema_objects
    ;   Schema_Objects = []),
    (   memberchk(inference,Types)
    ->  Inference_Objects = Transaction_Object.inference_objects
    ;   Inference_Objects = []),
    append([Instance_Objects,Schema_Objects,Inference_Objects],Read_Write_Objects).
filter_transaction_objects_read_write_objects(type_name_filter{ type : Type, names : Names}, Transaction_Object, Read_Write_Objects) :-
    (   Type = instance
    ->  Objs = Transaction_Objects.instance_objects
    ;   Type = schema
    ->  Objs = Transaction_Objects.schema_objects
    ;   Type = inference
    ->  Objs = Transaction_Objects.inference_objects),
    include([Obj]>>(memberchk(Obj.name,Names)), Objs, Read_Write_Objects).

filter_transaction_object_goal(type_filter{ types : Types }, Transaction_Object, Goal) :-
    (   memberchk(instance,Types)
    ->  Search_1 = [inference:inferredEdge(XE,PE,YE,Transaction_Object)]
    ;   Search_1 = []),
    (   memberchk(schema,Types)
    ->  Search_2 = [xrdf(Transaction_Object.schema_objects, XE, PE, YE)]
    ;   Search_2 = []),
    (   memberchk(inference,Types)
    ->  Search_3 = [xrdf(Transaction_Object.inference_objects, XE, PE, YE)]
    ;   Search_3 = []),
    append([Search_1,Search_2,Search_3], Searches),
    list_conjunction(Searches,Goal).
filter_transaction_object_goal(type_name_filter{ type : instance , names : Names}, Transaction_Object, Goal) :-
    filter_read_write_objects(Transaction_Object.instance_objects, Names, Objects),
    Inference_Object = Transaction_Object.put(instance_objects, Objects),
    Goal = inference:inferredEdge(XE,PE,YE,Inference_Object).
filter_transaction_object_goal(type_name_filter{ type : schema , names : Names}, Transaction_Object, Goal) :-
    filter_read_write_object(Transaction_Object.schema_objects, Names, Objects),
    Goal = xrdf(Objects, XE, PE, YE).
filter_transaction_object_goal(type_name_filter{ type : inference , names : Names}, Transaction_Object, Goal) :-
    filter_read_write_object(Transaction_Object.inference_objects, Names, Objects),
    Goal = xrdf(Objects, XE, PE, YE).
