:- module(path,[
              compile_pattern/4,
              calculate_path_solutions/6
          ]).

:- use_module(woql_compile, [not_literal/1]).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query), [run_context_ast_jsonld_response/5]).

:- use_module(library(lists)).

hop(type_filter{ types : Types}, X, P, Y, Transaction_Object) :-
    memberchk(instance,Types),
    not_literal(X),
    xrdf(Transaction_Object.instance_objects,X,P,Y).
hop(type_filter{ types : Types}, X, P, Y, Transaction_Object) :-
    memberchk(schema,Types),
    not_literal(X),
    xrdf(Transaction_Object.schema_objects, X, P, Y).

calculate_path_solutions(Pattern,XE,YE,Path,Filter,Transaction_Object) :-
    run_pattern(Pattern,XE,YE,Path,Filter,Transaction_Object).

/**
 * in_open_set(+Elt,+Set) is semidet.
 *
 * memberchk for partially bound objects
 */
in_open_set(_Elt,Set) :-
    var(Set),
    !,
    fail.
in_open_set(Elt,[Elt|_]) :-
    !.
in_open_set(Elt,[_|Set]) :-
    in_open_set(Elt,Set).

make_edge(X,P,Y,
          edge{ '@type' : "http://terminusdb.com/schema/woql#Edge",
                'http://terminusdb.com/schema/woql#subject' : X,
                'http://terminusdb.com/schema/woql#predicate' : P,
                'http://terminusdb.com/schema/woql#object' : Y}).

invert_path(p(P), n(P)).
invert_path(n(P), p(P)).
invert_path((P,Q), (QN,PN)) :-
    invert_path(Q,QN),
    invert_path(P,PN).
invert_path(plus(P), plus(Q)) :-
    invert_path(P,Q).
invert_path(star(P), star(Q)) :-
    invert_path(P,Q).
invert_path(times(P, M, N), times(Q, M, N)) :-
    invert_path(P,Q).
invert_path((P;Q), (PN;QN)) :-
    invert_path(Q,QN),
    invert_path(P,PN).

run_pattern(P,X,Y,Path,Filter,Transaction_Object) :-
    ground(Y),
    var(X),
    !,
    invert_path(P, Q),
    run_pattern_(Q,Y,X,Path,Path-[],Filter,Transaction_Object).
run_pattern(P,X,Y,Path,Filter,Transaction_Object) :-
    run_pattern_(P,X,Y,Path,Path-[],Filter,Transaction_Object).

run_pattern_(n(P),X,Y,Open_Set,_Path,_Filter,_Transaction_Object) :-
    make_edge(X,P,Y,Edge),
    in_open_set(Edge,Open_Set),
    !,
    fail.
run_pattern_(n(P),X,Y,_Open_Set,[Edge|Tail]-Tail,Filter,Transaction_Object) :-
    make_edge(X,P,Y,Edge),
    hop(Filter,Y,P,X,Transaction_Object).
run_pattern_(p(P),X,Y,Open_Set,_Path,_Filter,_Transaction_Object) :-
    make_edge(X,P,Y,Edge),
    in_open_set(Edge,Open_Set),
    !,
    fail.
run_pattern_(p(P),X,Y,_Open_Set,[Edge|Tail]-Tail,Filter,Transaction_Object) :-
    make_edge(X,P,Y,Edge),
    hop(Filter,X,P,Y,Transaction_Object).
run_pattern_((P,Q),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object) :-
    run_pattern_(P,X,Z,Open_Set,Path-Path_M,Filter,Transaction_Object),
    run_pattern_(Q,Z,Y,Open_Set,Path_M-Tail,Filter,Transaction_Object).
run_pattern_((P;Q),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object) :-
    (   run_pattern_(P,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object)
    ;   run_pattern_(Q,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object)).
run_pattern_(star(P),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object) :-
    (   X = Y,
        Path = Tail
    ;   run_pattern_n_m_(P,1,-1,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object)).
run_pattern_(plus(P),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object) :-
    run_pattern_n_m_(P,1,-1,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object).
run_pattern_(times(P,N,M),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object) :-
    run_pattern_n_m_(P,N,M,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object).

run_pattern_n_m_(P,1,1,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object) :-
    !,
    run_pattern_(P,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object).
run_pattern_n_m_(P,1,_,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object) :-
    run_pattern_(P,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object).
run_pattern_n_m_(P,N,M,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object) :-
    Np is max(1,N-1),
    Mp is M-1,
    run_pattern_(P,X,Z,Open_Set,Path-Path_IM,Filter,Transaction_Object),
    run_pattern_n_m_(P,Np,Mp,Z,Y,Open_Set,Path_IM-Tail,Filter,Transaction_Object).

/*
 * patterns have the following syntax:
 *
 * P,Q,R := p | n | p(P) | n(P) | P,Q | P;Q | plus(P) | star(P) | times(P,N,M)
 *
 * foo>,<baz,bar>
 */
compile_pattern(n, n(_), _Prefixes, _Transaction_Object).
compile_pattern(p, p(_), _Prefixes, _Transaction_Object).
compile_pattern(n(Pred), n(Pred_Expanded), Prefixes, _Transaction_Object) :-
    prefixed_to_property(Pred,Prefixes,Pred_Expanded).
compile_pattern(p(Pred), p(Pred_Expanded), Prefixes, _Transaction_Object) :-
    prefixed_to_property(Pred,Prefixes,Pred_Expanded).
compile_pattern((X,Y), (XC,YC), Prefixes, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Transaction_Object),
    compile_pattern(Y,YC,Prefixes,Transaction_Object).
compile_pattern((X;Y), (XC;YC), Prefixes, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Transaction_Object),
    compile_pattern(Y,YC,Prefixes,Transaction_Object).
compile_pattern(star(X), star(XC), Prefixes, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Transaction_Object).
compile_pattern(plus(X), plus(XC), Prefixes, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Transaction_Object).
compile_pattern(times(X,N,M), times(XC,N,M), Prefixes, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Transaction_Object).

right_edges(p(P),[P]).
right_edges(plus(P),Ps) :-
    right_edges(P,Ps).
right_edges(star(P),Ps) :-
    right_edges(P,Ps).
right_edges(times(P,_,_),Ps) :-
    right_edges(P,Ps).
right_edges((X,_Y),Ps) :-
    right_edges(X,Ps).
right_edges((X;Y),Rs) :-
    right_edges(X,Ps),
    right_edges(Y,Qs),
    append(Ps,Qs,Rs).

left_edges(p(P),[P]).
left_edges(star(P),Ps) :-
    left_edges(P,Ps).
left_edges(plus(P),Ps) :-
    left_edges(P,Ps).
left_edges(times(P,_,_),Ps) :-
    left_edges(P,Ps).
left_edges((_X,Y),Ps) :-
    left_edges(Y,Ps).
left_edges((X;Y),Rs) :-
    left_edges(X,Ps),
    left_edges(Y,Qs),
    append(Ps,Qs,Rs).

:- begin_tests(path).

:- use_module(core(util/test_utils)).
:- use_module(ask).
:- use_module(resolve_query_resource).

test(n_m, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing"},

    AST = (insert(a,b,c),
           insert(c,d,e),
           insert(e,f,g),
           insert(g,h,a)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    AST2 = path(a, times((p(b);p(d);p(f);p(h)),1,3), v(x), v(p)),

    create_context(Descriptor,Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings,Result,Bindings),
    length(Bindings, 3).

test(n_m_loop, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing"},

    AST = (insert(a,b,c),
           insert(c,d,e),
           insert(e,f,g),
           insert(g,h,a)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    AST2 = path(a, times((p(b);p(d);p(f);p(h)),1,5), v(x), v(p)),

    create_context(Descriptor,Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings,Result,Bindings),

    % test that we aren't going in circles
    length(Bindings, 4).

test(n_m_loop_reverse, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing"},

    AST = (insert(a,b,c),
           insert(c,d,e),
           insert(e,f,g),
           insert(g,h,a)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    AST2 = path(v(x), times((p(b);p(d);p(f);p(h)),1,5), a, v(p)),

    create_context(Descriptor,Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings,Result,Bindings),

    % test that we aren't going in circles
    length(Bindings, 4).

test(star_follows_reverse, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing"},

    AST = (insert(a,b,c),
           insert(c,f,e),
           insert(h,b,e),
           insert(e,f,g)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    AST2 = path(v(x), (p(b),star(p(f))), g, v(p)),

    create_context(Descriptor,Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings,Result,Bindings),

    % test that we aren't going in circles
    length(Bindings, 2).

:- end_tests(path).
