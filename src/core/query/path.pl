:- module(path,[
              compile_pattern/5,
              calculate_path_solutions/5,
              calculate_path_solutions/6,
              schema_subject_id/3,
              instance_subject_id/3,
              schema_predicate_id/3,
              instance_predicate_id/3,
              schema_object_id/3,
              instance_object_id/3
          ]).

:- use_module(woql_compile, [not_literal/1]).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query), [run_context_ast_jsonld_response/5]).
:- use_module(core(transaction), [read_write_obj_reader/2]).
:- use_module(library(terminus_store)).
:- use_module(library(lists)).

hop(type_filter{ types : [instance]}, XI, _XS, PI, _PS, YI, _YS, Transaction_Object) :-
    !,
    xrdf(Transaction_Object.instance_objects, id(XI), id(PI), id(YI)).
hop(type_filter{ types : [schema]}, _XI, XS, _PI, PS, _YI, YS, Transaction_Object) :-
    !,
    xrdf(Transaction_Object.schema_objects, id(XS), id(PS), id(YS)).
hop(type_filter{ types : Types}, XI, _XS, PI, _PS, YI, _YS, Transaction_Object) :-
    memberchk(instance,Types),
    xrdf(Transaction_Object.instance_objects, id(XI), id(PI), id(YI)).
hop(type_filter{ types : Types}, _XI, XS, _PI, PS, _YI, YS, Transaction_Object) :-
    memberchk(schema,Types),
    xrdf(Transaction_Object.schema_objects, id(XS), id(PS), id(YS)).

calculate_path_solutions(Pattern,XE,YE,Path,Filter,Transaction_Object) :-
    run_pattern(Pattern,XE,YE,Path,Filter,Transaction_Object, true).

calculate_path_solutions(Pattern,XE,YE,Filter,Transaction_Object) :-
    run_pattern(Pattern,XE,YE,_Path,Filter,Transaction_Object, false).

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

resolve_s(S,SI,SS,Transaction_Object,SR) :-
    (   ground(S)
    ->  SR = S
    ;   ground(SI)
    ->  database_instance(Transaction_Object, [G]),
        read_write_obj_reader(G, Layer),
        subject_id(Layer, SR, SI)
    ;   ground(SS)
    ->  database_instance(Transaction_Object, [G]),
        read_write_obj_reader(G, Layer),
        predicate_id(Layer, SR, SS)
    ;   true
    ).

resolve_p(true,P,PI,PS,Transaction_Pbject,PR) :-
    resolve_p(P,PI,PS,Transaction_Pbject,PR).
resolve_p(false,P,PI,PS,_Transaction_Pbject,PR) :-
    PR = P-PI-PS.

resolve_p(P,PI,PS,Transaction_Object,PR) :-
    (   ground(P)
    ->  PR = P
    ;   ground(PI)
    ->  database_instance(Transaction_Object, [G]),
        read_write_obj_reader(G, Layer),
        predicate_id(Layer, PR, PI)
    ;   ground(PS)
    ->  database_instance(Transaction_Object, [G]),
        read_write_obj_reader(G, Layer),
        predicate_id(Layer, PR, PS)
    ;   true
    ).

resolve_o(true,O,OI,OS,Transaction_Object,OR) :-
    resolve_o(O,OI,OS,Transaction_Object,OR).
resolve_o(false,O,OI,OS,_Transaction_Object,OR) :-
    OR = O-OI-OS.

resolve_o(O,OI,OS,Transaction_Object,OR) :-
    (   ground(O)
    ->  OR = O
    ;   ground(OI)
    ->  database_instance(Transaction_Object, [G]),
        read_write_obj_reader(G, Layer),
        object_id(Layer, Storage, OI),
        storage_object(Storage, OR)
    ;   ground(OS)
    ->  database_instance(Transaction_Object, [G]),
        read_write_obj_reader(G, Layer),
        object_id(Layer, Storage, OS),
        storage_object(Storage, OR)
    ;   true
    ).

invert_path(p(P,PI,PS), n(P,PI,PS)).
invert_path(n(P,PI,PS), p(P,PI,PS)).
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

run_pattern(P,X,Y,Path,Filter,Transaction_Object, Save_Path) :-
    ground(Y),
    var(X),
    !,
    invert_path(P, Q),
    run_pattern(Q,Y,X,Path,Filter,Transaction_Object, Save_Path).
run_pattern(P,X,Y,Path,Filter,Transaction_Object, Save_Path) :-
    filtered_object_ids(X, Filter, Transaction_Object, XI, XS),
    filtered_object_ids(Y, Filter, Transaction_Object, YI, YS),
    run_pattern_(P,node(X,XI,XS),node(Y,YI,YS),Path,Path-[],Filter,Transaction_Object, Save_Path),
    resolve_o(X,XI,XS,Transaction_Object,X),
    resolve_o(Y,YI,YS,Transaction_Object,Y).

run_pattern_(n(P,PI,PS),node(X,XI,XS),node(Y,YI,YS),Open_Set,_Path,_Filter,Transaction_Object,Save_Path) :-
    resolve_o(Save_Path,X,XI,XS,Transaction_Object,XR),
    resolve_p(Save_Path,P,PI,PS,Transaction_Object,PR),
    resolve_o(Save_Path,Y,YI,YS,Transaction_Object,YR),
    make_edge(XR,PR,YR,Edge),
    in_open_set(Edge,Open_Set),
    !,
    fail.
run_pattern_(n(P,PI,PS),node(X,XI,XS),node(Y,YI,YS),_Open_Set,[Edge|Tail]-Tail,Filter,Transaction_Object,Save_Path) :-
    resolve_o(Save_Path,X,XI,XS,Transaction_Object,XR),
    resolve_p(Save_Path,P,PI,PS,Transaction_Object,PR),
    resolve_o(Save_Path,Y,YI,YS,Transaction_Object,YR),
    hop(Filter,YI,YS,PI,PS,XI,XS,Transaction_Object),
    resolve_o(Save_Path,X,XI,XS,Transaction_Object,XR),
    resolve_p(Save_Path,P,PI,PS,Transaction_Object,PR),
    resolve_o(Save_Path,Y,YI,YS,Transaction_Object,YR),
    make_edge(XR,PR,YR,Edge).
run_pattern_(p(P,PI,PS),node(X,XI,XS),node(Y,YI,YS),Open_Set,_Path,_Filter,Transaction_Object,Save_Path) :-
    resolve_o(Save_Path,X,XI,XS,Transaction_Object,XR),
    resolve_p(Save_Path,P,PI,PS,Transaction_Object,PR),
    resolve_o(Save_Path,Y,YI,YS,Transaction_Object,YR),
    make_edge(XR,PR,YR,Edge),
    in_open_set(Edge,Open_Set),
    !,
    fail.
run_pattern_(p(P,PI,PS),node(X,XI,XS),node(Y,YI,YS),_Open_Set,[Edge|Tail]-Tail,Filter,Transaction_Object,Save_Path) :-
    resolve_o(Save_Path,X,XI,XS,Transaction_Object,XR),
    resolve_p(Save_Path,P,PI,PS,Transaction_Object,PR),
    resolve_o(Save_Path,Y,YI,YS,Transaction_Object,YR),
    hop(Filter,XI,XS,PI,PS,YI,YS,Transaction_Object),
    resolve_o(Save_Path,X,XI,XS,Transaction_Object,XR),
    resolve_p(Save_Path,P,PI,PS,Transaction_Object,PR),
    resolve_o(Save_Path,Y,YI,YS,Transaction_Object,YR),
    make_edge(XR,PR,YR,Edge).
run_pattern_((P,Q),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path) :-
    run_pattern_(P,X,Z,Open_Set,Path-Path_M,Filter,Transaction_Object,Save_Path),
    run_pattern_(Q,Z,Y,Open_Set,Path_M-Tail,Filter,Transaction_Object,Save_Path).
run_pattern_((P;Q),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path) :-
    (   run_pattern_(P,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path)
    ;   run_pattern_(Q,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path)).
run_pattern_(star(P),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path) :-
    (   X = Y,
        Path = Tail
    ;   run_pattern_n_m_(P,1,-1,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path)).
run_pattern_(plus(P),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path) :-
    run_pattern_n_m_(P,1,-1,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path).
run_pattern_(times(P,N,M),X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path) :-
    run_pattern_n_m_(P,N,M,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path).

% Case: {0,0} - exactly zero hops, just unify X and Y
run_pattern_n_m_(_P, 0, 0, X, X, _Open_Set, Tail-Tail, _Filter, _Transaction_Object, _Save_Path) :-
    !.
% Case: {0,M} where M>0 - zero OR 1 to M hops
run_pattern_n_m_(_P, 0, M, X, X, _Open_Set, Tail-Tail, _Filter, _Transaction_Object, _Save_Path) :-
    M > 0.
run_pattern_n_m_(P, 0, M, X, Y, Open_Set, Path-Tail, Filter, Transaction_Object, Save_Path) :-
    M > 0,
    run_pattern_n_m_(P, 1, M, X, Y, Open_Set, Path-Tail, Filter, Transaction_Object, Save_Path).
% Case: {1,1} - exactly one hop
run_pattern_n_m_(P,1,1,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path) :-
    !,
    copy_term(P, P_Fresh),  % Fresh copy needed even for single match
    run_pattern_(P_Fresh,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path).
% Case: {1,M} - one or more hops
run_pattern_n_m_(P,1,_,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path) :-
    copy_term(P, P_Fresh),  % Fresh copy needed to avoid binding across backtracking
    run_pattern_(P_Fresh,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path).
% Case: {N,M} where N>=1 - recursive case for N>1
run_pattern_n_m_(P,N,M,X,Y,Open_Set,Path-Tail,Filter,Transaction_Object,Save_Path) :-
    N >= 1,
    Np is max(1,N-1),
    Mp is M-1,
    copy_term(P, P_Fresh),  % Create fresh copy to avoid binding predicate variables across iterations
    run_pattern_(P_Fresh,X,Z,Open_Set,Path-Path_IM,Filter,Transaction_Object,Save_Path),
    run_pattern_n_m_(P,Np,Mp,Z,Y,Open_Set,Path_IM-Tail,Filter,Transaction_Object,Save_Path).

/*
 * Generate ids for the predicate based on filter (one for each graph)
 */
instance_predicate_id(Pred, Transaction_Object, IdI) :-
    database_instance(Transaction_Object, [Instance_RWO]),
    (   read_write_obj_reader(Instance_RWO, Layer),
        ground(Layer),
        predicate_id(Layer, Pred, IdI)
    ->  true
    ;   var(Pred)
    ->  true
    ;   IdI = 0 % impossible predicate
    ).

schema_predicate_id(Pred, Transaction_Object, IdS) :-
    database_schema(Transaction_Object, [Schema_RWO]),
    (   read_write_obj_reader(Schema_RWO, Layer),
        ground(Layer),
        predicate_id(Layer, Pred, IdS)
    ->  true
    ;   var(Pred)
    ->  true
    ;   IdS = 0 % impossible predicate
    ).

instance_subject_id(Subject, Transaction_Object, IdI) :-
    database_instance(Transaction_Object, [Instance_RWO]),
    (   ground(Subject),
        read_write_obj_reader(Instance_RWO, Layer),
        ground(Layer),
        subject_id(Layer, Subject, IdI)
    ->  true
    ;   var(Subject)
    ->  true
    ;   IdI = 0 % impossible subject
    ).

schema_subject_id(Subject, Transaction_Object, IdS) :-
    database_schema(Transaction_Object, [Schema_RWO]),
    (   ground(Subject),
        read_write_obj_reader(Schema_RWO, Layer),
        ground(Layer),
        subject_id(Layer, Subject, IdS)
    ->  true
    ;   var(Subject)
    ->  true
    ;   IdS = 0 % impossible subject
    ).

instance_object_id(Object, Transaction_Object, IdI) :-
    database_instance(Transaction_Object, [Instance_RWO]),
    (   ground(Object)
    ->  (   read_write_obj_reader(Instance_RWO, Layer),
            ground(Layer),
            object_storage(Object,Storage),
            object_id(Layer, Storage, IdI)
        ->  true
        ;   IdI = 0
        )
    ;   true
    ).

schema_object_id(Object, Transaction_Object, IdS) :-
    database_schema(Transaction_Object, [Schema_RWO]),
    (   ground(Object)
    ->  (   read_write_obj_reader(Schema_RWO, Layer),
            ground(Layer),
            object_storage(Object,Storage),
            object_id(Layer, Storage, IdS)
        ->  true
        ;   IdS = 0
        )
    ;   true
    ).

filtered_object_ids(Object, type_filter{ types : Types}, Transaction_Object, IdI, IdS) :-
    (   memberchk(instance, Types)
    ->  instance_object_id(Object, Transaction_Object, IdI)
    ;   true
    ),
    (   memberchk(schema, Types)
    ->  schema_object_id(Object, Transaction_Object, IdS)
    ;   true
    ).

filtered_predicate_ids(Pred, type_filter{ types : Types}, Transaction_Object, IdI, IdS) :-
    (   memberchk(instance, Types)
    ->  instance_predicate_id(Pred, Transaction_Object, IdI)
    ;   true
    ),
    (   memberchk(schema, Types)
    ->  schema_predicate_id(Pred, Transaction_Object, IdS)
    ;   true
    ).

/*
 * patterns have the following syntax:
 *
 * P,Q,R := p | n | p(P) | n(P) | P,Q | P;Q | plus(P) | star(P) | times(P,N,M)
 *
 * foo>,<baz,bar>
 */
compile_pattern(n, n(_,_,_), _Prefixes, _Filter, _Transaction_Object).
compile_pattern(p, p(_,_,_), _Prefixes, _Filter, _Transaction_Object).
compile_pattern(n(Pred), n(Pred_Expanded,PredI,PredS), Prefixes, Filter, Transaction_Object) :-
    prefixed_to_property(Pred,Prefixes,Pred_Expanded),
    filtered_predicate_ids(Pred_Expanded,Filter,Transaction_Object, PredI, PredS).
compile_pattern(p(Pred), p(Pred_Expanded,PredI,PredS), Prefixes, Filter, Transaction_Object) :-
    prefixed_to_property(Pred,Prefixes,Pred_Expanded),
    filtered_predicate_ids(Pred_Expanded,Filter,Transaction_Object, PredI, PredS).
compile_pattern((X,Y), (XC,YC), Prefixes, Filter, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Filter,Transaction_Object),
    compile_pattern(Y,YC,Prefixes,Filter,Transaction_Object).
compile_pattern((X;Y), (XC;YC), Prefixes, Filter, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Filter,Transaction_Object),
    compile_pattern(Y,YC,Prefixes,Filter,Transaction_Object).
compile_pattern(star(X), star(XC), Prefixes, Filter, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Filter,Transaction_Object).
compile_pattern(plus(X), plus(XC), Prefixes, Filter, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Filter,Transaction_Object).
compile_pattern(times(X,N,M), times(XC,N,M), Prefixes, Filter, Transaction_Object) :-
    compile_pattern(X,XC,Prefixes,Filter,Transaction_Object).

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

test(p, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing"},

    AST = (insert(a,b,c),
           insert(a,b,e),
           insert(a,b,g),
           insert(a,b,h)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    AST2 = path(a, p(b), v(x)),

    create_context(Descriptor,Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings,Result,Bindings),
    Bindings = [_{x:c},_{x:e},_{x:g},_{x:h}].

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

test(chained_data, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing"},

    AST = (insert(a,identifier, "a"^^xsd:string),
           insert(a,parent,b),
           insert(b,identifier, "b"^^xsd:string),
           insert(b,parent,c),
           insert(c,identifier, "c"^^xsd:string)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor,Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    AST2 = path(a, (star(p(parent)),p(identifier)), v(y), v(p)),

    create_context(Descriptor,Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings,Result,Bindings),

    length(Bindings, 3),

    % backwards
    AST3 = path(v(x), (star(p(parent)),p(identifier)), "c"^^xsd:string, v(p)),

    run_context_ast_jsonld_response(Context2, AST3, no_data_version, _, Result2),
    get_dict(bindings,Result2,Bindings2),

    length(Bindings2, 3),

    % path free
    AST4 = path(a, (star(p(parent)),p(identifier)), v(y)),

    run_context_ast_jsonld_response(Context2, AST4, no_data_version, _, Result3),
    get_dict(bindings,Result3,Bindings3),

    length(Bindings3, 3),

    % path free backwards
    AST5 = path(v(x), (star(p(parent)),p(identifier)), "c"^^xsd:string),

    run_context_ast_jsonld_response(Context2, AST5, no_data_version, _, Result4),
    get_dict(bindings,Result4,Bindings4),

    Bindings4 = [_{x:c},_{x:b},_{x:a}].

test(star_any_predicate_two_levels, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing star operator with ANY predicate - two levels with different predicates"},

    % Create a simple chain with TWO different predicates: a -p1-> b -p2-> c
    AST = (insert(a, p1, b),
           insert(b, p2, c)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor, Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    % Test: Use star(p) where p = ANY predicate
    % Should traverse: a -p1-> b -p2-> c
    AST2 = path(a, star(p), v(x)),

    create_context(Descriptor, Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings, Result, Bindings),

    % Should find: a (0 hops), b (1 hop via p1), c (2 hops: p1 then p2)
    % If predicate variable gets bound on first hop, we won't find c
    length(Bindings, 3),
    memberchk(_{x:a}, Bindings),
    memberchk(_{x:b}, Bindings),
    memberchk(_{x:c}, Bindings).

test(star_any_predicate_four_nodes, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing star operator with ANY predicate - four nodes"},

    % Create chain: a -p1-> b -p2-> c -p3-> d (like integration test)
    AST = (insert(a, p1, b),
           insert(b, p2, c),
           insert(c, p3, d)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor, Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    % Test: Use star(p) where p = ANY predicate
    AST2 = path(a, star(p), v(x)),

    create_context(Descriptor, Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings, Result, Bindings),

    % Should find: a (0 hops), b (1 hop), c (2 hops), d (3 hops)
    length(Bindings, 4),
    memberchk(_{x:a}, Bindings),
    memberchk(_{x:b}, Bindings),
    memberchk(_{x:c}, Bindings),
    memberchk(_{x:d}, Bindings).

% BUG: times{0,0} should return only the starting node (follow edge 0 times)
test(times_0_0_returns_start_only, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing times{0,0} - zero hops"},

    % Create chain: a -b-> c -d-> e
    AST = (insert(a, b, c),
           insert(c, d, e)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor, Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    % Test: times(p(b), 0, 0) should return only the starting node
    AST2 = path(a, times(p(b), 0, 0), v(x)),

    create_context(Descriptor, Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings, Result, Bindings),

    % Should find only: a (0 hops)
    length(Bindings, 1),
    memberchk(_{x:a}, Bindings).

% BUG: times{0,1} should return starting node and one hop
test(times_0_1_returns_start_and_one_hop, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing times{0,1} - zero or one hops"},

    % Create chain: a -b-> c -b-> e
    AST = (insert(a, b, c),
           insert(c, b, e)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor, Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    % Test: times(p(b), 0, 1) should return starting node and one hop
    AST2 = path(a, times(p(b), 0, 1), v(x)),

    create_context(Descriptor, Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings, Result, Bindings),

    % Should find: a (0 hops), c (1 hop)
    length(Bindings, 2),
    memberchk(_{x:a}, Bindings),
    memberchk(_{x:c}, Bindings).

% BUG: times{0,2} should return starting node and up to two hops
test(times_0_2_returns_start_and_up_to_two_hops, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "test"))),
         cleanup(teardown_temp_store(State))
     ]) :-

    Commit_Info = commit_info{ author : "automated test framework",
                               message : "testing times{0,2} - zero, one, or two hops"},

    % Create chain: a -b-> c -b-> e -b-> g
    AST = (insert(a, b, c),
           insert(c, b, e),
           insert(e, b, g)),

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor, Commit_Info, Context),
    run_context_ast_jsonld_response(Context, AST, no_data_version, _, _),

    % Test: times(p(b), 0, 2) should return starting node and up to two hops
    AST2 = path(a, times(p(b), 0, 2), v(x)),

    create_context(Descriptor, Commit_Info, Context2),
    run_context_ast_jsonld_response(Context2, AST2, no_data_version, _, Result),
    get_dict(bindings, Result, Bindings),

    % Should find: a (0 hops), c (1 hop), e (2 hops) - NOT g (3 hops)
    length(Bindings, 3),
    memberchk(_{x:a}, Bindings),
    memberchk(_{x:c}, Bindings),
    memberchk(_{x:e}, Bindings),
    \+ memberchk(_{x:g}, Bindings).

:- end_tests(path).
