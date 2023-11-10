:- module('query/reorder',[
              optimize_read_order/2,
              is_var/1,
              non_var/1
          ]).

:- use_module(core(util)).
:- use_module(partition, [partition/3]).
:- use_module(woql_compile, [mode_for_compound/2]).
:- use_module(definition, [mode/2, definition/1, cost/2, is_var/1, non_var/1, term_vars/2]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(sort)).
:- use_module(library(apply_macros)).

term_mvars(mv(X), MVars) =>
    MVars = [X].
term_mvars(List, MVars),
is_list(List) =>
    maplist(term_mvars, List, MVar_List),
    append(MVar_List, MVars_Unsorted),
    sort(MVars_Unsorted, MVars).
term_mvars(Dict, MVars),
is_dict(Dict) =>
    dict_pairs(Dict, _, Pairs),
    maplist([_-MV,MVar]>>term_mvars(MV,MVar), Pairs, MVar_List),
    append(MVar_List, MVars_Unsorted),
    sort(MVars_Unsorted, MVars).
term_mvars(select(MVL, Query), MVars) =>
    term_mvars(MVL, MVLMVars),
    term_mvars(Query, MV),
    intersection(MV,MVLMVars,MVars).
term_mvars(group_by(Unique,_Template,Query,Result), MVars) =>
    term_mvars(Unique, UMVars),
    term_mvars(Query, QMVars),
    intersection(UMVars, QMVars, Both_MVars),
    term_mvars(Result, RMVars),
    union(Both_MVars, RMVars, MVars).
term_mvars(Term, MVars) =>
    Term =.. [_|Rest],
    maplist(term_mvars, Rest, MVars_Lists),
    append(MVars_Lists, MVars_Unsorted),
    sort(MVars_Unsorted,MVars).

po(Term1, Term2) :-
    cost(Term1, Cost1),
    cost(Term2, Cost2),
    Cost1 =< Cost2.

po_comp(Op, X, Y) :-
    (   po(X, Y)
    ->  Op = (<)
    ;   po(Y, X)
    ->  Op = (>)
    ;   compare(Op,X,Y)
    ).

sort_definedness(Terms, Sorted) :-
    predsort(po_comp, Terms, Sorted).

order_conjuncts(Terms, Sorted) :-
    sort_definedness(Terms, Def_Sort),
    sort_bound(Def_Sort, Sorted).

metasub(v(X), Vars, XO),
memberchk(X, Vars) =>
    XO = mv(X).
metasub(select(Vars,_Query), Vars, _Result) =>
    throw(error(unimplemented)).
metasub(Dict, Vars, Result),
is_dict(Dict) =>
    dict_pairs(Dict, Functor, Pairs),
    maplist({Vars}/[P-V,P-V2]>>metasub(V,Vars,V2), Pairs, New_Pairs),
    dict_create(Result, Functor, New_Pairs).
metasub(Term, Vars, Result) =>
    Term =.. [F|Args],
    maplist({Vars}/[Arg,New]>>metasub(Arg, Vars, New),
            Args,
            New_Args),
    Result =.. [F|New_Args].

metaunsub(mv(X), XO) =>
    XO = v(X).
metaunsub(Dict, X),
is_dict(Dict) =>
    dict_pairs(Dict, Functor, Pairs),
    maplist([P-V,P-V2]>>metaunsub(V,V2), Pairs, New_Pairs),
    dict_create(X, Functor, New_Pairs).
metaunsub(Term, Result) =>
    Term =.. [F|Args],
    maplist([Arg,New]>>metaunsub(Arg, New),
            Args,
            New_Args),
    Result =.. [F|New_Args].

undummy_bind([], []).
undummy_bind([Term|Terms], [Unbound|Unbounds]) :-
    Term =.. [F|Args],
    maplist(metaunsub, Args, Unsubed),
    Unbound =.. [F|Unsubed],
    undummy_bind(Terms, Unbounds).

dummy_bind([], _, Terms) =>
    Terms = [].
dummy_bind([Term|Terms], Vars, Results) =>
    metasub(Term,Vars,TermSub),
    Results = [TermSub|Bound],
    dummy_bind(Terms,Vars,Bound).

flatten_conjuncts((A,B), F) =>
    flatten_conjuncts(A,AF),
    flatten_conjuncts(B,BF),
    append(AF,BF,F).
flatten_conjuncts(Term, F) =>
    F = [Term].

deep_order((A;B), Deep) =>
    deep_order(A, DA),
    deep_order(B, DB),
    Deep = (DA;DB).
deep_order(group_by(Selection,Template,Query,Result),Deep) =>
    deep_order(Query, DQuery),
    Deep = group_by(Selection,Template,DQuery,Result).
deep_order(order_by(L,Query), Deep) =>
    deep_order(Query, DQuery),
    Deep = order_by(L, DQuery).
deep_order(into(G, Query), Deep) =>
    deep_order(Query, DQuery),
    Deep = into(G, DQuery).
deep_order(count(Query, N), Deep) =>
    deep_order(Query, DQuery),
    Deep = count(DQuery, N).
deep_order(not(Query), Deep) =>
    deep_order(Query, DQuery),
    Deep = not(DQuery).
deep_order(select(Vars,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = select(Vars,DQuery).
deep_order(limit(N,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = limit(N,DQuery).
deep_order(using(G,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = using(G,DQuery).
deep_order(from(G,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = from(G,DQuery).
deep_order(start(N,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = start(N,DQuery).
deep_order(distinct(X,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = distinct(X,DQuery).
deep_order((A,B),Deep) =>
    flatten_conjuncts((A,B), Flat),
    optimize_conjuncts(Flat, Ordered),
    xfy_list(',', Deep, Ordered).
deep_order(Term, Deep) =>
    Term = Deep.

sort_bound([], []).
sort_bound([Term|Terms], [Deep|Sorted]) :-
    deep_order(Term, Deep),
    term_vars(Deep, Vars),
    dummy_bind(Terms, Vars, Bound),
    order_conjuncts(Bound, Sorted).

optimize_conjuncts(Read, Ordered) :-
    commutative_partitions(Read, Partitions),
    maplist(order_conjuncts, Partitions, Ordered_Partitions),
    append(Ordered_Partitions, Ordered).

optimize_read_order(Read, Ordered) :-
    optimize_conjuncts(Read, Ordered_Bound),
    die_if(
        (   xfy_list(',', Prog, Ordered_Bound),
            cost(Prog, inf)),
        error(query_has_no_viable_mode, _)),
    undummy_bind(Ordered_Bound, Ordered).

non_commutative((X,Y)) =>
    once(
        (   non_commutative(X)
        ;   non_commutative(Y))
    ).
non_commutative((X;Y)) =>
    once(
        (   non_commutative(X)
        ;   non_commutative(Y))
    ).
non_commutative(immediately(Query)) =>
    non_commutative(Query).
non_commutative(opt(Query)) =>
    non_commutative(Query).
non_commutative(select(_,Query)) =>
    non_commutative(Query).
non_commutative(count(Query,_)) =>
    non_commutative(Query).
non_commutative(not(Query)) =>
    non_commutative(Query).
non_commutative(group_by(_,_,Query,_)) =>
    non_commutative(Query).
non_commutative(distinct(_,Query)) =>
    non_commutative(Query).
non_commutative(using(_,Query)) =>
    non_commutative(Query).
non_commutative(from(_,Query)) =>
    non_commutative(Query).
non_commutative(into(_,Query)) =>
    non_commutative(Query).
non_commutative(start(_,_)) =>
    true.
non_commutative(limit(_,_)) =>
    true.
non_commutative(once(_)) =>
    true.
non_commutative(_) =>
    false.

commutative_partitions([], [[]]).
commutative_partitions([ReadHead|Rest], Partitions) :-
    (   non_commutative(ReadHead)
    ->  commutative_partitions(Rest,New_Partitions),
        Partitions = [[],[ReadHead]|New_Partitions]
    ;   commutative_partitions(Rest,[H|New_Partitions]),
        Partitions = [[ReadHead|H]|New_Partitions]
    ).

disconnected(T) :-
    term_mvars(T, MVars),
    MVars = [],
    term_vars(T, Vars),
    Vars \= [].

split_at([], [], []).
split_at([T|Rest], [], [T|Rest]) :-
    disconnected(T),
    !.
split_at([T|Rest], [T|Start], End) :-
    split_at(Rest,Start,End).

split_disconnected([], [], []).
split_disconnected([Head], [Head], []) :-
    !.
split_disconnected([Head|Rest], [Head|Start], End) :-
    split_at(Rest,Start,End).

disconnected_partitions([], []).
disconnected_partitions([H|Conjunctions], [Start|Partitions]) :-
    split_disconnected([H|Conjunctions], Start, Rest),
    disconnected_partitions(Rest, Partitions).

:- begin_tests(reorder_query).

test(reorder, []) :-

    Term = (
        t(a,b,v(c)),
        (   t(v(c),f,g),
            (   insert(a,b,c),
                insert(d,b,c),
                (   insert(e,f,g),
                    insert(f,g,h))))),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = (t(a,b,v(c)),t(v(c),f,g)).

test(reorder_partitioned, []) :-

    Term = (
        (   t(a,b,v(c)),
            t(v(c),f,v(a))),
        (   t(v(c),f,v(b)),
            t(v(b),h,v(c))
        )
    ),
    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),
    Prog = (
        t(a,b,v(c)),
        t(v(c),f,v(a)),
        t(v(c),f,v(b)),
        t(v(b),h,v(c))
    ).

test(reorder_partitioned_backwards, []) :-

    Term = (
        t(v(c),f,v(b)),
        t(v(c),f,v(a)),
        t(v(b),h,v(c)),
        t(a,b,v(c))
    ),
    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = (
        t(a,b,v(c)),
        t(v(c),f,v(b)),
        t(v(b),h,v(c)),
        t(v(c),f,v(a))
    ).

test(reorder_ors, []) :-

    Term = (
        t(v(c),f,v(b)),
        t(v(c),f,v(a)),
        (    t(v(e),f,v(a)),
             t(c,f,v(a))
        ;    t(v(e),f,v(a)),
             t(d,f,v(a))
        ),
        t(v(b),h,v(c)),
        t(a,b,v(c))
    ),
    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),

    xfy_list(',', Prog, Reads),

    Prog = (
        t(a,b,v(c)),
        t(v(c),f,v(b)),
        t(v(b),h,v(c)),
        t(v(c),f,v(a)),
        (   t(c,f,v(a)),
            t(v(e),f,v(a))
        ;   t(d,f,v(a)),
            t(v(e),f,v(a))
		)
    ).

test(select, []) :-

    Term = select(
               [v(a), v(b)],
               (   t(v(a),f,g),
                   t(a,b,v(b))
               )
           ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),
    Prog = select(
               [v(a), v(b)],
               (   t(a,b,v(b)),
                   t(v(a),f,g)
               )
           ).

test(limit, []) :-

    Term = limit(
               3,
               (   t(v(a),f,g),
                   t(a,b,v(b))
               )
           ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),
    Prog = limit(
               3,
               (   t(a,b,v(b)),
                   t(v(a),f,g)
               )
           ).

test(internal_cuts, []) :-

    Term = (
        t(v(a),p,v(d)),
        t(x,p,v(a)),
        once(t(v(b),g,v(a))),
        t(v(b),q,v(d)),
        limit(
            10,
            t(v(b),r,v(e))),
        t(v(f), s, v(e)),
        t(v(e), s, t)
    ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = (
        t(x,p,v(a)),
        t(v(a),p,v(d)),
        once(t(v(b),g,v(a))),
        t(v(b),q,v(d)),
        limit(10,t(v(b),r,v(e))),
        t(v(e),s,t),
        t(v(f),s,v(e))
    ).

test(order_chains) :-
    Term = (t(v(x1), 'P1082', v(x2)),
            t(v(x1), 'P17', v(x3)),
            t(v(x3), 'P31', 'Q6256'),
            t(v(x3), 'P463', v(x4))),
    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = (
        t(v(x3),'P31','Q6256'),
        t(v(x3),'P463',v(x4)),
        t(v(x1),'P17',v(x3)),
        t(v(x1),'P1082',v(x2))
    ).

test(order_dicts) :-
    order_conjuncts([_{a:1, b:v('X')}=_{a:v('Y'), b:2}],
                    [_{a:1, b:v('X')}=_{a:v('Y'), b:2}]).


test(order_type) :-
    Reads = [
        t(v(x), rdf:type, '@schema':'Branch'),
        t(v(x), name, "main"^^xsd:string)
    ],
    optimize_read_order(Reads, Ordered),

    Ordered = [
        t(v(x),name,"main"^^xsd:string),
		t(v(x),rdf:type,'@schema':'Branch')
	].

test(select_not, []) :-

    Term = select(
               [v(document)],
               (   member(v(doc_id),[doc1,doc2]),
                   t(v(doc_id), rdf:type, v(type)),
                   not(v(type) = rdf:'List'),
                   get_document(v(doc_id), v(document))
               )
           ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),
    Prog = select(
               [v(document)],
			   ( member(v(doc_id),[doc1,doc2]),
				 t(v(doc_id),rdf:type,v(type)),
				 not((v(type) = rdf:'List')),
				 get_document(v(doc_id),v(document))
			   )).

test(select_regexp_1, []) :-

    Term = select(
               [v(person_name), v(vehicle_name)],
               (   t(v(vehicle), pilot, v(person)),
                   t(v(vehicle), label, v(vehicle_name)),
                   t(v(person), label, v(person_name)),
                   re("M(.*)", v(vehicle_name), [v(all)])
               )
           ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = select([v(person_name),v(vehicle_name)],
						( t(v(vehicle),pilot,v(person)),
						  t(v(vehicle),
						    label,
						    v(vehicle_name)),
						  re("M(.*)",
						     v(vehicle_name),
						     [v(all)]),
                          t(v(person),label,v(person_name))
						)).

test(select_regexp_2, []) :-

    Term = select(
               [v(person_name), v(vehicle_name)],
               (   t(v(vehicle), pilot, v(person)),
                   t(v(vehicle), label, v(vehicle_name)),
                   t(v(person), label, v(person_name)),
                   re("M(.*)", v(vehicle_name), [v(all), v(match)])
               )
           ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = select([v(person_name),v(vehicle_name)],
						( t(v(vehicle),pilot,v(person)),
						  t(v(vehicle),
						    label,
						    v(vehicle_name)),
						  re("M(.*)",
						     v(vehicle_name),
						     [v(all),v(match)]),
                          t(v(person),label,v(person_name))
						)).

test(greater, []) :-
    Term = (t(v(uri), rdf:type, 'SubjectType'),
            t(v(uri), predicate, v(value)),
            v(value) < 1,
            get_document(v(uri), v(doc))),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = (
        t(v(uri),predicate,v(value)),
        t(v(uri),rdf:type,'SubjectType'),
        v(value) < 1,
        get_document(v(uri),v(doc))
    ).

test(reorder_once, []) :-
    Term = (
        t(v(x), v(y), v(z)),
        select([v(x)],
               once(
                   t(v(x), p, v(z))))
    ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Term = Prog.

test(unmodable, [error(query_has_no_viable_mode, _)]) :-
    Term = (
        t(a, b, v(var)),
        v(var) = v(unbound)
    ),
    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, _Reads).

test(disconnected_partitions) :-
    disconnected_partitions(
        [t(v(x), y, z), t(mv(x), w, y), t(v(y), z, w)],
        [[t(v(x), y, z), t(mv(x), w, y)],
         [t(v(y), z, w)]]
    ).

test(reorder_dictionary) :-
    % Issue #1992
    metasub(resource(post('note.csv'),csv,_{}),[docid,label],_).

test(sum) :-
    AST = (
        count(t(v('Doc_0'),
			    rdf:type,
			    '@schema':'Person'),
		      v('Person')),
        sum([v('Person')],v('Count'))
    ),
    partition(AST,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    % No reorder
    Prog = AST.


:- end_tests(reorder_query).
