:- module('query/definition',[
              mode/2,
              definition/1,
              cost/2,
              is_var/1,
              non_var/1,
              term_vars/2,
              metasub/3,
              unmodable/1
          ]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(core(util),
              [interpolate/2,
               json_read_list_stream_head/3,
               json_read_tail_stream/2,
               terminus_schema_path/1]).
:- use_module(library(http/json), [json_write_dict/3]).

is_var(v(_)).

is_mvar(mv(_)).

non_var(X) :- \+ is_var(X).

term_vars(v(X), Vars) =>
    Vars = [X].
term_vars(List, Vars),
is_list(List) =>
    maplist(term_vars, List, Var_List),
    append(Var_List, Vars_Unsorted),
    sort(Vars_Unsorted, Vars).
term_vars(Dict, Vars),
is_dict(Dict) =>
    dict_pairs(Dict, _, Pairs),
    maplist([_-V,Var]>>term_vars(V,Var), Pairs, Var_List),
    append(Var_List, Vars_Unsorted),
    sort(Vars_Unsorted, Vars).
term_vars(select(VL, Query), Vars) =>
    term_vars(VL, VLVars),
    term_vars(Query, V),
    intersection(V,VLVars,Vars).
term_vars(group_by(Unique,_Template,Query,Result), Vars) =>
    term_vars(Unique, UVars),
    term_vars(Query, QVars),
    intersection(UVars, QVars, Both_Vars),
    term_vars(Result, RVars),
    union(Both_Vars, RVars, Vars).
term_vars(Term, Vars) =>
    Term =.. [_|Rest],
    maplist(term_vars, Rest, Vars_Lists),
    append(Vars_Lists, Vars_Unsorted),
    sort(Vars_Unsorted,Vars).

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

/* Query level, aggregation and metalogic */
definition(
    ';'{
        name: 'Or',
        fields: [or],
        mode: [[:]],
        types: [list(query)]
    }).
definition(
    ','{
        name: 'And',
        fields: [and],
        mode: [[:]],
        types: [list(query)]
    }).
definition(
    immediately{
        name: 'Immediately',
        fields: [query],
        mode: [:],
        types: [query]
    }).
definition(
    opt{
        name: 'Optional',
        fields: [query],
        mode: [:],
        types: [query]
    }).
definition(
    once{
        name: 'Once',
        fields: [query],
        mode: [:],
        types: [query]
    }).
definition(
    select{
        name: 'Select',
        fields: [variables,query],
        mode: [[?],:],
        types: [list(string),query]
    }).
definition(
    start{
        name: 'Start',
        fields: [start,query],
        mode: [+,:],
        types: [integer,query]
    }).
definition(
    limit{
        name: 'Limit',
        fields: [limit,query],
        mode: [+,:],
        types: [integer,query]
    }).
definition(
    count{
        name: 'Count',
        fields: [query,count],
        mode: [:,?],
        types: [query,count]
    }).
definition(
    order_by{
        name: 'OrderBy',
        fields: [ordering,query],
        mode: [[?],:],
        types: [list(order),query]
    }).
definition(
    opt{
        name: 'Optional',
        fields: [query],
        mode: [:],
        types: [query]
    }).
definition(
    not{
        name: 'Not',
        fields: [query],
        mode: [:],
        types: [query]
    }).
definition(
    group_by{
        name: 'GroupBy',
        fields: [template,group_by,value,query],
        mode: [?,?,?,:],
        types: [value,template,data_value,query]
    }).
definition(
    distinct{
        name: 'Distinct',
        fields: [variables,query],
        mode: [?,:],
        types: [list(string),query]
    }).
definition(
    pin{
        name: 'Pin',
        fields: [query],
        mode: [:],
        types: [query]
    }).

/* collection selection */
definition(
    using{
        name: 'Using',
        fields: [collection,query],
        mode: [+,:],
        types: [collection,query]
    }).
definition(
    from{
        name: 'From',
        fields: [graph,query],
        mode: [+,:],
        types: [graph,query]
    }).
definition(
    into{
        name: 'Into',
        fields: [graph,query],
        mode: [+,:],
        types: [graph,query]
    }).

/* get / put */
definition(
    get{
        name: 'Get',
        fields: [columns, resource, optional(has_header)],
        mode: [[?],+,+],
        types: [list(column), resource, boolean]
    }).

/* documents */
definition(
    get_document{
        name: 'ReadDocument',
        fields: [identifier,document],
        mode: [+,?],
        types: [node,json]
    }).
definition(
    replace_document{
        name: 'UpdateDocument',
        fields: [document,optional(identifier)],
        mode: [+,?],
        types: [node,json]
    }).
definition(
    insert_document{
        name: 'InsertDocument',
        fields: [document,optional(identifier)],
        mode: [+,?],
        types: [node,json]
    }).
definition(
    delete_document{
        name: 'DeleteDocument',
        fields: [identifier],
        mode: [+],
        types: [node]
    }).
/* Triples */
definition(
    delete{
        name: 'DeleteTriple',
        fields: [subject,predicate,object,optional(graph)],
        mode: [+,+,+,+],
        types: [node,node,value,graph]
    }).
definition(
    addition{
        name: 'AddedTriple',
        fields: [subject,predicate,object,optional(graph)],
        mode: [?,?,?,+],
        types: [node,node,value,graph]
    }).
definition(
    removal{
        name: 'DeletedTriple',
        fields: [subject,predicate,object,optional(graph)],
        mode: [?,?,?,+],
        types: [node,node,value,graph]
    }).
definition(
    t{
        name: 'Triple',
        fields: [subject,predicate,object,optional(graph)],
        mode: [?,?,?,+],
        types: [node,node,value,graph]
    }).
definition(
    path{
        name: 'Path',
        fields: [subject,pattern,object,optional(path)],
        mode: [?,+,?,-],
        types: [node,pattern,node,json]
    }).
definition(
    insert{
        name: 'AddTriple',
        fields: [subject,predicate,object,optional(graph)],
        mode: [+,+,+,+],
        types: [node,node,value,graph]
    }).
/* operators */
definition(
    ={
        name: 'Equals',
        fields: [left,right],
        mode: [?,?],
        types: [any,any]
    }).
definition(
    <{
        name: 'Less',
        fields: [left,right],
        mode: [+,+],
        types: [any,any]
    }).
definition(
    >{
        name: 'Greater',
        fields: [left,right],
        mode: [+,+],
        types: [any,any]
    }).
definition(
    like{
        name: 'Like',
        fields: [left,right,similarity],
        mode: [+,+,?],
        types: [string,string,float]
    }).
definition(
    concat{
        name: 'Concatenate',
        fields: [list,result],
        mode: [+,?],
        types: [list(string),string]
    }).
definition(
    trim{
        name: 'Trim',
        fields: [untrimmed,trimmed],
        mode: [+,?],
        types: [string,string]
    }).
definition(
    pad{
        name: 'Pad',
        fields: [string,char,times,result],
        mode: [+,+,+,?],
        types: [string,string,integer,string]
    }).
definition(
    sub_string{
        name: 'Substring',
        fields: [string,before,length,after,substring],
        mode: [+,?,?,?,?],
        types: [string,integer,integer,integer,string]
    }).
definition(
    re{
        name: 'Regexp',
        fields: [pattern,string,result],
        mode: [+,+,?],
        types: [string,string,list(string)]
    }).
definition(
    split{
        name: 'Split',
        fields: [string,pattern,list],
        mode: [+,+,?],
        types: [string,string,list(string)]
    }).
definition(
    upper{
        name: 'Upper',
        fields: [mixed,upper],
        mode: [+,?],
        types: [string,string]
    }).
definition(
    lower{
        name: 'Lower',
        fields: [mixed,lower],
        mode: [+,?],
        types: [string,string]
    }).
definition(
    is{
        name: 'Eval',
        fields: [expression,result],
        mode: [+,?],
        types: [arithmetic,decimal]
    }).
definition(
    dot{
        name: 'Dot',
        fields: [document,field,value],
        mode: [+,+,?],
        types: [json,string,data_value]
    }).
definition(
    length{
        name: 'Length',
        fields: [list,length],
        mode: [+,+,?],
        types: [list(any),integer]
    }).
definition(
    member{
        name: 'Member',
        fields: [member,list],
        mode: [?,+],
        types: [any,list(any)]
    }).
definition(
    join{
        name: 'Join',
        fields: [list, separator, result],
        mode: [+,+,?],
        types: [list(any),string,string]
    }).
definition(
    sum{
        name: 'Sum',
        fields: [list, result],
        mode: [+,?],
        types: [list(any),decimal]
    }).
definition(
    timestamp_now{
        name: 'Now',
        fields: [result],
        mode: [-],
        types: [decimal]
    }).



/* types */
definition(
    isa{
        name: 'IsA',
        fields: [element,type],
        mode: [?,?],
        types: [node,node]
    }).
definition(
    <<{
        name: 'Subsumption',
        fields: [child,parent],
        mode: [?,?],
        types: [node,node]
    }).
definition(
    typecast{
        name: 'Typecast',
        fields: [value,type,result],
        mode:  [+,+,?],
        types: [value,node,value]
    }).
definition(
    typeof{
        name: 'TypeOf',
        fields: [value,type],
        mode: [+,?],
        types: [value,node]
    }).
definition(
    true{
        name: 'True',
        fields: [],
        mode: [],
        types: []
    }).
definition(
    false{
        name: 'False',
        fields: [],
        mode: [],
        types: []
    }).

% And, Or treated specially as they are n-ary in the AST external AST
% but binary in the internal one
mode(Term, Mode) :-
    Term =.. [Head|Args],
    mode_lookup(Head,Args,Mode).

mode_lookup(',', _, Mode) =>
    Mode = [:,:].
mode_lookup(';', _, Mode) =>
    Mode = [:,:].
mode_lookup(Head, Args, Mode) =>
    length(Args, N),
    Dict = Head{name:_,fields:_,mode:Mode_Candidate,types:_},
    definition(Dict),
    length(Mode, N),
    append(Mode, _, Mode_Candidate).

is_skeleton(Arg),
is_var(Arg) =>
    true.
is_skeleton(Args),
is_list(Args) =>
    \+ forall(member(Arg, Args),
              \+ is_skeleton(Arg)).
is_skeleton(Args),
is_dict(Args) =>
    \+ forall(get_dict(_Key, Args, Arg),
              \+ is_skeleton(Arg)).
is_skeleton(_) =>
    false.

check_term_mode(:, _Term) =>
    true.
check_term_mode([Mode], Term),
is_list(Term) =>
    maplist(check_term_mode(Mode), Term).
check_term_mode([_], Term) =>
    is_mvar(Term).
check_term_mode(-, Term) =>
    \+ is_var(Term),
    \+ is_mvar(Term),
    term_vars(Term, []).
check_term_mode(+, Term) =>
    term_vars(Term, []).
check_term_mode(?, _Term) =>
    true.

term_mode_correct(Term) :-
    mode(Term, Mode),
    Term =.. [_|Args],
    maplist(check_term_mode, Mode, Args).

operator(_=_).
operator(_>_).
operator(_<_).
operator(like(_,_,_)).
operator(concat(_,_)).
operator(trim(_,_)).
operator(pad(_,_,_,_)).
operator(sub_string(_,_,_,_,_)).
operator(re(_,_,_)).
operator(split(_,_,_)).
operator(upper(_,_)).
operator(lower(_,_)).
operator(_ is _).
operator(dot(_,_,_)).
operator(length(_,_,_)).
operator(join(_,_,_)).
operator(timestamp_now(_)).

cost(Term, Cost) :-
    catch(
        cost_(Term, Cost, pos),
        error(evaluation_error(float_overflow),_),
        Cost = inf
    ).

cost_(Term, Cost, _Polarity),
\+ term_mode_correct(Term) =>
    Cost = inf.

cost_((X,Y), Cost, Polarity) =>
    cost_(X, Cost_X, Polarity),
    term_vars(X, Vars),
    metasub(Y, Vars, Yp),
    cost_(Yp, Cost_Y, Polarity),
    (   memberchk(inf, [Cost_X,Cost_Y])
    ->  Cost = inf
    ;   Cost is Cost_X + Cost_Y
    ).

cost_((X;Y), Cost, Polarity) =>
    cost_(X, Cost_X, Polarity),
    cost_(Y, Cost_Y, Polarity),
    (   memberchk(inf, [Cost_X,Cost_Y])
    ->  Cost = inf
    ;   Cost is Cost_X * Cost_Y
    ).

cost_(immediately(Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(opt(Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(once(Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(select(_Vars,Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(start(N,Query), Cost, Polarity) =>
    cost_(Query, Cost_Query, Polarity),
    (   Cost_Query = inf
    ->  Cost = inf
    ;   Cost is max(1.0, Cost_Query - N / Cost_Query)
    ).

cost_(limit(N,Query), Cost, Polarity) =>
    cost_(Query, Cost_Query, Polarity),
    (   Cost_Query = inf
    ->  Cost = inf
    ;   Cost is max(1.0, Cost_Query - N / Cost_Query)
    ).

cost_(count(Query,_), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(order_by(_,Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(opt(Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(not(Query), Cost, _Polarity) =>
    cost_(Query, Cost, neg).

cost_(group_by(_,_,Query,_), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(distinct(_,Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(pin(Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(using(_,Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(from(_,Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(into(_,Query), Cost, Polarity) =>
    cost_(Query, Cost, Polarity).

cost_(get_document(_,_), Cost, _Polarity) =>
    Cost = 10.

cost_(insert_document(_), Cost, _Polarity) =>
    Cost = 15.

cost_(insert_document(_,_), Cost, _Polarity) =>
    Cost = 15.

cost_(replace_document(_), Cost, _Polarity) =>
    Cost = 20.

cost_(replace_document(_,_), Cost, _Polarity) =>
    Cost = 20.

cost_(delete_document(_), Cost, _Polarity) =>
    Cost = 15.

cost_(get(_,_,_), Cost, _Polarity) =>
    Cost = 0.

cost_(t(X, Y, Z), Cost, _Polarity),
non_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 1.

cost_(t(X, Y, Z, _), Cost, _Polarity),
non_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 1.

cost_(t(X, Y, Z), Cost, _Polarity),
is_var(X),
non_var(Y),
non_var(Z),
Y = rdf:type =>
    Cost = 14.

cost_(t(X, Y, Z, _), Cost, _Polarity),
is_var(X),
non_var(Y),
non_var(Z),
Y = rdf:type =>
    Cost = 14.

cost_(t(X, Y, Z), Cost, _Polarity),
is_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 5.

cost_(t(X, Y, Z, _), Cost, _Polarity),
is_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 5.

cost_(t(X, Y, Z), Cost, _Polarity),
non_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 3.

cost_(t(X, Y, Z, _), Cost, _Polarity),
non_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 3.

cost_(t(X, Y, Z), Cost, _Polarity),
non_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 2.

cost_(t(X, Y, Z, _), Cost, _Polarity),
non_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 2.

cost_(t(X, Y, Z), Cost, _Polarity),
non_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 6.

cost_(t(X, Y, Z, _), Cost, _Polarity),
non_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 6.

cost_(t(X, Y, Z), Cost, _Polarity),
is_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 15.

cost_(t(X, Y, Z, _), Cost, _Polarity),
is_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 15.

cost_(t(X, Y, Z), Cost, _Polarity),
is_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 10.

cost_(t(X, Y, Z, _), Cost, _Polarity),
is_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 10.

cost_(t(X, Y, Z), Cost, _Polarity),
is_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 30.

cost_(t(X, Y, Z, _), Cost, _Polarity),
is_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 30.

/* addition */
cost_(addition(X, Y, Z), Cost, _Polarity),
non_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 1.

cost_(addition(X, Y, Z, _), Cost, _Polarity),
non_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 1.

cost_(addition(X, Y, Z), Cost, _Polarity),
is_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 3.

cost_(addition(X, Y, Z, _), Cost, _Polarity),
is_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 3.

cost_(addition(X, Y, Z), Cost, _Polarity),
non_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 4.

cost_(addition(X, Y, Z, _), Cost, _Polarity),
non_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 4.

cost_(addition(X, Y, Z), Cost, _Polarity),
non_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 2.

cost_(addition(X, Y, Z, _), Cost, _Polarity),
non_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 2.

cost_(addition(X, Y, Z), Cost, _Polarity),
non_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 8.

cost_(addition(X, Y, Z, _), Cost, _Polarity),
non_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 8.

cost_(addition(X, Y, Z), Cost, _Polarity),
is_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 12.

cost_(addition(X, Y, Z, _), Cost, _Polarity),
is_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 12.

cost_(addition(X, Y, Z), Cost, _Polarity),
is_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 6.

cost_(addition(X, Y, Z, _), Cost, _Polarity),
is_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 6.

cost_(addition(X, Y, Z), Cost, _Polarity),
is_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 24.

cost_(addition(X, Y, Z, _), Cost, _Polarity),
is_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 24.

cost_(removal(X, Y, Z), Cost, _Polarity),
non_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 1.

cost_(removal(X, Y, Z, _), Cost, _Polarity),
non_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 1.

cost_(removal(X, Y, Z), Cost, _Polarity),
is_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 3.

cost_(removal(X, Y, Z, _), Cost, _Polarity),
is_var(X),
non_var(Y),
non_var(Z) =>
    Cost = 3.

cost_(removal(X, Y, Z), Cost, _Polarity),
non_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 4.

cost_(removal(X, Y, Z, _), Cost, _Polarity),
non_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 4.

cost_(removal(X, Y, Z), Cost, _Polarity),
non_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 2.

cost_(removal(X, Y, Z, _), Cost, _Polarity),
non_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 2.

cost_(removal(X, Y, Z), Cost, _Polarity),
non_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 8.

cost_(removal(X, Y, Z, _), Cost, _Polarity),
non_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 8.

cost_(removal(X, Y, Z), Cost, _Polarity),
is_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 12.

cost_(removal(X, Y, Z, _), Cost, _Polarity),
is_var(X),
non_var(Y),
is_var(Z) =>
    Cost = 12.

cost_(removal(X, Y, Z), Cost, _Polarity),
is_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 6.

cost_(removal(X, Y, Z, _), Cost, _Polarity),
is_var(X),
is_var(Y),
non_var(Z) =>
    Cost = 6.

cost_(removal(X, Y, Z), Cost, _Polarity),
is_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 24.

cost_(removal(X, Y, Z, _), Cost, _Polarity),
is_var(X),
is_var(Y),
is_var(Z) =>
    Cost = 24.

cost_(delete(_,_,_), Cost, _Polarity) =>
    Cost = 5.

cost_(delete(_,_,_,_), Cost, _Polarity) =>
    Cost = 5.

cost_(insert(_,_,_), Cost, _Polarity) =>
    Cost = 5.

cost_(insert(_,_,_,_), Cost, _Polarity) =>
    Cost = 5.

cost_(path(X, _, Y), Cost, _Polarity),
is_var(X),
non_var(Y) =>
    Cost = 15.

cost_(path(X, _, Y), Cost, _Polarity),
non_var(X),
is_var(Y) =>
    Cost = 15.

cost_(path(X, _, Y, _), Cost, _Polarity),
non_var(X),
is_var(Y) =>
    Cost = 25.

cost_(path(X, _, Y), Cost, _Polarity),
is_var(X),
is_var(Y) =>
    Cost = 225.

cost_(path(X, _, Y, _), Cost, _Polarity),
non_var(X),
is_var(Y) =>
    Cost = 625.

cost_(path(X, _, Y, _), Cost, _Polarity),
non_var(X),
is_var(Y) =>
    Cost = 625.

cost_(sum(_,_), Cost, _Polarity) =>
    Cost = 10.

cost_(member(_,_), Cost, _Polarity) =>
    Cost = 10.

cost_(X=Y, Cost, Polarity),
Polarity = neg,
\+ term_vars(X=Y, []) =>
    Cost = inf.

cost_(Term, Cost, _Polarity),
operator(Term) =>
    Cost = 1.

cost_(isa(X,Y), Cost, _Polarity),
non_var(X),
non_var(Y) =>
    Cost = 1.

cost_(isa(X,Y), Cost, _Polarity),
is_var(X),
non_var(Y) =>
    Cost = 100.

cost_(isa(X,Y), Cost, _Polarity),
non_var(X),
is_var(Y) =>
    Cost = 2.

cost_(isa(X,Y), Cost, _Polarity),
is_var(X),
is_var(Y) =>
    Cost = 200.

cost_(X << Y, Cost, _Polarity),
non_var(X),
non_var(Y) =>
    Cost = 1.

cost_(X << Y, Cost, _Polarity),
is_var(X),
non_var(Y) =>
    Cost = 10.

cost_(X << Y, Cost, _Polarity),
non_var(X),
is_var(Y) =>
    Cost = 10.

cost_(X << Y, Cost, _Polarity),
is_var(X),
is_var(Y) =>
    Cost = 100.

cost_(typecast(_,_,_), Cost, _Polarity) =>
    Cost = 1.

cost_(typeof(_,_), Cost, _Polarity) =>
    Cost = 1.

cost_(true, Cost, _Polarity) =>
    Cost = 0.

cost_(false, Cost, _Polarity) =>
    Cost = 0.

unmodable(Query) :-
    cost(Query, inf).

update_woql_schema :-
    terminus_schema_path(Path),
    interpolate([Path,'woql.json'], WOQL_Path),
    open(WOQL_Path, read, Stream),
    json_read_list_stream_head(Stream, Context, Tail_Stream),
    findall(
        Dict,
        json_read_tail_stream(Tail_Stream, Dict),
        Dicts),
    close(Stream),
    !,
    maplist(inject_woql_info, Dicts, Updated_Dicts),
    open(WOQL_Path, write, Output_Stream),
    json_write_dict(Output_Stream, Context, []),
    nl(Output_Stream),
    forall(
        member(Output_Dict, Updated_Dicts),
        (   json_write_dict(Output_Stream, Output_Dict, [serialize_unknown(true)]),
            nl(Output_Stream)
        )
    ),
    close(Output_Stream).

inject_woql_info(Dict, New_Dict) :-
    get_dict('@id', Dict, Name),
    atom_string(Atom, Name),
    Def = _{name:Atom,fields:Fields,mode:Mode,types:Types},
    definition(Def),
    !,
    metadata(Dict, Metadata),
    New_Metadata = _{ types: Types,
                      fields: Fields,
                      mode: Mode },
    put_dict('https://terminusdb.com', Metadata, New_Metadata, Output_Metadata),
    put_dict('@metadata', Dict, Output_Metadata, New_Dict).
inject_woql_info(Dict, Dict).

metadata(Dict, Metadata) :-
    (   get_dict('@metadata', Dict, Metadata)
    ->  true
    ;   Metadata = metadata{}
    ).

:- begin_tests(mode).

test(list_mvar_term_mode) :-
    term_mode_correct(sum(mv('List'), v('Count'))).

test(list_skeleton_term_mode) :-
    % You can't sum what you don't have
    \+ term_mode_correct(sum([v('Person')], v('Count'))).

test(list_skeleton_bound_term_mode) :-
    % You can't sum what you don't have
    term_mode_correct(sum([mv('Person')], v('Count'))).

test(dict_skeleton_term_mode) :-
    % Now creates a binding - should be ok
    term_mode_correct(dict{ asdf : v('Foo') } = dict{ asdf : "bar" }).


test(get_well_moded) :-
    AST = (get([as('Start date', v('Start date'), 'http://www.w3.org/2001/XMLSchema#dateTime')],
               resource(remote("https://terminusdb.com/t/data/bike_tutorial.csv"), csv, _{}),
               true)),
    term_mode_correct(AST).


test(cost_overflow) :-

    AST0 = select([v('Person'), v('Name')],
                        (   t(v('Person'), rdf:type, '@schema':'Person'),
                            path(v('Person'), "identified_by,content", v('Name')))),
    cost(AST0, Cost0),
    \+ Cost0 = inf,

    AST1 = using("admin/halloween",
                 select([v('Person'), v('Name')],
                        (   t(v('Person'), rdf:type, '@schema':'Person'),
                            path(v('Person'), "identified_by,content", v('Name'))))),

    AST1_1 = select([v('People'), v('Name')],
                    (   t(v('People'), rdf:type, '@schema':'People'),
                        t(v('People'), label, v('Name')))),
    cost(AST1_1, Cost1_1),
    \+ Cost1_1 = inf,

    AST2 = using("admin/star_wars",
                 select([v('People'), v('Name')],
                        (   t(v('People'), rdf:type, '@schema':'People'),
                            t(v('People'), label, v('Name'))))),

    AST = limit(10,
                (   AST1
                ;   AST2)
               ),

    cost(AST, Cost),

    \+ Cost = inf.

test(limit_distinct_start_select, []) :-
    AST = limit(100,
		        start(0,
				      distinct([v(docKey)],
						       select([ v(docKey),
								        v(label)
								      ],
								      ( t(v(docKey),
									      rdf : type,
									      '@schema' : 'Entity'),
								        ( opt(t(v(docKey),
										        '@schema' : label,
										        v(label)))
                                        ; opt(v(docKey) = v(label))
								        ),
								        concat([ v(label),
										         v(docKey)
										       ],
										       v(regex))
								      ))))),
    cost(AST, Cost),
    \+ Cost = inf.

test(cost_of_negation_binding, []) :-
    AST = not((v(type) = rdf:'List')),
    cost(AST, Cost),
    Cost = inf.

test(cost_of_supplied_binding, []) :-
    AST = not((t(v(id), rdf:type, v(type)),
               v(type) = rdf:'List')),
    cost(AST, Cost),
    \+ Cost = inf.

:- end_tests(mode).

:- begin_tests(woql_injection).

test(not_there, []) :-

    JSON = _{'@documentation':_{'@comment':"Specify an edge pattern in the graph.", '@properties':_{graph:"An optional graph (either 'instance' or 'schema')", object:"A URI, datatype or variable which is the target or object of the graph edge.", predicate:"A URI or variable which is the edge-label or predicate of the graph edge.", subject:"A URI or variable which is the source or subject of the graph edge."}}, '@id':"Triple", '@inherits':"Query", '@key':_{'@type':"ValueHash"}, '@type':"Class", graph:_{'@class':"xsd:string", '@type':"Optional"}, object:"Value", predicate:"NodeValue", subject:"NodeValue"},
    inject_woql_info(JSON, New),
    get_dict('@metadata', New, Metadata),
    Metadata = metadata{
                   'https://terminusdb.com':
                   _{ fields:[ subject,
							   predicate,
							   object,
							   optional(graph)
							 ],
					  mode:[ ?,
							 ?,
							 ?,
							 +
						   ],
					  types:[ node,
							  node,
							  value,
							  graph
							]
					}
			   }.

test(already_there, []) :-
    JSON = _{'@documentation':_{'@comment':"Specify an edge pattern in the graph.", '@properties':_{graph:"An optional graph (either 'instance' or 'schema')", object:"A URI, datatype or variable which is the target or object of the graph edge.", predicate:"A URI or variable which is the edge-label or predicate of the graph edge.", subject:"A URI or variable which is the source or subject of the graph edge."}}, '@id':"Triple", '@inherits':"Query", '@key':_{'@type':"ValueHash"}, '@type':"Class", graph:_{'@class':"xsd:string", '@type':"Optional"}, object:"Value", predicate:"NodeValue", subject:"NodeValue", '@metadata' : _{ my_own_values: ["asdf", "fdsa"]}},
    inject_woql_info(JSON, New),
    get_dict('@metadata', New, Metadata),
    Metadata = metadata{
                   my_own_values: ["asdf", "fdsa"],
                   'https://terminusdb.com':
                   _{ fields:[ subject,
							   predicate,
							   object,
							   optional(graph)
							 ],
					  mode:[ ?,
							 ?,
							 ?,
							 +
						   ],
					  types:[ node,
							  node,
							  value,
							  graph
							]
					}
			   }.

:- end_tests(woql_injection).
