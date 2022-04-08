:- module('document/diff',
          [simple_diff/4,
           start_state/1,
           patch_cost/2,
           best_cost/2,
           best_diff/2]).

:- use_module(core(util)).
:- use_module(core(util/tables)).
:- use_module(patch).

:- use_module(library(dicts)).
:- use_module(library(lists)).
:- use_module(library(plunit)).
:- use_module(library(thread)).
:- use_module(library(aggregate)).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- multifile table_diff/7.

simple_diff(Before,After,Keep,Diff) :-
    simple_diff(Before,After,Keep,_,Diff).

best_cost(best(Cost,_),Cost).
best_diff(best(_,Diff),Diff).

start_state(best(inf,json{})).

cost_bounded(State,Cost) :-
    best_cost(State,Best_Cost),
    Cost < Best_Cost.

simple_diff(Before,After,Keep,Cost,Diff) :-
    start_state(State),
    (   simple_diff(Before,After,Keep,New_Diff,State,0,New_Cost),
        nb_setarg(1,State,New_Cost),
        nb_setarg(2,State,New_Diff),
        %format(user_error,'~nNew_Cost: ~q', [New_Cost]),
        fail
    ;   best_cost(State, Cost),
        best_diff(State, Diff)
    ).

% semidet
simple_diff(Before,After,Keep,Diff,State,Cost,New_Cost) :-
    is_dict(Before),
    !,
    is_dict(After), % fail if not a dict
    dict_keys(Before,Before_Keys),
    dict_keys(After,After_Keys),
    % branch and bound
    best_cost(State,Best_Cost),
    Cost_Lower_Bound is Cost + 1,
    Cost_Lower_Bound < Best_Cost,
    %
    union(Before_Keys,After_Keys,Keys),
    simple_key_diff(Keys,Before,After,Keep,Diff_Pairs,State,Cost,New_Cost),
    dict_create(Diff,json,Diff_Pairs).
simple_diff(Before,After,Keep,Diff,State,Cost,New_Cost) :-
    (   is_table(Before)
    ;   is_table(After)),
    table_diff(Before,After,Keep,Diff,State,Cost,New_Cost),
    !.
simple_diff(Before,After,Keep,Diff,State,Cost,New_Cost) :-
    % null?
    is_list(Before),
    !,
    is_list(After),
    simple_list_diff(Before,After,Keep,Diff,State,Cost,New_Cost).
simple_diff(Before,After,_,_,_,_,_) :-
    % Copy is implicit
    string_normalise(Before, Value),
    string_normalise(After, Value),
    !,
    fail.
simple_diff(Before,After,_Keep,Diff,_State,Cost,New_Cost) :-
    Diff = json{ '@op' : "SwapValue",
                 '@before' : Before,
                 '@after' : After },
    New_Cost is Cost + 1.

string_normalise(Value, Norm) :-
    (   atom(Value), \+ memberchk(Value,[null,true,false])
    ->  atom_string(Value,Norm)
    ;   Value = Norm
    ).

simple_key_diff([],_Before,_After,_Keep,[],_State,Cost,Cost).
simple_key_diff([Key|Keys],Before,After,Keep,[Key-Value|Rest],State,Cost,New_Cost) :-
    get_dict(Key,Keep,true),
    !,
    do_or_die(
        (   get_dict(Key,Before,Before_Value),
            string_normalise(Before_Value, Value),
            get_dict(Key,After,After_Value),
            string_normalise(After_Value, Value)
        ),
        error(explicitly_copied_key_has_changed(Key),_)),
    simple_key_diff(Keys,Before,After,Keep,Rest,State,Cost,New_Cost).
simple_key_diff([Key|Keys],Before,After,Keep,New_Keys,State,Cost,New_Cost) :-
    get_dict(Key,Before,Sub_Before),
    get_dict(Key,After,Sub_After),
    !,
    (   get_dict(Key,Keep,Sub_Keep)
    ->  true
    ;   Sub_Keep = json{}),
    best_cost(State,Best_Cost),
    Cost_LB is Cost + 1,
    Cost_LB < Best_Cost,
    (   simple_diff(Sub_Before,Sub_After,Sub_Keep,Sub_Diff,State,Cost,Cost1)
    *-> (   \+ Sub_Diff = json{'@op':"KeepList"}
        ->  New_Keys = [Key-Sub_Diff|Rest]
        ;   New_Keys = Rest
        )
    ;   New_Keys = Rest,
        Cost = Cost1
    ),
    simple_key_diff(Keys,Before,After,Keep,Rest,State,Cost1,New_Cost).
simple_key_diff([Key|Keys],Before,After,Keep,[Key-Sub_Diff|Rest],State,Cost,New_Cost) :-
    get_dict(Key,Before,Sub_Before),
    % has it in the before but not the after.
    !,
    Sub_Diff = json{ '@op' : "SwapValue",
                     '@before' : Sub_Before,
                     '@after' : null },
    best_cost(State,Best_Cost),
    Cost1 is Cost + 1,
    Cost1 < Best_Cost,
    simple_key_diff(Keys,Before,After,Keep,Rest,State,Cost1,New_Cost).
simple_key_diff([Key|Keys],Before,After,Keep,[Key-Sub_Diff|Rest],State,Cost,New_Cost) :-
    get_dict(Key,After,Sub_After),
    % has it in the before but not the after.
    !,
    Sub_Diff = json{ '@op' : "SwapValue",
                     '@before' : null,
                     '@after' : Sub_After },
    best_cost(State,Best_Cost),
    Cost1 is Cost + 1,
    Cost1 < Best_Cost,
    simple_key_diff(Keys,Before,After,Keep,Rest,State,Cost1,New_Cost).

split(Index,List,Left,Right) :-
    length(Left, Index),
    append(Left,Right,List).

simple_list_diff(Before, After, Keep, Diff, State, Cost_In, Cost_Out) :-
    lcs_list_diff(Before, After, Keep, Diff, State, Cost_In, Cost_Out).
simple_list_diff(Before, After, Keep, Diff, State, Cost_In, Cost_Out) :-
    deep_list_diff(Before, After, Keep, Diff, State, Cost_In, Cost_Out).

deep_list_diff(Before, After, Keep, Diff, State, Cost_In, Cost_Out) :-
    first_solution([Diff,Cost_Out],
                   [
                       deep_list_diff_(Before,After,Keep,Diff,State,Cost_In,Cost_Out),
                       deep_list_timeout(Diff,Cost_Out)
                   ],
                   []).

deep_list_timeout(10).

deep_list_timeout(json{},inf) :-
    deep_list_timeout(Time),
    sleep(Time).

deep_list_diff_base(Same,Same,_Keep,Diff,State,Cost,New_Cost) :-
    Diff = json{ '@op' : "KeepList" },
    !,
    New_Cost is Cost + 1,
    cost_bounded(State,New_Cost).
deep_list_diff_base([],After,_Keep,Diff,State,Cost,New_Cost) :-
    !,
    Diff = json{ '@op' : "SwapList",
                 '@before' : [],
                 '@after' : After },
    json_size(After,Size),
    New_Cost is Cost + Size + 1,
    cost_bounded(State,New_Cost).
deep_list_diff_base(Before,[],_Keep,Diff,State,Cost,New_Cost) :-
    !,
    Diff = json{ '@op' : "SwapList",
                 '@before' : Before,
                 '@after' : [] },
    json_size(Before,Size),
    New_Cost is Cost + Size + 1,
    cost_bounded(State,New_Cost).
deep_list_diff_base(Before,After,Keep,Diff,State,Cost,New_Cost) :-
    length(Before, Length),
    length(After, Length),
    mapm({State,Keep}/[B,A,D]>>simple_diff(B,A,Keep,D,State),
         Before,
         After,
         Diff,
         Cost,
         New_Cost
        ),
    cost_bounded(State,New_Cost).

deep_list_diff_(Before,After,Keep,Diff,State,Cost,New_Cost) :-
    deep_list_diff_base(Before,After,Keep,Simple_Diff,State,Cost,New_Cost),
    !,
    (   is_list(Simple_Diff)
    ->  Simple_Diff = Diff
    ;   json{ '@op' : "SwapList" } :< Simple_Diff
    ->  put_dict(json{'@rest' : json{'@op' : "KeepList"}}, Simple_Diff, Diff)
    ;   Simple_Diff = Diff
    ).
deep_list_diff_(Before,After,Keep,Diff,State,Cost,New_Cost) :-
    length(Before,N),
    %between(0,N,I),
    down_from(N,0,I),
    length(After,M),
    %between(0,M,J),
    down_from(M,0,J),

    % Something must be getting smaller.
    (   I < N
    ;   J < M
    ),
    % And we don't want to do nothing.
    \+ (   N = 0,
           M = 0),

    split(I,Before,Before_Prefix,Before_Suffix),
    split(J,After,After_Prefix,After_Suffix),

    (   I = J,
        Before_Prefix = After_Prefix
    ->  Cost_Lower_Bound is Cost + 1,
        cost_bounded(State,Cost_Lower_Bound),
        Cost1 is Cost + 1,
        deep_list_diff_(Before_Suffix,After_Suffix,Keep,Patch,State,Cost1,New_Cost),
        Diff = json{ '@op' : "CopyList",
                     '@to' : I,
                     '@rest' : Patch }
    ;   Cost1 is Cost + 1,
        Cost_Lower_Bound is Cost1 + 1,
        cost_bounded(State,Cost_Lower_Bound),
        deep_list_diff_base(Before_Prefix,After_Prefix,Keep,Prefix_Patch,State,Cost1,Cost2),
        Cost3 is Cost2 + 1,
        cost_bounded(State,Cost3),
        deep_list_diff_(Before_Suffix,After_Suffix,Keep,Suffix_Patch,State,Cost3,New_Cost),
        (   is_list(Prefix_Patch)
        ->  Diff = json{ '@op' : "PatchList",
                         '@patch' : Prefix_Patch,
                         '@rest' : Suffix_Patch }
        ;   json{ '@op' : "SwapList" } :< Prefix_Patch,
            put_dict(json{'@rest' : Suffix_Patch}, Prefix_Patch, Diff)
        )
    ).

hash_terms([], []).
hash_terms([T|Rest], [H|HashRest]) :-
    variant_sha1(T,H),
    hash_terms(Rest,HashRest).

create_patch([], [], [], inserted, Memory, Patch) :-
    reverse(Memory,List),
    Patch = json{ '@op' : "SwapList",
                  '@before' : [],
                  '@after' : List,
                  '@rest' : json{ '@op' : "KeepList" }}.
create_patch([], [], [], deleted, Memory, Patch) :-
    reverse(Memory,List),
    Patch = json{ '@op' : "SwapList",
                  '@before' : List,
                  '@after' : [],
                  '@rest' : json{ '@op' : "KeepList" }}.
create_patch([], [], [], unchanged, _, json{ '@op' : "KeepList" }).
create_patch([unchanged|Operations],[Head|List1],[Head|List2],unchanged, Memory, Patch) :-
    !,
    create_patch(Operations,List1,List2,unchanged,[Head|Memory],Patch).
create_patch([inserted|Operations],List1,[Head|List2],inserted,Memory,Patch) :-
    !,
    create_patch(Operations,List1,List2,inserted,[Head|Memory],Patch).
create_patch([deleted|Operations],[Head|List1],List2,deleted,Memory,Patch) :-
    !,
    create_patch(Operations,List1,List2,deleted,[Head|Memory],Patch).
create_patch([unchanged|Operations],[Head|List1],[Head|List2],Last_Op,Memory,Patch) :-
    new_operation_patch(Last_Op,Memory,Patch,Rest_Patch),
    create_patch(Operations,List1,List2,unchanged,[Head],Rest_Patch).
create_patch([inserted|Operations],List1,[Head|List2],Last_Op,Memory,Patch) :-
    new_operation_patch(Last_Op,Memory,Patch,Rest_Patch),
    create_patch(Operations,List1,List2,inserted,[Head],Rest_Patch).
create_patch([deleted|Operations],[Head|List1],List2,Last_Op,Memory,Patch) :-
    new_operation_patch(Last_Op,Memory,Patch,Rest_Patch),
    create_patch(Operations,List1,List2,deleted,[Head],Rest_Patch).

new_operation_patch(inserted,Memory,Patch,Next) :-
    reverse(Memory,List),
    Patch = json{ '@op' : "SwapList",
                  '@before' : [],
                  '@after' : List,
                  '@rest' : Next }.
new_operation_patch(deleted,Memory,Patch,Next) :-
    reverse(Memory,List),
    Patch = json{ '@op' : "SwapList",
                  '@before' : List,
                  '@after' : [],
                  '@rest' : Next }.
new_operation_patch(unchanged,Memory,Patch,Next) :-
    length(Memory, N),
    Patch = json{ '@op' : "CopyList",
                  '@to' : N,
                  '@rest' : Next }.

create_patch([], [], [], json{ '@op' : "KeepList" }).
create_patch([unchanged|Operations],[Head|List1],[Head|List2],Patch) :-
    create_patch(Operations,List1,List2,unchanged,[Head],Patch).
create_patch([inserted|Operations],List1,[Head|List2],Patch) :-
    create_patch(Operations,List1,List2,inserted,[Head],Patch).
create_patch([deleted|Operations],[Head|List1],List2,Patch) :-
    create_patch(Operations,List1,List2,deleted,[Head],Patch).

lcs_list_diff(Before,After,_Keep,Patch,State,Cost,New_Cost) :-
    hash_terms(Before,Before_Hash),
    hash_terms(After,After_Hash),
    '$lcs':list_diff(Before_Hash,After_Hash,Changed),
    create_patch(Changed,Before,After,Patch),
    patch_cost(Patch,Patch_Cost),
    New_Cost is Cost + Patch_Cost,
    cost_bounded(State,New_Cost).

% Attempts to estimate a cost for a patch
patch_cost(Patch,Cost) :-
    is_dict(Patch),
    !,
    (   diff_op(Patch,Op)
    ->  patch_cost_op(Op,Patch,Cost)
    %   This is a copy - but may have deep patches
    ;   Closure = [Dict,C]>>(   C = 0
                            ;   dict_keys(Dict,Keys),
                                member(K,Keys),
                                get_dict(K,Dict,Val),
                                patch_cost(Val, C)
                            ),
        aggregate(sum(C),call(Closure,Patch,C),Cost)
    ).
patch_cost(Patch,Cost) :-
    is_list(Patch),
    !,
    Closure = [P,C]>>(   C = 0
                     ;   member(Elt,P),
                         patch_cost(Elt, C)
                     ),
    aggregate(sum(C),call(Closure,Patch,C),Cost).
% This is an explicit copy.
patch_cost(_Patch,0).

patch_cost_op('Insert',Patch,Cost) :-
    get_dict_or_null('@insert', Patch, Doc),
    json_size(Doc, Insert_Cost),
    Cost is Insert_Cost + 1.
patch_cost_op('Delete',Patch,Cost) :-
    get_dict_or_null('@delete', Patch, Doc),
    json_size(Doc, Insert_Cost),
    Cost is Insert_Cost + 1.
patch_cost_op('SwapValue',Patch,Cost) :-
    % Should this look at the size?
    get_dict_or_null('@before', Patch, Before),
    get_dict_or_null('@after', Patch, After),
    json_size(Before,Before_Size),
    json_size(After,After_Size),
    Cost is Before_Size + After_Size + 1.
patch_cost_op('ForceValue',_Patch,1).
patch_cost_op('CopyList',Patch,Cost) :-
    get_dict('@rest', Patch, Rest),
    patch_cost(Rest,Sub_Cost),
    Cost is Sub_Cost + 1.
patch_cost_op('SwapList',Patch,Cost) :-
    get_dict('@before', Patch, Before),
    get_dict('@after', Patch, After),
    get_dict('@rest', Patch, Rest),
    patch_cost(Rest,Sub_Cost),
    json_size(Before,Before_Size),
    json_size(After,After_Size),
    Cost is Before_Size + After_Size + Sub_Cost + 1.
patch_cost_op('PatchList',Patch,Cost) :-
    get_dict('@patch', Patch, Sub_Patch),
    get_dict('@rest', Patch, Rest),
    Closure = [SP,C]>>(   C = 0
                      ;   member(P,SP),
                          patch_cost(P,C)
                      ),
    aggregate(sum(C),call(Closure,Sub_Patch,C), List_Cost),
    patch_cost(Rest,Rest_Cost),
    Cost is Rest_Cost + List_Cost + 2.
patch_cost_op('KeepList',_Patch,1).
patch_cost_op('CopyTable',Patch,Cost) :-
    get_dict('@bottom_left', Patch, BL_Patch),
    get_dict('@top_right', Patch, TR_Patch),
    get_dict('@bottom_right', Patch, BR_Patch),
    patch_cost(BL_Patch, BL_Cost),
    patch_cost(TR_Patch, TR_Cost),
    patch_cost(BR_Patch, BR_Cost),
    Cost is 1 + BL_Cost + TR_Cost + BR_Cost.
patch_cost_op('SwapTable', Patch, Cost) :-
    get_dict('@before', Patch, Before),
    get_dict('@after', Patch, After),
    get_dict('@bottom_left', Patch, BL_Patch),
    get_dict('@top_right', Patch, TR_Patch),
    get_dict('@bottom_right', Patch, BR_Patch),
    json_size(Before, Size_Before),
    json_size(After, Size_After),
    patch_cost(BL_Patch, BL_Cost),
    patch_cost(TR_Patch, TR_Cost),
    patch_cost(BR_Patch, BR_Cost),
    Cost is 1 + Size_Before + Size_After + BL_Cost + TR_Cost + BR_Cost.
patch_cost_op('PatchTable', Patch, Cost) :-
    get_dict('@top_left', Patch, TL_Patch),
    get_dict('@bottom_left', Patch, BL_Patch),
    get_dict('@top_right', Patch, TR_Patch),
    get_dict('@bottom_right', Patch, BR_Patch),
    patch_cost(TL_Patch, TL_Cost),
    patch_cost(BL_Patch, BL_Cost),
    patch_cost(TR_Patch, TR_Cost),
    patch_cost(BR_Patch, BR_Cost),
    Cost is 1 + TL_Cost + BL_Cost + TR_Cost + BR_Cost.
patch_cost_op('ModifyTable',Patch,Cost) :-
    get_dict(copies,Patch,Copies),
    get_dict(moves,Patch,Moves),
    get_dict(inserts,Patch,Inserts),
    get_dict(deletes,Patch,Deletes),
    length(Copies, Copy_Cost),
    length(Moves, Move_Cost),
    Closure = [List,S]>>(   S = 0
                        ;   member(Elt,List),
                            get_dict('@value', Elt, Value),
                            json_size(Value,S)
                        ),
    aggregate(sum(S),call(Closure,Inserts,S), Insert_Cost),
    aggregate(sum(S),call(Closure,Deletes,S), Delete_Cost),
    Cost is Copy_Cost + Move_Cost + Insert_Cost + Delete_Cost.
patch_cost_op('KeepTable',_Patch,1).

json_size(Dict,Size) :-
    is_dict(Dict),
    !,
    Closure = [Dict,S]>>(   S = 0
                        ;   dict_keys(Dict,Keys),
                            member(K,Keys),
                            get_dict(K,Dict,Val),
                            json_size(Val,Sub),
                            S is Sub + 1
                        ),
    aggregate(sum(S), call(Closure,Dict,S), Size).
json_size(List,Size) :-
    is_list(List),
    !,
    Closure = [List,S]>>(   S = 0
                        ;   member(O,List),
                            json_size(O,Sub),
                            S is Sub + 1
                        ),
    aggregate(sum(S), call(Closure,List,S), Size).
json_size(_,1).

:- begin_tests(simple_diff).

test(simple_diff, []) :-

    Before = json{ '@id' : "Person/Ludwig",
                   '@type' : "Person",
                   name : "Ludwig"
                 },
    After = json{'@id':"Person/Ludwig",
                 '@type':"Person",
                 name:"Ludo"
                },
    simple_diff(Before,After,json{},Patch),
    Patch = json{name:json{'@after':"Ludo",
                           '@before':"Ludwig",
                           '@op':"SwapValue"}
                }.

test(simple_diff_id, []) :-

    Before = json{ '@id' : "Person/Ludwig",
                   '@type' : "Person",
                   name : "Ludwig"
                 },
    After = json{'@id':"Person/Ludwig",
                 '@type':"Person",
                 name:"Ludo"
                },
    simple_diff(Before,After,json{'@id' : true},Patch),
    Patch = json{'@id':"Person/Ludwig",
                 name:json{'@after':"Ludo",
                           '@before':"Ludwig",
                           '@op':"SwapValue"}
                }.

test(simple_diff_deep_value, []) :-

    Before = json{ '@id' : "Person/Ludwig",
                   '@type' : "Person",
                   name : "Ludwig",
                   address : json{ '@id' : "Person/Ludwig/Address/addresses/1",
                                   '@type' : "Address",
                                   address1 : "Mölker Bastei 8",
                                   address2 : null,
                                   city : "Vienna",
                                   country : "Austria"}
                 },
    After = json{'@id':"Person/Ludwig",
                 '@type':"Person",
                 name:"Ludo",
                 address : json{ '@id' : "Person/Ludwig/Address/addresses/1",
                                 '@type' : "Address",
                                 address1 : "Mölker Bastei 8",
                                 address2 : null,
                                 city : "Vienna",
                                 country : "Austria"}
                },
    simple_diff(Before,After,json{address : json{ city: true }},Patch),
    Patch = json{ name:json{'@after':"Ludo",
                            '@before':"Ludwig",
                            '@op':"SwapValue"},
                  address: json{ city : "Vienna" }
                }.

test(simple_diff_error, [
         error(explicitly_copied_key_has_changed('@id'),
               _)
     ]) :-

    Before = json{ '@id' : "Person/Ludwig",
                   '@type' : "Person",
                   name : "Ludwig"
                 },
    After = json{'@id':"Person/Ludwig_the_third",
                 '@type':"Person",
                 name:"Ludo"
                },
    simple_diff(Before,After,json{'@id' : true},Patch),
    Patch = json{'@id':"Person/Ludwig",
                 name:json{'@after':"Ludo",
                           '@before':"Ludwig",
                           '@op':"SwapValue"}
                }.

test(introduce_drop, []) :-
    Before = json{asdf:"fdsa"},
    After = json{name:"Ludo"},
    simple_diff(Before,After,json{},Patch),
    Patch = json{asdf:json{'@after':null,
                           '@before':"fdsa",
                           '@op':"SwapValue"},
                 name:json{'@after':"Ludo",
                           '@before':null,
                           '@op':"SwapValue"}}.

test(introduce_deep, []) :-
    Before = json{},
    After = json{name:json{asdf:"Ludo"}},
    simple_diff(Before,After,json{},Patch),
    Patch = json{name:json{'@after':json{asdf:"Ludo"},
                           '@before':null,
                           '@op':"SwapValue"}}.

test(simple_list_diff, []) :-
    List1 = [1],
    List2 = [1,2],
    simple_diff(List1,List2,json{},Diff),
    Diff = json{'@op':"CopyList",
                '@to':1,
                '@rest':json{'@op':"SwapList",
                             '@before':[],
                             '@after':[2],
                             '@rest':json{'@op':"KeepList"}
                            }}.

test(simple_list_diff_middle, []) :-
    List1 = [1,2,3],
    List2 = [1,3],
    simple_diff(List1,List2,json{},Diff),
    % This does not seem ideal...
    Diff = json{'@op':"CopyList",
                '@rest':json{'@after':[],
                             '@before':[2],
                             '@op':"SwapList",
                             '@rest':json{'@op':"KeepList"}},
                '@to':1}.

test(deep_list_diff_append, []) :-

    Before = json{ '@id' : "Person/Ludwig",
                   '@type' : "Person",
                   name : "Ludwig",
                   addresses : [
                       json{ '@id' : "Person/Ludwig/Address/addresses/1",
                             '@type' : "Address",
                             address1 : "Mölker Bastei 8",
                             address2 : null,
                             city : "Vienna",
                             country : "Austria"}
                   ]
                 },
    After = json{'@id':"Person/Ludwig",
                 '@type':"Person",
                 addresses: [
                     json{'@id':"Person/Ludwig/Address/addresses/1",
                          '@type':"Address",
                          address1:"Mölker Bastei 8",
                          address2:null,
                          city:"Vienna",
                          country:"Austria"},
                     json{'@id':"Person/Ludwig/Address/addresses/2",
                          '@type':"Address",
                          address1:"Probusgasse 6",
                          address2:null,
                          city:"Vienna",
                          country:"Austria"}],
                 name:"Ludwig"},

    simple_diff(Before,After,json{},Patch),

    Patch = json{addresses:
                 json{'@op':"CopyList",
                      '@to':1,
                      '@rest':
                      json{'@after':[json{'@id':"Person/Ludwig/Address/addresses/2",'@type':"Address",address1:"Probusgasse 6",address2:null,city:"Vienna",country:"Austria"}],
                           '@before':[],
                           '@op':"SwapList",
                           '@rest':json{'@op':"KeepList"}}}}.

test(deep_list_diff, [blocked('should start working with better list heuristic')]) :-

    Before = json{ '@id' : "Person/Ludwig",
                   '@type' : "Person",
                   name : "Ludwig",
                   addresses : [
                       json{ '@id' : "Person/Ludwig/Address/addresses/1",
                             '@type' : "Address",
                             address1 : "Mölker Bastei 7",
                             address2 : null,
                             city : "Vienna",
                             country : "Austria"}
                   ]
                 },
    After = json{'@id':"Person/Ludwig",
                 '@type':"Person",
                 name:"Ludwig",
                 addresses: [
                     json{'@id':"Person/Ludwig/Address/addresses/1",
                          '@type':"Address",
                          address1:"Mölker Bastei 8",
                          address2:null,
                          city:"Vienna",
                          country:"Austria"}
                 ]
                },

    simple_diff(Before,After,json{},Patch),
    Patch = json{addresses:[
                     json{address1:json{'@after':"Mölker Bastei 8",
                                        '@before':"Mölker Bastei 7",
                                        '@op':"SwapValue"}}]
                }.

test(list_middle, []) :-

    Before = json{ asdf : "fdsa",
                   bar : "baz",
                   list : [1,2,3,5,6] },
    After = json{ asdf : "ooo",
                  bar : "bazz",
                  list : [1,2,3,4,5,6] },
    simple_diff(Before,After,json{},Patch),
    Patch = json{asdf:json{'@after':"ooo",
                           '@before':"fdsa",
                           '@op':"SwapValue"},
                 bar:json{'@after':"bazz",
                          '@before':"baz",
                          '@op':"SwapValue"},
                 list:json{'@op':"CopyList",
                           '@rest':json{'@after':[4],'@before':[],
                                        '@op':"SwapList",
                                        '@rest':json{'@op':"KeepList"}},
                           '@to':3}}.

:- use_module(core('document/patch')).

test(deep_list_patch, []) :-
    Before = json{ asdf: json{ bar: [json{ baz: 'quux' }] } },
    After = json{ asdf: json{ bar: [json{ baz: 'quuz' }] } },
    simple_diff(Before,After,json{},Diff),
    simple_patch(Diff,Before,After).

:- end_tests(simple_diff).
