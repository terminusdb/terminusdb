:- module('document/diff',
          [simple_diff/4]).

:- use_module(core(util)).

:- use_module(library(dicts)).
:- use_module(library(lists)).

simple_diff(Before,After,Keep,Diff) :-
    simple_diff(Before,After,Keep,_,Diff).

best_cost(best(Cost,_),Cost).
best_diff(best(_,Diff),Diff).

down_from(From,To,X) :-
    between(To,From,Y),
    X is From - Y.

simple_diff(Before,After,Keep,Cost,Diff) :-
    State = best(inf,_{}),
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
    dict_create(Diff,_,Diff_Pairs).
simple_diff(Before,After,Keep,Diff,State,Cost,New_Cost) :-
    % null?
    is_list(Before),
    !,
    is_list(After),
    simple_list_diff(Before,After,Keep,Diff,State,Cost,New_Cost).
simple_diff(Same,Same,_,_,_,_,_) :-
    % Copy is implicit
    !,
    fail.
simple_diff(Before,After,_Keep,Diff,_State,Cost,New_Cost) :-
    Diff = _{ '@op' : "SwapValue",
              '@before' : Before,
              '@after' : After },
    New_Cost is Cost + 1.

simple_key_diff([],_Before,_After,_Keep,[],_State,Cost,Cost).
simple_key_diff([Key|Keys],Before,After,Keep,[Key-Value|Rest],State,Cost,New_Cost) :-
    get_dict(Key,Keep,true),
    !,
    do_or_die(
        (   get_dict(Key,Before,Value),
            get_dict(Key,After,Value)
        ),
        error(explicitly_copied_key_has_changed(Key),_)),
    simple_key_diff(Keys,Before,After,Keep,Rest,State,Cost,New_Cost).
simple_key_diff([Key|Keys],Before,After,Keep,New_Keys,State,Cost,New_Cost) :-
    get_dict(Key,Before,Sub_Before),
    get_dict(Key,After,Sub_After),
    !,
    (   get_dict(Key,Keep,Sub_Keep)
    ->  true
    ;   Sub_Keep = _{}),
    best_cost(State,Best_Cost),
    Cost_LB is Cost + 1,
    Cost_LB < Best_Cost,
    (   simple_diff(Sub_Before,Sub_After,Sub_Keep,Sub_Diff,State,Cost,Cost1)
    *-> (   \+ Sub_Diff = _{'@op':"KeepList"}
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
    Sub_Diff = _{ '@op' : "SwapValue",
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
    Sub_Diff = _{ '@op' : "SwapValue",
                  '@before' : null,
                  '@after' : Sub_After },
    best_cost(State,Best_Cost),
    Cost1 is Cost + 1,
    Cost1 < Best_Cost,
    simple_key_diff(Keys,Before,After,Keep,Rest,State,Cost1,New_Cost).

split(Index,List,Left,Right) :-
    length(Left, Index),
    append(Left,Right,List).

simple_list_diff(Before,After,_Keep, Diff, _State, Cost, Cost) :-
    lcs_list_diff(Before,After,_, Diff, _, Cost, Cost).

/*
simple_list_diff_base(Same,Same,_Keep,Diff,State,Cost,New_Cost) :-
    Diff = _{ '@op' : "KeepList" },
    !,
    best_cost(State,Best_Cost),
    New_Cost is Cost + 1,
    New_Cost < Best_Cost.
simple_list_diff_base([],After,_Keep,Diff,State,Cost,New_Cost) :-
    !,
    Diff = _{ '@op' : "SwapList",
              '@before' : [],
              '@after' : After },
    best_cost(State,Best_Cost),
    length(After,Length),
    New_Cost is Cost + Length + 1,
    New_Cost < Best_Cost.
simple_list_diff_base(Before,[],_Keep,Diff,State,Cost,New_Cost) :-
    !,
    Diff = _{ '@op' : "SwapList",
              '@before' : Before,
              '@after' : [] },
    best_cost(State,Best_Cost),
    length(Before,Length),
    New_Cost is Cost + Length + 1,
    New_Cost < Best_Cost.
simple_list_diff_base(Before,After,Keep,Diff,State,Cost,New_Cost) :-
    length(Before, Length),
    length(After, Length),
    mapm({State,Keep}/[B,A,D]>>simple_diff(B,A,Keep,D,State),
         Before,
         After,
         Diff,
         Cost,
         New_Cost
        ),
    best_cost(State,Best_Cost),
    New_Cost < Best_Cost.

simple_list_diff(Before,After,Keep,Diff,State,Cost,New_Cost) :-
    simple_list_diff_base(Before,After,Keep,Simple_Diff,State,Cost,New_Cost),
    !,
    (   is_list(Simple_Diff)
    ->  Simple_Diff = Diff
    ;   _{ '@op' : "SwapList" } :< Simple_Diff
    ->  put_dict(_{'@rest' : _{'@op' : "KeepList"}}, Simple_Diff, Diff)
    ;   Simple_Diff = Diff
    ).
simple_list_diff(Before,After,Keep,Diff,State,Cost,New_Cost) :-
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

    % branch and bound
    best_cost(State,Best_Cost),
    (   I = J,
        Before_Prefix = After_Prefix
    ->  Cost_Lower_Bound is Cost + 1,
        Cost_Lower_Bound < Best_Cost,
        Cost1 is Cost + 1,
        simple_list_diff(Before_Suffix,After_Suffix,Keep,Patch,State,Cost1,New_Cost),
        Diff = _{ '@op' : "CopyList",
                  '@to' : I,
                  '@rest' : Patch }
    ;   Cost1 is Cost + 1,
        Cost_Lower_Bound is Cost1 + 1,
        Cost_Lower_Bound < Best_Cost,
        simple_list_diff_base(Before_Prefix,After_Prefix,Keep,Prefix_Patch,State,Cost1,Cost2),
        Cost3 is Cost2 + 1,
        Cost3 < Best_Cost,
        simple_list_diff(Before_Suffix,After_Suffix,Keep,Suffix_Patch,State,Cost3,New_Cost),
        (   is_list(Prefix_Patch)
        ->  Diff = _{ '@op' : "PatchList",
                      '@patch' : Prefix_Patch,
                      '@rest' : Suffix_Patch }
        ;   _{ '@op' : "SwapList" } :< Prefix_Patch,
            put_dict(_{'@rest' : Suffix_Patch}, Prefix_Patch, Diff)
        )
    ).
*/

hash_terms([], []).
hash_terms([T|Rest], [H|HashRest]) :-
    variant_sha1(T,H),
    hash_terms(Rest,HashRest).

create_patch([], [], [], inserted, Memory, Patch) :-
    reverse(Memory,List),
    Patch = json{ '@op' : "SwapList",
                  '@before' : [],
                  '@after' : List,
                  '@rest' : _{ '@op' : "KeepList" }}.
create_patch([], [], [], deleted, Memory, Patch) :-
    reverse(Memory,List),
    Patch = json{ '@op' : "SwapList",
                  '@before' : List,
                  '@after' : [],
                  '@rest' : _{ '@op' : "KeepList" }}.
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

lcs_list_diff(Before,After,_Keep,Patch,_State,Cost,Cost) :-
    hash_terms(Before,Before_Hash),
    hash_terms(After,After_Hash),
    lcs:list_diff(Before_Hash,After_Hash,Changed),
    create_patch(Changed,Before,After,Patch).

:- begin_tests(simple_diff).

test(simple_diff, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig"
              },
    After = _{'@id':"Person/Ludwig",
              '@type':"Person",
              name:"Ludo"
             },
    simple_diff(Before,After,_{},Patch),
    Patch = _{name:_{'@after':"Ludo",
                     '@before':"Ludwig",
                     '@op':"SwapValue"}
             }.

test(simple_diff_id, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig"
              },
    After = _{'@id':"Person/Ludwig",
              '@type':"Person",
              name:"Ludo"
             },
    simple_diff(Before,After,_{'@id' : true},Patch),
    Patch = _{'@id':"Person/Ludwig",
              name:_{'@after':"Ludo",
                     '@before':"Ludwig",
                     '@op':"SwapValue"}
             }.

test(simple_diff_deep_value, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig",
                address : _{ '@id' : "Person/Ludwig/Address/addresses/1",
                             '@type' : "Address",
                             address1 : "Mölker Bastei 8",
                             address2 : null,
                             city : "Vienna",
                             country : "Austria"}
              },
    After = _{'@id':"Person/Ludwig",
              '@type':"Person",
              name:"Ludo",
              address : _{ '@id' : "Person/Ludwig/Address/addresses/1",
                           '@type' : "Address",
                           address1 : "Mölker Bastei 8",
                           address2 : null,
                           city : "Vienna",
                           country : "Austria"}
             },
    simple_diff(Before,After,_{address : _{ city: true }},Patch),
    Patch = _{ name:_{'@after':"Ludo",
                      '@before':"Ludwig",
                      '@op':"SwapValue"},
               address: _{ city : "Vienna" }
             }.

test(simple_diff_error, [
         error(explicitly_copied_key_has_changed('@id'),
               _)
     ]) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig"
              },
    After = _{'@id':"Person/Ludwig_the_third",
              '@type':"Person",
              name:"Ludo"
             },
    simple_diff(Before,After,_{'@id' : true},Patch),
    Patch = _{'@id':"Person/Ludwig",
              name:_{'@after':"Ludo",
                     '@before':"Ludwig",
                     '@op':"SwapValue"}
             }.

test(introduce_drop, []) :-
    Before = _{asdf:"fdsa"},
    After = _{name:"Ludo"},
    simple_diff(Before,After,_{},Patch),
    Patch = _{asdf:_{'@after':null,
                     '@before':"fdsa",
                     '@op':"SwapValue"},
              name:_{'@after':"Ludo",
                     '@before':null,
                     '@op':"SwapValue"}}.

test(introduce_deep, []) :-
    Before = _{},
    After = _{name:_{asdf:"Ludo"}},
    simple_diff(Before,After,_{},Patch),
    Patch = _{name:_{'@after':_{asdf:"Ludo"},
                     '@before':null,
                     '@op':"SwapValue"}}.

test(simple_list_diff, []) :-
    List1 = [1],
    List2 = [1,2],
    simple_diff(List1,List2,_{},Diff),
    Diff = _{'@op':"CopyList",
             '@to':1,
             '@rest':_{'@op':"SwapList",
                       '@before':[],
                       '@after':[2],
                       '@rest':_{'@op':"KeepList"}
                       }}.

test(simple_list_diff_middle, []) :-
    List1 = [1,2,3],
    List2 = [1,3],
    simple_diff(List1,List2,_{},Diff),
    % This does not seem ideal...
    Diff = _{'@op':"CopyList",
             '@rest':json{'@after':[],
                          '@before':[2],
                          '@op':"SwapList",
                          '@rest':json{'@op':"KeepList"}},
             '@to':1}.

test(deep_list_diff_append, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig",
                addresses : [
                    _{ '@id' : "Person/Ludwig/Address/addresses/1",
                       '@type' : "Address",
                       address1 : "Mölker Bastei 8",
                       address2 : null,
                       city : "Vienna",
                       country : "Austria"}
                ]
              },
    After = _{'@id':"Person/Ludwig",
              '@type':"Person",
              addresses: [
                  _{'@id':"Person/Ludwig/Address/addresses/1",
                    '@type':"Address",
                    address1:"Mölker Bastei 8",
                    address2:null,
                    city:"Vienna",
                    country:"Austria"},
                  _{'@id':"Person/Ludwig/Address/addresses/2",
                    '@type':"Address",
                    address1:"Probusgasse 6",
                    address2:null,
                    city:"Vienna",
                    country:"Austria"}],
              name:"Ludwig"},

    simple_diff(Before,After,_{},Patch),

    Patch = _{addresses:
              _{'@op':"CopyList",
                '@to':1,
                '@rest':
                _{'@after':[_{'@id':"Person/Ludwig/Address/addresses/2",'@type':"Address",address1:"Probusgasse 6",address2:null,city:"Vienna",country:"Austria"}],
                  '@before':[],
                  '@op':"SwapList",
                  '@rest':_{'@op':"KeepList"}}}}.

test(deep_list_diff, [blocked('should start working with better list heuristic')]) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig",
                addresses : [
                    _{ '@id' : "Person/Ludwig/Address/addresses/1",
                       '@type' : "Address",
                       address1 : "Mölker Bastei 7",
                       address2 : null,
                       city : "Vienna",
                       country : "Austria"}
                ]
              },
    After = _{'@id':"Person/Ludwig",
              '@type':"Person",
              name:"Ludwig",
              addresses: [
                  _{'@id':"Person/Ludwig/Address/addresses/1",
                    '@type':"Address",
                    address1:"Mölker Bastei 8",
                    address2:null,
                    city:"Vienna",
                    country:"Austria"}
              ]
             },

    simple_diff(Before,After,_{},Patch),
    Patch = _{addresses:[
                  _{address1:_{'@after':"Mölker Bastei 8",
                               '@before':"Mölker Bastei 7",
                               '@op':"SwapValue"}}]
             }.

test(list_middle, []) :-

    Before = _{ asdf : "fdsa",
                bar : "baz",
                list : [1,2,3,5,6] },
    After = _{ asdf : "ooo",
               bar : "bazz",
               list : [1,2,3,4,5,6] },
    simple_diff(Before,After,_{},Patch),
    Patch = _{asdf:_{'@after':"ooo",
                     '@before':"fdsa",
                     '@op':"SwapValue"},
              bar:_{'@after':"bazz",
                    '@before':"baz",
                    '@op':"SwapValue"},
              list:_{'@op':"CopyList",
                     '@rest':_{'@after':[4],'@before':[],
                               '@op':"SwapList",
                               '@rest':_{'@op':"KeepList"}},
                     '@to':3}}.

:- use_module(core('document/patch')).

test(deep_list_patch, []) :-
    Before = _{ asdf: _{ bar: [_{ baz: 'quux' }] } },
    After = _{ asdf: _{ bar: [_{ baz: 'quuz' }] } },
    simple_diff(Before,After,_{},Diff),
    simple_patch(Diff,Before,After).

:- end_tests(simple_diff).
