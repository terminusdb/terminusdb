:- module('document/diff',
          [simple_diff/3]).

:- use_module(core(util)).

simple_diff(Before,After,Diff) :-
    simple_diff(Before,After,_,Diff).

best_cost(best(Cost,_),Cost).
best_diff(best(_,Diff),Diff).

down_from(From,To,X) :-
    between(To,From,Y),
    X is From - Y.

simple_diff(Before,After,Cost,Diff) :-
    State = best(inf,_{}),
    (   simple_diff(Before,After,New_Diff,State,0,New_Cost),
        nb_setarg(1,State,New_Cost),
        nb_setarg(2,State,New_Diff),
        %format(user_error,'~nNew_Cost: ~q', [New_Cost]),
        fail
    ;   best_cost(State, Cost),
        best_diff(State, Diff)
    ).

% semidet
simple_diff(Before,After,Diff,State,Cost,New_Cost) :-
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
    simple_key_diff(Keys,Before,After,Diff_Pairs,State,Cost,New_Cost),
    dict_create(Diff,_,Diff_Pairs).
simple_diff(Before,After,Diff,State,Cost,New_Cost) :-
    % null?
    is_list(Before),
    !,
    is_list(After),
    simple_list_diff(Before,After,Diff,State,Cost,New_Cost).
simple_diff(Same,Same,_,_,_,_) :-
    % Copy is implicit
    !,
    fail.
simple_diff(Before,After,Diff,_State,Cost,New_Cost) :-
    Diff = _{ '@op' : "SwapValue",
              '@before' : Before,
              '@after' : After },
    size(Before,Size_Before),
    size(After,Size_After),
    New_Cost is Cost + Size_Before + Size_After.

size(JSON, Size) :-
    is_dict(JSON),
    !,
    dict_keys(JSON,Keys),
    aggregate(sum(S),
              Key^Sub^(   member(Key,Keys),
                          get_dict(Key,JSON,Sub),
                          size(Sub,S)
                      ),
              Total),
    length(Keys,L),
    Size is L + Total.
size(JSON, Size) :-
    is_list(JSON),
    !,
    aggregate(sum(S),
              M^(   member(M,JSON),
                    size(M,S)),
              Size).
size(_JSON, 1).

simple_key_diff([],_Before,_After,[],_State,Cost,Cost).
simple_key_diff([Key|Keys],Before,After,New_Keys,State,Cost,New_Cost) :-
    get_dict(Key,Before,Sub_Before),
    get_dict(Key,After,Sub_After),
    !,
    best_cost(State,Best_Cost),
    Cost_LB is Cost + 1,
    Cost_LB < Best_Cost,
    (   simple_diff(Sub_Before,Sub_After,Sub_Diff,State,Cost,Cost1)
    *-> (   \+ Sub_Diff = _{'@op':"KeepList"}
        ->  New_Keys = [Key-Sub_Diff|Rest]
        ;   New_Keys = Rest
        )
    ;   New_Keys = Rest,
        Cost = Cost1
    ),
    simple_key_diff(Keys,Before,After,Rest,State,Cost1,New_Cost).
simple_key_diff([Key|Keys],Before,After,[Key-Sub_Diff|Rest],State,Cost,New_Cost) :-
    get_dict(Key,Before,Sub_Before),
    % has it in the before but not the after.
    !,
    Sub_Diff = _{ '@op' : "SwapValue",
                  '@before' : Sub_Before,
                  '@after' : null },
    best_cost(State,Best_Cost),
    Cost1 is Cost + 1,
    Cost1 < Best_Cost,
    simple_key_diff(Keys,Before,After,Rest,State,Cost1,New_Cost).
simple_key_diff([Key|Keys],Before,After,[Key-Sub_Diff|Rest],State,Cost,New_Cost) :-
    get_dict(Key,After,Sub_After),
    % has it in the before but not the after.
    !,
    Sub_Diff = _{ '@op' : "SwapValue",
                  '@before' : null,
                  '@after' : Sub_After },
    best_cost(State,Best_Cost),
    Cost1 is Cost + 1,
    Cost1 < Best_Cost,
    simple_key_diff(Keys,Before,After,Rest,State,Cost1,New_Cost).

split(Index,List,Left,Right) :-
    length(Left, Index),
    append(Left,Right,List).

simple_list_diff_base(Same,Same,Diff,State,Cost,New_Cost) :-
    Diff = _{ '@op' : "KeepList" },
    !,
    best_cost(State,Best_Cost),
    New_Cost is Cost + 1,
    New_Cost < Best_Cost.
simple_list_diff_base([],After,Diff,State,Cost,New_Cost) :-
    !,
    Diff = _{ '@op' : "SwapList",
              '@before' : [],
              '@after' : After },
    best_cost(State,Best_Cost),
    length(After,Length),
    New_Cost is Cost + Length + 1,
    New_Cost < Best_Cost.
simple_list_diff_base(Before,[],Diff,State,Cost,New_Cost) :-
    !,
    Diff = _{ '@op' : "SwapList",
              '@before' : Before,
              '@after' : [] },
    best_cost(State,Best_Cost),
    length(Before,Length),
    New_Cost is Cost + Length + 1,
    New_Cost < Best_Cost.
simple_list_diff_base(Before,After,Diff,State,Cost,New_Cost) :-
    length(Before, Length),
    length(After, Length),
    mapm({State}/[B,A,D]>>simple_diff(B,A,D,State),
         Before,
         After,
         Diff,
         Cost,
         New_Cost
        ),
    best_cost(State,Best_Cost),
    New_Cost < Best_Cost.

simple_list_diff(Before,After,Diff,State,Cost,New_Cost) :-
    simple_list_diff_base(Before,After,Simple_Diff,State,Cost,New_Cost),
    !,
    (   is_list(Simple_Diff)
    ->  Simple_Diff = Diff
    ;   _{ '@op' : "SwapList" } :< Simple_Diff
    ->  put_dict(_{'@rest' : _{'@op' : "KeepList"}}, Simple_Diff, Diff)
    ;   Simple_Diff = Diff
    ).
simple_list_diff(Before,After,Diff,State,Cost,New_Cost) :-
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
        simple_list_diff(Before_Suffix,After_Suffix,Patch,State,Cost1,New_Cost),
        Diff = _{ '@op' : "CopyList",
                  '@to' : I,
                  '@rest' : Patch }
    ;   Cost1 is Cost + 1,
        Cost_Lower_Bound is Cost1 + 1,
        Cost_Lower_Bound < Best_Cost,
        simple_list_diff_base(Before_Prefix,After_Prefix,Prefix_Patch,State,Cost1,Cost2),
        Cost3 is Cost2 + 1,
        Cost3 < Best_Cost,
        simple_list_diff(Before_Suffix,After_Suffix,Suffix_Patch,State,Cost3,New_Cost),
        (   is_list(Prefix_Patch)
        ->  Diff = _{ '@op' : "PatchList",
                      '@patch' : Prefix_Patch,
                      '@rest' : Suffix_Patch }
        ;   _{ '@op' : "SwapList" } :< Prefix_Patch,
            put_dict(_{'@rest' : Suffix_Patch}, Prefix_Patch, Diff)
        )
    ).

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
    simple_diff(Before,After,Patch),
    Patch = _{name:_{'@after':"Ludo",
                     '@before':"Ludwig",
                     '@op':"SwapValue"}
             }.

test(introduce_drop, []) :-
    Before = _{asdf:"fdsa"},
    After = _{name:"Ludo"},
    simple_diff(Before,After,Patch),
    Patch = _{asdf:_{'@after':null,
                     '@before':"fdsa",
                     '@op':"SwapValue"},
              name:_{'@after':"Ludo",
                     '@before':null,
                     '@op':"SwapValue"}}.

test(introduce_deep, []) :-
    Before = _{},
    After = _{name:_{asdf:"Ludo"}},
    simple_diff(Before,After,Patch),
    Patch = _{name:_{'@after':_{asdf:"Ludo"},
                     '@before':null,
                     '@op':"SwapValue"}}.

test(simple_list_diff, []) :-
    List1 = [1],
    List2 = [1,2],
    simple_diff(List1,List2,Diff),
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
    simple_diff(List1,List2,Diff),
    % This does not seem ideal...
    Diff = _{'@op':"CopyList",
             '@rest':_{'@op':"PatchList",
                       '@patch':[_{'@after':3,'@before':2,'@op':"SwapValue"}],
                       '@rest':_{'@after':[],'@before':[3],'@op':"SwapList",
                                 '@rest':_{'@op':"KeepList"}}},'@to':1}.

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

    simple_diff(Before,After,Patch),

    Patch = _{addresses:
              _{'@op':"CopyList",
                '@to':1,
                '@rest':
                _{'@after':[_{'@id':"Person/Ludwig/Address/addresses/2",'@type':"Address",address1:"Probusgasse 6",address2:null,city:"Vienna",country:"Austria"}],
                  '@before':[],
                  '@op':"SwapList",
                  '@rest':_{'@op':"KeepList"}}}}.

test(deep_list_diff, []) :-

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

    simple_diff(Before,After,Patch),
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
    simple_diff(Before,After,Patch),
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
    simple_diff(Before,After,Diff),
    simple_patch(Diff,Before,After).

:- end_tests(simple_diff).

object_identifier(Obj,Hash) :-
    term_to_atom(Obj,Atom),
    crypto_data_hash(Atom, Hash, [algorithm(sha256)]).

list_to_identifier_list([],[]).
list_to_identifier_list([H|T],[H|Ids]) :-
    (   string(H)
    ;   atom(H)
    ;   number(H)),
    !,
    list_to_identifier_list(T,Ids).
list_to_identifier_list([H|T],[Id|Ids]) :-
    object_identifier(H,Id),
    list_to_identifier_list(T,Ids).

%%	lcs(+As:list, +Bs:list, -LCS:list) is det.
%
%	True if LCS is a longest common subsequence of As and Bs.
%	Elements =A= and =B= are can be common if =|A==B|=.
%
%	Implemented in terms of lcs/5.
lcs(A, B, LCS) :-
    lcs(equality_metric, A, B, LCS_Pairs, _Length),
    maplist(fst, LCS_Pairs, LCS).

fst(X-_, X).

% Place to stored memoized lcs/5 results
:- thread_local lcs_cache/3.

%%	lcs(+Cmp:callable,+As:list,+Bs:list,-LCS:list,-Length) is det.
%
%	True if LCS is a longest common subsequence of As and Bs.
%	LCS is a list of pairs =|A-B|= since Cmp allows non-identical
%	elements to be considered common.
%
%	Elements of As and Bs are compared by =|call(Cmp,A,B,Similarity)|=,
%	where larger =Similarity= values indicate more similar elements.
%	Length is the sum of similarity scores for elements in the
%	subsequence.
%
%	Implemented with memoization on top of a naive, exponential
%	algorithm.  It performs fairly well, but patches to use a better
%	algorithm are welcome.
:- meta_predicate lcs(3,+,+,-,-).
lcs(Cmp, As, Bs, LCS, Length) :-
    retractall(lcs_cache(_,_,_)),
    lcs_(Cmp,As,Bs,LCS,Length),
    retractall(lcs_cache(_,_,_)).

:- meta_predicate lcs_(3,+,+,-,-).
lcs_(Cmp, As, Bs, LCS, Length) :-
    term_hash((Cmp,As,Bs), Hash),
    lcs_cache(Hash, LCS, Length),
    !.
lcs_(Cmp,[A|As],[B|Bs],LCS,Length) :-
    !,
    call(Cmp, A, B, Similarity),
    lcs_(Cmp,   As ,   Bs ,LCS_AB, Length_AB0),
    lcs_(Cmp,   As ,[B|Bs],LCS_A,  Length_A),
    lcs_(Cmp,[A|As],   Bs ,LCS_B,  Length_B),
    Length_AB is Similarity + Length_AB0,
    ( Length_A >= Length_AB, Length_A >= Length_B ->
        LCS = LCS_A,
        Length is Length_A
    ; Length_B >= Length_AB, Length_B >= Length_A ->
        LCS = LCS_B,
        Length is Length_B
    ; otherwise ->
        LCS = [A-B|LCS_AB],
        Length = Length_AB
    ),
    term_hash((Cmp,[A|As],[B|Bs]), Hash),
    assert(lcs_cache(Hash, LCS, Length)).
lcs_(_,[],_,[],0) :- !.
lcs_(_,_,[],[],0).


%%	equality_metric(+A, +B, -Similarity) is det.
%
%	Similarity is 1 if =|A == B|=, otherwise 0.  This predicate
%	is helpful as the first argument to lcs/5.
equality_metric(A,B,Similarity) :-
    ( A==B -> Similarity=1
    ; true -> Similarity=0
    ).
