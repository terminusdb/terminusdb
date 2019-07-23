:- module(utils,[elt/2,
                 get_key/4,
                 get_key/3,
                 get_dict_default/4,
                 zip/3,
                 intersperse/3,
                 interpolate/2,
                 unique_solutions/3,
                 repeat_atom/3,
                 zero_pad/3,
                 exhaust/1,
                 take/3,
                 from_to/4,
                 drop/3,
                 truncate_list/4,
                 sfoldr/4,
                 trim/2,
                 split_atom/3,
                 op(920,fy, *),
                 '*'/1
                ]).

/** <module> Utils 
 * 
 * Utility predicates
 */ 

/*
 * Forget the next phrase.
 * 
 * Ueful for declarative debugging. 
 */ 
*_.

/** 
 * elt(+Key,+Set) is semidet.
 * 
 * This predicate is true when Key is in Set
 */
elt(Key,Set) :-
    (   member(Key,Set)
    ->  true
    ;   false).

/** 
 * get_key(+Key,+Object,-Val,+Default) is det. 
 * 
 * If Key=Val is a member of Object, we succeed with the given substitution, otherwise we 
 * assume Val=Default.
 */
get_key(Key,Object,Val,Default) :- 
    (   member(Key=Val, Object)
    ->  true 
    ;   Val = Default
    ).

/** 
 * get_key(+Key,+Object,-Val) is det. 
 * 
 * If Key=Val is a member of Object, we succeed with the given substitution, otherwise we 
 * throw an error. 
 */
get_key(Key,Object,Value) :-
	(   \+ ground(Object)
    ->  format(atom(M),'Arguments are not sufficiently bound for get_key/3 ~q=~q in ~q~n',
               [Key,Value,Object]),
        throw(http_reply(bad_request(M)))
    ;   member(Key=Value, Object)
    ->  true
	;   interpolate(['No "', Key,'" field specified as parameter in ', Object],M),
	    throw(http_reply(bad_request(M)))
    ).

/** 
 * zip(+A:list(T),+B:list(S),-C:list(pair(T,S))) is det.
 * zip(+A:list(T),-B:list(S),+C:list(pair(T,S))) is det.
 * zip(-A:list(T),+B:list(S),+C:list(pair(T,S))) is det.
 * 
 * Zip two lists into a list of pairs (or unzip, in the other two modes)
 */
%:- pred zip(list(T),list(V),list(pair(T,V))).
zip([A|RestA],[B|RestB],[(A-B)|Zip]) :-
    zip(RestA,RestB,Zip).
zip([],[],[]).


/** 
 * intersperse(+Item,+List,-Output) is det.
 * 
 * Puts an element between every element of a list.
 */
intersperse(_,[],[]).
intersperse(_,[X],[X]).
intersperse(Item,[X,Y|Rest],[X,Item|New_Rest]) :-
    intersperse(Item,[Y|Rest],New_Rest).

/** 
 * interpolate(L:list,A:atom) is det. 
 * 
 * Takes a list of values of mixed type, and appends them together as rendered atoms.
 */
interpolate([],'').
interpolate([H|T],S) :-
    atom(H),
    interpolate(T,Rest),
    atom_concat(H,Rest,S) , !.
interpolate([H|T],S) :-
    string(H), atom_string(C,H),
    interpolate(T,Rest),
    atom_concat(C,Rest,S) , !.
interpolate([H|T],S) :- 
    ground(H), term_to_atom(H,C),
    interpolate(T,Rest) ,
    atom_concat(C,Rest,S) , !.

/** 
 * unique_solutions(+Template,+Goal,-Collection) is det.
 * 
 * This implements the CORRECT semantics for setof. 
 * i.e. returns an empty list for failure to find solutions, rather than failing.
 */
unique_solutions(Template,Goal,Collection) :-
    (   setof(Template, Goal, CollectionX)
    ->  Collection=CollectionX
    ;   Collection=[]).

/** 
 * repeat_atom(+A:atom,+N:int,-L:list) is det.
 * 
 * Repeats an atom A, N times. 
 */
repeat_atom(_A,0,[]).
repeat_atom(A,N,[A|Z]) :-
	N > 0,
	N2 is N - 1,
	repeat_atom(A,N2,Z).

/** 
 * zero_pad(-A1:atom,Length:int,A2:atom) is det. 
 *
 * Pad the atom A1, with '0' to make it length Length. 
 */
zero_pad(A,L,A2) :-
	atom_chars(A,AtomList),
	length(AtomList, L1),
	L2 is L - L1,
	repeat_atom('0',L2,List), 
	append(List,AtomList,TotalList),
	atom_chars(A2,TotalList).

/** 
 * exhaust(+Goal:goal) is det. 
 * 
 * Run a goal through every possible solution, always succeeding.
 *
 */
:- meta_predicate exhaust(0).
exhaust(Goal) :-
    forall(Goal ; true, true).

/* 
 * take(+N:int,+List:list,-Out:list) is det.
 * 
 * Take the first elements of a list to min(length(List),N).
 */ 
take(N,List,Out) :- 
    (   N=0
    ->  Out=[]
    ;   NNext is N-1,
        (   List=[X|Rest]
        ->  Out=[X|Sub],
            take(NNext,Rest,Sub)
        ;   List = Out
        )
    ). 

/* 
 * from_to(+From:int,+To:int,+List:list,-Out) is det.
 */ 
from_to(From,To,List,Out) :-
    (   From=0
    ->  take(To,List,Out)
    ;   From > To
    ->  Out = []
    ;   FromNext is From-1,
        ToNext is To-1,
        List=[_|Rest],
        from_to(FromNext,ToNext,Rest,Out)
    ).

/**
 * drop(+N, +List, -ListMinFirstN) is semidet.
 *
 * Drop the first N elements from List and unify the remainder with
 * LastElements.
 */
drop(0,LastElements,LastElements) :- !.
drop(N,[_|Tail],LastElements) :-
	N > 0,
	N1 is N  - 1,
	drop(N1,Tail,LastElements).

/* 
 * truncate_list(Offset:integer, Limit:integer, Input:list, Output:list) is det. 
 * 
 * Truncate a list to some length, starting from some offset. 
 */
truncate_list(Offset,Limit,Input,Output) :-
    (   Limit < 0
    ->  drop(Offset,Input,Output)
    ;   length(Input,N),
        Top is Offset + Limit,
        min_list([N,Top],Final),
        from_to(Offset,Final,Input,Output)).

/**
 * sfoldr(+P,+Gen,+Z,-Result) is det.
 * 
 * Fold using predicate P with generator Gen and terminal solution Z. 
 * This is much more memory efficient than realising a list when the list is 
 * large.
 */
:- meta_predicate sfoldr(3,1,+,-).
sfoldr(Pred,Generator,Zero,Result) :-
    State = state(Zero),
    (  call(Generator,X),
       arg(1, State, M),
       call(Pred, X, M, R),
       nb_setarg(1, State, R),
       fail
    ;  arg(1, State, R),
       nonvar(R),
       R=Result
    ).

/* 
 * trim(+String, -Trimmed) is det.
 *
 * Replaces a string with its space trimmed equivalent  
 */
trim(String,Trimmed) :-
    re_replace('^\\s*(.*?)\\s*$','\\1', String, Trimmed).

/* 
 * get_dict_default(Key,Dict,Value,Default)
 */ 
get_dict_default(Key,Dict,Value,Default) :-
    (   get_dict(Key,Dict,Value)
    ->  true
    ;   Value = Default).

/* 
 * split_atom(Atom:atom,Delimiter:atom,Result:list(atom)) is det.
 */
split_atom(Atom,Delimiter,Result) :-
    split_string(Atom,Delimiter,'',Strings),
    maplist([S,A]>>(atom_string(A,S)), Strings, Result).

