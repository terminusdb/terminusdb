:- module(utils,[
              get_key/4,
              get_key/3,
              get_dict_default/4,
              zip/3,
              intersperse/3,
              interpolate/2,
              unique_solutions/3,
              repeat_term/3,
              zero_pad/3,
              pad/4,
              coerce_number/2,
              exhaust/1,
              take/3,
              from_to/4,
              drop/3,
              truncate_list/4,
              sfoldr/4,
              foldm/6,
              mapm/5,
              mapm/6,
              trim/2,
              split_atom/3,
              pattern_string_split/3,
              count/3,
              merge_dictionaries/3,
              command/1,
              coerce_literal_string/2,
              coerce_atom/2,
              xfy_list/3,
              yfx_list/3,
              snoc/3,
              join/3,
              op(920,fy, *),
              '*'/1,
              op(700,xfy,<>),
              '<>'/2
          ]).

/** <module> Utils
 *
 * Utility predicates
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

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

/*
 * Forget the next phrase.
 *
 * Ueful for declarative debugging.
 */
*_.

/*
 * '<>'(:Goal1,:Goal2) is det
 *
 * Deterministic
 */
:- meta_predicate '<>'(0,0).
'<>'(Goal1, Goal2) :-
    (   call(Goal1)
    ->  true
    ;   call(Goal2)).

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
    !,
    interpolate(T,Rest),
    atom_concat(H,Rest,S).
interpolate([H|T],S) :-
    string(H),
    !,
    atom_string(C,H),
    interpolate(T,Rest),
    atom_concat(C,Rest,S).
interpolate([H|T],S) :-
    ground(H),
    !,
    term_to_atom(H,C),
    interpolate(T,Rest),
    atom_concat(C,Rest,S).

/**
 * interpolate_string(L:list,A:atom) is det.
 *
 * Takes a list of values of mixed type, and appends them together as rendered atoms.
 */
interpolate_string([],'').
interpolate_string([H|T],S) :-
    string(H),
    !,
    interpolate_string(T,Rest),
    string_concat(H,Rest,S).
interpolate_string([H|T],S) :-
    atom(H),
    !,
    atom_string(H,C),
    interpolate_string(T,Rest),
    string_concat(C,Rest,S).
interpolate_string([H|T],S) :-
    ground(H),
    !,
    term_to_atom(H,C),
    interpolate_string(T,Rest) ,
    string_concat(C,Rest,S).

/**
 * unique_solutions(+Template,+Goal,-Collection) is det.
 *
 * This implements the CORRECT semantics for setof.
 * i.e. returns an empty list for failure to find solutions, rather than failing.
 */
:- meta_predicate unique_solutions(?,0,?).
unique_solutions(Template, Goal, Collection) :-
    (   setof(Template, Goal, CollectionX)
    ->  Collection=CollectionX
    ;   Collection=[]).

/**
 * repeat_term(+A:any,+N:int,-L:list) is det.
 *
 * Repeats a term A, N times.
 */
repeat_term(_A,0,[]).
repeat_term(A,N,[A|Z]) :-
	N > 0,
	N2 is N - 1,
	repeat_term(A,N2,Z).

/**
 * pad(-A1:term,C:atom,Length:int,A2:atom) is det.
 *
 * Pad the atom A1, with C to make it length Length.
 */
pad(T,C,L,A2) :-
    format(atom(A), '~w', [T]),
    (   interpolate([C],S),
        string_length(S,1)
    ->  true
    ;   format(atom(M), 'Not a single character in pad: ~q', [pad(A,C,L,A2)]),
        throw(syntax_error(M))),
	atom_chars(A,AtomList),
	length(AtomList, L1),
	L2 is L - L1,
    (   L2 < 0
    ->  format(atom(M), 'Bad pad length ~q for ~q', [L,T]),
        throw(error(M))
    ;   true),
	repeat_term(S,L2,List),
	append(List,AtomList,TotalList),
	atom_chars(A2,TotalList).

/**
 * zero_pad(-A1:atom,Length:int,A2:atom) is det.
 *
 * Pad the atom A1, with '0' to make it length Length.
 */
zero_pad(A,L,A2) :-
    pad(A,'0',L,A2).

/*
 * coerce_number(S,N) is det.
 *
 * Ensure that S is converted to a number N
 */
coerce_number(S,N) :-
    atom_string(A, S),
    !,
    atom_number(A, N).
coerce_number(A,N) :-
    atom(A),
    !,
    atom_number(A,N).
coerce_number(N,N) :-
    number(N).

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


/*
 * pattern_string_split(Pattern,String,List) is det.
 */
pattern_string_split(Pattern,String,List) :-
    re_split(Pattern,String,L),
    once(intersperse(_,List,L)).

/*
 * foldm(P:predicate,L:list,Zero:any,S0:any,SN:any) is det.
 *
 * Monadic fold over state
 */
:- meta_predicate foldm(5,?,?,?,?,?).
foldm(_P,[],Base,Base,S,S).
foldm(P,[H|T],Base,Result,S0,SN) :-
    foldm(P,T,Base,LastResult,S0,S1),
    call(P,H,LastResult,Result,S1,SN).

/*
 * mapm(P:predicate,L:list,O:list,S0:any,SN:any) is det.
 *
 * Monadic map over state
 */
:- meta_predicate mapm(4,?,?,?,?).
mapm(_P,[],[],S,S).
mapm(P,[H|T],[HP|TP],S0,SN) :-
    call(P,H,HP,S0,S1),
    mapm(P,T,TP,S1,SN).

/*
 * mapm(P:predicate,L:list,O:list,M:list,S0:any,SN:any) is det.
 *
 * Monadic map over state
 */
:- meta_predicate mapm(5,?,?,?,?,?).
mapm(_P,[],[],[],S,S).
mapm(P,[H|T],[HP|TP],[HM|TM],S0,SN) :-
    call(P,H,HP,HM,S0,S1),
    mapm(P,T,TP,TM,S1,SN).

/**
 * count(+A:atom,+L:list,-C:int) is det.
 *
 * Counts occurences of A in L.
 */
count(Atom, List, Count) :-
    count(List, Atom, 0, Count).

count([], _, Count, Count).
count([Head| Tail], Atom, Count0, Count) :-
    (   Head = Atom ->
        Count1 is Count0 + 1
    ;   Count1 is Count0
    ),
    count(Tail, Atom, Count1, Count).

/*
 * merge_dictionaries(+Dict1,+Dict2,-Dict3) is det.
 *
 * Merge favouring left.
 */
merge_dictionaries(Dict1,Dict2,Dict3) :-
    dict_pairs(Dict1, _, Pairs1),
    dict_pairs(Dict2, _, Pairs2),
    findall(A-B,
            (   member(A-B, Pairs1)
            ;   member(A-B, Pairs2),
                \+ member(A-_, Pairs1)
            ),
            Pairs3),
    dict_create(Dict3, _, Pairs3).

/*
 * command(Cmd) is semidet.
 *
 * True if Cmd exists.
 */
command(Cmd) :-
    catch(
        (   process_create(path(Cmd), [], [ stderr(null),
                                            stdout(null),
                                            process(PID)
                                          ]),
            process_wait(PID,_Status)),
        error(existence_error(source_sink,path(Cmd)),_),
        fail).

/*
 * coerce_literal_string(+S_or_L, -S) is det.
 *
 * We pun GET and POST parameters in requests but
 * to do this we need to be able to conflate literals
 * with strings.
 */
coerce_literal_string(SL,S) :-
    is_dict(SL),
    !,
    get_dict('@value',SL, S).
coerce_literal_string(SL,S) :-
    is_list(SL),
    !,
    string_codes(S, SL).
coerce_literal_string(SL,S) :-
    % \+ is_dict(SL),
    SL = S.

/*
 * xfy_list(Op, Term, List) is det.
 *
 * Folds a functor over a list.
 */
xfy_list(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list(Op, Right, List),
    !.
xfy_list(_, Term, [Term]).


/*
 * yfx_list(Op, Term, List) is det.
 *
 * Folds a functor over a list.
 */
yfx_list(Op, Term, List) :-
    reverse(List,Rev),
    yfx_list_aux(Op,Term,Rev).

yfx_list_aux(Op, Term, [Right|List]) :-
    Term =.. [Op, Left, Right],
    yfx_list_aux(Op, Left, List),
    !.
yfx_list_aux(_, Term, [Term]).


/*
 * coerce_atom(Atom_Or_String,Atom) is semidet.
 *
 * Coerces Atom_Or_String to an Atom if it is an atom or string
 * but fails otherwise.
 */
coerce_atom(Atom_Or_String, Atom) :-
    (   atom(Atom_Or_String)
    ->  Atom_Or_String = Atom
    ;   string(Atom_Or_String)
    ->  atom_string(Atom,Atom_Or_String)
    ).

/*
 * coerce_string(Atom_Or_String,String) is semidet.
 *
 * Coerces Atom_Or_String to a String if it is an atom or string
 * but fails otherwise.
 */
coerce_string(Atom_Or_String, String) :-
    (   atom(Atom_Or_String)
    ->  atom_string(Atom_Or_String, String)
    ;   string(Atom_Or_String)
    ->  Atom_Or_String = String
    ).

/*
 * snoc(Rest,Last,List) is det.
 *
 * Adds an element to the end of the list (or extracts it when run backwards).
 *
 * [cons backwards!]
 */
snoc([],Last,[Last]).
snoc([First|Tail],Last,[First|Rest]) :-
    snoc(Tail,Last,Rest).


/*
 * join(List,Sep,String) is det.
 *
 * Joins a list of strings/atoms into a single string.
 */
join(List, Sep, Atom) :-
    intersperse(Sep,List,New_List),
    interpolate_string(New_List, Atom).

/*
 * re(Pattern, String, Matches) is det.
 */
re(Pattern, String, Matches) :-
    re_matchsub(Pattern,String,Dict,[]),
    dict_pairs(Dict,_,Pairs),
    length(Pairs,N),
    length(Matches,N),
    maplist({Matches}/[Key-Value]>>nth0(Key,Matches,Value),Pairs).

