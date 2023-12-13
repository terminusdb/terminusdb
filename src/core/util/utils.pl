:- module(utils,[
              down_from/3,
              get_key/4,
              get_key/3,
              get_dict_default/4,
              zip/3,
              intersperse/3,
              interpolate/2,
              alternate/2,
              interpolate_string/2,
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
              mapm/4,
              mapm/5,
              mapm/6,
              mapm/7,
              mapm/8,
              convlist/4,
              exists/2,
              find/3,
              trim/2,
              split_atom/3,
              getenv_number/2,
              getenv_default/3,
              getenv_default_number/3,
              escape_pcre/2,
              pattern_string_split/3,
              merge_separator_split/3,
              count/3,
              merge_dictionaries/3,
              command/1,
              coerce_literal_string/2,
              coerce_atom/2,
              coerce_string/2,
              xfy_list/3,
              yfx_list/3,
              snoc/3,
              join/3,
              op(920,fy, *),
              '*'/1,
              op(1100,xfy,<>),
              '<>'/2,
              do_or_die/2,
              option_or_die/2,
              die_if/2,
              whole_arg/2,
              uri_has_protocol/1,
              uri_has_prefix/1,
              uri_has_prefix/2,
              uri_has_prefix_unsafe/2,
              choice_points/1,
              sol_set/2,
              sol_bag/2,
              optional/1,
              chunk/2,
              chunk/3,
              member_last/3,
              time_to_internal_time/2,
              datetime_to_internal_datetime/2,
              json_read_term/2,
              json_read_term_stream/2,
              json_read_list_stream/2,
              json_read_list_stream_head/3,
              json_init_tail_stream/2,
              json_read_tail_stream/2,
              skip_generate_nsols/3,
              input_to_integer/2,
              duplicates/2,
              has_duplicates/2,
              index_list/2,
              nb_thread_var_init/2,
              nb_thread_var/2,
              uri_encoded_string/3,
              text/1,
              with_memory_file/1,
              with_memory_file_stream/3,
              with_memory_file_stream/4,
              terminal_slash/2,
              dict_field_verifier/3,
              count_solutions/2,
              negative_to_infinity/2,

              init_local_memoize/1,
              local_memoize/3,

              %%% From the rust module
              random_string/1
          ]).

/** <module> Utils
 *
 * Utility predicates
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(library(pcre)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).
:- use_module(library(http/json)).
:- use_module(library(solution_sequences)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(process), [process_create/3, process_wait/2]).
:- use_module(library(uri), [uri_encoded/3]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(memfile)).
:- use_module(library(option)).

/*
 * The opposite of between/3
 */
down_from(From,To,X) :-
    between(To,From,Y),
    X is From - Y + 1.

/*
 * Forget the next phrase.
 *
 * Useful for declarative debugging.
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

/*
 * do_or_die(:Goal1,:Goal2) is det
 *
 * Deterministic
 */
:- meta_predicate do_or_die(:,?).
do_or_die(Goal, Error) :-
    (   call(Goal)
    ->  true
    ;   throw(Error)).

/*
 * option_or_die(?,+) is det
 *
 * Deterministic
 */
option_or_die(Option,Options) :-
    (   option(Option,Options)
    ->  true
    ;   Option =.. [Name,_]
    ->  throw(error(required_option_unspecified(Name), _))
    ).

/*
 * die_if(:Goal1,:Goal2) is det
 *
 * Deterministic
 */
:- meta_predicate die_if(:,?).
die_if(Goal, Error) :-
    (   call(Goal)
    ->  throw(Error)
    ;   true).

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
intersperse(Item, List, Output) :-
    (   List == []
    ->  Output = []
    ;   List = [X| Xs],
        intersperse_(Xs, X, Item, Output)
    ).

intersperse_([], X, _, [X]).
intersperse_([Y| Xs], X, Item, [X, Item| Tail]) :-
    intersperse_(Xs, Y, Item, Tail).


alternate([X], [X]).
alternate([X,_|Rest], [X|Tail]) :-
    alternate(Rest, Tail).

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
repeat_term(_A,0,[]) :-
    !.
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
    ;   throw(error(pad_not_length_one(C),_))),
	atom_chars(A,AtomList),
	length(AtomList, L1),
	L2 is L - L1,
    (   L2 < 0
    ->  throw(error(bad_pad_length(L)))
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

/**
 * count_solutions(Goal,Count) is det.
 *
 * Counts the number of solutions of Goal
 */
:- meta_predicate count_solutions(0,-).
count_solutions(Goal,Count) :-
    sfoldr(
        plus,
        {Goal}/[1]>>call(Goal),
        0,
        Count
    ).

/*
 * trim(+String, -Trimmed) is det.
 *
 * Replaces a string with its space trimmed equivalent
 */
trim(String,Trimmed) :-
    re_replace('^\\s*(.*?)\\s*$','$1', String, Trimmed).

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

/**
 * escape_pcre(String,Escaped) is det.
 */
escape_pcre(String, Escaped) :-
    re_replace('[-[\\]{}()*+?.,\\\\^$|#\\s]'/g, '\\$0', String, Escaped).

/**
 * merge_separator_split(+Merge, +Separator,-Split) is det.
 * merge_separator_split(-Merge, +Separator,+Split) is det.
 *
 * Splits an atom on the Separator or merges on the separator.
 *
 * BUG: Currently this works *only* on special character free separators,
 * as we do not escape the separator in the pattern.
 */
merge_separator_split(Merge, Separator, Split) :-
    ground(Split),
    var(Merge),
    !,
    once(intersperse(Separator, Split, Merge_List)),
    atomic_list_concat(Merge_List, Merge).
merge_separator_split(Merge, Separator, Split) :-
    ground(Merge),
    escape_pcre(Separator, Escaped),
    pattern_string_split(Escaped, Merge, Split_Strings),
    maplist(atom_string, Split, Split_Strings).

/**
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
 * mapm(P:predicate,L:list,S0:any,SN:any) is nondet.
 *
 * Monadic map over state
 */
:- meta_predicate mapm(3,?,?,?).
mapm(P,L1,S0,SN) :-
    mapm_(L1,S0,SN,P).

mapm_([],S,S,_P).
mapm_([H|T],S0,SN,P) :-
    call(P,H,S0,S1),
    mapm_(T,S1,SN,P).

/*
 * mapm(P:predicate,L:list,O:list,S0:any,SN:any) is nondet.
 *
 * Monadic map over state
 */
:- meta_predicate mapm(4,?,?,?,?).
mapm(P,L1,L2,S0,SN) :-
    mapm_(L1,L2,S0,SN,P).

mapm_([],[],S,S,_P).
mapm_([H|T],[HP|TP],S0,SN,P) :-
    call(P,H,HP,S0,S1),
    mapm_(T,TP,S1,SN,P).

/*
 * mapm(P:predicate,L:list,O:list,M:list,S0:any,SN:any) is nondet.
 *
 * Monadic map over state
 */
:- meta_predicate mapm(5,?,?,?,?,?).
mapm(P,L1,L2,L3,S0,SN) :-
    mapm_(L1,L2,L3,S0,SN,P).

mapm_([],[],[],S,S,_P).
mapm_([H|T],[HP|TP],[HM|TM],S0,SN,P) :-
    call(P,H,HP,HM,S0,S1),
    mapm_(T,TP,TM,S1,SN,P).

/*
 * mapm(P:predicate,L:list,O:list,M:list,N:list,S0:any,SN:any) is nondet.
 *
 * Monadic map over state
 */
:- meta_predicate mapm(6,?,?,?,?,?,?).
mapm(P,L1,L2,L3,L4,S0,SN) :-
    mapm_(L1,L2,L3,L4,S0,SN,P).

mapm_([],[],[],[],S,S,_P).
mapm_([H|T],[HP|TP],[HM|TM],[HL|TL],S0,SN,P) :-
    call(P,H,HP,HM,HL,S0,S1),
    mapm_(T,TP,TM,TL,S1,SN,P).

/*
 * mapm(P:predicate,L:list,O:list,M:list,N:list,P:list,S0:any,SN:any) is nondet.
 *
 * Monadic map over state
 */
:- meta_predicate mapm(7,?,?,?,?,?,?).
mapm(P,L1,L2,L3,L4,L5,S0,SN) :-
    mapm_(L1,L2,L3,L4,L5,S0,SN,P).

mapm_([],[],[],[],[],S,S,_P).
mapm_([H|T],[HP|TP],[HM|TM],[HL|TL],[HO|TO],S0,SN,P) :-
    call(P,H,HP,HM,HL,HO,S0,S1),
    mapm_(T,TP,TM,TL,TO,S1,SN,P).

:- meta_predicate convlist(3, +, +, -).
convlist(Goal, ListIn1, ListIn2, ListOut) :-
    convlist_(ListIn1, ListIn2, ListOut, Goal).

convlist_([], [], [], _).
convlist_([H0|T0], [H1|T1], ListOut, Goal) :-
    (   call(Goal, H0, H1, H)
    ->  ListOut = [H|T],
        convlist_(T0, T1, T, Goal)
    ;   convlist_(T0, T1, ListOut, Goal)
    ).



/*
 * exists(P:predicate,L:list)
 *
 * Succeeds if predicate holds for some member of the list.
 */
:- meta_predicate exists(1,?).
exists(P,[X|_Rest]) :-
    call(P,X),
    !.
exists(P,[_|Rest]) :-
    exists(P,Rest).

/**
 * find(P:predicate,L:list(ty),Result:ty) is semidet.
 *
 * Monadic fold over state
 */
:- meta_predicate find(1,?,?).
find(P,List,Result) :-
    find_(List, Result, P).

find_([H|_T], H, P) :-
    call(P,H),
    !.
find_([_H|T], Result, P) :-
    find_(T,Result,P).

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
    ;   atom_string(String, Atom_Or_String)
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
    do_or_die(
        (   ground(List),
            ground(Sep)),
        error(instantiation_error,_)),
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

%!  whole_arg(+N:integer, -Var:term) is det
%
%   @arg N 1 based nth argument
%   @arg Var the entirety of the Nth argument of this clause
%
%   @throws domain_error if N is invalid
%

whole_arg(N, Var) :-
    prolog_current_frame(Frame),
    prolog_frame_attribute(Frame, parent, Parent),
    (   prolog_frame_attribute(Parent, argument(N), Var)
    ->  true
    ;   prolog_frame_attribute(Parent, predicate_indicator, PI),
        strip_module(PI, Module, PIPFunctor/Arity),
        throw(error(domain_error(domain(1, Arity), Module:PIPFunctor/Arity), context(utils:whole_arg/2,
                                                                                     'whole_arg/2 takes a number from 1 to the argument arity')))
    ),
    !.
whole_arg(_, _) :-
    throw(error(system_error, context(utils:whole_arg/2, 'whole arg failed while trying to get its parents arity'))).

/*
 * uri_has_protocol(K) is semidet.
 *
 * Tests to see if a URI has a protocol.
 *
 * This performs a very simple check and does not support the full URI
 * specification. We can always improve on this if needed.
 */
uri_has_protocol(K) :-
    re_match('^\\p{L}\\p{Xan}*://.+', K).

/*
 * uri_has_prefix(K) is semidet.
 *
 * Tests to see if a URI has a prefix.
 */
uri_has_prefix(K) :-
    uri_has_prefix(K,_).

uri_has_prefix(K,Match) :-
    \+ uri_has_protocol(K),
    uri_has_prefix_unsafe(K,Match).

uri_has_prefix_unsafe(K,Match) :-
    re_matchsub('^(?<prefix>(\\p{L}|@)((\\p{Xwd}|-|\\.)*(\\p{Xwd}|-))?):(?<suffix>.*)$',
                K,
                Match, [auto_capture(false)]).

/*
 * getenv_number(+Name, +Value) is semidet.
 *
 * Get the env variable and convert it to a number
*/
getenv_number(Name, Value) :-
    getenv(Name, Value_Atom),
    atom_number(Value_Atom, Value).

/*
 * getenv_default(+Env_Key, +Default, -Value)
 *
 * Get the env variable or the default value if it is unset or empty
 */
getenv_default(Env_Key, Default, Value) :-
    (   getenv(Env_Key, V),
        V \= ''
    ->  Value = V
    ;   Value = Default).

/*
 * getenv_default_number(+Env_Key, +Default, -Value)
 *
 * Get the env variable or the default value if it isn't set as number
 */
getenv_default_number(Env_Key, Default, Value) :-
    (   getenv(Env_Key, Atom_Value)
    ->  atom_number(Atom_Value, Value)
    ;   Value = Default).



/*
 * choice_points(N) is det.
 *
 * Gets the current number of choice points, for debugging making
 * it easier to see who is the culprit.
 */
choice_points(N) :-
    prolog_current_choice(F),
    choice_points_(F,N).

choice_points_(F, 0) :-
    \+ prolog_choice_attribute(F, parent, _),
    !.
choice_points_(F, M) :-
    prolog_choice_attribute(F, parent, G),
    choice_points_(G, N),
    M is N + 1.

/**
 * sol_set(+Predicate,-Result:list) is det.
 *
 * Takes a uniary-predicate of mode p(-X) and finds all
 * unique generated solutions.
 */
:- meta_predicate sol_set(1,?).
sol_set(Predicate,Result) :-
    findall(Template,
            call(Predicate, Template),
            Templates),
    sort(Templates, Result).

/**
 * sol_set(+Predicate,-Result:list) is det.
 *
 * Takes a uniary-predicate of mode p(-X) and finds all
 * generated solutions.
 */
:- meta_predicate sol_bag(1,?).
sol_bag(Predicate,Result) :-
    findall(Template,
            call(Predicate, Template),
            Result).


/**
 * optional(+Goal) is nondet.
 *
 */
:- meta_predicate optional(0).
optional(Goal) :-
    (   call(Goal)
    *-> true
    ;   true).

chunk(String,Chunk) :-
    Size = 100000,
    chunk(String,Chunk,Size).

chunk(String,Chunk,Size) :-
    string_length(String,Length),
    Iterations is Length div Size + 1,
    between(0, Iterations, I),
    Offset is Size * I,
    End_Chunk is Offset + Size + 1,
    (   End_Chunk > Length
    ->  sub_string(String,Offset,_,0,Chunk),
        !
    ;   sub_string(String,Offset,Size,_,Chunk)).

/*
 * member_last(X,List,Last) is nondet.
 *
 * Binds `X` to every member of a list `List`,
 * with a boolean about whether it is the last.
 */
member_last(X,List,Last) :-
    member_last_(List,X,Last).

member_last_([A],A,true).
member_last_([A,_|_],A,false).
member_last_([_|Rest],A,Last) :-
    member_last_(Rest,A,Last).

datetime_to_internal_datetime(Date,date_time(Y,M,D,HH,MM,SS)) :-
    date_time_stamp(Date,TS),
    stamp_date_time(TS,date(Y, M, D, HH, MM, SS, 0, 'UTC', -), 'UTC').

time_to_internal_time(time(HH,MM,SS,Offset),time(HN,MN,SN)) :-
    HHOff is Offset div 3600,
    Offset1 is Offset - HHOff * 3600,
    MMOff is Offset1 div 60,
    SSOff is Offset1 - MMOff * 60,
    HN is HH + HHOff mod 24,
    MN is MM + MMOff mod 60,
    SN is SS + SSOff mod 60.

% Read a JSON Term from Stream. Fail if no JSON terms are found in Stream.
json_read_term(Stream, Term) :-
    '$util':json_read_dict_fast(Stream, Term),
    (   Term = eof
    ->  !,
        fail
    ;   true
    ).

% Read a JSON Term from Stream. Repeat with backtracking until eof. Fail if no
% JSON terms are found in Stream.
json_read_term_stream(Stream, Term) :-
    repeat,
    (   json_read_term(Stream, Term)
    *-> true
    ;   !,
        fail
    ).

% Read a JSON Term from Stream. If the term is a list, backtrack on members of
% the list and output each as Term. If term is not a list, output it as Term.
% Repeat until eof. Fail if no JSON terms are found in Stream.
json_read_list_stream(Stream, Term) :-
    json_read_term_stream(Stream, Term_1),
    (   is_list(Term_1)
    ->  member(Term, Term_1)
    ;   Term = Term_1
    ).

% json_read_list_stream_head(Stream, Term, Tail_Stream)
%
% behaves as
%
% json_read_list_stream(Stream, Term)
%
% but reads only the first Term without backtracking. The remaining terms in the
% Stream should now be read from the Tail_Stream with
%
% json_read_tail_stream(Tail_Stream, Term)
json_read_list_stream_head(Stream, Term, tail_stream(Tail, Stream)) :-
    json_read_term(Stream, T),
    (   is_list(T)
    ->  [Term|Tail] = T
    ;   Term = T,
        Tail = []
    ).

% json_init_tail_stream(Stream, Tail_Stream)
%
% behaves as
%
% json_read_list_stream_head(Stream, Term, Tail_Stream)
%
% without reading any terms. To read the terms from Tail_Stream, use
%
% json_read_tail_stream(Tail_Stream, Term)
json_init_tail_stream(Stream, tail_stream([], Stream)).

% json_read_tail_stream(Tail_Stream, Term)
%
% behaves as
%
% json_read_list_stream(Stream, Term)
%
% except that Tail_Stream may have elements from an initial list read from
% Stream.
json_read_tail_stream(tail_stream(Tail, Stream), Term) :-
    (   member(Term, Tail)
    ;   json_read_list_stream(Stream, Term)
    ).

:- begin_tests(json_read_term).

test("empty", [fail]) :-
    open_string("", Stream),
    json_read_term(Stream, _Term).

test("space", [fail]) :-
    open_string(" ", Stream),
    json_read_term(Stream, _Term).

test("illegal", [error(syntax_error(json(_)))]) :-
    open_string("{", Stream),
    json_read_term(Stream, _Term).

test("number", []) :-
    open_string(" 9", Stream),
    json_read_term(Stream, 9).

test("string", []) :-
    open_string("\"a\"", Stream),
    json_read_term(Stream, "a").

test("empty array", []) :-
    open_string("[]", Stream),
    json_read_term(Stream, []).

test("consecutive arrays", [fail]) :-
    open_string("[][9]", Stream),
    json_read_term(Stream, []),
    json_read_term(Stream, [9]),
    json_read_term(Stream, _Term3).

test("empty object", []) :-
    open_string("{}", Stream),
    json_read_term(Stream, json{}).

test("object", []) :-
    open_string("{\"a\":9}", Stream),
    json_read_term(Stream, json{a: 9}).

:- end_tests(json_read_term).

:- begin_tests(json_read_term_stream).

test("empty", [fail]) :-
    open_string("  ", Stream),
    json_read_term_stream(Stream, _Term).

test("not a stream", [error(domain_error(stream_or_alias,"not a stream"))]) :-
    json_read_term_stream("not a stream", _Term).

test("illegal", [error(syntax_error(json(illegal_json)))]) :-
    open_string("]", Stream),
    json_read_term_stream(Stream, _Term).

test("two objects", []) :-
    open_string("{\"a\":9}\n\n{\"b\":8}", Stream),
    findall(Term, json_read_term_stream(Stream, Term), Terms),
    assertion(Terms = [json{a: 9}, json{b: 8}]).

test("two lists", []) :-
    open_string("[{\"a\":9}]\n\n[1,2]", Stream),
    findall(Term, json_read_term_stream(Stream, Term), Terms),
    assertion(Terms = [[json{a: 9}], [1, 2]]).

:- end_tests(json_read_term_stream).

:- begin_tests(json_read_list_stream).

test("empty", [fail]) :-
    open_string("\n", Stream),
    json_read_list_stream(Stream, _Term).

test("two objects", []) :-
    open_string("{\"a\":9}\n\n{\"b\":8}", Stream),
    findall(Term, json_read_list_stream(Stream, Term), Terms),
    assertion(Terms = [json{a: 9}, json{b: 8}]).

test("two lists", []) :-
    open_string("[{\"a\":9}]\n\n[1,2]", Stream),
    findall(Term, json_read_list_stream(Stream, Term), Terms),
    assertion(Terms = [json{a: 9}, 1, 2]).

:- end_tests(json_read_list_stream).

:- begin_tests(json_read_list_stream_head_tail).

test("empty", [fail]) :-
    open_string("\n\n", Stream),
    json_read_list_stream_head(Stream, _Term, _Stream_Out).

test("three items", []) :-
    open_string("{\"a\":9}\n\n{\"b\":8}  4.05", Stream),
    json_read_list_stream_head(Stream, json{a: 9}, Tail_Stream),
    findall(Term, json_read_tail_stream(Tail_Stream, Term), Terms),
    assertion(Terms = [json{b: 8}, 4.05]).

test("three lists", []) :-
    open_string("[{\"a\":9}]\n\n[1 , 2][{\"b\":8}]", Stream),
    json_read_list_stream_head(Stream, json{a: 9}, Tail_Stream),
    findall(Term, json_read_tail_stream(Tail_Stream, Term), Terms),
    assertion(Terms = [1, 2, json{b: 8}]).

:- end_tests(json_read_list_stream_head_tail).

:- meta_predicate skip_generate_nsols(:, +, +).
skip_generate_nsols(Goal, Skip, Count) :-
    (   Count = unlimited
    ->  offset(Skip, Goal)
    ;   limit(Count, offset(Skip, Goal))).

/*
 * Convert unknown input to an integer.
 *
 * We don't throw exceptions here because the error will depend on the context.
 * Instead, we just fail and interpret failure as a type error.
 */
input_to_integer(Atom, Integer) :-
    (   Atom = inf
    ->  Integer = inf
    ;   integer(Atom)
    ->  Integer = Atom
    ;   text(Atom)
    ->  catch(atom_number(Atom, Integer), _, fail),
        integer(Integer)).

duplicates([], []) :- !.
duplicates(List, Duplicates) :-
    msort(List, Sorted),

    [First|Rest] = Sorted,

    duplicates_(Rest, First, Duplicates).

duplicates_([], _, []).
duplicates_([First,First|Rest], _, Duplicates) :-
    !,
    duplicates_([First|Rest], First, Duplicates).
duplicates_([First|Rest], First, [First|Duplicates]) :-
    !,
    duplicates_(Rest, First, Duplicates).
duplicates_([First|Rest], _, Duplicates) :-
    duplicates_(Rest, First, Duplicates).

has_duplicates(List, Duplicates) :-
    duplicates(List, Duplicates),
    Duplicates \= [].

index_list(List,Indexes) :-
    length(List, Len),
    (   Len >= 1
    ->  N is Len - 1,
        numlist(0, N, Indexes)
    ;   Indexes = []
    ).

nb_thread_var_init(Var, State) :-
    State = state(_),
    nb_setarg(1, State, Var).

:- meta_predicate nb_thread_var(2,+).
nb_thread_var(Callable, State) :-
    call(Callable, State, Out),
    nb_setarg(1,State, Out).

uri_encoded_string(Component, Value, Encoded_String) :-
    uri_encoded(Component, Value, Encoded),
    atom_string(Encoded, Encoded_String).

% This predicate is copied from library(error) to avoid using a private predicate.
% It can be found in the file library/error.pl inside the SWIPL directory.
text(X) :-
    (   atom(X)
    ;   string(X)
    ;   '$is_char_list'(X, _)
    ;   '$is_code_list'(X, _)
    ),
    !.

/* with_memory_file(+Goal).
 *
 * Create a new memory file Mem_File and call the Goal with Mem_File as the
 * argument. When Goal completes or throws an exception, close Mem_File.
 */
:- meta_predicate with_memory_file(1).
with_memory_file(Goal) :-
    setup_call_cleanup(
        new_memory_file(Mem_File),
        call(Goal, Mem_File),
        free_memory_file(Mem_File)
    ).

/* with_memory_file_stream(+Mem_File, +Mode, +Options, +Goal).
 *
 * Open the memory file Mem_File with the Mode and Options. Call the Goal with
 * the Stream from the memory file. When Goal completes or throws an exception,
 * close the stream.
 */
:- meta_predicate with_memory_file_stream(+,+,+,1).
with_memory_file_stream(Mem_File, Mode, Options, Goal) :-
    setup_call_cleanup(
        open_memory_file(Mem_File, Mode, Stream, Options),
        call(Goal, Stream),
        close(Stream)
    ).

/* with_memory_file_stream(+Mem_File, +Mode, +Goal).
 *
 * Same as with_memory_file_stream/4 with no options.
 */
:- meta_predicate with_memory_file_stream(+,+,1).
with_memory_file_stream(Mem_File, Mode, Goal) :-
    with_memory_file_stream(Mem_File, Mode, [], Goal).

/*
 * terminal_slash(+Atom, -Slashed) is det.
 *
 * Adds a slash if needed on the end of a path
 */
terminal_slash(Atom, Slashed) :-
    split_string(Atom, '/', '', List),
    last(List, Last),
    (   Last = ""
    ->  Atom = Slashed
    ;   atomic_list_concat([Atom, '/'], Slashed)).

/*
 * dict_field_verifier(+DictIn, +Fields, +Verifier:List[Goal(1)], -DictOut) is det.
 *
 * Creates a fresh dictionary having any of fields that exist in Dict
 * which meet the verifier goals.
 */
dict_field_verifier(DictIn, Field_Verifier, DictOut) :-
    findall(
        Field-Value,
        (   get_dict(Field,Field_Verifier,Verifier),
            get_dict(Field,DictIn,Value),
            do_or_die(
                field_verifier(Value,Verifier),
                error(invalid_field_value(Field,Value),_))),
        New_Pairs),
    dict_pairs(DictOut, _, New_Pairs).

:- meta_predicate field_verifier(+,1).
field_verifier(Field,Verifier) :-
    call(Verifier, Field).

negative_to_infinity(Z,P) :-
    (   Z < 0
    ->  P = inf
    ;   Z = P
    ).

init_local_memoize(state(_,[])).

is_first_run(State) :-
    State = state(Memoized, _List),
    var(Memoized).

finalize_memoization(State) :-
    State = state(_, List),
    reverse(List, Reversed),
    nb_setarg(1, State, memoized),
    nb_setarg(2, State, Reversed).

do_local_memoize(State, Val) :-
    State = state(_, List),
    NewList = [Val|List],
    nb_setarg(2, State, NewList).

get_local_memoized(state(_, List), Val) :-
    member(Val, List).

local_memoize(State, Template, Goal) :-
    (   is_first_run(State)
    ->  setup_call_cleanup(true,
                           (   call(Goal),
                               do_local_memoize(State, Template)),
                           finalize_memoization(State))
    ;   get_local_memoized(State, Template)).
