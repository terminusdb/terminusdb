:- module(casting,[
              typecast/4,
              hash/3,
              idgen/3,
              random_idgen/3
          ]).

/** <module> Casting
 *
 * Utilities for casting between types for I/O.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                      *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(literals).

:- use_module(core(util/utils)).
:- use_module(core(util/speculative_parse)).
:- use_module(core(validation)).

:- reexport(core(util/syntax)).

:- use_module(core(query/jsonld)). % dubious. we should not be importing query stuff here.

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

/**
 * idgen(?Base:uri,?Args:list(any),++Output:uri) is det.
 * idgen(++Base:uri,++Args:list(any),?Output:uri) is det.
 *
 * Create a safe uri starting from @Base using key @Args.
 *
 * NOTE: Invertibility is due to the impossibility of having a %5f
 * as the result of a path encoding. _ translates as _ and
 * %5f translates as %255f.
 */
idgen(Base,Args,Output) :-
    ground(Base),
    ground(Args),
    !,
    maplist([In,Out]>>uri_encoded(path,In,Out), Args, Safe_Parts),
    merge_separator_split(Output,'_',[Base|Safe_Parts]).
idgen(Base,Args,Output) :-
    merge_separator_split(Output,'_',[Base|Safe_Parts]),
    maplist([In,Out]>>uri_encoded(path,In,Out), Args, Safe_Parts).

random_idgen(Base,Args,Output) :-
    ground(Base),
    ground(Args),
    !,
    random_string(String),
    append(Args,[String], New_Args),
    idgen(Base,New_Args,Output).

/*
 * hash(+Base:uri,++Args:list(any),-Output:uri) is det.
 *
 * Create a hash uri starting from @Base using key @Args.
 */
hash(Base,Args,Output) :-
    maplist([In,Out]>>
            (   (   string(In)
                ;   atom(In))
            ->  trim(In,Out)
            ;   In=Out
            ), Args, Trimmed),
    interpolate(Trimmed,ArgAtom),
    md5_hash(ArgAtom,Hash,[]),
    atom_concat(Base,Hash,Output).

/*
 * Presumably this should record into prov on failure.
 */
typecast(Val, Type, Hint, Cast) :-
    (   var(Val)
    ->  format(atom(M), 'Variable unbound in typecast to ~q', [Type]),
        throw(error(M))
    ;   (   Val = Bare_Literal^^Source_Type
        ->  typecast_switch(Bare_Literal,Source_Type,Type,Hint,Cast)
        ;   Val = Bare_Literal@Source_Type
        ->  typecast_switch(Bare_Literal,Source_Type,Type,Hint,Cast)
        ;   throw(error(casting_error(Val, Type), _))
        )
    ).
typecast_switch(Val, _ST, 'http://www.w3.org/2002/07/owl#Thing', _, Val) :-
    /* It might be wise to check URI validity */
    !.
typecast_switch(Val, 'http://www.w3.org/2001/XMLSchema#dateTime',
                'http://www.w3.org/2001/XMLSchema#dateTime', _, Val^^'http://www.w3.org/2001/XMLSchema#dateTime') :-
    !.
typecast_switch(Val, _ST, 'http://www.w3.org/2001/XMLSchema#dateTime', _, Cast) :-
    (   guess_date(Val,Cast)
    ->  !
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#dateTime'),_))
    ).
typecast_switch(Val, _ST, 'http://www.w3.org/2001/XMLSchema#integer', _, Cast) :-
    (   guess_integer(Val,Cast)
    ->  !
    ;   throw(error(casting_error(Val,'http://www.w3.org/2001/XMLSchema#integer'),_))
    ).
typecast_switch(date(Y,M,D,HH,MM,SS,Z,_,_),
                _ST,
                'http://www.w3.org/2001/XMLSchema#decimal',
                _,
                Num^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    !,
    date_time_stamp(date(Y,M,D,HH,MM,SS,Z,-,-), Num).
typecast_switch(date(Y,M,D,HH,MM,SS,Z,OH,OM),
                _ST, 'http://www.w3.org/2001/XMLSchema#string', _,
                String^^'http://www.w3.org/2001/XMLSchema#string') :-
    !,
    date_string(date(Y,M,D,HH,MM,SS,Z,OH,OM),String).
typecast_switch(Val, _ST, Type, _, Cast^^Type) :-
    basetype_subsumption_of(Type,'http://www.w3.org/2001/XMLSchema#decimal'),
    !,
    (   guess_number(Val,Cast^^_)
    ->  true
    ;   throw(error(casting_error(Val,Type),_))
    ).
typecast_switch(Val, _ST, Type, _,
                Val^^Type) :-
    string(Val),
    basetype_subsumption_of(Type,'http://www.w3.org/2001/XMLSchema#string'),
    !.
typecast_switch(Val, _ST, Type, _,
                String^^Type) :-
    basetype_subsumption_of(Type,'http://www.w3.org/2001/XMLSchema#string'),
    !,
    format(string(String), '~w', [Val]).
typecast_switch(Val, _ST, 'http://terminusdb.com/schema/xdd#integerRange', _, Cast) :-
    (   atom(Val)
    ->  true
    ;   string(Val)),
    !,
    (   guess_integer_range(Val,Cast)
    ->  true
    ;   throw(error(casting_error(Val, 'http://terminusdb.com/schema/xdd#integerRange'), _))
    ).
typecast_switch(Val, _ST, 'http://terminusdb.com/schema/xdd#integerRange', _, Cast) :-
    integer(Val),
    !,
    Cast = Val^^'http://terminusdb.com/schema/xdd#integerRange'.
typecast_switch(Val, _ST, 'http://terminusdb.com/schema/xdd#decimalRange', _, Cast) :-
    (   atom(Val)
    ->  true
    ;   string(Val)),
    !,
    (   guess_decimal_range(Val,Cast)
    ->  true
    ;   throw(error(casting_error(Val, 'http://terminusdb.com/schema/xdd#integerRange'), _))
    ).
typecast_switch(Val, _ST, 'http://terminusdb.com/schema/xdd#decimalRange', _, Cast) :-
    number(Val),
    !,
    Cast = Val^^'http://terminusdb.com/schema/xdd#decimalRange'.
typecast_switch(Val, _ST, 'http://terminusdb.com/schema/xdd#url', _, Cast) :-
    is_absolute_url(Val),
    !,
    Cast = Val^^'http://terminusdb.com/schema/xdd#url'.



:- begin_tests(typecast).

:- use_module(core(util/test_utils)).

test(anyURI_url,[]) :-
    typecast("terminusdb:///schema#gitHub_user_html_url"^^'http://www.w3.org/2001/XMLSchema#anyURI', 'http://terminusdb.com/schema/xdd#url', [], "terminusdb:///schema#gitHub_user_html_url"^^'http://terminusdb.com/schema/xdd#url').

test(decimal_long,[]) :-
    typecast(12^^'http://www.w3.org/2001/XMLSchema#decimal', 'http://www.w3.org/2001/XMLSchema#long', [], 12^^'http://www.w3.org/2001/XMLSchema#long').

test(string_long,[]) :-
    typecast("12"^^'http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#long', [], 12^^'http://www.w3.org/2001/XMLSchema#long').

test(string_dateTime,[]) :-
    typecast("2012-10-09T00:00:00Z"^^'http://www.w3.org/2001/XMLSchema#string', 'http://www.w3.org/2001/XMLSchema#dateTime', [], date(2012, 10, 9, 0, 0, 0, 1, 0, 0)^^'http://www.w3.org/2001/XMLSchema#dateTime').

:- end_tests(typecast).
