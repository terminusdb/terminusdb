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
 * Type promotion is intended to give a type to bare literals
 * who have not had a specified type for some reason (integers
 * or booleans perhaps).
 */
maybe_promote(X^^Type,X^^Type) :-
    !.
maybe_promote(X@Lang,X@Lang) :-
    !.
maybe_promote(S,S^^'http://www.w3.org/2001/XMLSchema#string') :-
    string(S),
    !.
maybe_promote(A,S^^'http://www.w3.org/2001/XMLSchema#string') :-
    atom(A),
    !,
    atom_string(A,S).
maybe_promote(N,N^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    number(N),
    !.

/*
 * Presumably this should record into prov on failure.
 */
typecast(Val, Type, Hint, Cast) :-
    (   var(Val)
    ->  format(atom(M), 'Variable unbound in typcast to ~q', [Type]),
        throw(error(M))
    ;   maybe_promote(Val,Promoted),
        (   Promoted = Bare_Literal^^Source_Type
        ->  typecast_switch(Bare_Literal,Source_Type,Type,Hint,Cast)
        ;   Promoted = Bare_Literal@Source_Type
        ->  typecast_switch(Bare_Literal,Source_Type,Type,Hint,Cast)
        ;   format(atom(M), 'No possible cast ~q', [typecast(Val, Type, Hint, Cast)]),
            throw(error(M))
        )
    ).
typecast_switch(Val, _ST, 'http://www.w3.org/2002/07/owl#Thing', _, Val) :-
    /* It might be wise to check URI validity */
    !.
typecast_switch(Val, 'http://www.w3.org/2001/XMLSchema#dateTime',
                'http://www.w3.org/2001/XMLSchema#dateTime', _, Val) :-
    !.
typecast_switch(Val, _ST, 'http://www.w3.org/2001/XMLSchema#dateTime', _, Cast) :-
    (   guess_date(Val,Cast)
    ->  !
    ;   format(atom(M),'Unable to cast as (xsd:dateTime): ~q~n',
               [Val]),
        term_jsonld(Val,JVal),
        throw(http_reply(method_not_allowed(_{'@type' : 'vio:ViolationWithDatatypeObject',
                                              'vio:literal' : JVal,
                                              'vio:message' : M})))
    ).
typecast_switch(Val, _ST, 'http://www.w3.org/2001/XMLSchema#integer', _, Cast) :-
    (   guess_integer(Val,Cast)
    ->  !
    ;   format(atom(M),'Unable to cast as (xsd:integer): ~q~n',
               [Val]),
        term_jsonld(Val,JVal),
        throw(http_reply(method_not_allowed(_{'@type' : 'vio:ViolationWithDatatypeObject',
                                              'vio:literal' : JVal,
                                              'vio:message' : M})))
    ).
typecast_switch(date(Y,M,D,HH,MM,SS,Z,_,_),
                _ST,
                'http://www.w3.org/2001/XMLSchema#decimal',
                _,
                Num^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    !,
    date_time_stamp(date(Y,M,D,HH,MM,SS,Z,-,-), Num).
typecast_switch(Val, _ST, 'http://www.w3.org/2001/XMLSchema#decimal', _, Cast) :-
    !,
    (   guess_number(Val,Cast)
    ->  true
    ;   format(atom(M),'Unable to cast as (xsd:decimal): ~q~n',
               [Val]),
        term_jsonld(Val,JVal),
        throw(http_reply(method_not_allowed(_{'@type' : 'vio:ViolationWithDatatypeObject',
                                              'vio:literal' : JVal,
                                              'vio:message' : M})))
    ).
typecast_switch(Date, _ST, 'http://www.w3.org/2001/XMLSchema#string', _,
                String^^'http://www.w3.org/2001/XMLSchema#string') :-
    Date = date(_Y,_M,_D,_HH,_MM,_SS,_Z,_OH,_OM),
    !,
    date_string(Date,String).
typecast_switch(Val, _ST, 'http://www.w3.org/2001/XMLSchema#string', _,
                String^^'http://www.w3.org/2001/XMLSchema#string') :-
    format(string(String), '~w', [Val]).
typecast_switch(Val, _ST, 'http://terminusdb.com/schema/xdd#integerRange', _, Cast) :-
    (   atom(Val)
    ->  true
    ;   string(Val)),
    !,
    (   guess_integer_range(Val,Cast)
    ->  true
    ;    format(atom(M),'Unable to cast as (xdd:integerRange): ~q~n',
                [Val]),
         throw(map_error([type=cast_error,message=M]))
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
    ;   format(atom(M),'Unable to cast as (xdd:decimalRange): ~q~n',
               [Val]),
        throw(map_error([type=cast_error,message=M]))
    ).
typecast_switch(Val, _ST, 'http://terminusdb.com/schema/xdd#decimalRange', _, Cast) :-
    number(Val),
    !,
    Cast = Val^^'http://terminusdb.com/schema/xdd#decimalRange'.
