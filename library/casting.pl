:- module(casting,[
              typecast/4,
              hash/3,
              idgen/3
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

:- use_module(utils).
:- use_module(prefixes).
:- use_module(speculative_parse).
:- use_module(jsonld).
:- use_module(literals).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

/*
 * idgen(+Base:uri,++Args:list(any),-Output:uri) is det.
 *
 * Create a safe uri starting from @Base using key @Args.
 */
idgen(Base,Args,Output) :-
    maplist([In,Out]>>uri_encoded(path,In,Out), Args, Safe),
    interpolate([Base|Safe],Output).

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
maybe_promote(literal(L),literal(L)) :-
    !.
maybe_promote(S,literal(type('http://www.w3.org/2001/XMLSchema#string',S))) :-
    string(S),
    !.
maybe_promote(S,literal(type('http://www.w3.org/2001/XMLSchema#string',S))) :-
    atom(S),
    !.
maybe_promote(N,literal(type('http://www.w3.org/2001/XMLSchema#decimal',N))) :-
    number(N),
    !.

/*
 * Presumably this should record into prov on failure.
 */
typecast(Val, Type, Hint, Cast) :-
    (   (   var(Val)
        ->  true
        ;   Val = literal(L),
            var(L))
    ->  format(atom(M), 'Variable unbound in typcast to ~q', [Type]),
        throw(error(M))
    ;   maybe_promote(Val,Promoted),
        (   Promoted = literal(type(Source_Type,Bare_Literal))
        ->  typecast_switch(Bare_Literal,Source_Type,Type,Hint,Cast)
        ;   Promoted = literal(lang(Source_Type,Bare_Literal))
        ->  typecast_switch(Bare_Literal,Source_Type,Type,Hint,Cast)
        ;   format(atom(M), 'No possible cast ~q', [typecast(Val, Type, Hint, Cast)]),
            throw(error(M))
        )
    ).
typecast_switch(Val, _ST, 'http://www.w3.org/2002/07/owl#Thing', _, Val) :-
    /* It might be wise to check URI validity */
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
                literal(type('http://www.w3.org/2001/XMLSchema#decimal',Num))) :-
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
                literal(type('http://www.w3.org/2001/XMLSchema#string',String))) :-
    Date = date(_Y,_M,_D,_HH,_MM,_SS,_Z,_OH,_OM),
    !,
    date_string(Date,String).
typecast_switch(Val, _ST, 'http://www.w3.org/2001/XMLSchema#string', _,
                literal(type('http://www.w3.org/2001/XMLSchema#string',String))) :-
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
    Cast = literal(type('http://terminusdb.com/schema/xdd#integerRange', Val)).
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
    Cast = literal(type('http://terminusdb.com/schema/xdd#decimalRange', Val)).
