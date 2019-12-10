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
 * Presumably this should record into prov on failure.
 */
typecast(Val, Type, Hint, Cast) :-
    global_prefix_expand(Type,Type_URI),
    typecast_switch(Val,Type_URI,Hint,Cast).

typecast_switch(Val, 'http://www.w3.org/2001/XMLSchema#dateTime', _, Cast) :-
    (   guess_date(Val,Cast)
    ->  true
    ;   format(atom(M),'Unable to cast as (xsd:dateTime): ~q~n',
               [Val]),
        throw(map_error([type=cast_error,message=M]))
    ).
typecast_switch(Val, 'http://www.w3.org/2001/XMLSchema#integer', _, Cast) :-
    (   guess_integer(Val,Cast)
    ->  true
    ;   format(atom(M),'Unable to cast as (xsd:integer): ~q~n',
               [Val]),
        throw(map_error([type=cast_error,message=M]))
    ).
typecast_switch(Val, 'http://www.w3.org/2001/XMLSchema#decimal', _, Cast) :-
    (   guess_number(Val,Cast)
    ->  true
    ;   format(atom(M),'Unable to cast as (xsd:decimal): ~q~n',
               [Val]),
        throw(map_error([type=cast_error,message=M]))
    ).
typecast_switch(Val, 'http://www.w3.org/2001/XMLSchema#string', _, Cast) :-
    format(string(Cast), '~w', Val).
