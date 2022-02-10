:- module(speculative_parse, [guess_date/2,
                              guess_datetime_stamp/2,
                              guess_number/2,
                              guess_integer/2,
                              guess_integer_range/2,
                              guess_decimal_range/2
                             ]).

/** <module> Speculative Parse
 *
 * Guess the correct parse of an input.
 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the  the Apache License, Version 2.0           *
 *  (the "License");                                                     *
 *  you may not use this file except in compliance with the License.     *
 *  You may obtain a copy of the License at                              *
 *                                                                       *
 *  http://www.apache.org/licenses/LICENSE-2.0                           *
 *                                                                       *
 *  Unless required by applicable law or agreed to in writing, software  *
 *  distributed under the License is distributed on an "AS IS" BASIS,    *
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      *
 *  implied.                                                             *
 *  See the License for the specific language governing permissions and  *
 *  limitations under the License.                                       *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- reexport(core(util/syntax)).
:- use_module(xsd_parser).
:- use_module(utils).
:- use_module(library(dcg/basics), [whites//0, blanks//0]).
:- use_module(library(plunit)).

/*
 * guess_date(+Val,-Date) is nondet.
 *
 * Guess some possible dates.
 */
guess_date(Val,DateTime^^'http://www.w3.org/2001/XMLSchema#dateTime') :-
    atom_codes(Val,Codes),
    once(phrase(guess_date(Date),Codes)),
    datetime_to_internal_datetime(Date,DateTime).

guess_datetime_stamp(Val,DateTime^^'http://www.w3.org/2001/XMLSchema#dateTimeStamp') :-
    atom_codes(Val,Codes),
    once(phrase(dateTimeStamp(Y,M,D,HH,MM,SS,Offset,Zone,DST),Codes)),
    datetime_to_internal_datetime(date(Y,M,D,HH,MM,SS,Offset,Zone,DST),DateTime).

guess_time(Val,Time^^'http://www.w3.org/2001/XMLSchema#time') :-
    atom_codes(Val,Codes),
    once(phrase(time(HH,MM,SS,Offset,Zone,DST),Codes)),
    time_to_internal_time(time(HH,MM,SS,Offset,Zone,DST), Time).

guess_integer(Val,Val^^'http://www.w3.org/2001/XMLSchema#integer') :-
    integer(Val),
    !.
guess_integer(Val,N^^'http://www.w3.org/2001/XMLSchema#integer') :-
    (   atom(Val)
    ;   string(Val)),
    atom_codes(Val, Codes),
    phrase((whites,
            integer(N),
            whites),
           Codes).

/*
 * guess_number(+Val,-Number) is nondet. (or det?)
 *
 * Guess some possible numbers.
 */
guess_number(Val,Result^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    number(Val),
    !,
    Result = Val.
guess_number(Val,N^^'http://www.w3.org/2001/XMLSchema#decimal') :-
    (   atom(Val)
    ;   string(Val)),
    trim(Val, TrimmedVal),
    number_string(N,TrimmedVal).

guess_integer_range(Val,Val^^'http://terminusdb.com/schema/xdd#integerRange') :-
    (   atom(Val)
    ;   string(Val)),
    atom_codes(Val,Codes),
    phrase((blanks,integerRange(_,_),blanks),Codes).

guess_decimal_range(Val,Val^^'http://terminusdb.com/schema/xdd#decimalRange') :-
    (   atom(Val)
    ;   string(Val)),
    atom_codes(Val,Codes),
    phrase((blanks,decimalRange(_,_),blanks),Codes).

triplet(Number_String) -->
    digit(A), digit(B), digit(C),
    { string_concat(A,B,Inter),
      string_concat(Inter,C,Number_String)
    }.

doublet(Number_String) -->
    digit(A), digit(B),
    { string_concat(A,B,Number_String)
    }.

guess_comma_phrased_prefix(Number_String,Spacer) -->
    triplet(Number_String),Spacer, !.
guess_comma_phrased_prefix(Number_String,Spacer) -->
    doublet(Number_String),Spacer, !.
guess_comma_phrased_prefix(Number_String,Spacer) -->
    digit(Number_String), Spacer, !.

guess_comma_phrased_prefix(Number_String,Spacer) -->
    triplet(Number_String),Spacer, !.
guess_comma_phrased_prefix(Number_String,Spacer) -->
    doublet(Number_String),Spacer, !.
guess_comma_phrased_prefix(Number_String,Spacer) -->
    digit(Number_String),Spacer, !.

guess_comma_phrased_suffix(Number_String,Spacer) -->
    triplet(Prefix), Spacer, !, guess_comma_phrased_suffix(Suffix,Spacer),
    { string_concat(Prefix,Suffix,Number_String) }.
guess_comma_phrased_suffix(Number_String,_Spacer) -->
    triplet(Number_String).

guess_comma_phrased_decimal(Number,Punkt) -->
    Punkt, !, digits(Decimal),
    { string_concat(".", Decimal, Number) }.
guess_comma_phrased_decimal("",_Punkt) -->
    "".

guess_number(Number_String,Spacer,Punkt) -->
    guess_comma_phrased_prefix(Prefix,Spacer),
    guess_comma_phrased_suffix(Suffix,Spacer),
    guess_comma_phrased_decimal(Decimal,Punkt),
    { string_concat(Prefix,Suffix,Whole),
      string_concat(Whole,Decimal,Number_String)
    },
    !.
guess_number(Number,_Spacer,Punkt) -->
    triplet(Number_String),
    guess_comma_phrased_decimal(Decimal,Punkt),
    { string_concat(Number_String,Decimal,Number) },
    !.
guess_number(Number,_Spacer,Punkt) -->
    doublet(Number_String),
    guess_comma_phrased_decimal(Decimal,Punkt),
    { string_concat(Number_String,Decimal,Number)
    },
    !.
guess_number(Number,_Spacer,Punkt) -->
    digit(Number_String),
    guess_comma_phrased_decimal(Decimal,Punkt),
    { string_concat(Number_String,Decimal,Number)
    }.

guess_date(date(Y,Mo,D,H,M,S,Z,ZH,ZM)) -->
    dateTime(Y,Mo,D,H,M,S,Z,ZH,ZM),
    !.
guess_date(date(Y,Mo,D,H,M,S,Z,ZH,ZM)) -->
    year(Y), "-",  twoDigitNatural(Mo), "-", twoDigitNatural(D), " ", guess_time(time(H,M,S,Z,ZH,ZM)),
    !.
guess_date(date(Y,Mo,D,0,0,0,0,0,0)) -->
    year(Y), "-",  twoDigitNatural(Mo), "-", twoDigitNatural(D),
    !.
guess_date(date(Y,Mo,D,H,M,S,Z,ZH,ZM)) -->
    twoDigitNatural(D), "/", twoDigitNatural(Mo), "/", year(Y), " ", guess_time(time(H,M,S,Z,ZH,ZM)),
    { Mo =< 12 }.

guess_time(time(H,M,S,Z,ZH,ZM)) -->
    time(H,M,S,Z,ZH,ZM),
    !.
guess_time(time(H,M,0,0,-,-)) -->
    twoDigitNatural(H), ":", twoDigitNatural(M).


:- begin_tests(speculative_parse).

test(decimal_with_five_numbers, []) :-
    guess_number("43322.3243", X),
    X = 43322.3243^^'http://www.w3.org/2001/XMLSchema#decimal'.

test(negative_decimal, []) :-
    guess_number("-43322.3243", X),
    X = -43322.3243^^'http://www.w3.org/2001/XMLSchema#decimal'.

test(zero_prepend, []) :-
    guess_number("043322.3243", X),
    X = 43322.3243^^'http://www.w3.org/2001/XMLSchema#decimal'.

:- end_tests(speculative_parse).
