:- module(speculative_parse, [guess_date/2,
                              guess_number/2,
                              guess_integer/2,
                              guess_integer_range/2,
                              guess_decimal_range/2
                             ]).

/** <module> Speculative Parse
 *
 * Guess the correct parse of an input.
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

:- use_module(xsd_parser).
:- use_module(library(dcg/basics), [whites//0, blanks//0]).

/*
 * guess_date(+Val,-Date) is nondet.
 *
 * Guess some possible dates.
 */
guess_date(Val,literal(type('http://www.w3.org/2001/XMLSchema#dateTime',Date))) :-
    atom_codes(Val,Codes),
    once(phrase(guess_date(Date),Codes)).

guess_integer(Val,literal(type('http://www.w3.org/2001/XMLSchema#integer',Val))) :-
    integer(Val),
    !.
guess_integer(Val,literal(type('http://www.w3.org/2001/XMLSchema#integer',N))) :-
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
guess_number(Val,literal(type('http://www.w3.org/2001/XMLSchema#decimal',Val))) :-
    number(Val),
    !.
guess_number(Val,literal(type('http://www.w3.org/2001/XMLSchema#decimal',N))) :-
    (   atom(Val)
    ;   string(Val)),
    atom_codes(Val,Codes),
    phrase((whites,
            integer(N),
            whites),
           Codes),
    !.
guess_number(Val,literal(type('http://www.w3.org/2001/XMLSchema#decimal',Result))) :-
    (   atom(Val)
    ;   string(Val)),
    atom_codes(Val,Codes),
    phrase((whites,
            guess_number(Ans,",","."),
            whites),
           Codes),
    number_string(Result,Ans),
    !.
guess_number(Val,literal(type('http://www.w3.org/2001/XMLSchema#decimal',Result))) :-
    (   atom(Val)
    ;   string(Val)),
    atom_codes(Val,Codes),
    phrase((whites,
            guess_number(Ans,".",","),
            whites),
           Codes),
    number_string(Result,Ans).

guess_integer_range(Val,literal(type('http://terminusdb.com/schema/xdd#integerRange', Val))) :-
    (   atom(Val)
    ;   string(Val)),
    atom_codes(Val,Codes),
    phrase((blanks,integerRange(_,_),blanks),Codes).

guess_decimal_range(Val,literal(type('http://terminusdb.com/schema/xdd#decimalRange', Val))) :-
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
