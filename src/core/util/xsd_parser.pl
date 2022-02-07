:- module(xsd_parser, [digit//1,
                       oneDigitNatural//1,
                       twoDigitNatural//1,
                       threeDigitNatural//1,
                       decimal//1,
                       digits//1,
                       integer//1,
                       double//3,
                       positiveInteger//1,
                       negativeInteger//1,
                       nonPositiveInteger//1,
                       nonNegativeInteger//1,
                       unsignedDecimal//1,
                       year//1,
                       date//4,
                       dateTime//8,
                       dateTimeStamp//8,
                       gYear//2,
                       gYearMonth//3,
                       gMonth//2,
                       gMonthDay//3,
                       gDay//2,
                       duration//7,
                       yearMonthDuration//3,
                       dayTimeDuration//5,
                       string/3,
                       base64Binary//0,
                       hexBinary//0,
                       language//0,
                       whitespace//0,
                       anyBut//1,
                       time//5,
                       coordinatePolygon//1,
                       dateRange//2,
                       decimalRange//2,
                       email//0,
                       gYearRange//2,
                       integerRange//2,
                       point//2,
                       url//0
                       ]).

/** <module> XSD Parser
 *
 * Attempt to implement XSD parsing as faithfully as possible.
 *
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

:- use_module(core(triple/iana)). % dubious - seems to be the only place where iana is used, so shouldn't iana be part of this module?

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(yall)).

/******************
 *  Punctuation   *
 ******************/
fullstop --> ".".

whitespace --> space, whitespace .
whitespace --> "" .

space --> " " .
space --> "\n" .
space --> "\t" .
space --> "\r" .

equals --> "=" .

notslash --> anyBut('/'), notslash.
notslash --> anyBut('/').

/******************
 *  Number stuff  *
 ******************/

sign(1) --> "+".
sign(-1) --> "-".
sign(1) --> "".


digit("0") --> "0".
digit("1") --> "1".
digit("2") --> "2".
digit("3") --> "3".
digit("4") --> "4".
digit("5") --> "5".
digit("6") --> "6".
digit("7") --> "7".
digit("8") --> "8".
digit("9") --> "9".

notDigit --> anyBut('01234567890'), notDigit.
notDigit --> anyBut('01234567890').

oneDigitNatural(N) --> digit(A), { number_string(N,A) }.

twoDigitNatural(N) --> digit(A), digit(B), { string_concat(A,B,C), number_string(N,C) } .

threeDigitNatural(N) --> digit(A), digit(B), digit(C), { string_concat(A,B,I), string_concat(I,C,D), number_string(N,D) } .

fourDigitNatural(N) --> digit(A), digit(B), digit(C), digit(D),
			{ string_concat(A,B,S1), string_concat(S1,C,S2), string_concat(S2,D,S3), number_string(N,S3) } .

sixDigitNatural(N) --> digit(A), digit(B), digit(C), digit(D), digit(E), digit(F),
			           { atomic_list_concat([A,B,C,D,E,F],Atom),
                         atom_string(Atom,S),
                         number_string(N,S) }.

nineDigitNatural(N) --> digit(A), digit(B), digit(C),
                        digit(D), digit(E), digit(F),
                        digit(G), digit(H), digit(I),
			           { atomic_list_concat([A,B,C,D,E,F,G,H,I],Atom),
                         atom_string(Atom,S),
                         number_string(N,S) }.

digits(T) --> digit(X), digits(S),
	      { string_concat(X, S, T) } .
digits(S) --> digit(S) .

natural(N) --> digits(S),
	       { number_string(N,S) }.


integer(I) --> sign(S), natural(N),
	       { I is N * S } .

positiveInteger(I) --> natural(I) .
positiveInteger(I) --> "+", natural(I) .

negativeInteger(I) --> "-", natural(N) , { N > 0, I is N * -1 }.

nonPositiveInteger(I) --> "-", natural(N) , { I is N * -1 } .

nonNegativeInteger(I) --> natural(I) .
nonNegativeInteger(I) --> "+", natural(I) .
nonNegativeInteger(0) --> "-0" .

decimal(M) -->
    integer(I), fullstop, digits(S),
	{
        string_concat("0.", S, T),
        number_string(E,T),
        % Need to eliminate the sign and add
        % it later in order to use addition.
        Sign is sign(I),
        M is Sign * ((Sign * I) + E)
    }.
decimal(M) --> integer(M) .

unsignedDecimal(M) --> natural(I), fullstop, digits(S),
		       { string_concat("0.", S, T), number_string(E,T), M is I + E } .
unsignedDecimal(M) --> natural(M) .

exp --> "e" .
exp --> "E" .

double(0,0,nan) --> "NAN" .
double(S,0,inf) --> sign(S), "INF" .
double(M,1,double) --> decimal(M) .
double(M,E,double) --> decimal(M), exp, integer(E) .


/******************
 *  Time + Date   *
 ******************/

% To make invertible we should process time zones and dst here correctly.
time_offset(0) -->
    "Z" .
time_offset(S) -->
    "+",
    twoDigitNatural(ZH), ":", twoDigitNatural(ZM), ":", twoDigitNatural(ZS),
    { S is (ZH * 3600) + (ZM * 60) + ZS }.
time_offset(S) -->
    "+", twoDigitNatural(ZH), ":", twoDigitNatural(ZM),
    { S is (ZH * 3600) + (ZM * 60) }.
time_offset(S) -->
    "-", twoDigitNatural(ZH), ":", twoDigitNatural(ZM), ":", twoDigitNatural(ZS),
    { S is (-1 * ((ZH * 3600) + (ZM * 60) + ZS)) }.
time_offset(S) -->
    "-", twoDigitNatural(ZH), ":", twoDigitNatural(ZM),
    { S is (-1 * ((ZH * 3600) + (ZM * 60))) }.

optional_time_offset(Offset) -->
    time_offset(Offset).
optional_time_offset(0) -->
    "".

time_and_offset(H,M,S,NS,Offset) -->
    twoDigitNatural(H), ":", twoDigitNatural(M), ":", twoDigitNatural(S),
	".", digits(D),
    { length(L, 9),
      atom_codes(D,Codes),
      append(Codes, Tail, L),
      maplist([0'0]>>true, Tail),
      atom_codes(Number,L),
      atom_number(Number, NS)
    },
    time_offset(Offset).
time_and_offset(H,M,S,0,Offset) -->
    twoDigitNatural(H), ":", twoDigitNatural(M), ":", twoDigitNatural(S),
	time_offset(Offset) .
time_and_offset(H,M,0,0,Offset) -->
    twoDigitNatural(H), ":", twoDigitNatural(M), time_offset(Offset).

% Hour, Minute, Second, Nanosecond, Offset, Zone, DST
time(H,M,S,NS,Offset) -->
    twoDigitNatural(H), ":", twoDigitNatural(M), ":", twoDigitNatural(S),
	".",
    digits(D),
    { length(L, 9),
      atom_codes(D,Codes),
      append(Codes, Tail, L),
      maplist([0'0]>>true, Tail),
      atom_codes(Number,L),
      atom_number(Number, NS)
    },
    optional_time_offset(Offset).
time(H,M,S,0,Offset) -->
    twoDigitNatural(H), ":", twoDigitNatural(M), ":", twoDigitNatural(S),
	optional_time_offset(Offset) .
time(H,M,0,0,Offset) -->
    twoDigitNatural(H), ":", twoDigitNatural(M), optional_time_offset(Offset).

year(SY) --> sign(S), natural(Y),
	         { SY is S * Y }.

date(SY,Mo,D,Offset) -->
    year(SY), "-", twoDigitNatural(Mo), "-", twoDigitNatural(D), optional_time_offset(Offset).

time_separator --> "T".
time_separator --> " ".

dateTime(SY,Mo,D,H,M,S,NS,Offset) -->
    year(SY), "-", twoDigitNatural(Mo), "-", twoDigitNatural(D),
    time_separator, time(H,M,S,NS,Offset).

dateTimeStamp(SY,Mo,D,H,M,S,NS,Offset) -->
    year(SY), "-", twoDigitNatural(Mo), "-", twoDigitNatural(D),
    time_separator, time_and_offset(H,M,S,NS,Offset).

gYear(Y,Offset) --> year(Y), optional_time_offset(Offset) .

gYearMonth(Y,M,Offset) --> year(Y), "-", twoDigitNatural(M), optional_time_offset(Offset) .

gMonth(M,Offset) --> "--", twoDigitNatural(M), optional_time_offset(Offset) .

gMonthDay(Mo,D,Offset) --> "-", twoDigitNatural(Mo), "-", twoDigitNatural(D), optional_time_offset(Offset) .

gDay(D,Offset) --> "---", twoDigitNatural(D), optional_time_offset(Offset) .

maybeYear(Y) --> natural(Y), "Y" .
maybeYear(0) --> "" .

maybeMonth(M) --> natural(M), "M" .
maybeMonth(0) --> "" .

maybeDay(D) --> natural(D), "D" .
maybeDay(0) --> "" .

maybeHour(H) --> natural(H), "H" .
maybeHour(0) --> "" .

maybeMinute(M) --> natural(M), "M" .
maybeMinute(0) --> "" .

maybeSecond(S) --> unsignedDecimal(S), "S" .
maybeSecond(0) --> "" .

maybeTime(H,M,S) --> time_separator, maybeHour(MH), maybeMinute(MM), maybeSecond(MS),
		     { (MH < 0, MM < 0, MS < 0)
		       -> fail
		       ; (MH < 0 -> H = 0 ; MH = H),
			 (MM < 0 -> M = 0 ; MM = M),
			 (MS < 0 -> S = 0 ; MS = S) } .
maybeTime(0,0,0) --> "" .


duration(Sign,Y,Mo,D,H,M,S) --> sign(Sign), "P", maybeYear(Y), maybeMonth(Mo), maybeDay(D),
				maybeTime(H,M,S) .

yearMonthDuration(Sign,Y,Mo) --> sign(Sign), "P", maybeYear(Y), maybeMonth(Mo) .

dayTimeDuration(Sign,D,H,M,S) --> sign(Sign), "P", maybeDay(D), maybeTime(H,M,S) .

% xsd:hexBinary
hex_elt --> oneOf('abcdef0123456789') .

hexBinary --> hex_elt , hexBinary .
hexBinary --> hex_elt .

%  Base64 encoding

base64char --> "+" .
base64char --> "/" .

/*********************
 *  String and Char  *
 **********************/

charRange(First,Last,[H|T],T) :- atom_codes(First,[Start]), atom_codes(Last,[End]),
				 H >= Start, H =< End .

alphaUpper --> charRange('A','Z') .
alphaLower --> charRange('a','z') .

alpha --> alphaUpper .
alpha --> alphaLower .

alphas --> alphas, alpha .
alphas --> alpha .


base64elt --> alpha .
base64elt --> base64char .
base64elt --> digit(_) .

oneOf(Chars,[H|T],T) :- atom_codes(Chars,Codes),member(H,Codes) .

base64elt1 --> oneOf('AEIMQUYcgkosw048') .
base64elt2 --> oneOf('AQgw') .


base64Terminates --> whitespace, base64elt,
		     whitespace, base64elt,
		     whitespace, base64elt,
		     whitespace, base64elt .
base64Terminates --> whitespace, base64elt,
		     whitespace, base64elt2,
		     whitespace, equals,
		     whitespace, equals .
base64Terminates --> whitespace, base64elt,
		     whitespace, base64elt,
		     whitespace, base64elt1,
		     whitespace, equals .

base64Binary --> whitespace, base64elt,
		 whitespace, base64elt,
		 whitespace, base64elt,
		 whitespace, base64elt ,
		 base64Binary .
base64Binary --> base64Terminates .

iso_639_base([H1,H2,H3|T],T) :- atom_codes(A,[H1,H2,H3]), iso_639_3(A,_), !.
iso_639_base([H1,H2,H3|T],T) :- atom_codes(A,[H1,H2,H3]), iso_639_2(A,_).

language --> "en-US" .
language --> "en-GB" .
language --> iso_639_base .
language --> iana(_) . % IANA
language --> "x-", alphas . % unregistered

any([_H|T],T).

anyBut(S,[H|T],T) :- atom_codes(S,C), \+ member(H,C) .

normalizedChar --> anyBut('<&') .
normalizedChar --> "&amp;" .
normalizedChar --> "&lt;" .

normalizedString --> normalizedChar, normalizedString .
normazliedString --> "" .

nmtokenChar --> alpha .
nmtokenChar --> digit(_) .
nmtokenChar --> oneOf('.-_:') .

nmtoken --> nmtokenChar, nmtoken .
nmtoken --> "" .

name --> alpha, nmtoken .
name --> oneOf('_:'), nmtoken .

ncnameChar --> alpha .
ncnameChar --> digit(_) .
ncnameChar --> oneOf('_-.') .

ncolonSeq --> ncnameChar , ncolonSeq .
ncolonSeq --> "" .

ncname --> alpha, ncolonSeq .
ncname --> "_" , ncolonSeq .

/******************
 *  Geolocation   *
 ******************/

point(X,Y) --> "[" , whitespace, decimal(X), whitespace,
			   "," , whitespace, decimal(Y), whitespace,
			   "]" .


points([[X,Y]|L]) --> whitespace, "," , whitespace, point(X,Y), whitespace, points(L) .
points([]) -->  whitespace, "]" .

coordinatePolygon([[X,Y]|L]) --> "[" , whitespace, point(X,Y), points(L).
coordinatePolygon([]) --> "[" , whitespace , "]" .

decimalRange(X,X) --> decimal(X).
decimalRange(X,Y) --> "[" , whitespace, decimal(X), whitespace,
					  "," , whitespace, decimal(Y), whitespace,
					  "]" .
integerRange(X,X) --> integer(X).
integerRange(X,Y) --> "[" , whitespace, integer(X), whitespace,
					 "," , whitespace, integer(Y), whitespace,
					 "]" .

gYearRange(gyear(X,Z),gyear(X,Z)) -->
    gYear(X,Z).
gYearRange(gyear(X,ZX),gyear(Y,ZY)) -->
    "[" , whitespace, gYear(X,ZX), whitespace,
	"," , whitespace, gYear(Y,ZY), whitespace,
	"]" .

dateRange(date(Y,M,D,Offset),date(Y,M,D,Offset)) -->
    date(Y,M,D,Offset).
dateRange(date(Y1,M1,D1,Offset1),date(Y2,M2,D2,Offset2)) -->
    "[" , whitespace, date(Y1,M1,D1,Offset1), whitespace,
	"," , whitespace, date(Y2,M2,D2,Offset2), whitespace,
	"]" .

terminal([],_S).

emailChar --> alpha ; digit(_) ; "." ; "-".

domain --> emailChar, domain.
domain --> emailChar.

file --> any, file.
file --> any.

maybefile --> "/", file.
maybefile --> terminal.

url --> ("http://" ; "https://" ; "") , domain, file.

emailname --> emailChar, emailname.
emailname --> emailChar.

email --> emailname, "@", domain.

string(S,L,[]) :- string_codes(S,L).
