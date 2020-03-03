
:- module(literals, [
              fixup_schema_literal/2,
              normalise_triple/2,
              object_storage/2,
              storage_object/2,
              date_string/2
          ]).

/** <module> Literals
 *
 * Literal handling
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

:- op(2, xfx, @).
:- op(2, xfx, ^^).

/*
 * date_string(-Date,+String) is det.
 * date_string(+Date,-String) is det.
 */
date_string(Date,String) :-
    nonvar(Date),
    !,
    % ToDo, add appropriate time zone! Doesn't work in xsd_time_string!
    Date = date(Y,M,D,HH,MM,SS,_Z,_ZH,_ZM),
    format(string(String),
           '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+',
           [Y,M,D,HH,MM,SS]).
date_string(date(Y,M,D,HH,MM,SS,Z,ZH,ZM),String) :-
    % So expensive! Let's do this faster somehow.
    nonvar(String),
    !,
    atom_codes(String,Codes),
    phrase(xsd_parser:dateTime(Y,M,D,HH,MM,SS,Z,ZH,ZM),Codes).

/*
 * fixup_schema_literal(+Literal,-Literal) is det.
 *
 * Deal with precularities of rdf_process_turtle
 */
fixup_schema_literal(literal(lang(Lang,S)),String@Lang) :-
    (   atom(S)
    ->  atom_string(S,String)
    ;   S = String),
    !.
fixup_schema_literal(literal(type(Type,S)),String^^Type) :-
    (   atom(S)
    ->  atom_string(S,String)
    ;   S = String),
    !.
fixup_schema_literal(literal(L),String@en) :-
    (   atom(L)
    ->  atom_string(L,String)
    ;   L = String).

normalise_triple(rdf(X,P,Y),rdf(XF,P,YF)) :-
    (   X = node(N)
    ->  atomic_list_concat(['_:',N], XF)
    ;   X = XF),

    (   Y = node(M)
    ->  atomic_list_concat(['_:',M], YF)
    %   Bare atom literal needs to be lifted.
    ;   Y = literal(_)
    ->  fixup_schema_literal(Y,YF)
    %   Otherwise walk on by...
    ;   Y = YF).

/*
 * We can only make a concrete referrent if all parts are bound.
 */
nonvar_literal(Atom@Lang, Literal) :-
    atom(Atom),
    !,
    atom_string(Atom, String),
    nonvar_literal(String@Lang, Literal).
nonvar_literal(Atom^^Type, Literal) :-
    atom(Atom),
    !,
    atom_string(Atom, String),
    nonvar_literal(String^^Type, Literal).
nonvar_literal(String@Lang, value(S)) :-
    nonvar(Lang),
    nonvar(String),
    !,
    format(string(S), '~q@~q', [String,Lang]).
nonvar_literal(Val^^Type, value(S)) :-
    nonvar(Type),
    nonvar(Val),
    !,
    (   Type = 'http://www.w3.org/2001/XMLSchema#dateTime',
        Val = date(_Y, _M, _D, _HH, _MM, _SS, _Z, _ZH, _ZM)
    ->  date_string(Val,Date_String),
        format(string(S), '~q^^~q', [Date_String,Type])
    ;   format(string(S), '~q^^~q', [Val,Type])).
nonvar_literal(Val^^Type, Val^^Type) :-
    var(Val),
    once(var(Val) ; var(Type)),
    !.
nonvar_literal(Val@Lang, Val@Lang) :-
    var(Val),
    once(var(Val) ; var(Lang)),
    !.
nonvar_literal(O, node(S)) :-
    var(O),

    atom_string(O,S).

object_storage(O,V) :-
    nonvar(O),
    !,
    nonvar_literal(O,V).
object_storage(_O,_V). % Do nothing if input is a variable

storage_atom(TS,T) :-
    var(T),
    !,
    TS = T.
storage_atom(TS,T) :-
    (   atom(T)
    ->  TS = T
    ;   atom_string(TS,T)).

storage_value(X,V) :-
    var(V),
    !,
    X = V.
storage_value(X,V) :-
    (   string(V)
    ->  X = V
    ;   atom_string(V,X)).

storage_literal(X1^^T1,X3^^T2) :-
    storage_atom(T1,T2),
    storage_value(X1,X2),
    (   T2 = 'http://www.w3.org/2001/XMLSchema#dateTime'
    ->  date_string(X3,X2)
    ;   X2 = X3).
storage_literal(X1@L1,X2@L2) :-
    storage_atom(L1,L2),
    storage_value(X1,X2).

/*
 * Too much unnecessary marshalling...
 */
storage_object(value(S),O) :-
    (   read_term_from_atom(S,Term,[])
    ->  (   Term = X^^T
        ->  storage_literal(X^^T,O)
        ;   Term = X@Lang
        ->  storage_literal(X@Lang,O)
        ;   format(atom(M),'What fell term is this? ~q', [Term]),
            throw(error(M)))
    ;   format(atom(M),'Bad stored value ~q', [S]),
        throw(error(M))).
storage_object(node(S),O) :-
    (   nonvar(O)
    ->  (   atom(O)
        ->  atom_string(O,S)
        ;   O = S)
    ;   atom_string(O,S)).
