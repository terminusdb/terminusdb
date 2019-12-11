
:- module(literals, [
              fixup_schema_literal/2,
              normalise_triple/2,
              object_storage/2,
              storage_object/2
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

/*
 * fixup_schema_literal(+Literal,-Literal) is det.
 *
 * deal with precularities of rdf_process_turtle
 */
fixup_schema_literal(literal(lang(Lang,S)),literal(lang(Lang,String))) :-
    (   atom(S)
    ->  atom_string(S,String)
    ;   S = String).
fixup_schema_literal(literal(type(Type,S)),literal(type(Type,String))) :-
    (   atom(S)
    ->  atom_string(S,String)
    ;   S = String).
fixup_schema_literal(literal(L),literal(lang(en,String))) :-
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
nonvar_literal(lang(Lang,String), S) :-
    (   nonvar(Lang),
        nonvar(String)
    ->  format(string(S), '~q@~q', [String,Lang])
    ;   true).
nonvar_literal(type(Type,String), S) :-
    (   nonvar(Type),
        nonvar(String)
    ->  (   Type = 'http://www.w3.org/2001/XMLSchema#dateTime',
            String = date(Y, M, D, HH, MM, SS, _Z, _ZH, _ZM)
        ->  format(string(Date),
                   '~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+',
                   [Y,M,D,HH,MM,SS]),
            format(string(S), '~q^^~q', [Date,Type])
        ;   format(string(S), '~q^^~q', [String,Type]))
    ;   true).

nonvar_storage(literal(L),value(V)) :-
    !,
    % check groundness of literal arg
    % and do nothing if a var
    (   nonvar(L)
    ->  nonvar_literal(L,V)
    ;   true).
nonvar_storage(O,node(S)) :-
    atom_string(O,S).

object_storage(O,V) :-
    nonvar(O),
    !,
    nonvar_storage(O,V).
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

storage_literal_obj(Obj, Type) :-
    var(Type),
    !,
    Obj = Type.
storage_literal_obj(type(T1,X1), type(T2,X2)) :-
    storage_atom(T1,T2),
    storage_value(X1,X2).
storage_literal_obj(lang(L1,X1), lang(L2,X2)) :-
    storage_atom(L1,L2),
    storage_value(X1,X2).

storage_literal(A, O) :-
    var(O),
    !,
    A = O.
storage_literal(literal(Type), literal(L)) :-
    storage_literal_obj(Type,L).

/*
 * Too much unnecessary marshalling...
 */
:- op(2, xfx, @).
:- op(2, xfx, ^^).
storage_object(value(S),O) :-
    (   read_term_from_atom(S,Term,[])
    ->  (   Term = X^^T
        ->  storage_literal(literal(type(T,X)),O)
        ;   Term = X@Lang
        ->  storage_literal(literal(lang(Lang,X)),O)
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
