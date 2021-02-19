:- module(frame_types, [
              is_property_restriction/1,
              is_property_frame/1,
              is_class_choice_frame/1,
              is_logical_frame/1,
              is_one_of_frame/1,
              is_and_frame/1,
              is_xor_frame/1,
              is_document_frame/1,
              is_frame/1
          ]).

/** <module> Frame Types
 *
 * Type checking predicates for frames.
 *
 *
 * Frame Grammar:
 *
 * % example
 *  [type=objectProperty,
 *   domain=Domain,
 *   range=Range,
 *   frame=[[...], ..., [...]] => { 'type' : 'objectProperty',
 *                                  'domain' : 'Domain',
 *                                  'range' : 'Range',
 *                                  'frame' : [{ ... } , ... , { ... }] }.
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

is_property_restriction(true).
is_property_restriction(L) :-
    memberchk(type=Type,L),
    memberchk(Type,[and,or,not,xor,sub]), % [type=xor,Ops=[....]]
    memberchk(operands=Ops,L),
    exclude(is_property_restriction,Ops,[]).
is_property_restriction(L) :- % [mincard=?,valuesFrom=?, ...]
    memberchk(minCardinality=_,L).
is_property_restriction(L) :-
    memberchk(maxCardinality=_,L).
is_property_restriction(L) :-
    memberchk(cardinality=_,L).
is_property_restriction(L) :-
    memberchk(hasValue=_,L).
is_property_restriction(L) :-
    memberchk(allValuesFrom=_,L).
is_property_restriction(L) :-
    memberchk(someValuesFrom=_,L).

:- multifile error:has_type/2.

error:has_type(property_restriction,X) :-
    is_property_restriction(X).

error:has_type(restriction_formula,restriction(X)) :-
    is_property_restriction(X).

is_property_frame(F) :-
    memberchk(type=Type,F),
    memberchk(Type, [objectProperty, datatypeProperty]),
    memberchk(property=_PP,F),
    memberchk(domain=_Domain,F),
    memberchk(range=_Range,F),
    (memberchk(label=_Label,F) ; true), % This is for documentation purposes as it cant' fail
    (memberchk(comment=_Comment,F) ; true), % This is for documentation purposes as it cant' fail
    (   memberchk(frame=Frame,F)
    ->  is_frame(Frame)
    ;   true),
    (   memberchk(restriction=R,F)
    ->  is_property_restriction(R)
    ;   true).
is_property_frame(F) :-
    memberchk(type=restriction,F),
    memberchk(property=_PP,F),
    memberchk(restriction=R,F),
    is_property_restriction(R).

error:has_type(property_frame,X) :-
    is_property_frame(X).

is_logical_frame(F) :-
    memberchk(type=Type,F),
    memberchk(Type,[and,or,not,xor]),
    memberchk(operands=Ops,F),
    exclude(is_frame,Ops,[]).

is_one_of_frame(F) :-
    memberchk(type=oneOf,F),
    memberchk(elements=_Elts,F).

is_document_frame(F) :-
    memberchk(type=document,F),
    memberchk(class=_Class,F).

is_class_choice_frame(F) :-
    memberchk(type=class_choice,F),
    memberchk(operands=_Classes,F).

is_or_frame(F) :-
    memberchk(type=or,F),
    memberchk(operands=_,F).

is_and_frame(F) :-
    memberchk(type=and,F),
    memberchk(operands=_,F).

is_xor_frame(F) :-
    memberchk(type=xor,F),
    memberchk(operands=_,F).

is_frame(F) :- exclude(is_property_frame,F,[]).
is_frame(F) :- is_document_frame(F).
is_frame(F) :- is_one_of_frame(F).
is_frame(F) :- is_class_choice_frame(F).

error:has_type(frame, X) :-
    is_frame(X).
