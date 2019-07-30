:- module(frame_types, [
              is_property_restriction/1,
              is_property_frame/1,
              is_class_choice_frame/1,
              is_logical_frame/1,
              is_one_of_frame/1,
              is_and_frame/1,
              is_xor_frame/1,
              is_entity_frame/1,
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
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
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
    member(type=Type,L),
    member(Type,[and,or,not,xor,sub]), % [type=xor,Ops=[....]]
    member(operands=Ops,L),
    exclude(is_property_restriction,Ops,[]).
is_property_restriction(L) :- % [mincard=?,valuesFrom=?, ...]
    member(minCardinality=_,L).
is_property_restriction(L) :-
    member(maxCardinality=_,L).
is_property_restriction(L) :-
    member(cardinality=_,L).
is_property_restriction(L) :-
    member(hasValue=_,L).
is_property_restriction(L) :-
    member(allValuesFrom=_,L).
is_property_restriction(L) :-
    member(someValuesFrom=_,L).

error:has_type(property_restriction,X) :-
    is_property_restriction(X).

error:has_type(restriction_formula,restriction(X)) :-
    is_property_restriction(X).

is_property_frame(F) :-
    member(type=Type,F),
    member(Type, [objectProperty, datatypeProperty]),
    member(property=_PP,F),
    member(domain=_Domain,F),
    member(range=_Range,F),
    (member(label=_Label,F) ; true), % This is for documentation purposes as it cant' fail
    (member(comment=_Comment,F) ; true), % This is for documentation purposes as it cant' fail
    (   member(frame=Frame,F)
    ->  is_frame(Frame)
    ;   true),
    (   member(restriction=R,F)
    ->  is_property_restriction(R)
    ;   true).
is_property_frame(F) :-
    member(type=restriction,F),
    member(property=_PP,F),
    member(restriction=R,F),
    is_property_restriction(R).

error:has_type(property_frame,X) :-
    is_property_frame(X).

is_logical_frame(F) :-
    member(type=Type,F),
    member(Type,[and,or,not,xor]),
    member(operands=Ops,F),
    exclude(is_frame,Ops,[]).

is_one_of_frame(F) :-
    member(type=oneOf,F),
    member(elements=_Elts,F).

is_entity_frame(F) :-
    member(type=entity,F),
    member(class=_Class,F).

is_class_choice_frame(F) :-
    member(type=class_choice,F),
    member(operands=_Classes,F).

is_or_frame(F) :-
    member(type=or,F),
    member(operands=_,F).

is_and_frame(F) :-
    member(type=and,F),
    member(operands=_,F).

is_xor_frame(F) :-
    member(type=xor,F),
    member(operands=_,F).

is_frame(F) :- exclude(is_property_frame,F,[]).
is_frame(F) :- is_entity_frame(F).
is_frame(F) :- is_one_of_frame(F).
is_frame(F) :- is_class_choice_frame(F).
    
error:has_type(frame, X) :-
    is_frame(X).
