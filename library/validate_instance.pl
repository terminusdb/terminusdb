:- module(validate_instance,[
              instanceClass/3,
              most_specific_type/3,
              refute_insertion/5,
              refute_deletion/5,
              refute_basetype_elt/3
          ]).

/** <module> Instance Validation
 *
 * This module deals with instance checking as against a given schema.
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

:- use_module(library(validate_schema)).
:- use_module(library(database)).
:- use_module(library(triplestore)).
:- use_module(library(utils)).
:- use_module(library(types)).
:- use_module(library(base_type)).
:- use_module(library(inference)).
:- use_module(library(expansions)).
:- use_module(library(http/json)).

/**
 * most_specific_type(+Document, -Sorted, +Database)
 *
 * Gets the most specific type for a class
 **/
most_specific_type(Document, Document_Type, Database):-
    get_ordered_instance_classes(Document, [Document_Type|_], Database).

/**
 * get_ordered_instance_classes(+Document, -Sorted, +Database)
 *
 * Gets all classes for a specific document and returns
 * them ordered by subsumption.
 **/
get_ordered_instance_classes(Document, Sorted, Database) :-
    findall(
        Class,
        instanceClass(Document, Class, Database),
        Classes
    ),

    predsort(
        {Database}/[Delta,C1,C2]>>
        (   strictSubsumptionOf(C1, C2, Database)
        ->  Delta = (<)
        ;   strictSubsumptionOf(C2, C1, Database)
        ->  Delta = (>)
        ;   C1 = C2
        ->  Delta = (=)
        ), Classes, Sorted
    ).

/**
 * instanceClass(?X:uri, ?C:uri, +Database:database is nondet.
 *
 * Determines the class C identified with the instance X.
 */
instanceClass(X, Y, Database) :-
    database_instance(Database,Instance),
    xrdf(Database,Instance, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Y).
instanceClass(X, Y, Database) :-
    database_schema(Database,Schema), % instances can also exist in the schema
    xrdf(Database,Schema, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Y).

% X has cardinality N at property OP
card(X,OP,Y,Database,N) :-
    (   setof(Y,inferredEdge(X,OP,Y,Database), ListX)
    ->  ListX = L
    ;   L = []),
    length(L,N).

% X has qualified cardinality N at property OP and class C
qualifiedCard(X,OP,Y,C,Database,N) :-
    (   setof(Y,
              (   inferredEdge(X,OP,Y,Database),
	              \+ refute_node_at_range(Y,C,Database,_)
              ),
	          ListX)
    ->  ListX = L
    ;   L = []),
    length(L,N).

/*
 * refute_insertion(+Database, +Graph, +X, +P, +Y, -Reason)
 *

For readability:

S,G |- X : C  :=  \+ refute_node_at(S,G,X,C,Reason)

inserts follow this pattern:

forall (x p y) \in Inserts.
  p = 'rdf:type' => S,G |- x : y
  p /= 'rdf:type' =>
    S,G |- x : dom(p) /\ S,G |- y : rng(p)

  forall R \in Resrictions |- R < dom(p) => S |- R p Ca card
    S,G' |- Ca(x p y)

*/
refute_insertion(Database,
                 X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',Class,
                 Reason) :-
    !,
    refute_node_at_class(Database,X,Class,Reason).
refute_insertion(_Database,
                 X,'http://www.w3.org/2000/01/rdf-schema#label',Label,
                 Reason) :-
    !,
    refute_basetype_elt(Label,'http://www.w3.org/2001/XMLSchema#string',Base_Reason),
    Reason = Base_Reason.put(_{'vio:subject' : X,
                               'vio:property' : 'http://www.w3.org/2000/01/rdf-schema#label'}).
refute_insertion(_Database,
                 X,'http://www.w3.org/2000/01/rdf-schema#comment',Label,
                 Reason) :-
    !,
    refute_basetype_elt(Label,'http://www.w3.org/2001/XMLSchema#string',Base_Reason),
    Reason = Base_Reason.put(_{'vio:subject' : X,
                               'vio:property' : 'http://www.w3.org/2000/01/rdf-schema#label'}).
refute_insertion(Database,X,P,_Y,Reason) :-
    % \+ P = 'rdf:type'
    (   domain(P,Domain,Database)
    ->  refute_node_at_class(Database,X,Domain,Reason)
    ;   interpolate(['The property ', P,' has an undefined domain.'],Message),
        Reason = _{
                     '@type' : 'vio:PropertyWithUndefinedDomain',
                     'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                     'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'}
                 }
    ).
refute_insertion(Database,_X,P,Y,Reason) :-
    % \+ P = 'rdf:type'
    (   range(P,Range,Database)
    ->  refute_node_at_range(Database,Y,Range,Reason)
    ;   format(atom(Message),'The property ~q has an undefined domain.',[P]),
        Reason = _{
                     '@type' : 'vio:PropertyWithUndefinedRange',
                     'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                     'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'}
                 }
    ).
refute_insertion(Database,X,P,_Y,Reason) :-
    refute_all_restrictions(Database,X,P,Reason).
refute_insertion(Database,X,P,Y,Reason) :-
    refute_functional_property(X,P,Y,Database,Reason).
refute_insertion(Database,X,P,Y,Reason) :-
    refute_inverse_functional_property(X,P,Y,Database,Reason).

/*
 * refute_node_at_class(+D,+G,+X,+C,Reason) is nondet.
 *
 * Refute that X is at class C.
 */
refute_node_at_class(Database,X,Class,Reason) :-
    (   instanceClass(X,IC,Database)
    ->  (   subsumptionOf(IC,Class,Database)
        ->  false
        ;   format(atom(Message),'The subject ~q has a class ~q not subsumed by ~q.',[X,IC,Class]),
            Reason = _{
                         '@type' : 'vio:InstanceSubsumptionViolation',
                         'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                         'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                         'vio:class' : _{ '@value' : IC, '@type' : 'xsd:anyURI' },
                         'vio:parent' : _{ '@value' : Class, '@type' : 'xsd:anyURI' }
                     })
    ;   format(atom(Message),'The subject ~q has no defined class.',[X]),
        Reason = _{
                     '@type' : 'vio:UntypedInstance',
                     'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' }
                 }).

/*
 * refute_node_at_range(+Database,+Graph,+X,+Class,-Reason) is nondet.
 *
 * This does extra work for datatypes, the passes to the generic class check.
 */
refute_node_at_range(_Database,X,Class,Reason) :-
    is_literal(X),
    !,
    refute_basetype_elt(X,Class,Reason).
refute_node_at_range(Database,X,Class,Reason) :-
    % \+ is_literal(X)
    refute_node_at_class(Database,X,Class,Reason).

/*
 * refute_sp_restriction(+Database,+Graph,+X,+P,-Reason) is nondet.
 *

forall R \in Resrictions |- R < dom(p) => S |- R p Ca card
    S,G' |- Ca(x p y)

 */
refute_all_restrictions(Database,X,P,Reason) :-
    % Domain existence must be checked separately
    instanceClass(X,C,Database),
    restrictionOnProperty(CR,P,Database),
    subsumptionOf(CR,C,Database),
    % check to see does X satisfy each restriction
	refute_restriction(Database,X,CR,P,Reason).

/*
 * refute_deletion(Database,X,P,Y,Reason) is nondet.
 *
 * We no longer need to show satisfaction of domain and range
 * as we have been taken off the charts, but we do have to show
 * that cardinalities have not been violated.

deletes following the following pattern:

forall (x p y) \in Deletes.

  forall R \in Resrictions |- R < dom(p) => S |- R p Ca card
    S,G' |- Ca(x p y)

*/

/* Is this even valid? If we have deleted this it shouldn't exist...
refute_deletion(Database,
                X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',_Class,
                Reason) :-
    !,
    database_instance(Database,Instance),
    \+ xrdf(Database,Instance,X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',_SomeClass),
    interpolate(['The subject ',X,' has no defined class.'],Message),
    Reason = _{
                 '@type' : 'vio:UntypedInstance',
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' }
             }.
*/
refute_deletion(Database,X,P,_Y,Reason) :-
    refute_all_restrictions(Database,X,P,Reason).
refute_deletion(Database,X,P,Y,Reason) :-
    refute_functional_property(X,P,Y,Database,Reason).
refute_deletion(Database,X,P,Y,Reason) :-
    refute_inverse_functional_property(X,P,Y,Database,Reason).

%%%%%%%%%%%%%%%%%%%%%%
%%   RESTRICTIONS   %%
%%%%%%%%%%%%%%%%%%%%%%

/*
 * refute_restriction(X,CR,Reason)
 *
 * X is not an element of the restriction CR at P (for Reason)
 */
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CR,owl:someValuesFrom,C),
    forall(inferredEdge(X,P,Y,Database),
           (   \+ refute_node_at_range(Database,Y,C,_Reason)
           )),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedRestrictionViolation',
			     'vio:message' : 'No values from restriction class',
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CR,owl:allValuesFrom,C),
    inferredEdge(X,P,Y,Database),
    refute_node_at_range(Database,Y,C,Reason),
    interpolate(['Some values not from restriction class: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CR,owl:minCardinality,literal(type(xsd:nonNegativeInteger, CardStr))),
    coerce_number(CardStr,N),
    card(X,P,_,Database,M),
    M < N, atom_number(A,M),
    interpolate(['Cardinality not great enough for restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CR,owl:maxCardinality,literal(type(xsd:nonNegativeInteger, CardStr))),
    coerce_number(CardStr,N),
    card(X,P,_,Database,M),
    N < M, atom_number(A,M),
    interpolate(['Cardinality too great enough for restrction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CR,owl:cardinality,literal(type(xsd:nonNegativeInteger, CardStr))),
    coerce_number(CardStr,N),
    card(X,P,_,Database,M),
    N \= M, atom_number(A,M),
    interpolate(['Cardinality does not match for restrction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CR,owl:minQualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr))),
    xrdf(Database,Schema,CR,owl:onClass,C),
    coerce_number(CardStr,N),
    qualifiedCard(X,P,_,C,Database,M),
    M < N, atom_number(A,M),
    interpolate(['Cardinality too low for restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CR,owl:maxQualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr))),
    xrdf(Database,Schema,CR,owl:onClass,C),
    coerce_number(CardStr,N),
    qualifiedCard(X,P,_,C,Database,M),
    N < M, atom_number(A,M),
    interpolate(['Cardinality too high for restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CR,owl:qualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr))),
    xrdf(Database,Schema,CR,owl:onClass,C),
    coerce_number(CardStr,N),
    qualifiedCard(X,P,_,C,Database,N),
    N \= M, atom_number(A,M),
    interpolate(['Cardinality unequal on restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceQualifiedCardinalityRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
			     'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:cardinality' : _{ '@value' : A, '@type' : 'xsd:string' },
                 'vio:qualified_class' : _{ '@value' : C, '@type' : 'xsd:anyURI' }
             }.
refute_restriction(Database,X,CR,P,Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CR,owl:hasValue,V),
    inferredEdge(X,P,Y,Database),
    Y \= V,
    interpolate(['Wrong value on restriction: ',CR],Msg),
    Reason = _{
                 '@type' : 'vio:InstanceValueRestrictionViolation',
			     'vio:message' : _{ '@value' : Msg, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:restriction' : _{ '@value' : CR, '@type' : 'xsd:anyURI' },
                 'vio:value' : _{ '@value' : V, '@type' : 'xsd:anyURI' }
             }.

/**
 * refute_functional_property(?X,?P,?Y,+Instance:atom,+Schema:atom,-Reason:vio) is nondet.
 *
 * Determines of ?P is actually a functional property or not.
 */
refute_functional_property(X,P,Y,Database,Reason) :-
    functionalProperty(P,Database),
    database_instance(Database,Instance),
    xrdf(Database,Instance,X,P,_),
    card(X,P,_,Database,N),
    N \= 1,
    interpolate(['Functional Property ',P,' is not functional.'],Message),
    Reason = _{
                 '@type' : 'vio:FunctionalPropertyViolation',
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:object' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.

/**
 * refute_inverse_functional_property(?X,?P,?Y,+Instance:atom,+Schema:atom,-Reason:vio) is nondet.
 *
 * Determines of ?P is actually an inverse functional property or not.
 */
refute_inverse_functional_property(X,P,Y,Database,Reason) :-
    database_instance(Database,Instance),
    inverseFunctionalProperty(P,Database),
    xrdf(Database,Instance,_,P,Y),
    card(_,P,Y,Database,N),
    N \= 1,
    interpolate(['Inverse Functional Property ',P,' is not inverse functional.'],Message),
    Reason = _{
                 '@type' : 'vio:FunctionalPropertyViolation',
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string' },
			     'vio:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
                 'vio:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:object' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.

days_in_month(_,1,31).
days_in_month(Y,2,D) :- (Y mod 4 =:= 0 -> D = 29 ; D = 28).
days_in_month(_,3,31).
days_in_month(_,4,30).
days_in_month(_,5,31).
days_in_month(_,6,30).
days_in_month(_,7,31).
days_in_month(_,8,31).
days_in_month(_,9,30).
days_in_month(_,10,31).
days_in_month(_,11,30).
days_in_month(_,12,31).

%%%%%%%%%%%%%%%%%%%%%%
%%  BASETYPES ONLY  %%
%%%%%%%%%%%%%%%%%%%%%%

/*
 * refute_basetype_elt(+Literal,+Type,-Reason)
 */
refute_basetype_elt(literal(lang(L,S)),'http://www.w3.org/2001/XMLSchema#string',Reason) :-
    (   \+ atom(L), term_to_atom(lang(L,S),A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom in language section, found term.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt(literal(lang(L,S)),'http://www.w3.org/2001/XMLSchema#string',Reason) :-
    (   \+ (atom(S) ; string(S)), term_to_atom(lang(L,S),A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom in language section, found term.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt(literal(type(T,S)),'http://www.w3.org/2001/XMLSchema#string',Reason) :-
    (   \+ (atom(S) ; string(S)), term_to_atom(type(T,S),A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom, found term as element.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt(literal(type(T,S)),'http://www.w3.org/2001/XMLSchema#string',Reason) :-
    (   \+ atom(T), term_to_atom(type(T,S),A)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom, found term as type.',
			         'vio:literal' : _{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt(literal(type(T2,_)),T1,Reason) :-
    (   \+ basetypeSubsumptionOf(T1,T2)
    ->  Reason = _{
                     '@type' : 'vio:DataTypeSubsumptionViolation',
                     'vio:message' : 'Could not subsume type1:required_type with type2:found_type',
			         'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : T1},
			         'vio:parent_type' : _{ '@type' : 'xsd:string', '@value' : T2}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#coordinatePolygon', Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:coordinatePolygon(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed coordinate polygon',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:coordinatePolygon'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#coordinatePolyline', Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:coordinatePolygon(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed coordinate polyline',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:coordinatePolyline'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#coordinate', Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:point(_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed coordinate',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:coordinate'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#dateRange', Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:dateRange(_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed dateRange',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:dateRange'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#integerRange', Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:integerRange(_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed integerRange',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:integerRange'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#decimalRange', Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:decimalRange(_,_),C,[]))
	->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed decimalRange',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:decimalRange'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#gYearRange', Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:gYearRange(_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed gYearRange',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:gYearRange'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#url', Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:url,C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a valid URL',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:url'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#email', Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:email,C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a valid email address',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:email'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://terminusdb.com/schema/xdd#json', Reason) :-
    (   \+ (catch(atom_json_dict(S,_,[]),_,fail))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a valid json object',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xdd:json'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#boolean',Reason) :-
    (   \+ (atom_string(A,S), member(A,['true','false','1','0']))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed boolean.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:boolean'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#decimal',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:decimal(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed decimal.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:decimal'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#integer',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed integer.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:integer'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#double',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:double(_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed double.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:double'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#double',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:double(M,_,_),C,[]),
        abs(M) > 2 ^ 53
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed double: Mantisa is massive.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:double'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#double',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:double(_,E,_),C,[]),
        (   E > 970
        ;   E < -1075)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Not a well formed double: exponent excessive.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:double'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#float',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:double(_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed float.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:float'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#float',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:double(M,_,_),C,[]),
        abs(M, N), Max is 2 ^ 24, N > Max
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed float: mantisa is massive.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:float'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#float',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:double(_,E,_),C,[]),
        (   E > 104
        ;   E < -149)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed float: exponent excessive.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:float'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#time',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:time(_,_,_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:time',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:time'}
                 }
    ).
refute_basetype_elt(literal(type(_,String)),'http://www.w3.org/2001/XMLSchema#time',Reason) :-
    (   atom_codes(String,C), phrase(xsd_parser:time(H,M,S,Z,ZH,ZM),C,[]),
        (   H > 23
        ;   M > 59
        ;   (\+ member(Z,[1,-1]))
        ;   ZH > 6
        ;   ZM > 59)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:time : parameter out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:time'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#date',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:date(_,_,_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:date.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:date'}
                 }
    ).
refute_basetype_elt(literal(type(_,date_time(SY,Mo,D,H,M,S))),'http://www.w3.org/2001/XMLSchema#dateTime',Reason) :-
    (   (   Mo > 12
        ;   Mo < 1
        ;   days_in_month(SY,Mo,Days), D > Days
        ;   D < 1
        ;   H < 0
        ;   H > 23
        ;   M < 0
        ;   M > 59
        ;   S < 0
        ;   S > 59)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:dateTime : parameter out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:dateTime'}
                 }
    ).
refute_basetype_elt(literal(type(_,Atom)),'http://www.w3.org/2001/XMLSchema#dateTime',Reason) :-
    (   atom(Atom), \+ (atom_codes(Atom,C), phrase(xsd_parser:dateTime(_,_,_,_,_,_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:dateTime.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : Atom},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:dateTime'}
                 }
    ).
refute_basetype_elt(literal(type(_,Atom)),'http://www.w3.org/2001/XMLSchema#dateTime',Reason) :-
    (   atom(Atom), atom_codes(Atom,C), phrase(xsd_parser:dateTime(SY,Mo,D,H,M,S,Z,ZH,ZM),C,[]),
        (   Mo > 12
        ;   Mo < 1
        ;   days_in_month(SY,Mo,Days), D > Days
        ;   D < 1
        ;   H < 0
        ;   H > 23
        ;   M < 0
        ;   M > 59
        ;   S < 0
        ;   S > 59
        ;   (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59 )
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:dateTime : parameter out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:dateTime'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gYear',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:gYear(_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYear',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gYear'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gYear',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:gYear(_,Z,ZH,ZM),C,[]),
        (   (\+ member(Z,[1,-1]))
        ;   ZH > 6
        ;   ZM > 59)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYear : parameters out of range',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gYear'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gMonth',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:gMonth(_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:Month',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gMonth'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gMonth',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:gMonth(M,Z,ZH,ZM),C,[]),
        (   M > 12
        ;   M < 1
        ;   (\+ member(Z,[1,-1]))
        ;   ZH > 6
        ;   ZM > 59)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gMonth : parameters out of range',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gMonth'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gDay',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:gDay(_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gMonth',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gMonth'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gDay',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:gDay(D,Z,ZH,ZM),C,[]),
        (   D < 1
        ;   D > 31
        ;   (\+ member(Z,[1,-1]))
        ;   ZH > 6
        ;   ZM > 59)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gMonth : parameters out of range',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gMonth'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gYearMonth',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:gYearMonth(_,_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYearMonth',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gYearMonth'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gYearMonth',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:gYearMonth(_,M,Z,ZH,ZM),C,[]),
        (   M > 12
        ;   M < 1
        ;   (\+ member(Z,[1,-1]))
        ;   ZH > 6
        ;   ZM > 59)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYearMonth : parameters out of range',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gYearMonth'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gMonthDay',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:gMonthDay(_,_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYearMonth',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gMonthDay'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#gMonthDay',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:gMonthDay(M,D,Z,ZH,ZM),C,[]),
        (   M > 12
        ;   M < 1
        ;   D < 1
        ;   D > 31
        ;   (\+ member(Z,[1,-1]))
        ;   ZH > 6
        ;   ZM > 59)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gMonthDay : parameters out of range',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:gMonthDay'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#duration',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:duration(_,_,_,_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:duration',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:duration'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#yearMonthDuration',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:yearMonthDuration(_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:yearMonthDuration',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:yearMonthDuration'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#dayTimeDuration',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:dayTimeDuration(_,_,_,_,_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:dayTimeDuration',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:dayTimehDuration'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#byte',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:byte',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:byte'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#byte',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:integer(I),C,[]),
        (   I < -128
        ;   I > 127 )
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:byte: out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:byte'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#short',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:short',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:short'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#short',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:integer(I),C,[]),
        (I < -32768 ; I > 32767 )
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:short: out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:short'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#int',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:int',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:int'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#int',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:integer(I),C,[]),
        (   I < -2147483648
        ;   I > 2147483647 )
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:int: out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:int'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#long',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:long',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:long'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#long',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:integer(I),C,[]),
        (   I < -9223372036854775808
        ;   I > 9223372036854775807 )
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:long: out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:long'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#unsignedByte',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedByte',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedByte'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#unsignedByte',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
        (I < 0 ; I > 255 )
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedByte: out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedByte'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#unsignedShort',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedShort',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedShort'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#unsignedShort',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
        (I < 0 ; I > 65535 )
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedShort: out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedShort'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#unsignedInt',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedInt',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedInt'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#unsignedInt',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
        (I < 0 ; I > 4294967295 )
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedInt: out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedInt'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#unsignedLong',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedLong',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedLong'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#unsignedLong',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
        (I < 0 ; I > 18446744073709551615 )
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedLong: out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedLong'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#positiveInteger',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:positiveInteger',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:positiveInteger'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#positiveInteger',Reason) :-
    (   atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
        I < 1
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:positiveInteger: out of range.',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:positiveInteger'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:nonNegativeInteger',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
	                 'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:nonNegativeInteger'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#negativeInteger',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:negativeInteger(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:negativeInteger',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:negativeInteger'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#nonPositiveInteger',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:nonPositiveInteger(_),C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:nonPositiveInteger',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:nonPositiveInteger'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#base64Binary',Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:base64Binary,C,[]))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:base64Binary',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:base64Binary'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#anyURI',Reason) :-
    (   \+ uri_components(S,_)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:anyUri',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:anyURI'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#language',Reason) :-
    (   \+ uri_components(xsd_parser:language,_)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:language',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:language'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#normalizedString',Reason) :-
    (   \+ uri_components(xsd_parser:normalizedString,_)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:normalizedString',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:normalizedString'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#token',Reason) :-
    (   \+ uri_components(xsd_parser:normalizedString,_)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:token',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:token'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#NMTOKEN',Reason) :-
    (   \+ uri_components(xsd_parser:nmtoken,_)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:NMTOKEN',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:NMTOKEN'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#Name',Reason) :-
    (   \+ uri_components(xsd_parser:name,_)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:Name',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:Name'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#NCName',Reason) :-
    (   \+ uri_components(xsd_parser:ncname,_)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:NCName',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:NCName'}
                 }
    ).
refute_basetype_elt(literal(type(_,S)),'http://www.w3.org/2001/XMLSchema#NCName',Reason) :-
    (   \+ uri_components(xsd_parser:ncname,_)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:NCName',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : S},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:NCName'}
                 }
    ).
refute_basetype_elt(literal(T),'http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral',Reason) :-
    (   (   lang(_,_) \= T
        ;   \+ atom(T))
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed rdf:PlainLiteral',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : T},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'rdf:PlainLiteral'}
                 }
                 %% All that follows looks a bit dodgy...  Probably not up to spec
    ).
refute_basetype_elt(X,'http://www.w3.org/2000/01/rdf-schema#Literal',Reason) :-
    (   literal(_) \= X, term_to_atom(X,T)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed Literal',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : T},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'rdfs:Literal'}
                 }
    ).
refute_basetype_elt(X,'http://www.w3.org/2000/01/rdf-schema#XMLLiteral',Reason) :-
    (   literal(_) \= X, term_to_atom(X,T)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed rdfs:XMLLiteral',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : T},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'rdfs:XMLLiteral'}
                 }
    ).
refute_basetype_elt(X,'http://www.w3.org/2000/01/rdf-schema#anySimpleType',Reason) :-
    (   literal(_) \= X, term_to_atom(X,T)
    ->  Reason = _{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:anySimpleType',
                     'vio:literal' : _{ '@type' : 'xsd:anySimpleType', '@value' : T},
                     'vio:base_type' : _{ '@type' : 'xsd:string', '@value' : 'xsd:anySimpleType'}
                 }
    ).
