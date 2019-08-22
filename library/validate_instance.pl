:- module(validate_instance,[
              instanceClass/3,
              most_specific_type/3
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
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
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

/**
 * most_specific_type(+Entity, -Sorted, +Database)
 *
 * Gets the most specific type for a class 
 **/
most_specific_type(Entity, Entity_Type, Database):-
    get_ordered_instance_classes(Entity, [Entity_Type|_], Database).

/**
 * get_ordered_instance_classes(+Entity, -Sorted, +Database)
 *
 * Gets all classes for a specific entity and returns
 * them ordered by subsumption.  
 **/
get_ordered_instance_classes(Entity, Sorted, Database) :-
    findall(
        Class,
        instanceClass(Entity, Class, Database),
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
 * instanceClass(?X:uri, ?C:uri, +Database:graph) is nondet.
 * 
 * Determines the class C identified with the instance X.
 */
instanceClass(X, Y, Database) :-
    database_name(Database,Collection),
    database_instance(Database,Instance),
    xrdf(Collection,Instance, X, rdf:type, Y).
instanceClass(X, Y, Database) :-
    database_name(Database,Collection),
    database_schema(Database,Schema), % instances can also exist in the schema
    xrdf(Collection,Schema, X, rdf:type, Y).

% X has cardinality N at property OP
card(X,OP,Y,Database,N) :-
    (setof(Y,inferredEdge(X,OP,Y,Database), ListX) *-> ListX = L ; L = []),
    length(L,N).

% X has qualified cardinality N at property OP and class C
qualifiedCard(X,OP,Y,C,Database,N) :-
    (setof(Y,(inferredEdge(X,OP,Y,Database),
	      \+ nelt(Y,C,Database,_)),
	   ListX) *-> ListX = L ; L = []),
    length(L,N).

/*
inserts: 

forall (x p y) \in Inserts. 
  p = 'rdf:type' => S,G |- x : y 
  p /= 'rdf:type' => 
    S,G |- x : dom(p) /\ rng(p)

  forall R \in Resrictions |- R < dom(p) => S |- R p Ca card
    S,G' |- Ca(x p y)

deletes: 

forall (x p y) \in Deletes. 
  p = 'rdf:type' => S,G |- x : y 
  p /= 'rdf:type' => 
    S,G |- x : dom(p) /\ rng(p)

  forall R \in Resrictions |- R < dom(p) => S |- R p Ca card
    S,G' |- Ca(x p y)

*/ 



/*

/** 
 * edge_orphan_instance(?X,?P,?Y,+Database:graph,-Reason:rvo) is nondet.
 * 
 * Detects the existence of instance who are orphaned from a well defined class.
 * i.e. X(/Y) has no class in the instance AND schema OR has a class that is not in the schema 
 */
edge_orphan_instance(X,P,Y,Database,Reason) :-
    database_name(Database,Collection),
    database_instance(Database,Instance), 
    xrdf(Collection,Instance,X,P,Y), % edge exists 
    \+ instanceClass(X, _, Database),   % no type in Instance OR Schema
    Reason=_{
               '@type' : 'rvo:EdgeOrphanInstanceViolation',
	           'rvo:message' : _{ '@value' : 'Instance has no class', '@type' : 'xsd:string'},
	           'rvo:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI'},
               'rvo:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI'},
               'rvo:object' : _{ '@value' : Y, '@type' : 'xsd:anyURI'}
           }.
edge_orphan_instance(X,P,Y,Database,Reason) :-
    database_instance(Database,Instance),
    xrdf(X,P,Y,Instance), % Added
    orphanInstance(X,C,Database),            % checks if X has a type that isn't in the Schema
    Reason=_{
               '@type' : 'rvo:EdgeOrphanInstanceViolation',
	           'rvo:message' : _{ '@value' : 'Instance has no class', '@type' : 'xsd:string'},
	           'rvo:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI'},
               'rvo:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI'},
               'rvo:object' : _{ '@value' : Y, '@type' : 'xsd:anyURI'},
               'rvo:class' :  _{ '@value' : C, '@type' : 'xsd:anyURI'},
           }.
edge_orphan_instance(X,P,Y,Database,Reason) :-
    objectProperty(P,Database),
    database_instance(Database,Instance),
    xrdf(X,P,Y,Instance),
    rdf_is_iri(Y), % no point in checking if it is a literal
    \+ instanceClass(Y, _, Database),        % no type in Instance OR Schema
    Reason=_{
               '@type' : 'rvo:EdgeOrphanInstanceViolation',
	           'rvo:message' : _{ '@value' : 'Instance has no class', '@type' : 'xsd:string'},
	           'rvo:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI'},
               'rvo:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI'},
               'rvo:object' : _{ '@value' : Y, '@type' : 'xsd:anyURI'},
           }.
edge_orphan_instance(X,P,Y,Database,Reason) :-
    objectProperty(P,Database),
    database_instance(Database,Instance),
    xrdf(X,P,Y,Instance), % Added
    rdf_is_iri(Y), % no point in checking if it is a literal
    orphanInstance(Y,C,Database), % checks other side of triple to see does it have a type not in the schema
    Reason=_{
               '@type' : 'rvo:EdgeOrphanInstanceViolation',
	           'rvo:message' : _{ '@value' : 'Instance has no class', '@type' : 'xsd:string'},
	           'rvo:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI'},
               'rvo:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI'},
               'rvo:object' : _{ '@value' : Y, '@type' : 'xsd:anyURI'},
               'rvo:class' :  _{ '@value' : C, '@type' : 'xsd:anyURI'},
           }.

/** 
 * noPropertyRangeIC(?X,?P,?Y,+Instance:atom,+Schema:atom,-Reason:rvo) is nondet.
 * 
 * Dectects the precence of properties without well defined range.
 *   
 * The triple (X,P,Y) comes from the Herbrand base. 
 */
:- rdf_meta noPropertyRangeIC(r,r,r,o,o,t).
noPropertyRangeIC(X,P,Y,Database,Reason) :-
    property(P,Database),
    database_name(Database,Collection),
    database_instance(Database,Instance),    
    xrdf(X,P,Y,Instance), % Added
    %subsumptionPropertiesOf(P,SuperP,Schema),
    \+ anyRange(P,_,Database),
    Reason=_{
               '@type' : 'rvo:NoPropertyRangeViolation',
	           'rvo:message' : _{ '@value' : 'Property has no well defined range',
                                  '@type' : 'xsd:string'},
	           'rvo:subject' : _{ '@value' : X, '@type' : 'xsd:anyURI'},
               'rvo:predicate' : _{ '@value' : P, '@type' : 'xsd:anyURI'},
               'rvo:object' : _{ '@value' : Y, '@type' : 'xsd:anyURI'}
           }.

/** 
 * notFunctionalPropertyIC(?X,?P,?Y,+Instance:atom,+Schema:atom,-Reason:rvo) is nondet.
 * 
 * Determines of ?P is actually a functional property or not. 
 */
notFunctionalPropertyIC(X,P,_,Database,Reason) :-
    functionalProperty(P,Database),
    database_instance(Database,Instance),
    xrdf(X,P,_,Instance),
    card(X,P,_,Database,N),
    N \= 1,
    interpolate(['Functional Property ',P,' is not functional.'],Message),
    Reason = ['rdf:type'='NotFunctionalPropertyViolation',
			  bestPractice=literal(type('xsd:boolean',false)),
			  subject=X,
			  predicate=P,
			  message=Message].

/** 
 * notInverseFunctionalPropertyIC(?X,?P,?Y,+Instance:atom,+Schema:atom,-Reason:rvo) is nondet.
 * 
 * Determines of ?P is actually an inverse functional property or not. 
 */
notInverseFunctionalPropertyIC(X,P,Y,Database,Reason) :-
    database_instance(Database,Instance),
    inverseFunctionalProperty(P,Database),
    xrdf(_,P,Y,Instance),
    card(_,P,Y,Database,N),
    N \= 1,
    interpolate(['Functional Property ',P,' is not functional.'],Message),
    Reason = ['rdf:type'='NotInverseFunctionalPropertyViolation',
			  bestPractice=literal(type('xsd:boolean',false)),
			  subject=X,
			  predicate=P,
			  object=Y,
			  message=Message].

/**
 * localPropertyIC(?X,?P,?Y,+Instance:atom,+Schema:atom,-Reason:rvo) is nondet.
 * 
 * localOrphanProperty/6 checks if property on an instance has range, domain or 
 * is defined as a subproperty but is not actually defined as a property in the 
 * schema.
 **/
localOrphanPropertyIC(X,P,Y,Database,[rvo:subject=X,rvo:object=Y|Reason]) :-
    database_instance(Database,Instance),
	xrdf(X,P,Y,Instance),
    \+ P='http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
	orphanProperty(P,_,Database,Reason).

instanceSubjectBlankNode(X,Database) :-
    database_instance(Database,Instance),
    xrdf(X,_,_,Instance),
    rdf_is_bnode(X).
instancePredicateBlankNode(Y,Database) :-
    database_instance(Database,Instance),
    xrdf(_,Y,_,Instance),
    rdf_is_bnode(Y).
instanceObjectBlankNode(Z,Database) :-
    database_instance(Database,Instance),
    xrdf(_,_,Z,Instance),
    rdf_is_bnode(Z).

/** 
 * instanceBlankNodeIC(?X,?P,?Y,+Instance:atom,+Schema:atom,-Reason:rvo) is nondet.
 * 
 * Determines Any of X, P or Y are blank nodes and gives a best practice Reason for failure.
 */    
instanceBlankNodeIC(X,_P,_Y,Database,Reason) :-
    instanceSubjectBlankNode(X,Database),
    interpolate(['The subject ', X, ' is a blank node'],Message),
    Reason=['rdf:type'='InstanceBlankNodeViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    message=Message,
	    subject=X].
instanceBlankNodeIC(_,X,_,Database,Reason) :-
    instancePredicateBlankNode(X,Database),
    interpolate(['The predicate ', X, ' is a blank node'],Message),
    Reason=['rdf:type'='InstanceBlankNodeViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    message=Message,
	    predicate=X].
instanceBlankNodeIC(_,_,X,Database,Reason) :-
    instanceObjectBlankNode(X,Database),
    interpolate(['The object ', X, ' is a blank node'],Message),
    Reason=['rdf:type'='InstanceBlankNodeViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    message=Message,
	    object=X].		

days_in_month(_,1,31).
days_in_month(Y,2,D) :- Ans is Y mod 4, Ans = 0 -> D = 29 ; D = 28 .
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

:- rdf_meta n_basetype_elt(r,r,t).
n_basetype_elt(literal(lang(S,L)),xsd:string,Reason) :-
    (   \+ atom(S), term_to_atom(lang(S,L),A)
    ->  Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			      rvo:message='Expected atom in string section, found term.',
			      rvo:literal=A]
    ).
n_basetype_elt(literal(lang(S,L)),xsd:string,Reason) :-
    (   \+ atom(L), term_to_atom(lang(S,L),A)
    ->  Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			      rvo:message='Expected atom in language section, found term.',
			      rvo:literal=A]
    ).
n_basetype_elt(literal(type(T,S)),xsd:string,Reason) :-
    (   \+ (atom(S) ; string(S)), term_to_atom(type(T,S),A)
    ->  Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			      rvo:message='Expected atom, found term as element.',
			      rvo:literal=A]
    ).
n_basetype_elt(literal(type(T,S)),xsd:string,Reason) :-
    (   \+ atom(T), term_to_atom(type(T,S),A)
    ->  Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			      rvo:message='Expected atom, found term as type.',
			      rvo:literal=A]
    ).
n_basetype_elt(literal(type(T2,_)),T1,Reason) :-
    (   \+ basetypeSubsumptionOf(T1,T2)
    ->  Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			      rvo:message='Could not subsume type1:required_type with type2:found_type',
			      rvo:type1=T1,
			      rvo:type2=T2]
    ).
n_basetype_elt(literal(type(_,S)),xdd:coordinatePolygon, Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:coordinatePolygon(_),C,[]))
    ->  Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			      rvo:message='Not a well formed coordinate polygon',
			      rvo:literal=S,
			      rvo:type=xdd:coordinatePolygon]
    ).
n_basetype_elt(literal(type(_,S)),xdd:coordinatePolyline, Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:coordinatePolygon(_),C,[]))
    ->  Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			      rvo:message='Not a well formed coordinate polyline',
			      rvo:literal=S,
			      rvo:type=xdd:coordinatePolyline]
    ).
n_basetype_elt(literal(type(_,S)),xdd:coordinate, Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:point(_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed coordinate',
			  rvo:literal=S,
			  rvo:type=xdd:coordinate].
n_basetype_elt(literal(type(_,S)),xdd:integerRange, Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:integerRange(_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed integerRange',
			  rvo:literal=S,
			  rvo:type=xdd:integerRange].
n_basetype_elt(literal(type(_,S)),xdd:decimalRange, Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:decimalRange(_,_),C,[]))
	->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed decimalRange',
			  rvo:literal=S,
			  rvo:type=xdd:decimalRange].
n_basetype_elt(literal(type(_,S)),xdd:gYearRange, Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:gYearRange(_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed gYearRange',
			  rvo:literal=S,
			  rvo:type=xdd:gYearRange].
n_basetype_elt(literal(type(_,N)),xdd:pesel, Reason) :-
	integer(N),
	\+ (atom_number(Spre_pad,N), dqs_utils:zeroPad(Spre_pad,11,S), atom_codes(S,C), phrase(xsd_parser_ipg:pesel(_,_,_,_,_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed pesel',
			  rvo:literal=N,
			  rvo:type=xdd:pesel].
n_basetype_elt(literal(type(_,S)),xdd:pesel, Reason) :-
	\+ integer(S),
	\+ (atom_codes(S,C), phrase(xsd_parser_ipg:pesel(_,_,_,_,_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed pesel',
			  rvo:literal=S,
			  rvo:type=xdd:pesel].
n_basetype_elt(literal(type(_,N)),xdd:pesel, Reason) :-
	integer(N),
	\+ (atom_number(Spre_pad,N), dqs_utils:zeroPad(Spre_pad,11,S),atom_codes(S,C), phrase(xsd_parser_ipg:peselCheck,C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Pesel # is not valid',
			  rvo:literal=N,
			  rvo:type=xdd:pesel].
n_basetype_elt(literal(type(_,S)),xdd:pesel, Reason) :-
	\+ integer(S),
	\+ (atom_codes(S,C), phrase(xsd_parser_ipg:peselCheck,C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Pesel # is not valid',
			  rvo:literal=S,
			  rvo:type=xdd:pesel].
n_basetype_elt(literal(type(_,S)),xdd:url, Reason) :-
	\+ (atom_codes(S,C), phrase(xsd_parser:url,C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a valid URL',
			  rvo:literal=S,
			  rvo:type=xdd:url].
n_basetype_elt(literal(type(_,S)),xdd:email, Reason) :-
	\+ (atom_codes(S,C), phrase(xsd_parser:email,C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a valid email address',
			  rvo:literal=S,
			  rvo:type=xdd:email].
n_basetype_elt(literal(type(_,S)),xsd:boolean,Reason) :-
    \+ member(S,['true','false','1','0']), term_to_atom(S,A)
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed boolean.',
			  rvo:literal=A,
			  rvo:type=xsd:boolean].
n_basetype_elt(literal(type(_,S)),xsd:decimal,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:decimal(_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed decimal.',
			  rvo:literal=S,
			  rvo:type=xsd:decimal].
n_basetype_elt(literal(type(_,S)),xsd:integer,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed integer.',
			  rvo:literal=S,
			  rvo:type=xsd:integer].
n_basetype_elt(literal(type(_,S)),xsd:double,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:double(_,_,_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed double.',
			  rvo:literal=S,
			  rvo:type=xsd:double].
n_basetype_elt(literal(type(_,S)),xsd:double,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:double(M,_,_),C,[]),
    abs(M, N), Max is 2 ^ 53, N > Max
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed double: Mantisa is massive.',
			  rvo:literal=S,
			  rvo:type=xsd:double].
n_basetype_elt(literal(type(_,S)),xsd:double,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:double(_,E,_),C,[]),
    (E > 970 ; E < -1075)
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  age=rvo:'Not a well formed double: exponent excessive.',
			  rvo:literal=S,
			  rvo:type=xsd:double].
n_basetype_elt(literal(type(_,S)),xsd:float,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:double(_,_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
	          rvo:message='Not a well formed float.',
	          rvo:literal=S,
	          rvo:type=xsd:float].
n_basetype_elt(literal(type(_,S)),xsd:float,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:double(M,_,_),C,[]),
    abs(M, N), Max is 2 ^ 24, N > Max
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed float: mantisa is massive.',
			  rvo:literal=S,
			  rvo:type=xsd:float].
n_basetype_elt(literal(type(_,S)),xsd:float,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:double(_,E,_),C,[]),
    (E > 104 ; E < -149)
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed float: exponent excessive.',
			  rvo:literal=S,
			  rvo:type=xsd:float].
n_basetype_elt(literal(type(_,S)),xsd:time,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:time(_,_,_,_,_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:time',
			  rvo:literal=S,
			  rvo:type=xsd:time].
n_basetype_elt(literal(type(_,S)),xsd:time,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:time(H,M,S,Z,ZH,ZM),C,[]),
    (H > 23 ; M > 59 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59 )
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:time : parameter out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:time].
n_basetype_elt(literal(type(_,S)),xsd:date,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:date(_,_,_,_,_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:date.',
			  rvo:literal=S,
			  rvo:type=xsd:date].
n_basetype_elt(literal(type(_,date_time(SY,Mo,D,H,M,S))),xsd:dateTime,Reason) :-
	(Mo > 12 ; Mo < 1
     ; days_in_month(SY,Mo,Days), D > Days
     ; D < 1 ; H < 0 ; H > 23 ; M < 0 ; M > 59 ; S < 0 ; S > 59)
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:dateTime : parameter out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:dateTime].
n_basetype_elt(literal(type(_,Atom)),xsd:dateTime,Reason) :-
    atom(Atom), \+ (atom_codes(Atom,C), phrase(xsd_parser:dateTime(_,_,_,_,_,_,_,_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:dateTime.',
			  rvo:literal=Atom,
			  rvo:type=xsd:dateTime].
n_basetype_elt(literal(type(_,Atom)),xsd:dateTime,Reason) :-
    atom(Atom), atom_codes(Atom,C), phrase(xsd_parser:dateTime(SY,Mo,D,H,M,S,Z,ZH,ZM),C,[]),
    (Mo > 12 ; Mo < 1
     ; days_in_month(SY,Mo,Days), D > Days
     ; D < 1 ; H < 0 ; H > 23 ; M < 0 ; M > 59 ; S < 0 ; S > 59
     ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59 )
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:dateTime : parameter out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:dateTime].
n_basetype_elt(literal(type(_,S)),xsd:gYear,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:gYear(_,_,_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:gYear',
			  rvo:literal=S,
			  rvo:type=xsd:gYear].
n_basetype_elt(literal(type(_,S)),xsd:gYear,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:gYear(_,Z,ZH,ZM),C,[]),
    ((\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:gYear : parameters out of range',
			  rvo:literal=S,
			  rvo:type=xsd:gYear].
n_basetype_elt(literal(type(_,S)),xsd:gMonth,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:gMonth(_,_,_,_),C,[]))
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:Month',
			  rvo:literal=S,
			  rvo:type=xsd:gMonth].
n_basetype_elt(literal(type(_,S)),xsd:gMonth,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:gMonth(M,Z,ZH,ZM),C,[]),
    (M < 12 ; M > 1 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    -> 
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:gMonth : parameters out of range',
			  rvo:literal=S,
	          rvo:type=xsd:gMonth].
n_basetype_elt(literal(type(_,S)),xsd:gDay,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:gDay(_,_,_,_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:gMonth',
			  rvo:literal=S,
			  rvo:type=xsd:gMonth].
n_basetype_elt(literal(type(_,S)),xsd:gDay,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:gDay(D,Z,ZH,ZM),C,[]),
    (D < 1 ; D > 31 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:gMonth : parameters out of range',
			  rvo:literal=S,
			  rvo:type=xsd:gMonth].
n_basetype_elt(literal(type(_,S)),xsd:gYearMonth,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:gYearMonth(_,_,_,_,_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:gYearMonth',
			  rvo:literal=S,
			  rvo:type=xsd:gYearMonth].
n_basetype_elt(literal(type(_,S)),xsd:gYearMonth,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:gYearMonth(_,M,Z,ZH,ZM),C,[]),
    (M > 12 ; M < 1 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:gYearMonth : parameters out of range',
			  rvo:literal=S,
			  rvo:type=xsd:gYearMonth].
n_basetype_elt(literal(type(_,S)),xsd:gMonthDay,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:gMonthDay(_,_,_,_,_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:gYearMonth',
			  rvo:literal=S,
			  rvo:type=xsd:gMonthDay].
n_basetype_elt(literal(type(_,S)),xsd:gMonthDay,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:gMonthDay(M,D,Z,ZH,ZM),C,[]),
    (M > 12 ; M < 1 ; D < 1 ; D > 31 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:gMonthDay : parameters out of range',
			  rvo:literal=S,
			  rvo:type=xsd:gMonthDay].
n_basetype_elt(literal(type(_,S)),xsd:duration,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:duration(_,_,_,_,_,_,_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:duration',
			  rvo:literal=S,
			  rvo:type=xsd:duration].
n_basetype_elt(literal(type(_,S)),xsd:yearMonthDuration,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:yearMonthDuration(_,_,_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:yearMonthDuration',
			  rvo:literal=S,
			  rvo:type=xsd:yearMonthDuration].
n_basetype_elt(literal(type(_,S)),xsd:dayTimeDuration,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:dayTimeDuration(_,_,_,_,_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:dayTimeDuration',
			  rvo:literal=S,
			  rvo:type=xsd:dayTimehDuration].
n_basetype_elt(literal(type(_,S)),xsd:byte,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:byte',
			  rvo:literal=S,
			  rvo:type=xsd:byte].
n_basetype_elt(literal(type(_,S)),xsd:byte,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:integer(I),C,[]),
    (I < -128 ; I > 127 )
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:byte: out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:byte].
n_basetype_elt(literal(type(_,S)),xsd:short,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:short',
			  rvo:literal=S,
			  rvo:type=xsd:short].
n_basetype_elt(literal(type(_,S)),xsd:short,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:integer(I),C,[]),
    (I < -32768 ; I > 32767 )
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:short: out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:short].
n_basetype_elt(literal(type(_,S)),xsd:int,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:int',
			  rvo:literal=S,
			  rvo:type=xsd:int].
n_basetype_elt(literal(type(_,S)),xsd:int,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:integer(I),C,[]),
    (I < -2147483648 ; I > 2147483647 )
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:int: out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:int].
n_basetype_elt(literal(type(_,S)),xsd:long,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:integer(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:long',
			  rvo:literal=S,
			  rvo:type=xsd:long].
n_basetype_elt(literal(type(_,S)),xsd:long,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:integer(I),C,[]),
    (I < -9223372036854775808 ; I > 9223372036854775807 )
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:long: out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:long].
n_basetype_elt(literal(type(_,S)),xsd:unsignedByte,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:unsignedByte',
			  rvo:literal=S,
			  rvo:type=xsd:unsignedByte].
n_basetype_elt(literal(type(_,S)),xsd:unsignedByte,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
    (I < 0 ; I > 255 )
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:unsignedByte: out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:unsignedByte].
n_basetype_elt(literal(type(_,S)),xsd:unsignedShort,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:unsignedShort',
			  rvo:literal=S,
			  rvo:type=xsd:unsignedShort].
n_basetype_elt(literal(type(_,S)),xsd:unsignedShort,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
    (I < 0 ; I > 65535 )
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:unsignedShort: out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:unsignedShort].
n_basetype_elt(literal(type(_,S)),xsd:unsignedInt,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:unsignedInt',
			  rvo:literal=S,
			  rvo:type=xsd:unsignedInt].
n_basetype_elt(literal(type(_,S)),xsd:unsignedInt,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
    (I < 0 ; I > 4294967295 )
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:unsignedInt: out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:unsignedInt].
n_basetype_elt(literal(type(_,S)),xsd:unsignedLong,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:unsignedLong',
			  rvo:literal=S,
			  rvo:type=xsd:unsignedLong].
n_basetype_elt(literal(type(_,S)),xsd:unsignedLong,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
    (I < 0 ; I > 18446744073709551615 )
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:unsignedLong: out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:unsignedLong].
n_basetype_elt(literal(type(_,S)),xsd:positiveInteger,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:positiveInteger',
			  rvo:literal=S,
			  rvo:type=xsd:positiveInteger].
n_basetype_elt(literal(type(_,S)),xsd:positiveInteger,Reason) :-
    atom_codes(S,C), phrase(xsd_parser:positiveInteger(I),C,[]),
    I < 1 
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:positiveInteger: out of range.',
			  rvo:literal=S,
			  rvo:type=xsd:positiveInteger].
n_basetype_elt(literal(type(_,S)),xsd:nonNegativeInteger,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:positiveInteger(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:nonNegativeInteger',
			  rvo:literal=S,
	      typervo:=xsd:nonNegativeInteger].
n_basetype_elt(literal(type(_,S)),xsd:negativeInteger,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:negativeInteger(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:negativeInteger',
			  rvo:literal=S,
			  rvo:type=xsd:negativeInteger].
n_basetype_elt(literal(type(_,S)),xsd:nonPositiveInteger,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:nonPositiveInteger(_),C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:nonPositiveInteger',
			  rvo:literal=S,
			  rvo:type=xsd:nonPositiveInteger].
n_basetype_elt(literal(type(_,S)),xsd:base64Binary,Reason) :-
    \+ (atom_codes(S,C), phrase(xsd_parser:base64Binary,C,[]))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:base64Binary',
			  rvo:literal=S,
			  rvo:type=xsd:base64Binary].
n_basetype_elt(literal(type(_,S)),xsd:anyURI,Reason) :-
    \+ uri_components(S,_)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:anyUri',
			  rvo:literal=S,
			  rvo:type=xsd:anyURI].
n_basetype_elt(literal(type(_,S)),xsd:language,Reason) :-
    \+ uri_components(xsd_parser:language,_)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:language',
			  rvo:literal=S,
			  rvo:type=xsd:language].
n_basetype_elt(literal(type(_,S)),xsd:normalizedString,Reason) :-
    \+ uri_components(xsd_parser:normalizedString,_)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:normalizedString',
			  rvo:literal=S,
			  rvo:type=xsd:normalizedString].
n_basetype_elt(literal(type(_,S)),xsd:token,Reason) :-
    \+ uri_components(xsd_parser:normalizedString,_)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:token',
			  rvo:literal=S,
			  rvo:type=xsd:token].
n_basetype_elt(literal(type(_,S)),xsd:'NMTOKEN',Reason) :-
    \+ uri_components(xsd_parser:nmtoken,_)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:NMTOKEN',
			  rvo:literal=S,
			  rvo:type=xsd:'NMTOKEN'].
n_basetype_elt(literal(type(_,S)),xsd:'Name',Reason) :-
    \+ uri_components(xsd_parser:name,_)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:Name',
			  rvo:literal=S,
			  rvo:type=xsd:'Name'].
n_basetype_elt(literal(type(_,S)),xsd:'NCName',Reason) :-
    \+ uri_components(xsd_parser:ncname,_)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:NCName',
			  rvo:literal=S,
			  rvo:type=xsd:'NCName'].
n_basetype_elt(literal(type(_,S)),xsd:'NCName',Reason) :-
    \+ uri_components(xsd_parser:ncname,_)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:NCName',
			  rvo:literal=S,
			  rvo:type=xsd:'NCName'].
n_basetype_elt(literal(T),rdf:'PlainLiteral',Reason) :-
    (lang(_,_) \= T ; \+ atom(T))
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed rdf:PlainLiteral',
			  rvo:literal=T,
			  rvo:type=rdf:'PlainLiteral'].
%% All that follows looks a bit dodgy...  Probably not up to spec
n_basetype_elt(X,rdfs:'Literal',Reason) :-
    literal(_) \= X, term_to_atom(X,T)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed rdfs:Literal',
			  rvo:literal=T,
			  rvo:type=rdfs:'Literal'].
n_basetype_elt(X,rdfs:'XMLLiteral',Reason) :-
    literal(_) \= X, term_to_atom(X,T)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed rdfs:XMLLiteral',
			  rvo:literal=T,
			  rvo:type=rdfs:'XMLLiteral'].
n_basetype_elt(X,rdfs:anySimpleType,Reason) :-
    literal(_) \= X, term_to_atom(X,T)
    ->   
    Reason = [rdf:type=rvo:'NotBaseTypeElementViolation',
			  rvo:message='Not a well formed xsd:anySimpleType',
			  rvo:literal=T,
			  rvo:type=xsd:anySimpleType].
*/
