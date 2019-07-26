:- module(validate_schema,[
              % OWL semantics
	          class/2, immediateClass/2,
              restriction/2, classOrRestriction/2,
		      subClassOf/3, unionOf/3, intersectionOf/3, subClassStrict/3,
		      disjointUnionOf/3, label/3, comment/3,
		      subsumptionOf/3, strictSubsumptionOf/3, complementOf/3,
		      dcog_tag/3, entity/2,
              
		      unionOfList/3, intersectionOfList/3, disjointUnionOfList/3,
		      oneOfList/3,
		      
		      datatypeProperty/2, objectProperty/2, annotationProperty/2,
		      property/2, subPropertyOf/3, subsumptionPropertiesOf/3,
		      range/3, domain/3, anyRange/3, anyDomain/3,
		      mostSpecificDomain/3, mostSpecificRange/3,
		      collect/3, functionalProperty/2,
		      inverseFunctionalProperty/2, restrictionOnProperty/3,
		      datatypeSubsumptionOf/3, basetypeSubsumptionOf/2,
		      customDatatype/2, datatype/2,
		      strictSubsumptionPropertiesOf/3,
		      orphanProperty/4,
		      rdfMetaProperty/1,
              rdfsProperty/1,
		      % SC == Schema Constraints
		      % constraints must be pred/2
              
		      % REQUIRED Best Practice 
		      classCycleSC/2,               % Best Practice
		      propertyCycleSC/2,            % Best Practice
              
		      % Best practice
		      noImmediateDomainSC/2, noImmediateRangeSC/2,      % Best Practice
		      schemaBlankNodeSC/2, notUniqueClassLabelSC/2,       % Best Practice
		      notUniqueClassSC/2, notUniquePropertySC/2,        % Best Practice
		      noImmediateClassSC/2,
		      annotationOverloadSC/2, 
		      propertyTypeOverloadSC/2,
		
		% OWL DL (Constraint)
		orphanClassSC/2,              % OWL
		orphanPropertySC/2,           % OWL
		invalidDomainSC/2, invalidRangeSC/2,         % OWL
		domainNotSubsumedSC/2, rangeNotSubsumedSC/2  % OWL
	       ]).

/** <module> The Tbox module of semantics level checks
 *
 * This module deals with schema validation predicates as well as queries
 * for subsumption, domains, ranges, classes etc.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of RegulumDB.                                      *
 *                                                                       *
 *  RegulumDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  RegulumDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with RegulumDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

%:- use_module(library(semweb/rdf_db), except([rdf/4, rdf_retractall/4])).
%:- use_module(library(semweb/turtle)).
%:- use_module(library(mavis)).
:- use_module(management).
:- use_module(triplestore).
:- use_module(utils). 
:- use_module(types).


/*
OWL DL Syntactic correctness

It would be useful to do a complete check on the syntactic
correctness of our ontology according to the OWL 2 / RDF mapping
*/

/* 
Classes 
*/

/** 
 * immediateClass(?X:uri_or_id, +Graph:graph) is nondet.
 * immediateClass(+X:uri_or_id, +Graph:graph) is det.
 *
 * Check to see if class definitions are immediate (best practices).
 * 
 * @param X URI_OR_ID identifier for which to check if the schema has recorded a 
 *        a non-inferred rfds or owl Class.
 * @param Schema Atom idntifying the current schema graph.
*/
% :- rdf_meta immediateClass(r,o).
immediateClass(X,Graph) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection, Schema, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2000/01/rdf-schema#Class').
immediateClass(X,Graph) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection, Schema, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Class').
immediateClass('http://www.w3.org/2002/07/owl#Thing',_).
immediateClass('http://www.w3.org/2002/07/owl#Nothing',_).

%% class(?X:uri_or_id, +Schema:graph) is nondet
% class(+X:uri_or_id, +Schema:graph) is det
%
% All class designations - with inferences.
% 
% @param X URI_OR_ID identifier for which to check if the schema has recorded a 
%        an inferred rfds or owl Class.
% @param Graph object with the current schema graph.
%% :- rdf_meta class(r,o).
class(X,Graph) :- immediateClass(X,Graph). 
class(X,Graph) :- subClassOf(X,Y,Graph), class(Y,Graph).
class(X,Graph) :- equivalentClass(X,Y,Graph), class(Y,Graph).

% restriction(+R:uri_or_id, +Graph:graph) is nondet.
%
% All restriction designations - with inferences.
% 
% @param R URI_OR_ID identifier for which to check if the schema has recorded a 
%        an inferred owl Restriction.
% @param Graph identifying the current schema graph.
%% :- rdf_meta restriction(r,o).
restriction(R,Graph) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection, Schema, R, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Restriction').
restriction(R,Graph) :-
    subClassOf(R,R2,Graph),
    restriction(R2,Graph).
restriction(R,Graph) :-
    equivalentClass(R,R2,Graph),
    restriction(R2,Graph).

%% noImmediateClassSC(+Graph:graph, -Reason:any) is nondet
%
% Check to see if a class is used without a class definition.
% 
% @param Schema Graph idntifying the current schema graph.
% @param Reason A prolog representation of a JSON Linked Data structure 
%               detailing the violation.
noImmediateClassSC(Graph, Reason) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#ObjectProperty'),
    domain(P,X,Graph),
    \+ immediateClass(X,Graph), \+ restriction(X,Graph),
    interpolate([X,' is used as a domain for property ',P,' but is not defined'], Message),
    Reason = ['rdf:type'='NoImmediateClassViolation',  
	      message=Message,
	      bestPractice=literal(type('xsd:boolean',true)),
	      class=X,
	      property=P].
noImmediateClassSC(Graph, Reason) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#ObjectProperty'),
    range(P,X,Graph),
    \+ immediateClass(X,Graph), \+ restriction(X,Graph),
    interpolate([X,' is used as a range for property ',P,' but is not defined'], Message),
    Reason = ['rdf:type'='NoImmediateClassViolation',
	      bestPractice=literal(type('xsd:boolean',true)),
	      message=Message,
	      class=X,
	      property=P].
noImmediateClassSC(Graph, Reason) :-
    subClassOf(X,Y,Graph),
    \+ customDatatype(X,Graph), \+ immediateClass(X,Graph), \+ restriction(X,Graph),
    interpolate(['The class ',Y,' is not a superclass of a defined class ',X], Message),
    Reason = ['rdf:type'='NoImmediateClassViolation',
	      bestPractice=literal(type('xsd:boolean',true)),		      
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Graph, Reason) :-
    subClassOf(X,Y,Graph), 
    \+ customDatatype(Y,Graph), \+ immediateClass(Y,Graph), \+ restriction(Y,Graph),
    interpolate(['The class ',X,' is not a subclass of a defined class ',Y], Message),
    Reason = ['rdf:type'='NoImmediateClassViolation',
	      bestPractice=literal(type('xsd:boolean',true)),
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Graph, Reason) :-
    intersectionOf(X,Y,Graph),
    \+ immediateClass(X,Graph), \+ restriction(X,Graph),
    interpolate(['The class ',X,' is an intersection of ', Y,' but not a defined class'], Message),
    Reason = ['rdf:type'='NoImmediateClassViolation',
	      bestPractice=literal(type('xsd:boolean',true)),
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Graph, Reason) :-
    intersectionOf(X,Y,Graph),
    \+ immediateClass(Y,Graph), \+ restriction(Y,Graph),
    interpolate(['The class ',X,' is not an intersection of a defined class ',Y], Message),
    Reason = ['rdf:type'='NoImmediateClassViolation',
	      bestPractice=literal(type('xsd:boolean',true)),	      
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Graph, Reason) :-
    unionOf(X,Y,Graph),
    \+ immediateClass(Y,Graph), \+ restriction(Y,Graph),
    interpolate(['The class ',X,' is not a union of a defined class ',Y], Message),
    Reason = ['rdf:type'='NoImmediateClassViolation',
	      bestPractice=literal(type('xsd:boolean',true)),
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Graph, Reason) :-
    unionOf(X,Y,Graph),
    \+ immediateClass(X,Graph), \+ restriction(X,Graph),
    interpolate(['The class ',X,' is a union of ', Y,' but not a defined class'], Message),
    Reason = ['rdf:type'='NoImmediateClassViolation',
	      bestPractice=literal(type('xsd:boolean',true)),
	      message=Message,
	      child=X,
	      parent=Y].

%% restrctionOnProperty(?CR:uri_or_id, ?P:uri_or_id, +Graph:graph) is nondet
%
% Defines the relation between properties and their restrictions.
%
% @param CR A restriction class specified as a URI_OR_ID
% @param P A property specified as a URI_OR_ID
% @param Graph the current graph
%% :- rdf_meta restrictionOnProperty(r, r, o).
restrictionOnProperty(CR,P,Graph) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema),
	xrdf(Collection,Schema,CR,'http://www.w3.org/2002/07/owl#onProperty'),
	restriction(CR,Graph).
restrictionOnProperty(CR,P,Graph) :-
	strictSubsumptionPropertiesOf(P,Q,Graph),
	restrictionOnProperty(CR,Q,Graph).
	
%% classOrRestriction(?X:uri_or_id, +Graph:graph) is nondet
%
% All URI_OR_IDs which are either a class or restriction.
%
% @param X A class or restriction class specified as a URI_OR_ID
% @param Graph The current graph 
classOrRestriction(X,Graph) :- class(X,Graph).
classOrRestriction(X,Graph) :- restriction(X,Graph).

% TODO: compound is vague, reasons should have a specification.

%% notUniqueClass(+Y:uri_or_id, +Graph:graph, -Reason:any) is nondet
%
% Is a class multiply defined?
%
% @param Y A class specified as a URI_OR_ID
% @param Schema The current schema graph
notUniqueClass(Y, Graph, Reason) :-
    classOrRestriction(Y, Graph), setof(X, classOrRestriction(X,Graph), L),
    \+ count(Y,L,1),
    interpolate(['The class or restriction ',Y,
		 ' is not a unique. Some existing class has this identifier']
		,Message),
    Reason = ['rdf:type'='NotUniqueClassNameViolation',
	      bestPractice=literal(type('xsd:boolean',true)),
	      message=Message,
	      class=Y].
notUniqueClassSC(Graph,Reason) :- notUniqueClass(_,Graph,Reason).


%% collect(+X:uri_or_id, -L:any, +Graph_ID:graph_identifier) is semidet.
%
% Collect the RDF list into a prolog list.
% It may be better to treat lists programmatically through rdf rather than
% collect them, using a derived predicate like rdfListMembership
%
% @param X The URI_OR_ID of an RDF list
% @param L Term
% @param Graph The current graph
%% :- rdf_meta collect(r,t,o).
collect(X,[],_) :-
    rdf_global_id('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',X),
    !.
collect(X,[H|T],GraphAtom) :-
    xrdf(X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H,GraphAtom),
    !, % assume one result
    xrdf(X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',Y,GraphAtom),
    !, % assume one result
    collect(Y,T,GraphAtom).

%% subClassOf(?Child:uri_or_id,?Parent:uri_or_id,+Graph:graph) is nondet
%
% One step subclassing (only gives the immediate child-parent class relationship)
%
% @param Child Child class URI_OR_ID
% @param Parent Parent class URI_OR_ID.
subClassOf(Child,Parent,Graph) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,Child, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', Parent).

%% unionOf(?Super:uri_or_id,?Sub:uri_or_id,+Graph:graph) is nondet
%
% Gives URI_OR_ID solutions for which the Super URI_OR_ID is determined to be a union of.
%
% @param Super The class URI_OR_ID which is the union of other classes.
% @param Sub The class URI_OR_ID which is unioned to form the Super class.
% @param Graph The current graph
%% :- rdf_meta unionOf(r,r,o).
unionOf(C,U,Graph) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,C,'http://www.w3.org/2002/07/owl#unionOf',ListObj ),
    collect(ListObj,L,Schema),
    member(U,L).

%% unionOfList(+Super:uri_or_id,-Sub:list(uri_or_id),+Graph:graph) is det
%
% Gives a list of URI_OR_IDs which are solutions for which the Super URI_OR_ID is determined to be a union of.
%
% @param Super The class URI_OR_ID which is the union of other classes.
% @param Sub The class URI_OR_ID which is unioned to form the Super class.
% @param Graph The current graph
%% :- rdf_meta unionOfList(r,r,o).
unionOfList(C,UList,Graph) :-
    xrCollection,Graph,df(C,'http://www.w3.org/2002/07/owl#unionOf',ListOb),
    collect(ListObj,UList,Schema), !.

%% disjointUnionOf(?Super:uri_or_id,?Sub:uri_or_id,+Graph:graph) is nondet
%
% Gives URI_OR_ID solutions for which the Super URI_OR_ID is determined to be a union of.
%
% @param Super The class URI_OR_ID which is the disjoint union of other classes.
% @param Sub A class URI_OR_ID which is disjointly unioned to form the Super class.
% @param Graph The current graph
%% :- rdf_meta disjointUnionOf(r,r,o).
disjointUnionOf(C,U,Graph) :-
    graph_schema(Graph,Schema),
    xrdf(C,'http://www.w3.org/2002/07/owl#disjointUnionOf',ListObj, Schema),
    collect(ListObj,L,Schema),
    member(U,L).

%% disjointUnionOfList(+Super:uri_or_id,-SubList:list(uri_or_id),+Graph:graph) is det
%
% Gives URI_OR_ID solutions as a list for which the Super URI_OR_ID is determined to be a djsoint union of.
%
% @param Super The class URI_OR_ID which is the disjoint union of other classes.
% @param SubList The prolog list of class URI_OR_IDs which are disjointly unioned to form the Super class.
% @param Graph The current schema graph.
%% :- rdf_meta disjointUnionOfList(r,r,o).
disjointUnionOfList(C,UList,Graph) :-
    graph_schema(Graph,Schema),
    xrdf(C,'http://www.w3.org/2002/07/owl#disjointUnionOf',ListObj, Schema),
    collect(ListObj,UList,Schema), !.

%% intersectionOf(?Inter:uri_or_id,?Class:uri_or_id,+Graph:graph) is nondet
%
% Gives URI_OR_ID solutions for which the Super URI_OR_ID is determined to be an intersection.
%
% @param Inter The class URI_OR_ID which is the intersection of the Class URI_OR_ID.
% @param Class A class URI_OR_ID of which Inter is an intersection.
% @param Graph The current schema graph
%% :- rdf_meta intersectionOf(r,r,o).
intersectionOf(C,I,Graph) :-
    graph_schema(Graph,Schema),
    xrdf(C,'http://www.w3.org/2002/07/owl#intersectionOf',ListObj,Schema),
    collect(ListObj,L,Schema),
    member(I,L).

%% disjointUnionOfList(+Inter:uri_or_id,-Classes:list(uri_or_id),+Graph:graph) is det
%
% Gives URI_OR_ID solutions as a list for which the Super URI_OR_ID is determined to be an intersection.
%
% @param Inter The class URI_OR_ID which is the disjoint union of other classes.
% @param Classes The prolog list of class URI_OR_IDs which are intersected.
% @param Graph The current schema graph.
%% :- rdf_meta intersectionOfList(r,r,o).
intersectionOfList(C,IList,Graph) :-
    graph_schema(Graph,Schema),
 Collection, Schema,    xrdf(C,'http://www.w3.org/2002/07/owl#intersectionOf',ListObj),
    collect(ListObj,IList,Schema), !.

%% oneOf(?CC:uri_or_id,?X:uri_or_id,+Graph:graph) is det
%
% Gives elements which are members of a class by enumeration.
%% :- rdf_meta unionOfList(r,r,o).
unionOfList(C,UList,Graph) :-
% @param Collection,Schema,CC The class URI_OR_ID of which X isr.
% @param X The URI_OR_ID of the element which is a member of CC.
% @param Graph The current schema graph.
%% :- rdf_meta oneOf(r,r,o).
oneOf(CC,X,Graph) :-
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,CC,'http://www.w3.org/2002/07/owl#oneOf',ListObj,Schema),
    collect(ListObj,L,,
    member(X,L).

%% oneOfList(+CC:uri_or_id,-OneList:list(uri_or_id),+Graph:graph) is det
%
% Gives a prolog list of elements associated with an enumerated class.
%
% @param CC The class URI_OR_ID of which X is a member.
% @param OneList The URI_OR_ID list of elements which are members of CC.
% @param Graph The current schema graph.
%% :- rdf_meta oneOfList(r,r,o).
oneOfList(C,OneList,Graph) :-
    graph_collection(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,C,'http://www.w3.org/2002/07/owl#oneOf',ListObj),
    collect(ListObj,OneList,Schema).

%% :- rdf_meta complementOf(r,r,o).
complementOf(CC,CN,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,CC,'http://www.w3.org/2002/07/owl#complementOf',CN).

%% :- rdf_meta datatypeComplementOf(r,r,o).
datatypeComplementOf(CC,CN,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,CC,'http://www.w3.org/2002/07/owl#datatypeComplementOf',CN).

%% :- rdf_meta equivalentClass(r,r,o).
equivalentClass(CC,CE,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,CC,'http://www.w3.org/2002/07/owl#equivalentClass',CE).

/*  Previous code for equivalent class.
    xrdf(CC,'http://www.w3.org/2002/07/owl#equivalentClass',ListObj,Schema),
    collect(ListObj,L,Schema),
    member(CE,L). */

%% :- rdf_meta equivalentClass(r,r,o).
anonymousEquivalentClass(C,CE,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    equivalentClass(C,CE,Schema),
    % Exactly one reference to this class, or everything will go to hell.
    (setof(X,xrdf(Collection,Schema,X,_,CE), ListX) *-> ListX = L ; L = []),
    length(L,1).

% transitive strict relation
subClassStrict(X,Y,Graph) :- subClassOf(X,Y,Graph). 
subClassStrict(X,Z,Graph) :- subClassOf(X,Y,Graph), subClassStrict(Y,Z, Graph).

% Implements class subsumption
% - complementOf classes do not give subsumption properly yet (unimplemented).
%   Requires anti-subsumption predicate
% - oneOf should probably have individual sets for both CC, CP
%:- table subsumptionOf/3.
%:- rdf_meta subsumptionOf(r,r,o).
subsumptionOf(CC,CC,Graph) :-
    immediateClass(CC,Graph).
subsumptionOf(CC,CP,Graph) :-
    subClassOf(CC,CZ,Graph),
    subsumptionOf(CZ,CP,Graph).
subsumptionOf(CC,CP,Graph) :-
    immediateClass(CC,Graph),
    unionOf(CZ,CC,Graph),
    subsumptionOf(CZ,CP,Graph).
subsumptionOf(CC,CP,Graph) :-
    immediateClass(CC,Graph),	
    disjointUnionOf(CZ,CC,Graph),
    subsumptionOf(CZ,CP,Graph).
subsumptionOf(CC,CP,Graph) :-
    immediateClass(CC,Graph),	
    intersectionOf(CC,CZ,Graph), 
    subsumptionOf(CZ,CP,Graph).
subsumptionOf(CC,CP,Graph) :-
    anonymousEquivalentClass(CC,CZ,Graph),
    subsumptionOf(CZ,CP,Graph).
subsumptionOf(CC,CP,Graph) :- % datatypes
    datatype(CC,Graph),
    datatypeSubsumptionOf(CC,CP,Graph).
subsumptionOf(_,'http://www.w3.org/2002/07/owl#Thing',_). % Is this worth throwing in? Might conflict with other constraints
subsumptionOf('http://www.w3.org/2002/07/owl#Nothing',_,_).

%% :- rdf_meta customDatatype(r,o).
customDatatype(X,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2000/01/rdf-schema#Datatype').

/** 
 * datatype(-X:uri_or_id,+Graph:graph) is nondet.
 * datatype(-X:uri_or_id,+Graph:graph) is det.
 */
%% :- rdf_meta datatype(r,o).
datatype(X,Graph) :- customDatatype(X,Graph).
datatype(X,_) :- baseType(X).

% implements strict class subsumption (CC < CP) [Needs fully instantiated arguments]
%% :- rdf_meta strictSubsumptionOf(r,r,o).
strictSubsumptionOf(CC,'http://www.w3.org/2002/07/owl#Thing',_) :- CC \= 'http://www.w3.org/2002/07/owl#Thing'.
strictSubsumptionOf('http://www.w3.org/2002/07/owl#Nothing',CP,_) :- CP \= 'http://www.w3.org/2002/07/owl#Nothing'.
strictSubsumptionOf(CC,CP,Graph) :-
    subClassOf(CC,CP,Graph).
strictSubsumptionOf(CC,CP,Graph) :-
    class(CC,Graph),
    unionOf(CP,CC,Graph).
strictSubsumptionOf(CC,CP,Graph) :-
    class(CC,Graph),
    intersectionOf(CC,CP,Graph).
strictSubsumptionOf(CC,CP,Graph) :-
    subClassOf(CC,CZ,Graph),
    strictSubsumptionOf(CZ,CP,Graph).
strictSubsumptionOf(CC,CP,Graph) :-
    class(CC,Graph),
    unionOf(CZ,CC,Graph),
    strictSubsumptionOf(CZ,CP,Graph).
strictSubsumptionOf(CC,CP,Graph) :-
    class(CC,Graph),
    disjointUnionOf(CZ,CC,Graph),
    strictSubsumptionOf(CZ,CP,Graph).
strictSubsumptionOf(CC,CP,Graph) :-
    class(CC,Graph),
    intersectionOf(CC,CZ,Graph), 
    strictSubsumptionOf(CZ,CP,Graph).
strictSubsumptionOf(CC,CP,Graph) :- % xsd and custom data types
    datatypeStrictSubsumptionOf(CC,CP,Graph).




/** 
 * basetypeSubsumptionOf(?Sub,?Super) is nondet. 
 * 
 * Implements the subsumption latice for concrete base types by making use of reflexivity and 
 * baseTypeParent\2.
 */
%% :- rdf_meta basetypeSubsumptionOf(r,r).
basetypeSubsumptionOf(T,T) :- baseType(T).
basetypeSubsumptionOf(Sub,Super) :-
    baseTypeParent(Sub,Parent), basetypeSubsumptionOf(Parent,Super).

/** 
 * datatypeSubsumptionOf(?Sub,?Super,+Graph:graph) is nondet. 
 * 
 * Implements the subsumption latice for datatypes by making use of reflexivity and 
 * baseTypeParent\2.
 */
%% :- rdf_meta datatypeSubsumptionOf(r,r).
datatypeSubsumptionOf(T,T,Graph) :- datatype(T,Graph).
datatypeSubsumptionOf(Sub,Super,Graph) :-
    customDatatype(Sub,Graph),
    unionOf(Super,Sub,Graph).
datatypeSubsumptionOf(Sub,Super,Graph) :-
    customDatatype(Sub,Graph),
    intersectionOf(Sub,Super,Graph).
datatypeSubsumptionOf(Sub,Super,Graph) :-
    % This only works because of the strict hierarchy (no derived union / intersections)
    customDatatype(Sub,Graph),
    datatypeComplementOf(Sub,CN,Graph),
    \+ datatypeSubsumptionOf(CN,Super,Graph).
datatypeSubsumptionOf(Sub,Super,Graph) :-
    baseTypeParent(Sub,Parent), datatypeSubsumptionOf(Parent,Super,Graph).

/**
 * datatypeSubsumptionOf(?Sub,?Super,+Graph:graph) is nondet. 
 * 
 * Implements strict (non reflexive) subsumption latice for datatypes by 
 * baseTypeParent\2.
 */
%% :- rdf_meta datatypeStrictSubsumptionOf(r,r).
datatypeStrictSubsumptionOf(Sub,Super,_) :-
    baseTypeParent(Sub,Super).
datatypeStrictSubsumptionOf(Sub,Super,Graph) :-
    % DDD probably need one more clause for each owl custom build property
    baseTypeParent(Sub,Parent),
    datatypeSubsumptionOf(Parent,Super,Graph).

/**
 * orphanClassSC(+Graph:graph,-Reason:rvo) is nondet. 
 * 
 * Find all orphaned classes in Schema.
 */
orphanClassSC(Graph, Reason) :-
    subClassOf(X,Y,Graph),
    \+ class(Y,Graph), \+ restriction(Y,Graph),
    interpolate(['The class ',X,' is not a subclass of a valid class ',Y], Message),
    Reason = ['rdf:type'='NotSubClassofClassViolation',
	      bestPractice=literal(type('xsd:boolean',false)),	      
	      message=Message,
	      child=X,
	      parent=Y].
orphanClassSC(Graph, Reason) :-
    intersectionOf(X,Y,Graph),
    \+ class(Y,Graph), \+ restriction(Y,Graph),
    interpolate(['The class ',X, ' is not an intersection of a valid class ',Y], Message),
    Reason = ['rdf:type'='NotIntersectionOfClassViolation',
	      bestPractice=literal(type('xsd:boolean',false)),	      	      
	      message=Message,
	      child=X,
	      parent=Y].
orphanClassSC(Graph, Reason) :-
    unionOf(X,Y,Graph),
    \+ class(Y,Graph), \+ restriction(Y,Graph),
    interpolate(['The class ',X,' is not a union of a valid class ',Y], Message),
    Reason = ['rdf:type'='NotUnionOfClassViolation',
	      bestPractice=literal(type('xsd:boolean',false)),
	      message=Message,
	      child=X,
	      parent=Y].


% Cycles in subsumption diagram
classCycleHelp(C,S,[],_) :- get_assoc(C,S,true), !.
classCycleHelp(C,S,[K|P],Graph) :-
    subClassOf(K,C,Graph), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Graph).
classCycleHelp(C,S,[K|P],Graph) :-
    unionOf(C,K,Graph), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Graph).
classCycleHelp(C,S,[K|P],Graph) :-
    disjointUnionOf(C,K,Graph), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Graph).
classCycleHelp(C,S,[K|P],Graph) :-
    intersectionOf(K,C,Graph), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Graph).
classCycleHelp(C,S,[K|P],Graph) :-
    equivalentClass(K,C,Graph),
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Graph).
classCycleHelp(C,S,[K|P],Graph) :-
    complementOf(K,C,Graph), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Graph).
classCycleHelp(C,S,[K|P],Graph) :-
    datatypeComplementOf(K,C,Graph), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Graph).

classCycle(C,P,Graph,Reason) :-
    empty_assoc(S), classCycleHelp(C,S,P,Graph),
    interpolate(['Class, ',C,' has a class cycle with path: ', P], Message),
    Reason = ['rdf:type'='ClassCycleViolation',
	      bestPractice=literal(type('xsd:boolean',false)),
	      message=Message,
	      class=C,
	      path=P].

classCycleSC(Graph,Reason) :- classCycle(_,_,Graph,Reason), !. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Properties

/* 
:- rdf_meta rdfsProperty(r).
rdfsProperty(rdfs:label).
rdfsProperty(rdfs:comment).
rdfsProperty(rdfs:seeAlso).
*/

%:- rdf_meta rdfMetaProperty(r).
rdfMetaProperty('http://www.w3.org/1999/02/22-rdf-syntax-ns#type').

%:- rdf_meta rdfsDatatypeProperty(r).
rdfsDatatypeProperty('http://www.w3.org/2000/01/rdf-schema#label').
rdfsDatatypeProperty('http://www.w3.org/2000/01/rdf-schema#comment').

%:- rdf_meta rdfsObjectProperty(r).
rdfsObjectProperty('http://www.w3.org/2000/01/rdf-schema#seeAlso').

%:- rdf_meta rdfsProperty(r).
rdfsProperty(P) :- rdfsDatatypeProperty(P).
rdfsProperty(P) :- rdfsObjectProperty(P).

%:- rdf_meta rdfProperty(r,o).
rdfProperty(P,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').

%:- rdf_meta datatypeProperty(r,o).
datatypeProperty(P,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#DatatypeProperty').
datatypeProperty(P,_) :- rdfsDatatypeProperty(P).

%:- rdf_meta annotationProperty(r,o).
annotationProperty(P,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#AnnotationProperty').
annotationProperty(P,Graph) :-
    subsumptionPropertiesOf(P,'https://datachemist.net/ontology/dcog#pseudo_property', Graph).


%:- rdf_meta functionalProperty(r,o).
functionalProperty(P,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#FunctionalProperty').

%:- rdf_meta inverseFunctionalProperty(r,o).
inverseFunctionalProperty(P,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#InverseFunctionalProperty').

%:- rdf_meta objectProperty(r,o).
objectProperty(P,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#ObjectProperty').
objectProperty(P,_) :- rdfsObjectProperty(P).

/** 
 * property(?P,+Graph:graph) is nondet. 
 * 
 * ?P is a valid property in the schema. 
 */
%:- rdf_meta property(r,o).
property(P,Graph) :-
    % Don't predicate over annotations (even if they are otherwise declared as properties).
    (   annotationProperty(P,Graph)
    *-> fail
    ;   (   datatypeProperty(P, Graph)
        ;   objectProperty(P,Graph)
        ;   rdfProperty(P,Graph)
        )
    ).

%uniqueProperty(P,Graph) :- property(P,Graph), bagof(P2, property(P2,Graph), L), count(P,L,1).

notUniqueProperty(P,Graph,Reason) :-
    property(P,Graph), bagof(P2, property(P2,Graph), L),
    \+ count(P,L,1),
    interpolate([P,' is not a unique property name, some property with this name already exists'],
		Message),
    Reason=['rdf:type'='NotUniquePropertyNameViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    property=P,
	    message=Message].

propertyTypeOverloadSC(Graph,Reason) :- 
    datatypeProperty(P,Graph), objectProperty(P,Graph),
    interpolate([P,' is an objectProperty and a datatypeProperty'], Message),
    Reason=['rdf:type'='PropertyTypeOverloadViolation',
	    bestPractice=literal(type('xsd:boolean',false)),
	    property=P,
	    message=Message].

annotationOverloadSC(Graph,Reason) :-
    (datatypeProperty(P,Graph) ; objectProperty(P,Graph) ; rdfProperty(P,Graph)),
    annotationProperty(P,Graph),
    interpolate([P,' is defined as a property and defined as an annotationProperty'], Message),
    Reason=['rdf:type'='annotationOverloadViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    property=P,
	    message=Message].

/** 
 * notUniquePropertySC(+Graph:graph,-Reason:any) is nondet. 
 * 
 * All redundantly defined properties.
 */
notUniquePropertySC(Graph,Reason) :-
    notUniqueProperty(_,Graph, Reason).

% One step subproperty relation
%:- rdf_meta subPropertyOf(r,r,o).
subPropertyOf(X,Y,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,X,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',Y).

/** 
 * subsumptionPropertiesOf(?PChild,?PParent,+Graph:graph) is nondet. 
 * 
 * Transitive reflexive closure of Subproperty relation.
 */
%:- rdf_meta subsumptionPropertiesOf(r,r,o).
subsumptionPropertiesOf(PC,PC,_).
subsumptionPropertiesOf(PC,PP,Graph) :-
    subPropertyOf(PC, PZ, Graph),
    subsumptionPropertiesOf(PZ,PP,Graph).
subsumptionPropertiesOf(PC,'http://www.w3.org/2002/07/owl#topObjectProperty',Graph) :-
    objectProperty(PC,Graph).
subsumptionPropertiesOf(PC,'http://www.w3.org/2002/07/owl#topDataProperty',Graph) :-
    datatypeProperty(PC,Graph).

/** 
 * strictSubsumptionPropertiesOf(?PChild,?PParent,-Graph:graph) is nondet. 
 * 
 * Non-reflexive subsumption relation for properties in Graph. 
 */
%:- rdf_meta strictSubsumptionPropertiesOf(r,r,o).
strictSubsumptionPropertiesOf(PC,PP,Graph) :-
    subPropertyOf(PC, PP, Graph).
strictSubsumptionPropertiesOf(PC,PP,Graph) :-
    subPropertyOf(PC, PZ, Graph),
    subsumptionPropertiesOf(PZ,PP,Graph).
strictSubsumptionPropertiesOf(PC,'http://www.w3.org/2002/07/owl#topObjectProperty',Graph) :-
    PC \= 'http://www.w3.org/2002/07/owl#topObjectProperty',
    objectProperty(PC,Graph).
strictSubsumptionPropertiesOf(PC,'http://www.w3.org/2002/07/owl#topDataProperty',Graph) :-
    PC \= 'http://www.w3.org/2002/07/owl#topDataProperty',
    datatypeProperty(PC,Graph).

orphanProperty(X,Y,Graph,Reason) :-     % is a subproperty of Y but Y is not a valid property 
    subPropertyOf(X,Y,Graph),
    \+ property(Y,Graph), \+ annotationProperty(Y,Graph),
     interpolate([X,' is a sub-property of', Y,' which is not designated as a property of any type in the schema!' ], Message),
    Reason=['rdf:type'='OrphanPropertyViolation',
	    bestPractice=literal(type('xsd:boolean',false)),
	    child=X,
	    parent=Y,
	    message=Message].
orphanProperty(P,R,Graph,Reason) :-  % property has a range but it is not the property of a Classs
	range(P,R,Graph),
    \+ property(P,Graph), \+ annotationProperty(P,Graph),
	interpolate([P,' has a range, but is not designated as a property of any type in the schema!'],Message),
	Reason=['rdf:type'='OrphanPropertyViolation',
			bestPractice=literal(type('xsd:boolean',false)),
			range=R,
			property=P,
			message=Message].
orphanProperty(P,D,Graph,Reason) :-     %% property has a domain but it is not the property of a Classs
	domain(P,D,Graph),
    \+ property(P,Graph), \+ annotationProperty(P,Graph),
    interpolate([P,' has a domain, but is not designated as a property of any type in the schema!'],Message),
	Reason=['rdf:type'='OrphanPropertyViolation',
			bestPractice=literal(type('xsd:boolean',false)),
			domain=D,
			property=P,
			message=Message].

/** 
 * orphanPropertySC(+Graph:graph,-Reason:any) is nondet. 
 *  
 * All properties which are referred to, but which do not have definitions. 
 */ 
orphanPropertySC(Graph,Reason) :-
    orphanProperty(_,_,Graph,Reason).

% subProperty cycles 

propertyCycleHelp(P,S,[],_) :- get_assoc(P,S,true), !.
propertyCycleHelp(P,S,[Q|T],Graph) :-
    property(P,Graph), subPropertyOf(Q,P,Graph), put_assoc(P, S, true, S2),
    propertyCycleHelp(Q,S2,T,Graph).

propertyCycle(P,PC,Graph,Reason) :-
    empty_assoc(S), propertyCycleHelp(P,S,PC,Graph),
    interpolate(['Property class ', P, ' has a cycle with path: ', PC], Message),
    Reason=['rdf:type'='PropertyClassCycleViolation',
	    bestPractice=literal(type('xsd:boolean',false)),
	    property=P,
	    path=PC,
	    message=Message].

/** 
 * propertyCycleSC(+Graph:graph,-Reason:any) is nondet. 
 *  
 * All property subsumption cycles in Graph.
 */ 
propertyCycleSC(Graph,Reason) :- propertyCycle(_,_,Graph,Reason).

/** 
 * range(?P:uri, ?R:uri, +Graph:graph) is nondet. 
 *
 * Actually specified range for P.
 */
range(P,R,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/2000/01/rdf-schema#range',R).

/** 
 * entity(?Class,+Graph) is nondet.
 * 
 * This is a foundational predicate for DQS which establishes 
 * the places in which to "clip" the graph. 
 */
entity(Class,Graph) :-
	subsumptionOf(Class,'https://datachemist.net/ontology/dcog#Entity', Graph). 

/** 
 * anyRange(?P,?R,+Graph:graph) is nondet. 
 * 
 * Determine if R is a viable range for P. 
 * This must be ordered according to Hasse diagram!
 */ 
%:- rdf_meta anyRange(r,r,o).
anyRange('http://www.w3.org/2002/07/owl#topObjectProperty','http://www.w3.org/2002/07/owl#Thing',_).
anyRange('http://www.w3.org/2002/07/owl#topDataProperty','http://www.w3.org/2000/01/rdf-schema#Literal',_).
anyRange('http://www.w3.org/2000/01/rdf-schema#label','http://www.w3.org/2001/XMLSchema#string',_).
anyRange('http://www.w3.org/2000/01/rdf-schema#comment','http://www.w3.org/2001/XMLSchema#string',_).
anyRange(P,R,Graph) :-
    range(P,R,Graph).
anyRange(P,R,Graph) :-
    strictSubsumptionPropertiesOf(P,P2,Graph),
    anyRange(P2,R,Graph).
anyRange(OP,R,Graph) :-
    graph_inference(Graph,Inference),
    xrdf(OP,'http://www.w3.org/2002/07/owl#inverseOf',P, Inference),
    anyDomain(P,R,Graph).

%:- rdf_meta mostSpecificRange(r,r,o).
mostSpecificRange(P,R,Graph) :- anyRange(P,R,Graph), !.

/** 
 * domain(P:uri,D:uri,Graph:graph) is nondet.
 *
 * Actually specified domain in the database.
 */ 
domain(P,D,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,P,'http://www.w3.org/2000/01/rdf-schema#domain',D).

/** 
 * anyDomain(?P,?R,+Graph:graph) is nondet. 
 * 
 * Determine if R is a viable domain for P. 
 */ 
%:- rdf_meta anyDomain(r,r,o).
anyDomain('http://www.w3.org/2002/07/owl#topObjectProperty',
          'http://www.w3.org/2002/07/owl#Thing',_).
anyDomain('http://www.w3.org/2002/07/owl#topDataProperty',
          'http://www.w3.org/2000/01/rdf-schema#Literal',_).
anyDomain('http://www.w3.org/2000/01/rdf-schema#label',D,Graph) :-
    entity(D,Graph).
anyDomain('http://www.w3.org/2000/01/rdf-schema#comment',D,Graph) :-
    entity(D,Graph).
anyDomain(P,R,Graph) :-
    domain(P,R,Graph).
anyDomain(OP,R,Graph) :-
    graph_inference(Graph,Inference),
    xrdf(OP,'http://www.w3.org/2002/07/owl#inverseOf',P, Inference),
    anyRange(P,R,Graph).
anyDomain(P,R,Graph) :-
    strictSubsumptionPropertiesOf(P,P2,Graph),
    anyDomain(P2,R,Graph).

% TODO inverse isn't enough! need to do more inference

%:- rdf_meta mostSpecificDomain(r,r,o).
mostSpecificDomain(P,R,Graph) :- anyDomain(P,R,Graph), !.

/** 
 * noImmediateDomainSC(+Graph:graph,-Reason:rvo) is nondet. 
 *
 * All properties which do not have a specified domain.
 */
noImmediateDomainSC(Graph,Reason) :-
    property(P,Graph),
    \+ rdfsProperty(P),
    \+ domain(P,_,Graph),
    (   datatypeProperty(P,Graph)
    ->  M='Data property '
    ;   annotationProperty(P,Graph)
    ->  M='Annotation property '
    ;   objectProperty(P,Graph)
    ->  M='Object property '
    ;   rdfProperty(P,Graph)
    ->  M='Rdf Property '
    ;   M='Unknown Property '),
    interpolate([M, P, ' has no specified domain.'], Message),
    Reason = ['rdf:type'=noImmediateDomain,
	      property=P,
	      message = Message].

/** 
 * noImmediateRangeSC(+Graph:graph,-Reason:rvo) is nondet. 
 *
 * All properties which do not have a specified range.
 */
noImmediateRangeSC(Graph,Reason) :-
    property(P,Graph),
    \+ rdfsProperty(P),
    \+ range(P,_,Graph),
    (   datatypeProperty(P,Graph)
    ->  M='Data property '
    ;   annotationProperty(P,Graph)
    ->  M='Annotation property '
    ;   objectProperty(P,Graph)
    ->  M='Object property '
    ;   rdfProperty(P,Graph)
    ->  M='Rdf Property '
    ;   M='Unknown Property '),
    interpolate([M, P, ' has no specified range.'], Message),
    Reason = ['rdf:type'=noImmediateRange,
	      property=P,
	      message = Message].

/** 
 * invalidDomainSC(+Graph:graph,-Reason:any) is nondet. 
 *
 * Finds all domains that are not valid classes for a given property in Graph. 
 */
invalidDomainSC(Graph,Reason) :-
    property(P,Graph),
    domain(P,D,Graph),
    \+ class(D,Graph),
    interpolate(['The property ', P,' has an undefined domain.'],Message),
    Reason=['rdf:type'='InvalidDomainViolation',
	    bestPractice=literal(type('xsd:boolean',false)),
	    message=Message,
	    property=P,
	    domain=D].

/** 
 * invalidRangeSC(+Graph:graph,-Reason:any) is nondet. 
 *
 * Finds all ranges that are not valid classes for a given property in Graph. 
 */
invalidRangeSC(Graph,Reason) :-
    datatypeProperty(P,Graph),
    range(P,R,Graph),
    \+ datatype(R,Graph), \+ rdfsProperty(P),
    interpolate(['DataProperty Range ', R, ' is not a valid (or implemented) datatype for property ', P,'.'], Message),
    Reason=['rdf:type'='InvalidRangeViolation',
	    bestPractice=literal(type('xsd:boolean', false)),
	    message=Message,
	    property=P,
	    range=R].
invalidRangeSC(Graph,Reason) :-
    objectProperty(P,Graph),
    range(P,R,Graph),
    \+ class(R,Graph), \+ rdfsProperty(P),
    interpolate(['ObjectProperty Range ',R,' is not a valid range for property ',P,'.'],Message),
    Reason=['rdf:type'='InvalidRangeViolation',
	    bestPractice=literal(type('xsd:boolean', false)),
	    message=Message,
	    property=P,
	    range=R].
invalidRangeSC(Graph,Reason) :-
    rdfProperty(P,Graph),
    range(P,R,Graph),
    \+ class(R,Graph), \+ baseType(R),
    interpolate(['rdf:Property range ',R,' is not a valid range for property ',P,'.'],Message),
    Reason=['rdf:type'='InvalidRangeViolation',
	    bestPractice=literal(type('xsd:boolean', false)),
	    message=Message,
	    property=P,
	    range=R].

% Logging / Turn off for production
:- use_module(library(http/http_log)).
/** 
 * domainNotSubsumedsC(+Graph:graph,-Reason:any) is nondet. 
 *
 * All domains which can not validly be subsumed within a property subsumption hierarchy for 
 * the Graph .
 */
domainNotSubsumedSC(Graph,Reason) :-
    property(P,Graph),
    strictSubsumptionPropertiesOf(P,P2,Graph),
    domain(P,D,Graph), domain(P2,D2,Graph), % DDD too many solutions
    %http_log_stream(Log),
    %current_output(Log),
    %findall(X, subsumptionOf(D,X,Graph), L),
    %nl(Log), nl(Log), write(Log, 'Subsumptions: '), write_canonical(Log, L), nl(Log), nl(Log),
    %nl(Log), nl(Log), write(Log, 'Listing: '), write_canonical(Log, L), nl(Log), nl(Log),    
    \+ subsumptionOf(D, D2, Graph),
    interpolate(['Invalid domain on property ', P,
		 ', due to failure of domain subsumption.'], Message),
    Reason = ['rdf:type'='DomainNotSubsumedViolation',
	      bestPractice=literal(type('xsd:boolean', false)),
	      message=Message,
	      property=P,
	      parentProperty=P2,
	      domain=D,
	      parentDomain=D2].

/** 
 * rangeNotSubsumedSC(+Graph:graph,-Reason:any) is nondet. 
 * 
 * Graph constraint determining that ranges must be valid 
 * a property through its entire subsumption chain. 
 */ 
rangeNotSubsumedSC(Graph,Reason) :-
    property(P,Graph),
    strictSubsumptionPropertiesOf(P,P2,Graph),
    range(P,R,Graph), range(P2,R2,Graph), % DDD too many solutions
    \+ subsumptionOf(R, R2, Graph), 
    interpolate(['Invalid range on property ', P,
		 ', due to failure of range subsumption.'], Message),
    Reason = ['rdf:type'='RangeNotSubsumedViolation',
	      bestPractice=literal(type('xsd:boolean', false)),
	      message=Message,
	      property=P,
	      parentProperty=P2,
	      range=R,
	      parentRange=R2].

schemaSubjectBlankNode(X,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,X,_,_),
    rdf_is_bnode(X).
schemaPredicateBlankNode(Y,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,_,Y,_),
    rdf_is_bnode(Y).
schemaObjectBlankNode(Z,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,_,_,Z),
    rdf_is_bnode(Z).

/** 
 * schemaBlankNodeSC(+Graph:graph,-Reason:any) is nondet. 
 * 
 * Is this a blank node in the schema.
 */ 
schemaBlankNodeSC(Graph,Reason) :-
    schemaSubjectBlankNode(X,Graph),
    interpolate(['The subject ', X, ' is a blank node'],Message),
    Reason=['rdf:type'='GraphBlankNodeViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    message=Message,
	    subject=X].
schemaBlankNodeSC(Graph,Reason) :-
    schemaPredicateBlankNode(X,Graph),
    interpolate(['The predicate ', X, ' is a blank node'],Message),
    Reason=['rdf:type'='GraphBlankNodeViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    message=Message,
	    predicate=X].
schemaBlankNodeSC(Graph,Reason) :-
    schemaObjectBlankNode(X,Graph),
    interpolate(['The object ', X, ' is a blank node'],Message),
    Reason=['rdf:type'='GraphBlankNodeViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    message=Message,
	    object=X].

/** 
 * label(?X,?Y,+Graph:graph) is det. 
 * 
 * Get the rdfs:label for X as Y.
 */
label(X,Y,Graph) :-
    graph_instance(Graph,Instance),
    xrdf(X, 'http://www.w3.org/2000/01/rdf-schema#label',Y,Instance).
label(X,Y,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,X, 'http://www.w3.org/2000/01/rdf-schema#label',Y).

/** 
 * comment(?X,?Y,+Graph:graph) is det. 
 * 
 * Get the rdfs:comment for X as Y.
 */
comment(X,Y,Graph) :-
    graph_instance(Graph,Instance),
    xrdf(X, 'http://www.w3.org/2000/01/rdf-schema#comment', Y, Instance).
comment(X,Y,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,X, 'http://www.w3.org/2000/01/rdf-schema#comment', Y).

/** 
 * dcog_tag(?X:uri_or_id,?Y:any,+Graph:graph) is det. 
 * 
 * TODO: Rename!
 * Get the dcog:tag for X as Y.
 */
dcog_tag(X,Y,Graph) :-
    graph_colletcion(Graph,Collection),
    graph_schema(Graph,Schema),
    xrdf(Collection,Schema,X, 'https://datachemist.net/ontology/dcog#tag', Y).

classHasLabel(X,Y,Graph) :- class(X,Graph), label(X,Y,Graph).
%classHasNoLabel(X,Graph) :- class(X,Graph), \+ label(X,_,Graph).

classHasOneLabel(X,Graph) :-
    classHasLabel(X,Label,Graph),
    bagof(label(Y,Label2), classHasLabel(Y,Label2,Graph), L), count(label(X,Label),L,1).

notUniqueClassLabel(X,Graph,Reason) :- 
    \+ classHasOneLabel(X,Graph),
    interpolate(['Class ', X,' does not have exactly one lable.'], Message),
    Reason = ['rdf:type'='NotUniqueClassLabelViolation',
	      bestPractice=literal(type('xsd:boolean',true)),	    
	      class=X,
	      message=Message].

/**
 * notUniqueClassLabelSC(+Graph:graph, -Reason:rvo) is nondet. 
 * 
 * What it says on the tin: every non unique label in Graph as a schema constraint. 
 */ 
notUniqueClassLabelSC(Graph,Reason) :- notUniqueClassLabel(_,Graph,Reason).
