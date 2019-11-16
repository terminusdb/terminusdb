:- module(validate_schema,[
              % OWL semantics
	          class/2,
              immediateClass/2,
              restriction/2,
              classOrRestriction/2,
		      subClassOf/3,
              unionOf/3,
              intersectionOf/3,
              subClassStrict/3,
		      disjointUnionOf/3,
              label/3,
              comment/3,
		      subsumptionOf/3,
              strictSubsumptionOf/3,
              complementOf/3,
		      tcs_tag/3,
              document/2,

		      unionOfList/3,
              intersectionOfList/3,
              disjointUnionOfList/3,
		      oneOfList/3,

		      datatypeProperty/2,
              objectProperty/2,
              annotationProperty/2,
		      property/2,
              subPropertyOf/3,
              subsumptionPropertiesOf/3,
		      range/3,
              domain/3,
              anyRange/3,
              anyDomain/3,
		      mostSpecificDomain/3,
              mostSpecificRange/3,
		      collect/4,
              functionalProperty/2,
		      inverseFunctionalProperty/2,
              restrictionOnProperty/3,
		      datatypeSubsumptionOf/3,
              basetypeSubsumptionOf/2,
		      customDatatype/2,
              datatype/2,
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
		      noImmediateDomainSC/2,
              noImmediateRangeSC/2,         % Best Practice
		      % schemaBlankNodeSC/2,
              notUniqueClassLabelSC/2,      % Best Practice
		      notUniqueClassSC/2,
              notUniquePropertySC/2,        % Best Practice
		      noImmediateClassSC/2,
		      annotationOverloadSC/2,
		      propertyTypeOverloadSC/2,

		      % OWL DL (Constraint)
		      orphanClassSC/2,              % OWL
		      orphanPropertySC/2,           % OWL
		      invalidDomainSC/2,
              invalidRangeSC/2,             % OWL
		      domainNotSubsumedSC/2,
              rangeNotSubsumedSC/2,         % OWL
              invalid_RDFS_property_SC/2    % OWL
	      ]).

/** <module> Schema Validation
 *
 * This module deals with schema validation predicates as well as queries
 * for subsumption, domains, ranges, classes etc.
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

:- use_module(database).
:- use_module(triplestore).
:- use_module(utils).
:- use_module(types).
:- use_module(base_type).
:- use_module(validate_instance).

/*
 * Vio JSON util
 */
pathify(L, JSON) :-
    pathify(L, 0, JSON).

pathify([],_N,[]).
pathify([URI|Rest],N,[JSON|JSON_Rest]) :-
    JSON = _{ 'vio:index' : _{ '@value' : N, '@type' : 'xsd:nonNegativeInteger' },
              'vio:path_component' : _{ '@value' : URI, '@type' : 'xsd:string' }
            },
    M is N + 1,
    pathify(Rest,M, JSON_Rest).

/*
OWL DL Syntactic correctness

It would be useful to do a complete check on the syntactic
correctness of our ontology according to the OWL 2 / RDF mapping
*/

/*
Classes
*/

/**
 * immediateClass(?X:uri_or_id, +Database:database) is nondet.
 * immediateClass(+X:uri_or_id, +Database:database) is det.
 *
 * Check to see if class definitions are immediate (best practices) rather than inferred.
 *
 * @param X URI_OR_ID identifier for which to check if the schema has recorded a
 *        a non-inferred rfds or owl Class.
 * @param Schema Atom idntifying the current schema graph.
*/
% :- rdf_meta immediateClass(r,o).
immediateClass(X,Database) :-
    database_schema(Database,Schema),
    xrdf(Database, Schema, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2000/01/rdf-schema#Class').
immediateClass(X,Database) :-
    database_schema(Database,Schema),
    xrdf(Database, Schema, X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Class').
immediateClass('http://www.w3.org/2002/07/owl#Thing',_).
immediateClass('http://www.w3.org/2002/07/owl#Nothing',_).
% This makes me nevious... [ Gavin ]
immediateClass('http://www.w3.org/2002/07/owl#Ontology', _).
% Should this be here?
immediateClass('http://terminusdb.com/schema/tcs#Entity', _).
immediateClass('http://terminusdb.com/schema/tcs#Document', _).
immediateClass('http://terminusdb.com/schema/tcs#Relationship', _).


%% class(?X:uri_or_id, +Schema:database is nondet
% class(+X:uri_or_id, +Schema:database is det
%
% All class designations - with inferences.
%
% @param X URI_OR_ID identifier for which to check if the schema has recorded a
%        an inferred rfds or owl Class.
% @param Database object with the current schema graph.
%% :- rdf_meta class(r,o).
class(X,Database) :- immediateClass(X,Database).
class(X,Database) :- subClassOf(X,Y,Database), class(Y,Database).
class(X,Database) :- equivalentClass(X,Y,Database), class(Y,Database).

% restriction(+R:uri_or_id, +Database:database is nondet.
%
% All restriction designations - with inferences.
%
% @param R URI_OR_ID identifier for which to check if the schema has recorded a
%        an inferred owl Restriction.
% @param Database identifying the current schema graph.
%% :- rdf_meta restriction(r,o).
restriction(R,Database) :-
    database_schema(Database,Schema),
    xrdf(Database, Schema, R, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Restriction').
restriction(R,Database) :-
    subClassOf(R,R2,Database),
    restriction(R2,Database).
restriction(R,Database) :-
    equivalentClass(R,R2,Database),
    restriction(R2,Database).

%% noImmediateClassSC(+Database:database -Reason:any) is nondet
%
% Check to see if a class is used without a class definition.
%
% @param Schema Database idntifying the current schema graph.
% @param Reason A prolog representation of a JSON Linked Data structure
%               detailing the violation.
noImmediateClassSC(Database, Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#ObjectProperty'),
    domain(P,X,Database),
    \+ immediateClass(X,Database), \+ restriction(X,Database),
    interpolate([X,' is used as a domain for property ',P,' but is not defined'], Message),
    Reason = _{
                 '@type' : 'vio:InvalidClassInDomain',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
	             'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' }
             }.
noImmediateClassSC(Database, Reason) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#ObjectProperty'),
    range(P,X,Database),
    \+ immediateClass(X,Database), \+ restriction(X,Database),
    interpolate([X,' is used as a range for property ',P,' but is not defined'], Message),
    Reason = _{
                 '@type' : 'vio:InvalidClassInRange',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
	             'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' }
             }.
noImmediateClassSC(Database, Reason) :-
    subClassOf(X,Y,Database),
    \+ customDatatype(X,Database), \+ immediateClass(X,Database), \+ restriction(X,Database),
    interpolate(['The class ',Y,' is not a superclass of a defined class ',X], Message),
    Reason = _{
                 '@type' : 'vio:ClassInheritanceVioltion',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
	             'vio:parent' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.
noImmediateClassSC(Database, Reason) :-
    intersectionOf(X,Y,Database),
    \+ immediateClass(X,Database), \+ restriction(X,Database),
    interpolate(['The class ',X,' is an intersection of ', Y,' but not a defined class'], Message),
    Reason = _{
                 '@type' : 'vio:ClassInheritanceVioltion',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
	             'vio:parent' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.
noImmediateClassSC(Database, Reason) :-
    unionOf(X,Y,Database),
    \+ immediateClass(Y,Database), \+ restriction(Y,Database),
    interpolate(['The class ',X,' is not a union of a defined class ',Y], Message),
    Reason = _{
                 '@type' : 'vio:ClassInheritanceVioltion',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
	             'vio:parent' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.

%% restrctionOnProperty(?CR:uri_or_id, ?P:uri_or_id, +Database:database) is nondet
%
% Defines the relation between properties and their restrictions.
%
% @param CR A restriction class specified as a URI_OR_ID
% @param P A property specified as a URI_OR_ID
% @param Database the current graph
restrictionOnProperty(CR,P,Database) :-
    database_schema(Database,Schema),
	xrdf(Database,Schema,CR,'http://www.w3.org/2002/07/owl#onProperty',P),
	restriction(CR,Database).
restrictionOnProperty(CR,P,Database) :-
	strictSubsumptionPropertiesOf(P,Q,Database),
	restrictionOnProperty(CR,Q,Database).

%% classOrRestriction(?X:uri_or_id, +Database:database is nondet
%
% All URI_OR_IDs which are either a class or restriction.
%
% @param X A class or restriction class specified as a URI_OR_ID
% @param Database The current graph
classOrRestriction(X,Database) :- class(X,Database).
classOrRestriction(X,Database) :- restriction(X,Database).

% TODO: compound is vague, reasons should have a specification.

%% notUniqueClass(+Y:uri_or_id, +Database:database -Reason:any) is nondet
%
% Is a class multiply defined?
%
% @param Y A class specified as a URI_OR_ID
% @param Schema The current schema graph
notUniqueClass(Y, Database, Reason) :-
    classOrRestriction(Y, Database),
    bagof(Y, classOrRestriction(Y, Database), L),
    \+ length(L,1),
    interpolate(['The class or restriction ',Y,
		         ' is not a unique. Some existing class has this identifier'],
		        Message),
    Reason = _{
                 '@type' : 'vio:InvalidClassViolation',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.
notUniqueClassSC(Database,Reason) :- notUniqueClass(_,Database,Reason).


%% collect(+collection:atom, +Database_ID:graph_identifier, +X:uri_or_id, -L:any) is semidet.
%
% Collect the RDF list into a prolog list.
% It may be better to treat lists programmatically through rdf rather than
% collect them, using a derived predicate like rdfListMembership
%
% @param X The URI_OR_ID of an RDF list
% @param L Term
% @param Database The current graph
collect(_,_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :-
    !.
collect(Database,DatabaseAtom,X,[H|T]) :-
    xrdf(Database,DatabaseAtom,X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H),
    !, % assume one result
    xrdf(Database,DatabaseAtom,X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',Y),
    !, % assume one result
    collect(Database,DatabaseAtom,Y,T).

%% subClassOf(?Child:uri_or_id,?Parent:uri_or_id,+Database:database is nondet
%
% One step subclassing (only gives the immediate child-parent class relationship)
%
% @param Child Child class URI_OR_ID
% @param Parent Parent class URI_OR_ID.
subClassOf(Child,Parent,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,Child, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', Parent).

%% unionOf(?Super:uri_or_id,?Sub:uri_or_id,+Database:database is nondet
%
% Gives URI_OR_ID solutions for which the Super URI_OR_ID is determined to be a union of.
%
% @param Super The class URI_OR_ID which is the union of other classes.
% @param Sub The class URI_OR_ID which is unioned to form the Super class.
% @param Database The current graph
unionOf(C,U,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,C,'http://www.w3.org/2002/07/owl#unionOf',ListObj),
    collect(Database,Schema,ListObj,L),
    member(U,L).

%% unionOfList(+Super:uri_or_id,-Sub:list(uri_or_id),+Database:database is det
%
% Gives a list of URI_OR_IDs which are solutions for which the Super URI_OR_ID is determined to be a union of.
%
% @param Super The class URI_OR_ID which is the union of other classes.
% @param Sub The class URI_OR_ID which is unioned to form the Super class.
% @param Database The current graph
unionOfList(C,UList,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,C,'http://www.w3.org/2002/07/owl#unionOf',ListObj),
    collect(Database,Schema,ListObj,UList),
    % This looks so dubious, I hope I didn't write it.
    !.

%% disjointUnionOf(?Super:uri_or_id,?Sub:uri_or_id,+Database:database is nondet
%
% Gives URI_OR_ID solutions for which the Super URI_OR_ID is determined to be a union of.
%
% @param Super The class URI_OR_ID which is the disjoint union of other classes.
% @param Sub A class URI_OR_ID which is disjointly unioned to form the Super class.
% @param Database The current graph
%% :- rdf_meta disjointUnionOf(r,r,o).
disjointUnionOf(C,U,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,C,'http://www.w3.org/2002/07/owl#disjointUnionOf',ListObj),
    collect(Database,Schema,ListObj,L),
    member(U,L).

%% disjointUnionOfList(+Super:uri_or_id,-SubList:list(uri_or_id),+Database:database is det
%
% Gives URI_OR_ID solutions as a list for which the Super URI_OR_ID is determined to be a djsoint union of.
%
% @param Super The class URI_OR_ID which is the disjoint union of other classes.
% @param SubList The prolog list of class URI_OR_IDs which are disjointly unioned to form the Super class.
% @param Database The current schema graph.
%% :- rdf_meta disjointUnionOfList(r,r,o).
disjointUnionOfList(C,UList,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,C,'http://www.w3.org/2002/07/owl#disjointUnionOf',ListObj),
    collect(Database,Schema,ListObj,UList),
    % DUBIOUS
    !.

%% intersectionOf(?Inter:uri_or_id,?Class:uri_or_id,+Database:database is nondet
%
% Gives URI_OR_ID solutions for which the Super URI_OR_ID is determined to be an intersection.
%
% @param Inter The class URI_OR_ID which is the intersection of the Class URI_OR_ID.
% @param Class A class URI_OR_ID of which Inter is an intersection.
% @param Database The current schema graph
%% :- rdf_meta intersectionOf(r,r,o).
intersectionOf(C,I,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,C,'http://www.w3.org/2002/07/owl#intersectionOf',ListObj),
    collect(Database,Schema,ListObj,L),
    member(I,L).

%% disjointUnionOfList(+Inter:uri_or_id,-Classes:list(uri_or_id),+Database:database is det
%
% Gives URI_OR_ID solutions as a list for which the Super URI_OR_ID is determined to be an intersection.
%
% @param Inter The class URI_OR_ID which is the disjoint union of other classes.
% @param Classes The prolog list of class URI_OR_IDs which are intersected.
% @param Database The current schema graph.
%% :- rdf_meta intersectionOfList(r,r,o).
intersectionOfList(C,IList,Database) :-
    database_schema(Database,Schema),
    xrdf(Database, Schema, C,'http://www.w3.org/2002/07/owl#intersectionOf',ListObj),
    collect(Database,Schema,ListObj,IList),
    !.

%% oneOf(?CC:uri_or_id,?X:uri_or_id,+Database:database is det
%
% Gives elements which are members of a class by enumeration.
%
% @param Collection,Schema,CC The class URI_OR_ID of which X isr.
% @param X The URI_OR_ID of the element which is a member of CC.
% @param Database The current schema graph.
%% :- rdf_meta oneOf(r,r,o).
oneOf(CC,X,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CC,'http://www.w3.org/2002/07/owl#oneOf',ListObj),
    collect(Database,Schema,ListObj,OneList),
    member(X,OneList).

%% oneOfList(+CC:uri_or_id,-OneList:list(uri_or_id),+Database:database is det
%
% Gives a prolog list of elements associated with an enumerated class.
%
% @param CC The class URI_OR_ID of which X is a member.
% @param OneList The URI_OR_ID list of elements which are members of CC.
% @param Database The current schema graph.
%% :- rdf_meta oneOfList(r,r,o).
oneOfList(C,OneList,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,C,'http://www.w3.org/2002/07/owl#oneOf',ListObj),
    collect(Database,Schema,ListObj,OneList).

%% :- rdf_meta complementOf(r,r,o).
complementOf(CC,CN,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CC,'http://www.w3.org/2002/07/owl#complementOf',CN).

%% :- rdf_meta datatypeComplementOf(r,r,o).
datatypeComplementOf(CC,CN,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CC,'http://www.w3.org/2002/07/owl#datatypeComplementOf',CN).

%% :- rdf_meta equivalentClass(r,r,o).
equivalentClass(CC,CE,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,CC,'http://www.w3.org/2002/07/owl#equivalentClass',CE).

/*  Previous code for equivalent class.
    xrdf(CC,'http://www.w3.org/2002/07/owl#equivalentClass',ListObj,Schema),
    collect(ListObj,L,Schema),
    member(CE,L). */

%% :- rdf_meta equivalentClass(r,r,o).
anonymousEquivalentClass(C,CE,Database) :-
    database_schema(Database,Schema),
    equivalentClass(C,CE,Database),
    % Exactly one reference to this class, or everything will go to hell.
    (setof(X,xrdf(Database,Schema,X,_,CE), ListX) *-> ListX = L ; L = []),
    length(L,1).

% transitive strict relation
subClassStrict(X,Y,Database) :- subClassOf(X,Y,Database).
subClassStrict(X,Z,Database) :- subClassOf(X,Y,Database), subClassStrict(Y,Z, Database).

% Implements class subsumption
% - complementOf classes do not give subsumption properly yet (unimplemented).
%   Requires anti-subsumption predicate
% - oneOf should probably have individual sets for both CC, CP
%
% static solutions first.
subsumptionOf(_,'http://www.w3.org/2002/07/owl#Thing',_).
subsumptionOf('http://terminusdb.com/schema/tcs#Entity','http://terminusdb.com/schema/tcs#Document',_).
subsumptionOf('http://terminusdb.com/schema/tcs#Relationship','http://terminusdb.com/schema/tcs#Document',_).
subsumptionOf(CC,CC,Database) :-
    immediateClass(CC,Database).
subsumptionOf(CC,CP,Database) :-
    subClassOf(CC,CZ,Database),
    subsumptionOf(CZ,CP,Database).
subsumptionOf(CC,CP,Database) :-
    immediateClass(CC,Database),
    unionOf(CZ,CC,Database),
    subsumptionOf(CZ,CP,Database).
subsumptionOf(CC,CP,Database) :-
    immediateClass(CC,Database),
    disjointUnionOf(CZ,CC,Database),
    subsumptionOf(CZ,CP,Database).
subsumptionOf(CC,CP,Database) :-
    immediateClass(CC,Database),
    intersectionOf(CC,CZ,Database),
    subsumptionOf(CZ,CP,Database).
subsumptionOf(CC,CP,Database) :-
    anonymousEquivalentClass(CC,CZ,Database),
    subsumptionOf(CZ,CP,Database).
subsumptionOf(CC,CP,Database) :- % datatypes
    datatype(CC,Database),
    datatypeSubsumptionOf(CC,CP,Database).

%% :- rdf_meta customDatatype(r,o).
customDatatype(X,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2000/01/rdf-schema#Datatype').

/**
 * datatype(-X:uri_or_id,+Database:database is nondet.
 * datatype(-X:uri_or_id,+Database:database is det.
 */
%% :- rdf_meta datatype(r,o).
datatype(X,Database) :- customDatatype(X,Database).
datatype(X,_) :- baseType(X).

% implements strict class subsumption (CC < CP) [Needs fully instantiated arguments]
%% :- rdf_meta strictSubsumptionOf(r,r,o).
strictSubsumptionOf(CC,'http://www.w3.org/2002/07/owl#Thing',_) :- CC \= 'http://www.w3.org/2002/07/owl#Thing'.
strictSubsumptionOf('http://www.w3.org/2002/07/owl#Nothing',CP,_) :- CP \= 'http://www.w3.org/2002/07/owl#Nothing'.
strictSubsumptionOf(CC,CP,Database) :-
    subClassOf(CC,CP,Database).
strictSubsumptionOf(CC,CP,Database) :-
    class(CC,Database),
    unionOf(CP,CC,Database).
strictSubsumptionOf(CC,CP,Database) :-
    class(CC,Database),
    intersectionOf(CC,CP,Database).
strictSubsumptionOf(CC,CP,Database) :-
    subClassOf(CC,CZ,Database),
    strictSubsumptionOf(CZ,CP,Database).
strictSubsumptionOf(CC,CP,Database) :-
    class(CC,Database),
    unionOf(CZ,CC,Database),
    strictSubsumptionOf(CZ,CP,Database).
strictSubsumptionOf(CC,CP,Database) :-
    class(CC,Database),
    disjointUnionOf(CZ,CC,Database),
    strictSubsumptionOf(CZ,CP,Database).
strictSubsumptionOf(CC,CP,Database) :-
    class(CC,Database),
    intersectionOf(CC,CZ,Database),
    strictSubsumptionOf(CZ,CP,Database).
strictSubsumptionOf(CC,CP,Database) :- % xsd and custom data types
    datatypeStrictSubsumptionOf(CC,CP,Database).




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
 * datatypeSubsumptionOf(?Sub,?Super,+Database:database is nondet.
 *
 * Implements the subsumption latice for datatypes by making use of reflexivity and
 * baseTypeParent\2.
 */
%% :- rdf_meta datatypeSubsumptionOf(r,r).
datatypeSubsumptionOf(T,T,Database) :- datatype(T,Database).
datatypeSubsumptionOf(Sub,Super,Database) :-
    customDatatype(Sub,Database),
    unionOf(Super,Sub,Database).
datatypeSubsumptionOf(Sub,Super,Database) :-
    customDatatype(Sub,Database),
    intersectionOf(Sub,Super,Database).
datatypeSubsumptionOf(Sub,Super,Database) :-
    % This only works because of the strict hierarchy (no derived union / intersections)
    customDatatype(Sub,Database),
    datatypeComplementOf(Sub,CN,Database),
    \+ datatypeSubsumptionOf(CN,Super,Database).
datatypeSubsumptionOf(Sub,Super,Database) :-
    baseTypeParent(Sub,Parent), datatypeSubsumptionOf(Parent,Super,Database).

/**
 * datatypeSubsumptionOf(?Sub,?Super,+Database:database is nondet.
 *
 * Implements strict (non reflexive) subsumption latice for datatypes by
 * baseTypeParent\2.
 */
%% :- rdf_meta datatypeStrictSubsumptionOf(r,r).
datatypeStrictSubsumptionOf(Sub,Super,_) :-
    baseTypeParent(Sub,Super).
datatypeStrictSubsumptionOf(Sub,Super,Database) :-
    % DDD probably need one more clause for each owl custom build property
    baseTypeParent(Sub,Parent),
    datatypeSubsumptionOf(Parent,Super,Database).

/**
 * orphanClassSC(+Database:database-Reason:vio) is nondet.
 *
 * Find all orphaned classes in Schema.
 */
orphanClassSC(Database, Reason) :-
    subClassOf(X,Y,Database),
    \+ class(Y,Database), \+ restriction(Y,Database),
    interpolate(['The class ',X,' is not a subclass of a valid class ',Y], Message),
    Reason = _{
                 '@type' : 'vio:ClassInheritanceVioltion',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
	             'vio:parent' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.
orphanClassSC(Database, Reason) :-
    intersectionOf(X,Y,Database),
    \+ class(Y,Database), \+ restriction(Y,Database),
    interpolate(['The class ',X, ' is not an intersection of a valid class ',Y], Message),
    Reason = _{
                 '@type' : 'vio:ClassInheritanceVioltion',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
	             'vio:parent' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.
orphanClassSC(Database, Reason) :-
    unionOf(X,Y,Database),
    \+ class(Y,Database), \+ restriction(Y,Database),
    interpolate(['The class ',X,' is not a union of a valid class ',Y], Message),
    Reason = _{
                 '@type' : 'vio:ClassInheritanceVioltion',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : X, '@type' : 'xsd:anyURI' },
	             'vio:parent' : _{ '@value' : Y, '@type' : 'xsd:anyURI' }
             }.

% Cycles in subsumption diagram
classCycleHelp(C,S,[],_) :- get_assoc(C,S,true), !.
classCycleHelp(C,S,[K|P],Database) :-
    subClassOf(K,C,Database),
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Database).
classCycleHelp(C,S,[K|P],Database) :-
    unionOf(C,K,Database),
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Database).
classCycleHelp(C,S,[K|P],Database) :-
    disjointUnionOf(C,K,Database),
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Database).
classCycleHelp(C,S,[K|P],Database) :-
    intersectionOf(K,C,Database),
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Database).
classCycleHelp(C,S,[K|P],Database) :-
    equivalentClass(K,C,Database),
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Database).
classCycleHelp(C,S,[K|P],Database) :-
    complementOf(K,C,Database),
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Database).
classCycleHelp(C,S,[K|P],Database) :-
    datatypeComplementOf(K,C,Database),
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Database).

classCycle(C,P,Database,Reason) :-
    empty_assoc(S), classCycleHelp(C,S,P,Database),
    interpolate(['Class, ',C,' has a class cycle with path: ', P], Message),
    pathify(P,JSON),
    Reason = _{
                 '@type' : 'vio:ClassCycle',
                 'vio:path' : JSON,
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:class' : _{ '@value' : C, '@type' : 'xsd:anyURI' }
             }.

classCycleSC(Database,Reason) :- classCycle(_,_,Database,Reason), !.

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
rdfProperty(P,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').

%:- rdf_meta datatypeProperty(r,o).
datatypeProperty(P,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#DatatypeProperty').
datatypeProperty(P,_) :- rdfsDatatypeProperty(P).

%:- rdf_meta annotationProperty(r,o).
annotationProperty(P,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#AnnotationProperty').
% Gavin nuked on Sep 20th 2019
%annotationProperty(P,Database) :-
%    subsumptionPropertiesOf(P,'http://terminusdb.com/schema/tcs#pseudo_property', Database).


%:- rdf_meta functionalProperty(r,o).
functionalProperty(P,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#FunctionalProperty').

%:- rdf_meta inverseFunctionalProperty(r,o).
inverseFunctionalProperty(P,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#InverseFunctionalProperty').

%:- rdf_meta objectProperty(r,o).
objectProperty(P,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#ObjectProperty').
objectProperty(P,_) :- rdfsObjectProperty(P).

/**
 * property(?P,+Database:database is nondet.
 *
 * ?P is a valid property in the schema.
 */
%:- rdf_meta property(r,o).
property(P,Database) :-
    % Don't predicate over annotations (even if they are otherwise declared as properties).
    (   annotationProperty(P,Database)
    *-> fail
    ;   (   datatypeProperty(P, Database)
        ;   objectProperty(P,Database)
        ;   rdfProperty(P,Database)
        )
    ).

%uniqueProperty(P,Database) :- property(P,Database), bagof(P2, property(P2,Database), L), count(P,L,1).

notUniqueProperty(P,Database,Reason) :-
    property(P,Database),
    bagof(P, property(P,Database), L),
    \+ length(L,1),
    %break,
    interpolate([P,' is not a unique property name, some property with this name already exists'],
		        Message),
    Reason = _{
                 '@type' : 'vio:NonUniquePropertyName',
	             'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' }
             }.

propertyTypeOverloadSC(Database,Reason) :-
    datatypeProperty(P,Database), objectProperty(P,Database),
    interpolate([P,' is an objectProperty and a datatypeProperty'], Message),
    Reason= _{
                '@type' : 'vio:PropertyTypeOverload',
                'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
	            'vio:property' :  _{ '@value' : P, '@type' : 'xsd:anyURI' }
            }.

annotationOverloadSC(Database,Reason) :-
    (datatypeProperty(P,Database) ; objectProperty(P,Database) ; rdfProperty(P,Database)),
    annotationProperty(P,Database),
    interpolate([P,' is defined as a property and defined as an annotationProperty'], Message),
    Reason= _{
                '@type' : 'vio:PropertyTypeOverload',
                'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
	            'vio:property' :  _{ '@value' : P, '@type' : 'xsd:anyURI' }
            }.

/**
 * notUniquePropertySC(+Database:database-Reason:any) is nondet.
 *
 * All redundantly defined properties.
 */
notUniquePropertySC(Database,Reason) :-
    notUniqueProperty(_,Database, Reason).

% One step subproperty relation
%:- rdf_meta subPropertyOf(r,r,o).
subPropertyOf(X,Y,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,X,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',Y).

/**
 * subsumptionPropertiesOf(?PChild,?PParent,+Database:database is nondet.
 *
 * Transitive reflexive closure of Subproperty relation.
 */
%:- rdf_meta subsumptionPropertiesOf(r,r,o).
subsumptionPropertiesOf(PC,PC,_).
subsumptionPropertiesOf(PC,PP,Database) :-
    subPropertyOf(PC, PZ, Database),
    subsumptionPropertiesOf(PZ,PP,Database).
subsumptionPropertiesOf(PC,'http://www.w3.org/2002/07/owl#topObjectProperty',Database) :-
    objectProperty(PC,Database).
subsumptionPropertiesOf(PC,'http://www.w3.org/2002/07/owl#topDataProperty',Database) :-
    datatypeProperty(PC,Database).

/**
 * strictSubsumptionPropertiesOf(?PChild,?PParent,-Database:database is nondet.
 *
 * Non-reflexive subsumption relation for properties in Database.
 */
%:- rdf_meta strictSubsumptionPropertiesOf(r,r,o).
strictSubsumptionPropertiesOf(PC,PP,Database) :-
    subPropertyOf(PC, PP, Database).
strictSubsumptionPropertiesOf(PC,PP,Database) :-
    subPropertyOf(PC, PZ, Database),
    subsumptionPropertiesOf(PZ,PP,Database).
strictSubsumptionPropertiesOf(PC,'http://www.w3.org/2002/07/owl#topObjectProperty',Database) :-
    PC \= 'http://www.w3.org/2002/07/owl#topObjectProperty',
    objectProperty(PC,Database).
strictSubsumptionPropertiesOf(PC,'http://www.w3.org/2002/07/owl#topDataProperty',Database) :-
    PC \= 'http://www.w3.org/2002/07/owl#topDataProperty',
    datatypeProperty(PC,Database).

orphanProperty(X,Y,Database,Reason) :-     % is a subproperty of Y but Y is not a valid property
    subPropertyOf(X,Y,Database),
    \+ property(Y,Database), \+ annotationProperty(Y,Database),
    interpolate([X,' is a sub-property of', Y,' which is not designated as a property of any type in the schema!' ], Message),
    Reason= _{
                '@type' : 'vio:PropertyInheritanceViolation',
                'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
	            'vio:property' :  _{ '@value' : X, '@type' : 'xsd:anyURI' },
                'vio:parent_property' :  _{ '@value' : Y, '@type' : 'xsd:anyURI' }
            }.
orphanProperty(P,R,Database,Reason) :-  % property has a range but it is not the property of a Classs
	range(P,R,Database),
    \+ property(P,Database), \+ annotationProperty(P,Database),
	interpolate([P,' has a range, but is not designated as a property of any type in the schema!'],Message),
    Reason= _{
                '@type' : 'vio:UntypedPropertyWithRange',
                'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
	            'vio:property' :  _{ '@value' : P, '@type' : 'xsd:anyURI' },
                'vio:range' :  _{ '@value' : R, '@type' : 'xsd:anyURI' }
            }.
orphanProperty(P,D,Database,Reason) :-     %% property has a domain but it is not the property of a Classs
	domain(P,D,Database),
    \+ property(P,Database), \+ annotationProperty(P,Database),
    interpolate([P,' has a domain, but is not designated as a property of any type in the schema!'],Message),
    Reason= _{
                '@type' : 'vio:UntypedPropertyWithDomain',
                'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
	            'vio:property' :  _{ '@value' : P, '@type' : 'xsd:anyURI' },
                'vio:domain' :  _{ '@value' : D, '@type' : 'xsd:anyURI' }
            }.

/**
 * orphanPropertySC(+Database:database-Reason:any) is nondet.
 *
 * All properties which are referred to, but which do not have definitions.
 */
orphanPropertySC(Database,Reason) :-
    orphanProperty(_,_,Database,Reason).

% subProperty cycles

propertyCycleHelp(P,S,[],_) :- get_assoc(P,S,true), !.
propertyCycleHelp(P,S,[Q|T],Database) :-
    property(P,Database), subPropertyOf(Q,P,Database), put_assoc(P, S, true, S2),
    propertyCycleHelp(Q,S2,T,Database).

propertyCycle(P,PC,Database,Reason) :-
    empty_assoc(S), propertyCycleHelp(P,S,PC,Database),
    interpolate(['Property class ', P, ' has a cycle with path: ', PC], Message),
    pathify(PC, JSON),
    Reason = _{
                 '@type' : 'vio:ClassCycle',
                 'vio:property_cycle' : JSON,
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' }
             }.

/**
 * propertyCycleSC(+Database:database-Reason:any) is nondet.
 *
 * All property subsumption cycles in Database.
 */
propertyCycleSC(Database,Reason) :- propertyCycle(_,_,Database,Reason).

/**
 * range(?P:uri, ?R:uri, +Database:database is nondet.
 *
 * Actually specified range for P.
 */
range(P,R,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/2000/01/rdf-schema#range',R).

/**
 * document(?Class,+Database) is nondet.
 *
 * This is a foundational predicate for DQS which establishes
 * the places in which to "clip" the graph.
 */
document(Class,Database) :-
	subsumptionOf(Class,'http://terminusdb.com/schema/tcs#Document', Database).

/**
 * anyRange(?P,?R,+Database:database is nondet.
 *
 * Determine if R is a viable range for P.
 * This must be ordered according to Hasse diagram!
 */
%:- rdf_meta anyRange(r,r,o).
anyRange('http://www.w3.org/2002/07/owl#topObjectProperty','http://www.w3.org/2002/07/owl#Thing',_).
anyRange('http://www.w3.org/2002/07/owl#topDataProperty','http://www.w3.org/2000/01/rdf-schema#Literal',_).
anyRange('http://www.w3.org/2000/01/rdf-schema#label','http://www.w3.org/2001/XMLSchema#string',_).
anyRange('http://www.w3.org/2000/01/rdf-schema#comment','http://www.w3.org/2001/XMLSchema#string',_).
anyRange(P,R,Database) :-
    range(P,R,Database).
anyRange(P,R,Database) :-
    strictSubsumptionPropertiesOf(P,P2,Database),
    anyRange(P2,R,Database).
anyRange(OP,R,Database) :-
    database_inference(Database,Inference),
    xrdf(Database,Inference,OP,'http://www.w3.org/2002/07/owl#inverseOf',P),
    anyDomain(P,R,Database).

%:- rdf_meta mostSpecificRange(r,r,o).
mostSpecificRange(P,R,Database) :- anyRange(P,R,Database), !.

/**
 * domain(P:uri,D:uri,Database:database) is nondet.
 *
 * Actually specified domain in the database.
 */
domain(P,D,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,P,'http://www.w3.org/2000/01/rdf-schema#domain',D).

/**
 * anyDomain(?P,?R,+Database:database) is nondet.
 *
 * Determine if R is a viable domain for P.
 */
%:- rdf_meta anyDomain(r,r,o).
anyDomain('http://www.w3.org/2002/07/owl#topObjectProperty',
          'http://www.w3.org/2002/07/owl#Thing',_).
anyDomain('http://www.w3.org/2002/07/owl#topDataProperty',
          'http://www.w3.org/2000/01/rdf-schema#Literal',_).
anyDomain('http://www.w3.org/2000/01/rdf-schema#label',D,Database) :-
    document(D,Database).
anyDomain('http://www.w3.org/2000/01/rdf-schema#comment',D,Database) :-
    document(D,Database).
anyDomain(P,R,Database) :-
    domain(P,R,Database).
anyDomain(OP,R,Database) :-
    database_inference(Database,Inference),
    xrdf(Database,Inference,OP,'http://www.w3.org/2002/07/owl#inverseOf',P),
    anyRange(P,R,Database).
anyDomain(P,R,Database) :-
    strictSubsumptionPropertiesOf(P,P2,Database),
    anyDomain(P2,R,Database).

% TODO inverse isn't enough! need to do more inference

%:- rdf_meta mostSpecificDomain(r,r,o).
mostSpecificDomain(P,R,Database) :- anyDomain(P,R,Database), !.

/**
 * noImmediateDomainSC(+Database:database-Reason:vio) is nondet.
 *
 * All properties which do not have a specified domain.
 */
noImmediateDomainSC(Database,Reason) :-
    property(P,Database),
    \+ rdfsProperty(P),
    \+ domain(P,_,Database),
    (   datatypeProperty(P,Database)
    ->  M='Data property '
    ;   annotationProperty(P,Database)
    ->  M='Annotation property '
    ;   objectProperty(P,Database)
    ->  M='Object property '
    ;   rdfProperty(P,Database)
    ->  M='Rdf Property '
    ;   M='Unknown Property '),
    interpolate([M, P, ' has no specified domain.'], Message),
    Reason = _{
                 '@type' : 'vio:PropertyWithNoDomain',
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'}
             }.

/**
 * noImmediateRangeSC(+Database:database-Reason:vio) is nondet.
 *
 * All properties which do not have a specified range.
 */
noImmediateRangeSC(Database,Reason) :-
    property(P,Database),
    \+ rdfsProperty(P),
    \+ range(P,_,Database),
    (   datatypeProperty(P,Database)
    ->  M='Data property '
    ;   annotationProperty(P,Database)
    ->  M='Annotation property '
    ;   objectProperty(P,Database)
    ->  M='Object property '
    ;   rdfProperty(P,Database)
    ->  M='Rdf Property '
    ;   M='Unknown Property '),
    interpolate([M, P, ' has no specified range.'], Message),
    Reason = _{
                 '@type' : 'vio:PropertyWithNoRange',
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'}
             }.

/**
 * invalidDomainSC(+Database:database-Reason:any) is nondet.
 *
 * Finds all domains that are not valid classes for a given property in Database.
 */
invalidDomainSC(Database,Reason) :-
    property(P,Database),
    domain(P,D,Database),
    \+ class(D,Database),
    interpolate(['The property ', P,' has an undefined domain.'],Message),
    Reason = _{
                 '@type' : 'vio:PropertyWithUndefinedDomain',
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:domain' : _{ '@value' : D, '@type' : 'xsd:anyURI'}
             }.

/**
 * invalid_RDFS_property_SC(+Database:database-Reason:any) is nondet.
 *
 * Finds all domains that are not valid classes for a given property in Database.
 */
invalid_RDFS_property_SC(Database,Reason) :-
    database_schema(Database,Schema),
    rdfsProperty(P),
    xrdf(Database,Schema,_,P,Y),
    refute_basetype_elt(Y,'http://www.w3.org/2001/XMLSchema#string',Reason).

/**
 * invalidRangeSC(+Database:database-Reason:any) is nondet.
 *
 * Finds all ranges that are not valid classes for a given property in Database.
 */
invalidRangeSC(Database,Reason) :-
    datatypeProperty(P,Database),
    range(P,R,Database),
    \+ datatype(R,Database), \+ rdfsProperty(P),
    interpolate(['DataProperty Range ', R, ' is not a valid (or implemented) datatype for property ', P,'.'], Message),
    Reason = _{
                 '@type' : 'vio:PropertyWithUndefinedRange',
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:range' : _{ '@value' : R, '@type' : 'xsd:anyURI'}
             }.
invalidRangeSC(Database,Reason) :-
    objectProperty(P,Database),
    range(P,R,Database),
    \+ class(R,Database), \+ rdfsProperty(P),
    interpolate(['ObjectProperty Range ',R,' is not a valid range for property ',P,'.'],Message),
    Reason = _{
                 '@type' : 'vio:PropertyWithUndefinedRange',
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:range' : _{ '@value' : R, '@type' : 'xsd:anyURI'}
             }.
invalidRangeSC(Database,Reason) :-
    rdfProperty(P,Database),
    range(P,R,Database),
    \+ class(R,Database), \+ baseType(R),
    interpolate(['rdf:Property range ',R,' is not a valid range for property ',P,'.'],Message),
    Reason = _{
                 '@type' : 'vio:PropertyWithUndefinedRange',
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'},
                 'vio:range' : _{ '@value' : R, '@type' : 'xsd:anyURI'}
             }.



% Logging / Turn off for production
:- use_module(library(http/http_log)).
/**
 * domainNotSubsumedsC(+Database:database-Reason:any) is nondet.
 *
 * All domains which can not validly be subsumed within a property subsumption hierarchy for
 * the Database .
 */
domainNotSubsumedSC(Database,Reason) :-
    property(P,Database),
    strictSubsumptionPropertiesOf(P,P2,Database),
    domain(P,D,Database), domain(P2,D2,Database), % DDD too many solutions
    %http_log_stream(Log),
    %current_output(Log),
    %findall(X, subsumptionOf(D,X,Database), L),
    %nl(Log), nl(Log), write(Log, 'Subsumptions: '), write_canonical(Log, L), nl(Log), nl(Log),
    %nl(Log), nl(Log), write(Log, 'Listing: '), write_canonical(Log, L), nl(Log), nl(Log),
    \+ subsumptionOf(D, D2, Database),
    interpolate(['Invalid domain on property ', P,
		         ', due to failure of domain subsumption.'], Message),
    Reason = _{
                 '@type' : 'vio:PropertyDomainNotSubsumed',
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:parent_property' : _{ '@value' : P2, '@type' : 'xsd:anyURI' },
                 'vio:class' : _{ '@value' : D, '@type' : 'xsd:anyURI'},
                 'vio:parent' : _{ '@value' : D2, '@type' : 'xsd:anyURI'},
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'}
             }.

/**
 * rangeNotSubsumedSC(+Database:database-Reason:any) is nondet.
 *
 * Database constraint determining that ranges must be valid
 * a property through its entire subsumption chain.
 */
rangeNotSubsumedSC(Database,Reason) :-
    property(P,Database),
    strictSubsumptionPropertiesOf(P,P2,Database),
    range(P,R,Database), range(P2,R2,Database), % DDD too many solutions
    \+ subsumptionOf(R, R2, Database),
    interpolate(['Invalid range on property ', P,
		         ', due to failure of range subsumption.'], Message),
    Reason = _{
                 '@type' : 'vio:PropertyRangeNotSubsumed',
                 'vio:property' : _{ '@value' : P, '@type' : 'xsd:anyURI' },
                 'vio:parent_property' : _{ '@value' : P2, '@type' : 'xsd:anyURI' },
                 'vio:class' : _{ '@value' : R, '@type' : 'xsd:anyURI'},
                 'vio:parent' : _{ '@value' : R2, '@type' : 'xsd:anyURI'},
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'}
             }.

schemaSubjectBlankNode(X,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,X,_,_),
    is_bnode(X).

schemaPredicateBlankNode(Y,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,_,Y,_),
    is_bnode(Y).

schemaObjectBlankNode(Z,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,_,_,Z),
    is_bnode(Z).

/**
 * schemaBlankNodeSC(+Database:database-Reason:any) is nondet.
 *
 * Is this a blank node in the schema.
 */
/*
schemaBlankNodeSC(Database,Reason) :-
    schemaSubjectBlankNode(X,Database),
    interpolate(['The subject ', X, ' is a blank node'],Message),
    Reason=['rdf:type'='DatabaseBlankNodeViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    message=Message,
	    subject=X].
schemaBlankNodeSC(Database,Reason) :-
    schemaPredicateBlankNode(X,Database),
    interpolate(['The predicate ', X, ' is a blank node'],Message),
    Reason=['rdf:type'='DatabaseBlankNodeViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    message=Message,
	    predicate=X].
schemaBlankNodeSC(Database,Reason) :-
    schemaObjectBlankNode(X,Database),
    interpolate(['The object ', X, ' is a blank node'],Message),
    Reason=['rdf:type'='DatabaseBlankNodeViolation',
	    bestPractice=literal(type('xsd:boolean',true)),
	    message=Message,
	    object=X].
*/

/**
 * label(?X,?Y,+Database:database is det.
 *
 * Get the rdfs:label for X as Y.
 */
label(X,Y,Database) :-
    database_instance(Database,Instance),
    xrdf(Database,Instance,X, 'http://www.w3.org/2000/01/rdf-schema#label',Y).
label(X,Y,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,X, 'http://www.w3.org/2000/01/rdf-schema#label',Y).

/**
 * comment(?X,?Y,+Database:database is det.
 *
 * Get the rdfs:comment for X as Y.
 */
comment(X,Y,Database) :-
    database_instance(Database,Instance),
    xrdf(Database,Instance,X, 'http://www.w3.org/2000/01/rdf-schema#comment', Y).
comment(X,Y,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,X, 'http://www.w3.org/2000/01/rdf-schema#comment', Y).

/**
 * tcs_tag(?X:uri_or_id,?Y:any,+Database:database is det.
 *
 * TODO: Rename!
 * Get the tcs:tag for X as Y.
 */
tcs_tag(X,Y,Database) :-
    database_schema(Database,Schema),
    xrdf(Database,Schema,X, 'http://terminusdb.com/schema/tcs#tag', Y).

classHasLabel(X,Y,Database) :- class(X,Database), label(X,Y,Database).
%classHasNoLabel(X,Database) :- class(X,Database), \+ label(X,_,Database).

classHasOneLabel(X,Database) :-
    classHasLabel(X,Label,Database),
    bagof(label(Y,Label2), classHasLabel(Y,Label2,Database), L), count(label(X,Label),L,1).

notUniqueClassLabel(X,Database,Reason) :-
    \+ classHasOneLabel(X,Database),
    interpolate(['Class ', X,' does not have exactly one lable.'], Message),
    Reason = _{
                 '@type' : 'vio:DuplicateClassLabelViolation',
                 'vio:class' : _{ '@value' : X, '@type' : 'xsd:anyURI'},
                 'vio:message' : _{ '@value' : Message, '@type' : 'xsd:string'}
             }.

/**
 * notUniqueClassLabelSC(+Database:database -Reason:vio) is nondet.
 *
 * What it says on the tin: every non unique label in Database as a schema constraint.
 */
notUniqueClassLabelSC(Database,Reason) :- notUniqueClassLabel(_,Database,Reason).
