:- module(inference,[
              inferredEdge/4
          ]).

/** <module> Inference
 *
 * Predicates for inferring additional triples from the Inference ontology.
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

:- use_module(expansions).

:- reexport(core(util/syntax)).

:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(validation)).

/**
 * runChain(?X,?P:list(uri),?Y,+Instance:atom,+Database:database is nondet.
 *
 * Run a property axiom chain PropList from X to Y.
 */
%:- table runChain/4.
runChain(X,[P],Y,Database) :-
    inferredEdge(X,P,Y,Database).
runChain(X,[P|PropList],Z,Database) :-
    database_instance(Database,I),
    xrdf(I,X,P,Y),
    runChain(Y,PropList,Z,Database).

/**
 * inferredTransitiveEdge(?X,?OP:property_uri,Z,+Instance:atom,+Database:database is nondet.
 *
 * We impose an ordering to avoid non-termination (subproperty ordering)
 * Concrete links are already in InferredEdge
 */
inferredTransitiveEdge(X,OP,Z,Database) :-
    database_inference(Database,Inference),
    xrdf(Inference,
         SOP,rdfs:subPropertyOf,OP),
    inferredEdge(X,SOP,Y,Database),
    inferredEdge(Y,OP,Z,Database).

/**
 * inferredEdge(?X,?OP,?Y,+Database:database) is nondet.
 *
 * Calculates all available triples under inference
 *
 * [ owl:EquivalentProperty owl:ReflexiveProperty and others not yet implemented ]
 */
inferredEdge(X,OP,Y,Database) :-
    % No inference
    database_instance(Database,Instance),
    database_inference(Database,[]),
    !,
    xrdf(Instance,X,OP,Y).
inferredEdge(X,OP,Y,Database) :-
    % Base case for inference
    database_instance(Database,Instance),
    xrdf(Instance,X,OP,Y).
inferredEdge(X,OP,Y,Database) :-
    database_inference(Database,Inference),
    xrdf(Inference,OP,rdf:type,owl:'TransitiveProperty'),
    inferredTransitiveEdge(X,OP,Y,Database).
inferredEdge(X,OP,Y,Database) :-
    database_inference(Database,Inference),
    xrdf(Inference,OP,owl:'inverseOf',P),
    inferredEdge(Y,P,X,Database).
inferredEdge(X,OP,Y,Database) :-
    database_inference(Database,Inference),
    xrdf(Inference,OP,owl:propertyChainAxiom,ListObj),
    collect(Database,Inference,ListObj,PropList),
    runChain(X,PropList,Y,Database).
inferredEdge(X,OP,Y,Database) :-
    database_inference(Database,Inference),
    xrdf(Inference,SOP,rdfs:subPropertyOf,OP),
    inferredEdge(X,SOP,Y,Database).
