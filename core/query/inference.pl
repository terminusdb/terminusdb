:- module(inference,[
              inferredEdge/4,
              inferredQuad/5
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
 * runChain(?X,?P:list(uri),?Y,+Instance:atom,+Database:database) is nondet.
 *
 * Run a property axiom chain PropList from X to Y.
 */

% NOTE: (dubious)
% We stratefy property chain inference - never using it inside ourselves.
% This is probably incorrect, but it terminates
runChain(X,[P],Y,Database) :-
    inferred_quad_unchained(_,X,P,Y,Database).
runChain(X,[P|PropList],Z,Database) :-
    % database_instance(Database,I),
    inferred_quad_unchained(_,X,P,Y,Database), % xrdf(I,X,P,Y),
    runChain(Y,PropList,Z,Database).

/**
 * inferredTransitiveEdge(?X,?OP:property_uri,Z,+Instance:atom,+Database:database) is nondet.
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
 * inferredQuad(?G,?X,?Op,?Y,+Database:database) is nondet
 *
 * This predicate finds an edge together with the graph in which
 * it appears *OR* with a special 'inferred' atom.
 *
 * NOTE: for deletes it might be useful to know *how* it was inferred,
 * i.e. what were the base facts that allowed us to make the derivation
 * we will punt on that for now.
 */
inferredQuad(G,X,OP,Y,Database) :-
    % Base case for inference
    inferred_quad_unchained(G,X,OP,Y,Database).
inferredQuad(inferred,X,OP,Y,Database) :-
    database_inference(Database,Inference),
    xrdf(Inference,OP,owl:propertyChainAxiom,ListObj),
    collect(Database,Inference,ListObj,PropList),
    runChain(X,PropList,Y,Database).

inferred_quad_unchained(G,X,OP,Y,Database) :-
    % No inference
    database_instance(Database,Instance),
    database_inference(Database,[]),
    !,
    xquad(Instance,G,X,OP,Y).
inferred_quad_unchained(G,X,OP,Y,Database) :-
    % Base case for inference
    database_instance(Database,Instance),
    xquad(Instance,G,X,OP,Y).
inferred_quad_unchained(inferred,X,OP,Y,Database) :-
    database_inference(Database,Inference),
    xrdf(Inference,SOP,rdfs:subPropertyOf,OP),
    inferredEdge(X,SOP,Y,Database).
inferred_quad_unchained(inferred,X,OP,Y,Database) :-
    database_inference(Database,Inference),
    xrdf(Inference,OP,rdf:type,owl:'TransitiveProperty'),
    inferredTransitiveEdge(X,OP,Y,Database).
inferred_quad_unchained(inferred,X,OP,Y,Database) :-
    database_inference(Database,Inference),
    xrdf(Inference,OP,owl:'inverseOf',P),
    inferredEdge(Y,P,X,Database).

/**
 * inferredEdge(?X,?OP,?Y,+Database:database) is nondet.
 *
 * Calculates all available triples under inference
 *
 * [ owl:EquivalentProperty owl:ReflexiveProperty and others not yet implemented ]
 */
inferredEdge(X,OP,Y,Database) :-
    inferredQuad(_,X,OP,Y,Database).
