:- module(relationships,[
              relationship_source_property/3,
              relationship_target_property/3,
              relationship_to_pseudoedges_and_classes/4
          ]).

/** <module> Management of relationships 
 * 
 * This module helps other modules with the representation of graphs and collections 
 * by bundling them as objects with some convenience operators.
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

:- use_module(library(utils)).
:- use_module(library(validate_schema)).
:- use_module(library(database)).
:- use_module(library(triplestore)).
:- use_module(library(semweb/rdf_db)).

relationship_source_property(Relationship,Property,Graph) :-
    database_name(Graph,Collection),        
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,Relationship,dcog:source_property,Property).

relationship_target_property(Relationship,Property,Graph) :-
    database_name(Graph,Collection),        
    database_schema(Graph,Schema),
    xrdf(Collection,Schema,Relationship,dcog:target_property,Property).

pseudo_domain(Relationship,Domain,Graph) :-
    relationship_source_property(Relationship,Property,Graph), 
    range(Property,Domain,Graph).

pseudo_range(Relationship,Range,Graph) :-
    relationship_target_property(Relationship,Property,Graph), 
    range(Property,Range,Graph).

/** 
 * relationship_to_pseudoedges_and_classes(+Relationship:uri,+Graph:graph,-PseudoEdge:pseudoEdge,Classes:list(uri)) is det.
 * 
 * Calculate the pseudoedge and classes associated with a given relationship class
 */
relationship_to_pseudoedges_and_classes(Relationship,Graph,
                                        pseudoEdge(SourceP,Relationship,TargetP),
                                        [Domain,Range]) :-
    % We really only want one solution.
    once(
        (   relationship_source_property(Relationship,SourceP,Graph),
            relationship_target_property(Relationship,TargetP,Graph),
            pseudo_domain(Relationship,Domain,Graph),
            pseudo_range(Relationship,Range,Graph)
        )
    ).

    
