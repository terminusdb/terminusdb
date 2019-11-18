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

:- use_module(utils).
:- use_module(validate_schema).
:- use_module(database).
:- use_module(triplestore).
:- use_module(library(semweb/rdf_db)).

relationship_source_property(Relationship,Property,Database) :-
    database_name(Database,Collection),
    database_schema(Database,Schema),
    xrdf(Collection,Schema,Relationship,tcs:source_property,Property).

relationship_target_property(Relationship,Property,Database) :-
    database_name(Database,Collection),
    database_schema(Database,Schema),
    xrdf(Collection,Schema,Relationship,tcs:target_property,Property).

pseudo_domain(Relationship,Domain,Database) :-
    relationship_source_property(Relationship,Property,Database),
    range(Property,Domain,Database).

pseudo_range(Relationship,Range,Database) :-
    relationship_target_property(Relationship,Property,Database),
    range(Property,Range,Database).

/**
 * relationship_to_pseudoedges_and_classes(+Relationship:uri,+Database:database-PseudoEdge:pseudoEdge,Classes:list(uri)) is det.
 *
 * Calculate the pseudoedge and classes associated with a given relationship class
 */
relationship_to_pseudoedges_and_classes(Relationship,Database,
                                        pseudoEdge(SourceP,Relationship,TargetP),
                                        [Domain,Range]) :-
    % We really only want one solution.
    once(
        (   relationship_source_property(Relationship,SourceP,Database),
            relationship_target_property(Relationship,TargetP,Database),
            pseudo_domain(Relationship,Domain,Database),
            pseudo_range(Relationship,Range,Database)
        )
    ).
