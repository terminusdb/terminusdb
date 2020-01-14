:- module(schema_definitions,
          [schema_predicate/1,
           rdfs_properties/1,
           owl_basic_properties/1,
           owl_list_properties/1,
           rdfs_types/1,
           owl_types/1,
           metadata_predicates/1,
           util_predicates/1]).

/** <module> Schema Definitions
 *
 * Those elements of the OWL ontology which will be rewriten as internalised
 * predicates.
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

rdfs_properties([range,
                 domain,
                 subClassOf,
                 subPropertyOf,
                 label,
                 comment,
                 seeAlso,
                 isDefinedBy]).

owl_basic_properties([allValuesFrom,
                      backwardCompatibleWith,
                      complementOf,
                      differentFrom,
                      disjointWith,
                      equivalentClass,
                      equivalentProperty,
                      hasValue,
                      imports,
                      incompatibleWith,
                      inverseOf,
                      maxCardinality,
                      minCardinality,
                      cardinality,
                      qualifiedCardinality,
                      maxQualifiedCardinality,
                      minQualifiedCardinality,
                      onProperty,
                      priorVersion,
                      sameAs,
                      someValuesFrom,
                      versionInfo,
                      hasSelf,
                      datatypeComplementOf,
                      withRestrictions,
                      propertyDisjointWith,
                      sourceIndividual,
                      assertionProperty,
                      targetIndividual,
                      targetValue,
                      annotatedSource,
                      annotatedProperty,
                      annotatedtarget
                     ]).

owl_list_properties([distinctMembers,
                     intersectionOf,
                     oneOf,
                     unionOf,
                     disjointUnionOf,
                     members,
                     propertyChainAxiom,
                     hasKey]).

rdfs_types([datatype]).

owl_types([allDifferent,
           annotationProperty,
           class,
           dataRange,
           dataTypeProperty,
           deprecatedClass,
           deprecatedProperty,
           functionalProperty,
           inverseFunctionalProperty,
           nothing,
           objectProperty,
           ontology,
           ontologyProperty,
           restriction,
           thing,
           allDisjointClasses,
           allDisjointProperties,
           reflexiveProperty,
           irreflexiveProperty,
           symmetricProperty,
           asymmetricProperty,
           transitiveProperty,
           namedIndividual]).

metadata_predicates([schema/2]).
calculated_predicates([subsumption_of/2]).
util_predicates([document/1]).

schema_predicate(Predicate) :-
    (   rdfs_types(Rdfs_Types),
        member(Rdfs_Type, Rdfs_Types),
        Predicate = Rdfs_Type/1
    ;   owl_types(Owl_Predicates),
        member(Owl_Type, Owl_Predicates),
        Predicate = Owl_Type/1
    ;   rdfs_properties(Rdfs_Properties),
        member(Rdfs_Property, Rdfs_Properties),
        Predicate = Rdfs_Property/2
    ;   owl_basic_properties(Owl_Properties),
        member(Owl_Property, Owl_Properties),
        Predicate = Owl_Property/2
    ;   owl_list_properties(Owl_List_Properties),
        member(Owl_List_Property, Owl_List_Properties),
        Predicate = Owl_List_Property/2
    ;   metadata_predicates(Predicates),
        member(Predicate, Predicates)
    ;   calculated_predicates(Predicates),
        member(Predicate, Predicates)
    ;   util_predicates(Predicates),
        member(Predicate, Predicates)).
