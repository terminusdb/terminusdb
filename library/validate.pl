:- module(validate, [
              schema_transaction/4,
              document_transaction/4
          ]).

/** <module> Validation
 * 
 * Implements schema and instance validation
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

:- use_module(library(triplestore)).
:- use_module(library(database)).
:- use_module(library(journaling)).
:- use_module(library(utils)).
:- use_module(library(validate_schema)).
:- use_module(library(validate_instance)).
:- use_module(library(frame)).
:- use_module(library(jsonld)).
:- use_module(library(semweb/turtle)).

% Required for consistency
pre_test_schema(classCycleSC).
pre_test_schema(propertyCycleSC).

/** 
 * test_schema(-Pred:atom) is nondet.
 * 
 * This predicate gives each available schema constraint.
 */
% Restrictions on schemas 
test_schema(noImmediateClassSC).
test_schema(noImmediateDomainSC).
test_schema(noImmediateRangeSC).
%test_schema(notUniqueClassLabelSC).
%test_schema(notUniqueClassSC).
%test_schema(notUniquePropertySC). % still useful with annotationOverloadSC?
%test_schema(schemaBlankNodeSC). % should never be used.
test_schema(annotationOverloadSC).
% OWL DL constraints
test_schema(orphanClassSC).
test_schema(orphanPropertySC). 
test_schema(invalidRangeSC). 
test_schema(invalidDomainSC).
test_schema(domainNotSubsumedSC).
test_schema(rangeNotSubsumedSC).
test_schema(propertyTypeOverloadSC).

schema_transaction(Database, Schema, New_Schema_Stream, Witnesses) :-
    database_name(Database,Database_Name),
    with_transaction(
        [collection(Database_Name),
         graphs([Schema]),
         success(Success_Flag)],
        validate:(
            % deletes (everything)
            forall( xrdf(Database_Name, [Schema], A, B, C),
                    delete(Database_Name, Schema, A,B,C)),
            forall( xrdf(Database_Name, [Schema], A, B, C),
                    format('WTF? (~q,~q,~q)~n', [A,B,C])),
            %break,
            %database_module(Database, Module),
            %cleanup_schema_module(Module),
            % inserts (everything)
            rdf_process_turtle(New_Schema_Stream,
                               {Database_Name, Schema}/
                               [Triples,_Resource]>>(
                                   forall(member(rdf(X,P,Y), Triples),
                                          (   (   X = node(N)
                                              ->  interpolate(['_:',N], XF)
                                              ;   X = XF),
                                              (   Y = node(M)
                                              ->  interpolate(['_:',M], YF)
                                              ;   Y = literal(L),
                                                  atom(L)
                                              ->  YF = literal(lang(en,L))
                                              ;   Y = YF),
                                              %nl,nl,writeq(rdf(X,P,Y)),nl,nl,
                                              insert(Database_Name,Schema,XF,P,YF)
                                          )
                                         )
                               ), []),

            % First, Check pre tests. 
            findall(Pre_Witness,
                    (   pre_test_schema(Pre_Check),
                        call(Pre_Check,Database,Pre_Witness)),
                    Pre_Witnesses),
            (   \+ Pre_Witnesses = []
            % We have witnesses of failure and must bail
            ->  Success_Flag = false,
                Witnesses = Pre_Witnesses
            % We survived the pre_tests, better check schema constriants
            ;   findall(Schema_Witness,
                        (   test_schema(Check),
                            call(Check,Database,Schema_Witness)),
                        Schema_Witnesses),
                (   \+ Schema_Witnesses = []
                ->  Success_Flag = false,
                    Witnesses = Schema_Witnesses
                    % Winning!
                ;   Success_Flag = true,
                    Witnesses = []
                )
            )
        )
    ).

/* 
 * document_update(Database:database, Graph:graph_identifier, 
 *                 Document:json_ld, Witnesses:json_ld) is det.
 * 
 * Update the database with a document, or fail with a constraint witness. 
 * 
 */
document_transaction(Database, Graph, Goal, Witnesses) :-
    database_name(Database,Database_Name),
    with_transaction(
        [collection(Database_Name),
         graphs([Graph]),
         success(Success_Flag)],
        validate:(
            call(Goal),
            findall(Pos_Witness,
                    (
                        triplestore:xrdf_pos_trans(Database_Name,Graph, X, P, Y),
                        refute_insertion(Database, X, P, Y, Pos_Witness)
                    ),
                    Pos_Witnesses),
            
            findall(Neg_Witness,
                    (   
                        triplestore:xrdf_neg_trans(Database_Name,Graph, X, P, Y),
                        refute_deletion(Database, X, P, Y, Neg_Witness)
                    ),
                    Neg_Witnesses),
            
            append(Pos_Witnesses, Neg_Witnesses, Witnesses),
            
            (   Witnesses = []
            ->  Success_Flag = true
            ;   Success_Flag = false
            )
        )
    ).
