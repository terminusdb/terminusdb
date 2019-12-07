:- module(validate, [
              turtle_schema_transaction/4,
              document_transaction/5
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

:- use_module(triplestore).
:- use_module(database).
:- use_module(utils).
:- use_module(schema,[
                  database_module/2,
                  compile_schema_to_module/2
              ]).
:- use_module(validate_schema).
:- use_module(validate_instance).
:- use_module(frame).
:- use_module(jsonld).
:- use_module(library(semweb/turtle)).

:- use_module(literals).

% For debugging
:- use_module(library(http/http_log)).

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
test_schema(invalid_RDFS_property_SC).

/*
 *  schema_transaction ...
 *
 * We need a schema transaction now - that allows for both schema and instance updates
 * in a single transaction
 *
 */

/*
 * turtle_schema_transaction(+Database,-Database,+Schema,+New_Schema_Stream, Witnesses) is det.
 *
 * Updates a schema using a turtle formatted stream.
 */
turtle_schema_transaction(Database, Schema, New_Schema_Stream, Witnesses) :-

    % make a fresh empty graph against which to diff
    open_memory_store(Store),
    open_write(Store, Builder),

    % write to a temporary builder.
    rdf_process_turtle(
        New_Schema_Stream,
        {Builder}/
        [Triples,_Resource]>>(
            forall(member(T, Triples),
                   (   normalise_triple(T, rdf(X,P,Y)),
                       object_storage(Y,S),
                       nb_add_triple(Builder, X, P, S)
                   ))),
        [encoding(utf8)]),
    % commit this builder to a temporary layer to perform a diff.
    nb_commit(Builder,Layer),

    with_transaction(
        [transaction_record{
             pre_database: Database,
             write_graphs: [Schema],
             update_database: Update_DB,
             post_database: Post_DB},
         witnesses(Witnesses)],
        % Update
        validate:(
            % first write everything into the layer-builder that is in the new
            % file, but not in the db.
            forall(
                (   xrdf(Update_DB,[Schema], A_Old, B_Old, C_Old),
                    \+ xrdf_db(Layer,A_Old,B_Old,C_Old)),
                delete(Update_DB,Schema,A_Old,B_Old,C_Old)
            ),
            forall(
                (   xrdf_db(Layer,A_New,B_New,C_New),
                    \+ xrdf(Update_DB,[Schema], A_New, B_New, C_New)),
                insert(Update_DB,Schema,A_New,B_New,C_New)
            )
        ),
        % Post conditions
        validate:(
            findall(Pre_Witness,
                    (   pre_test_schema(Pre_Check),
                        call(Pre_Check,Post_DB,Pre_Witness)),
                    Pre_Witnesses),
            (   \+ Pre_Witnesses = []
            % We have witnesses of failure and must bail
            ->  Witnesses = Pre_Witnesses
            % We survived the pre_tests, better check schema constriants
            ;   findall(Schema_Witness,
                        (   test_schema(Check),
                            call(Check,Post_DB,Schema_Witness)),
                        Schema_Witnesses),
                (   \+ Schema_Witnesses = []
                ->  Witnesses = Schema_Witnesses
                    % Winning!
                ;   % TODO: We do not need to perform a global check of instances
                    % Better would be a local check derived from schema delta.
                    findall(Witness,
                            (   database_instance(Post_DB, Instance),
                                xrdf(Post_DB,Instance,E,F,G),
                                refute_insertion(Post_DB,E,F,G,Witness)),
                            Witnesses)

                )
            ),
            % if we got here, I think this might be safe.
            database_module(Database, Module),
            compile_schema_to_module(Post_DB, Module)
        )
    ).

/*
 * instance_schema_transaction(+Database,-Update_DB,+Write_Graphs,+Goal,-Witnesses) is det.
 *
 * Instance and schema simultaneous transaction updates.
 */
instance_schema_transaction(Database, Update_DB, Write_Graphs, Goal, Witnesses) :-
    with_transaction(
        [transaction_record{
             pre_database: Database,
             write_graphs: Write_Graphs,
             update_database: Update_DB,
             post_database: Post_DB},
         witnesses(Witnesses)],
        % Update
        Goal,
        % Post conditions
        validate:(
            findall(Pre_Witness,
                    (   pre_test_schema(Pre_Check),
                        call(Pre_Check,Post_DB,Pre_Witness)),
                    Pre_Witnesses),
            (   \+ Pre_Witnesses = []
            % We have witnesses of failure and must bail
            ->  Witnesses = Pre_Witnesses
            % We survived the pre_tests, better check schema constriants
            ;   findall(Schema_Witness,
                        (   test_schema(Check),
                            call(Check,Post_DB,Schema_Witness)),
                        Schema_Witnesses),
                (   \+ Schema_Witnesses = []
                ->  Witnesses = Schema_Witnesses
                    % Winning!
                ;   % TODO: We do not need to perform a global check of instances
                    % Better would be a local check derived from schema delta.
                    findall(Witness,
                            (   database_instance(Post_DB, Instance),
                                xrdf(Post_DB,Instance,E,F,G),
                                refute_insertion(Post_DB,E,F,G,Witness)),
                            Witnesses)

                )
            ),
            % if we got here, I think this might be safe.
            database_module(Database, Module),
            compile_schema_to_module(Post_DB, Module)
        )
    ).

/*
 * document_transaction(Database:database, Transaction_Database:database, Graph:graph_identifier,
 *                      Document:json_ld, Witnesses:json_ld) is det.
 *
 * Update the database with a document, or fail with a constraint witness.
 *
 */
document_transaction(Database, Update_Database, Graph, Goal, Witnesses) :-
    with_transaction(
        [transaction_record{
             pre_database: Database,
             update_database: Update_Database,
             post_database: Post_Database,
             write_graphs: [Graph]},
         witnesses(Witnesses)],
        Goal,
        validate:(   findall(Pos_Witness,
                             (
                                 triplestore:xrdf_added(Post_Database, Graph, X, P, Y),
                                 refute_insertion(Post_Database, X, P, Y, Pos_Witness)
                             ),
                             Pos_Witnesses),

                     findall(Neg_Witness,
                             (
                                 triplestore:xrdf_deleted(Post_Database, Graph, X, P, Y),
                                 refute_deletion(Post_Database, X, P, Y, Neg_Witness)
                             ),
                             Neg_Witnesses),

                     append(Pos_Witnesses, Neg_Witnesses, Witnesses)
                 )
    ).


/*
 * instance_transaction(Database:database, Transaction_Database:database, Write_Graphs:list,
 *                      Document:json_ld, Witnesses:json_ld) is det.
 *
 * Update of instance alone
 *
 */
instance_transaction(Database, Update_Database, Write_Graphs, Goal, Witnesses) :-
    with_transaction(
        [transaction_record{
             pre_database: Database,
             update_database: Update_Database,
             post_database: Post_Database,
             write_graphs: Write_Graphs},
         witnesses(Witnesses)],
        Goal,
        validate:(
            findall(Graph_Witnesses,
                    (   member(Graph,Write_Graphs),
                        findall(Pos_Witness,
                                (
                                    triplestore:xrdf_added(Post_Database, Graph, X, P, Y),
                                    refute_insertion(Post_Database, X, P, Y, Pos_Witness)
                                ),
                                Pos_Witnesses),

                        findall(Neg_Witness,
                                (
                                    triplestore:xrdf_deleted(Post_Database, Graph, X, P, Y),
                                    refute_deletion(Post_Database, X, P, Y, Neg_Witness)
                                ),
                                Neg_Witnesses),

                        append(Pos_Witnesses, Neg_Witnesses, Graph_Witnesses)
                    ),
                    Witnesses_List),
            append(Witnesses_List,Witnesses)
        )
    ).
