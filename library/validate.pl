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

/*
 * graph_validation_obj { descriptor: graph_descriptor, read: layer, changed: bool }
 *
 * validation_obj { descriptor: collection_descriptor,
 *                  <parent>: transaction_obj % we still keep it a transaction obj and transform it later
 *                  instance_objects: list(graph_validation_obj)
 *                  schema_objects: list(graph_validation_obj)
 *                  inference_objects: list(graph_validation_obj)
 */

read_write_obj_to_graph_validation_obj(Read_Write_Obj, Graph_Validation_Obj, Map, Map) :-
    memberchk(Read_Write_Obj=Graph_Validation_Obj, Map),
    !.
read_write_obj_to_graph_validation_obj(Read_Write_Obj, Graph_Validation_Obj, Map, [Read_Write_Obj=Graph_Validation_Obj|Map]) :-
    Read_Write_Obj = read_write_obj{ descriptor: Descriptor,
                                     read: Layer,
                                     write: Layer_Builder },
    Graph_Validation_Obj = graph_validation_obj{ descriptor: Descriptor,
                                                 read: New_Layer,
                                                 changed: Changed },
    (   var(Layer_Builder)
    ->  New_Layer = Layer,
        Changed = false
    ;   nb_commit(Layer_Builder, New_Layer),
        Changed = true).

transaction_object_to_validation_object(Transaction_Object, Validation_Object, Map, New_Map) :-
    transaction_object{descriptor: Descriptor,
                       instance_objects: Instance_Objects,
                       schema_objects: Schema_Objects,
                       inference_objects: Inference_Objects} :< Transaction_Object,
    Intermediate_Validation_Object = validation_object{
                                         descriptor: Descriptor,
                                         instance_objects: Validation_Instance_Objects,
                                         schema_objects: Validation_Schema_Objects,
                                         inference_objects: Validation_Inference_Objects
                                     },
    (   Parent = Transaction_Object.get(parent),
    ->  transaction_object_to_validation_object(Parent, Parent_Validation_Object, Map, Map_1),
        Intermediate_Validation_Object_1 = Intermediate_Validation_Object.put(parent, Parent)
    ;   Intermediate_Validation_Object_1 = Intermediate_Validation_Object
    ),
    mapm([Object,Validation_Object]>>read_write_obj_to_graph_validation_obj(Object, Validation_Object),
         Instance_Objects,
         Validation_Instance_Objects,
         Map_1,
         Map_2),
    mapm([Object,Validation_Object]>>read_write_obj_to_graph_validation_obj(Object, Validation_Object),
         Schema_Objects,
         Validation_Schema_Objects,
         Map_2,
         Map_3),
    mapm([Object,Validation_Object]>>read_write_obj_to_graph_validation_obj(Object, Validation_Object),
         Inference_Objects,
         Validation_Inference_Objects,
         Map_3,
         New_Map),
    (   Commit_Info = Transaction_Object.get(commit_info)
    ->  Validation_Object = Intermediate_Validation_Object_1.put(commit_info, Commit_Info)
    ;   Validation_Object = Intermediate_Validation_Object_1).


commit_validation_object(Validation_Object) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object]
    } :< Validation_Object,
    terminus_descriptor{
    } = Descriptor,
    !,
    terminus_instance_name(Label),
    % This is okay for now, since we don't want the
    % schema to be changed in WOQL queries
    (   Instance_Object.changed = true
    ->  storage(Store),
        safe_open_named_graph(Store, Label, Graph),
        nb_set_head(Graph, Instance_Object.read)
    ;   true).
commit_validation_object(Validation_Object) :-
    validation_object{
        descriptor: Descriptor
        instance_objects: [Instance_Object]
    } :< Validation_Object,
   database_descriptor{
        database_name: Database_Name
    } = Descriptor,
    !,
    % super simple case, we just need to set head
    % That is, we don't need to check the schema
    (   Instance_Object.changed = true
    ->  storage(Store),
        % The label is the same as the same as the database_name
        % therefore we can just open it
        safe_open_named_graph(Store, Database_Name, Graph),
        nb_set_head(Graph, Instance_Object.read)
    ;   true).
commit_validation_object(Validation_Object) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object],
        parent: Parent_Transaction
    } :< Validation_Object,
    repository_descriptor{
        database_descriptor: Descriptor,
        repository_name: Repo_Name
    } = Descriptor,
    layer_to_id(Instance_Object.read, Layer_ID),
    (   Instance_Object.changed = true
    ->  once(ask(Parent_Transaction,
                 (   t(URI, repository:repository_name, Repo_Name^^xsd:string),
                     t(URI, repository:repository_head, ExistingRepositoryHead),
                     t(ExistingRepositoryHead, layer:layer_id, LayerIdOfHeadToRemove^^xsd:string),
                     delete(ExistingRepositoryHead, layer:layer_id, LayerIdOfHeadToRemove^^xsd:string),
                     delete(URI, repository:repository_head, ExistingRepositoryHead),
                     idgen(layer:'ShadowLayer', [Layer_ID], NewShadowLayerID),
                     insert(NewShadowLayerID, rdf:type, layer:'ShadowLayer'),
                     insert(NewShadowLayerID, layer:shadow_layer, Layer_ID^^xsd:string)
                 )
                ))
    ;   true).
commit_validation_object(Validation_Object) :-
    validation_object{
        parent : Parent_Transaction,
        descriptor: Descriptor,
        instance_objects: Instance_Objects,
        schema_objects: Schema_Objects,
        inference_objects: Inference_Objects
    } :< Validation_Object,

    branch_descriptor{ repository_descriptor: Repository_Descriptor,
                       branch_name : Branch},
    !,
    append([Instance_Objects,Schema_Objects,Inference_Objects],
           Union_Objects),

    atomic_list_concat(['http://terminushub.com/document/'],Branch_Base),
    % TODO: This is where it all went wrong.
    random_uri(ref:Branch_Name,'Commit',Commit_URI),
    (   exists([Obj]>>(Obj.changed = true), Union_Objects)
    ->  once(ask(Parent_Transaction,
                 (   t(Branch_URI, ref:branch_name, Branch_Name^^xsd:string),
                     idgen(ref:'Commit',[],
                 )
                ))
    ;   true
    ).
commit_validation_object(Validation_Object) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object]
    } :< Validation_Object,
    label_descriptor{
        label: Label
    } = Descriptor,
    !,
    % super simple case, we just need to set head
    % That is, assuming anything changed
    (   Instance_Object.changed = true
    ->  storage(Store),
        safe_open_named_graph(Store, Label, Graph),
        nb_set_head(Graph, Instance_Object.read)
    ;   true).



% Required for consistency
pre_test_schema(class_cycle_SC).
pre_test_schema(property_cycle_SC).

/**
 * test_schema(-Pred:atom) is nondet.
 *
 * This predicate gives each available schema constraint.
 */
% Restrictions on schemas
test_schema(no_immediate_class_SC).
test_schema(no_immediate_domain_SC).
test_schema(no_immediate_range_SC).
%test_schema(notUniqueClassLabelSC).
%test_schema(notUniqueClassSC).
%test_schema(notUniquePropertySC). % still useful with annotationOverloadSC?
%test_schema(schemaBlankNodeSC). % should never be used.
test_schema(annotation_overload_SC).
% OWL DL constraints
test_schema(orphan_class_SC).
test_schema(orphan_property_SC).
test_schema(invalid_range_SC).
test_schema(invalid_domain_SC).
test_schema(domain_not_subsumed_SC).
test_schema(range_not_subsumed_SC).
test_schema(property_type_overload_SC).
test_schema(invalid_RDFS_property_SC).

/*
 *  schema_transaction ...
 *
 * We need a schema transaction now - that allows for both schema and instance updates
 * in a single transaction
 *
 */

/*
 * schema_validation_skippable(+Update_DB, +Schema, +Layer)
 *
 * Checks whether the DB should be fully checked after a schema change.
 * Should be validated if triples are deleted in the schema or when a
 * constraint has been added.
 */
schema_validation_skippable(Update_DB, Schema, Layer) :-
    forall((xrdf([Schema], A_Old, B_Old, C_Old),
            \+ xrdf_db(Layer,A_Old,B_Old,C_Old)),
           schema_triple_deletion_no_check(A_Old, B_Old, C_Old)
    ),
    forall((xrdf_db(Layer,A_New,B_New,C_New),
             \+ xrdf([Schema], A_New, B_New, C_New)),
           schema_triple_addition_no_check(A_New, B_New, C_New)
    ).

schema_triple_addition_no_check(_, _, 'http://www.w3.org/2002/07/owl#Restriction'):-
    !,
    false.
schema_triple_addition_no_check(_, _, _):-
    !,
    true.

schema_triple_deletion_no_check(_, _, _):-
    false.


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
                (   xrdf([Schema], A_Old, B_Old, C_Old),
                    \+ xrdf_db(Layer,A_Old,B_Old,C_Old)),
                delete(Schema,A_Old,B_Old,C_Old)
            ),
            forall(
                (   xrdf_db(Layer,A_New,B_New,C_New),
                    \+ xrdf([Schema], A_New, B_New, C_New)),
                insert(Schema,A_New,B_New,C_New)
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
                    (    schema_validation_skippable(Update_DB, Schema, Layer)
                    ->   true
                    ;    findall(Witness,
                              (   database_instance(Post_DB, Instance),
                                  xrdf(Instance,E,F,G),
                                  refute_insertion(Post_DB,E,F,G,Witness)),
                              Witnesses)
                    )

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
                                xrdf(Instance,E,F,G),
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
