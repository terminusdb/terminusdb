:- module(database,[
          ]).

/** <module> Implementation of database graph management
 *
 * This module helps other modules with the representation of databases and
 * their associated graphs by bundling them as objects with some convenience
 * operators and accessors.
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
:- use_module(types).
:- use_module(sdk).
:- use_module(file_utils).
:- use_module(triplestore).

:- use_module(library(prolog_stack)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

:- op(2, xfx, ^^).

/*
 * Types:
 *
 * graph_descriptor --> named_graph{ name : atom }
 *                    | ref_graph{ layer_id : atom }
 *
 * A named_graph refers to a file in the store - this has to be made unique so we don't get
 * collisions between different databases. Currently only used for the terminus and database graph
 * - one per repository. This uses the file label mechanism.
 *
 * A ref_graph is a layer id that can be resolved to a graph.
 *
 * collection_descriptor --> terminus_descriptor
 *                         | database_descriptor{ database_name : atom,
 *                                                instance : list(graph_descriptor)}
 *                         | repository_descriptor{ database_descriptor : database_descriptor,
 *                                                  repository_layer : layer,
 *                                                  repository_name : atom,
 *                                                  instance : list(graph_descriptor)}
 *                         | ref_descriptor{ repository_descriptor: repository_descriptor,
 *                                           ref_layer : layer,
 *                                           ref_name : atom,
 *                                           schema : list(graph_descriptor)
 *                                           instance : list(graph_descriptor)
 *                                           inference : list(graph_descriptor)}
 *
 * terminus_descriptor: refers to the core database with user and database management.
 * This database refers to the various database descriptors which can be opened "by name"
 * using the label mechanism.
 *
 * database_descriptor: is a per database graph collections that store information about all
 * local and remotes which exist
 *
 * repository descriptors: is used to refer to the branchs that exist in a given repository.
 *
 * branch_descriptors: refers to the precise graph collection which is a 'normal' database
 * object used in WOQL.
 *
 * Transaction management requires that we advance a file system label associated with the
 * repository descriptor when we have performed a full transaction. All other multi-graph
 * management is done within the graphs associated with the repository descriptor.
 *
 * The type polymorphism is designed to allow WOQL read transactions to work identically over
 * all of these cases. However, *write* transactions have to be done differently on each of
 * these by case, though triple writes can be done identically.
 *
 * read_obj ---> read_obj{ descriptor : graph_descriptor, read : Layer}
 * write_obj ---> write_obj{ descriptor : graph_descriptor, write : Layer_Builder}
 * query_object ---> query_object{ collection_descriptor : collection_descriptor,
 *                                 instance_read_objects : list(read_obj),
 *                                 schema_read_objects : list(read_obj),
 *                                 inference_read_objects : list(read_obj),
 *                                 instance_write_objects : list(write_obj),
 *                                 schema_write_objects : list(read_obj),
 *                                 inference_write_objects : list(read_obj) }
 *
 * transaction_object ---> transaction_object{ collection_descriptors : list(collection_descriptor),
 *                                             query_objs : list(query_objs),
 *                                             post_objs : list(query_objs) }
 */


/*
 * terminus_repository_schema(Schema) is det.
 *
 * Returns the name of the repository schema
 */
terminus_repository_schema(Schema) :-
    db_path(Path),
    www_form_encode('terminus-repository-schema',Safe_GID),
    interpolate([Path,Safe_GID,'.label'],Label).

/**
 * open_read_obj(Graph_Descriptor : graph_descriptor, Read_Object : read_object) is det.
 *
 * opens a graph descriptor with a querable read object.
 */
open_read_obj(Graph, read_obj{ descriptor : Graph, read : Layer }) :-
    Graph = named_graph{ name : Name },
    storage(Store),
    safe_open_named_graph(Store,Name,Obj),
    head(Obj,Layer).
open_read_obj(Graph, read_obj{ descriptor : Graph, read : Layer }) :-
    Graph = ref_graph{ layer_id : Layer_ID },
    storage(Store),
    store_id_layer(Store,Layer_ID,Layer).

/**
 * open_write_obj(Graph_Descriptor : graph_descriptor, Write_Object : write_object) is det.
 *
 * opens a graph descriptor with a writer builder.
 */
open_write_obj(Graph, write_obj{ descriptor : Graph, write : Layer_Builder }) :-
    Graph = named_graph{ name : Name },
    storage(Store),
    safe_open_named_graph(Store,Name,Obj),
    open_write(Obj,Layer_Builder).
open_write_obj(Graph, write_obj{ descriptor : Graph, write : Layer_Builder }) :-
    Graph = ref_graph{ layer_id : Layer_ID },
    storage(Store),
    store_id_layer(Store,Layer_ID,Layer),
    open_write(Layer,Layer_Builder).

included_read_objects(Graph_Descriptors, Read_Graph_Descriptors, Read_Objects) :-
    include({Read_Graph_Descriptors}/[X]>>memberchk(X,Read_Graph_Descriptors),
            Graph_Descriptors,
            Read_Descriptors),
    maplist([Descriptor,Read_Obj]>>open_read_obj(Descriptor,Read_Obj),
            Read_Descriptors, Read_Objects).

included_write_objects(Graph_Desriptors, Read_Graph_Descriptors, Read_Objects) :-
    include({Read_Graph_Descriptors}/[X]>>memberchk(X,Read_Graph_Descriptors),
            Graph_Descriptors,
            Read_Descriptors),
    maplist([Descriptor,Read_Obj]>>open_write_obj(Descriptor,Read_Obj),
            Read_Descriptors, Read_Objects).

/**
 * descriptor_query(Descriptor, Write_Mask, Read_Mask, Query_Obj) is det.
 *
 * Impements the logic which allows us to special-case the
 * opening of descriptors for repository, branch and graph.
 *
 * @Descriptor has type repository{ ... } | branch{ ... }
 * @Write_Mask has type list(descriptor)
 *             This opens read queries. No mask means everything is opened.
 */
descriptor_query(Collection_Descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors, Query_Obj) :-
    Collection_Descriptor = database_descriptor{
                                database_name : DB_Name,
                                instance : IL
                            },
    !,
    layer_ontology(Layer_Ontology_Name),
    Layer_Ontology_Graph = named_graph{ name : Layer_Ontology_Name },
    repository_ontology(Repository_Ontology_Name),
    Repository_Ontology_Graph = named_graph{ name : Repository_Ontology_Name },

    % Get hard coded layer and repository schema read/write objects
    Schema_Graphs = [Repository_Ontology_Graph, Layer_Ontology_Graph],
    include_read_objects(Schema_Graphs,Read_Graph_Descriptors, Read_Schema_Objects),
    include_write_objects(Schema_Graphs,Write_Graph_Descriptors, Write_Schema_Objects),

    % Get instance read/write objects
    include_read_objects(IL,Read_Graph_Descriptors,Read_Instance_Objects),
    include_write_objects(IL,Write_Graph_Descriptors, Write_Instance_Objects),

    Query_Obj = query_obj{
                    collection_descriptor : Collection_Descriptor,
                    instance_read_objects : Instance_Read_Objects,
                    schema_read_objects : Schema_Read_Objects,
                    inference_read_objects : [],
                    instance_write_objects : Write_Instance_Objects,
                    inference_write_objects : [],
                    schema_write_objects : Schema_Write_Objects
                }.
descriptor_query(Repository, Read_Mask, Write_Mask, Query_Obj) :-
    Collection_Descriptor = repository_descriptor{
                                database_descriptor : _DB,
                                repository_layer : _Layer,
                                repository_name : _Name,
                                instance : IL
                            },
    !,
    layer_ontology(Layer_Ontology_Name),
    Layer_Ontology_Graph = named_graph{ name : Layer_Ontology_Name },
    ref_ontology(Ref_Ontology_Name),
    Repository_Ontology_Graph = named_graph{ name : Repository_Ontology_Name },

    % Get hard coded layer and repository schema read/write objects
    Schema_Graphs = [Ref_Ontology_Graph, Layer_Ontology_Graph],
    include_read_objects(Schema_Graphs,Read_Graph_Descriptors, Read_Schema_Objects),
    include_write_objects(Schema_Graphs,Write_Graph_Descriptors, Write_Schema_Objects),

    % Get instance read/write objects
    include_read_objects(IL,Read_Graph_Descriptors,Read_Instance_Objects),
    include_write_objects(IL,Write_Graph_Descriptors, Write_Instance_Objects),

    Query_Obj = query_obj{
                    collection_descriptor : Collection_Descriptor,
                    instance_read_objects : Read_Instance_Objects,
                    schema_read_objects : Read_Schema_Objects,
                    inference_read_objects : [],
                    instance_write_objects : Write_Instance_Objects,
                    schema_write_objects : Write_Schema_Objects,
                    inference_write_objects : []
                }.
descriptor_query(Branch, Read_Mask, Write_Mask, Query_Obj) :-
    Branch = ref_descriptor{ database_name : DB_Name,
                             repository_name : Repository_Name,
                             branch_name : Branch_Name,
                             schema : Schema_List,
                             inference : Inf_List,
                             instance : Instance_List },
    !,
    true.

update_read_objects([], New_Read_Objects, New_Read_Objects).
update_read_objects([Old_Read_Object|Old_Read_Objects], New_Read_Objects, Result) :-
    memberchk(read_obj{ descriptor: Old_Read_Object.descriptor, read: _}, New_Read_Objects),
    !,
    update_read_objects(Old_Read_Objects, New_Read_Objects, Result).
update_read_objects([Old_Read_Object|Old_Read_Objects], New_Read_Objects, [Old_Read_Object|Results]) :-
    update_read_objects(Old_Read_Objects, New_Read_Objects, Result).

commit_write_object(write_obj{
                       descriptor: Graph_Descriptor,
                       write: Layer_Builder
                   },
                    read_obj {
                        descriptor: Graph_Descriptor,
                        read: Layer
                    }) :-
    terminus_store:nb_commit(Layer_Builder, Layer).

commit_query_object(query_object{
                        collection_descriptor: Collection_Descriptor,
                        instance_read_objects: Instance_Read_Objects,
                        inference_read_objects: Inference_Read_Objects,
                        schema_read_objects: Schema_Read_Objects,
                        instance_write_objects: Instance_Write_Objects,
                        inference_write_objects: Inference_Write_Objects,
                        schema_write_objects: Schema_Write_Objects
                    },
                    query_object {
                        collection_descriptor: Collection_Descriptor,
                        instance_read_objects: New_Instance_Read_Objects,
                        inference_read_objects: New_Inference_Read_Objects,
                        schema_read_objects: New_Schema_Read_Objects,
                        instance_write_objects: [],
                        inference_write_objects: [],
                        schema_write_objects: []
                        }) :-
    maplist(commit_write_object,
            Instance_Write_Objects,
            Committed_Instance_Objects),
    maplist(commit_write_object,
            Schema_Write_Objects,
            Committed_Schema_Objects),
    maplist(commit_write_object,
            Inference_Write_Objects,
            Committed_Inference_Objects),

    update_read_objects(Instance_Read_Objects, Committed_Instance_Objects, New_Instance_Read_Objects),
    update_read_objects(Schema_Read_Objects, Committed_Schema_Objects, New_Schema_Read_Objects),
    update_read_objects(Inference_Read_Objects, Committed_Inference_Objects, New_Inference_Read_Objects).

die(Message) :-
    throw(not_acceptable(_{'terminus:status': 'terminus:error',
                           'terminus:message': Message
                          })).

:- meta_predicate call_or_die(:, +).
call_or_die(Call, Message) :-
    (   call(Call)
    ->  true
    ;   die(Message)).

/**
 * with_transaction(+Pre_Descriptors, +Read_Descriptors, +Write_Descriptors, :Query_Update, :Post, -Witnesses) is semidet.
 */
:- meta_predicate with_transaction(+,+,+,+,+,:,:,-).
with_transaction(Pre_Descriptors,
                 Read_Descriptors,
                 Write_Descriptors,
                 Update_Query_Objects,
                 Post_Query_Objects,
                 Query_Update,
                 Post,
                 Witnesses) :-
    % turn descriptors into query objects
    between(1,5,_),
    % this guy has to be responsible for opening writers in a reasonable way, so that we don't get multiple versions of a layer builder for a repo graph.
    maplist({Read_Descriptors, Write_Descriptors}/[Pre_Descriptor, Query_Object]>>(descriptor_query(Pre_Descriptor, Read_Descriptors, Write_Descriptors, Query_Object)),
            Pre_Descriptors,
            Update_Query_Objects),
    % call update_query which will use those query objects
    call_or_die(Query_Update, 'unable to perform the query'),

    % turn builders into layers resulting in post query objects
    maplist(commit_query_object,
            Update_Query_Objects,
            Post_Query_Objects),

    % call post with post query objects for final check
    call_or_die(Post, 'post condition failed'),

    % set heads (magic!)
    set_heads(Post_Query_Objects),
    !.
with_transaction(Pre_Descriptors,
                 Read_Descriptors,
                 Write_Descriptors,
                 Update_Query_Objects,
                 Post_Query_Objects,
                 Query_Update,
                 Post,
                 Witnesses) :-
    die('too many transaction retries, dying.').

descriptor_compare('=', Left, Right) :-
    Left.descriptor = Right.descriptor,
    !.
descriptor_compare('<', Left, Right) :-
    Left @< Right,
    !.
descriptor_compare('>', _Left, _Right).

collect_query_objects(Query_Objects,
                      Ref_Query_Objects,
                      Repo_Query_Objects,
                      Label_Query_Objects) :-
    include([Query_Object]>>(ref_descriptor{} :< Query_Object.collection_descriptor),
            Query_Objects,
            Ref_Query_Objects),

    include([Query_Object]>>(repository_descriptor{} :< Query_Object.collection_descriptor),
            Query_Objects,
            Toplevel_Repo_Query_Objects),

    maplist(


set_heads(Query_Objects) :-
    % split query objects by type (ref, repo, label)
    collect_query_objects(Query_Objects,
                          Ref_Query_Objects,
                          Repo_Query_Objects,
                          Label_Query_Objects),
    % set heads for all the refs
    maplist(set_ref_head, Ref_Query_Objects),
    maplist(set_repo_head, Repo_Query_Objects),
    maplist(set_label_head, Label_Query_Objects).
