:- module(database,[
              with_transaction/8,
              op(2, xfx, ^^),
              op(2, xfx, @)
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
:- use_module(file_utils).
:- use_module(triplestore).
:- use_module(global_prefixes, [global_prefix_expand/2]).
:- use_module(literals, [object_storage/2]).
:- use_module(casting, [idgen/3]).
:- use_module(library(terminus_store)).

:- use_module(library(prolog_stack)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(terminus_bootstrap)).

:- use_module(descriptor).

/*
 * terminus_repository_schema(Schema) is det.
 *
 * Returns the name of the repository schema
 */
terminus_repository_schema(Schema) :-
    db_path(Path),
    www_form_encode('terminus-repository-schema',Safe_GID),
    interpolate([Path,Safe_GID,'.label'],Schema).

commit_write_object(write_obj{
                       descriptor: Graph_Descriptor,
                       write: Layer_Builder
                   },
                    read_obj{
                        descriptor: Graph_Descriptor,
                        read: Layer
                    }) :-
    terminus_store:nb_commit(Layer_Builder, Layer).

/**
 * commit_query_object(Query_Object, Commited_Query_Object) is det.
 *
 * Changes associated write objects to new read objects in the Query_Object.
 */
commit_query_object(Query_Object,New_Query_Object) :-
    query_object{
        instance_read_objects: Instance_Read_Objects,
        inference_read_objects: Inference_Read_Objects,
        schema_read_objects: Schema_Read_Objects,
        instance_write_objects: Instance_Write_Objects,
        inference_write_objects: Inference_Write_Objects,
        schema_write_objects: Schema_Write_Objects
    } :< Query_Object,

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
    update_read_objects(Inference_Read_Objects, Committed_Inference_Objects, New_Inference_Read_Objects),

    New_Query_Object = Query_Object.put(_{instance_read_objects: New_Instance_Read_Objects,
                                          inference_read_objects: New_Inference_Read_Objects,
                                          schema_read_objects: New_Schema_Read_Objects,
                                          instance_write_objects: [],
                                          inference_write_objects: [],
                                          schema_write_objects: []}).

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
 * open_descriptor_queries_uniquely(Descriptors, Read_Descriptors, Write_Descriptors, Query_Objects, Map) is det.
 *
 * Only creates one object per descriptor - actually a fold.
 */
open_descriptor_queries_uniquely([], _Read_Descriptors, _Write_Descriptors, [], _Map).
open_descriptor_queries_uniquely([Descriptor|Descriptors], Read_Descriptors, Write_Descriptors,
                                 Map, New_Map) :-
    descriptor_query(Descriptor, Read_Descriptors, Write_Descriptors,
                     Map, Middle_Map),
    % difference list anyone?
    open_descriptor_queries_uniquely(Descriptors, Read_Descriptors,
                                     Write_Descriptors,
                                     Middle_Map, New_Map).

key_equal((=), X=_, X=_).
key_equal((<), X=_, Y=_) :-
    X @< Y.
key_equal((>), X=_, Y=_) :-
    X @> Y.

/**
 * open_descriptor_queries(Descriptors, Read_Descriptors, Write_Descriptors, Query_Objects) is det.
 *
 * open descriptors responsibly
 */
open_descriptor_queries(Descriptors, Read_Descriptors, Write_Descriptors, Query_Objects) :-
    open_descriptor_queries_uniquely(Descriptors, Read_Descriptors, Write_Descriptors, [], Map),
    predsort(key_equal,Map,Sorted_Map),
    maplist([_Descriptor=Query_Object,Query_Object]>>true, Sorted_Map, Query_Objects).

/**
 * terminus_max_retries(-Retries) is det.
 *
 * Environmental setable to give the number of times to try optimistic concurrency.
 */
terminus_max_retries(Retries) :-
    getenv('TERMINUS_TRANSACTION_RETRIES', Retries_Atom),
    !,
    atom_number(Retries_Atom, Retries).
terminus_max_retries(5).

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
                 _Witnesses) :-
    % turn descriptors into query objects
    terminus_max_retries(Max_Retries),

    between(1,Max_Retries,_),
    % Get unique query objects per descriptor
    open_descriptor_queries(Pre_Descriptors, Read_Descriptors, Write_Descriptors, Update_Query_Objects),
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
with_transaction(_Pre_Descriptors,
                 _Read_Descriptors,
                 _Write_Descriptors,
                 _Update_Query_Objects,
                 _Post_Query_Objects,
                 _Query_Update,
                 _Post,
                 _Witnesses) :-
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

    maplist([Ref_Query_Object,Repo_Query_Object]>>(
                Ref_Query_Object.parent = Repo_Query_Object),
            Ref_Query_Objects,
            Second_Level_Repo_Query_Objects),

    union(Toplevel_Repo_Query_Objects,
          Second_Level_Repo_Query_Objects,
          Repo_Query_Objects),

    include([Query_Object]>>(database_descriptor{} :< Query_Object.collection_descriptor),
            Query_Objects,
            Toplevel_Label_Query_Objects),

    maplist([Repo_Query_Object,Label_Query_Object]>>(
                Repo_Query_Object.parent = Label_Query_Object),
            Repo_Query_Objects,
            Second_Level_Label_Query_Objects),

    union(Toplevel_Label_Query_Objects,
          Second_Level_Label_Query_Objects,
          Label_Query_Objects).

set_ref_head(Ref_Query_Object, New_Ref_Query_Object) :-
    Ref_Name = Ref_Query_Object.descriptor.ref_name,
    Last_Commit = Ref_Query_Object.descriptor.last_commit,
    Ref_Query_Object.instance_read_objects = [Ref_Layer],
    open_write(Ref_Layer, Ref_Layer_Builder),

    Ref_Instance_Read_Objects = Ref_Query_Object.instance_read_objects,
    maplist([Ref_Instance_Read_Object]>>(Ref_Instance_Read_Object.read),
            Ref_Instance_Read_Objects, Instance_Layers),
    Ref_Schema_Read_Objects = Ref_Query_Object.schema_read_objects,
    maplist([Ref_Schema_Read_Object]>>(Ref_Schema_Read_Object.read),
            Ref_Schema_Read_Objects, Schema_Layers),
    Ref_Inference_Read_Objects = Ref_Query_Object.inference_read_objects,
    maplist([Ref_Inference_Read_Object]>>(Ref_Inference_Read_Object.read),
            Ref_Inference_Read_Objects, Inference_Layers),


    atomic_list_concat([Ref_Name,'/document/'],Ref_Base),
    random_uri(Ref_Base,'Commit',Commit_URI),
    write_ref_commit(Ref_Layer_Builder, Last_Commit, Commit_URI), % TODO defined for 5

    maplist({Commit_URI,Ref_Base,Ref_Layer_Builder}/[Layer]>>
            write_layer_to_commit(instance,Ref_Layer_Builder,Ref_Base,Layer,Commit_URI),
            Instance_Layers),

    maplist({Commit_URI,Ref_Base,Ref_Layer_Builder}/[Layer]>>
            write_layer_to_commit(schema,Ref_Layer_Builder,Ref_Base,Layer,Commit_URI),
            Schema_Layers),

    maplist({Commit_URI,Ref_Base,Ref_Layer_Builder}/[Layer]>>
            write_layer_to_commit(inference,Ref_Layer_Builder,Ref_Base,Layer,Commit_URI),
            Inference_Layers),

    nb_commit(Ref_Layer_Builder, New_Ref_Layer),
    New_Ref_Query_Object = Ref_Query_Object.put(_{instance_read_object: [New_Ref_Layer]}).

:- table metadata_prefix_expand/2.
metadata_prefix_expand(Prefixed_URI,URI) :-
    global_prefix_expand(Prefixed_URI, URI).

write_ref_commit(Ref_Layer_Builder,Ref_URI,Author,Last_Commit_URI,Commit_URI) :-
    metadata_prefix_expand(terminus:'Commit', Commit_Type),
    metadata_prefix_expand(rdf:type, RDF_Type),
    metadata_prefix_expand(xsd:string, XSD_String),
    metadata_prefix_expand(terminus:ref_commit, Ref_Commit_Prop),
    metadata_prefix_expand(terminus:commit_parent, Parent_Prop),
    metadata_prefix_expand(terminus:commit_timestamp, Commit_Timestamp_Prop),
    metadata_prefix_expand(terminus:commit_author, Author_Prop),
    metadata_prefix_expand(terminus:no_commit, No_Commit_URI),
    unix_time_string_now(Unix_Time_String),
    object_storage(Unix_Time_String^^XSD_String, Timestamp_Literal),
    object_storage(literal(Author,XSD_String), Author_Literal),
    nb_add_triple(Ref_Layer_Builder, Commit_URI, RDF_Type, node(Commit_Type)),
    (   Last_Commit_URI = No_Commit_URI
    ->  true
    ;   nb_remove_triple(Ref_Layer_Builder, Ref_URI, Ref_Commit_Prop, node(Last_Commit_URI)),
        nb_add_triple(Ref_Layer_Builder, Commit_URI, Parent_Prop, node(Last_Commit_URI))),
    nb_add_triple(Ref_Layer_Builder, Ref_URI, Ref_Commit_Prop, node(Commit_URI)),
    nb_add_triple(Ref_Layer_Builder, Commit_URI, RDF_Type, node(Commit_Type)),
    nb_add_triple(Ref_Layer_Builder, Commit_URI, Commit_Timestamp_Prop, value(Timestamp_Literal)),
    nb_add_triple(Ref_Layer_Builder, Commit_URI, Author_Prop, value(Author_Literal)).

write_layer_to_commit(Type,Ref_Layer_Builder,Ref_Base,Layer,Commit_URI) :-
    metadata_prefix_expand(terminus:Type, Graph_Property),
    metadata_prefix_expand(terminus:'Layer', Layer_Type),
    metadata_prefix_expand(terminus:layer_id, Layer_ID_Prop),
    metadata_prefix_expand(terminus:layer_parent, Layer_Parent_Prop),
    metadata_prefix_expand(rdf:type, RDF_Type),
    metadata_prefix_expand(xsd:string, XSD_String),
    layer_to_id(Layer,Layer_ID),
    object_storage(literal(Layer_ID,XSD_String), Layer_Literal),
    atomic_list_concat([Ref_Base,'/document/Layer'],Layer_Base),
    parent(Layer,Parent),
    layer_to_id(Parent,Parent_Layer_ID),
    idgen(Layer_Base ,[Layer_ID], Layer_URI),
    idgen(Layer_Base ,[Parent_Layer_ID], Parent_Layer_URI),
    nb_add_triple(Ref_Layer_Builder, Layer_URI, RDF_Type, node(Layer_Type)),
    nb_add_triple(Ref_Layer_Builder, Commit_URI, Graph_Property, node(Layer_URI)),
    nb_add_triple(Ref_Layer_Builder, Layer_URI, Layer_ID_Prop, value(Layer_Literal)),
    nb_add_triple(Ref_Layer_Builder, Layer_URI, Layer_Parent_Prop, node(Parent_Layer_URI)).

unix_time_string_now(Unix_Time_String) :-
    get_time(Time),
    Unix_Time is floor(Time),
    number_string(Unix_Time,Unix_Time_String).

random_uri(Base,Type,URI) :-
    atomic_list_concat([Base,Type],Type_Base),
    Size is 2 ** (20 * 8),
    random(0, Size, Num),
    format(atom(Random), '~36r', [Num]),
    idgen(Type_Base ,[Random], URI).

nb_remove_shadow_layer(Layer_Builder, Repo_Name, Layer_Id) :-
    metadata_prefix_expand(terminus:'ShadowLayer', Layer_Type),
    metadata_prefix_expand(terminus:layer_id, Layer_ID_Prop),
    metadata_prefix_expand(rdf:type, RDF_Type),
    metadata_prefix_expand(xsd:string, XSD_String),

    atomic_list_concat([Repo_Name,'/document/ShadowLayer'], Layer_Base),

    idgen(Layer_Base, [Layer_Id], Layer_URI),
    object_storage(literal(Layer_Id,XSD_String), Layer_Literal),

    ignore(nb_remove_triple(Layer_Builder, Layer_URI, RDF_Type, node(Layer_Type))),
    ignore(nb_remove_triple(Layer_Builder, Layer_URI, Layer_ID_Prop, value(Layer_Literal))).

nb_add_shadow_layer(Layer_Builder, Repo_Name, Layer_Id) :-
    metadata_prefix_expand(terminus:'ShadowLayer', Layer_Type),
    metadata_prefix_expand(terminus:layer_id, Layer_ID_Prop),
    metadata_prefix_expand(rdf:type, RDF_Type),
    metadata_prefix_expand(xsd:string, XSD_String),

    atomic_list_concat([Repo_Name,'/document/ShadowLayer'], Layer_Base),

    idgen(Layer_Base, [Layer_Id], Layer_URI),
    object_storage(literal(Layer_Id,XSD_String), Layer_Literal),

    nb_add_triple(Layer_Builder, Layer_URI, RDF_Type, node(Layer_Type)),
    nb_add_triple(Layer_Builder, Layer_URI, Layer_ID_Prop, value(Layer_Literal)).


update_repository_data(Repo_Name, Repo_Layer_Builder, URI, Layer) :-
    layer_to_id(Layer, Layer_Id),
    parent(Layer, Parent),
    layer_to_id(Parent, Parent_Id),

    metadata_prefix_expand(terminus:repository_head, Repository_Head_Property),
    atomic_list_concat([Repo_Name,'/document/ShadowLayer'], Layer_Base),

    idgen(Layer_Base, [Layer_Id], Layer_URI),
    idgen(Layer_Base, [Parent_Id], Parent_URI),

    nb_remove_shadow_layer(Repo_Layer_Builder, Repo_Name, Parent_URI),
    nb_add_shadow_layer(Repo_Layer_Builder, Repo_Name, Layer_Id),

    nb_remove_triple(URI, Repository_Head_Property, node(Parent_URI)),   % TODO arity
    nb_add_triple(URI, Repository_Head_Property, node(Layer_URI)).  % TODO arity


set_repo_head(Layer_Builder, Repo_Query_Object - Ref_Query_Objects) :-
    Repo_Name = Repo_Query_Object.descriptor.repo_name,
    maplist({Repo_Name, Layer_Builder}/[Ref_Query_Object]>>(
                [Layer] = Ref_Query_Object.instance_read_objects,
                URI = Ref_Query_Object.descriptor.ref_name,
                update_repository_data(Repo_Name, Layer_Builder, URI, Layer)),
            Ref_Query_Objects).

query_object_parents(Query_Objects, Query_Object_Candidates, Child_Parent_Pairs) :-
    findall(Query_Object - Associated_Parents,
            (   member(Query_Object,Query_Objects),
                convlist({Query_Object}/[Query_Object_Candidate]>>
                         descriptor_compare('=',Query_Object.parent.descriptor,
                                                Query_Object_Candidate.descriptor),  % TODO need another argument
                         Query_Object_Candidates,
                         Associated_Parents)),
            Child_Parent_Pairs).

query_object_layer([Query_Object|_Query_Objects], Layer) :-
    database_descriptor{} :< Query_Object.descriptor,
    !,
    Query_Object.instance_read_objects = [Read_Obj],
    Layer = Read_Obj.read.
query_object_layer([_|Query_Objects], Layer) :-
    query_object_layer(Query_Objects, Layer).

set_heads_for_db(Database_Name - Query_Objects) :-
    query_object_layer(Query_Objects, Layer),
    open_write(Layer, Layer_Builder),
    % split query objects by type (ref, repo, label)
    collect_query_objects(Query_Objects,
                          Ref_Query_Objects,
                          Repo_Query_Objects,
                          Label_Query_Objects),

    (   % If we don't have one database, we're
        % probably...?
        [_Label_Query_Object] = Label_Query_Objects
    ->  true
    ;   [] = Label_Query_Objects
    ->  Format = 'No label object associated with the database ~q',
        debug(database, Format, [Database_Name]),
        format(atom(M), Format, [Database_Name]),
        throw(transaction_error(M))
    ;   Format = 'Too many label objects associated with the database ~q',
        debug(database, Format, [Database_Name]),
        format(atom(M), Format, [Database_Name]),
        throw(transaction_error(M))),

    % set heads for all the refs
    maplist(set_ref_head, Ref_Query_Objects, New_Ref_Query_Objects),

    query_object_parents(Repo_Query_Objects, New_Ref_Query_Objects, Repo_Refs),

    % set everything from repos into the layer builder
    maplist(set_repo_head(Layer_Builder), Repo_Refs),
    nb_commit(Layer_Builder, NewLayer),
    storage(Store),
    safe_open_named_graph(Store, Database_Name, Graph_Object),
    % Finally we can set the HEAD after we did all
    nb_set_head(Graph_Object, NewLayer).


query_object_database_name(Descriptor, Database_Name) :-
    database_descriptor{database_name: Database_Name} :< Descriptor.
query_object_database_name(Descriptor, Database_Name) :-
    ref_descriptor{repository_descriptor: Repository_Descriptor} :< Descriptor,
    query_object_database_name(Repository_Descriptor, Database_Name).
query_object_database_name(Descriptor, Database_Name) :-
    terminus_descriptor = Descriptor,
    terminus_database_name(Database_Name).
query_object_database_name(Descriptor, Database_Name) :-
    repository_descriptor{database_descriptor: Database_Descriptor} :< Descriptor,
    query_object_database_name(Database_Descriptor, Database_Name).

set_heads(Query_Objects) :-
    % split query objects by type (ref, repo, label)
    findall(Database_Name - Query_Object, (
                member(Query_Object, Query_Objects),
                query_object_database_name(Query_Object, Database_Name)
            ), Database_Name_Query_Objects),
    % Resulting structure of group_pairs DatabaseName-[list of query objects]
    group_pairs_by_key(Database_Name_Query_Objects, Database_Query_Objects),
    maplist(set_heads_for_db, Database_Query_Objects).


		 /*******************************
		 *         Missing Predicates   *
		 *******************************/
update_read_objects(_Instance_Read_Objects, _Committed_Instance_Objects, _New_Instance_Read_Objects) :-
    true.


% note there's a descriptor_query/2 in woql_compile
descriptor_query(_Descriptor,
                 _Read_Descriptors,
                 _Write_Descriptors,
                 _Map,
                 _Middle_Map) :-
    true.


