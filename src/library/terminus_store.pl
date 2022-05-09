:- module(terminus_store, [
              terminus_store_version/1,
              open_memory_store/1,
              open_directory_store/2,

              create_named_graph/3,
              open_named_graph/3,
              delete_named_graph/2,

              head/2,
              head/3,
              nb_set_head/2,
              nb_force_set_head/2,
              nb_force_set_head/3,

              open_write/2,

              nb_add_triple/4,
              nb_remove_triple/4,
              nb_commit/2,
              builder_committed/1,
              nb_apply_delta/2,
              nb_apply_diff/2,

              node_and_value_count/2,
              predicate_count/2,
              subject_id/3,
              predicate_id/3,
              object_id/3,

              id_triple/4,
              triple/4,

              id_triple_addition/4,
              triple_addition/4,

              id_triple_removal/4,
              triple_removal/4,

              sp_card/4,

              parent/2,
              squash/2,

              layer_addition_count/2,
              layer_removal_count/2,
              layer_total_addition_count/2,
              layer_total_removal_count/2,
              layer_total_triple_count/2,

              layer_to_id/2,
              store_id_layer/3,

              pack_export/3,
              pack_layerids_and_parents/2,
              pack_import/3,

              csv_builder/3,
              csv_builder/4,
              csv_builder/5,
              csv_iri/3,

              count_layer_stack_size/2,

              rollup/1,
              rollup_upto/2,
              imprecise_rollup_upto/2,

              layer_stack_names/2,
              layer_equals/2
            ]).

terminus_store_version('0.19.8').

% There is two ways that this library is used.
% 1. Standalone - in this case we need to load the internal foreign library.
% 2. As part of TerminusDB - in this case we expect all the foreign
%    predicates to have been preloaded into the module '$terminus_store'.
%
% The reason for this is that TerminusDB builds its own rust module
% which bundles the internal foreign library. This is necessary cause
% otherwise TerminusDB is unable to make use of the types defined in
% this library, as these are not exposed through ordinarly shared
% objects. TerminusDB needs these types in order to build
% TerminusDB-specific native logic that works with store, graph,
% layer, and builder blobs.
%
% In order to switch between the two kinds of behavior, TerminusDB
% defines a special prolog flag, 'terminusdb_monolithic_module'. This
% suppresses loading of the internal library, and instead imports
% foreign predicates which are expected to have been preloaded into
% '$terminus_store'.
:- if(\+ current_prolog_flag(terminusdb_monolithic_module, true)).
:- use_foreign_library(foreign(libterminus_store)).
:- endif.

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(plunit)).
:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pldocs for the foreign predicates %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! open_memory_store(-Store:store) is det
%
% Opens an in-memory store and unifies it with Store.
%
% @arg Store the returned in-memory store.

%! open_directory_store(+Path:text, -Store:store) is det.
%
% Opens a store backed by a directory, and unifies it with Store.
%
% This predicate does not check if the directory actually exists, but
% other store-related predicates will error when used with a store
% backed by a non-existent directory.
%
% @arg Path a file system path to the store directory. This can be either absolute and relative.
% @arg Store the returned directory store.

%! create_named_graph(+Store:store, +Name:text, -Graph:named_graph) is det.
%
% Create a new named graph with the given name, and unifies it with Graph.
%
% @arg Store the store to create the graph in.
% @arg Name the name which the new graph should have.
% @arg Graph the returned named graph.
% @throws if a graph with the given name already exists.

%! open_named_graph(+Store:store, +Name:text, -Graph:named_graph) is semidet.
%
% Opens an existing named graph with the given name.
%
% Fails if no graph with that name exists.
%
% @arg Store the store to create the graph in.
% @arg Name the name of the graph to be opened.
% @arg Graph the returned named graph.

%! delete_named_graph(+Store:store, +Name:text) is semidet.
%
% Deletes an existing named graph with the given name.
%
% Fails if no graph with that name exists.
%
% @arg Store the store to create the graph in.
% @arg Name the name of the graph to be opened.

%! head(+Graph:named_graph, -Layer:layer) is semidet.
%
% Retrieve the layer that a named graph points at.
% This is the equivalent of opening a read transaction with snapshot isolation on a named graph.
%
% Fails if the given graph has no head yet.
%
% @arg Graph the named graph to retrieve the head layer from.
% @arg Layer the returned head layer.


%! head(+Graph:named_graph, -Layer:layer, -Version:version) is semidet.
%
% Retrieve the layer that a named graph points at and retrieve the version.
% This is the equivalent of opening a read transaction with snapshot isolation on a named graph.
%
% Fails if the given graph has no head yet.
%
% @arg Graph the named graph to retrieve the head layer from.
% @arg Layer the returned head layer.
% @arg Version the version of the label.

%! nb_set_head(+Graph:named_graph, +Layer:layer) is semidet.
%
% Set the given layer as the new head of the given graph.
%
% Fails if the new layer is not a proper child of the current head.
%
% This predicate does not support backtracking.
%
% @arg Graph the named graph to set the head layer of.
% @arg Layer the layer to make the new head of the graph.


%! nb_set_head(+Graph:named_graph, +Layer:layer, +Version:version) is semidet.
%
% Set the given layer as the new head of the given graph and checks if version
% matches.
%
% Fails if the new layer is not a proper child of the current head.
%
% This predicate does not support backtracking.
%
% @arg Graph the named graph to set the head layer of.
% @arg Layer the layer to make the new head of the graph.
% @arg Version the version of the label.

%! open_write(+Store_Or_Layer:term, -Builder:layer_builder) is det.
%
% Creates a layer builder from either a parent layer, or a store.
%
% When Store_Or_Layer is a store, the resulting builder will create a
% base layer.
%
% When Store_Or_Layer is a layer, the resulting builder will create a
% child layer whose parent is the given layer.
%
% @arg Store_Or_layer a store when creating a new base layer, or the parent layer when creating a child layer.
% @arg Builder a layer builder to create the new layer.

%! nb_add_id_triple(+Builder:layer_builder, +Subject_Id:integer, +Predicate_Id:integer, +Object_Id: integer) is semidet.
%
% Add the given subject, predicate and object as a triple to the builder object.
%
% This fails if any of the Ids is out of range, or if the triple
% already exists, either in this builder or in a parent layer.
%
% @arg Builder the builder object to add this triple to.
% @arg Subject_Id the id of the triple subject.
% @arg Predicate_Id the id of the triple predicate.
% @arg Object_Id the id of the triple object.

%! nb_add_string_node_triple(+Builder:layer_builder, +Subject:text, +Predicate:text, +Object:text) is semidet.
%
% Add the given subject, predicate, and object as a triple to the
% builder object. The object is interpreted as pointing at a node,
% rather than being a literal value.
%
% This fails if the triple already exists in this builder object or a parent layer.
%
% @arg Builder the builder object to add this triple to.
% @arg Subject the triple subject.
% @arg Predicate the triple predicate.
% @arg Object the triple object, which is interpreted as a node.

%! nb_add_string_value_triple(+Builder:layer_builder, +Subject:text, +Predicate:text, +Object:text) is semidet.
%
% Add the given subject, predicate, and object as a triple to the
% builder object. The object is interpreted as a value, rather than a node.
%
% This fails if the triple already exists in this builder object or a parent layer.
%
% @arg Builder the builder object to add this triple to.
% @arg Subject the triple subject.
% @arg Predicate the triple predicate.
% @arg Object the triple object, which is interpreted as a value.

%! nb_remove_id_triple(+Builder:layer_builder, +Subject_Id:integer, +Predicate_Id:integer, +Object_Id: integer) is semidet.
%
% Add the given subject, predicate and object as a triple removal to the builder object.
%
% This fails if any of the Ids is out of range, or if the triple does
% not exist in a parent layer, or if the removal has already been
% registered in this builder.
%
% @arg Builder the builder object to add this triple removal to.
% @arg Subject_Id the id of the triple subject.
% @arg Predicate_Id the id of the triple predicate.
% @arg Object_Id the id of the triple object.

%! nb_remove_string_node_triple(+Builder:layer_builder, +Subject:text, +Predicate:text, +Object:text) is semidet.
%
% Add the given subject, predicate, and object as a triple removal to the
% builder object. The object is interpreted as pointing at a node,
% rather than being a literal value.
%
% This fails if the triple does not exist in a parent layer, or if the
% removal has already been registered in this builder.
%
% @arg Builder the builder object to add this triple removal to.
% @arg Subject the triple subject.
% @arg Predicate the triple predicate.
% @arg Object the triple object, which is interpreted as a node.

%! nb_remove_string_value_triple(+Builder:layer_builder, +Subject:text, +Predicate:text, +Object:text) is semidet.
%
% Add the given subject, predicate, and object as a triple removal to
% the builder object. The object is interpreted as a value, rather
% than a node.
%
% This fails if the triple does not exist in a parent layer, or if the
% removal has already been registered in this builder.
%
% @arg Builder the builder object to add this triple removal to.
% @arg Subject the triple subject.
% @arg Predicate the triple predicate.
% @arg Object the triple object, which is interpreted as a node.

%! nb_apply_delta(+Builder:layer_builder, +Layer:layer) is det.
%
% Add and remove all additions and removals from Layer into Builder
%
% @arg Builder the layer builder to make changes to.
% @arg Layer the layer that will apply changes from.
% @throws if the builder has already been committed.

%! nb_apply_diff(+Builder:layer_builder, +Layer:layer) is det.
%
% Make whatever changes are necessary to Builder, to bring it in line
% with Layer. Our final visabile state should be as layer, so we are
% calculating the diff which should go into builder to do so.
%
% @arg Builder the layer builder to make changes to.
% @arg Layer the layer that we will view as a prototype.
% @throws if the builder has already been committed.

%! nb_commit(+Builder:layer_builder, -Layer:layer) is det.
%
% Commit the layer builder, turning it into a layer.
%
% @arg Builder the layer builder to commit.
% @arg Layer the layer that will be returned.
% @throws if the builder has already been committed.

%! node_and_value_count(+Layer:layer, -Count:integer) is det.
%
% Unify Count with the amount of nodes and values known to this layer,
% including all parent layers.
%
% @arg Layer the layer for which to get a count.
% @arg Count the returned count.

%! predicate_count(+Layer:layer, -Count:integer) is det.
%
% Unify Count with the amount of predicates known to this layer,
% including all parent layers.
%
% @arg Layer the layer for which to get a count.
% @arg Count the returned count.

%! subject_to_id(+Layer:layer, +Subject:text, -Id:integer) is semidet.
%
% Convert the given subject to its id representation in the given layer.
% Fails if this subject is not known in the given layer.
%
% @arg Layer the layer to use for the conversion.
% @arg Subject an atom or string containing the subject.
% @arg Id the id of the subject in the given layer.

%! id_to_subject(Layer:layer, +Id:integer, -Subject:string) is semidet.
%
% Convert the given id to a subject using the given layer.
% Fails if the id is out of range for subjects.
%
% @arg Layer the layer to use for the conversion.
% @arg Id the id to convert into a subject.
% @arg Subject the subject which the id refers to.

%! predicate_to_id(+Layer:layer, +Predicate:text, -Id:integer) is semidet.
%
% Convert the given predicate to its id representation in the given layer.
% Fails if this predicate is not known in the given layer.
%
% @arg Layer the layer to use for the conversion.
% @arg Predicate an atom or string containing the predicate.
% @arg Id the id of the predicate in the given layer.

%! id_to_predicate(Layer:layer, +Id:integer, -Predicate:string) is semidet.
%
% Convert the given id to a predicate using the given layer.
% Fails if the id is out of range for predicates.
%
% @arg Layer the layer to use for the conversion.
% @arg Id the id to convert into a predicate.
% @arg Predicate the predicate which the id refers to.

%! object_to_id(+Layer:layer, +Object:text, -Id:integer) is semidet.
%
% Convert the given node object to its id representation in the given layer.
% Fails if this subject is not known in the given layer.
%
% @arg Layer the layer to use for the conversion.
% @arg Object an atom or string containing the object. The object is assumed to refer to a node.
% @arg Id the id of the object in the given layer.

%! id_to_object(Layer:layer, +Id:integer, -Object:string, -Object_Type:atom) is semidet.
%
% Convert the given id to a object using the given layer.
% Fails if the id is out of range for objects.
%
% @arg Layer the layer to use for the conversion.
% @arg Id the id to convert into a object.
% @arg Object the object which the id refers to.
% @arg Object_Type the type of the object, either 'node' or 'value'.

%! parent(+Layer:layer, +Parent:layer) is semidet.
%
% Unifies Parent with the parent layer of Layer. Fails if that layer
% has no parent.
%
% @arg Layer the layer for which to do the parent lookup.
% @arg Parent the retrieved parent layer.

%! squash(+Layer:layer, +Squash:layer) is semidet.
%
% Squashes a layer-stack to create a new fresh layer
%
% @arg Layer the layer for which to do the parent lookup.
% @arg Parent the retrieved parent layer.

%! rollup(+Layer:layer) is semidet.
%
% Produces a rollup of the current layer
%
% @arg Layer the layer for which to do the parent lookup.

%! rollup_upto(+Layer:layer, +Upto:layer) is semidet.
%
% Produces a rollup of the current layer upto (but not including)
% the specified layer
%
% @arg Layer the layer for which to do the parent lookup.
% @arg Upto the layer at which to stop the rollup.

%! csv_builder(+Name:string, +Csv:path, +Builder:builder, +Options:options) is det
%
% Creates a layer with the contents of a csv as triples
%
% @arg Name Name of the CSV object
% @arg Csv The path to the csv to be loaded
% @arg Builder The builder into which to place the CSV
% @arg Layer The returned Layer
% @arg Options A list containing any of the following:
%     * data_prefix(Prefix) (default is "csv:///data#")
%     * predicate_prefix(Prefix) (default is "csv:///schema#")
%     * header(Bool) (Boolean to read a header, default true)
%     * skip_header(Bool) (Skip the header regardless of presence,
%                          default false)

%! csv_builder(+Name:string, +Csv:path, +Builder:builder) is det
%
% Creates a layer with the contents of a csv as triples. Options are defaults.
%
% @arg Name Name of the CSV object
% @arg Csv The path to the csv to be loaded
% @arg Builder The builder into which to place the CSV
% @arg Layer The returned Layer

%! csv_iri(Name, Prefix, IRI) is det.
%
% Creates a CSV IRI from a name and prefix

%! layer_stack_names(+Layer:layer, -Stack:list) is det.
%
% Creates a layer-id stack from a layer which contains all ancestor
% layers.
%
% @arg Layer The layer from which to obtain the stack.
% @arg Stack A list of the layer-ids of all ancestors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of foreign predicate pldocs   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! nb_add_triple(+Builder, +Subject, +Predicate, +Object) is semidet
%
% Add a triple to the builder.
nb_add_triple(Builder, Subject, Predicate, Object) :-
    integer(Subject),
    integer(Predicate),
    integer(Object),
    !,
    nb_add_id_triple(Builder, Subject, Predicate, Object).

nb_add_triple(Builder, Subject, Predicate, Object) :-
    !,
    nb_add_string_triple(Builder, Subject, Predicate, Object).

/*
 * nb_add_triple(+Builder, +Subject, +Predicate, +Object) is semidet
 *
 * Remove a trible from the builder
 */
nb_remove_triple(Builder, Subject, Predicate, Object) :-
    integer(Subject),
    integer(Predicate),
    integer(Object),
    !,
    nb_remove_id_triple(Builder, Subject, Predicate, Object).

nb_remove_triple(Builder, Subject, Predicate, Object) :-
    !,
    nb_remove_string_triple(Builder, Subject, Predicate, Object).

/*
 * subject_id(+Layer, +Subject, -Id) is semidet
 *
 * Get the ID from a subject
 */
subject_id(Layer, Subject, Id) :-
    ground(Id),
    !,
    id_to_subject(Layer, Id, Subject).

subject_id(Layer, Subject, Id) :-
    ground(Subject),
    !,
    subject_to_id(Layer, Subject, Id).

subject_id(Layer, Subject, Id) :-
    node_and_value_count(Layer, Count),
    between(1, Count, Id),
    id_to_subject(Layer, Id, Subject).


/*
 * predicate_id(+Layer, +Predicate, -Id) is semidet
 *
 * Get the ID from a predicate
 */
predicate_id(Layer, Predicate, Id) :-
    ground(Id),
    !,
    id_to_predicate(Layer, Id, Predicate).

predicate_id(Layer, Predicate, Id) :-
    ground(Predicate),
    !,
    predicate_to_id(Layer, Predicate, Id).

predicate_id(Layer, Predicate, Id) :-
    node_and_value_count(Layer, Count),
    between(1, Count, Id),
    id_to_predicate(Layer, Id, Predicate).


/*
 * object_id(+Layer, +Predicate, -Id) is semidet
 *
 * Get the ID from an object
 */
object_id(Layer, Object, Id) :-
    ground(Id),
    !,
    id_to_object(Layer, Id, Object).

object_id(Layer, node(Object), Id) :-
    ground(Object),
    !,
    object_to_id(Layer, node(Object), Id).

object_id(Layer, value(Object), Id) :-
    ground(Object),
    !,
    object_to_id(Layer, value(Object), Id).

object_id(Layer, Object, Id) :-
    node_and_value_count(Layer, Count),
    between(1, Count, Id),
    id_to_object(Layer, Id, Object).

triple(Layer, Subject, Predicate, Object) :-
    (   ground(Subject)
    ->  subject_id(Layer, Subject, S_Id)
    ;   true),

    (   ground(Predicate)
    ->  predicate_id(Layer, Predicate, P_Id)
    ;   true),

    (   ground(Object)
    ->  object_id(Layer, Object, O_Id)
    ;   true),

    id_triple(Layer, S_Id, P_Id, O_Id),

    (   ground(Subject)
    ->  true
    ;   subject_id(Layer, Subject, S_Id)),


    (   ground(Predicate)
    ->  true
    ;   predicate_id(Layer, Predicate, P_Id)),


    (   ground(Object)
    ->  true
    ;   object_id(Layer,Object, O_Id)).

csv_builder(Name, Csv, Builder) :-
    csv_builder(Name, Csv,Builder,[]).

csv_builder(Name, Csv, Builder, Options) :-
    option(data_prefix(Data), Options, 'csv:///data/'),
    option(schema_prefix(Schema), Options, 'csv:///schema#'),
    option(header(Header), Options, true),
    option(skip_header(Skip), Options, false),
    csv_builder(Name, Csv, Builder, Data, Schema, Header, Skip).

csv_builder(Name, Csv, Builder, Schema_Builder, Options) :-
    option(data_prefix(Data), Options, 'csv:///data/'),
    option(schema_prefix(Schema), Options, 'csv:///schema#'),
    option(header(Header), Options, true),
    option(skip_header(Skip), Options, false),
    csv_builder(Name, Csv, Builder, Schema_Builder, Data, Schema, Header, Skip).

triple_addition(Layer, Subject, Predicate, Object) :-
    (   ground(Subject)
    ->  subject_id(Layer, Subject, S_Id)
    ;   true),

    (   ground(Predicate)
    ->  predicate_id(Layer, Predicate, P_Id)
    ;   true),

    (   ground(Object)
    ->  object_id(Layer, Object, O_Id)
    ;   true),

    id_triple_addition(Layer, S_Id, P_Id, O_Id),

    (   ground(Subject)
    ->  true
    ;   subject_id(Layer, Subject, S_Id)),


    (   ground(Predicate)
    ->  true
    ;   predicate_id(Layer, Predicate, P_Id)),


    (   ground(Object)
    ->  true
    ;   object_id(Layer,Object, O_Id)).

triple_removal(Layer, Subject, Predicate, Object) :-
    (   ground(Subject)
    ->  subject_id(Layer, Subject, S_Id)
    ;   true),

    (   ground(Predicate)
    ->  predicate_id(Layer, Predicate, P_Id)
    ;   true),

    (   ground(Object)
    ->  object_id(Layer, Object, O_Id)
    ;   true),

    id_triple_removal(Layer, S_Id, P_Id, O_Id),

    (   ground(Subject)
    ->  true
    ;   subject_id(Layer, Subject, S_Id)),


    (   ground(Predicate)
    ->  true
    ;   predicate_id(Layer, Predicate, P_Id)),


    (   ground(Object)
    ->  true
    ;   object_id(Layer,Object, O_Id)).

count_layer_stack_size(Layer, Acc, Count) :-
    parent(Layer, Parent),
    !,
    NextAcc is Acc + 1,
    count_layer_stack_size(Parent, NextAcc, Count).
count_layer_stack_size(_Layer, Acc, Count) :-
    Count is Acc + 1.

count_layer_stack_size(Layer, Count) :-
    count_layer_stack_size(Layer, 0, Count).


layer_stack_names(Layer, Stack) :-
    ground(Layer),
    !,
    retrieve_layer_stack_names(Layer, Stack).
layer_stack_names(_Layer, _Stack) :-
    throw(error(domain_error('Layer not bound in layer_stack_names/2'),_)).

:- begin_tests(terminus_store).

:- use_module(library(filesex)).

		 /*******************************
		 *     Developer Utilities      *
		 *******************************/

/**
 * random_string(String) is det.
 */
random_string(String) :-
    Size is 2 ** (20 * 8),
    random(0, Size, Num),
    format(string(String), '~36r', [Num]).

clean(TestDir) :-
    delete_directory_and_contents(TestDir).

createng(TestDir) :-
    random_string(RandomString),
    atomic_list_concat(["testdir", RandomString], TestDir),
    make_directory(TestDir),
    open_directory_store(TestDir, X),
    create_named_graph(X, "sometestdb", _).

create_memory_ng(DB) :-
    open_memory_store(X),
    create_named_graph(X, "sometestdb", DB).

test(open_memory_store) :-
    open_memory_store(_).

test(open_directory_store_atom) :-
    open_directory_store(this_is_an_atom, _),
    open_directory_store("this is a string", _).

test(open_directory_store_atom_exception, [
         throws(error(type_error(text,234), _))
     ]) :-
    open_directory_store(234, _).

test(create_db, [cleanup(clean(TestDir))]) :-
    make_directory("testdir"),
    TestDir = 'testdir',
    open_directory_store("testdir", X),
    create_named_graph(X, "sometestdb", _).


test(create_db_on_memory) :-
    open_memory_store(X),
    create_named_graph(X, "sometestdb", _).

test(open_named_graph, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, X),
    open_named_graph(X, "sometestdb", _).

test(open_named_graph_memory) :-
    open_memory_store(X),
    create_named_graph(X, "sometestdb", _),
    open_named_graph(X, "sometestdb", _).

test(delete_named_graph_memory) :-
    open_memory_store(X),
    create_named_graph(X, "sometestdb", _),
    delete_named_graph(X, "sometestdb"),
    \+ open_named_graph(X, "sometestdb", _).

test(delete_named_graph_directory, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, X),
    \+ delete_named_graph(X, "unknowndb").

test(head_from_empty_db, [fail, cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, X),
    open_named_graph(X, "sometestdb", DB),
    head(DB, _). % should be false because we have no HEAD yet

test(head_from_empty_db_memory, [fail, setup(create_memory_ng(DB))]) :-
     head(DB, _).

test(open_write_from_db_without_head, [
    cleanup(clean(TestDir)),
    setup(createng(TestDir)),
    throws(
        error(cannot_open_named_graph_without_base_layer, _)
    )]) :-
    open_directory_store(TestDir, X),
    open_named_graph(X, "sometestdb", DB),
    open_write(DB, _).

test(open_write_from_db_with_head, [
         cleanup(clean(TestDir)),
         setup(createng(TestDir))
     ]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_commit(Builder, Layer),
    open_named_graph(Store, "sometestdb", DB),
    nb_set_head(DB, Layer),
    open_write(DB, _).


test(open_write_from_memory_ng_without_head, [
    setup(create_memory_ng(DB)),
    throws(
        error(cannot_open_named_graph_without_base_layer, _)
    )]) :-
    open_write(DB, _).

test(create_base_layer, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, _).


test(create_base_layer_memory) :-
    open_memory_store(Store),
    open_write(Store, _).

test(write_value_triple, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_string_triple(Builder, "Subject", "Predicate", value("Object")).

test(write_value_triple_memory) :-
    open_memory_store(Store),
    open_write(Store, Builder),
    nb_add_string_triple(Builder, "Subject", "Predicate", value("Object")).

test(commit_and_set_header, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    open_named_graph(Store, "sometestdb", DB),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    nb_set_head(DB, Layer).


test(commit_and_set_header_version_first, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    open_named_graph(Store, "sometestdb", DB),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    nb_force_set_head(DB, Layer, 0).


test(commit_and_set_header_version_first_wrong_version, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    open_named_graph(Store, "sometestdb", DB),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    \+ nb_force_set_head(DB, Layer, 1).

test(commit_and_set_header_version_multiple_commits, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    open_named_graph(Store, "sometestdb", DB),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    nb_force_set_head(DB, Layer, 0),

    head(DB, _, 1),

    open_write(Store, Builder2),
    nb_add_triple(Builder2, "Subject2", "Predicate2", value("Object2")),
    nb_commit(Builder2, Layer2),
    nb_force_set_head(DB, Layer2, 1),

    head(DB, _, 2),
    \+ head(DB, _, 3).


test(commit_and_set_header_version_incorrect, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    open_named_graph(Store, "sometestdb", DB),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    \+ nb_force_set_head(DB, Layer, 1).


test(commit_and_set_header_version_multiples_incorrect, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    open_named_graph(Store, "sometestdb", DB),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    nb_force_set_head(DB, Layer, 0),

    head(DB, _, 1),

    open_write(Store, Builder2),
    nb_add_triple(Builder2, "Subject2", "Predicate2", value("Object2")),
    nb_commit(Builder2, Layer2),
    \+ nb_force_set_head(DB, Layer2, 0).


test(commit_and_set_header_memory) :-
    open_memory_store(Store),
    open_write(Store, Builder),
    create_named_graph(Store, "sometestdb", DB),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    nb_set_head(DB, Layer).

test(head_after_first_commit, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_named_graph(Store, "sometestdb", DB),
    open_write(Store, Builder),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    nb_set_head(DB, Layer),
    head(DB, _).

test(predicate_count, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_named_graph(Store, "sometestdb", DB),
    open_write(Store, Builder),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    nb_set_head(DB, Layer),
    head(DB, LayerHead),
    predicate_count(LayerHead, Count),
    Count == 1.

test(node_and_value_count, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    node_and_value_count(Layer, Count),
    Count == 2.

test(predicate_count_2, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_named_graph(Store, "sometestdb", DB),
    open_write(Store, Builder),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_add_triple(Builder, "Subject2", "Predicate2", value("Object2")),
    nb_commit(Builder, Layer),
    nb_set_head(DB, Layer),
    predicate_count(Layer, Count),
    Count == 2.

test(remove_triple, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    open_write(Layer, LayerBuilder),
    nb_remove_triple(LayerBuilder, "Subject", "Predicate", value("Object")).

test(triple_search_test, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    setof(X, triple(Layer, "Subject", "Predicate", value(X)), Bag),
    Bag == ["Object"].


test(triple_search_test, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    setof(Y-X, triple(Layer, "Subject", Y, value(X)), Bag),
    Bag == ["Predicate"-"Object"].


test(triple_search_test, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "Subject", "Predicate", value("Object")),
    nb_commit(Builder, Layer),
    setof(X-Y-Z, triple(Layer, X, Y, value(Z)), Bag),
    Bag == ["Subject"-"Predicate"-"Object"].

test(backtracking_test, [cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    create_named_graph(Store, "testdb", DB),
    nb_add_triple(Builder, "A", "B", node("C")),
    nb_add_triple(Builder, "A", "D", node("C")),
    nb_add_triple(Builder, "A", "E", node("C")),
    nb_add_triple(Builder, "A", "E", node("O")),
    nb_add_triple(Builder, "A", "D", node("O")),
    nb_commit(Builder, Layer),
    nb_set_head(DB, Layer),

    findall(P, triple(Layer, "A", P, node("O")), Ps),
    Ps = ["D", "E"].

test(query_builder_for_committed, [cleanup(clean(TestDir)),setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),

    \+ builder_committed(Builder),

    nb_commit(Builder, _Layer),

    builder_committed(Builder).

test(squash_a_tower,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    create_named_graph(Store, "testdb", DB),
    nb_add_triple(Builder, "joe", "eats", node("urchin")),
    nb_commit(Builder, Layer),

    open_write(Layer,Builder2),
    nb_add_triple(Builder2, "jill", "eats", node("caviar")),
    nb_commit(Builder2, Layer2),

    squash(Layer2,Squashed_Layer),

    nb_set_head(DB, Squashed_Layer),

    open_named_graph(Store, "testdb", DB2),
    head(DB2,Squash),

    findall(X-P-Y, triple(Squash, X, P, Y), Triples),

    Triples = ["jill"-"eats"-node("caviar"),
               "joe"-"eats"-node("urchin")
              ],
    \+ parent(Squash,_).


test(force_set_head,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder1),
    create_named_graph(Store, "testdb", DB1),
    nb_add_triple(Builder1, "joe", "eats", node("urchin")),
    nb_commit(Builder1, _Layer1),

    open_write(Store, Builder2),
    nb_add_triple(Builder2, "jill", "eats", node("caviar")),
    nb_commit(Builder2, Layer2),

    nb_force_set_head(DB1,Layer2),

    head(DB1,Layer3),
    findall(X-P-Y, triple(Layer3, X, P, Y), Triples),

    Triples = ["jill"-"eats"-node("caviar")],

    \+ parent(Layer3,_).

test(apply_a_delta,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "joe", "eats", node("urchin")),
    nb_commit(Builder, Layer),

    open_write(Layer,Builder2),
    nb_add_triple(Builder2, "jill", "eats", node("caviar")),
    nb_commit(Builder2, Delta),

    open_write(Store, Builder_Base),
    nb_add_triple(Builder_Base, "cathie", "eats", node("seaweed")),
    nb_commit(Builder_Base, Base),

    open_write(Base, Builder_New),
    nb_apply_delta(Builder_New,Delta),
    nb_commit(Builder_New,Final_Layer),

    findall(X-P-Y, triple(Final_Layer, X, P, Y), Triples),

    Triples = ["cathie"-"eats"-node("seaweed"),
               "jill"-"eats"-node("caviar")
              ].

test(apply_a_diff,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "joe", "eats", node("urchin")),
    nb_add_triple(Builder, "jill", "eats", node("caviar")),
    nb_add_triple(Builder, "cathie", "eats", node("seaweed")),
    nb_commit(Builder, Layer),

    open_write(Store, Builder2),
    nb_add_triple(Builder2, "joe", "eats", node("seals")),
    nb_add_triple(Builder2, "jill", "eats", node("caviar")),
    nb_commit(Builder2, Prototype),

    open_write(Layer, Diff_Builder),
    nb_apply_diff(Diff_Builder,Prototype),
    nb_commit(Diff_Builder,Final_Layer),

    findall(X-P-Y, triple(Final_Layer, X, P, Y), Triples),
    Triples = [
        "jill"-"eats"-node("caviar"),
        "joe"-"eats"-node("seals")
    ],

    findall(X-P-Y, triple_addition(Final_Layer, X, P, Y), Triple_Additions),
    Triple_Additions = [
        "joe"-"eats"-node("seals")
    ],

    findall(X-P-Y, triple_removal(Final_Layer, X, P, Y), Triple_Removals),
    Triple_Removals = [
        "cathie"-"eats"-node("seaweed"),
        "joe"-"eats"-node("urchin")
    ].

test(apply_empty_diff,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "joe", "eats", node("urchin")),
    nb_add_triple(Builder, "jill", "eats", node("caviar")),
    nb_add_triple(Builder, "cathie", "eats", node("seaweed")),
    nb_commit(Builder, Prototype),

    open_write(Store, Diff_Builder),
    nb_apply_diff(Diff_Builder,Prototype),
    nb_commit(Diff_Builder,Final_Layer),

    findall(X-P-Y, triple(Final_Layer, X, P, Y), Triples),

    Triples = [
        "cathie"-"eats"-node("seaweed"),
        "jill"-"eats"-node("caviar"),
        "joe"-"eats"-node("urchin")
    ],

    findall(X-P-Y, triple_addition(Final_Layer, X, P, Y), Triple_Additions),
    Triple_Additions = Triples,
    findall(X-P-Y, triple_removal(Final_Layer, X, P, Y), Triple_Removals),
    Triple_Removals = [].

test(add_csv,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    tmp_file_stream(Filename, Stream, [encoding(utf8)]),
    format(Stream, "some,header~n", []),
    format(Stream, "1,2~n", []),
    format(Stream, "3,4~n", []),
    close(Stream),
    csv_builder("csv",Filename, Builder, []),
    nb_commit(Builder, Layer),
    findall(X-P-Y, triple(Layer, X, P, Y), Triples),

    Triples = [
        X-"csv:///schema#csv_column_header"-value("\"2\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        X-"csv:///schema#csv_column_some"-value("\"1\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        X-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node(Row1),
        Y-"csv:///schema#csv_column_header"-value("\"4\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        Y-"csv:///schema#csv_column_some"-value("\"3\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        Y-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node(Row1),
        "csv:///data/CSV_csv"-"csv:///schema#csv_column"-node("csv:///data/ColumnObject_csv_header"),
        "csv:///data/CSV_csv"-"csv:///schema#csv_column"-node("csv:///data/ColumnObject_csv_some"),
        "csv:///data/CSV_csv"-"csv:///schema#csv_row"-node(X),
        "csv:///data/CSV_csv"-"csv:///schema#csv_row"-node(Y),
        "csv:///data/CSV_csv"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("csv:///schema#CSV"),
        "csv:///data/CSV_csv"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"csv\"@en"),
        "csv:///data/ColumnObject_csv_header"-"csv:///schema#csv_column_index"-value("1^^'http://www.w3.org/2001/XMLSchema#integer'"),
        "csv:///data/ColumnObject_csv_header"-"csv:///schema#csv_column_name"-value("\"header\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        "csv:///data/ColumnObject_csv_header"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("csv:///schema#Column"),
        "csv:///data/ColumnObject_csv_some"-"csv:///schema#csv_column_index"-value("0^^'http://www.w3.org/2001/XMLSchema#integer'"),
        "csv:///data/ColumnObject_csv_some"-"csv:///schema#csv_column_name"-value("\"some\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        "csv:///data/ColumnObject_csv_some"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("csv:///schema#Column")
    ].

test(add_csv_skip_header,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    tmp_file_stream(Filename, Stream, [encoding(utf8)]),
    format(Stream, "1,2~n", []),
    format(Stream, "3,4~n", []),
    close(Stream),
    csv_builder("csv",Filename, Builder, [skip_header(true)]),
    nb_commit(Builder, Layer),
    findall(X-P-Y, triple(Layer, X, P, Y), Triples),
    Triples = [
        Row1-"csv:///schema#csv_column_0"-value("\"1\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        Row1-"csv:///schema#csv_column_1"-value("\"2\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        Row1-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node(Row_Type),
        Row2-"csv:///schema#csv_column_0"-value("\"3\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        Row2-"csv:///schema#csv_column_1"-value("\"4\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        Row2-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node(Row_Type),
        "csv:///data/CSV_csv"-"csv:///schema#csv_column"-node("csv:///data/ColumnObject_csv_0"),
        "csv:///data/CSV_csv"-"csv:///schema#csv_column"-node("csv:///data/ColumnObject_csv_1"),
        "csv:///data/CSV_csv"-"csv:///schema#csv_row"-node(Row1),
        "csv:///data/CSV_csv"-"csv:///schema#csv_row"-node(Row2),
        "csv:///data/CSV_csv"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("csv:///schema#CSV"),
        "csv:///data/CSV_csv"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"csv\"@en"),
        "csv:///data/ColumnObject_csv_0"-"csv:///schema#csv_column_index"-value("0^^'http://www.w3.org/2001/XMLSchema#integer'"),
        "csv:///data/ColumnObject_csv_0"-"csv:///schema#csv_column_name"-value("\"0\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        "csv:///data/ColumnObject_csv_0"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("csv:///schema#Column"),
        "csv:///data/ColumnObject_csv_1"-"csv:///schema#csv_column_index"-value("1^^'http://www.w3.org/2001/XMLSchema#integer'"),
        "csv:///data/ColumnObject_csv_1"-"csv:///schema#csv_column_name"-value("\"1\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        "csv:///data/ColumnObject_csv_1"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("csv:///schema#Column")
    ].

test(csv_prefixes,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),

    tmp_file_stream(Filename, Stream, [encoding(utf8)]),
    format(Stream, "some,header~n", []),
    format(Stream, "1,2~n", []),
    close(Stream),

    csv_builder("csv",Filename, Builder, [data_prefix('that/'),
                                          schema_prefix('this#')]),
    nb_commit(Builder, Layer),
    findall(X-P-Y, triple(Layer, X, P, Y), Triples),

    Triples = [
        Row1-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node(_),
        Row1-"this#csv_column_header"-value("\"2\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        Row1-"this#csv_column_some"-value("\"1\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        "that/CSV_csv"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("this#CSV"),
        "that/CSV_csv"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"csv\"@en"),
        "that/CSV_csv"-"this#csv_column"-node("that/ColumnObject_csv_header"),
        "that/CSV_csv"-"this#csv_column"-node("that/ColumnObject_csv_some"),
        "that/CSV_csv"-"this#csv_row"-node(Row1),
        "that/ColumnObject_csv_header"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("this#Column"),
        "that/ColumnObject_csv_header"-"this#csv_column_index"-value("1^^'http://www.w3.org/2001/XMLSchema#integer'"),
        "that/ColumnObject_csv_header"-"this#csv_column_name"-value("\"header\"^^'http://www.w3.org/2001/XMLSchema#string'"),
        "that/ColumnObject_csv_some"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("this#Column"),
        "that/ColumnObject_csv_some"-"this#csv_column_index"-value("0^^'http://www.w3.org/2001/XMLSchema#integer'"),
        "that/ColumnObject_csv_some"-"this#csv_column_name"-value("\"some\"^^'http://www.w3.org/2001/XMLSchema#string'")
    ].

test(csv_with_schema,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    open_write(Store, Schema_Builder),
    tmp_file_stream(Filename, Stream, [encoding(utf8)]),
    format(Stream, "some,header~n", []),
    format(Stream, "1,2~n", []),
    format(Stream, "3,4~n", []),
    close(Stream),
    csv_builder("csv",Filename, Builder, Schema_Builder,
                [data_prefix('data/'),
                 schema_prefix('')]),
    nb_commit(Schema_Builder, Schema_Layer),
    findall(X-P-Y, triple(Schema_Layer, X, P, Y), Schema_Triples),

    Schema_Expected = [
        "CSV"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#Class"),
        "CSV"-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"CSV object\"@en"),
        "CSV"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"CSV\"@en"),
        "CSV"-"http://www.w3.org/2000/01/rdf-schema#subClassOf"-node("http://terminusdb.com/schema/system#Document"),
        "CSVRow"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#Class"),
        "CSVRow"-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"Generic Row of a CSV file\"@en"),
        "CSVRow"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"CSV Row\"@en"),
        Row_Type-"http://terminusdb.com/schema/system#csv_name"-value("\"csv\"@en"),
        Row_Type-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#Class"),
        Row_Type-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"CSV Row object for columns [\\\"header\\\", \\\"some\\\"]\"@en"),
        Row_Type-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"CSV Row from csv\"@en"),
        Row_Type-"http://www.w3.org/2000/01/rdf-schema#subClassOf"-node("CSVRow"),
        "Column"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#Class"),
        "Column"-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"Column information object for a CSV\"@en"),
        "Column"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"Column\"@en"),
        "csv_column_header"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#DatatypeProperty"),
        "csv_column_header"-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"CSV Column for header name header\"@en"),
        "csv_column_header"-"http://www.w3.org/2000/01/rdf-schema#domain"-node(Row_Type),
        "csv_column_header"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"Column header\"@en"),
        "csv_column_header"-"http://www.w3.org/2000/01/rdf-schema#range"-node("http://www.w3.org/2001/XMLSchema#string"),
        "csv_column_some"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#DatatypeProperty"),
        "csv_column_some"-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"CSV Column for header name some\"@en"),
        "csv_column_some"-"http://www.w3.org/2000/01/rdf-schema#domain"-node(Row_Type),
        "csv_column_some"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"Column some\"@en"),
        "csv_column_some"-"http://www.w3.org/2000/01/rdf-schema#range"-node("http://www.w3.org/2001/XMLSchema#string"),
        "csv_column"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#ObjectProperty"),
        "csv_column"-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"Associates a CSV with a column object\"@en"),
        "csv_column"-"http://www.w3.org/2000/01/rdf-schema#domain"-node("CSV"),
        "csv_column"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"csv column\"@en"),
        "csv_column"-"http://www.w3.org/2000/01/rdf-schema#range"-node("Column"),
        "csv_column_index"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#DatatypeProperty"),
        "csv_column_index"-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"The ordering index for a column in a csv\"@en"),"csv_column_index"-"http://www.w3.org/2000/01/rdf-schema#domain"-node("Column"),
        "csv_column_index"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"csv column index\"@en"),"csv_column_index"-"http://www.w3.org/2000/01/rdf-schema#range"-node("http://www.w3.org/2001/XMLSchema#integer"),
        "csv_column_name"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#DatatypeProperty"),
        "csv_column_name"-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"The name of the column as it was verbatim in the CSV\"@en"),
        "csv_column_name"-"http://www.w3.org/2000/01/rdf-schema#domain"-node("Column"),
        "csv_column_name"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"csv column name\"@en"),
        "csv_column_name"-"http://www.w3.org/2000/01/rdf-schema#range"-node("http://www.w3.org/2001/XMLSchema#string"),
        "csv_row"-"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"-node("http://www.w3.org/2002/07/owl#ObjectProperty"),
        "csv_row"-"http://www.w3.org/2000/01/rdf-schema#comment"-value("\"Connects a CSV to its rows\"@en"),
        "csv_row"-"http://www.w3.org/2000/01/rdf-schema#domain"-node("CSV"),
        "csv_row"-"http://www.w3.org/2000/01/rdf-schema#label"-value("\"csv row\"@en"),
        "csv_row"-"http://www.w3.org/2000/01/rdf-schema#range"-node("CSVRow")
    ],

    forall(member(Triple,Schema_Triples),
           (   member(Triple,Schema_Expected))).

test(so_mode,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "A", "B", node("C")),
    nb_add_triple(Builder, "A", "B", node("D")),
    nb_commit(Builder, Layer),
    findall(X-C, triple(Layer, X, "B", C), Ps),
    Ps = ["A"-node("C"),
          "A"-node("D")].

test(sp_mode,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "A", "B", node("D")),
    nb_add_triple(Builder, "C", "B", node("D")),
    nb_commit(Builder, Layer),
    findall(X-C, triple(Layer, X, C, node("D")), Ps),
    Ps = ["A"-"B",
          "C"-"B"].

test(op_mode,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "A", "B", node("D")),
    nb_add_triple(Builder, "C", "B", node("D")),
    nb_commit(Builder, Layer),
    findall(X, triple(Layer, X, "B", node("D")), Ps),
    Ps = ["A","C"].

test(p_mode,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "A", "B", node("D")),
    nb_add_triple(Builder, "C", "B", node("D")),
    nb_commit(Builder, Layer),
    findall(X, triple(Layer, "A", X, node("D")), Ps),
    Ps = ["B"].

test(rollup,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "A", "B", node("D")),
    nb_add_triple(Builder, "C", "B", node("D")),
    nb_commit(Builder, Layer),

    open_write(Layer, New_Builder),
    nb_add_triple(New_Builder, "E", "F", node("G")),
    nb_remove_triple(New_Builder, "C", "B", node("D")),
    nb_commit(New_Builder, New_Layer),
    rollup(New_Layer),
    layer_to_id(New_Layer, Id),
    store_id_layer(Store, Id, Rollup),
    findall(X-P-Y, triple(Rollup, X, P, node(Y)), Triples),

    Triples = ["A"-"B"-"D","E"-"F"-"G"].

test(rollup_upto,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "A", "B", node("D")),
    nb_add_triple(Builder, "C", "B", node("D")),
    nb_commit(Builder, Layer),

    open_write(Layer, New_Builder),
    nb_add_triple(New_Builder, "E", "F", node("G")),
    nb_remove_triple(New_Builder, "C", "B", node("D")),
    nb_commit(New_Builder, New_Layer),

    open_write(New_Layer, New_Builder_2),
    nb_add_triple(New_Builder_2, "G", "H", node("I")),
    nb_remove_triple(New_Builder_2, "A", "B", node("D")),
    nb_commit(New_Builder_2, New_Layer_2),

    rollup_upto(New_Layer_2, Layer),
    layer_to_id(New_Layer_2, Id),

    store_id_layer(Store, Id, Rollup),
    findall(X-P-Y, triple(Rollup, X, P, node(Y)), Triples),

    Triples = ["E"-"F"-"G","G"-"H"-"I"].

test(layer_stack_names,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "A", "B", node("D")),
    nb_add_triple(Builder, "C", "B", node("D")),
    nb_commit(Builder, Layer),
    layer_to_id(Layer, Layer_Id),

    open_write(Layer, New_Builder),
    nb_add_triple(New_Builder, "E", "F", node("G")),
    nb_remove_triple(New_Builder, "C", "B", node("D")),
    nb_commit(New_Builder, New_Layer),
    layer_to_id(New_Layer, New_Layer_Id),

    open_write(New_Layer, New_Builder_2),
    nb_add_triple(New_Builder_2, "G", "H", node("I")),
    nb_remove_triple(New_Builder_2, "A", "B", node("D")),
    nb_commit(New_Builder_2, New_Layer_2),
    layer_to_id(New_Layer_2, New_Layer_2_Id),

    layer_stack_names(New_Layer_2, Layers),

    Expected = [Layer_Id,New_Layer_Id,New_Layer_2_Id],

    Expected = Layers.

test(precise_rollup_rolls_up_precisely,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "a", "a", value("a")),
    nb_add_triple(Builder, "a", "b", value("a")),
    nb_add_triple(Builder, "c", "c", node("a")),
    nb_commit(Builder, Layer),

    open_write(Layer, Builder2),
    nb_remove_triple(Builder2, "a", "a", value("a")),
    nb_add_triple(Builder2, "c", "b", node("d")),
    nb_commit(Builder2, Layer2),

    open_write(Layer2, Builder3),
    nb_add_triple(Builder3, "c", "c", node("c")),
    nb_remove_triple(Builder3, "c", "b", node("d")),
    nb_commit(Builder3, Layer3),

    open_write(Layer3, Builder4),
    nb_add_triple(Builder4, "a", "a", value("a")),
    nb_add_triple(Builder4, "x", "y", node("z")),
    nb_commit(Builder4, Layer4),

    rollup_upto(Layer3, Layer),

    % lets reload the top layer
    layer_to_id(Layer4, Layer4_Id),
    store_id_layer(Store, Layer4_Id, Layer4_Reloaded),

    % and rollup again
    rollup_upto(Layer4_Reloaded, Layer2),

    % and reload again!
    store_id_layer(Store, Layer4_Id, Layer4_Reloaded_Again),
    
    findall(t(S,P,O), triple(Layer4_Reloaded_Again, S, P, O), Triples),

    Expected = [
        t("a", "a", value("a")),
        t("a", "b", value("a")),
        t("c", "c", node("a")),
        t("c", "c", node("c")),
        t("x", "y", node("z"))
    ],

    Triples = Expected.

test(imprecise_rollup_rolls_up_imprecisely,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "a", "a", value("a")),
    nb_add_triple(Builder, "a", "b", value("a")),
    nb_add_triple(Builder, "c", "c", node("a")),
    nb_commit(Builder, Layer),

    open_write(Layer, Builder2),
    nb_remove_triple(Builder2, "a", "a", value("a")),
    nb_add_triple(Builder2, "c", "b", node("d")),
    nb_commit(Builder2, Layer2),

    open_write(Layer2, Builder3),
    nb_add_triple(Builder3, "c", "c", node("c")),
    nb_remove_triple(Builder3, "c", "b", node("d")),
    nb_commit(Builder3, Layer3),

    open_write(Layer3, Builder4),
    nb_add_triple(Builder4, "a", "a", value("a")),
    nb_add_triple(Builder4, "x", "y", node("z")),
    nb_commit(Builder4, Layer4),

    rollup_upto(Layer3, Layer),

    % lets reload the top layer
    layer_to_id(Layer4, Layer4_Id),
    store_id_layer(Store, Layer4_Id, Layer4_Reloaded),

    % and rollup again
    imprecise_rollup_upto(Layer4_Reloaded, Layer2),

    % and reload again!
    store_id_layer(Store, Layer4_Id, Layer4_Reloaded_Again),
    
    findall(t(S,P,O), triple(Layer4_Reloaded_Again, S, P, O), Triples),

    Expected = [
        t("a", "a", value("a")),
        t("a", "b", value("a")),
        t("c", "c", node("a")),
        t("c", "c", node("c")),
        t("x", "y", node("z"))
    ],

    Triples = Expected.

test(sp_card,[cleanup(clean(TestDir)), setup(createng(TestDir))]) :-
    open_directory_store(TestDir, Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, "A", "B", node("C")),
    nb_add_triple(Builder, "A", "B", node("D")),
    nb_commit(Builder, Layer),
    subject_id(Layer, "A", A_Id),
    predicate_id(Layer, "B", B_Id),
    sp_card(Layer, A_Id, B_Id, Count),
    Count = 2.

:- end_tests(terminus_store).
