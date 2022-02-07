:- module(metadata, [
              read_object_size/2,
              transaction_object_size/2,
              read_object_triple_count/2,
              transaction_object_triple_count/2
          ]).

:- use_module(library(terminus_store)).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(plunit)).
:- use_module(core(transaction)).
:- use_module(core(util)).

transaction_object_read_write_objs(Transaction, Objects) :-
    Instance_Objects = (Transaction.instance_objects),
    Schema_Objects = (Transaction.schema_objects),
    Inference_Objects = (Transaction.schema_objects),
    append([Instance_Objects,Schema_Objects,Inference_Objects], Objects).

transaction_object_triple_count(Transaction, Triple_Count) :-
    transaction_object_read_write_objs(Transaction,Objects),
    maplist(read_object_triple_count,Objects,Counts),
    sum_list(Counts,Triple_Count).

read_object_triple_count(Object, Count) :-
    Layer = (Object.read),
    (   var(Layer)
    ->  Count = 0
    ;   layer_total_triple_count(Layer, Count)).

transaction_object_size(Transaction, Size) :-
    transaction_object_read_write_objs(Transaction,Objects),
    maplist(read_object_size, Objects, Sizes),
    sum_list(Sizes, Size).

read_object_size(Object, Size) :-
    Layer = (Object.read),
    (   var(Layer)
    ->  Size = 0
    ;   layer_stack_names(Layer, Names),
        maplist(layer_size,Names,Sizes),
        sum_list(Sizes,Size)).

% NOTE: Stub! Should be in store.
layer_directory_prefix_length(3).

% NOTE: Stub! layer_size Should be in store.
layer_size(Layer_Name,Size) :-
    layerid_to_directory(Layer_Name,Directory),
    size_directory(Directory,Size).

:- thread_local current_db_path_pred/1.
current_db_path(Path) :-
    current_db_path_pred(Path),
    !.
current_db_path(Path) :-
    db_path(Path).

set_current_db_path(Path) :-
    atomic_list_concat([Path,'/'],Directory),
    asserta(current_db_path_pred(Directory)).

unset_current_db_path :-
    retractall(current_db_path_pred(_)).

% layerid_to_directory
layerid_to_directory(Layer_ID, Directory) :-
    current_db_path(Path),
    layer_directory_prefix_length(Length),
    sub_string(Layer_ID, 0, Length, _, Prefix),
    atomic_list_concat([Path,Prefix,'/',Layer_ID], Directory).

size_directory(Directory,Size) :-
    directory_files(Directory,Files),
    exclude([File]>>member(File,['.','..']),Files,Plain_Files),
    maplist({Directory}/[File,Path]>>
            atomic_list_concat([Directory,'/',File],Path),
            Plain_Files,Paths),
    maplist(size_file,Paths,Sizes),
    sum_list(Sizes,Size).

:- begin_tests(metadata).
:- use_module(ask).
:- use_module(resolve_query_resource).
:- use_module(core(util/test_utils)).

test(count_and_size, [
         setup((setup_temp_store(State),
                State = _-Path,
                metadata:set_current_db_path(Path),
                create_db_without_schema("admin", "test"))),
         cleanup((metadata:unset_current_db_path,
                  teardown_temp_store(State)))
     ]) :-

    resolve_absolute_string_descriptor("admin/test", Descriptor),
    Commit_Info = commit_info{ author : "test", message : "testing semantics"},
    create_context(Descriptor, Commit_Info, Context),

    with_transaction(
        Context,
        ask(Context,
            (
                insert(a, b, c),
                insert(d, e, f),
                insert(d, e, a),
                insert(f, g, h),
                insert(h, j, k),
                insert(h, j, "asdfasflaksdjflaksdjf;laksdjflaksdfjlaskdjfa;lskdfja;lskdfja;lskdfj;aslkfdj;lskjf;laskjdf"^^xsd:string)
            )),
        _Meta_Data
    ),

    open_descriptor(Descriptor, Transaction),
    transaction_object_size(Transaction, Size),
    transaction_object_triple_count(Transaction, 14),

    Size > 0,
    Size < 10000.

:- end_tests(metadata).
