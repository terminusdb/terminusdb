:- module(api_rollup, [api_rollup/5]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(apply)).

/* api_rollup(System_DB, Auth, Path, Options, Status_List) */

api_rollup(System_DB, Auth, Path, _Options, Status_List) :-
    do_or_die(resolve_absolute_string_descriptor(Path, Descriptor),
              error(invalid_absolute_path(Path), _)),

    do_or_die(
        askable_context(Descriptor, System_DB, Auth, Context),
        error(unresolvable_collection(Descriptor), _)),

    assert_write_access(Context),
    get_dict(transaction_objects, Context, [Transaction]),

    database_instance(Transaction, Instances),
    database_schema(Transaction, Schema),
    database_inference(Transaction, Inference),

    append([Instances, Schema, Inference], Read_Write_Objects),

    maplist([Read_Write_Object, success(Desc)]>>(
                read_write_obj_reader(Read_Write_Object, Layer),
                get_dict(descriptor, Read_Write_Object, Desc),
                rollup(Layer)
            ),
            Read_Write_Objects,
            Status_List).
