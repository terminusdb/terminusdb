:- module(api_rollup, [api_rollup/4]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).

/* api_rollup(System_DB, Auth, Path, Option) */

api_rollup(System_DB, Auth, Path, none) :-
    do_or_die(resolve_absolute_string_descriptor(Path, Descriptor),
              error(invalid_absolute_path(Path), _)),

    do_or_die(
        askable_context(Descriptor, System_DB, Auth, Context),
        error(unresolvable_collection(Descriptor), _)),

    assert_write_access(Context),
    writeq(hello),nl,
    writeq(Context),
    get_dict(transaction_objects, Context, [Transaction]),
    writeq(olleh),nl,
    database_instance(Transaction, Instances),
    database_schema(Transaction, Schema),
    database_inference(Transaction, Inference),

    append([Instances, Schema, Inference], Read_Write_Objects),

    forall((   member(Read_Write_Object, Read_Write_Objects),
               read_write_obj_reader(Read_Write_Object, Layer)),
           rollup(Layer)).


:- begin_tests(rollup).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

:- end_tests(rollup).
