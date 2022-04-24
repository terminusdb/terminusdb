:- module(api_patch, [api_patch/5,api_diff/6,api_diff_id/8, api_diff_id_document/8]).

:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).

api_patch(_System_DB, _Auth, Patch, Before, After) :-
    % no auth yet.
    simple_patch(Patch,Before,After).

api_diff(_System_DB, _Auth, Before, After, Keep, Diff) :-
    % no auth yet.
    do_or_die((ground(Before),
               ground(After)),
              error(unground_patch, _)),
    simple_diff(Before,After,Keep,Diff).

coerce_to_commit(Commit_Or_Version, Commit_Id) :-
    (   read_data_version(Commit_Or_Version, data_version(Type, Commit_Id))
    ->  do_or_die(
            (   branch = Type
            ;   commit = Type),
            error(bad_data_version(Commit_Or_Version), _)
        )
    ;   Commit_Id = Commit_Or_Version
    ).

document_from_commit(Branch_Descriptor, Commit_Id, Doc_Id, Document, Transaction,
                     Map_In, Map_Out) :-
    resolve_relative_descriptor(Branch_Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),

   do_or_die(
       open_descriptor(Commit_Descriptor, commit_info{}, Transaction, Map_In, Map_Out),
       error(unresolvable_collection(Commit_Descriptor),_)),

    get_document(Transaction, Doc_Id, Document).


api_diff_id(System_DB, Auth, Path, Before_Version, After_Version, Doc_Id, Keep, Diff) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),

    coerce_to_commit(Before_Version, Before_Commit_Id),
    coerce_to_commit(After_Version, After_Commit_Id),

    document_from_commit(Branch_Descriptor, Before_Commit_Id, Doc_Id, Before, _, [], Map),
    document_from_commit(Branch_Descriptor, After_Commit_Id, Doc_Id, After, _, Map, _),

    simple_diff(Before,After,Keep,Diff).

api_diff_id_document(System_DB, Auth, Path, Before_Version, After_Document, Doc_Id, Keep, Diff) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),

    coerce_to_commit(Before_Version, Before_Commit_Id),

    document_from_commit(Branch_Descriptor, Before_Commit_Id, Doc_Id, Before, Transaction, [], _),

    normalize_document(Transaction, After_Document, Normal_Document),
    simple_diff(Before,Normal_Document,Keep,Diff).
