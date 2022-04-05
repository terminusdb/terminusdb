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

document_from_version_header(Branch_Descriptor, Version_Header, Doc_Id, Document, Map_In, Map_Out) :-
    do_or_die(
        read_data_version(Version_Header, data_version(branch, Commit_Id)),
        error(bad_data_version(Version_Header), _)
    ),

    resolve_relative_descriptor(Branch_Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),

    do_or_die(
        open_descriptor(Commit_Descriptor, commit_info{}, Transaction, Map_In, Map_Out),
        error(unresolvable_collection(Commit_Descriptor),_)),

    get_document(Transaction, Doc_Id, Document).


api_diff_id(System_DB, Auth, Path, Before_Version, After_Version, Doc_Id, Keep, Diff) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),

    document_from_version_header(Branch_Descriptor, Before_Version, Doc_Id, Before, [], Map),
    document_from_version_header(Branch_Descriptor, After_Version, Doc_Id, After, Map, _),

    simple_diff(Before,After,Keep,Diff).

api_diff_id_document(System_DB, Auth, Path, Before_Version, After_Document, Doc_Id, Keep, Diff) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),

    document_from_version_header(Branch_Descriptor, Before_Version, Doc_Id, Before, [], _),

    simple_diff(Before,After_Document,Keep,Diff).
