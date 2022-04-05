:- module(api_patch, [api_patch/5,api_diff/6,api_diff_id/8]).

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

api_diff_id(System_DB, Auth, Path, Before_Version, After_Version, Doc_Id, Keep, Diff) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),

    do_or_die(
        read_data_version(Before_Version, data_version(branch, Before_Commit_Id)),
        error(bad_data_version(Before_Version), _)
    ),
    do_or_die(
        read_data_version(After_Version, data_version(branch, After_Commit_Id)),
        error(bad_data_version(After_Version), _)
    ),

    resolve_relative_descriptor(Branch_Descriptor,
                                ["commit", Before_Commit_Id],
                                Before_Commit_Descriptor),
    resolve_relative_descriptor(Branch_Descriptor,
                                ["commit", After_Commit_Id],
                                After_Commit_Descriptor),
    do_or_die(
        open_descriptor(Before_Commit_Descriptor, commit_info{}, Before_Transaction, [], After_Map),
        error(unresolvable_collection(Before_Commit_Descriptor),_)),
    do_or_die(
        open_descriptor(After_Commit_Descriptor, commit_info{}, After_Transaction, After_Map, _),
        error(unresolvable_collection(After_Commit_Descriptor),_)),

    get_document(Before_Transaction, Doc_Id, Before),
    get_document(After_Transaction, Doc_Id, After),

    simple_diff(Before,After,Keep,Diff).
