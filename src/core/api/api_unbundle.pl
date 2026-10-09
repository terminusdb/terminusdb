:- module(api_unbundle, [unbundle/4]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(api/api_remote)).
:- use_module(core(api/db_pull)).
:- use_module(db_pack).

:- use_module(library(yall)).
:- use_module(library(md5)).
:- use_module(library(apply)).

unbundle(System_DB, Auth, Path, Payload) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Branch_Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        (branch_descriptor{} :< Branch_Descriptor),
        error(push_requires_branch(Branch_Descriptor),_)),

    setup_call_cleanup(
        % 1. create repo
        (   random_string(String),
            md5_hash(String, Remote_Name_Atom, []), % 32 chars.
            atom_string(Remote_Name_Atom,Remote_Name),
            add_remote(System_DB, Auth, Path, Remote_Name, "terminusdb:///bundle")
        ),
        % 2. pull from repo with fake remote predicate
        (   fabricate_missing_fringe_layers(Payload),
            pull(System_DB, Auth, Path, Remote_Name, "main",
                 {Payload}/[_URL,_Repository_Head_Option,some(P)]>>(
                     Payload = P),
                 _Result
            )
        ),
        % 3. remove repo
        remove_remote(System_DB, Auth, Path, Remote_Name)
    ).

% Bundles produced by older versions are missing the fake repository
% head layer that the bundle's head layer refers to as its parent.
% That layer was created as an empty base layer in the source store,
% so it can be recreated here exactly. Any other missing fringe layer
% is left for the normal unpack fringe check to reject.
fabricate_missing_fringe_layers(Payload) :-
    payload_repository_head_and_pack(Payload, _Head, Pack),
    pack_layerids_and_parents(Pack, Layer_Parents),
    layerids_and_parents_fringe(Layer_Parents, Fringe),
    exclude(layer_exists, Fringe, Missing_Fringe),
    triple_store(Store),
    maplist(create_empty_base_layer(Store), Missing_Fringe).
