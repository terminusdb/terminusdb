:- module(api_patch, [api_patch/5,api_diff/6,api_diff_id/8, api_diff_id_document/8,api_diff_all_documents/7]).

:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).

:- use_module(library(solution_sequences)).
:- use_module(library(lists)).

api_patch(_System_DB, _Auth, Patch, Before, After) :-
    % no auth yet.
    simple_patch(Patch,Before,After,[match_final_state(true)]).

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

changed_id(Transaction,Containing) :-
    ask(Transaction,
        distinct(Containing,
                 (   distinct(Id, (   addition(Id, _, _)
                                  ;   removal(Id, _, _),
                                      once(t(Id, _, _))
                                  )),
                     once((path(Containing, star(p), Id),
                           t(Containing,rdf:type,Type),
                           (   t(Type,rdf:type,sys:'Class',schema)
                           ;   t(Type,rdf:type,sys:'TaggedUnion',schema)),
                           not(t(Type,sys:subdocument, _,schema))))
                 ;   removal(Id, rdf:type, Type),
                     once(((   t(Type,rdf:type,sys:'Class',schema)
                           ;   t(Type,rdf:type,sys:'TaggedUnion',schema)),
                           not(t(Type,sys:subdocument, _,schema))))
                 )
                )
       ).

commits_changed_id(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Changed) :-
    create_context(Branch_Descriptor.repository_descriptor, Context),
    most_recent_common_ancestor(Context, Context,
                                Before_Commit_Id, After_Commit_Id,
                                _Shared_Commit_Id, Commit1_Path, Commit2_Path),

    distinct(Changed,
             (   union(Commit1_Path, Commit2_Path, All_Commits),
                 member(Commit_Id, All_Commits),
                 resolve_relative_descriptor(Branch_Descriptor,
                                             ["commit", Commit_Id],
                                             Commit_Descriptor),
                 do_or_die(
                     open_descriptor(Commit_Descriptor, Transaction),
                     error(unresolvable_collection(Commit_Descriptor), _)),
                 changed_id(Transaction, Changed)
             )).

document_diffs_from_commits(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Keep, Diff) :-
    commits_changed_id(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Doc_Id),
    (   document_from_commit(Branch_Descriptor, Before_Commit_Id, Doc_Id, Before, _, [], Map)
    ->  (   document_from_commit(Branch_Descriptor, After_Commit_Id, Doc_Id, After, _, Map, _)
        ->  simple_diff(Before,After,Keep,Diff)
        ;   Diff = _{ '@op' : 'Delete',
                      '@delete' : Before }
        )
    ;   (   document_from_commit(Branch_Descriptor, After_Commit_Id, Doc_Id, After, _, [], _)
        ->  Diff = _{ '@op' : 'Insert',
                      '@insert' : After }
        ;   fail)
    ),
    \+ patch_cost(Diff, 0).

api_diff_all_documents(System_DB, Auth, Path, Before_Version, After_Version, Keep, Diffs) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),
    coerce_to_commit(Before_Version, Before_Commit_Id),
    coerce_to_commit(After_Version, After_Commit_Id),

    findall(Diff,
            document_diffs_from_commits(Branch_Descriptor,
                                        Before_Commit_Id,
                                        After_Commit_Id,
                                        Keep,
                                        Diff),
            Diffs
           ).

api_apply_squash_commit(System_DB, Auth, Path, Commit_Info, Before_Version, After_Version, Options) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),
    coerce_to_commit(Before_Version, Before_Commit_Id),
    coerce_to_commit(After_Version, After_Commit_Id),

    create_context(Path, Commit_Info, Context),
    with_transaction(
        Context,
        (   findall(Witness,
                    (   document_diffs_from_commits(Branch_Descriptor,
                                                    Before_Commit_Id,
                                                    After_Commit_Id,
                                                    _{'@id':true, '@type':true},
                                                    Diff),
                        apply_diff(Context,Diff,Witness,Options),
                        \+ Witness = null
                    ),
                    Witnesses),
            (   Witnesses = []
            ->  true
            ;   throw(error(apply_squash_witnesses(Witnesses)))
            )
        ),
        _
    ).
