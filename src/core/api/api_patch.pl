:- module(api_patch, [
              api_patch_resource/7,
              api_patch/6,
              api_diff/6,
              api_diff_id/8,
              api_diff_id_document/8,
              api_diff_all_documents/7,
              api_apply_squash_commit/7,
              api_check_apply_squash_commit/8,
              squash_apply_access/5
          ]).

:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(api/api_document), [
                  idlists_duplicates_toplevel/3,
                  nonground_captures/2,
                  compress_doc_ids/4,
                  check_validation_witnesses/3,
                  compress_witness_json/3
              ]).
:- use_module(core(document/apply), [apply_diff_ids_captures/7]).
:- use_module(core(document/migration),
              [create_class_dictionary/2, plan_schema_migration/4, stage_schema_migration/4]).
:- use_module(core(transaction/database),
              [pre_transaction_tabling/0, post_transaction_tabling/0]).
:- use_module(core(document/history),
              [changed_document_id/2,
               commits_changed_id/5
              ]).
:- use_module(library(solution_sequences)).
:- use_module(library(lists)).
:- use_module(library(dicts), [dict_keys/2]).
:- use_module(library(json), [atom_json_dict/3]).
:- use_module(library(terminus_store), [layer_to_id/2]).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).
:- use_module(library(option)).
:- use_module(library(error), [must_be/2]).
:- use_module(library(assoc)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).

api_patch(System_DB, Auth, Patch, Before, After, Options) :-
    % No path to scope a descriptor check, so require the caller to
    % hold instance_read_access somewhere; anonymous qualifies only on
    % deployments with a public database.
    assert_auth_action_scope(System_DB, Auth,
                             '@schema':'Action/instance_read_access', _),
    simple_patch(Patch,Before,Result0,Options),
    % Wire ^^ tags are parse metadata for storage typing; the response is
    % plain JSON, so strip them before rendering (storage paths keep them).
    strip_wire_hints(Result0, After).

patch_id(Patch, Id) :-
    get_dict('@insert', Patch, Insert_Doc), !,
    (   get_dict('@id', Insert_Doc, Id)
    ->  true
    ;   get_dict('@capture', Insert_Doc, Capture)
    ->  atom_string(Id, Capture)
    ;   throw(error(no_id_in_patch(Patch), _))
    ).
patch_id(Patch, Id) :-
    get_dict('@delete', Patch, Delete_Candidate), !,
    (   is_dict(Delete_Candidate)
    ->  do_or_die(get_dict('@id', Delete_Candidate, Id),
                  error(no_id_in_patch(Patch), _))
    ;   (string(Delete_Candidate) ; atom(Delete_Candidate))
    ->  Delete_Candidate = Id
    ;   throw(error(no_id_in_patch(Patch), _))
    ).
patch_id(Patch, Id) :-
    do_or_die(
        get_dict('@id', Patch, Id),
        error(no_id_in_patch(Patch), _)
    ).

patch_id_pairs(Patch, Patch_And_Ids) :-
    (   is_list(Patch)
    ->  findall(P-Id,
                (
                    member(P, Patch),
                    patch_id(P, Id)
                ),
                Patch_And_Ids)
    ;   patch_id(Patch, Id),
        Patch_And_Ids = [Patch-Id]
    ).

map_apply_captures(Context,Options,Patch_And_Ids,Conflicts,Ids_List,Empty,Captures) :-
    mapm({Context, Options}/[Patch-_,Conflict,Ids,C1,C2]>>(
             apply_diff_ids_captures(Context, Patch, Conflict, Ids, Options, C1, C2)
         ),
         Patch_And_Ids,
         Conflicts,
         Ids_List,
         Empty,
         Captures
        ).

api_patch_resource(System_DB, Auth, Path, Patch, Commit_Info, Ids, Options) :-
    resolve_descriptor_auth(write, System_DB, Auth, Path, instance, Branch_Descriptor),
    create_context(Branch_Descriptor, Commit_Info, Context),
    merge_options(Options, options{keep:json{'@id':true, '@type':true}}, Merged_Options),
    empty_assoc(Empty),
    with_transaction(
        Context,
        (
            patch_id_pairs(Patch, Patch_Ids),
            map_apply_captures(Context,Merged_Options,Patch_Ids,Conflicts,Ids_List,Empty,Captures),
            !,
            die_if(nonground_captures(Captures, Nonground),
                   error(not_all_captures_found(Nonground), _)),

            exclude([null]>>true, Conflicts, Witnesses),
            (   Witnesses = []
            ->  true
            ;   throw(error(patch_conflicts(Witnesses)))
            ),
            exclude(=([]), Ids_List, Non_Empty_Ids_List),
            idlists_duplicates_toplevel(Non_Empty_Ids_List, Duplicates, Raw_Ids),
            die_if(Duplicates \= [], error(same_ids_in_one_transaction(Duplicates), _)),
            compress_doc_ids(true, Context, Raw_Ids, Ids)
        ),
        _
    ).

api_diff(System_DB, Auth, Before, After, Diff, Options) :-
    % Same existential read gate as api_patch/6.
    assert_auth_action_scope(System_DB, Auth,
                             '@schema':'Action/instance_read_access', _),
    do_or_die((ground(Before),
               ground(After)),
              error(unground_patch, _)),
    do_or_die(simple_diff(Before,After,Diff,Options),
              error(unable_to_diff_documents, _)).

coerce_to_commit(Branch_Descriptor, Commit_Or_Version, Commit_Id) :-
    do_or_die(
        (   read_data_version(Commit_Or_Version, data_version(Type, Commit_Id))
        ->  do_or_die(
                (   branch = Type
                ;   commit = Type),
                error(bad_data_version(Commit_Or_Version), _)
            )
        ;   resolve_relative_descriptor(Branch_Descriptor,
                                        ["_commits"],
                                        Ref_Descriptor)
        ->  open_descriptor(Ref_Descriptor, T),
            (   commit_id_uri(T,
                              Commit_Or_Version,
                              _)
            ->  Commit_Id = Commit_Or_Version
            ;   branch_head_commit(T,
                                   Commit_Or_Version,
                                   Commit_Uri),
                commit_id_uri(T, Commit_Id, Commit_Uri))
        ;   Commit_Id = Commit_Or_Version
        ),
        error(not_a_valid_commit_or_branch(Commit_Or_Version), _)
    ).

document_from_commit(Branch_Descriptor, Commit_Id, Doc_Id, Document, Transaction,
                     Map_In, Map_Out) :-
    document_from_commit(Branch_Descriptor, Commit_Id, Doc_Id, Document, Transaction,
                         Map_In, Map_Out, options{}).

document_from_commit(Branch_Descriptor, Commit_Id, Doc_Id, Document, Transaction,
                     Map_In, Map_Out, Options) :-
    resolve_relative_descriptor(Branch_Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),

    do_or_die(
        open_descriptor(Commit_Descriptor, commit_info{}, Transaction, Map_In, Map_Out),
        error(unresolvable_collection(Commit_Descriptor),_)),

    commit_doc_options(Options, Get_Document_Options),
    get_document(Transaction, Doc_Id, Document, Get_Document_Options).

api_diff_id(System_DB, Auth, Path, Before_Version, After_Version, Doc_Id, Diff, Options) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),
    coerce_to_commit(Branch_Descriptor, Before_Version, Before_Commit_Id),
    coerce_to_commit(Branch_Descriptor, After_Version, After_Commit_Id),

    (   document_from_commit(Branch_Descriptor, Before_Commit_Id, Doc_Id, Before, _, [], Map, Options)
    ->  (   document_from_commit(Branch_Descriptor, After_Commit_Id, Doc_Id, After, _, Map, _, Options)
        ->  do_or_die(simple_diff(Before,After,Diff,Options),
                      error(unable_to_diff_document(Doc_Id), _))
        ;   Diff = json{ '@op' : "Delete",
                         '@delete' : Before }
        )
    ;   (   document_from_commit(Branch_Descriptor, After_Commit_Id, Doc_Id, After, _, [], _, Options)
        ->  Diff = json{ '@op' : "Insert",
                         '@insert' : After }
        ;   fail)
    ).

api_diff_id_document(System_DB, Auth, Path, Before_Version, After_Document, Doc_Id, Diff, Options) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),
    coerce_to_commit(Branch_Descriptor, Before_Version, Before_Commit_Id),

    document_from_commit(Branch_Descriptor, Before_Commit_Id, Doc_Id, Before, Transaction, [], _, Options),

    normalize_document(Transaction, After_Document, Normal_Document),
    do_or_die(simple_diff(Before,Normal_Document,Diff,Options),
              error(unable_to_diff_document(Doc_Id), _)).

commits_changed_id(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Changed) :-
    commits_changed_id(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Changed, options{}).

%% commit_doc_options(+Options, -Doc_Options) is det.
%
%  The fixed option set get_document sees when reading a document at a
%  commit — shared by enumeration and theirs-resolution.
commit_doc_options(Options, Doc_Options) :-
    option(unfold(Unfold), Options, true),
    must_be(boolean, Unfold),
    Doc_Options = options{compress_ids:true,
                          unfold:Unfold,
                          keep_json_type:true}.

%% open_commit_transaction(+Branch_Descriptor, +Commit_Id, -Transaction)
%
%  Opens the commit's read transaction once — callers reuse it across
%  every changed document instead of reopening the descriptor per doc.
open_commit_transaction(Branch_Descriptor, Commit_Id, Transaction) :-
    resolve_relative_descriptor(Branch_Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    open_diff_commit(Commit_Descriptor, Transaction).

open_diff_commit(Commit_Descriptor, Transaction) :-
    do_or_die(
        open_descriptor(Commit_Descriptor, Transaction),
        error(unresolvable_collection(Commit_Descriptor),_)).

open_apply_commit(System_DB, Auth, Branch_Descriptor, Commit_Id, Transaction) :-
    resolve_relative_descriptor(Branch_Descriptor, ["commit", Commit_Id], Descriptor),
    % Ordinary graph access is checked on the destination; protected commits also gate reads.
    assert_read_access(System_DB, Auth, Descriptor, type_filter{types:[]}),
    open_diff_commit(Descriptor, Transaction).

document_diffs_from_commits(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Diff, Options) :-
    commit_doc_options(Options, Doc_Options),
    once(open_commit_transaction(Branch_Descriptor, Before_Commit_Id,
                                 Before_Transaction)),
    once(open_commit_transaction(Branch_Descriptor, After_Commit_Id,
                                 After_Transaction)),
    document_diffs_from_transactions(Branch_Descriptor, Before_Commit_Id,
                                     After_Commit_Id, Before_Transaction,
                                     After_Transaction, Doc_Options, Diff,
                                     Options).

document_diffs_from_transactions(Branch_Descriptor, Before_Commit_Id,
                                 After_Commit_Id, Before_Transaction,
                                 After_Transaction, Doc_Options, Diff,
                                 Options) :-
    option(start(Start), Options),
    option(count(Count), Options),
    limit(
        Count,
        offset(
            Start,
            (   commits_changed_id(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Doc_Id),
                (   get_diff_document(Before_Transaction, Doc_Id, Before, Doc_Options)
                ->  (   get_diff_document(After_Transaction, Doc_Id, After, Doc_Options)
                    ->  do_or_die(simple_diff(Before,After,Diff0,Options),
                                  error(unable_to_diff_document(Doc_Id),
                                        _)),
                        (   option(inject_subdocument_ids(true), Options)
                        ->  diff_with_subdocument_ids(Diff0, Before, Diff)
                        ;   Diff = Diff0
                        )
                    ;   Diff = json{ '@op' : "Delete",
                                     '@delete' : Before }
                    )
                ;   (   get_diff_document(After_Transaction, Doc_Id, After, Doc_Options)
                    ->  Diff = json{ '@op' : "Insert",
                                     '@insert' : After }
                    ;   fail)
                ),
                \+ patch_cost(Diff, 0)
            )
        )
    ).

get_diff_document(Transaction, Id, Document, Options) :-
    (   get_dict(apply_prefixes, Options, Prefixes)
    ->  get_document(Transaction, Prefixes, Id, Document, Options)
    ;   get_document(Transaction, Id, Document, Options)
    ).

api_diff_all_documents(System_DB, Auth, Path, Before_Version, After_Version, Diffs, Options) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),
    coerce_to_commit(Branch_Descriptor, Before_Version, Before_Commit_Id),
    coerce_to_commit(Branch_Descriptor, After_Version, After_Commit_Id),
    findall(Diff,
            document_diffs_from_commits(Branch_Descriptor,
                                        Before_Commit_Id,
                                        After_Commit_Id,
                                        Diff,
                                        Options),
            Diffs
           ).

api_apply_squash_commit(System_DB, Auth, Path, Commit_Info, Before_Version, After_Version, Options) :-
    squash_apply_setup(System_DB, Auth, Path, Commit_Info, Before_Version,
                       After_Version, Options, Context, Source, Mode, Merged_Options),
    with_transaction(
        Context,
        (   squash_merge_witnesses(System_DB, Auth, Path, Context, Source,
                                   Mode, Merged_Options, Witnesses),
            (   Witnesses = []
            ->  true
            ;   throw(error(apply_squash_witnesses(Witnesses)))
            )
        ),
        _,
        [skip_migration_inference(true)]
    ).

/* api_check_apply_squash_commit — dry-run for POST /api/apply.
 *
 *  Runs the same merge against a context whose transaction is never run:
 *  the uncommitted layer is discarded when the call returns, so the
 *  branch head does not move. Witnesses carry @expected/@found so a
 *  caller can render both sides of a conflict before committing.
 */
api_check_apply_squash_commit(System_DB, Auth, Path, Commit_Info, Before_Version, After_Version, Options, Result) :-
    squash_apply_setup(System_DB, Auth, Path, Commit_Info, Before_Version,
                       After_Version, Options, Context, Source, Mode, Merged_Options),
    once(query_context_transaction_objects(Context, [Transaction])),
    database_prefixes(Transaction, Prefixes),
    setup_call_cleanup(
        pre_transaction_tabling,
        once(catch(
            (   squash_merge_witnesses(System_DB, Auth, Path, Context, Source, Mode,
                                       Merged_Options.put(_{dry_run:true}),
                                       Merge_Witnesses),
                check_validation_witnesses(Context, [skip_migration_inference(true)|Options],
                                           Validation_Witnesses0),
                maplist(compress_witness_json(Prefixes),
                        Validation_Witnesses0, Validation_Witnesses),
                append(Merge_Witnesses, Validation_Witnesses, Witnesses),
                check_apply_result(Witnesses, Result)
            ),
            error(schema_check_failure(Witnesses0), _),
            (   maplist(compress_witness_json(Prefixes),
                        Witnesses0, Witnesses),
                check_apply_result(Witnesses, Result)))),
        post_transaction_tabling).

check_apply_result(Witnesses, Result) :-
    (   Witnesses = []
    ->  Result = json{'@type':'api:CheckResponse',
                      'api:valid':true,
                      'api:witnesses':[]}
    ;   Result = json{'@type':'api:CheckResponse',
                      'api:valid':false,
                      'api:witnesses':Witnesses}).

squash_apply_setup(System_DB, Auth, Path, Commit_Info, Before_Version,
                   After_Version, Options, Context, Source, Mode, Merged_Options) :-
    option(type(Type), Options, squash),
    do_or_die(Type = squash,
              error(bad_apply_type(Type), _)),
    option(graph_type(Mode), Options, both),
    do_or_die((atom(Mode), memberchk(Mode, [both, schema, instance])),
              error(bad_apply_graph_type(Mode), _)),
    squash_apply_access(Mode, System_DB, Auth, Path, Branch_Descriptor),
    do_or_die(is_dict(Branch_Descriptor, branch_descriptor),
              error(unexpected_descriptor_type(Branch_Descriptor, branch_descriptor), _)),
    coerce_to_commit(Branch_Descriptor, Before_Version, Before_Commit_Id),
    coerce_to_commit(Branch_Descriptor, After_Version, After_Commit_Id),
    create_context(Branch_Descriptor.repository_descriptor, Repo_Context),
    ref_entity:commit_id_uri(Repo_Context, After_Commit_Id, After_Commit_Uri),
    put_dict(merge_source, Commit_Info,
             _{operation: apply, commits: [After_Commit_Uri]},
             Commit_Info_With_Provenance),
    create_context(Branch_Descriptor, Commit_Info_With_Provenance, Context),
    once(open_apply_commit(System_DB, Auth, Branch_Descriptor, Before_Commit_Id, Before)),
    once(open_apply_commit(System_DB, Auth, Branch_Descriptor, After_Commit_Id, After)),
    Source = source{before_id:Before_Commit_Id, after_id:After_Commit_Id,
                    before:Before, after:After},
    merge_options(Options, options{keep:json{'@id':true, '@type':true},
                                   count:inf,
                                   start:0,
                                   match_final_state:true,
                                   conflict_resolution:error,
                                   inject_subdocument_ids:true}, Merged_Options).

squash_apply_access(instance, System_DB, Auth, Path, Descriptor) :-
    resolve_descriptor_auth(write, System_DB, Auth, Path, instance, Descriptor).
squash_apply_access(schema, System_DB, Auth, Path, Descriptor) :-
    catch(resolve_descriptor_auth(write, System_DB, Auth, Path, schema, Descriptor),
          error(access_not_authorised(_, _, _), _),
          resolve_descriptor_auth(read, System_DB, Auth, Path, schema, Descriptor)).
squash_apply_access(both, System_DB, Auth, Path, Descriptor) :-
    catch(resolve_descriptor_auth(write, System_DB, Auth, Path, instance, Descriptor),
          error(access_not_authorised(_, _, _), _),
          resolve_descriptor_auth(write, System_DB, Auth, Path, schema, Descriptor)).

same_apply_layer(Graph, Before, After) :-
    get_dict(Graph, Before, [Before_Object]),
    get_dict(Graph, After, [After_Object]),
    (   nonvar(Before_Object.read), nonvar(After_Object.read)
    ->  layer_to_id(Before_Object.read, Id),
        layer_to_id(After_Object.read, Id)
    ;   var(Before_Object.read), var(After_Object.read)
    ).

squash_merge_witnesses(System_DB, Auth, Path, Context, Source, Mode, Options, Witnesses) :-
    (   Mode \== schema,
        \+ same_apply_layer(instance_objects, Source.before, Source.after)
    ->  resolve_descriptor_auth(write, System_DB, Auth, Path, instance, _),
        Apply_Instances = true
    ;   Apply_Instances = false
    ),
    (   Mode \== instance,
        \+ same_apply_layer(schema_objects, Source.before, Source.after)
    ->  squash_apply_schema(System_DB, Auth, Path, Context, Source, Options)
    ;   true
    ),
    normalize_conflict_resolution(Options.conflict_resolution, Context, Resolution),
    (   Apply_Instances = true
    ->  squash_instance_witnesses(Context, Source, Resolution, Options, Witnesses)
    ;   Witnesses = []
    ).

squash_apply_schema(System_DB, Auth, Path, Context, Source, Options) :-
    create_class_dictionary(Source.before, Base),
    create_class_dictionary(Source.after, Incoming),
    query_default_collection(Context, Target_Transaction),
    create_class_dictionary(Target_Transaction, Target),
    do_or_die(
        merge_schema_dict(Base, Incoming, Options.match_final_state, document,
                          Target-false, Merged-Already_Present),
        error(apply_schema_delta(Source.before_id, Source.after_id), _)),
    (   Target =@= Merged
    ->  true
    ;   resolve_descriptor_auth(write, System_DB, Auth, Path, schema, _),
        create_context(Target_Transaction.parent, Repo),
        squash_recorded_migrations(Repo, Source, Recorded),
        (   plan_schema_migration(Target, Merged, Recorded, Operations)
        ->  true
        ;   Already_Present == true, Recorded \== []
        ->  throw(error(apply_partial_recorded_migration(Source.before_id, Source.after_id), _))
        ;   throw(error(apply_unsupported_schema_migration, _))
        ),
        do_or_die(stage_schema_migration(Target_Transaction, Operations, Merged, Staged),
                  error(apply_unsupported_schema_migration, _)),
        atom_json_dict(Migration, Operations, [default_tag(json), width(0)]),
        create_context(Staged, Context.commit_info.put(migration, Migration), Staged_Context),
        % Backtrackable updates keep retries on the original branch transaction.
        b_set_dict(commit_info, Context, Staged_Context.commit_info),
        b_set_dict(prefixes, Context, Staged_Context.prefixes),
        b_set_dict(transaction_objects, Context, Staged_Context.transaction_objects)
    ).

squash_recorded_migrations(Repo, Source, Operations) :-
    (   most_recent_common_ancestor(Repo, Repo, Source.before_id, Source.after_id,
                                   _, [], Commits),
        maplist(squash_recorded_migration(Repo), Commits, Migrations)
    ->  append(Migrations, Operations)
    ;   Operations = []
    ).

squash_recorded_migration(Repo, Commit, Operations) :-
    schema_change_for_commit(Repo, Commit, Change),
    (   Change = no_change
    ->  Operations = []
    ;   Change = migration(Operations)
    ).

merge_schema_dict(Base, Incoming, Match, Level, State0, State) :-
    dict_keys(Base, Base_Keys),
    dict_keys(Incoming, Incoming_Keys),
    ord_union(Base_Keys, Incoming_Keys, Keys),
    foldl(merge_schema_key(Base, Incoming, Match, Level), Keys, State0, State).

merge_schema_key(Base, Incoming, Match, Level, Key, Target-Present0, Merged-Present) :-
    schema_key_value(Key, Base, Before),
    schema_key_value(Key, Incoming, After),
    schema_key_value(Key, Target, Current),
    (   Before =@= After
    ->  Merged = Target, Present = Present0
    ;   Current =@= Before
    ->  (   After = value(Value)
        ->  put_dict(Key, Target, Value, Merged)
        ;   del_dict(Key, Target, _, Merged)
        ),
        Present = Present0
    ;   Match == true, Current =@= After
    ->  Merged = Target, Present = true
    ;   Level == document, Key \== '@context',
        Before = value(Before_Doc), After = value(After_Doc), Current = value(Current_Doc),
        get_dict('@type', Before_Doc, Type),
        get_dict('@type', After_Doc, Type),
        get_dict('@type', Current_Doc, Type),
        merge_schema_dict(Before_Doc, After_Doc, Match, field,
                          Current_Doc-Present0, Merged_Doc-Present),
        put_dict(Key, Target, Merged_Doc, Merged)
    ).

schema_key_value(Key, Dict, Value) :-
    (   get_dict(Key, Dict, Field)
    ->  Value = value(Field)
    ;   Value = absent
    ).

squash_instance_witnesses(Context, Source, Resolution, Merged_Options, Witnesses) :-
    commit_doc_options(Merged_Options, Doc_Options0),
    database_prefixes(Context, Apply_Prefixes),
    Doc_Options = Doc_Options0.put(apply_prefixes, Apply_Prefixes),
    Source = source{before_id:Before_Commit_Id, after_id:After_Commit_Id,
                    before:Before_Transaction, after:After_Transaction},
    Branch_Descriptor = Context.default_collection,
    findall(Witness,
            (   document_diffs_from_transactions(Branch_Descriptor,
                                                 Before_Commit_Id,
                                                 After_Commit_Id,
                                                 Before_Transaction,
                                                 After_Transaction,
                                                 Doc_Options, Diff,
                                                 Merged_Options),
                do_or_die(apply_diff(Context,Diff,Witness0,Merged_Options),
                          error(unable_to_apply_document_diff(Diff), _)),
                resolve_squash_conflict(Context, After_Transaction,
                                        Doc_Options, Diff,
                                        Witness0, Resolution, Witness),
                \+ Witness = null
            ),
            Witnesses).

%% normalize_conflict_resolution(+Raw, +Context, -Resolution) is det.
%
% Accepts a bare winner atom/string ("error", "ours", "theirs") or a dict
% with `default` and `documents` keys. `documents` maps each winner atom
% to a list of document ids — `{"theirs": ["Person/Jane"], "ours": [...]}` —
% so arbitrary document ids never sit in JSON object keys. Ids may be
% @base-compressed, prefixed or absolute IRIs; all are expanded against
% the context prefixes and stored id->winner so lookup happens in the
% expanded form the diffs carry. An id listed under two winners is
% ambiguous and rejected.
normalize_conflict_resolution(Raw, Context, resolution{default:Default, documents:Documents}) :-
    (   is_dict(Raw)
    ->  (   get_dict(default, Raw, Default0)
        ->  true
        ;   Default0 = error
        ),
        (   get_dict(documents, Raw, Documents0)
        ->  do_or_die(is_dict(Documents0),
                      error(bad_conflict_resolution(Raw), _)),
            get_dict(prefixes, Context, Prefixes),
            dict_pairs(Documents0, json, Winner_Pairs),
            findall(Id-Winner,
                    (   member(Win_Raw-Ids, Winner_Pairs),
                        (   resolution_winner_value(Win_Raw, Winner)
                        ->  do_or_die(is_list(Ids),
                                      error(bad_conflict_resolution(Raw), _)),
                            member(Id0, Ids),
                            (   catch((atom_string(Id_Atom, Id0),
                                       prefix_expand(Id_Atom, Prefixes, Id)),
                                      error(_, _), fail)
                            ->  true
                            ;   throw(error(bad_conflict_resolution(Raw), _)))
                        ;   throw(error(bad_conflict_resolution(Raw), _))
                        )
                    ),
                    Doc_Pairs0),
            keysort(Doc_Pairs0, Sorted_Pairs),
            group_pairs_by_key(Sorted_Pairs, Grouped),
            (   member(_-Winners, Grouped),
                sort(Winners, Distinct_Winners),
                length(Distinct_Winners, N), N > 1
            ->  throw(error(bad_conflict_resolution(Raw), _))
            ;   true),
            maplist([Id-[W|_], Id-W]>>true, Grouped, Doc_Pairs),
            dict_create(Documents, json, Doc_Pairs)
        ;   Documents = json{}
        ),
        (   resolution_winner_value(Default0, Default)
        ->  true
        ;   throw(error(bad_conflict_resolution(Raw), _))
        )
    ;   resolution_winner_value(Raw, Default)
    ->  Documents = json{}
    ;   throw(error(bad_conflict_resolution(Raw), _))
    ).

%% resolution_winner_value(+Raw, -Winner) is semidet.
%
% Winner values are atoms in {error, ours, theirs}; raw strings are
% converted, atoms pass through.
resolution_winner_value(Raw, Winner) :-
    (   atom(Raw)
    ->  Winner = Raw
    ;   catch(atom_string(Winner, Raw), _, fail)
    ),
    memberchk(Winner, [error, ours, theirs]).

%% resolution_winner(+Resolution, +Doc_Id, -Winner) is det.
resolution_winner(resolution{default:Default, documents:Documents}, Doc_Id, Winner) :-
    atom_string(Key, Doc_Id),
    (   get_dict(Key, Documents, Winner)
    ->  true
    ;   Winner = Default
    ).

%% resolve_squash_conflict(+Context, +After_Transaction, +Doc_Options,
%%                         +Diff, +Witness0, +Resolution, -Witness) is det.
%
% Decides the outcome of a conflict witness under the resolution recipe:
% `error` keeps the witness (the apply will fail with it), `ours` keeps
% the target state, and `theirs` applies the incoming side — a delete for
% delete diffs, otherwise the document read through After_Transaction.
resolve_squash_conflict(_Context, _After_Transaction, _Doc_Options, _Diff, null, _Resolution, null) :-
    !.
resolve_squash_conflict(Context, After_Transaction, Doc_Options, Diff, Witness0, Resolution, Witness) :-
    % Documents map keys are expanded at normalize time, so the expanded
    % IRI carried by the diff is the lookup key directly.
    patch_id(Diff, Doc_Id),
    resolution_winner(Resolution, Doc_Id, Winner),
    (   Winner = error
    ->  Witness = Witness0
    ;   Winner = ours
    ->  Witness = null
    ;   Winner = theirs
    ->  squash_apply_theirs(Context, After_Transaction, Doc_Options, Diff, Doc_Id),
        Witness = null
    ).

squash_apply_theirs(Context, _After_Transaction, _Doc_Options, Diff, Doc_Id) :-
    get_dict('@delete', Diff, _),
    !,
    catch(delete_document(Context, Doc_Id),
          error(document_not_found(_), _),
          true).
squash_apply_theirs(Context, After_Transaction, Doc_Options, Diff, Doc_Id) :-
    (   get_dict('@insert', Diff, Doc)
    ->  true
    ;   get_diff_document(After_Transaction, Doc_Id, Doc, Doc_Options)
    ),
    squash_force_document(Context, Doc, _Id).

%% squash_force_document(+Context, +Doc, -Id) is det.
%
% Writes Doc as the target state whether or not it already exists,
% mirroring the raw-JSON bypass in apply_replace_document for
% JSONDocument instances.
squash_force_document(Context, Doc, Id) :-
    (   get_dict('@type', Doc, Type),
        is_json_document_type(Type)
    ->  del_dict('@type', Doc, _, Raw_Doc),
        replace_document(Context, Raw_Doc, true, true, Id)
    ;   replace_document(Context, Doc, true, false, Id)
    ).


:- begin_tests(apply).

:- use_module(core(util/test_utils)).
:- use_module(core(document)).
:- use_module(core(triple)).
:- use_module(core(api/api_document)).
:- use_module(core(api/db_branch)).
:- use_module(library(http/json)).

test(delete_missing,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Author = "gavin",
    Message = "test.",
    Graph_Type = instance,
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    Options0 = [graph_type(Graph_Type),
                author(Author),
                message(Message),
                full_replace(false),
                json(false)],
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, Data_Version1, _Transaction_Meta_Data1, Ids, Options0),

    atom_json_dict(Id_Atom, Ids, []),
    open_string(Id_Atom, Stream2),

    Options1 = [graph_type(Graph_Type),
                author(Author),
                message(Message)],
    api_delete_documents(SystemDB, Auth, Path, Stream2, Data_Version1, Data_Version2, _Transaction_Meta_Data2, _Ids, Options1),

    Commit_Info = commit_info{ author : Author, message: Message },
    Options2 = [match_final_state(true)],
    Data_Version1 = data_version(branch,Commit1),
    Data_Version2 = data_version(branch,Commit2),
    % Matches because we have Options1
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, Commit1, Commit2, Options2),
    % Does not match because we have Options2
    Options3 = [match_final_state(false)],
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, Commit1, Commit2, Options3),
        E,
        E = error(apply_squash_witnesses([json{'@id_does_not_exists':_,
                                               '@op':'DeleteConflict'}]))
    ).

test(insert_twice,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Author = "gavin",
    Message = "test.",
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author(Author),
               message(Message),
               full_replace(false),
               json(false)],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream1),
    api_insert_documents(SystemDB, Auth, Path, Stream1, no_data_version, Data_Version1, _Transaction_Meta_Data1, _Ids1, Options),
    open_string('{"@type" : "City", "name" : "Dublin"}', Stream2),
    api_insert_documents(SystemDB, Auth, Path, Stream2, Data_Version1, Data_Version2, _Transaction_Meta_Data2, _Ids2, Options),

    Commit_Info = commit_info{ author : Author, message: Message },
    Options1 = [match_final_state(true)],
    Data_Version1 = data_version(branch,Commit1),
    Data_Version2 = data_version(branch,Commit2),
    % Matches because we have Options1
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, Commit1, Commit2, Options1),
    % Does not match because we have Options2
    Options2 = [match_final_state(false)],
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, Commit1, Commit2, Options2),
        E,
        E = error(apply_squash_witnesses(
                      [json{'@id_already_exists':_,
                            '@op':'InsertConflict'}]))
    ).

test(schemaless_apply_insert_modify_delete,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [author("gavin"), message("test."), raw_json(true)],

    open_string('[{"@id":"item/a","name":"alpha","n":1},{"@id":"item/c","name":"gamma","n":3}]', Stream1),
    api_insert_documents(SystemDB, Auth, Path, Stream1, no_data_version,
                         data_version(branch,Commit1), _, _Ids1, Options),

    open_string('["item/c"]', Stream2),
    api_delete_documents(SystemDB, Auth, Path, Stream2, no_data_version,
                         _Data_Version2, _, _Ids2, Options),

    open_string('{"@id":"item/a","name":"alpha","n":99}', Stream3),
    api_replace_documents(SystemDB, Auth, Path, Stream3, no_data_version,
                          _Data_Version3, _, _Ids3, [create(false)|Options]),

    open_string('{"@id":"item/b","name":"beta","n":2}', Stream4),
    api_insert_documents(SystemDB, Auth, Path, Stream4, no_data_version,
                         data_version(branch,Commit4), _, _Ids4, Options),

    atomic_list_concat([Path, '/local/branch/dev'], Dev_Path),
    atomic_list_concat([Path, '/local/commit/', Commit1], Commit_Path),
    branch_create(SystemDB, Auth, Dev_Path, branch(Commit_Path), _Branch_Uri),

    Commit_Info = commit_info{ author : "gavin", message: "apply" },
    api_apply_squash_commit(SystemDB, Auth, Dev_Path, Commit_Info,
                            Commit1, Commit4, [match_final_state(false)]),

    resolve_absolute_string_descriptor(Dev_Path, Dev_Descriptor),
    open_descriptor(Dev_Descriptor, Dev_Transaction),
    get_document(Dev_Transaction, 'item/a', Doc_A),
    get_dict(n, Doc_A, 99),
    get_document(Dev_Transaction, 'item/b', _Doc_B),
    \+ get_document(Dev_Transaction, 'item/c', _Doc_C).

/* Three-way merge setup shared by the delete-edit tests:
 *
 *   Commit1 (base)        documents exist
 *      |-- target branch: Main_Body runs (edits and/or deletes)
 *      `-- dev branch:     Dev_Body runs (does the opposite)
 *
 * apply diff(Commit1 -> dev head) onto the target head must report a
 * DeleteEditConflict witness instead of silently dropping a side.
 */

% Bodies are arity-5 call targets: (SystemDB, Auth, Path, Head, Options).
% Head is bound only for the dev run; the main run leaves it anonymous.
delete_edit_setup(SystemDB, Auth, Main_Path, Dev_Path, Commit1, Dev_Head,
                  Docs_Base, Main_Body, Dev_Body) :-
    delete_edit_setup(SystemDB, Auth, Main_Path, Dev_Path, Commit1, Dev_Head,
                      [author("gavin"), message("test."), raw_json(true)],
                      Docs_Base, Main_Body, Dev_Body).

delete_edit_setup(SystemDB, Auth, Main_Path, Dev_Path, Commit1, Dev_Head,
                  Options, Docs_Base, Main_Body, Dev_Body) :-
    open_string(Docs_Base, S1),
    api_insert_documents(SystemDB, Auth, Main_Path, S1, no_data_version,
                         data_version(branch,Commit1), _, _, Options),

    call(Main_Body, SystemDB, Auth, Main_Path, _Main_Head, Options),

    atomic_list_concat([Main_Path,'/local/branch/dev'], Dev_Path),
    atomic_list_concat([Main_Path,'/local/commit/',Commit1], Commit_Path),
    branch_create(SystemDB, Auth, Dev_Path, branch(Commit_Path), _Branch_Uri),

    call(Dev_Body, SystemDB, Auth, Dev_Path, Dev_Head, Options).

delete_doc_body(SystemDB, Auth, Path, Head, Options) :-
    open_string('["item/a"]', S),
    api_delete_documents(SystemDB, Auth, Path, S, no_data_version,
                         data_version(branch,Head), _, _, Options).

edit_doc_body(SystemDB, Auth, Path, Head, Options) :-
    open_string('{"@id":"item/a","name":"alpha","n":2}', S),
    api_replace_documents(SystemDB, Auth, Path, S, no_data_version,
                          data_version(branch,Head), _, _,
                          [create(false)|Options]).

% Finds the diff in Diffs whose document id — '@id' on field diffs,
% '@insert'.'@id' on inserts, '@delete'.'@id' on deletes — expands to
% the compressed form under the test @base.
diff_id_in(Diffs, Compressed_Id, Diff) :-
    member(Diff, Diffs),
    (   get_dict('@insert', Diff, Doc)
    ->  get_dict('@id', Doc, Raw_Id)
    ;   get_dict('@delete', Diff, Doc)
    ->  get_dict('@id', Doc, Raw_Id)
    ;   get_dict('@id', Diff, Raw_Id)),
    atom_string(Id_Atom, Raw_Id),
    atomic_list_concat(['http://somewhere.for.now/document/',
                        Compressed_Id], Id_Atom).

test(apply_delete_edit_conflict_diff_deletes,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    delete_edit_setup(SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
                      '[{"@id":"item/a","name":"alpha","n":1}]',
                      edit_doc_body, delete_doc_body),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                Commit1, Dev_Head, []),
        E,
        true),
    E = error(apply_squash_witnesses(Witnesses)),
    member(W, Witnesses),
    json{'@op':'DeleteEditConflict',
         '@id':"item/a",
         '@deleted_by':"theirs"} :< W,
    % witnesses are identities only — @expected/@found belong to dry_run
    \+ get_dict('@expected', W, _),
    \+ get_dict('@found', W, _),

    % nothing was written: ours' edit survives
    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    get_document(Main_T, 'item/a', Doc_A),
    get_dict(n, Doc_A, 2).

test(apply_delete_edit_conflict_target_deletes,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    delete_edit_setup(SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
                      '[{"@id":"item/a","name":"alpha","n":1}]',
                      delete_doc_body, edit_doc_body),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                Commit1, Dev_Head, []),
        E,
        true),
    E = error(apply_squash_witnesses(Witnesses)),
    member(W, Witnesses),
    json{'@op':'DeleteEditConflict',
         '@id':"item/a",
         '@deleted_by':"ours"} :< W,

    % nothing was written: ours' delete survives
    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    \+ get_document(Main_T, 'item/a', _).

test(apply_delete_edit_resolution_ours_keeps_target,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    % diff deletes item/a, ours edited it: ours wins, doc keeps n=2
    delete_edit_setup(SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
                      '[{"@id":"item/a","name":"alpha","n":1}]',
                      edit_doc_body, delete_doc_body),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                            Commit1, Dev_Head,
                            [conflict_resolution(ours)]),

    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    get_document(Main_T, 'item/a', Doc_A),
    get_dict(n, Doc_A, 2).

test(apply_delete_edit_resolution_theirs_deletes,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    % diff deletes item/a, ours edited it: theirs wins, doc is gone
    delete_edit_setup(SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
                      '[{"@id":"item/a","name":"alpha","n":1}]',
                      edit_doc_body, delete_doc_body),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                            Commit1, Dev_Head,
                            [conflict_resolution(theirs)]),

    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    \+ get_document(Main_T, 'item/a', _).

test(apply_delete_edit_resolution_theirs_resurrects,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    % diff edits item/a, ours deleted it: theirs wins, doc back at n=2
    delete_edit_setup(SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
                      '[{"@id":"item/a","name":"alpha","n":1}]',
                      delete_doc_body, edit_doc_body),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                            Commit1, Dev_Head,
                            [conflict_resolution(theirs)]),

    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    get_document(Main_T, 'item/a', Doc_A),
    get_dict(n, Doc_A, 2).

test(apply_delete_edit_resolution_per_document,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    % ours deletes item/a and edits item/b; the diff edits item/a and
    % deletes item/b. Recipe: item/a -> theirs (resurrect at n=2),
    % item/b -> default ours (keep ours' edit at n=9).
    delete_edit_setup(
        SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
        '[{"@id":"item/a","name":"alpha","n":1},{"@id":"item/b","name":"beta","n":1}]',
        {SystemDB, Auth}/[_, _, Main_Path, _Main_Head, Options]>>(
            open_string('["item/a"]', SD),
            api_delete_documents(SystemDB, Auth, Main_Path, SD, no_data_version,
                                 _, _, _, Options),
            open_string('{"@id":"item/b","name":"beta","n":9}', SE),
            api_replace_documents(SystemDB, Auth, Main_Path, SE, no_data_version,
                                  _, _, _, [create(false)|Options])
        ),
        {SystemDB, Auth}/[_, _, Dev_Path_In, Dev_Head_Out, Options]>>(
            open_string('{"@id":"item/a","name":"alpha","n":2}', SE),
            api_replace_documents(SystemDB, Auth, Dev_Path_In, SE, no_data_version,
                                  _, _, _, [create(false)|Options]),
            open_string('["item/b"]', SD),
            api_delete_documents(SystemDB, Auth, Dev_Path_In, SD, no_data_version,
                                 data_version(branch,Dev_Head_Out), _, _, Options)
        )),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                            Commit1, Dev_Head,
                            [conflict_resolution(
                                 json{default:"ours",
                                      documents:json{theirs:["item/a"]}})]),

    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    get_document(Main_T, 'item/a', Doc_A),
    get_dict(n, Doc_A, 2),
    get_document(Main_T, 'item/b', Doc_B),
    get_dict(n, Doc_B, 9).

test(apply_rejects_unknown_type,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [author("gavin"), message("test."), raw_json(true)],
    open_string('[{"@id":"item/a","name":"alpha","n":1}]', S1),
    api_insert_documents(SystemDB, Auth, Path, S1, no_data_version,
                         data_version(branch,Commit1), _, _, Options),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                Commit1, Commit1, [type(banana)]),
        E,
        true),
    E = error(bad_apply_type(banana), _).

test(apply_rejects_malformed_conflict_resolution,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [author("gavin"), message("test."), raw_json(true)],
    open_string('[{"@id":"item/a","name":"alpha","n":1}]', S1),
    api_insert_documents(SystemDB, Auth, Path, S1, no_data_version,
                         data_version(branch,Commit1), _, _, Options),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                Commit1, Commit1,
                                [conflict_resolution(json{documents:"item/a"})]),
        E1,
        true),
    E1 = error(bad_conflict_resolution(_), _),
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                Commit1, Commit1,
                                [conflict_resolution(5)]),
        E2,
        true),
    E2 = error(bad_conflict_resolution(5), _).

/* Schema'd documents exercise the same compare as schemaless ones but
 * through a different render path: the delete proceeds only when the
 * target document is structurally equal to the base embedded in the
 * @delete dict. */
test(apply_delete_edit_schema_delete_untouched,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [graph_type(instance), author("gavin"), message("test."),
               full_replace(false), json(false)],

    delete_edit_setup(
        SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head, Options,
        '{"@id":"City/a","@type":"City","name":"Warsaw"}',
        {}/[_, _, _, _, _]>>true,
        {}/[SystemDB_, Auth_, Path_, Head_, Options_]>>(
            open_string('["City/a"]', SD),
            api_delete_documents(SystemDB_, Auth_, Path_, SD, no_data_version,
                                 data_version(branch,Head_), _, _, Options_))),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                            Commit1, Dev_Head, []),

    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    \+ get_document(Main_T, 'City/a', _).

test(apply_delete_edit_conflict_schema,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [graph_type(instance), author("gavin"), message("test."),
               full_replace(false), json(false)],

    delete_edit_setup(
        SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head, Options,
        '{"@id":"City/a","@type":"City","name":"Warsaw"}',
        {}/[SystemDB_, Auth_, Path_, _Head, Options_]>>(
            open_string('{"@id":"City/a","@type":"City","name":"Krakow"}', SE),
            api_replace_documents(SystemDB_, Auth_, Path_, SE, no_data_version,
                                  _, _, _, [create(false)|Options_])),
        {}/[SystemDB_, Auth_, Path_, Head_, Options_]>>(
            open_string('["City/a"]', SD),
            api_delete_documents(SystemDB_, Auth_, Path_, SD, no_data_version,
                                 data_version(branch,Head_), _, _, Options_))),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                Commit1, Dev_Head, []),
        E,
        true),
    E = error(apply_squash_witnesses(Witnesses)),
    member(W, Witnesses),
    json{'@op':'DeleteEditConflict',
         '@id':"City/a",
         '@deleted_by':"theirs"} :< W,

    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    get_document(Main_T, 'City/a', Doc_A),
    get_dict(name, Doc_A, "Krakow").

/* match_final_state tolerates a delete that is already the target state;
 * a target that diverged from base is a real conflict, not the final
 * state, so the witness still fires. */
test(apply_delete_edit_match_final_state_still_conflicts,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    delete_edit_setup(SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
                      '[{"@id":"item/a","name":"alpha","n":1}]',
                      edit_doc_body, delete_doc_body),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                Commit1, Dev_Head, [match_final_state(true)]),
        E,
        true),
    E = error(apply_squash_witnesses(Witnesses)),
    member(W, Witnesses),
    json{'@op':'DeleteEditConflict', '@deleted_by':"theirs"} :< W.

test(apply_delete_edit_match_final_state_still_conflicts_schema,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [graph_type(instance), author("gavin"), message("test."),
               full_replace(false), json(false)],

    delete_edit_setup(
        SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head, Options,
        '{"@id":"City/a","@type":"City","name":"Warsaw"}',
        {}/[SystemDB_, Auth_, Path_, _Head, Options_]>>(
            open_string('{"@id":"City/a","@type":"City","name":"Krakow"}', SE),
            api_replace_documents(SystemDB_, Auth_, Path_, SE, no_data_version,
                                  _, _, _, [create(false)|Options_])),
        {}/[SystemDB_, Auth_, Path_, Head_, Options_]>>(
            open_string('["City/a"]', SD),
            api_delete_documents(SystemDB_, Auth_, Path_, SD, no_data_version,
                                 data_version(branch,Head_), _, _, Options_))),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                Commit1, Dev_Head, [match_final_state(true)]),
        E,
        true),
    E = error(apply_squash_witnesses(Witnesses)),
    member(W, Witnesses),
    json{'@op':'DeleteEditConflict', '@deleted_by':"theirs"} :< W.

/* theirs on an InsertConflict: the incoming document replaces the
 * conflicting target document entirely. */
test(apply_resolution_theirs_insert_conflict,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    delete_edit_setup(
        SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
        '[{"@id":"item/a","name":"alpha","n":1}]',
        {}/[SystemDB_, Auth_, Path_, _Head, Options_]>>(
            open_string('[{"@id":"item/x","name":"main","n":1}]', SM),
            api_insert_documents(SystemDB_, Auth_, Path_, SM, no_data_version,
                                 _, _, _, Options_)),
        {}/[SystemDB_, Auth_, Path_, Head_, Options_]>>(
            open_string('[{"@id":"item/x","name":"dev","n":7}]', SD),
            api_insert_documents(SystemDB_, Auth_, Path_, SD, no_data_version,
                                 data_version(branch,Head_), _, _, Options_))),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                            Commit1, Dev_Head,
                            [conflict_resolution(theirs)]),

    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    get_document(Main_T, 'item/x', Doc_X),
    get_dict(n, Doc_X, 7).

/* theirs on a field-level Conflict: the whole incoming document state
 * wins, not just the conflicting field. */
test(apply_resolution_theirs_field_conflict,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    delete_edit_setup(
        SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
        '[{"@id":"item/a","name":"alpha","n":1}]',
        edit_doc_body,
        {}/[SystemDB_, Auth_, Path_, Head_, Options_]>>(
            open_string('{"@id":"item/a","name":"alpha","n":3}', SD),
            api_replace_documents(SystemDB_, Auth_, Path_, SD, no_data_version,
                                  data_version(branch,Head_), _, _,
                                  [create(false)|Options_]))),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                            Commit1, Dev_Head,
                            [conflict_resolution(theirs)]),

    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    get_document(Main_T, 'item/a', Doc_A),
    get_dict(n, Doc_A, 3).

/* Every conflicting document must surface in a single witnesses list so
 * callers resolve them in one pass instead of discovering them serially. */
test(apply_delete_edit_conflict_reports_all_witnesses,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    delete_edit_setup(
        SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
        '[{"@id":"item/a","name":"alpha","n":1},{"@id":"item/b","name":"beta","n":1}]',
        {SystemDB, Auth}/[_, _, Main_Path, _Main_Head, Options]>>(
            open_string('["item/a"]', SD),
            api_delete_documents(SystemDB, Auth, Main_Path, SD, no_data_version,
                                 _, _, _, Options),
            open_string('{"@id":"item/b","name":"beta","n":9}', SE),
            api_replace_documents(SystemDB, Auth, Main_Path, SE, no_data_version,
                                  _, _, _, [create(false)|Options])
        ),
        {SystemDB, Auth}/[_, _, Dev_Path_In, Dev_Head_Out, Options]>>(
            open_string('{"@id":"item/a","name":"alpha","n":2}', SE),
            api_replace_documents(SystemDB, Auth, Dev_Path_In, SE, no_data_version,
                                  _, _, _, [create(false)|Options]),
            open_string('["item/b"]', SD),
            api_delete_documents(SystemDB, Auth, Dev_Path_In, SD, no_data_version,
                                 data_version(branch,Dev_Head_Out), _, _, Options)
        )),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                Commit1, Dev_Head, []),
        E,
        true),
    E = error(apply_squash_witnesses(Witnesses)),
    length(Witnesses, 2),
    once((member(W_A, Witnesses),
          json{'@op':'DeleteEditConflict',
               '@id':"item/a",
               '@deleted_by':"ours"} :< W_A)),
    once((member(W_B, Witnesses),
          json{'@op':'DeleteEditConflict',
               '@id':"item/b",
               '@deleted_by':"theirs"} :< W_B)).

test(apply_dry_run_reports_conflict_without_committing,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    delete_edit_setup(SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
                      '[{"@id":"item/a","name":"alpha","n":1}]',
                      edit_doc_body, delete_doc_body),

    Commit_Info = commit_info{author:"gavin", message:"preview"},
    api_check_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                  Commit1, Dev_Head, [], Result),
    json{'@type':'api:CheckResponse',
         'api:valid':false} :< Result,
    [W] = Result.'api:witnesses',
    json{'@op':'DeleteEditConflict',
         '@id':"item/a",
         '@deleted_by':"theirs"} :< W,
    get_dict('@expected', W, Expected),
    get_dict(n, Expected, 1),
    get_dict('@found', W, Found),
    get_dict(n, Found, 2),

    % the dry run wrote nothing: ours' edit is still the live state
    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    get_document(Main_T, 'item/a', Doc_A),
    get_dict(n, Doc_A, 2).

test(apply_dry_run_clean_merge_reports_valid,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    delete_edit_setup(
        SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
        '[{"@id":"item/a","name":"alpha","n":1}]',
        edit_doc_body,
        {SystemDB, Auth}/[_, _, Dev_Path_In, Dev_Head_Out, Options]>>(
            open_string('[{"@id":"item/b","name":"beta"}]', S),
            api_insert_documents(SystemDB, Auth, Dev_Path_In, S,
                                 no_data_version,
                                 data_version(branch,Dev_Head_Out), _, _,
                                 Options))),

    Commit_Info = commit_info{author:"gavin", message:"preview"},
    api_check_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                  Commit1, Dev_Head, [], Result),
    json{'@type':'api:CheckResponse',
         'api:valid':true,
         'api:witnesses':[]} :< Result,

    % nothing committed: item/b does not exist on main
    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    \+ get_document(Main_T, 'item/b', _).

test(apply_dry_run_resolution_reports_verdict,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    delete_edit_setup(SystemDB, Auth, Path, _Dev_Path, Commit1, Dev_Head,
                      '[{"@id":"item/a","name":"alpha","n":1}]',
                      edit_doc_body, delete_doc_body),

    Commit_Info = commit_info{author:"gavin", message:"preview"},
    api_check_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                  Commit1, Dev_Head,
                                  [conflict_resolution(theirs)], Result),
    json{'@type':'api:CheckResponse',
         'api:valid':true,
         'api:witnesses':[]} :< Result,

    % still nothing committed: ours' edit survives
    resolve_absolute_string_descriptor(Path, Main_Desc),
    open_descriptor(Main_Desc, Main_T),
    get_document(Main_T, 'item/a', Doc_A),
    get_dict(n, Doc_A, 2).

/* A dry run must run the same validation the commit would: a merge that
 * produces a schema-invalid document reports api:valid:false, not a
 * clean preview that fails at commit time. */
test(apply_dry_run_reports_schema_invalid_merge,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [graph_type(instance), author("gavin"), message("test."),
               full_replace(false), json(false)],

    % Commit1: a valid City on main
    open_string('[{"@id":"City/a","@type":"City","name":"Warsaw"}]', S1),
    api_insert_documents(SystemDB, Auth, Path, S1, no_data_version,
                         data_version(branch,Commit1), _, _, Options),

    % Fork dev at Commit1.
    atomic_list_concat([Path, "/local/branch/dev"], Dev_Path),
    branch_create(system_descriptor{}, Auth, Dev_Path, branch(Path), _),

    % main tightens the Person schema after the fork — no Person exists
    % on main yet, so the update itself validates.
    open_string('{"@id":"Person","@type":"Class","@inherits":"Named","occupation":"xsd:string","address":"xsd:string","parent":{"@type":"Optional","@class":"Person"},"birthday":{"@type":"Optional","@class":"xsd:dateTime"}}', SS),
    api_replace_documents(SystemDB, Auth, Path, SS, no_data_version,
                          data_version(branch,_), _, _,
                          [create(false), graph_type(schema)|Options]),

    % dev inserts a Person under the loose base schema — invalid under
    % main's tightened schema when merged.
    open_string('[{"@id":"Person/p","@type":"Person","name":"Jo","address":"here"}]', SP),
    api_insert_documents(SystemDB, Auth, Dev_Path, SP, no_data_version,
                         data_version(branch,Dev_Head), _, _, Options),

    Commit_Info = commit_info{author:"gavin", message:"preview"},
    api_check_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                  Commit1, Dev_Head, [], Result),
    % The merged Person lacks occupation — invalid under the new schema.
    json{'@type':'api:CheckResponse',
         'api:valid':false} :< Result,
    get_dict('api:witnesses', Result, Witnesses),
    Witnesses = [_|_].

test(commits_changed_id_enumerates_touched_documents,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [author("gavin"), message("test."), raw_json(true)],

    % Commit1: base with the docs each case exercises
    open_string('[{"@id":"keep/a"},{"@id":"gone/a"},{"@id":"revert/a"},{"@id":"untouched/a"}]', S1),
    api_insert_documents(SystemDB, Auth, Path, S1, no_data_version,
                         data_version(branch,Commit1), _, _, Options),
    % Commit2: create created/a (positive -> insert diff)
    open_string('[{"@id":"created/a"}]', S2),
    api_insert_documents(SystemDB, Auth, Path, S2, no_data_version,
                         data_version(branch,_), _, _, Options),
    % Commit3: modify keep/a (positive -> field diff)
    open_string('{"@id":"keep/a","name":"edited"}', S3),
    api_replace_documents(SystemDB, Auth, Path, S3, no_data_version,
                          data_version(branch,_), _, _, [create(false)|Options]),
    % Commit4: delete gone/a (positive -> delete diff)
    open_string('["gone/a"]', S4),
    api_delete_documents(SystemDB, Auth, Path, S4, no_data_version,
                         data_version(branch,_), _, _, Options),
    % Commit5a/5b: insert then delete flash/a — touched inside the range
    % but absent at both ends: enumerated here, no diff produced.
    open_string('[{"@id":"flash/a"}]', S5),
    api_insert_documents(SystemDB, Auth, Path, S5, no_data_version,
                         data_version(branch,_), _, _, Options),
    open_string('["flash/a"]', S5b),
    api_delete_documents(SystemDB, Auth, Path, S5b, no_data_version,
                         data_version(branch,_), _, _, Options),
    % Commit6a/6b: modify revert/a then put it back — touched inside the
    % range, identical at both ends: zero-cost diff is filtered.
    open_string('{"@id":"revert/a","name":"changed"}', S6),
    api_replace_documents(SystemDB, Auth, Path, S6, no_data_version,
                          data_version(branch,_), _, _, [create(false)|Options]),
    open_string('{"@id":"revert/a"}', S6b),
    api_replace_documents(SystemDB, Auth, Path, S6b, no_data_version,
                          data_version(branch,Head), _, _, [create(false)|Options]),

    resolve_absolute_string_descriptor(Path, Desc),
    Enum_Options = options{start:0, count:inf,
                           keep:json{'@id':true, '@type':true}},
    findall(C, commits_changed_id(Desc, Commit1, Head, C,
                                  Enum_Options),
            Changed),
    sort(Changed, Sorted_Changed),

    % Positive set: documents whose state differs between the endpoints —
    % the range squashes to a net delta, so flash/a and revert/a net out
    % and are never enumerated.
    maplist([Compressed, Expanded]>>(
                atomic_list_concat(
                    ['http://somewhere.for.now/document/', Compressed],
                    Expanded)),
            ["created/a", "gone/a", "keep/a"],
            Expected_Changed),
    ord_subset(Expected_Changed, Sorted_Changed),
    % Negative set: untouched, net-zero and untouched-by-diff documents are
    % never enumerated.
    \+ member('http://somewhere.for.now/document/untouched/a', Sorted_Changed),
    \+ member('http://somewhere.for.now/document/flash/a', Sorted_Changed),
    \+ member('http://somewhere.for.now/document/revert/a', Sorted_Changed),

    % Diff-level contract — this is what the apply semantics need.
    % JSONDocument ids stay expanded (anyURI), so compare by expansion.
    findall(D, document_diffs_from_commits(Desc, Commit1, Head, D,
                                           Enum_Options), Diffs),
    once(diff_id_in(Diffs, 'created/a', Insert_Diff)),
    get_dict('@op', Insert_Diff, "Insert"),
    once(diff_id_in(Diffs, 'keep/a', Keep_Diff)),
    get_dict(name, Keep_Diff, Name_Diff),
    get_dict('@op', Name_Diff, "SwapValue"),
    once(diff_id_in(Diffs, 'gone/a', Gone_Diff)),
    get_dict('@op', Gone_Diff, "Delete"),
    % Negative set at diff level: nothing for flash, revert, untouched.
    \+ diff_id_in(Diffs, 'flash/a', _),
    \+ diff_id_in(Diffs, 'revert/a', _),
    \+ diff_id_in(Diffs, 'untouched/a', _).

test(apply_squash_matches_an_already_applied_schema_delta,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [graph_type(instance), author("gavin"), message("test."),
               full_replace(false), json(false)],

    open_string('[{"@id":"City/a","@type":"City","name":"Warsaw"}]', S1),
    api_insert_documents(SystemDB, Auth, Path, S1, no_data_version,
                         data_version(branch,Commit1), _, _, Options),

    % Same branch, second commit tightens the Person schema.
    open_string('{"@id":"Person","@type":"Class","@inherits":"Named","occupation":"xsd:string","address":"xsd:string","parent":{"@type":"Optional","@class":"Person"},"birthday":{"@type":"Optional","@class":"xsd:dateTime"}}', SS),
    api_replace_documents(SystemDB, Auth, Path, SS, no_data_version,
                          data_version(branch,Head), _, _,
                          [create(false), graph_type(schema)|Options]),

    Commit_Info = commit_info{author:"gavin", message:"merge"},
    api_check_apply_squash_commit(SystemDB, Auth, Path, Commit_Info,
                                  Commit1, Head, [], Result),
    get_dict('api:valid', Result, true),
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, Commit1, Head, []),
    resolve_absolute_string_descriptor(Path, Descriptor),
    open_descriptor(Descriptor, Transaction),
    transaction_data_version(Transaction, data_version(branch, Head)).

test(apply_records_merge_source,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [author("gavin"), message("test."), raw_json(true)],

    open_string('[{"@id":"item/a","name":"alpha","n":1}]', S1),
    api_insert_documents(SystemDB, Auth, Path, S1, no_data_version,
                         data_version(branch,Commit1), _, _, Options),
    open_string('{"@id":"item/b","name":"beta","n":2}', S2),
    api_insert_documents(SystemDB, Auth, Path, S2, no_data_version,
                         data_version(branch,Commit4), _, _, Options),

    atomic_list_concat([Path, '/local/branch/dev'], Dev_Path),
    atomic_list_concat([Path, '/local/commit/', Commit1], Commit_Path),
    branch_create(SystemDB, Auth, Dev_Path, branch(Commit_Path), _),

    Commit_Info = commit_info{author:"gavin", message:"apply"},
    api_apply_squash_commit(SystemDB, Auth, Dev_Path, Commit_Info,
                            Commit1, Commit4, []),

    resolve_absolute_string_descriptor(Dev_Path, Dev_Descriptor),
    Repo_Descriptor = (Dev_Descriptor.repository_descriptor),
    branch_head_commit(Repo_Descriptor, "dev", Head_Uri),
    create_context(Repo_Descriptor, Repo_Ctx),
    get_document(Repo_Ctx, Head_Uri, Head_Doc),
    get_dict(merge_source, Head_Doc, Merge_Source),
    get_dict(operation, Merge_Source, apply),
    ref_entity:commit_id_uri(Repo_Ctx, Commit4, Source_Uri),
    atom_string(Source_Uri, Source_Uri_String),
    get_dict(commits, Merge_Source, [Source_Uri_String]).

test(commits_changed_id_enumerates_over_a_layerless_base,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema(admin, X))),
      cleanup(teardown_temp_store(State))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [author("gavin"), message("test.")],

    % The schema commit has no instance layer — the squash anchor below
    % the merge base is absent, so enumeration must squash the head
    % segment fully instead of stopping at a base layer.
    open_string('[{"@type":"City","name":"one"},{"@type":"City","name":"two"}]',
                S1),
    api_insert_documents(SystemDB, Auth, Path, S1, no_data_version,
                         data_version(branch,Head), _, Ids, Options),

    resolve_absolute_string_descriptor(Path, Desc),
    create_context(Desc.repository_descriptor, Repo_Ctx),
    ref_entity:commit_id_to_parent_uri(Repo_Ctx, Head, Parent_Uri),
    ref_entity:commit_id_uri(Repo_Ctx, Base_Id, Parent_Uri),
    % Fixture sanity: the base commit really has no instance layer.
    \+ 'document/history':commit_graph_layer(Repo_Ctx, Base_Id, instance, _),

    findall(C, commits_changed_id(Desc, Base_Id, Head, C,
                                  options{start:0, count:inf}),
            Changed),
    sort(Changed, Sorted_Changed),
    maplist([Compressed, Expanded]>>(
                atomic_list_concat(
                    ['http://example.com/data/world/', Compressed],
                    Expanded)),
            Ids, Expected_Changed),
    sort(Expected_Changed, Expected_Sorted),
    ord_subset(Expected_Sorted, Sorted_Changed).

test(document_diffs_throw_when_a_document_cannot_be_diffed,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X))),
      cleanup(teardown_temp_store(State)),
      throws(error(missing_diff_option(keep), _))]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Options = [author("gavin"), message("test."), raw_json(true)],

    open_string('[{"@id":"keep/a"}]', S1),
    api_insert_documents(SystemDB, Auth, Path, S1, no_data_version,
                         data_version(branch,Commit1), _, _, Options),
    open_string('{"@id":"keep/a","name":"edited"}', S2),
    api_replace_documents(SystemDB, Auth, Path, S2, no_data_version,
                          data_version(branch,Head), _, _,
                          [create(false)|Options]),

    resolve_absolute_string_descriptor(Path, Desc),
    % No `keep` option: simple_diff cannot produce a diff for the
    % modified document. Enumeration must surface that as an error —
    % a document that cannot be diffed may not silently drop out.
    findall(D, document_diffs_from_commits(Desc, Commit1, Head, D,
                                           options{start:0, count:inf}),
            _Diffs).

:- end_tests(apply).

:- begin_tests(patch_resource).

:- use_module(core(util/test_utils)).
:- use_module(core(document)).
:- use_module(core(triple)).
:- use_module(core(api/api_document)).
:- use_module(library(json)).

test(patch_resource_insert,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "insert via patch" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':"City/Leipzig",
                                    '@type':"City",
                                    name:"Leipzig" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    get_document(T, 'City/Leipzig', Doc),
    get_dict(name, Doc, "Leipzig").

test(patch_resource_delete,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, _Transaction_Meta_Data, Inserted_Ids, Options),
    Inserted_Ids = [City_Id],
    Commit_Info = commit_info{ author : "test", message: "delete via patch" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':City_Id }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    \+ get_document(T, City_Id, _).

test(patch_resource_delete_string,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, _Transaction_Meta_Data, Inserted_Ids, Options),
    Inserted_Ids = [City_Id],
    Commit_Info = commit_info{ author : "test", message: "delete via patch string" },
    Patch = [json{ '@op':'Delete',
                    '@delete':City_Id }],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    \+ get_document(T, City_Id, _).

test(patch_resource_mixed,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, _Transaction_Meta_Data, [City_Id], Options),
    Commit_Info = commit_info{ author : "test", message: "mixed patch" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':City_Id }},
             json{ '@op':'Insert',
                    '@insert':json{ '@id':"City/Berlin",
                                    '@type':"City",
                                    name:"Berlin" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    get_document(T, 'City/Berlin', Berlin),
    get_dict(name, Berlin, "Berlin"),
    \+ get_document(T, City_Id, _).

test(patch_resource_insert_conflict,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(patch_conflicts([json{'@id_already_exists':_,
                                          '@op':'InsertConflict'}])))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, _Transaction_Meta_Data, [City_Id], Options),
    Commit_Info = commit_info{ author : "test", message: "insert conflict" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':City_Id,
                                    '@type':"City",
                                    name:"Warsaw" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_delete_conflict,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(patch_conflicts([json{'@id_does_not_exists':_,
                                         '@op':'DeleteConflict'}])))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "delete conflict" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':"City/Nonexistent" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_insert_match_final_state,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, _Transaction_Meta_Data, [City_Id], Options),
    Commit_Info = commit_info{ author : "test", message: "insert match final state" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':City_Id,
                                    '@type':"City",
                                    name:"Warsaw" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids,
                       [match_final_state(true)]).

test(patch_resource_delete_match_final_state,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "delete match final state" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':'City/Nonexistent' }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids,
                       [match_final_state(true)]).

test(patch_resource_mixed_conflict,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(patch_conflicts(_)))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "mixed conflict" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':"City/Berlin",
                                    '@type':"City",
                                    name:"Berlin" }},
             json{ '@op':'Delete',
                    '@delete':json{ '@id':"City/Nonexistent" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_match_final_state_empty_ids,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "match final state empty ids" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':"City/Nonexistent" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids,
                       [match_final_state(true)]).

test(patch_resource_insert_no_id_error,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(no_id_in_patch(_), _))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "no id" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@type':"City",
                                    name:"NoID" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_insert_with_captures,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:"Person" },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Commit_Info = commit_info{ author : "test", message: "captures insert" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@type':"Person",
                                    '@capture':"PersonA",
                                    name:"Alice",
                                    friend:json{'@ref':"PersonB"}}},
             json{ '@op':'Insert',
                    '@insert':json{ '@type':"Person",
                                    '@capture':"PersonB",
                                    name:"Bob",
                                    friend:json{'@ref':"PersonA"}}}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, Ids, []),
    length(Ids, 2),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    findall(Name,
            (   member(Id, Ids),
                get_document(T, Id, Doc),
                get_dict(name, Doc, Name)
            ),
            Names),
    memberchk("Alice", Names),
    memberchk("Bob", Names).

test(patch_resource_unmatched_capture_fails,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(not_all_captures_found(_), _))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:"Person" },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Commit_Info = commit_info{ author : "test", message: "unmatched capture" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@type':"Person",
                                    '@capture':"PersonA",
                                    name:"Alice",
                                    friend:json{'@ref':"Nonexistent"}}}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_insert_with_id_and_capture,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:"Person" },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Commit_Info = commit_info{ author : "test", message: "id and capture" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Alice",
                                    '@type':"Person",
                                    '@capture':"PersonA",
                                    name:"Alice",
                                    friend:json{'@ref':"PersonB"}}},
             json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Bob",
                                    '@type':"Person",
                                    '@capture':"PersonB",
                                    name:"Bob",
                                    friend:json{'@ref':"PersonA"}}}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, Ids, []),
    length(Ids, 2),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    findall(Name,
            (   member(Id, Ids),
                get_document(T, Id, Doc),
                get_dict(name, Doc, Name)
            ),
            Names),
    length(Names, 2),
    memberchk(Name_A, Names),
    (   atom_string(Name_A, "Alice")
    ->  true
    ;   atom_string(Name_A, "Bob")
    ->  true
    ;   fail
    ).

test(patch_resource_field_patch_with_captures,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:"Person" },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Commit_Info = commit_info{ author : "test", message: "field patch with captures" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Alice",
                                    '@type':"Person",
                                    '@capture':"PersonA",
                                    name:"Alice",
                                    friend:json{'@ref':"PersonB"}}},
             json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Bob",
                                    '@type':"Person",
                                    '@capture':"PersonB",
                                    name:"Bob",
                                    friend:json{'@ref':"PersonA"}}},
             json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Charlie",
                                    '@type':"Person",
                                    name:"Charlie",
                                    friend:json{'@ref':"PersonA"}}}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, Ids, []),
    Field_Commit = commit_info{ author : "test", message: "field patch" },
    Field_Patch = [json{ '@id':"Person/Charlie",
                         name:json{'@op':'SwapValue',
                                   '@before':"Charlie",
                                   '@after':"Charles"}}],
    api_patch_resource(SystemDB, Auth, Path, Field_Patch, Field_Commit, _, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    findall(Name,
            (   member(Id, Ids),
                get_document(T, Id, Doc),
                get_dict(name, Doc, Name)
            ),
            Names),
    length(Names, 3),
    get_document(T, 'Person/Charlie', Doc_C),
    get_dict(name, Doc_C, Name_C),
    atom_string(Name_C, "Charles").

test(patch_resource_field_patch_swapvalue_with_ref,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:json{'@type':"Optional", '@class':"Person"} },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Setup_Commit = commit_info{ author : "test", message: "insert Charlie" },
    Setup_Patch = [json{ '@op':'Insert',
                         '@insert':json{ '@id':"Person/Charlie",
                                         '@type':"Person",
                                         name:"Charlie"}}],
    api_patch_resource(SystemDB, Auth, Path, Setup_Patch, Setup_Commit, _, []),
    Patch = [json{ '@op':'Insert',
                   '@insert':json{ '@id':"Person/Alice",
                                   '@type':"Person",
                                   '@capture':"PersonA",
                                   name:"Alice",
                                   friend:json{'@ref':"PersonB"}}},
             json{ '@op':'Insert',
                   '@insert':json{ '@id':"Person/Bob",
                                   '@type':"Person",
                                   '@capture':"PersonB",
                                   name:"Bob",
                                   friend:json{'@ref':"PersonA"}}},
             json{ '@id':"Person/Charlie",
                   friend:json{'@op':'SwapValue',
                               '@before':null,
                               '@after':json{'@ref':"PersonA"}}}],
    Commit_Info = commit_info{ author : "test", message: "insert and swap Charlie friend with ref" },
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    get_document(T, 'Person/Charlie', Doc_C),
    get_dict(friend, Doc_C, Friend_C),
    atom_string(Friend_C, "Person/Alice").

test(patch_resource_schemaless_field_patch,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_without_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    open_string('{"@id":"item/a","name":"alpha","n":1}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV, _, _Ids,
                         [author("test"), message("setup"), raw_json(true)]),
    Commit_Info = commit_info{ author : "test", message: "field patch" },
    Patch = [json{ '@id':"item/a",
                   n:json{ '@op':'SwapValue',
                           '@before':1,
                           '@after':42}}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Patch_Ids, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    get_document(T, 'item/a', Doc),
    get_dict(n, Doc, 42).

test(patch_resource_returns_compressed_ids,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "compressed ids" },
    Patch = [json{ '@op':'Insert',
                   '@insert':json{ '@id':"City/Berlin",
                                   '@type':"City",
                                   name:"Berlin" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, Ids, []),
    Ids = [Id],
    atom_string(Id_Atom, Id),
    atom_concat('City/', Local, Id_Atom),
    atom_concat('Berlin', '', Local).

:- end_tests(patch_resource).

:- begin_tests(patch_diff_auth).

:- use_module(core(util/test_utils)).

% The pure patch/diff helpers take no path, so they cannot scope a
% descriptor check. They still burn CPU on request, so require the
% caller to hold instance_read_access on some resource — the same
% action class their descriptor-scoped siblings require — which public
% databases grant to anonymous.
expect_read_denied(Goal) :-
    (   catch(Goal, Ball, true)
    ->  (   Ball = error(access_not_authorised(_,_,_),_)
        ->  true
        ;   format(user_error, '~nexpected access_not_authorised, got ~p~n', [Ball]),
            fail)
    ;   format(user_error, '~nexpected access_not_authorised, but call succeeded~n', []),
        fail).

test(deny_caller_without_any_read_access,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","priv"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    forall(member(Goal,
                  [ api_patch(SystemDB, 'User/unprivileged', json{n:1}, json{n:2}, _After, []),
                    api_diff(SystemDB, 'User/unprivileged', json{n:1}, json{n:2}, _Diff, [])
                  ]),
           expect_read_denied(Goal)).

test(allow_anonymous_on_public_database,
     [setup((setup_temp_store(State),
             create_public_db_without_schema("admin","pub"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    api_patch(SystemDB, 'User/anonymous',
              json{n:json{'@op':'SwapValue','@before':1,'@after':2}},
              json{n:1}, _After, []),
    api_diff(SystemDB, 'User/anonymous', json{n:1}, json{n:2}, _Diff,
             [keep(json{}),unfold(true)]).

:- end_tests(patch_diff_auth).
