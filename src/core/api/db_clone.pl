:- module(db_clone, [
              clone/11
          ]).


:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(db_create).
:- use_module(db_delete).
:- use_module(db_fetch).
:- use_module(db_fast_forward).
:- use_module(api_remote).
:- use_module(core(plugins)).

:- meta_predicate clone_(+,+,+,+,+,+,+,+,+,3,-).
:- meta_predicate clone(+,+,+,+,+,+,+,+,+,3,-).
clone(System_DB, Auth, Account,DB,Label,Comment,Public,Remote,Source,Fetch_Predicate,Meta_Data) :-
    setup_call_catcher_cleanup(
        true,
        clone_(System_DB, Auth, Account,DB,Label,Comment,Public,Remote,Source,Fetch_Predicate,Meta_Data),
        exception(E),

        (   clone_cleanup_required(E)
        ->  force_delete_db(Account, DB)
        ;   true)).

clone_cleanup_required(remote_pack_failed(_)).
clone_cleanup_required(remote_pack_unpexected_failure(_)).
clone_cleanup_required(error(http_open_error(_), _)).
clone_cleanup_required(error(remote_connection_failure(_, _), _)).

do_clone_(Auth,Account,DB,Remote,Source, Fetch_Predicate,Meta_Data) :-
    open_descriptor(system_descriptor{}, System_DB),

    % Is this local or over the network
    remote_path(Source, Remote_Path),

    resolve_absolute_descriptor([Account,DB,"_meta"], Database_Descriptor),
    create_context(Database_Descriptor, Database_Context),
    % Add remote
    with_transaction(
        Database_Context,
        insert_remote_repository(Database_Context,
                                 Remote,
                                 Remote_Path,
                                 _Head,
                                 _Repo_URI
                                ),
        _Meta_Data),

    From_Path_List = [Account,DB,Remote,"_commits"],
    merge_separator_split(From_Path, '/', From_Path_List),

    % Fetch remote
    (   Remote_Path = db(_Remote_Team,_Remote_DB)
    ->  local_fetch(System_DB, Auth, From_Path, _New_Head, _Has_Updated)
    ;   remote_fetch(System_DB, Auth, From_Path, Fetch_Predicate, _New_Head, _Has_Updated)
    ),

    resolve_absolute_descriptor([Account,DB,"local","branch","main"], To_Branch_Descriptor),
    resolve_absolute_descriptor([Account,DB,Remote,"branch","main"], From_Branch_Descriptor),

    % Fast forward commits from master in remote to master in local
    fast_forward_branch(To_Branch_Descriptor, From_Branch_Descriptor, Applied_Commits),

    Meta_Data = _{ applied_commits : Applied_Commits }.


clone_(System_DB,Auth,Account,DB,Label,Comment,Public,Remote,Source,Fetch_Predicate,Meta_Data) :-
    % Create DB
    create_db_unfinalized(System_DB, Auth, Account, DB, Label, Comment, false, Public, _{'@base' : 'http://example.com/', '@schema' : 'http://example.com#'}, Db_Uri),

    text_to_string(Account, Account_String),
    text_to_string(DB, DB_String),
    % Mark as creating so other threads are blocked by the gate in open_descriptor.
    % Wrapped in setup_call_cleanup to guarantee retraction even if finalize_db throws.
    setup_call_cleanup(
        assert(db_currently_creating(Account_String, DB_String)),
        (   setup_call_cleanup(
                assert(db_creation_bypass(Account_String, DB_String)),
                catch_with_backtrace(do_clone_(Auth,Account,DB,Remote,Source,Fetch_Predicate,Meta_Data),
                                     Error,
                                     true),
                retract(db_creation_bypass(Account_String, DB_String))
            ),
            (   var(Error)
            ->  finalize_db(Db_Uri)
            ;   force_delete_db(Account, DB),
                throw(Error)
            )
        ),
        ignore(retract(db_currently_creating(Account_String, DB_String)))
    ),

    % The fast-forward applied commits without passing through the
    % transaction commit path — fire the hook only after finalize_db and
    % the creation gate have lifted, so consumers calling back into
    % open_descriptor (e.g. the indexer) see a finished database.
    (   get_dict(applied_commits, Meta_Data, Applied_Commits),
        Applied_Commits \== []
    ->  resolve_absolute_descriptor([Account, DB, "local", "branch", "main"],
                                    Cloned_Branch_Descriptor),
        ignore(forall(post_fast_forward_hook(Cloned_Branch_Descriptor,
                                             Applied_Commits),
                      true))
    ;   true).


:- begin_tests(clone_indexer_hook, [concurrent(false)]).

:- use_module(library(yall)).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(plugins)).

:- dynamic ff_hook_clause_ref/1.

% The recorder clause records into the global recorded database — test
% block code does not live in the module, so a qualified call back into
% db_clone would not resolve at run time.
install_ff_hook_recorder :-
    assertz(plugins:(post_fast_forward_hook(D, I) :-
                     recordz(clone_ff_hook_call, D-I)),
            Ref),
    assertz(ff_hook_clause_ref(Ref)).

uninstall_ff_hook_recorder :-
    forall(ff_hook_clause_ref(Ref), erase(Ref)),
    retractall(ff_hook_clause_ref(_)),
    forall(recorded(clone_ff_hook_call, _, Ref2), erase(Ref2)).

test("clone fires post_fast_forward_hook for the cloned branch",
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "ffsrc"),
             install_ff_hook_recorder)),
      cleanup((uninstall_ff_hook_recorder,
               teardown_temp_store(State)))
     ]) :-
    % Commit a triple on the source so there is history to apply.
    resolve_absolute_string_descriptor("admin/ffsrc", Src_Branch),
    create_context(Src_Branch,
                   commit_info{author:"test", message:"test"},
                   Src_Context),
    with_transaction(Src_Context,
                     ask(Src_Context, insert(a,b,c)),
                     _),

    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    clone(System, Auth, "admin", "ffdest", "label", "comment", false,
          "origin", "admin/ffsrc", [_U,_H,_P]>>fail, Meta_Data),

    get_dict(applied_commits, Meta_Data, Applied_Commits),
    assertion(Applied_Commits \== []),
    once(recorded(clone_ff_hook_call, Descriptor-Commit_Ids)),
    assertion(Commit_Ids == Applied_Commits),
    resolve_absolute_string_descriptor(Path, Descriptor),
    assertion(Path == 'admin/ffdest/local/branch/main').

:- end_tests(clone_indexer_hook).

