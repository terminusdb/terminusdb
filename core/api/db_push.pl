:- module(db_push, [
              push/6
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_pack).
:- use_module(db_rebase).

% 1. rebase on remote
% 2. pack and send
% 3. error if head moved

push(Branch_Descriptor, Remote_Name, Remote_Branch, _Auth_ID,
     Push_Predicate, Result) :-
    do_or_die(
        open_descriptor(Branch_Descriptor, _Branch_Transaction), % dodgy underscore
        error(branch_does_not_exist_in_push(Branch_Descriptor))),

    Repository_Descriptor = (Branch_Descriptor.repository_descriptor),

    do_or_die(
        repository_type(Database_Descriptor, Remote_Name, Type),
        error(no_repository_with_name(Database_Descriptor,Remote_Name))),

    do_or_die(
        Type = remote,
        error(push_attempted_on_non_remote(Database_Descriptor,Remote_Name))),

    do_or_die(
        repository_remote_url(Repository_Descriptor, Remote_Name, Remote_URL),
        error(remote_repository_does_not_exist(Remote_Name))),

    % 1. Begin hypothetical rebase for pack
    create_context(Repository_Descriptor, Repository_Context_With_Prefixes),
    context_default_prefixes(Repository_Context_With_Prefixes, Repository_Context),

    Database_Descriptor = (Repository_Descriptor.database_descriptor),

    resolve_relative_descriptor(Database_Descriptor,[Remote_Name],
                                Remote_Repository),

    (   create_context(Remote_Repository, Remote_Repository_Context_With_Prefixes),
        context_default_prefixes(Remote_Repository_Context_With_Prefixes,
                                 Remote_Repository_Context),
        do_or_die(repository_head(Remote_Repository_Context, Remote_Name, Last_Head_Id),
                  error(push_has_no_repository_head(Remote_Repository)))
    ->  branch_head_commit(Repository_Context,
                           (Branch_Descriptor.branch_name),
                           Local_Commit_Uri),
        (   has_branch(Remote_Repository_Context,
                       Remote_Branch)
        ->  (   branch_head_commit(Remote_Repository_Context,
                                   Remote_Branch,
                                   Remote_Commit_Uri)
            ->  Remote_Commit_Uri_Option = some(Remote_Commit_Uri)
            ;   Remote_Commit_Uri_Option = none
            ),
            branch_name_uri(Remote_Repository_Context, Remote_Branch, Remote_Branch_Uri)
        ;   insert_branch_object(Remote_Repository_Context, Remote_Branch, Remote_Branch_Uri),
            Remote_Commit_Uri_Option = none
        ),

        commit_id_uri(Repository_Context, Local_Commit_Id, Local_Commit_Uri),
        (   Remote_Commit_Uri_Option = some(Remote_Commit_Uri),
            commit_id_uri(Remote_Repository_Context, Remote_Commit_Id, Remote_Commit_Uri),
            most_recent_common_ancestor(Repository_Context, Remote_Repository_Context, Local_Commit_Id,
                                        Remote_Commit_Id, _Common_Commit_Id, _Local_Branch_Path, Remote_Branch_Path)
        % Shared history
        ->  (   Remote_Branch_Path = []
            ->  true
            ;   throw(error(remote_diverged(Remote_Branch_Path)))
            )
        % No shared history
        ;   true),

        copy_commits(Repository_Context, Remote_Repository_Context, Local_Commit_Id),
        branch_name_uri(Remote_Repository_Context, Remote_Branch, Remote_Branch_Uri),
        reset_branch_head(Remote_Repository_Context, Remote_Branch_Uri, Local_Commit_Uri)
    ;   % Remote doesn't even exist yet
        throw(error(remote_does_not_exist(Remote_Repository)))
    ),

    cycle_context(Remote_Repository_Context, Final_Context, Remote_Transaction_Object, _),
    repository_context__previous_head_option__payload(Final_Context, some(Last_Head_Id), Payload),

    Database_Transaction_Object = (Remote_Transaction_Object.parent),
    repository_head(Database_Transaction_Object,Remote_Name,Current_Head_Id),

    (   Last_Head_Id = Current_Head_Id
    ->  Result = _{'terminus:status' : "terminus:success"} % nothing to be done!
    ;   call(Push_Predicate, Remote_URL, Remote_Branch, Payload, Result),
        (   Result = _{'terminus:status' : "terminus:success"}
        ->  update_repository_head(Database_Transaction_Object, Remote_Name, Current_Head_Id),
            run_transactions([Database_Transaction_Object], _)
        ;   true)
    ).



:- begin_tests(push).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(account)).

%:- use_module(db_create).
%:- use_module(db_branch).

test(push_something,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))])
:-

    true.

:- end_tests(push).
