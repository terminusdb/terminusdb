:- module(db_push, [
              push/6
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_pack).

% 1. rebase on remote
% 2. pack and send
% 3. error if head moved

push(Branch_Descriptor, Remote_Name, Remote_Branch, Push_Predicate, Force, Result) :-
    do_or_die(
        open_descriptor(Branch_Descriptor, Branch_Transaction),
        error(branch_does_not_exist_in_push(Branch_Descriptor))),

    Repository_Descriptor = (Branch_Descriptor.repository_descriptor),
    do_or_die(
        repository_remote_url(Repository_Descriptor, Remote_Name, Remote_URL),
        error(remote_repository_does_not_exist(Remote_Name))),

    % 1. Begin hypothetical rebase for pack
    create_context(Repository_Descriptor, Repository_Context),
    % Database_Descriptor = (Repository_Descriptor.database_descriptor),

    resolve_relative_descriptor(Database_Descriptor,[Remote_Name],
                                Remote_Repository),
    % Is this needed?
    %resolve_relative_descriptor(Remote_Repository,[branch,Remote_Branch],
    %                            Remote_Branch_Repository),

    (   create_context(Remote_Repository, Remote_Repository_Context)
    ->  branch_head_commit(Repository_Context,
                           (Branch_Descriptor.branch_name),
                           Local_Commit_Uri),
        branch_head_commit(Remote_Repository_Context,
                           Remote_Name,
                           Remote_Commit_Uri),
        commit_id_uri(Repository_Context, Local_Commit_Id, Local_Commit_Uri),
        commit_id_uri(Remote_Repository_Transaction, Remote_Commit_Id, Remote_Commit_Uri),

        (   most_recent_common_ancestor(Repository_Transaction, Remote_Repository_Transaction, Local_Commit_Id, Remote_Commit_Id, Common_Commit_Id, Local_Branch_Path, Remote_Branch_Path)
        ->  % Shared history
            true
        ;   % No shared history
            true
        )
    ;   % Remote doesn't even exist yet
        true
    ),


    call(Push_Predicate, Remote_URL, Remote_Branch, Pack, Force, Result).

