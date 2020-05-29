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

    % 1. Make the pack somehow
    open_descriptor(Repository_Descriptor, Repository_Transaction),
    Database_Descriptor = (Repository_Descriptor.database_descriptor),
    resolve_relative_descriptor(Database_Descriptor,[Remote_Name,branch,Remote_Branch],
                                Remote_Branch_Descriptor),

    (   open_descriptor(Remote_Branch_Descriptor, Remote_Branch_Transaction)
    ->  branch_head_commit(Repository_Transaction, "master", Commit_B_Uri),
        most_recent_common_ancestor(Branch_Transaction, Repository_Branch_Transaction, Head1_Commit_Id, Head2_Commit_Id, Common_Commit_Id, Branch1_Path, Branch2_Path),
        true
    ;   everything),

    call(Push_Predicate, Remote_URL, Remote_Branch, Pack, Force, Result).

