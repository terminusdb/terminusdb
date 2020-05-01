:- module(db_rebase, []).
:- use_module(core(query)).
:- use_module(core(transaction)).

apply_commit_chain(_Our_Repo_Context, _Their_Repo_Context, _Branch_Name, _Author, [], []) :-
    true.
apply_commit_chain(Our_Repo_Context, Their_Repo_Context, Branch_Name, Author, [Commit_Id|Commit_Ids], [Strategy|Strategies]) :-
    % apply the commit
    commit_id_uri(Their_Repo_Context, Commit_Id, Commit_Uri),
    apply_commit(Our_Repo_Context, Their_Repo_Context, Branch_Name, Commit_Uri, Author, _New_Commit_Id, _New_Commit_Uri),

    % turn our repo context into a validation object
    [Transaction_Object] = Our_Repo_Context.transaction_objects,
    transaction_objects_to_validation_objects([Transaction_Object], [Validation_Object]),

    % somehow get a branch context from that
    validation_objects_to_transaction_objects([Validation_Object], [New_Transaction_Object]),
    Branch_Descriptor = branch_descriptor{
                            repository_descriptor: New_Transaction_Object.descriptor,
                            branch_name: Branch_Name
                        },
    open_descriptor(Branch_Descriptor, Branch_Transaction, [New_Transaction_Object.descriptor=New_Transaction_Object], _),
    transaction_objects_to_validation_objects([Branch_Transaction], [Branch_Validation_Object]),

    % validate the branch context
    validate_validation_object([Branch_Validation_Object], Witnesses),

    % if it validates, yay! continue to the next commit (see below)
    (   Witnesses = []
    ->  true
    ;   Strategy = error
        ->  throw(error(commit_apply_schema_validation_error(Commit_Id)))
    ;   Strategy = continue
        -> true % turn into an invalid commit
    ;   Strategy = fixup(Woql)
        -> true % apply woql fixup
    ;   throw(error(unknown_rebase_strategy(Strategy)))),
        
    % somehow go back to a repo context and call apply_commit_chain on the remainder
    true.


rebase_on_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Author, Strategy) :-
    Our_Repo_Descriptor = Our_Branch_Descriptor.repo_descriptor,
    Their_Repo_Descriptor = Their_Branch_Descriptor.repo_descriptor,
    create_context(Our_Repo_Descriptor, Our_Repo_Context),
    create_context(Their_Repo_Descriptor, Their_Repo_Context),

    branch_head_commit(Our_Repo_Context, "master", Our_Commit_Uri),
    commit_id_uri(Our_Repo_Context, Our_Commit_Id, Our_Commit_Uri),
    branch_head_commit(Their_Repo_Context, "second", Their_Commit_Uri),
    commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri),

    most_recent_common_ancestor(Our_Repo_Context, Their_Repo_Context, Our_Commit_Id, Their_Commit_Id, Common_Commit_Id, _Our_Branch_Path, Their_Branch_Path),

    (   Their_Branch_Path = []
    ->  % yay we're done! All commits are known to us, no need to do a thing
        true
    ;   apply_commit_chain(Our_Repo_Context,
                           Their_Repo_Context, 
                           Our_Branch_Descriptor.branch_name,
                           Their_Branch_Path,
                           Strategy)).
