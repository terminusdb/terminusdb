:- module(db_branch, []).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).

branch_create_(Repository_Descriptor, local_branch(Branch_Descriptor), New_Branch_Name) :-
    % easy! copy whatever is in old branch, put it in new branch
    % got to make sure that the given branch is part of the same repository
    Repository_Descriptor = Branch_Descriptor.repository_descriptor,
    true.
branch_create_(Repository_Descriptor, remote_branch(Branch_Descriptor), New_Branch_Name) :-
    % easy! copy whatever is in old branch, put it in new branch.
    % got to make sure that the given branch is part of the same database, and not part of the same repository
    % also register the remote branch as upstream
    Repository_Descriptor /= Branch_Descriptor.repository_descriptor,
    Repository_Descriptor.database_descriptor = Branch_Descriptor.repository_descriptor.database_descriptor,

    % commit object might not exist yet in the local repository, so make sure to insert it. Same goes for any parent commit that may or may not exist
    open_descriptor(Branch_Descriptor, Remote_Transaction),
    branch_head_commit(Remote_Transaction, Branch_Descriptor.branch_name, Commit_Uri),
    commit_id_uri(Remote_Transaction, Commit_Id, Commit_Uri),
    commit_to_metadata(Remote_transaction, Commit_Id, Author, Message, Timestamp),

    true.
branch_create_(Repository_Descriptor, commit(Commit_Id, Base_Uri), New_Branch_Name) :-
    % easy! make a new branch object and point it at the given commit
    insert_branch_object(Repository_Descriptor,
                         New_Branch_Name,
                         Base_Uri,
                         Branch_Uri),
    commit_id_uri(Repository_Descriptor, Commit_Id, Commit_Uri),
    link_commit_object_to_branch(Repository_Descriptor,
                                 Branch_Uri,
                                 Commit_Uri).
branch_create_(Repository_Descriptor, empty(Base_Uri), New_Branch_Name) :-
    % easy! just create the branch object and nothing else
    insert_branch_object(Repository_Descriptor,
                         New_Branch_Name,
                         Base_Uri,
                         _Branch_Uri).

/* create a branch in the given repository. Origin is the thing we wish to create a branch out of. it can be any kind of branch descriptor or commit descriptor. branch name is the name of the new branch. options contain a branch default prefix thingie. */
branch_create(Repository_Descriptor, Origin_Descriptor, New_Branch_Name, Options) :-
    % ensure that we're putting this branch into a local repository
    (   repository_type(Repository_Descriptor.database_descriptor,
                        Repository_Descriptor.repository_name,
                        local)
    ->  true
    ;   throw(error(repository_is_not_local(Repository_Descriptor)))),

    branch_create_(Repository_Descriptor, Old_Ref, New_Branch_Name).
