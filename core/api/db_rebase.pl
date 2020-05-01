:- module(db_rebase, [
              rebase_on_branch/7
          ]).
:- use_module(core(query)).
:- use_module(core(transaction)).

cycle_context(Context, New_Context, Transaction_Object, Validation_Object) :-
    [Transaction_Object] = Context.transaction_objects,
    transaction_objects_to_validation_objects([Transaction_Object], [Validation_Object]),
    validate_validation_objects([Validation_Object], Witnesses),
    (   Witnesses = []
    ->  true
    % put more stuff in error!
    ;   throw(error(schema_validation_error(Witnesses)))),
    validation_objects_to_transaction_objects([Validation_Object], [New_Transaction_Object]),

    create_context(New_Transaction_Object, New_Context).

apply_commit_chain(_Our_Repo_Context, _Their_Repo_Context, _Branch_Name, _Author, _Auth_Object, [], []) :-
    true.
apply_commit_chain(Our_Repo_Context, Their_Repo_Context, Branch_Name, Author, Auth_Object, [Commit_Id|Commit_Ids], [Strategy|Strategies]) :-
    % apply the commit
    commit_id_uri(Their_Repo_Context, Commit_Id, Commit_Uri),
    apply_commit(Our_Repo_Context, Their_Repo_Context, Branch_Name, Commit_Uri, Author, _New_Commit_Id, _New_Commit_Uri),

    % turn our repo context into a validation object
    cycle_context(Our_Repo_Context, Our_Repo_Context2, New_Our_Repo_Transaction_Object, _Our_Repo_Validation_Object),

    Our_Branch_Descriptor = branch_descriptor{
                            repository_descriptor: New_Our_Repo_Transaction_Object.descriptor,
                            branch_name: Branch_Name
                        },
    open_descriptor(Our_Branch_Descriptor, Our_Branch_Transaction, [New_Our_Repo_Transaction_Object.descriptor=New_Our_Repo_Transaction_Object], _),

    transaction_objects_to_validation_objects([Our_Branch_Transaction], [Our_Branch_Validation_Object]),

    % validate the branch context
    validate_validation_objects([Our_Branch_Validation_Object], Witnesses),

    % if it validates, yay! continue to the next commit (see below)
    (   Witnesses = []
    ->  Our_Repo_Context3 = Our_Repo_Context
    ;   Strategy = error
        ->  throw(error(commit_apply_schema_validation_error(Commit_Id)))
    ;   Strategy = continue
        ->invalidate_commit(Our_Repo_Context2, Commit_Id),
          cycle_context(Our_Repo_Context2, Our_Repo_Context3, _, _)
    ;   Strategy = fixup(Message, Woql)
        ->create_context(Our_Branch_Transaction, Our_Branch_Fixup_Context_Without_Auth),
          put_dict(_{authorization: Auth_Object,
                     commit_info: commit_info{author:Author,message:Message}},
                   Our_Branch_Fixup_Context_Without_Auth,
                   Our_Branch_Fixup_Context),
          compile_query(Woql, Prog, Our_Branch_Fixup_Context, Our_Branch_Fixup_Context2),
          forall(woql_compile:Prog, true),
          % turn context into validation
          Our_Branch_Fixup_Context2.transaction_objects = [Branch_Fixup_Transaction_Object],
          transaction_objects_to_validation_objects([Branch_Fixup_Transaction_Object], [Branch_Fixup_Validation_Object]),
          % validate
          validate_validation_objects([Branch_Fixup_Validation_Object], Fixup_Witnesses),
          (   Fixup_Witnesses = []
          ->  true
          ;   throw(error(commit_apply_fixup_error(Commit_Id, Fixup_Witnesses)))),

          % commit validation
          commit_validation_object(Branch_Fixup_Validation_Object, _),
          % write commit object into our repo context
          cycle_context(Our_Repo_Context2, Our_Repo_Context3, _, _)),

    apply_commit_chain(Our_Repo_Context3, Their_Repo_Context, Branch_Name, Author, Auth_Object, Commit_Ids, Strategies).


rebase_on_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Author, Auth_Object, _Strategy, Common_Commit_Id, Their_Branch_Path) :-
    Our_Repo_Descriptor = Our_Branch_Descriptor.repo_descriptor,
    Their_Repo_Descriptor = Their_Branch_Descriptor.repo_descriptor,
    create_context(Our_Repo_Descriptor, Our_Repo_Context),
    create_context(Their_Repo_Descriptor, Their_Repo_Context),

    branch_head_commit(Our_Repo_Context, "master", Our_Commit_Uri),
    commit_id_uri(Our_Repo_Context, Our_Commit_Id, Our_Commit_Uri),
    branch_head_commit(Their_Repo_Context, "second", Their_Commit_Uri),
    commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri),

    most_recent_common_ancestor(Our_Repo_Context, Their_Repo_Context, Our_Commit_Id, Their_Commit_Id, Common_Commit_Id, _Our_Branch_Path, Their_Branch_Path),

    length(Their_Branch_Path, Len),
    length(Strategies, Len),
    maplist([error]>>true, Strategies),

    (   Their_Branch_Path = []
    ->  % yay we're done! All commits are known to us, no need to do a thing
        true
    ;   apply_commit_chain(Our_Repo_Context,
                           Their_Repo_Context, 
                           Our_Branch_Descriptor.branch_name,
                           Author,
                           Auth_Object,
                           Their_Branch_Path,
                           Strategies)).
