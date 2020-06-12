:- module(db_pull, [
              pull/7
          ]).

pull(Our_Branch_Descriptor,Local_Auth,Remote_Name, Remote_Branch_Name, Fetch_Predicate,
     status{ fetch_status : Head_Has_Updated,
             branch_status : Branch_Status
           }) :-

    Our_Repository_Descriptor = (Our_Branch_Descriptor.repository_descriptor),
    Their_Repository_Descriptor = (Our_Repository_Descriptor.put({ repository_name : Remote_Name })),
    Their_Branch_Descriptor = branch_descriptor{
                                  branch_name : Remote_Branch_Name,
                                  repository_descriptor : Their_Repository_Descriptor
                              },

    % 1. fetch
    remote_fetch(Repository_Descriptor, Fetch_Predicate,
                 _New_Head_Layer_Id, Head_Has_Updated),

    % 2. try fast foward - alert front end if impossible.
    catch(
        (   fast_forward_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Applied_Commit_Ids),
            Branch_Status = branch_status{
                                status : "branch_fast_forwarded"
                            }
        ),
        E,
        (   E = error(Inner_E)
        ->  (   E = divergent_history(Common_Commit_Option)
            ->  (   Common_Commit_Option = none
                ->  Branch_Status = branch_status{
                                        status : "no_common_history"
                                    }
                ;   Common_Commit_Option = some(Common_Commit)
                ->  Branch_Status = branch_status{
                                        status : "history_diverged",
                                        common_commit : Common_Commit
                                    }
                )
            ;   throw(E)
            )
        ;   throw(E))),



