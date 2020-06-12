:- module(db_pull, [
              pull/6
          ]).

:- use_module(core(util)).
:- use_module(db_fetch).
:- use_module(db_fast_forward).
:- use_module(core(transaction)).

:- meta_predicate pull(+, +, +, +, 4, -).
pull(Our_Branch_Descriptor,_Local_Auth,Remote_Name, Remote_Branch_Name, Fetch_Predicate,
     status{ fetch_status : Head_Has_Updated,
             branch_status : Branch_Status
           }) :-

    do_or_die((branch_descriptor{} :< Our_Branch_Descriptor,
               open_descriptor(Our_Branch_Descriptor, _)),
              error(not_a_valid_local_branch(Our_Branch_Descriptor))),

    Our_Repository_Descriptor = (Our_Branch_Descriptor.repository_descriptor),
    Their_Repository_Descriptor = (Our_Repository_Descriptor.put(_{ repository_name : Remote_Name })),
    Their_Branch_Descriptor = branch_descriptor{
                                  branch_name : Remote_Branch_Name,
                                  repository_descriptor : Their_Repository_Descriptor
                              },

    do_or_die(open_descriptor(Their_Branch_Descriptor, _),
              error(not_a_valid_remote_branch(Their_Branch_Descriptor))),

    % 1. fetch
    remote_fetch(Their_Repository_Descriptor, Fetch_Predicate,
                 _New_Head_Layer_Id, Head_Has_Updated),

    % 2. try fast forward - alert front end if impossible.
    catch(
        (   fast_forward_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Applied_Commit_Ids),
            Branch_Status = branch_status{
                                status: Inner_Status
                            },
            (   Applied_Commit_Ids = []
            ->  Inner_Status = "branch_unchanged"
            ;   Inner_Status = "branch_fast_forwarded"
            )
        ),
        E,
        (   E = error(Inner_E)
        ->  (   Inner_E = divergent_history(Common_Commit)
            ->  Branch_Status = branch_status{
                                    status : "history_diverged",
                                    common_commit : Common_Commit
                                }
            ;   Inner_E = no_common_history
            ->  Branch_Status = branch_status{
                                    status : "no_common_history"
                                }
            ;   throw(E)
            )
        ;   throw(E))).




