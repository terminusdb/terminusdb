:- module(db_pull, [
              pull/7
          ]).

:- use_module(core(util)).
:- use_module(db_fetch).
:- use_module(db_fast_forward).
:- use_module(core(transaction)).
:- use_module(core(account)).

:- meta_predicate pull(+, +, +, +, +, 3, -).
pull(System_DB, Local_Auth, Our_Branch_Path, Remote_Name, Remote_Branch_Name, Fetch_Predicate,
     status{ fetch_status : Head_Has_Updated,
             branch_status : Branch_Status
           }) :-

    do_or_die(
        resolve_absolute_string_descriptor(Our_Branch_Path,Our_Branch_Descriptor),
        error(invalid_absolute_path(Our_Branch_Path),_)),

    check_descriptor_auth(System_DB, Our_Branch_Descriptor, system:schema_write_access, Local_Auth),
    check_descriptor_auth(System_DB, Our_Branch_Descriptor, system:instance_write_access, Local_Auth),

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
    remote_fetch(System_DB, Local_Auth, Their_Repository_Descriptor, Fetch_Predicate,
                 _New_Head_Layer_Id, Head_Has_Updated),

    % 2. try fast forward - alert front end if impossible.
    fast_forward_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Applied_Commit_Ids),
    Branch_Status = branch_status{
                        '@type' : "api:PullReport",
                        'api:pull_status': Inner_Status
                    },
    (   Applied_Commit_Ids = []
    ->  Inner_Status = "api:pull_unchanged"
    ;   Inner_Status = "api:pull_fast_forwarded"
    ).



