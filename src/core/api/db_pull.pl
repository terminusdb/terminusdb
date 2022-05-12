:- module(db_pull, [
              pull/7
          ]).

:- use_module(core(util)).
:- use_module(db_fetch).
:- use_module(db_fast_forward).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(query)).

:- meta_predicate pull(+, +, +, +, +, 3, -).
pull(System_DB, Local_Auth, Our_Branch_Path, Remote_Name, Remote_Branch_Name, Fetch_Predicate,
     Status) :-

    do_or_die(
        resolve_absolute_string_descriptor(Our_Branch_Path,Our_Branch_Descriptor),
        error(invalid_absolute_path(Our_Branch_Path),_)),

    check_descriptor_auth(System_DB, Our_Branch_Descriptor, '@schema':'Action/schema_write_access', Local_Auth),
    check_descriptor_auth(System_DB, Our_Branch_Descriptor, '@schema':'Action/instance_write_access', Local_Auth),

    do_or_die(
        (   branch_descriptor{ branch_name : Our_Branch_Name } :< Our_Branch_Descriptor,
            open_descriptor(Our_Branch_Descriptor, _)),
        error(unknown_local_branch(Our_Branch_Path),_)),

    (   var(Remote_Branch_Name)
    ->  Remote_Branch_Name = Our_Branch_Name
    ;   true
    ),

    Our_Repository_Descriptor = (Our_Branch_Descriptor.repository_descriptor),
    Their_Repository_Descriptor = (Our_Repository_Descriptor.put(_{ repository_name : Remote_Name })),

    Their_Branch_Descriptor = branch_descriptor{
                                  branch_name : Remote_Branch_Name,
                                  repository_descriptor : Their_Repository_Descriptor
                              },

    resolve_absolute_string_descriptor(Their_Repository_Path, Their_Repository_Descriptor),
    % 1. fetch
    remote_fetch(System_DB, Local_Auth, Their_Repository_Path, Fetch_Predicate,
                 _New_Head_Layer_Id, Head_Has_Updated),

    do_or_die(open_descriptor(Their_Branch_Descriptor, _),
              error(not_a_valid_remote_branch(Their_Branch_Descriptor),_)),

    Status = status{
                 'api:pull_status': Inner_Status,
                 'api:fetch_status' : Head_Has_Updated
             },
    % 2. try fast forward - alert front end if impossible.
    catch_with_backtrace(
        (   fast_forward_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Applied_Commit_Ids),
            (   Applied_Commit_Ids = []
            ->  Inner_Status = "api:pull_unchanged"
            ;   Inner_Status = "api:pull_fast_forwarded"
            )
        ),
        E,
        (   E = error(Inner_E,Ctx),
            (   Inner_E = divergent_history(Common_Commit,_,[])
            ->  Inner_Status = "api:pull_ahead"
            ;   Inner_E = divergent_history(Common_Commit,_,_)
            ->  throw(error(pull_divergent_history(Common_Commit,Head_Has_Updated),Ctx))
            ;   Inner_E = no_common_history
            ->  throw(error(pull_no_common_history(Head_Has_Updated),Ctx))
            ;   throw(E))
        )
    ).


