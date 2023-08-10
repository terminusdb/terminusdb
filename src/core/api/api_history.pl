:- module(api_history, [
              api_document_history/6
          ]).

:- use_module(core(document/history)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(util)).

:- use_module(library(option)).

api_document_history(System_DB, Auth, Path, Id, Response, Options) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path,Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        branch_descriptor{} :< Descriptor,
        error(not_a_branch_descriptor(Descriptor), _)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/commit_read_access', Auth),

    (   option(created(false), Options),
        option(updated(false), Options)
    ->  option(start(Start), Options),
        option(count(Count), Options),
        document_history(Descriptor, Id, Start, Count, Response)
    ;   (   option(created(true), Options)
        ->  document_created_at(Descriptor, Id, Created_Info),
            Response0 = json{ created : Created_Info }
        ;   Response0 = json{}
        ),
        (   option(updated(true), Options)
        ->  document_updated_at(Descriptor, Id, Updated_Info),
            put_dict(json{ updated : Updated_Info }, Response0, Response)
        ;   Response = Response0
        )
    ).
