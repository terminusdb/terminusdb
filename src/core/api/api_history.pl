:- module(api_history, [
              api_document_history/6,
              api_document_history_streaming/5
          ]).

:- use_module(core(document/history)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(util)).

:- use_module(library(option)).
:- use_module(library(date), [parse_time/3]).
:- use_module(library(lists), [append/2]).
:- use_module(library(http/json), [json_write_dict/3]).

iso8601_to_rational_epoch(ISO8601, Epoch) :-
    parse_time(ISO8601, iso_8601, FloatEpoch),
    Epoch is rationalize(FloatEpoch).

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
        build_history_options(Options, History_Options),
        document_history(Descriptor, Id, Start, Count, Response, History_Options)
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

build_history_options(Options, History_Options) :-
    (   option(before(Before_ISO), Options),
        ground(Before_ISO)
    ->  iso8601_to_rational_epoch(Before_ISO, Before_Epoch),
        Before_Opts = [before(Before_Epoch)]
    ;   Before_Opts = []
    ),
    (   option(after(After_ISO), Options),
        ground(After_ISO)
    ->  iso8601_to_rational_epoch(After_ISO, After_Epoch),
        After_Opts = [after(After_Epoch)]
    ;   After_Opts = []
    ),
    (   option(graph_type(Graph_Type), Options),
        ground(Graph_Type)
    ->  GT_Opts = [graph_type(Graph_Type)]
    ;   GT_Opts = []
    ),
    (   option(fast(true), Options)
    ->  Fast_Opts = [fast(true)]
    ;   Fast_Opts = []
    ),
    (   option(complete(true), Options)
    ->  Complete_Opts = [complete(true)]
    ;   Complete_Opts = []
    ),
    (   option(diff(true), Options)
    ->  Diff_Opts = [diff(true)]
    ;   Diff_Opts = []
    ),
    append([Before_Opts, After_Opts, GT_Opts, Fast_Opts,
            Complete_Opts, Diff_Opts], History_Options).

stream_history_entry(Entry) :-
    json_write_dict(current_output, Entry, [width(0)]),
    nl,
    flush_output.

api_document_history_streaming(System_DB, Auth, Path, Id, Options) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path), _)),

    do_or_die(
        branch_descriptor{} :< Descriptor,
        error(not_a_branch_descriptor(Descriptor), _)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/commit_read_access', Auth),

    build_history_options(Options, History_Options),
    Enrichment_Options = History_Options,
    document_history_entries(Descriptor, Id, api_history:stream_history_entry, Enrichment_Options, History_Options).
