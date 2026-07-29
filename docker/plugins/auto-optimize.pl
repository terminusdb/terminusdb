:- module('plugins/auto_optimize', []).
:- use_module(core(api)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(document/commit_queue)).
:- use_module(config(terminus_config), [log_level/1]).
:- use_module(library(http/http_server)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

% Flag to track if GC is currently running (prevents piling up requests)
:- dynamic gc_running/0.

% Requirement to add to plugins since TerminusDB 11.2.0
:- multifile plugins:post_commit_hook/2.

% Copyright (c) TerminusDB, Licensed under the Apache License, Version 2.0
% Source: https://github.com/terminusdb-labs/terminusdb-plugins/tree/main

% Configuration constants
gc_chance(0.005).          % 10% - GC runs on average every 100th commit
optimize_chance(0.1).     % 10% - Optimization runs on average every 10th commit, for control and content graphs

:- gc_chance(GC), optimize_chance(OPT),
   GC_Pct is round(GC * 100),
   OPT_Pct is round(OPT * 100),
   format(atom(Msg), "auto-optimize plugin loaded: probabilistic GC (~w%) and optimization (~w%) enabled", [GC_Pct, OPT_Pct]),
   json_log_info(Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Optimization predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

descriptor_optimize_chance(system_descriptor{}, C) => optimize_chance(C).
descriptor_optimize_chance(D, C), database_descriptor{} :< D => optimize_chance(C).
descriptor_optimize_chance(D, C), repository_descriptor{} :< D => optimize_chance(C).
descriptor_optimize_chance(D, C), branch_descriptor{} :< D => optimize_chance(C).
descriptor_optimize_chance(_, _) => false.

should_optimize(Descriptor) :-
    descriptor_optimize_chance(Descriptor, Chance),
    random(X),
    X < Chance.

optimize(Descriptor) :-
    optimize(Descriptor, 3).

optimize(Descriptor, Retries) :-
    (   resolve_absolute_string_descriptor(Path, Descriptor)
    ->  true
    ;   Path = "unknown"
    ),
    (   catch(
            (   api_optimize:descriptor_optimize(Descriptor),
                json_log_debug_formatted("Optimized ~s", [Path])
            ),
            Error,
            handle_optimize_error(Descriptor, Path, Retries, Error)
        )
    ->  true
    ;   json_log_debug_formatted("Optimization skipped for ~s (no layers)", [Path])
    ).

handle_optimize_error(Descriptor, _Path, Retries, error(label_version_changed(_, _), _)) :-
    Retries > 1,
    !,
    NewRetries is Retries - 1,
    sleep(0.5),
    optimize(Descriptor, NewRetries).
handle_optimize_error(_, Path, _, error(label_version_changed(_, _), _)) :-
    !,
    json_log_debug_formatted("Optimization of ~s deferred (label version changed)", [Path]).
handle_optimize_error(_, Path, _, error(branch_does_not_exist(_), _)) :-
    !,
    json_log_debug_formatted("Optimization of ~s skipped (branch no longer exists)", [Path]).
handle_optimize_error(_, Path, _, error(existence_error(graph, _), _)) :-
    !,
    json_log_debug_formatted("Optimization of ~s skipped (graph does not exist)", [Path]).
handle_optimize_error(_, Path, _, Error) :-
    json_log_error_formatted("Optimization of ~s failed: ~w", [Path, Error]).

all_descriptor(Descriptor, Descriptor).
all_descriptor(Descriptor, Parent_Descriptor) :-
    get_dict(repository_descriptor, Descriptor, Intermediate_Descriptor),
    all_descriptor(Intermediate_Descriptor, Parent_Descriptor).
all_descriptor(Descriptor, Parent_Descriptor) :-
    get_dict(database_descriptor, Descriptor, Intermediate_Descriptor),
    all_descriptor(Intermediate_Descriptor, Parent_Descriptor).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Garbage collection and statistics predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_stack_statistics_json(Label) :-
    statistics(localused, LocalUsed),
    statistics(globalused, GlobalUsed),
    statistics(trailused, TrailUsed),
    statistics(stack_limit, StackLimit),
    statistics(table_space_used, TableUsed),
    statistics(atoms, Atoms),
    statistics(inferences, Inferences),
    statistics(cputime, CpuTime),
    prolog_current_frame(Frame),
    prolog_frame_attribute(Frame, level, FrameDepth),
    findall(json{id: IdAtom, status: StatusAtom},
            (   thread_property(Id, status(Status)),
                term_to_atom(Id, IdAtom),
                term_to_atom(Status, StatusAtom)
            ),
            Threads),
    length(Threads, ThreadCount),
    Total is LocalUsed + GlobalUsed + TrailUsed,
    Percent is float(100 * Total / StackLimit),
    Stats = json{
        message: Label,
        label: Label,
        stacks: json{
            total: Total,
            limit: StackLimit,
            percent: Percent,
            local: LocalUsed,
            global: GlobalUsed,
            trail: TrailUsed,
            frame_depth: FrameDepth
        },
        tables: TableUsed,
        atoms: Atoms,
        inferences: Inferences,
        cpu_time: CpuTime,
        threads: json{
            count: ThreadCount,
            list: Threads
        }
    },
    json_log_debug(Stats).

% Probability of running abolish_all_tables during GC.
% This is a rare global table cleanup — most GC cycles just collect
% atoms and trim stacks. At 10%, this fires roughly once per 2000
% commits (0.5% GC chance × 10% abolish chance), which is frequent
% enough to reclaim stale tabled results without thrashing.
abolish_tables_chance(0.10).

% Actual GC work - runs in dedicated thread
do_garbage_collect :-
    maybe_print_stats('PRE-GC'),
    garbage_collect,
    trim_stacks,
    maybe_abolish_tables,
    maybe_print_stats('POST-GC'),
    json_log_debug("Ran garbage_collect").

maybe_abolish_tables :-
    abolish_tables_chance(Chance),
    random(X),
    (   X < Chance
    ->  abolish_all_tables,
        json_log_info("GC: abolished all tables (periodic cleanup)")
    ;   true
    ).

maybe_print_stats(Label) :-
    log_level('DEBUG'),
    !,
    print_stack_statistics_json(Label).
maybe_print_stats(_).

% GC thread wrapper - clears flag when done
gc_thread_wrapper :-
    % Reset streams to avoid inheriting closed HTTP streams
    set_prolog_IO(user_input, user_output, user_error),
    catch(
        do_garbage_collect,
        Error,
        json_log_error_formatted("GC error: ~w", [Error])
    ),
    with_mutex(auto_optimize_gc, retractall(gc_running)).

% Schedule async GC if not already running (mutex prevents race condition)
maybe_garbage_collect :-
    gc_chance(Chance),
    random(X),
    X < Chance,
    !,
    with_mutex(auto_optimize_gc, schedule_gc).
maybe_garbage_collect.

schedule_gc :-
    gc_running,
    !,
    json_log_debug("GC skipped (already running)").
schedule_gc :-
    assertz(gc_running),
    json_log_debug("GC scheduled"),
    thread_create(gc_thread_wrapper, _, [detached(true)]).

% Actual optimization work - optimizes all descriptors (parent-first order)
% This is used only as a synchronous fallback in non-server contexts.
do_optimize_descriptors(Validation_Objects) :-
    findall(Descriptor,
            (member(V, Validation_Objects),
             all_descriptor(V.descriptor, Descriptor)),
            Descriptors),
    sort_descriptors(Descriptors, Sorted),
    forall(member(D, Sorted), optimize(D)).

% Sort descriptors: branch first, then repository, then database last
% Database (_meta) is optimized last so branch/repo are done before its slow squash
descriptor_priority(D, 1) :- branch_descriptor{} :< D, !.
descriptor_priority(D, 2) :- repository_descriptor{} :< D, !.
descriptor_priority(D, 3) :- is_dict(D, system_descriptor), !.
descriptor_priority(D, 4) :- database_descriptor{} :< D, !.
descriptor_priority(_, 5).

sort_descriptors(Descriptors, Sorted) :-
    map_list_to_pairs(descriptor_priority, Descriptors, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

% Schedule a descriptor optimization through the commit queue so it runs when
% the relevant queue drains, rather than spawning a dedicated thread here.
schedule_optimizations(Validation_Objects) :-
    findall(Descriptor,
            (member(V, Validation_Objects),
             all_descriptor(V.descriptor, Descriptor)),
            Descriptors),
    sort_descriptors(Descriptors, Sorted),
    forall(member(D, Sorted), schedule_descriptor_optimization(D)).

schedule_descriptor_optimization(Descriptor) :-
    (   system_descriptor{} :< Descriptor
    ->  true
    ;   branch_descriptor{} :< Descriptor
    ->  commit_queue:schedule_branch_optimization(Descriptor)
    ;   (   database_descriptor{} :< Descriptor
        ;   repository_descriptor{} :< Descriptor
        )
    ->  commit_queue:schedule_database_optimization(Descriptor)
    ;   true
    ).

% Schedule async optimization if not already running (random check at commit level like GC)
maybe_optimize(Validation_Objects) :-
    optimize_chance(Chance),
    random(X),
    X < Chance,
    !,
    (   http_server_property(_, _)
    ->  schedule_optimizations(Validation_Objects)
    ;   do_optimize_descriptors(Validation_Objects)
    ).
maybe_optimize(_).

commit_has_changes(Meta_Data) :-
    get_dict(inserts, Meta_Data, Inserts),
    get_dict(deletes, Meta_Data, Deletes),
    (Inserts > 0 ; Deletes > 0),
    !.

plugins:post_commit_hook(Validation_Objects, Meta_Data) :-
    % GC runs async in dedicated thread
    maybe_garbage_collect,
    % Only optimize if the commit actually changed triples
    commit_has_changes(Meta_Data),
    % Optimization runs async in dedicated thread
    maybe_optimize(Validation_Objects).
