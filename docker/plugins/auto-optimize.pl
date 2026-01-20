:- module('plugins/auto_optimize', []).
:- use_module(core(api)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(library(http/http_server)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(thread_pool)).

% Requirement to add to plugins since TerminusDB 11.2.0
:- multifile plugins:post_commit_hook/2.

% Copyright (c) TerminusDB, Licensed under the Apache License, Version 2.0
% Source: https://github.com/terminusdb-labs/terminusdb-plugins/tree/main

% Disable background Garbage Collection thread AND enable automatic GC on plugin load.
% GC will ONLY run via explicit gc_safe/0 calls which coordinate with Rust FFI.
:- set_prolog_flag(gc_thread, false).
:- set_prolog_flag(gc, false).
:- format(user_error, '% auto-optimize: swipl GC fully disabled, coordinated GC only~n', []).

optimize_chance(system_descriptor{}, C) =>
    C = 0.1.
optimize_chance(D, C), database_descriptor{} :< D =>
    C = 0.1.
optimize_chance(D, C), repository_descriptor{} :< D =>
    C = 0.1.
optimize_chance(D, C), branch_descriptor{} :< D =>
    C = 0.1.
optimize_chance(_, _) => false.

should_optimize(Descriptor) :-
    optimize_chance(Descriptor, Chance),
    random(X),
    X < Chance.

% GC runs on average every 10 commits (10% chance)
gc_chance(0.1).

should_garbage_collect :-
    gc_chance(Chance),
    random(X),
    X < Chance.

do_garbage_collect :-
    % Use gc_safe/0 from Rust gc_sync module - acquires write lock, waits for Rust term creation
    '$gc_sync':gc_safe,
    json_log_debug_formatted("Garbage collection triggered by auto-optimize plugin (gc_safe)", []).

optimize(Descriptor) :-
    api_optimize:descriptor_optimize(Descriptor),
    resolve_absolute_string_descriptor(Path, Descriptor),
    json_log_debug_formatted("Optimized ~s", [Path]).

all_descriptor(Descriptor, Descriptor).
all_descriptor(Descriptor, Parent_Descriptor) :-
    get_dict(repository_descriptor, Descriptor, Intermediate_Descriptor),
    all_descriptor(Intermediate_Descriptor, Parent_Descriptor).
all_descriptor(Descriptor, Parent_Descriptor) :-
    get_dict(database_descriptor, Descriptor, Intermediate_Descriptor),
    all_descriptor(Intermediate_Descriptor, Parent_Descriptor).

optimize_all(Validation_Objects) :-
    forall((member(V, Validation_Objects),
            all_descriptor(V.descriptor, Descriptor),
            should_optimize(Descriptor)),
           optimize(Descriptor)).

:- multifile thread_pool:create_pool/1.
thread_pool:create_pool(terminusdb_optimizer) :-
    current_prolog_flag(cpu_count, Count),
    thread_pool_create(terminusdb_optimizer, Count, []).

plugins:post_commit_hook(Validation_Objects, _Meta_Data) :-
    % Probabilistic garbage collection
    (   should_garbage_collect
    ->  do_garbage_collect
    ;   true
    ),
    % Probabilistic optimization (every ~10 commits)
    (   http_server_property(_, _)
    ->  catch(thread_create_in_pool(terminusdb_optimizer,
                                    optimize_all(Validation_Objects),
                                    _,
                                    [wait(false)]),
	      error(resource_error(threads_in_pool(terminusdb_optimizer)), _),
	      true
        )
    ;   optimize_all(Validation_Objects)
    ).
