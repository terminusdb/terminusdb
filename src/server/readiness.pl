:- module(readiness, [
              register_components/0,
              readiness_pending/1,
              readiness_ready/1,
              readiness_failed/2,
              reset_readiness/0,
              await_readiness/2,
              await_readiness/4,
              mark_hooks_done/0,
              await_all_components/1,
              server_ready/0,
              publish_readiness/0,
              readiness_timeout/1
          ]).

:- use_module(core(plugins)).
:- use_module(core(util/json_log)).

/** <module> Startup readiness registry
 *
 * Tracks the lifecycle state of subsystems that must finish initializing
 * before the server should be considered ready. Plugins that gate
 * readiness declare a component via the multifile hook
 * plugins:readiness_component/1; the server calls register_components/0
 * before the HTTP socket starts accepting and mark_hooks_done/0 after
 * the post-startup hooks have run.
 *
 * Callers that depend on a component use await_readiness/2 — an
 * event-based wait on per-waiter message queues (no polling). A request
 * blocked in await_readiness wakes the instant the component reports
 * ready or failed; an unregistered component returns `ready`
 * immediately so untracked paths behave exactly as before.
 *
 * server_ready/0 also drives the Rust-held readiness flag behind
 * /api/v1/health (published through $appserver:appserver_set_ready/1)
 * and the Prolog /api/ok handler.
 */

:- dynamic component_state/2,     % component_state(Component, pending | ready | failed(Error))
           readiness_waiter/2,    % readiness_waiter(Component, Queue)
           hooks_done/0.

%% register_components is det.
%
%  Mark every declared readiness component pending. Runs before the HTTP
%  socket binds so the full accept-to-initialized window is covered.
register_components :-
    forall(readiness_component(Component),
           readiness_pending(Component)).

%% readiness_pending(+Component) is det.
readiness_pending(Component) :-
    set_component_state(Component, pending),
    publish_readiness.

%% readiness_ready(+Component) is det.
readiness_ready(Component) :-
    set_component_state(Component, ready),
    publish_readiness.

%% readiness_failed(+Component, +Error) is det.
readiness_failed(Component, Error) :-
    set_component_state(Component, failed(Error)),
    publish_readiness.

%% set_component_state(+Component, +State) is det.
%
%  Replace the component state and, for terminal states, wake every
%  waiting request. State update and waiter notification happen under
%  the readiness mutex so a subscriber that checked pending under the
%  same mutex can never miss a notification.
set_component_state(Component, State) :-
    with_mutex(readiness,
               (   retractall(component_state(Component, _)),
                   assertz(component_state(Component, State)),
                   (   State == pending
                   ->  true
                   ;   forall(readiness_waiter(Component, Queue),
                              thread_send_message(Queue, State))))).

%% reset_readiness is det.
%
%  Drop all readiness state including the hooks_done flag. Test and
%  in-process-restart support only — never on a running server.
reset_readiness :-
    with_mutex(readiness,
               (   retractall(component_state(_, _)),
                   retractall(readiness_waiter(_, _)),
                   retractall(hooks_done))).

%% mark_hooks_done is det.
%
%  Called by the server after the post-server-startup hooks have run.
%  From this point server readiness is decided solely by component
%  states.
mark_hooks_done :-
    (   hooks_done
    ->  true
    ;   assertz(hooks_done)),
    publish_readiness.

%% await_all_components(-Failed) is det.
%
%  Block until every registered component reaches a terminal state. Boot
%  uses this instead of the request deadline — startup may take as long as
%  initialization takes (matching the old synchronous hooks), and failures
%  are reported, not timed out. Failed = [failed(Component, Error), ...].
await_all_components(Failed) :-
    findall(Name, component_state(Name, _), Names),
    findall(failed(Name, Error),
            (   member(Name, Names),
                await_terminal(Name, failed(Error))),
            Failed).

%% await_terminal(+Component, -State) is det.
%
%  Event-based wait with no deadline. Unregistered components report
%  ready, like await_readiness/2.
await_terminal(Component, State) :-
    (   component_state(Component, pending)
    ->  message_queue_create(Queue),
        setup_call_cleanup(
            with_mutex(readiness,
                       (   component_state(Component, pending)
                       ->  assertz(readiness_waiter(Component, Queue)),
                           Subscribed = true
                       ;   Subscribed = false)),
            % Fetch with a free variable: thread_get_message/2 blocks until a
            % message *unifies*, so a constrained State would wait forever.
            (   Subscribed == true
            ->  thread_get_message(Queue, Message),
                State = Message
            ;   component_state(Component, State)),
            (   with_mutex(readiness, retractall(readiness_waiter(Component, Queue))),
                message_queue_destroy(Queue)))
    ;   component_state(Component, State)
    ->  true
    ;   State = ready).

%% server_ready is semidet.
%
%  The server is ready when no component is pending or failed and either
%  the startup hooks have completed or no component ever registered
%  (untracked deployments keep reporting ready, as before).
server_ready :-
    \+ component_state(_, pending),
    \+ component_state(_, failed(_)),
    (   hooks_done
    ->  true
    ;   \+ component_state(_, _)).

%% publish_readiness is det.
%
%  Push the current server readiness to the Rust webserver flag behind
%  /api/v1/health. No-op when the FFI predicate is absent (swipl backend
%  or a build without the appserver predicates).
publish_readiness :-
    (   current_predicate('$appserver':appserver_set_ready/1)
    ->  (   server_ready -> Ready = true ; Ready = false ),
        '$appserver':appserver_set_ready(Ready)
    ;   true).

%% readiness_timeout(-Seconds) is det.
%
%  Maximum time a gated request waits for a component. Overridable with
%  TERMINUSDB_READINESS_TIMEOUT. Defaults to 6s in the built binary and
%  3s when serving from an interpreted toplevel (swipl, terminusdb.sh)
%  so a slow startup is caught immediately during development.
readiness_timeout(Seconds) :-
    (   getenv('TERMINUSDB_READINESS_TIMEOUT', Value),
        atom_number(Value, Seconds)
    ->  true
    ;   current_prolog_flag(executable, Executable),
        file_base_name(Executable, Base),
        sub_atom(Base, 0, _, _, swipl)
    ->  Seconds = 3
    ;   Seconds = 6).

%% await_readiness(+Component, -Outcome) is det.
%% await_readiness(+Component, +Timeout, +WarnAfter, -Outcome) is det.
%
%  Event-based wait on a component's readiness. Outcome is `ready`,
%  `failed(Error)` or `timeout`. A warning is logged once when the wait
%  exceeds WarnAfter seconds. Components that never registered return
%  `ready` immediately — untracked paths are not gated.
await_readiness(Component, Outcome) :-
    readiness_timeout(Timeout),
    await_readiness(Component, Timeout, 3, Outcome).

await_readiness(Component, Timeout, WarnAfter, Outcome) :-
    (   component_state(Component, pending)
    ->  subscribe_and_wait(Component, Timeout, WarnAfter, Outcome)
    ;   component_state(Component, State)
    ->  Outcome = State
    ;   Outcome = ready).

%% subscribe_and_wait(+Component, +Timeout, +WarnAfter, -Outcome) is det.
%
%  Re-check the state under the readiness mutex while subscribing, so a
%  transition between the first check and the subscribe can never be
%  missed. Then block on the private queue until woken or the deadline
%  passes.
subscribe_and_wait(Component, Timeout, WarnAfter, Outcome) :-
    get_time(Start),
    Deadline is Start + Timeout,
    WarnAt is Start + WarnAfter,
    message_queue_create(Queue),
    setup_call_cleanup(
        with_mutex(readiness,
                   (   component_state(Component, pending)
                   ->  assertz(readiness_waiter(Component, Queue)),
                       Subscribed = true
                   ;   Subscribed = false)),
        (   Subscribed == true
        ->  wait_slices(Component, Queue, WarnAfter, WarnAt, Deadline, Outcome)
        ;   component_state(Component, Outcome)
        ->  true
        ;   Outcome = ready),
        (   with_mutex(readiness, retractall(readiness_waiter(Component, Queue))),
            message_queue_destroy(Queue))).

%% wait_slices(+Component, +Queue, +WarnAfter, +WarnAt, +Deadline, -Outcome) is det.
%
%  Block in at most two slices: up to WarnAt, then — after logging the
%  slow-startup warning once — for the remainder up to Deadline.
wait_slices(Component, Queue, WarnAfter, WarnAt, Deadline, Outcome) :-
    get_time(Now),
    Wait1 is min(WarnAt, Deadline) - Now,
    (   Wait1 > 0,
        thread_get_message(Queue, Message, [timeout(Wait1)])
    ->  Outcome = Message
    ;   get_time(Now2),
        Now2 < Deadline
    ->  (   Now2 >= WarnAt
        ->  json_log_warning_formatted(
                'readiness: component ~w still initializing after ~w seconds — slow startup',
                [Component, WarnAfter])
        ;   true),
        Wait2 is Deadline - Now2,
        (   thread_get_message(Queue, Message, [timeout(Wait2)])
        ->  Outcome = Message
        ;   Outcome = timeout)
    ;   Outcome = timeout).

:- begin_tests(readiness).

test("await_readiness returns ready for an unregistered component",
     [setup(reset_readiness), cleanup(reset_readiness), true(Outcome == ready)]) :-
    await_readiness(unregistered, 1, 3, Outcome).

test("await_readiness returns the terminal state without waiting",
     [setup((reset_readiness, readiness_failed(broken, some_error))),
      cleanup(reset_readiness), true(Outcome == failed(some_error))]) :-
    await_readiness(broken, 1, 3, Outcome).

test("await_readiness blocks a pending component until it is ready",
     [setup((reset_readiness, readiness_pending(engine))), cleanup(reset_readiness)]) :-
    thread_create((sleep(0.2), readiness_ready(engine)), _, [detached(true)]),
    get_time(Start),
    await_readiness(engine, 5, 3, Outcome),
    get_time(End),
    assertion(Outcome == ready),
    assertion(End - Start >= 0.2).

test("await_readiness wakes waiters on failure",
     [setup((reset_readiness, readiness_pending(engine))), cleanup(reset_readiness)]) :-
    thread_create((sleep(0.2), readiness_failed(engine, init_broken)), _, [detached(true)]),
    get_time(Start),
    await_readiness(engine, 5, 3, Outcome),
    get_time(End),
    assertion(Outcome == failed(init_broken)),
    assertion(End - Start >= 0.2).

test("await_readiness wakes every waiter",
     [setup((reset_readiness, readiness_pending(engine))), cleanup(reset_readiness)]) :-
    message_queue_create(Done),
    forall(between(1, 3, _),
           thread_create((await_readiness(engine, 5, 3, O),
                          thread_send_message(Done, O)), _, [detached(true)])),
    sleep(0.1),
    readiness_ready(engine),
    findall(Outcome,
            (   between(1, 3, _),
                thread_get_message(Done, Outcome, [timeout(2)])),
            Outcomes),
    assertion(Outcomes == [ready, ready, ready]),
    message_queue_destroy(Done).

test("await_readiness times out when the component never reports",
     [setup((reset_readiness, readiness_pending(stuck))), cleanup(reset_readiness)]) :-
    get_time(Start),
    await_readiness(stuck, 0.3, 1, Outcome),
    get_time(End),
    assertion(Outcome == timeout),
    assertion(End - Start >= 0.25),
    assertion(End - Start < 2).

test("await_readiness warns once when the wait exceeds the warn threshold",
     [setup((reset_readiness, readiness_pending(slow))), cleanup(reset_readiness)]) :-
    new_memory_file(MF),
    stream_property(OldErr, alias(user_error)),
    setup_call_cleanup(
        (   open_memory_file(MF, write, W),
            set_stream(W, alias(user_error))),
        await_readiness(slow, 0.4, 0.1, Outcome),
        (   flush_output(W),
            set_stream(OldErr, alias(user_error)),
            close(W),
            open_memory_file(MF, read, R),
            read_string(R, _, Content),
            close(R),
            free_memory_file(MF))),
    assertion(Outcome == timeout),
    assertion(sub_string(Content, _, _, _, "still initializing")).

test("server_ready is true when nothing is tracked",
     [setup(reset_readiness), cleanup(reset_readiness)]) :-
    assertion(server_ready).

test("server_ready waits for hooks when components are tracked",
     [setup((reset_readiness, readiness_pending(engine))), cleanup(reset_readiness)]) :-
    assertion(\+ server_ready),
    readiness_ready(engine),
    assertion(\+ server_ready),
    mark_hooks_done,
    assertion(server_ready).

test("await_all_components collects failures after all resolve",
     [setup((reset_readiness,
             readiness_pending(a),
             readiness_pending(b),
             readiness_pending(c))),
      cleanup(reset_readiness)]) :-
    thread_create((sleep(0.05), readiness_ready(a)), _, [detached(true)]),
    thread_create((sleep(0.15), readiness_failed(b, boom)), _, [detached(true)]),
    thread_create((sleep(0.10), readiness_ready(c)), _, [detached(true)]),
    get_time(Start),
    await_all_components(Failed),
    get_time(End),
    assertion(Failed == [failed(b, boom)]),
    assertion(End - Start >= 0.15).

test("await_all_components returns empty when everything is ready",
     [setup((reset_readiness,
             readiness_pending(a),
             readiness_ready(a))),
      cleanup(reset_readiness), true(Failed == [])]) :-
    await_all_components(Failed).

test("await_all_components returns empty with no components",
     [setup(reset_readiness), cleanup(reset_readiness),
      true(Failed == [])]) :-
    await_all_components(Failed).

test("server_ready stays false for a failed component",
     [setup((reset_readiness,
             readiness_pending(engine),
             readiness_failed(engine, init_broken),
             mark_hooks_done)),
      cleanup(reset_readiness)]) :-
    assertion(\+ server_ready).

:- end_tests(readiness).
