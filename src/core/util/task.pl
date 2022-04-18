:- module(task, []).

:- dynamic loggers/1.

task_log(Severity, Message) :-
    forall(loggers(Logger),
           call(Logger, Severity, Message)).

task_log(Severity, Format, Args) :-
    format(string(Message), Format, Args),
    task_log(Severity, Message).

default_task_logger(Severity, Message) :-
    format(user_error, "~s: ~s\n", [Severity, Message]).

setup_default_task_logger :-
    retractall(loggers(default_task_logger)),
    asserta(loggers(default_task_logger)).

:- setup_default_task_logger.


start_task_runner :-
    thread_create(run_task_runner, _, [alias(task_runner)]).

kill_task_runner :-
    ignore(thread_signal(task_runner, throw(task(kill)))),
    thread_join(task_runner, _).

run_task_runner :-
    setup_call_cleanup(
        init_task_runner,
        (   repeat,
            task_runner_step,
            fail),
        cleanup_task_runner).

:- dynamic task_worker/2.
init_task_runner :-
    current_prolog_flag(cpu_count, CPU_Count),
    task_log('INFO', "Initializing ~d worker threads", [CPU_Count]),
    retractall(task_worker(_)),
    forall(between(1, CPU_Count, C),
           (   format(atom(Worker_Name), "task_runner_worker_~d", [C]),
               task_log('INFO', "Initialize worker ~s", [Worker_Name]),
               Affinity is C - 1,
               thread_create(run_task_worker(Worker_Name), _, [alias(Worker_Name), affinity([Affinity])]),
               assertz(task_worker(available, Worker_Name)),
               task_log('INFO', "thread ~s created", [Worker_Name])
           )).


cleanup_task_runner :-
    forall(task_worker(_, Worker),
           ignore((thread_signal(Worker, throw(task(kill))),
                   thread_join(Worker, _)))).

task_runner_step :-
    thread_get_message(Message),
    (   Message = start(Engine)
    ->  thread_wait(task_worker(available, Worker), [wait_preds([+(task_worker/2)])]),
        claim_worker(Worker),
        thread_send_message(Worker, start(Engine))
    ;   task_log('ERROR', "Unknown Message: ~q", [Message])).

run_task_worker(Name) :-
    repeat,
    thread_get_message(Message),
    task_worker_step(Message,Name),
    fail.

task_worker_step(start(Engine), Name) :-
    !,
    task_set_state(Engine, _, started),
    engine_next_reified(Engine, Result),
    task_log('INFO', "result: ~q", [Result]),
    (   Result = the(Answer)
    ->  task_set_state(Engine, _, result(Answer))
    ;   Result = no
        ->  task_set_state(Engine, _, result(failure))
    ;   Result = result(exception(E))
        ->  task_set_state(Engine, _, exception(E))
    ;   task_set_state(Engine, _, strange)),
    free_worker(Name),

    (   Result = the(success(_))
    ->  true % there are more results so we can't destroy the engine just yet
    ;   engine_destroy(Engine),
        task_log('INFO', "destroyed task engine ~q", [Engine])).
task_worker_step(Message, _Name) :-
    task_log('ERROR', "Unknown worker message: ~q", [Message]).

claim_worker(Worker) :-
    transaction(
        (
            (   task_worker(available, Worker)
            ->  true
            ;   throw(error(worker_unavailable(Worker), _))),
            retractall(task_worker(_, Worker)),
            asserta(task_worker(busy, Worker)),
            task_log('DEBUG', "Claimed worker ~q", [Worker])
        )
    ).

free_worker(Worker) :-
    transaction(
        (
            (   task_worker(busy, Worker)
            ->  true
            ;   throw(error(worker_not_busy(Worker), _))),
            retractall(task_worker(_, Worker)),
            asserta(task_worker(available, Worker)),
            task_log('DEBUG', "Freed worker ~q", [Worker])
        )
    ).

:- dynamic task_info/3.

% todo - we need to be a lot smarter about spawning
% this needs to be able to iterate over results with backtracking
% we can detect when we're done backtracking using prolog_current_choice(C)
% C will revert to what it was before calling goal when there's no more choice points.
% So that should allow us to return not just success or failure, but iterate over successes and mark the very last success (when backtracking is done) to ensure steadfastness.
:- meta_predicate task_spawn(+, :).
task_spawn(Template, Goal) :-
    engine_create(Result,
                  reify_result(Template, Goal, Result),
                  Engine),
    (   engine_self(P),
        task_info(P, _, _)
    ->  Parent = some(P)
    ;   Parent = none),
    format("parent ~q~n", [Parent]),
    assert(task_info(Engine, Parent, starting)),
    thread_send_message(task_runner,
                        start(Engine)).

:- meta_predicate task_spawn(:).
task_spawn(Goal) :-
    task_spawn(_, Goal).

task_set_state(Engine, Old_State, New_State) :-
    transaction((task_info(Engine, Parent, Old_State),
                 retract(task_info(Engine, Parent, Old_State)),
                 assert(task_info(Engine, Parent, New_State)))).

% Note: tracing any of the choice predicates will completely mess up
% this calculation as tracing introduces new choice points
prolog_current_choice_reified(Choice, some(Choice), N) :-
    N = 0,
    !.
prolog_current_choice_reified(Intermediate, Choice, N) :-
    Next_N is N - 1,
    prolog_choice_attribute(Intermediate, parent, P),
    prolog_current_choice_reified(P, Choice, Next_N).

prolog_current_choice_reified(Choice, N) :-
    (   prolog_current_choice(C),
        prolog_choice_attribute(C, parent, P),
        prolog_current_choice_reified(P, Choice, N)
    ->  true
    ;   Choice = none).
prolog_current_choice_reified(Choice) :-
    prolog_current_choice_reified(Choice, 0).


:- meta_predicate reify_result(+, :, -).
reify_result(Template, Clause, Result) :-
    prolog_current_choice_reified(Choice),

    (   Clause
    *-> (   prolog_current_choice_reified(Choice,1)
        ->  Result = final(Template)
        ;   Result = success(Template))
    ;   Result = failure).
