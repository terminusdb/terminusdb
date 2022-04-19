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
    (   is_thread(task_runner)
    ->  true
    ;   thread_create(run_task_runner, _, [alias(task_runner)])).

kill_task_runner :-
    ignore(thread_signal(task_runner, throw(task(kill)))),
    thread_join(task_runner, _).

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

:- dynamic task_queue/1.

work_available(Work) :-
    task_worker(available, Worker),
    task_queue(start(Task)),
    retract(task_queue(start(Task))),
    Work = start(Task, Worker).
work_available(Work) :-
    task_worker(available, Worker),
    task_queue(wait(Waiting_Task, Task)),
    task_info(Task, _, result(_)),
    retract(task_queue(wait(Waiting_Task, Task))),
    Work = start(Waiting_Task, Worker).

run_task_runner :-
    setup_call_cleanup(
        init_task_runner,
        (   repeat,
            thread_wait(work_available(Work),
                        [wait_preds([+(task_worker/2),
                                     +(task_queue/1)])]),
            task_runner_step(Work),
            fail),
        cleanup_task_runner).

task_runner_step(start(Task, Worker)) :-
    !,
    claim_worker(Worker),
    thread_send_message(Worker, next(Task)).
task_runner_step(Message) :-
    task_log('ERROR', "Unknown Message: ~q", [Message]).

run_task_worker(Name) :-
    repeat,
    thread_get_message(Message),
    task_worker_step(Message,Name),
    fail.

task_worker_step(next(Engine), Name) :-
    !,
    task_set_state(Engine, _, started),
    engine_next_reified(Engine, Result),
    task_log('INFO', "result: ~q", [Result]),
    (   Result = the(waiting)
    ->  task_set_state(Engine, _, waiting),
        task_log('INFO', 'task is now waiting: ~q', [Engine])
    ;   Result = the(Answer)
    ->  task_set_state(Engine, _, result(Answer))
    ;   Result = no
        ->  task_set_state(Engine, _, result(failure))
    ;   Result = exception(E)
    ->  task_set_state(Engine, _, result(exception(E)))
    ;   task_set_state(Engine, _, result(strange))),
    free_worker(Name),

    (   (   Result = the(success(_))
        ;   Result = the(waiting))
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
    task_log('DEBUG', "Before transaction for freeing worker ~q", [Worker]),
    transaction(
        (
            task_log('DEBUG', "In transaction for freeing worker ~q", [Worker]),
            (   task_worker(busy, Worker)
            ->  true
            ;   throw(error(worker_not_busy(Worker), _))),
            retractall(task_worker(_, Worker)),
            asserta(task_worker(available, Worker)),
            task_log('DEBUG', "Freed worker ~q", [Worker])
        )
    ).

:- dynamic task_info/3.

:- meta_predicate task_spawn(+, :, -).
task_spawn(Template, Goal, Task) :-
    engine_create(Result,
                  reify_result(Template, Goal, Result),
                  Task),
    (   engine_self(P),
        task_info(P, _, _)
    ->  Parent = some(P)
    ;   Parent = none),
    assert(task_info(Task, Parent, starting)),
    assert(task_queue(start(Task))).

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

wait_for_result(Task, Result) :-
    engine_self(E),
    task_info(E, _, _),
    !,
    task_wait_for_result(E, Task, Result).
wait_for_result(Task, Result) :-
    % not a task, block thread.
    thread_wait_for_result(Task, Result).

task_might_get_result(Task) :-
    snapshot((once(task_info(Task, _, State)),
              (   State = result(_)
              ;   State = starting
              ;   State = started
              ;   State = waiting))).

assert_task_might_get_result(Task) :-
    (   task_might_get_result(Task)
    ->  true
    ;   throw(error(waiting_for_task_that_wont_have_result(Task), _))).

task_wait_for_result(_Self, Task, Result) :-
    task_info(Task, Parent, result(Result)),
    !,
    retract(task_info(Task, Parent, result(Result))).
task_wait_for_result(Self, Task, Result) :-
    assert_task_might_get_result(Task),
    assert(task_queue(wait(Self, Task))),
    engine_yield(waiting),
    task_info(Task, Parent, result(Result)),
    retract(task_info(Task, Parent, result(Result))).

thread_wait_for_result(Task, Result) :-
    % I'd prefer using assert_task_might_get_result here but it
    % weirdly kills the transaction mechanism and I'm not sure why.
    thread_wait((   task_might_get_result(Task)
                ->  R=result(Result),
                    task_info(Task, Parent, R)
                ;   R=no_result),
                [wait_preds([+(task_info/3)])]),

    (   R = no_result
    ->  throw(error(waiting_for_task_that_wont_have_result(Task), _))
    ;   true),
    format("done waiting!~n"),

    retract(task_info(Task, Parent, result(Result))).
