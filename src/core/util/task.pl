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

%:- setup_default_task_logger.

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

wakeup_task_runner :-
    thread_send_message(task_runner, true).

:- meta_predicate wakeup_task_runner(:).
wakeup_task_runner(Goal) :-
    Goal,
    wakeup_task_runner.

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
work_available(Work) :-
    task_queue(wait_blocking(Waiting_Queue, Task)),
    task_info(Task, _, result(_)),
    retract(task_queue(wait_blocking(Waiting_Queue, Task))),
    Work = wakeup_queue(Waiting_Queue).

wait_for_work_available(Work) :-
    repeat,
    thread_get_message(_),
    work_available(Work),
    !.

run_task_runner :-
    setup_call_cleanup(
        init_task_runner,
        (   repeat,
            wait_for_work_available(Work),
            task_runner_step(Work),
            fail),
        cleanup_task_runner).

task_runner_step(start(Task, Worker)) :-
    !,
    claim_worker(Worker),
    thread_send_message(Worker, next(Task)).
task_runner_step(wakeup_queue(Q)) :-
    !,
    thread_send_message(Q, true).
task_runner_step(Message) :-
    task_log('ERROR', "Unknown Message: ~q", [Message]).

run_task_worker(Name) :-
    repeat,
    thread_get_message(Message),
    task_worker_step(Message,Name),
    wakeup_task_runner,
    fail.

task_worker_step(next(Engine), Name) :-
    !,
    task_set_state(Engine, _, started),
    engine_next_reified(Engine, Result),
    task_log('INFO', "result: ~q", [Result]),
    (   Result = the(waiting(Other_Task))
    ->  task_set_state(Engine, _, waiting),
        assert(task_queue(wait(Engine, Other_Task)))
    ;   Result = the(exception(E))
    ->  task_set_state(Engine, _, result(exception(E)))
    ;   Result = the(Answer)
    ->  task_set_state(Engine, _, result(Answer))
    ;   Result = no
        ->  task_set_state(Engine, _, result(failure))
    ;   Result = exception(E)
    ->  task_set_state(Engine, _, result(exception(E)))
    ;   task_set_state(Engine, _, result(strange))),
    free_worker(Name),

    (   (   Result = the(success(_))
        ;   Result = the(waiting(_)))
    ->  true % there are more results so we can't destroy the engine just yet
    ;   engine_destroy(Engine)).
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
            asserta(task_worker(available, Worker))
        )
    ).

:- dynamic task_info/3.

:- meta_predicate task_spawn(+, :, -).
task_spawn(Template, Goal, Task) :-
    engine_create(Result,
                  catch_with_backtrace(
                      reify_result(Template, Goal, Result),
                      E,
                      Result=exception(E)),
                  Task),
    (   engine_self(P),
        task_info(P, _, _)
    ->  Parent = some(P)
    ;   Parent = none),
    wakeup_task_runner(
        transaction((assert(task_info(Task, Parent, starting)),
                     assert(task_queue(start(Task)))))
    ).

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

wait_for_result_reified(Task, Result) :-
    engine_self(E),
    task_info(E, _, _),
    !,
    task_wait_for_result(Task, Result).
wait_for_result_reified(Task, Result) :-
    % not a task, block thread.
    thread_wait_for_result(Task, Result).

task_might_get_result(Task) :-
    once(task_info(Task, _, State)),
    (   State = result(_)
    ;   State = starting
    ;   State = started
    ;   State = waiting).

assert_task_might_get_result(Task) :-
    (   task_might_get_result(Task)
    ->  true
    ;   throw(error(waiting_for_task_that_wont_have_result(Task), _))).

task_wait_for_result(Task, Result) :-
    task_info(Task, Parent, result(Result)),
    !,
    retract(task_info(Task, Parent, result(Result))).
task_wait_for_result(Task, Result) :-
    assert_task_might_get_result(Task),
    engine_yield(waiting(Task)),
    task_info(Task, Parent, result(Result)),
    retract(task_info(Task, Parent, result(Result))).

thread_wait_for_result(Task, Result) :-
    assert_task_might_get_result(Task),
    (   task_info(Task, Parent, result(Result))
    ->  true
    ;   setup_call_cleanup(
            message_queue_create(Q),
            (   wakeup_task_runner(assert(task_queue(wait_blocking(Q, Task)))),
                thread_get_message(Q, _)),
            message_queue_destroy(Q)),
        (   task_info(Task, Parent, result(Result))
        ->  true
        ;   throw(error(task_had_no_result(Task), _)))),

    retract(task_info(Task, Parent, result(Result))).

wait_for_result(Task, Result) :-
    wait_for_result_reified(Task, Reified_Result),
    (   Reified_Result = success(Result)
    ;   Reified_Result = final(Result)
    ;   Reified_Result = exception(E),
        throw(E)),
    !.

demonstration(Result) :-
    task_spawn(R,
               (   task_spawn(R,
                              (   sleep(4),
                                  format("task 1 done waiting~n"),
                                  R = 12),
                              T1),
                   task_spawn(R,
                              (   sleep(3),
                                  format("task 2 done waiting~n"),
                                  R = 20),
                              T2),
                   task_spawn(R,
                              (   sleep(5),
                                  format("task 3 done waiting~n"),
                                  R = 10),
                              T3),
                   wait_for_result(T1, R1),
                   wait_for_result(T2, R2),
                   wait_for_result(T3, R3),

                   R is R1 + R2 + R3),
               Task),

    wait_for_result(Task, Result).



