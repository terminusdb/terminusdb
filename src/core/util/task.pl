:- module(task, []).

:- use_module(library(lists)).

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

:- dynamic task_queue/2.
work_available(Work) :-
    task_queue(cleanup, Task),
    retract(task_queue(cleanup, Task)),
    Work = cleanup(Task).
work_available(Work) :-
    task_queue(reap, Task),
    task_info(Task, _, Status),
    (   Status = result(exception(_))
    ;   Status = result(failure)
    ;   Status = result(final(_))
    ;   Status = killed
    ;   Status = strange),
    retract(task_queue(reap, Task)),
    Work = reap(Task).
work_available(Work) :-
    task_queue(reap, Task),
    task_info(Task, _, Status),
    (   Status = result(success(_))
    ;   Status = waiting
    ;   Status = ready),
    task_worker(available, Worker),
    retract(task_queue(reap, Task)),
    Work = start(Task, Worker).
work_available(Work) :-
    task_worker(available, Worker),
    task_queue(start, Task),
    retract(task_queue(start, Task)),
    task_info(Task, _, State),
    (   State = starting
    ;   State = ready),
    Work = start(Task, Worker).
work_available(Work) :-
    task_worker(available, Worker),
    task_queue(wait(Task), Waiting_Task),
    task_info(Task, _, result(_)),
    retract(task_queue(wait(Task), Waiting_Task)),
    Work = start(Waiting_Task, Worker).
work_available(Work) :-
    task_worker(available, Worker),
    task_queue(wait_any(Tasks), Waiting_Task),
    member(Task, Tasks),
    task_info(Task, _, result(_)),
    retract(task_queue(wait_any(Tasks), Waiting_Task)),
    Work = start(Waiting_Task, Worker).
work_available(Work) :-
    task_queue(wait_blocking(Waiting_Queue, Task), none),
    task_info(Task, _, result(_)),
    retract(task_queue(wait_blocking(Waiting_Queue, Task), none)),
    Work = wakeup_queue(Waiting_Queue).
work_available(Work) :-
    task_queue(wait_any_blocking(Waiting_Queue, Tasks), none),
    member(Task, Tasks),
    task_info(Task, _, result(_)),
    retract(task_queue(wait_any_blocking(Waiting_Queue, Tasks), none)),
    Work = wakeup_queue(Waiting_Queue).
work_available(Work) :-
    task_worker(available, Worker),
    task_queue(wait_send(Queue), Task),
    Work = start(Task, Worker),
    (   message_queue_property_noraise(Queue, max_size(Max_Size))
    ->  message_queue_property_noraise(Queue, size(Size)),
        Max_Size > Size
    ;   true),
    retract(task_queue(wait_send(Queue), Task)).
work_available(Work) :-
    task_worker(available, Worker),
    task_queue(wait_receive(Queue), Task),
    message_queue_property_noraise(Queue, size(Size)),
    Size > 0,
    Work = start(Task, Worker),
    retract(task_queue(wait_receive(Queue), Task)).

message_queue_property_noraise(Queue, Property) :-
    catch(message_queue_property(Queue, Property),
          error(existence_error(message_queue, Queue), _),
          fail).

consume_work :-
    repeat,
    (   once(work_available(Work))
    ->  task_runner_step(Work)
    ;   !),
    fail.

run_task_runner :-
    setup_call_cleanup(
        init_task_runner,
        catch((   repeat,
                  thread_get_message(_),
                  consume_work,
                  fail),
              E,
              task_log('ERROR', "task runner thread crashed: ~q", [E])),
        cleanup_task_runner).

task_runner_step(start(Task, Worker)) :-
    !,
    task_set_state(Task, _, started),
    claim_worker(Worker),
    thread_send_message(Worker, next(Task)).
task_runner_step(wakeup_queue(Q)) :-
    !,
    thread_send_message(Q, true).
task_runner_step(cleanup(Task)) :-
    !,
    perform_cleanup_task(Task).
task_runner_step(reap(Task)) :-
    !,
    % If we're here, the task has naturally ended or has been killed.
    % All that is left to do is remove its task info.
    % This will also destroy the engine in case this has not yet happened.
    retract(task_info(Task, _, _)),
    catch(engine_destroy(Task),
          error(existence_error(engine, Task), _),
          true).
task_runner_step(Message) :-
    task_log('ERROR', "Unknown Message: ~q", [Message]).

perform_cleanup_task(Task) :-
    forall(task_info(Child, some(Task), _),
           perform_cleanup_task(Child)),

    retractall(task_queue(_, Task)),

    task_info(Task, _, Result),
    (   (   Result = final(_)
        ;   Result = failure
        ;   Result = exception(_)
        ;   Result = killed
        ;   Result = starting)
    % engine already killed, or not yet started, great
    ->  true
    % all other cases, we have to inject
    % this may error because we're racing the task and it may terminate after the previous check but before the receiving of the signal
    ;   catch(thread_signal(Task, throw(task_kill)),
              error(existence_error(thread, Task), _),
              true)),


    % At this point, this task and all its children are terminated or
    % have a signal sent to them to be terminated. What is left to do
    % is reap.
    assert(task_queue(reap, Task)).

run_task_worker(Name) :-
    repeat,
    thread_get_message(Message),
    task_worker_step(Message,Name),
    wakeup_task_runner,
    fail.

task_worker_step(next(Engine), Name) :-
    !,
    thread_self(Thread_Self),
    task_log('DEBUG', "~q: claimed worker ~q", [Engine, Thread_Self]),
    engine_next_reified(Engine, Result),
    task_log('DEBUG', "~q: result: ~q", [Engine, Result]),
    (   Result = the(waiting(Other_Task))
    ->  task_set_state(Engine, _, waiting),
        assert(task_queue(wait(Other_Task), Engine))
    ;   Result = the(waiting_any(Other_Tasks))
    ->  task_set_state(Engine, _, waiting),
        assert(task_queue(wait_any(Other_Tasks), Engine))
    ;   Result = the(waiting_send(Queue))
    ->  task_set_state(Engine, _, waiting),
        assert(task_queue(wait_send(Queue), Engine))
    ;   Result = the(waiting_receive(Queue))
    ->  task_set_state(Engine, _, waiting),
        assert(task_queue(wait_receive(Queue), Engine))
    ;   Result = the(exception(task_kill))
    ->  task_set_state(Engine, _, killed),
        assert(task_queue(reap, Engine))
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
        ;   Result = the(waiting(_))
        ;   Result = the(waiting_any(_))
        ;   Result = the(waiting_send(_))
        ;   Result = the(waiting_receive(_)))
    ->  true % there are more results so we can't destroy the engine just yet
    ;   engine_destroy(Engine),
        task_log('DEBUG', "~q: destroyed", [Engine])),
    task_log('DEBUG', "~q: freed worker ~q", [Engine, Thread_Self]).
task_worker_step(Message, _Name) :-
    task_log('ERROR', "Unknown worker message: ~q", [Message]).

claim_worker(Worker) :-
    transaction(
        (
            (   task_worker(available, Worker)
            ->  true
            ;   throw(error(worker_unavailable(Worker), _))),
            retractall(task_worker(_, Worker)),
            asserta(task_worker(busy, Worker))
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
    (   on_task_engine(P)
    ->  Parent = some(P)
    ;   Parent = none),
    wakeup_task_runner(
        transaction((assert(task_info(Task, Parent, starting)),
                     assert(task_queue(start, Task))))
    ).

task_continue(Task) :-
    (   task_info(Task, _, State)
    ->  true
    ;   throw(error(existence_error(task, Task), _))),

    (   State = ready
    ->  true
    ;   throw(error(task_not_ready(Task),_))),

    wakeup_task_runner(assert(task_queue(start, Task))).

:- meta_predicate task_spawn(:, -).
task_spawn(Goal, Task) :-
    task_spawn(_, Goal, Task).

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

on_task_engine :-
    on_task_engine(_).

on_task_engine(E) :-
    engine_self(E),
    task_info(E, _, _).

:- meta_predicate reify_result(+, :, -).
reify_result(Template, Clause, Result) :-
    prolog_current_choice_reified(Choice),

    (   Clause
    *-> (   prolog_current_choice_reified(Choice,1)
        ->  Result = final(Template)
        ;   Result = success(Template))
    ;   Result = failure).

reified_result_is_final(final(_)).
reified_result_is_final(exception(_)).
reified_result_is_final(failure).
reified_result_is_final(killed).

wait_for_result_reified(Task, Result) :-
    wait_for_result_reified_(Task, Result),
    (   reified_result_is_final(Result)
    ->  cleanup_task(Task)
    ;   task_set_state(Task, _, ready)).
wait_for_result_reified_(Task, Result) :-
    on_task_engine,
    !,
    task_wait_for_result(Task, Result).
wait_for_result_reified_(Task, Result) :-
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
    task_info(Task, _, result(Result)),
    !.
task_wait_for_result(Task, Result) :-
    assert_task_might_get_result(Task),
    engine_yield(waiting(Task)),
    task_info(Task, _, result(Result)).

thread_wait_for_result(Task, Result) :-
    assert_task_might_get_result(Task),
    (   task_info(Task, _, result(Result))
    ->  true
    ;   setup_call_cleanup(
            message_queue_create(Q),
            (   wakeup_task_runner(assert(task_queue(wait_blocking(Q, Task), none))),
                thread_get_message(Q, _)),
            message_queue_destroy(Q)),
        (   task_info(Task, _, result(Result))
        ->  true
        ;   throw(error(task_had_no_result(Task), _)))).

unreify_result(Reified_Result, Result) :-
    (   Reified_Result = success(Result)
    ;   Reified_Result = final(Result)
    ;   Reified_Result = exception(E),
        throw(E)),
    !.

wait_for_result(Task, Result) :-
    wait_for_result_reified(Task, Reified_Result),
    unreify_result(Reified_Result, Result).

wait_for_results_bt_reified(Task, Result) :-
    setup_call_cleanup(
        true,
        (   repeat,
            wait_for_result_reified(Task, Result),
            (   reified_result_is_final(Result)
            ->  !
            ;   task_set_state(Task, _, starting),
                wakeup_task_runner(assert(task_queue(start, Task))))),
        cleanup_task(Task)).

wait_for_results_bt(Task, Result) :-
    wait_for_results_bt_reified(Task, Reified_Result),
    unreify_result(Reified_Result, Result).

wait_for_any_result_reified(Tasks, Task-Result) :-
    wait_for_any_result_reified_(Tasks, Task-Result),
    (   reified_result_is_final(Result)
    ->  cleanup_task(Task)
    ;   task_set_state(Task, _, ready)).
wait_for_any_result_reified_(Tasks, Result) :-
    on_task_engine,
    !,
    task_wait_for_any_result(Tasks, Result).
wait_for_any_result_reified_(Tasks, Result) :-
    % not a task, block thread.
    thread_wait_for_any_result(Tasks, Result).

task_wait_for_any_result(Tasks, Task-Result) :-
    member(Task, Tasks),
    task_info(Task, _, result(Result)),
    !.
task_wait_for_any_result(Tasks, Task-Result) :-
    forall(member(Task, Tasks),
           assert_task_might_get_result(Task)),
    engine_yield(waiting_any(Tasks)),
    member(Task, Tasks),
    task_info(Task, _, result(Result)),
    !.

thread_wait_for_any_result(Tasks, Task-Result) :-
    forall(member(Task, Tasks),
           assert_task_might_get_result(Task)),
    (   member(Task, Tasks),
        task_info(Task, _, result(Result))
    ->  true
    ;   setup_call_cleanup(
            message_queue_create(Q),
            (   wakeup_task_runner(assert(task_queue(wait_any_blocking(Q, Tasks), none))),
                thread_get_message(Q, _)),
            message_queue_destroy(Q)),
        (   member(Task, Tasks),
            task_info(Task, _, result(Result))
        ->  true
        ;   throw(error(tasks_had_no_result(Tasks), _)))).

wait_for_any_result(Tasks, Task-Result) :-
    wait_for_any_result_reified(Tasks, Task-Reified_Result),
    unreify_result(Reified_Result, Result).

wait_for_all_results_bt_reified([], _Result) :-
    !,
    fail.
wait_for_all_results_bt_reified(Tasks, Result) :-
    setup_call_cleanup(
        true,
        (   repeat,
            wait_for_any_result_reified(Tasks, Task-R),
            (   reified_result_is_final(R)
            ->  !,
                (   [Task] = Tasks
                ->  Result = Task-R
                ;   delete(Tasks, Task, New_Tasks),
                    (   Result = Task-R
                    ;   wait_for_all_results_bt_reified(New_Tasks, Result)))
            ;   Result = Task-R,
                task_set_state(Task, _, starting),
                wakeup_task_runner(assert(task_queue(start, Task))))),
        forall(member(Task, Tasks),
               cleanup_task(Task))).

wait_for_all_results_bt(Tasks, Task-Result) :-
    wait_for_all_results_bt_reified(Tasks, Task-Reified_Result),
    unreify_result(Reified_Result, Result).

cleanup_task(Task) :-
    assert(task_queue(cleanup, Task)),
    wakeup_task_runner.

%%% Message Queues %%%
task_message_queue_create(Queue) :-
    task_message_queue_create(Queue, []).

task_message_queue_create(Queue, Options) :-
    % TODO if this is a task we should register the queue somewhere with the current task listed as parent, so it will cleanup
    message_queue_create(Queue, Options).

task_message_queue_destroy(Queue) :-
    setup_call_cleanup(true,
                       % TODO CLEANUP
                       true,
                       catch(message_queue_destroy(Queue),
                             error(existence_error(message_queue, Queue), _),
                             true)).

task_get_message(Queue, Message) :-
    on_task_engine,
    !,
    task_wait_get_message(Queue, Message).
task_get_message(Queue, Message) :-
    thread_get_message(Queue, Message),
    wakeup_task_runner.

task_wait_get_message(Queue, Message) :-
    repeat,
    (   thread_get_message(Queue, Message, [timeout(0)])
    ->  !,
        wakeup_task_runner
    ;   engine_yield(waiting_receive(Queue)),
        fail).

task_send_message(Queue, Message) :-
    on_task_engine,
    !,
    task_wait_send_message(Queue, Message).
task_send_message(Queue, Message) :-
    thread_send_message(Queue, Message),
    wakeup_task_runner.

task_wait_send_message(Queue, Message) :-
    repeat,
    (   thread_send_message(Queue, Message, [timeout(0)])
    ->  !,
        wakeup_task_runner
    ;   engine_yield(waiting_send(Queue)),
        fail).

task_concurrent_goal(Template1, Generator, Template2, Goal, Result) :-
    % TODO queue limits
    setup_call_cleanup(
        (   task_message_queue_create(Work_Queue),
            task_message_queue_create(Collect_Queue),
            task_message_queue_create(Result_Queue)),

        (
            task_spawn(_,
                       task_concurrent_goal_generate(Template1, Generator, Work_Queue, Collect_Queue),
                       Generate_Task),

            % TODO worker limits
            current_prolog_flag(cpu_count, CPU_Count),
            findall(Worker_Task,
                    (   between(1, CPU_Count, _Worker),
                        task_spawn(_,
                                   task_concurrent_goal_worker(Template1, Template2, Goal, Work_Queue, Collect_Queue),
                                   Worker_Task)),
                    Worker_Tasks),

            task_spawn(_,
                       task_concurrent_goal_collect(Collect_Queue, Result_Queue),
                       Collect_Task),

            setup_call_cleanup(
                true,
                (   repeat,
                    task_get_message(Result_Queue, Message),
                    (   Message = result(Reified_Result)
                    ->  (   unreify_result(Reified_Result, Result)
                        ->  true
                        ;   !,
                            fail)
                    ;   Message = done
                        ->  !,
                            fail
                    ;   throw(error(unknown_result_message(Message), _)))),
                % TODO We really need some better approach to killing these tasks.
                % If something crashed before we get to this point we never end up doing the required cleanup.
                % We need some way to mark tasks as having a blocking thread as parent. Then the cleanup can just cleanup every task with this thread as its parent.
                (   cleanup_task(Generate_Task),
                    forall(member(Task, Worker_Tasks),
                           cleanup_task(Task)),
                    cleanup_task(Collect_Task)))),

        (   task_message_queue_destroy(Work_Queue),
            task_message_queue_destroy(Collect_Queue),
            task_message_queue_destroy(Result_Queue))).

task_concurrent_goal_generate(Template, Generator, Work_Queue, Collect_Queue) :-
    Counter = count(0),
    catch(forall(call(Generator),
                 (   Counter = count(Count),
                     task_send_message(Work_Queue, run(Count,Template)),
                     New_Count is Count + 1,
                     nb_setarg(1, Counter, New_Count))),
          E,
          task_send_message(Collect_Queue, generate_error(Count, E))),

    Counter = count(Count),
    (   var(E)
    ->  task_send_message(Collect_Queue, generate_done(Count))
    ;   true).

task_concurrent_goal_worker(Template1, Template2, Goal, Work_Queue, Collect_Queue) :-
    engine_self(E),
    repeat,
    task_get_message(Work_Queue, run(Count,Template1)),
    task_send_message(Collect_Queue, start(Count, E)),
    reify_result(Template2, Goal, Result),
    task_send_message(Collect_Queue, result(Count, Result)),
    fail.

task_concurrent_goal_collect(Collect_Queue, Result_Queue) :-
    empty_assoc(Results),
    task_concurrent_goal_collect(Collect_Queue, Result_Queue, Results, 0, _Done).

task_concurrent_goal_collect(Collect_Queue, Result_Queue, Results0, Cur, Done) :-
    task_get_message(Collect_Queue, Element),
    (   Element = start(Count, Engine)
    ->  (   (   Cur > Count
            ;   get_assoc(Count, Results0, _))
        % it's possible that the result message overtook the start message in a race. In that case we should just ignore it.
        ->  true
        ;   put_assoc(Count, Results0, running_on(Engine), Results2))
    ;   Element = result(Count, Result)
    % TODO check for error or failure and make sure we kill stuff then
    ->  (   del_assoc(Count, Results0, _, Results1)
        ->  true
        ;   Results1 = Results0),
        put_assoc(Count, Results1, result(Result), Results2)
    ;   Element = generate_error(Count, Error)
    ->  (   del_assoc(Count, Results0, _, Results1)
        ->  true
        ;   Results1 = Results0),
        put_assoc(Count, Results1, generate_error(Error), Results2)
    ;   Element = generate_done(Count)
    ->  Done = Count,
        Results2 = Results0
    ;   throw(error(idunno, _))),

    task_concurrent_goal_process(Result_Queue, Results2, Cur, New_Cur, New_Results),

    (   New_Cur == Done
    ->  task_send_message(Result_Queue, done)
    ;   task_concurrent_goal_collect(Collect_Queue, Result_Queue, New_Results, New_Cur, Done)).

task_concurrent_goal_process(Result_Queue, Results, Cur, New_Cur, New_Results) :-
    get_assoc(Cur, Results, Result),
    Result \= running_on(_),
    !,
    task_send_message(Result_Queue, Result),
    Next_Cur is Cur + 1,
    del_assoc(Cur, Results, _, Next_Results),
    task_concurrent_goal_process(Result_Queue, Next_Results, Next_Cur, New_Cur, New_Results).
task_concurrent_goal_process(_Result_Queue, Results, Cur, Cur, Results).

demonstration(Result) :-
    task_spawn(R,
               (   task_spawn(R,
                              (   sleep(4),
                                  format("task 1 done waiting~n"),
                                  R = 12),
                              T1),
                   task_spawn(R,
                              (   task_spawn(R,
                                             (   sleep(2),
                                                 R = 10),
                                             T2_1),
                                  task_spawn(R,
                                             (   sleep(3),
                                                 R = 10),
                                            T2_2),
                                  wait_for_result(T2_1, R1),
                                  wait_for_result(T2_2, R2),
                                  format("task 2 done waiting~n"),
                                  R is R1 + R2),
                              T2),
                   task_spawn(R,
                              (   sleep(5),
                                  format("task 3 done waiting~n"),
                                  R = 10),
                              T3),
                   task_spawn(R,
                              (   sleep(30),
                                  format("I will never be printed~n"),
                                  % useless task that we will just kill when the rest is done
                                  true),
                              _T4),
                   wait_for_result(T1, R1),
                   format("result 1: ~q~n", [R1]),
                   wait_for_result(T2, R2),
                   format("result 2: ~q~n", [R2]),
                   wait_for_result(T3, R3),
                   format("result 3: ~q~n", [R3]),

                   R is R1 + R2 + R3),
               Task),

    wait_for_result(Task, Result).

demonstration_2(Result) :-
    task_spawn(Result,
               (   task_spawn(R,
                              (   sleep(2),
                                  (   R = 1
                                  ;   (   sleep(3),
                                          R=2))),
                              T1),

                   task_spawn(R,
                              (   sleep(3),
                                  R = 3),
                              T2),

                   findall(R,
                           wait_for_all_results_bt([T1, T2], R),
                           Result)),
               Task),

    wait_for_result(Task, Result).


demonstration_3 :-
    task_message_queue_create(Queue1),
    task_message_queue_create(Queue2),

    task_spawn(Message,
               (
                   task_get_message(Queue1, Message),
                   format("message 1: ~q~n", [Message]),
                   sleep(2),
                   task_send_message(Queue2, world)
               ),
               E1),
    task_spawn(Message,
               (   sleep(2),
                   task_send_message(Queue1, hello),
                   task_get_message(Queue2, Message),
                   format("message 2: ~q~n", [Message])
               ),
               E2),

    wait_for_result(E1, R1),
    wait_for_result(E2, R2),
    format("~q ~q~n", [R1, R2]).


demonstration_4 :-
    task_message_queue_create(Queue, [max_size(1)]),

    task_spawn(_,
               (
                   string_chars("Hello, world", Cs),
                   forall(member(C, Cs),
                          task_send_message(Queue, C)),

                   task_send_message(Queue, done)
               ),
               E1),
    task_spawn(_,
               (   repeat,
                   sleep(1),
                   task_get_message(Queue, Message),
                   (   Message = done
                   ->  !,
                       nl
                   ;   write(Message),
                       nl,
                       fail)
               ),
               E2),

    wait_for_result(E1, _),
    wait_for_result(E2, _).
