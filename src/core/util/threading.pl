:- module('util/threading',
          [cpu_concurrent_findall/4,
           cpu_concurrent_forall/2,
           cpu_concurrent_findfirst/4]).

initialize_cpu_pool :-
    current_thread_pool(cpu_thread_pool),
    !.
initialize_cpu_pool :-
    current_prolog_flag(cpu_count, Count),
    thread_pool_create(cpu_thread_pool, Count, [backlog(infinite)]).

:- initialize_cpu_pool.

collect_from_queue(Queue, Count, Result) :-
    collect_from_queue_(Queue, Count, Count, [], Result).

collect_from_queue_(Queue, Max, Count, Acc, Result) :-
    collect_from_queue(Queue, Count, [], Result_Unsorted),
    append(Acc, Result_Unsorted, Result_Accumulated),
    sort(Result_Accumulated, Unprocessed_Result),
    maplist(element_index, Unprocessed_Result, Unprocessed_Result_Indexes),
    sort(Unprocessed_Result_Indexes, Sorted_Result_Indexes),

    holes_upto(Sorted_Result_Indexes, Max, Holes),

    (   Holes = []
    ->  Result = Unprocessed_Result
    ;   length(Holes, Holes_Length),
        max_list(Holes, New_Max),
        New_Max_1 is New_Max + 1,
        collect_from_queue_(Queue, New_Max_1, Holes_Length, Unprocessed_Result, Result)
    ).

collect_from_queue(_Queue, Count, Result, Result) :-
    Count is 0,
    !.
collect_from_queue(Queue, Count, Acc, Result) :-
    thread_get_message(Queue, Message),
    Next_Count is Count - 1,
    collect_from_queue(Queue, Next_Count, [Message|Acc], Result).

element_index(error(Index-_), Index).
element_index(failure(Index), Index).
element_index(success(Index-_), Index).

holes_upto(Sorted, Count, Holes) :-
    holes_upto(Sorted, Count, 0, Holes).

holes_upto([], Count, Index, [Index|Holes]) :-
    Index < Count,
    !,
    New_Index is Index + 1,
    holes_upto([], Count, New_Index, Holes).
holes_upto([], _Count, _Index, []).
holes_upto([_Elt|_Rest], Count, Index, []) :-
    Index >= Count,
    !.
holes_upto([Elt|Rest], Count, Index, [Index|Holes]) :-
    Index < Elt,
    !,
    New_Index is Index + 1,
    holes_upto([Elt|Rest], Count, New_Index, Holes).
holes_upto([_Elt|Rest], Count, Index, Holes) :-
    New_Index is Index + 1,
    holes_upto(Rest, Count, New_Index, Holes).



process_concurrent_findall_result([], []).
process_concurrent_findall_result([success(_Count-Template)|Unprocessed], [Template|Rest]) :-
    !,
    process_concurrent_findall_result(Unprocessed, Rest).
process_concurrent_findall_result([_|Unprocessed], Rest) :-
    process_concurrent_findall_result(Unprocessed, Rest).

:- meta_predicate cpu_concurrent_findall(+, :, :, -).
cpu_concurrent_findall(Template, Generator, Concurrent, Result) :-
    setup_call_cleanup(
        message_queue_create(Q, []),
        (
            cpu_concurrent_findall_(Q, Template, Generator, Concurrent, Count),

            collect_from_queue(Q, Count, Sorted),
            (   [error(_-E)|_] = Sorted
            ->  throw(E)
            ;   process_concurrent_findall_result(Sorted, Result))
        ),
        message_queue_destroy(Q)).

:- meta_predicate cpu_concurrent_findall_(+, +, :, :, -).
cpu_concurrent_findall_(Queue, Template, Generator, Concurrent, Final_Count) :-
    setup_call_cleanup(
        message_queue_create(Error_Queue, []),
        (
            Count = count(0),
            Threads = threads([]),
            (   call(Generator),
                count(C) = Count,
                (   thread_peek_message(Error_Queue, Errored_Count)
                ->  !,
                    threads(Interrupt_Threads) = Threads,
                    interrupt_threads(Interrupt_Threads, Errored_Count),
                    Final_Count is Errored_Count + 1
                ;   (   thread_create_in_pool(cpu_thread_pool,
                                              (   catch(call(Concurrent),
                                                        E,
                                                        E=E)
                                              ->  (   nonvar(E)
                                                  ->  thread_send_message(Queue, error(C-E)),
                                                      thread_send_message(Error_Queue, C)
                                                  ;   thread_send_message(Queue, success(C-Template)))
                                              ;   thread_send_message(Queue, failure(C))),
                                              Thread,
                                              []),
                        threads(Old_Threads) = Threads,
                        nb_setarg(1, Threads, [C-Thread|Old_Threads]),
                        New_C is C + 1,
                        nb_setarg(1, Count, New_C),
                        fail))
            ;   Count = count(Final_Count))
        ),
        message_queue_destroy(Error_Queue)).

interrupt_thread(_C, T) :-
    catch(thread_signal(T, throw(interrupt)),
          error(existence_error(thread, _), _),
          true).

interrupt_threads([], _C).
interrupt_threads([C-T|Threads], Min_Kill) :-
    (   C > Min_Kill
    ->  interrupt_thread(C, T)
    ;   true),
    interrupt_threads(Threads, Min_Kill).

:- meta_predicate cpu_concurrent_forall(:, :).
cpu_concurrent_forall(Generator, Action) :-
    catch(
        (   cpu_concurrent_findall(_,
                                   Generator,
                                   (   Action
                                   ->  true
                                   ;   throw(failure)),
                                   _),
            Result=success),
        failure,
        Result=failure),

    Result = success.

:- meta_predicate cpu_concurrent_findall(+, :, :, -).
cpu_concurrent_findfirst(Template, Generator, Action, Result) :-
    catch(cpu_concurrent_findall(Template,
                                 Generator,
                                 (   Action,
                                     throw(success(Template))),
                                 _),
          success(R),
          Result = R),
    nonvar(Result).

:- begin_tests(cpu_concurrent_findall).
test(normal_result) :-
    List = [a, c, x, y, b, 42],
    cpu_concurrent_findall(X, member(X, List), sleep(1), Result),

    List = Result.

test(error,
     [error(foo)]) :-
    Instructions = [sleep(3),
                    error(foo),
                    sleep(5),
                    error(bar),
                    sleep(10000),
                    sleep(10000),
                    sleep(10000),
                    sleep(10000),
                    sleep(10000),
                    sleep(10000),
                    sleep(10000),
                    sleep(10000),
                    sleep(10000),
                    sleep(10000),
                    sleep(10000),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(0),
                    sleep(100)
                   ],
    cpu_concurrent_findall(X,
                           member(I, Instructions),
                           (   I = sleep(Amount)
                           ->  sleep(Amount), X=slept
                           ;   I = error(E)
                           ->  sleep(1),
                               throw(error(E, -))
                           ;   X = default),
                           _Result).

test(all_false) :-
    cpu_concurrent_findall(_,
                           member(_, [a,b,c,d,e,f]),
                           false,
                           []).
:- end_tests(cpu_concurrent_findall).
