:- module(plugin_api_broadcast, [
              broadcast_subscribe/3,
              broadcast_send/2,
              broadcast_send_raw/2,
              broadcast_unsubscribe/2,
              broadcast_unsubscribe_all/1,
              subscriber/3
          ]).

/**
 * Channel broadcast: one Prolog thread per subscriber drains
 * a FIFO queue into the HTTP stream. The send path pushes the already
 * serialized payload to every queue — threads scale to subscriber
 * count.
 */

:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(http/json)).

:- multifile broadcast_subscribe/3.
:- dynamic broadcast_subscribe/3.
:- multifile broadcast_send/2.
:- dynamic broadcast_send/2.
:- multifile broadcast_send_raw/2.
:- dynamic broadcast_send_raw/2.
:- multifile broadcast_unsubscribe/2.
:- dynamic broadcast_unsubscribe/2.
:- multifile broadcast_unsubscribe_all/1.
:- dynamic broadcast_unsubscribe_all/1.

:- dynamic subscriber/3. % Channel, StreamId, Queue

% Test seam: assert deliver_hook/2 or close_hook/1 to replace the FFI
% calls; absent hooks fall through to the appserver stream predicates.
:- dynamic deliver_hook/2.
:- dynamic close_hook/1.

deliver(StreamId, Bytes) :-
    (   deliver_hook(StreamId, Bytes)
    ->  true
    ;   catch('$appserver':appserver_stream_send_raw(StreamId, Bytes), _, fail)
    ).

close_stream(StreamId) :-
    (   close_hook(StreamId)
    ->  true
    ;   catch('$appserver':appserver_stream_close(StreamId), _, true)
    ).

broadcast_subscribe(Channel, StreamId, TimeoutSecs) :-
    message_queue_create(Queue),
    thread_create(subscriber_main(Queue, StreamId, TimeoutSecs), _, [detached(true)]),
    with_mutex(plugin_api_broadcast, assertz(subscriber(Channel, StreamId, Queue))).

broadcast_send(Channel, Data) :-
    with_output_to(string(Json),
                   json_write_dict(current_output, Data, [as(string), width(0)])),
    broadcast_send_raw(Channel, Json).

% Matches appserver_broadcast_send_raw: the payload is one NDJSON line.
broadcast_send_raw(Channel, Json) :-
    string_concat(Json, "\n", Bytes),
    forall(subscriber(Channel, _StreamId, Queue),
           thread_send_message(Queue, Bytes)).

broadcast_unsubscribe(Channel, StreamId) :-
    forall(subscriber(Channel, StreamId, Queue),
           thread_send_message(Queue, close)).

broadcast_unsubscribe_all(StreamId) :-
    forall(subscriber(_Channel, StreamId, Queue),
           thread_send_message(Queue, close)).

subscriber_main(Queue, StreamId, TimeoutSecs) :-
    ignore(call_cleanup(
               subscriber_loop(Queue, StreamId, TimeoutSecs),
               (   close_stream(StreamId),
                   retractall(subscriber(_, StreamId, Queue)),
                   message_queue_destroy(Queue)
               ))).

subscriber_loop(Queue, StreamId, TimeoutSecs) :-
    thread_get_message(Queue, Msg, [timeout(TimeoutSecs)]),
    (   Msg == timeout
    ->  true
    ;   Msg == close
    ->  true
    ;   deliver(StreamId, Msg)
    ->  subscriber_loop(Queue, StreamId, TimeoutSecs)
    ;   true
    ).

:- begin_tests(broadcast).

% Contract: subscribe registers a queue and the writer thread delivers
% sent bytes; disconnect, unsubscribe and idle timeout close the
% subscriber. Tests replace the stream write/close FFI via the deliver
% and close hooks, so no real sockets exist; deliveries are recorded in
% the user module where every thread context can reach them.

:- dynamic user:sb_delivery/2, user:sb_close/1.

reset :-
    retractall(user:sb_delivery(_, _)),
    retractall(user:sb_close(_)),
    retractall(plugin_api_broadcast:deliver_hook(_, _)),
    retractall(plugin_api_broadcast:close_hook(_)),
    forall(plugin_api_broadcast:subscriber(_, Id, _),
           plugin_api_broadcast:broadcast_unsubscribe_all(Id)),
    sleep(0.1).

setup :-
    reset,
    assertz(plugin_api_broadcast:(deliver_hook(S, B) :-
              assertz(user:sb_delivery(S, B)))),
    assertz(plugin_api_broadcast:(close_hook(S) :-
              assertz(user:sb_close(S)))).

delivered(StreamId, Bytes) :-
    user:sb_delivery(StreamId, Bytes).

test(subscribe_send_delivers, [setup(setup), cleanup(reset)]) :-
    broadcast_subscribe(chan1, stream1, 60),
    broadcast_send_raw(chan1, "{\"n\":1}"),
    sleep(0.2),
    assertion(delivered(stream1, "{\"n\":1}\n")).

test(fifo_per_subscriber, [setup(setup), cleanup(reset)]) :-
    broadcast_subscribe(chan1, stream1, 60),
    broadcast_send_raw(chan1, "first"),
    broadcast_send_raw(chan1, "second"),
    sleep(0.2),
    findall(B, user:sb_delivery(stream1, B), Got),
    assertion(Got == ["first\n", "second\n"]).

test(channel_isolation, [setup(setup), cleanup(reset)]) :-
    broadcast_subscribe(chan1, stream1, 60),
    broadcast_subscribe(chan2, stream2, 60),
    broadcast_send_raw(chan1, "only-1"),
    sleep(0.2),
    assertion(delivered(stream1, "only-1\n")),
    assertion(\+ user:sb_delivery(stream2, _)).

test(unsubscribe_closes, [setup(setup), cleanup(reset)]) :-
    broadcast_subscribe(chan1, stream1, 60),
    broadcast_unsubscribe(chan1, stream1),
    sleep(0.2),
    assertion(user:sb_close(stream1)),
    broadcast_send_raw(chan1, "after"),
    sleep(0.1),
    assertion(\+ user:sb_delivery(stream1, _)).

test(broadcast_unsubscribe_all_closes_everything, [setup(setup), cleanup(reset)]) :-
    broadcast_subscribe(chan1, stream1, 60),
    broadcast_subscribe(chan2, stream1, 60),
    broadcast_subscribe(chan2, stream2, 60),
    broadcast_unsubscribe_all(stream1),
    sleep(0.2),
    assertion(user:sb_close(stream1)),
    assertion(\+ user:sb_close(stream2)),
    broadcast_send_raw(chan2, "to-2-only"),
    sleep(0.1),
    assertion(delivered(stream2, "to-2-only\n")).

test(write_failure_disconnects, [setup(setup), cleanup(reset)]) :-
    broadcast_subscribe(chan1, stream1, 60),
    retractall(plugin_api_broadcast:deliver_hook(_, _)),
    assertz(plugin_api_broadcast:(deliver_hook(_, _) :- fail)),
    broadcast_send_raw(chan1, "boom"),
    sleep(0.2),
    assertion(user:sb_close(stream1)),
    assertion(\+ subscriber(chan1, stream1, _)).

test(idle_timeout_closes, [setup(setup), cleanup(reset)]) :-
    broadcast_subscribe(chan1, stream1, 0.2),
    sleep(0.5),
    assertion(user:sb_close(stream1)).

:- end_tests(broadcast).
