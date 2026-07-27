:- module(webserver_events, []).

:- use_module(core(plugin_api)).
:- use_module(library(lists)).

:- dynamic event_stream/1.

:- multifile appserver_hooks:appserver_stream/3.
:- multifile plugins:post_commit_hook/2.

%% appserver_hooks:appserver_stream(+Method, +Path, +Handler) is det.
%
%  Register the `/api/v1/ext/events` NDJSON event stream endpoint.
appserver_hooks:appserver_stream(get, '/api/v1/ext/events', webserver_events:events_handler).

%% events_handler(+Request, +StreamId, -Response) is det.
%
%  Remember the stream id so the post_commit_hook can broadcast to it.
%  The stream is removed automatically when Rust detects the client has
%  disconnected, but a failed send will also retract it here.
events_handler(_Request, StreamId, Response) :-
    assertz(event_stream(StreamId)),
    Response = _{
        status: 200,
        headers: _{'Content-Type': 'application/x-ndjson'},
        body: stream
    }.

%% post_commit_hook(+Validations, +Meta_Data) is det.
%
%  Broadcast every commit as an NDJSON event to all connected listeners.
%  This hook is best-effort; failures are caught to avoid breaking commits.
%
%  This hook is unoptimized and run by prolog in a simple mode.
%
%  Note: Validations is a LIST of validation_object dicts, not a single
%  dict. We check the first element to determine if this is a real commit.
%  The cut was removed to allow other plugins' post_commit_hook clauses
%  to run after this one.
plugins:post_commit_hook(Validations, Meta_Data) :-
    catch(
        (   is_list(Validations),
            member(Validation, Validations),
            is_dict(Validation)
        ->  Event = _{
                event: commit,
                validations: Validations,
                meta_data: Meta_Data
            },
            findall(StreamId, event_stream(StreamId), StreamIds),
            forall(
                member(StreamId, StreamIds),
                (   '$appserver':appserver_stream_send(StreamId, Event)
                ->  true
                ;   retract(event_stream(StreamId))
                )
            )
        ;   true
        ),
        Error,
        json_log_error_formatted("Error emitting commit event: ~q", [Error])
    ).
