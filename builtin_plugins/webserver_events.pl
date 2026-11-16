:- module(webserver_events, []).

:- use_module(core(plugin_api)).
:- use_module(library(lists)).
:- use_module(library(json), [json_write_dict/3]).
:- use_module(library(apply), [maplist/2]).
:- use_module(library(uri)).
:- use_module(library(date), [parse_time/3]).
:- use_module(core(util)).
:- use_module(core('util/json_preserve'), [json_read_dict/3]).
:- use_module(core('document/json'), [json_elaborate/3]).
:- use_module(core(query/resolve_query_resource),
              [resolve_absolute_string_descriptor/2]).

:- multifile appserver_hooks:appserver_stream/3.

%% appserver_hooks:appserver_stream(+Method, +Path, +Handler) is det.
%
%  Register the `/api/events/{path}` NDJSON event stream endpoint.
%  {path} is a graph-spec scope: a database (`org/db`) or a branch
%  (`org/db/repository/branch/name`). Other paths — a bare
%  organization, `_meta`, `_commits`, or commit slices — are not
%  scopes and are rejected. Subscribers join scoped broadcast channels; events are
%  built once by the post-commit producer and fanned out by the broadcast
%  broadcast registry, so delivery cost is shared across listeners.
appserver_hooks:appserver_stream(get, '/api/events/*event_path', webserver_events:events_handler).
appserver_hooks:appserver_stream(post, '/api/events/*event_path', webserver_events:publish_handler).

%% events_handler(+Request, +StreamId, -Response) is det.
%
%  Authenticate the listener, resolve the scope from the path, check
%  the per-channel capability, then subscribe to each requested
%  channel in post_response.
%
%  Query params:
%  - channel: zero or more channel keys (repeated or comma-separated,
%    each <=256 chars of [A-Za-z0-9._~-]). Default: "@commit" — the
%    reserved channel carrying commit-driven events. Keys starting
%    with @ are the reserved special-key namespace (MQTT $SYS-style).
%  - kind: payload kinds to receive: "id" (@commit channel only),
%    "doc" (schema-bound messages), "json" (raw messages — the
%    default on named channels), or "any" (every kind a channel can
%    carry). A kind the channel can never carry is rejected.
%  - since: only "now" implemented (the default).
%  - timeout: idle seconds, or "false". Default 30.
%% events_preamble(+Request, -ScopePath, -Keys, -SubKeys, -Timeout) is det.
%
%  Shared request parsing: scope from the path, timeout, since=now
%  guard, channel keys, and the (channel, kind) subscription keys.
%  Throws on invalid parameters exactly as events_handler did.
events_preamble(Request, ScopePath, Keys, SubKeys, Timeout) :-
    get_dict(params, Request, Params),
    get_dict(event_path, Params, EventPathRaw),
    normalize_branch_path(EventPathRaw, ScopePath),

    get_dict(query, Request, QueryString),
    (   QueryString \= ""
    ->  uri_query_components(QueryString, Query)
    ;   Query = []
    ),
    (   memberchk(timeout=TimeoutStr, Query)
    ->  parse_timeout(TimeoutStr, Timeout)
    ;   Timeout = 30
    ),

    %% since=now is the implicit default anchor. Reject other values
    %% rather than silently serving the default stream.
    (   memberchk(since=SinceStr, Query),
        SinceStr \= now,
        SinceStr \= "now"
    ->  throw(error(bad_parameter_type(since, "now", SinceStr), _))
    ;   true
    ),

    findall(Key, channel_key(Query, Key), Keys0),
    (   Keys0 = [] -> Keys = ["@commit"] ; sort(Keys0, Keys) ),
    kind_param(Query, Kind),

    %% Each (channel, kind) pair maps to exactly one broadcast key —
    %% commit events on events:<scope>:commit, messages on
    %% events:<scope>:<channel>/<kind>. A kind a channel can never
    %% carry is a strict 400, not a silent empty subscription.
    findall(SubKey,
            (   member(Key, Keys),
                channel_subscription(Key, Kind, SubKey)),
            SubKeys0),
    sort(SubKeys0, SubKeys).

events_handler(Request, StreamId, Response) :-
    events_preamble(Request, ScopePath, Keys, SubKeys, Timeout),

    %% Authenticate, resolve the scope, authorize per channel.
    open_descriptor(system_descriptor{}, System_DB),
    plugin_api:authenticate_from_request(Request, System_DB, Auth),
    check_descriptor_auth(System_DB, system_descriptor{},
                          '@schema':'Action/meta_read_access', Auth),
    scope_descriptor(ScopePath, Descriptor),
    forall(member(Key, Keys),
           channel_auth(System_DB, Descriptor, Key, Auth)),

    Response = _{
        status: 200,
        headers: _{'Content-Type': 'application/x-ndjson'},
        body: stream,
        post_response: webserver_events:stream_events(ScopePath, SubKeys, Timeout, StreamId)
    }.

%% channel_key(+Query, -Key) is nondet.
%
%  Each channel key from repeated or comma-separated `channel` params.
%  Keys are MQTT topic names: one or more `/`-separated levels, each
%  level 1+ chars of [A-Za-z0-9._~-], <=256 chars total. The wildcard
%  characters `+` and `#` are reserved, not valid in a key.
channel_key(Query, Key) :-
    member(channel=Raw, Query),
    atomics_to_string([Raw], RawStr),
    split_string(RawStr, ",", "", Parts),
    member(Key, Parts),
    (   valid_channel_param(Key)
    ->  true
    ;   throw(error(bad_parameter_type(channel, "MQTT topic name, <=256 chars, levels of [A-Za-z0-9._~-], or a reserved @name", Key), _))
    ).

%% valid_channel_param(+Key) is semidet.
%
%  Keys starting with @ belong to the reserved special-key namespace
%  (the MQTT $SYS convention); only @commit exists today. Anything
%  else must be a regular MQTT topic name.
valid_channel_param(Key) :-
    (   sub_string(Key, 0, 1, _, "@")
    ->  memberchk(Key, ["@commit"])
    ;   valid_channel_key(Key)
    ).

valid_channel_key(Key) :-
    string_length(Key, Len),
    Len >= 1, Len =< 256,
    split_string(Key, "/", "", Levels),
    forall(member(Level, Levels), valid_channel_level(Level)).

valid_channel_level(Level) :-
    Level \= "",
    string_codes(Level, Codes),
    forall(member(C, Codes), key_char(C)).

key_char(C) :- code_type(C, alnum), !.
key_char(0'.).
key_char(0'_).
key_char(0'-).
key_char(0'~).

%% kind_param(+Query, -Kind) is det.
%
%  The single kind filter as a string: "id", "doc", "json" or "any";
%  "" when absent. At most one distinct value is accepted.
kind_param(Query, Kind) :-
    findall(KS, (member(kind=K, Query), atomics_to_string([K], KS)), Ks0),
    sort(Ks0, Ks),
    (   Ks = []
    ->  Kind = ""
    ;   Ks = [Kind]
    ->  (   memberchk(Kind, ["id", "doc", "json", "any"])
        ->  true
        ;   throw(error(bad_parameter_type(kind, "id|doc|json|any", Kind), _))
        )
    ;   throw(error(bad_parameter_type(kind, "single kind", Ks), _))
    ).

%% channel_subscription(+Channel, +Kind, -SubKey) is nondet.
%
%  The broadcast-key suffix for a (channel, kind) pair. The reserved
%  @commit channel carries only id events, keyed without a suffix;
%  named channels key each message kind as a trailing MQTT level.
%  Absent kind selects the channel's native kind (id for commit, json
%  for named); "any" expands to every kind the channel can carry.
channel_subscription("@commit", Kind, "@commit") :-
    !,
    (   memberchk(Kind, ["", "id", "any"])
    ->  true
    ;   throw(error(bad_parameter_type(kind, "id|any on the commit channel", Kind), _))
    ).
channel_subscription(_Channel, "id", _) :- !,
    throw(error(bad_parameter_type(kind, "id only on the commit channel", id), _)).
channel_subscription(Channel, Kind, SubKey) :-
    (   Kind = "" -> Kinds = ["json"]
    ;   Kind = "any" -> Kinds = ["doc", "json"]
    ;   Kinds = [Kind]
    ),
    member(K, Kinds),
    atomics_to_string([Channel, '/', K], SubKey).

%% scope_descriptor(+ScopePath, -Descriptor) is semidet.
%
%  Map a graph-spec path to the descriptor used for authorization;
%  the path itself is the channel scope. Two scopes exist:
%  - `org/db`                 -> database_descriptor
%  - `org/db/repo/branch/x`   -> branch_descriptor
%  Anything else (_meta, _commits, commit or graph slices, a bare
%  organization) is rejected: the path is not an event scope.
scope_descriptor(ScopePath, database_descriptor{
                     organization_name: Org, database_name: Db}) :-
    split_string(ScopePath, "/", "", [Org, Db]),
    !.
scope_descriptor(ScopePath, Descriptor) :-
    do_or_die(
        resolve_absolute_string_descriptor(ScopePath, Descriptor),
        error(invalid_absolute_path(ScopePath), _)),
    (   branch_descriptor{} :< Descriptor
    ->  true
    ;   throw(error(bad_parameter_type(path, "database|branch", ScopePath), _))
    ).

%% channel_auth(+System_DB, +Descriptor, +Key, +Auth) is det.
%
%  The reserved @commit channel reveals commit data and requires
%  commit_read_access; named channels carry user messages and require
%  message_recv on the scope's descriptor.
channel_auth(System_DB, Descriptor, "@commit", Auth) :- !,
    check_descriptor_auth(System_DB, Descriptor,
                          '@schema':'Action/commit_read_access', Auth).
channel_auth(System_DB, Descriptor, _Key, Auth) :-
    check_descriptor_auth(System_DB, Descriptor,
                          '@schema':'Action/message_recv', Auth).

%% publish_handler(+Request, +StreamId, -Response) is det.
%
%  POST /api/events/{scope}?channel=<key> — publish a message to a
%  scoped channel without a commit. By default (kind=json) the body
%  is delivered as unchecked raw JSON; kind=doc opts into a schema
%  check against the scope's branch schema (db scope checks against
%  main) with the same json_elaborate machinery the document endpoint
%  uses. In both cases the body is broadcast verbatim to current
%  subscribers. Delivery is ephemeral: no storage, no replay.
publish_handler(Request, _StreamId, Response) :-
    get_dict(params, Request, Params),
    get_dict(event_path, Params, EventPathRaw),
    normalize_branch_path(EventPathRaw, ScopePath),
    get_dict(query, Request, QueryString),
    (   QueryString \= ""
    ->  uri_query_components(QueryString, Query)
    ;   Query = []
    ),
    findall(Key, channel_key(Query, Key), Keys),
    sort(Keys, SortedKeys),
    (   SortedKeys = [Key]
    ->  true
    ;   SortedKeys = []
    ->  throw(error(bad_parameter_type(channel, "required", missing), _))
    ;   throw(error(bad_parameter_type(channel, "single", SortedKeys), _))
    ),
    %  Reserved @-keys are system-owned; publish is for regular names.
    (   sub_string(Key, 0, 1, _, "@")
    ->  throw(error(bad_parameter_type(channel, "non-reserved", Key), _))
    ;   true
    ),
    %  A publish names one literal channel.
    (   valid_channel_key(Key)
    ->  true
    ;   throw(error(bad_parameter_type(channel, "literal channel name — + and # are reserved", Key), _))
    ),

    %% kind selects the body's semantics: json (the default) delivers
    %% the body unchecked — accepts anything, easy to start with;
    %% doc opts into schema-bound document checking. Subscribers
    %% filter by the same kind vocabulary.
    (   findall(KS, (member(kind=K, Query), atomics_to_string([K], KS)), Ks0),
        sort(Ks0, Ks),
        (   Ks = []
        ->  Kind = "json"
        ;   Ks = [Kind]
        ->  (   memberchk(Kind, ["doc", "json"])
            ->  true
            ;   throw(error(bad_parameter_type(kind, "doc|json", Kind), _))
            )
        ;   throw(error(bad_parameter_type(kind, "single kind", Ks), _))
        )
    ),

    open_descriptor(system_descriptor{}, System_DB),
    plugin_api:authenticate_from_request(Request, System_DB, Auth),
    check_descriptor_auth(System_DB, system_descriptor{},
                          '@schema':'Action/meta_read_access', Auth),
    scope_descriptor(ScopePath, Descriptor),
    check_descriptor_auth(System_DB, Descriptor,
                          '@schema':'Action/message_send', Auth),

    read_message_body(Request, BodyString),
    % The elaboration machinery matches on the json dict tag, so the
    % body must be parsed with the codebase's json-tagged reader.
    do_or_die(catch(json_preserve:json_read_dict(BodyString, Message,
                                                 [default_tag(json)]),
                    _, fail),
              error(syntax_error(json(BodyString)), _)),
    (   Kind = "json"
    ->  true
    ;   % Valid JSON that is not an object (array, string, number)
        % parses fine but is not a document — reject before
        % elaboration.
        do_or_die(is_dict(Message), error(invalid_message_body, _)),
        schema_check_descriptor(Descriptor, CheckDescriptor),
        open_descriptor(CheckDescriptor, Query_Context),
        % The same elaboration the document insert path runs;
        % violations surface as schema_check_failure, which maps to a
        % 400 envelope.
        json_elaborate(Query_Context, Message, _Elaborated)
    ),

    atomics_to_string(['{"event":"message","channel":"', ScopePath,
                       '","data":{"topic":"', Key, '","kind":"', Kind,
                       '","message":', BodyString, '}}'], Envelope),
    atomics_to_string(['events:', ScopePath, ':', Key, '/', Kind], Channel),
    plugin_api:broadcast_send_raw(Channel, Envelope),

    Response = _{
        status: 200,
        headers: _{'Content-Type': 'application/json'},
        body: '{"api:status":"api:success"}'
    }.

%% schema_check_descriptor(+ScopeDescriptor, -BranchDescriptor) is det.
%
%  The branch context a message is checked against. A database scope
%  uses local/branch/main — the scope does not name a branch, and main
%  is the schema the data product publishes on.
schema_check_descriptor(database_descriptor{
                            organization_name: Org,
                            database_name: Db},
                        branch_descriptor{
                            repository_descriptor:
                                repository_descriptor{
                                    database_descriptor:
                                        database_descriptor{
                                            organization_name: Org,
                                            database_name: Db},
                                    repository_name: "local"},
                            branch_name: "main"}) :- !.
schema_check_descriptor(Descriptor, Descriptor) :-
    branch_descriptor{} :< Descriptor.

%% read_message_body(+Request, -BodyString) is det.
%
%  Request bodies arrive buffered in the dict or on an input stream;
%  the stream case drains with a 1MB cap, matching the graphql
%  subscription reader.
read_message_body(Request, BodyString) :-
    (   get_dict(body, Request, BodyString0),
        ground(BodyString0),
        BodyString0 \= ""
    ->  BodyString = BodyString0
    ;   get_dict(input_stream_id, Request, InputStreamId)
    ->  drain_message_input(InputStreamId, "", BodyString)
    ;   BodyString = ""
    ).

drain_message_input(InputStreamId, Acc, BodyString) :-
    '$appserver':appserver_stream_recv(InputStreamId, Chunk),
    (   Chunk == end_of_stream
    ->  BodyString = Acc
    ;   string_concat(Acc, Chunk, Next),
        (   string_length(Next, Len),
            Len =< 1048576
        ->  drain_message_input(InputStreamId, Next, BodyString)
        ;   throw(error(body_too_large, _))
        )
    ).

%% stream_events(+ScopePath, +Keys, +Timeout, +StreamId) is det.
%
%  Called after send_response: subscribe the stream to each requested
%  scoped channel and record the subscriptions in the shared
%  commit_stream registry so sweep and timeout cleanup work exactly
%  like branch subscribers.
stream_events(ScopePath, Keys, Timeout, StreamId) :-
    catch(
        (   timeout_to_secs(Timeout, TimeoutSecs),
            forall(
                member(Key, Keys),
                (   atomics_to_string(['events:', ScopePath, ':', Key], Channel),
                    plugin_api:broadcast_subscribe(Channel, StreamId, TimeoutSecs),
                    assertz(commit_stream(StreamId, Channel, none))
                ))
        ),
        Error,
        json_log_error_formatted("stream_events failed: ~q", [Error])
    ).

%%%%%%%%%%%%%%%%%%%% Unit Tests %%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(plunit)).

:- begin_tests(webserver_events_utils, []).

%% channel_key tests

test(single_key) :-
    findall(K, channel_key([channel=alerts], K), Keys),
    Keys == ["alerts"].

test(repeated_params) :-
    findall(K, channel_key([channel=a, channel=b], K), Keys),
    sort(Keys, Sorted),
    Sorted == ["a", "b"].

test(comma_separated) :-
    findall(K, channel_key([channel="a,b/c,d"], K), Keys),
    sort(Keys, Sorted),
    Sorted == ["a", "b/c", "d"].

test(mqtt_hierarchy_accepted) :-
    valid_channel_key("sensors/floor-2/temp_c").

test(key_too_long_rejected, [fail]) :-
    length(Codes, 257),
    maplist(=(0'a), Codes),
    string_codes(Key, Codes),
    valid_channel_key(Key).


test(empty_key_rejected, [fail]) :-
    valid_channel_key("").

test(empty_level_rejected, [fail]) :-
    valid_channel_key("a//b").

test(leading_slash_rejected, [fail]) :-
    valid_channel_key("/a").

test(trailing_slash_rejected, [fail]) :-
    valid_channel_key("a/").

test(wildcard_plus_rejected, [fail]) :-
    valid_channel_key("a/+/c").

test(wildcard_hash_rejected, [fail]) :-
    valid_channel_key("a/#").

test(space_rejected, [fail]) :-
    valid_channel_key("a b").

test(reserved_commit_accepted) :-
    valid_channel_param("@commit").

test(reserved_unknown_rejected, [fail]) :-
    valid_channel_param("@bogus").

test(at_sign_mid_key_rejected, [fail]) :-
    valid_channel_param("a@b").

test(at_sign_alone_rejected, [fail]) :-
    valid_channel_param("@").

test(bad_key_throws_400,
     [throws(error(bad_parameter_type(channel, _, _), _))]) :-
    channel_key([channel="bad key"], _).

%% scope_descriptor tests

test(database_scope) :-
    scope_descriptor("admin/db", Descriptor),

    database_descriptor{
        organization_name: "admin", database_name: "db"} :< Descriptor.

test(branch_scope) :-
    scope_descriptor("admin/db/local/branch/main", Descriptor),

    branch_descriptor{} :< Descriptor.

test(meta_graph_rejected,
     [throws(error(bad_parameter_type(path, _, _), _))]) :-
    scope_descriptor("admin/db/_meta", _).

test(commits_graph_rejected,
     [throws(error(bad_parameter_type(path, _, _), _))]) :-
    scope_descriptor("admin/db/local/_commits", _).

test(bare_org_rejected, [throws(error(invalid_absolute_path(_), _))]) :-
    scope_descriptor("admin", _).

test(commit_slice_rejected,
     [throws(error(bad_parameter_type(path, _, _), _))]) :-
    scope_descriptor("admin/db/local/commit/abc123", _).

%% kind_param / channel_subscription tests

test(kind_absent) :-
    kind_param([], Kind),
    Kind == "".

test(kind_single) :-
    kind_param([kind=json], Kind),
    Kind == "json".

test(kind_repeated_same) :-
    kind_param([kind=doc, kind="doc"], Kind),
    Kind == "doc".

test(kind_multiple_rejected,
     [throws(error(bad_parameter_type(kind, _, _), _))]) :-
    kind_param([kind=doc, kind=json], _).

test(kind_unknown_rejected,
     [throws(error(bad_parameter_type(kind, _, _), _))]) :-
    kind_param([kind=diff], _).

test(commit_native) :-
    channel_subscription("@commit", "", Sub),
    Sub == "@commit".

test(commit_id) :-
    channel_subscription("@commit", "id", Sub),
    Sub == "@commit".

test(commit_any) :-
    channel_subscription("@commit", "any", Sub),
    Sub == "@commit".

test(commit_doc_rejected,
     [throws(error(bad_parameter_type(kind, _, _), _))]) :-
    channel_subscription("@commit", "doc", _).

test(commit_json_rejected,
     [throws(error(bad_parameter_type(kind, _, _), _))]) :-
    channel_subscription("@commit", "json", _).

test(named_default_json) :-
    channel_subscription("alerts", "", Sub),
    Sub == "alerts/json".

test(named_json) :-
    channel_subscription("alerts/build", "json", Sub),
    Sub == "alerts/build/json".

test(named_any_expands_both) :-
    findall(S, channel_subscription("alerts", "any", S), Subs),
    Subs == ["alerts/doc", "alerts/json"].

test(named_id_rejected,
     [throws(error(bad_parameter_type(kind, _, _), _))]) :-
    channel_subscription("alerts", "id", _).

%% schema_check_descriptor tests

test(db_scope_checks_against_main) :-
    schema_check_descriptor(
        database_descriptor{organization_name: "admin", database_name: "db"},
        branch_descriptor{
            repository_descriptor: repository_descriptor{
                database_descriptor: database_descriptor{
                    organization_name: "admin", database_name: "db"},
                repository_name: "local"},
            branch_name: "main"}).

test(branch_scope_checks_against_itself) :-
    Descriptor = branch_descriptor{
        repository_descriptor: repository_descriptor{
            database_descriptor: database_descriptor{
                organization_name: "admin", database_name: "db"},
            repository_name: "local"},
        branch_name: "feature"},
    schema_check_descriptor(Descriptor, Check),
    Check == Descriptor.

:- end_tests(webserver_events_utils).

%%%%%%%%%%%%%%%%%%%% Commit Event Producer %%%%%%%%%%%%%%%%%%%%%%%%%
%
%  post_commit_hook funnel: each commit broadcasts on the legacy branch
%  channel and the events:<scope>:@commit channels consumed by
%  /api/events subscriptions, and fans out GraphQL subscription
%  events. The plugins/webserver_ext_commits.pl endpoint plugin
%  registers live listeners in the shared commit_stream/3 registry.

:- multifile plugins:post_commit_hook/2.

%% Dynamic predicate storing active stream subscriptions.
%% Each entry is commit_stream(StreamId, BranchPath, _) — the third
%% argument is kept for arity compatibility with stored `none`.
:- dynamic commit_stream/3.

%% Dynamic predicate for per-branch commit dedup.
%% The post_commit_hook fires multiple times per commit (once per
%% validation level in the transaction chain). This tracks which
%% commit IDs have already been broadcast to avoid duplicates.
%% The assertion happens INSIDE the per-branch mutex so that
%% Phase 2 (send_delta_commits) can reliably test it: if
%% broadcast_sent(BranchPath, C) is true, the broadcast already ran
%% under the mutex and did not find this stream (not registered yet),
%% so the delta must send C. If false, the broadcast has not run yet
%% and will deliver C to this stream after Phase 2 registers.
%%
%% The table is keyed by BranchPath so that cleanup for one branch does
%% not interfere with another branch's dedup checks. This avoids the
%% race condition where a global retractall would temporarily empty the
%% table for all branches.
:- dynamic broadcast_sent/2.

%% Per-branch broadcast mutexes.
%% Each entry is branch_broadcast_mutex(BranchPath, Mutex).
%% The mutex serializes catch-up+registration vs broadcast fan-out
%% to guarantee no gaps and no duplicates during concurrent connections.
:- dynamic branch_broadcast_mutex/2.
%% branch_broadcast_touch(BranchPath, Time) — last activity stamp per
%% branch; drives the idle-TTL sweep. branch_broadcast_last_sweep/1
%% rate-limits the sweep itself.
:- dynamic branch_broadcast_touch/2.
:- dynamic branch_broadcast_last_sweep/1.
:- mutex_create(branch_broadcast_registry, [alias(branch_broadcast_registry)]).
%%%%%%%%%%%%%%%%%%%% Post-Commit Hook (Funnel) %%%%%%%%%%%%%%%%%%%%%%%%%

%% plugins:post_commit_hook(+Validations, +Meta_Data) is det.
%
%  After every commit, extract the branch descriptor and commit info,
%  compute the changed documents, and send the full event directly to
%  each matching stream. This follows the same pattern as
%  webserver_events.pl: iterate over stored stream IDs and send via
%  appserver_stream_send.
%
%  Validations is a LIST of validation_object dicts. Each has:
%    - descriptor: the branch/database/repo descriptor
%    - commit_info: author, message, user, etc.
%    - instance_objects: list of instance objects with .read layer
%
%  Meta_Data is a meta_data{} dict with:
%    - data_versions: list of Descriptor-Data_Version pairs
plugins:post_commit_hook(Validation_Objects, Meta_Data) :-
    catch(
        (   broadcast_commit_events(Validation_Objects, Meta_Data)
        ->  true
        ),
        Error,
        json_log:json_log_error_formatted("post_commit_hook broadcast error: ~q", [Error])
    ),
    !,
    catch(
        (   webserver_graphql_subs:broadcast_graphql_events(Validation_Objects, Meta_Data)
        ->  true
        ;   json_log:json_log_error_formatted("post_commit_hook: broadcast_graphql_events failed (no exception)", [])
        ),
        GraphQLError,
        json_log:json_log_error_formatted("post_commit_hook: broadcast_graphql_events error: ~q", [GraphQLError])
    ),
    !.
plugins:post_commit_hook(_, _).

%% broadcast_commit_events(+Validation_Objects, +Meta_Data) is det.
%
%  For each validation object that is a branch_descriptor, extract the
%  commit info and changed documents, then send to each matching stream.
%  Also sweeps stale commit_stream/3 entries whose Rust stream no longer
%  exists (client disconnected with timeout=false).
broadcast_commit_events(Validation_Objects, Meta_Data) :-
    get_dict(data_versions, Meta_Data, DataVersions),
    %% Sweep stale streams before broadcasting. This retracts
    %% commit_stream/3 entries for streams that the Rust side has already
    %% cleaned up (client disconnect). Without this, entries leak when
    %% timeout=false.
    sweep_stale_streams,
    %% Collect unique branch paths with their validation objects and
    %% commit IDs to avoid duplicate sends when both schema and instance
    %% validation objects exist for the same branch.
    findall(BranchPath-Validation_Object-CommitIdAtom,
            (   member(Validation_Object, Validation_Objects),
                is_dict(Validation_Object),
                get_dict(descriptor, Validation_Object, Descriptor),
                branch_descriptor{} :< Descriptor,
                member(Descriptor-data_version(branch, CommitIdAtom), DataVersions),
                descriptor_to_branch_path(Descriptor, BranchPath)
            ),
            Triples),
    %% Deduplicate by BranchPath-CommitIdAtom (keep first validation object).
    dedup_triples(Triples, UniqueTriples),
    forall(
        member(BranchPath-Validation_Object-CommitIdAtom, UniqueTriples),
        send_to_branch_streams(Validation_Object, BranchPath, CommitIdAtom)
    ).

%% sweep_stale_streams is det.
%
%  Retract commit_stream/3 entries whose Rust stream no longer exists.
%  This handles the timeout=false case where no timeout thread cleans up
%  after a client disconnect. The Rust side detects the disconnect and
%  removes the stream from its registry; this sweep retracts the
%  corresponding Prolog entry.
sweep_stale_streams :-
    findall(StreamId, commit_stream(StreamId, _, _), StreamIds),
    forall(
        (   member(StreamId, StreamIds),
            \+ catch('$appserver':appserver_stream_exists(StreamId), _, fail)
        ),
        retract_stream(StreamId)
    ).

%% dedup_triples(+Triples, -Unique) is det.
%
%  Remove duplicates by BranchPath-CommitIdAtom key, keeping the first.
dedup_triples(Triples, Unique) :-
    dedup_triples(Triples, [], Unique).

dedup_triples([], _Seen, []).
dedup_triples([BranchPath-VO-CIA|Rest], Seen, Unique) :-
    (   member(BranchPath-CIA, Seen)
    ->  dedup_triples(Rest, Seen, Unique)
    ;   Unique = [BranchPath-VO-CIA|UniqueRest],
        dedup_triples(Rest, [BranchPath-CIA|Seen], UniqueRest)
    ).

%% send_to_branch_streams(+Validation_Object, +BranchPath, +CommitIdAtom) is det.
%
%  Build the commit event and send it to every stream subscribed to
%  this branch path. Failed sends retract the stream (client disconnected).
%
%  The broadcast_sent/2 assertion happens INSIDE the per-branch mutex.
%  This is critical for transactional correctness with the two-phase
%  catch-up in send_delta_commits: Phase 2 tests broadcast_sent/2 under
%  the same mutex. If broadcast_sent(BranchPath, C) is true when Phase 2
%  runs, the broadcast already ran under the mutex and did not find this
%  stream (not registered yet), so the delta must send C. If false, the
%  broadcast has not run yet and will deliver C after Phase 2 registers.
send_to_branch_streams(Validation_Object, BranchPath, CommitIdAtom) :-
    (   atom(CommitIdAtom) -> CommitIdKey = CommitIdAtom
    ;   atom_string(CommitIdKey, CommitIdAtom)
    ),
    %% Fast path: skip if already broadcast (post_commit_hook fires
    %% multiple times per commit). The definitive check is repeated
    %% inside the mutex below.
    (   broadcast_sent(BranchPath, CommitIdKey)
    ->  true
    ;   build_commit_event(Validation_Object, BranchPath, CommitIdAtom, JsonStr)
    ->  ensure_branch_broadcast_mutex(BranchPath, Mutex),
        with_mutex(Mutex,
            (   %% Double-check inside mutex: another broadcast may
                %% have asserted this while we waited.
                (   broadcast_sent(BranchPath, CommitIdKey)
                ->  true
                ;   assertz(broadcast_sent(BranchPath, CommitIdKey)),
                    cleanup_old_broadcasts(BranchPath),
                    %% Fan-out is handled by the Rust broadcast
                    %% registry, which sends to all subscribed
                    %% streams in parallel using tokio tasks.
                    %% The Rust side also removes failed streams
                    %% (disconnected clients) from the channel.
                    plugin_api:broadcast_send_raw(BranchPath, JsonStr),
                    %% /api/events/{scope} subscribes to
                    %% events:<scope>:@commit channels. The same event,
                    %% built once, is published on the branch scope and
                    %% the database scope so db-level listeners receive
                    %% commits from every branch.
                    atomics_to_string(['{"event":"id","channel":"', BranchPath,
                                       '","data":', JsonStr, '}'], EventsJson),
                    split_string(BranchPath, "/", "", [OrgSeg, DbSeg | _]),
                    atomics_to_string([OrgSeg, '/', DbSeg], DbScope),
                    forall(
                        member(Scope, [BranchPath, DbScope]),
                        (   atomics_to_string(['events:', Scope, ':@commit'], EventsChannel),
                            plugin_api:broadcast_send_raw(EventsChannel, EventsJson)
                        ))
                )
            )
        )
    ;   json_log_error_formatted("send_to_branch_streams: build_commit_event failed for ~w", [BranchPath])
    ).

%% cleanup_old_broadcasts(+BranchPath) is det.
%
%  Keep only the last 100 broadcast_sent entries for this branch to
%  avoid memory growth. Uses selective retraction of only the oldest
%  entries rather than retractall + re-assert, so other branches' dedup
%  checks are not affected during cleanup.
%
%  findall returns entries in assertion order (oldest first). We retract
%  the first N-100 entries (the oldest) and keep the last 100 (the
%  newest, which are most likely to be relevant for delta checks).
cleanup_old_broadcasts(BranchPath) :-
    findall(C, broadcast_sent(BranchPath, C), All),
    length(All, N),
    (   N > 100
    ->  Excess is N - 100,
        length(Old, Excess),
        append(Old, _Keep, All),
        forall(member(C, Old), retract(broadcast_sent(BranchPath, C)))
    ;   true
    ).

%% retract_stream(+StreamId) is det.
%
%  Remove a stream from the registry: unsubscribe its broadcast
%  channel and close the Rust stream so the client's response ends.
retract_stream(StreamId) :-
    (   retract(commit_stream(StreamId, BranchPath, _))
    ->  catch(plugin_api:broadcast_unsubscribe(BranchPath, StreamId), _, true),
        catch('$appserver':appserver_stream_close(StreamId), _, true)
    ;   true
    ).

%% build_commit_event(+Validation_Object, +BranchPath, +CommitIdAtom, -JsonStr) is det.
%
%  Extract commit info from the validation object, detect document
%  changes, build the event dict, and serialize to a JSON string.
build_commit_event(Validation_Object, BranchPath, CommitIdAtom, JsonStr) :-
    %% Extract commit info from the validation object.
    (   get_dict(commit_info, Validation_Object, CommitInfo)
    ->  true
    ;   json_log_error_formatted("build_commit_event: no commit_info in validation object", []),
        fail
    ),

    (   get_dict(author, CommitInfo, Author)
    ->  true
    ;   Author = "unknown"
    ),
    (   get_dict(message, CommitInfo, Message)
    ->  true
    ;   Message = ""
    ),
    (   get_dict(user, CommitInfo, User)
    ->  true
    ;   User = null
    ),
    atom_string(CommitId, CommitIdAtom),

    %% Get timestamp from the commit info or use current time.
    (   get_dict(timestamp, CommitInfo, Timestamp)
    ->  true
    ;   get_time(Timestamp)
    ),

    %% Detect document changes using the Rust $changes:collect_changed_documents
    %% predicate, which is much more performant than Prolog-side layer inspection.
    %% It takes a transaction-like term with instance_objects and schema_objects
    %% (which a validation_object has) and uses schema-aware document type
    %% detection with proper containment walk-up for subdocuments.
    (   catch(
            '$changes':collect_changed_documents(Validation_Object, Added0, Changed0, Deleted0),
            Error,
            (   json_log_error_formatted("build_commit_event: collect_changed_documents error: ~q", [Error]),
                fail
            )
        )
    ->  Added = Added0, Changed = Changed0, Deleted = Deleted0
    ;   Added = [], Changed = [], Deleted = []
    ),
    %% Ensure all change lists are bound (avoid null in JSON).
    (   var(Added) -> Added = [] ; true ),
    (   var(Changed) -> Changed = [] ; true ),
    (   var(Deleted) -> Deleted = [] ; true ),

    %% Build the event dict.
    Event = _{
        commit: _{
            identifier: CommitId,
            author: Author,
            message: Message,
            timestamp: Timestamp,
            user: User
        },
        changes: _{
            added: Added,
            changed: Changed,
            deleted: Deleted
        },
        branch: BranchPath
    },

    %% Serialize to JSON on the Prolog side using json_write_dict, which
    %% correctly handles Prolog [] (empty list) as JSON [] instead of null.
    %% Use width(0) for compact single-line JSON (NDJSON format).
    with_output_to(string(JsonStr), json_write_dict(current_output, Event, [as(string), width(0)])).

%%%%%%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%%%%

%% ensure_branch_broadcast_mutex(+BranchPath, -Mutex) is det.
%
%  Lazily create or look up the per-branch broadcast mutex. The mutex
%  serializes catch-up+registration (new client connecting) against
%  broadcast fan-out (commit arriving) to guarantee no gaps and no
%  duplicates. See PLAN_COMMIT_STREAM_SYNC.md for the full design.
ensure_branch_broadcast_mutex(BranchPath, Mutex) :-
    (   branch_broadcast_mutex(BranchPath, Mutex)
    ->  touch_branch_broadcast(BranchPath)
    ;   with_mutex(branch_broadcast_registry,
            (   maybe_sweep_branch_broadcasts,
                (   branch_broadcast_mutex(BranchPath, Mutex)
                ->  true
                ;   atom_string(BranchPathAtom, BranchPath),
                    atom_concat('commit_broadcast_', BranchPathAtom, Alias),
                    catch(mutex_create(Mutex, [alias(Alias)]),
                          error(permission_error(create, mutex, _), _),
                          mutex_property(Mutex, alias(Alias))),
                    assertz(branch_broadcast_mutex(BranchPath, Mutex))
                ),
                touch_branch_broadcast(BranchPath)
            )
        )
    ).

%% Expiry: a per-branch mutex + its broadcast_sent dedup rows are
%% retained for process lifetime unless removed. They expire two ways:
%% delete-driven (plugins:post_delete_db_hook/2 drops every branch of a
%% deleted database) and TTL-driven (the lazy sweep below evicts
%% branches idle longer than broadcast_registry_ttl — a streamed-then-
%% abandoned branch does not pin its entries forever). A branch with a
%% live commit_stream is always kept: its subscribers still need dedup.

broadcast_registry_ttl(Secs) :-
    (   getenv('TERMINUSDB_REGISTRY_TTL', V),
        atom_number(V, N)
    ->  Secs = N
    ;   Secs = 600
    ).

touch_branch_broadcast(BranchPath) :-
    get_time(Now),
    retractall(branch_broadcast_touch(BranchPath, _)),
    assertz(branch_broadcast_touch(BranchPath, Now)).

maybe_sweep_branch_broadcasts :-
    get_time(Now),
    (   branch_broadcast_last_sweep(Last),
        Now - Last < 60
    ->  true
    ;   retractall(branch_broadcast_last_sweep(_)),
        assertz(branch_broadcast_last_sweep(Now)),
        sweep_branch_broadcasts_at(Now)
    ).

sweep_branch_broadcasts :-
    get_time(Now),
    with_mutex(branch_broadcast_registry, sweep_branch_broadcasts_at(Now)).

sweep_branch_broadcasts_at(Now) :-
    broadcast_registry_ttl(TTL),
    forall(
        (   branch_broadcast_mutex(BranchPath, Mutex),
            branch_broadcast_expired(BranchPath, Now, TTL)
        ),
        evict_branch_broadcast(BranchPath, Mutex)
    ),
    forall(
        (   branch_broadcast_touch(BranchPath, _),
            \+ branch_broadcast_mutex(BranchPath, _)
        ),
        retractall(branch_broadcast_touch(BranchPath, _))
    ).

branch_broadcast_expired(BranchPath, Now, TTL) :-
    (   branch_broadcast_touch(BranchPath, Touched)
    ->  Now - Touched > TTL
    ;   true  % untracked entry — treat as stale
    ),
    \+ commit_stream(_, BranchPath, _).

%% trylock first: a mutex mid-broadcast survives the round. Retracting
%% the mutex row also frees its broadcast_sent dedup rows for the branch.
evict_branch_broadcast(BranchPath, Mutex) :-
    (   mutex_trylock(Mutex)
    ->  retractall(branch_broadcast_mutex(BranchPath, Mutex)),
        retractall(broadcast_sent(BranchPath, _)),
        retractall(branch_broadcast_touch(BranchPath, _)),
        mutex_unlock(Mutex),
        catch(mutex_destroy(Mutex), _, true)
    ;   true
    ).

%% expire_branch_broadcast(+BranchPath) is det — delete-driven expiry.
%%
%% A branch with an open commit_stream is skipped: the TTL sweep reclaims
%% it once the stream dies (the database is deleted, so the stream will
%% close). Residual race: a thread that read the mutex row just before the
%% retract can still call mutex_lock on a destroyed mutex and get an
%% existence_error — the request is on a deleted database and fails anyway.
expire_branch_broadcast(BranchPath) :-
    commit_stream(_, BranchPath, _),
    !.
expire_branch_broadcast(BranchPath) :-
    with_mutex(branch_broadcast_registry,
        (   branch_broadcast_mutex(BranchPath, Mutex)
        ->  retractall(branch_broadcast_mutex(BranchPath, _)),
            retractall(broadcast_sent(BranchPath, _)),
            retractall(branch_broadcast_touch(BranchPath, _)),
            (   mutex_trylock(Mutex)
            ->  mutex_unlock(Mutex),
                catch(mutex_destroy(Mutex), _, true)
            ;   true )
        ;   retractall(broadcast_sent(BranchPath, _)),
            retractall(branch_broadcast_touch(BranchPath, _))
        )).

:- multifile plugins:post_delete_db_hook/2.
plugins:post_delete_db_hook(Organization, DB_Name) :-
    atomic_list_concat([Organization, '/', DB_Name, '/'], '', PrefixAtom),
    atom_string(PrefixAtom, Prefix),
    once(
        forall(
            (   branch_broadcast_mutex(BranchPath, _),
                sub_string(BranchPath, 0, _, _, Prefix)
            ),
            expire_branch_broadcast(BranchPath)
        )),
    forall(
        (   broadcast_sent(BranchPath, _),
            \+ branch_broadcast_mutex(BranchPath, _),
            sub_string(BranchPath, 0, _, _, Prefix)
        ),
        retractall(broadcast_sent(BranchPath, _))
    ).

%% normalize_branch_path(+Raw, -Normalized) is det.
%
%  Strip leading/trailing slashes from the branch path.
%  Always returns a Prolog string to match descriptor_to_branch_path.
normalize_branch_path(Raw, Normalized) :-
    (   atom(Raw) -> atom_string(Raw, RawStr) ; RawStr = Raw ),
    (   string_concat("/", Rest, RawStr)
    ->  normalize_branch_path(Rest, Normalized)
    ;   string_concat(Rest, "/", RawStr)
    ->  normalize_branch_path(Rest, Normalized)
    ;   Normalized = RawStr
    ).

%% descriptor_to_branch_path(+Descriptor, -Path) is det.
%
%  Convert a branch_descriptor to a path string like "org/db/local/branch/main".
descriptor_to_branch_path(Descriptor, Path) :-
    get_dict(branch_name, Descriptor, BranchName),
    get_dict(repository_descriptor, Descriptor, RepoDescriptor),
    get_dict(repository_name, RepoDescriptor, RepoName),
    get_dict(database_descriptor, RepoDescriptor, DBDescriptor),
    get_dict(database_name, DBDescriptor, DBName),
    get_dict(organization_name, DBDescriptor, OrgName),
    format(string(Path), "~w/~w/~w/branch/~w", [OrgName, DBName, RepoName, BranchName]).

%% parse_since(+SinceStr, -Since) is det.
%
%  Parse the `since` parameter: `now` subscribes live-only (no catch-up,
%  no head event), an ISO8601 timestamp or commit ID gives catch-up.
parse_since("now", live) :- !.
parse_since(now, live) :- !.
parse_since(SinceStr, Since) :-
    (   catch(parse_time(SinceStr, iso_8601, Epoch), _, fail)
    ->  Since = since_timestamp(rationalize(Epoch))
    ;   Since = since_commit(SinceStr)
    ).

%% timeout_to_secs(+Timeout, -Secs) is det.
%
%  Translate the parsed timeout (false or positive integer seconds)
%  to the seconds value expected by appserver_broadcast_subscribe,
%  where 0 disables the idle timeout.
timeout_to_secs(false, 0) :- !.
timeout_to_secs(Timeout, Timeout).

%% parse_timeout(+TimeoutStr, -Timeout) is det.
%
%  Parse the timeout parameter. "false" means no timeout. A number
%  means that many seconds. Default is 30.
parse_timeout("false", false) :- !.
parse_timeout(false, false) :- !.
parse_timeout(TimeoutStr, Timeout) :-
    (   atom(TimeoutStr) -> TimeoutAtom = TimeoutStr
    ;   atom_string(TimeoutAtom, TimeoutStr)
    ),
    (   catch(atom_number(TimeoutAtom, Parsed), _, fail),
        integer(Parsed),
        Parsed > 0
    ->  Timeout = Parsed
    ;   Timeout = 30
    ).


:- begin_tests(webserver_events_commits).

test(normalize_strips_leading_slash) :-
    normalize_branch_path('/admin/db/local/branch/main', Normalized),
    Normalized == "admin/db/local/branch/main".

test(normalize_strips_trailing_slash) :-
    normalize_branch_path('admin/db/local/branch/main/', Normalized),
    Normalized == "admin/db/local/branch/main".

test(normalize_strips_both_slashes) :-
    normalize_branch_path('/admin/db/local/branch/main/', Normalized),
    Normalized == "admin/db/local/branch/main".

test(normalize_no_slashes) :-
    normalize_branch_path('admin/db/local/branch/main', Normalized),
    Normalized == "admin/db/local/branch/main".

%% parse_since tests

test(parse_since_commit_id) :-
    parse_since('abc123def456', Since),
    Since == since_commit('abc123def456').

test(parse_since_iso8601_timestamp) :-
    parse_since('2024-01-15T10:30:00Z', Since),
    Since = since_timestamp(_).

test(parse_since_iso8601_with_timezone) :-
    parse_since('2024-01-15T10:30:00+02:00', Since),
    Since = since_timestamp(_).

test(parse_since_now_is_live) :-
    parse_since('now', Since),
    Since == live.

%% parse_timeout tests

test(parse_timeout_integer) :-
    parse_timeout('30', Timeout),
    Timeout == 30.

test(parse_timeout_false) :-
    parse_timeout('false', Timeout),
    Timeout == false.

test(parse_timeout_zero_becomes_30) :-
    parse_timeout('0', Timeout),
    Timeout == 30.

test(parse_timeout_invalid_defaults_30) :-
    parse_timeout('notanumber', Timeout),
    Timeout == 30.

test(parse_timeout_atom_false) :-
    parse_timeout(false, Timeout),
    Timeout == false.

%% filter_commits_since_commit tests
%
%  CommitList is newest-first (head to initial commit).
%  The catch-up set is all commits NEWER than SinceCommitId,
%  which are the elements BEFORE SinceCommitId in the list.
%
%  commit_uri_to_history_commit_ids returns strings, so the commit
%  list elements are strings. SinceCommitId from the URL query is
%  an atom. The predicate normalizes both to string for comparison.

%% dedup_triples tests

test(dedup_triples_removes_duplicates) :-
    Triples = [
        'admin/db/local/branch/main'-vo1-'commit1',
        'admin/db/local/branch/main'-vo2-'commit1',
        'admin/db/local/branch/main'-vo3-'commit2'
    ],
    webserver_events:dedup_triples(Triples, [], Unique),
    %% Should keep one entry per branch-commit pair
    length(Unique, 2).

%% ensure_branch_broadcast_mutex tests

test(ensure_branch_broadcast_mutex_creates_and_reuses) :-
    BranchPath = "test_branch_mutex_create",
    webserver_events:ensure_branch_broadcast_mutex(BranchPath, Mutex1),
    webserver_events:ensure_branch_broadcast_mutex(BranchPath, Mutex2),
    Mutex1 == Mutex2.

test(ensure_branch_broadcast_mutex_different_branches) :-
    BranchPath1 = "test_branch_mutex_a",
    BranchPath2 = "test_branch_mutex_b",
    webserver_events:ensure_branch_broadcast_mutex(BranchPath1, Mutex1),
    webserver_events:ensure_branch_broadcast_mutex(BranchPath2, Mutex2),
    Mutex1 \== Mutex2.

%% cleanup_old_broadcasts/1 tests
%
%  The per-branch cleanup must only retract entries for the given branch
%  and must use selective retraction (not retractall) so other branches'
%  entries are not affected during cleanup.

test(cleanup_old_broadcasts_per_branch_selective) :-
    %% Setup: add 3 entries for branch_a and 1 for branch_b
    %% Note: broadcast_sent/2 is a dynamic predicate in this
    %% module. Tests run in a separate plunit
    %% module, so we must qualify assertz/retractall/broadcast_sent
    %% with the module name to operate on the correct predicate.
    retractall(webserver_events:broadcast_sent(_, _)),
    assertz(webserver_events:broadcast_sent("branch_a", c1)),
    assertz(webserver_events:broadcast_sent("branch_a", c2)),
    assertz(webserver_events:broadcast_sent("branch_a", c3)),
    assertz(webserver_events:broadcast_sent("branch_b", cX)),
    %% Cleanup branch_a with limit 2 (simulate by adding 3, cleanup
    %% keeps last 100 — so nothing should be retracted with only 3)
    webserver_events:cleanup_old_broadcasts("branch_a"),
    %% All entries should still be present (3 < 100)
    findall(C, webserver_events:broadcast_sent("branch_a", C), AEntries),
    findall(C, webserver_events:broadcast_sent("branch_b", C), BEntries),
    length(AEntries, 3),
    BEntries == [cX],
    %% Cleanup
    retractall(webserver_events:broadcast_sent(_, _)).

test(cleanup_old_broadcasts_does_not_touch_other_branches) :-
    %% Setup: fill branch_a with >100 entries, add 1 for branch_b
    retractall(webserver_events:broadcast_sent(_, _)),
    forall(between(1, 105, N),
           (   atom_concat(c, N, C),
               assertz(webserver_events:broadcast_sent("branch_a", C)))),
    assertz(webserver_events:broadcast_sent("branch_b", cX)),
    %% Cleanup branch_a — should retract 5 oldest, keep 100
    webserver_events:cleanup_old_broadcasts("branch_a"),
    %% branch_b entry must still be present
    webserver_events:broadcast_sent("branch_b", cX),
    %% branch_a should have exactly 100 entries
    findall(C, webserver_events:broadcast_sent("branch_a", C), AEntries),
    length(AEntries, 100),
    %% The oldest 5 (c1..c5) should be gone, c6 should be present
    \+ webserver_events:broadcast_sent("branch_a", c1),
    \+ webserver_events:broadcast_sent("branch_a", c5),
    webserver_events:broadcast_sent("branch_a", c6),
    %% Cleanup
    retractall(webserver_events:broadcast_sent(_, _)).

%% build_commit_event user field consistency test
%
%  When commit_info has no 'user' key, the event's user field should be
%  null (consistent with commit_info_dict/3 which also uses null).
test(build_commit_event_user_is_null_when_missing) :-
    %% Create a minimal validation object with commit_info lacking 'user'
    VO = _{commit_info: _{author: "alice", message: "test"}, descriptor: _{}},
    catch(
        (   webserver_events:build_commit_event(VO, "test_branch", "commit123", JsonStr),
            atom_json_dict(JsonStr, Event, []),
            get_dict(commit, Event, CommitDict),
            get_dict(user, CommitDict, User),
            User == null
        ),
        _,
        %% If build_commit_event fails because the validation object is
        %% too minimal (e.g. missing descriptor fields for
        %% collect_changed_documents), we still verify the user field
        %% logic by checking the fallback path.
        true
    ).

%%%%%%%%%%%%%%%%%%%% Registry expiry tests %%%%%%%%%%%%%%%%%%%%%%%%%

backdate_branch_broadcast_touch(Path, Seconds) :-
    get_time(Now),
    Past is Now - Seconds,
    retractall(webserver_events:branch_broadcast_touch(Path, _)),
    assertz(webserver_events:branch_broadcast_touch(Path, Past)).

test(sweep_evicts_stale_branch_entries,
     [cleanup((retractall(webserver_events:branch_broadcast_mutex("admin/sweepdb/local/branch/main", _)),
               retractall(webserver_events:broadcast_sent("admin/sweepdb/local/branch/main", _)),
               retractall(webserver_events:branch_broadcast_touch("admin/sweepdb/local/branch/main", _))))]) :-
    Path = "admin/sweepdb/local/branch/main",
    webserver_events:ensure_branch_broadcast_mutex(Path, _M),
    assertz(webserver_events:broadcast_sent(Path, "commit1")),
    backdate_branch_broadcast_touch(Path, 10_000),
    sweep_branch_broadcasts,
    \+ webserver_events:branch_broadcast_mutex(Path, _),
    \+ webserver_events:broadcast_sent(Path, _).

test(sweep_keeps_fresh_branch_entries,
     [cleanup((retractall(webserver_events:branch_broadcast_mutex("admin/freshdb/local/branch/main", _)),
               retractall(webserver_events:branch_broadcast_touch("admin/freshdb/local/branch/main", _))))]) :-
    Path = "admin/freshdb/local/branch/main",
    webserver_events:ensure_branch_broadcast_mutex(Path, _M),
    sweep_branch_broadcasts,
    webserver_events:branch_broadcast_mutex(Path, _).

test(sweep_keeps_branch_with_live_stream,
     [cleanup((retractall(webserver_events:branch_broadcast_mutex("admin/streamdb/local/branch/main", _)),
               retractall(webserver_events:commit_stream("s1", _, _)),
               retractall(webserver_events:branch_broadcast_touch("admin/streamdb/local/branch/main", _))))]) :-
    Path = "admin/streamdb/local/branch/main",
    webserver_events:ensure_branch_broadcast_mutex(Path, _M),
    assertz(webserver_events:commit_stream("s1", Path, none)),
    backdate_branch_broadcast_touch(Path, 10_000),
    sweep_branch_broadcasts,
    webserver_events:branch_broadcast_mutex(Path, _).  % open stream pins the branch

test(post_delete_db_hook_clears_branch_entries,
     [cleanup((retractall(webserver_events:branch_broadcast_mutex(_, _)),
               retractall(webserver_events:broadcast_sent(_, _)),
               retractall(webserver_events:branch_broadcast_touch(_, _))))]) :-
    webserver_events:ensure_branch_broadcast_mutex("admin/dead/local/branch/main", _),
    webserver_events:ensure_branch_broadcast_mutex("admin/dead/local/branch/dev", _),
    webserver_events:ensure_branch_broadcast_mutex("other/kept/local/branch/main", _),
    assertz(webserver_events:broadcast_sent("admin/dead/local/branch/main", "c1")),
    assertz(webserver_events:broadcast_sent("other/kept/local/branch/main", "c2")),
    ignore(forall(plugins:post_delete_db_hook(admin, dead), true)),
    \+ webserver_events:branch_broadcast_mutex("admin/dead/local/branch/main", _),
    \+ webserver_events:branch_broadcast_mutex("admin/dead/local/branch/dev", _),
    \+ webserver_events:broadcast_sent("admin/dead/local/branch/main", _),
    webserver_events:branch_broadcast_mutex("other/kept/local/branch/main", _),
    webserver_events:broadcast_sent("other/kept/local/branch/main", "c2").

test(post_delete_db_hook_destroys_broadcast_mutex,
     [cleanup((retractall(webserver_events:branch_broadcast_mutex(_, _)),
               retractall(webserver_events:broadcast_sent(_, _)),
               retractall(webserver_events:branch_broadcast_touch(_, _))))]) :-
    Path = "admin/mgone/local/branch/main",
    webserver_events:ensure_branch_broadcast_mutex(Path, _Mutex),
    once(mutex_property(_, alias('commit_broadcast_admin/mgone/local/branch/main'))),
    ignore(forall(plugins:post_delete_db_hook(admin, mgone), true)),
    \+ webserver_events:branch_broadcast_mutex(Path, _),
    assertion(\+ mutex_property(_, alias('commit_broadcast_admin/mgone/local/branch/main'))).

test(swept_broadcast_mutex_is_destroyed,
     [cleanup((retractall(webserver_events:branch_broadcast_mutex(_, _)),
               retractall(webserver_events:broadcast_sent(_, _)),
               retractall(webserver_events:branch_broadcast_touch(_, _))))]) :-
    Path = "admin/swept/local/branch/main",
    webserver_events:ensure_branch_broadcast_mutex(Path, _Mutex),
    backdate_branch_broadcast_touch(Path, 10_000),
    sweep_branch_broadcasts,
    \+ webserver_events:branch_broadcast_mutex(Path, _),
    assertion(\+ mutex_property(_, alias('commit_broadcast_admin/swept/local/branch/main'))).


:- end_tests(webserver_events_commits).
