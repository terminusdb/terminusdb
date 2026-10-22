:- module(mcp, [
              dispatch/4,
              dispatch/5,
              run_stdio/1,
              register_plugin_tool/4,
              mcp_plugin_tool/4,
              mcp_tool/3,
              mcp_handler/4
          ]).

/** <module> MCP (Model Context Protocol) endpoint
 *
 *  Transport-neutral core: dispatch(+Auth, +System_DB, +RequestDict,
 *  -ResponseDict) answers JSON-RPC 2.0 requests (initialize, ping,
 *  tools/list, tools/call). Two adapters sit on top:
 *
 *  - HTTP: POST /api/mcp, stateless streamable-HTTP, application/json.
 *    Enterprise-only: routes register only under TERMINUSDB_ENTERPRISE.
 *    Auth comes free via cors_handler (basic, API key, JWT bearer).
 *  - stdio: `terminusdb mcp` — newline-delimited JSON-RPC on
 *    stdin/stdout against the local store as the invoking user
 *    (superuser by default). Community. No port is opened.
 *
 *  Tools are workflow-centric: schema, query, write, git, admin,
 *  plugins. Tool failures are isError:true results; protocol problems
 *  are JSON-RPC errors carrying data.hint for the agent.
 */

:- use_module(core(plugin_api)).
:- use_module(core(plugins), [post_server_startup_hook/1]).
:- use_module(core(util)).
:- use_module(config(terminus_config), [terminusdb_version/1,
                                        indexer_backend/1,
                                        builtin_plugin_path/1]).
:- use_module(library(http/http_json), [reply_json/2]).
:- use_module(library(readutil), [read_line_to_string/2,
                                 read_file_to_string/3]).
:- use_module(library(pcre)).
:- use_module(core(util/test_utils)).
:- use_module(core(api)).
:- use_module(core(api/api_document),
              [before_read/4,
               api_get_documents_by_type/5,
               api_generate_document_ids_by_type/5,
               api_get_documents_by_query/6]).
:- use_module(core(api/api_jsonschema)).
:- use_module(core(api/api_patch), [api_patch_resource/7,
                                   api_apply_squash_commit/7]).
:- use_module(core(api/api_log), [api_log/5]).
:- use_module(core(api/api_reset), [api_reset/4]).
:- use_module(core(api/api_squash), [api_squash/6]).
:- use_module(core(api/db_branch), [branch_create/5, branch_delete/3]).
:- use_module(core(api/db_rebase), [rebase_on_branch/9]).
:- use_module(core(api/api_db), [list_databases/4]).
:- use_module(core(api/db_create), [create_db/10]).
:- use_module(core(api/db_delete), [delete_db/5]).
:- use_module(core(api/api_optimize), [api_optimize_queued/3]).
:- use_module(core(api/api_bundle), [bundle/5]).
:- use_module(core(api/api_unbundle), [unbundle/4]).
:- use_module(core(api/api_graphql), [handle_graphql_request/11,
                                     get_or_create_graphql_context/2]).
:- use_module(core(api/api_history), [api_document_history/6]).
:- use_module(plugins(vectorlink), [vectorlink_endpoint/1,
                                    io_statistics_for_domain/3,
                                    path_to_domain/2]).
:- use_module(core(query)).
:- use_module(core(document)).
:- use_module(core(document/json), [compress_schema_uri/4]).
:- use_module(core(document/schema), [is_frame_class/2, is_base_type/1]).
:- use_module(core(triple/constants), [super_user_authority/1]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(utf8)).
:- use_module(library(yall)).
:- use_module(library(option)).
:- use_module(library(http/json)).
:- use_module(library(plunit)).

% ==========================================================================
% JSON-RPC 2.0 dispatch
% ==========================================================================

%% supported_protocol_versions(-Versions) is det.
supported_protocol_versions(["2024-11-05", "2025-03-26", "2025-06-18"]).

%% dispatch(+Auth, +System_DB, +Request, -Response) is det.
%
%  Single JSON-RPC request dict, or a batch list. Response is the
%  response dict (or list for batch), or the atom `none` when the
%  request is a notification that must not be answered.
dispatch(Auth, System_DB, Request, Response) :-
    dispatch(Auth, System_DB, Request, http, Response).

dispatch(_Auth, _System_DB, [], _Transport, Response) :-
    !,
    error_response(null, -32600,
                           'Invalid Request: empty batch',
                           'A batch is a non-empty array of JSON-RPC requests',
                           Response).
dispatch(Auth, System_DB, Requests, Transport, Response) :-
    is_list(Requests),
    !,
    findall(One,
            (   member(R, Requests),
                dispatch_one(Auth, System_DB, R, Transport, One),
                One \= none),
            Responses),
    (   Responses = []
    ->  Response = none
    ;   Response = Responses).
dispatch(Auth, System_DB, Request, Transport, Response) :-
    dispatch_one(Auth, System_DB, Request, Transport, Response).

dispatch_one(_Auth, _System_DB, Request, _Transport, Response) :-
    \+ is_dict(Request),
    !,
    error_response(null, -32600,
                   'Invalid Request: expected a JSON object',
                   'Send {"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}',
                   Response).
dispatch_one(Auth, System_DB, Request, Transport, Response) :-
    (   get_dict(method, Request, Method)
    ->  true
    ;   Method = none),
    (   get_dict(id, Request, Id)
    ->  Has_Id = true
    ;   Id = null, Has_Id = false),
    (   Method = none
    ->  error_response(Id, -32600,
                       'Invalid Request: missing "method"',
                       'Send {"jsonrpc":"2.0","id":1,"method":<string>,"params":{}}',
                       Response)
    ;   \+ (   atom(Method)
           ;   string(Method))
    ->  error_response(Id, -32600,
                       'Invalid Request: "method" must be a string',
                       'Send {"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}',
                       Response)
    ;   is_notification(Method, Has_Id)
    ->  % notifications get no response — a throw inside must not kill
        % the stdio loop, so errors are logged and swallowed
        catch(once(handle_method(Auth, System_DB, Request, Transport, _Ignored)),
              NError,
              format(user_error, "MCP notification error: ~w~n", [NError])),
        Response = none
    ;   get_dict(jsonrpc, Request, "2.0")
    ->  (   catch(once(handle_method(Auth, System_DB, Request, Transport, Result)),
                  Error,
                  (   Error = mcp_error(Code, Message, Hint)
                  ->  Result = protocol_error(Code, Message, Hint)
                  ;   format(user_error, "MCP request error: ~w~n", [Error]),
                      Result = dispatch_failure))
        ->  (   Result = protocol_error(Code, Message, Hint)
            ->  error_response(Id, Code, Message, Hint, Response)
            ;   Result = dispatch_failure
            ->  error_response(Id, -32603, 'Internal error',
                               'The request raised an unexpected condition — retry, or report a bug',
                               Response)
            ;   Response = _{jsonrpc:"2.0", id:Id, result:Result})
        ;   error_response(Id, -32603, 'Internal error',
                           'The request raised an unexpected condition — retry, or report a bug',
                           Response))
    ;   error_response(Id, -32600,
                       'Invalid Request: "jsonrpc" must be "2.0"',
                       'Set "jsonrpc":"2.0" on every request',
                       Response)).

is_notification(Method, Has_Id) :-
    (   \+ Has_Id
    ->  true
    ;   (   atom(Method)
        ;   string(Method)),
        atom_string(MethodAtom, Method),
        sub_atom(MethodAtom, 0, _, _, 'notifications/')).

error_response(Id, Code, Message, Hint,
               _{jsonrpc:"2.0", id:Id,
                 error:_{code:Code, message:Message, data:_{hint:Hint}}}).

%% handle_method(+Auth, +System_DB, +Request, +Transport, -Result) is det.
handle_method(_Auth, _System_DB, Request, Transport, Result) :-
    get_dict(method, Request, "initialize"),
    !,
    initialize_result(Request, Transport, Result).
handle_method(_Auth, _System_DB, Request, _Transport, Result) :-
    get_dict(method, Request, Method),
    sub_string(Method, 0, _, _, "notifications/"),
    !,
    Result = none.
handle_method(_Auth, _System_DB, Request, _Transport, Result) :-
    get_dict(method, Request, "ping"),
    !,
    Result = _{}.
handle_method(_Auth, _System_DB, Request, _Transport, Result) :-
    get_dict(method, Request, "tools/list"),
    !,
    findall(Spec,
            (   mcp_tool(_Name, Spec, _Pred)
            ;   mcp_plugin_tool(Plugin_Name, Plugin_Desc, Plugin_Schema, _G),
                Spec = _{name:Plugin_Name, description:Plugin_Desc,
                         inputSchema:Plugin_Schema}),
            Tools),
    Result = _{tools:Tools}.
handle_method(Auth, System_DB, Request, Transport, Result) :-
    get_dict(method, Request, "tools/call"),
    !,
    (   get_dict(params, Request, Params), is_dict(Params)
    ->  true
    ;   throw(mcp_error(-32602, 'Invalid params: "params" object required',
                        'tools/call takes {"name":<tool>,"arguments":{…}}'))),
    (   get_dict(name, Params, Name), string(Name)
    ->  true
    ;   throw(mcp_error(-32602, 'Invalid params: "name" (string) required',
                        'tools/call takes {"name":<tool>,"arguments":{…}}'))),
    (   get_dict(arguments, Params, Args), is_dict(Args)
    ->  true
    ;   Args = _{}),
    call_tool(Auth, System_DB, Transport, Name, Args, Result).
handle_method(_Auth, _System_DB, Request, _Transport, _Result) :-
    get_dict(method, Request, Method),
    throw(mcp_error(-32601, Method,
                    'Unknown method — supported: initialize, ping, tools/list, tools/call')).

%% initialize_result(+Request, +Transport, -Result) is det.
initialize_result(Request, Transport, Result) :-
    supported_protocol_versions(Supported),
    last(Supported, Latest),
    (   get_dict(params, Request, Params),
        get_dict(protocolVersion, Params, Client_Version),
        memberchk(Client_Version, Supported)
    ->  Version = Client_Version
    ;   Version = Latest),
    terminusdb_version(TDB_Version),
    instructions(Transport, Instructions),
    Result = _{
        protocolVersion: Version,
        capabilities: _{tools:_{}},
        serverInfo: _{name:"terminusdb", version:TDB_Version},
        instructions: Instructions}.

instructions(Transport, Text) :-
    format(string(Core),
"TerminusDB MCP server. Workflow: orient with `schema` (summary/full/frame/vocab/contract/datatypes), retrieve and verify with `query` (document/history/diff/woql/check/graphql/search), change data with `write` (every write requires commit_info; run `query` kind `check` first — it is free), version with `git` (log/branches/merge/rebase/squash/reset/clone), operate with `admin`, extend with `plugins`. Resources are addressed as \"org/db\", \"org/db/local/branch/main\" or \"org/db/local/commit/<id>\". Writes are commits — capture the returned data_version and pass expected_data_version to guard against drift.", []),
    (   Transport = stdio
    ->  format(string(Tail),
" Running on the local store as the invoking user — no token needed. Onboarding doc: call admin action `docs`.", [])
    ;   format(string(Tail),
" Authenticate with basic auth, an API key, or a JWT bearer token. Onboarding doc: GET /llms.txt.", [])),
    string_concat(Core, Tail, Text).

% ==========================================================================
% Tool registry
% ==========================================================================

%% mcp_tool(?Name, ?Spec, ?Pred) is nondet.
%
%  Pred is Module:pred of arity 5:
%  pred(+Auth, +System_DB, +Args, +Transport, -Result).
:- dynamic mcp_tool/3.
:- dynamic mcp_plugin_tool/4.
:- meta_predicate register_plugin_tool(+, +, +, 4).

%% register_plugin_tool(+Name, +Description, +InputSchema, +Goal) is det.
%
%  Goal(+Auth, +System_DB, +ArgsDict, -ResultDict) — ResultDict becomes
%  the structuredContent payload. The meta_predicate declaration
%  qualifies Goal with the calling module.
register_plugin_tool(Name, Description, InputSchema, Goal) :-
    (   mcp_plugin_tool(Name, _, _, _)
    ->  throw(mcp_tool_error(plugin_name_collision(Name),
                             'A plugin already registered this MCP tool name — pick another'))
    ;   assertz(mcp_plugin_tool(Name, Description, InputSchema, Goal))).

%% call_tool(+Auth, +System_DB, +Transport, +Name, +Args, -Result) is det.
%
%  Built-in tools take Transport (stdio|http) — file-path arguments are
%  server-local and must not be reachable from an HTTP client. Plugin
%  tools keep their 4-ary signature.
call_tool(Auth, System_DB, Transport, Name, Args, Result) :-
    atom_string(Atom_Name, Name),
    (   mcp_tool(Atom_Name, _Spec, Tool)
    ->  Goal = call(Tool, Auth, System_DB, Args, Transport, Payload)
    ;   mcp_plugin_tool(Atom_Name, _Desc, _Schema, Tool),
        Goal = call(Tool, Auth, System_DB, Args, Payload)),
    !,
    (   catch(Goal, Error, Payload = tool_error(Error, none))
    ->  true
    ;   Payload = tool_error(unexpected_failure,
                             'The tool call failed without an error term')),
    result_from_payload(Payload, Result).
call_tool(_Auth, _System_DB, _Transport, Name, _Args, Result) :-
    findall(N, mcp_tool(N, _, _), Names),
    format(string(Hint),
           'Unknown tool "~w". Available: ~w. Call tools/list for the live set.',
           [Name, Names]),
    result_from_payload(tool_error(unknown_tool(Name), Hint), Result).

result_from_payload(tool_error(Error, Hint0), Result) :-
    !,
    error_hint(Error, Hint0, Hint),
    error_message(Error, Message),
    % Error often arrives as error(Formal, context(prolog_stack(…),…)) —
    % report the formal term only; stack dumps help nobody in JSON-RPC
    (   Error = error(Formal, _Context)
    ->  Pretty = Formal
    ;   Error = mcp_tool_error(Formal, _)
    ->  Pretty = Formal
    ;   Pretty = Error),
    format(string(Error_Atom), "~w", [Pretty]),
    format(string(Text), "Error: ~s~nHint: ~s", [Message, Hint]),
    % witnesses are data, not prose — expose them structured so agents
    % can read @expected/@found/@incoming without parsing the error text
    (   Pretty = merge_conflict(Witnesses)
    ->  Extra = _{witnesses:Witnesses}
    ;   Extra = _{}),
    Result = _{content:[_{type:"text", text:Text}],
               isError:true,
               structuredContent:_{error:Error_Atom, message:Message,
                                   hint:Hint}.put(Extra)}.
result_from_payload(Payload, Result) :-
    (   get_dict('_text', Payload, Label)
    ->  del_dict('_text', Payload, _, Clean),
        % text-only clients never see structuredContent — mirror the
        % payload after the label so they can read real values
        (   catch(atom_json_dict(JSON, Clean, [width(0)]), _, fail)
        ->  format(string(Text), "~s\n~s", [Label, JSON])
        ;   format(string(Text), "~s\n~w", [Label, Clean]))
    ;   format(string(Text), "~w", [Payload]),
        Clean = Payload),
    Result = _{content:[_{type:"text", text:Text}],
               structuredContent:Clean}.

%% error_hint(+Error, +Given, -Hint) is det.
error_hint(_, Hint, Hint) :- Hint \= none, !.
error_hint(Error, _, Hint) :-
    mcp_error_hint(Error, Hint),
    !.
error_hint(_, _, 'See the tool contracts in admin action `docs` (or GET /llms.txt when served over HTTP).').

%% error_message(+Error, -Message) is det.
error_message(error(Formal, _), Message) :-
    !,
    error_message(Formal, Message).
error_message(mcp_tool_error(Formal, _), Message) :-
    !,
    error_message(Formal, Message).
error_message(context(_Stack, Message), Message) :-
    !.
error_message(Formal, Message) :-
    format(string(Message), "~w", [Formal]).

%% mcp_error_hint(+Error, -Hint) is semidet.
%
%  Educational hints: what failed → why → corrected call → where next.
mcp_error_hint(error(schema_type_unknown(Type), _),
               Hint) :-
    default_prefixes(Prefixes),
    once((   is_base_type(IRI),
             compress_dict_uri(IRI, Prefixes, Type))),
    !,
    format(string(Hint),
           '"~w" is a datatype, not a schema document type — a property range is the bare string: {"name":"~w"} (or a family wrapper {"@type":"Optional","@class":"~w"}). Every valid spelling: schema action `datatypes`.', [Type, Type, Type]).
mcp_error_hint(error(schema_type_unknown(Type), _),
               Hint) :-
    format(string(Hint),
           '"~w" is not a valid schema type — a property range is a datatype string like "xsd:string" or a family wrapper {"@type":"Optional","@class":"xsd:string"}; a schema document @type is Class, TaggedUnion, Enum or Foreign. Every valid spelling: schema action `datatypes` (no resource needed).', [Type]).
mcp_error_hint(error(schema_check_failure(_), _),
               'Read each witness: `property` names the offending class member, `class` the range it wanted. Remember every non-@ member of a Class document is a property slot — "comment"/"description"/"title" are not annotations; valid type spellings come from schema action `datatypes`.').
mcp_error_hint(error(resource_not_found(Path), _),
               Hint) :-
    format(string(Hint),
           'Resource "~w" not found. Resources look like "org/db" or "org/db/local/branch/main". List what exists: admin action `list_databases`.', [Path]).
mcp_error_hint(error(access_not_authorised(_, Action, _), _),
               Hint) :-
    format(string(Hint),
           'The authenticated identity lacks the capability for action ~w. Authenticate as a user that has it, or ask an admin to grant it.', [Action]).
mcp_error_hint(error(submitted_id_does_not_match_generated_id(_, Generated, _), _),
               Hint) :-
    format(string(Hint),
           'Your @id does not match the id this class generates from its key — omit @id entirely and the key produces "~w" (percent-encoded) for you.', [Generated]).
mcp_error_hint(error(data_version_mismatch(_, _), _),
               'The branch head moved since the version you passed — re-read the current data_version (a write, check, or schema `summary` returns one), re-run `check`, then retry the write with the fresh expected_data_version.').
mcp_error_hint(error(existence_error(source_sink, File), _),
               Hint) :-
    format(string(Hint),
           '"~w" does not exist on the server filesystem — `export` first produces the bundle; check the path spelling.', [File]).
mcp_error_hint(error(unknown_database(Org, DB), _),
               Hint) :-
    format(string(Hint),
           'Database "~w/~w" does not exist — create it first (admin action `create_database`), then retry. `import` needs the target database to exist already.', [Org, DB]).
mcp_error_hint(mcp_error(_, Message, Hint0), Hint) :-
    format(string(Hint), "~s — ~s", [Message, Hint0]).
mcp_error_hint(mcp_tool_error(_, Hint), Hint).
mcp_error_hint(error(mcp_tool_error(_, Hint), _), Hint).
mcp_error_hint(unknown_tool(_),
               'Call tools/list to see the registered tools, then retry with an exact name.').

%% register_builtin_tools is det. Called once at load.
register_builtin_tools :-
    register_tool(schema, _{
        name: "schema",
        description: "Inspect the data model of a data product. Actions: `summary` (counts, class names, data_version), `full` (all class frames), `frame` (one class — `class`, `format`:\"frame\"|\"jsonschema\"), `vocab` (the @context/prefix map), `contract` (compact per-class shapes for orientation), `datatypes` (the whole type vocabulary — every base datatype spelling like \"xsd:string\", the schema @type spellings and the property wrapper families; global, no resource needed). `resource` (\"org/db\" — branch/commit addressing works too) is required for all actions except `datatypes`.",
        inputSchema: _{
            type:"object",
            properties:_{
                action:_{type:"string",
                         enum:["summary","full","frame","vocab","contract","datatypes"]},
                resource:_{type:"string", description:"org/db or org/db/local/branch/name — not needed for action datatypes"},
                class:_{type:"string", description:"class name for action frame"},
                format:_{type:"string", enum:["frame","jsonschema"],
                         description:"frame (default) or jsonschema (draft-07)"}},
            required:["action"]},
        annotations:_{readOnlyHint:true}
    }, mcp:schema_tool),
    register_tool(query, _{
        name: "query",
        description: "Retrieve and verify without mutating. Actions: `document` (get docs — `id`|`ids`|`type`|`query` selector, `skip`/`count`, `id_only`, `fields` projection, `format`:\"json\"|\"embedding\"|\"markdown\" — embedding renders each doc through its class's @metadata.embedding GraphQL+Handlebars template, markdown renders it as markdown; returns documents:[{id,text}] plus `page`, the concatenated page), `woql` (JSON AST — read-only: document/triple writes are rejected; use `write` for mutations; `count` caps the returned bindings), `check` (dry-run a write — `verb`:insert|replace|delete, `documents`|`id`|`ids`|`type`|`nuke` — reports valid, witnesses, and embedding readiness; valid:false is data, not an error), `graphql` (read queries — `query` holds the GraphQL document string; mutations are rejected, use write action `graphql`), `search` (VectorLink retrieval — `kind`:search|similar|suggest|duplicates, params `q` (or `query`)/`id`/`text`/`count`/`start`/`doc_type`; failure hints explain reindexing and the WOQL fallback), `history` (per-document history — `id` required; repo-level history is git `log`. Flags `created`/`updated`/`diff`, `before`/`after` ISO bounds, `start`/`count`), `diff` (document diffs between `before` and `after` commits — `id` narrows to one document). Optional `path` (\"org/db\", default the woql target; `resource` is accepted too) and `expected_data_version` (\"branch:<commit>\") guard reads.",
        inputSchema: _{
            type:"object",
            properties:_{
                action:_{type:"string",
                         enum:["document","woql","check","graphql","search","history","diff"]},
                path:_{type:"string", description:"org/db or org/db/local/branch/name"},
                before:_{type:"string", description:"diff/history: commit id, commit path, branch name, or data-version"},
                after:_{type:"string", description:"diff/history: commit id, commit path, branch name, or data-version"},
                graph_type:_{type:"string", enum:["instance","schema","inference"]},
                id:_{type:"string"},
                ids:_{type:"array", items:_{type:"string"}},
                type:_{type:"string", description:"class name to select documents by type"},
                kind:_{type:"string", enum:["search","similar","suggest","duplicates"],
                       description:"search action: retrieval kind"},
                query:_{description:"WOQL JSON AST, document-match query for document, or GraphQL document string for graphql"},
                q:_{type:"string", description:"search text"},
                text:_{type:"string", description:"similar-search free text"},
                threshold:_{type:"number", description:"duplicates similarity threshold"},
                variables:_{type:"object", description:"graphql variables — mutations take document payloads here (e.g. variables:{input:{…}} with `json: $input`)"},
                skip:_{type:"integer"}, count:_{type:"integer"},
                start:_{type:"integer", description:"history/diff pagination offset"},
                created:_{type:"boolean", description:"history: report creation info only"},
                updated:_{type:"boolean", description:"history: report last-update info only"},
                diff:_{type:"boolean", description:"history: include per-commit diffs"},
                fast:_{type:"boolean", description:"history: fast path"},
                complete:_{type:"boolean", description:"history: complete commit list"},
                unfold:_{type:"boolean"}, id_only:_{type:"boolean"},
                fields:_{type:"array", items:_{type:"string"},
                         description:"document projection — property names to keep (@id/@type always kept)"},
                format:_{type:"string", enum:["json","embedding","markdown"],
                         description:"document output format — embedding renders the class @metadata.embedding GraphQL+Handlebars template per doc; combines with id/ids/type/query selectors"},
                expected_data_version:_{type:"string"},
                verb:_{type:"string", enum:["insert","replace","delete"]},
                documents:_{description:"document or list of documents for check insert/replace"},
                nuke:_{type:"boolean"},
                commit_info:_{type:"object",
                             properties:_{author:_{type:"string"}, message:_{type:"string"}}}},
            required:["action"]},
        annotations:_{readOnlyHint:true}
    }, mcp:query_tool),
    register_tool(admin, _{
        name: "admin",
        description: "Server and data-product lifecycle. Actions: `info` (version, auth, indexer status), `list_databases`, `create_database`, `delete_database` (irreversible — requires confirm:\"permanently delete <name>\"), `optimize`, `index_status` (per-branch indexing lag), `reindex`, `export`/`import` (bundle moves inline as `payload` — `path` for a server-local file is stdio only), `docs` (the bundled onboarding doc). Resources look like \"org/db\".",
        inputSchema: _{
            type:"object",
            properties:_{
                action:_{type:"string",
                         enum:["info","list_databases","create_database","delete_database","optimize","index_status","reindex","export","import","docs"]},
                resource:_{type:"string", description:"org/db or org/db/local/branch/name"},
                path:_{type:"string", description:"server-local file for export/import — stdio transport only; over HTTP use `payload`"},
                payload:_{type:"string", description:"import: bundle text (as returned by export's `payload`)"},
                label:_{type:"string"},
                comment:_{type:"string"},
                confirm:_{type:"string", description:"required for irreversible actions — the exact string \"permanently delete <name>\""}},
            required:["action"]},
        outputSchema:_{type:"object"},
        annotations:_{destructiveHint:true}
    }, mcp:admin_tool),
    register_tool(write, _{
        name: "write",
        description: "Change data or schema — one commit contract. Actions: `insert`/`update` (a document or list in `documents`; `graph_type`:\"instance\"|\"schema\" — schema writes auto-inject embedding metadata so classes become vector-searchable; opt out with `indexing`:false or override with `indexing`:{\"query\",\"template\"}), `delete` (`id`|`ids`|`type`|`nuke` selectors), `patch` (a list of diff ops in `patch` — e.g. `[{\"@id\":\"Person/x\",\"name\":{\"@op\":\"SwapValue\",\"@before\":\"a\",\"@after\":\"b\"}}]` or `[{\"@op\":\"Insert\",\"@insert\":{…}}]`; SwapValue's `@before` must equal the stored value exactly — `{\"@op\":\"ForceValue\",\"@after\":\"b\"}` sets the field with no `@before` check), `woql` (a write-capable WOQL JSON AST in `query` — for InsertDocument the document goes in `{\"@type\":\"Value\",\"dictionary\":{\"@type\":\"DictionaryTemplate\",\"data\":[{\"@type\":\"FieldValuePair\",\"field\":\"name\",\"value\":{\"@type\":\"Value\",\"data\":\"x\"}}…]}}`; see admin action `docs` for the full shape), `graphql` (a GraphQL mutation string in `query` — `json:` takes a JSON string or a `JSON` variable; the mutation's own `_commitInfo(author,message)` field also lands in history). Every action requires `commit_info`:{\"author\",\"message\"}. Optional `expected_data_version` (\"branch:<commit>\") guards against drift — on conflict, re-read, re-check, rewrite at the new head. Dry-run any of these first with query action `check`. Returns the new data_version and the assigned document ids.",
        inputSchema: _{
            type:"object",
            properties:_{
                action:_{type:"string",
                         enum:["insert","update","delete","patch","woql","graphql"]},
                resource:_{type:"string", description:"org/db or org/db/local/branch/name"},
                graph_type:_{type:"string", enum:["instance","schema"]},
                commit_info:_{type:"object",
                              properties:_{author:_{type:"string"}, message:_{type:"string"}},
                              required:["message"]},
                documents:_{description:"document or list of documents for insert/update"},
                id:_{type:"string"}, ids:_{type:"array", items:_{type:"string"}},
                type:_{type:"string", description:"class name — delete all docs of this type"},
                nuke:_{type:"boolean", description:"delete every document"},
                patch:_{description:"patch document or list of patch documents"},
                query:_{description:"WOQL JSON AST (write ops allowed) for woql, or GraphQL document string for graphql"},
                variables:_{type:"object", description:"graphql variables — mutations take document payloads here (e.g. variables:{input:{…}} with `json: $input`)"},
                indexing:_{description:"schema writes: false opts out of auto-embedding; {\"query\",\"template\"} overrides the generated embedding query"},
                expected_data_version:_{type:"string"}},
            % commit_info deliberately absent from required — a schema-level
            % refusal would preempt the designed missing_argument error that
            % explains every write is a commit and points at query check
            required:["action","resource"]},
        outputSchema:_{type:"object"},
        annotations:_{destructiveHint:true}
    }, mcp:write_tool),
    register_tool(git, _{
        name: "git",
        description: "Version control for data — the TerminusDB differentiator. Actions: `log` (commit history — `count`,`start`), `list_branches` (branches + head commits), `create_branch` (`source` — branch path or commit; defaults to the db's main branch, `empty`:true creates a schema-less branch), `delete_branch` (irreversible — requires confirm:\"permanently delete <name>\"), `merge` (the branch/commit to merge in — `source`, `to` or `onto` — computes the true common ancestor itself, never trusts caller ancestry; real conflicts surface as isError with witnesses), `squash` (branch history → one commit and move the head — irreversible, requires confirm:\"rewrite history of <branch>\" + commit_info), `reset` (the commit to rewind to — `to`, `source` or `onto`, bare commit id or commit path — irreversible, requires confirm:\"rewrite history of <branch>\"), `rebase` (the head to replay onto — `source`, `to` or `onto`; `strategy_map` optional). `clone` is deferred — use admin export/import. Resources look like \"org/db/local/branch/name\".",
        inputSchema: _{
            type:"object",
            properties:_{
                action:_{type:"string",
                         enum:["log","list_branches","create_branch","delete_branch","merge","squash","reset","rebase","clone"]},
                resource:_{type:"string", description:"org/db or org/db/local/branch/name"},
                source:_{type:"string", description:"the other-end ref for merge/rebase/reset and the origin for create_branch — \"to\"/\"onto\" are aliases"},
                to:_{type:"string", description:"same as \"source\" — commit id or path"},
                branch:_{type:"string", description:"branch name for create_branch when resource is org/db"},
                empty:_{type:"boolean", description:"create_branch: schema-less branch (default branches off main)"},
                commit_info:_{type:"object",
                              properties:_{author:_{type:"string"}, message:_{type:"string"}},
                              required:["message"]},
                confirm:_{type:"string", description:"required for irreversible actions — \"permanently delete <name>\" for deletes, \"rewrite history of <branch>\" for reset/squash"},
                strategy_map:_{type:"object", description:"rebase conflict strategies keyed by commit id"},
                count:_{type:"integer"}, start:_{type:"integer"}},
            required:["action","resource"]},
        outputSchema:_{type:"object"},
        annotations:_{destructiveHint:true}
    }, mcp:git_tool),
    register_tool(plugins, _{
        name: "plugins",
        description: "Extension namespace. Actions: `list` (plugin-contributed tools beyond the built-ins), `call` (`name` + `arguments` — invoke one; plain tools/call with the plugin's name also works). Plugins register tools via mcp:register_plugin_tool/4; a name collision fails with an educational error.",
        inputSchema: _{
            type:"object",
            properties:_{
                action:_{type:"string", enum:["list","call"]},
                name:_{type:"string", description:"plugin tool name for call"},
                arguments:_{type:"object"}},
            required:["action"]},
        outputSchema:_{type:"object"},
        annotations:_{readOnlyHint:true}
    }, mcp:plugins_tool).

register_tool(Name, Spec, Pred) :-
    (   mcp_tool(Name, _, _)
    ->  true
    ;   assertz(mcp_tool(Name, Spec, Pred))).

:- initialization(register_builtin_tools).

% ==========================================================================
% admin tool
% ==========================================================================

admin_tool(Auth, System_DB, Args, Transport, Result) :-
    get_dict(action, Args, Action),
    !,
    % `path` reads/writes files on the server — over HTTP that is
    % arbitrary file access (CWE-22/73) and useless to a remote agent,
    % so file IO is stdio-only; HTTP moves bundles inline as `payload`
    (   memberchk(Action, ["export", "import"]),
        get_dict(path, Args, _),
        Transport \= stdio
    ->  throw(mcp_tool_error(server_local_path(Action),
                           '`path` reads/writes a file on the server — pass `payload` inline over HTTP, or run over stdio for file paths.'))
    ;   admin_action(Auth, System_DB, Action, Args, Result)).
admin_tool(_Auth, _System_DB, _Args, _Transport, _Result) :-
    throw(mcp_error(-32602, 'Invalid params: "action" required',
                    'admin takes {"action":"info|list_databases|create_database|delete_database|optimize|index_status|reindex|export|import|docs", …}')).

admin_action(Auth, System_DB, "info", _Args, Result) :-
    !,
    info(System_DB, Auth, Info),
    Result = _{'_text':"server info", info:Info}.

admin_action(Auth, System_DB, "list_databases", _Args, Result) :-
    !,
    api_db:list_databases(System_DB, Auth, Databases, _{branches:true}),
    Result = _{'_text':"databases", databases:Databases}.

admin_action(Auth, _System_DB, "create_database", Args, Result) :-
    !,
    required_arg(Args, resource, Path, admin),
    org_db_path(Path, Org, DB),
    Label = Args.get(label, DB),
    Comment = Args.get(comment, ""),
    Schema = Args.get(schema, true),
    Public = Args.get(public, false),
    Input_Prefixes = Args.get(prefixes, _{}),
    put_dict(Input_Prefixes,
             _{'@base':"terminusdb:///data/", '@schema':"terminusdb:///schema#"},
             Prefixes),
    atom_string(Org_Atom, Org), atom_string(DB_Atom, DB),
    open_descriptor(system_descriptor{}, Fresh_System),
    db_create:create_db(Fresh_System, Auth, Org_Atom, DB_Atom, Label, Comment,
                        Schema, Public, Prefixes, DB_Uri),
    Result = _{'_text':"database created",
               resource:Path, database_uri:DB_Uri}.

admin_action(Auth, _System_DB, "delete_database", Args, Result) :-
    !,
    required_arg(Args, resource, Path, admin),
    org_db_path(Path, Org, DB),
    require_confirm(Args, Path),
    atom_string(Org_Atom, Org), atom_string(DB_Atom, DB),
    open_descriptor(system_descriptor{}, Fresh_System),
    db_delete:delete_db(Fresh_System, Auth, Org_Atom, DB_Atom, true),
    Result = _{'_text':"database deleted", resource:Path}.

admin_action(Auth, System_DB, "optimize", Args, Result) :-
    !,
    required_arg(Args, resource, Path, admin),
    % api_optimize_queued blocks until the optimize has run — inline when
    % the branch queue is idle, or after pending commits drain.
    api_optimize:api_optimize_queued(System_DB, Auth, Path),
    Result = _{'_text':"optimize completed", resource:Path}.

admin_action(Auth, System_DB, "index_status", Args, Result) :-
    !,
    required_arg(Args, resource, Path, admin),
    (   catch(indexer_backend(Backend), _, fail)
    ->  true
    ;   Backend = none),
    indexer_status_report(Auth, System_DB, Path, Backend, Report),
    Result = _{'_text':"index status", resource:Path,
               indexer:Report}.

admin_action(Auth, System_DB, "reindex", Args, Result) :-
    !,
    required_arg(Args, resource, Path, admin),
    vectorlink:io_index_branch(System_DB, Auth, Path),
    Result = _{'_text':"reindex started", resource:Path}.

admin_action(Auth, System_DB, "export", Args, Result) :-
    !,
    required_arg(Args, resource, Resource_Path, admin),
    api_bundle:bundle(System_DB, Auth, Resource_Path, Payload, []),
    % bundle carries a single branch's history — name what was skipped
    % so export/import is not mistaken for a full clone
    resolve_absolute_string_descriptor(Resource_Path, Branch_Desc),
    Exported = Branch_Desc.branch_name,
    resolve_relative_string_descriptor(Branch_Desc, '_commits', Repo),
    open_descriptor(Repo, Repo_Tx),
    (   setof(B, ref_entity:has_branch(Repo_Tx, B), All_Branches)
    ->  subtract(All_Branches, [Exported], Skipped)
    ;   Skipped = []),
    (   Skipped = []
    ->  Text = "exported"
    ;   format(string(Text),
               'exported — branch "~w" only; ~w not in the bundle',
               [Exported, Skipped])),
    Base = _{'_text':Text, resource:Resource_Path,
             branches_exported:[Exported],
             branches_not_exported:Skipped},
    (   get_dict(path, Args, File)
    ->  setup_call_cleanup(
            open(File, write, Stream),
            format(Stream, "~w", [Payload]),
            close(Stream)),
        size_file(File, Bytes),
        Result = Base.put(_{path:File, bytes:Bytes})
    ;   % no path — return the same text the file would have carried so
        % an HTTP agent can hand it back to `import` as `payload`
        format(string(Payload_Text), "~w", [Payload]),
        Result = Base.put(_{payload:Payload_Text})).

admin_action(Auth, System_DB, "import", Args, Result) :-
    !,
    required_arg(Args, resource, Resource_Path, admin),
    (   get_dict(payload, Args, Payload)
    ->  Extra = _{}
    ;   get_dict(path, Args, File)
    ->  setup_call_cleanup(
            open(File, read, Stream),
            read_string(Stream, _, Payload),
            close(Stream)),
        Extra = _{path:File}
    ;   throw(mcp_tool_error(
              missing_argument(admin, payload),
              'import takes {"action":"import","resource":"org/db","payload":"<bundle>"} — or "path" in place of "payload" on stdio only'))),
    api_unbundle:unbundle(System_DB, Auth, Resource_Path, Payload),
    Result = _{'_text':"imported", resource:Resource_Path}.put(Extra).

admin_action(_Auth, _System_DB, "docs", _Args, Result) :-
    !,
    config:builtin_plugin_path(Builtin_Path),
    atomic_list_concat([Builtin_Path, 'assets/llms.txt'], '/', File),
    read_file_to_string(File, Text, []),
    Result = _{'_text':"onboarding document", docs:Text}.

admin_action(_Auth, _System_DB, Action, _Args, _Result) :-
    format(string(Hint),
           'admin action "~w" unknown. Actions: info, list_databases, create_database, delete_database, optimize, index_status, reindex, export, import, docs.', [Action]),
    throw(mcp_tool_error(unsupported_action(admin, Action), Hint)).

%% org_db_path(+Path, -Org, -DB) is det.
org_db_path(Path, Org, DB) :-
    (   re_matchsub('^([^/]+)/([^/]+)$', Path, Sub, [])
    ->  Org = Sub.1, DB = Sub.2
    ;   format(string(Hint),
               '"~w" is not a database path — databases look like "org/db" (no branch/commit suffix).', [Path]),
        throw(mcp_tool_error(bad_resource_path(Path), Hint))).

%% indexer_status_report(+Auth, +System_DB, +Path, +Backend, -Report) is det.
%
%  Per-branch indexing state: head commit + engine stats when reachable.
indexer_status_report(_Auth, _System_DB, _Path, none, Report) :-
    !,
    Report = _{backend:"none", ready:false,
               reason:"indexer backend is none — no documents are embedded; enable it with TERMINUSDB_INDEXER_BACKEND (and a running Ollama with the embedding model)"}.
indexer_status_report(Auth, System_DB, Path, Backend, Report) :-
    (   catch(vectorlink:vectorlink_endpoint(Endpoint), _, fail)
    ->  Endpoint_Info = Endpoint
    ;   Endpoint_Info = unconfigured),
    (   resolve_absolute_string_descriptor(Path, Desc),
        resolve_relative_string_descriptor(Desc, '_commits', Repo),
        open_descriptor(Repo, Repo_Tx),
        setof(B, ref_entity:has_branch(Repo_Tx, B), Names)
    ->  maplist(branch_index_status(Endpoint_Info, Repo_Tx, Path),
                Names, Branches)
    ;   Branches = []),
    _ = Auth, _ = System_DB,
    Report = _{backend:Backend, endpoint:Endpoint_Info, branches:Branches}.

branch_index_status(unconfigured, _Repo_Tx, _Path, Name,
                    _{name:Name, ready:false, reason:"indexer endpoint not configured"}) :-
    !.
branch_index_status(Endpoint, Repo_Tx, Path, Name, Info) :-
    atom_string(Name_Atom, Name),
    (   ref_entity:branch_head_commit(Repo_Tx, Name_Atom, Uri),
        ref_entity:commit_id_uri(Repo_Tx, Id, Uri)
    ->  format(atom(Head), 'commit:~w', [Id])
    ;   Head = null),
    (   catch(vectorlink:path_to_domain(Path, Domain), _, fail),
        catch(
            vectorlink:io_statistics_for_domain(Endpoint, Domain, Stats),
            Stats_Err, true)
    ->  (   var(Stats_Err)
        ->  Info = _{name:Name, head:Head, ready:true, stats:Stats}
        ;   format(string(Reason),
                   'indexer engine stats unavailable (~w) — check Ollama is running (ollama serve) and the embedding model is pulled; see admin action `docs`',
                   [Stats_Err]),
            Info = _{name:Name, head:Head, ready:false, reason:Reason})
    ;   Info = _{name:Name, head:Head, ready:false,
                 reason:"indexer engine stats unavailable — check Ollama is running (ollama serve) and the embedding model is pulled; see admin action `docs`"}).

% ==========================================================================
% Shared tool helpers
% ==========================================================================

%% required_arg(+Args, +Key, -Value, +Tool) is det.
required_arg(Args, Key, Value, Tool) :-
    (   get_dict(Key, Args, Value)
    ->  true
    ;   Key == path, Tool == query, get_dict(resource, Args, Value)
    ->  true
    ;   format(string(Hint),
               '~w requires "~w" — e.g. {"action":<action>,"~w":"org/db"}',
               [Tool, Key, Key]),
        throw(mcp_tool_error(missing_argument(Tool, Key), Hint))).

%% mcp_expected_data_version(+Args, -Data_Version) is det.
%
%  "branch:<commit>" → data_version(branch, Commit); absent → no_data_version.
mcp_expected_data_version(Args, no_data_version) :-
    \+ get_dict(expected_data_version, Args, _),
    !.
mcp_expected_data_version(Args, data_version(Label, Value)) :-
    get_dict(expected_data_version, Args, String),
    atom_string(Header, String),
    (   read_data_version(Header, data_version(Label, Value))
    ->  true
    ;   throw(mcp_tool_error(
                  bad_data_version(String),
                  'expected_data_version looks like "branch:<commit_id>" — capture it from a write or check result'))).

%% mcp_read_transaction(+Auth, +System_DB, +Path, +Graph_Type, +Args,
%%                      -Transaction, -Data_Version_String) is det.
%
%  Resolve read access, honour expected_data_version, and open the
%  collection. Graph_Type parses "instance"|"schema"|"inference".
mcp_read_transaction(Auth, System_DB, Path, Graph_Type, Args,
                     Transaction, Data_Version) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, Graph_Type,
                            Descriptor),
    mcp_expected_data_version(Args, Requested),
    api_document:before_read(Descriptor, Requested, Actual, Transaction),
    serialize_data_version(Actual, Data_Version).

mcp_graph_type(Args, Graph_Type) :-
    (   get_dict(graph_type, Args, Name)
    ->  atom_string(Graph_Type, Name)
    ;   Graph_Type = instance).

mcp_doc_config(Args, config{skip:Skip, count:Count, as_list:false,
                            compress:true, unfold:Unfold,
                            minimized:true, raw_id:false}) :-
    Skip = Args.get(skip, 0),
    Count = Args.get(count, unlimited),
    Unfold = Args.get(unfold, true).

%% compressed class names in the schema of Transaction
schema_class_name(Transaction, Name) :-
    database_prefixes(Transaction, Prefixes),
    is_frame_class(Transaction, Class),
    compress_schema_uri(Class, Prefixes, Name, [compress_ids(true)]).

%% docs_json_stream(+Docs, -Stream) is det.
%
%  Serialize a document or list of documents as newline-delimited JSON on
%  a string stream — the same wire format json_read_list_stream reads.
%
%  The string carries raw UTF-8 BYTES as code points 0-255 — the same
%  octet-string convention the HTTP layer uses when it buffers request
%  bodies (http_read_json_stream_for_documents_body). Feeding decoded
%  characters instead would emit each non-ASCII code point as a lone
%  latin-1 byte — invalid UTF-8 for the byte-level Rust parser.
docs_json_stream(Docs, Stream) :-
    (   is_list(Docs)
    ->  List = Docs
    ;   List = [Docs]),
    with_output_to(
        codes(Chars),
        forall(member(Doc, List),
               (   (   is_dict(Doc)
                   ->  json_write_dict(current_output, Doc, [width(0)])
                   ;   json_write(current_output, Doc, [width(0)])),
                   nl))),
    once(phrase(utf8_codes(Chars), Bytes)),
    string_codes(Byte_String, Bytes),
    open_string(Byte_String, Stream).

% ==========================================================================
% schema tool
% ==========================================================================

schema_tool(Auth, System_DB, Args, _Transport, Result) :-
    get_dict(action, Args, Action),
    !,
    (   Action = "datatypes"
    ->  schema_action(Auth, System_DB, Action, _{}, Args, Result)
    ;   required_arg(Args, resource, Path, schema),
        schema_action(Auth, System_DB, Action, Path, Args, Result)).
schema_tool(_Auth, _System_DB, _Args, _Transport, _Result) :-
    throw(mcp_error(-32602, 'Invalid params: "action" required',
                    'schema takes {"action":"summary|full|frame|vocab|contract|datatypes","resource":"org/db"}')).

schema_action(Auth, System_DB, "summary", Path, _Args, Result) :-
    !,
    mcp_read_transaction(Auth, System_DB, Path, instance, _{},
                         Transaction, Data_Version),
    mcp_doc_config(_{}, Config),
    findall(_, api_document:api_generate_document_ids(
                   instance, Transaction, Config, _), Instance_Ids),
    findall(_, api_document:api_generate_document_ids(
                   schema, Transaction, Config, _), Schema_Ids),
    findall(Name, schema_class_name(Transaction, Name), Classes0),
    sort(Classes0, Classes),
    length(Instance_Ids, Doc_Count),
    length(Schema_Ids, Schema_Count),
    format(string(Text),
           "~w: ~d documents, ~d schema elements, classes: ~w",
           [Path, Doc_Count, Schema_Count, Classes]),
    Result = _{'_text':Text,
               resource:Path,
               data_version:Data_Version,
               documents:Doc_Count,
               schema_elements:Schema_Count,
               classes:Classes}.
schema_action(Auth, System_DB, "full", Path, _Args, Result) :-
    !,
    api_class_frame(System_DB, Auth, Path, all, Frames,
                    _{compress_ids:true, expand_abstract:true}),
    Result = _{'_text':"all class frames",
               resource:Path,
               frames:Frames}.
schema_action(Auth, System_DB, "frame", Path, Args, Result) :-
    !,
    required_arg(Args, class, Class, schema),
    (   get_dict(format, Args, "jsonschema")
    ->  api_class_jsonschema(System_DB, Auth, Path, uri(Class), Schema),
        Result = _{'_text':"json schema",
                   resource:Path,
                   class:Class,
                   json_schema:Schema}
    ;   api_class_frame(System_DB, Auth, Path, uri(Class), Frame,
                        _{compress_ids:true, expand_abstract:true}),
        % an unknown class gets a bare {"@type":"Class"} stub — refuse it
        (   dict_pairs(Frame, _, [_])
        ->  format(string(Hint),
                   'No class "~w" — schema action `contract` lists the classes that exist', [Class]),
            throw(mcp_tool_error(unknown_class(schema, Class), Hint))
        ;   true),
        Result = _{'_text':"class frame",
                   resource:Path,
                   class:Class,
                   frame:Frame}).
schema_action(Auth, System_DB, "vocab", Path, _Args, Result) :-
    !,
    mcp_read_transaction(Auth, System_DB, Path, instance, _{},
                         Transaction, _),
    database_prefixes(Transaction, Prefixes),
    Result = _{'_text':"prefix vocabulary",
               resource:Path,
               vocab:Prefixes}.
schema_action(Auth, System_DB, "contract", Path, _Args, Result) :-
    !,
    api_class_frame(System_DB, Auth, Path, all, Frames,
                    _{compress_ids:true, expand_abstract:true}),
    del_dict('@context', Frames, _, Data),
    dict_pairs(Data, _, Pairs),
    maplist(frame_to_contract, Pairs, Contracts),
    Result = _{'_text':"schema contract",
               resource:Path,
               contract:Contracts}.
schema_action(_Auth, _System_DB, "datatypes", _Path, _Args, Result) :-
    !,
    default_prefixes(Prefixes),
    findall(Compressed,
            (   is_base_type(IRI),
                compress_dict_uri(IRI, Prefixes, Compressed)),
            Types0),
    sort(Types0, Types),
    Result = _{'_text':"schema type vocabulary — spell a range as a string (\"name\":\"xsd:string\") or a family wrapper ({\"@type\":\"Optional\",\"@class\":\"xsd:string\"}); every non-@ member of a Class document is a property slot",
               datatypes:Types,
               schema_types:["Class","TaggedUnion","Enum","Foreign"],
               families:["Optional","Set","List","Array","Table",
                         "Cardinality"],
               annotations:["@key","@inherits","@subdocument","@abstract",
                            "@unfoldable","@shared","@metadata",
                            "@documentation"]}.
schema_action(_Auth, _System_DB, Action, _Path, _Args, _Result) :-
    format(string(Hint),
           'schema action "~w" unknown. Actions: summary, full, frame (needs class, format), vocab, contract, datatypes.', [Action]),
    throw(mcp_tool_error(unsupported_action(schema, Action), Hint)).

%% frame_to_contract(+Pair, -Contract) is det.
frame_to_contract(Name-Frame, Contract) :-
    Kind = Frame.get('@type'),
    dict_pairs(Frame, _, All_Pairs),
    exclude([K-_V]>>(sub_atom(K, 0, _, _, '@')), All_Pairs, Prop_Pairs),
    maplist(prop_contract, Prop_Pairs, Prop_List),
    dict_pairs(Props, json, Prop_List),
    Contract0 = _{name:Name, kind:Kind, properties:Props},
    (   get_dict('@key', Frame, Key)
    ->  Contract = Contract0.put(key, Key)
    ;   Contract = Contract0).

prop_contract(Prop-Spec, Prop-Label) :-
    (   is_dict(Spec),
        get_dict('@type', Spec, Wrapper),
        get_dict('@class', Spec, Cls)
    ->  format(string(Label), "~w ~w", [Wrapper, Cls])
    ;   is_dict(Spec),
        get_dict('@class', Spec, Cls)
    ->  Label = Cls
    ;   is_dict(Spec),
        get_dict('@type', Spec, Wrapper)
    ->  Label = Wrapper
    ;   Label = Spec).

% ==========================================================================
% query tool
% ==========================================================================

query_tool(Auth, System_DB, Args, _Transport, Result) :-
    get_dict(action, Args, Action),
    !,
    query_action(Auth, System_DB, Action, Args, Result).
query_tool(_Auth, _System_DB, _Args, _Transport, _Result) :-
    throw(mcp_error(-32602, 'Invalid params: "action" required',
                    'query takes {"action":"document|woql|check", …}')).

%% mcp_commit_ref(+Ref0, -Ref) is det.
%
%  Agents hand over commit paths ("org/db/local/commit/<id>") as readily as
%  bare ids — strip the path so a plausible value resolves instead of
%  silently diffing nothing.
mcp_commit_ref(Ref0, Ref) :-
    (   sub_string(Ref0, _, _, After, "/commit/")
    ->  sub_string(Ref0, _, After, 0, Ref)
    ;   string_concat("commit/", Ref, Ref0)
    ->  true
    ;   Ref = Ref0).

query_action(Auth, System_DB, "document", Args, Result) :-
    !,
    required_arg(Args, path, Path, query),
    mcp_graph_type(Args, Graph_Type),
    mcp_document_format(Args, Format, Format_String),
    mcp_read_transaction(Auth, System_DB, Path, Graph_Type, Args,
                         Transaction, Data_Version),
    mcp_doc_config(Args, Config),
    (   Format \= json
    ->  render_query_documents(Format, Format_String, System_DB,
                               Transaction, Graph_Type, Args, Config,
                               Data_Version, Result)
    ;   get_dict(id, Args, Id)
    ->  api_document:api_get_document(Graph_Type, Transaction, Id,
                                      Config, Doc),
        project_fields(Args, Doc, Out),
        Result = _{'_text':"document",
                   data_version:Data_Version,
                   document:Out}
    ;   get_dict(id_only, Args, true)
    ->  collect_ids(Transaction, Graph_Type, Args, Config, Ids0),
        database_prefixes(Transaction, Prefixes),
        compress_ids(Ids0, Prefixes, Ids),
        Result = _{'_text':"document ids",
                   data_version:Data_Version,
                   ids:Ids}
    ;   findall(Doc,
                select_document(Transaction, Graph_Type, Args, Config, Doc),
                Docs0),
        maplist(project_fields(Args), Docs0, Docs),
        length(Docs, N),
        Result = _{'_text':"documents",
                   data_version:Data_Version,
                   count:N,
                   documents:Docs}).

query_action(Auth, System_DB, "woql", Args, Result) :-
    !,
    (   get_dict(query, Args, Query), is_dict(Query)
    ->  true
    ;   throw(mcp_error(-32602,
                        'Invalid params: "query" must be a WOQL JSON AST object',
                        'Try {"action":"woql","query":{"@type":"IsA","element":{"@type":"NodeValue","variable":"X"},"type":{"@type":"NodeValue","node":"<class>"}}}'))),
    assert_read_only_woql(Query),
    assert_no_ingest_woql(Query),
    (   get_dict(count, Args, Limit_N)
    ->  Run_Query = _{'@type':"Limit", limit:Limit_N, query:Query}
    ;   Run_Query = Query),
    (   get_dict(path, Args, Path)
    ->  Path_Option = some(Path)
    ;   Path_Option = none),
    mcp_expected_data_version(Args, Requested),
    Options = _{files:[],
                all_witnesses:Args.get(all_witnesses, false),
                data_version:Requested,
                commit_info:_{},
                optimize:false,
                streaming:false,
                library:none},
    woql_query_json(System_DB, Auth, Path_Option, json_query(Run_Query),
                    _Context, New_Data_Version, _Meta, Response, Options),
    serialize_data_version(New_Data_Version, Data_Version),
    Result = _{'_text':"woql result",
               data_version:Data_Version,
               result:Response}.

query_action(Auth, System_DB, "check", Args, Result) :-
    !,
    required_arg(Args, path, Path, query),
    required_arg(Args, verb, Verb, query),
    mcp_graph_type(Args, Graph_Type),
    mcp_expected_data_version(Args, Requested),
    Commit_Info = Args.get(commit_info, _{}),
    Author = Commit_Info.get(author, "mcp-agent"),
    Message = Commit_Info.get(message, "dry-run check via MCP"),
    check_body(Args, Graph_Type, Verb, Body, Touched),
    api_check_documents(System_DB, Auth, Path, Graph_Type, Author,
                        Message, Requested, Body, Touched, options{}, Check),
    (   get_dict('api:valid', Check, true)
    ->  Text = "check: VALID — safe to write"
    ;   Text = "check: INVALID — see witnesses for what would fail"),
    % ids in api:extra arrive expanded — compress them to the Class/id
    % form every other tool returns
    (   get_dict('api:extra', Check, Extra),
        is_dict(Extra),
        get_dict(ids, Extra, Ids0),
        mcp_read_transaction(Auth, System_DB, Path, instance, _{},
                             Transaction, _),
        database_prefixes(Transaction, Prefixes),
        compress_ids(Ids0, Prefixes, Ids)
    ->  Check2 = Check.put('api:extra', Extra.put(ids, Ids))
    ;   Check2 = Check),
    Result = Check2.put('_text', Text).

query_action(Auth, System_DB, "graphql", Args, Result) :-
    !,
    required_arg(Args, path, Path, query),
    required_arg(Args, query, Query, query),
    (   string(Query)
    ->  true
    ;   throw(mcp_tool_error(
                  bad_graphql_query(Query),
                  'graphql takes a GraphQL document string: {"action":"graphql","path":"org/db","query":"{ Person { name } }"}'))),
    Variables = Args.get(variables, none),
    mcp_graphql(System_DB, Auth, Path, Query, Variables, read,
                Response, Data_Version),
    Result = _{'_text':"graphql result",
               data_version:Data_Version,
               response:Response}.

query_action(Auth, System_DB, "search", Args, Result) :-
    !,
    required_arg(Args, path, Path, query),
    mcp_search(Auth, System_DB, Path, Args, Result).

query_action(Auth, System_DB, "history", Args, Result) :-
    !,
    required_arg(Args, path, Path, query),
    (   get_dict(id, Args, Id)
    ->  true
    ;   throw(mcp_tool_error(
                  missing_argument(query, id),
                  'history reports per-document history and needs `id` — for repo-level change history use git action `log`'))),
    put_dict(_{start:Args.get(start, 0),
               count:Args.get(count, 50),
               created:Args.get(created, false),
               updated:Args.get(updated, false),
               before:Args.get(before, _),
               after:Args.get(after, _),
               fast:Args.get(fast, false),
               complete:Args.get(complete, false),
               graph_type:Args.get(graph_type, _),
               diff:Args.get(diff, false)},
             _{}, Options),
    api_history:api_document_history(System_DB, Auth, Path, Id,
                                     Response, Options),
    Result = _{'_text':"document history", history:Response}.

query_action(Auth, System_DB, "diff", Args, Result) :-
    !,
    required_arg(Args, path, Path, query),
    required_arg(Args, before, Before0, query),
    required_arg(Args, after, After0, query),
    mcp_commit_ref(Before0, Before),
    mcp_commit_ref(After0, After),
    % keep/unfold mirror the diff route defaults — without keep,
    % simple_diff fails and update diffs silently come back empty.
    Options = options{start:Args.get(start, 0),
                      count:Args.get(count, 100),
                      keep:_{'@id':true, '_id':true},
                      unfold:true},
    (   get_dict(id, Args, Doc_Id)
    ->  (   api_patch:api_diff_id(System_DB, Auth, Path, Before, After,
                                  Doc_Id, Diff, Options)
        ->  Diffs = [Diff]
        ;   Diffs = [])
    ;   api_patch:api_diff_all_documents(System_DB, Auth, Path,
                                       Before, After, Diffs, Options)),
    length(Diffs, N),
    Result = _{'_text':"diff", count:N, diffs:Diffs}.

query_action(_Auth, _System_DB, Action, _Args, _Result) :-
    format(string(Hint),
           'query action "~w" unknown. Actions: document, woql, check, graphql, search, history, diff.', [Action]),
    throw(mcp_tool_error(unsupported_action(query, Action), Hint)).

%% mcp_document_format(+Args, -Format, -Format_String) is det.
%
%  `format` selects the same output renderers the document HTTP endpoint
%  offers (document_output_format/1): json (default), embedding, markdown.
mcp_document_format(Args, Format, Format_String) :-
    (   get_dict(format, Args, Format_String)
    ->  atom_string(Format, Format_String),
        (   api_document:document_output_format(Format)
        ->  true
        ;   findall(F, api_document:document_output_format(F), Formats),
            atomic_list_concat(Formats, ', ', Valid),
            format(string(Hint),
                   'format "~w" is not a document output format — valid: ~w',
                   [Format_String, Valid]),
            throw(mcp_tool_error(unsupported_document_format(Format_String),
                                 Hint)))
    ;   Format = json, Format_String = "json").

%% render_query_documents(+Format, +Format_String, +System_DB, +Transaction,
%%                        +Graph_Type, +Args, +Config, +Data_Version, -Result)
%
%  Render selected documents through the document-stream format handlers
%  (embedding = per-class @metadata.embedding GraphQL+Handlebars template;
%  markdown = json2markdown). Returns per-document text plus `page`, the
%  concatenated rendered page — the same payload format=… streams over HTTP.
render_query_documents(Format, Format_String, System_DB, Transaction,
                       Graph_Type, Args, Config, Data_Version, Result) :-
    (   get_dict(id_only, Args, true)
    ->  throw(mcp_tool_error(incompatible_arguments(format, id_only),
              'id_only returns ids, not renderable documents — drop id_only (or drop format)'))
    ;   get_dict(fields, Args, Fields), is_list(Fields)
    ->  throw(mcp_tool_error(incompatible_arguments(format, fields),
              'fields projects JSON output — drop it: a rendered format always renders the whole document'))
    ;   true),
    (   get_dict(id, Args, Id)
    ->  Selector_Args = Args.put(ids, [Id])
    ;   Selector_Args = Args),
    Stream_Config = _{system_db:System_DB,
                      transaction:Transaction, request:[]},
    api_document:document_stream_start(Format, Stream_Config, State),
    findall(Doc_Id-Text,
            (   select_document(Transaction, Graph_Type, Selector_Args,
                                Config, Doc),
                get_dict('@id', Doc, Doc_Id),
                with_output_to(
                    string(Text),
                    api_document:document_stream_write(Format, Stream_Config,
                                                       State, Doc))),
            Pairs),
    api_document:document_stream_end(Format, Stream_Config),
    maplist([I-T, _{id:I, text:T}]>>true, Pairs, Documents),
    maplist([_-T, T]>>true, Pairs, Texts),
    atomic_list_concat(Texts, Page),
    length(Pairs, N),
    Result = _{'_text':Page,
               format:Format_String,
               data_version:Data_Version,
               count:N,
               documents:Documents,
               page:Page}.

%% mcp_graphql(+System_DB, +Auth, +Path, +Query, +Variables, +Mode,
%%             -Response, -Data_Version) is det.
%
%  Runs a GraphQL document through handle_graphql_request with an
%  in-memory body stream. Mode read rejects mutation operations before
%  they reach the engine — the API would otherwise let a sufficiently
%  privileged caller commit through `query`, which must stay read-only.
%  Variables go into the request body's `variables` object — mutations
%  take their JSON document payloads that way (e.g. `json: $input`).
mcp_graphql(System_DB, Auth, Path, Query, Variables, Mode,
            Response, Data_Version) :-
    atom_string(Path_Atom, Path),
    (   resolve_absolute_string_descriptor(Path, Desc)
    ->  true
    ;   throw(mcp_tool_error(
                  invalid_absolute_path(Path),
                  'Path should look like "org/db" or "org/db/local/branch/main"'))),
    open_descriptor(Desc, Transaction),
    api_graphql:get_or_create_graphql_context(Transaction, Ctx),
    (   is_dict(Variables)
    ->  Body_Dict = _{query:Query, variables:Variables}
    ;   Body_Dict = _{query:Query}),
    with_output_to(string(Body),
                   json_write_dict(current_output, Body_Dict,
                                   [width(0)])),
    (   catch('$graphql':get_operation_type_from_body(Ctx, Body, Op),
              _, fail)
    ->  true
    ;   Op = mutation),
    (   Mode = read, Op = mutation
    ->  throw(mcp_tool_error(
                  read_only_graphql(Op),
                  'query is read-only — route GraphQL mutations through write: {"name":"write","arguments":{"action":"graphql","resource":"<path>","query":"<mutation>","commit_info":{"author":…,"message":…}}}'))
    ;   true),
    % Octet-string convention (same as docs_json_stream): each code point
    % is one raw UTF-8 byte — the graphql engine parses bodies byte-level.
    string_codes(Body, Codes),
    once(phrase(utf8_codes(Codes), Bytes)),
    string_codes(Byte_Body, Bytes),
    (   catch(
            setup_call_cleanup(
                open_string(Byte_Body, In),
                api_graphql:handle_graphql_request(
                    System_DB, Auth, post, Path_Atom, In,
                    Response_Atom0, _Content_Type, _Content_Length,
                    New_DV0, _Meta, true),
                close(In)),
            response(Response_Atom0),
            New_DV0 = none)
    ->  true
    ;   throw(mcp_tool_error(graphql_failed,
                             'graphql call failed without a response'))),
    (   catch(atom_json_dict(Response_Atom0, Parsed, []), _, fail)
    ->  Response = Parsed
    ;   Response = Response_Atom0),
    (   is_dict(Response), get_dict(errors, Response, Errors)
    ->  Errors = [First|_],
        Message = First.get(message, "graphql error"),
        errors_in(Errors, More),
        format(string(Hint),
               'graphql error: ~s (~w more may follow). Fix the document and retry; nothing was committed.',
               [Message, More]),
        throw(mcp_tool_error(graphql_errors(Errors), Hint))
    ;   true),
    (   memberchk(New_DV0, [none, no_data_version])
    ->  Data_Version = null
    ;   serialize_data_version(New_DV0, Data_Version)).

%% errors_in(+Errors, -N) is det — count of errors after the first.
errors_in(Errors, N) :-
    length(Errors, L),
    N is L - 1.

%% mcp_search(+Auth, +System_DB, +Path, +Args, -Result) is det.
%
%  VectorLink-backed retrieval. Mirrors the HTTP search handlers but
%  returns the engine body as structuredContent instead of writing a
%  response. Every failure mode gets its own educational hint.
mcp_search(Auth, System_DB, Path, Args, Result) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance,
                            Descriptor),
    (   catch(indexer_backend(Backend), _, fail)
    ->  true
    ;   Backend = none),
    (   Backend = none
    ->  throw(mcp_tool_error(
                  indexer_backend_none,
                  'indexing is not enabled — no document will be embedded. Enable it: TERMINUSDB_INDEXER_BACKEND=vectorlink plus Ollama running with the embedding model: `ollama serve`, `ollama pull hf.co/vectorlink/multilingual-e5-base-GGUF:q8_0`, `ollama cp … multilingual-e5-base`. Meanwhile use query action `woql` or `document` for lexical retrieval.'))
    ;   true),
    (   catch(vectorlink:vectorlink_endpoint(Endpoint), _, fail)
    ->  true
    ;   throw(mcp_tool_error(
                  vectorlink_endpoint_not_configured(search),
                  'no VectorLink endpoint configured — check admin action `index_status`, then `reindex` once indexing is up'))),
    (   branch_descriptor{} :< Descriptor
    ;   commit_descriptor{} :< Descriptor
    ->  true
    ;   throw(mcp_tool_error(
                  search_requires_branch_descriptor(Path),
                  'search needs a branch or commit resource — e.g. "org/db" or "org/db/local/branch/main"'))),
    vectorlink:descriptor_domain(Descriptor, Domain),
    vectorlink:resolve_search_commit(Descriptor, Search_Ref, _Uri,
                                     Ancestors),
    vectorlink:compress_flag([], Compress),
    vectorlink:maybe_prefixes(Compress, Descriptor, Prefixes),
    search_body(Args, Body),
    Kind = Args.get(kind, "search"),
    (   (   branch_descriptor{branch_name:Branch_Name} :< Descriptor
        ->  true
        ;   Branch_Name = none)),
    (   Kind = "search"
    ->  vectorlink:search_extra_params([], Body, Prefixes, Params),
        Forward = io_search_forward
    ;   Kind = "similar"
    ->  vectorlink:similar_extra_params([], Body, Prefixes, Params),
        Forward = io_similar_forward
    ;   Kind = "suggest"
    ->  vectorlink:search_extra_params([], Body, Prefixes, Params),
        Forward = io_suggest_forward
    ;   Kind = "duplicates"
    ->  vectorlink:duplicates_extra_params([], Body, Prefixes, Params),
        Forward = io_duplicates_forward
    ;   format(string(Hint),
               'search kind "~w" unknown — kinds: search, similar, suggest, duplicates',
               [Kind]),
        throw(mcp_tool_error(unsupported_action(search, Kind), Hint))),
    catch(
        call(vectorlink:Forward, Endpoint, Domain, Search_Ref, Ancestors,
             Params, Response_Body, Data_Version_Header),
        error(vectorlink_forward_failed(404, Engine_Body, _URL), _),
        (   vectorlink:maybe_nudge_push_async(System_DB, Auth, Path,
                                            Branch_Name),
            format(string(Idx_Hint),
                   'the index for this resource is missing or stale — run admin action `reindex` for the resource, then retry. If it persists, check Ollama is running with the embedding model pulled. Engine said: ~w',
                   [Engine_Body]),
            throw(mcp_tool_error(search_not_indexed(Path), Idx_Hint)))),
    (   catch(atom_json_dict(Response_Body, Parsed, []), _, fail)
    ->  Response = Parsed
    ;   Response = Response_Body),
    Result = _{'_text':"search results",
               kind:Kind,
               data_version:Data_Version_Header,
               response:Response}.

%% search_body(+Args, -Body) is det.
%
%  Only parameters the VectorLink handlers already accept. `query` is an
%  alias for `q` — the other query actions all take `query`, so agents
%  reach for it first.
search_body(Args, Body) :-
    findall(K-V,
            (   member(K, [q, mode, start, count, snippet, id, text,
                           threshold, doc_type, doc_id,
                           target_doc_type, target_doc_id]),
                get_dict(K, Args, V)),
            Pairs),
    (   \+ memberchk(q-_, Pairs),
        get_dict(query, Args, Alias_Q)
    ->  All_Pairs = [q-Alias_Q|Pairs]
    ;   All_Pairs = Pairs),
    dict_pairs(Body, _, All_Pairs).

%% select_document(+Transaction, +Graph_Type, +Args, +Config, -Doc) is nondet.
select_document(Transaction, Graph_Type, Args, Config, Doc) :-
    (   get_dict(ids, Args, Ids)
    ->  member(Id, Ids),
        api_document:api_get_document(Graph_Type, Transaction, Id,
                                      Config, Doc)
    ;   get_dict(query, Args, Query)
    ->  Type0 = Args.get(type, _),
        expand_query_document(Transaction, Type0, Query, Query_Ex, Type_Ex),
        api_document:api_get_documents_by_query(
            Transaction, Graph_Type, Type_Ex, Query_Ex, Config, Doc)
    ;   get_dict(type, Args, Type)
    ->  api_document:api_get_documents_by_type(
            Transaction, Graph_Type, Type, Config, Doc)
    ;   api_document:api_get_documents(
            Transaction, Graph_Type, Config, Doc)).

collect_ids(Transaction, Graph_Type, Args, Config, Ids) :-
    (   get_dict(type, Args, Type)
    ->  findall(Id,
                api_document:api_generate_document_ids_by_type(
                    Graph_Type, Transaction, Type, Config, Id),
                Ids)
    ;   findall(Id,
                api_document:api_generate_document_ids(
                    Graph_Type, Transaction, Config, Id),
                Ids)).

project_fields(Args, Doc, Out) :-
    (   get_dict(fields, Args, Fields),
        is_list(Fields)
    ->  dict_pairs(Doc, Tag, Pairs),
        findall(K-V,
                (   member(K-V, Pairs),
                    (   memberchk(K, ['@id','@type'])
                    ;   member(F, Fields),
                        atom_string(K, F))),
                Kept),
        dict_pairs(Out, Tag, Kept)
    ;   Out = Doc).

%% check_body(+Args, +Graph_Type, +Verb, -Body, -Touched) is det.
check_body(Args, Graph_Type, "insert", Body, Touched) :-
    !,
    required_arg(Args, documents, Docs, query),
    Options = options{raw_json:false, full_replace:false,
                      merge_repeats:false, overwrite:false},
    put_dict(document_id, Options, Args.get(document_id, null), Options2),
    docs_json_stream(Docs, Stream),
    api_check_insert_body(Stream, Graph_Type, Options2, Body),
    touched_types(Docs, Touched).
check_body(Args, Graph_Type, "replace", Body, Touched) :-
    !,
    required_arg(Args, documents, Docs, query),
    Options = options{raw_json:false,
                      create:Args.get(create, false),
                      merge_repeats:false},
    put_dict(document_id, Options, Args.get(document_id, null), Options2),
    docs_json_stream(Docs, Stream),
    api_check_replace_body(Stream, Graph_Type, Options2, Body),
    touched_types(Docs, Touched).
check_body(Args, Graph_Type, "delete", Body, []) :-
    !,
    (   get_dict(ids, Args, Ids)
    ->  docs_json_stream(Ids, Stream),
        api_check_delete_body(ids_stream(Stream), Graph_Type, Body)
    ;   (   get_dict(nuke, Args, true)
        ->  Spec = nuke
        ;   get_dict(id, Args, Id)
        ->  Spec = id(Id)
        ;   get_dict(type, Args, Type)
        ->  Spec = type(Type)
        ;   throw(mcp_tool_error(
                      missing_argument(query, targets),
                      'check delete needs one of: nuke:true, id, type, or ids'))),
        api_check_delete_body(Spec, Graph_Type, Body)).
check_body(_Args, _Graph_Type, Verb, _Body, _Touched) :-
    format(string(Hint),
           'check verb "~w" unknown — verbs: insert, replace, delete', [Verb]),
    throw(mcp_tool_error(unsupported_action(check, Verb), Hint)).

touched_types(Docs, Touched) :-
    (   is_list(Docs)
    ->  List = Docs
    ;   List = [Docs]),
    findall(T,
            (   member(Doc, List),
                get_dict('@type', Doc, TV),
                (   is_list(TV) -> member(T, TV) ; T = TV)),
            Touched0),
    sort(Touched0, Touched).

%% assert_read_only_woql(+AST) is det.
%
%  WOQL via query is read-only: any write op anywhere in the AST is
%  rejected with a hint pointing at the write tool.
assert_read_only_woql(AST) :-
    woql_write_types(Write_Types),
    forall(
        woql_ast_type(AST, Type),
        (   memberchk(Type, Write_Types)
        ->  format(string(Hint),
                   '"~w" mutates data and query is read-only. Route through write instead: {"name":"write","arguments":{"action":"woql","query":<your AST>,"commit_info":{"author":…,"message":…}}} — or dry-run first with query action `check`.',
                   [Type]),
            throw(mcp_tool_error(read_only_woql(Type), Hint))
        ;   true)).

woql_ast_type(Term, Type) :-
    is_dict(Term),
    get_dict('@type', Term, Type).
woql_ast_type(Term, Type) :-
    is_dict(Term),
    dict_pairs(Term, _, Pairs),
    member(_-V, Pairs),
    woql_ast_type(V, Type).
woql_ast_type(Term, Type) :-
    is_list(Term),
    member(V, Term),
    woql_ast_type(V, Type).

% Write-capable WOQL @types — the set woql_compile maps to mutating
% goals (insert_document, replace_document, delete_document,
% update_object, delete_object, insert, delete). Added*/Deleted* are NOT
% here: they compile to xrdf_added/xrdf_deleted reads over the commit
% delta. Mirrors (and is more complete than) update_operators in the JS
% client's woqlCore.containsUpdate, which misses Update*/DeleteObject.
woql_write_types(["InsertDocument","UpdateDocument","DeleteDocument",
                  "UpdateObject","DeleteObject",
                  "AddTriple","DeleteTriple","AddQuad","DeleteQuad"]).

%% assert_no_ingest_woql(+AST) is det.
%
%  `Get` ingests an external resource: `post` needs an upload channel
%  MCP does not have, and `url` is an unrestricted server-side fetch
%  (copy_remote → http_open — CWE-918 SSRF). Block it on both woql paths.
assert_no_ingest_woql(AST) :-
    forall(
        woql_ast_type(AST, Type),
        (   memberchk(Type, ["Get"])
        ->  format(string(Hint),
                   '"~w" ingests an external file or URL — not available over MCP: `post` has no upload channel and `url` makes the server fetch arbitrary addresses. Insert data with `write` instead.',
                   [Type]),
            throw(mcp_tool_error(woql_ingest_unsupported(Type), Hint))
        ;   true)).

% ==========================================================================
% write tool
% ==========================================================================

write_tool(Auth, System_DB, Args, _Transport, Result) :-
    get_dict(action, Args, Action),
    !,
    required_arg(Args, resource, Path, write),
    write_commit_info(Args, Author, Message, Commit_Info),
    mcp_graph_type(Args, Graph_Type),
    mcp_expected_data_version(Args, Requested),
    write_action(Auth, System_DB, Action, Path, Graph_Type, Requested,
                 Author, Message, Commit_Info, Args, Result).
write_tool(_Auth, _System_DB, _Args, _Transport, _Result) :-
    throw(mcp_error(-32602, 'Invalid params: "action" required',
                    'write takes {"action":"insert|update|delete|patch|woql","resource":"org/db","commit_info":{…}, …}')).

%% write_commit_info(+Args, -Author, -Message, -Commit_Info) is det.
%
%  Every write is a commit — commit_info is mandatory so history stays
%  meaningful. Author defaults to the MCP agent identity.
write_commit_info(Args, Author, Message, Commit_Info) :-
    (   get_dict(commit_info, Args, CI), is_dict(CI)
    ->  true
    ;   throw(mcp_tool_error(
                  missing_argument(write, commit_info),
                  'write requires commit_info{"author","message"} — every change becomes a commit. Dry-run first with query action `check` (free, no commit).'))),
    (   get_dict(message, CI, Message), Message \= ""
    ->  true
    ;   throw(mcp_tool_error(
                  missing_argument(write, 'commit_info.message'),
                  'commit_info.message is required — it lands in history; make it describe the change.'))),
    Author = CI.get(author, "mcp-agent"),
    Commit_Info = commit_info{author:Author, message:Message}.

write_action(Auth, System_DB, "insert", Path, Graph_Type, Requested,
             Author, Message, _CI, Args, Result) :-
    !,
    required_arg(Args, documents, Docs, write),
    (   Graph_Type = schema
    ->  inject_embedding_metadata(Docs, Args, Docs2, Embedded, Not_Embedded)
    ;   Docs2 = Docs, Embedded = [], Not_Embedded = []),
    Options = [author(Author), message(Message), graph_type(Graph_Type)],
    docs_json_stream(Docs2, Stream),
    api_insert_documents(System_DB, Auth, Path, Stream, Requested,
                         New_DV, _Meta, Ids, Options),
    serialize_data_version(New_DV, Data_Version),
    Result = _{'_text':"insert committed",
               data_version:Data_Version,
               ids:Ids,
               embedded:Embedded,
               not_embedded:Not_Embedded}.
write_action(Auth, System_DB, "update", Path, Graph_Type, Requested,
             Author, Message, _CI, Args, Result) :-
    !,
    required_arg(Args, documents, Docs, write),
    (   Graph_Type = schema
    ->  inject_embedding_metadata(Docs, Args, Docs2, Embedded, Not_Embedded)
    ;   Docs2 = Docs, Embedded = [], Not_Embedded = []),
    Options = [author(Author), message(Message), graph_type(Graph_Type),
               create(false)],
    docs_json_stream(Docs2, Stream),
    api_replace_documents(System_DB, Auth, Path, Stream, Requested,
                          New_DV, _Meta, Ids, Options),
    serialize_data_version(New_DV, Data_Version),
    Result = _{'_text':"update committed",
               data_version:Data_Version,
               ids:Ids,
               embedded:Embedded,
               not_embedded:Not_Embedded}.
write_action(Auth, System_DB, "delete", Path, Graph_Type, Requested,
             Author, Message, _CI, Args, Result) :-
    !,
    Options = [author(Author), message(Message), graph_type(Graph_Type)],
    (   get_dict(nuke, Args, true)
    ->  api_nuke_documents(System_DB, Auth, Path, Requested,
                           New_DV, _Meta, Options),
        Ids = []
    ;   get_dict(id, Args, Id)
    ->  atom_string(Id_Atom, Id),
        api_delete_document(System_DB, Auth, Path, Id_Atom, Requested,
                            New_DV, _Meta, Options),
        Ids = [Id]
    ;   get_dict(type, Args, Type)
    ->  api_delete_documents_by_type(System_DB, Auth, Path, Type, Requested,
                                     New_DV, _Meta, Options),
        Ids = []
    ;   get_dict(ids, Args, Ids)
    ->  docs_json_stream(Ids, Stream),
        api_delete_documents(System_DB, Auth, Path, Stream, Requested,
                             New_DV, _Meta, _Out, Options)
    ;   throw(mcp_tool_error(
                  missing_argument(write, delete_selector),
                  'delete needs a selector: id | ids | type | nuke:true'))),
    serialize_data_version(New_DV, Data_Version),
    Result = _{'_text':"delete committed",
               data_version:Data_Version,
               ids:Ids}.
write_action(Auth, System_DB, "patch", Path, _Graph_Type, _Requested,
             _Author, _Message, Commit_Info, Args, Result) :-
    !,
    required_arg(Args, patch, Patch, write),
    api_patch:api_patch_resource(System_DB, Auth, Path, Patch, Commit_Info,
                                 Ids0, []),
    mcp_read_transaction(Auth, System_DB, Path, instance, _{},
                         Transaction, Data_Version),
    database_prefixes(Transaction, Prefixes),
    compress_ids(Ids0, Prefixes, Ids),
    Result = _{'_text':"patch committed",
               data_version:Data_Version,
               ids:Ids}.
write_action(Auth, System_DB, "woql", Path, _Graph_Type, Requested,
             _Author, _Message, Commit_Info, Args, Result) :-
    !,
    (   get_dict(query, Args, Query), is_dict(Query)
    ->  true
    ;   throw(mcp_error(-32602,
                        'Invalid params: "query" must be a WOQL JSON AST object',
                        'Try {"action":"woql","query":{"@type":"InsertDocument","document":{"@id":"City/x","@type":"City","name":"x"}}, …} — put @id inside the document'))),
    assert_no_ingest_woql(Query),
    Options = _{files:[],
                all_witnesses:false,
                data_version:Requested,
                commit_info:Commit_Info,
                optimize:false,
                streaming:false,
                library:none},
    woql_query_json(System_DB, Auth, some(Path), json_query(Query),
                    _Context, New_DV, _Meta, Bindings, Options),
    serialize_data_version(New_DV, Data_Version),
    Result = _{'_text':"woql write committed",
               data_version:Data_Version,
               result:Bindings}.
write_action(Auth, System_DB, "graphql", Path, _Graph_Type, Requested,
             _Author, _Message, _CI, Args, Result) :-
    !,
    required_arg(Args, query, Query, write),
    (   string(Query)
    ->  true
    ;   throw(mcp_tool_error(
                  bad_graphql_query(Query),
                  'graphql takes a GraphQL document string: {"action":"graphql","query":"mutation($i:JSON!) { _commitInfo(author:\"a\",message:\"m\") _insertDocuments(json:$i) }","variables":{"i":[{doc…}]}} — json: accepts a JSON string or a JSON variable, not an inline object'))),
    (   Requested \= no_data_version
    ->  mcp_assert_head(Path, Requested)
    ;   true),
    Variables = Args.get(variables, none),
    mcp_graphql(System_DB, Auth, Path, Query, Variables, write,
                Response, Data_Version),
    Result = _{'_text':"graphql committed",
               data_version:Data_Version,
               response:Response}.

write_action(_Auth, _System_DB, Action, _Path, _Graph_Type, _Requested,
             _A, _M, _CI, _Args, _Result) :-
    format(string(Hint),
           'write action "~w" unknown. Actions: insert, update, delete, patch, woql, graphql.', [Action]),
    throw(mcp_tool_error(unsupported_action(write, Action), Hint)).

%% mcp_assert_head(+Path, +Requested) is det.
%
%  Optimistic-concurrency check for write paths whose underlying API
%  (GraphQL) does not take a data_version: resolve the descriptor and
%  compare its head commit with the caller's expectation.
mcp_assert_head(Path, data_version(_Label, Expected)) :-
    (   resolve_absolute_string_descriptor(Path, Desc)
    ->  true
    ;   throw(mcp_tool_error(
                  invalid_absolute_path(Path),
                  'Path should look like "org/db" or "org/db/local/branch/main"'))),
    (   branch_descriptor{branch_name:Name} :< Desc
    ->  resolve_relative_string_descriptor(Desc, '_commits', Repo),
        open_descriptor(Repo, Repo_Tx),
        (   catch((ref_entity:branch_head_commit(Repo_Tx, Name, Uri),
                   ref_entity:commit_id_uri(Repo_Tx, Head_Id, Uri)),
                  _, fail)
        ->  true
        ;   Head_Id = none),
        (   atom_string(Head_String, Head_Id),
            Expected == Head_String
        ->  true
        ;   throw(mcp_tool_error(
                      commit_conflict(Expected, Head_Id),
                      'expected_data_version does not match the branch head — the data moved. Re-read, re-check, retry the write at the new head.')))
    ;   true).

% ==========================================================================
% Auto-embedding on schema writes (GraphRAG by default)
% ==========================================================================

%% inject_embedding_metadata(+Docs, +Args, -Docs2, -Embedded, -Not_Embedded) is det.
%
%  For every class document lacking @metadata.embedding, inject a
%  generated embedding query covering its string properties — every
%  class becomes vector-searchable by default. indexing:false opts out;
%  indexing:{"query","template"} overrides. Reports per-class outcome.
inject_embedding_metadata(Docs, Args, Docs2, Embedded, Not_Embedded) :-
    (   is_list(Docs) -> List = Docs ; List = [Docs]),
    config:indexer_backend(Backend),
    (   Backend = none -> Capable = false ; Capable = true),
    maplist(maybe_inject_embedding(Args, Capable), List, List2,
            Embedded0, Not_Embedded0),
    append(Embedded0, Embedded),
    append(Not_Embedded0, Not_Embedded),
    (   is_list(Docs) -> Docs2 = List2 ; List2 = [Docs2]).

maybe_inject_embedding(_Args, _Capable, Doc, Doc, [], []) :-
    \+ (is_dict(Doc), get_dict('@type', Doc, "Class")),
    !.
maybe_inject_embedding(_Args, _Capable, Doc, Doc, [Class], []) :-
    get_dict('@metadata', Doc, MD), is_dict(MD),
    get_dict(embedding, MD, _),
    !,
    Class = Doc.get('@id', "<unnamed>").
maybe_inject_embedding(Args, _Capable, Doc, Doc, [], [Report]) :-
    get_dict(indexing, Args, false),
    !,
    Report = _{class:Doc.get('@id', "<unnamed>"),
               reason:"indexing:false — class will not be embedded or searchable via `query.search`; documents of this class are invisible to semantic search"}.
% explicit indexing:{query[,template]} is stored data, not a backend
% capability — honor it with no indexer too, so query action document
% format:"embedding" can render it; only semantic search needs a backend
maybe_inject_embedding(Args, Capable, Doc, Doc2, Embedded, Not_Embedded) :-
    get_dict(indexing, Args, Indexing), is_dict(Indexing),
    get_dict(query, Indexing, Query),
    !,
    (   get_dict(template, Indexing, Template)
    ->  Embedding = _{query:Query, template:Template}
    ;   Embedding = _{query:Query}),
    Class = Doc.get('@id', "<unnamed>"),
    (   get_dict('@metadata', Doc, MD0), is_dict(MD0)
    ->  MD = MD0.put(embedding, Embedding)
    ;   MD = _{embedding:Embedding}),
    Doc2 = Doc.put('@metadata', MD),
    (   Capable = true
    ->  Embedded = [Class], Not_Embedded = []
    ;   Embedded = [],
        Not_Embedded = [_{class:Class,
                          reason:"indexer backend is none — embedding metadata stored and renders via query action document format:\"embedding\", but the class is not searchable via `query.search` until an indexer is enabled"}]).
maybe_inject_embedding(_Args, false, Doc, Doc, [], [Report]) :-
    !,
    Report = _{class:Doc.get('@id', "<unnamed>"),
               reason:"indexer backend is none — no documents are embedded; enable it with TERMINUSDB_INDEXER_BACKEND (and a running Ollama with the embedding model)"}.
maybe_inject_embedding(_Args, true, Doc, Doc2, [Class], []) :-
    class_string_properties(Doc, Props),
    !,
    Class = Doc.get('@id', "<unnamed>"),
    atomic_list_concat(Props, ' ', Prop_Text),
    format(atom(Q), 'query($id: ID){ ~w(id : $id) { ~w } }',
           [Class, Prop_Text]),
    atom_string(Q, Query_String),
    Embedding = _{query:Query_String},
    (   get_dict('@metadata', Doc, MD0), is_dict(MD0)
    ->  MD = MD0.put(embedding, Embedding)
    ;   MD = _{embedding:Embedding}),
    Doc2 = Doc.put('@metadata', MD).
maybe_inject_embedding(_Args, true, Doc, Doc, [], [Report]) :-
    Report = _{class:Doc.get('@id', "<unnamed>"),
               reason:"no string properties found — nothing to embed; pass indexing:{\"query\":…} to embed a custom projection"}.

class_string_properties(Doc, Props) :-
    dict_pairs(Doc, _, Pairs),
    include([K-V]>>(sub_atom(K, 0, 1, _, First), First \= '@',
                  string_prop_type(V)),
            Pairs, Selected),
    maplist([K-_V, K]>>true, Selected, Props),
    Props \= [].

string_prop_type(V) :-
    atom_string('xsd:string', V), !.
string_prop_type(V) :-
    is_dict(V),
    get_dict('@class', V, C),
    atom_string('xsd:string', C).

% ==========================================================================
% git tool — version control for data
% ==========================================================================

git_tool(Auth, System_DB, Args, _Transport, Result) :-
    get_dict(action, Args, Action),
    !,
    required_arg(Args, resource, Path, git),
    git_action(Auth, System_DB, Action, Path, Args, Result).
git_tool(_Auth, _System_DB, _Args, _Transport, _Result) :-
    throw(mcp_error(-32602, 'Invalid params: "action" required',
                    'git takes {"action":"log|list_branches|create_branch|delete_branch|merge|squash|reset|rebase","resource":"org/db[/local/branch/name]", …}')).

git_action(Auth, System_DB, "log", Path, Args, Result) :-
    !,
    Options = _{count:Args.get(count, -1), start:Args.get(start, 0)},
    api_log:api_log(System_DB, Auth, Path, Log, Options),
    length(Log, N),
    format(string(Text), "~d commits", [N]),
    Result = _{'_text':Text, log:Log}.

git_action(Auth, System_DB, "list_branches", Path, _Args, Result) :-
    !,
    do_or_die(
        (   resolve_absolute_string_descriptor(Path, Desc),
            resolve_relative_string_descriptor(Desc, '_commits', Repo)),
        error(invalid_absolute_path(Path), _)),
    check_descriptor_auth(System_DB, Repo,
                          '@schema':'Action/meta_read_access', Auth),
    open_descriptor(Repo, Repo_Transaction),
    (   setof(Branch, ref_entity:has_branch(Repo_Transaction, Branch),
              Names)
    ->  true
    ;   Names = []),
    maplist(branch_info(Repo_Transaction), Names, Branches),
    Result = _{'_text':"branches", branches:Branches}.

git_action(Auth, System_DB, "create_branch", Path0, Args, Result) :-
    !,
    (   get_dict(branch, Args, Branch),
        \+ sub_string(Path0, _, _, _, "/local/branch/")
    ->  atomic_list_concat([Path0, 'local', 'branch', Branch], '/', Path)
    ;   Path = Path0),
    % No source means "branch off main", matching `git branch <name>` —
    % an empty branch is a silent schema-less trap, so it needs
    % empty:true explicitly.
    (   get_dict(source, Args, Source)
    ->  Origin = branch(Source)
    ;   get_dict(empty, Args, true)
    ->  Origin = empty(_{},_{})
    ;   re_matchsub('^([^/]+/[^/]+)/local/branch/', Path, Sub, [])
    ->  Origin = branch(Sub.1)
    ;   Origin = empty(_{},_{})),
    db_branch:branch_create(System_DB, Auth, Path, Origin, _Branch_Uri),
    (   catch(mcp_read_transaction(Auth, System_DB, Path, instance, _{},
                                   _T, DV), _, fail)
    ->  Result = _{'_text':"branch created", branch:Path,
                   data_version:DV}
    ;   Result = _{'_text':"branch created", branch:Path}).

git_action(Auth, System_DB, "delete_branch", Path, Args, Result) :-
    !,
    branch_name_from_path(Path, Name),
    require_confirm(Args, Name),
    db_branch:branch_delete(System_DB, Auth, Path),
    Result = _{'_text':"branch deleted", branch:Path}.

git_action(Auth, System_DB, "merge", Path, Args, Result) :-
    !,
    git_ref_arg(Args, Source),
    write_commit_info(Args, _Author, _Message, Commit_Info),
    git_merge(Auth, System_DB, Path, Source, Commit_Info, Result).

git_action(Auth, System_DB, "squash", Path, Args, Result) :-
    !,
    write_commit_info(Args, _Author, _Message, Commit_Info),
    % Squash produces an unattached commit — moving the branch to it
    % rewrites history, so the same confirm gate as reset applies.
    branch_name_from_path(Path, Name),
    require_confirm(Args, 'rewrite history of ~w', Name),
    (   api_squash:api_squash(System_DB, Auth, Path, Commit_Info,
                              Commit, Old_Commit)
    ->  api_reset:api_reset(System_DB, Auth, Path, Commit),
        mcp_read_transaction(Auth, System_DB, Path, instance, _{},
                             _T, Data_Version),
        Result = _{'_text':"squashed",
                   commit:Commit, old_commit:Old_Commit,
                   data_version:Data_Version}
    ;   Result = _{'_text':"nothing to squash", empty_commit:true}).

git_action(Auth, System_DB, "reset", Path, Args, Result) :-
    !,
    git_ref_arg(Args, Ref0),
    (   sub_string(Ref0, _, _, _, "/")     % already a path — leave it
    ->  Ref = Ref0
    ;   sub_string(Path, B, _, _, "/local")  % branch path → its db
    ->  sub_string(Path, 0, B, _, DB),
        atomic_list_concat([DB, "local/commit", Ref0], '/', Ref)
    ;   atomic_list_concat([Path, "local/commit", Ref0], '/', Ref)),
    branch_name_from_path(Path, Name),
    require_confirm(Args, 'rewrite history of ~w', Name),
    api_reset:api_reset(System_DB, Auth, Path, Ref),
    mcp_read_transaction(Auth, System_DB, Path, instance, _{},
                         _T, Data_Version),
    Result = _{'_text':"reset committed", resource:Path, to:Ref,
               data_version:Data_Version}.

git_action(Auth, System_DB, "rebase", Path, Args, Result) :-
    !,
    git_ref_arg(Args, Source),
    write_commit_info(Args, Author, _Message, _CI),
    (   get_dict(strategy_map, Args, SM0)
    ->  (   is_dict(SM0)
        ->  dict_pairs(SM0, _, Strategy_Map)
        ;   Strategy_Map = SM0)
    ;   Strategy_Map = []),
    db_rebase:rebase_on_branch(System_DB, Auth, Path, Source, Author,
                               Strategy_Map, _Common_Commit, _History, Reports),
    mcp_read_transaction(Auth, System_DB, Path, instance, _{},
                         _T, Data_Version),
    Result = _{'_text':"rebase committed", reports:Reports,
               data_version:Data_Version}.

git_action(_Auth, _System_DB, "clone", _Path, _Args, _Result) :-
    !,
    throw(mcp_tool_error(
              unsupported_action(git, clone),
              'git.clone is not implemented yet — it needs remote fetch machinery. Use admin action `export`/`import`, or the REST /api/clone endpoint.')).

git_action(_Auth, _System_DB, Action, _Path, _Args, _Result) :-
    format(string(Hint),
           'git action "~w" unknown. Actions: log, list_branches, create_branch, delete_branch, merge, squash, reset, rebase. Clone is deferred.', [Action]),
    throw(mcp_tool_error(unsupported_action(git, Action), Hint)).

%% git_ref_arg(+Args, -Ref) is det.
%
%  The other-end ref for merge/rebase/reset. Agents reach for `source`,
%  `to` and `onto` interchangeably — all three name the same thing.
git_ref_arg(Args, Ref) :-
    (   member(K, [source, to, onto]),
        get_dict(K, Args, Ref)
    ->  true
    ;   throw(mcp_tool_error(
                  missing_argument(git, source),
                  'git merge/rebase/reset take the other-end ref as "source" (also accepted: "to", "onto") — e.g. {"action":"merge","resource":"org/db","source":"org/db/local/branch/feature"}'))).


branch_info(Repo_Transaction, Name, _{name:Name, head:Head}) :-
    atom_string(Name_Atom, Name),
    (   ref_entity:branch_head_commit(Repo_Transaction, Name_Atom, Uri),
        ref_entity:commit_id_uri(Repo_Transaction, Id, Uri)
    ->  atom_string(Id_Atom, Id),
        format(atom(Head), 'commit:~w', [Id_Atom])
    ;   Head = null).

%% branch_name_from_path(+Path, -Name) is det.
%% compress_ids(+Ids0, +Prefixes, -Ids) is det.
%
%  Expanded document IRIs (terminusdb:///data/…) come back compressed
%  (Class/id) so ids are reusable across calls without trimming.
compress_ids(Ids0, Prefixes, Ids) :-
    findall(Id,
            (   member(I0, Ids0),
                atom_string(IA, I0),
                compress_dict_uri(IA, Prefixes, IC),
                atom_string(IC, Id)),
            Ids).

branch_name_from_path(Path, Name) :-
    (   re_matchsub('branch/([^/]+)$', Path, Sub, [])
    ->  Name = Sub.1
    ;   Name = Path).

%% require_confirm(+Args, +Name) is det.
%% require_confirm(+Args, +Template, +Name) is det.
%
%  Irreversible actions demand a verbatim confirm string: deletes use
%  "permanently delete <name>", history rewrites "rewrite history of <name>".
require_confirm(Args, Name) :-
    require_confirm(Args, 'permanently delete ~w', Name).
require_confirm(Args, Template, Name) :-
    format(string(Expected), Template, [Name]),
    (   get_dict(confirm, Args, Expected)
    ->  true
    ;   format(string(Hint),
               'This is irreversible. Pass confirm:"~w" verbatim to proceed.', [Expected]),
        throw(mcp_tool_error(confirmation_required(Expected), Hint))).

%% git_merge(+Auth, +System_DB, +Path, +Source, +Commit_Info, -Result) is det.
%
%  Merge Source (branch or commit path) into the branch at Path. The
%  true common ancestor is computed — never caller-supplied — so the
%  three-way apply sees real conflicts instead of silently discarding
%  either side.
git_merge(Auth, System_DB, Path, Source, Commit_Info, Result) :-
    resolve_descriptor_auth(write, System_DB, Auth, Path, instance,
                            Branch_Desc),
    do_or_die(
        branch_descriptor{repository_descriptor: Our_Repo} :< Branch_Desc,
        error(not_a_branch_descriptor(Branch_Desc), _)),
    create_context(Our_Repo, Our_Ctx),
    descriptor_commit_id_uri(Our_Ctx, Branch_Desc, Our_Id, _Our_Uri),

    do_or_die(
        resolve_absolute_string_descriptor(Source, Source_Desc),
        error(invalid_absolute_path(Source), _)),
    do_or_die(
        Source_Desc.repository_descriptor = Our_Repo,
        error(mcp_cross_repository_merge(Source), _)),
    descriptor_commit_id_uri(Our_Ctx, Source_Desc, Their_Id, _Their_Uri),

    ref_entity:most_recent_common_ancestor(
        Our_Ctx, Our_Ctx, Our_Id, Their_Id,
        Common_Option, _Our_Path, _Their_Path),
    (   Common_Option = some(Common_Id)
    ->  true
    ;   throw(mcp_tool_error(
                  no_common_ancestor(Path, Source),
                  'histories share no commit — git action `reset` to a known commit, or admin `export`/`import` instead'))),
    format(atom(Before_A), 'commit:~w', [Common_Id]),
    atom_string(Before, Before_A),
    format(atom(After_A), 'commit:~w', [Their_Id]),
    atom_string(After, After_A),
    catch(
        api_patch:api_apply_squash_commit(
            System_DB, Auth, Path, Commit_Info,
            Before, After, [conflict_incoming(true),
                            % both sides resolving to the same value is
                            % convergence, not a conflict
                            match_final_state(true)]),
        error(apply_squash_witnesses(Witnesses)),
        (   format(string(Merge_Hint),
                   'changes to the same fields conflict — decide the correct value per document (witnesses show @expected ancestor, @found yours, @incoming theirs), then `write` the resolved documents on ~w: that commit completes the merge manually (retrying `merge` re-reports the same conflict)',
                   [Path]),
            throw(mcp_tool_error(
                      merge_conflict(Witnesses), Merge_Hint)))),
    mcp_read_transaction(Auth, System_DB, Path, instance, _{},
                         _T, Data_Version),
    Result = _{'_text':"merge committed",
               merged_from:Source,
               common_ancestor:Common_Id,
               data_version:Data_Version}.

% ==========================================================================
% plugins tool — extension namespace
% ==========================================================================

%% plugins_tool(+Auth, +System_DB, +Args, +Transport, -Result) is det.
plugins_tool(Auth, System_DB, Args, _Transport, Result) :-
    required_arg(Args, action, Action, plugins),
    (   Action = "list"
    ->  findall(_{name:N, description:D, inputSchema:S},
                mcp_plugin_tool(N, D, S, _G),
                Tools),
        (   Tools = []
        ->  Result = _{'_text':"no plugin tools registered", tools:[],
                       hint:"Plugins self-register via mcp:register_plugin_tool(Name, Description, InputSchema, Goal)"}
        ;   Result = _{'_text':"plugin tools", tools:Tools})
    ;   Action = "call"
    ->  required_arg(Args, name, Name, plugins),
        Arguments = Args.get(arguments, _{}),
        (   atom_string(N_Atom, Name),
            mcp_plugin_tool(N_Atom, _, _, Goal)
        ->  call(Goal, Auth, System_DB, Arguments, Result)
        ;   findall(N2, mcp_plugin_tool(N2, _, _, _), Names),
            format(string(Hint),
                   'No plugin tool "~w" — registered plugin tools: ~w. Builtins (schema, query, write, git, admin, plugins) go through tools/call directly.', [Name, Names]),
            throw(mcp_tool_error(unknown_plugin_tool(Name), Hint)))
    ;   format(string(Hint),
               'plugins action "~w" unknown. Actions: list, call.', [Action]),
        throw(mcp_tool_error(unsupported_action(plugins, Action), Hint))).

%% mcp_handler(+Method, +Request, +System_DB, +Auth) is det.
mcp_handler(post, Request, System_DB, Auth) :-
    api_report_errors(
        mcp,
        Request,
        (   (   memberchk(payload(Payload), Request)
            ->  true
            ;   Payload = none),
            (   dispatch(Auth, System_DB, Payload, http, Response),
                Response \= none
            ->  write_cors_headers(Request),
                reply_json(Response, [json_object(dict), width(0)])
            ;   write_cors_headers(Request),
                format('Status: 202~n~n', [])))).
mcp_handler(get, Request, _System_DB, _Auth) :-
    method_not_allowed(Request).
mcp_handler(delete, Request, _System_DB, _Auth) :-
    method_not_allowed(Request).

method_not_allowed(Request) :-
    write_cors_headers(Request),
    reply_json(_{error:"Method Not Allowed",
                 hint:"The MCP endpoint is stateless streamable-HTTP: POST JSON-RPC 2.0 requests only. No SSE sessions in this version."},
               [status(405), width(0)]).

%% llms_txt_handler(+Method, +Request, +System_DB, +Auth) is det.
%
%  GET /llms.txt — the same onboarding document the `admin` tool's
%  `docs` action serves. Public: it describes how to talk to the
%  server, never what's in it.
llms_txt_handler(get, Request, _System_DB, _Auth) :-
    !,
    config:builtin_plugin_path(Builtin_Path),
    atomic_list_concat([Builtin_Path, 'assets/llms.txt'], '/', File),
    read_file_to_string(File, Text, []),
    write_cors_headers(Request),
    format('Content-Type: text/plain; charset=utf-8~n~n'),
    format('~s', [Text]).
llms_txt_handler(_Method, Request, _System_DB, _Auth) :-
    method_not_allowed(Request).

:- plugin_api:register_route(
       '/llms.txt',
       plugin_api:cors_handler(Method, mcp:llms_txt_handler,
                               [skip_authentication(true)]),
       [method(Method), methods([options,get])]).

:- if(current_prolog_flag(terminusdb_enterprise, true)).
:- plugin_api:register_route(
       api(mcp),
       plugin_api:cors_handler(Method, mcp:mcp_handler,
                               [www_authenticate('Bearer realm="terminusdb"')]),
       [method(Method), methods([options,get,post,delete])]).
:- endif.

% ==========================================================================
% stdio adapter — `terminusdb mcp`
% ==========================================================================

%% run_stdio(+Auth) is det.
%
%  Newline-delimited JSON-RPC on stdin/stdout against the local store.
%  Diagnostics go to stderr — stdout is protocol-clean.
run_stdio(Auth) :-
    (   stream_property(user_input, tty(true))
    ->  format(user_error,
               "MCP stdio server — connect a client, e.g. `claude mcp add terminusdb -- terminusdb mcp`. Exiting.~n", []),
        halt(0)
    ;   true),
    init_indexer,
    set_stream(user_output, buffer(line)),
    stdio_loop(Auth).

%% init_indexer is det.
%
%  Reuses the plugins' server-startup hooks to bring up the configured
%  indexer backend (embedded vectorlink engine, or remote endpoint
%  registration). Best-effort: a failure warns on stderr and the MCP
%  session continues without indexing — search then reports the gap.
init_indexer :-
    forall(
        catch(plugins:post_server_startup_hook(0),
              Error,
              format(user_error,
                     "Warning: indexer initialization failed (~w) — continuing without indexing.~n",
                     [Error])),
        true).

%% stdio_loop(+Auth) is det.
%
%  System_DB is opened per request: a descriptor opened at startup sees a
%  stale system graph, so resources created mid-session (create_database,
%  branches) would resolve as unknown_database forever.
stdio_loop(Auth) :-
    read_line_to_string(user_input, Line),
    (   Line == end_of_file
    ->  true
    ;   (   catch((atom_string(Atom, Line),
                   atom_json_dict(Atom, Request, [])),
                  _, fail)
        ->  open_descriptor(system_descriptor{}, System_DB),
            dispatch(Auth, System_DB, Request, stdio, Response)
        ;   error_response(null, -32700,
                           'Parse error: line is not valid JSON',
                           'Send one JSON-RPC 2.0 request per line',
                           Response)),
        (   Response = none
        ->  true
        ;   (   catch(with_output_to(
                          string(Line_Out),
                          (   json_write_dict(current_output, Response,
                                              [width(0)]),
                              nl)),
                      _, fail)
            ->  format(user_output, "~s", [Line_Out])
            ;   % a dict that can't serialize must not leave a truncated
                % line on the protocol stream — emit a clean error instead
                error_response(get_dict(id, Request, null), -32603,
                               'Internal error',
                               'The response could not be serialized — report a bug',
                               Fallback),
                json_write_dict(user_output, Fallback, [width(0)]),
                nl(user_output)),
            flush_output(user_output)),
        stdio_loop(Auth)).

%% sample_plugin_tool(+Auth, +System_DB, +Args, -Result) is det.
%
%  Outside the test block on purpose: plunit renames predicates defined
%  between begin_tests/end_tests, so a registered plugin goal must live
%  here to remain callable as mcp:sample_plugin_tool/4.
sample_plugin_tool(_Auth, _System_DB, Args,
                   _{'_text':"sample plugin", echo:Args.get(echo, none)}).

% ==========================================================================
% Tests
% ==========================================================================

:- begin_tests(mcp, []).

superuser(Auth) :- super_user_authority(Auth).

req(Method, Params, Id, _{jsonrpc:"2.0", id:Id, method:Method, params:Params}).

test(initialize_returns_server_info) :-
    superuser(Auth),
    req("initialize", _{protocolVersion:"2025-03-26"}, 1, Req),
    dispatch(Auth, _, Req, http, _{jsonrpc:"2.0", id:1, result:R}),
    R.serverInfo.name == "terminusdb",
    R.protocolVersion == "2025-03-26",
    is_dict(R.capabilities.tools).

test(initialize_negotiates_unknown_version) :-
    superuser(Auth),
    req("initialize", _{protocolVersion:"1999-01-01"}, 2, Req),
    dispatch(Auth, _, Req, http, R),
    R.result.protocolVersion == "2025-06-18".

test(ping_answers_empty) :-
    superuser(Auth),
    req("ping", _{}, 3, Req),
    dispatch(Auth, _, Req, http, R),
    R.result =@= _{}.

test(tools_list_shape) :-
    superuser(Auth),
    req("tools/list", _{}, 4, Req),
    dispatch(Auth, _, Req, http, R),
    Tools = R.result.tools,
    is_list(Tools),
    once(member(T, Tools)),
    get_dict(name, T, Name), string(Name),
    get_dict(description, T, _),
    get_dict(inputSchema, T, Schema),
    get_dict(type, Schema, "object").

test(unknown_method_is_jsonrpc_error_with_hint) :-
    superuser(Auth),
    req("frobnicate", _{}, 5, Req),
    dispatch(Auth, _, Req, http, R),
    R.error.code =:= -32601,
    Hint = R.error.data.hint,
    once(sub_string(Hint, _, _, _, "tools/list")).

test(missing_method_is_invalid_request) :-
    superuser(Auth),
    dispatch(Auth, _, _{jsonrpc:"2.0", id:6}, http, R),
    R.error.code =:= -32600.

test(bad_jsonrpc_version_is_invalid_request) :-
    superuser(Auth),
    dispatch(Auth, _, _{jsonrpc:"1.0", id:7, method:"ping"}, http, R),
    R.error.code =:= -32600.

test(notification_gets_no_response) :-
    superuser(Auth),
    dispatch(Auth, _, _{jsonrpc:"2.0", method:"notifications/initialized"}, http, none).

test(batch_answers_each) :-
    superuser(Auth),
    req("ping", _{}, 8, Req1),
    req("ping", _{}, 9, Req2),
    dispatch(Auth, _, [Req1, Req2], http, [R1, R2]),
    R1.id =:= 8, R2.id =:= 9.

test(batch_of_notifications_is_none) :-
    superuser(Auth),
    dispatch(Auth, _, [_{jsonrpc:"2.0", method:"notifications/initialized"},
                       _{jsonrpc:"2.0", method:"notifications/cancelled"}],
             http, none).

test(tools_call_unknown_tool_is_iserror_with_hint) :-
    superuser(Auth),
    req("tools/call", _{name:"nope", arguments:_{}}, 10, Req),
    dispatch(Auth, _, Req, http, R),
    get_dict(isError, R.result, true),
    Hint = R.result.structuredContent.hint,
    once(sub_string(Hint, _, _, _, "tools/list")).

test(tools_call_missing_params_is_protocol_error) :-
    superuser(Auth),
    dispatch(Auth, _, _{jsonrpc:"2.0", id:11, method:"tools/call"}, http, R),
    R.error.code =:= -32602.

test(dispatch_non_text_method_is_error_not_death) :-
    % a dict-valued method throws type_error in is_notification — outside
    % call_tool and not mcp_error. Over stdio an escaping throw kills the
    % server process permanently; it must become an error response instead
    superuser(Auth),
    dispatch(Auth, _, _{jsonrpc:"2.0", id:9, method:_{a:1}}, http, R),
    R.error.code =:= -32600.

test(dispatch_notification_with_unknown_method_is_swallowed) :-
    % a request with no id takes the notification path, which called
    % handle_method with no catch — an unknown method threw mcp_error and
    % killed the stdio process. Notifications get no response: swallow it
    superuser(Auth),
    dispatch(Auth, _, _{jsonrpc:"2.0", method:"bogus"}, http, R),
    R = none.

test(admin_info_returns_payload) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System_DB),
    req("tools/call", _{name:"admin", arguments:_{action:"info"}}, 12, Req),
    dispatch(Auth, System_DB, Req, http, R),
    \+ get_dict(isError, R.result, true),
    is_dict(R.result.structuredContent.info).

test(admin_unknown_action_is_iserror_with_hint) :-
    superuser(Auth),
    req("tools/call", _{name:"admin", arguments:_{action:"teleport"}}, 13, Req),
    dispatch(Auth, _, Req, http, R),
    get_dict(isError, R.result, true),
    Hint = R.result.structuredContent.hint,
    once(sub_string(Hint, _, _, _, "info")).

% -- schema tool ---------------------------------------------------------

tool_call(Name, Args, Id, Req) :-
    req("tools/call", _{name:Name, arguments:Args}, Id, Req).

test(schema_summary_counts_and_classes,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpschema))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("schema", _{action:"summary", resource:"admin/mcpschema"}, 20, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    once(member('Person', Content.classes)),
    once(member('City', Content.classes)),
    Content.documents =:= 0,
    once(sub_string(Content.data_version, 0, _, _, "branch:")).

test(schema_frame_jsonschema_for_class,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpschema2))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("schema",
              _{action:"frame", resource:"admin/mcpschema2",
                class:"Person", format:"jsonschema"},
              21, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Schema = R.result.structuredContent.json_schema,
    get_dict(type, Schema, "object"),
    get_dict(properties, Schema, Props),
    get_dict(address, Props, _{type:"string"}).

test(schema_vocab_returns_prefixes,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpschema3))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("schema", _{action:"vocab", resource:"admin/mcpschema3"},
              22, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict('@schema', R.result.structuredContent.vocab,
             "http://example.com/schema/worldOntology#").

test(schema_datatypes_lists_vocabulary) :-
    superuser(Auth),
    tool_call("schema", _{action:"datatypes"}, 25, Req),
    dispatch(Auth, _, Req, http, R),
    \+ get_dict(isError, R.result, true),
    get_dict(result, R, Res),
    get_dict(structuredContent, Res, Content),
    get_dict(datatypes, Content, Datatypes),
    get_dict(schema_types, Content, SchemaTypes),
    get_dict(families, Content, Families),
    once(member('xsd:string', Datatypes)),
    once(member('xdd:coordinate', Datatypes)),
    once(member("Class", SchemaTypes)),
    once(member("Optional", Families)).

test(schema_unknown_action_is_iserror) :-
    superuser(Auth),
    tool_call("schema", _{action:"frob", resource:"admin/x"}, 23, Req),
    dispatch(Auth, _, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "summary")).

test(schema_frame_unknown_class_is_iserror,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpghost))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("schema",
              _{action:"frame", resource:"admin/mcpghost", class:"Ghost"},
              26, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "Ghost")).

% -- query tool ----------------------------------------------------------

test(query_document_by_id,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpdoc))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    open_string('{"@type":"Person","name":"Ada","address":"1 Loop Rd"}',
                Stream),
    api_insert_documents(System, Auth, "admin/mcpdoc", Stream,
                         no_data_version, _DV, _MD, Ids,
                         [graph_type(instance), author("a"), message("m")]),
    [Id] = Ids,
    tool_call("query",
              _{action:"document", path:"admin/mcpdoc", id:Id},
              24, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    get_dict(name, R.result.structuredContent.document, "Ada").

test(query_document_id_only_by_type,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpdoc2))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    open_string('{"@type":"City","name":"Metropolis"}', Stream),
    api_insert_documents(System, Auth, "admin/mcpdoc2", Stream,
                         no_data_version, _DV, _MD, _Ids,
                         [graph_type(instance), author("a"), message("m")]),
    tool_call("query",
              _{action:"document", path:"admin/mcpdoc2",
                type:"City", id_only:true},
              25, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Ids = R.result.structuredContent.ids,
    length(Ids, 1),
    once(member(I, Ids)), once(sub_string(I, _, _, _, "City/")).

test(query_document_match_query_selects,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpqmatch))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    open_string('[{"@type":"Person","name":"Grace","address":"a"},
                 {"@type":"Person","name":"Hopper","address":"b"}]',
                Stream),
    api_insert_documents(System, Auth, "admin/mcpqmatch", Stream,
                         no_data_version, _DV, _MD, _Ids,
                         [graph_type(instance), author("a"), message("m")]),
    tool_call("query",
              _{action:"document", path:"admin/mcpqmatch",
                type:"Person", query:_{name:"Grace"}},
              42, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    get_dict(count, Content, 1),
    [Doc] = Content.documents,
    get_dict(name, Doc, "Grace").

test(query_document_fields_projection,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpfields))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    open_string('{"@type":"Person","name":"Proj","address":"p"}', Stream),
    api_insert_documents(System, Auth, "admin/mcpfields", Stream,
                         no_data_version, _DV, _MD, _Ids,
                         [graph_type(instance), author("a"), message("m")]),
    tool_call("query",
              _{action:"document", path:"admin/mcpfields",
                type:"Person", fields:["name"]},
              43, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    [Doc] = R.result.structuredContent.documents,
    get_dict(name, Doc, "Proj"),
    \+ get_dict(address, Doc, _).

test(query_document_id_only_compressed_ids,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpidonly))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    open_string('{"@type":"Person","name":"Cmp","address":"c"}', Stream),
    api_insert_documents(System, Auth, "admin/mcpidonly", Stream,
                         no_data_version, _DV, _MD, _Ids,
                         [graph_type(instance), author("a"), message("m")]),
    tool_call("query",
              _{action:"document", path:"admin/mcpidonly",
                type:"Person", id_only:true},
              44, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    [Id] = R.result.structuredContent.ids,
    once(sub_string(Id, _, _, _, "Person/")),
    \+ sub_string(Id, _, _, _, "example.com").

test(query_document_format_embedding_renders_template,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpfmtemb))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    % a class doc carrying @metadata.embedding renders through the
    % GraphQL+Handlebars pipeline with no indexer backend configured
    open_string('{"@type":"Class","@id":"Task","@key":{"@type":"Lexical","@fields":["name"]},"name":"xsd:string","status":"xsd:string","@metadata":{"embedding":{"query":"query($id: ID){ Task(id: $id) { name status } }","template":"| {{name}} | {{status}} |"}}}',
                Schema_Stream),
    api_insert_documents(System, Auth, "admin/mcpfmtemb", Schema_Stream,
                         no_data_version, _DV0, _MD0, _SIds,
                         [graph_type(schema), author("a"), message("m")]),
    open_string('{"@type":"Task","name":"A1","status":"PASS"}', Stream),
    api_insert_documents(System, Auth, "admin/mcpfmtemb", Stream,
                         no_data_version, _DV, _MD, _Ids,
                         [graph_type(instance), author("a"), message("m")]),
    tool_call("query",
              _{action:"document", path:"admin/mcpfmtemb",
                type:"Task", format:"embedding"},
              45, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    get_dict(format, Content, "embedding"),
    [Pair] = Content.documents,
    once(sub_string(Pair.id, _, _, _, "Task/A1")),
    once(sub_string(Pair.text, _, _, _, "| A1 | PASS |")),
    once(sub_string(Content.page, _, _, _, "| A1 | PASS |")).

test(query_document_format_embedding_by_id,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpfmtid))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    open_string('{"@type":"Class","@id":"Task","@key":{"@type":"Lexical","@fields":["name"]},"name":"xsd:string","@metadata":{"embedding":{"query":"query($id: ID){ Task(id: $id) { name } }","template":"## {{name}}"}}}',
                Schema_Stream),
    api_insert_documents(System, Auth, "admin/mcpfmtid", Schema_Stream,
                         no_data_version, _DV0, _MD0, _SIds,
                         [graph_type(schema), author("a"), message("m")]),
    open_string('{"@type":"Task","name":"solo"}', Stream),
    api_insert_documents(System, Auth, "admin/mcpfmtid", Stream,
                         no_data_version, _DV, _MD, [Id],
                         [graph_type(instance), author("a"), message("m")]),
    tool_call("query",
              _{action:"document", path:"admin/mcpfmtid",
                id:Id, format:"embedding"},
              51, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    [Pair] = Content.documents,
    once(sub_string(Pair.text, _, _, _, "## solo")),
    once(sub_string(Content.page, _, _, _, "## solo")).

test(query_document_format_markdown_renders,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpfmtmd))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    open_string('{"@type":"Person","name":"Ada","address":"1 Loop Rd"}',
                Stream),
    api_insert_documents(System, Auth, "admin/mcpfmtmd", Stream,
                         no_data_version, _DV, _MD, _Ids,
                         [graph_type(instance), author("a"), message("m")]),
    tool_call("query",
              _{action:"document", path:"admin/mcpfmtmd",
                type:"Person", format:"markdown"},
              52, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    get_dict(format, Content, "markdown"),
    [Pair] = Content.documents,
    once(sub_string(Pair.text, _, _, _, "Ada")),
    once(sub_string(Content.page, _, _, _, "Ada")).

test(query_document_format_rejects_id_only,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpfmtbad))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"document", path:"admin/mcpfmtbad",
                type:"Person", id_only:true, format:"embedding"},
              53, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "id_only")).

test(query_document_format_rejects_fields,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpfmtbad2))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"document", path:"admin/mcpfmtbad2",
                type:"Person", fields:["name"], format:"markdown"},
              54, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "fields")).

test(query_document_format_rejects_unknown_format,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpfmtbad3))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"document", path:"admin/mcpfmtbad3",
                type:"Person", format:"yaml"},
              55, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "embedding")).

test(query_woql_read_query_works,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwoql))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    Query = _{'@type':"IsA",
              element:_{'@type':"NodeValue", variable:"X"},
              type:_{'@type':"NodeValue", node:"City"}},
    tool_call("query",
              _{action:"woql", path:"admin/mcpwoql", query:Query},
              26, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    is_dict(R.result.structuredContent.result).

test(query_woql_rejects_write_op_with_hint,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwoql2))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    Query = _{'@type':"InsertDocument",
              identifier:_{'@type':"NodeValue", node:"Person/x"},
              document:_{'@type':"Person", name:"x", address:"y"}},
    tool_call("query",
              _{action:"woql", path:"admin/mcpwoql2", query:Query},
              27, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "write")).

test(query_woql_rejects_remote_ingest,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwoqlr))),
      cleanup(teardown_temp_store(State))]) :-
    % CWE-918: Get with source{url} makes the server fetch an arbitrary
    % URI (copy_remote → http_open, no allowlist) — block the ingest op
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    Query = _{'@type':"Get",
              columns:[_{'@type':"Column", indicator:_{name:"x"},
                         variable:"X"}],
              resource:_{'@type':"QueryResource",
                         source:_{url:"http://169.254.169.254/latest"},
                         format:"csv"}},
    tool_call("query",
              _{action:"woql", path:"admin/mcpwoqlr", query:Query},
              29, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "ingest")).

test(write_woql_rejects_remote_ingest,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwoqlw))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    Query = _{'@type':"Get",
              columns:[_{'@type':"Column", indicator:_{name:"x"},
                         variable:"X"}],
              resource:_{'@type':"QueryResource",
                         source:_{url:"http://169.254.169.254/latest"},
                         format:"csv"}},
    tool_call("write",
              _{action:"woql", resource:"admin/mcpwoqlw",
                commit_info:_{author:"a", message:"m"},
                query:Query},
              30, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "ingest")).

test(query_woql_delta_read_ops_are_not_writes,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwoqld))),
      cleanup(teardown_temp_store(State))]) :-
    % AddedTriple compiles to xrdf_added — a commit-delta READ, so query
    % must not reject it as a write (it may return empty bindings, but
    % never the read-only error)
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    Query = _{'@type':"AddedTriple",
              subject:_{'@type':"NodeValue", variable:"S"},
              predicate:_{'@type':"NodeValue", variable:"P"},
              object:_{'@type':"NodeValue", variable:"O"}},
    tool_call("query",
              _{action:"woql", path:"admin/mcpwoqld", query:Query},
              28, Req),
    dispatch(Auth, System, Req, http, R),
    (   get_dict(isError, R.result, true)
    ->  \+ sub_string(R.result.structuredContent.error, _, _, _,
                      "read_only_woql")
    ;   true).

test(query_check_valid_insert_is_data_not_error,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpcheck))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"check", path:"admin/mcpcheck", verb:"insert",
                documents:_{'@type':"City", name:"Checkville"}},
              28, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    get_dict('api:valid', R.result.structuredContent, true),
    % ids the dry run would assign are compressed like everywhere else
    get_dict('api:extra', R.result.structuredContent, Extra),
    get_dict(ids, Extra, [Extra_Id|_]),
    once(sub_string(Extra_Id, 0, _, _, "City/")).

test(query_check_invalid_insert_reports_witnesses,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpcheck2))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"check", path:"admin/mcpcheck2", verb:"insert",
                documents:_{'@type':"City"}},
              29, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    get_dict('api:valid', Content, false),
    get_dict('api:witnesses', Content, [_|_]),
    % embedding readiness is reported on invalid verdicts too — a caller
    % reading api:embedding.will_embed must not crash here
    get_dict('api:embedding', Content, Embedding),
    get_dict('api:will_embed', Embedding, _).

test(query_check_schema_error_keeps_embedding_block,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpcheck4))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"check", path:"admin/mcpcheck4", verb:"insert",
                documents:_{'@type':"City", name:_{nested:"object"}}},
              97, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    get_dict('api:valid', Content, false),
    get_dict('api:embedding', Content, Embedding),
    get_dict('api:will_embed', Embedding, false).

test(query_check_delete_missing_targets_hints,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpcheck3))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"check", path:"admin/mcpcheck3", verb:"delete"},
              30, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "nuke")).

test(query_unimplemented_action_is_iserror) :-
    superuser(Auth),
    tool_call("query", _{action:"frobnicate", path:"admin/x"}, 31, Req),
    dispatch(Auth, _, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "document")).

% -- write tool ----------------------------------------------------------

test(write_insert_requires_commit_info,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite",
                documents:_{'@type':"City", name:"NoCommit"}},
              40, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "commit_info")),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "check")).

test(write_insert_commits_and_returns_ids,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite2))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite2",
                commit_info:_{author:"a", message:"m"},
                documents:_{'@type':"City", name:"Written"}},
              41, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    once(member(Id, Content.ids)),
    once(sub_string(Id, _, _, _, "City/")),
    once(sub_string(Content.data_version, 0, _, _, "branch:")),
    % and the document is really there
    mcp_read_transaction(Auth, System, "admin/mcpwrite2", instance, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    api_document:api_get_document(instance, Transaction, Id, Config, Doc),
    get_dict(name, Doc, "Written").

test(write_insert_non_ascii_documents_all_commit,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite_u8))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite_u8",
                commit_info:_{author:"a", message:"m"},
                documents:[_{'@type':"City", name:"Lisbon"},
                           _{'@type':"City", name:"Umeå"}]},
              41, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    length(Content.ids, 2),
    mcp_read_transaction(Auth, System, "admin/mcpwrite_u8", instance, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    findall(Doc,
            api_document:api_get_documents_by_type(
                Transaction, instance, 'City', Config, Doc),
            Docs),
    length(Docs, 2).

test(write_update_replaces_document,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite3))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite3",
                commit_info:_{author:"a", message:"m"},
                documents:_{'@type':"City", name:"OldName"}},
              42, Req),
    dispatch(Auth, System, Req, http, R1),
    [Id] = R1.result.structuredContent.ids,
    tool_call("write",
              _{action:"update", resource:"admin/mcpwrite3",
                commit_info:_{author:"a", message:"m2"},
                documents:_{'@id':Id, '@type':"City", name:"NewName"}},
              43, Req2),
    dispatch(Auth, System, Req2, http, R2),
    \+ get_dict(isError, R2.result, true),
    mcp_read_transaction(Auth, System, "admin/mcpwrite3", instance, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    api_document:api_get_document(instance, Transaction, Id, Config, Doc),
    get_dict(name, Doc, "NewName").

test(write_patch_returns_compressed_ids,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcppatch))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    tool_call("write",
              _{action:"insert", resource:"admin/mcppatch",
                commit_info:CI,
                documents:_{'@type':"City", name:"Patchton"}},
              98, ReqW),
    dispatch(Auth, System, ReqW, http, RW),
    get_dict(ids, RW.result.structuredContent, [Doc_Id|_]),
    tool_call("write",
              _{action:"patch", resource:"admin/mcppatch",
                commit_info:CI,
                patch:[_{'@id':Doc_Id,
                         name:_{'@op':"SwapValue",
                                '@before':"Patchton",
                                '@after':"Patched"}}]},
              99, ReqP),
    dispatch(Auth, System, ReqP, http, R),
    \+ get_dict(isError, R.result, true),
    get_dict(ids, R.result.structuredContent, [Id|_]),
    once(sub_string(Id, 0, _, _, "City/")),
    \+ sub_string(Id, _, _, _, "terminusdb:///").

test(write_delete_by_id,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite4))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite4",
                commit_info:_{author:"a", message:"m"},
                documents:_{'@type':"City", name:"Doomed"}},
              44, Req),
    dispatch(Auth, System, Req, http, R1),
    [Id] = R1.result.structuredContent.ids,
    tool_call("write",
              _{action:"delete", resource:"admin/mcpwrite4",
                commit_info:_{author:"a", message:"m"},
                id:Id},
              45, Req2),
    dispatch(Auth, System, Req2, http, R2),
    \+ get_dict(isError, R2.result, true),
    mcp_read_transaction(Auth, System, "admin/mcpwrite4", instance, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    \+ catch(api_document:api_get_document(instance, Transaction, Id, Config, _),
             _, fail).

test(write_delete_by_ids_returns_ids,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite4b))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite4b",
                commit_info:_{author:"a", message:"m"},
                documents:_{'@type':"City", name:"Doomed"}},
              44, Req),
    dispatch(Auth, System, Req, http, R1),
    [Id] = R1.result.structuredContent.ids,
    tool_call("write",
              _{action:"delete", resource:"admin/mcpwrite4b",
                commit_info:_{author:"a", message:"m"},
                ids:[Id]},
              45, Req2),
    dispatch(Auth, System, Req2, http, R2),
    \+ get_dict(isError, R2.result, true),
    R2.result.structuredContent.ids == [Id],
    % response must fully serialize — an unbound ids value crashed
    % json_write_dict mid-line and truncated the protocol stream
    with_output_to(string(_),
                   json_write_dict(current_output, R2, [width(0)])).

test(content_text_mirrors_payload,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpmirror))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpmirror",
                commit_info:_{author:"a", message:"m"},
                documents:_{'@type':"City", name:"Mirror"}},
              46, Req),
    dispatch(Auth, System, Req, http, R),
    % text-only clients read content[0].text — it must carry the real
    % values, not just a label
    [C] = R.result.content,
    once(sub_string(C.text, _, _, _, "data_version")),
    once(sub_string(C.text, _, _, _, "City/")).

test(write_woql_insert_commits,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite5))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    Query = _{'@type':"InsertDocument",
              identifier:_{'@type':"NodeValue", node:"City/WoqlVille"},
              document:_{'@type':"Value",
                         dictionary:_{'@type':"DictionaryTemplate",
                                      data:[_{'@type':"FieldValuePair",
                                              field:"@type",
                                              value:_{'@type':"Value",
                                                      data:"City"}},
                                            _{'@type':"FieldValuePair",
                                              field:"name",
                                              value:_{'@type':"Value",
                                                      data:"WoqlVille"}}]}}},
    tool_call("write",
              _{action:"woql", resource:"admin/mcpwrite5",
                commit_info:_{author:"a", message:"m"},
                query:Query},
              46, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.data_version, 0, _, _, "branch:")).

test(write_expected_data_version_conflict_is_iserror,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite6))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite6",
                commit_info:_{author:"a", message:"m"},
                expected_data_version:"branch:0000deadbeef",
                documents:_{'@type':"City", name:"Conflict"}},
              47, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true).

test(write_schema_insert_injects_embedding_when_indexer_enabled,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite7),
             setenv('TERMINUSDB_INDEXER_BACKEND', 'vectorlink'),
             config:clear_indexer_backend_config)),
      cleanup((unsetenv('TERMINUSDB_INDEXER_BACKEND'),
               config:clear_indexer_backend_config,
               teardown_temp_store(State)))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite7",
                graph_type:"schema",
                commit_info:_{author:"a", message:"m"},
                documents:_{'@type':"Class", '@id':"EmbClass",
                            '@key':_{'@type':"Lexical", '@fields':["name"]},
                            name:"xsd:string"}},
              48, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Content = R.result.structuredContent,
    once(member("EmbClass", Content.embedded)),
    % read the schema doc back — it must carry the injected metadata
    mcp_read_transaction(Auth, System, "admin/mcpwrite7", schema, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    api_document:api_get_document(schema, Transaction, "EmbClass",
                                  Config, Doc),
    get_dict('@metadata', Doc, Metadata),
    get_dict(embedding, Metadata, Embedding),
    once(sub_string(Embedding.query, _, _, _, "EmbClass")).

test(write_schema_insert_indexing_false_reports_not_embedded,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite8),
             setenv('TERMINUSDB_INDEXER_BACKEND', 'vectorlink'),
             config:clear_indexer_backend_config)),
      cleanup((unsetenv('TERMINUSDB_INDEXER_BACKEND'),
               config:clear_indexer_backend_config,
               teardown_temp_store(State)))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite8",
                graph_type:"schema", indexing:false,
                commit_info:_{author:"a", message:"m"},
                documents:_{'@type':"Class", '@id':"NoEmb",
                            '@key':_{'@type':"Lexical", '@fields':["name"]},
                            name:"xsd:string"}},
              49, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    once(member(Report, R.result.structuredContent.not_embedded)),
    get_dict(class, Report, "NoEmb").

test(write_schema_insert_backend_none_reports_not_embedded,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite9),
             setenv('TERMINUSDB_INDEXER_BACKEND', 'none'),
             config:clear_indexer_backend_config)),
      cleanup((unsetenv('TERMINUSDB_INDEXER_BACKEND'),
               config:clear_indexer_backend_config,
               teardown_temp_store(State)))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite9",
                graph_type:"schema",
                commit_info:_{author:"a", message:"m"},
                documents:_{'@type':"Class", '@id':"NoBackend",
                            '@key':_{'@type':"Lexical", '@fields':["name"]},
                            name:"xsd:string"}},
              50, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    once(member(Report, R.result.structuredContent.not_embedded)),
    get_dict(class, Report, "NoBackend"),
    once(sub_string(Report.reason, _, _, _, "indexer")),
    mcp_read_transaction(Auth, System, "admin/mcpwrite9", schema, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    api_document:api_get_document(schema, Transaction, "NoBackend",
                                  Config, Doc),
    \+ get_dict('@metadata', Doc, _).

test(write_schema_indexing_override_stored_without_backend,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwrite10),
             setenv('TERMINUSDB_INDEXER_BACKEND', 'none'),
             config:clear_indexer_backend_config)),
      cleanup((unsetenv('TERMINUSDB_INDEXER_BACKEND'),
               config:clear_indexer_backend_config,
               teardown_temp_store(State)))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwrite10",
                graph_type:"schema",
                indexing:_{query:"query($id: ID){ Rendered(id: $id) { name } }",
                           template:"{{name}}"},
                commit_info:_{author:"a", message:"m"},
                documents:_{'@type':"Class", '@id':"Rendered",
                            '@key':_{'@type':"Lexical", '@fields':["name"]},
                            name:"xsd:string"}},
              56, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    % explicit indexing metadata is data — stored even with no indexer,
    % so query action document format:"embedding" can render it
    mcp_read_transaction(Auth, System, "admin/mcpwrite10", schema, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    api_document:api_get_document(schema, Transaction, "Rendered",
                                  Config, Doc),
    get_dict('@metadata', Doc, Metadata),
    get_dict(embedding, Metadata, Embedding),
    get_dict(template, Embedding, "{{name}}").

% -- git tool ------------------------------------------------------------

test(git_log_returns_commits,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgit))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("git",
              _{action:"log", resource:"admin/mcpgit"},
              60, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    Log = R.result.structuredContent.log,
    is_list(Log),
    Log \= [],
    once(member(Entry, Log)),
    get_dict(identifier, Entry, _).

test(git_list_branches,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgit2))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("git",
              _{action:"list_branches", resource:"admin/mcpgit2"},
              61, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    once(member(B, R.result.structuredContent.branches)),
    get_dict(name, B, "main").

test(git_create_branch_then_list,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgit3))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpgit3/local/branch/feature",
                source:"admin/mcpgit3"},
              62, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    tool_call("git",
              _{action:"list_branches", resource:"admin/mcpgit3"},
              63, Req2),
    dispatch(Auth, System, Req2, http, R2),
    once(member(B, R2.result.structuredContent.branches)),
    get_dict(name, B, "feature").

test(git_delete_branch_requires_confirm,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgit4))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpgit4/local/branch/ephemeral",
                source:"admin/mcpgit4"},
              64, Req),
    dispatch(Auth, System, Req, http, _),
    tool_call("git",
              _{action:"delete_branch",
                resource:"admin/mcpgit4/local/branch/ephemeral"},
              65, Req2),
    dispatch(Auth, System, Req2, http, R2),
    get_dict(isError, R2.result, true),
    once(sub_string(R2.result.structuredContent.hint, _, _, _,
                    "permanently delete ephemeral")).

test(git_delete_branch_with_confirm,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgit5))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpgit5/local/branch/ephemeral",
                source:"admin/mcpgit5"},
              66, Req),
    dispatch(Auth, System, Req, http, _),
    tool_call("git",
              _{action:"delete_branch",
                resource:"admin/mcpgit5/local/branch/ephemeral",
                confirm:"permanently delete ephemeral"},
              67, Req2),
    dispatch(Auth, System, Req2, http, R2),
    \+ get_dict(isError, R2.result, true),
    tool_call("git",
              _{action:"list_branches", resource:"admin/mcpgit5"},
              68, Req3),
    dispatch(Auth, System, Req3, http, R3),
    \+ (member(B, R3.result.structuredContent.branches),
        get_dict(name, B, "ephemeral")).

test(git_merge_computes_common_ancestor,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpmerge))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    % seed a shared doc on main
    tool_call("write",
              _{action:"insert", resource:"admin/mcpmerge",
                commit_info:CI,
                documents:_{'@type':"City", name:"Base"}},
              70, Req0),
    dispatch(Auth, System, Req0, http, _),
    % branch off
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpmerge/local/branch/feature",
                source:"admin/mcpmerge"},
              71, ReqB),
    dispatch(Auth, System, ReqB, http, _),
    % diverge: feature adds one doc, main adds another
    tool_call("write",
              _{action:"insert",
                resource:"admin/mcpmerge/local/branch/feature",
                commit_info:CI,
                documents:_{'@type':"City", name:"Feature"}},
              72, ReqF),
    dispatch(Auth, System, ReqF, http, _),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpmerge",
                commit_info:CI,
                documents:_{'@type':"City", name:"Main"}},
              73, ReqM),
    dispatch(Auth, System, ReqM, http, _),
    % merge feature into main — the true common ancestor is the seed commit
    tool_call("git",
              _{action:"merge", resource:"admin/mcpmerge",
                source:"admin/mcpmerge/local/branch/feature",
                commit_info:CI},
              74, ReqG),
    dispatch(Auth, System, ReqG, http, R),
    \+ get_dict(isError, R.result, true),
    % the merge head is returned so the next write can be drift-guarded
    get_dict(data_version, R.result.structuredContent, Merge_DV),
    once(sub_string(Merge_DV, 0, _, _, "branch:")),
    % both docs now on main
    mcp_read_transaction(Auth, System, "admin/mcpmerge", instance, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    findall(N, (api_document:api_get_documents_by_type(
                    Transaction, instance, "City", Config, D),
                get_dict(name, D, N)),
            Names),
    sort(Names, ["Base","Feature","Main"]).

test(git_merge_conflict_witness_names_incoming_value,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpmerge3))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    tool_call("write",
              _{action:"insert", resource:"admin/mcpmerge3",
                commit_info:CI,
                documents:_{'@type':"City", name:"Shared"}},
              92, Req0),
    dispatch(Auth, System, Req0, http, R0),
    get_dict(ids, R0.result.structuredContent, [Doc_Id|_]),
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpmerge3/local/branch/feature"},
              93, ReqB),
    dispatch(Auth, System, ReqB, http, _),
    % both sides change the same field differently
    tool_call("write",
              _{action:"patch",
                resource:"admin/mcpmerge3/local/branch/feature",
                commit_info:CI,
                patch:[_{'@id':Doc_Id,
                         name:_{'@op':"SwapValue",
                                '@before':"Shared",
                                '@after':"theirs"}}]},
              94, ReqF),
    dispatch(Auth, System, ReqF, http, _),
    tool_call("write",
              _{action:"patch", resource:"admin/mcpmerge3",
                commit_info:CI,
                patch:[_{'@id':Doc_Id,
                         name:_{'@op':"SwapValue",
                                '@before':"Shared",
                                '@after':"ours"}}]},
              95, ReqM),
    dispatch(Auth, System, ReqM, http, _),
    tool_call("git",
              _{action:"merge", resource:"admin/mcpmerge3",
                source:"admin/mcpmerge3/local/branch/feature",
                commit_info:CI},
              96, ReqG),
    dispatch(Auth, System, ReqG, http, R),
    get_dict(isError, R.result, true),
    get_dict(error, R.result.structuredContent, Err),
    once(sub_string(Err, _, _, _, "merge_conflict")),
    get_dict(witnesses, R.result.structuredContent, [Witness|_]),
    get_dict(name, Witness, Name_Witness),
    get_dict('@expected', Name_Witness, "Shared"),
    get_dict('@found', Name_Witness, "ours"),
    % the source branch's value must be named — no second read needed
    get_dict('@incoming', Name_Witness, "theirs"),
    % the hint names the merge target explicitly
    once(sub_string(R.result.structuredContent.hint, _, _, _,
                    "admin/mcpmerge3")).

test(git_merge_identical_final_value_converges,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpmerge4))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    tool_call("write",
              _{action:"insert", resource:"admin/mcpmerge4",
                commit_info:CI,
                documents:_{'@type':"City", name:"Shared"}},
              97, Req0),
    dispatch(Auth, System, Req0, http, R0),
    get_dict(ids, R0.result.structuredContent, [Doc_Id|_]),
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpmerge4/local/branch/feature"},
              98, ReqB),
    dispatch(Auth, System, ReqB, http, _),
    % both sides resolve the same field to the SAME value — semantically
    % converged, so the merge must commit rather than report Conflict
    tool_call("write",
              _{action:"patch",
                resource:"admin/mcpmerge4/local/branch/feature",
                commit_info:CI,
                patch:[_{'@id':Doc_Id,
                         name:_{'@op':"SwapValue",
                                '@before':"Shared",
                                '@after':"resolved"}}]},
              99, ReqF),
    dispatch(Auth, System, ReqF, http, _),
    tool_call("write",
              _{action:"patch", resource:"admin/mcpmerge4",
                commit_info:CI,
                patch:[_{'@id':Doc_Id,
                         name:_{'@op':"SwapValue",
                                '@before':"Shared",
                                '@after':"resolved"}}]},
              100, ReqM),
    dispatch(Auth, System, ReqM, http, _),
    tool_call("git",
              _{action:"merge", resource:"admin/mcpmerge4",
                source:"admin/mcpmerge4/local/branch/feature",
                commit_info:CI},
              101, ReqG),
    dispatch(Auth, System, ReqG, http, R),
    \+ get_dict(isError, R.result, true),
    mcp_read_transaction(Auth, System, "admin/mcpmerge4", instance, _{},
                         Transaction, _),
    api_document:get_document(Transaction, Doc_Id, Doc),
    get_dict(name, Doc, "resolved").

test(git_merge_unrelated_histories_is_iserror,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpmerge2),
             super_user_authority(SU),
             db_branch:branch_create(system_descriptor{}, SU,
                                     "admin/mcpmerge2/local/branch/detached",
                                     empty(_{},_{}), _))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("git",
              _{action:"merge", resource:"admin/mcpmerge2",
                source:"admin/mcpmerge2/local/branch/detached",
                commit_info:_{author:"a", message:"m"}},
              75, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _,
                    "share no commit")).

test(git_create_branch_from_branch_arg,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpbrarg))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpbrarg",
                branch:"side",
                source:"admin/mcpbrarg"},
              76, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    tool_call("git",
              _{action:"list_branches", resource:"admin/mcpbrarg"},
              77, ReqL),
    dispatch(Auth, System, ReqL, http, RL),
    once((member(B, RL.result.structuredContent.branches),
          get_dict(name, B, "side"))).

test(git_create_branch_defaults_source_to_main,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpdefsrc))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    tool_call("write",
              _{action:"insert", resource:"admin/mcpdefsrc",
                commit_info:CI,
                documents:_{'@type':"City", name:"Base"}},
              89, ReqW),
    dispatch(Auth, System, ReqW, http, _),
    % no source — branch should come off main, not empty
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpdefsrc/local/branch/copied"},
              90, ReqB),
    dispatch(Auth, System, ReqB, http, RB),
    \+ get_dict(isError, RB.result, true),
    get_dict(data_version, RB.result.structuredContent, _),
    % the branch carries main's data — a write against its schema works
    tool_call("write",
              _{action:"insert",
                resource:"admin/mcpdefsrc/local/branch/copied",
                commit_info:CI,
                documents:_{'@type':"City", name:"OnBranch"}},
              91, ReqW2),
    dispatch(Auth, System, ReqW2, http, RW2),
    \+ get_dict(isError, RW2.result, true).

test(git_rebase_defaults_to_empty_strategy_map,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcprebase))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    tool_call("write",
              _{action:"insert", resource:"admin/mcprebase",
                commit_info:CI,
                documents:_{'@type':"City", name:"Base"}},
              80, ReqW0),
    dispatch(Auth, System, ReqW0, http, _),
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcprebase/local/branch/feature",
                source:"admin/mcprebase"},
              81, ReqB),
    dispatch(Auth, System, ReqB, http, _),
    tool_call("write",
              _{action:"insert", resource:"admin/mcprebase",
                commit_info:CI,
                documents:_{'@type':"City", name:"MainOnly"}},
              82, ReqM),
    dispatch(Auth, System, ReqM, http, _),
    tool_call("write",
              _{action:"insert",
                resource:"admin/mcprebase/local/branch/feature",
                commit_info:CI,
                documents:_{'@type':"City", name:"FeatureOnly"}},
              83, ReqF),
    dispatch(Auth, System, ReqF, http, _),
    % no strategy_map — must not crash on the default
    tool_call("git",
              _{action:"rebase",
                resource:"admin/mcprebase/local/branch/feature",
                source:"admin/mcprebase",
                commit_info:CI},
              84, ReqR),
    dispatch(Auth, System, ReqR, http, R),
    \+ get_dict(isError, R.result, true),
    % only the post-branchpoint commit is replayed — reports must not
    % contain the schema/base commits that predate the common ancestor
    get_dict(reports, R.result.structuredContent, Reports),
    length(Reports, 1),
    % the branch ends up with both sides' data
    mcp_read_transaction(Auth, System,
                         "admin/mcprebase/local/branch/feature",
                         instance, _{}, Transaction, _),
    mcp_doc_config(_{}, Config),
    findall(N, (api_document:api_get_documents_by_type(
                    Transaction, instance, "City", Config, D),
                get_dict(name, D, N)),
            Names),
    sort(Names, ["Base","FeatureOnly","MainOnly"]).

test(git_squash_moves_branch_head,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpsquash))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    tool_call("write",
              _{action:"insert", resource:"admin/mcpsquash",
                commit_info:CI,
                documents:[_{'@type':"City", name:"One"},
                           _{'@type':"City", name:"Two"}]},
              85, ReqW),
    dispatch(Auth, System, ReqW, http, _),
    % without confirm the squash is refused
    tool_call("git",
              _{action:"squash", resource:"admin/mcpsquash",
                commit_info:CI},
              86, ReqNo),
    dispatch(Auth, System, ReqNo, http, RNo),
    get_dict(isError, RNo.result, true),
    once(sub_string(RNo.result.structuredContent.hint, _, _, _, "confirm")),
    % with confirm the head moves to the single squashed commit
    tool_call("git",
              _{action:"squash", resource:"admin/mcpsquash",
                commit_info:CI,
                confirm:"rewrite history of admin/mcpsquash"},
              87, ReqS),
    dispatch(Auth, System, ReqS, http, RS),
    \+ get_dict(isError, RS.result, true),
    get_dict(data_version, RS.result.structuredContent, Squash_DV),
    once(sub_string(Squash_DV, 0, _, _, "branch:")),
    tool_call("git",
              _{action:"log", resource:"admin/mcpsquash"},
              88, ReqL),
    dispatch(Auth, System, ReqL, http, RL),
    get_dict(log, RL.result.structuredContent, Log),
    length(Log, 1),
    % the squashed head still holds the data
    mcp_read_transaction(Auth, System, "admin/mcpsquash", instance,
                         _{}, Transaction, _),
    mcp_doc_config(_{}, Config),
    findall(N, (api_document:api_get_documents_by_type(
                    Transaction, instance, "City", Config, D),
                get_dict(name, D, N)),
            Names),
    sort(Names, ["One","Two"]).

test(git_reset_requires_confirm,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpreset))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("git",
              _{action:"reset", resource:"admin/mcpreset",
                to:"HEAD"},
              76, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "confirm")).

% -- admin tool -----------------------------------------------------------

test(admin_list_databases,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpadmin))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("admin", _{action:"list_databases"}, 80, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    once(member(D, R.result.structuredContent.databases)),
    get_dict(path, D, "admin/mcpadmin").

test(admin_create_and_delete_database,
     [setup(setup_temp_store(State)),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("admin",
              _{action:"create_database", resource:"admin/mcpnewdb",
                label:"New"},
              81, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    tool_call("admin",
              _{action:"delete_database", resource:"admin/mcpnewdb"},
              82, Req2),
    dispatch(Auth, System, Req2, http, R2),
    get_dict(isError, R2.result, true),
    once(sub_string(R2.result.structuredContent.hint, _, _, _,
                    "permanently delete admin/mcpnewdb")),
    tool_call("admin",
              _{action:"delete_database", resource:"admin/mcpnewdb",
                confirm:"permanently delete admin/mcpnewdb"},
              83, Req3),
    dispatch(Auth, System, Req3, http, R3),
    \+ get_dict(isError, R3.result, true),
    tool_call("admin", _{action:"list_databases"}, 84, Req4),
    dispatch(Auth, System, Req4, http, R4),
    \+ (member(D, R4.result.structuredContent.databases),
        get_dict(path, D, "admin/mcpnewdb")).

test(admin_export_reports_unexported_branches,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpexport))),
      cleanup((teardown_temp_store(State),
               delete_file('/tmp/mcpexport.terminusdb')))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    % a second branch main does not carry
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpexport/local/branch/side"},
              90, ReqB),
    dispatch(Auth, System, ReqB, http, _),
    % file paths write on the server — stdio only
    tool_call("admin",
              _{action:"export", resource:"admin/mcpexport",
                path:"/tmp/mcpexport.terminusdb"},
              91, ReqE),
    dispatch(Auth, System, ReqE, stdio, R),
    \+ get_dict(isError, R.result, true),
    get_dict(branches_exported, R.result.structuredContent, _),
    get_dict(branches_not_exported, R.result.structuredContent, Skipped),
    memberchk("side", Skipped).

test(admin_export_http_path_rejected,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpexpsec))),
      cleanup(teardown_temp_store(State))]) :-
    % CWE-22/73: a client-controlled `path` writes an arbitrary file on the
    % server filesystem — over HTTP the file is useless to the agent anyway
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("admin",
              _{action:"export", resource:"admin/mcpexpsec",
                path:"/tmp/mcpexpsec.terminusdb"},
              92, ReqE),
    dispatch(Auth, System, ReqE, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "payload")).

test(admin_export_import_roundtrip_over_http_payload,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpexpr2))),
      cleanup((superuser(CAuth),
               open_descriptor(system_descriptor{}, CSystem),
               catch(db_delete:delete_db(CSystem, CAuth, admin, mcpimpr2, true),
                     _, true),
               teardown_temp_store(State)))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("admin",
              _{action:"export", resource:"admin/mcpexpr2"}, 93, ReqE),
    dispatch(Auth, System, ReqE, http, RE),
    \+ get_dict(isError, RE.result, true),
    Payload = RE.result.structuredContent.payload,
    string(Payload),
    tool_call("admin",
              _{action:"create_database", resource:"admin/mcpimpr2"},
              94, ReqC),
    dispatch(Auth, System, ReqC, RC),
    \+ get_dict(isError, RC.result, true),
    % a descriptor opened before create_database sees a stale system
    % graph — reopen, as stdio_loop does per request
    open_descriptor(system_descriptor{}, System2),
    tool_call("admin",
              _{action:"import", resource:"admin/mcpimpr2",
                payload:Payload},
              95, ReqI),
    dispatch(Auth, System2, ReqI, http, RI),
    \+ get_dict(isError, RI.result, true).

test(admin_docs_returns_llms_txt) :-
    superuser(Auth),
    tool_call("admin", _{action:"docs"}, 85, Req),
    dispatch(Auth, _, Req, http, R),
    \+ get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.docs, _, _, _,
                    "tools/call")).

test(admin_index_status_backend_none,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpadminix),
             setenv('TERMINUSDB_INDEXER_BACKEND', 'none'),
             config:clear_indexer_backend_config)),
      cleanup((unsetenv('TERMINUSDB_INDEXER_BACKEND'),
               config:clear_indexer_backend_config,
               teardown_temp_store(State)))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("admin",
              _{action:"index_status", resource:"admin/mcpadminix"},
              86, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    get_dict(backend, R.result.structuredContent.indexer, "none"),
    get_dict(ready, R.result.structuredContent.indexer, false).

% -- graphql + search --------------------------------------------------------

test(query_graphql_read_query,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgql))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"graphql", path:"admin/mcpgql",
                query:"{ Person { name } }"},
              95, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    is_dict(R.result.structuredContent.response).

test(query_graphql_error_surfaces_graphql_errors,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgqlerr))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"graphql", path:"admin/mcpgqlerr",
                query:"{ NoSuchType { field } }"},
              99, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _,
                    "graphql error")).

test(query_graphql_rejects_mutation,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgql2))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"graphql", path:"admin/mcpgql2",
                query:"mutation { _commitInfo(author:\"a\") }"},
              96, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _, "write")).

test(write_graphql_insert,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgql3))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"graphql", resource:"admin/mcpgql3",
                commit_info:_{author:"a", message:"gql insert"},
                query:"mutation($input: JSON!) { _commitInfo(author:\"a\", message:\"gql insert\") _insertDocuments(json: $input) }",
                variables:_{input:_{'@type':"Person", name:"Grapha",
                                    address:"1 Graph Lane"}}},
              97, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    % the document is really committed
    tool_call("query",
              _{action:"document", path:"admin/mcpgql3", type:"Person"},
              98, Req2),
    dispatch(Auth, System, Req2, http, R2),
    once(member(D, R2.result.structuredContent.documents)),
    get_dict(name, D, "Grapha").

test(write_graphql_insert_utf8,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgqlutf8))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"graphql", resource:"admin/mcpgqlutf8",
                commit_info:_{author:"a", message:"gql insert utf8"},
                query:"mutation($input: JSON!) { _commitInfo(author:\"a\", message:\"gql insert utf8\") _insertDocuments(json: $input) }",
                variables:_{input:_{'@type':"Person", name:"Åsa Öst",
                                    address:"1 Graph Lane"}}},
              97, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    tool_call("query",
              _{action:"document", path:"admin/mcpgqlutf8", type:"Person"},
              98, Req2),
    dispatch(Auth, System, Req2, http, R2),
    once(member(D, R2.result.structuredContent.documents)),
    get_dict(name, D, "Åsa Öst").

test(write_graphql_replace_utf8,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpgqlrepl))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpgqlrepl",
                commit_info:_{author:"a", message:"seed"},
                documents:_{'@type':"Person", name:"Plain",
                            address:"0 Seed Rd"}},
              90, Req0),
    dispatch(Auth, System, Req0, http, R0),
    \+ get_dict(isError, R0.result, true),
    [Seed_Id] = R0.result.structuredContent.ids,
    tool_call("write",
              _{action:"graphql", resource:"admin/mcpgqlrepl",
                commit_info:_{author:"a", message:"gql replace utf8"},
                query:"mutation($input: JSON!) { _commitInfo(author:\"a\", message:\"gql replace utf8\") _replaceDocuments(json: $input) }",
                variables:_{input:_{'@type':"Person", '@id':Seed_Id,
                                    name:"Björk Guðmundsdóttir",
                                    address:"9 Reykjavík Rd"}}},
              91, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    tool_call("query",
              _{action:"document", path:"admin/mcpgqlrepl", type:"Person"},
              92, Req2),
    dispatch(Auth, System, Req2, http, R2),
    once(member(D, R2.result.structuredContent.documents)),
    get_dict(name, D, "Björk Guðmundsdóttir").

test(query_search_backend_none_educates,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpsearch),
             setenv('TERMINUSDB_INDEXER_BACKEND', 'none'),
             config:clear_indexer_backend_config)),
      cleanup((unsetenv('TERMINUSDB_INDEXER_BACKEND'),
               config:clear_indexer_backend_config,
               teardown_temp_store(State)))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"search", path:"admin/mcpsearch",
                kind:"search", q:"anything"},
              99, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _,
                    "TERMINUSDB_INDEXER_BACKEND")).

test(query_history_and_diff,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcphist))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    % two writes so there is a commit delta
    tool_call("write",
              _{action:"insert", resource:"admin/mcphist",
                commit_info:_{author:"a", message:"h1"},
                documents:_{'@type':"Person", name:"Hist1",
                            address:"a"}},
              100, ReqW1),
    dispatch(Auth, System, ReqW1, http, RW1),
    DV1 = RW1.result.structuredContent.data_version,
    tool_call("write",
              _{action:"insert", resource:"admin/mcphist",
                commit_info:_{author:"a", message:"h2"},
                documents:_{'@type':"Person", name:"Hist2",
                            address:"b"}},
              101, ReqW2),
    dispatch(Auth, System, ReqW2, http, RW2),
    DV2 = RW2.result.structuredContent.data_version,
    % history of the first document — repo-level history is git log
    [Doc_Id] = RW1.result.structuredContent.ids,
    tool_call("query",
              _{action:"history", path:"admin/mcphist", id:Doc_Id},
              102, ReqH),
    dispatch(Auth, System, ReqH, http, RH),
    \+ get_dict(isError, RH.result, true),
    % diff between the two commit versions shows the second insert
    atom_concat("branch:", Tail1, DV1),
    atom_concat("branch:", Tail2, DV2),
    tool_call("query",
              _{action:"diff", path:"admin/mcphist",
                before:Tail1, after:Tail2},
              103, ReqD),
    dispatch(Auth, System, ReqD, http, RD),
    \+ get_dict(isError, RD.result, true),
    get_dict(count, RD.result.structuredContent, N),
    N >= 1,
    % update diff between consecutive commits exercises the simple_diff
    % path — needs the same keep/unfold option defaults as the HTTP route
    [Doc_Id] = RW1.result.structuredContent.ids,
    tool_call("write",
              _{action:"update", resource:"admin/mcphist",
                commit_info:_{author:"a", message:"h3"},
                documents:_{'@type':"Person", '@id':Doc_Id,
                            name:"Hist1", address:"c"}},
              104, ReqW3),
    dispatch(Auth, System, ReqW3, http, RW3),
    DV3 = RW3.result.structuredContent.data_version,
    atom_concat("branch:", Tail3, DV3),
    tool_call("query",
              _{action:"diff", path:"admin/mcphist",
                before:Tail2, after:Tail3},
              105, ReqD2),
    dispatch(Auth, System, ReqD2, http, RD2),
    \+ get_dict(isError, RD2.result, true),
    get_dict(count, RD2.result.structuredContent, N2),
    N2 >= 1.

test(query_history_without_id_explains,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcphistnoid))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"history", path:"admin/mcphistnoid"},
              110, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _,
                    "git action `log`")).

test(query_resource_arg_accepted_as_path,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpalias))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"document", resource:"admin/mcpalias", type:"City"},
              106, Req),
    dispatch(Auth, System, Req, http, R),
    \+ get_dict(isError, R.result, true),
    get_dict(documents, R.result.structuredContent, []).

test(query_diff_accepts_commit_paths,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpdiffpath))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpdiffpath",
                commit_info:_{author:"a", message:"d1"},
                documents:_{'@type':"Person", name:"D1", address:"a"}},
              107, ReqW1),
    dispatch(Auth, System, ReqW1, http, RW1),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpdiffpath",
                commit_info:_{author:"a", message:"d2"},
                documents:_{'@type':"Person", name:"D2", address:"b"}},
              108, ReqW2),
    dispatch(Auth, System, ReqW2, http, RW2),
    atom_concat("branch:", Tail1,
                RW1.result.structuredContent.data_version),
    atom_concat("branch:", Tail2,
                RW2.result.structuredContent.data_version),
    format(string(Before), "admin/mcpdiffpath/local/commit/~w", [Tail1]),
    format(string(After), "admin/mcpdiffpath/local/commit/~w", [Tail2]),
    tool_call("query",
              _{action:"diff", path:"admin/mcpdiffpath",
                before:Before, after:After},
              109, ReqD),
    dispatch(Auth, System, ReqD, http, RD),
    \+ get_dict(isError, RD.result, true),
    get_dict(count, RD.result.structuredContent, N),
    N >= 1.

% -- plugins tool -----------------------------------------------------------

test(plugins_list_empty) :-
    superuser(Auth),
    tool_call("plugins", _{action:"list"}, 90, Req),
    dispatch(Auth, _, Req, http, R),
    \+ get_dict(isError, R.result, true),
    get_dict(tools, R.result.structuredContent, []).

test(plugins_register_list_call_and_collision,
     [cleanup(retractall(mcp:mcp_plugin_tool(mcp_sample, _, _, _)))]) :-
    mcp:register_plugin_tool(
        mcp_sample, "test plugin",
        _{type:"object",
          properties:_{echo:_{type:"string"}}},
        mcp:sample_plugin_tool),
    superuser(Auth),
    tool_call("plugins", _{action:"list"}, 91, Req1),
    dispatch(Auth, _, Req1, http, R1),
    once(member(T, R1.result.structuredContent.tools)),
    get_dict(name, T, mcp_sample),
    % plugin tools appear in tools/list too
    tool_call("plugins",
              _{action:"call", name:"mcp_sample", arguments:_{echo:"hi"}},
              92, Req2),
    dispatch(Auth, _, Req2, http, R2),
    \+ get_dict(isError, R2.result, true),
    get_dict(echo, R2.result.structuredContent, "hi"),
    % and are callable directly by name
    tool_call("mcp_sample", _{echo:"direct"}, 93, Req3),
    dispatch(Auth, _, Req3, http, R3),
    \+ get_dict(isError, R3.result, true),
    get_dict(echo, R3.result.structuredContent, "direct"),
    % name collision -> educational error
    catch(mcp:register_plugin_tool(mcp_sample, "dup", _{}, mcp:sample_plugin_tool),
          mcp_tool_error(plugin_name_collision(mcp_sample), _), true),
    % unknown plugin name -> educational error
    tool_call("plugins", _{action:"call", name:"mcp_nope"}, 94, Req4),
    dispatch(Auth, _, Req4, http, R4),
    get_dict(isError, R4.result, true),
    once(sub_string(R4.result.structuredContent.hint, _, _, _, "mcp_sample")).

test(query_diff_accepts_bare_commit_ids,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpdiffbare))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpdiffbare",
                commit_info:_{author:"a", message:"d1"},
                documents:_{'@type':"Person", name:"D1", address:"a"}},
              107, ReqW1),
    dispatch(Auth, System, ReqW1, http, RW1),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpdiffbare",
                commit_info:_{author:"a", message:"d2"},
                documents:_{'@type':"Person", name:"D2", address:"b"}},
              108, ReqW2),
    dispatch(Auth, System, ReqW2, http, RW2),
    atom_concat("branch:", Tail1,
                RW1.result.structuredContent.data_version),
    atom_concat("branch:", Tail2,
                RW2.result.structuredContent.data_version),
    tool_call("query",
              _{action:"diff", path:"admin/mcpdiffbare",
                before:Tail1, after:Tail2},
              109, ReqD),
    dispatch(Auth, System, ReqD, http, RD),
    \+ get_dict(isError, RD.result, true),
    get_dict(count, RD.result.structuredContent, N),
    N >= 1.

test(git_reset_accepts_bare_commit_id_and_source_alias,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpresetbare))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    tool_call("write",
              _{action:"insert", resource:"admin/mcpresetbare",
                commit_info:CI,
                documents:_{'@type':"Person", name:"R1", address:"a"}},
              110, ReqW1),
    dispatch(Auth, System, ReqW1, http, RW1),
    atom_concat("branch:", Head1,
                RW1.result.structuredContent.data_version),
    tool_call("write",
              _{action:"insert", resource:"admin/mcpresetbare",
                commit_info:CI,
                documents:_{'@type':"Person", name:"R2", address:"b"}},
              111, ReqW2),
    dispatch(Auth, System, ReqW2, http, _),
    % reset takes a bare commit id under `source` as readily as `to`
    tool_call("git",
              _{action:"reset", resource:"admin/mcpresetbare",
                source:Head1,
                confirm:"rewrite history of admin/mcpresetbare"},
              112, ReqR),
    dispatch(Auth, System, ReqR, http, RR),
    \+ get_dict(isError, RR.result, true),
    mcp_read_transaction(Auth, System, "admin/mcpresetbare", instance, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    findall(N, (api_document:api_get_documents_by_type(
                    Transaction, instance, "Person", Config, D),
                get_dict(name, D, N)),
            Names),
    sort(Names, ["R1"]).

test(git_merge_accepts_to_alias,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpmergeto))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    tool_call("write",
              _{action:"insert", resource:"admin/mcpmergeto",
                commit_info:CI,
                documents:_{'@type':"Person", name:"Base", address:"a"}},
              113, ReqW),
    dispatch(Auth, System, ReqW, http, _),
    tool_call("git",
              _{action:"create_branch",
                resource:"admin/mcpmergeto/local/branch/feature"},
              114, ReqB),
    dispatch(Auth, System, ReqB, http, _),
    tool_call("write",
              _{action:"insert",
                resource:"admin/mcpmergeto/local/branch/feature",
                commit_info:CI,
                documents:_{'@type':"Person", name:"Feat", address:"b"}},
              115, ReqF),
    dispatch(Auth, System, ReqF, http, _),
    tool_call("git",
              _{action:"merge", resource:"admin/mcpmergeto",
                to:"admin/mcpmergeto/local/branch/feature",
                commit_info:CI},
              116, ReqM),
    dispatch(Auth, System, ReqM, http, RM),
    \+ get_dict(isError, RM.result, true),
    mcp_read_transaction(Auth, System, "admin/mcpmergeto", instance, _{},
                         Transaction, _),
    mcp_doc_config(_{}, Config),
    findall(N, (api_document:api_get_documents_by_type(
                    Transaction, instance, "Person", Config, D),
                get_dict(name, D, N)),
            Names),
    sort(Names, ["Base","Feat"]).

test(query_woql_count_limits_bindings,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpwoqlcnt))),
      cleanup(teardown_temp_store(State))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    CI = _{author:"a", message:"m"},
    tool_call("write",
              _{action:"insert", resource:"admin/mcpwoqlcnt",
                commit_info:CI,
                documents:[_{'@type':"Person", name:"P1", address:"a"},
                           _{'@type':"Person", name:"P2", address:"b"},
                           _{'@type':"Person", name:"P3", address:"c"}]},
              117, ReqW),
    dispatch(Auth, System, ReqW, http, _),
    tool_call("query",
              _{action:"woql", path:"admin/mcpwoqlcnt", count:2,
                query:_{'@type':"IsA",
                        element:_{'@type':"NodeValue", variable:"E"},
                        type:_{'@type':"NodeValue", node:"@schema:Person"}}},
              118, ReqQ),
    dispatch(Auth, System, ReqQ, http, RQ),
    \+ get_dict(isError, RQ.result, true),
    get_dict(bindings, RQ.result.structuredContent.result, Bindings),
    length(Bindings, 2).

test(query_search_accepts_query_alias,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin, mcpsearchalias),
             setenv('TERMINUSDB_INDEXER_BACKEND', 'none'),
             config:clear_indexer_backend_config)),
      cleanup((unsetenv('TERMINUSDB_INDEXER_BACKEND'),
               config:clear_indexer_backend_config,
               teardown_temp_store(State)))]) :-
    superuser(Auth),
    open_descriptor(system_descriptor{}, System),
    tool_call("query",
              _{action:"search", path:"admin/mcpsearchalias",
                kind:"search", query:"anything"},
              119, Req),
    dispatch(Auth, System, Req, http, R),
    get_dict(isError, R.result, true),
    once(sub_string(R.result.structuredContent.hint, _, _, _,
                    "TERMINUSDB_INDEXER_BACKEND")).

:- end_tests(mcp).
