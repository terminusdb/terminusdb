:- module(json_log, [
              json_log_error/1,
              json_log_error_formatted/2,
              json_log_warning/1,
              json_log_warning_formatted/2,
              json_log_info/1,
              json_log_info_formatted/2,
              json_log_trace/1,
              json_log_trace_formatted/2
          ]).

:- use_module(utils).

:- use_module(library(http/json)).
:- use_module(library(http/http_stream)).
:- use_module(library(broadcast)).

:- listen(http(Term), http_request_logger(Term)).

json_log_raw_error(E, Dict) :-
    % oh no! something went wrong while writing a log entry!
    % This is usually a programming error, where a variable is fed
    % into the json writer. We'll log an error here in a known good
    % format.

    Error_Dict = _{
                     message: "Error while trying to write a log entry",
                     loggable: Dict_String,
                     error: E_String,
                     severity: "ERROR"
                 },
    term_string(Dict, Dict_String),
    term_string(E, E_String),

    json_write_dict(user_error,
                    Error_Dict,
                    [width(0)]),
    with_output_to(user_error,
                   nl).

json_log_raw(Dict) :-
    catch((with_output_to(string(Result),
                          json_write_dict(current_output,
                                          Dict,
                                          [width(0)])),
           with_output_to(user_error,
                          (   write(Result),
                              nl))),
          E,
          json_log_raw_error(E, Dict)).

generate_time(Time) :-
    get_time(Now),
    format_time(string(Time), '%FT%T.%f%:z', Now).

expand_operation_id(first(Operation_Id), Dict) :-
    !,
    Dict = json{
               id: Operation_Id,
               first: true
           }.
expand_operation_id(last(Operation_Id), Dict) :-
    !,
    Dict = json{
               id: Operation_Id,
               last: true
           }.
expand_operation_id(Operation_Id, Dict) :-
    ground(Operation_Id),
    !,
    Dict = json{
               id: Operation_Id
           }.
expand_operation_id(_,_).

expand_json_log(Dict, Operation_Id, Severity, Output) :-
    generate_time(Time),
    expand_operation_id(Operation_Id, Operation_Dict),
    include([_-V]>>(nonvar(V)), [severity-Severity,
                                 timestamp-Time,
                                 'logging.googleapis.com/operation'-Operation_Dict
                                ],
            Aux_Pairs),

    dict_create(Aux, json, Aux_Pairs),

    put_dict(Dict, Aux, Output).

generate_operation_id(Local_Id, Operation_Id) :-
    server_name(Server_Name),
    format(string(Operation_Id), "~w-~w", [Server_Name, Local_Id]).
generate_operation_id_from_stream(CGI, Operation_Id) :-
    cgi_property(CGI, id(Local_Id)),
    !,
    generate_operation_id(Local_Id, Operation_Id).
generate_operation_id_from_stream(_, _).

generate_operation_id(Operation_Id) :-
    current_output(Stream),
    (   is_cgi_stream(Stream)
    ->  generate_operation_id_from_stream(Stream, Operation_Id)
    ;   Operation_Id = _).

json_log(Operation_Id, Severity, Loggable) :-
    \+ is_dict(Loggable),
    !,
    format(string(Message), "~w", [Loggable]),
    Dict = _{message: Message},
    json_log(Operation_Id, Severity, Dict).
json_log(Operation_Id, Severity, Dict) :-
    expand_json_log(Dict, Operation_Id, Severity, Output),
    json_log_raw(Output).
json_log(Severity, Loggable) :-
    generate_operation_id(Operation_Id),
    json_log(Operation_Id, Severity, Loggable).

json_log_error(Operation_Id, Loggable) :-
    json_log(Operation_Id, 'ERROR', Loggable).
json_log_error(Loggable) :-
    json_log('ERROR', Loggable).

json_log_error_formatted(Format, Arguments) :-
    format(string(Message), Format, Arguments),
    json_log_error(Message).

json_log_warning(Operation_Id, Loggable) :-
    json_log(Operation_Id, 'WARNING', Loggable).
json_log_warning(Loggable) :-
    json_log('WARNING', Loggable).

json_log_warning_formatted(Format, Arguments) :-
    format(string(Message), Format, Arguments),
    json_log_warning(Message).

json_log_info(Operation_Id, Loggable) :-
    json_log(Operation_Id, 'INFO', Loggable).
json_log_info(Loggable) :-
    json_log('INFO', Loggable).

json_log_info_formatted(Format, Arguments) :-
    format(string(Message), Format, Arguments),
    json_log_info(Message).

json_log_trace(Operation_Id, Loggable) :-
    json_log(Operation_Id, 'TRACE', Loggable).
json_log_trace(Loggable) :-
    json_log('TRACE', Loggable).

json_log_trace_formatted(Format, Arguments) :-
    format(string(Message), Format, Arguments),
    json_log_trace(Message).

match_http_info(method(Method), Method_Upper, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size) :-
    string_upper(Method, Method_Upper).
match_http_info(protocol(Protocol), _Method, Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size).
match_http_info(host(Host), _Method, _Protocol, Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size).
match_http_info(port(Port), _Method, _Protocol, _Host, Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size).
match_http_info(request_uri(Url_Suffix), _Method, _Protocol, _Host, _Port, Url_Suffix, _Remote_Ip, _User_Agent, _Size).
match_http_info(peer(Peer), _Method, _Protocol, _Host, _Port, _Url_Suffix, Remote_Ip, _User_Agent, _Size) :-
    % what about ipv6 though?
    % is there any other sort of peer possible?
    Peer = ip(N1,N2,N3,N4),
    format(string(Remote_Ip), "~w.~w.~w.~w", [N1, N2, N3, N4]).
match_http_info(user_agent(User_Agent), _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, User_Agent, _Size).
match_http_info(content_length(Size), _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, Size_String) :-
    term_string(Size, Size_String).
match_http_info(_, _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size).

extract_http_info_([], _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size).
extract_http_info_([First|Rest], Method, Protocol, Host, Port, Url_Suffix, Remote_Ip, User_Agent, Size) :-
    match_http_info(First, Method, Protocol, Host, Port, Url_Suffix, Remote_Ip, User_Agent, Size),
    !,
    extract_http_info_(Rest, Method, Protocol, Host, Port, Url_Suffix, Remote_Ip, User_Agent, Size).

extract_http_info(Request, Method, Url, Path, Remote_Ip, User_Agent, Size) :-
    extract_http_info_(Request, Method, Protocol, Host, Port, Path, Remote_Ip, User_Agent, Size),

    (   var(Port)
    ->  format(string(Url), "~w://~w~w", [Protocol, Host, Path])
    ;   format(string(Url), "~w://~w:~w~w", [Protocol, Host, Port, Path])).

http_request_logger(request_start(Local_Id, Request)) :-
    extract_http_info(Request, Method, Url, Path, Remote_Ip, User_Agent, Size),
    include([_-V]>>(nonvar(V)), [requestMethod-Method,
                                 requestUrl-Url,
                                 requestSize-Size,
                                 remoteIp-Remote_Ip,
                                 userAgent-User_Agent
                                ],
           Http_Pairs),
    dict_create(Http, json, Http_Pairs),

    include([_-V]>>(nonvar(V)), [httpRequest-Http,
                                 path-Path,
                                 message-"Request started"
                                ],
            Dict_Pairs),
    generate_operation_id(Local_Id, Operation_Id),
    dict_create(Dict, json, Dict_Pairs),
    json_log_info(first(Operation_Id),
                  Dict).

http_request_logger(request_finished(Local_Id, Code, _Status, _Cpu, Bytes)) :-
    term_string(Bytes, Bytes_String),
    generate_operation_id(Local_Id, Operation_Id),
    json_log_info(last(Operation_Id),
                  json{
                      httpRequest: json{
                                       status: Code,
                                       responseSize: Bytes_String
                                   },
                      message: "Request completed"
                  }).
