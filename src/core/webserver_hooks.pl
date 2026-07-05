:- module(webserver_hooks, [
              rust_webserver_route/3,
              rust_webserver_static_path/3,
              rust_webserver_stream/3
          ]).

:- use_module(library(lists)).

:- multifile rust_webserver_route/3.
:- multifile rust_webserver_static_path/3.
:- multifile rust_webserver_stream/3.

:- dynamic webserver_hooks:rust_webserver_static_path_normalized/4.
:- multifile webserver_hooks:rust_webserver_static_path_normalized/4.

%% rust_webserver_static_options(+Options, -Fallback, -Auth) is det.
%
%  Normalize the option list used by rust_webserver_static_path/3.
%  `Fallback` is the empty atom when not specified; `Auth` defaults to `none`.
%  Duplicates for either option are rejected with a hard error.
rust_webserver_static_options(Options, Fallback, Auth) :-
    (   findall(F, member(fallback(F), Options), Fallbacks),
        (   Fallbacks = []
        ->  Fallback = ''
        ;   Fallbacks = [Fallback]
        ->  true
        ;   throw(error(static_path_duplicate_fallback(Options), _))
        )
    ),
    (   findall(A, member(auth(A), Options), Auths),
        (   Auths = []
        ->  Auth = none
        ;   Auths = [Auth]
        ->  true
        ;   throw(error(static_path_duplicate_auth(Options), _))
        )
    ).

%% normalize_static_paths is det.
%
%  Collect all rust_webserver_static_path/3 registrations, normalize their
%  option lists, and assert rust_webserver_static_path_normalized/4 so the
%  Rust webserver can read them without parsing Prolog lists.
normalize_static_paths :-
    retractall(webserver_hooks:rust_webserver_static_path_normalized(_, _, _, _)),
    forall(
        webserver_hooks:rust_webserver_static_path(Prefix, Directory, Options),
        (   rust_webserver_static_options(Options, Fallback, Auth),
            assertz(webserver_hooks:rust_webserver_static_path_normalized(Prefix, Directory, Fallback, Auth))
        )
    ).
