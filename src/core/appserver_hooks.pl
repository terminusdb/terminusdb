:- module(appserver_hooks, [
              appserver_route/3,
              appserver_route/4,
              appserver_static_path/3,
              appserver_stream/3
          ]).

:- use_module(library(lists)).

:- multifile appserver_route/3.
:- dynamic appserver_route/3.
:- multifile appserver_route/4.
:- dynamic appserver_route/4.
:- multifile appserver_static_path/3.
:- multifile appserver_stream/3.
:- dynamic appserver_stream/3.

:- dynamic appserver_hooks:appserver_static_path_normalized/4.
:- multifile appserver_hooks:appserver_static_path_normalized/4.

%% appserver_static_options(+Options, -Fallback, -Auth) is det.
%
%  Normalize the option list used by appserver_static_path/3.
%  `Fallback` is the empty atom when not specified; `Auth` defaults to `none`.
%  Duplicates for either option are rejected with a hard error.
appserver_static_options(Options, Fallback, Auth) :-
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
%  Collect all appserver_static_path/3 registrations, normalize their
%  option lists, and assert appserver_static_path_normalized/4 so the
%  appserver can read them without parsing Prolog lists.
normalize_static_paths :-
    retractall(appserver_hooks:appserver_static_path_normalized(_, _, _, _)),
    forall(
        appserver_hooks:appserver_static_path(Prefix, Directory, Options),
        (   appserver_static_options(Options, Fallback, Auth),
            assertz(appserver_hooks:appserver_static_path_normalized(Prefix, Directory, Fallback, Auth))
        )
    ).
