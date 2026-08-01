:- module(appserver_hooks, [
              appserver_route/3,
              appserver_route/4,
              appserver_static_path/2,
              appserver_static_path/3,
              appserver_stream/3,
              appserver_ws/3
          ]).

:- use_module(library(lists)).

:- multifile appserver_route/3.
:- dynamic appserver_route/3.
:- multifile appserver_route/4.
:- dynamic appserver_route/4.
:- multifile appserver_static_path/2.
:- multifile appserver_static_path/3.
:- multifile appserver_stream/3.
:- dynamic appserver_stream/3.
:- multifile appserver_ws/3.
:- dynamic appserver_ws/3.

:- dynamic appserver_hooks:appserver_static_path_normalized/3.
:- multifile appserver_hooks:appserver_static_path_normalized/3.

%% appserver_static_options(+Options, -Normalized) is det.
%
%  Normalize the option list used by appserver_static_path/3 into a dict.
%  Recognised keys: fallback (default 'index.html'), auth (default none),
%  csp_nonce (default true), not_found (default '' — no custom 404 page).
%  Duplicates for any option are rejected
%  with a hard error. New options can be added by extending this
%  predicate without changing its arity.
appserver_static_options(Options, Normalized) :-
    (   findall(F, member(fallback(F), Options), Fallbacks),
        (   Fallbacks = []
        ->  Fallback = 'index.html'
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
    ),
    (   findall(N, member(csp_nonce(N), Options), Nonces),
        (   Nonces = []
        ->  CspNonce = true
        ;   Nonces = [CspNonce]
        ->  true
        ;   throw(error(static_path_duplicate_csp_nonce(Options), _))
        )
    ),
    (   findall(NF, member(not_found(NF), Options), NotFoundFiles),
        (   NotFoundFiles = []
        ->  NotFound = ''
        ;   NotFoundFiles = [NotFound]
        ->  true
        ;   throw(error(static_path_duplicate_not_found(Options), _))
        )
    ),
    Normalized = _{fallback:Fallback, auth:Auth, csp_nonce:CspNonce, not_found:NotFound}.

%% normalize_static_paths is det.
%
%  Collect all appserver_static_path/2 and /3 registrations, normalize
%  their option lists into dicts, and assert
%  appserver_static_path_normalized/3 so the appserver can read them
%  without parsing Prolog lists. The arity/2 form is equivalent to
%  passing an empty option list.
normalize_static_paths :-
    retractall(appserver_hooks:appserver_static_path_normalized(_, _, _)),
    forall(
        (   appserver_hooks:appserver_static_path(Prefix, Directory, Options)
        ;   appserver_hooks:appserver_static_path(Prefix, Directory),
            Options = []
        ),
        (   appserver_static_options(Options, Normalized),
            assertz(appserver_hooks:appserver_static_path_normalized(
                        Prefix, Directory, Normalized))
        )
    ).
