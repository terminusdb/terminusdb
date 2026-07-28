:- module(webserver_spa, []).

:- use_module(core(plugin_api)).

:- multifile appserver_hooks:appserver_static_path/2.
:- multifile appserver_hooks:appserver_static_path/3.

%% appserver_hooks:appserver_static_path(+Prefix, +Directory) is det.
%% appserver_hooks:appserver_static_path(+Prefix, +Directory, +Options) is det.
%
%  Register the built-in SPA applications with the Rust webserver.
%  Each entry maps a URL prefix to a directory on disk. The directory may be
%  absolute or relative to the `TERMINUSDB_ADDON_PATH` environment variable
%  (falling back to the current working directory).
%
%  The arity/2 form uses sensible defaults: fallback to index.html,
%  auth none, and csp_nonce enabled. The arity/3 form accepts an option
%  list that can include `fallback(File)`, `auth(none | authenticated)`,
%  and `csp_nonce(true)`. To disable the fallback (serve 404 for missing
%  paths instead of the SPA entry point), use `fallback('')`. To serve a
%  custom 404 page, use `not_found('404.html')`.

% Simple form — defaults apply (fallback: index.html, auth: none, csp_nonce: true)
appserver_hooks:appserver_static_path('/dashboard', 'dashboard/src').

% Full form — explicit options
appserver_hooks:appserver_static_path('/app/admin', 'app/admin/dist', [fallback('index.html'), auth(none), csp_nonce(true)]).
appserver_hooks:appserver_static_path('/app/data',  'app/data/dist',  [fallback('index.html'), auth(none), csp_nonce(true)]).
