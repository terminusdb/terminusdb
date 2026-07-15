:- module(webserver_spa, []).

:- use_module(core(appserver_hooks)).

:- multifile appserver_hooks:appserver_static_path/3.

%% appserver_hooks:appserver_static_path(+Prefix, +Directory, +Options) is det.
%
%  Register the built-in SPA applications with the Rust webserver.
%  Each entry maps a URL prefix to a directory on disk. The directory may be
%  absolute or relative to the `TERMINUSDB_ADDON_PATH` environment variable
%  (falling back to the current working directory). Options is a list that can
%  include `fallback(File)` and `auth(none | authenticated)`.
appserver_hooks:appserver_static_path('/app/admin', 'app/admin/dist', [fallback('index.html'), auth(none)]).
appserver_hooks:appserver_static_path('/app/data',  'app/data/dist',  [fallback('index.html'), auth(none)]).
