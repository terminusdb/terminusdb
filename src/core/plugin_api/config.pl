:- module(plugin_api_config, [
    plugin_env/2,
    plugin_env_default/3,
    plugin_consume_env/2,
    plugin_consume_env_default/3,
    abolish_plugin_env/1,
    abolish_plugin_env_default/2
]).

:- use_module(core(util), [getenv_default/3]).

/** <module> Generic tabled environment-variable access for plugins

Provides tabled env-var predicates so plugins can read configuration
without each one reinventing getenv + tabling.  Tables are keyed by
the env-var name (an atom), so abolishing a single key is cheap.

The plugin_consume_env/2 and plugin_consume_env_default/3 variants
read the env var and then unsetenv it from the process, so the value
is cached in the table but no longer accessible via getenv/2 to other
plugins.  This protects credentials from cross-plugin leakage.

@see plugin_api for the umbrella re-export module.
*/

%% plugin_env(+EnvVar, -Value) is semidet.
%
%  Tabled.  Succeeds iff EnvVar is set in the process environment.
:- table plugin_env/2 as private.
plugin_env(EnvVar, Value) :-
    getenv(EnvVar, Value).

%% plugin_env_default(+EnvVar, +Default, -Value) is det.
%
%  Tabled.  Returns the env var value or Default if unset/empty.
:- table plugin_env_default/3 as private.
plugin_env_default(EnvVar, Default, Value) :-
    getenv_default(EnvVar, Default, Value).

%% plugin_consume_env(+EnvVar, -Value) is semidet.
%
%  Tabled.  Reads EnvVar then unsets it from the process environment.
%  The value is cached in the table.  Other plugins cannot getenv it.
:- table plugin_consume_env/2 as private.
plugin_consume_env(EnvVar, Value) :-
    getenv(EnvVar, Value),
    unsetenv(EnvVar).

%% plugin_consume_env_default(+EnvVar, +Default, -Value) is det.
%
%  Tabled.  Reads EnvVar (or Default), then unsets it from the process.
:- table plugin_consume_env_default/3 as private.
plugin_consume_env_default(EnvVar, Default, Value) :-
    getenv_default(EnvVar, Default, Value),
    unsetenv(EnvVar).

%% abolish_plugin_env(+EnvVar) is det.
%
%  Clear the tabled cache for plugin_env/2 for a specific env var.
abolish_plugin_env(EnvVar) :-
    abolish_table_subgoals(plugin_env(EnvVar, _)).

%% abolish_plugin_env_default(+EnvVar, +Default) is det.
%
%  Clear the tabled cache for plugin_env_default/3 for a specific
%  env var + default pair.
abolish_plugin_env_default(EnvVar, Default) :-
    abolish_table_subgoals(plugin_env_default(EnvVar, Default, _)).
