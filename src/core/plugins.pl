:- module(plugins, [
              post_commit_hook/2,
              pre_server_startup_hook/1,
              post_server_startup_hook/1,
              load_plugins/0
          ]).
:- use_module(library(lists)).
:- use_module(library(filesex)).
:- use_module(config(terminus_config)).

:- multifile post_commit_hook/2.
:- multifile pre_server_startup_hook/1.
:- multifile post_server_startup_hook/1.

load_plugins :-
    plugin_path(Path),
    exists_directory(Path),
    !,
    directory_files(Path, Files),
    forall((member(File, Files),
            file_name_extension(_, '.pl', File)),

           (   directory_file_path(Path, File, Full_Path),
               [Full_Path])).
load_plugins.
