:- module(plugins, [
              pre_commit_hook/2,
              post_commit_hook/2,
              pre_server_startup_hook/1,
              post_server_startup_hook/1,
              load_plugins/0
          ]).
:- use_module(library(lists)).
:- use_module(library(filesex)).
:- use_module(config(terminus_config)).

:- multifile pre_commit_hook/2.
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
               load_files(Full_Path, [if(not_loaded), must_be_module(false)]))).
load_plugins.
