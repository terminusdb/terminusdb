:- module(plugins, [
              post_commit_hook/2,
              load_plugins/0
          ]).
:- use_module(config(terminus_config)).

:- multifile post_commit_hook/2.

load_plugins :-
    plugin_path(Path),
    exists_directory(Path),
    directory_files(Path, Files),
    foreach((member(File, Files),
             file_name_extension(_, '.pl', File)),

            (   directory_file_path(Path, File, Full_Path),
                [Full_Path])).
