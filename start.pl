#!/usr/bin/env swipl

:- initialization(main).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(terminus_home, Dir)).

add_library_path :-
    user:file_search_path(terminus_home, Dir),
    writeq(Dir),
    atom_concat(Dir,'/library',Library),
    asserta(user:file_search_path(library, Library)).

:- add_library_path.
   
initialise_server_settings :-
    file_search_path(terminus_home, BasePath),    
    !,
    atom_concat(BasePath, '/config.pl', Settings_Path),
    (   exists_file(Settings_Path)
    ->  true
    ;   atom_concat(BasePath, '/config-example.pl',
                    Example_Settings_Path),
        copy_file(Example_Settings_Path,Settings_Path)
    ).

:- initialise_server_settings.

:- use_module(library(api)).
:- use_module(library(server)).

main(Argv) :-
    server(Argv).
