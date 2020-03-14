:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

add_terminus_home_path :-
    prolog_load_context(file, File),
    file_directory_name(File, Dir),
    
    asserta(user:file_search_path(terminus_home, Dir)).

:- add_terminus_home_path.

add_library_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/library',Library),
    asserta(user:file_search_path(library, Library)).

:- add_library_path.

add_config_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/config',Config),
    asserta(user:file_search_path(config, Config)).

:- add_config_path.

add_test_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/test',Config),
    asserta(user:file_search_path(test, Config)).

:- add_test_path.

add_plugin_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/plugins',Config),
    asserta(user:file_search_path(plugins, Config)).

:- add_plugin_path.

add_ontology_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/terminus-schema',Ontology),
    asserta(user:file_search_path(ontology, Ontology)).

:- add_ontology_path.
