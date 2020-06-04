:- module(init, [
              initialize_config/4,
              initialize_registry/0,
              initialize_index/2,
              initialize_database/2,
              initialize_database_with_path/3,
              initialize_database_with_store/3
          ]).

:- use_module(core(triple)).

:- use_module(library(semweb/turtle)).

:- use_module(library(terminus_store)).

/**
 * create_graph_from_turtle(DB:database, Graph_ID:graph_identifier, Turtle:string) is det.
 *
 * Reads in Turtle String and writes initial database.
 */
create_graph_from_turtle(Store, Graph_ID, TTL_Path) :-
    safe_create_named_graph(Store,Graph_ID,Graph_Obj),
    open_write(Store, Builder),

    % write to a temporary builder.
    rdf_process_turtle(
        TTL_Path,
        {Builder}/
        [Triples,_Resource]>>(
            forall(member(T, Triples),
                   (   normalise_triple(T, rdf(X,P,Y)),
                       object_storage(Y,S),
                       nb_add_triple(Builder, X, P, S)))),
        []),
    % commit this builder to a temporary layer to perform a diff.
    nb_commit(Builder,Layer),
    nb_set_head(Graph_Obj, Layer).

example_registry_path(Path) :-
    once(expand_file_search_path(template('example_registry.pl'), Path)).

template_terminus_instance_ttl(Path) :-
    once(expand_file_search_path(template('terminus_instance_template.ttl'), Path)).


terminus_inference_ttl(Path) :-
    once(expand_file_search_path(ontology('terminus_inference.owl.ttl'), Path)).

terminus_schema_ttl(Path) :-
    once(expand_file_search_path(ontology('terminus_schema.owl.ttl'), Path)).

repository_schema_ttl(Path) :-
    once(expand_file_search_path(ontology('repository.owl.ttl'), Path)).

layer_schema_ttl(Path) :-
    once(expand_file_search_path(ontology('layer.owl.ttl'), Path)).

ref_schema_ttl(Path) :-
    once(expand_file_search_path(ontology('ref.owl.ttl'), Path)).

config_path(Path) :-
    once(expand_file_search_path(config('terminus_config.pl'), Path)).

config_template_path(Path) :-
    once(expand_file_search_path(template('config-template.tpl'), Path)).

index_url(PublicUrl, PublicUrl, Opts) :-
    memberchk(autoattach(AutoAttach), Opts),
    AutoAttach,
    !.
index_url(_, "", _).

index_key(Key, Key, Opts) :-
    memberchk(autologin(AutoLogin), Opts),
    AutoLogin,
    !.
index_key(_, "", _).

index_template_path(Path) :-
    once(expand_file_search_path(terminus_home('index.tpl'), Path)).

%% I find it highly dubious that these 3 files should be in a directory called tmp
instance_path(Path) :-
    config:tmp_path(Tmp),
    atomic_list_concat([Tmp, '/instance.ttl'], Path).

inference_path(Path) :-
    config:tmp_path(Tmp),
    atomic_list_concat([Tmp, '/inference.ttl'], Path).

schema_path(Path) :-
    config:tmp_path(Tmp),
    atomic_list_concat([Tmp, '/schema.ttl'], Path).

replace_in_file(Path, Pattern, With) :-
    read_file_to_string(Path, FileString, []),
    atomic_list_concat(Split, Pattern, FileString),
    atomic_list_concat(Split, With, NewFileString),
    open(Path, write, FileStream),
    write(FileStream, NewFileString),
    close(FileStream).

write_config_file(Public_URL, Config_Tpl_Path, Config_Path, Server_Name, Port, Workers) :-
    open(Config_Tpl_Path, read, Tpl_Stream),
    read_string(Tpl_Stream, _, Tpl_String),
    close(Tpl_Stream),
    open(Config_Path, write, Stream),
    format(Stream, Tpl_String, [Server_Name, Port, Public_URL, Workers]),
    close(Stream).

write_index_file(Index_Tpl_Path, Index_Path, Password) :-
    open(Index_Tpl_Path, read, Tpl_Stream),
    read_string(Tpl_Stream, _, Tpl_String),
    close(Tpl_Stream),
    open(Index_Path, write, Stream),
    config:console_base_url(BaseURL),
    format(Stream, Tpl_String, [BaseURL, Password, BaseURL]),
    close(Stream).

initialize_config(PUBLIC_URL, Server, Port, Workers) :-
    config_template_path( Config_Tpl_Path),
    config_path(Config_Path),
    write_config_file(PUBLIC_URL, Config_Tpl_Path, Config_Path, Server,
                      Port, Workers).


initialize_registry :-
    config:registry_path(Registry_Path),
    (   exists_file(Registry_Path)
    ->  true
    ;   example_registry_path(Example_Registry_Path),
        copy_file(Example_Registry_Path, Registry_Path)
    ).

initialize_database(Public_URL, Key) :-
    config:default_database_path(DB_Path),
    initialize_database_with_path(Public_URL, Key, DB_Path).

initialize_database_with_path(Public_URL, Key, DB_Path) :-
    make_directory_path(DB_Path),
    delete_directory_contents(DB_Path),
    initialize_server_version(DB_Path),
    open_directory_store(DB_Path, Store),
    initialize_database_with_store(Public_URL, Key, Store).

initialize_server_version(DB_Path) :-
    atomic_list_concat([DB_Path,'/SERVER_VERSION'],Path),
    open(Path, write, FileStream),
    writeq(FileStream, 1),
    close(FileStream).

initialize_database_with_store(Public_URL, Key, Store) :-
    template_terminus_instance_ttl(Example_Instance_TTL),

    terminus_inference_ttl(Terminus_Inference_TTL),
    terminus_schema_ttl(Terminus_Schema_TTL),

    ref_schema_ttl(Terminus_Ref_TTL),
    layer_schema_ttl(Terminus_Layer_TTL),
    repository_schema_ttl(Terminus_Repository_TTL),


    instance_path(Instance_TTL_Path),

    crypto_password_hash(Key,Hash, [cost(15)]),

    % Need to copy this one from a template as we alter it.
    copy_file(Example_Instance_TTL, Instance_TTL_Path),
    replace_in_file(Instance_TTL_Path, "SEKRET_ADMIN_KEY", Hash),
    replace_in_file(Instance_TTL_Path, "SERVER_NAME", Public_URL),

    terminus_instance_name(Instance_Name),
    create_graph_from_turtle(Store,Instance_Name,Instance_TTL_Path),

    terminus_schema_name(Schema_Name),
    create_graph_from_turtle(Store,Schema_Name,Terminus_Schema_TTL),

    terminus_inference_name(Inference_Name),
    create_graph_from_turtle(Store,Inference_Name,Terminus_Inference_TTL),

    layer_ontology(Layer_Name),
    create_graph_from_turtle(Store,Layer_Name,Terminus_Layer_TTL),

    ref_ontology(Ref_Name),
    create_graph_from_turtle(Store,Ref_Name,Terminus_Ref_TTL),

    repository_ontology(Repository_Name),
    create_graph_from_turtle(Store,Repository_Name,Terminus_Repository_TTL).


initialize_index(Key, Opts) :-
    index_key(Key, Password, Opts),
    index_template_path(IndexTplPath),
    config:index_path(IndexPath),
    write_index_file(IndexTplPath, IndexPath, Password).
