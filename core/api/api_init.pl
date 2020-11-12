:- module(api_init, [
              bootstrap_files/0,
              initialize_config/4,
              initialize_registry/0,
              initialize_database/1,
              initialize_database_with_store/2
          ]).

:- use_module(core(triple)).
:- use_module(core(util)).

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

:- dynamic template_system_instance/1.
:- dynamic system_inference/1.
:- dynamic system_schema/1.
:- dynamic repo_schema/1.
:- dynamic layer_schema/1.
:- dynamic ref_schema/1.
bootstrap_files :-
    template_system_instance_ttl(InstancePath),
    file_to_predicate(InstancePath, template_system_instance),
    system_inference_ttl(InferencePath),
    file_to_predicate(InferencePath, system_inference),
    system_schema_ttl(SchemaPath),
    file_to_predicate(SchemaPath, system_schema),
    repository_schema_ttl(RepoPath),
    file_to_predicate(RepoPath, repo_schema),
    layer_schema_ttl(LayerSchemaPath),
    file_to_predicate(LayerSchemaPath, layer_schema),
    ref_schema_ttl(RefSchemaPath),
    file_to_predicate(RefSchemaPath, ref_schema).

example_registry_path(Path) :-
    once(expand_file_search_path(template('example_registry.pl'), Path)).

template_system_instance_ttl(Path) :-
    once(expand_file_search_path(template('system_instance_template.ttl'), Path)).


system_inference_ttl(Path) :-
    once(expand_file_search_path(ontology('system_inference.owl.ttl'), Path)).

system_schema_ttl(Path) :-
    once(expand_file_search_path(ontology('system_schema.owl.ttl'), Path)).

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

initialize_database(Key) :-
    db_path(DB_Path),
    initialize_database_with_path(Key, DB_Path).

initialize_database_with_path(Key, DB_Path) :-
    make_directory_path(DB_Path),
    delete_directory_contents(DB_Path),
    initialize_server_version(DB_Path),
    open_directory_store(DB_Path, Store),
    initialize_database_with_store(Key, Store).

initialize_server_version(DB_Path) :-
    atomic_list_concat([DB_Path,'/SERVER_VERSION'],Path),
    open(Path, write, FileStream),
    writeq(FileStream, 1),
    close(FileStream).

initialize_database_with_store(Key, Store) :-
    crypto_password_hash(Key,Hash, [cost(15)]),

    template_system_instance(Template_Instance_String),
    format(string(Instance_String), Template_Instance_String, [Hash]),
    open_string(Instance_String, Instance_Stream),

    system_instance_name(Instance_Name),
    create_graph_from_turtle(Store,Instance_Name,Instance_Stream),

    system_schema(System_Schema_String),
    open_string(System_Schema_String, System_Schema_Stream),
    system_schema_name(Schema_Name),
    create_graph_from_turtle(Store,Schema_Name,System_Schema_Stream),

    system_inference(System_Inference_String),
    open_string(System_Inference_String, System_Inference_Stream),
    system_inference_name(Inference_Name),
    create_graph_from_turtle(Store,Inference_Name,System_Inference_Stream),

    layer_schema(Layer_Schema_String),
    open_string(Layer_Schema_String, Layer_Schema_Stream),
    layer_ontology(Layer_Name),
    create_graph_from_turtle(Store,Layer_Name,Layer_Schema_Stream),

    ref_schema(Ref_Schema_String),
    open_string(Ref_Schema_String, Ref_Schema_Stream),
    ref_ontology(Ref_Name),
    create_graph_from_turtle(Store,Ref_Name,Ref_Schema_Stream),

    repo_schema(Repo_Schema_String),
    open_string(Repo_Schema_String, Repo_Schema_Stream),
    repository_ontology(Repository_Name),
    create_graph_from_turtle(Store,Repository_Name,Repo_Schema_Stream).

