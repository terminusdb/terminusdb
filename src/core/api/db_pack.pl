:- module(db_pack, [
              payload_repository_head_and_pack/3,
              pack_in_background/5,
              check_pack_status/2,
              try_open_pack/3,
              repository_head_layerid/2,
              pack/5,
              pack_from_context/3,
              unpack/1,
              layer_layerids/2
          ]).

:- use_module(library(terminus_store)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(core(document), [idgen_random/2]).

:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(library(apply)).
:- use_module(library(shell), [mv/2]).
:- use_module(library(plunit)).

payload_repository_head_and_pack(Data, Head, Pack) :-
    ground(Head),
    ground(Pack),
    !,
    string_concat(Head,Pack,Data).
payload_repository_head_and_pack(Data, Head, Pack) :-
    ground(Data),
    !,
    sub_string(Data, 0, 40, Remaining_Length, Head),
    sub_string(Data, 40, Remaining_Length, _, Pack).

repository_context__previous_head_option__current_repository_head__pack(Repository_Context, Repo_Head_Option, Current_Repository_Head, Pack_Option) :-
    % NOTE: Check to see that commit is in the history of Descriptor
    repository_context_to_layer(Repository_Context, Layer),
    layer_to_id(Layer, Current_Repository_Head),

    (   Repo_Head_Option = some(Current_Repository_Head)
    ->  Pack_Option = none
    ;   repository_layer_to_layerids(Layer, Repo_Head_Option, Layer_Ids),
        storage(Store),
        pack_export(Store,Layer_Ids,Pack),
        Pack_Option = some(Pack)).

get_pack_storage_path(FilePath) :-
    (   config:file_upload_storage_path(FilePath)
    ->  true
    ;   config:tmp_path(FilePath)
    ).

pack_partial_filename(Random, Part_Filename) :-
    get_pack_storage_path(FilePath),
    atomic_list_concat([FilePath, '/', Random, '.part'], Part_Filename).

pack_processed_filename(Random, Processed_Filename) :-
    get_pack_storage_path(FilePath),
    atomic_list_concat([FilePath, '/', Random], Processed_Filename).

pack_in_background(System_DB, Auth, Path, Repo_Head_Option, Resource_ID) :-
    idgen_random(Path, Unsafe_Random),
    uri_encoded(segment, Unsafe_Random, Random),
    pack_partial_filename(Random, Part_Filename),
    pack_processed_filename(Random, Processed_Filename),
    json_log_debug_formatted('~N[Debug] Opening file ~q', [Part_Filename]),
    open(Part_Filename, write, FileStream),
    thread_create(
        (    json_log_debug_formatted('~N[Debug] Generating pack for ~q', [Processed_Filename]),
             pack(System_DB, Auth, Path, Repo_Head_Option, Payload_Option),
             json_log_debug_formatted('~N[Debug] Pack created on ~q', [Path]),
             (   Payload_Option = some(Payload)
             ->  write(FileStream, Payload)
             ;   true
             ),
             json_log_debug_formatted('~N[Debug] Closing filestream', []),
             close(FileStream),
             json_log_debug_formatted('~N[Debug] Moving to processed filename', []),
             mv(Part_Filename, Processed_Filename),
             json_log_debug_formatted('~N[Debug] Moved', [])
        ), _, [detached(true)]),
    Resource_ID = Random.

check_pack_status(Resource_Id, Status) :-
    pack_processed_filename(Resource_Id, Processed_Filename),
    pack_partial_filename(Resource_Id, Partial_Filename),

    (   exists_file(Partial_Filename)
    ->  Status = busy
    ;   exists_file(Processed_Filename)
    ->  Status = complete
    ;   Status = unknown
    ).

try_open_pack(Resource_Id, Size, Stream) :-
    pack_processed_filename(Resource_Id, Processed_Filename),
    catch(
        (    open(Processed_Filename, read, Stream),
             size_file(Processed_Filename, Size)
        ),
        error(existence_error(source_sink, _), _),
        fail
    ).

pack(System_DB, Auth, Path, Repo_Head_Option, Payload_Option) :-
    atomic_list_concat([Path, '/local/_commits'], Repository_Path),
    do_or_die(
        resolve_absolute_string_descriptor(Repository_Path,
                                           Repository_Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        (repository_descriptor{} :< Repository_Descriptor),
        error(not_a_repository_descriptor(Repository_Descriptor),_)),

    do_or_die(askable_context(Repository_Descriptor, System_DB, Auth, Repository_Context),
              error(unresolvable_collection(Repository_Descriptor),_)),

    pack_from_context(Repository_Context, Repo_Head_Option, Payload_Option).

pack_from_context(Repository_Context, Repo_Head_Option, Payload_Option) :-
    assert_read_access(Repository_Context),

    repository_context__previous_head_option__current_repository_head__pack(Repository_Context, Repo_Head_Option, Current_Repository_Head, Pack_Option),
    (   some(Pack) = Pack_Option
    ->  payload_repository_head_and_pack(Payload, Current_Repository_Head, Pack),
        Payload_Option = some(Payload)
    ;   Payload_Option = none).

repository_context_to_layer(Repository_Context,Layer) :-
    [Transaction_Object] = (Repository_Context.transaction_objects),
    [Read_Write_Obj] = (Transaction_Object.instance_objects),
    Layer = (Read_Write_Obj.read).

repository_layer_to_layerids(Layer, Repo_Head_Option, Layer_Ids) :-
    % Should only be one instance object
    child_until_parents(Layer, Repo_Head_Option, Layers),
    maplist(layer_layerids, Layers, Layer_Ids_List),
    append(Layer_Ids_List, Layer_Ids).

child_until_parents(Child, some(Child_ID), []) :-
    layer_to_id(Child, Child_ID),
    !.
child_until_parents(Child, Until, [Child|Layer]) :-
    parent(Child,Parent), % has a parent
    !,
    child_until_parents(Parent, Until, Layer).
child_until_parents(Child, _Until, [Child]).

% Include self!
layer_layerids(Layer, [Self_Layer_Id|Layer_Ids]) :-
    layer_to_id(Layer,Self_Layer_Id),
    Descriptor = layer_descriptor{
                     variety: repository_descriptor,
                     instance: Layer
                 },
    findall(Layer_Id,
            ask(Descriptor,
                addition(_, layer:identifier, Layer_Id^^xsd:string)),
            Layer_Ids).

repository_head_layerid(Repository,Layer_ID) :-
    [Read_Write_Obj] = (Repository.instance_objects),
    Layer = (Read_Write_Obj.read),
    layer_to_id(Layer,Layer_ID).

layer_exists(Layer_ID) :-
    storage(Store),
    store_id_layer(Store,Layer_ID,_).

assert_fringe_is_known([]).
assert_fringe_is_known([Layer_ID|Rest]) :-
    (   layer_exists(Layer_ID)
    ->  true
    ;   throw(error(unknown_layer_reference(Layer_ID)))),
    assert_fringe_is_known(Rest).

layerids_and_parents_fringe(Layerids_Parents,Fringe) :-
    layerids_and_parents_fringe_(Layerids_Parents,Layerids_Parents,Fringe).

layerids_and_parents_fringe_([],_,[]).
layerids_and_parents_fringe_([_-some(Parent_ID)|Remainder], Layerids_Parents, Fringe) :-
    member(Parent_ID-_,Layerids_Parents),
    !,
    layerids_and_parents_fringe_(Remainder,Layerids_Parents,Fringe).
layerids_and_parents_fringe_([_-some(Parent_ID)|Remainder], Layerids_Parents, [Parent_ID|Fringe]) :-
    !,
    layerids_and_parents_fringe_(Remainder,Layerids_Parents,Fringe).
layerids_and_parents_fringe_([_-none|Remainder], Layerids_Parents, Fringe) :-
    layerids_and_parents_fringe_(Remainder,Layerids_Parents,Fringe).

layerids_unknown(Layer_Ids,Unknown_Layer_Ids) :-
    exclude(layer_exists,Layer_Ids,Unknown_Layer_Ids).

unpack(Pack) :-
   pack_layerids_and_parents(Pack,Layer_Parents),
   % all layers and their parents [Layer_ID-Parent_ID,....]
   % Are these valid? Parent is a Layer in the list or we have the parent.
   layerids_and_parents_fringe(Layer_Parents,Fringe),
   assert_fringe_is_known(Fringe),
   % Filter this list to layers we don't know about
   findall(L, member(L-_,Layer_Parents), Layer_Ids),
   layerids_unknown(Layer_Ids, Unknown_Layer_Ids),
   % Extract only these layers.
   storage(Store),
   pack_import(Store,Unknown_Layer_Ids,Pack).


:- begin_tests(pack).
:- use_module(core(util/test_utils)).
:- use_module(db_branch).

test(context_repository_head_pack,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    Origin_Path = "admin/foo",
    resolve_absolute_string_descriptor(Origin_Path, Origin_Branch_Descriptor),

    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"first commit"}, Context1),
    with_transaction(Context1,
                     once(ask(Context1, insert(foo,bar,baz))),
                     _),

    open_descriptor(database_descriptor{
                        organization_name: "admin",
                        database_name: "foo" },
                    Database_Transaction),

    repository_head(Database_Transaction, "local", Repo_Stage_1_Layer_ID),

    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"second commit"}, Context2),
    with_transaction(Context2,
                     once(ask(Context2, insert(baz,bar,foo))),
                     _),

    Destination_Path = "admin/foo/local/branch/moo",
    super_user_authority(Auth),

    branch_create(system_descriptor{}, Auth, Destination_Path, branch(Origin_Path), _),

    pack(system_descriptor{}, Auth,
        "admin/foo",
        some(Repo_Stage_1_Layer_ID),
        Payload_Option),

    % We test that the payload behaves like we expect, meaning
    % - it is splittable into a layer id and a pack
    % - the pack can be queried for its ids and parents
    some(Payload) = Payload_Option,
    payload_repository_head_and_pack(Payload, _New_Head_Layer_Id, Pack),
    pack_layerids_and_parents(Pack, _Layerids_and_Parents).

:- end_tests(pack).
