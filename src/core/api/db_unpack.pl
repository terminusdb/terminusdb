:- module(db_unpack, [
              unpack/4
          ]).

:- use_module(library(terminus_store)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_pack).
:- use_module(core(account)).

:- use_module(library(lists)).
:- use_module(library(tus)).
:- use_module(library(url)).
:- use_module(library(readutil)).

child_parent_linear_history(Child,Parent,Graph) :-
    memberchk(Child-some(Parent), Graph),
    !.
child_parent_linear_history(Child,Parent,Graph) :-
    memberchk(Child-some(Intermediate), Graph),
    delete(Graph, Child-some(Intermediate), New_Graph),
    child_parent_linear_history(Intermediate,Parent,New_Graph).

% error conditions:
% - history diverged
% - (note, in routes we also check that the descriptor is valid)
% NOTE: There is an unpack/1 in db_pack
unpack(System_DB, Auth, Path, Payload_or_Resource) :-

    string_concat(Path, "/local/_commits", Full_Path),
    do_or_die(
        resolve_absolute_string_descriptor(Full_Path,Repository_Descriptor),
        error(invalid_absolute_path(Full_Path),_)),
    do_or_die(
        (repository_descriptor{} :< Repository_Descriptor),
        error(not_a_repository_descriptor(Repository_Descriptor))),

    check_descriptor_auth(System_DB, Repository_Descriptor,
                          '@schema':'Action/commit_write_access',
                          Auth),

    % 0. Get Payload
    (   resource(Resource_URL) = Payload_or_Resource
        % We are TUS!
    ->  tus_uri_resource(Resource_URL, Resource),
        www_form_encode(Auth, Domain),
        tus_resource_path(Resource, Resource_Path, [domain(Domain)]),
        read_file_to_string(Resource_Path, Payload, [encoding(octet)])
        % We are a raw payload
    ;   payload(Payload) = Payload_or_Resource
    ),

    % 1. Deconstruct Payload, as head and tgz
    payload_repository_head_and_pack(Payload,New_Head,Pack),
    % 2. linear future for current repository head
    Database_Descriptor = (Repository_Descriptor.database_descriptor),
    do_or_die(create_context(Database_Descriptor, Database_Context),
              error(unresolvable_absolute_descriptor(Database_Descriptor))),

    with_transaction(
        Database_Context,
        (
            repository_head(Database_Context,
                            (Repository_Descriptor.repository_name),
                            Repository_Head_Layer_Id),
            pack_layerids_and_parents(Pack,Layer_Parents),
            do_or_die(
                child_parent_linear_history(New_Head,Repository_Head_Layer_Id,Layer_Parents),
                error(not_a_linear_history_in_unpack(Layer_Parents),_)),
            % 3. unpack
            unpack(Pack),
            % 4. advance repository head.
            update_repository_head(Database_Context, "local", New_Head)
        ),
        _),

    true.

