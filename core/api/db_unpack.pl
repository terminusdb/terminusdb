:- module(db_unpack, [
              unpack/2
          ]).

:- use_module(library(terminus_store)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_pack).

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
unpack(Repository_Descriptor, Payload) :-
    % 1. Deconstruct Payload, as head and tgz
    payload_repository_head_and_pack(Payload,New_Head,Pack),
    % 2. linear future for current repository head
    Database_Descriptor = (Repository_Descriptor.database_descriptor),
    do_or_die(create_context(Database_Descriptor, Database_Context),
              error(database_not_found(Database_Descriptor))),
    with_transaction(
        Database_Context,
        (
            repository_head(Database_Context,
                            (Repository_Descriptor.repository_name),
                            Repository_Head_Layer_Id),
            pack_layerids_and_parents(Pack,Layer_Parents),
            do_or_die(
                child_parent_linear_history(New_Head,Repository_Head_Layer_Id,Layer_Parents),
                error(not_a_linear_history_in_unpack(Layer_Parents))),
            % 3. unpack
            unpack(Pack),

            % 4. advance repository head.
            update_repository_head(Database_Context, "local", New_Head)
        ),
        _),

    true.

