:- module(db_unpack, [
              unpack/2
          ]).

:- use_module(library(terminus_store)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_pack).

child_parent_linear_history(Child,Parent,Graph) :-
    memberchk(Child-Parent, Graph),
    !.
child_parent_linear_history(Child,Parent,Graph) :-
    memberchk(Child-Intermediate, Graph),
    delete(Graph, Child-Intermediate, New_Graph),
    child_parent_linear_history(Intermediate,Parent,New_Graph).

% NOTE: There is an unpack/1 in db_pack
unpack(Branch_Descriptor, Payload) :-
    % 1. Deconstruct Payload, as head and tgz
    payload_repository_head_and_pack(Payload,New_Head,Pack),
    % 2. look at the branch, make sure it exists
    do_or_die(open_descriptor(Branch_Descriptor,_Transaction), % maybe context?
              error(unpacking_unknown_branch(Branch))),
    % 3. linear future for current repository head
    Repository_Descriptor = (Branch.repository_descriptor),
    Database_Descriptor = (Repository_Descriptor.database_descriptor),
    create_context(Database_Descriptor, Database_Context),
    repository_head(Database_Context,
                    (Repository_Descriptor.repository_name),
                    Repository_Head_Layer_Id),
    pack_layerids_and_parents(Pack,Layer_Parents),
    do_or_die(
        child_parent_linear_history(New_Head,Repository_Head_Layer_Id,Layer_Parents),
        error(not_a_linear_history_in_unpack(Layer_Parents))),
    % 4. unpack, advance repository head
    unpack(Pack).
