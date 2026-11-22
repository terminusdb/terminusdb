:- module(db_rebase, [
              rebase_on_branch/9,
              run_rebase_contract/1,
              cycle_context/4
          ]).
:- use_module(library(terminus_store)).
:- use_module(rebase_merge).
:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(library(lists)).

:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(document/schema), [is_schema_simple_class/2, schema_is_subdocument/2]).
:- use_module(core(api/api_optimize)).
:- use_module(core(api/api_document), [
                  producer_timeout/1,
                  get_commit_result/4,
                  deliver_commit_result/2,
                  api_insert_documents/9,
                  api_replace_documents/9
              ]).
:- use_module(core(document/commit_queue), [
                  enqueue_commit/2
              ]).
:- use_module(core(document/json), [
                  delete_document/3,
                  insert_document/3,
                  replace_document/2
              ]).
:- use_module(core(document/meta_commit_queue)).
:- use_module(core(transaction/descriptor), [branch_key_from_descriptor/2]).
:- use_module(core(transaction/ref_entity), [
                  apply_layer_change/6,
                  commit_id_uri/3,
                  commit_uri_to_parent_uri/3,
                  layer_uri_for_commit/4
              ]).

:- dynamic rebase_pre_database_commit_hook/1.
:- dynamic rebase_pre_lock_hook/1.
:- dynamic rebase_optimize_thread/1.

% Default hook used by the race regression test. It is a no-op unless the
% current test has registered an optimizer thread via rebase_optimize_thread/1.
rebase_pre_database_commit_hook(_Database_Transaction_Object) :-
    rebase_optimize_thread(OptimizeThread),
    % Guard: only fire if the registered thread is still alive. This prevents a
    % stale thread fact from affecting production rebases.
    catch(thread_property(OptimizeThread, status(running)), _, fail),
    !,
    thread_send_message(OptimizeThread, optimize_now).
rebase_pre_database_commit_hook(_) :- fail.

cycle_context(Context, New_Context, New_Transaction_Object, Validation_Object) :-
    [Transaction_Object] = Context.transaction_objects,
    transaction_objects_to_validation_objects([Transaction_Object], [Validation_Object]),
    validate_validation_objects([Validation_Object], Witnesses),
    (   Witnesses = []
    ->  true
    % put more stuff in error!
    ;   throw(error(schema_validation_error(Witnesses), _))),
    validation_objects_to_transaction_objects([Validation_Object], [New_Transaction_Object]),

    create_context(New_Transaction_Object, New_Context).

make_graph_validation_object_appear_as_changed(Graph_Validation_Object, New_Graph_Validation_Object) :-
    % TODO actually do a super awesome check to see if the last layer is different and if so set changed to true
    New_Graph_Validation_Object = Graph_Validation_Object.put(changed, true).

make_validation_object_appear_as_changed(Validation_Object, New_Validation_Object) :-
    maplist(make_graph_validation_object_appear_as_changed,
            Validation_Object.schema_objects,
            Schema_Objects),
    maplist(make_graph_validation_object_appear_as_changed,
            Validation_Object.instance_objects,
            Instance_Objects),
    maplist(make_graph_validation_object_appear_as_changed,
            Validation_Object.inference_objects,
            Inference_Objects),

    Objects = _{ instance_objects: Instance_Objects,
                 schema_objects: Schema_Objects,
                 inference_objects: Inference_Objects },

    New_Validation_Object = (Validation_Object.put(Objects)).

apply_commit_chain(Our_Repo_Context, _Their_Repo_Context, Us_Commit_Uri, _Author, _Auth_Object, [], [], Us_Commit_Uri, Our_Repo_Context, []) :-
    !,
    true.
apply_commit_chain(Our_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Author, Auth_Object, [Their_Commit_Id|Their_Commit_Ids], [Strategy|Strategies], Final_Commit_Uri, Return_Context, [Report|Reports]) :-
    Report = report{
                 '@type' : "api:RebaseReport",
                 'api:origin_commit': Their_Commit_Id,
                 'api:applied': Applied_Commit_Ids,
                 'api:commit_type': Commit_Application_Type
             },
    % apply the commit
    commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri), % we are renaming, Commit_Uri -> Their_Commit_Uri
    rebase_apply_commit(Our_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Their_Commit_Id, Their_Commit_Uri, Author, New_Commit_Id, New_Commit_Uri),

    % turn our repo context into a validation object
    cycle_context(Our_Repo_Context, Our_Repo_Context2, New_Our_Repo_Transaction_Object, _Our_Repo_Validation_Object),

    Our_Commit_Descriptor = commit_descriptor{
                            repository_descriptor: New_Our_Repo_Transaction_Object.descriptor,
                            commit_id: New_Commit_Id
                        },
    % todo also include database descriptor and read write objects in that list
    % should not actually matter though
    [Commit_Read_Write_Obj] = New_Our_Repo_Transaction_Object.instance_objects,
    open_descriptor(Our_Commit_Descriptor, _, Our_Commit_Transaction, [New_Our_Repo_Transaction_Object.descriptor=New_Our_Repo_Transaction_Object,Commit_Read_Write_Obj.descriptor=Commit_Read_Write_Obj], _Output_Map),

    transaction_objects_to_validation_objects([Our_Commit_Transaction], [Our_Commit_Validation_Object_Unchanged]),

    make_validation_object_appear_as_changed(Our_Commit_Validation_Object_Unchanged,
                                             Our_Commit_Validation_Object),

    % validate the branch context
    validate_validation_objects([Our_Commit_Validation_Object], Witnesses),

    % if it validates, yay! continue to the next commit (see below)

    (   Witnesses = []
    ->  (   Strategy = error
        ->  Our_Repo_Context3 = Our_Repo_Context2,
            New_Commit_Uri2 = New_Commit_Uri,
            Commit_Application_Type = "api:valid_commit",
            Applied_Commit_Ids = [New_Commit_Id]
        ;   Strategy = continue
        ->  throw(error(apply_commit(continue_on_valid_commit(Their_Commit_Id)), _))
        ;   Strategy = fixup(_Message,_Woql)
        ->  throw(error(apply_commit(fixup_on_valid_commit(Their_Commit_Id)), _)))
    ;   (   Strategy = error
        ->  throw(error(apply_commit(schema_validation_error(Their_Commit_Id, Witnesses)), _))
        ;   Strategy = continue
        ->  invalidate_commit(Our_Repo_Context2, New_Commit_Id),
            cycle_context(Our_Repo_Context2, Our_Repo_Context3, _, _),
            New_Commit_Uri2 = New_Commit_Uri,
            Commit_Application_Type = "api:invalid_commit",
            Applied_Commit_Ids = [New_Commit_Id]
        ;   Strategy = fixup(Message, Woql)
        ->  create_context(Our_Commit_Transaction, Our_Commit_Fixup_Context_Without_Auth),
            put_dict(_{authorization: Auth_Object,
                       commit_info: commit_info{author:Author,message:Message}},
                     Our_Commit_Fixup_Context_Without_Auth,
                     Our_Commit_Fixup_Context),
            compile_query(Woql, Prog, Our_Commit_Fixup_Context, Our_Commit_Fixup_Context2),
            forall(woql_compile:Prog, true),
            % turn context into validation
            Our_Commit_Fixup_Context2.transaction_objects = [Commit_Fixup_Transaction_Object],
            transaction_objects_to_validation_objects([Commit_Fixup_Transaction_Object], [Commit_Fixup_Validation_Object]),
            % validate
            validate_validation_objects([Commit_Fixup_Validation_Object], Fixup_Witnesses),
            (   Fixup_Witnesses = []
            ->  true
            ;   throw(error(apply_commit(fixup_error(Their_Commit_Id, Fixup_Witnesses)), _))),

            % commit validation
            commit_commit_validation_object(Commit_Fixup_Validation_Object, _Parent_Transaction_List, New_Commit_Id2, New_Commit_Uri2),
            % write commit object into our repo context
            cycle_context(Our_Repo_Context2, Our_Repo_Context3, _, _),

            Commit_Application_Type = fixup,
            Applied_Commit_Ids = [New_Commit_Id, New_Commit_Id2])
    ),

    apply_commit_chain(Our_Repo_Context3, Their_Repo_Context, New_Commit_Uri2, Author, Auth_Object, Their_Commit_Ids, Strategies, Final_Commit_Uri, Return_Context, Reports).

% rebase_apply_commit(+Our_Repo_Context, +Their_Repo_Context, +Us_Commit_Uri, +Their_Commit_Id, +Their_Commit_Uri, +Author, -New_Commit_Id, -New_Commit_Uri)
%
% A commit that rewrites List or Array cells cannot replay as a raw triple
% delta: cells get fresh random ids per rewrite and subdocument ids embed
% element positions, so the delta cannot line up with the target branch.
% Such commits merge each affected document three-way; all others replay raw.
rebase_apply_commit(Us_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Their_Commit_Id, Their_Commit_Uri, Author, New_Commit_Id, New_Commit_Uri) :-
    (   collection_instance_layer(Their_Repo_Context, Their_Commit_Uri, Their_Layer),
        merged_roots(Us_Repo_Context, Us_Commit_Uri, Their_Layer, Roots, Cell_Set)
    ->  apply_commit_merged(Us_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Their_Commit_Id, Their_Commit_Uri, Author, Their_Layer, Roots, Cell_Set, New_Commit_Id, New_Commit_Uri)
    ;   apply_commit_on_commit(Us_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Their_Commit_Uri, Author, New_Commit_Id, New_Commit_Uri)).

collection_instance_layer(Askable, Commit_Uri, Layer) :-
    layer_uri_for_commit(Askable, Commit_Uri, instance, Layer_Uri),
    layer_id_uri(Askable, Layer_Id, Layer_Uri),
    triple_store(Store),
    store_id_layer(Store, Layer_Id, Layer),
    layer_touches_collections(Layer).

delta_edges(Layer, Added, Deleted) :-
    RWO = read_write_obj{read: Layer},
    findall(S-P-O, xrdf_added([RWO], S, P, O), Added),
    findall(S-P-O, xrdf_deleted([RWO], S, P, O), Deleted).

% merged_roots(+Us_Repo_Context, +Us_Commit_Uri, +Their_Layer, -Roots, -Cell_Set)
%
% Fails when cell ownership cannot be resolved, in which case the caller
% falls back to raw replay.
merged_roots(Us_Repo_Context, Us_Commit_Uri, Their_Layer, Roots, Cell_Set) :-
    delta_edges(Their_Layer, Added, Deleted),
    append(Added, Deleted, Edges),
    commit_id_uri(Us_Repo_Context, Us_Commit_Id, Us_Commit_Uri),
    commit_query_context(Us_Repo_Context, Us_Commit_Id, Head_Transaction, _),
    database_schema(Head_Transaction, Schema),
    affected_roots(Edges, Schema, Their_Layer, Roots, Cell_Set).

commit_query_context(Repo_Context, Commit_Id, Transaction, Context) :-
    [TO] = Repo_Context.transaction_objects,
    Descriptor = commit_descriptor{repository_descriptor: TO.descriptor, commit_id: Commit_Id},
    (   catch(open_descriptor(Descriptor, _, Transaction, [], _), _, fail)
    ->  true
    ;   [Commit_RWO] = TO.instance_objects,
        open_descriptor(Descriptor, _, Transaction, [TO.descriptor=TO, Commit_RWO.descriptor=Commit_RWO], _)),
    create_context(Transaction, Context).

% affected_roots(+Edges, +Schema, +Layer, -Roots, -Cell_Set)
%
% Roots are the typed non-subdocument nodes whose delta edges reach the
% collection cells; Cell_Set holds the cells as a dict for membership tests.
% Fails when any cell has no unique owner, in which case the caller falls
% back to raw replay.
affected_roots(Edges, Schema, Layer, Roots, Cell_Set) :-
    findall(Cell, (member(Cell-P-_, Edges), collection_predicate(P)), Cells0),
    sort(Cells0, Cells),
    Cells \== [],
    outgoing_map(Edges, Out_Map),
    findall(Node, member(Node-_-_, Edges), Nodes0),
    sort(Nodes0, Nodes),
    findall(Candidate, (member(Candidate, Nodes),
                        node_type_layer(Layer, Candidate, Type),
                        is_schema_simple_class(Schema, Type),
                        \+ schema_is_subdocument(Schema, Type)),
            Candidates0),
    sort(Candidates0, Candidates),
    cell_set(Cells, Cell_Set),
    maplist(owned_cells(Out_Map, Cell_Set), Candidates, Owned_Lists),
    append(Owned_Lists, Owned),
    sort(Owned, Owned_Sorted),
    length(Owned, Owned_Length),
    length(Cells, Cells_Length),
    Owned_Sorted == Cells,
    Owned_Length = Cells_Length,
    maplist(candidate_owned, Candidates, Owned_Lists, Candidate_Owned),
    findall(Root, (member(Root-Owned, Candidate_Owned), Owned \== []), Roots0),
    sort(Roots0, Roots),
    Roots \== [].

% A rewritten document keeps its rdf:type triple out of the delta, so types
% resolve against the commit layer, which reads as its full stack.
node_type_layer(Layer, Node, Type) :-
    rdf_type_uri(Type_Predicate),
    xrdf([read_write_obj{read: Layer}], Node, Type_Predicate, Type).

candidate_owned(Candidate, Owned, Candidate-Owned).

outgoing_map(Edges, Map) :-
    foldl(outgoing_step, Edges, _{}, Map).

outgoing_step(S-_-O, Map_In, Map_Out) :-
    (   get_dict(S, Map_In, Old)
    ->  put_dict(S, Map_In, [O|Old], Map_Out)
    ;   put_dict(S, Map_In, [O], Map_Out)).

cell_set(Cells, Set) :-
    foldl(cell_set_step, Cells, _{}, Set).

cell_set_step(Cell, Set_In, Set_Out) :-
    (   get_dict(Cell, Set_In, _)
    ->  Set_Out = Set_In
    ;   put_dict(Cell, Set_In, true, Set_Out)).

owned_cells(Out_Map, Cell_Set, Start, Owned) :-
    dfs_cells(Out_Map, Cell_Set, Start, _{}, [], _Visited, Owned).

% Literal objects are leaves: they are compounds, not dict keys.
dfs_cells(_, _, Node, Visited, Found, Visited, Found) :-
    \+ atom(Node),
    !.
dfs_cells(Map, Cell_Set, Node, Visited_In, Found_In, Visited_Out, Found_Out) :-
    (   get_dict(Node, Visited_In, _)
    ->  Visited_Out = Visited_In,
        Found_Out = Found_In
    ;   put_dict(Node, Visited_In, true, Visited1),
        (   get_dict(Node, Cell_Set, _)
        ->  Found1 = [Node|Found_In]
        ;   Found1 = Found_In),
        (   get_dict(Node, Map, Objects)
        ->  dfs_cells_all(Map, Cell_Set, Objects, Visited1, Found1, Visited_Out, Found_Out)
        ;   Visited_Out = Visited1,
            Found_Out = Found1)).

dfs_cells_all(_, _, [], Visited, Found, Visited, Found).
dfs_cells_all(Map, Cell_Set, [Object|Objects], Visited_In, Found_In, Visited_Out, Found_Out) :-
    dfs_cells(Map, Cell_Set, Object, Visited_In, Found_In, Visited1, Found1),
    dfs_cells_all(Map, Cell_Set, Objects, Visited1, Found1, Visited_Out, Found_Out).

apply_commit_merged(Us_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Their_Commit_Id, Their_Commit_Uri, Author, Their_Layer, Roots, Cell_Set, New_Commit_Id, New_Commit_Uri) :-
    commit_id_to_metadata(Their_Repo_Context, Their_Commit_Id, _Their_Author, Message, _Their_Timestamp),
    Commit_Info = commit_info{author: Author, message: Message,
                              merge_source: _{operation: rebase,
                                              commits: [Their_Commit_Uri]}},
    commit_query_context(Us_Repo_Context, Their_Commit_Id, _Their_Transaction, Their_Context),
    (   commit_uri_to_parent_uri(Us_Repo_Context, Their_Commit_Uri, Parent_Uri),
        commit_id_uri(Us_Repo_Context, Parent_Id, Parent_Uri)
    ->  commit_query_context(Us_Repo_Context, Parent_Id, _Base_Transaction, Base_Context)
    ;   Base_Context = none),
    commit_id_uri(Us_Repo_Context, Us_Commit_Id, Us_Commit_Uri),
    commit_query_context(Us_Repo_Context, Us_Commit_Id, Head_Transaction, Head_Context),
    database_schema(Head_Transaction, Schema),
    database_prefixes(Head_Context, Prefixes),
    findall(Action, merged_document_action(Base_Context, Their_Context, Head_Context, Schema, Prefixes, Roots, Action), Actions),
    apply_layer_change(Us_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Their_Commit_Uri, schema, Schema_Layer_Uri),
    build_merged_instance_layer(Us_Repo_Context, Us_Commit_Uri, Their_Layer, Roots, Cell_Set, Head_Context, Actions, Instance_Layer_Uri),
    insert_child_commit_object(Us_Repo_Context, Us_Commit_Uri, Schema_Layer_Uri, Instance_Layer_Uri, Commit_Info, New_Commit_Id, New_Commit_Uri).

% merged_document_action(+Base_Context, +Their_Context, +Head_Context, +Schema, +Prefixes, +Roots, -Action)
%
% Action is doc(Id, Merged, Kind) with Kind in delete/insert/replace, or
% fails (no action) when the merged document equals head.
merged_document_action(Base_Context, Their_Context, Head_Context, Schema, Prefixes, Roots, Action) :-
    member(Root, Roots),
    get_document_maybe(Head_Context, Root, OursDoc),
    get_document_maybe(Their_Context, Root, TheirsDoc),
    (   Base_Context == none
    ->  BaseDoc = missing
    ;   get_document_maybe(Base_Context, Root, BaseDoc)),
    merge_document(Schema, Prefixes, BaseDoc, OursDoc, TheirsDoc, Merged),
    \+ (   Merged == missing, OursDoc == missing
       ;   is_dict(Merged), is_dict(OursDoc),
           normalize_document(Schema, Prefixes, OursDoc, OursStripped),
           OursStripped == Merged),
    doc_action(Merged, OursDoc, Root, Action).

doc_action(Merged, OursDoc, Root, Action) :-
    (   Merged == missing
    ->  Kind = delete
    ;   OursDoc == missing
    ->  Kind = insert
    ;   Kind = replace),
    document_id(Merged, OursDoc, Root, Id),
    Action = doc(Id, Merged, Kind).

document_id(Merged, _, _, Id) :- is_dict(Merged), get_dict('@id', Merged, Id), !.
document_id(_, OursDoc, _, Id) :- is_dict(OursDoc), get_dict('@id', OursDoc, Id), !.
document_id(_, _, Root, Root).

get_document_maybe(Context, Id, Document) :-
    (   catch(get_document(Context, Id, Document), _, fail)
    ->  true
    ;   Document = missing).

% build_merged_instance_layer(+Us_Repo_Context, +Us_Commit_Uri, +Their_Layer, +Roots, +Cell_Set, +Head_Context, +Actions, -Instance_Layer_Uri)
%
% One instance layer for the replayed commit: the commit's raw delta minus
% the affected document closures, plus document-level writes of the merged
% documents, all in a single layer builder.
build_merged_instance_layer(Us_Repo_Context, Us_Commit_Uri, Their_Layer, Roots, Cell_Set, Head_Context, Actions, Instance_Layer_Uri) :-
    delta_edges(Their_Layer, Added, Deleted),
    triple_store(Store),
    (   layer_uri_for_commit(Us_Repo_Context, Us_Commit_Uri, instance, Us_Layer_Uri)
    ->  layer_id_uri(Us_Repo_Context, Us_Layer_Id, Us_Layer_Uri),
        store_id_layer(Store, Us_Layer_Id, Us_Layer),
        open_write(Us_Layer, Builder)
    ;   open_write(Store, Builder)),
    [Head_TO] = Head_Context.transaction_objects,
    [Head_RWO] = Head_TO.instance_objects,
    Write_RWO = Head_RWO.put(write, Builder),
    Write_TO = Head_TO.put(instance_objects, [Write_RWO]),
    create_context(Write_TO, Write_Context),
    forall(member(S-P-O, Added),
           (   skip_subject(S, Roots, Cell_Set)
           ->  true
           ;   insert(Write_RWO, S, P, O, _))),
    forall(member(S-P-O, Deleted),
           (   skip_subject(S, Roots, Cell_Set)
           ->  true
           ;   delete(Write_RWO, S, P, O, _))),
    forall(member(doc(Id, Merged, Kind), Actions),
           apply_doc_action(Write_Context, Id, Merged, Kind)),
    nb_commit(Builder, New_Layer),
    store_id_layer(Store, New_Layer_Id, New_Layer),
    insert_layer_object(Us_Repo_Context, New_Layer_Id, Instance_Layer_Uri).

skip_subject(S, Roots, _) :-
    member(Root, Roots),
    atom_concat(Root, Suffix, S),
    (   Suffix == ''
    ;   sub_atom(Suffix, 0, 1, _, '/')),
    !.
skip_subject(S, _Roots, Cell_Set) :-
    get_dict(S, Cell_Set, _),
    !.

apply_doc_action(Write_Context, Id, _, delete) :-
    !,
    delete_document(Write_Context, false, Id).
apply_doc_action(Write_Context, Id, Merged, insert) :-
    !,
    insert_document(Write_Context, Merged, Id).
apply_doc_action(Write_Context, _, Merged, replace) :-
    replace_document(Write_Context, Merged).

% segment_writes_schema(+Repo_Context, +Commit_Ids) is semidet.
%
% Every commit stamps the effective schema layer — content-addressed, so
% unchanged schema reuses the parent's URI. A commit wrote schema iff its
% schema layer differs from its parent's (or it has none and has a layer).
segment_writes_schema(Repo_Context, Commit_Ids) :-
    member(Commit_Id, Commit_Ids),
    commit_id_uri(Repo_Context, Commit_Id, Commit_Uri),
    layer_uri_for_commit(Repo_Context, Commit_Uri, schema, Schema_Uri),
    (   commit_uri_to_parent_uri(Repo_Context, Commit_Uri, Parent_Uri)
    ->  \+ layer_uri_for_commit(Repo_Context, Parent_Uri, schema, Schema_Uri)
    ;   true),
    !.

create_strategies([], _Strategy_Map, []).
create_strategies([Commit_ID|Their_Branch_Path], Strategy_Map, [Strategy|Strategies]) :-
    (   memberchk(Commit_ID=Strategy, Strategy_Map)
    ->  true
    ;   Strategy = error),
    create_strategies(Their_Branch_Path, Strategy_Map, Strategies).

rebase_on_branch(System_DB, Auth, Our_Branch_Path, Their_Branch_Path, Author, Strategy_Map, Optional_Common_Commit_Id, Their_Branch_History, Reports) :-
    do_rebase_with_retry(System_DB, Auth, Our_Branch_Path, Their_Branch_Path,
                         Author, Strategy_Map, Optional_Common_Commit_Id,
                         Their_Branch_History, Reports, 10).

% do_rebase_with_retry/10 submits a prepared rebase contract to the branch
% commit queue and retries synchronously if the worker rejects it because the
% target branch head changed. This follows the same contract-and-retry pattern
% used by regular document commits.
do_rebase_with_retry(System_DB, Auth, Our_Branch_Path, Their_Branch_Path,
                     Author, Strategy_Map, Optional_Common_Commit_Id,
                     Their_Branch_History, Reports, Retries) :-
    rebase_on_branch_inner(System_DB, Auth, Our_Branch_Path, Their_Branch_Path,
                           Author, Strategy_Map, Optional_Common_Commit_Id,
                           Their_Branch_History, Contract),
    submit_rebase_contract(Contract, Result),
    (   Result = rebase_success(Reports)
    ->  true
    ;   Result = reject(rebase_target_branch_changed),
        Retries > 1
    ->  New_Retries is Retries - 1,
        do_rebase_with_retry(System_DB, Auth, Our_Branch_Path, Their_Branch_Path,
                             Author, Strategy_Map, Optional_Common_Commit_Id,
                             Their_Branch_History, Reports, New_Retries)
    ;   Result = reject(rebase_target_branch_changed)
    ->  throw(error(rebase_target_branch_changed(Our_Branch_Path), _))
    ;   Result = error(Error)
    ->  throw(error(Error, _))
    ;   Result = timeout(RequestId)
    ->  throw(error(commit_queue_timeout(RequestId), _))
    ).

rebase_on_branch_inner(System_DB, Auth, Our_Branch_Path, Their_Branch_Path,
                       Author, Strategy_Map, Optional_Common_Commit_Id,
                       Their_Branch_History, Contract) :-
    do_or_die(
        resolve_absolute_string_descriptor(Our_Branch_Path, Our_Branch_Descriptor),
        error(invalid_target_absolute_path(Our_Branch_Path),_)),

    check_descriptor_auth(System_DB, Our_Branch_Descriptor, '@schema':'Action/commit_read_access', Auth),
    check_descriptor_auth(System_DB, Our_Branch_Descriptor, '@schema':'Action/instance_write_access', Auth),

    do_or_die(
        (branch_descriptor{} :< Our_Branch_Descriptor),
        error(rebase_requires_target_branch(Our_Branch_Descriptor),_)),

    do_or_die(
        resolve_absolute_string_descriptor(Their_Branch_Path, Their_Ref_Descriptor),
        error(invalid_source_absolute_path(Their_Branch_Path),_)),

    do_or_die(
        (   branch_descriptor{} :< Their_Ref_Descriptor
        ->  true
        ;   commit_descriptor{} :< Their_Ref_Descriptor),
        error(rebase_requires_source_branch(Their_Ref_Descriptor),_)),

    check_descriptor_auth(System_DB, Their_Ref_Descriptor, '@schema':'Action/commit_read_access', Auth),

    Our_Repo_Descriptor = (Our_Branch_Descriptor.repository_descriptor),
    Their_Repo_Descriptor = (Their_Ref_Descriptor.repository_descriptor),

    prepare_rebase_contract(System_DB, Auth, Our_Branch_Descriptor,
                            Their_Ref_Descriptor,
                            Our_Repo_Descriptor, Their_Repo_Descriptor,
                            Our_Branch_Path, Their_Branch_Path, Author,
                            Strategy_Map, Optional_Common_Commit_Id,
                            Contract, Their_Branch_History).

% prepare_rebase_contract/13 does all the rebase work that can safely happen
% outside the branch lock: history computation, commit copying, and replay.
% It returns a rebase_contract dict that the worker will verify and commit.
prepare_rebase_contract(System_DB, Auth, Our_Branch_Descriptor,
                        Their_Ref_Descriptor,
                        Our_Repo_Descriptor, Their_Repo_Descriptor,
                        Our_Branch_Path, Their_Branch_Path, Author,
                        Strategy_Map, Optional_Common_Commit_Id,
                        Contract, Their_Branch_History) :-

    do_or_die(
        create_context(Our_Repo_Descriptor, Our_Repo_Context),
        error(unresolvable_target_descriptor(Our_Repo_Descriptor))),
    do_or_die(
        create_context(Their_Repo_Descriptor, Their_Repo_Context),
        error(unresolvable_source_descriptor(Their_Repo_Descriptor))),

    do_or_die(
        branch_name_uri(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Our_Branch_Uri),
        error(target_branch_not_found(Our_Branch_Path), _)),
    do_or_die(
        descriptor_commit_id_uri(Their_Repo_Context, Their_Ref_Descriptor, Their_Commit_Id, Their_Commit_Uri),
        error(source_branch_not_found(Their_Branch_Path), _)),

    (   branch_head_commit(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Our_Commit_Uri),
        commit_id_uri(Our_Repo_Context, Our_Commit_Id, Our_Commit_Uri),
        commit_type(Our_Repo_Context, Our_Commit_Uri, 'http://terminusdb.com/schema/ref#ValidCommit')
    ->  Original_Our_Head = some(Our_Commit_Id),
        (   most_recent_common_ancestor(Our_Repo_Context, Their_Repo_Context, Our_Commit_Id, Their_Commit_Id, Optional_Common_Commit_Id, Our_Branch_History, Their_Branch_History)
        ->  true
        % We have no common history
        ;   Optional_Common_Commit_Id = none,
            commit_uri_to_history_commit_ids(Our_Repo_Context, Our_Commit_Uri, Our_Branch_History),
            commit_uri_to_history_commit_ids(Their_Repo_Context, Their_Commit_Uri, Their_Branch_History))
    % Branch head commit may not exist if our branch is empty...
    ;   Optional_Common_Commit_Id = none,
        Original_Our_Head = none,
        Our_Branch_History = [],
        commit_uri_to_history_commit_ids(Their_Repo_Context, Their_Commit_Uri, Their_Branch_History)
    ),

    % Rebase grafts the source commits wholesale and replays ours — schema
    % content lands on the target whenever either segment contains a commit
    % that wrote schema, which is what schema_write_access is charged for.
    % A commit references a schema layer only when it wrote schema, so a
    % segment carries schema iff some commit in it does.
    (   (   segment_writes_schema(Their_Repo_Context, Their_Branch_History)
        ;   segment_writes_schema(Our_Repo_Context, Our_Branch_History))
    ->  check_descriptor_auth(System_DB, Our_Branch_Descriptor,
                              '@schema':'Action/schema_write_access', Auth)
    ;   true),

    % copy commits from their repo into ours, ensuring we got the latest
    copy_commits(Their_Repo_Context, Our_Repo_Context, Their_Commit_Id),
    cycle_context(Our_Repo_Context, Our_Repo_Context2, _, _),

    % take their commit uri as the top on which we're gonna apply_commit_chain
    % list of commits to apply is ours since common

    create_strategies(Our_Branch_History, Strategy_Map, Strategies),

    (   Our_Branch_History = []
    % yay we're done! All commits are known to us, no need to do a thing
    ->  Semifinal_Context = Our_Repo_Context2,
        Final_Commit_Uri = Their_Commit_Uri,
        Reports = []
    ;   catch(
            apply_commit_chain(Our_Repo_Context2,
                               Their_Repo_Context,
                               Their_Commit_Uri,
                               Author,
                               Auth,
                               Our_Branch_History,
                               Strategies,
                               Final_Commit_Uri,
                               Semifinal_Context,
                               Reports),
            error(apply_commit(Error), _),
            throw(error(rebase_commit_application_failed(Error,Our_Branch_History), _))
        )
    ),

    Database_Descriptor = Our_Branch_Descriptor.repository_descriptor.database_descriptor,
    database_descriptor_key(Database_Descriptor, Database_Key),

    Contract = rebase_contract{
        package_type: rebase,
        branch_key: Our_Branch_Path,
        database_key: Database_Key,
        our_repo_descriptor: Our_Repo_Descriptor,
        our_branch_descriptor: Our_Branch_Descriptor,
        our_branch_path: Our_Branch_Path,
        original_head: Original_Our_Head,
        semifinal_context: Semifinal_Context,
        our_branch_uri: Our_Branch_Uri,
        final_commit_uri: Final_Commit_Uri,
        reports: Reports
    }.

% submit_rebase_contract/2 submits a rebase contract to the branch commit queue
% and waits for the worker result. If the commit worker pool is not running
% (e.g. during PLUnit tests), the contract is executed synchronously in the
% request thread as a fallback. The result uses the same commit_result envelope
% as regular commits when queued, or the raw result term when synchronous.
submit_rebase_contract(Contract, Result) :-
    (   commit_queue:multi_purpose_worker_thread(_)
    ->  submit_rebase_contract_queued(Contract, Result)
    ;   % No worker pool is running (e.g. PLUnit tests). Run the final commit
        % window synchronously in the request thread, but still under the
        % meta_commit_lock so the branch relink and repository head update are
        % serialized. Ignore the result of the hook (used when race testing).
        get_dict(database_key, Contract, Database_Key),
        ignore(rebase_pre_lock_hook(Contract)),
        with_meta_commit_lock(
            Database_Key,
            execute_rebase_contract(Contract, Result)
        )
    ).

submit_rebase_contract_queued(Contract, Result) :-
    get_dict(branch_key, Contract, BranchKey),
    message_queue_create(ReplyQueue, []),
    gensym(request, RequestId),
    ContractWithReply = Contract.put(_{reply_queue: ReplyQueue, request_id: RequestId}),
    commit_queue:enqueue_commit(BranchKey, ContractWithReply),
    producer_timeout(ProducerTimeout),
    (   get_commit_result(ReplyQueue, RequestId, ProducerTimeout, Result)
    ->  true
    ;   Result = timeout(RequestId)
    ),
    catch(message_queue_destroy(ReplyQueue), _, true).

% run_rebase_contract(+Package) is det.
%
% Called by the commit worker when it dequeues a rebase_contract. The worker
% already holds the branch lock; this predicate acquires the meta_commit_lock,
% executes the final commit window, and sends the result back to the reply queue.
run_rebase_contract(Package) :-
    get_dict(database_key, Package, Database_Key),
    catch(
        (   with_meta_commit_lock(
                Database_Key,
                (   execute_rebase_contract(Package, Result)
                ->  true
                ;   Result = error(unexpected_rebase_failure)
                )
            ),
            deliver_commit_result(Package, Result)
        ),
        Error,
        deliver_commit_result(Package, error(Error))
    ).

execute_rebase_contract(Package, Result) :-
    get_dict(our_repo_descriptor, Package, Our_Repo_Descriptor),
    get_dict(our_branch_descriptor, Package, Our_Branch_Descriptor),
    get_dict(our_branch_path, Package, Our_Branch_Path),
    get_dict(original_head, Package, Original_Our_Head),
    get_dict(semifinal_context, Package, Semifinal_Context),
    get_dict(our_branch_uri, Package, Our_Branch_Uri),
    get_dict(final_commit_uri, Package, Final_Commit_Uri),
    get_dict(reports, Package, Reports),
    catch(
        (   rebase_commit_window(Our_Repo_Descriptor, Our_Branch_Descriptor,
                                 Our_Branch_Path, Original_Our_Head,
                                 Semifinal_Context, Our_Branch_Uri, Final_Commit_Uri),
            Result = rebase_success(Reports)
        ),
        error(rebase_target_branch_changed(_), _),
        Result = reject(rebase_target_branch_changed)
    ).

% register_rebase_head_in_change_window(+BranchDescriptor, +RepoContext, +HeadCommitUri)
%
% Rebase creates new commits on the target branch outside the normal commit
% validation path, so the change window does not see them. The change window enables
% verification of writes against what changed recently. Without this, the
% next commit to the branch tries to open a commit window with a parent that
% is not present in the window, falls into a retry loop, and
% effectively stalls the database. We register the new head (with an
% empty change set) so the window knows the current branch head and commits
% can open their guards immediately.
register_rebase_head_in_change_window(BranchDescriptor, RepoContext, HeadCommitUri) :-
    branch_key_from_descriptor(BranchDescriptor, BranchKey),
    commit_id_uri(RepoContext, HeadCommitId, HeadCommitUri),
    (   commit_uri_to_parent_uri(RepoContext, HeadCommitUri, ParentCommitUri)
    ->  commit_id_uri(RepoContext, ParentCommitId, ParentCommitUri)
    ;   ParentCommitId = none
    ),
    (   atom(HeadCommitId)
    ->  HeadCommitIdAtom = HeadCommitId
    ;   atom_string(HeadCommitIdAtom, HeadCommitId)
    ),
    (   ParentCommitId = none
    ->  ParentCommitIdArg = none
    ;   atom_string(ParentCommitIdArg, ParentCommitId)
    ),
    (   catch(
            '$change_window':register_commit(BranchKey, HeadCommitIdAtom, ParentCommitIdArg,
                                             none, none, [], []),
            Error,
            json_log_error_formatted("register_rebase_head_in_change_window failed for ~w: ~w",
                                     [BranchKey, Error])
        )
    ->  true
    ;   fail
    ).

rebase_commit_window(Our_Repo_Descriptor, Our_Branch_Descriptor, Our_Branch_Path,
                     Original_Our_Head, Semifinal_Context, Our_Branch_Uri, Final_Commit_Uri) :-
    % The target branch head may have moved between our history computation and
    % the moment we acquired the lock. Re-read it here and fail with a
    % structured error so the caller can retry with the new head.
    (   branch_head_commit(Our_Repo_Descriptor, Our_Branch_Descriptor.branch_name, Current_Our_Commit_Uri),
        commit_id_uri(Our_Repo_Descriptor, Current_Our_Commit_Id, Current_Our_Commit_Uri),
        commit_type(Our_Repo_Descriptor, Current_Our_Commit_Uri, 'http://terminusdb.com/schema/ref#ValidCommit')
    ->  Current_Our_Head = some(Current_Our_Commit_Id)
    ;   Current_Our_Head = none
    ),
    (   Current_Our_Head = Original_Our_Head
    ->  true
    ;   throw(error(rebase_target_branch_changed(Our_Branch_Path), _))
    ),

    ignore(unlink_commit_object_from_branch(Semifinal_Context, Our_Branch_Uri)),

    link_commit_object_to_branch(Semifinal_Context, Our_Branch_Uri, Final_Commit_Uri),

    register_rebase_head_in_change_window(Our_Branch_Descriptor, Semifinal_Context,
                                          Final_Commit_Uri),

    cycle_context(Semifinal_Context, _Final_Context, Transaction_Object, _),

    Repo_Name = Transaction_Object.descriptor.repository_name,
    [Read_Write_Obj] = Transaction_Object.instance_objects,
    Layer = Read_Write_Obj.read,
    layer_to_id(Layer, Layer_Id),
    Database_Transaction_Object = (Transaction_Object.parent),

    update_repository_head(Database_Transaction_Object, Repo_Name, Layer_Id),

    % Test hook: allows unit tests to interleave an optimization between
    % the repository head update and the database commit, which is the
    % exact window in which the auto-optimizer races with the rebase's
    % _meta commit. The lock is recursive, so the inner run_transactions
    % can acquire it again safely.
    (   rebase_pre_database_commit_hook(Database_Transaction_Object)
    ->  true
    ;   true
    ),

    run_transactions([Database_Transaction_Object], true, _).

:- begin_tests(rebase, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(document)).

:- use_module(db_create).
:- use_module(db_branch).

test(rebase_target_branch_does_not_exist,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State)),
      throws(error(target_branch_not_found("admin/foo/local/branch/does_not_exist"), _))
     ])
:-
    super_user_authority(Auth),
    rebase_on_branch(system_descriptor{},
                     Auth,
                     "admin/foo/local/branch/does_not_exist",
                     "admin/foo/local/branch/other",
                     "rebaser",
                     [], _, _, []).

test(rebase_source_branch_does_not_exist,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State)),
      throws(error(source_branch_not_found("admin/foo/local/branch/does_not_exist"), _))
     ])
:-
    super_user_authority(Auth),
    rebase_on_branch(system_descriptor{},
                     Auth,
                     "admin/foo/local/branch/main",
                     "admin/foo/local/branch/does_not_exist",
                     "rebaser",
                     [], _, _, []).

test(rebase_fast_forward,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor("admin/foo", Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                    _),

    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),

    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    super_user_authority(Auth),

    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Second_Path, "rebaser",  [], some(Common_Commit_Id), Their_Applied_Commit_Ids, []),
    Repo_Descriptor = Master_Descriptor.repository_descriptor,

    commit_id_to_metadata(Repo_Descriptor, Common_Commit_Id, _, "commit a", _),

    [Old_Commit_B_Id, Old_Commit_C_Id] = Their_Applied_Commit_Ids,
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_B_Id, _, "commit b", _),
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_C_Id, _, "commit c", _),

    branch_head_commit(Repo_Descriptor, "main", Commit_C_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_C_Uri, Commit_B_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_B_Uri, Commit_A_Uri),


    commit_uri_to_metadata(Repo_Descriptor, Commit_A_Uri, _, "commit a", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_B_Uri, _, "commit b", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_C_Uri, _, "commit c", _),

    findall(t(S,P,O),
            ask(Master_Descriptor,
                t(S,P,O)),
            Triples_Unsorted),

    sort(Triples_Unsorted, Triples),
    [t(d,e,f),
     t(g,h,i)] = Triples,

    true.

test(rebase_fast_forward_from_commit,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor("admin/foo", Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                    _),

    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),

    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    super_user_authority(Auth),

    descriptor_commit_id_uri((Second_Descriptor.repository_descriptor),
                             Second_Descriptor,
                             Commit_Id,
                             _Commit_Uri),

    atomic_list_concat(['admin/foo/local/commit/',Commit_Id], Commit_Path),

    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Commit_Path, "rebaser",  [], some(Common_Commit_Id), Their_Applied_Commit_Ids, []),
    Repo_Descriptor = Master_Descriptor.repository_descriptor,

    commit_id_to_metadata(Repo_Descriptor, Common_Commit_Id, _, "commit a", _),

    [Old_Commit_B_Id, Old_Commit_C_Id] = Their_Applied_Commit_Ids,
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_B_Id, _, "commit b", _),
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_C_Id, _, "commit c", _),

    branch_head_commit(Repo_Descriptor, "main", Commit_C_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_C_Uri, Commit_B_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_B_Uri, Commit_A_Uri),


    commit_uri_to_metadata(Repo_Descriptor, Commit_A_Uri, _, "commit a", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_B_Uri, _, "commit b", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_C_Uri, _, "commit c", _),

    findall(t(S,P,O),
            ask(Master_Descriptor,
                t(S,P,O)),
            Triples_Unsorted),

    sort(Triples_Unsorted, Triples),
    [t(d,e,f),
     t(g,h,i)] = Triples,

    true.


test(rebase_divergent_history,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),
    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    % we're also doing a commit on the original branch, to create a divergent history
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit d"}, Master_Context2),
    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         insert(j,k,l)),
                     _),

    Repo_Descriptor = Master_Descriptor.repository_descriptor,
    branch_head_commit(Repo_Descriptor, "main", Old_Commit_D_Uri),
    commit_id_uri(Repo_Descriptor, Old_Commit_D_Id, Old_Commit_D_Uri),

    super_user_authority(Auth),
    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Second_Path, "rebaser", [], some(Common_Commit_Id), Their_Commit_Ids, Reports),

    commit_id_to_metadata(Repo_Descriptor, Common_Commit_Id, _, "commit a", _),

    branch_head_commit(Repo_Descriptor, "main", New_Commit_D_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, New_Commit_D_Uri, Commit_C_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_C_Uri, Commit_B_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_B_Uri, Commit_A_Uri),

    commit_uri_to_metadata(Repo_Descriptor, Commit_A_Uri, _, "commit a", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_B_Uri, _, "commit b", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_C_Uri, _, "commit c", _),
    commit_uri_to_metadata(Repo_Descriptor, New_Commit_D_Uri, _, "commit d", _),
    commit_id_uri(Repo_Descriptor, Commit_B_Id, Commit_B_Uri),
    commit_id_uri(Repo_Descriptor, Commit_C_Id, Commit_C_Uri),
    commit_id_uri(Repo_Descriptor, New_Commit_D_Id, New_Commit_D_Uri),

    [Commit_B_Id, Commit_C_Id] = Their_Commit_Ids,

    [_{'@type' : "api:RebaseReport",
       'api:origin_commit': Old_Commit_D_Id,
       'api:applied' : [New_Commit_D_Id],
       'api:commit_type': "api:valid_commit"}] = Reports,

    findall(t(S,P,O),
            ask(Master_Descriptor,
                t(S,P,O)),
            Triples_Unsorted),

    sort(Triples_Unsorted, Triples),
    [t(d,e,f),
     t(g,h,i),
     t(j,k,l)] = Triples,

    true.

test(rebase_replayed_commit_records_merge_source,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),
    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         insert(d,e,f)),
                     _),

    create_context(Master_Descriptor, commit_info{author:"test",message:"commit d"}, Master_Context2),
    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         insert(j,k,l)),
                     _),

    Repo_Descriptor = Master_Descriptor.repository_descriptor,
    branch_head_commit(Repo_Descriptor, "main", Old_Commit_D_Uri),

    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Second_Path, "rebaser", [], none, _, _),

    % The replayed copy of commit d records the commit it replays.
    branch_head_commit(Repo_Descriptor, "main", New_Commit_D_Uri),
    create_context(Repo_Descriptor, Repo_Ctx),
    get_document(Repo_Ctx, New_Commit_D_Uri, New_Commit_D),
    get_dict(merge_source, New_Commit_D, Merge_Source),
    get_dict(operation, Merge_Source, rebase),
    atom_string(Old_Commit_D_Uri, Old_Commit_D_Uri_String),
    get_dict(commits, Merge_Source, [Old_Commit_D_Uri_String]).

test(rebase_divergent_history_no_common_commit_option,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    % Same shape as rebase_divergent_history but with the `none`
    % common-commit option — the variant the MCP layer drives.
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),
    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         insert(d,e,f)),
                     _),

    create_context(Master_Descriptor, commit_info{author:"test",message:"commit d"}, Master_Context2),
    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         insert(j,k,l)),
                     _),

    rebase_on_branch(system_descriptor{}, Auth, Second_Path, Master_Path, "rebaser", [], none, _Their_Commit_Ids, _Reports),

    findall(t(S,P,O),
            ask(Second_Descriptor,
                t(S,P,O)),
            Triples_Unsorted),

    sort(Triples_Unsorted, Triples),
    [t(a,b,c),
     t(d,e,f),
     t(j,k,l)] = Triples.

test(rebase_conflicting_history_errors,
     [setup((setup_temp_store(State),
             create_db_with_test_schema("admin","test"))),
      cleanup(teardown_temp_store(State)),
      throws(error(rebase_commit_application_failed(schema_validation_error(Failure_Commit_Id,_),[Failure_Commit_Id]), _))
     ])
:-
    Master_Path = "admin/test",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    City_Uri_String = "City/Dublin",

    Document = _{'@type': "City",
                 '@id' : City_Uri_String,
                 'name': "Dublin"
                },
    with_transaction(Master_Context1,
                     insert_document(Master_Context1,Document,_),
                     _
                    ),

    Repository_Descriptor = (Master_Descriptor.repository_descriptor),

    Second_Path = "admin/test/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    % create a commit on the master branch, diverging history
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit b"}, Master_Context3_),
    context_extend_prefixes(Master_Context3_, _{worldOnt: "http://example.com/schema/worldOntology#"}, Master_Context3),

    Document2 = _{'@type': "City",
                  '@id': City_Uri_String,
                  'name': "Dubhlinn"
                 },

    with_transaction(Master_Context3,
                     replace_document(Master_Context3,Document2),
                     _),

    branch_head_commit(Repository_Descriptor, "main", Failure_Commit_Uri),
    commit_id_uri(Repository_Descriptor, Failure_Commit_Id, Failure_Commit_Uri),

    % create a commit on the second branch, diverging history
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context_),
    context_extend_prefixes(Second_Context_, _{worldOnt: "http://example.com/schema/worldOntology#"}, Second_Context),

    Document3 = _{'@type': "City",
                  '@id': City_Uri_String,
                  'name': "Baile Átha Cliath"
                 },

    with_transaction(Second_Context,
                     replace_document(Second_Context,Document3),
                     _),

    % rebase time!
    super_user_authority(Auth),


    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Second_Path, "rebaser", [], _Common_Commit_Id, _Their_Commit_Ids, _Reports).

test(rebase_fast_forward_from_nothing,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    Main_Path = "admin/foo",
    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Main_Path), _),

    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    descriptor_commit_id_uri((Second_Descriptor.repository_descriptor),
                             Second_Descriptor,
                             Commit_Id1,
                             _Commit_Uri1),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    super_user_authority(Auth),

    descriptor_commit_id_uri((Second_Descriptor.repository_descriptor),
                             Second_Descriptor,
                             Commit_Id2,
                             _Commit_Uri2),

    atomic_list_concat(['admin/foo/local/commit/',Commit_Id2], Commit_Path),

    rebase_on_branch(system_descriptor{}, Auth, Main_Path, Commit_Path, "rebaser",  [], Optional_Common_Commit_Id, Their_Applied_Commit_Ids, []),

    Optional_Common_Commit_Id = none,
    [Commit_Id1,Commit_Id2] = Their_Applied_Commit_Ids.

test(rebase_schema_only_commit_keeps_instance,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    % A replayed commit that touched only the schema graph has no
    % instance layer. The replay must carry the base instance layer
    % forward, or everything the base knew silently disappears.
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),
    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    % schema-only commit on second — no instance layer
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit s"}, Second_Context_S),
    with_transaction(Second_Context_S,
                     insert_schema_document(Second_Context_S,
                                            _{ '@id' : "Extra",
                                               '@type' : "Class" }),
                    _),

    % instance commit on second
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         insert(d,e,f)),
                     _),

    % divergent commit on main
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit d"}, Master_Context2),
    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         insert(j,k,l)),
                     _),

    rebase_on_branch(system_descriptor{}, Auth, Second_Path, Master_Path, "rebaser", [], some(_), _Their_Commit_Ids, _Reports),

    findall(t(S,P,O),
            ask(Second_Descriptor,
                t(S,P,O)),
            Triples_Unsorted),

    sort(Triples_Unsorted, Triples),
    [t(a,b,c),
     t(d,e,f),
     t(j,k,l)] = Triples.

test(rebase_conflict,
     [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "foo"),
                resolve_absolute_string_descriptor("admin/foo", Desc))),
         cleanup(teardown_temp_store(State)),
         error(rebase_commit_application_failed(
                   schema_validation_error(
                       _,
                       [witness{'@type':instance_not_cardinality_one,
                                class:'http://www.w3.org/2001/XMLSchema#string',
                                instance:'http://somewhere.for.now/document/User/Document1',
                                predicate:'http://somewhere.for.now/schema#name'}]),
                   _),
               _)
     ])
:-

    Schema = _{ '@id' : "User",
                '@type' : "Class",
                name : "xsd:string" },

    create_context(Desc, commit{author: "me", message: "something"}, Schema_Context),
    with_transaction(
        Schema_Context,
        insert_schema_document(Schema_Context, Schema),
        _
    ),


    Document1 = _{ '@id' : "User/Document1",
                   '@type' : "User",
                   name : "Document1"},

    create_context(Desc, commit{author: "me", message: "something"}, Context),
    with_transaction(
        Context,
        insert_document(Context, Document1, _),
        _),

    super_user_authority(Auth),
    Path = "admin/foo/local/branch/moo",
    branch_create(system_descriptor{}, Auth, Path, branch("admin/foo"), _),

    resolve_absolute_string_descriptor(Path, Desc2),
    create_context(Desc2, commit{author: "me", message: "something"}, Context2),

    Document2 = _{ '@id' : "User/Document1",
                   '@type' : "User",
                   name : "Document1 is changed X"},

    with_transaction(
        Context2,
        replace_document(Context2, Document2, _),
        _),


    Document3 = _{ '@id' : "User/Document1",
                   '@type' : "User",
                   name : "Document1 is changed Y"},

    create_context(Desc, commit{author: "me", message: "something"}, Context3),
    with_transaction(
        Context3,
        replace_document(Context3, Document3, _),
        _),

    rebase_on_branch(system_descriptor{}, Auth, "admin/foo", Path, "me", [], _Common_Commit_Id, _Their_Commit_Ids, _Reports).

:- use_module(core(api/api_access_control),
              [api_add_role/4,
               api_grant_capability/3,
               api_get_resource_from_name/5]).

% A caller holding commit read + instance write on admin/foo, no schema
% write. Rebase grafts the source segment wholesale and replays the
% target segment — schema_write_access must be charged only when a
% commit that is actually replayed or grafted wrote schema.
grant_rebase_access(SystemDB, Admin_Auth, Db_Name, User_Auth) :-
    api_add_role(SystemDB, Admin_Auth,
                 _{name: "instance_writer",
                   action: ["commit_read_access", "instance_write_access"]},
                 Role_Id),
    api_get_resource_from_name(SystemDB, Admin_Auth, Db_Name, database,
                               Resource),
    get_dict('@id', Resource, Scope_Id),
    atom_string(User_Auth, User_Auth_String),
    atom_string(Role_Id, Role_Id_String),
    api_grant_capability(SystemDB, Admin_Auth,
                         _{scope: Scope_Id, user: User_Auth_String,
                           roles: [Role_Id_String]}).

test(rebase_instance_only_history_needs_no_schema_write,
     [setup((setup_temp_store(State),
             create_db_with_test_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))]) :-
    super_user_authority(Admin_Auth),
    open_descriptor(system_descriptor{}, SystemDB),
    Options = [author("test"), message("c"), graph_type(instance),
               full_replace(false)],

    Master_Path = "admin/foo",
    open_string('[{"@id":"City/a","@type":"City","name":"Warsaw"}]', S1),
    api_insert_documents(SystemDB, Admin_Auth, Master_Path, S1,
                         no_data_version, data_version(branch,_), _, _, Options),

    Second_Path = "admin/foo/local/branch/second",
    branch_create(system_descriptor{}, Admin_Auth, Second_Path,
                  branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, _),
    open_string('[{"@id":"City/b","@type":"City","name":"Krakow"}]', S2),
    api_insert_documents(SystemDB, Admin_Auth, Second_Path, S2,
                         no_data_version, data_version(branch,_), _, _, Options),

    add_user("mallory", some('password'), Mallory_Auth),
    grant_rebase_access(system_descriptor{}, Admin_Auth, "admin/foo", Mallory_Auth),

    rebase_on_branch(system_descriptor{}, Mallory_Auth, Master_Path,
                     Second_Path, "mallory", [], _Base, _Ids, _Reports).

test(rebase_schema_segment_on_source_requires_schema_write,
     [setup((setup_temp_store(State),
             create_db_with_test_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))]) :-
    super_user_authority(Admin_Auth),
    open_descriptor(system_descriptor{}, SystemDB),
    Options = [author("test"), message("c"), graph_type(instance),
               full_replace(false)],

    Master_Path = "admin/foo",
    open_string('[{"@id":"City/a","@type":"City","name":"Warsaw"}]', S1),
    api_insert_documents(SystemDB, Admin_Auth, Master_Path, S1,
                         no_data_version, data_version(branch,_), _, _, Options),

    Second_Path = "admin/foo/local/branch/second",
    branch_create(system_descriptor{}, Admin_Auth, Second_Path,
                  branch(Master_Path), _),

    % The source segment carries a schema-writing commit.
    open_string('{"@type":"Class","@id":"Widget","f":"xsd:string"}', SW),
    api_replace_documents(SystemDB, Admin_Auth, Second_Path, SW,
                          no_data_version, data_version(branch,_), _, _,
                          [create(true), graph_type(schema)|Options]),

    % Divergent instance commit on the target so both segments exist.
    open_string('[{"@id":"City/b","@type":"City","name":"Krakow"}]', S2),
    api_insert_documents(SystemDB, Admin_Auth, Master_Path, S2,
                         no_data_version, data_version(branch,_), _, _, Options),

    add_user("mallory", some('password'), Mallory_Auth),
    grant_rebase_access(system_descriptor{}, Admin_Auth, "admin/foo", Mallory_Auth),

    catch(rebase_on_branch(system_descriptor{}, Mallory_Auth, Master_Path,
                           Second_Path, "mallory", [], _Base, _Ids, _Reports),
          E, true),
    E = error(access_not_authorised(Mallory_Auth,
                                    '@schema':'Action/schema_write_access',
                                    _), _).

test(rebase_schema_segment_on_target_requires_schema_write,
     [setup((setup_temp_store(State),
             create_db_with_test_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))]) :-
    super_user_authority(Admin_Auth),
    open_descriptor(system_descriptor{}, SystemDB),
    Options = [author("test"), message("c"), graph_type(instance),
               full_replace(false)],

    Master_Path = "admin/foo",
    open_string('[{"@id":"City/a","@type":"City","name":"Warsaw"}]', S1),
    api_insert_documents(SystemDB, Admin_Auth, Master_Path, S1,
                         no_data_version, data_version(branch,_), _, _, Options),

    Second_Path = "admin/foo/local/branch/second",
    branch_create(system_descriptor{}, Admin_Auth, Second_Path,
                  branch(Master_Path), _),
    open_string('[{"@id":"City/b","@type":"City","name":"Krakow"}]', S2),
    api_insert_documents(SystemDB, Admin_Auth, Second_Path, S2,
                         no_data_version, data_version(branch,_), _, _, Options),

    % The target segment carries a schema-writing commit — the replay
    % re-writes it, so the gate applies symmetrically.
    open_string('{"@type":"Class","@id":"Widget","f":"xsd:string"}', SW),
    api_replace_documents(SystemDB, Admin_Auth, Master_Path, SW,
                          no_data_version, data_version(branch,_), _, _,
                          [create(true), graph_type(schema)|Options]),

    add_user("mallory", some('password'), Mallory_Auth),
    grant_rebase_access(system_descriptor{}, Admin_Auth, "admin/foo", Mallory_Auth),

    catch(rebase_on_branch(system_descriptor{}, Mallory_Auth, Master_Path,
                           Second_Path, "mallory", [], _Base, _Ids, _Reports),
          E, true),
    E = error(access_not_authorised(Mallory_Auth,
                                    '@schema':'Action/schema_write_access',
                                    _), _).

:- end_tests(rebase).

:- begin_tests(rebase_queue, []).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(core(document)).
:- use_module(db_create).
:- use_module(db_branch).

test(rebase_through_commit_queue,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup((commit_queue:stop_workers,
               teardown_temp_store(State)))
     ])
:-
    Master_Path = "admin/foo",
    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),

    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test", message:"commit a"}, Ctx1),
    with_transaction(Ctx1, ask(Ctx1, insert(a,b,c)), _),

    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),
    create_context(Second_Descriptor, commit_info{author:"test", message:"commit b"}, Ctx2),
    with_transaction(Ctx2, ask(Ctx2, insert(d,e,f)), _),

    commit_queue:start_workers(1),
    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Second_Path, "rebaser", [], _Common_Commit_Id, _Their_Commit_Ids, _Reports),
    commit_queue:stop_workers.

:- end_tests(rebase_queue).

optimize_thread_loop(Store, Auth, Master_Meta_Path) :-
    thread_get_message(Message),
    (   Message == optimize_now
    ->  api_optimize(system_descriptor{}, Auth, Master_Meta_Path)
    ;   true
    ),
    (   Message == stop
    ->  true
    ;   optimize_thread_loop(Store, Auth, Master_Meta_Path)
    ).

insert_race_doc(_Auth, Master_Path) :-
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test", message:"race insert"}, Ctx),
    with_transaction(Ctx, ask(Ctx, insert(race,g,h)), _).

% Guard used by rebase_races_with_target_branch_insert to ensure the pre-lock
% hook inserts exactly once, producing a deterministic single retry.
:- dynamic rebase_race_insert_done/0.

rebase_race_insert_once(Contract, Auth) :-
    (   rebase_race_insert_done
    ->  true
    ;   get_dict(our_branch_path, Contract, Master_Path),
        insert_race_doc(Auth, Master_Path),
        assertz(rebase_race_insert_done)
    ).

:- begin_tests(rebase_race, [concurrent(false)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(document)).
:- use_module(db_create).
:- use_module(db_branch).

% Regression test for the race between rebase's database commit and a
% concurrent optimization that squashes the _meta head. Before the fix, the
% rebase fails because the new _meta layer is a child of the pre-squash head,
% not the post-squash head. After the fix, the meta commit lock serializes the
% two operations.
%
% The rebase path calls run_transactions/3 directly, without the retry that
% can mask the race in with_transaction/4.
test(rebase_races_with_database_optimize,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo")
            )),
      cleanup((retractall(db_rebase:rebase_optimize_thread(_)),
               teardown_temp_store(State)))
     ])
:-
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    super_user_authority(Auth),

    % Commit on main so _meta has a parent layer to squash.
    create_context(Master_Descriptor, commit_info{author:"test", message:"commit a"}, Ctx1),
    with_transaction(Ctx1, ask(Ctx1, insert(a,b,c)), _),

    % Create a feature branch with a commit to rebase.
    Feature_Path = "admin/foo/local/branch/feature",
    branch_create(system_descriptor{}, Auth, Feature_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Feature_Path, Feature_Descriptor),
    create_context(Feature_Descriptor, commit_info{author:"test", message:"feature commit"}, Ctx2),
    with_transaction(Ctx2, ask(Ctx2, insert(d,e,f)), _),

    triple_store(Store),
    Master_Meta_Path = "admin/foo/_meta",

    % Start the optimizer thread before the rebase. The permanent hook in
    % db_rebase.pl sends optimize_now to the registered thread exactly in the
    % window between the repository head update and the database commit.
    setup_call_cleanup(
        (   thread_create(
                with_triple_store(Store,
                                  optimize_thread_loop(Store, Auth, Master_Meta_Path)),
                OptimizeThread,
                []
            ),
            assertz(db_rebase:rebase_optimize_thread(OptimizeThread))
        ),
        rebase_on_branch(system_descriptor{}, Auth, Master_Path, Feature_Path, "rebaser", [], some(_), _, []),
        (   db_rebase:rebase_optimize_thread(OptimizeThread),
            thread_send_message(OptimizeThread, stop),
            thread_join(OptimizeThread, OptimizeResult),
            assertion(OptimizeResult == true)
        )
    ).

% Regression test for the race between rebase's history computation and a
% concurrent insert on the target branch. Before the fix, the rebase failed
% silently because the target branch head moved after the history was computed
% and before the database commit, producing a final commit that was not a
% descendant of the current head. After the fix, the rebase detects the change
% inside the meta commit lock and retries once.
%
% The race is triggered deterministically via the rebase_pre_lock_hook/1 test
% hook: it runs after the rebase history has been computed but before the meta
% commit lock is acquired, so the inserted document is guaranteed to move the
% target branch head exactly once.
test(rebase_races_with_target_branch_insert,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo")
            )),
      cleanup((retractall(db_rebase:rebase_pre_lock_hook(_)),
               retractall(db_rebase:rebase_race_insert_done),
               teardown_temp_store(State)))
     ])
:-
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    super_user_authority(Auth),

    % Commit on main so the target branch has a history to race with.
    create_context(Master_Descriptor, commit_info{author:"test", message:"commit a"}, Ctx1),
    with_transaction(Ctx1, ask(Ctx1, insert(a,b,c)), _),

    % Create a feature branch with a commit so the rebase has work to do.
    Feature_Path = "admin/foo/local/branch/feature",
    branch_create(system_descriptor{}, Auth, Feature_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Feature_Path, Feature_Descriptor),
    create_context(Feature_Descriptor, commit_info{author:"test", message:"feature commit"}, Ctx2),
    with_transaction(Ctx2, ask(Ctx2, insert(d,e,f)), _),

    setup_call_cleanup(
        (   retractall(db_rebase:rebase_race_insert_done),
            assertz((db_rebase:rebase_pre_lock_hook(Contract) :-
                        db_rebase:rebase_race_insert_once(Contract, Auth)))
        ),
        rebase_on_branch(system_descriptor{}, Auth, Master_Path, Feature_Path,
                         "rebaser", [], some(_), _, _Reports),
        (   retractall(db_rebase:rebase_pre_lock_hook(_)),
            retractall(db_rebase:rebase_race_insert_done)
        )
    ).

:- end_tests(rebase_race).
