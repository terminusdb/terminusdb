:- module(ask,[
              ask/2,
              ask_ast/3,
              create_context/2,
              create_context/3,
              context_to_parent_transaction/2,
              collection_descriptor_prefixes/2,
              context_extend_prefixes/3,
              context_default_prefixes/2,
              empty_context/1,
              query_default_collection/2,
              query_default_write_graph/2
          ]).

/** <module> Ask
 *
 * Prolog interface to ask queries
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- reexport(core(util/syntax)).
:- use_module(woql_compile).
:- use_module(global_prefixes).

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

prefix_preterm(Ctx, Woql_Var, Pre_Term) :-
    freeze(Woql_Var,
           (   is_dict(Woql_Var) % Document
           ->  Woql_Var = Pre_Term
           ;   Woql_Var = [_|_]
           ->  maplist(prefix_preterm(Ctx),Woql_Var,Pre_Term)
           ;   Woql_Var = _@_
           ->  Pre_Term = Woql_Var
           ;   Woql_Var = Elt^^Type
           ->  freeze(Type,
                      (   uri_to_prefixed(Type,Ctx,Prefixed_Type),
                          Pre_Term = Elt^^Prefixed_Type))
           ;   uri_to_prefixed(Woql_Var,Ctx,Pre_Term))).

/**
 * pre_term_to_term_and_bindings(Pre_Term, Woql_Term, Bindings_In, Bindings_Out) is det.
 *
 * Pre term has free variables that need to be changed into woql variables witha binding
 */
pre_term_to_term_and_bindings(Ctx,Pre_Term,Term,Bindings_In,Bindings_Out) :-
    (   var(Pre_Term)
    ->  (   lookup_backwards(Pre_Term,V,Bindings_In)
        ->  Bindings_In = Bindings_Out,
            Term = v(V)
        ;   gensym('Var',G),
            Bindings_Out = [var_binding{ var_name : G,
                                         prolog_var: Pre_Term,
                                         woql_var : Woql_Var}|Bindings_In],
            prefix_preterm(Ctx,Woql_Var,Pre_Term),
            Term = v(G)
        )
    ;   is_dict(Pre_Term)
    ->  Term = Pre_Term,
        Bindings_In=Bindings_Out
    ;   Pre_Term =.. [F|Args],
        mapm(pre_term_to_term_and_bindings(Ctx),Args,New_Args,Bindings_In,Bindings_Out),
        Term =.. [F|New_Args]
    ).

collection_descriptor_prefixes_(Descriptor, Prefixes) :-
    terminus_descriptor{} :< Descriptor,
    !,
    Prefixes = _{doc: 'terminus:///terminus/document/'}.
collection_descriptor_prefixes_(Descriptor, Prefixes) :-
    id_descriptor{} :< Descriptor,
    !,
    Prefixes = _{}.
collection_descriptor_prefixes_(Descriptor, Prefixes) :-
    label_descriptor{label: Label} :< Descriptor,
    !,
    atomic_list_concat(['terminus:///',Label,'/document/'], Doc_Prefix),
    Prefixes = _{doc: Doc_Prefix}.
collection_descriptor_prefixes_(Descriptor, Prefixes) :-
    database_descriptor{
        database_name: Name
    } :< Descriptor,
    !,
    atomic_list_concat(['terminus:///',Name,'/document/'], Doc_Prefix),
    Prefixes = _{doc: Doc_Prefix}.
collection_descriptor_prefixes_(Descriptor, Prefixes) :-
    repository_descriptor{
        database_descriptor: Database_Descriptor
    } :< Descriptor,
    !,
    database_descriptor{
        database_name: Database_Name
    } :< Database_Descriptor,
    atomic_list_concat(['terminus:///', Database_Name, '/commits/document/'], Commit_Document_Prefix),
    Prefixes = _{doc : Commit_Document_Prefix}.
collection_descriptor_prefixes_(Descriptor, Prefixes) :-
    % Note: possible race condition.
    % We're querying the ref graph to find the branch base uri. it may have changed by the time we actually open the transaction.
    branch_descriptor{
        repository_descriptor: Repository_Descriptor
    } :< Descriptor,
    !,
    repository_prefixes(Repository_Descriptor, Prefixes).
collection_descriptor_prefixes_(Descriptor, Prefixes) :-
    % We don't know which documents you are retrieving
    % because we don't know the branch you are on,
    % and you can't write so it's up to you to set this
    % in the query.
    commit_descriptor{} :< Descriptor,
    !,
    Prefixes = _{}.

collection_descriptor_prefixes(Descriptor, Prefixes) :-
    default_prefixes(Default_Prefixes),
    collection_descriptor_prefixes_(Descriptor, Nondefault_Prefixes),
    merge_dictionaries(Nondefault_Prefixes, Default_Prefixes, Prefixes).

collection_descriptor_default_write_graph(terminus_descriptor{}, Graph_Descriptor) :-
    !,
    Graph_Descriptor = terminus_graph{
                           type : instance,
                           name : "main"
                       }.
collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor) :-
    database_descriptor{ database_name : Name } = Descriptor,
    !,
    Graph_Descriptor = repo_graph{
                           database_name : Name,
                           type : instance,
                           name : "main"
                       }.
collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor) :-
    repository_descriptor{
        database_descriptor : Database_Descriptor,
        repository_name : Repository_Name
    } = Descriptor,
    !,
    database_descriptor{ database_name : Database_Name } = Database_Descriptor,
    Graph_Descriptor = commit_graph{
                           database_name : Database_Name,
                           repository_name : Repository_Name,
                           type : instance,
                           name : "main"
                       }.
collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor) :-
    branch_descriptor{ branch_name : Branch_Name,
                       repository_descriptor : Repository_Descriptor
                     } :< Descriptor,
    !,
    repository_descriptor{
        database_descriptor : Database_Descriptor,
        repository_name : Repository_Name
    } :< Repository_Descriptor,
    database_descriptor{
        database_name : Database_Name
    } :< Database_Descriptor,

    Graph_Descriptor = branch_graph{
                           database_name : Database_Name,
                           repository_name : Repository_Name,
                           branch_name : Branch_Name,
                           type : instance,
                           name : "main"
                       }.
collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor) :-
    label_descriptor{ label: Label} :< Descriptor,
    !,
    text_to_string(Label, Label_String),
    Graph_Descriptor = labelled_graph{label:Label_String,
                                      type: instance,
                                      name:"main"
                                     }.
collection_descriptor_default_write_graph(_, empty).

create_context(Layer, Context) :-
    blob(Layer, layer),
    !,
    open_descriptor(Layer, Transaction),
    create_context(Transaction, Context).
create_context(Context, Context) :-
    query_context{} :< Context,
    !.
create_context(Transaction_Object, Context) :-
    transaction_object{ descriptor : Descriptor } :< Transaction_Object,
    !,
    collection_descriptor_prefixes(Descriptor, Prefixes),
    collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor),

    % Note: should we be using terminus_descriptor{} below? or open it?

    Context = query_context{
        authorization : 'terminus:///terminus/document/access_all_areas',
        transaction_objects : [Transaction_Object],
        default_collection : Descriptor,
        filter : type_filter{ types : [instance] },
        prefixes : Prefixes,
        write_graph : Graph_Descriptor,
        terminus : terminus_descriptor{},
        update_guard : _Guard,
        files : [],
        bindings : [],
        selected : []
    }.
create_context(Descriptor, Context) :-
    open_descriptor(Descriptor, Transaction_Object),
    create_context(Transaction_Object, Context).

/**
 * create_context(Askable, Commit_Info, Context).
 *
 * Add Commit Info
 */
create_context(Askable, Commit_Info, Context) :-
    create_context(Askable, Context_Without_Commit),
    Context = Context_Without_Commit.put(commit_info, Commit_Info).

/**
 * descriptor_capabilities_context(Askable, Capabilities, Context) is det.
 *
 * Create a context with capabilities included
 */
descriptor_capabilities_context(Askable, Capabilities, Context) :-
    create_context(Askable, Context),
    Context.put(capabilities, Capabilities).

/*
 * context_extend_prefixes(+Context:query_context, +Prefixes:prefixes,
 *                             -New_Context:query_context) is det.
 *
 * Override the current query context with these prefixes when
 * there are collisions.
 */
context_extend_prefixes(Context, Prefixes, New_Context) :-
    Query_Prefixes = Context.prefixes,
    merge_dictionaries(Prefixes,Query_Prefixes, New_Prefixes),
    New_Context = Context.put(prefixes, New_Prefixes).

/*
 * context_default_prefixes(+Context:query_context, -New_Context:query_context) is det.
 *
 * Transform the given context, stripping out all prefixes, and
 * replacing them with the default global prefix set.
 */
context_default_prefixes(Context, New_Context) :-
    default_prefixes(Default),
    New_Context = Context.put(prefixes, Default).

context_to_parent_transaction(Context,Parent_Transaction) :-
    do_or_die(
        (   get_dict(transaction_objects,Context,[Transaction]),
            get_dict(parent,Transaction,Parent_Transaction)
        ),
        error(context_has_multiple_parents(Context))).

/*
 * ask(+Transaction_Object, Pre_Term:Goal) is nondet.
 *
 * Ask a woql query.
 *
 * Presumably this should commit the context since
 * it can't give anything back?
 */
ask(Askable, Pre_Term) :-
    create_context(Askable, Query_Context),

    pre_term_to_term_and_bindings(Query_Context.prefixes,
                                  Pre_Term,Term,
                                  [],Bindings_Out),
    New_Query_Ctx = Query_Context.put(bindings,Bindings_Out),

    ask_ast(New_Query_Ctx, Term, _).

/*
 * ask(+Transaction_Object, Pre_Term:Goal) is nondet.
 *
 * Ask a woql query and get back the resulting context.
 */
ask_ast(Context, Ast, Output_Context) :-
    compile_query(Ast,Prog, Context, Output_Context),
    debug(terminus(sdk),'Program: ~q~n', [Prog]),

    woql_compile:Prog.

/*
 * query_default_collection(Query_Context, Collection) is semidet.
 *
 * Finds the transaction object for the default collection if it exists.
 */
query_default_collection(Query_Context, Collection) :-
    collection_descriptor_transaction_object(Query_Context.default_collection,
                                             Query_Context.transaction_objects,
                                             Collection).

/*
 * query_default_write_graph(Query_Context, Graph) is semidet.
 *
 * Finds the transaction object for the default collection if it exists.
 */
query_default_write_graph(Query_Context, Write_Graph) :-
    Graph_Descriptor = Query_Context.write_graph,
    Transaction_Objects = Query_Context.transaction_objects,
    graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor,
                                                           Transaction_Objects,
                                                           Write_Graph).

