:- module(ask,[
              ask/2,
              ask/3,
              ask_ast/3,
              create_context/2,
              create_context/3,
              askable_context/4,
              askable_context/5,
              askable_settings_context/3,
              context_to_parent_transaction/2,
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
           ;   number(Woql_Var)
           ->  Woql_Var = Pre_Term
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
pre_term_to_term_and_bindings(Ctx,Options,Pre_Term,Term,Bindings_In,Bindings_Out) :-
    (   var(Pre_Term)
    ->  (   lookup_backwards(Pre_Term,V,Bindings_In)
        ->  Bindings_In = Bindings_Out,
            Term = v(V)
        ;   gensym('Var',G),
            Bindings_Out = [var_binding{ var_name : G,
                                         prolog_var: Pre_Term,
                                         woql_var : Woql_Var}|Bindings_In],
            (   member(compress_prefixes(true), Options)
            ->  prefix_preterm(Ctx,Woql_Var,Pre_Term)
            ;   Pre_Term = Woql_Var),
            Term = v(G)
        )
    ;   is_dict(Pre_Term)
    ->  Term = Pre_Term,
        Bindings_In=Bindings_Out
    ;   Pre_Term =.. [F|Args],
        mapm(pre_term_to_term_and_bindings(Ctx,Options),Args,New_Args,Bindings_In,Bindings_Out),
        Term =.. [F|New_Args]
    ).

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

    % Note: should we be using system_descriptor{} below? or open it?
    super_user_authority(Super_User),
    Context = query_context{
        authorization : Super_User,
        transaction_objects : [Transaction_Object],
        default_collection : Descriptor,
        filter : type_filter{ types : [instance] },
        prefixes : Prefixes,
        write_graph : Graph_Descriptor,
        system : system_descriptor{},
        update_guard : _Guard,
        all_witnesses : false,
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
    Context = (Context_Without_Commit.put(commit_info, Commit_Info)).

/*
 * askable_context/N
 *
 * Create a context from an askable with system/auth and optional commit_info
 */
askable_context(Askable, System_DB, Auth, Context) :-
    create_context(Askable, Context_Without_Commit),
    Context = (Context_Without_Commit.put(_{ authorization : Auth,
                                             system: System_DB })).

askable_context(Askable, System_DB, Auth, Commit_Info, Context) :-
    create_context(Askable, Context_Without_Commit),
    Context = (Context_Without_Commit.put(_{ authorization: Auth,
                                             system: System_DB,
                                             commit_info: Commit_Info})).

askable_settings_context(Askable, Settings, Context) :-
    create_context(Askable, Context_Without_Settings),
    Context = (Context_Without_Settings.put(Settings)).

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
    ask(Askable, Pre_Term, [compress_prefixes(true)]).

/*
 * ask(+Transaction_Object, ?Pre_Term:Goal, +Options) is nondet.
 *
 * Ask with options
 *
 * Options include:
 * - compress_prefixes(Bool): Compress prefixes for results.
 */
ask(Askable, Pre_Term, Options) :-
    create_context(Askable, Query_Context),

    pre_term_to_term_and_bindings(Query_Context.prefixes,Options,
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
