:- module(resolve_query_resource,[
              resolve_query_resource/2,
              resolve_filter/2
          ]).

/** <module> Resolve Query Resource
 *
 * Resolves resource URIs to the appropiate associated graph_descriptor.
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

:- use_module(library(pcre)).
:- use_module(utils).

/*****************************************
 * URI Resource Resolution:
 *
 * We need to resolve URIs to the appropriate object - i.e. a desriptor which
 * can be interpreted by the prolog term output by WOQL. This involves two specific
 * scenarios: read and write.
 *
 * WOQL should *compile* to the resolving descriptor, which can then be used in the transaction to
 * find the right query_object, allowing the transaction logic to be simplified, and eventually
 * supporting nested transactions.
 *
 ** Read: Querying a specific object.
 *
 * Here we need to be able to read the union of the instance graphs of the appropriate query_object,
 * against the union of schema graphs (where relevant - i.e. with subsumption).
 *
 * As long as the resolution leaves us with a query_object, we can treat it irrespective of the
 * "layer of the union".
 *
 ** Write: Writing to a specific graph
 *
 * Here we need to be more directed. If we are unable to resolve to a specific graph we need to
 * throw an error, describing the graph set which might be intended, with their specific URIs.
 *
 * We can default the write graph to Server://DB_Name/local/document/main
 * Server can be defaulted to terminusHub
 */

/**
 * resolve_query_resource(Uri, Descriptor) is semidet.
 *
 * We need to be able to resolve arbitrary resource URI's
 * to the appropriate portion of the query_object.
 *
 */
% Terminus
%
% 'terminus:///terminus'
%
resolve_query_resource('terminus:///terminus/',terminus_descriptor).
% Branches
%
% 'http://[Server]/[User]/[Database_Name]'
% 'http://[Server]/[User]/[Database_Name]/<Repo_Name>'
% 'http://[Server]/[User]/[Database_Name]/<Repo_Name>/<Ref_Name>'
%
resolve_query_resource(URI, Branch_Descriptor) :-
    (   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/(?P<repo>[^/]*)/(?P<branch>[^/]*)$', URI, Resource_Dict)
    ->  true
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/(?P<repo>[^/]*)$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{branch : "master"})
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)$', URI, Dict),
        Resource_Dict = Dict.put(_{repo : "local", branch : "master"})),
    !,
    user_database_name(Resource_Dict.user,Resource_Dict.database,Database_Name),
    % convenience predicate?
    Database_Descriptor = database_descriptor{
                              database_name : Database_Name
                          },

    Repository_Descriptor = repository_descriptor{
                                database_descriptor : Database_Descriptor,
                                repository_name : Resource_Dict.repo
                            },

    Branch_Descriptor = branch_descriptor{
                            repository_descriptor : Repository_Descriptor,
                            branch_name : Resource_Dict.branch
                        }.
% Repository descriptor (the Commit Graph)
%
% 'http://[Server]/[User]/[Database_Name]/commits/'
% 'http://[Server]/[User]/[Database_Name]/commits/<Repo_Name>'
%
resolve_query_resource(URI, Repository_Descriptor) :-
    (   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/commits/(?P<repo>)$', URI, Resource_Dict)
    ->  true
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/commits$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{repo : "local"})
    ),
    !,
    user_database_name(Resource_Dict.user,Resource_Dict.database,Database_Name),

    Database_Descriptor = database_descriptor{
                              database_name : Database_Name
                          },
    Repository_Descriptor = repository_descriptor{
                                database_descriptor : Database_Descriptor,
                                repository_name : Resource_Dict.repo
                            }.
% Database descriptor
%
% 'http://[Server]/[User]/[Database_Name]/repositories'
%
resolve_query_resource(URI, Database_Descriptor) :-
    re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/repositories$', URI, Resource_Dict),

    user_database_name(Resource_Dict.user,Resource_Dict.database,Database_Name),

    Database_Descriptor = database_descriptor{
                              database_name : Database_Name
                          }.

/**
 * resolve_graph_resource(URI, Graph_Descriptor) is det + error.
 *
 * If we know the resource is a graph, we can use defaults to obtain a graph.
 *
 * user / dbname / repo / my_branch / graph
 */
% Branch graphs
%
% 'http://[Server]/[User]/[Database_Name]'
% 'http://[Server]/[User]/[Database_Name]/<Repo_Name>'
% 'http://[Server]/[User]/[Database_Name]/[Repo_Name]/<Branch_Name>'
% 'http://[Server]/[User]/[Database_Name]/[Repo_Name]/[Branch_Name]/<Graph_Type>'
% 'http://[Server]/[User]/[Database_Name]/[Repo_Name]/[Branch_Name]/[Graph_Type]/<Graph_Name>'
%
resolve_graph_resource(URI,Descriptor) :-
    (   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/(?P<repo>[^/]*)/(?P<branch>[^/]*)/(?P<type>[^/]*)/(?P<name>[^/]*)$', URI, Resource_Dict)
    ->  true
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/(?P<repo>[^/]*)/(?P<branch>[^/]*)/(?P<type>[^/]*)$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{name : "main"})
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/(?P<repo>[^/]*)/(?P<branch>[^/]*)$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{name : "main", type : "instance"})
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/(?P<repo>[^/]*)$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{name : "main", type : "instance", branch : "master"})
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{name : "main", type : "instance", branch : "master", repo : "local"})),
    !,
    user_database_name(Resource_Dict.user,Resource_Dict.database,Database_Name),

    Descriptor = branch_graph{
                     database_name : Database_Name,
                     repository_name : Resource_Dict.repo,
                     branch_name : Resource_Dict.branch,
                     type : Resource_Dict.type,
                     name : Resource_Dict.name
                 }.
% Commit Graphs
%
% 'http://[Server]/[User]/[Database_Name]/commits'
% 'http://[Server]/[User]/[Database_Name]/commits/<instance>'
% 'http://[Server]/[User]/[Database_Name]/commits/[instance]/main'
% 'http://[Server]/[User]/[Database_Name]/commits/[schema]'
% 'http://[Server]/[User]/[Database_Name]/commits/[schema]/main'
%
resolve_graph_resource(URI,Descriptor) :-
    (   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/commits/(?P<type>[^/]*)/(?P<name>[^/]*)$', URI, Resource_Dict)
    ->  true
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/commits/(?P<type>[^/]*)/(?P<name>[^/]*)$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{name : "main"})
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/commits$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{type : "instance", name : "main"})),
    !,
    user_database_name(Resource_Dict.user,Resource_Dict.database,Database_Name),

    Descriptor = commit_graph{
                     database_name : Database_Name,
                     repository_name : Resource_Dict.repo,
                     type : Resource_Dict.type,
                     name : Resource_Dict.name
                 }.
% Repository Graphs
%
% 'http://[Server]/[User]/[Database_Name]/repositories'
% 'http://[Server]/[User]/[Database_Name]/repositories/<instance>'
% 'http://[Server]/[User]/[Database_Name]/repositories/[instance]/main'
% 'http://[Server]/[User]/[Database_Name]/repositories/<schema>'
% 'http://[Server]/[User]/[Database_Name]/repositories/[schema]/main'
%
resolve_graph_resource(URI,Descriptor) :-
    (   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/repositories/(?P<type>[^/]*)/(?P<name>[^/]*)$', URI, Resource_Dict)
    ->  true
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/repositories/(?P<type>[^/]*)$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{name : "main"})
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/repositories$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{type : "instance", name : "main"})),
    !,
    user_database_name(Resource_Dict.user,Resource_Dict.database,Database_Name),

    % TODO: This doesn't yet exist in this form!
    Descriptor = repo_graph{
                     database_name : Database_Name,
                     repository_name : Resource_Dict.repo,
                     type : Resource_Dict.type,
                     name : Resource_Dict.name
                 }.
% Terminus Graph
%
resolve_graph_resource(URI,Descriptor) :-
    (   re_matchsub('^terminus:///terminus/(?P<type>[^/]*)/(?P<name>[^/]*)$', URI, Resource_Dict)
    ->  true
    ;   re_matchsub('^terminus:///terminus/(?P<type>[^/]*)$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{name : "main"})
    ;   re_matchsub('^terminus:///terminus$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{type : "instance", name : "main"})),
    !,
    user_database_name(Resource_Dict.user,Resource_Dict.database,Database_Name),
    % TODO: How do I resolve this?

    Descriptor = labelled_graph{
                     name : Database_Name % This is wrong!
                 }.
% Error
resolve_graph_resource(URI,_Descriptor) :-
    format(atom(Msg), 'Unknown graph resource ~q', [URI]),
    throw(error(resolution_error(URI,Msg))).


%%
% resolve_filter(Filter_String,Filter) is det.
%
%  Turn a filter string into a a filter - used with 'from'
%
%  Syntax:
%
%  'schema/*' => search schema graphs
%  '{instance,schema}/*' => search the union of instance and schema
%  '*/*' => search everything
%  'instance/{foo,main}' => search in foo and main
%
resolve_filter(Filter_String,Filter) :-
    (   re_matchsub('^\\*/\\*$', Filter_String, _Resource_Dict,[])
    ->  Filter = type_filter{ types : [instance, schema, inference]}
    ;   re_matchsub('^\\{(?P<types>[^}]*)\\}/\\*$', Filter_String, Resource_Dict,[])
    ->  pattern_string_split(',',Resource_Dict.types, Type_Strings),
        maplist(atom_string, Types, Type_Strings),
        forall(member(Type, Types),
               memberchk(Type, [instance,schema,inference])),
        Filter = type_filter{ types : Types }
    ;   re_matchsub('^(?P<type>[^/]*)/\\*$', Filter_String, Resource_Dict,[])
    ->  atom_string(Type, Resource_Dict.type),
        memberchk(Type, [instance,schema,inference]),
        Filter = type_filter{ types : [Type] }
    ;   re_matchsub('^(?P<type>[^/]*)/\\{(?P<names>[^\\}]*)\\}$', Filter_String, Resource_Dict,[])
    ->  pattern_string_split(',',Resource_Dict.names, Names),
        atom_string(Type,Resource_Dict.type),
        memberchk(Type, [instance,schema,inference]),
        Filter = type_name_filter{ type : Type,
                                   names: Names }
    ;   re_matchsub('^(?P<type>[^/]*)/(?P<name>[^/]*)$', Filter_String, Resource_Dict,[])
    ->  atom_string(Type, Resource_Dict.type),
        memberchk(Type, [instance,schema,inference]),
        Filter = type_name_filter{ type : Type,
                                   names : [Resource_Dict.name] }
    ).
