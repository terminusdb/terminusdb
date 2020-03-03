:- module(resolve_query_resource,[
              connect/2,
              resolve_query_resource/2
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
 * TODO: Make this exist
 */
resolve_query_resource('terminus:///terminus/',terminus_descriptor).
% Branches
%
% 'http://[Server]/[User]/[Database_Name]'
% 'http://[Server]/[User]/[Database_Name]/<Repo_Name>'
% 'http://[Server]/[User]/[Database_Name]/<Repo_Name>/<Ref_Name>'
%
resolve_query_resource(URI, Resource) :-
    (   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/(?P<repo>[^/]*)/(?P<ref>[^/]*)$', URI, Resource_Dict)
    ->  true
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)/(?P<repo>[^/]*)$', URI, Dict)
    ->  Resource_Dict = Dict.put(_{repo : local})
    ;   re_matchsub('^(?P<protocol>[^:]*)://(?P<server>[^/]*)/(?P<user>[^/]*)/(?P<database>[^/]*)$', URI, Dict),
        Resource_Dict = Dict.put(_{repo : local, ref : master})),

    user_database_name(User,Database,Label_Name),
    % convenience predicate?
    Database_Descriptor = database_descriptor{
                              database_name : Resource_Dict.database,
                              instance : [labelled_graph{ name : Label_Name }]},

    ask(Database_Descriptor,
        (
            t(Resource_Dict.repo,repository:repository_head, Shadow_Layer),
            t(Resource_Dict.repo,repository:repository_head, Shadow_Layer)
        )
       ),

    Repository_Descriptor = repository_descriptor{
                                database_descriptor : Database_Descriptor,
                                repository_name : Resource_Dict.repo
                                instance : [id_graph { layer_id : Layer_ID }]
                            },
    format(atom(Ref_Name), '%s://%s/%s/%s/%s/%s', [Resource_Dict.protocol,
                                                   Resource_Dict.server,
                                                   Resource_Dict.user,
                                                   Resource_Dict.database,
                                                   Resource_Dict.repo,
                                                   Resource_Dict.ref]),
    % We will need to query for
    % schema and instance here.
    Resource = ref_descriptor{ repository_descriptor : Repository_Descriptor,
                               ref_name : Ref_Name,
                               last_commit URI, % How do we get this?
                               author: '???',
                               message: 'MMMM',
                               schema: ['???'],
                               instance: ['???']
                             }.
resolve_query_resource(URI, Resource) :-

    true.
resolve_query_resource(URI, Resource) :-

    true.

/*
 * If we know the resource is a graph, we can use defaults to obtain a graph.
 *
 * server / dbname / repo / my_branch / 
 */
resolve_graph_resource(URI,Resource) :-
    true.
