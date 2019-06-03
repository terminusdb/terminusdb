:- module(collection,[
              graph_collection/2,
              graph_instance/2,
              graph_schema/2,
              graph_error_instance/2,
              graph_error_schema/2,
              graph_inference/2,
              graph_prefix_database/2,
              make_graph/2,
              make_raw_graph/2,
              graph_identifiers/2,
              make_graph_from_collection/3
          ]).

:- use_module(utils).

/** <module> Implementation of graph management

This module helps other modules with the representation of graphs and collections 
by bundling them as objects with some convenience operators.

*/

/* 
 * Graph term accessors.
 */ 
graph_collection(graph(_, Collection, _ , _ ,_, _, _),Collection).

graph_instance(graph(_, _, Instance, _ ,_, _, _),Instance).

graph_inference(graph(_, _, _,Inference,_,_,_),Inference).

graph_schema(graph(_, _, _,_,Schema,_,_),Schema).

graph_error_instance(graph(_,_,_,_,_,Error_Instance,_), Error_Instance).

graph_error_schema(graph(_,_,_,_,_,_,Error_Schema), Error_Schema).

graph_prefix_database(graph(Prefix_Database,_,_,_,_,_,_), Prefix_Database).

/** 
 * make_graph(+GraphList:list,-Graph:graph) is det.
 * 
 * Create a graph from a list containing some number of graph elements.
 * [instance=Instance,schema=Schema...]
 */ 
make_graph(GraphList, Graph) :-
    Graph = graph(Prefix_Database,Collection,Instance,Inference,Schema,Error_Instance,Error_Schema),
    get_key(prefix_database,GraphList,Prefix_Database,[]),
    get_key(collection,GraphList,Collection,none),
    get_key(schema,GraphList,Schema,none),
    get_key(instance,GraphList,Instance,none),
    get_key(inference,GraphList,Inference,none),
    get_key(error_schema,GraphList,Error_Schema,none),
    get_key(error_instance,GraphList,Error_Instance,none),

    schema:ensure_schema_in_module(Graph, Schema).

/** 
 * make_graph(+GraphList:list,-Graph:graph) is det.
 * 
 * Create a graph from a list containing some number of graph elements.
 * [instance=Instance,schema=Schema...]
 *  
 * This does NO schema compilation and is only used during initial pre-testing for cycles.
 */ 
make_raw_graph(GraphList, Graph) :-
    Graph = graph(Prefix_Database,Collection,Instance,Inference,Schema,Error_Instance,Error_Schema),
    get_key(prefix_database,GraphList,Prefix_Database,[]),
    get_key(collection,GraphList,Collection,none),
    get_key(schema,GraphList,Schema,none),
    get_key(instance,GraphList,Instance,none),
    get_key(inference,GraphList,Inference,none),
    get_key(error_schema,GraphList,Error_Schema,none),
    get_key(error_instance,GraphList,Error_Instance,none).

/** 
 * graph_identifiers(+Graph:graph, -GraphList:list(graph_identifier)) is det.
 * 
 * Create a list of names of the graphs elements contained in a graph structure. 
 */ 
graph_identifiers(Graph,Names) :-
    graph_instance(Graph,GI),
    graph_inference(Graph,Inf),
    graph_schema(Graph,GS),
    graph_error_instance(Graph,EI),
    graph_error_schema(Graph,ES),
    exclude(is_empty_graph_name,[GI,Inf,GS,EI,ES], Names).

/** 
 * make_graph_from_collection(+URI,+CTX,-Graph) is det.
 * 
 * Use a base uri to 
 */ 
make_graph_from_collection(Name,Ctx,Graph) :-
    interpolate([Name,'/graph/main'],Instance),
    interpolate([Name,'/graph/main/schema'],Schema),
    interpolate([Name,'/graph/main/inference'],Inference),
    interpolate([Name,'/graph/main/error'],Error_Instance),
    interpolate([Name,'/graph/main/error/schema'],Error_Schema),
    make_graph([prefix_database=Ctx,
                collection=Name,
                schema=Schema,instance=Instance,inference=Inference,
                error_instance=Error_Instance, error_schema=Error_Schema], Graph).

