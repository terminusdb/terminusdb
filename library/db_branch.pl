:- module(db_branch, []).

create_branch(Db_Label, Repo_Name, Branch_Name) :-
    % retrieve proper ref graph
    storage(Store),
    open_named_graph(Db_Label, Repo_Graph),
    head(Repo_Graph, Repo_Layer),
