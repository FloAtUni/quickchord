-module(bench).

-import(config, [getC/1]).
-import(common, [gen_node_id/0]).
-import(tman, [distributeSubsets/2]).
-import(qc, [insertDataFromFile/2, fetchDataRow/2]).

-export([master/0]).

insertData(Node) ->
    insertDataFromFile(Node, "data/small_data.txt"),
    io:format("Stored all data in DHT~n").


fetchData(Node, Key) ->
    Response = fetchDataRow(Node, Key),
    case Response of
        error -> io:format("Could not find entry ~p~n", [Key]);
        {ok, Value} -> io:format("Key: ~p Value: ~p", [Key, Value])
    end.


master() ->
    random:seed(now()),
    NodeIds = [gen_node_id() || _ <- lists:seq(1,getC(n))],
    Nodes = [ {NodeId, spawn(tman, node_init, [NodeId])} || NodeId <- NodeIds],
                                                % send to each node a random subset of nodes (initial neighbors)
    distributeSubsets(Nodes, Nodes),

    timer:sleep(1000),
    insertData(hd(Nodes)),

    %timer:sleep(200),
    %lists:foreach(fun({_,Proc}) ->
    %                      Proc ! {printstate},
    %                      timer:sleep(200)
    %              end, Nodes),
    %timer:sleep(500),
    fetchData(hd(Nodes), "Ox3wMCN"),

%{_,FirstProc} = hd(Nodes),
%FirstProc ! {printstate},
%timer:sleep(200),
    pass.
