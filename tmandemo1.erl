-module(tmandemo1).

-import(config, [getC/1]).
-import(common, [gen_node_id/0]).
-import(tman, [distributeSubsets/2]).
-import(qc, [insertDataFromFileP/2, insertDataFromFile/2, fetchDataRow/2]).

-export([master/0]).

insertCountries(Nodes) ->
    insertDataFromFileP(Nodes, "countries.txt"),
    io:format("Stored all countries in DHT~n").


fetchCountry(Node, Key) ->
    Response = fetchDataRow(Node, Key),
    case Response of
        error -> io:format("Could not find Countrie ~p~n", [Key]);
        {ok, Value} -> io:format("Capitol of Country ~p is ~p", [Key, Value])
    end.


master() ->
    random:seed(now()),
    NodeIds = [gen_node_id() || _ <- lists:seq(1,getC(n))],
    Nodes = [ {NodeId, spawn(tman, node_init, [NodeId])} || NodeId <- NodeIds],
                                                % send to each node a random subset of nodes (initial neighbors)
    distributeSubsets(Nodes, Nodes),

    timer:sleep(1000),
    insertCountries(lists:sublist(Nodes, 3)),

    timer:sleep(200),
    lists:foreach(fun({_,Proc}) ->
                          Proc ! {printstate},
                          timer:sleep(200)
                  end, Nodes),
    timer:sleep(500),
    fetchCountry(hd(Nodes), "Switzerland"),
                                                %{_,FirstProc} = hd(Nodes),
                                                %FirstProc ! {printstate},
                                                %timer:sleep(200),
    pass.
