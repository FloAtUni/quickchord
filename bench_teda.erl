-module(bench_teda).

-import(config, [getC/1]).
-import(common, [gen_node_id/0]).
-import(tman, [distributeSubsets/2, spawnLocalTManNodes/0, spawnRemoteTManNodes/0]).
-import(qc, [insertDataFromFile/2, insertDataFromFileP/2,
             fetchDataRow/2, fetchHopcount/2]).

-export([master/0]).

insertSmall(Nodes) ->
    insertDataFromFile(hd(Nodes), "data/small_data.txt"),
    %insertDataFromFileP(Nodes, "data/small_data.txt"),
    io:format("Stored all data in DHT~n").

insertData(Nodes) ->
    % We don't use the Parallel Version because it tends
    insertDataFromFile(Nodes, "data/random_data.txt"),
    io:format("Stored all data in DHT~n").


fetchData(Node, Key) ->
    Response = fetchDataRow(Node, Key),
    case Response of
        error -> io:format("Could not find entry ~p~n", [Key]);
        {ok, Value} -> io:format("Key: ~p Value: ~p", [Key, Value])
    end.

master() ->
    random:seed(now()),
    Nodes = spawnRemoteTManNodes(),
                                                % send to each node a random subset of nodes (initial neighbors)
    distributeSubsets(Nodes, Nodes),

    % Wait nbcycles and 2 additional cycles
    timer:sleep(getC(cycletimems)*(2+getC(nbcycles))),

    %insertData(hd(Nodes)),
    %insertSmall(Nodes),

    %lists:foreach(fun({_,Proc}) ->
    %                      Proc ! {printstate},
    %                      timer:sleep(200)
    %              end, Nodes),
    %timer:sleep(500),

    fetchData(hd(Nodes), "LgGNFU2"),
    Count = fetchHopcount(hd(Nodes), "LgGNFU2"),
    io:format("~nCount: ~p~n", [Count]),

    pass.
