-module(teda).

-import(config, [getC/1]).
-import(common, [gen_node_id/0]).
-import(tman, [distributeSubsets/2, spawnRemoteTManNodes/0]).
-import(qc, [insertDataFromFile/2, fetchDataRow/2, fetchHopcount/2]).

-export([master/0]).


master() ->
    random:seed(now()),

    % send to each node a random subset of nodes (initial neighbors)
    Nodes = spawnRemoteTManNodes(),

    distributeSubsets(Nodes, Nodes),
    io:format("Distributed Subsets, waiting for t-man~n"),
    timer:sleep(getC(cycletimems)*getC(nbcycles)),
    io:format("Try to determine the Hopcount~n"),
    Count = fetchHopcount(hd(Nodes), "unknown key"),
    io:format("Count: ~p~n", [Count]),
    pass.
