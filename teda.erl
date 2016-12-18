-module(teda).

-import(config, [getC/1]).
-import(common, [gen_node_id/0]).
-import(tman, [distributeSubsets/2]).
-import(qc, [insertDataFromFile/2, fetchDataRow/2, fetchHopcount/2]).

-export([master/0]).


master() ->
    random:seed(now()),
    {ok, [_|Es]} = file:consult('enodes.conf'),
    N = getC(n),
    PN = (getC(n) div length(Es)) + 1,
    Ps = lists:sublist(lists:flatten(lists:duplicate(PN,Es)),N),
    NodeIds = lists:zip([gen_node_id() || _ <- lists:seq(1,N)],Ps),
    Nodes = [ {NodeId, spawn(P, tman, node_init, [NodeId])} || {NodeId,P} <- NodeIds],
                                                % send to each node a random subset of nodes (initial neighbors)

    Count = fetchHopcount(hd(Nodes), "unknown key"),
    io:format("Count: ~p~n", [Count]),
    pass.
