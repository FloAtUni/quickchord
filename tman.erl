-module(tman).
%-export([master/0]).

-compile(export_all).

-import(config, [getC/1]).
-import(common, [gen_node_id/0, randomSubset/2]).
-import(qc, [buildFullNode/3, node_await/1, modRing/1, fetchCountry/2]).


% Gossip Message: {gossip, FromId, FromProc, {SuccesorsNeirestToRecipient, PredecessorsNeirestToRecipient}}
% Gossip Response: {gossip2, {SuccesorsNeirestToRecipient, PredecessorsNeirestToRecipient}
% Next Cycle Message: {nextcycle}

distributeSubsets([], _) -> done;
distributeSubsets([Node|Nodes], AllNodes) ->
    {_, Pid} = Node,
    Pid ! {init, randomSubset(lists:delete(Node, AllNodes), getC(nbinit))},
    distributeSubsets(Nodes, AllNodes).

node_init(NodeId) ->
    receive
        {init, NodeSubset} ->
            cycle(NodeId, sortByDist(NodeSubset, NodeId), getC(nbcycles))
    end.

cycle(NodeId, Neighbors, 1) ->
    Pred = hd(sortByPredDist(Neighbors, NodeId)),
    FullNode = buildFullNode(NodeId, Pred, sortBySuccDist(Neighbors, NodeId)),
    %timer:sleep(random:uniform(100)),
    %printTmanNode(NodeId, Neighbors),
    %io:format("TMan Node ~p start chord~n", [NodeId]),
    node_await(FullNode);
cycle(NodeId, Neighbors, CycleNr) ->
    {RandomNeighborId, RandomNeighborProc} = randomElem(Neighbors),
    WaitMs = random:uniform(getC(cycletimems)),
    erlang:send_after(WaitMs, RandomNeighborProc, {gossip, NodeId, self(), nearestNeighbors(Neighbors, RandomNeighborId)}),
    erlang:send_after(getC(cycletimems), self(), {nextcycle}),
    cycle_waiting(NodeId, Neighbors, CycleNr).


cycle_waiting(NodeId, Neighbors, CycleNr) ->
    receive
        {gossip, FromId, FromProc, {NewSuccs, NewPreds}} ->
            FromProc ! {gossip2, nearestNeighbors(Neighbors, FromId)},
            %cycle_waiting(NodeId, sortByDist(lists:concat([Neighbors, NewSuccs, NewPreds]), NodeId), CycleNr);
            cycle_waiting(NodeId, mergeNeighbors([Neighbors, NewSuccs, NewPreds], NodeId), CycleNr);
        {gossip2, {NewSuccs, NewPreds}} ->
            %cycle_waiting(NodeId, sortByDist(lists:concat([Neighbors, NewSuccs, NewPreds]), NodeId), CycleNr);
            cycle_waiting(NodeId, mergeNeighbors([Neighbors, NewSuccs, NewPreds], NodeId), CycleNr);
        {nextcycle} ->
            cycle(NodeId, Neighbors, CycleNr-1);
        {printstate} ->
            printTmanNode(NodeId, Neighbors),
            cycle_waiting(NodeId, Neighbors, CycleNr)
    end.


%% ===== FUNCTIONS ===== %%

printTmanNode(NodeId, Neighbors) ->
    Out1 = io_lib:format("~n~nNode ~p has Neighbors:~n", [NodeId]),
    Out2 = lists:map(fun({Id,_}) -> io_lib:format("~c~p~n", [9, Id]) end, Neighbors),
    Out = lists:flatten([Out1|Out2]),
    io:fwrite(Out).

mergeNeighbors(Lists, NodeId) ->
    lists:delete({NodeId, self()}, sortByDist(lists:concat(Lists), NodeId)).


nearestNeighbors(Neighbors, OtherId) ->
    SortedBySucc = sortBySuccDist(Neighbors, OtherId),
    MsgSize = getC(m) div 2,
    FromSuccs = lists:sublist(SortedBySucc, MsgSize),
    FromPreds = lists:sublist(sortByPredDist(Neighbors, OtherId), getC(m) div 2),
    {FromSuccs, FromPreds}.

% Sort by distance
sortByDist(L, Id) ->
    Compare = fun({A,_},{B,_}) -> dist(A, Id) =< dist(B, Id) end,
    lists:usort(Compare, L).

sortBySuccDist(L, Id) ->
    Compare = fun({A,_}, {B,_}) -> distSucc(Id, A) =< distSucc(Id, B) end,
    lists:usort(Compare, L).

sortByPredDist(L, Id) ->
    Compare = fun({A,_}, {B,_}) -> distPred(Id, A) =< distPred(Id, B) end,
    lists:usort(Compare, L).

distSucc(A, B) when B-A >= 0 -> B-A; % (MyId, Successor)
distSucc(A, B) -> B - A + (1 bsl getC(t)).

distPred(A, B) when A-B >= 0 -> A-B;
distPred(A, B) -> A - B + (1 bsl getC(t)).

% Calculate the distance between A and B on the chord ring
dist(A, B) -> min(abs(A-B), (1 bsl getC(t))-abs(A-B)).

randomElem(L) ->
    Idx = random:uniform(length(L)),
    lists:nth(Idx, L).


setNthElem(L, Idx, Elem) ->
    lists:sublist(L, Idx-1) ++ [Elem] ++ lists:nthtail(Idx, L).

