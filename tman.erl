-module(tman).
%-export([master/0]).

-compile(export_all).

-define(N, 10). % Number of nodes
-define(T, 8). % Number of bites of the identifier
-define(M, 2). % Message size for T-Man
-define(NBINIT, 5). % Number of initial neighbors
-define(CycleTimeMs, 200). % The cycle interval for the T-Man to ask neighbors in Miliseconds

hash(Identifier) ->
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Identifier),
    X rem (1 bsl ?T). % calculates 2^m

gen_node_id() ->
    Salt = atom_to_list(node()),
    RandomNumber = random:uniform(16#ffffffff),
    Identifier = integer_to_list(RandomNumber) ++ Salt,
    hash(Identifier).

% Gossip Message: {gossip, FromId, FromProc, {SuccesorsNeirestToRecipient, PredecessorsNeirestToRecipient}}
% Gossip Response: {gossip2, {SuccesorsNeirestToRecipient, PredecessorsNeirestToRecipient}
% Next Cycle Message: {nextcycle}

master() ->
    random:seed(now()),
    NodeIds = [gen_node_id() || list:seq(1,?N)],
    Nodes = [ {NodeId, spawn(?MODULE, node_init, [NodeId])} || NodeId <- NodeIds],

    % send to each node a random subset of nodes (initial neighbors)
    [Pid ! {init, randomSubset(Nodes, ?NBINIT)} || Pid <- Nodes].

node_init(NodeId) ->
    receive
        {init, NodeSubset} ->
            cycle(NodeId, sortByDist(NodeSubset, NodeId), 1)
    end.

cycle(NodeId, Neighbors, CycleNr) ->
    {RandomNeighborId, RandomNeighborProc} = randomElem(Neighbors),
    WaitMs = random:uniform(?CycleTimeMs),
    erlang:send_after(WaitMs, RandomNeighborProc, {gossip, NodeId, self(), nearestNeighbors(Neighbors, RandomNeighborId)}),
    erlang:send_after(?CycleTimeMs, self(), {nextcycle}),
    cycle_waiting(NodeId, Neighbors, CycleNr).


cycle_waiting(NodeId, Neighbors, CycleNr) ->
    receive
        {gossip, FromId, FromProc, {NewSuccs, NewPreds}} ->
            FromProc ! {gossip2, nearestNeighbors(Neighbors, FromId)},
            cycle_waiting(NodeId, sortByDist(lists:concat([Neighbors, NewSuccs, NewPreds]), NodeId), CycleNr);
        {gossip2, {NewSuccs, NewPreds}} ->
            cycle_waiting(NodeId, sortByDist(lists:concat([Neighbors, NewSuccs, NewPreds]), NodeId), CycleNr);
        {nextcycle} ->
            cycle(NodeId, Neighbors, CycleNr+1)
    end.

%% ===== FUNCTIONS ===== %%

nearestNeighbors(Neighbors, OtherId) ->
    FromSuccs = lists:sublist(sortBySuccDist(Neighbors, OtherId), ?M / 2),
    FromPreds = lists:sublist(sortByPredDist(Neighbors, OtherId), ?M / 2),
    {FromSuccs, FromPreds}.

% Sort by distance
sortByDist(L, Id) ->
    Compare = fun(A,B) -> dist(A, Id) =< dist(B, Id) end,
    lists:sort(Compare, L).

sortBySuccDist(L, Id) ->
    Compare = fun(A, B) -> distSucc(Id, A) =< distSucc(Id, B) end,
    lists:sort(Compare, L).

sortByPredDist(L, Id) ->
    Compare = fun(A, B) -> distPred(Id, A) =< distPred(Id, B) end,
    lists:sort(Compare, L).

distSucc(A, B) when B-A >= 0 -> B-A; % (MyId, Successor)
distSucc(A, B) -> B - A + (1 bsl ?T).

distPred(A, B) when A-B >= 0 -> A-B;
distPred(A, B) -> A - B + (1 bsl ?T).

% Calculate the distance between A and B on the chord ring
dist(A, B) -> min(abs(A-B), (1 bsl ?T)-abs(A-B)).

randomElem(L) ->
    Idx = random:uniform(list:length(L)),
    lists:nth(Idx, L).


% Use the reservoir algorithm to choose a random subset of K nodes
randomSubset(NodesList, K) when length(NodesList) < K->
    erlang:error("Sample Size bigger than Population");
randomSubset(NodesList, K) ->
    {Sample, Rest} = lists:split(K, NodesList),
    randomSubset(Sample, Rest, K, K).

randomSubset(Sample, [], _, _) ->
    Sample;
randomSubset(Sample, [X|Remaining], K, I) ->
    R = random:uniform(I),
    if
        R-1 < K ->
            randomSubset(setNthElem(Sample, R, X), Remaining, K, I+1);
        R > K ->
            randomSubset(Sample, Remaining, K, I+1);
        true -> io:format("R: ~p, K: ~p", [R, K])
    end.

setNthElem(L, Idx, Elem) ->
    lists:sublist(L, Idx-1) ++ [Elem] ++ lists:nthtail(Idx, L).
