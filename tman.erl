-module(tman).
-export([master/0]).

-define(N, 10). % Number of nodes
-define(M, 8). % Number of bites of the identifier
-define(NBINIT, 5). % Number of initial neighbors
-define(CycleTimeMs, 200). % The cycle interval for the T-Man to ask neighbors

hash(Identifier) ->
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Identifier),
    X rem (1 bsl ?M). % calculates 2^m

gen_node_id() ->
    Salt = atom_to_list(node()),
    RandomNumber = rand:uniform(16#ffffffff),
    Identifier = integer_to_list(RandomNumber) ++ Salt,
    hash(Identifier).


master() ->
    %rand:seed(now()),
    NodeIds = [gen_node_id() || list:seq(1,?N)],
    Nodes = [ {NodeId, spawn(?MODULE, node_init, [NodeId])} || NodeId <- NodeIds],

    % send to each node a random subset of nodes (initial neighbors)
    [Pid ! {init, randomSubset(Nodes, ?NBINIT)} || Pid <- Nodes].

node_init(NodeId) ->
    receive
        {init, NodeSubset} ->
            node_init(NodeId, NodeSubset)
    end.
            
node_init(NodeId, Neighbors) ->
    NB = rand:uniform(1,lists:length(Neighbors)),
    Neighbor = lists:nth(NB, Neighbors),
    % Nearest: a neighbor from Neighbors with smallest distance to Neighbor
    lists:sort(fun(A,B) -> abs(A - Neighbor) =< abs(B - Neighbor) end, Neighbors),
    Nearest = lists:nth(1, Neighbors),
    WaitMs = rand:uniform(?CycleTimeMs),
    erlang:send_after(WaitMs, self(), {gossip, NodeId, self(), Nearest}).
    % TODO

% TODO
cycle(Neighbors, Timeout) ->
    Now = erlang:timestamp(),
    receive
        {gossip, T} ->
            true
    end.


%% ===== FUNCTIONS ===== %%    
    
% Num: the size of the random sublist of nodes
randomSubset(NodesList, Num) ->
    lists:sublist( [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- NodesList])], Num).
