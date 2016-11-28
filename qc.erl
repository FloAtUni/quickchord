% Chord and T-Man algorithm

% Compile with: c(qc).
% Examples:
%    quickchord:master().

-module(qc).

% TODO replace with -export([master/2,...]).
-compile(export_all).

-define(M, 8). % Number of bites of the identifier

hash(Identifier) ->
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Identifier),
    X rem (1 bsl ?M). % calculates 2^m

gen_node_id() ->
    Salt = atom_to_list(node()),
    RandomNumber = random:uniform(16#ffffffff),
    Identifier = integer_to_list(RandomNumber) ++ Salt,
    hash(Identifier).

-record(node,
       {nodeid,
        successors = [],
        predecessor,
        fingers = [],
        data}).

new_node() ->
    Id = gen_node_id,
    #node{nodeid=Id, data=dict:new()}.

node_start(Self, InitialNode) ->


node_await(Self) ->
    receive
        {ping} ->
             pass
    end.

master() ->
    InitialProc = spawn(?MODULE, node_await, [new_node()]),

    pass.
