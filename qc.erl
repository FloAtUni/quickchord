% Chord and T-Man algorithm

% Compile with: c(qc).
% Examples:
%    quickchord:master().


% Messages:
% {getsucc, From} Get successor of receipient
% {findsucc, From, Id} Find successor to node with id
% {foundsucc, Succ} The response to above query
% {findpred, From, Id} Find predecessor to node with id
% {foundpred, Pred} The response to above query
% {getprefing, From, Id} Get the closest preceding finger
% {gotprefing, PreFing} Get the closest preceding finger

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

init_initial_node(Self) ->
    MyId = Self#node.nodeid,
    SelfRef = {MyId, self()},
    Fingers = lists:duplicate(?M, SelfRef),
    NewSelf = Self#node{successors=[SelfRef], predecessor=SelfRef, fingers=Fingers},
    node_await(NewSelf).


node_await(Self) ->
    receive
        {getprefing, From, Id} ->
            ClosestFinger = get_closest_finger(Self#node.fingers, Self#node.nodeid, Id),
            From ! {gotprefing, ClosestFinger};
        {findpred, From, Id} ->
            Pred = find_predecessor(Self, Id),
            From ! {foundpred, Pred}
    end.


% {findsucc, From, Id} Find successor to node with id
% {getprefing, From, Id} Get the closest preceding finger
%find_predecessor(Pred, Id) ->




% {foundsucc, Succ} The response to above query
% {findpred, From, Id} Find predecessor to node with id
% {foundpred, Pred} The response to above query
% {gotprefing, PreFing} Get the closest preceding finger

%join(Self, SomeNode) ->

get_closest_finger([{FingerId,FingerPID}|Fingers], MyId, Id) ->
    IsBetween = is_between(FingerId, MyId, Id),
    if
        IsBetween ->
            {FingerId,FingerPID};
        true ->
            get_closest_finger(Fingers, MyId, Id)
    end.


is_between(Id, From, To) when From <= To ->
    Id >= From and Id <= To.
is_between(Id, From, To) when From > To ->
    Id >= From or Id <= To.

master() ->
    InitialProc = spawn(?MODULE, node_await, [new_node()]),
    pass.
