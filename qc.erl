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


-define(T, 8). % Number of bites of the identifier

hash(Identifier) ->
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Identifier),
    X rem (1 bsl ?T). % calculates 2^m

gen_node_id() ->
    Salt = atom_to_list(node()),
    RandomNumber = random:uniform(16#ffffffff),
    Identifier = integer_to_list(RandomNumber) ++ Salt,
    hash(Identifier).

-record(node,
       {nodeid,
        predecessor,
        fingers = [], % List of {id, proc} of length
        data}).

successor(Self) -> hd(Self#node.fingers).
predecessor(Self) -> Self#node.predecessor.

new_node() ->
    Id = gen_node_id(),
    #node{nodeid=Id, data=dict:new()}.

init_initial_node(Self) ->
    MyId = Self#node.nodeid,
    SelfRef = {MyId, self()},
    Fingers = lists:duplicate(?T, SelfRef),
    NewSelf = Self#node{predecessor=SelfRef, fingers=Fingers},
    node_await(NewSelf).


node_await(Self) ->
    MyId = Self#node.nodeid,
    receive
        {getid, FromProc} ->
            FromProc ! {gotId, MyId};
        {getsucc, FromProc} ->
            FromProc ! {gotsucc, successor(Self)};
        {getpred, FromProc} ->
            FromProc ! {gotpred, predecessor(Self)};
        {closestprecedingfinger, FromProc, Id} ->
            FromProc ! {gotclosestprecedingfinger, closest_preceding_finger(Self, Id)};
        {findpred, From, Id} ->
            Pred = find_predecessor(Self, {MyId, self()}, Id),
            From ! {foundpred, Pred};
        {findsucc, From, Id} ->
            Succ = find_successor(Self, Id),
            From ! {foundsucc, Succ};
        {newpred, Pred} ->
            node_await(Self#node{predecessor=Pred})
    end.

get_pred(Self, {_, NodeProc}) when NodeProc =:= self() ->
    predecessor(Self);
get_pred(_, {_, NodeProc}) ->
    NodeProc ! {getpred, self()},
    receive
        {gotpred, Predecessor} ->
            Predecessor
    end.

get_succ(Self, {_, NodeProc}) when NodeProc =:= self() ->
    successor(Self);
get_succ(_, {_, NodeProc}) ->
    NodeProc ! {getsucc, self()},
    receive
        {gotsucc, Successor} ->
            Successor
    end.

% Only used by the master
get_id(NodeProc) ->
    NodeProc ! {getid, self()},
    receive
        {gotid, Id} -> Id
    end.

get_closest_preceding_finger(Self, {_, NodeProc}, Id) when NodeProc =:= self() ->
    closest_preceding_finger(Self, Id);
get_closest_preceding_finger(_, {_, NodeProc}, Id) ->
    NodeProc ! {closestprecedingfinger, self(), Id},
    receive
        {gotclosestprecedingfinger, NewNode} -> NewNode
    end.

% {findsucc, From, Id} Find successor to node with id

find_predecessor(Self,{PredId, PredProc}, Id) ->
    {SuccToPredId, _} = get_succ(Self, {PredId, PredProc}),
    if
        PredId < Id  andalso Id =< SuccToPredId -> {PredId, PredProc};
        true -> find_predecessor(Self, get_closest_preceding_finger(Self, {PredId, PredProc}, Id), Id)
    end.

find_successor(Self, Id) ->
    MyId = Self#node.nodeid,
    Succ = find_predecessor(Self, {MyId, self()}, Id),
    get_succ(Self, Succ).

% Node has the new predecessor Pred
send_new_predecessor(Self, {_, NodeProc}, Pred) when NodeProc =:= self() ->
    Self#node{predecessor=Pred};
send_new_predecessor(Self, {_,NodeProc}, Pred) ->
    NodeProc ! {newpred, Pred},
    Self.

send_new_finger()

% {foundsucc, Succ} The response to above query
% {findpred, From, Id} Find predecessor to node with id
% {foundpred, Pred} The response to above query

get_fingers(Self, {LastFingerId, LastFingerProc}, Idx) ->
    MyId = Self#node.nodeid,
    Start = finger_start(Self, 1),
    ReuseLastFinger = is_between(Start, MyId, LastFingerId),
    NewFinger = if
        ReuseLastFinger -> {LastFingerId, LastFingerProc};
        true -> find_successor(Self, Start)
    end,
    if
        Idx == ?T-1 -> [NewFinger];
        true -> [NewFinger|get_fingers(Self, NewFinger, Idx+1)]
    end.

init_finger_table(Self) ->
    Successor = find_successor(Self, finger_start(Self, 1)),
    Predecessor = get_pred(Self, Successor),
    send_new_predecessor(Self, Successor, {Self#node.nodeid, self()}),
    Fingers = [Successor|get_fingers(Self, Successor, 2)],
    Self#node{predecessor=Predecessor, fingers=Fingers}.



join(Self, SomeNode) ->
    Self = init_finger_table(Self),
    pass.

%% ===== FUNCTIONS ===== %%

finger_start(Self, Idx) ->
    MyId = Self#node.nodeid,
    MyId + (1 bsl (Idx-1)).

closest_preceding_finger(Self, Id) ->
    get_closest_finger(lists:reverse(Self#node.fingers), Self#node.nodeid, Id).


get_closest_finger([{FingerId,FingerPID}|Fingers], MyId, Id) ->
    IsBetween = is_between(FingerId, MyId, Id),
    if
        IsBetween ->
            {FingerId,FingerPID};
        true ->
            get_closest_finger(Fingers, MyId, Id)
    end.


is_between(Id, From, To) when From =< To ->
    Id >= From andalso Id =< To;
is_between(Id, From, To) when From > To ->
    Id >= From orelse Id =< To.

master() ->
    InitialProc = spawn(?MODULE, init_initial_node, [new_node()]),
    InitialId = get_id(InitialProc),
    SecondProc = spawn(?MODULE, join, [new_node(), {InitialId, InitialProc}]),
    SecondId = get_id(SecondProc),
    io:format("Chord with 2 nodes: ~p(~p), ~p(~p)", [InitialId, InitialProc, SecondId, SecondProc]),
    pass.
