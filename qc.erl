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


-define(T, 3). % Default: 8, Number of bites of the identifier

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
    io:format("Created new Node with id: ~p~n", [Id]),
    #node{nodeid=Id, data=dict:new()}.
new_node(Id) ->
    io:format("Created new Node with id: ~p~n", [Id]),
    #node{nodeid=Id, data=dict:new()}.

id({Id, _}) -> Id.
proc({_, Proc}) -> Proc.

printNode(Node) ->
    try
        io:format("~nNode ~p: ~n", [Node#node.nodeid]),
        io:format("~cSucessor: ~p~n", [9, id(hd(Node#node.fingers))]),
        io:format("~cPredecessor: ~p~n", [9, id(Node#node.predecessor)]),
        PrintFinger = fun({Idx, Finger}) -> printFinger(Node, Idx, Finger) end,
        Fingers = Node#node.fingers,
        IndexedFingers = lists:zip(lists:seq(1, length(Fingers)), Fingers),
        lists:map(PrintFinger, IndexedFingers),
        io:format("~n~n")
    catch
        exit:function_clause -> io:format("Error printing node~n");
        exit:badarg -> io:format("Error printing node~n");
        _:_ -> io:format("Error printing node: unhandled exception~n")
    end.

printFinger(Node, Idx, {FingerId, _}) ->
    Start = finger_start(Node, Idx),
    End = finger_start(Node, Idx+1),
    io:format("~cstart: ~p, range: [~p, ~p), succ:~p~n", [9, Start, Start, End, FingerId]).

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
            io:format("Node ~p: Got GetId Query~n", [MyId]),
            FromProc ! {gotid, MyId};
        {getsucc, FromProc} ->
            io:format("Node ~p: Got GetSucc Query~n", [MyId]),
            FromProc ! {gotsucc, successor(Self)};
        {getpred, FromProc} ->
            io:format("Node ~p: Got GetPred Query~n", [MyId]),
            FromProc ! {gotpred, predecessor(Self)};
        {closestprecedingfinger, FromProc, Id} ->
            io:format("Node ~p: Got ClosestPrecedingFinger Query~n", [MyId]),
            FromProc ! {gotclosestprecedingfinger, closest_preceding_finger(Self, Id)};
        {findpred, From, Id} ->
            io:format("Node ~p: Got FindPred Query~n", [MyId]),
            Pred = find_predecessor(Self, Id),
            From ! {foundpred, Pred};
        {findsucc, From, Id} ->
            io:format("Node ~p: Got FindSuccessor Query~n", [MyId]),
            Succ = find_successor(Self, Id),
            From ! {foundsucc, Succ};
        {newpred, Pred} ->
            io:format("Node ~p: Got New Predecessor Update Query~n", [MyId]),
            node_await(Self#node{predecessor=Pred});
        {updatefinger, NewNode, Idx} ->
            io:format("Node ~p: Got UpdateFinger Query with Index ~p~n", [MyId, Idx]),
            NewSelf = update_finger(Self, self(), NewNode, Idx),
            node_await(NewSelf);
        {printstate} ->
            io:format("Node ~p: Got PrintState Query~n", [MyId]),
            %io:format("~p~n~n", [Self])
            printNode(Self)
    end,
    node_await(Self).

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
    io:format("get_closest_preceding_finger on self~n"),
    closest_preceding_finger(Self, Id);
get_closest_preceding_finger(_, {_, NodeProc}, Id) ->
    io:format("get_closest_preceding_finger rpc~n"),
    NodeProc ! {closestprecedingfinger, self(), Id},
    receive
        {gotclosestprecedingfinger, NewNode} -> NewNode
    end.

% {findsucc, From, Id} Find successor to node with id

find_predecessor(Self, Id) ->
    io:format("Find predecessor called on self()~n"),
    MyId = Self#node.nodeid,
    findpred(Self, {MyId, self()}, Id).

find_predecessor(_, {_, SomeNodeProc}, Id) ->
    io:format("Find predecessor called for rpc~n"),
    SomeNodeProc ! {findpred, self(), Id},
    receive
        {foundpred, Pred} ->
            Pred
    end.

findpred(Self, {PredId, PredProc}, Id) ->
    io:format("~nFindPred with n'=~p and id=~p called~n", [PredId, Id]),
    io:format("With Node: "),
    printNode(Self),
    {SuccToPredId, _} = get_succ(Self, {PredId, PredProc}),
    io:format("Successor: ~p~n~n", [SuccToPredId]),
    IsBetween = is_betweenOC(Id, PredId, SuccToPredId),
    io:format("Is Id(~p) Between PredId(~p) and his Successor(~p)~n", [Id, PredId, SuccToPredId]),
    if
        IsBetween -> {PredId, PredProc};
        true ->
            ClosestPrecFinger = get_closest_preceding_finger(Self, {PredId, PredProc}, Id),
            io:format("Closest Preceeding Finger: ~p~n", [ClosestPrecFinger]),
            findpred(Self, ClosestPrecFinger, Id)
    end.

find_successor(Self, Id) ->
    io:format("Find successor called on self()~n"),
    Pred = find_predecessor(Self, Id),
    get_succ(Self, Pred).
find_successor(_, {_, OtherProc}, Id) ->
    io:format("Find successor sends rpc~n"),
    OtherProc ! {findsucc, self(), Id},
    receive
        {foundsucc, Succ} ->
            Succ
    end.

% Node has the new predecessor Pred
send_new_predecessor(Self, {_, NodeProc}, Pred) when NodeProc =:= self() ->
    Self#node{predecessor=Pred};
send_new_predecessor(Self, {_,NodeProc}, Pred) ->
    NodeProc ! {newpred, Pred},
    Self.


update_finger(Self, RecipientProc, NewNode, Index) when RecipientProc =:= self() ->
    MyId = Self#node.nodeid,
    {NewNodeId,_} = NewNode,
    {NthFingerId, _} = lists:nth(Index, Self#node.fingers),
    IsBetween = is_betweenCC(NewNodeId, MyId, NthFingerId),
    if
        IsBetween ->
            io:format("Updated Finger[~p], New Node(~p) between me(~p) and current finger(~p)~n", [Index, NewNodeId, MyId, NthFingerId]),
            Fingers = setNthElem(Self#node.fingers, Index, NewNode),
            {_, PredProc} = Self#node.predecessor,
            update_finger(Self, PredProc, NewNode, Index),
            Self#node{fingers=Fingers};
        true ->
            io:format("Not Updated Finger[~p], New Node(~p) not between me(~p) and current finger(~p)~n", [Index, NewNodeId, MyId, NthFingerId]),
            Self
    end;
update_finger(Self, RecipientProc, NewNode, Index) ->
    RecipientProc ! {updatefinger, NewNode, Index},
    Self.


% {foundsucc, Succ} The response to above query
% {findpred, From, Id} Find predecessor to node with id
% {foundpred, Pred} The response to above query

get_fingers(Self, {LastFingerId, LastFingerProc}, Idx) ->
    MyId = Self#node.nodeid,
    Start = finger_start(Self, Idx),
    ReuseLastFinger = is_between(Start, MyId, LastFingerId),
    NewFinger = if
        ReuseLastFinger -> {LastFingerId, LastFingerProc};
        true -> find_successor(Self, Start)
    end,
    if
        Idx == ?T -> [NewFinger];
        true -> [NewFinger|get_fingers(Self, NewFinger, Idx+1)]
    end.

init_finger_table(Self, SomeNode) ->
    Successor = find_successor(Self, SomeNode, finger_start(Self, 1)),
    Predecessor = get_pred(Self, Successor),
    Self2 = Self#node{predecessor=Predecessor, fingers=[Successor]},
    send_new_predecessor(Self2, Successor, {Self2#node.nodeid, self()}),
    Fingers = [Successor|get_fingers(Self2, Successor, 2)],
    Self2#node{predecessor=Predecessor, fingers=Fingers}.

update_others(Self, Idx) ->
    MyId = Self#node.nodeid,
    MyNode = {MyId, self()},
    SearchId = mod(1 + MyId - (1 bsl (Idx-1)), (1 bsl ?T)), % TODO 1 +... because otherwise we dont find nodeid=searchid
    {PredId, PProc} = find_predecessor(Self, SearchId),
    io:format("Node ~p: Predecessor to SearchId(~p) is ~p~n", [MyId, SearchId, PredId]),
    update_finger(Self, PProc, MyNode, Idx),
    if
        Idx == ?T ->
            pass;
        Idx =< ?T -> update_others(Self, Idx+1)
    end.


join(Self, SomeNode) ->
    Self2 = init_finger_table(Self, SomeNode),
    update_others(Self2, 1),
    node_await(Self2).

%% ===== FOR TMAN ===== %%
buildFullNode(NodeId, Predecessor, SuccNeighbors) ->

    pass.

buildFinger(Idx, SuccNeighbors, LastFinger) ->
    pass.

%% ===== FUNCTIONS ===== %%

finger_start(Self, Idx) ->
    MyId = Self#node.nodeid,
    Start = MyId + (1 bsl (Idx-1)),
    modRing(Start).

closest_preceding_finger(Self, Id) ->
    get_closest_finger(lists:reverse(Self#node.fingers), Self#node.nodeid, Id).


get_closest_finger([], _, _) ->
    io:format("Error: get_closest_finger got empty list~n"),
    erlang:error(fingers_empty);
get_closest_finger([Finger], _, _) -> Finger;
get_closest_finger([{FingerId,FingerPID}|Fingers], MyId, Id) ->
    io:format("closest_finger: FingerId(~p) between me(~p) and id(~p)~n", [FingerId, MyId, Id]),
    IsBetween = is_betweenCC(FingerId, MyId, Id) andalso FingerId /= MyId,
    if
        IsBetween ->
            {FingerId,FingerPID};
        true ->
            get_closest_finger(Fingers, MyId, Id)
    end.


is_between(_, From, To) when From == To -> true;
is_between(Id, From, To) when From < To ->
    Id >= From andalso Id =< To;
is_between(Id, From, To) when From > To ->
    Id >= From orelse Id =< To.

% Open Closed: Id != From, but Id==To
% If From = To we assume the whole circle is meant
is_betweenOC(_, From, To) when From == To -> true;
is_betweenOC(Id, From, To) when From < To ->
    Id > From andalso Id =< To;
is_betweenOC(Id, From, To) when From > To ->
    Id > From orelse Id =< To.


is_betweenCC(_, From, To) when From == To -> true;
is_betweenCC(Id, From, To) when From < To ->
    Id > From andalso Id < To;
is_betweenCC(Id, From, To) when From > To ->
    Id > From orelse Id < To.

mod(X,Y) -> (X rem Y + Y) rem Y.
modRing(X) -> mod(X, (1 bsl ?T)).

setNthElem(L, Idx, Elem) ->
    lists:sublist(L, Idx-1) ++ [Elem] ++ lists:nthtail(Idx, L).

master() ->
    random:seed(now()),
    InitialProc = spawn(?MODULE, init_initial_node, [new_node(0)]),
    InitialId = get_id(InitialProc),
    SecondProc = spawn(?MODULE, join, [new_node(1), {InitialId, InitialProc}]),
    SecondId = get_id(SecondProc),
    ThirdProc = spawn(?MODULE, join, [new_node(3), {InitialId, InitialProc}]),
    ThirdId = get_id(ThirdProc),
    ForthProc = spawn(?MODULE, join, [new_node(6), {InitialId, InitialProc}]),
    ForthId = get_id(ThirdProc),
    %io:format("Chord with 2 nodes: ~p(~p), ~p(~p)", [InitialId, InitialProc, SecondId, SecondProc]),

    timer:sleep(200),
    InitialProc ! {printstate},
    timer:sleep(200),
    SecondProc ! {printstate},
    timer:sleep(200),
    ThirdProc ! {printstate},
    timer:sleep(200),
    ForthProc ! {printstate},
    timer:sleep(200),
    pass.
