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

-import(config, [getC/1]).
-import(common, [sum/1, setNthElem/3, randomElem/1, modRing/1, mod/2, hash/1]).

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
    %io:format("Created new Node with id: ~p~n", [Id]),
    #node{nodeid=Id, data=dict:new()}.
new_node(Id) ->
    %io:format("Created new Node with id: ~p~n", [Id]),
    #node{nodeid=Id, data=dict:new()}.

id({Id, _}) -> Id.
proc({_, Proc}) -> Proc.

printNode(Node) ->
    try
        io:format("~nNode ~p: ~n", [Node#node.nodeid]),
        io:format("~cSucessor: ~p~n", [9, id(hd(Node#node.fingers))]),
        io:format("~cPredecessor: ~p~n", [9, id(Node#node.predecessor)]),
        io:format("~cKeys stored: ~p~n", [9, dict:fetch_keys(Node#node.data)]),
        PrintFinger = fun({Idx, Finger}) -> printFinger(Node, Idx, Finger) end,
        Fingers = Node#node.fingers,
        IndexedFingers = lists:zip(lists:seq(1, length(Fingers)), Fingers),
        lists:map(PrintFinger, IndexedFingers),
        io:format("~n~n")
    catch
        exit:function_clause -> io:format("Error printing node, empty list~n");
        exit:badarg -> io:format("Error printing node, missing field~n");
        _:_ -> io:format("Error printing node: unhandled exception~n")
    end.

printFinger(Node, Idx, {FingerId, _}) ->
    Start = finger_start(Node, Idx),
    End = finger_start(Node, Idx+1),
    io:format("~cstart: ~p, range: [~p, ~p), succ:~p~n", [9, Start, Start, End, FingerId]).

init_initial_node(Self) ->
    MyId = Self#node.nodeid,
    SelfRef = {MyId, self()},
    Fingers = lists:duplicate(getC(t), SelfRef),
    NewSelf = Self#node{predecessor=SelfRef, fingers=Fingers},
    node_await(NewSelf).


node_await(Self) ->
    MyId = Self#node.nodeid,
    receive
        {getid, FromProc} ->
            %io:format("Node ~p: Got GetId Query~n", [MyId]),
            FromProc ! {gotid, MyId},
            node_await(Self);
        {getsucc, FromProc} ->
            %io:format("Node ~p: Got GetSucc Query~n", [MyId]),
            FromProc ! {gotsucc, successor(Self)},
            node_await(Self);
        {getpred, FromProc} ->
            %io:format("Node ~p: Got GetPred Query~n", [MyId]),
            FromProc ! {gotpred, predecessor(Self)},
            node_await(Self);
        {closestprecedingfinger, FromProc, Id} ->
            %io:format("Node ~p: Got ClosestPrecedingFinger Query~n", [MyId]),
            FromProc ! {gotclosestprecedingfinger, closest_preceding_finger(Self, Id)},
            node_await(Self);
        {findpred, From, Id} ->
            %io:format("Node ~p: Got FindPred Query~n", [MyId]),
            Pred = find_predecessor(Self, Id),
            From ! {foundpred, Pred},
            node_await(Self);
        {findsucc, From, Id} ->
            %io:format("Node ~p: Got FindSuccessor Query~n", [MyId]),
            Succ = find_successor(Self, Id),
            From ! {foundsucc, Succ},
            node_await(Self);
        {newpred, Pred} ->
            %io:format("Node ~p: Got New Predecessor Update Query~n", [MyId]),
            node_await(Self#node{predecessor=Pred});
        {updatefinger, NewNode, Idx} ->
            %io:format("Node ~p: Got UpdateFinger Query with Index ~p~n", [MyId, Idx]),
            NewSelf = update_finger(Self, self(), NewNode, Idx),
            node_await(NewSelf);
        {setkey, Key, Value} ->
            Data = dict:store(Key, Value, Self#node.data),
            node_await(Self#node{data=Data});
        {getkey, From, Key} ->
            Dict = Self#node.data,
            Result = dict:find(Key, Dict),
            case Result of
            %case dict:find(Key, Self#node.data) of
                {ok, Value} -> From ! {gotkey, Value};
                error ->  From ! {gotkey, notfound}
            end,
            node_await(Self);
        {insert, From, Key, Value} ->
            KeyHash = hash(Key),
            Succ = find_successor(Self, KeyHash),
            Self2 = set_key(Self, Succ, Key, Value),
            From ! {storedkey, id(Succ)},
            node_await(Self2);
        {fetch, From, Key} ->
            KeyHash = hash(Key),
            Succ = find_successor(Self, KeyHash),
            Value = get_key(Self, Succ, Key),
            From ! {fetched, Value},
            node_await(Self);
        {hopcount, From, Key} ->
            KeyHash = hash(Key),
            Count = hopcount(Self, KeyHash)+1, %plus successor
            From ! {hopcount, Count},
            node_await(Self);
        {printstate} ->
            %io:format("Node ~p: Got PrintState Query~n", [MyId]),
            %io:format("~p~n~n", [Self])
            printNode(Self),
            node_await(Self);
        {exit} -> exit
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

get_key(Self, {_,NodeProc}, Key) when NodeProc == self() ->
    Dict = Self#node.data,
    dict:find(Key, Dict);
get_key(_, {_,NodeProc}, Key) ->
    NodeProc ! {getkey, self(), Key},
    receive
        {gotkey, notfound} -> error;
        {gotkey, Value} -> {ok, Value}
    end.


set_key(Self, {_,NodeProc}, Key, Value) when NodeProc == self() ->
    Data = dict:store(Key, Value, Self#node.data),
    Self#node{data=Data};
set_key(Self, {_,NodeProc}, Key, Value) ->
    NodeProc ! {setkey, Key, Value},
    Self.

get_closest_preceding_finger(Self, {_, NodeProc}, Id) when NodeProc =:= self() ->
    %io:format("get_closest_preceding_finger on self~n"),
    closest_preceding_finger(Self, Id);
get_closest_preceding_finger(_, {_, NodeProc}, Id) ->
    %io:format("get_closest_preceding_finger rpc~n"),
    NodeProc ! {closestprecedingfinger, self(), Id},
    receive
        {gotclosestprecedingfinger, NewNode} -> NewNode
    end.

hopcount(Self, Id) ->
    MyId = Self#node.nodeid,
    {Hopcount, _} = findpred(Self, {MyId, self()}, Id),
    Hopcount.

find_predecessor(Self, Id) ->
    %io:format("Find predecessor called on self()~n"),
    MyId = Self#node.nodeid,
    {_, Pred} = findpred(Self, {MyId, self()}, Id),
    Pred.

find_predecessor(_, {_, SomeNodeProc}, Id) ->
    %io:format("Find predecessor called for rpc~n"),
    SomeNodeProc ! {findpred, self(), Id},
    receive
        {foundpred, Pred} ->
            Pred
    end.

findpred(Self, {PredId, PredProc}, Id) ->
    %io:format("~nFindPred with n'=~p and id=~p called~n", [PredId, Id]),
    %io:format("With Node: "),
    %printNode(Self),
    {SuccToPredId, _} = get_succ(Self, {PredId, PredProc}),
    %io:format("Successor: ~p~n~n", [SuccToPredId]),
    IsBetween = common:is_betweenOC(Id, PredId, SuccToPredId),
    %io:format("Is Id(~p) Between PredId(~p) and his Successor(~p)~n", [Id, PredId, SuccToPredId]),
    if
        IsBetween -> {1, {PredId, PredProc}};
        true ->
            ClosestPrecFinger = get_closest_preceding_finger(Self, {PredId, PredProc}, Id),
            %io:format("Closest Preceeding Finger: ~p~n", [ClosestPrecFinger]),
            {HopCount, Pred} = findpred(Self, ClosestPrecFinger, Id),
            {HopCount+1, Pred}
    end.

find_successor(Self, Id) ->
    %io:format("Find successor called on self()~n"),
    Pred = find_predecessor(Self, Id),
    get_succ(Self, Pred).
find_successor(_, {_, OtherProc}, Id) ->
    %io:format("Find successor sends rpc~n"),
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
    IsBetween = common:is_betweenCC(NewNodeId, MyId, NthFingerId),
    if
        IsBetween ->
            %io:format("Updated Finger[~p], New Node(~p) between me(~p) and current finger(~p)~n", [Index, NewNodeId, MyId, NthFingerId]),
            Fingers = setNthElem(Self#node.fingers, Index, NewNode),
            {_, PredProc} = Self#node.predecessor,
            update_finger(Self, PredProc, NewNode, Index),
            Self#node{fingers=Fingers};
        true ->
            %io:format("Not Updated Finger[~p], New Node(~p) not between me(~p) and current finger(~p)~n", [Index, NewNodeId, MyId, NthFingerId]),
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
    ReuseLastFinger = common:is_between(Start, MyId, LastFingerId),
    T = getC(t),
    NewFinger = if
        ReuseLastFinger -> {LastFingerId, LastFingerProc};
        true -> find_successor(Self, Start)
    end,
    if
        Idx == T -> [NewFinger];
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
    T = getC(t),
    SearchId = mod(1 + MyId - (1 bsl (Idx-1)), (1 bsl T)), % TODO 1 +... because otherwise we dont find nodeid=searchid
    {_, PProc} = find_predecessor(Self, SearchId),
    %io:format("Node ~p: Predecessor to SearchId(~p) is ~p~n", [MyId, SearchId, PredId]),
    update_finger(Self, PProc, MyNode, Idx),
    if
        Idx == T ->
            pass;
        Idx =< T -> update_others(Self, Idx+1)
    end.


join(Self, SomeNode) ->
    Self2 = init_finger_table(Self, SomeNode),
    update_others(Self2, 1),
    node_await(Self2).

%% ===== FOR TMAN ===== %%

buildFullNode(NodeId, Predecessor, SuccNeighbors) ->
    Fingers = buildFinger(NodeId, 1, SuccNeighbors, hd(SuccNeighbors)),
    #node{nodeid=NodeId, predecessor=Predecessor, fingers=Fingers, data=dict:new()}.

lastFinger([], Backup) -> Backup;
lastFinger(L, _) -> lists:last(L).

buildFinger(MyId, Idx, SuccNeighbors, LastFinger) ->
    IntervalEnd = finger_start(MyId, Idx+1),
    FilterFun = fun({Id,_}) -> common:is_betweenCO(Id, MyId, IntervalEnd) end,
    {Fingers, Tail} = lists:splitwith(FilterFun, SuccNeighbors),
    T = getC(t),
    Finger = case Fingers of
                 [] -> LastFinger;
                 _ -> randomElem(Fingers)
             end,
    if
        Idx == T -> [Finger];
        true -> [Finger|buildFinger(MyId, Idx+1, Tail, lastFinger(Fingers, Finger))]
    end.

%% ===== IMPORT/EXPORT


insertData(Proc, DataRows) ->
    lists:foreach(fun({Key,Value}) ->
                          Proc ! {insert, self(), Key, Value},
                          receive
                              {storedkey, _} -> pass
                          end
                  end, DataRows).

insertDataFromFile({_, Proc}, FileName) ->
    {ok, DataRows} = file:consult(FileName),
    io:format("Read all Input Data~n"),
    insertData(Proc, DataRows).

insertDataFromFileP(Nodes, FileName) ->
    {ok, DataRows} = file:consult(FileName),
    N = min(length(Nodes), DataRows),
    M = length(DataRows) div length(Nodes),
    {Rest, Pids} = lists:foldl(fun(Node, {RestData, Procs}) ->
                                    {MyDataRows, RestData2} = lists:split(M, RestData),
                                    Pid = spawn(?MODULE, insertWorker, [self(), Node, MyDataRows]),
                                    {RestData2,[Pid|Procs]}
                            end, {DataRows,[]},lists:sublist(Nodes, N)),
    insertData(proc(hd(Nodes)), Rest),
    lists:foreach(fun(_) ->
                          receive
                              {finishedwork} -> pass
                          end
                  end, Pids),
    io:format("All worker finished~n").

insertWorker(Parent, {_,NodeProc}, DataRows) ->
    lists:foreach(fun({Key,Value}) ->
                          NodeProc ! {insert, self(), Key, Value},
                          receive
                              {storedkey, _} -> pass
                          end
                  end, DataRows),
    Parent ! {finishedwork}.

fetchDataRow({_, Proc}, Key) ->
    Proc ! {fetch, self(), Key},
    receive
        {fetched, Resp} -> Resp
    end.

fetchHopcount({_, Proc}, Key) ->
    Proc ! {hopcount, self(), Key},
    receive
        {hopcount, Count} -> Count
    end.


failureRate(Nodes, Keys) ->
    N = min(length(Nodes), Keys),
    M = length(Keys) div length(Nodes),
    {_, Pids} = lists:foldl(fun(Node, {RestKeys, Procs}) ->
                             {MyKeys, RestKeys2} = lists:split(M, RestKeys),
                             Pid = spawn(?MODULE, failureWorker, [self(), Node, MyKeys]),
                             {RestKeys2,[Pid|Procs]}
                     end, {Keys,[]},lists:sublist(Nodes, N)),
    Counters = lists:map(fun(_) ->
                      receive
                          {finishedwork, Failures} -> Failures
                      end
                  end, Pids),
    sum(Counters).

failureRateWorker(Parent, Node, Keys) ->
    Fetches = lists:map(fun(Key) -> fetchDataRow(Node, Key) end, Keys),
    Failures = lists:foldl(
      fun (A, B) ->
              case A of
                  {ok, _} -> B;
                   error -> B+1
              end
      end, 0, Fetches),
    Parent ! {finishedwork, Failures}.

%% ===== FUNCTIONS ===== %%

finger_start(NodeId, Idx) when is_integer(NodeId) ->
    Start = NodeId + (1 bsl (Idx-1)),
    modRing(Start);
finger_start(Self, Idx) ->
    MyId = Self#node.nodeid,
    finger_start(MyId, Idx).

closest_preceding_finger(Self, Id) ->
    get_closest_finger(lists:reverse(Self#node.fingers), Self#node.nodeid, Id).


get_closest_finger([], _, _) ->
    %io:format("Error: get_closest_finger got empty list~n"),
    erlang:error(fingers_empty);
get_closest_finger([Finger], _, _) -> Finger;
get_closest_finger([{FingerId,FingerPID}|Fingers], MyId, Id) ->
    %io:format("closest_finger: FingerId(~p) between me(~p) and id(~p)~n", [FingerId, MyId, Id]),
    IsBetween = common:is_betweenCC(FingerId, MyId, Id) andalso FingerId /= MyId,
    if
        IsBetween ->
            {FingerId,FingerPID};
        true ->
            get_closest_finger(Fingers, MyId, Id)
    end.
