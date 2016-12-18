-module(demo1_teda).

-export([master/0]).

-import(qc, [new_node/1, init_initial_node/1, join/2, get_id/1]).

master() ->
    random:seed(now()),
    {ok, [_,Node1,Node2,Node3,Node4|_]} = file:consult('enodes.conf'),
    InitialProc = spawn(Node1, qc, init_initial_node, [new_node(0)]),
    InitialId = get_id(InitialProc),
    timer:sleep(50),
    SecondProc = spawn(Node2, qc, join, [new_node(1), {InitialId, InitialProc}]),
    timer:sleep(50),
    ThirdProc = spawn(Node3, qc, join, [new_node(3), {InitialId, InitialProc}]),
    timer:sleep(50),
    ForthProc = spawn(Node4, qc, join, [new_node(6), {InitialId, InitialProc}]),

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
