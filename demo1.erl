-module(demo1).

-export([master/0]).

-import(qc, [new_node/1, init_initial_node/1, join/2, get_id/1]).

master() ->
    random:seed(now()),
    InitialProc = spawn(qc, init_initial_node, [new_node(0)]),
    InitialId = get_id(InitialProc),
    timer:sleep(50),
    SecondProc = spawn(qc, join, [new_node(1), {InitialId, InitialProc}]),
    timer:sleep(50),
    ThirdProc = spawn(qc, join, [new_node(3), {InitialId, InitialProc}]),
    timer:sleep(50),
    ForthProc = spawn(qc, join, [new_node(6), {InitialId, InitialProc}]),

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
