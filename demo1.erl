-module(demo1).

-export([master/0]).

-import(qc, [new_node/1, init_initial_node/1, join/2, get_id/1]).

master() ->
    random:seed(now()),
    InitialProc = spawn(?MODULE, init_initial_node, [new_node(0)]),
    InitialId = get_id(InitialProc),
    SecondProc = spawn(?MODULE, join, [new_node(1), {InitialId, InitialProc}]),
    SecondId = get_id(SecondProc),
    ThirdProc = spawn(?MODULE, join, [new_node(3), {InitialId, InitialProc}]),
    ThirdId = get_id(ThirdProc),
    ForthProc = spawn(?MODULE, join, [new_node(6), {InitialId, InitialProc}]),
    ForthId = get_id(ForthProc),
    FifthProc = spawn(?MODULE, join, [new_node(5), {InitialId, InitialProc}]),
    FifthId = get_id(FifthProc),
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
    FifthProc ! {printstate},
    timer:sleep(200),
    pass.
