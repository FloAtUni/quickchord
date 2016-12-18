-module(config).

-define(N, 6). % Number of nodes
-define(T, 8). %3 For Demo, Default: 8, Number of bites of the identifier
-define(M, 4). % Message size for T-Man(even number)
-define(NBINIT, 2). % Number of initial neighbors
-define(CycleTimeMs, 50). % The cycle interval for the T-Man to ask neighbors in Miliseconds
-define(NBCycles, 15).

-export([getC/1]).

getC(Key) ->
    case Key of
        n -> ?N;
        t -> ?T;
        m -> ?M;
        nbinit -> ?NBINIT;
        cycletimems -> ?CycleTimeMs;
        nbcycles -> ?NBCycles
    end.
