-module(config).

-define(N, 1024). % Number of nodes
-define(T, 32). %3 For Demo, Default: 8, Number of bites of the identifier
-define(M, 16). %4 for Demo Message size for T-Man(even number)
-define(NBINIT, 20). %4 for demo. Number of initial neighbors
-define(CycleTimeMs, 500). %50ms for Demo. The cycle interval for the T-Man to ask neighbors in Miliseconds
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
