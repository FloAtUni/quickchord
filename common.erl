-module(common).

-import(config, [getC/1]).
-export([sum/1, setNthElem/3, randomElem/1, is_between/3, is_betweenCC/3,
         is_betweenCO/3, is_betweenOC/3, randomSubset/2, mod/2, modRing/1,
        hash/1, gen_node_id/0]).

hash(Identifier) ->
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Identifier),
    modRing(X). %X rem (1 bsl ?T). % calculates 2^m

gen_node_id() ->
    Salt = atom_to_list(node()),
    RandomNumber = random:uniform(16#ffffffff),
    Identifier = integer_to_list(RandomNumber) ++ Salt,
    hash(Identifier).


mod(X,Y) -> (X rem Y + Y) rem Y.
modRing(X) -> mod(X, (1 bsl getC(t))).

is_between(_, From, To) when From == To -> true;
is_between(Id, From, To) when From < To ->
    Id >= From andalso Id =< To;
is_between(Id, From, To) when From > To ->
    Id >= From orelse Id =< To.

is_betweenOC(_, From, To) when From == To -> true;
is_betweenOC(Id, From, To) when From < To ->
    Id > From andalso Id =< To;
is_betweenOC(Id, From, To) when From > To ->
    Id > From orelse Id =< To.

is_betweenCO(_, From, To) when From == To -> true;
is_betweenCO(Id, From, To) when From < To ->
    Id >= From andalso Id < To;
is_betweenCO(Id, From, To) when From > To ->
    Id >= From orelse Id < To.

is_betweenCC(_, From, To) when From == To -> true;
is_betweenCC(Id, From, To) when From < To ->
    Id > From andalso Id < To;
is_betweenCC(Id, From, To) when From > To ->
    Id > From orelse Id < To.

setNthElem(L, Idx, Elem) ->
    lists:sublist(L, Idx-1) ++ [Elem] ++ lists:nthtail(Idx, L).

randomElem(L) ->
    Idx = random:uniform(length(L)),
    lists:nth(Idx, L).

% Use the reservoir algorithm to choose a random subset of K nodes
randomSubset(NodesList, K) when length(NodesList) < K->
    erlang:error("Sample Size bigger than Population");
randomSubset(NodesList, K) ->
    {Sample, Rest} = lists:split(K, NodesList),
    randomSubset(Sample, Rest, K, K+1).

randomSubset(Sample, [], _, _) ->
    Sample;
randomSubset(Sample, [X|Remaining], K, I) ->
    R = random:uniform(I),
    if
        R-1 < K ->
            randomSubset(setNthElem(Sample, R, X), Remaining, K, I+1);
        R > K ->
            randomSubset(Sample, Remaining, K, I+1);
        true -> io:format("R: ~p, K: ~p", [R, K])
    end.

sum(L) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, L).
