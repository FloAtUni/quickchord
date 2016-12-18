-module(tests).

-export([testRandomSubset/2]).

count(Counter, []) -> Counter;
count(Counter, [Element|Tail]) ->
    C = lists:nth(Element, Counter),
    Counter2 = common:setNthElem(Counter, Element, C+1),
    count(Counter2, Tail).
                                                % Just to make sure that randomSubset works as expected
testRandomSubset(Rounds, SampleSize) ->
    Counters = [0 || _ <- lists:seq(1,SampleSize)],
    testRandomSubset(Counters, Rounds, SampleSize).

testRandomSubset(Counters, 0, _) -> Counters;
testRandomSubset(Counters, Rounds, SampleSize) ->
    Counters2 = count(Counters, common:randomSubset(lists:seq(1, SampleSize), 3)),
    testRandomSubset(Counters2, Rounds-1, SampleSize).
