-module(xb5_test_utils).

-include_lib("stdlib/include/assert.hrl").

%%%%%

-export([foreach_tested_size/1]).

%%%%%

-define(MANDATORILY_TESTED_SIZES,
    (lists:seq(0, 30, 1) ++ lists:seq(40, 200, 20))
).

%%%%%

foreach_tested_size(Fun) ->
    Iterations = 200,

    RandomSizesAmount = Iterations - length(?MANDATORILY_TESTED_SIZES),
    ?assert(RandomSizesAmount >= 0),

    TestedSizes = prefix_random_sizes(RandomSizesAmount, ?MANDATORILY_TESTED_SIZES),

    lists:foreach(
        fun(Size) ->
            Fun(Size)
        end,
        TestedSizes
    ).

%%%%%

prefix_random_sizes(Amount, Acc) when Amount > 0 ->
    Size = new_random_size(),
    prefix_random_sizes(Amount - 1, [Size | Acc]);
prefix_random_sizes(0, Acc) ->
    Acc.

new_random_size() ->
    Die = rand:uniform(),

    if
        Die < 2 / 3 ->
            rand:uniform(201) - 1;
        %
        Die < 2 / 3 + (2 / 3 * 1 / 3) ->
            200 + rand:uniform(300);
        %
        true ->
            500 + rand:uniform(500)
    end.
