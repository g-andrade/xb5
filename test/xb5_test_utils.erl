-module(xb5_test_utils).

-include_lib("stdlib/include/assert.hrl").

%%%%%

-export([
    foreach_tested_size/1,
    list_enumerate/1,
    canon_key/1
]).

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

list_enumerate(L) -> list_enumerate(1, 1, L).

canon_key(Element) when is_float(Element) ->
    case math:fmod(Element, 1.0) == 0 of
        true ->
            trunc(Element);
        %
        false ->
            Element
    end;
canon_key(Integer) when is_integer(Integer) ->
    Integer;
canon_key([H | T]) ->
    [canon_key(H) | canon_key(T)];
canon_key(Tuple) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    Mapped = lists:map(fun canon_key/1, List),
    list_to_tuple(Mapped);
canon_key(Map) when is_map(Map) ->
    List = maps:to_list(Map),
    Mapped = lists:map(fun canon_key/1, List),
    maps:from_list(Mapped);
canon_key(Other) ->
    Other.

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

%%%%%%

-if(OTP_RELEASE >= 26).

list_enumerate(Index, Step, L) -> lists:enumerate(Index, Step, L).

-else.

list_enumerate(Index, Step, [H | T]) ->
    [{Index, H} | list_enumerate(Index + Step, Step, T)];
list_enumerate(_, _, []) ->
    [].

-endif.
