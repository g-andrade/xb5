-module(b5_ranks_v1_test_helpers).

-export([
    shuffle/1,
    take_random/2,
    sort_by/2,
    random_number/0,
    random_int32/0,
    randomly_switch_key_type/1,
    new_key_after/1,
    generate_kvs/2,
    for_each_tree/2,
    error_not_to_be_called/0,
    error_not_to_be_called/1,
    error_not_to_be_called/2
]).

%% ------------------------------------------------------------------
%% Tweaks
%% ------------------------------------------------------------------

-hank([
    {unnecessary_function_arguments, [
        {error_not_to_be_called, 1, 1},
        {error_not_to_be_called, 2, 1},
        {error_not_to_be_called, 2, 2}
    ]}
]).

%% ------------------------------------------------------------------
%% List Utilities
%% ------------------------------------------------------------------

%% @doc Shuffle a list randomly
shuffle(List) ->
    Tagged = [{rand:uniform(), X} || X <- List],
    Sorted = lists:sort(Tagged),
    [X || {_, X} <- Sorted].

%% @doc Take N random elements from a list
take_random(List, N) when N >= length(List) ->
    shuffle(List);
take_random(List, N) when N > 0 ->
    Shuffled = shuffle(List),
    lists:sublist(Shuffled, N);
take_random(_List, 0) ->
    [].

%% @doc Sort list by key function result
sort_by(List, KeyFun) ->
    Tagged = [{KeyFun(X), X} || X <- List],
    Sorted = lists:sort(Tagged),
    [X || {_, X} <- Sorted].

%% ------------------------------------------------------------------
%% Random Number Generation
%% ------------------------------------------------------------------

%% @doc Generate random number (int or float)
random_number() ->
    case rand:uniform(2) of
        1 -> random_int32();
        2 -> float(random_int32())
    end.

%% @doc Generate random 32-bit integer
random_int32() ->
    (1 bsl 31) - rand:uniform(1 bsl 32).

%% @doc Randomly switch between int and float representation
randomly_switch_key_type(Key) when is_number(Key) ->
    case rand:uniform(2) of
        1 -> trunc(Key);
        2 -> float(Key)
    end;
randomly_switch_key_type(Key) ->
    Key.

%% @doc Generate new key that is greater than given number
new_key_after(Number) when is_number(Number) ->
    NewNumber = Number + rand:uniform(1000),
    randomly_switch_key_type(NewNumber).

%% ------------------------------------------------------------------
%% Test Data Generation
%% ------------------------------------------------------------------

%% @doc Generate key-value pairs for testing
%% Returns {ExistentMap, NonExistentKeys}
generate_kvs(_N, TreeSizes) when is_list(TreeSizes) ->
    [generate_kvs(Size, undefined) || Size <- TreeSizes];
generate_kvs(N, _) ->
    generate_kvs_impl(N, [], []).

generate_kvs_impl(N, [], []) when N > 0 ->
    NonExistentKey = random_number(),
    ExistentKey = new_key_after(NonExistentKey),
    NewValue = random_number(),
    generate_kvs_impl(N - 1, [{ExistentKey, NewValue}], [NonExistentKey]);
generate_kvs_impl(N, [{PrevK, _} | _] = ExistentAcc, NonExistentAcc) when N > 0 ->
    NonExistentKey = new_key_after(PrevK),
    ExistentKey = new_key_after(NonExistentKey),
    NewValue = random_number(),
    generate_kvs_impl(
        N - 1,
        [{ExistentKey, NewValue} | ExistentAcc],
        [NonExistentKey | NonExistentAcc]
    );
generate_kvs_impl(0, [{PrevK, _} | _] = ExistentAcc, NonExistentAcc) ->
    NonExistentKey = new_key_after(PrevK),
    NonExistentAcc2 = [NonExistentKey | NonExistentAcc],

    ExistentMap = maps:from_list(ExistentAcc),
    NonExistentKeys = shuffle(NonExistentAcc2),
    {ExistentMap, NonExistentKeys}.

%% ------------------------------------------------------------------
%% Test Iteration Framework
%% ------------------------------------------------------------------

%% @doc Execute test function for each tree size
for_each_tree(TreeSizes, TestFun) ->
    lists:foreach(
        fun(TreeSize) ->
            {ExistentMap, NonExistentKeys} = generate_kvs(TreeSize, undefined),
            Tree = b5_ranks_v1:from_list(maps:to_list(ExistentMap)),
            case b5_ranks_v1:validate(Tree) of
                {ok, _} -> ok;
                Error -> error({validation_failed, Error, TreeSize})
            end,
            TestFun(Tree, ExistentMap, NonExistentKeys)
        end,
        TreeSizes
    ).

-spec error_not_to_be_called() -> no_return().
error_not_to_be_called() ->
    error(not_to_be_called).

-spec error_not_to_be_called(_) -> no_return().
error_not_to_be_called(_) ->
    error(not_to_be_called).

-spec error_not_to_be_called(term(), term()) -> no_return().
error_not_to_be_called(_, _) ->
    error(not_to_be_called).
