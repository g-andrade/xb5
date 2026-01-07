%% @doc B5 Trees test suite ported from Elixir test suite
-module(b5_trees_test_SUITE).

-ifndef(NO_CT_SUITE_BEHAVIOUR).
-behaviour(ct_suite).
-endif.

-include_lib("stdlib/include/assert.hrl").

%% CT exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

%% Test exports - Basic API tests
-export([
    test_from_list_repeated_keys/1,
    test_get_operations/1,
    test_to_list_operations/1,
    test_empty_function/1,
    test_constituent_parts/1
]).

%% Test exports - Tree size tests
-export([
    test_insert_operations/1,
    test_insert_with_operations/1,
    test_update_operations/1,
    test_update_with_3_operations/1,
    test_update_with_4_operations/1,
    test_smallest_largest_operations/1,
    test_delete_operations/1,
    test_keys_values_operations/1,
    test_smaller_larger_operations/1,
    test_take_smallest_operations/1,
    test_take_largest_operations/1,
    test_take_operations/1,
    test_iterator_operations/1,
    test_iterator_ordered_operations/1,
    test_iterator_reversed_operations/1,
    test_iterator_from_operations/1,
    test_iterator_from_ordered_operations/1,
    test_iterator_from_reversed_operations/1,
    test_foldl_foldr_operations/1,
    test_map_operations/1,
    test_enter_operations/1,
    test_is_defined_operations/1,
    test_lookup_operations/1,
    test_take_any_operations/1
]).

%% Test constants
-define(REGULAR_TREE_SIZES,
    lists:seq(1, 50) ++
        [
            55,
            60,
            65,
            70,
            75,
            80,
            85,
            90,
            95,
            100,
            105,
            110,
            115,
            120,
            125,
            130,
            135,
            140,
            145,
            150,
            155,
            160,
            165,
            170,
            175,
            180,
            185,
            190,
            195,
            200
        ] ++ [997]
).

%% ------------------------------------------------------------------
%% CT Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [
        {basic_api, [parallel], [
            test_from_list_repeated_keys,
            test_get_operations,
            test_to_list_operations,
            test_empty_function,
            test_constituent_parts
        ]},
        {tree_operations, [parallel], [
            %{tree_operations, [], [  % Sequential for better debugging
            test_insert_operations,
            test_insert_with_operations,
            test_update_operations,
            test_update_with_3_operations,
            test_update_with_4_operations,
            test_smallest_largest_operations,
            test_delete_operations,
            test_keys_values_operations,
            test_smaller_larger_operations,
            test_take_smallest_operations,
            test_take_largest_operations,
            test_take_operations,
            test_iterator_operations,
            test_iterator_ordered_operations,
            test_iterator_reversed_operations,
            test_iterator_from_operations,
            test_iterator_from_ordered_operations,
            test_iterator_from_reversed_operations,
            test_foldl_foldr_operations,
            test_map_operations,
            test_enter_operations,
            test_is_defined_operations,
            test_lookup_operations,
            test_take_any_operations
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%% ------------------------------------------------------------------
%% Basic API Tests
%% ------------------------------------------------------------------

test_from_list_repeated_keys(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}, {1.0, c}, {4, d}]),
    %% Keys 1 and 1.0 are equal, so 1.0 should win (last one)
    ?assertEqual([{1.0, c}, {2, b}, {4, d}], b5_trees:to_list(Tree)).

test_get_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertError({badkey, foobar}, b5_trees:get(foobar, EmptyTree)),

    %% Test with multiple tree sizes
    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            logger:notice("Tree: ~p", [Tree]),

            %% Test existent keys
            maps:fold(
                fun(Key, ExpectedValue, _) ->
                    logger:notice("Key: ~p", [Key]),
                    Key2 = b5_trees_test_helpers:randomly_switch_key_type(Key),
                    ?assertEqual(ExpectedValue, b5_trees:get(Key2, Tree))
                end,
                ok,
                ExistentMap
            ),

            %% Test non-existent keys
            lists:foreach(
                fun(Key) ->
                    ?assertError({badkey, Key}, b5_trees:get(Key, Tree))
                end,
                b5_trees_test_helpers:take_random(NonExistentKeys, 10)
            )
        end
    ).

test_to_list_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual([], b5_trees:to_list(EmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, _NonExistentKeys) ->
            Expected = b5_trees_test_helpers:sort_by(
                maps:to_list(ExistentMap),
                fun({K, _}) -> K end
            ),
            ?assertEqual(Expected, b5_trees:to_list(Tree))
        end
    ).

test_empty_function(_Config) ->
    %% Test that empty/0 is equivalent to new/0
    EmptyTree1 = b5_trees:empty(),
    EmptyTree2 = b5_trees:new(),
    ?assertEqual(EmptyTree2, EmptyTree1),
    ?assert(b5_trees:is_empty(EmptyTree1)),
    ?assertEqual(0, b5_trees:size(EmptyTree1)),
    ?assertEqual([], b5_trees:to_list(EmptyTree1)).

test_constituent_parts(_Config) ->
    %% Test the low-level constituent parts API
    EmptyTree = b5_trees:new(),

    %% Test empty tree round-trip
    {ok, Parts1} = b5_trees:to_constituent_parts(EmptyTree),
    ?assertMatch(#{root := _, size := 0}, Parts1),
    ReconstructedEmpty = b5_trees:from_constituent_parts(Parts1),
    ?assertEqual(EmptyTree, ReconstructedEmpty),

    %% Test with non-empty tree
    Tree = b5_trees:from_list([{1, a}, {2, b}, {3, c}]),
    {ok, Parts2} = b5_trees:to_constituent_parts(Tree),
    ?assertMatch(#{root := _, size := 3}, Parts2),
    ReconstructedTree = b5_trees:from_constituent_parts(Parts2),
    ?assertEqual(Tree, ReconstructedTree),
    ?assertEqual(b5_trees:to_list(Tree), b5_trees:to_list(ReconstructedTree)),
    ?assertEqual(b5_trees:size(Tree), b5_trees:size(ReconstructedTree)),

    %% Test that constituent parts have the expected structure
    #{root := Root, size := Size} = Parts2,
    ?assertEqual(3, Size),
    ?assert(Root =/= b5_trees_node:new()),

    %% Test error case for invalid input
    ?assertEqual(error, b5_trees:to_constituent_parts(invalid_tree)),
    ?assertEqual(error, b5_trees:to_constituent_parts(not_a_tree)),
    ?assertEqual(error, b5_trees:to_constituent_parts({some, random, tuple})).

%% ------------------------------------------------------------------
%% Tree Operation Tests
%% ------------------------------------------------------------------

test_insert_operations(_Config) ->
    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            ExistentKvs = b5_trees_test_helpers:sort_by(
                maps:to_list(ExistentMap),
                fun({K, _}) -> K end
            ),
            ShuffledNonExistent = b5_trees_test_helpers:shuffle(NonExistentKeys),
            test_inserts(Tree, ExistentKvs, ShuffledNonExistent)
        end
    ).

test_insert_with_operations(_Config) ->
    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            ExistentKvs = b5_trees_test_helpers:sort_by(
                maps:to_list(ExistentMap),
                fun({K, _}) -> K end
            ),
            ShuffledNonExistent = b5_trees_test_helpers:shuffle(NonExistentKeys),
            test_inserts_with(Tree, ExistentKvs, ShuffledNonExistent)
        end
    ).

test_update_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertError({badkey, foobar}, b5_trees:update(foobar, v, EmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            ExistentKvs = b5_trees_test_helpers:sort_by(
                maps:to_list(ExistentMap),
                fun({K, _}) -> K end
            ),
            test_updates(Tree, ExistentKvs, ExistentKvs, NonExistentKeys)
        end
    ).

test_update_with_3_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertError(
        {badkey, foobar},
        b5_trees:update_with(foobar, fun(V) -> V end, EmptyTree)
    ),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            ExistentKvs = b5_trees_test_helpers:sort_by(
                maps:to_list(ExistentMap),
                fun({K, _}) -> K end
            ),
            test_updates_with3(Tree, ExistentKvs, ExistentKvs, NonExistentKeys)
        end
    ).

test_update_with_4_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertError(
        {badkey, foobar},
        b5_trees:update_with(foobar, fun(V) -> V end, EmptyTree)
    ),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            ExistentKvs = b5_trees_test_helpers:sort_by(
                maps:to_list(ExistentMap),
                fun({K, _}) -> K end
            ),
            test_updates_with4(Tree, ExistentKvs, ExistentKvs, NonExistentKeys)
        end
    ).

%% ------------------------------------------------------------------
%% Helper Functions (translated from Elixir)
%% ------------------------------------------------------------------

test_inserts(Tree, ExistentKvs, []) ->
    ?assertEqual(ExistentKvs, b5_trees:to_list(Tree)),
    ?assertEqual(length(ExistentKvs), b5_trees:size(Tree));
test_inserts(Tree, ExistentKvs, [NonExistentKey | Rest]) ->
    %% Test that inserting existing keys fails
    ExistentSample = b5_trees_test_helpers:take_random(ExistentKvs, 10),
    lists:foreach(
        fun({Key, _V}) ->
            Key2 = b5_trees_test_helpers:randomly_switch_key_type(Key),
            ?assertError({key_exists, Key2}, b5_trees:insert(Key2, v, Tree))
        end,
        ExistentSample
    ),

    %% Insert new key
    Value = b5_trees_test_helpers:random_number(),
    InsertKey = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
    Tree2 = b5_trees:insert(InsertKey, Value, Tree),

    %% Verify insertion
    NewExistentKvs = orddict:store(InsertKey, Value, ExistentKvs),
    ?assertEqual(NewExistentKvs, b5_trees:to_list(Tree2)),
    ?assertEqual(length(NewExistentKvs), b5_trees:size(Tree2)),
    ?assertNot(b5_trees:is_empty(Tree2)),

    test_inserts(Tree2, NewExistentKvs, Rest).

test_inserts_with(Tree, ExistentKvs, []) ->
    ?assertEqual(ExistentKvs, b5_trees:to_list(Tree)),
    ?assertEqual(length(ExistentKvs), b5_trees:size(Tree));
test_inserts_with(Tree, ExistentKvs, [NonExistentKey | Rest]) ->
    %% Test that inserting existing keys fails (function shouldn't be called)
    ExistentSample = b5_trees_test_helpers:take_random(ExistentKvs, 10),
    lists:foreach(
        fun({Key, _V}) ->
            Key2 = b5_trees_test_helpers:randomly_switch_key_type(Key),
            ValueFun = fun b5_trees_test_helpers:error_not_to_be_called/0,
            ?assertError(
                {key_exists, Key2},
                b5_trees:insert_with(Key2, ValueFun, Tree)
            )
        end,
        ExistentSample
    ),

    %% Insert new key
    Value = b5_trees_test_helpers:random_number(),
    ValueFun = fun() -> Value end,
    InsertKey = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
    Tree2 = b5_trees:insert_with(InsertKey, ValueFun, Tree),

    %% Verify insertion
    NewExistentKvs = orddict:store(InsertKey, Value, ExistentKvs),
    ?assertEqual(NewExistentKvs, b5_trees:to_list(Tree2)),
    ?assertEqual(length(NewExistentKvs), b5_trees:size(Tree2)),
    ?assertNot(b5_trees:is_empty(Tree2)),

    test_inserts_with(Tree2, NewExistentKvs, Rest).

test_updates(_Tree, [], _ExistentKvs, _NonExistentKeys) ->
    ok;
test_updates(Tree, [{Key, _V} | NextKeys], ExistentKvs, NonExistentKeys) ->
    NewValue = b5_trees_test_helpers:random_number(),
    SizeBefore = b5_trees:size(Tree),

    UpdateKey = b5_trees_test_helpers:randomly_switch_key_type(Key),
    Tree2 = b5_trees:update(UpdateKey, NewValue, Tree),
    ?assertEqual(SizeBefore, b5_trees:size(Tree2)),
    ?assertNot(b5_trees:is_empty(Tree2)),

    NewExistentKvs = orddict:store(UpdateKey, NewValue, ExistentKvs),
    ?assertEqual(NewExistentKvs, b5_trees:to_list(Tree2)),

    %% Test updating non-existent keys fails
    NonExistentSample = b5_trees_test_helpers:take_random(NonExistentKeys, 10),
    lists:foreach(
        fun(NonExistentKey) ->
            Key2 = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
            ?assertError({badkey, Key2}, b5_trees:update(Key2, NewValue, Tree2))
        end,
        NonExistentSample
    ),

    test_updates(Tree2, NextKeys, NewExistentKvs, NonExistentKeys).

test_updates_with3(_Tree, [], _ExistentKvs, _NonExistentKeys) ->
    ok;
test_updates_with3(Tree, [{Key, PrevV} | NextKeys], ExistentKvs, NonExistentKeys) ->
    NewValue = b5_trees_test_helpers:random_number(),
    SizeBefore = b5_trees:size(Tree),

    UpdateKey = b5_trees_test_helpers:randomly_switch_key_type(Key),
    UpdateFun = fun(TreeV) ->
        ?assertEqual(PrevV, TreeV),
        NewValue
    end,

    Tree2 = b5_trees:update_with(UpdateKey, UpdateFun, Tree),
    ?assertEqual(SizeBefore, b5_trees:size(Tree2)),
    ?assertNot(b5_trees:is_empty(Tree2)),

    NewExistentKvs = orddict:store(UpdateKey, NewValue, ExistentKvs),
    ?assertEqual(NewExistentKvs, b5_trees:to_list(Tree2)),

    %% Test updating non-existent keys fails
    NonExistentSample = b5_trees_test_helpers:take_random(NonExistentKeys, 10),
    lists:foreach(
        fun(NonExistentKey) ->
            Key2 = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
            ?assertError(
                {badkey, Key2},
                b5_trees:update_with(Key2, fun(V) -> V end, Tree2)
            )
        end,
        NonExistentSample
    ),

    test_updates_with3(Tree2, NextKeys, NewExistentKvs, NonExistentKeys).

test_updates_with4(_Tree, ExistentAttempts, _ExistentKvs, []) when ExistentAttempts =:= [] ->
    ok;
test_updates_with4(_Tree, [], _ExistentKvs, NonExistentKeys) when NonExistentKeys =:= [] ->
    ok;
test_updates_with4(Tree, ExistentAttempts, ExistentKvs, NonExistentKeys) ->
    NewValue = b5_trees_test_helpers:random_number(),
    SizeBefore = b5_trees:size(Tree),

    case {ExistentAttempts, NonExistentKeys} of
        {[{ExistentKey, ExistentPrevV} | NextExistent], [NonExistentKey | NextNonExistent]} ->
            case rand:uniform(2) of
                % Test non-existent key (gets default)
                1 ->
                    UpdateKey = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
                    UpdateFun = fun b5_trees_test_helpers:error_not_to_be_called/1,
                    Tree2 = b5_trees:update_with(UpdateKey, UpdateFun, NewValue, Tree),
                    ?assertEqual(SizeBefore + 1, b5_trees:size(Tree2)),
                    ?assertNot(b5_trees:is_empty(Tree2)),

                    NewExistentKvs = orddict:store(UpdateKey, NewValue, ExistentKvs),
                    ?assertEqual(NewExistentKvs, b5_trees:to_list(Tree2)),
                    test_updates_with4(Tree2, ExistentAttempts, NewExistentKvs, NextNonExistent);
                % Test existent key (calls function)
                2 ->
                    UpdateKey = b5_trees_test_helpers:randomly_switch_key_type(ExistentKey),
                    UpdateFun = fun(TreeV) ->
                        ?assertEqual(ExistentPrevV, TreeV),
                        NewValue
                    end,
                    Tree2 = b5_trees:update_with(UpdateKey, UpdateFun, unused_default, Tree),
                    ?assertEqual(SizeBefore, b5_trees:size(Tree2)),
                    ?assertNot(b5_trees:is_empty(Tree2)),

                    NewExistentKvs = orddict:store(UpdateKey, NewValue, ExistentKvs),
                    ?assertEqual(NewExistentKvs, b5_trees:to_list(Tree2)),
                    test_updates_with4(Tree2, NextExistent, NewExistentKvs, NonExistentKeys)
            end;
        _ ->
            ok
    end.

%% ------------------------------------------------------------------
%% Additional Test Functions (from Elixir suite)
%% ------------------------------------------------------------------

test_smallest_largest_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertError(empty_tree, b5_trees:smallest(EmptyTree)),
    ?assertError(empty_tree, b5_trees:largest(EmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, _NonExistentKeys) ->
            {SmallestK, SmallestV} = hd(
                b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) -> K end)
            ),
            {LargestK, LargestV} = lists:last(
                b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) -> K end)
            ),
            ?assertEqual({SmallestK, SmallestV}, b5_trees:smallest(Tree)),
            ?assertEqual({LargestK, LargestV}, b5_trees:largest(Tree))
        end
    ).

test_delete_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertError({badkey, foobar}, b5_trees:delete(foobar, EmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            test_delete_each(Tree, ExistentMap, NonExistentKeys),
            test_delete_all(Tree, ExistentMap, NonExistentKeys)
        end
    ).

test_keys_values_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual([], b5_trees:keys(EmptyTree)),
    ?assertEqual([], b5_trees:values(EmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            ExpectedKeys = b5_trees_test_helpers:sort_by(maps:keys(ExistentMap), fun(K) -> K end),
            test_keys_impl(Tree, ExpectedKeys, b5_trees_test_helpers:shuffle(NonExistentKeys)),

            ExpectedAux = gb_trees:from_orddict(
                b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) -> K end)
            ),
            test_values_impl(Tree, ExpectedAux, b5_trees_test_helpers:shuffle(NonExistentKeys))
        end
    ).

test_smaller_larger_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual(none, b5_trees:smaller(any_key, EmptyTree)),
    ?assertEqual(none, b5_trees:larger(any_key, EmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            AllKeys = b5_trees_test_helpers:shuffle(maps:keys(ExistentMap) ++ NonExistentKeys),
            test_smaller_impl(Tree, ExistentMap, AllKeys),
            test_larger_impl(Tree, ExistentMap, AllKeys)
        end
    ).

test_take_smallest_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertError(empty_tree, b5_trees:take_smallest(EmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, _NonExistentKeys) ->
            ExistentKvs = b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) ->
                K
            end),
            test_take_all_smallest(Tree, ExistentKvs)
        end
    ).

test_take_largest_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertError(empty_tree, b5_trees:take_largest(EmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, _NonExistentKeys) ->
            ExistentKvs = lists:reverse(
                b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) -> K end)
            ),
            test_take_all_largest(Tree, ExistentKvs)
        end
    ).

test_take_operations(_Config) ->
    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            ExistentKvs = b5_trees_test_helpers:shuffle(maps:to_list(ExistentMap)),
            test_take_all(Tree, ExistentKvs, NonExistentKeys)
        end
    ).

test_iterator_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual(none, b5_trees:next(b5_trees:iterator(EmptyTree))),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, _NonExistentKeys) ->
            Iterator = b5_trees:iterator(Tree),
            ExpectedKvs = b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) ->
                K
            end),
            Next = b5_trees:next(Iterator),
            test_iterator_impl(ExpectedKvs, Next)
        end
    ).

test_iterator_ordered_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual(none, b5_trees:next(b5_trees:iterator(EmptyTree, ordered))),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, _NonExistentKeys) ->
            Iterator = b5_trees:iterator(Tree, ordered),
            ExpectedKvs = b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) ->
                K
            end),
            Next = b5_trees:next(Iterator),
            test_iterator_impl(ExpectedKvs, Next)
        end
    ).

test_iterator_reversed_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual(none, b5_trees:next(b5_trees:iterator(EmptyTree, reversed))),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, _NonExistentKeys) ->
            Iterator = b5_trees:iterator(Tree, reversed),
            ExpectedKvs = lists:reverse(
                b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) -> K end)
            ),
            Next = b5_trees:next(Iterator),
            test_iterator_impl(ExpectedKvs, Next)
        end
    ).

test_iterator_from_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual(none, b5_trees:next(b5_trees:iterator_from(any_key, EmptyTree))),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            AllKeys = lists:sort(maps:keys(ExistentMap) ++ NonExistentKeys),
            lists:foreach(
                fun(StartingKey) ->
                    StartingKey2 = b5_trees_test_helpers:randomly_switch_key_type(StartingKey),
                    Iterator = b5_trees:iterator_from(StartingKey2, Tree),

                    ExpectedKvs = lists:filter(
                        fun({K, _}) -> K >= StartingKey2 end,
                        b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) ->
                            K
                        end)
                    ),

                    Next = b5_trees:next(Iterator),
                    test_iterator_impl(ExpectedKvs, Next)
                end,
                AllKeys
            )
        end
    ).

test_iterator_from_ordered_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual(none, b5_trees:next(b5_trees:iterator_from(any_key, EmptyTree, ordered))),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            AllKeys = lists:sort(maps:keys(ExistentMap) ++ NonExistentKeys),
            lists:foreach(
                fun(StartingKey) ->
                    StartingKey2 = b5_trees_test_helpers:randomly_switch_key_type(StartingKey),
                    Iterator = b5_trees:iterator_from(StartingKey2, Tree, ordered),

                    ExpectedKvs = lists:filter(
                        fun({K, _}) -> K >= StartingKey2 end,
                        b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) ->
                            K
                        end)
                    ),

                    Next = b5_trees:next(Iterator),
                    test_iterator_impl(ExpectedKvs, Next)
                end,
                AllKeys
            )
        end
    ).

test_iterator_from_reversed_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual(none, b5_trees:next(b5_trees:iterator_from(any_key, EmptyTree, reversed))),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            AllKeys = lists:sort(maps:keys(ExistentMap) ++ NonExistentKeys),
            lists:foreach(
                fun(StartingKey) ->
                    StartingKey2 = b5_trees_test_helpers:randomly_switch_key_type(StartingKey),
                    Iterator = b5_trees:iterator_from(StartingKey2, Tree, reversed),

                    ExpectedKvs = lists:reverse(
                        lists:filter(
                            fun({K, _}) -> K =< StartingKey2 end,
                            b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) ->
                                K
                            end)
                        )
                    ),

                    Next = b5_trees:next(Iterator),
                    test_iterator_impl(ExpectedKvs, Next)
                end,
                AllKeys
            )
        end
    ).

test_foldl_foldr_operations(_Config) ->
    FoldFun = fun(K, V, Acc) -> [{K, V} | Acc] end,

    EmptyTree = b5_trees:new(),
    ?assertEqual([], b5_trees:foldl(FoldFun, [], EmptyTree)),
    ?assertEqual([], b5_trees:foldr(FoldFun, [], EmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, _NonExistentKeys) ->
            AccFoldl = b5_trees:foldl(FoldFun, [], Tree),
            ExpectedFoldl = lists:reverse(
                b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) -> K end)
            ),
            ?assertEqual(ExpectedFoldl, AccFoldl),

            AccFoldr = b5_trees:foldr(FoldFun, [], Tree),
            ExpectedFoldr = b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) ->
                K
            end),
            ?assertEqual(ExpectedFoldr, AccFoldr)
        end
    ).

test_map_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    MappedEmptyTree = b5_trees:map(fun b5_trees_test_helpers:error_not_to_be_called/2, EmptyTree),
    ?assertEqual(0, b5_trees:size(MappedEmptyTree)),
    ?assert(b5_trees:is_empty(MappedEmptyTree)),
    ?assertEqual([], b5_trees:to_list(MappedEmptyTree)),

    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, _NonExistentKeys) ->
            NewValues = maps:from_list([
                {K, b5_trees_test_helpers:random_number()}
             || {K, _V} <- maps:to_list(ExistentMap)
            ]),
            MapFun = fun(K, _V) -> maps:get(K, NewValues) end,
            MappedTree = b5_trees:map(MapFun, Tree),

            ?assertEqual(
                b5_trees_test_helpers:sort_by(maps:to_list(NewValues), fun({K, _}) -> K end),
                b5_trees:to_list(MappedTree)
            ),
            ?assertEqual(maps:size(ExistentMap), b5_trees:size(MappedTree)),
            ?assertNot(b5_trees:is_empty(MappedTree))
        end
    ).

test_enter_operations(_Config) ->
    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            KeysToEnter = b5_trees_test_helpers:shuffle(maps:keys(ExistentMap) ++ NonExistentKeys),
            ExistentKvs = b5_trees_test_helpers:sort_by(maps:to_list(ExistentMap), fun({K, _}) ->
                K
            end),
            test_enter_impl(Tree, KeysToEnter, ExistentKvs, NonExistentKeys)
        end
    ).

test_is_defined_operations(_Config) ->
    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            maps:fold(
                fun(Key, _ExpectedValue, _) ->
                    ?assert(b5_trees:is_defined(Key, Tree))
                end,
                ok,
                ExistentMap
            ),

            lists:foreach(
                fun(Key) ->
                    ?assertNot(b5_trees:is_defined(Key, Tree))
                end,
                NonExistentKeys
            )
        end
    ).

test_lookup_operations(_Config) ->
    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            maps:fold(
                fun(Key, ExpectedValue, _) ->
                    ?assertEqual({value, ExpectedValue}, b5_trees:lookup(Key, Tree))
                end,
                ok,
                ExistentMap
            ),

            lists:foreach(
                fun(Key) ->
                    ?assertEqual(none, b5_trees:lookup(Key, Tree))
                end,
                NonExistentKeys
            )
        end
    ).

test_take_any_operations(_Config) ->
    b5_trees_test_helpers:for_each_tree(
        ?REGULAR_TREE_SIZES,
        fun(Tree, ExistentMap, NonExistentKeys) ->
            ExistentKvs = b5_trees_test_helpers:shuffle(maps:to_list(ExistentMap)),
            test_take_any_impl(Tree, ExistentKvs, NonExistentKeys)
        end
    ).

%% ------------------------------------------------------------------
%% More Helper Functions (from Elixir suite)
%% ------------------------------------------------------------------

test_delete_each(Tree, ExistentMap, NonExistentKeys) ->
    maps:fold(
        fun(Key, ExpectedValue, _) ->
            DeleteKey = b5_trees_test_helpers:randomly_switch_key_type(Key),
            ?assertEqual(ExpectedValue, b5_trees:get(DeleteKey, Tree)),

            ExpectedListAfter = b5_trees_test_helpers:sort_by(
                maps:to_list(maps:remove(Key, ExistentMap)), fun({K, _}) -> K end
            ),
            Tree2 = b5_trees:delete(DeleteKey, Tree),

            ?assertEqual(ExpectedListAfter, b5_trees:to_list(Tree2)),
            ?assertEqual(maps:size(ExistentMap) - 1, b5_trees:size(Tree2)),
            ?assertEqual(maps:size(ExistentMap) =:= 1, b5_trees:is_empty(Tree2)),

            ?assertError({badkey, Key}, b5_trees:delete(Key, Tree2)),

            NonExistentSample = b5_trees_test_helpers:take_random(NonExistentKeys, 20),
            lists:foreach(
                fun(NonExistentKey) ->
                    NonExistentKey2 = b5_trees_test_helpers:randomly_switch_key_type(
                        NonExistentKey
                    ),
                    ?assertError({badkey, NonExistentKey2}, b5_trees:delete(NonExistentKey2, Tree2))
                end,
                NonExistentSample
            )
        end,
        ok,
        ExistentMap
    ).

test_delete_all(Tree, ExistentMap, NonExistentKeys) ->
    KvList = maps:to_list(ExistentMap),
    ElementsToDelete = b5_trees_test_helpers:shuffle(KvList),
    ElementsToExpect = b5_trees_test_helpers:sort_by(KvList, fun({K, _}) -> K end),
    test_delete_all_impl(Tree, ElementsToDelete, ElementsToExpect, NonExistentKeys).

test_delete_all_impl(Tree, [{Key, _ExpectedV} | Next], ElementsToExpected, NonExistentKeys) ->
    DeleteKey = b5_trees_test_helpers:randomly_switch_key_type(Key),
    Tree2 = b5_trees:delete(DeleteKey, Tree),

    ElementsToExpected2 = lists:keydelete(Key, 1, ElementsToExpected),
    ?assertEqual(ElementsToExpected2, b5_trees:to_list(Tree2)),
    ?assertEqual(length(ElementsToExpected2), b5_trees:size(Tree2)),
    ?assertEqual(ElementsToExpected2 =:= [], b5_trees:is_empty(Tree2)),

    ?assertError({badkey, Key}, b5_trees:delete(Key, Tree2)),

    NonExistentSample = b5_trees_test_helpers:take_random(NonExistentKeys, 20),
    lists:foreach(
        fun(NonExistentKey) ->
            NonExistentKey2 = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
            ?assertError({badkey, NonExistentKey2}, b5_trees:delete(NonExistentKey2, Tree2))
        end,
        NonExistentSample
    ),

    test_delete_all_impl(Tree2, Next, ElementsToExpected2, NonExistentKeys);
test_delete_all_impl(Tree, [], [], NonExistentKeys) ->
    ?assertEqual([], b5_trees:to_list(Tree)),
    ?assertEqual(0, b5_trees:size(Tree)),
    ?assert(b5_trees:is_empty(Tree)),

    NonExistentSample = b5_trees_test_helpers:take_random(NonExistentKeys, 10),
    lists:foreach(
        fun(NonExistentKey) ->
            NonExistentKey2 = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
            ?assertError({badkey, NonExistentKey2}, b5_trees:delete(NonExistentKey2, Tree))
        end,
        NonExistentSample
    ).

test_keys_impl(Tree, ExpectedKeys, [NonExistentKey | Next]) ->
    ?assertEqual(ExpectedKeys, b5_trees:keys(Tree)),

    InsertKey = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
    NewValue = b5_trees_test_helpers:random_number(),

    Tree2 = b5_trees:insert(InsertKey, NewValue, Tree),
    ExpectedKeys2 = lists:sort([InsertKey | ExpectedKeys]),
    test_keys_impl(Tree2, ExpectedKeys2, Next);
test_keys_impl(_Tree, _ExpectedKeys, []) ->
    ok.

test_values_impl(Tree, ExpectedAux, [NonExistentKey | Next]) ->
    ?assertEqual(gb_trees:values(ExpectedAux), b5_trees:values(Tree)),

    InsertKey = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
    NewValue = b5_trees_test_helpers:random_number(),

    Tree2 = b5_trees:insert(InsertKey, NewValue, Tree),
    ExpectedAux2 = gb_trees:insert(InsertKey, NewValue, ExpectedAux),
    test_values_impl(Tree2, ExpectedAux2, Next);
test_values_impl(_Tree, _ExpectedAux, []) ->
    ok.

test_smaller_impl(Tree, ExistentMap, [Key | Next]) ->
    ExpectedPair =
        case lists:filter(fun({K, _V}) -> K < Key end, maps:to_list(ExistentMap)) of
            [] ->
                none;
            ValidPairs ->
                lists:foldl(
                    fun
                        ({K1, V1}, {K2, _V2}) when K1 > K2 -> {K1, V1};
                        (_, Acc) -> Acc
                    end,
                    hd(ValidPairs),
                    tl(ValidPairs)
                )
        end,

    LookupKey = b5_trees_test_helpers:randomly_switch_key_type(Key),
    ?assertEqual(ExpectedPair, b5_trees:smaller(LookupKey, Tree)),

    test_smaller_impl(Tree, ExistentMap, Next);
test_smaller_impl(_Tree, _ExistentMap, []) ->
    ok.

test_larger_impl(Tree, ExistentMap, [Key | Next]) ->
    ExpectedPair =
        case lists:filter(fun({K, _V}) -> K > Key end, maps:to_list(ExistentMap)) of
            [] ->
                none;
            ValidPairs ->
                lists:foldl(
                    fun
                        ({K1, V1}, {K2, _V2}) when K1 < K2 -> {K1, V1};
                        (_, Acc) -> Acc
                    end,
                    hd(ValidPairs),
                    tl(ValidPairs)
                )
        end,

    LookupKey = b5_trees_test_helpers:randomly_switch_key_type(Key),
    ?assertEqual(ExpectedPair, b5_trees:larger(LookupKey, Tree)),

    test_larger_impl(Tree, ExistentMap, Next);
test_larger_impl(_Tree, _ExistentMap, []) ->
    ok.

test_take_all_smallest(Tree, [{ExpectedKey, ExpectedValue} | Next] = ExistentKvs) ->
    ?assertEqual(length(ExistentKvs), b5_trees:size(Tree)),
    ?assertNot(b5_trees:is_empty(Tree)),

    {TakenKey, TakenValue, Tree2} = b5_trees:take_smallest(Tree),
    ?assertEqual(ExpectedKey, TakenKey),
    ?assertEqual(ExpectedValue, TakenValue),

    test_take_all_smallest(Tree2, Next);
test_take_all_smallest(Tree, []) ->
    ?assertEqual(0, b5_trees:size(Tree)),
    ?assert(b5_trees:is_empty(Tree)),
    ?assertError(empty_tree, b5_trees:take_smallest(Tree)).

test_take_all_largest(Tree, [{ExpectedKey, ExpectedValue} | Next] = ExistentKvs) ->
    ?assertEqual(length(ExistentKvs), b5_trees:size(Tree)),
    ?assertNot(b5_trees:is_empty(Tree)),

    {TakenKey, TakenValue, Tree2} = b5_trees:take_largest(Tree),
    ?assertEqual(ExpectedKey, TakenKey),
    ?assertEqual(ExpectedValue, TakenValue),

    test_take_all_largest(Tree2, Next);
test_take_all_largest(Tree, []) ->
    ?assertEqual(0, b5_trees:size(Tree)),
    ?assert(b5_trees:is_empty(Tree)),
    ?assertError(empty_tree, b5_trees:take_largest(Tree)).

test_take_all(Tree, ExistentKvs, NonExistentKeys) ->
    ?assertEqual(length(ExistentKvs), b5_trees:size(Tree)),
    ?assertEqual(ExistentKvs =:= [], b5_trees:is_empty(Tree)),

    NonExistentSample = b5_trees_test_helpers:take_random(NonExistentKeys, 20),
    lists:foreach(
        fun(NonExistentKey) ->
            NonExistentKey2 = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
            ?assertError({badkey, NonExistentKey2}, b5_trees:take(NonExistentKey2, Tree))
        end,
        NonExistentSample
    ),

    case ExistentKvs of
        [{Key, Value} | Next] ->
            LookupKey = b5_trees_test_helpers:randomly_switch_key_type(Key),
            {TakenValue, Tree2} = b5_trees:take(LookupKey, Tree),
            ?assertEqual(Value, TakenValue),

            test_take_all(Tree2, Next, NonExistentKeys);
        [] ->
            ok
    end.

test_take_any_impl(Tree, ExistentKvs, NonExistentKeys) ->
    ?assertEqual(length(ExistentKvs), b5_trees:size(Tree)),
    ?assertEqual(ExistentKvs =:= [], b5_trees:is_empty(Tree)),

    NonExistentSample = b5_trees_test_helpers:take_random(NonExistentKeys, 20),
    lists:foreach(
        fun(NonExistentKey) ->
            NonExistentKey2 = b5_trees_test_helpers:randomly_switch_key_type(NonExistentKey),
            ?assertEqual(error, b5_trees:take_any(NonExistentKey2, Tree))
        end,
        NonExistentSample
    ),

    case ExistentKvs of
        [{Key, Value} | Next] ->
            LookupKey = b5_trees_test_helpers:randomly_switch_key_type(Key),
            {TakenValue, Tree2} = b5_trees:take_any(LookupKey, Tree),
            ?assertEqual(Value, TakenValue),

            test_take_any_impl(Tree2, Next, NonExistentKeys);
        [] ->
            ok
    end.

test_iterator_impl([{ExpectedK, ExpectedV} | Next], {NextK, NextV, Iter}) ->
    ?assertEqual(ExpectedK, NextK),
    ?assertEqual(ExpectedV, NextV),
    test_iterator_impl(Next, b5_trees:next(Iter));
test_iterator_impl([], none) ->
    ok.

test_enter_impl(Tree, [Key | NextKeys], ExistentKvs, NonExistentKeys) ->
    ?assertEqual(length(ExistentKvs), b5_trees:size(Tree)),
    ?assertNot(b5_trees:is_empty(Tree)),
    ?assertEqual(ExistentKvs, b5_trees:to_list(Tree)),

    EnterKey = b5_trees_test_helpers:randomly_switch_key_type(Key),
    EnterValue = b5_trees_test_helpers:random_number(),
    Tree2 = b5_trees:enter(EnterKey, EnterValue, Tree),

    ExistentKvs2 = orddict:store(EnterKey, EnterValue, ExistentKvs),
    test_enter_impl(Tree2, NextKeys, ExistentKvs2, NonExistentKeys);
test_enter_impl(_Tree, [], _ExistentKvs, _NonExistentKeys) ->
    ok.
