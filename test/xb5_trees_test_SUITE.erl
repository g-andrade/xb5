-module(xb5_trees_test_SUITE).

-ifndef(NO_CT_SUITE_BEHAVIOUR).
-behaviour(ct_suite).
-endif.

-include_lib("stdlib/include/assert.hrl").

%% CT exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

%% Test exports - Basic API
-export([
    test_construction/1,
    test_construction_repeated/1,
    test_extensive_construction_from_orddict/1,
    test_lookup/1,
    test_insert/1,
    test_insert_with/1,
    test_delete_sequential/1,
    test_delete_shuffled/1,
    test_update/1,
    test_update_with3/1,
    test_update_with4/1,
    test_enter/1,
    test_keys/1,
    test_values/1,
    test_take_sequential/1,
    test_take_shuffled/1
]).

%% Test exports - smaller and larger
-export([
    test_smallest/1,
    test_largest/1,
    test_smaller/1,
    test_larger/1,
    test_take_smallest/1,
    test_take_largest/1
]).

%% Test exports - iterators
-export([
    test_iterator/1,
    test_iterator_reversed/1,
    test_iterator_from/1,
    test_iterator_from_reversed/1
]).

%% Test exports - additional functions
-export([
    test_balance/1,
    test_foldl/1,
    test_foldr/1,
    test_map/1,
    test_rewrap/1
]).

%% Test exports - structure
-export([
    test_structure_sequentially_built/1,
    test_structure_built_from_orddict/1,
    test_structure_randomly_built/1,
    test_structure_build_seqIns2x_seqDelSmallerHalf/1,
    test_structure_build_seqIns2x_seqDelGreaterHalf/1,
    test_structure_build_seqIns2x_randomlyDelHalf/1,
    test_structure_build_randomlyIns2x_randomlyDelHalf/1,
    test_structure_build_randomlyIns2x_seqDelSmallerHalf/1,
    test_structure_build_adversarial_deletion/1
]).

-define(STRUCTURE_TEST_ITERATIONS, 1000).
-define(STRUCTURE_TEST_BASE_SIZE, 1000).

-define(Z_SCORE_95, 1.960).

-define(assertKvListsCanonEqual(ExpectedL, TestedL),
    (?assertEqual(
        lists:keymap(fun canon_key/1, 1, (ExpectedL)),
        lists:keymap(fun canon_key/1, 1, (TestedL))
    ))
).

-define(assertCanonEqual(ExpectedPair, TestedPair),
    (?assertEqual(
        canon_pair(ExpectedPair),
        canon_pair(TestedPair)
    ))
).

-define(assertPreciseStat(Id, SingleValue, CondensedStats),
    (?assertMatch(
        #{
            min := SingleValue,
            max := SingleValue
        },
        maps:get(Id, CondensedStats)
    ))
).

-define(assertConfidentStat(Id, ExpectedAvg, CondensedStats),
    (begin
        ?assertMatch(
            #{
                lower := Lower,
                higher := Higher
            } when Lower =< ExpectedAvg andalso ExpectedAvg =< Higher,
            observed_error_margin(Id, CondensedStats)
        )
    end)
).

%% ------------------------------------------------------------------
%% CT Treeup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [
        {basic_api, [parallel], [
            test_construction,
            test_construction_repeated,
            test_extensive_construction_from_orddict,
            test_lookup,
            test_insert,
            test_insert_with,
            test_delete_sequential,
            test_delete_shuffled,
            test_update,
            test_update_with3,
            test_update_with4,
            test_enter,
            test_keys,
            test_values,
            test_take_sequential,
            test_take_shuffled
        ]},
        {smaller_and_larger, [parallel], [
            test_smallest,
            test_largest,
            test_smaller,
            test_larger,
            test_take_smallest,
            test_take_largest
        ]},
        {iterators, [parallel], [
            test_iterator,
            test_iterator_reversed,
            test_iterator_from,
            test_iterator_from_reversed
        ]},
        {additional_functions, [parallel], [
            test_balance,
            test_foldl,
            test_foldr,
            test_map,
            test_rewrap
        ]},
        {structure, [parallel], [
            % Uncomment as needed, these take a long time in CI.
            %
            % test_structure_sequentially_built,
            % test_structure_built_from_orddict,
            % test_structure_randomly_built,
            % test_structure_build_seqIns2x_seqDelSmallerHalf,
            % test_structure_build_seqIns2x_seqDelGreaterHalf,
            % test_structure_build_seqIns2x_randomlyDelHalf,
            % test_structure_build_randomlyIns2x_randomlyDelHalf,
            % test_structure_build_randomlyIns2x_seqDelSmallerHalf,
            % test_structure_build_adversarial_deletion
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%% ------------------------------------------------------------------
%% Tests - Basic API
%% ------------------------------------------------------------------

test_construction(_Config) ->
    foreach_tested_size(
        fun(Size, RefKvs) ->
            Tree = xb5_trees:from_list(RefKvs),
            ?assertKvListsCanonEqual(RefKvs, xb5_trees:to_list(Tree)),
            ?assertEqual(Size, xb5_trees:size(Tree)),
            ?assertEqual(Size =:= 0, xb5_trees:is_empty(Tree)),
            ?assertEqual(Tree, new_tree_from_each_inserted(RefKvs)),

            ?assertKvListsCanonEqual(RefKvs, xb5_trees:to_list(xb5_trees:from_orddict(RefKvs))),

            _ = (Size =:= 0 andalso ?assertEqual(Tree, xb5_trees:empty()))
        end
    ).

test_construction_repeated(_Config) ->
    foreach_tested_size(fun run_construction_repeated_test/2).

test_extensive_construction_from_orddict(_Config) ->
    test_extensive_construction_from_orddict_recur(10000, []).

test_lookup(_Config) ->
    foreach_test_tree(
        fun(Size, RefKvs, Tree) ->
            foreach_existing_pair(
                fun(Key, Value) ->
                    ?assertEqual(true, xb5_trees:is_defined(Key, Tree)),
                    ?assertEqual(Value, xb5_trees:get(Key, Tree)),
                    ?assertEqual({value, Value}, xb5_trees:lookup(Key, Tree))
                end,
                RefKvs,
                Size
            ),

            %%%%%%%%%%%

            foreach_non_existent_key(
                fun(Key) ->
                    ?assertEqual(false, xb5_trees:is_defined(Key, Tree)),
                    ?assertError({badkey, Key}, xb5_trees:get(Key, Tree)),
                    ?assertEqual(none, xb5_trees:lookup(Key, Tree))
                end,
                RefKvs,
                100
            )
        end
    ).

test_insert(_Config) ->
    foreach_test_tree(
        fun(Size, RefKvs, Tree) ->
            foreach_existing_pair(
                fun(Key, _Value) ->
                    ?assertError({key_exists, Key}, xb5_trees:insert(Key, new_value, Tree))
                end,
                RefKvs,
                min(50, Size)
            ),

            %%%%%%%%%%%%%%%%%%

            foreach_non_existent_key(
                fun(Key) ->
                    Value = {new_value, make_ref()},
                    Tree2 = xb5_trees:insert(Key, Value, Tree),
                    ?assertEqual(Size + 1, xb5_trees:size(Tree2)),
                    ?assertKvListsCanonEqual(
                        add_to_sorted_list(Key, Value, RefKvs),
                        xb5_trees:to_list(Tree2)
                    )
                end,
                RefKvs,
                50
            )
        end
    ).

test_insert_with(_Config) ->
    foreach_test_tree(
        fun(Size, RefKvs, Tree) ->
            foreach_existing_pair(
                fun(Key, _Value) ->
                    Fun = fun0_error_not_to_be_called(),
                    ?assertError({key_exists, Key}, xb5_trees:insert_with(Key, Fun, Tree))
                end,
                RefKvs,
                min(50, Size)
            ),

            %%%%%%%%%%%%%%%%%%

            foreach_non_existent_key(
                fun(Key) ->
                    Value = {new_value, make_ref()},
                    Fun = fun() -> Value end,
                    Tree2 = xb5_trees:insert_with(Key, Fun, Tree),
                    ?assertEqual(Size + 1, xb5_trees:size(Tree2)),
                    ?assertKvListsCanonEqual(
                        add_to_sorted_list(Key, Value, RefKvs),
                        xb5_trees:to_list(Tree2)
                    )
                end,
                RefKvs,
                50
            )
        end
    ).

test_delete_sequential(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            DeleteKeys = lists:map(fun randomly_switch_number_type/1, list_keys(RefKvs)),

            {TreeN, []} =
                lists:foldl(
                    fun(Key, {Tree1, RemainingKvs1}) ->
                        test_delete_non_existing_keys(Tree1, RemainingKvs1, 3),

                        Tree2 = xb5_trees:delete(Key, Tree1),
                        RemainingKvs2 = remove_from_sorted_list(Key, RemainingKvs1),
                        ?assertKvListsCanonEqual(RemainingKvs2, xb5_trees:to_list(Tree2)),
                        ?assertEqual(length(RemainingKvs2), xb5_trees:size(Tree2)),
                        ?assertEqual(RemainingKvs2 =:= [], xb5_trees:is_empty(Tree2)),

                        ?assertEqual(Tree2, xb5_trees:delete_any(Key, Tree1)),

                        {Tree2, RemainingKvs2}
                    end,
                    {Tree, RefKvs},
                    DeleteKeys
                ),

            ?assertEqual([], xb5_trees:to_list(TreeN)),
            ?assertEqual(0, xb5_trees:size(TreeN)),
            ?assertEqual(true, xb5_trees:is_empty(TreeN)),

            test_delete_non_existing_keys(TreeN, [], 3)
        end
    ).

test_delete_shuffled(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            DeleteKeys = lists:map(
                fun randomly_switch_number_type/1, list_keys(list_shuffle(RefKvs))
            ),

            {TreeN, []} =
                lists:foldl(
                    fun(Key, {Tree1, RemainingKvs1}) ->
                        test_delete_non_existing_keys(Tree1, RemainingKvs1, 3),

                        Tree2 = xb5_trees:delete(Key, Tree1),
                        RemainingKvs2 = remove_from_sorted_list(Key, RemainingKvs1),
                        ?assertKvListsCanonEqual(RemainingKvs2, xb5_trees:to_list(Tree2)),
                        ?assertEqual(length(RemainingKvs2), xb5_trees:size(Tree2)),
                        ?assertEqual(RemainingKvs2 =:= [], xb5_trees:is_empty(Tree2)),

                        ?assertEqual(Tree2, xb5_trees:delete_any(Key, Tree1)),

                        {Tree2, RemainingKvs2}
                    end,
                    {Tree, RefKvs},
                    DeleteKeys
                ),

            ?assertEqual([], xb5_trees:to_list(TreeN)),
            ?assertEqual(0, xb5_trees:size(TreeN)),
            ?assertEqual(true, xb5_trees:is_empty(TreeN)),

            test_delete_non_existing_keys(TreeN, [], 3)
        end
    ).

test_update(_Config) ->
    foreach_test_tree(
        fun(Size, RefKvs, Tree) ->
            foreach_existing_pair(
                fun(Key, _Value) ->
                    NewValue = {new_value, make_ref()},
                    Tree2 = xb5_trees:update(Key, NewValue, Tree),

                    ?assertEqual(Size, xb5_trees:size(Tree2)),

                    ?assertKvListsCanonEqual(
                        update_in_sorted_list(Key, NewValue, RefKvs),
                        xb5_trees:to_list(Tree2)
                    )
                end,
                RefKvs,
                Size
            ),

            %%%%%%%%%%%%%%%%%%%

            foreach_non_existent_key(
                fun(Key) ->
                    NewValue = {new_value, make_ref()},
                    ?assertError({badkey, Key}, xb5_trees:update(Key, NewValue, Tree))
                end,
                RefKvs,
                50
            )
        end
    ).

test_update_with3(_Config) ->
    foreach_test_tree(
        fun(Size, RefKvs, Tree) ->
            foreach_existing_pair(
                fun(Key, Value) ->
                    NewValue = make_ref(),

                    Fun =
                        fun(PrevValue) ->
                            ?assertEqual(Value, PrevValue),
                            NewValue
                        end,

                    Tree2 = xb5_trees:update_with(Key, Fun, Tree),

                    ?assertEqual(Size, xb5_trees:size(Tree2)),

                    ?assertKvListsCanonEqual(
                        update_in_sorted_list(Key, NewValue, RefKvs),
                        xb5_trees:to_list(Tree2)
                    )
                end,
                RefKvs,
                Size
            ),

            %%%%%%%%%%%%%%%%%%%%%%%%%

            foreach_non_existent_key(
                fun(Key) ->
                    Fun = fun1_error_not_to_be_called(),
                    ?assertError({badkey, Key}, xb5_trees:update_with(Key, Fun, Tree))
                end,
                RefKvs,
                50
            )
        end
    ).

test_update_with4(_Config) ->
    foreach_test_tree(
        fun(Size, RefKvs, Tree) ->
            foreach_existing_pair(
                fun(Key, Value) ->
                    NewValue = make_ref(),

                    Fun =
                        fun(PrevValue) ->
                            ?assertEqual(Value, PrevValue),
                            NewValue
                        end,

                    InitValue = make_ref(),

                    Tree2 = xb5_trees:update_with(Key, Fun, InitValue, Tree),

                    ?assertEqual(Size, xb5_trees:size(Tree2)),

                    ?assertKvListsCanonEqual(
                        update_in_sorted_list(Key, NewValue, RefKvs),
                        xb5_trees:to_list(Tree2)
                    )
                end,
                RefKvs,
                Size
            ),

            %%%%%%%%%%%%%%%%%%%%%%%%%

            foreach_non_existent_key(
                fun(Key) ->
                    Fun = fun1_error_not_to_be_called(),
                    InitValue = make_ref(),

                    Tree2 = xb5_trees:update_with(Key, Fun, InitValue, Tree),

                    ?assertEqual(Size + 1, xb5_trees:size(Tree2)),

                    ?assertKvListsCanonEqual(
                        add_to_sorted_list(Key, InitValue, RefKvs),
                        xb5_trees:to_list(Tree2)
                    )
                end,
                RefKvs,
                50
            )
        end
    ).

test_enter(_Config) ->
    foreach_test_tree(
        fun(Size, RefKvs, Tree) ->
            foreach_existing_pair(
                fun(Key, _Value) ->
                    NewValue = {new_value, make_ref()},
                    Tree2 = xb5_trees:enter(Key, NewValue, Tree),

                    ?assertEqual(Size, xb5_trees:size(Tree2)),

                    ?assertKvListsCanonEqual(
                        update_in_sorted_list(Key, NewValue, RefKvs),
                        xb5_trees:to_list(Tree2)
                    )
                end,
                RefKvs,
                min(50, Size)
            ),

            %%%%%%%%%%%%%%%%%%

            foreach_non_existent_key(
                fun(Key) ->
                    Value = {new_value, make_ref()},
                    Tree2 = xb5_trees:enter(Key, Value, Tree),
                    ?assertEqual(Size + 1, xb5_trees:size(Tree2)),
                    ?assertKvListsCanonEqual(
                        add_to_sorted_list(Key, Value, RefKvs),
                        xb5_trees:to_list(Tree2)
                    )
                end,
                RefKvs,
                50
            )
        end
    ).

test_keys(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            ?assertEqual(
                lists:map(fun canon_key/1, list_keys(RefKvs)),
                lists:map(fun canon_key/1, xb5_trees:keys(Tree))
            )
        end
    ).

test_values(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            ?assertEqual(
                list_values(RefKvs),
                xb5_trees:values(Tree)
            )
        end
    ).

test_take_sequential(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            TakePairs = lists:keymap(fun randomly_switch_number_type/1, 1, RefKvs),

            {TreeN, []} =
                lists:foldl(
                    fun({Key, Value}, {Tree1, RemainingKvs1}) ->
                        test_take_non_existing_keys(Tree1, RemainingKvs1, 3),

                        {TakenValue, Tree2} = xb5_trees:take(Key, Tree1),
                        ?assertEqual(Value, TakenValue),

                        RemainingKvs2 = remove_from_sorted_list(Key, RemainingKvs1),
                        ?assertKvListsCanonEqual(RemainingKvs2, xb5_trees:to_list(Tree2)),
                        ?assertEqual(length(RemainingKvs2), xb5_trees:size(Tree2)),
                        ?assertEqual(RemainingKvs2 =:= [], xb5_trees:is_empty(Tree2)),

                        ?assertEqual({TakenValue, Tree2}, xb5_trees:take_any(Key, Tree1)),

                        {Tree2, RemainingKvs2}
                    end,
                    {Tree, RefKvs},
                    TakePairs
                ),

            ?assertEqual([], xb5_trees:to_list(TreeN)),
            ?assertEqual(0, xb5_trees:size(TreeN)),
            ?assertEqual(true, xb5_trees:is_empty(TreeN)),

            test_take_non_existing_keys(TreeN, [], 3)
        end
    ).

test_take_shuffled(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            TakePairs = list_shuffle(lists:keymap(fun randomly_switch_number_type/1, 1, RefKvs)),

            {TreeN, []} =
                lists:foldl(
                    fun({Key, Value}, {Tree1, RemainingKvs1}) ->
                        test_take_non_existing_keys(Tree1, RemainingKvs1, 3),

                        {TakenValue, Tree2} = xb5_trees:take(Key, Tree1),
                        ?assertEqual(Value, TakenValue),

                        RemainingKvs2 = remove_from_sorted_list(Key, RemainingKvs1),
                        ?assertKvListsCanonEqual(RemainingKvs2, xb5_trees:to_list(Tree2)),
                        ?assertEqual(length(RemainingKvs2), xb5_trees:size(Tree2)),
                        ?assertEqual(RemainingKvs2 =:= [], xb5_trees:is_empty(Tree2)),

                        ?assertEqual({TakenValue, Tree2}, xb5_trees:take_any(Key, Tree1)),

                        {Tree2, RemainingKvs2}
                    end,
                    {Tree, RefKvs},
                    TakePairs
                ),

            ?assertEqual([], xb5_trees:to_list(TreeN)),
            ?assertEqual(0, xb5_trees:size(TreeN)),
            ?assertEqual(true, xb5_trees:is_empty(TreeN)),

            test_take_non_existing_keys(TreeN, [], 3)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Smaller and Larger
%% ------------------------------------------------------------------

test_smallest(_Config) ->
    foreach_test_tree(
        fun
            (0, _RefKvs, Tree) ->
                ?assertError(empty_tree, xb5_trees:smallest(Tree));
            %
            (_Size, RefKvs, Tree) ->
                ?assertCanonEqual(hd(RefKvs), xb5_trees:smallest(Tree))
        end
    ).

test_largest(_Config) ->
    foreach_test_tree(
        fun
            (0, _RefKvs, Tree) ->
                ?assertError(empty_tree, xb5_trees:largest(Tree));
            %
            (_Size, RefKvs, Tree) ->
                ?assertCanonEqual(lists:last(RefKvs), xb5_trees:largest(Tree))
        end
    ).

test_smaller(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            run_smaller(RefKvs, Tree)
        end
    ).

test_larger(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            run_larger(RefKvs, Tree)
        end
    ).

test_take_smallest(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            run_take_smallest(RefKvs, Tree)
        end
    ).

test_take_largest(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            run_take_largest(lists:reverse(RefKvs), Tree)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Iterators
%% ------------------------------------------------------------------

test_iterator(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            Iter = new_iterator(Tree),
            ?assertKvListsCanonEqual(RefKvs, iterate(Iter))
        end
    ).

test_iterator_reversed(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            Iter = xb5_trees:iterator(Tree, reversed),
            ?assertKvListsCanonEqual(lists:reverse(RefKvs), iterate(Iter))
        end
    ).

test_iterator_from(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            run_iterator_from(RefKvs, Tree)
        end
    ).

test_iterator_from_reversed(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            run_iterator_from_reversed(RefKvs, Tree)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Additional Functions
%% ------------------------------------------------------------------

test_balance(_Config) ->
    foreach_test_tree(
        fun(_Size, _RefKvs, Tree) ->
            % Balancing does nothing
            ?assertEqual(Tree, xb5_trees:balance(Tree))
        end
    ).

test_foldl(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            run_foldl(RefKvs, Tree)
        end
    ).

test_foldr(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            run_foldr(RefKvs, Tree)
        end
    ).

test_map(_Config) ->
    foreach_test_tree(
        fun(_Size, RefKvs, Tree) ->
            run_map(RefKvs, Tree)
        end
    ).

test_rewrap(_Config) ->
    ?assertMatch({error, _}, xb5_trees:unwrap(xb5_sets:new())),
    ?assertMatch({error, _}, xb5_trees:unwrap(xb5_bag:new())),
    ?assertMatch({error, _}, xb5_trees:unwrap({xb5_tree, -1, xb5_trees_node:new()})),
    ?assertMatch({error, _}, xb5_trees:unwrap({xb5_tree, 2, xb5_trees_node:new()})),
    ?assertMatch({error, _}, xb5_trees:unwrap({xb5_tree, 2, make_ref()})),

    foreach_test_tree(
        fun(_Size, _RefElements, Col) ->
            {ok, Unwrapped} = xb5_trees:unwrap(Col),
            ?assertEqual(Col, xb5_trees:wrap(Unwrapped))
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Structure
%% ------------------------------------------------------------------

test_structure_sequentially_built(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefKvs) ->
                ?assertEqual(RefKvs, lists:keysort(1, RefKvs)),

                case rand:uniform(4) of
                    1 ->
                        new_tree_from_each_inserted(RefKvs);
                    2 ->
                        xb5_trees:from_list(RefKvs);
                    3 ->
                        new_tree_from_each_inserted(lists:reverse(RefKvs));
                    4 ->
                        xb5_trees:from_list(lists:reverse(RefKvs))
                end
            end
        ),

    %%%%%%%%%%

    ?assertPreciseStat(height, 5.0, CondensedStats),

    ?assertPreciseStat(avg_keys_per_node, 2.9940119760479043, CondensedStats).

%%%%

test_structure_built_from_orddict(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefKvs) ->
                ?assertEqual(RefKvs, lists:keysort(1, RefKvs)),
                xb5_trees:from_orddict(RefKvs)
            end
        ),

    %%%%%%%%%%

    ?assertPreciseStat(height, 5.0, CondensedStats),

    ?assertPreciseStat(avg_keys_per_node, 2.9940119760479043, CondensedStats).

%%%%

test_structure_randomly_built(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefKvs) ->
                new_tree_from_each_inserted(list_shuffle(RefKvs))
            end
        ),

    %%%%%%%%%%

    ?assertConfidentStat(height, 5.0, CondensedStats),

    ?assertConfidentStat(avg_keys_per_node, 2.9143881267511302, CondensedStats).

%%%%

test_structure_build_seqIns2x_seqDelSmallerHalf(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefKvs) ->
                % 1) build sequentially
                Tree1 = new_tree_from_each_inserted(RefKvs),

                % 2) delete smaller half sequentially
                AmountToDelete = xb5_trees:size(Tree1) div 2,
                KeysToDelete = list_keys(lists:sublist(RefKvs, AmountToDelete)),
                lists:foldl(fun xb5_trees:delete/2, Tree1, KeysToDelete)
            end,
            [{size_multiplier, 2}]
        ),

    %%%%%%%%%%

    ?assertPreciseStat(height, 5.0, CondensedStats),

    ?assertPreciseStat(avg_keys_per_node, 2.9940119760479043, CondensedStats).

%%%%

test_structure_build_seqIns2x_seqDelGreaterHalf(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefKvs) ->
                % 1) build sequentially
                Tree1 = new_tree_from_each_inserted(RefKvs),

                % 2) delete greater half sequentially
                AmountToDelete = xb5_trees:size(Tree1) div 2,
                KeysToDelete = list_keys(lists:sublist(lists:reverse(RefKvs), AmountToDelete)),
                lists:foldl(fun xb5_trees:delete/2, Tree1, KeysToDelete)
            end,
            [{size_multiplier, 2}]
        ),

    %%%%%%%%%%

    ?assertPreciseStat(height, 5.0, CondensedStats),

    ?assertPreciseStat(avg_keys_per_node, 2.9940119760479043, CondensedStats).

%%%%

test_structure_build_seqIns2x_randomlyDelHalf(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefKvs) ->
                % 1) build sequentially
                Tree1 = new_tree_from_each_inserted(RefKvs),

                % 2) delete half randomly
                AmountToDelete = xb5_trees:size(Tree1) div 2,
                KeysToDelete = list_keys(lists:sublist(list_shuffle(RefKvs), AmountToDelete)),
                lists:foldl(fun xb5_trees:delete/2, Tree1, KeysToDelete)
            end,
            [{size_multiplier, 2}]
        ),

    %%%%%%%%%%

    ?assertConfidentStat(height, 5.826, CondensedStats),

    ?assertConfidentStat(avg_keys_per_node, 2.5855965970699875, CondensedStats).

%%%%

test_structure_build_randomlyIns2x_randomlyDelHalf(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefKvs) ->
                % 1) build randomly
                Tree1 = new_tree_from_each_inserted(list_shuffle(RefKvs)),

                % 2) delete half randomly
                AmountToDelete = xb5_trees:size(Tree1) div 2,
                KeysToDelete = list_keys(lists:sublist(list_shuffle(RefKvs), AmountToDelete)),
                lists:foldl(fun xb5_trees:delete/2, Tree1, KeysToDelete)
            end,
            [{size_multiplier, 2}]
        ),

    %%%%%%%%%%

    ?assertConfidentStat(height, 5.82, CondensedStats),

    ?assertConfidentStat(avg_keys_per_node, 2.59362583949796, CondensedStats).

%%%%

test_structure_build_randomlyIns2x_seqDelSmallerHalf(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefKvs) ->
                % 1) build randomly
                Tree1 = new_tree_from_each_inserted(list_shuffle(RefKvs)),

                % 2) delete smaller half sequentially
                AmountToDelete = xb5_trees:size(Tree1) div 2,
                KeysToDelete = list_keys(lists:sublist(RefKvs, AmountToDelete)),
                lists:foldl(fun xb5_trees:delete/2, Tree1, KeysToDelete)
            end,
            [{size_multiplier, 2}]
        ),

    %%%%%%%%%%

    ?assertConfidentStat(height, 5.01, CondensedStats),

    ?assertConfidentStat(avg_keys_per_node, 2.9089119962162733, CondensedStats).

%%%%

test_structure_build_adversarial_deletion(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefKvs) ->
                % 1) build sequentially
                Tree1 = new_tree_from_each_inserted(RefKvs),

                % 2) delete every 5th item sequentially
                KeysToDelete =
                    lists:filtermap(
                        fun({Index, {K, _}}) ->
                            ((Index rem 5 =:= 0) andalso
                                {true, K})
                        end,
                        lists:enumerate(RefKvs)
                    ),

                lists:foldl(fun xb5_trees:delete/2, Tree1, KeysToDelete)
            end,
            [{size_multiplier, 1.25}]
        ),

    %%%%%%%%%%

    ?assertPreciseStat(height, 5.00, CondensedStats),

    % NOTE: if we delete every _4th_ item, it's actually worse for `avg_keys_per_internal_node`.

    ?assertConfidentStat(avg_keys_per_node, 2.3980815347722086, CondensedStats).

%% ------------------------------------------------------------------
%% Helper Functions: shared
%% ------------------------------------------------------------------

foreach_test_tree(Fun) ->
    foreach_test_tree(Fun, []).

foreach_test_tree(Fun, Opts) ->
    foreach_tested_size(
        fun(Size, RefKvs) ->
            Tree = xb5_trees:from_list(maybe_shuffle_list_for_new_tree(RefKvs)),
            ?assertEqual(Size, xb5_trees:size(Tree)),

            Stats = xb5_trees:structural_stats(Tree),
            ?assertEqual(Size, proplists:get_value(total_keys, Stats)),

            Fun(Size, RefKvs, Tree)
        end,
        Opts
    ).

maybe_shuffle_list_for_new_tree(RefKvs) ->
    % Sequential insertion takes very different paths from random insertion,
    % the occasional shuffle means we get good coverage of both.

    case rand:uniform() < 1 / 3 of
        true ->
            list_shuffle(RefKvs);
        %
        false ->
            RefKvs
    end.

foreach_tested_size(Fun) ->
    foreach_tested_size(Fun, []).

foreach_tested_size(Fun, Opts) ->
    NumericOnly = proplists:get_value(numeric_only, Opts, false),

    xb5_test_utils:foreach_tested_size(
        fun(Size) ->
            run_test_for_size(Size, NumericOnly, Fun)
        end
    ).

run_test_for_size(Size, NumericOnly, Fun) ->
    RefKvs = new_ref_kvs(Size, NumericOnly),
    Fun(Size, RefKvs).

new_ref_kvs(Size, NumericOnly) ->
    new_ref_kvs_recur(Size, NumericOnly, _Acc = []).

new_ref_kvs_recur(Size, NumericOnly, Acc) when Size > 0 ->
    NewKey = new_key(NumericOnly),

    case lists:keymember(NewKey, 1, Acc) of
        false ->
            Value = {initial_value_for, NewKey},
            UpdatedAcc = [{NewKey, Value} | Acc],
            new_ref_kvs_recur(Size - 1, NumericOnly, UpdatedAcc);
        %
        true ->
            new_ref_kvs_recur(Size, NumericOnly, Acc)
    end;
new_ref_kvs_recur(0, _, Acc) ->
    lists:keysort(1, Acc).

randomly_switch_number_type(Key) ->
    case rand:uniform(3) of
        1 when is_integer(Key) ->
            float(Key);
        %
        1 when is_float(Key) ->
            trunc(Key);
        %
        _ ->
            Key
    end.

canon_pair({Key, Value}) ->
    {canon_key(Key), Value}.

canon_key(Key) when is_float(Key) ->
    case math:fmod(Key, 1.0) == 0 of
        true ->
            trunc(Key);
        %
        false ->
            Key
    end;
canon_key(Integer) when is_integer(Integer) ->
    Integer;
canon_key(List) when is_list(List) ->
    canon_list(List);
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

canon_list([H | T]) ->
    [canon_key(H) | canon_list(T)];
canon_list([]) ->
    [];
canon_list(ImproperTail) ->
    canon_key(ImproperTail).

new_tree_from_each_inserted(List) ->
    Tree = xb5_trees:new(),

    ?assertEqual(0, xb5_trees:size(Tree)),
    ?assertEqual(true, xb5_trees:is_empty(Tree)),

    new_tree_from_each_inserted_recur(List, Tree).

new_tree_from_each_inserted_recur([{Key, Value} | Next], Tree) ->
    UpdatedTree = xb5_trees:insert(Key, Value, Tree),
    new_tree_from_each_inserted_recur(Next, UpdatedTree);
new_tree_from_each_inserted_recur([], Tree) ->
    Tree.

%%%%%%%%%%%%%%%

foreach_existing_pair(Fun, RefKvs, Amount) ->
    Chosen = lists:sublist(list_shuffle(RefKvs), Amount),

    lists:foreach(
        fun({Key, Value}) ->
            Fun(randomly_switch_number_type(Key), Value)
        end,
        Chosen
    ).

foreach_non_existent_key(Fun, RefKvs, Amount) when Amount > 0 ->
    Key = new_key(),

    case lists:any(fun({K, _}) -> K == Key end, RefKvs) of
        false ->
            Fun(Key),
            foreach_non_existent_key(Fun, RefKvs, Amount - 1);
        %
        true ->
            foreach_non_existent_key(Fun, RefKvs, Amount)
    end;
foreach_non_existent_key(_, _, 0) ->
    ok.

list_keys(List) ->
    lists:map(fun({K, _}) -> K end, List).

list_values(List) ->
    lists:map(fun({_, V}) -> V end, List).

list_shuffle(List) ->
    WithWeights = lists:map(fun(V) -> [rand:uniform() | V] end, List),
    Shuffled = lists:sort(WithWeights),
    lists:map(fun([_ | V]) -> V end, Shuffled).

add_to_sorted_list(Key, Value, [{HK, _} = H | T]) ->
    case Key > HK of
        true ->
            [H | add_to_sorted_list(Key, Value, T)];
        %
        false ->
            [{Key, Value}, H | T]
    end;
add_to_sorted_list(Key, Value, []) ->
    [{Key, Value}].

remove_from_sorted_list(Key, [{HK, _} = H | T]) ->
    if
        Key > HK ->
            [H | remove_from_sorted_list(Key, T)];
        %
        Key == HK ->
            T
    end.

update_in_sorted_list(Key, Value, [{HK, _} = H | T]) ->
    if
        Key > HK ->
            [H | update_in_sorted_list(Key, Value, T)];
        %
        Key == HK ->
            [{Key, Value} | T]
    end.

sort_kv_list_keep_last_repeated(List) ->
    Aux = gb_trees:empty(),
    sort_kv_list_keep_last_repeated_recur(List, Aux).

sort_kv_list_keep_last_repeated_recur([{K, V} | Next], Aux) ->
    UpdatedAux = gb_trees:enter(K, V, Aux),
    sort_kv_list_keep_last_repeated_recur(Next, UpdatedAux);
sort_kv_list_keep_last_repeated_recur([], Aux) ->
    gb_trees:to_list(Aux).

-dialyzer({nowarn_function, fun0_error_not_to_be_called/0}).
fun0_error_not_to_be_called() ->
    fun() ->
        error(not_to_be_called)
    end.

-dialyzer({nowarn_function, fun1_error_not_to_be_called/0}).
fun1_error_not_to_be_called() ->
    fun(_) ->
        error(not_to_be_called)
    end.

%% ------------------------------------------------------------------
%% Helper Functions: construction repeated
%% ------------------------------------------------------------------

run_construction_repeated_test(Size, RefKvs) ->
    Amount = min(length(RefKvs), 50),

    KeysToRepeat = lists:sublist(list_shuffle(list_keys(RefKvs)), Amount),

    run_construction_repeated_test(Size, KeysToRepeat, RefKvs).

run_construction_repeated_test(Size, [KeyToRepeat | Next], RefKvs) ->
    List = add_to_sorted_list(randomly_switch_number_type(KeyToRepeat), repeated, RefKvs),

    Tree = xb5_trees:from_list(List),

    ?assertEqual(Size, xb5_trees:size(Tree)),

    ?assertKvListsCanonEqual(
        sort_kv_list_keep_last_repeated(List),
        xb5_trees:to_list(Tree)
    ),

    %%%

    ShuffledList = list_shuffle(List),
    TreeShuffled = xb5_trees:from_list(ShuffledList),

    ?assertEqual(Size, xb5_trees:size(TreeShuffled)),

    ?assertKvListsCanonEqual(
        sort_kv_list_keep_last_repeated(ShuffledList),
        xb5_trees:to_list(TreeShuffled)
    ),

    run_construction_repeated_test(Size, Next, RefKvs);
run_construction_repeated_test(_, [], _) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers: extensive construction from orddict
%% ------------------------------------------------------------------

test_extensive_construction_from_orddict_recur(FirstKey, Acc) when FirstKey > 0 ->
    List = [{FirstKey, make_ref()} | Acc],
    Tree = xb5_trees:from_orddict(List),

    ?assertKvListsCanonEqual(List, xb5_trees:to_list(Tree)),

    _ = xb5_trees:structural_stats(Tree),

    test_extensive_construction_from_orddict_recur(FirstKey - 1, List);
test_extensive_construction_from_orddict_recur(0, _) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers: deletion
%% ------------------------------------------------------------------

test_delete_non_existing_keys(Tree, RemainingKeys, Amount) when Amount > 0 ->
    Key = new_key(),

    case lists:any(fun({K, _}) -> K == Key end, RemainingKeys) of
        false ->
            ?assertError({badkey, Key}, xb5_trees:delete(Key, Tree)),
            ?assertEqual(Tree, xb5_trees:delete_any(Key, Tree)),

            test_delete_non_existing_keys(Tree, RemainingKeys, Amount - 1);
        %
        true ->
            test_delete_non_existing_keys(Tree, RemainingKeys, Amount)
    end;
test_delete_non_existing_keys(_, _, 0) ->
    ok.

test_take_non_existing_keys(Tree, RemainingKeys, Amount) when Amount > 0 ->
    Key = new_key(),

    case lists:any(fun({K, _}) -> K == Key end, RemainingKeys) of
        false ->
            ?assertError({badkey, Key}, xb5_trees:take(Key, Tree)),
            ?assertEqual(error, xb5_trees:take_any(Key, Tree)),

            test_take_non_existing_keys(Tree, RemainingKeys, Amount - 1);
        %
        true ->
            test_take_non_existing_keys(Tree, RemainingKeys, Amount)
    end;
test_take_non_existing_keys(_, _, 0) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers: smaller and larger
%% ------------------------------------------------------------------

run_smaller(RefKvs, Tree) ->
    case RefKvs of
        [] ->
            Key = new_key(),
            ?assertEqual(none, xb5_trees:smaller(Key, Tree));
        %
        [{SingleKey, _} = SinglePair] ->
            ?assertEqual(none, xb5_trees:smaller(randomly_switch_number_type(SingleKey), Tree)),

            LargerKey = key_larger(SingleKey),
            ?assertEqual(SinglePair, xb5_trees:smaller(LargerKey, Tree)),

            SmallerKey = key_smaller(SingleKey),
            ?assertEqual(none, xb5_trees:smaller(SmallerKey, Tree));
        %
        [{FirstKey, FirstValue} | Next] ->
            ?assertEqual(none, xb5_trees:smaller(randomly_switch_number_type(FirstKey), Tree)),

            SmallerKey = key_smaller(FirstKey),
            ?assertEqual(none, xb5_trees:smaller(SmallerKey, Tree)),

            run_smaller_recur(FirstKey, FirstValue, Next, Tree)
    end.

run_smaller_recur(ExpectedKey, ExpectedValue, [{LastKey, LastValue}], Tree) ->
    ?assertCanonEqual(
        {ExpectedKey, ExpectedValue},
        xb5_trees:smaller(randomly_switch_number_type(LastKey), Tree)
    ),

    LargerKey = key_larger(LastKey),
    ?assert(LargerKey > LastKey),
    ?assert(xb5_trees:smaller(LargerKey, Tree) == {LastKey, LastValue});
run_smaller_recur(ExpectedKey, ExpectedValue, [{Key, Value} | Next], Tree) ->
    ?assert(
        xb5_trees:smaller(randomly_switch_number_type(Key), Tree) == {ExpectedKey, ExpectedValue}
    ),

    case key_in_between(ExpectedKey, Key) of
        {found, InBetween} ->
            ?assert(InBetween > ExpectedKey),
            ?assert(InBetween < Key),
            ?assertCanonEqual({ExpectedKey, ExpectedValue}, xb5_trees:smaller(InBetween, Tree));
        %
        none ->
            ok
    end,

    run_smaller_recur(Key, Value, Next, Tree).

%%%%%%%%%%%%%%%%%

run_larger(RefKvs, Tree) ->
    case lists:reverse(RefKvs) of
        [] ->
            Key = new_key(),
            ?assertEqual(none, xb5_trees:larger(Key, Tree));
        %
        [{SingleKey, SingleValue}] ->
            ?assertEqual(none, xb5_trees:larger(SingleKey, Tree)),

            LargerKey = key_larger(SingleKey),
            ?assertEqual(none, xb5_trees:larger(LargerKey, Tree)),

            SmallerKey = key_smaller(SingleKey),
            ?assertEqual({SingleKey, SingleValue}, xb5_trees:larger(SmallerKey, Tree));
        %
        [{LastKey, LastValue} | Next] ->
            ?assertEqual(none, xb5_trees:larger(randomly_switch_number_type(LastKey), Tree)),

            LargerKey = key_larger(LastKey),
            ?assertEqual(none, xb5_trees:larger(LargerKey, Tree)),

            run_larger_recur(LastKey, LastValue, Next, Tree)
    end.

run_larger_recur(ExpectedKey, ExpectedValue, [{FirstKey, FirstValue}], Tree) ->
    ?assertCanonEqual(
        {ExpectedKey, ExpectedValue},
        xb5_trees:larger(randomly_switch_number_type(FirstKey), Tree)
    ),

    SmallerKey = key_smaller(FirstKey),
    ?assert(SmallerKey < FirstKey),
    ?assertEqual({FirstKey, FirstValue}, xb5_trees:larger(SmallerKey, Tree));
run_larger_recur(ExpectedKey, ExpectedValue, [{Key, Value} | Next], Tree) ->
    ?assertEqual(
        {ExpectedKey, ExpectedValue}, xb5_trees:larger(randomly_switch_number_type(Key), Tree)
    ),

    case key_in_between(Key, ExpectedKey) of
        {found, InBetween} ->
            ?assert(InBetween < ExpectedKey),
            ?assert(InBetween > Key),
            ?assertCanonEqual({ExpectedKey, ExpectedValue}, xb5_trees:larger(InBetween, Tree));
        %
        none ->
            ok
    end,

    run_larger_recur(Key, Value, Next, Tree).

%%%%%%%%%%%%%%%%%

run_take_smallest([{ExpectedKey, ExpectedValue} | Next], Tree) ->
    {TakenKey, TakenValue, Tree2} = xb5_trees:take_smallest(Tree),
    ?assertEqual({ExpectedKey, ExpectedValue}, {TakenKey, TakenValue}),
    ?assertEqual(length(Next), xb5_trees:size(Tree2)),
    run_take_smallest(Next, Tree2);
run_take_smallest([], Tree) ->
    ?assertError(empty_tree, xb5_trees:take_smallest(Tree)).

run_take_largest([{ExpectedKey, ExpectedValue} | Next], Tree) ->
    {TakenKey, TakenValue, Tree2} = xb5_trees:take_largest(Tree),
    ?assertEqual({ExpectedKey, ExpectedValue}, {TakenKey, TakenValue}),
    ?assertEqual(length(Next), xb5_trees:size(Tree2)),
    run_take_largest(Next, Tree2);
run_take_largest([], Tree) ->
    ?assertError(empty_tree, xb5_trees:take_largest(Tree)).

%% ------------------------------------------------------------------
%% Helpers: iterators
%% ------------------------------------------------------------------

run_iterator_from(RefKvs, Tree) ->
    case RefKvs of
        [] ->
            Iter = new_iterator_from(new_key(), Tree),
            ?assertEqual([], iterate(Iter));
        %
        [{SingleKey, _}] ->
            Iter = new_iterator_from(randomly_switch_number_type(SingleKey), Tree),
            ?assertKvListsCanonEqual(RefKvs, iterate(Iter)),

            SmallerKey = key_smaller(SingleKey),
            Iter2 = new_iterator_from(SmallerKey, Tree),
            ?assertKvListsCanonEqual(RefKvs, iterate(Iter2)),

            LargerKey = key_larger(SingleKey),
            Iter3 = new_iterator_from(LargerKey, Tree),
            ?assertEqual([], iterate(Iter3));
        %
        [{FirstKey, _} | _] ->
            SmallerKey = key_smaller(FirstKey),
            Iter = new_iterator_from(SmallerKey, Tree),
            ?assertKvListsCanonEqual(RefKvs, iterate(Iter)),

            run_iterator_from_recur(RefKvs, Tree)
    end.

run_iterator_from_recur([{LastKey, LastValue}], Tree) ->
    run_iterator_from_last_element(LastKey, LastValue, Tree);
run_iterator_from_recur([{Key1, _} | [{Key2, _} | _] = Next] = List, Tree) ->
    Iter = new_iterator_from(Key1, Tree),
    ?assertKvListsCanonEqual(List, iterate(Iter)),

    case key_in_between(Key1, Key2) of
        {found, InBetween} ->
            ?assert(InBetween > Key1),
            ?assert(InBetween < Key2),
            Iter2 = new_iterator_from(InBetween, Tree),
            ?assertKvListsCanonEqual(Next, iterate(Iter2));
        %
        none ->
            ok
    end,

    run_iterator_from_recur(Next, Tree).

run_iterator_from_last_element(LastKey, LastValue, Tree) ->
    Iter = new_iterator_from(randomly_switch_number_type(LastKey), Tree),
    ?assertKvListsCanonEqual([{LastKey, LastValue}], iterate(Iter)),

    LargerKey = key_larger(LastKey),
    Iter2 = new_iterator_from(LargerKey, Tree),
    ?assertEqual([], iterate(Iter2)).

new_iterator_from(Key, Tree) ->
    Iter = xb5_trees:iterator_from(Key, Tree),
    ?assertEqual(Iter, xb5_trees:iterator_from(Key, Tree, ordered)),
    Iter.

%%%%%%%%%%%%%%%%%

run_iterator_from_reversed(RefKvs, Tree) ->
    case lists:reverse(RefKvs) of
        [] ->
            Iter = xb5_trees:iterator_from(new_key(), Tree, reversed),
            ?assertEqual([], iterate(Iter));
        %
        [{SingleKey, _}] ->
            Iter = xb5_trees:iterator_from(
                randomly_switch_number_type(SingleKey), Tree, reversed
            ),
            ?assertKvListsCanonEqual(RefKvs, iterate(Iter)),

            SmallerKey = key_smaller(SingleKey),
            Iter2 = xb5_trees:iterator_from(SmallerKey, Tree, reversed),
            ?assertEqual([], iterate(Iter2)),

            LargerKey = key_larger(SingleKey),
            Iter3 = xb5_trees:iterator_from(LargerKey, Tree, reversed),
            ?assertKvListsCanonEqual(RefKvs, iterate(Iter3));
        %
        [{LastKey, _} | _] = ReverseRefKvs ->
            LargerKey = key_larger(LastKey),
            Iter = xb5_trees:iterator_from(LargerKey, Tree, reversed),
            ?assertKvListsCanonEqual(ReverseRefKvs, iterate(Iter)),

            run_iterator_from_reversed_recur(ReverseRefKvs, Tree)
    end.

run_iterator_from_reversed_recur([{FirstKey, FirstValue}], Tree) ->
    run_iterator_from_reversed_first_key(FirstKey, FirstValue, Tree);
run_iterator_from_reversed_recur([{Key2, _} | [{Key1, _} | _] = Tail] = List, Tree) ->
    Iter = xb5_trees:iterator_from(Key2, Tree, reversed),
    ?assertKvListsCanonEqual(List, iterate(Iter)),

    case key_in_between(Key1, Key2) of
        {found, InBetween} ->
            ?assert(InBetween > Key1),
            ?assert(InBetween < Key2),
            Iter2 = xb5_trees:iterator_from(InBetween, Tree, reversed),
            ?assertKvListsCanonEqual(Tail, iterate(Iter2));
        %
        none ->
            ok
    end,

    run_iterator_from_reversed_recur(Tail, Tree).

run_iterator_from_reversed_first_key(FirstKey, FirstValue, Tree) ->
    Iter = xb5_trees:iterator_from(randomly_switch_number_type(FirstKey), Tree, reversed),
    ?assertKvListsCanonEqual([{FirstKey, FirstValue}], iterate(Iter)),

    SmallerKey = key_smaller(FirstKey),
    Iter2 = xb5_trees:iterator_from(SmallerKey, Tree, reversed),
    ?assertEqual([], iterate(Iter2)).

%%%%%%%%%%%%%%%%%

new_iterator(Tree) ->
    Iter = xb5_trees:iterator(Tree),
    ?assertEqual(Iter, xb5_trees:iterator(Tree, ordered)),
    Iter.

iterate(Iter) ->
    case xb5_trees:next(Iter) of
        {Key, Value, Iter2} ->
            [{Key, Value} | iterate(Iter2)];
        %
        none ->
            []
    end.

%% ------------------------------------------------------------------
%% Helper Functions: foldl
%% ------------------------------------------------------------------

run_foldl(RefKvs, Tree) ->
    Tag = make_ref(),

    Fun =
        fun(K, V, Acc) ->
            case Acc of
                [{_, PrevK, _} | _] ->
                    ?assert(PrevK < K);
                [] ->
                    ok
            end,

            [{Tag, K, V} | Acc]
        end,

    ?assertKvListsCanonEqual(
        orddict:fold(Fun, [], orddict:from_list(RefKvs)),
        xb5_trees:foldl(Fun, [], Tree)
    ).

%% ------------------------------------------------------------------
%% Helper Functions: foldr
%% ------------------------------------------------------------------

run_foldr(RefKvs, Tree) ->
    Tag = make_ref(),

    Fun =
        fun(K, V, Acc) ->
            case Acc of
                [{_, PrevK, _} | _] ->
                    ?assert(PrevK > K);
                [] ->
                    ok
            end,

            [{Tag, K, V} | Acc]
        end,

    ?assertKvListsCanonEqual(
        lists:foldr(
            fun({K, V}, Acc) ->
                Fun(K, V, Acc)
            end,
            [],
            RefKvs
        ),
        %
        xb5_trees:foldr(Fun, [], Tree)
    ).

%% ------------------------------------------------------------------
%% Helper Functions: map
%% ------------------------------------------------------------------

run_map(RefKvs, Tree) ->
    PercentagesMapped = [
        0.0,
        0.2,
        0.5,
        0.7,
        1.0
    ],

    %%%%

    lists:foreach(
        fun(PercentageMapped) ->
            PHashRange = 100000,
            PHashCeiling = round(PercentageMapped * PHashRange),
            RandomFactor = rand:uniform(),

            MapFun =
                fun(K, V) ->
                    case erlang:phash2(canon_key(K), PHashRange) < PHashCeiling of
                        true ->
                            erlang:phash2([RandomFactor | canon_key(K)], 3);
                        %
                        false ->
                            V
                    end
                end,

            MappedTree = xb5_trees:map(MapFun, Tree),

            ExpectedMappedRef = orddict:to_list(orddict:map(MapFun, orddict:from_list(RefKvs))),

            ?assertEqual(
                length(ExpectedMappedRef),
                xb5_trees:size(MappedTree)
            ),

            ?assertKvListsCanonEqual(
                ExpectedMappedRef,
                xb5_trees:to_list(MappedTree)
            )
        end,
        PercentagesMapped
    ).

%% ------------------------------------------------------------------
%% Helpers: Structural Tests
%% ------------------------------------------------------------------

run_structure_test(InitFun) ->
    run_structure_test(InitFun, []).

run_structure_test(InitFun, Opts) ->
    SizeMultiplier = proplists:get_value(size_multiplier, Opts, 1),
    Size = round(SizeMultiplier * ?STRUCTURE_TEST_BASE_SIZE),

    _ = rand:seed(exsss, 1404887150367571),

    StatsAcc =
        lists:foldl(
            fun(_, Acc) ->
                % faster with numeric only
                NumericOnly = true,
                RefKvs = new_ref_kvs(Size, NumericOnly),

                Tree = InitFun(RefKvs),
                ?assertEqual(?STRUCTURE_TEST_BASE_SIZE, xb5_trees:size(Tree)),

                Stats = xb5_trees:structural_stats(Tree),

                %%%%%%%%%%

                {_, Height} = lists:keyfind(height, 1, Stats),
                {_, NodePercentages} = lists:keyfind(node_percentages, 1, Stats),
                {_, AvgKeysPerNode} = lists:keyfind(avg_keys_per_node, 1, Stats),

                Acc2 = structure_test_stats_acc(height, Height, Acc),
                Acc3 = structure_test_stats_acc(avg_keys_per_node, AvgKeysPerNode, Acc2),

                lists:foldl(
                    fun({NodeType, Percentage}, SubAcc) ->
                        structure_test_stats_acc({node_percentage, NodeType}, Percentage, SubAcc)
                    end,
                    Acc3,
                    NodePercentages
                )
            end,
            #{},
            lists:seq(1, ?STRUCTURE_TEST_ITERATIONS)
        ),

    %%%%%

    maps:map(
        fun(_Id, Samples) ->
            condense_stats(Samples)
        end,
        StatsAcc
    ).

structure_test_stats_acc(Id, Sample, Acc) ->
    try maps:get(Id, Acc) of
        PrevSamples ->
            Acc#{Id := [Sample | PrevSamples]}
    catch
        error:{badkey, K} when K =:= Id ->
            maps:put(Id, [Sample], Acc)
    end.

condense_stats(Samples) ->
    Sorted = lists:sort(Samples),
    Len = length(Sorted),

    Min = hd(Sorted),
    Max = lists:last(Sorted),

    Avg = lists:sum(Sorted) / Len,

    Median = list_median(Sorted, Len),

    StdDev = float(list_std_dev(Avg, Sorted)),

    #{
        min => float(Min),
        max => float(Max),
        avg => Avg,
        std_dev => StdDev,
        std_err => StdDev / math:sqrt(Len),
        median => float(Median)
    }.

list_median(SortedList, Len) ->
    case Len rem 2 of
        0 ->
            LeftPos = (Len div 2),
            [Left, Right] = lists:sublist(SortedList, LeftPos, 2),
            (Left + Right) / 2.0;
        %
        1 ->
            MidPos = (Len div 2) + 1,
            lists:nth(MidPos, SortedList)
    end.

list_std_dev(Avg, List) ->
    SquareDevs = lists:map(fun(Sample) -> math:pow(Sample - Avg, 2.0) end, List),
    Variance = lists:sum(SquareDevs) / length(SquareDevs),
    math:sqrt(Variance).

observed_error_margin(Id, CondensedStats) ->
    #{
        avg := ObservedAvg,
        std_err := ObservedStdErr
    } = Stats = maps:get(Id, CondensedStats),

    Margin = ?Z_SCORE_95 * ObservedStdErr,

    Lower = ObservedAvg - Margin,
    Higher = ObservedAvg + Margin,

    #{lower => Lower, higher => Higher, stats => Stats}.

%% ------------------------------------------------------------------
%% Helpers: element generation
%% ------------------------------------------------------------------

new_key() ->
    new_key(_NumericOnly = false).

new_key(NumericOnly) when NumericOnly ->
    new_number();
new_key(NumericOnly) when not NumericOnly ->
    Die = rand:uniform(30),

    case Die of
        _ when Die =< 25 ->
            new_number();
        %
        26 ->
            BinSize = rand:uniform(16),
            crypto:strong_rand_bytes(BinSize);
        %
        27 ->
            TupleSize = rand:uniform(10) - 1,
            List = lists:map(fun(_) -> new_number() end, lists:seq(1, TupleSize)),
            list_to_tuple(List);
        %
        28 ->
            ListSize = rand:uniform(10) - 1,
            lists:map(fun(_) -> new_number() end, lists:seq(1, ListSize));
        %
        29 ->
            LikelyMapSize = rand:uniform(10) - 1,
            List = lists:map(
                fun(_) -> {new_number(), new_number()} end, lists:seq(1, LikelyMapSize)
            ),
            maps:from_list(List);
        %
        30 ->
            make_ref()
    end.

new_number() ->
    randomly_switch_number_type(rand:uniform(1 bsl 50) - (1 bsl 49)).

% number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
key_in_between(Key1, Key2) ->
    case key_type(Key1) of
        number ->
            case key_type(Key2) of
                number when Key2 - Key1 > 1 ->
                    {found, Key1 + 1};
                %
                number ->
                    none;
                %
                _ ->
                    {found, Key1 + 1}
            end;
        %
        %
        _ ->
            % Not worth the effort
            none
    end.

key_smaller(Key) ->
    case key_type(Key) of
        number ->
            Key - 1;
        %
        _ ->
            % Ensured to be smaller
            -100
    end.

key_larger(Key) ->
    case key_type(Key) of
        binary ->
            <<Key/bytes, Key/bytes>>;
        %
        _ ->
            <<"ensured to be larger">>
    end.

key_type(Key) when is_number(Key) ->
    number;
key_type(Key) when is_binary(Key) ->
    binary;
key_type(Key) when is_tuple(Key) ->
    tuple;
key_type(Key) when is_list(Key) ->
    list;
key_type(Key) when is_map(Key) ->
    map;
key_type(Key) when is_reference(Key) ->
    reference.
