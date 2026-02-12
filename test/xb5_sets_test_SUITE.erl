-module(xb5_sets_test_SUITE).

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
    test_lookup/1,
    test_add/1,
    test_insert/1,
    test_delete_sequential/1,
    test_delete_shuffled/1
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

%% Test exports - set operations
-export([
    test_difference/1,
    test_intersection/1,
    test_intersection3/1,
    test_is_disjoint/1,
    test_is_equal/1,
    test_is_subset/1,
    test_union/1,
    test_union3/1
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
    test_filter/1,
    test_filtermap/1,
    test_fold/1,
    test_is_set/1,
    test_map/1,
    test_rewrap/1
]).

%% Test exports - structure
-export([
    test_structure_sequentially_built/1,
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

-define(assertListsCanonEqual(ExpectedL, TestedL),
    (?assertEqual(
        lists:map(fun canon_element/1, (ExpectedL)),
        lists:map(fun canon_element/1, (TestedL))
    ))
).

-define(assertCanonEqual(Expected, Tested),
    (?assertEqual(
        canon_element(Expected),
        canon_element(Tested)
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
%% CT Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [
        {basic_api, [parallel], [
            test_construction,
            test_construction_repeated,
            test_lookup,
            test_add,
            test_insert,
            test_delete_sequential,
            test_delete_shuffled
        ]},
        {smaller_and_larger, [parallel], [
            test_smallest,
            test_largest,
            test_smaller,
            test_larger,
            test_take_smallest,
            test_take_largest
        ]},
        {set_operations, [parallel], [
            test_difference,
            test_intersection,
            test_intersection3,
            test_is_disjoint,
            test_is_equal,
            test_is_subset,
            test_union,
            test_union3
        ]},
        {iterators, [parallel], [
            test_iterator,
            test_iterator_reversed,
            test_iterator_from,
            test_iterator_from_reversed
        ]},
        {additional_functions, [parallel], [
            test_balance,
            test_filter,
            test_filtermap,
            test_fold,
            test_is_set,
            test_map,
            test_rewrap
        ]},
        {structure, [parallel], [
            % Uncomment as needed, these take a long time in CI.
            %
            % test_structure_sequentially_built,
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
        fun(Size, RefElements) ->
            Set = xb5_sets:from_list(RefElements),
            ?assertListsCanonEqual(RefElements, xb5_sets:to_list(Set)),
            ?assertEqual(Size, xb5_sets:size(Set)),
            ?assertEqual(Size =:= 0, xb5_sets:is_empty(Set)),
            ?assertEqual(Set, new_set_from_each_inserted(RefElements)),
            ?assertEqual(Set, xb5_sets:from_ordset(RefElements)),

            case RefElements of
                [] ->
                    ?assertEqual(Set, xb5_sets:empty());
                %
                [SingleElement] ->
                    ?assertEqual(Set, xb5_sets:singleton(SingleElement));
                %
                _ ->
                    ok
            end
        end
    ).

test_construction_repeated(_Config) ->
    foreach_tested_size(fun run_construction_repeated_test/2).

test_lookup(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Set) ->
            foreach_existing_element(
                fun(Element) ->
                    ?assertEqual(true, xb5_sets:is_member(Element, Set)),
                    ?assertEqual(true, xb5_sets:is_element(Element, Set))
                end,
                RefElements,
                Size
            ),

            %%%%%%%%%%%

            foreach_non_existent_element(
                fun(Element) ->
                    ?assertEqual(false, xb5_sets:is_member(Element, Set)),
                    ?assertEqual(false, xb5_sets:is_element(Element, Set))
                end,
                RefElements,
                100
            )
        end
    ).

test_add(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Set) ->
            foreach_existing_element(
                fun(Element) ->
                    Set2 = xb5_sets:add(Element, Set),
                    ?assertEqual(Size, xb5_sets:size(Set2)),
                    ?assertListsCanonEqual(
                        RefElements,
                        xb5_sets:to_list(Set2)
                    ),

                    ?assertEqual(Set2, xb5_sets:add_element(Element, Set))
                end,
                RefElements,
                min(50, Size)
            ),

            %%%%%%%%%%%%%%%%%%

            foreach_non_existent_element(
                fun(Element) ->
                    Set2 = xb5_sets:add(Element, Set),
                    ?assertEqual(Size + 1, xb5_sets:size(Set2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefElements),
                        xb5_sets:to_list(Set2)
                    ),

                    ?assertEqual(Set2, xb5_sets:add_element(Element, Set))
                end,
                RefElements,
                50
            )
        end
    ).

test_insert(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Set) ->
            foreach_existing_element(
                fun(Element) ->
                    ?assertError({key_exists, Element}, xb5_sets:insert(Element, Set))
                end,
                RefElements,
                min(50, Size)
            ),

            %%%%%%%%%%%%%%%%%%

            foreach_non_existent_element(
                fun(Element) ->
                    Set2 = xb5_sets:insert(Element, Set),
                    ?assertEqual(Size + 1, xb5_sets:size(Set2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefElements),
                        xb5_sets:to_list(Set2)
                    )
                end,
                RefElements,
                50
            )
        end
    ).

test_delete_sequential(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            DeleteKeys = lists:map(fun randomly_switch_number_type/1, RefElements),

            {SetN, []} =
                lists:foldl(
                    fun(Element, {Set1, RemainingElements1}) ->
                        test_delete_non_existing_keys(Set1, RemainingElements1, 3),

                        Set2 = xb5_sets:delete(Element, Set1),
                        RemainingElements2 = remove_from_sorted_list(Element, RemainingElements1),
                        ?assertListsCanonEqual(RemainingElements2, xb5_sets:to_list(Set2)),
                        ?assertEqual(length(RemainingElements2), xb5_sets:size(Set2)),
                        ?assertEqual(RemainingElements2 =:= [], xb5_sets:is_empty(Set2)),

                        ?assertEqual(Set2, xb5_sets:delete_any(Element, Set1)),
                        ?assertEqual(Set2, xb5_sets:del_element(Element, Set1)),

                        {Set2, RemainingElements2}
                    end,
                    {Set, RefElements},
                    DeleteKeys
                ),

            ?assertEqual([], xb5_sets:to_list(SetN)),
            ?assertEqual(0, xb5_sets:size(SetN)),
            ?assertEqual(true, xb5_sets:is_empty(SetN)),

            test_delete_non_existing_keys(SetN, [], 3)
        end
    ).

test_delete_shuffled(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            DeleteKeys = lists:map(fun randomly_switch_number_type/1, list_shuffle(RefElements)),

            {SetN, []} =
                lists:foldl(
                    fun(Element, {Set1, RemainingElements1}) ->
                        test_delete_non_existing_keys(Set1, RemainingElements1, 3),

                        Set2 = xb5_sets:delete(Element, Set1),
                        RemainingElements2 = remove_from_sorted_list(Element, RemainingElements1),
                        ?assertListsCanonEqual(RemainingElements2, xb5_sets:to_list(Set2)),
                        ?assertEqual(length(RemainingElements2), xb5_sets:size(Set2)),
                        ?assertEqual(RemainingElements2 =:= [], xb5_sets:is_empty(Set2)),

                        ?assertEqual(Set2, xb5_sets:delete_any(Element, Set1)),
                        ?assertEqual(Set2, xb5_sets:del_element(Element, Set1)),

                        {Set2, RemainingElements2}
                    end,
                    {Set, RefElements},
                    DeleteKeys
                ),

            ?assertEqual([], xb5_sets:to_list(SetN)),
            ?assertEqual(0, xb5_sets:size(SetN)),
            ?assertEqual(true, xb5_sets:is_empty(SetN)),

            test_delete_non_existing_keys(SetN, [], 3)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Smaller and Larger
%% ------------------------------------------------------------------

test_smallest(_Config) ->
    foreach_test_set(
        fun
            (0, _RefElements, Set) ->
                ?assertError(empty_set, xb5_sets:smallest(Set));
            %
            (_Size, RefElements, Set) ->
                ?assert(xb5_sets:smallest(Set) == hd(RefElements))
        end
    ).

test_largest(_Config) ->
    foreach_test_set(
        fun
            (0, _RefElements, Set) ->
                ?assertError(empty_set, xb5_sets:largest(Set));
            %
            (_Size, RefElements, Set) ->
                ?assert(xb5_sets:largest(Set) == lists:last(RefElements))
        end
    ).

test_smaller(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            run_smaller(RefElements, Set)
        end
    ).

test_larger(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            run_larger(RefElements, Set)
        end
    ).

test_take_smallest(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            run_take_smallest(RefElements, Set)
        end
    ).

test_take_largest(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            run_take_largest(lists:reverse(RefElements), Set)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Set Operations
%% ------------------------------------------------------------------

test_difference(_Config) ->
    foreach_test_set(fun run_difference_test/3).

test_intersection(_Config) ->
    ?assertError(function_clause, xb5_sets:intersection(xb5_utils:dialyzer_opaque_term([]))),
    foreach_test_set(fun run_intersection_test/3).

test_intersection3(_Config) ->
    foreach_test_set(fun run_intersection3_test/3).

test_is_disjoint(_Config) ->
    foreach_test_set(fun run_is_disjoint_test/3).

test_is_equal(_Config) ->
    foreach_test_set(fun run_is_equal_test/3).

test_is_subset(_Config) ->
    foreach_test_set(fun run_is_subset_test/3).

test_union(_Config) ->
    ?assertEqual(xb5_sets:new(), xb5_sets:union([])),
    foreach_test_set(fun run_union_test/3).

test_union3(_Config) ->
    foreach_test_set(fun run_union3_test/3).

%% ------------------------------------------------------------------
%% Tests - Iterators
%% ------------------------------------------------------------------

test_iterator(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            Iter = new_iterator(Set),
            ?assertListsCanonEqual(RefElements, iterate(Iter))
        end
    ).

test_iterator_reversed(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            Iter = xb5_sets:iterator(Set, reversed),
            ?assertListsCanonEqual(lists:reverse(RefElements), iterate(Iter))
        end
    ).

test_iterator_from(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            run_iterator_from(RefElements, Set)
        end
    ).

test_iterator_from_reversed(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            run_iterator_from_reversed(RefElements, Set)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Additional Functions
%% ------------------------------------------------------------------

test_balance(_Config) ->
    foreach_test_set(
        fun(_Size, _RefElements, Set) ->
            % Balancing does nothing
            ?assertEqual(Set, xb5_sets:balance(Set))
        end
    ).

test_filter(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Set) ->
            run_filter(Size, RefElements, Set)
        end
    ).

test_filtermap(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Set) ->
            run_filtermap(Size, RefElements, Set)
        end
    ).

test_fold(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            run_fold(RefElements, Set)
        end
    ).

test_is_set(_Config) ->
    ?assertEqual(false, xb5_sets:is_set(foobar)),
    ?assertEqual(false, xb5_sets:is_set(xb5_trees:new())),
    ?assertEqual(false, xb5_sets:is_set(xb5_bag:new())),

    foreach_test_set(
        fun(_Size, _RefElements, Set) ->
            ?assertEqual(true, xb5_sets:is_set(Set)),

            {xb5_set, Size, Root} = xb5_utils:dialyzer_opaque_term(Set),

            ?assertEqual(true, xb5_sets:is_set({xb5_set, Size, Root})),

            ?assertEqual(false, xb5_sets:is_set({xb5_set, -1, Root})),

            ?assertEqual((Size =:= 0), xb5_sets:is_set({xb5_set, 0, Root})),

            ?assertEqual(false, xb5_sets:is_set({xb5_set, Size, invalid_root})),

            ?assertEqual(false, xb5_sets:is_set({xb5_set, not_an_integer, Root}))
        end
    ).

test_map(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Set) ->
            run_map(RefElements, Set)
        end
    ).

test_rewrap(_Config) ->
    ?assertMatch({error, _}, xb5_sets:unwrap(xb5_bag:new())),
    ?assertMatch({error, _}, xb5_sets:unwrap(xb5_trees:new())),
    ?assertMatch({error, _}, xb5_sets:unwrap({xb5_set, -1, xb5_sets_node:new()})),
    ?assertMatch({error, _}, xb5_sets:unwrap({xb5_set, 2, xb5_sets_node:new()})),
    ?assertMatch({error, _}, xb5_sets:unwrap({xb5_set, 2, make_ref()})),

    foreach_test_set(
        fun(_Size, _RefElements, Col) ->
            {ok, Unwrapped} = xb5_sets:unwrap(Col),
            ?assertEqual(Col, xb5_sets:wrap(Unwrapped))
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Structure
%% ------------------------------------------------------------------

test_structure_sequentially_built(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefElements) ->
                ?assertEqual(RefElements, lists:usort(RefElements)),

                case rand:uniform(4) of
                    1 ->
                        new_set_from_each_inserted(RefElements);
                    2 ->
                        xb5_sets:from_list(RefElements);
                    3 ->
                        new_set_from_each_inserted(lists:reverse(RefElements));
                    4 ->
                        xb5_sets:from_list(lists:reverse(RefElements))
                end
            end
        ),

    %%%%%%%%%%

    ?assertPreciseStat(height, 5.0, CondensedStats),

    ?assertPreciseStat(avg_keys_per_node, 2.9940119760479043, CondensedStats).

%%%%

test_structure_randomly_built(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefElements) ->
                new_set_from_each_inserted(list_shuffle(RefElements))
            end
        ),

    %%%%%%%%%%

    ?assertConfidentStat(height, 5.0, CondensedStats),

    ?assertConfidentStat(avg_keys_per_node, 2.9143881267511302, CondensedStats).

%%%%

test_structure_build_seqIns2x_seqDelSmallerHalf(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefElements) ->
                % 1) build sequentially
                Set1 = new_set_from_each_inserted(RefElements),

                % 2) delete smaller half sequentially
                AmountToDelete = xb5_sets:size(Set1) div 2,
                ItemsToDelete = lists:sublist(RefElements, AmountToDelete),
                lists:foldl(fun xb5_sets:delete/2, Set1, ItemsToDelete)
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
            fun(RefElements) ->
                % 1) build sequentially
                Set1 = new_set_from_each_inserted(RefElements),

                % 2) delete greater half sequentially
                AmountToDelete = xb5_sets:size(Set1) div 2,
                ItemsToDelete = lists:sublist(lists:reverse(RefElements), AmountToDelete),
                lists:foldl(fun xb5_sets:delete/2, Set1, ItemsToDelete)
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
            fun(RefElements) ->
                % 1) build sequentially
                Set1 = new_set_from_each_inserted(RefElements),

                % 2) delete half randomly
                AmountToDelete = xb5_sets:size(Set1) div 2,
                ItemsToDelete = lists:sublist(list_shuffle(RefElements), AmountToDelete),
                lists:foldl(fun xb5_sets:delete/2, Set1, ItemsToDelete)
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
            fun(RefElements) ->
                % 1) build randomly
                Set1 = new_set_from_each_inserted(list_shuffle(RefElements)),

                % 2) delete half randomly
                AmountToDelete = xb5_sets:size(Set1) div 2,
                ItemsToDelete = lists:sublist(list_shuffle(RefElements), AmountToDelete),
                lists:foldl(fun xb5_sets:delete/2, Set1, ItemsToDelete)
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
            fun(RefElements) ->
                % 1) build randomly
                Set1 = new_set_from_each_inserted(list_shuffle(RefElements)),

                % 2) delete smaller half sequentially
                AmountToDelete = xb5_sets:size(Set1) div 2,
                ItemsToDelete = lists:sublist(RefElements, AmountToDelete),
                lists:foldl(fun xb5_sets:delete/2, Set1, ItemsToDelete)
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
            fun(RefElements) ->
                % 1) build sequentially
                Set1 = new_set_from_each_inserted(RefElements),

                % 2) delete every 5th item sequentially
                ItemsToDelete =
                    lists:filtermap(
                        fun({Index, E}) ->
                            ((Index rem 5 =:= 0) andalso
                                {true, E})
                        end,
                        lists:enumerate(RefElements)
                    ),

                lists:foldl(fun xb5_sets:delete/2, Set1, ItemsToDelete)
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

foreach_test_set(Fun) ->
    foreach_test_set(Fun, []).

foreach_test_set(Fun, Opts) ->
    foreach_tested_size(
        fun(Size, RefElements) ->
            Set = xb5_sets:from_list(maybe_shuffle_elements_for_new_set(RefElements)),
            ?assertEqual(Size, xb5_sets:size(Set)),

            Stats = xb5_sets:structural_stats(Set),
            ?assertEqual(Size, proplists:get_value(total_keys, Stats)),

            Fun(Size, RefElements, Set)
        end,
        Opts
    ).

maybe_shuffle_elements_for_new_set(RefElements) ->
    % Sequential insertion takes very different paths from random insertion,
    % the occasional shuffle means we get good coverage of both.

    case rand:uniform() < 1 / 3 of
        true ->
            list_shuffle(RefElements);
        %
        false ->
            RefElements
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
    RefElements = new_ref_elements(Size, NumericOnly),
    Fun(Size, RefElements).

new_ref_elements(Size, NumericOnly) ->
    new_ref_elements_recur(Size, NumericOnly, _Acc = []).

new_ref_elements_recur(Size, NumericOnly, Acc) when Size > 0 ->
    NewElement = new_element(NumericOnly),

    case lists:member(NewElement, Acc) of
        false ->
            UpdatedAcc = [NewElement | Acc],
            new_ref_elements_recur(Size - 1, NumericOnly, UpdatedAcc);
        %
        true ->
            new_ref_elements_recur(Size, NumericOnly, Acc)
    end;
new_ref_elements_recur(0, _, Acc) ->
    lists:sort(Acc).

randomly_switch_number_type(Element) ->
    case rand:uniform(3) of
        1 when is_integer(Element) ->
            float(Element);
        %
        1 when is_float(Element) ->
            trunc(Element);
        %
        _ ->
            Element
    end.

canon_element(Element) when is_float(Element) ->
    case math:fmod(Element, 1.0) == 0 of
        true ->
            trunc(Element);
        %
        false ->
            Element
    end;
canon_element(Integer) when is_integer(Integer) ->
    Integer;
canon_element(List) when is_list(List) ->
    canon_list(List);
canon_element(Tuple) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    Mapped = lists:map(fun canon_element/1, List),
    list_to_tuple(Mapped);
canon_element(Map) when is_map(Map) ->
    List = maps:to_list(Map),
    Mapped = lists:map(fun canon_element/1, List),
    maps:from_list(Mapped);
canon_element(Other) ->
    Other.

canon_list([H | T]) ->
    [canon_element(H) | canon_list(T)];
canon_list([]) ->
    [];
canon_list(ImproperTail) ->
    canon_element(ImproperTail).

new_set_from_each_inserted(List) ->
    Set = xb5_sets:new(),

    ?assertEqual(0, xb5_sets:size(Set)),
    ?assertEqual(true, xb5_sets:is_empty(Set)),

    new_set_from_each_inserted_recur(List, Set).

new_set_from_each_inserted_recur([Element | Next], Set) ->
    UpdatedSet = xb5_sets:insert(Element, Set),
    new_set_from_each_inserted_recur(Next, UpdatedSet);
new_set_from_each_inserted_recur([], Set) ->
    Set.

%%%%%%%%%%%%%%%

foreach_existing_element(Fun, RefElements, Amount) ->
    Chosen = lists:sublist(list_shuffle(RefElements), Amount),

    lists:foreach(
        fun(Element) ->
            Fun(randomly_switch_number_type(Element))
        end,
        Chosen
    ).

foreach_non_existent_element(Fun, RefElements, Amount) when Amount > 0 ->
    Element = new_element(),

    case lists:any(fun(E) -> E == Element end, RefElements) of
        false ->
            Fun(Element),
            foreach_non_existent_element(Fun, RefElements, Amount - 1);
        %
        true ->
            foreach_non_existent_element(Fun, RefElements, Amount)
    end;
foreach_non_existent_element(_, _, 0) ->
    ok.

list_shuffle(List) ->
    WithWeights = lists:map(fun(V) -> [rand:uniform() | V] end, List),
    Shuffled = lists:sort(WithWeights),
    lists:map(fun([_ | V]) -> V end, Shuffled).

add_to_sorted_list(Elem, [H | T]) ->
    case Elem > H of
        true ->
            [H | add_to_sorted_list(Elem, T)];
        %
        false ->
            [Elem, H | T]
    end;
add_to_sorted_list(Elem, []) ->
    [Elem].

remove_from_sorted_list(Elem, [H | T]) ->
    if
        Elem > H ->
            [H | remove_from_sorted_list(Elem, T)];
        %
        Elem == H ->
            T
    end.

-dialyzer({nowarn_function, fun1_error_not_to_be_called/0}).
fun1_error_not_to_be_called() ->
    fun(_) ->
        error(not_to_be_called)
    end.

%% ------------------------------------------------------------------
%% Helper Functions: construction repeated
%% ------------------------------------------------------------------

run_construction_repeated_test(Size, RefElements) ->
    Amount = min(length(RefElements), 50),

    ElementsToRepeat = lists:sublist(list_shuffle(RefElements), Amount),

    run_construction_repeated_test(Size, ElementsToRepeat, RefElements).

run_construction_repeated_test(Size, [ElementToRepeat | Next], RefElements) ->
    List = add_to_sorted_list(randomly_switch_number_type(ElementToRepeat), RefElements),

    Set = xb5_sets:from_list(List),

    ?assertEqual(Size, xb5_sets:size(Set)),

    ?assertListsCanonEqual(
        RefElements,
        xb5_sets:to_list(Set)
    ),

    %%%

    SetShuffled = xb5_sets:from_list(list_shuffle(List)),

    ?assertEqual(Size, xb5_sets:size(SetShuffled)),

    ?assertListsCanonEqual(
        RefElements,
        xb5_sets:to_list(SetShuffled)
    ),

    run_construction_repeated_test(Size, Next, RefElements);
run_construction_repeated_test(_, [], _) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers: deletion
%% ------------------------------------------------------------------

test_delete_non_existing_keys(Set, RemainingElements, Amount) when Amount > 0 ->
    Element = new_element(),

    case lists:any(fun(E) -> E == Element end, RemainingElements) of
        false ->
            ?assertError({badkey, Element}, xb5_sets:delete(Element, Set)),
            ?assertEqual(Set, xb5_sets:delete_any(Element, Set)),

            ?assertEqual(Set, xb5_sets:del_element(Element, Set)),

            test_delete_non_existing_keys(Set, RemainingElements, Amount - 1);
        %
        true ->
            test_delete_non_existing_keys(Set, RemainingElements, Amount)
    end;
test_delete_non_existing_keys(_, _, 0) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers: smaller and larger
%% ------------------------------------------------------------------

run_smaller(RefElements, Set) ->
    case RefElements of
        [] ->
            Element = new_element(),
            ?assertEqual(none, xb5_sets:smaller(Element, Set));
        %
        [SingleElement] ->
            ?assertEqual(none, xb5_sets:smaller(randomly_switch_number_type(SingleElement), Set)),

            LargerElement = element_larger(SingleElement),
            ?assert(xb5_sets:smaller(LargerElement, Set) == {found, SingleElement}),

            SmallerElement = element_smaller(SingleElement),
            ?assertEqual(none, xb5_sets:smaller(SmallerElement, Set));
        %
        [FirstElement | Next] ->
            ?assertEqual(none, xb5_sets:smaller(randomly_switch_number_type(FirstElement), Set)),

            SmallerElement = element_smaller(FirstElement),
            ?assertEqual(none, xb5_sets:smaller(SmallerElement, Set)),

            run_smaller_recur(FirstElement, Next, Set)
    end.

run_smaller_recur(Expected, [LastElement], Set) ->
    ?assertCanonEqual(
        {found, Expected},
        xb5_sets:smaller(randomly_switch_number_type(LastElement), Set)
    ),

    LargerElement = element_larger(LastElement),
    ?assert(LargerElement > LastElement),
    ?assert(xb5_sets:smaller(LargerElement, Set) == {found, LastElement});
run_smaller_recur(Expected, [Element | Next], Set) ->
    ?assert(xb5_sets:smaller(randomly_switch_number_type(Element), Set) == {found, Expected}),

    case element_in_between(Expected, Element) of
        {found, InBetween} ->
            ?assert(InBetween > Expected),
            ?assert(InBetween < Element),
            ?assert(xb5_sets:smaller(InBetween, Set) == {found, Expected});
        %
        none ->
            ok
    end,

    run_smaller_recur(Element, Next, Set).

%%%%%%%%%%%%%%%%%

run_larger(RefElements, Set) ->
    case lists:reverse(RefElements) of
        [] ->
            Element = new_element(),
            ?assertEqual(none, xb5_sets:larger(Element, Set));
        %
        [SingleElement] ->
            ?assertEqual(none, xb5_sets:larger(SingleElement, Set)),

            LargerElement = element_larger(SingleElement),
            ?assertEqual(none, xb5_sets:larger(LargerElement, Set)),

            SmallerElement = element_smaller(SingleElement),
            ?assert(xb5_sets:larger(SmallerElement, Set) == {found, SingleElement});
        %
        [LastElement | Next] ->
            ?assertEqual(none, xb5_sets:larger(randomly_switch_number_type(LastElement), Set)),

            LargerElement = element_larger(LastElement),
            ?assertEqual(none, xb5_sets:larger(LargerElement, Set)),

            run_larger_recur(LastElement, Next, Set)
    end.

run_larger_recur(Expected, [FirstElement], Set) ->
    ?assertCanonEqual(
        {found, Expected},
        xb5_sets:larger(randomly_switch_number_type(FirstElement), Set)
    ),

    SmallerElement = element_smaller(FirstElement),
    ?assert(SmallerElement < FirstElement),
    ?assert(xb5_sets:larger(SmallerElement, Set) == {found, FirstElement});
run_larger_recur(Expected, [Element | Next], Set) ->
    ?assert(xb5_sets:larger(randomly_switch_number_type(Element), Set) == {found, Expected}),

    case element_in_between(Element, Expected) of
        {found, InBetween} ->
            ?assert(InBetween < Expected),
            ?assert(InBetween > Element),
            ?assert(xb5_sets:larger(InBetween, Set) == {found, Expected});
        %
        none ->
            ok
    end,

    run_larger_recur(Element, Next, Set).

%%%%%%%%%%%%%%%%%

run_take_smallest([Expected | Next], Set) ->
    {Taken, Set2} = xb5_sets:take_smallest(Set),
    ?assert(Taken == Expected),
    ?assertEqual(length(Next), xb5_sets:size(Set2)),
    run_take_smallest(Next, Set2);
run_take_smallest([], Set) ->
    ?assertError(empty_set, xb5_sets:take_smallest(Set)).

run_take_largest([Expected | Next], Set) ->
    {Taken, Set2} = xb5_sets:take_largest(Set),
    ?assert(Taken == Expected),
    ?assertEqual(length(Next), xb5_sets:size(Set2)),
    run_take_largest(Next, Set2);
run_take_largest([], Set) ->
    ?assertError(empty_set, xb5_sets:take_largest(Set)).

%% ------------------------------------------------------------------
%% Helpers: Difference
%% ------------------------------------------------------------------

run_difference_test(Size, RefElements, Set) ->
    SelfDifference = xb5_sets:difference(Set, Set),

    ?assertEqual(0, xb5_sets:size(SelfDifference)),
    ?assertEqual([], xb5_sets:to_list(SelfDifference)),

    %%%%%%%%%%%%%%%

    foreach_second_set(
        fun(RefElements2, Set2) ->
            Difference = xb5_sets:difference(Set, Set2),

            %%

            ExpectedRemainingElements = ordsets:subtract(
                ordsets:from_list(RefElements), ordsets:from_list(RefElements2)
            ),

            ?assertEqual(
                ordsets:size(ExpectedRemainingElements),
                xb5_sets:size(Difference)
            ),

            ?assertListsCanonEqual(
                ordsets:to_list(ExpectedRemainingElements),
                xb5_sets:to_list(Difference)
            ),

            %%

            ?assertEqual(Difference, xb5_sets:subtract(Set, Set2))
        end,
        Size,
        RefElements
    ).

%%%

foreach_second_set(Fun, Size, RefElements) ->
    foreach_second_set(Fun, Size, RefElements, _Opts = []).

foreach_second_set(Fun, Size, RefElements, Opts) ->
    foreach_second_set_variants1(Fun, Size, RefElements, Opts),

    _ =
        proplists:get_value(test_variants2, Opts, false) andalso
            foreach_second_set_variants2(Fun, Size, RefElements),

    ok.

%%

foreach_second_set_variants1(Fun, Size, RefElements, Opts) ->
    Amounts2 = lists:usort([
        0,
        1,
        Size,
        rand:uniform(max(1, Size)),
        rand:uniform(Size + 100)
    ]),

    PercentagesInCommon = [0.0, 0.5, 1.0],

    ParamCombos = [
        {Amount2, PercentageInCommon}
     || Amount2 <- Amounts2,
        PercentageInCommon <- PercentagesInCommon
    ],

    MaxCombos = proplists:get_value(max_combos, Opts, length(ParamCombos)),

    lists:foreach(
        fun({Amount2, PercentageInCommon}) ->
            RepeatedAmount = floor(PercentageInCommon * min(Amount2, Size)),
            NewAmount = Amount2 - RepeatedAmount,

            RepeatedElements = lists:sublist(list_shuffle(RefElements), RepeatedAmount),
            NewElements = [new_element() || _ <- lists:seq(1, NewAmount)],

            RefElements2 = lists:map(
                fun randomly_switch_number_type/1, lists:usort(RepeatedElements ++ NewElements)
            ),

            Set2 = xb5_sets:from_list(maybe_shuffle_elements_for_new_set(RefElements2)),

            Fun(RefElements2, Set2)
        end,
        lists:sublist(list_shuffle(ParamCombos), MaxCombos)
    ).

%%

foreach_second_set_variants2(Fun, Size, RefElements) ->
    % sequential

    Amounts2 = [S || S <- lists:usort([0, 1, Size - 1, Size + 1]), S >= 0],

    List2 = [
        sequential_ref_elements(Placement, S, RefElements)
     || Placement <- [before, after_],
        S <- Amounts2
    ],

    lists:foreach(
        fun(RefElements2) ->
            Set2 = xb5_sets:from_list(RefElements2),
            Fun(RefElements2, Set2)
        end,
        List2
    ).

sequential_ref_elements(_, Size, RefElements) when Size =:= 0 orelse RefElements =:= [] ->
    [];
sequential_ref_elements(before, Size, RefElements) ->
    sequential_ref_elements_before(Size, hd(RefElements), []);
sequential_ref_elements(after_, Size, RefElements) ->
    sequential_ref_elements_after(Size, lists:last(RefElements)).

sequential_ref_elements_before(Size, Next, Acc) when Size > 0 ->
    SmallerElement = element_smaller(Next),
    ?assertMatch(_ when SmallerElement < Next, {SmallerElement, Next}),
    sequential_ref_elements_before(Size - 1, SmallerElement, [SmallerElement | Acc]);
sequential_ref_elements_before(0, _, Acc) ->
    Acc.

sequential_ref_elements_after(Size, Prev) when Size > 0 ->
    LargerElement = element_larger(Prev),
    ?assertMatch(_ when LargerElement > Prev, {LargerElement, Prev}),
    [LargerElement | sequential_ref_elements_after(Size - 1, LargerElement)];
sequential_ref_elements_after(0, _) ->
    [].

%% ------------------------------------------------------------------
%% Helpers: Intersection
%% ------------------------------------------------------------------

run_intersection_test(Size, RefElements, Set) ->
    SelfIntersect = xb5_sets:intersection(Set, Set),

    ?assertEqual(Size, xb5_sets:size(SelfIntersect)),

    ?assertListsCanonEqual(
        RefElements,
        xb5_sets:to_list(SelfIntersect)
    ),

    %%%%%%%%%%%%%%%

    foreach_second_set(
        fun(RefElements2, Set2) ->
            Intersection = xb5_sets:intersection(Set, Set2),

            %%

            ExpectedRemainingElements = ordsets:intersection(
                ordsets:from_list(RefElements), ordsets:from_list(RefElements2)
            ),

            ?assertEqual(
                ordsets:size(ExpectedRemainingElements),
                xb5_sets:size(Intersection)
            ),

            ?assertListsCanonEqual(
                ordsets:to_list(ExpectedRemainingElements),
                xb5_sets:to_list(Intersection)
            ),

            %%

            ?assertListsCanonEqual(
                ordsets:to_list(ExpectedRemainingElements),
                xb5_sets:to_list(xb5_sets:intersection(Set, Set2))
            )
        end,
        Size,
        RefElements
    ).

%% ------------------------------------------------------------------
%% Helpers: Intersection 3
%% ------------------------------------------------------------------

run_intersection3_test(Size, RefElements, Set) ->
    foreach_second_set(
        fun(RefElements2, Set2) ->
            foreach_second_set(
                fun(RefElements3, Set3) ->
                    ExpectedRemainingElements = ordsets:intersection([
                        ordsets:from_list(RefElements),
                        ordsets:from_list(RefElements2),
                        ordsets:from_list(RefElements3)
                    ]),

                    IntersectionInputs = [Set, Set2, Set3],

                    Intersection = xb5_sets:intersection(IntersectionInputs),

                    ?assertEqual(length(ExpectedRemainingElements), xb5_sets:size(Intersection)),

                    ?assertListsCanonEqual(
                        ExpectedRemainingElements, xb5_sets:to_list(Intersection)
                    ),

                    %%%

                    Intersection2 = xb5_sets:intersection(list_shuffle(IntersectionInputs)),

                    ?assertEqual(length(ExpectedRemainingElements), xb5_sets:size(Intersection2)),

                    ?assertListsCanonEqual(
                        ExpectedRemainingElements, xb5_sets:to_list(Intersection2)
                    ),

                    %%%

                    Intersection3 = xb5_sets:intersection(list_shuffle(IntersectionInputs)),

                    ?assertEqual(length(ExpectedRemainingElements), xb5_sets:size(Intersection3)),

                    ?assertListsCanonEqual(
                        ExpectedRemainingElements, xb5_sets:to_list(Intersection3)
                    )
                end,
                Size,
                RefElements,
                [{max_combos, 4}]
            )
        end,
        Size,
        RefElements,
        [{max_combos, 4}]
    ).

%% ------------------------------------------------------------------
%% Helpers: Is Disjoint
%% ------------------------------------------------------------------

run_is_disjoint_test(Size, RefElements, Set) ->
    ?assertEqual((Size =:= 0), xb5_sets:is_disjoint(Set, Set)),

    %%%%%%%

    foreach_second_set(
        fun(RefElements2, Set2) ->
            IsDisjoint = xb5_sets:is_disjoint(Set, Set2),

            ExpectedIsDisjoint = ordsets:is_disjoint(
                ordsets:from_list(RefElements), ordsets:from_list(RefElements2)
            ),

            ?assertEqual(ExpectedIsDisjoint, IsDisjoint),

            ?assertEqual(ExpectedIsDisjoint, xb5_sets:is_disjoint(Set2, Set))
        end,
        Size,
        RefElements
    ).

%% ------------------------------------------------------------------
%% Helpers: Is Equal
%% ------------------------------------------------------------------

run_is_equal_test(Size, RefElements, Set) ->
    ?assertEqual(true, xb5_sets:is_equal(Set, Set)),

    %%%%%%%

    foreach_second_set(
        fun(RefElements2, Set2) ->
            IsEqual = xb5_sets:is_equal(Set, Set2),

            ExpectedIsEqual = (RefElements == RefElements2),

            ?assertEqual(ExpectedIsEqual, IsEqual),

            %%

            ?assertEqual(ExpectedIsEqual, xb5_sets:is_equal(Set2, Set))
        end,
        Size,
        RefElements
    ).

%% ------------------------------------------------------------------
%% Helpers: Is Subset
%% ------------------------------------------------------------------

run_is_subset_test(Size, RefElements, Set) ->
    ?assertEqual(true, xb5_sets:is_subset(Set, Set)),

    %%%%%%

    foreach_second_set(
        fun(RefElements2, Set2) ->
            IsSubset = xb5_sets:is_subset(Set, Set2),

            ExpectedIsSubset = ordsets:is_subset(
                ordsets:from_list(RefElements), ordsets:from_list(RefElements2)
            ),

            ?assertEqual(ExpectedIsSubset, IsSubset),

            %%

            Size1 = xb5_sets:size(Set),
            Size2 = xb5_sets:size(Set2),

            case Size1 =:= Size2 of
                true ->
                    ?assertEqual(IsSubset, xb5_sets:is_subset(Set2, Set));
                _ ->
                    ok
            end
        end,
        Size,
        RefElements
    ).

%% ------------------------------------------------------------------
%% Helpers: Union
%% ------------------------------------------------------------------

run_union_test(Size, RefElements, Set) ->
    SelfUnion = xb5_sets:union(Set, Set),

    ?assertEqual(Size, xb5_sets:size(SelfUnion)),

    ?assertListsCanonEqual(RefElements, xb5_sets:to_list(SelfUnion)),

    %%%%%%%%%%%%%%

    foreach_second_set(
        fun(RefElements2, Set2) ->
            ExpectedUnionElements = lists:usort(RefElements ++ RefElements2),

            Union = xb5_sets:union(Set, Set2),

            ?assertEqual(
                length(ExpectedUnionElements),
                xb5_sets:size(Union)
            ),

            ?assertListsCanonEqual(
                ExpectedUnionElements,
                xb5_sets:to_list(Union)
            ),

            %%%%%%%

            Union2 = xb5_sets:union(Set2, Set),

            ?assertEqual(
                length(ExpectedUnionElements),
                xb5_sets:size(Union2)
            ),

            ?assertListsCanonEqual(
                ExpectedUnionElements,
                xb5_sets:to_list(Union2)
            ),

            %%%%%%%%%

            Union3 = xb5_sets:union([Set, Set2]),

            ?assertEqual(
                length(ExpectedUnionElements),
                xb5_sets:size(Union3)
            ),

            ?assertListsCanonEqual(
                ExpectedUnionElements,
                xb5_sets:to_list(Union3)
            ),

            %%%%%%%%%

            Union4 = xb5_sets:union([Set2, Set]),

            ?assertEqual(
                length(ExpectedUnionElements),
                xb5_sets:size(Union4)
            ),

            ?assertListsCanonEqual(
                ExpectedUnionElements,
                xb5_sets:to_list(Union4)
            ),

            %%%%%%%%%

            Union5Inputs = list_shuffle(
                lists:duplicate(rand:uniform(10), Set) ++
                    lists:duplicate(rand:uniform(10), Set2)
            ),
            Union5 = xb5_sets:union(Union5Inputs),

            ?assertEqual(
                length(ExpectedUnionElements),
                xb5_sets:size(Union5)
            ),

            ?assertListsCanonEqual(
                ExpectedUnionElements,
                xb5_sets:to_list(Union5)
            )
        end,
        Size,
        RefElements,
        [test_variants2]
    ).

%% ------------------------------------------------------------------
%% Helpers: Union List
%% ------------------------------------------------------------------

run_union3_test(Size, RefElements, Set) ->
    foreach_second_set(
        fun(RefElements2, Set2) ->
            foreach_second_set(
                fun(RefElements3, Set3) ->
                    ExpectedUnionElements = lists:usort(
                        RefElements ++ RefElements2 ++ RefElements3
                    ),
                    UnionInputs = [Set, Set2, Set3],

                    Union = xb5_sets:union(UnionInputs),

                    ?assertEqual(length(ExpectedUnionElements), xb5_sets:size(Union)),

                    ?assertListsCanonEqual(ExpectedUnionElements, xb5_sets:to_list(Union)),

                    %%%

                    Union2 = xb5_sets:union(list_shuffle(UnionInputs)),

                    ?assertEqual(length(ExpectedUnionElements), xb5_sets:size(Union2)),

                    ?assertListsCanonEqual(ExpectedUnionElements, xb5_sets:to_list(Union2)),

                    %%%

                    Union3 = xb5_sets:union(list_shuffle(UnionInputs)),

                    ?assertEqual(length(ExpectedUnionElements), xb5_sets:size(Union3)),

                    ?assertListsCanonEqual(ExpectedUnionElements, xb5_sets:to_list(Union3))
                end,
                Size,
                RefElements,
                [{max_combos, 4}]
            )
        end,
        Size,
        RefElements,
        [{max_combos, 4}, test_variants2]
    ).

%% ------------------------------------------------------------------
%% Helpers: iterators
%% ------------------------------------------------------------------

run_iterator_from(RefElements, Set) ->
    case RefElements of
        [] ->
            Iter = new_iterator_from(new_element(), Set),
            ?assertEqual([], iterate(Iter));
        %
        [SingleElement] ->
            Iter = new_iterator_from(randomly_switch_number_type(SingleElement), Set),
            ?assertListsCanonEqual(RefElements, iterate(Iter)),

            SmallerElement = element_smaller(SingleElement),
            Iter2 = new_iterator_from(SmallerElement, Set),
            ?assertListsCanonEqual(RefElements, iterate(Iter2)),

            LargerElement = element_larger(SingleElement),
            Iter3 = new_iterator_from(LargerElement, Set),
            ?assertEqual([], iterate(Iter3));
        %
        [FirstElement | _] ->
            SmallerElement = element_smaller(FirstElement),
            Iter = new_iterator_from(SmallerElement, Set),
            ?assertListsCanonEqual(RefElements, iterate(Iter)),

            run_iterator_from_recur(RefElements, Set)
    end.

run_iterator_from_recur([LastElement], Set) ->
    run_iterator_from_last_element(LastElement, Set);
run_iterator_from_recur([Elem1 | [Elem2 | _] = Next] = List, Set) ->
    Iter = new_iterator_from(Elem1, Set),
    ?assertListsCanonEqual(List, iterate(Iter)),

    case element_in_between(Elem1, Elem2) of
        {found, InBetween} ->
            ?assert(InBetween > Elem1),
            ?assert(InBetween < Elem2),
            Iter2 = new_iterator_from(InBetween, Set),
            ?assertListsCanonEqual(Next, iterate(Iter2));
        %
        none ->
            ok
    end,

    run_iterator_from_recur(Next, Set).

run_iterator_from_last_element(LastElement, Set) ->
    Iter = new_iterator_from(randomly_switch_number_type(LastElement), Set),
    ?assertListsCanonEqual([LastElement], iterate(Iter)),

    LargerElement = element_larger(LastElement),
    Iter2 = new_iterator_from(LargerElement, Set),
    ?assertEqual([], iterate(Iter2)).

new_iterator_from(Elem, Set) ->
    Iter = xb5_sets:iterator_from(Elem, Set),
    ?assertEqual(Iter, xb5_sets:iterator_from(Elem, Set, ordered)),
    Iter.

%%%%%%%%%%%%%%%%%

run_iterator_from_reversed(RefElements, Set) ->
    case lists:reverse(RefElements) of
        [] ->
            Iter = xb5_sets:iterator_from(new_element(), Set, reversed),
            ?assertEqual([], iterate(Iter));
        %
        [SingleElement] ->
            Iter = xb5_sets:iterator_from(
                randomly_switch_number_type(SingleElement), Set, reversed
            ),
            ?assertListsCanonEqual([SingleElement], iterate(Iter)),

            SmallerElement = element_smaller(SingleElement),
            Iter2 = xb5_sets:iterator_from(SmallerElement, Set, reversed),
            ?assertEqual([], iterate(Iter2)),

            LargerElement = element_larger(SingleElement),
            Iter3 = xb5_sets:iterator_from(LargerElement, Set, reversed),
            ?assertListsCanonEqual([SingleElement], iterate(Iter3));
        %
        [LastElement | _] = ReverseRefElements ->
            LargerElement = element_larger(LastElement),
            Iter = xb5_sets:iterator_from(LargerElement, Set, reversed),
            ?assertListsCanonEqual(ReverseRefElements, iterate(Iter)),

            run_iterator_from_reversed_recur(ReverseRefElements, Set)
    end.

run_iterator_from_reversed_recur([FirstElement], Set) ->
    run_iterator_from_reversed_first_element(FirstElement, Set);
run_iterator_from_reversed_recur([Elem2 | [Elem1 | _] = Tail] = List, Set) ->
    Iter = xb5_sets:iterator_from(Elem2, Set, reversed),
    ?assertListsCanonEqual(List, iterate(Iter)),

    case element_in_between(Elem1, Elem2) of
        {found, InBetween} ->
            ?assert(InBetween > Elem1),
            ?assert(InBetween < Elem2),
            Iter2 = xb5_sets:iterator_from(InBetween, Set, reversed),
            ?assertListsCanonEqual(Tail, iterate(Iter2));
        %
        none ->
            ok
    end,

    run_iterator_from_reversed_recur(Tail, Set).

run_iterator_from_reversed_first_element(FirstElement, Set) ->
    Iter = xb5_sets:iterator_from(randomly_switch_number_type(FirstElement), Set, reversed),
    ?assertListsCanonEqual([FirstElement], iterate(Iter)),

    SmallerElement = element_smaller(FirstElement),
    Iter2 = xb5_sets:iterator_from(SmallerElement, Set, reversed),
    ?assertEqual([], iterate(Iter2)).

%%%%%%%%%%%%%%%%%

new_iterator(Set) ->
    Iter = xb5_sets:iterator(Set),
    ?assertEqual(Iter, xb5_sets:iterator(Set, ordered)),
    Iter.

iterate(Iter) ->
    case xb5_sets:next(Iter) of
        {Element, Iter2} ->
            [Element | iterate(Iter2)];
        %
        none ->
            []
    end.

%% ------------------------------------------------------------------
%% Helper Functions: filter
%% ------------------------------------------------------------------

run_filter(Size, RefElements, Set) ->
    AmountsToRemove =
        case Size of
            0 ->
                [0];
            %
            _ ->
                lists:usort([
                    0, 1, Size, Size - 1, rand:uniform(Size), rand:uniform(Size), rand:uniform(Size)
                ])
        end,

    lists:foreach(
        fun(AmountToRemove) ->
            FilterFun =
                case Size of
                    0 ->
                        fun1_error_not_to_be_called();
                    %
                    _ ->
                        ElementsToRemove = lists:sublist(
                            list_shuffle(RefElements), AmountToRemove
                        ),
                        AuxSet = gb_sets:from_list(ElementsToRemove),
                        fun(E) -> not gb_sets:is_element(E, AuxSet) end
                end,

            FilteredSet = xb5_sets:filter(FilterFun, Set),

            ExpectedElementsRemaining = lists:filter(FilterFun, RefElements),
            ?assertListsCanonEqual(ExpectedElementsRemaining, xb5_sets:to_list(FilteredSet)),

            ?assertEqual(length(ExpectedElementsRemaining), xb5_sets:size(FilteredSet))
        end,
        AmountsToRemove
    ).

%% ------------------------------------------------------------------
%% Helper Functions: filtermap
%% ------------------------------------------------------------------

run_filtermap(Size, RefElements, Set) ->
    AmountsToKeep =
        case Size of
            0 ->
                [0];
            %
            _ ->
                lists:usort([
                    0, 1, Size, Size - 1, rand:uniform(Size), rand:uniform(Size), rand:uniform(Size)
                ])
        end,

    PercentagesToMap =
        case Size of
            0 ->
                [0.0];
            %
            _ ->
                [0.0, 0.2, 0.5, 0.8, 1.0]
        end,

    ParamCombos = [
        {AmountToKeep, PercentageToMap}
     || AmountToKeep <- AmountsToKeep,
        PercentageToMap <- PercentagesToMap
    ],

    %%%%

    lists:foreach(
        fun({AmountToKeep, PercentageToMap}) ->
            AmountToMap = floor(PercentageToMap * AmountToKeep),

            FiltermapFun =
                case Size of
                    0 ->
                        fun1_error_not_to_be_called();
                    %
                    _ ->
                        ElementsToKeep = lists:sublist(
                            list_shuffle(RefElements), AmountToKeep
                        ),
                        ElementsToMap = lists:sublist(
                            list_shuffle(ElementsToKeep), AmountToMap
                        ),

                        KeepSet = gb_sets:from_list(ElementsToKeep),
                        MapSet = gb_sets:from_list(ElementsToMap),

                        fun(E) ->
                            case gb_sets:is_element(E, MapSet) of
                                true ->
                                    {true, erlang:phash2(canon_element(E), 4)};
                                %
                                false ->
                                    gb_sets:is_element(E, KeepSet)
                            end
                        end
                end,

            FiltermappedSet = xb5_sets:filtermap(FiltermapFun, Set),

            ExpectedElementsRemaining = lists:usort(lists:filtermap(FiltermapFun, RefElements)),
            ?assertListsCanonEqual(ExpectedElementsRemaining, xb5_sets:to_list(FiltermappedSet)),

            ?assertEqual(length(ExpectedElementsRemaining), xb5_sets:size(FiltermappedSet))
        end,
        ParamCombos
    ).

%% ------------------------------------------------------------------
%% Helper Functions: fold
%% ------------------------------------------------------------------

run_fold(RefElements, Set) ->
    Tag = make_ref(),

    Fun =
        fun(E, Acc) ->
            case Acc of
                [{_, PrevE} | _] ->
                    ?assert(PrevE < E);
                [] ->
                    ok
            end,

            [{Tag, E} | Acc]
        end,

    ?assertListsCanonEqual(
        ordsets:fold(Fun, [], ordsets:from_list(RefElements)),
        xb5_sets:fold(Fun, [], Set)
    ).

%% ------------------------------------------------------------------
%% Helper Functions: map
%% ------------------------------------------------------------------

run_map(RefElements, Set) ->
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
                fun(E) ->
                    case erlang:phash2(canon_element(E), PHashRange) < PHashCeiling of
                        true ->
                            erlang:phash2([RandomFactor | canon_element(E)], 3);
                        %
                        false ->
                            E
                    end
                end,

            MappedSet = xb5_sets:map(MapFun, Set),

            ExpectedMappedRef = lists:usort(lists:map(MapFun, RefElements)),

            ?assertEqual(
                length(ExpectedMappedRef),
                xb5_sets:size(MappedSet)
            ),

            ?assertListsCanonEqual(
                ExpectedMappedRef,
                xb5_sets:to_list(MappedSet)
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
                RefElements = new_ref_elements(Size, NumericOnly),

                Set = InitFun(RefElements),
                ?assertEqual(?STRUCTURE_TEST_BASE_SIZE, xb5_sets:size(Set)),

                Stats = xb5_sets:structural_stats(Set),

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

new_element() ->
    new_element(_NumericOnly = false).

new_element(NumericOnly) when NumericOnly ->
    new_number();
new_element(NumericOnly) when not NumericOnly ->
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
element_in_between(Element1, Element2) ->
    case element_type(Element1) of
        number ->
            case element_type(Element2) of
                number when Element2 - Element1 > 1 ->
                    {found, Element1 + 1};
                %
                number ->
                    none;
                %
                _ ->
                    {found, Element1 + 1}
            end;
        %
        %
        _ ->
            % Not worth the effort
            none
    end.

element_smaller(Element) ->
    case element_type(Element) of
        number ->
            Element - 1;
        %
        tuple ->
            very_large_reference();
        %
        _ ->
            1 bsl 128
    end.

element_larger(Element) ->
    case element_type(Element) of
        binary ->
            <<Element/bytes, "_">>;
        %
        _ ->
            <<"ensured to be larger">>
    end.

very_large_reference() ->
    % https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#newer_reference_ext
    binary_to_term(<<
        131,
        90,
        0,
        3,
        %%
        119,
        (length(atom_to_list(node()))),
        (atom_to_binary(node()))/bytes,
        %%
        0,
        0,
        0,
        0,
        0,
        3,
        255,
        255,
        255,
        255,
        255,
        255,
        255,
        255,
        255,
        255
    >>).

element_type(Element) when is_number(Element) ->
    number;
element_type(Element) when is_reference(Element) ->
    reference;
element_type(Element) when is_tuple(Element) ->
    tuple;
element_type(Element) when is_map(Element) ->
    map;
element_type(Element) when is_list(Element) ->
    list;
element_type(Element) when is_binary(Element) ->
    binary.
