-module(xb5_bag_test_SUITE).

-ifndef(NO_CT_SUITE_BEHAVIOUR).
-behaviour(ct_suite).
-endif.

-include_lib("stdlib/include/assert.hrl").

%% CT exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

%% Test exports - Basic API
-export([
    test_construction/1,
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

%% Test exports - iterators
-export([
    test_iterator/1,
    test_iterator_reversed/1,
    test_iterator_from/1,
    test_iterator_from_reversed/1
]).

%% Test exports - order statistics
-export([
    test_nth/1,
    test_rank/1,
    test_rank_smaller/1,
    test_rank_larger/1,
    test_percentile_inclusive/1,
    test_percentile_exclusive/1,
    test_percentile_nearest_rank/1,
    test_percentile_hardcoded1/1,
    test_percentile_hardcoded2/1,
    test_percentile_hardcoded3/1,
    test_percentile_hardcoded4/1,
    test_percentile_rank/1,
    test_percentile_rank_hardcoded/1
]).

%% Test exports - additional functions
-export([
    test_filter/1,
    test_filtermap/1,
    test_fold/1,
    test_map/1,
    test_merge/1,
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

%% Test constants
-define(TESTED_SIZES,
    (lists:seq(0, 50) ++ lists:seq(55, 200, 5) ++ [997])
).

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

-define(assertCanonError(Reason, Block),
    (begin
        try (Block) of
            Return ->
                error({not_an_exception, Return})
        catch
            error:ThrownReason when ThrownReason == (Reason) ->
                ok;
            %
            ThrownClass:ThrownReason ->
                error(
                    {exception_mismatch, [
                        {expected, error, (Reason)},
                        {but_got, ThrownClass, ThrownReason}
                    ]}
                )
        end
    end)
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

-define(OPAQUE_TERM(V), (xb5_utils:dialyzer_opaque_term((V)))).

%% Reference data of most hardcoded tests was generated using this Google spreadsheet:
%% * https://docs.google.com/spreadsheets/d/1U2_9zVV0ZoLMHz1n7Hw-qENwbl-uCe93BUrOzCI4zvI/edit?gid=1769164134#gid=1769164134

%% ------------------------------------------------------------------
%% CT Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [
        {basic_api, [parallel], [
            test_construction,
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
        {iterators, [parallel], [
            test_iterator,
            test_iterator_reversed,
            test_iterator_from,
            test_iterator_from_reversed
        ]},
        {order_statistics, [parallel], [
            test_nth,
            test_rank,
            test_rank_smaller,
            test_rank_larger,
            test_percentile_inclusive,
            test_percentile_exclusive,
            test_percentile_nearest_rank,
            test_percentile_hardcoded1,
            test_percentile_hardcoded2,
            test_percentile_hardcoded3,
            test_percentile_hardcoded4,
            test_percentile_rank,
            test_percentile_rank_hardcoded
        ]},
        {additional_functions, [parallel], [
            test_filter,
            test_filtermap,
            test_fold,
            test_map,
            test_merge,
            test_rewrap
        ]},
        {structure, [parallel], [
            test_structure_sequentially_built,
            test_structure_randomly_built,
            test_structure_build_seqIns2x_seqDelSmallerHalf,
            test_structure_build_seqIns2x_seqDelGreaterHalf,
            test_structure_build_seqIns2x_randomlyDelHalf,
            test_structure_build_randomlyIns2x_randomlyDelHalf,
            test_structure_build_randomlyIns2x_seqDelSmallerHalf,
            test_structure_build_adversarial_deletion
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
            Col = xb5_bag:from_list(RefElements),
            ?assertListsCanonEqual(RefElements, xb5_bag:to_list(Col)),
            ?assertEqual(Size, xb5_bag:size(Col)),
            ?assertEqual(Size =:= 0, xb5_bag:is_empty(Col)),
            ?assertEqual(Col, new_collection_from_each_added(RefElements)),
            ?assertEqual(Col, xb5_bag:from_ordset(RefElements))
        end
    ).

test_lookup(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            foreach_existing_element(
                fun(Element) ->
                    ?assertEqual(true, xb5_bag:is_member(Element, Col))
                end,
                RefElements,
                Size
            ),

            %%%%%%%%%%%

            foreach_non_existent_element(
                fun(Element) ->
                    ?assertEqual(false, xb5_bag:is_member(Element, Col))
                end,
                RefElements,
                100
            )
        end
    ).

test_add(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            foreach_existing_element(
                fun(Element) ->
                    Col2 = xb5_bag:add(Element, Col),
                    ?assertEqual(Size + 1, xb5_bag:size(Col2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefElements),
                        xb5_bag:to_list(Col2)
                    )
                end,
                RefElements,
                min(50, Size)
            ),

            %%%%%%%%%%%%%%%%%%

            foreach_non_existent_element(
                fun(Element) ->
                    Col2 = xb5_bag:add(Element, Col),
                    ?assertEqual(Size + 1, xb5_bag:size(Col2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefElements),
                        xb5_bag:to_list(Col2)
                    )
                end,
                RefElements,
                50
            )
        end
    ).

test_insert(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            foreach_existing_element(
                fun(Element) ->
                    ?assertError({key_exists, Element}, xb5_bag:insert(Element, Col))
                end,
                RefElements,
                min(50, Size)
            ),

            %%%%%%%%%%%%%%%%%%

            foreach_non_existent_element(
                fun(Element) ->
                    Col2 = xb5_bag:insert(Element, Col),
                    ?assertEqual(Size + 1, xb5_bag:size(Col2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefElements),
                        xb5_bag:to_list(Col2)
                    )
                end,
                RefElements,
                50
            )
        end
    ).

test_delete_sequential(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            DeleteKeys = lists:map(fun randomly_switch_number_type/1, RefElements),

            {ColN, []} =
                lists:foldl(
                    fun(Element, {Col1, RemainingElements1}) ->
                        test_delete_non_existing_keys(Col1, RemainingElements1, 3),

                        Col2 = xb5_bag:delete(Element, Col1),
                        RemainingElements2 = remove_from_sorted_list(Element, RemainingElements1),
                        ?assertListsCanonEqual(RemainingElements2, xb5_bag:to_list(Col2)),
                        ?assertEqual(length(RemainingElements2), xb5_bag:size(Col2)),
                        ?assertEqual(RemainingElements2 =:= [], xb5_bag:is_empty(Col2)),

                        ?assertEqual(Col2, xb5_bag:delete_any(Element, Col1)),

                        {Col2, RemainingElements2}
                    end,
                    {Col, RefElements},
                    DeleteKeys
                ),

            ?assertEqual([], xb5_bag:to_list(ColN)),
            ?assertEqual(0, xb5_bag:size(ColN)),
            ?assertEqual(true, xb5_bag:is_empty(ColN)),

            test_delete_non_existing_keys(ColN, [], 3)
        end
    ).

test_delete_shuffled(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            DeleteKeys = lists:map(fun randomly_switch_number_type/1, list_shuffle(RefElements)),

            {ColN, []} =
                lists:foldl(
                    fun(Element, {Col1, RemainingElements1}) ->
                        test_delete_non_existing_keys(Col1, RemainingElements1, 3),

                        Col2 = xb5_bag:delete(Element, Col1),
                        RemainingElements2 = remove_from_sorted_list(Element, RemainingElements1),
                        ?assertListsCanonEqual(RemainingElements2, xb5_bag:to_list(Col2)),
                        ?assertEqual(length(RemainingElements2), xb5_bag:size(Col2)),
                        ?assertEqual(RemainingElements2 =:= [], xb5_bag:is_empty(Col2)),

                        ?assertEqual(Col2, xb5_bag:delete_any(Element, Col1)),

                        {Col2, RemainingElements2}
                    end,
                    {Col, RefElements},
                    DeleteKeys
                ),

            ?assertEqual([], xb5_bag:to_list(ColN)),
            ?assertEqual(0, xb5_bag:size(ColN)),
            ?assertEqual(true, xb5_bag:is_empty(ColN)),

            test_delete_non_existing_keys(ColN, [], 3)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Smaller and Larger
%% ------------------------------------------------------------------

test_smallest(_Config) ->
    foreach_test_collection(
        fun
            (0, _RefElements, Col) ->
                ?assertError(empty_items, xb5_bag:smallest(Col));
            %
            (_Size, RefElements, Col) ->
                ?assert(xb5_bag:smallest(Col) == hd(RefElements))
        end
    ).

test_largest(_Config) ->
    foreach_test_collection(
        fun
            (0, _RefElements, Col) ->
                ?assertError(empty_items, xb5_bag:largest(Col));
            %
            (_Size, RefElements, Col) ->
                ?assert(xb5_bag:largest(Col) == lists:last(RefElements))
        end
    ).

test_smaller(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_smaller(RefElements, Col)
        end
    ).

test_larger(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_larger(RefElements, Col)
        end
    ).

test_take_smallest(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_take_smallest(RefElements, Col)
        end
    ).

test_take_largest(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_take_largest(lists:reverse(RefElements), Col)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Iterators
%% ------------------------------------------------------------------

test_iterator(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            Iter = new_iterator(Col),
            ?assertListsCanonEqual(RefElements, iterate(Iter))
        end
    ).

test_iterator_reversed(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            Iter = xb5_bag:iterator(Col, reversed),
            ?assertListsCanonEqual(lists:reverse(RefElements), iterate(Iter))
        end
    ).

test_iterator_from(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_iterator_from(RefElements, Col)
        end
    ).

test_iterator_from_reversed(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_iterator_from_reversed(RefElements, Col)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Order Statistics
%% ------------------------------------------------------------------

test_nth(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            test_invalid_nth(Size, Col),

            _ = (Size =/= 0 andalso test_valid_nth(RefElements, Col))
        end
    ).

test_rank(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            foreach_non_existent_element(
                fun(Element) ->
                    ?assertEqual(none, xb5_bag:rank(Element, Col))
                end,
                RefElements,
                50
            ),

            %%%%

            foreach_existing_element(
                fun(Element) ->
                    ?assertEqual(
                        {rank, rank_in_sorted_list(Element, RefElements)},
                        xb5_bag:rank(Element, Col)
                    )
                end,
                RefElements,
                Size
            )
        end
    ).

test_rank_smaller(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_rank_smaller(RefElements, Col)
        end
    ).

test_rank_larger(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            run_rank_larger(Size, RefElements, Col)
        end
    ).

%%%%%%%%

test_percentile_inclusive(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            test_invalid_percentile_inclusive(Col),
            test_valid_percentile_inclusive(Size, RefElements, Col)
        end
    ),

    %%%%%%%%%%

    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            test_invalid_percentile_inclusive(Col),
            test_valid_percentile_inclusive(Size, RefElements, Col)
        end,
        [numeric_only]
    ).

%%%%%%%%

test_percentile_exclusive(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            test_invalid_percentile_exclusive(Col),
            test_valid_percentile_exclusive(Size, RefElements, Col)
        end
    ),

    %%%%%%%%%

    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            test_invalid_percentile_exclusive(Col),
            test_valid_percentile_exclusive(Size, RefElements, Col)
        end,
        [numeric_only]
    ).

%%%%%%%%%%%%

test_percentile_nearest_rank(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            test_invalid_percentile_nearest_rank(Col),
            test_valid_percentile_nearest_rank(Size, RefElements, Col)
        end
    ),

    %%%%%%%%%%%%

    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            test_invalid_percentile_nearest_rank(Col),

            _ = (Size =/= 0 andalso test_valid_percentile_nearest_rank(Size, RefElements, Col))
        end,
        [numeric_only]
    ).

%%%%%%%%

test_percentile_hardcoded1(_Config) ->
    Col = xb5_bag:from_list([1, 2, 3, 4]),
    Size = xb5_bag:size(Col),
    RefElements = xb5_bag:to_list(Col),

    % Inclusive

    ?assertCanonEqual({value, 1}, inclusive_percentile_rounded(0, Col)),
    ?assertCanonEqual({value, 1.15}, inclusive_percentile_rounded(0.05, Col)),
    ?assertCanonEqual({value, 1.3}, inclusive_percentile_rounded(0.1, Col)),
    ?assertCanonEqual({value, 1.45}, inclusive_percentile_rounded(0.15, Col)),
    ?assertCanonEqual({value, 1.6}, inclusive_percentile_rounded(0.2, Col)),
    ?assertCanonEqual({value, 1.75}, inclusive_percentile_rounded(0.25, Col)),
    ?assertCanonEqual({value, 1.9}, inclusive_percentile_rounded(0.3, Col)),
    ?assertCanonEqual({value, 2.05}, inclusive_percentile_rounded(0.35, Col)),
    ?assertCanonEqual({value, 2.2}, inclusive_percentile_rounded(0.4, Col)),
    ?assertCanonEqual({value, 2.35}, inclusive_percentile_rounded(0.45, Col)),
    ?assertCanonEqual({value, 2.5}, inclusive_percentile_rounded(0.5, Col)),
    ?assertCanonEqual({value, 2.65}, inclusive_percentile_rounded(0.55, Col)),
    ?assertCanonEqual({value, 2.8}, inclusive_percentile_rounded(0.6, Col)),
    ?assertCanonEqual({value, 2.95}, inclusive_percentile_rounded(0.65, Col)),
    ?assertCanonEqual({value, 3.1}, inclusive_percentile_rounded(0.7, Col)),
    ?assertCanonEqual({value, 3.25}, inclusive_percentile_rounded(0.75, Col)),
    ?assertCanonEqual({value, 3.4}, inclusive_percentile_rounded(0.8, Col)),
    ?assertCanonEqual({value, 3.55}, inclusive_percentile_rounded(0.85, Col)),
    ?assertCanonEqual({value, 3.7}, inclusive_percentile_rounded(0.9, Col)),
    ?assertCanonEqual({value, 3.85}, inclusive_percentile_rounded(0.95, Col)),
    ?assertCanonEqual({value, 4}, inclusive_percentile_rounded(1, Col)),

    test_valid_percentile_inclusive(Size, RefElements, Col),

    % Exclusive

    ?assertCanonEqual(none, exclusive_percentile_rounded(0.00, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.05, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.10, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.15, Col)),
    ?assertCanonEqual({value, 1}, exclusive_percentile_rounded(0.20, Col)),
    ?assertCanonEqual({value, 1.25}, exclusive_percentile_rounded(0.25, Col)),
    ?assertCanonEqual({value, 1.5}, exclusive_percentile_rounded(0.30, Col)),
    ?assertCanonEqual({value, 1.75}, exclusive_percentile_rounded(0.35, Col)),
    ?assertCanonEqual({value, 2}, exclusive_percentile_rounded(0.40, Col)),
    ?assertCanonEqual({value, 2.25}, exclusive_percentile_rounded(0.45, Col)),
    ?assertCanonEqual({value, 2.5}, exclusive_percentile_rounded(0.50, Col)),
    ?assertCanonEqual({value, 2.75}, exclusive_percentile_rounded(0.55, Col)),
    ?assertCanonEqual({value, 3}, exclusive_percentile_rounded(0.60, Col)),
    ?assertCanonEqual({value, 3.25}, exclusive_percentile_rounded(0.65, Col)),
    ?assertCanonEqual({value, 3.5}, exclusive_percentile_rounded(0.70, Col)),
    ?assertCanonEqual({value, 3.75}, exclusive_percentile_rounded(0.75, Col)),
    ?assertCanonEqual({value, 4}, exclusive_percentile_rounded(0.80, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.85, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.90, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.95, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(1.00, Col)),

    test_valid_percentile_exclusive(Size, RefElements, Col),

    % Nearest rank

    ?assertCanonEqual(none, xb5_bag:percentile(0.00, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.05, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.1, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.15, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.2, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.25, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.3, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.35, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.4, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.45, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.5, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.55, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.6, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.65, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.7, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.75, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.8, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.85, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.9, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.95, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(1, Col, [{method, nearest_rank}])),

    test_valid_percentile_nearest_rank(Size, RefElements, Col),

    ok.

%%%%%%%%%%%%%%%

test_percentile_hardcoded2(_Config) ->
    Col = xb5_bag:from_list([1, 2, 3, 4, 5]),
    Size = xb5_bag:size(Col),
    RefElements = xb5_bag:to_list(Col),

    % Inclusive

    ?assertCanonEqual({value, 1}, inclusive_percentile_rounded(0.00, Col)),
    ?assertCanonEqual({value, 1.2}, inclusive_percentile_rounded(0.05, Col)),
    ?assertCanonEqual({value, 1.4}, inclusive_percentile_rounded(0.10, Col)),
    ?assertCanonEqual({value, 1.6}, inclusive_percentile_rounded(0.15, Col)),
    ?assertCanonEqual({value, 1.8}, inclusive_percentile_rounded(0.20, Col)),
    ?assertCanonEqual({value, 2}, inclusive_percentile_rounded(0.25, Col)),
    ?assertCanonEqual({value, 2.2}, inclusive_percentile_rounded(0.30, Col)),
    ?assertCanonEqual({value, 2.4}, inclusive_percentile_rounded(0.35, Col)),
    ?assertCanonEqual({value, 2.6}, inclusive_percentile_rounded(0.40, Col)),
    ?assertCanonEqual({value, 2.8}, inclusive_percentile_rounded(0.45, Col)),
    ?assertCanonEqual({value, 3}, inclusive_percentile_rounded(0.50, Col)),
    ?assertCanonEqual({value, 3.2}, inclusive_percentile_rounded(0.55, Col)),
    ?assertCanonEqual({value, 3.4}, inclusive_percentile_rounded(0.60, Col)),
    ?assertCanonEqual({value, 3.6}, inclusive_percentile_rounded(0.65, Col)),
    ?assertCanonEqual({value, 3.8}, inclusive_percentile_rounded(0.70, Col)),
    ?assertCanonEqual({value, 4}, inclusive_percentile_rounded(0.75, Col)),
    ?assertCanonEqual({value, 4.2}, inclusive_percentile_rounded(0.80, Col)),
    ?assertCanonEqual({value, 4.4}, inclusive_percentile_rounded(0.85, Col)),
    ?assertCanonEqual({value, 4.6}, inclusive_percentile_rounded(0.90, Col)),
    ?assertCanonEqual({value, 4.8}, inclusive_percentile_rounded(0.95, Col)),
    ?assertCanonEqual({value, 5}, inclusive_percentile_rounded(1.00, Col)),

    test_valid_percentile_inclusive(Size, RefElements, Col),

    % Exclusive

    ?assertCanonEqual(none, exclusive_percentile_rounded(0.00, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.05, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.10, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.15, Col)),
    ?assertCanonEqual({value, 1.2}, exclusive_percentile_rounded(0.20, Col)),
    ?assertCanonEqual({value, 1.5}, exclusive_percentile_rounded(0.25, Col)),
    ?assertCanonEqual({value, 1.8}, exclusive_percentile_rounded(0.30, Col)),
    ?assertCanonEqual({value, 2.1}, exclusive_percentile_rounded(0.35, Col)),
    ?assertCanonEqual({value, 2.4}, exclusive_percentile_rounded(0.40, Col)),
    ?assertCanonEqual({value, 2.7}, exclusive_percentile_rounded(0.45, Col)),
    ?assertCanonEqual({value, 3}, exclusive_percentile_rounded(0.50, Col)),
    ?assertCanonEqual({value, 3.3}, exclusive_percentile_rounded(0.55, Col)),
    ?assertCanonEqual({value, 3.6}, exclusive_percentile_rounded(0.60, Col)),
    ?assertCanonEqual({value, 3.9}, exclusive_percentile_rounded(0.65, Col)),
    ?assertCanonEqual({value, 4.2}, exclusive_percentile_rounded(0.70, Col)),
    ?assertCanonEqual({value, 4.5}, exclusive_percentile_rounded(0.75, Col)),
    ?assertCanonEqual({value, 4.8}, exclusive_percentile_rounded(0.80, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.85, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.90, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.95, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(1.00, Col)),

    test_valid_percentile_exclusive(Size, RefElements, Col),

    % Nearest rank

    ?assertCanonEqual(none, xb5_bag:percentile(0.00, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.05, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.1, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.15, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.2, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.25, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.3, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.35, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.4, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.45, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.5, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.55, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.6, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.65, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.7, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.75, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.8, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, xb5_bag:percentile(0.85, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, xb5_bag:percentile(0.9, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, xb5_bag:percentile(0.95, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, xb5_bag:percentile(1, Col, [{method, nearest_rank}])),

    test_valid_percentile_nearest_rank(Size, RefElements, Col).

%%%%%%%%%%%%%%%

test_percentile_hardcoded3(_Config) ->
    Col = xb5_bag:from_list([1, 2, 3, 4, 5, 6]),
    Size = xb5_bag:size(Col),
    RefElements = xb5_bag:to_list(Col),

    % Inclusive

    ?assertCanonEqual({value, 1}, inclusive_percentile_rounded(0.00, Col)),
    ?assertCanonEqual({value, 1.25}, inclusive_percentile_rounded(0.05, Col)),
    ?assertCanonEqual({value, 1.5}, inclusive_percentile_rounded(0.10, Col)),
    ?assertCanonEqual({value, 1.75}, inclusive_percentile_rounded(0.15, Col)),
    ?assertCanonEqual({value, 2}, inclusive_percentile_rounded(0.20, Col)),
    ?assertCanonEqual({value, 2.25}, inclusive_percentile_rounded(0.25, Col)),
    ?assertCanonEqual({value, 2.5}, inclusive_percentile_rounded(0.30, Col)),
    ?assertCanonEqual({value, 2.75}, inclusive_percentile_rounded(0.35, Col)),
    ?assertCanonEqual({value, 3}, inclusive_percentile_rounded(0.40, Col)),
    ?assertCanonEqual({value, 3.25}, inclusive_percentile_rounded(0.45, Col)),
    ?assertCanonEqual({value, 3.5}, inclusive_percentile_rounded(0.50, Col)),
    ?assertCanonEqual({value, 3.75}, inclusive_percentile_rounded(0.55, Col)),
    ?assertCanonEqual({value, 4}, inclusive_percentile_rounded(0.60, Col)),
    ?assertCanonEqual({value, 4.25}, inclusive_percentile_rounded(0.65, Col)),
    ?assertCanonEqual({value, 4.5}, inclusive_percentile_rounded(0.70, Col)),
    ?assertCanonEqual({value, 4.75}, inclusive_percentile_rounded(0.75, Col)),
    ?assertCanonEqual({value, 5}, inclusive_percentile_rounded(0.80, Col)),
    ?assertCanonEqual({value, 5.25}, inclusive_percentile_rounded(0.85, Col)),
    ?assertCanonEqual({value, 5.5}, inclusive_percentile_rounded(0.90, Col)),
    ?assertCanonEqual({value, 5.75}, inclusive_percentile_rounded(0.95, Col)),
    ?assertCanonEqual({value, 6}, inclusive_percentile_rounded(1.00, Col)),

    test_valid_percentile_inclusive(Size, RefElements, Col),

    % Exclusive

    ?assertCanonEqual(none, exclusive_percentile_rounded(0.00, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.05, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.10, Col)),
    ?assertCanonEqual({value, 1.05}, exclusive_percentile_rounded(0.15, Col)),
    ?assertCanonEqual({value, 1.4}, exclusive_percentile_rounded(0.20, Col)),
    ?assertCanonEqual({value, 1.75}, exclusive_percentile_rounded(0.25, Col)),
    ?assertCanonEqual({value, 2.1}, exclusive_percentile_rounded(0.30, Col)),
    ?assertCanonEqual({value, 2.45}, exclusive_percentile_rounded(0.35, Col)),
    ?assertCanonEqual({value, 2.8}, exclusive_percentile_rounded(0.40, Col)),
    ?assertCanonEqual({value, 3.15}, exclusive_percentile_rounded(0.45, Col)),
    ?assertCanonEqual({value, 3.5}, exclusive_percentile_rounded(0.50, Col)),
    ?assertCanonEqual({value, 3.85}, exclusive_percentile_rounded(0.55, Col)),
    ?assertCanonEqual({value, 4.2}, exclusive_percentile_rounded(0.60, Col)),
    ?assertCanonEqual({value, 4.55}, exclusive_percentile_rounded(0.65, Col)),
    ?assertCanonEqual({value, 4.9}, exclusive_percentile_rounded(0.70, Col)),
    ?assertCanonEqual({value, 5.25}, exclusive_percentile_rounded(0.75, Col)),
    ?assertCanonEqual({value, 5.6}, exclusive_percentile_rounded(0.80, Col)),
    ?assertCanonEqual({value, 5.95}, exclusive_percentile_rounded(0.85, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.90, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(0.95, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(1.00, Col)),

    test_valid_percentile_exclusive(Size, RefElements, Col),

    % Nearest rank

    ?assertCanonEqual(none, xb5_bag:percentile(0.00, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.05, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.1, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.15, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.2, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.25, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.3, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.35, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.4, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.45, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.5, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.55, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.6, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.65, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, xb5_bag:percentile(0.7, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, xb5_bag:percentile(0.75, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, xb5_bag:percentile(0.8, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, xb5_bag:percentile(0.85, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, xb5_bag:percentile(0.9, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, xb5_bag:percentile(0.95, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, xb5_bag:percentile(1, Col, [{method, nearest_rank}])),

    test_valid_percentile_nearest_rank(Size, RefElements, Col).

%%%%%%%%%%%%%%%

test_percentile_hardcoded4(_Config) ->
    Col = xb5_bag:from_list([1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 9, 9, 9, 9, 9, 9]),
    Size = xb5_bag:size(Col),
    RefElements = xb5_bag:to_list(Col),

    % Inclusive

    ?assertCanonEqual({value, 1}, inclusive_percentile_rounded(0.00, Col)),
    ?assertCanonEqual({value, 1.1}, inclusive_percentile_rounded(0.05, Col)),
    ?assertCanonEqual({value, 2}, inclusive_percentile_rounded(0.10, Col)),
    ?assertCanonEqual({value, 2}, inclusive_percentile_rounded(0.15, Col)),
    ?assertCanonEqual({value, 2.4}, inclusive_percentile_rounded(0.20, Col)),
    ?assertCanonEqual({value, 3}, inclusive_percentile_rounded(0.25, Col)),
    ?assertCanonEqual({value, 3}, inclusive_percentile_rounded(0.30, Col)),
    ?assertCanonEqual({value, 3}, inclusive_percentile_rounded(0.35, Col)),
    ?assertCanonEqual({value, 3.8}, inclusive_percentile_rounded(0.40, Col)),
    ?assertCanonEqual({value, 4}, inclusive_percentile_rounded(0.45, Col)),
    ?assertCanonEqual({value, 4}, inclusive_percentile_rounded(0.50, Col)),
    ?assertCanonEqual({value, 5}, inclusive_percentile_rounded(0.55, Col)),
    ?assertCanonEqual({value, 5.2}, inclusive_percentile_rounded(0.60, Col)),
    ?assertCanonEqual({value, 6.3}, inclusive_percentile_rounded(0.65, Col)),
    ?assertCanonEqual({value, 7.4}, inclusive_percentile_rounded(0.70, Col)),
    ?assertCanonEqual({value, 8.5}, inclusive_percentile_rounded(0.75, Col)),
    ?assertCanonEqual({value, 9}, inclusive_percentile_rounded(0.80, Col)),
    ?assertCanonEqual({value, 9}, inclusive_percentile_rounded(0.85, Col)),
    ?assertCanonEqual({value, 9}, inclusive_percentile_rounded(0.90, Col)),
    ?assertCanonEqual({value, 9}, inclusive_percentile_rounded(0.95, Col)),
    ?assertCanonEqual({value, 9}, inclusive_percentile_rounded(1.00, Col)),

    test_valid_percentile_inclusive(Size, RefElements, Col),

    % Exclusive

    ?assertCanonEqual(none, exclusive_percentile_rounded(0.00, Col)),
    ?assertCanonEqual({value, 1}, exclusive_percentile_rounded(0.05, Col)),
    ?assertCanonEqual({value, 1.4}, exclusive_percentile_rounded(0.10, Col)),
    ?assertCanonEqual({value, 2}, exclusive_percentile_rounded(0.15, Col)),
    ?assertCanonEqual({value, 2}, exclusive_percentile_rounded(0.20, Col)),
    ?assertCanonEqual({value, 3}, exclusive_percentile_rounded(0.25, Col)),
    ?assertCanonEqual({value, 3}, exclusive_percentile_rounded(0.30, Col)),
    ?assertCanonEqual({value, 3}, exclusive_percentile_rounded(0.35, Col)),
    ?assertCanonEqual({value, 3.6}, exclusive_percentile_rounded(0.40, Col)),
    ?assertCanonEqual({value, 4}, exclusive_percentile_rounded(0.45, Col)),
    ?assertCanonEqual({value, 4}, exclusive_percentile_rounded(0.50, Col)),
    ?assertCanonEqual({value, 5}, exclusive_percentile_rounded(0.55, Col)),
    ?assertCanonEqual({value, 5.4}, exclusive_percentile_rounded(0.60, Col)),
    ?assertCanonEqual({value, 6.6}, exclusive_percentile_rounded(0.65, Col)),
    ?assertCanonEqual({value, 7.8}, exclusive_percentile_rounded(0.70, Col)),
    ?assertCanonEqual({value, 9}, exclusive_percentile_rounded(0.75, Col)),
    ?assertCanonEqual({value, 9}, exclusive_percentile_rounded(0.80, Col)),
    ?assertCanonEqual({value, 9}, exclusive_percentile_rounded(0.85, Col)),
    ?assertCanonEqual({value, 9}, exclusive_percentile_rounded(0.90, Col)),
    ?assertCanonEqual({value, 9}, exclusive_percentile_rounded(0.95, Col)),
    ?assertCanonEqual(none, exclusive_percentile_rounded(1.00, Col)),

    test_valid_percentile_exclusive(Size, RefElements, Col),

    % Nearest rank

    ?assertCanonEqual(none, xb5_bag:percentile(0.00, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, xb5_bag:percentile(0.05, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.1, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.15, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, xb5_bag:percentile(0.2, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.25, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.3, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, xb5_bag:percentile(0.35, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.4, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.45, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, xb5_bag:percentile(0.5, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, xb5_bag:percentile(0.55, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, xb5_bag:percentile(0.6, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, xb5_bag:percentile(0.65, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 8}, xb5_bag:percentile(0.7, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, xb5_bag:percentile(0.75, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, xb5_bag:percentile(0.8, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, xb5_bag:percentile(0.85, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, xb5_bag:percentile(0.9, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, xb5_bag:percentile(0.95, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, xb5_bag:percentile(1, Col, [{method, nearest_rank}])),

    test_valid_percentile_nearest_rank(Size, RefElements, Col).

%%%%%%%%%%%%%%%%

test_percentile_rank(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_percentile_rank(RefElements, Col)
        end
    ).

%%%%%%%%%%%%%%%%

test_percentile_rank_hardcoded(_Config) ->
    Col1 = xb5_bag:from_list([1, 2, 3, 4]),
    List1 = xb5_bag:to_list(Col1),

    ?assertCanonEqual(0.125, percentile_rank_rounded(1, Col1)),
    ?assertCanonEqual(0.375, percentile_rank_rounded(2, Col1)),
    ?assertCanonEqual(0.625, percentile_rank_rounded(3, Col1)),
    ?assertCanonEqual(0.875, percentile_rank_rounded(4, Col1)),

    run_percentile_rank(List1, Col1),

    %%%%%%%

    Col2 = xb5_bag:from_list([1, 2, 3, 4, 5]),
    List2 = xb5_bag:to_list(Col2),

    ?assertCanonEqual(0.1, percentile_rank_rounded(1, Col2)),
    ?assertCanonEqual(0.3, percentile_rank_rounded(2, Col2)),
    ?assertCanonEqual(0.5, percentile_rank_rounded(3, Col2)),
    ?assertCanonEqual(0.7, percentile_rank_rounded(4, Col2)),
    ?assertCanonEqual(0.9, percentile_rank_rounded(5, Col2)),

    run_percentile_rank(List2, Col2),

    %%%%%%

    Col3 = xb5_bag:from_list([1, 2, 3, 4, 5, 6]),
    List3 = xb5_bag:to_list(Col3),

    ?assertCanonEqual(0.0833333333, percentile_rank_rounded(1, Col3)),
    ?assertCanonEqual(0.25, percentile_rank_rounded(2, Col3)),
    ?assertCanonEqual(0.4166666667, percentile_rank_rounded(3, Col3)),
    ?assertCanonEqual(0.5833333333, percentile_rank_rounded(4, Col3)),
    ?assertCanonEqual(0.75, percentile_rank_rounded(5, Col3)),
    ?assertCanonEqual(0.9166666667, percentile_rank_rounded(6, Col3)),

    run_percentile_rank(List3, Col3),

    %%%%%%

    % From Wikipedia article example
    Col4 = xb5_bag:from_list([7, 5, 5, 4, 4, 3, 3, 3, 2, 1]),
    List4 = xb5_bag:to_list(Col4),

    ?assertCanonEqual(0.05, percentile_rank_rounded(1, Col4)),
    ?assertCanonEqual(0.15, percentile_rank_rounded(2, Col4)),
    ?assertCanonEqual(0.35, percentile_rank_rounded(3, Col4)),
    ?assertCanonEqual(0.60, percentile_rank_rounded(4, Col4)),
    ?assertCanonEqual(0.80, percentile_rank_rounded(5, Col4)),
    ?assertCanonEqual(0.90, percentile_rank_rounded(6, Col4)),
    ?assertCanonEqual(0.95, percentile_rank_rounded(7, Col4)),

    run_percentile_rank(List4, Col4),

    %%%%%%

    Col5 = xb5_bag:from_list([1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 9, 9, 9, 9, 9, 9]),
    List5 = xb5_bag:to_list(Col5),

    ?assertCanonEqual(0.0434782609, percentile_rank_rounded(1, Col5)),
    ?assertCanonEqual(0.152173913, percentile_rank_rounded(2, Col5)),
    ?assertCanonEqual(0.3043478261, percentile_rank_rounded(3, Col5)),
    ?assertCanonEqual(0.4565217391, percentile_rank_rounded(4, Col5)),
    ?assertCanonEqual(0.5652173913, percentile_rank_rounded(5, Col5)),
    ?assertCanonEqual(0.6304347826, percentile_rank_rounded(6, Col5)),
    ?assertCanonEqual(0.6739130435, percentile_rank_rounded(7, Col5)),
    ?assertCanonEqual(0.7173913043, percentile_rank_rounded(8, Col5)),
    ?assertCanonEqual(0.8695652174, percentile_rank_rounded(9, Col5)),

    run_percentile_rank(List5, Col5).

%% ------------------------------------------------------------------
%% Tests - Additional Functions
%% ------------------------------------------------------------------

test_filter(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            run_filter(Size, RefElements, Col)
        end
    ).

test_filtermap(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            run_filtermap(Size, RefElements, Col)
        end
    ).

test_fold(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_fold(RefElements, Col)
        end
    ).

test_map(_Config) ->
    foreach_test_collection(
        fun(_Size, RefElements, Col) ->
            run_map(RefElements, Col)
        end
    ).

test_merge(_Config) ->
    foreach_test_collection(
        fun(Size, RefElements, Col) ->
            run_merge(Size, RefElements, Col)
        end
    ).

test_rewrap(_Config) ->
    ?assertMatch({error, _}, xb5_bag:unwrap(xb5_sets:new())),
    ?assertMatch({error, _}, xb5_bag:unwrap(xb5_trees:new())),
    ?assertMatch({error, _}, xb5_bag:unwrap({xb5_bag, -1, xb5_bag_node:new()})),
    ?assertMatch({error, _}, xb5_bag:unwrap({xb5_bag, 2, xb5_bag_node:new()})),
    ?assertMatch({error, _}, xb5_bag:unwrap({xb5_bag, 2, make_ref()})),

    foreach_test_collection(
        fun(_Size, _RefElements, Col) ->
            {ok, Unwrapped} = xb5_bag:unwrap(Col),
            ?assertEqual(Col, xb5_bag:wrap(Unwrapped))
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
                        new_collection_from_each_added(RefElements);
                    2 ->
                        xb5_bag:from_list(RefElements);
                    3 ->
                        new_collection_from_each_added(lists:reverse(RefElements));
                    4 ->
                        xb5_bag:from_list(lists:reverse(RefElements))
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
                new_collection_from_each_added(list_shuffle(RefElements))
            end
        ),

    %%%%%%%%%%

    ?assertConfidentStat(height, 5.0002, CondensedStats),

    ?assertConfidentStat(avg_keys_per_node, 2.9143881267511302, CondensedStats).

%%%%

test_structure_build_seqIns2x_seqDelSmallerHalf(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefElements) ->
                % 1) build sequentially
                Col1 = new_collection_from_each_added(RefElements),

                % 2) delete smaller half sequentially
                AmountToDelete = xb5_bag:size(Col1) div 2,
                ItemsToDelete = lists:sublist(RefElements, AmountToDelete),
                lists:foldl(fun xb5_bag:delete/2, Col1, ItemsToDelete)
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
                Col1 = new_collection_from_each_added(RefElements),

                % 2) delete greater half sequentially
                AmountToDelete = xb5_bag:size(Col1) div 2,
                ItemsToDelete = lists:sublist(lists:reverse(RefElements), AmountToDelete),
                lists:foldl(fun xb5_bag:delete/2, Col1, ItemsToDelete)
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
                Col1 = new_collection_from_each_added(RefElements),

                % 2) delete half randomly
                AmountToDelete = xb5_bag:size(Col1) div 2,
                ItemsToDelete = lists:sublist(list_shuffle(RefElements), AmountToDelete),
                lists:foldl(fun xb5_bag:delete/2, Col1, ItemsToDelete)
            end,
            [{size_multiplier, 2}]
        ),

    %%%%%%%%%%

    ?assertConfidentStat(height, 5.748, CondensedStats),

    ?assertConfidentStat(avg_keys_per_node, 2.6014331611359016, CondensedStats).

%%%%

test_structure_build_randomlyIns2x_randomlyDelHalf(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefElements) ->
                % 1) build randomly
                Col1 = new_collection_from_each_added(list_shuffle(RefElements)),

                % 2) delete half randomly
                AmountToDelete = xb5_bag:size(Col1) div 2,
                ItemsToDelete = lists:sublist(list_shuffle(RefElements), AmountToDelete),
                lists:foldl(fun xb5_bag:delete/2, Col1, ItemsToDelete)
            end,
            [{size_multiplier, 2}]
        ),

    %%%%%%%%%%

    ?assertConfidentStat(height, 5.847, CondensedStats),

    ?assertConfidentStat(avg_keys_per_node, 2.59362583949796, CondensedStats).

%%%%

test_structure_build_randomlyIns2x_seqDelSmallerHalf(_Config) ->
    CondensedStats =
        run_structure_test(
            fun(RefElements) ->
                % 1) build randomly
                Col1 = new_collection_from_each_added(list_shuffle(RefElements)),

                % 2) delete smaller half sequentially
                AmountToDelete = xb5_bag:size(Col1) div 2,
                ItemsToDelete = lists:sublist(RefElements, AmountToDelete),
                lists:foldl(fun xb5_bag:delete/2, Col1, ItemsToDelete)
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
                Col1 = new_collection_from_each_added(RefElements),

                % 2) delete every 5th item sequentially
                ItemsToDelete =
                    lists:filtermap(
                        fun({Index, E}) ->
                            ((Index rem 5 =:= 0) andalso
                                {true, E})
                        end,
                        lists:enumerate(RefElements)
                    ),

                lists:foldl(fun xb5_bag:delete/2, Col1, ItemsToDelete)
            end,
            [{size_multiplier, 1.25}]
        ),

    %%%%%%%%%%

    ?assertPreciseStat(height, 5.00, CondensedStats),

    % NOTE: if we delete every _4th_ item, it's actually worse for `avg_keys_per_internal_node`.

    ?assertConfidentStat(avg_keys_per_node, 2.403846153846179, CondensedStats).

%% ------------------------------------------------------------------
%% Helper Functions: shared
%% ------------------------------------------------------------------

foreach_test_collection(Fun) ->
    foreach_test_collection(Fun, []).

foreach_test_collection(Fun, Opts) ->
    foreach_tested_size(
        fun(Size, RefElements) ->
            Col = xb5_bag:from_list(maybe_shuffle_elements_for_new_collection(RefElements)),

            Stats = xb5_bag:structural_stats(Col),
            ?assertEqual(Size, proplists:get_value(total_keys, Stats)),

            Fun(Size, RefElements, Col)
        end,
        Opts
    ).

maybe_shuffle_elements_for_new_collection(RefElements) ->
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

    lists:foreach(
        fun(Size) ->
            NrOfSubIterations =
                case Size =:= 0 of
                    true ->
                        1;
                    %
                    false ->
                        max(3, ceil(1000 / Size))
                end,

            lists:foreach(
                fun(_) ->
                    run_test_for_size(Size, NumericOnly, Fun)
                end,
                lists:seq(1, NrOfSubIterations)
            )
        end,
        ?TESTED_SIZES
    ).

run_test_for_size(Size, NumericOnly, Fun) ->
    RepetitionChance = 0.20,
    RefElements = new_ref_elements(Size, NumericOnly, RepetitionChance),
    Fun(Size, RefElements).

new_ref_elements(Size, NumericOnly, RepetitionChance) ->
    new_ref_elements_recur(Size, NumericOnly, RepetitionChance, _Acc = []).

new_ref_elements_recur(Size, NumericOnly, RepetitionChance, Acc) when Size > 0 ->
    case Acc =/= [] andalso rand:uniform() < RepetitionChance of
        true ->
            RepeatedElement = randomly_switch_number_type(list_pick_random(Acc)),
            UpdatedAcc = [RepeatedElement | Acc],
            new_ref_elements_recur(Size - 1, NumericOnly, RepetitionChance, UpdatedAcc);
        %
        false ->
            NewElement = new_element(NumericOnly),

            case lists:member(NewElement, Acc) of
                false ->
                    UpdatedAcc = [NewElement | Acc],
                    new_ref_elements_recur(Size - 1, NumericOnly, RepetitionChance, UpdatedAcc);
                %
                true ->
                    new_ref_elements_recur(Size, NumericOnly, RepetitionChance, Acc)
            end
    end;
new_ref_elements_recur(0, _, _, Acc) ->
    lists:sort(Acc).

list_pick_random([_ | _] = List) ->
    N = rand:uniform(length(List)),
    lists:nth(N, List).

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

new_collection_from_each_added(List) ->
    Col = xb5_bag:new(),

    ?assertEqual(0, xb5_bag:size(Col)),
    ?assertEqual(true, xb5_bag:is_empty(Col)),

    new_collection_from_each_added_recur(List, Col).

new_collection_from_each_added_recur([Element | Next], Col) ->
    UpdatedCol = xb5_bag:add(Element, Col),
    new_collection_from_each_added_recur(Next, UpdatedCol);
new_collection_from_each_added_recur([], Col) ->
    Col.

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

rank_in_sorted_list(Elem, [H | T]) ->
    if
        Elem > H ->
            1 + rank_in_sorted_list(Elem, T);
        %
        Elem == H ->
            1
    end.

-dialyzer({nowarn_function, fun1_error_not_to_be_called/0}).
fun1_error_not_to_be_called() ->
    fun(_) ->
        error(not_to_be_called)
    end.

%% ------------------------------------------------------------------
%% Helpers: deletion
%% ------------------------------------------------------------------

test_delete_non_existing_keys(Col, RemainingElements, Amount) when Amount > 0 ->
    Element = new_element(),

    case lists:any(fun(E) -> E == Element end, RemainingElements) of
        false ->
            ?assertError({badkey, Element}, xb5_bag:delete(Element, Col)),
            ?assertEqual(Col, xb5_bag:delete_any(Element, Col)),

            test_delete_non_existing_keys(Col, RemainingElements, Amount - 1);
        %
        true ->
            test_delete_non_existing_keys(Col, RemainingElements, Amount)
    end;
test_delete_non_existing_keys(_, _, 0) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers: smaller and larger
%% ------------------------------------------------------------------

run_smaller(RefElements, Col) ->
    case lists:usort(RefElements) of
        [] ->
            Element = new_element(),
            ?assertEqual(none, xb5_bag:smaller(Element, Col));
        %
        [SingleElement] ->
            ?assertEqual(none, xb5_bag:smaller(randomly_switch_number_type(SingleElement), Col)),

            LargerElement = element_larger(SingleElement),
            ?assert(xb5_bag:smaller(LargerElement, Col) == {found, SingleElement}),

            SmallerElement = element_smaller(SingleElement),
            ?assertEqual(none, xb5_bag:smaller(SmallerElement, Col));
        %
        [FirstElement | Next] ->
            ?assertEqual(none, xb5_bag:smaller(randomly_switch_number_type(FirstElement), Col)),

            SmallerElement = element_smaller(FirstElement),
            ?assertEqual(none, xb5_bag:smaller(SmallerElement, Col)),

            run_smaller_recur(FirstElement, Next, Col)
    end.

run_smaller_recur(Expected, [LastElement], Col) ->
    ?assertCanonEqual(
        {found, Expected},
        xb5_bag:smaller(randomly_switch_number_type(LastElement), Col)
    ),

    LargerElement = element_larger(LastElement),
    ?assert(LargerElement > LastElement),
    ?assert(xb5_bag:smaller(LargerElement, Col) == {found, LastElement});
run_smaller_recur(Expected, [Element | Next], Col) ->
    ?assert(xb5_bag:smaller(randomly_switch_number_type(Element), Col) == {found, Expected}),

    case element_in_between(Expected, Element) of
        {found, InBetween} ->
            ?assert(InBetween > Expected),
            ?assert(InBetween < Element),
            ?assert(xb5_bag:smaller(InBetween, Col) == {found, Expected});
        %
        none ->
            ok
    end,

    run_smaller_recur(Element, Next, Col).

%%%%%%%%%%%%%%%%%

run_larger(RefElements, Col) ->
    case lists:reverse(lists:usort(RefElements)) of
        [] ->
            Element = new_element(),
            ?assertEqual(none, xb5_bag:larger(Element, Col));
        %
        [SingleElement] ->
            ?assertEqual(none, xb5_bag:larger(SingleElement, Col)),

            LargerElement = element_larger(SingleElement),
            ?assertEqual(none, xb5_bag:larger(LargerElement, Col)),

            SmallerElement = element_smaller(SingleElement),
            ?assert(xb5_bag:larger(SmallerElement, Col) == {found, SingleElement});
        %
        [LastElement | Next] ->
            ?assertEqual(none, xb5_bag:larger(randomly_switch_number_type(LastElement), Col)),

            LargerElement = element_larger(LastElement),
            ?assertEqual(none, xb5_bag:larger(LargerElement, Col)),

            run_larger_recur(LastElement, Next, Col)
    end.

run_larger_recur(Expected, [FirstElement], Col) ->
    ?assertCanonEqual(
        {found, Expected},
        xb5_bag:larger(randomly_switch_number_type(FirstElement), Col)
    ),

    SmallerElement = element_smaller(FirstElement),
    ?assert(SmallerElement < FirstElement),
    ?assert(xb5_bag:larger(SmallerElement, Col) == {found, FirstElement});
run_larger_recur(Expected, [Element | Next], Col) ->
    ?assert(xb5_bag:larger(randomly_switch_number_type(Element), Col) == {found, Expected}),

    case element_in_between(Element, Expected) of
        {found, InBetween} ->
            ?assert(InBetween < Expected),
            ?assert(InBetween > Element),
            ?assert(xb5_bag:larger(InBetween, Col) == {found, Expected});
        %
        none ->
            ok
    end,

    run_larger_recur(Element, Next, Col).

%%%%%%%%%%%%%%%%%

run_take_smallest([Expected | Next], Col) ->
    {Taken, Col2} = xb5_bag:take_smallest(Col),
    ?assert(Taken == Expected),
    ?assertEqual(length(Next), xb5_bag:size(Col2)),
    run_take_smallest(Next, Col2);
run_take_smallest([], Col) ->
    ?assertError(empty_items, xb5_bag:take_smallest(Col)).

run_take_largest([Expected | Next], Col) ->
    {Taken, Col2} = xb5_bag:take_largest(Col),
    ?assert(Taken == Expected),
    ?assertEqual(length(Next), xb5_bag:size(Col2)),
    run_take_largest(Next, Col2);
run_take_largest([], Col) ->
    ?assertError(empty_items, xb5_bag:take_largest(Col)).

%% ------------------------------------------------------------------
%% Helpers: iterators
%% ------------------------------------------------------------------

run_iterator_from(RefElements, Col) ->
    case RefElements of
        [] ->
            Iter = new_iterator_from(new_element(), Col),
            ?assertEqual([], iterate(Iter));
        %
        [SingleElement] ->
            Iter = new_iterator_from(randomly_switch_number_type(SingleElement), Col),
            ?assertListsCanonEqual(RefElements, iterate(Iter)),

            SmallerElement = element_smaller(SingleElement),
            Iter2 = new_iterator_from(SmallerElement, Col),
            ?assertListsCanonEqual(RefElements, iterate(Iter2)),

            LargerElement = element_larger(SingleElement),
            Iter3 = new_iterator_from(LargerElement, Col),
            ?assertEqual([], iterate(Iter3));
        %
        [FirstElement | _] ->
            SmallerElement = element_smaller(FirstElement),
            Iter = new_iterator_from(SmallerElement, Col),
            ?assertListsCanonEqual(RefElements, iterate(Iter)),

            run_iterator_from_recur(RefElements, Col)
    end.

run_iterator_from_recur([LastElement], Col) ->
    run_iterator_from_last_elements([LastElement], Col);
run_iterator_from_recur([Elem1 | [Elem2 | _] = Tail] = List, Col) ->
    Iter = new_iterator_from(Elem1, Col),
    ?assertListsCanonEqual(List, iterate(Iter)),

    case element_in_between(Elem1, Elem2) of
        {found, InBetween} ->
            ?assert(InBetween > Elem1),
            ?assert(InBetween < Elem2),
            Iter2 = new_iterator_from(InBetween, Col),
            ?assertListsCanonEqual(Tail, iterate(Iter2));
        %
        none ->
            ok
    end,

    case lists:dropwhile(fun(E) -> E == Elem1 end, Tail) of
        [] ->
            % Last element case
            run_iterator_from_last_elements(List, Col);
        %
        Next ->
            run_iterator_from_recur(Next, Col)
    end.

run_iterator_from_last_elements([Elem | _] = LastElements, Col) ->
    Iter = new_iterator_from(randomly_switch_number_type(Elem), Col),
    ?assertListsCanonEqual(LastElements, iterate(Iter)),

    LargerElement = element_larger(Elem),
    Iter2 = new_iterator_from(LargerElement, Col),
    ?assertEqual([], iterate(Iter2)).

new_iterator_from(Elem, Col) ->
    Iter = xb5_bag:iterator_from(Elem, Col),
    ?assertEqual(Iter, xb5_bag:iterator_from(Elem, Col, ordered)),
    Iter.

%%%%%%%%%%%%%%%%%

run_iterator_from_reversed(RefElements, Col) ->
    case lists:reverse(RefElements) of
        [] ->
            Iter = xb5_bag:iterator_from(new_element(), Col, reversed),
            ?assertEqual([], iterate(Iter));
        %
        [SingleElement] ->
            Iter = xb5_bag:iterator_from(
                randomly_switch_number_type(SingleElement), Col, reversed
            ),
            ?assertListsCanonEqual([SingleElement], iterate(Iter)),

            SmallerElement = element_smaller(SingleElement),
            Iter2 = xb5_bag:iterator_from(SmallerElement, Col, reversed),
            ?assertEqual([], iterate(Iter2)),

            LargerElement = element_larger(SingleElement),
            Iter3 = xb5_bag:iterator_from(LargerElement, Col, reversed),
            ?assertListsCanonEqual([SingleElement], iterate(Iter3));
        %
        [LastElement | _] = ReverseRefElements ->
            LargerElement = element_larger(LastElement),
            Iter = xb5_bag:iterator_from(LargerElement, Col, reversed),
            ?assertListsCanonEqual(ReverseRefElements, iterate(Iter)),

            run_iterator_from_reversed_recur(ReverseRefElements, Col)
    end.

run_iterator_from_reversed_recur([FirstElement], Col) ->
    run_iterator_from_reversed_first_elements([FirstElement], Col);
run_iterator_from_reversed_recur([Elem2 | [Elem1 | _] = Tail] = List, Col) ->
    Iter = xb5_bag:iterator_from(Elem2, Col, reversed),
    ?assertListsCanonEqual(List, iterate(Iter)),

    case element_in_between(Elem1, Elem2) of
        {found, InBetween} ->
            ?assert(InBetween > Elem1),
            ?assert(InBetween < Elem2),
            Iter2 = xb5_bag:iterator_from(InBetween, Col, reversed),
            ?assertListsCanonEqual(Tail, iterate(Iter2));
        %
        none ->
            ok
    end,

    case lists:dropwhile(fun(E) -> E == Elem2 end, Tail) of
        [] ->
            % Last element case
            run_iterator_from_reversed_first_elements(List, Col);
        %
        Next ->
            run_iterator_from_reversed_recur(Next, Col)
    end.

run_iterator_from_reversed_first_elements([Elem | _] = FirstElements, Col) ->
    Iter = xb5_bag:iterator_from(randomly_switch_number_type(Elem), Col, reversed),
    ?assertListsCanonEqual(FirstElements, iterate(Iter)),

    SmallerElement = element_smaller(Elem),
    Iter2 = xb5_bag:iterator_from(SmallerElement, Col, reversed),
    ?assertEqual([], iterate(Iter2)).

%%%%%%%%%%%%%%%%%

new_iterator(Col) ->
    Iter = xb5_bag:iterator(Col),
    ?assertEqual(Iter, xb5_bag:iterator(Col, ordered)),
    Iter.

iterate(Iter) ->
    case xb5_bag:next(Iter) of
        {Element, Iter2} ->
            [Element | iterate(Iter2)];
        %
        none ->
            []
    end.

%% ------------------------------------------------------------------
%% Helpers: nth
%% ------------------------------------------------------------------

test_invalid_nth(Size, Col) ->
    lists:foreach(
        fun(_) ->
            ?assertError({badarg, not_numerical}, xb5_bag:nth(?OPAQUE_TERM(not_numerical), Col)),

            BelowN = -(rand:uniform(100) - 1),
            BelowFloat = float(BelowN),
            ?assertError({badarg, BelowN}, xb5_bag:nth(BelowN, Col)),
            ?assertError({badarg, BelowFloat}, xb5_bag:nth(?OPAQUE_TERM(BelowFloat), Col)),

            BeyondN = Size + rand:uniform(100),
            BeyondFloat = float(BeyondN),
            ?assertError({badarg, BeyondN}, xb5_bag:nth(BeyondN, Col)),
            ?assertError({badarg, BeyondFloat}, xb5_bag:nth(?OPAQUE_TERM(BeyondFloat), Col)),

            (Size > 1 andalso
                begin
                    InBetween = rand:uniform(Size - 1) + 0.5,
                    ?assertError({badarg, InBetween}, xb5_bag:nth(?OPAQUE_TERM(InBetween), Col))
                end),

            (Size =/= 0 andalso
                begin
                    ValidButFloat = float(rand:uniform(Size)),
                    ?assertError(
                        {badarg, ValidButFloat}, xb5_bag:nth(?OPAQUE_TERM(ValidButFloat), Col)
                    )
                end)
        end,
        lists:seq(1, 20)
    ).

test_valid_nth(RefElements, Col) ->
    test_valid_nth_recur(1, RefElements, Col).

test_valid_nth_recur(N, [RefElement | Next], Col) ->
    ?assert(xb5_bag:nth(N, Col) == RefElement),
    test_valid_nth_recur(N + 1, Next, Col);
test_valid_nth_recur(_, [], _) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers: rank smaller
%% ------------------------------------------------------------------

run_rank_smaller(RefElements, Col) ->
    case RefElements of
        [] ->
            Element = new_element(),
            ?assertEqual(none, xb5_bag:rank_smaller(Element, Col));
        %
        [FirstElement | Next] ->
            ?assertEqual(
                none, xb5_bag:rank_smaller(randomly_switch_number_type(FirstElement), Col)
            ),

            run_rank_smaller_recur(1, FirstElement, Next, Col)
    end.

run_rank_smaller_recur(Pos1, Elem1, Tail, Col) ->
    case lists:dropwhile(fun(E) -> E == Elem1 end, Tail) of
        [Elem2 | Next] ->
            RepeatedCount = length(Tail) - (length(Next) + 1),
            Pos2 = Pos1 + RepeatedCount + 1,

            {SmallerPos, SmallerElem} =
                Smaller = xb5_bag:rank_smaller(randomly_switch_number_type(Elem2), Col),
            ?assertEqual(Pos1 + RepeatedCount, SmallerPos),
            ?assertCanonEqual(Elem1, SmallerElem),

            case element_in_between(Elem1, Elem2) of
                {found, InBetween} ->
                    ?assertEqual(Smaller, xb5_bag:rank_smaller(InBetween, Col));
                %
                none ->
                    ok
            end,

            run_rank_smaller_recur(Pos2, Elem2, Next, Col);
        %
        [] ->
            LastPos = xb5_bag:size(Col),

            LargerElem = element_larger(Elem1),
            {SmallerPos, SmallerElem} = xb5_bag:rank_smaller(LargerElem, Col),
            ?assertEqual(LastPos, SmallerPos),
            ?assertCanonEqual(Elem1, SmallerElem)
    end.

%% ------------------------------------------------------------------
%% Helpers: rank larger
%% ------------------------------------------------------------------

run_rank_larger(Size, RefElements, Col) ->
    case lists:reverse(RefElements) of
        [] ->
            Element = new_element(),
            ?assertEqual(none, xb5_bag:rank_larger(Element, Col));
        %
        [LastElement | Next] ->
            ?assertEqual(
                none, xb5_bag:rank_larger(randomly_switch_number_type(LastElement), Col)
            ),

            run_rank_larger_recur(Size, LastElement, Next, Col)
    end.

run_rank_larger_recur(Pos2, Elem2, Tail, Col) ->
    case lists:dropwhile(fun(E) -> E == Elem2 end, Tail) of
        [Elem1 | Next] ->
            RepeatedCount = length(Tail) - (length(Next) + 1),
            Pos1 = Pos2 - RepeatedCount - 1,

            {LargerPos, LargerElem} =
                Larger = xb5_bag:rank_larger(randomly_switch_number_type(Elem1), Col),
            ?assertEqual(Pos1 + 1, LargerPos),
            ?assertCanonEqual(Elem2, LargerElem),

            case element_in_between(Elem1, Elem2) of
                {found, InBetween} ->
                    ?assertEqual(Larger, xb5_bag:rank_larger(InBetween, Col));
                %
                none ->
                    ok
            end,

            run_rank_larger_recur(Pos1, Elem1, Next, Col);
        %
        [] ->
            SmallerElem = element_smaller(Elem2),
            {LargerPos, LargerElem} = xb5_bag:rank_larger(SmallerElem, Col),
            ?assertEqual(1, LargerPos),
            ?assertCanonEqual(Elem2, LargerElem)
    end.

%% ------------------------------------------------------------------
%% Helpers: Percentile Bracket - Inclusive
%% ------------------------------------------------------------------

test_invalid_percentile_inclusive(Col) ->
    ?assertError({badarg, -1}, new_percentile_bracket_inclusive(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_bracket_inclusive(2, Col)),
    ?assertError({badarg, 100}, new_percentile_bracket_inclusive(100, Col)),
    ?assertError({badarg, 50}, new_percentile_bracket_inclusive(50, Col)),
    ?assertError(
        {badarg, not_numerical}, new_percentile_bracket_inclusive(?OPAQUE_TERM(not_numerical), Col)
    ),

    ?assertError({badarg, -1}, new_percentile_inclusive(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_inclusive(2, Col)),
    ?assertError({badarg, 100}, new_percentile_inclusive(100, Col)),
    ?assertError({badarg, 50}, new_percentile_inclusive(50, Col)),
    ?assertError(
        {badarg, not_numerical}, new_percentile_inclusive(?OPAQUE_TERM(not_numerical), Col)
    ).

test_valid_percentile_inclusive(0 = Size, _, Col) ->
    foreach_percentile(
        fun(Percentile, _, _) ->
            ?assertEqual(none, new_percentile_bracket_inclusive(Percentile, Col)),
            ?assertEqual(none, new_percentile_inclusive(Percentile, Col))
        end,
        Size,
        inclusive
    );
test_valid_percentile_inclusive(Size, RefElements, Col) ->
    foreach_percentile(
        fun
            (Percentile, LowRank, HighRank) when LowRank == HighRank ->
                ExactElem = lists:nth(LowRank, RefElements),
                ?assertCanonEqual(
                    {exact, ExactElem}, new_percentile_bracket_inclusive(Percentile, Col)
                ),
                ?assertCanonEqual({value, ExactElem}, new_percentile_inclusive(Percentile, Col));
            %
            (Percentile, LowRank, HighRank) ->
                case lists:sublist(RefElements, LowRank, 2) of
                    [LowElem, HighElem] when LowElem == HighElem ->
                        ?assertCanonEqual(
                            {exact, LowElem}, new_percentile_bracket_inclusive(Percentile, Col)
                        ),
                        ?assertCanonEqual(
                            {value, LowElem}, new_percentile_inclusive(Percentile, Col)
                        );
                    %
                    [LowElem, HighElem] ->
                        LowPerc = inclusive_percentile_bracket_perc(LowRank, Size),
                        HighPerc = inclusive_percentile_bracket_perc(HighRank, Size),

                        PercRange = HighPerc - LowPerc,
                        HighWeight = (Percentile - LowPerc) / PercRange,
                        LowWeight = 1.0 - HighWeight,

                        LowBound = #{
                            percentile => LowPerc,
                            weight => LowWeight,
                            value => LowElem
                        },

                        HighBound = #{
                            percentile => HighPerc,
                            weight => HighWeight,
                            value => HighElem
                        },

                        ?assertCanonEqual(
                            {between, LowBound, HighBound},
                            new_percentile_bracket_inclusive(Percentile, Col)
                        ),

                        %%

                        if
                            not is_number(LowElem) ->
                                ?assertCanonError(
                                    {bracket_value_not_a_number, LowBound},
                                    new_percentile_inclusive(Percentile, Col)
                                );
                            %
                            not is_number(HighElem) ->
                                ?assertCanonError(
                                    {bracket_value_not_a_number, HighBound},
                                    new_percentile_inclusive(Percentile, Col)
                                );
                            %
                            true ->
                                ?assertCanonEqual(
                                    {value, (LowWeight * LowElem) + (HighWeight * HighElem)},
                                    new_percentile_inclusive(Percentile, Col)
                                )
                        end
                end
        end,
        Size,
        inclusive
    ).

inclusive_percentile_bracket_perc(Rank, Size) ->
    (Rank - 1) / (Size - 1).

new_percentile_bracket_inclusive(Percentile, Col) ->
    Bracket = new_percentile_bracket_inclusive_do(Percentile, Col),

    if
        Percentile =:= +0.0 orelse Percentile =:= 1.0 ->
            ?assertEqual(Bracket, new_percentile_bracket_inclusive_do(trunc(Percentile), Col)),
            Bracket;
        %
        is_integer(Percentile) ->
            ?assertEqual(Bracket, new_percentile_bracket_inclusive_do(float(Percentile), Col)),
            Bracket;
        %
        true ->
            Bracket
    end.

new_percentile_bracket_inclusive_do(Percentile, Col) ->
    try xb5_bag:percentile_bracket(Percentile, Col) of
        Bracket ->
            ?assertEqual(
                Bracket, xb5_bag:percentile_bracket(Percentile, Col, [{method, inclusive}])
            ),
            ?assertEqual(
                Bracket,
                xb5_bag:percentile_bracket(Percentile, Col, [
                    {method, inclusive}, {method, exclusive}
                ])
            ),
            Bracket
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class, Reason, xb5_bag:percentile_bracket(Percentile, Col, [{method, inclusive}])
            ),
            ?assertException(
                Class,
                Reason,
                xb5_bag:percentile_bracket(Percentile, Col, [
                    {method, inclusive}, {method, exclusive}
                ])
            ),
            erlang:raise(Class, Reason, Stacktrace)
    end.

new_percentile_inclusive(Percentile, Col) ->
    Result = new_percentile_inclusive_do(Percentile, Col),

    if
        Percentile =:= +0.0 orelse Percentile =:= 1.0 ->
            ?assertEqual(Result, new_percentile_inclusive_do(trunc(Percentile), Col)),
            Result;
        %
        is_integer(Percentile) ->
            ?assertEqual(Result, new_percentile_inclusive_do(float(Percentile), Col)),
            Result;
        %
        true ->
            Result
    end.

new_percentile_inclusive_do(Percentile, Col) ->
    try xb5_bag:percentile(Percentile, Col) of
        Result ->
            ?assertEqual(Result, xb5_bag:percentile(Percentile, Col, [{method, inclusive}])),
            ?assertEqual(
                Result,
                xb5_bag:percentile(Percentile, Col, [{method, inclusive}, {method, exclusive}])
            ),
            Result
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class, Reason, xb5_bag:percentile(Percentile, Col, [{method, inclusive}])
            ),
            ?assertException(
                Class,
                Reason,
                xb5_bag:percentile(Percentile, Col, [{method, inclusive}, {method, exclusive}])
            ),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% ------------------------------------------------------------------
%% Helpers: Percentile - Exclusive
%% ------------------------------------------------------------------

test_invalid_percentile_exclusive(Col) ->
    ?assertError({badarg, -1}, new_percentile_bracket_exclusive(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_bracket_exclusive(2, Col)),
    ?assertError({badarg, 100}, new_percentile_bracket_exclusive(100, Col)),
    ?assertError({badarg, 50}, new_percentile_bracket_exclusive(50, Col)),
    ?assertError(
        {badarg, not_numerical}, new_percentile_bracket_exclusive(?OPAQUE_TERM(not_numerical), Col)
    ),

    ?assertError({badarg, -1}, new_percentile_exclusive(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_exclusive(2, Col)),
    ?assertError({badarg, 100}, new_percentile_exclusive(100, Col)),
    ?assertError({badarg, 50}, new_percentile_exclusive(50, Col)),
    ?assertError(
        {badarg, not_numerical}, new_percentile_exclusive(?OPAQUE_TERM(not_numerical), Col)
    ).

test_valid_percentile_exclusive(0 = Size, _, Col) ->
    foreach_percentile(
        fun(Percentile, _, _) ->
            ?assertCanonEqual(none, new_percentile_bracket_exclusive(Percentile, Col)),
            ?assertEqual(none, new_percentile_exclusive(Percentile, Col))
        end,
        Size,
        exclusive
    );
test_valid_percentile_exclusive(Size, RefElements, Col) ->
    foreach_percentile(
        fun
            (Percentile, LowRank, HighRank) when LowRank < 1 orelse HighRank > Size ->
                ?assertCanonEqual(none, new_percentile_bracket_exclusive(Percentile, Col)),
                ?assertCanonEqual(none, new_percentile_exclusive(Percentile, Col));
            %
            (Percentile, LowRank, HighRank) when LowRank == HighRank ->
                ExactElem = lists:nth(LowRank, RefElements),
                ?assertCanonEqual(
                    {exact, ExactElem}, new_percentile_bracket_exclusive(Percentile, Col)
                ),
                ?assertCanonEqual({value, ExactElem}, new_percentile_exclusive(Percentile, Col));
            %
            (Percentile, LowRank, HighRank) ->
                case lists:sublist(RefElements, LowRank, 2) of
                    [LowElem, HighElem] when LowElem == HighElem ->
                        ?assertCanonEqual(
                            {exact, LowElem}, new_percentile_bracket_exclusive(Percentile, Col)
                        ),
                        ?assertCanonEqual(
                            {value, LowElem}, new_percentile_exclusive(Percentile, Col)
                        );
                    %
                    [LowElem, HighElem] ->
                        LowPerc = exclusive_percentile_bracket_perc(LowRank, Size),
                        HighPerc = exclusive_percentile_bracket_perc(HighRank, Size),

                        PercRange = HighPerc - LowPerc,
                        HighWeight = (Percentile - LowPerc) / PercRange,
                        LowWeight = 1.0 - HighWeight,

                        LowBound = #{
                            percentile => LowPerc,
                            weight => LowWeight,
                            value => LowElem
                        },

                        HighBound = #{
                            percentile => HighPerc,
                            weight => HighWeight,
                            value => HighElem
                        },

                        ?assertCanonEqual(
                            {between, LowBound, HighBound},
                            new_percentile_bracket_exclusive(Percentile, Col)
                        ),

                        %%

                        if
                            not is_number(LowElem) ->
                                ?assertCanonError(
                                    {bracket_value_not_a_number, LowBound},
                                    new_percentile_exclusive(Percentile, Col)
                                );
                            %
                            not is_number(HighElem) ->
                                ?assertCanonError(
                                    {bracket_value_not_a_number, HighBound},
                                    new_percentile_exclusive(Percentile, Col)
                                );
                            %
                            true ->
                                ?assertCanonEqual(
                                    {value, (LowWeight * LowElem) + (HighWeight * HighElem)},
                                    new_percentile_exclusive(Percentile, Col)
                                )
                        end
                end
        end,
        Size,
        exclusive
    ).

exclusive_percentile_bracket_perc(Rank, Size) ->
    Rank / (Size + 1).

new_percentile_bracket_exclusive(Percentile, Col) ->
    Bracket = new_percentile_bracket_exclusive_do(Percentile, Col),

    if
        Percentile =:= +0.0 orelse Percentile =:= 1.0 ->
            ?assertEqual(Bracket, new_percentile_bracket_exclusive_do(trunc(Percentile), Col)),
            Bracket;
        %
        is_integer(Percentile) ->
            ?assertEqual(Bracket, new_percentile_bracket_exclusive_do(float(Percentile), Col)),
            Bracket;
        %
        true ->
            Bracket
    end.

new_percentile_bracket_exclusive_do(Percentile, Col) ->
    try xb5_bag:percentile_bracket(Percentile, Col, [{method, exclusive}]) of
        Bracket ->
            ?assertEqual(
                Bracket,
                xb5_bag:percentile_bracket(Percentile, Col, [
                    {method, exclusive}, {method, inclusive}
                ])
            ),
            Bracket
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class,
                Reason,
                xb5_bag:percentile_bracket(Percentile, Col, [
                    {method, exclusive}, {method, inclusive}
                ])
            ),
            erlang:raise(Class, Reason, Stacktrace)
    end.

new_percentile_exclusive(Percentile, Col) ->
    Result = new_percentile_exclusive_do(Percentile, Col),

    if
        Percentile =:= +0.0 orelse Percentile =:= 1.0 ->
            ?assertEqual(Result, new_percentile_exclusive_do(trunc(Percentile), Col)),
            Result;
        %
        is_integer(Percentile) ->
            ?assertEqual(Result, new_percentile_exclusive_do(float(Percentile), Col)),
            Result;
        %
        true ->
            Result
    end.

new_percentile_exclusive_do(Percentile, Col) ->
    try xb5_bag:percentile(Percentile, Col, [{method, exclusive}]) of
        Result ->
            ?assertEqual(
                Result,
                xb5_bag:percentile(Percentile, Col, [{method, exclusive}, {method, inclusive}])
            ),
            Result
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class,
                Reason,
                xb5_bag:percentile(Percentile, Col, [{method, exclusive}, {method, inclusive}])
            ),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% ------------------------------------------------------------------
%% Helpers: Percentile - Nearest Rank
%% ------------------------------------------------------------------

test_invalid_percentile_nearest_rank(Col) ->
    ?assertError({badarg, -1}, new_percentile_bracket_nearest_rank(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_bracket_nearest_rank(2, Col)),
    ?assertError({badarg, 100}, new_percentile_bracket_nearest_rank(100, Col)),
    ?assertError({badarg, 50}, new_percentile_bracket_nearest_rank(50, Col)),
    ?assertError(
        {badarg, not_numerical},
        new_percentile_bracket_nearest_rank(?OPAQUE_TERM(not_numerical), Col)
    ),

    ?assertError({badarg, -1}, new_percentile_nearest_rank(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_nearest_rank(2, Col)),
    ?assertError({badarg, 100}, new_percentile_nearest_rank(100, Col)),
    ?assertError({badarg, 50}, new_percentile_nearest_rank(50, Col)),
    ?assertError(
        {badarg, not_numerical}, new_percentile_nearest_rank(?OPAQUE_TERM(not_numerical), Col)
    ).

test_valid_percentile_nearest_rank(0 = Size, _, Col) ->
    foreach_percentile(
        fun(Percentile, _, _) ->
            ?assertEqual(none, new_percentile_bracket_nearest_rank(Percentile, Col)),
            ?assertEqual(none, new_percentile_nearest_rank(Percentile, Col))
        end,
        Size,
        nearest_rank
    );
test_valid_percentile_nearest_rank(Size, RefElements, Col) ->
    foreach_percentile(
        fun
            (Percentile, _, 0) ->
                ?assertCanonEqual(0, Percentile),
                ?assertEqual(none, new_percentile_bracket_nearest_rank(Percentile, Col)),
                ?assertEqual(none, new_percentile_nearest_rank(Percentile, Col));
            %
            (Percentile, _, ExactRank) ->
                ExactElem = lists:nth(ExactRank, RefElements),
                ?assertCanonEqual(
                    {exact, ExactElem}, new_percentile_bracket_nearest_rank(Percentile, Col)
                ),
                ?assertCanonEqual({value, ExactElem}, new_percentile_nearest_rank(Percentile, Col))
        end,
        Size,
        nearest_rank
    ).

new_percentile_bracket_nearest_rank(Percentile, Col) ->
    Bracket = new_percentile_bracket_nearest_rank_do(Percentile, Col),

    if
        Percentile =:= +0.0 orelse Percentile =:= 1.0 ->
            ?assertEqual(Bracket, new_percentile_bracket_nearest_rank_do(trunc(Percentile), Col)),
            Bracket;
        %
        is_integer(Percentile) ->
            ?assertEqual(Bracket, new_percentile_bracket_nearest_rank_do(float(Percentile), Col)),
            Bracket;
        %
        true ->
            Bracket
    end.

new_percentile_bracket_nearest_rank_do(Percentile, Col) ->
    try xb5_bag:percentile_bracket(Percentile, Col, [{method, nearest_rank}]) of
        Bracket ->
            ?assertEqual(
                Bracket,
                xb5_bag:percentile_bracket(Percentile, Col, [
                    {method, nearest_rank}, {method, inclusive}
                ])
            ),
            Bracket
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class,
                Reason,
                xb5_bag:percentile_bracket(Percentile, Col, [
                    {method, nearest_rank}, {method, inclusive}
                ])
            ),
            erlang:raise(Class, Reason, Stacktrace)
    end.

new_percentile_nearest_rank(Percentile, Col) ->
    Result = new_percentile_nearest_rank_do(Percentile, Col),

    if
        Percentile =:= +0.0 orelse Percentile =:= 1.0 ->
            ?assertEqual(Result, new_percentile_nearest_rank_do(trunc(Percentile), Col)),
            Result;
        %
        is_integer(Percentile) ->
            ?assertEqual(Result, new_percentile_nearest_rank_do(float(Percentile), Col)),
            Result;
        %
        true ->
            Result
    end.

new_percentile_nearest_rank_do(Percentile, Col) ->
    try xb5_bag:percentile(Percentile, Col, [{method, nearest_rank}]) of
        Result ->
            ?assertEqual(
                Result,
                xb5_bag:percentile(Percentile, Col, [{method, nearest_rank}, {method, inclusive}])
            ),
            Result
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class,
                Reason,
                xb5_bag:percentile(Percentile, Col, [{method, nearest_rank}, {method, inclusive}])
            ),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%%%%%%%%%%

foreach_percentile(Fun, Size, Method) ->
    CacheKey = {?MODULE, foreach_percentile, cached_percentiles, Size},

    Percentiles =
        case get(CacheKey) of
            List when is_list(List) ->
                List;
            %
            undefined ->
                ForExactPositions = [Pos / Size || Pos <- lists:seq(1, Size)],

                MaxGenericSlice = 100,
                ForGenericSlices = [
                    SliceNr / MaxGenericSlice
                 || SliceNr <- lists:seq(0, MaxGenericSlice)
                ],

                List = lists:usort(ForGenericSlices ++ ForExactPositions),
                put(CacheKey, List),
                List
        end,

    lists:foreach(
        fun(Percentile) ->
            Pos = percentile_bracket_pos(Percentile, Size, Method),
            LowRank = floor(Pos),
            HighRank = ceil(Pos),
            Fun(Percentile, LowRank, HighRank)
        end,
        Percentiles
    ).

percentile_bracket_pos(Percentile, Size, inclusive) ->
    1 + (Size - 1) * Percentile;
percentile_bracket_pos(Percentile, Size, exclusive) ->
    (Size + 1) * Percentile;
percentile_bracket_pos(Percentile, Size, nearest_rank) ->
    ceil(Percentile * Size).

%%%%

inclusive_percentile_rounded(Percentile, Col) ->
    percentile_rounded(Percentile, Col, []).

exclusive_percentile_rounded(Percentile, Col) ->
    percentile_rounded(Percentile, Col, [{method, exclusive}]).

percentile_rounded(Percentile, Col, Opts) ->
    case xb5_bag:percentile(Percentile, Col, Opts) of
        {value, Value} = Res when is_integer(Value) ->
            Res;
        %
        {value, Value} when is_float(Value) ->
            {value, round_float_precision(Value)};
        %
        none ->
            none
    end.

round_float_precision(Value) ->
    Decimals = 10,
    Factor = math:pow(10, Decimals),
    round(Value * Factor) / Factor.

%%%%

percentile_rank_rounded(Elem, Col) ->
    round_float_precision(xb5_bag:percentile_rank(Elem, Col)).

%% ------------------------------------------------------------------
%% Helpers: Percentile Rank
%% ------------------------------------------------------------------

run_percentile_rank(RefElements, Col) ->
    case RefElements of
        [] ->
            ?assertError(empty_items, xb5_bag:percentile_rank(foobar, Col));
        %
        [SingleElement] ->
            ?assertEqual(0.5, xb5_bag:percentile_rank(SingleElement, Col)),
            ?assertEqual(
                0.5, xb5_bag:percentile_rank(randomly_switch_number_type(SingleElement), Col)
            ),

            LargerElement = element_larger(SingleElement),
            ?assertEqual(1.0, xb5_bag:percentile_rank(LargerElement, Col)),

            SmallerElement = element_smaller(SingleElement),
            ?assertEqual(0.0, xb5_bag:percentile_rank(SmallerElement, Col));
        %
        [FirstElement | Next] ->
            SmallerElement = element_smaller(FirstElement),
            ?assertEqual(0.0, xb5_bag:percentile_rank(SmallerElement, Col)),

            run_percentile_rank_recur(FirstElement, Next, 0, length(RefElements), Col)
    end.

run_percentile_rank_recur(Elem, Next, CF, Size, Col) ->
    CountRepeated = lists_count_while(fun(E) -> E == Elem end, Next),
    F = 1 + CountRepeated,
    UpdatedCF = CF + F,

    ElemRank = (UpdatedCF - (0.5 * F)) / Size,
    ?assertEqual(ElemRank, xb5_bag:percentile_rank(Elem, Col)),

    case lists:sublist(Next, CountRepeated + 1, length(Next) - CountRepeated) of
        [NextElem | NextNext] ->
            case element_in_between(Elem, NextElem) of
                {found, InBetween} ->
                    InBetweenRank = UpdatedCF / Size,
                    ?assertEqual(InBetweenRank, xb5_bag:percentile_rank(InBetween, Col));
                none ->
                    ok
            end,

            run_percentile_rank_recur(NextElem, NextNext, UpdatedCF, Size, Col);
        %
        [] ->
            LargerElem = element_larger(Elem),
            LargerRank = 1.0,
            ?assertEqual(LargerRank, xb5_bag:percentile_rank(LargerElem, Col))
    end.

lists_count_while(Fun, [H | T]) ->
    case Fun(H) of
        true ->
            1 + lists_count_while(Fun, T);
        %
        false ->
            0
    end;
lists_count_while(_, []) ->
    0.

%% ------------------------------------------------------------------
%% Helper Functions: filter
%% ------------------------------------------------------------------

run_filter(Size, RefElements, Col) ->
    RoughAmountsToRemove =
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
        fun(RoughAmountToRemove) ->
            FilterFun =
                case Size of
                    0 ->
                        fun1_error_not_to_be_called();
                    %
                    _ ->
                        ElementsToRemove = lists:sublist(
                            list_shuffle(RefElements), RoughAmountToRemove
                        ),
                        AuxSet = gb_sets:from_list(ElementsToRemove),
                        fun(E) -> not gb_sets:is_element(E, AuxSet) end
                end,

            FilteredCol = xb5_bag:filter(FilterFun, Col),

            ExpectedElementsRemaining = lists:filter(FilterFun, RefElements),
            ?assertListsCanonEqual(ExpectedElementsRemaining, xb5_bag:to_list(FilteredCol)),

            ?assertEqual(length(ExpectedElementsRemaining), xb5_bag:size(FilteredCol))
        end,
        RoughAmountsToRemove
    ).

%% ------------------------------------------------------------------
%% Helper Functions: filtermap
%% ------------------------------------------------------------------

run_filtermap(Size, RefElements, Col) ->
    RoughAmountsToKeep =
        case Size of
            0 ->
                [0];
            %
            _ ->
                lists:usort([
                    0, 1, Size, Size - 1, rand:uniform(Size), rand:uniform(Size), rand:uniform(Size)
                ])
        end,

    RoughPercentagesToMap =
        case Size of
            0 ->
                [0.0];
            %
            _ ->
                [0.0, 0.2, 0.5, 0.8, 1.0]
        end,

    ParamCombos = [
        {RoughAmountToKeep, RoughPercentageToMap}
     || RoughAmountToKeep <- RoughAmountsToKeep,
        RoughPercentageToMap <- RoughPercentagesToMap
    ],

    %%%%

    lists:foreach(
        fun({RoughAmountToKeep, RoughPercentageToMap}) ->
            RoughAmountToMap = floor(RoughPercentageToMap * RoughAmountToKeep),

            FiltermapFun =
                case Size of
                    0 ->
                        fun1_error_not_to_be_called();
                    %
                    _ ->
                        ElementsToKeep = lists:sublist(
                            list_shuffle(RefElements), RoughAmountToKeep
                        ),
                        ElementsToMap = lists:sublist(
                            list_shuffle(ElementsToKeep), RoughAmountToMap
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

            FiltermappedCol = xb5_bag:filtermap(FiltermapFun, Col),

            ExpectedElementsRemaining = lists:sort(lists:filtermap(FiltermapFun, RefElements)),
            ?assertListsCanonEqual(ExpectedElementsRemaining, xb5_bag:to_list(FiltermappedCol)),

            ?assertEqual(length(ExpectedElementsRemaining), xb5_bag:size(FiltermappedCol))
        end,
        ParamCombos
    ).

%% ------------------------------------------------------------------
%% Helper Functions: fold
%% ------------------------------------------------------------------

run_fold(RefElements, Col) ->
    Tag = make_ref(),

    Fun =
        fun(E, Acc) ->
            case Acc of
                [{_, PrevE} | _] ->
                    ?assert(PrevE =< E);
                [] ->
                    ok
            end,

            [{Tag, E} | Acc]
        end,

    ?assertListsCanonEqual(
        lists:foldl(Fun, [], RefElements),
        xb5_bag:fold(Fun, [], Col)
    ).

%% ------------------------------------------------------------------
%% Helper Functions: map
%% ------------------------------------------------------------------

run_map(RefElements, Col) ->
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

            MappedCol = xb5_bag:map(MapFun, Col),

            ExpectedMappedRef = lists:sort(lists:map(MapFun, RefElements)),

            ?assertEqual(
                length(ExpectedMappedRef),
                xb5_bag:size(MappedCol)
            ),

            ?assertListsCanonEqual(
                ExpectedMappedRef,
                xb5_bag:to_list(MappedCol)
            )
        end,
        PercentagesMapped
    ).

%% ------------------------------------------------------------------
%% Helper Functions: merge
%% ------------------------------------------------------------------

run_merge(Size, RefElements, Col) ->
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

    %%%%

    lists:foreach(
        fun({Amount2, PercentageInCommon}) ->
            RepeatedAmount = floor(PercentageInCommon * min(Amount2, Size)),
            NewAmount = Amount2 - RepeatedAmount,

            RepeatedElements = lists:sublist(list_shuffle(RefElements), RepeatedAmount),
            NewElements = [new_element() || _ <- lists:seq(1, NewAmount)],

            RefElements2 = lists:sort(RepeatedElements ++ NewElements),

            Col2 = xb5_bag:from_list(maybe_shuffle_elements_for_new_collection(RefElements2)),

            MergedCol = xb5_bag:merge(Col, Col2),

            %%%

            ExpectedMergedRef = lists:sort(RefElements ++ RefElements2),

            ?assertEqual(
                length(ExpectedMergedRef),
                xb5_bag:size(MergedCol)
            ),

            ?assertListsCanonEqual(
                ExpectedMergedRef,
                xb5_bag:to_list(MergedCol)
            )
        end,
        ParamCombos
    ).

%% ------------------------------------------------------------------
%% Helpers: Structurel Tests
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
                RepetitionChance = 0.0,
                % faster with numeric only
                NumericOnly = true,
                RefElements = new_ref_elements(Size, NumericOnly, RepetitionChance),

                Col = InitFun(RefElements),
                ?assertEqual(?STRUCTURE_TEST_BASE_SIZE, xb5_bag:size(Col)),

                Stats = xb5_bag:structural_stats(Col),

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
        _ ->
            % Ensured to be smaller
            -100
    end.

element_larger(Element) ->
    case element_type(Element) of
        binary ->
            <<Element/bytes, Element/bytes>>;
        %
        _ ->
            <<"ensured to be larger">>
    end.

element_type(Element) when is_number(Element) ->
    number;
element_type(Element) when is_binary(Element) ->
    binary;
element_type(Element) when is_tuple(Element) ->
    tuple;
element_type(Element) when is_list(Element) ->
    list;
element_type(Element) when is_map(Element) ->
    map;
element_type(Element) when is_reference(Element) ->
    reference.
