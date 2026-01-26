-module(b5_items_test_SUITE).

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
    test_delete/1
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

%% Test constants
-define(TESTED_SIZES,
    (lists:seq(0, 50) ++ lists:seq(55, 200, 5) ++ [997])
).

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
            test_delete
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
            test_percentile_inclusive,
            test_percentile_exclusive,
            test_percentile_nearest_rank,
            test_percentile_hardcoded1,
            test_percentile_hardcoded2,
            test_percentile_hardcoded3,
            test_percentile_hardcoded4,
            test_percentile_rank,
            test_percentile_rank_hardcoded
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
        fun(Size, RefElements, RefUqElements) ->
            Col = b5_items:from_list(RefElements),
            ?assertListsCanonEqual(RefElements, b5_items:to_list(Col)),
            ?assertEqual(Size, b5_items:size(Col)),
            ?assertEqual(Size =:= 0, b5_items:is_empty(Col)),
            ?assertEqual(Col, new_collection_from_each_added(RefElements)),
            ?assertEqual(Col, b5_items:from_list(RefElements, [])),
            ?assertEqual(Col, b5_items:from_list(RefElements, [{unique, false}])),
            ?assertEqual(Col, b5_items:from_list(RefElements, [{unique, false}, unique])),
            ?assertEqual(Col, b5_items:from_ordset(RefElements)),
            ?assertEqual(Col, b5_items:from_ordset(RefElements, [])),
            ?assertEqual(Col, b5_items:from_ordset(RefElements, [{unique, false}])),
            ?assertEqual(Col, b5_items:from_ordset(RefElements, [{unique, false}, unique])),

            UqCol = b5_items:from_list(RefElements, [unique]),
            ?assertListsCanonEqual(RefUqElements, b5_items:to_list(UqCol)),
            ?assertEqual(length(RefUqElements), b5_items:size(UqCol)),
            ?assertEqual(RefUqElements =:= [], b5_items:is_empty(Col)),
            ?assertEqual(UqCol, new_collection_from_each_added(RefElements, [unique])),
            ?assertEqual(UqCol, b5_items:from_list(RefElements, [{unique, true}])),
            ?assertEqual(UqCol, b5_items:from_list(RefElements, [{unique, true}, {unique, false}])),
            ?assertEqual(UqCol, b5_items:from_list(RefElements, [unique, {unique, false}])),
            ?assertEqual(UqCol, b5_items:from_ordset(RefElements, [{unique, true}])),
            ?assertEqual(
                UqCol, b5_items:from_ordset(RefElements, [{unique, true}, {unique, false}])
            ),
            ?assertEqual(UqCol, b5_items:from_ordset(RefElements, [unique, {unique, false}]))
        end
    ).

test_lookup(_Config) ->
    foreach_test_set(
        fun(Size, _RefElements, Col, RefUqElements, UqCol) ->
            foreach_existing_element(
                fun(Element) ->
                    ?assertEqual(true, b5_items:is_member(Element, Col)),
                    ?assertEqual(true, b5_items:is_member(Element, UqCol))
                end,
                RefUqElements,
                Size
            ),

            %%%%%%%%%%%

            foreach_non_existent_element(
                fun(Element) ->
                    ?assertEqual(false, b5_items:is_member(Element, Col)),
                    ?assertEqual(false, b5_items:is_member(Element, UqCol))
                end,
                RefUqElements,
                100
            )
        end
    ).

test_add(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Col, RefUqElements, UqCol) ->
            foreach_existing_element(
                fun(Element) ->
                    Col2 = b5_items:add(Element, Col),
                    ?assertEqual(Size + 1, b5_items:size(Col2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefElements),
                        b5_items:to_list(Col2)
                    ),

                    %%%%%%%%%%%%

                    UqCol2 = b5_items:add(randomly_switch_number_type(Element), UqCol),
                    ?assertEqual(length(RefUqElements), b5_items:size(UqCol2)),
                    ?assertEqual(UqCol, UqCol2)
                end,
                RefUqElements,
                min(50, Size)
            ),

            %%%%%%%%%%%%%%%%%%

            foreach_non_existent_element(
                fun(Element) ->
                    Col2 = b5_items:add(Element, Col),
                    ?assertEqual(Size + 1, b5_items:size(Col2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefElements),
                        b5_items:to_list(Col2)
                    ),

                    UqCol2 = b5_items:add(Element, UqCol),
                    ?assertEqual(length(RefUqElements) + 1, b5_items:size(UqCol2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefUqElements),
                        b5_items:to_list(UqCol2)
                    )
                end,
                RefUqElements,
                50
            )
        end
    ).

test_insert(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Col, RefUqElements, UqCol) ->
            foreach_existing_element(
                fun(Element) ->
                    ?assertError({key_exists, Element}, b5_items:insert(Element, Col)),
                    ?assertError({key_exists, Element}, b5_items:insert(Element, UqCol))
                end,
                RefUqElements,
                min(50, Size)
            ),

            %%%%%%%%%%%%%%%%%%

            foreach_non_existent_element(
                fun(Element) ->
                    Col2 = b5_items:insert(Element, Col),
                    ?assertEqual(Size + 1, b5_items:size(Col2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefElements),
                        b5_items:to_list(Col2)
                    ),

                    UqCol2 = b5_items:insert(Element, UqCol),
                    ?assertEqual(length(RefUqElements) + 1, b5_items:size(UqCol2)),
                    ?assertListsCanonEqual(
                        add_to_sorted_list(Element, RefUqElements),
                        b5_items:to_list(UqCol2)
                    )
                end,
                RefUqElements,
                50
            )
        end
    ).

test_delete(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Col, RefUqElements, UqCol) ->
            DeleteKeys = lists:map(fun randomly_switch_number_type/1, list_shuffle(RefElements)),

            {ColN, []} =
                lists:foldl(
                    fun(Element, {Col1, RemainingElements1}) ->
                        test_delete_non_existing_keys(Col1, RemainingElements1, 3),

                        Col2 = b5_items:delete(Element, Col1),
                        RemainingElements2 = remove_from_sorted_list(Element, RemainingElements1),
                        ?assertListsCanonEqual(RemainingElements2, b5_items:to_list(Col2)),
                        ?assertEqual(length(RemainingElements2), b5_items:size(Col2)),
                        ?assertEqual(RemainingElements2 =:= [], b5_items:is_empty(Col2)),

                        ?assertEqual(Col2, b5_items:delete_any(Element, Col1)),

                        {Col2, RemainingElements2}
                    end,
                    {Col, lists:sort(DeleteKeys)},
                    DeleteKeys
                ),

            ?assertEqual([], b5_items:to_list(ColN)),
            ?assertEqual(0, b5_items:size(ColN)),
            ?assertEqual(true, b5_items:is_empty(ColN)),

            test_delete_non_existing_keys(ColN, [], 3),

            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

            UqDeleteKeys = lists:map(
                fun randomly_switch_number_type/1, list_shuffle(RefUqElements)
            ),

            {UqColN, []} =
                lists:foldl(
                    fun(Element, {UqCol1, RemainingElements1}) ->
                        test_delete_non_existing_keys(UqCol1, RemainingElements1, 3),

                        UqCol2 = b5_items:delete(Element, UqCol1),
                        RemainingElements2 = remove_from_sorted_list(Element, RemainingElements1),
                        ?assertListsCanonEqual(RemainingElements2, b5_items:to_list(UqCol2)),
                        ?assertEqual(length(RemainingElements2), b5_items:size(UqCol2)),
                        ?assertEqual(RemainingElements2 =:= [], b5_items:is_empty(UqCol2)),

                        ?assertEqual(UqCol2, b5_items:delete_any(Element, UqCol1)),

                        {UqCol2, RemainingElements2}
                    end,
                    {UqCol, lists:sort(UqDeleteKeys)},
                    UqDeleteKeys
                ),

            ?assertEqual([], b5_items:to_list(UqColN)),
            ?assertEqual(0, b5_items:size(UqColN)),
            ?assertEqual(true, b5_items:is_empty(UqColN)),

            test_delete_non_existing_keys(UqColN, [], 3)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Smaller and Larger
%% ------------------------------------------------------------------

test_smallest(_Config) ->
    foreach_test_set(
        fun
            (0, _RefElements, Col, _RefUqElements, UqCol) ->
                ?assertError(empty_items, b5_items:smallest(Col)),
                ?assertError(empty_items, b5_items:smallest(UqCol));
            %
            (_Size, RefElements, Col, RefUqElements, UqCol) ->
                ?assert(b5_items:smallest(Col) == hd(RefElements)),
                ?assert(b5_items:smallest(UqCol) == hd(RefUqElements))
        end
    ).

test_largest(_Config) ->
    foreach_test_set(
        fun
            (0, _RefElements, Col, _RefUqElements, UqCol) ->
                ?assertError(empty_items, b5_items:largest(Col)),
                ?assertError(empty_items, b5_items:largest(UqCol));
            %
            (_Size, RefElements, Col, RefUqElements, UqCol) ->
                ?assert(b5_items:largest(Col) == lists:last(RefElements)),
                ?assert(b5_items:largest(UqCol) == lists:last(RefUqElements))
        end
    ).

test_smaller(_Config) ->
    foreach_test_set(
        fun(_Size, _RefElements, Col, RefUqElements, UqCol) ->
            run_smaller(RefUqElements, Col),
            run_smaller(RefUqElements, UqCol)
        end
    ).

test_larger(_Config) ->
    foreach_test_set(
        fun(_Size, _RefElements, Col, RefUqElements, UqCol) ->
            run_larger(RefUqElements, Col),
            run_larger(RefUqElements, UqCol)
        end
    ).

test_take_smallest(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Col, RefUqElements, UqCol) ->
            run_take_smallest(RefElements, Col),
            run_take_smallest(RefUqElements, UqCol)
        end
    ).

test_take_largest(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Col, RefUqElements, UqCol) ->
            run_take_largest(lists:reverse(RefElements), Col),
            run_take_largest(lists:reverse(RefUqElements), UqCol)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Iterators
%% ------------------------------------------------------------------

test_iterator(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Col, RefUqElements, UqCol) ->
            Iter = new_iterator(Col),
            ?assertListsCanonEqual(RefElements, iterate(Iter)),

            UqIter = new_iterator(UqCol),
            ?assertListsCanonEqual(RefUqElements, iterate(UqIter))
        end
    ).

test_iterator_reversed(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Col, RefUqElements, UqCol) ->
            Iter = b5_items:iterator(Col, reversed),
            ?assertListsCanonEqual(lists:reverse(RefElements), iterate(Iter)),

            UqIter = b5_items:iterator(UqCol, reversed),
            ?assertListsCanonEqual(lists:reverse(RefUqElements), iterate(UqIter))
        end
    ).

test_iterator_from(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Col, RefUqElements, UqCol) ->
            run_iterator_from(RefElements, Col),
            run_iterator_from(RefUqElements, UqCol)
        end
    ).

test_iterator_from_reversed(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Col, RefUqElements, UqCol) ->
            run_iterator_from_reversed(RefElements, Col),
            run_iterator_from_reversed(RefUqElements, UqCol)
        end
    ).

%% ------------------------------------------------------------------
%% Tests - Order Statistics
%% ------------------------------------------------------------------

test_nth(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Col, RefUqElements, UqCol) ->
            test_invalid_nth(Size, Col),
            test_invalid_nth(Size, UqCol),

            _ = (Size =/= 0 andalso test_valid_nth(RefElements, Col)),
            _ = (Size =/= 0 andalso test_valid_nth(RefUqElements, UqCol))
        end
    ).

%%%%%%%%

test_percentile_inclusive(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Col, RefUqElements, UqCol) ->
            test_invalid_percentile_inclusive(Col),
            test_invalid_percentile_inclusive(UqCol),

            test_valid_percentile_inclusive(Size, RefElements, Col),
            test_valid_percentile_inclusive(length(RefUqElements), RefUqElements, UqCol)
        end
    ),

    %%%%%%%%%%

    foreach_test_set(
        fun(Size, RefElements, Col, RefUqElements, UqCol) ->
            test_invalid_percentile_inclusive(Col),
            test_invalid_percentile_inclusive(UqCol),

            test_valid_percentile_inclusive(Size, RefElements, Col),
            test_valid_percentile_inclusive(length(RefUqElements), RefUqElements, UqCol)
        end,
        [numeric_only]
    ).

%%%%%%%%

test_percentile_exclusive(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Col, RefUqElements, UqCol) ->
            test_invalid_percentile_exclusive(Col),
            test_invalid_percentile_exclusive(UqCol),

            test_valid_percentile_exclusive(Size, RefElements, Col),
            test_valid_percentile_exclusive(length(RefUqElements), RefUqElements, UqCol)
        end
    ),

    %%%%%%%%%

    foreach_test_set(
        fun(Size, RefElements, Col, RefUqElements, UqCol) ->
            test_invalid_percentile_exclusive(Col),
            test_invalid_percentile_exclusive(UqCol),

            test_valid_percentile_exclusive(Size, RefElements, Col),
            test_valid_percentile_exclusive(length(RefUqElements), RefUqElements, UqCol)
        end,
        [numeric_only]
    ).

%%%%%%%%%%%%

test_percentile_nearest_rank(_Config) ->
    foreach_test_set(
        fun(Size, RefElements, Col, RefUqElements, UqCol) ->
            test_invalid_percentile_nearest_rank(Col),
            test_invalid_percentile_nearest_rank(UqCol),

            test_valid_percentile_nearest_rank(Size, RefElements, Col),
            test_valid_percentile_nearest_rank(length(RefUqElements), RefUqElements, UqCol)
        end
    ),

    %%%%%%%%%%%%

    foreach_test_set(
        fun(Size, RefElements, Col, RefUqElements, UqCol) ->
            test_invalid_percentile_nearest_rank(Col),
            test_invalid_percentile_nearest_rank(UqCol),

            _ = (Size =/= 0 andalso test_valid_percentile_nearest_rank(Size, RefElements, Col)),
            _ =
                (Size =/= 0 andalso
                    test_valid_percentile_nearest_rank(length(RefUqElements), RefUqElements, UqCol))
        end,
        [numeric_only]
    ).

%%%%%%%%

test_percentile_hardcoded1(_Config) ->
    Col = b5_items:from_list([1, 2, 3, 4]),
    Size = b5_items:size(Col),
    RefElements = b5_items:to_list(Col),

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

    ?assertCanonEqual(none, b5_items:percentile(0.00, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.05, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.1, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.15, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.2, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.25, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.3, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.35, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.4, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.45, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.5, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.55, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.6, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.65, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.7, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.75, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.8, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.85, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.9, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.95, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(1, Col, [{method, nearest_rank}])),

    test_valid_percentile_nearest_rank(Size, RefElements, Col),

    ok.

%%%%%%%%%%%%%%%

test_percentile_hardcoded2(_Config) ->
    Col = b5_items:from_list([1, 2, 3, 4, 5]),
    Size = b5_items:size(Col),
    RefElements = b5_items:to_list(Col),

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

    ?assertCanonEqual(none, b5_items:percentile(0.00, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.05, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.1, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.15, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.2, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.25, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.3, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.35, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.4, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.45, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.5, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.55, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.6, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.65, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.7, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.75, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.8, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, b5_items:percentile(0.85, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, b5_items:percentile(0.9, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, b5_items:percentile(0.95, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, b5_items:percentile(1, Col, [{method, nearest_rank}])),

    test_valid_percentile_nearest_rank(Size, RefElements, Col).

%%%%%%%%%%%%%%%

test_percentile_hardcoded3(_Config) ->
    Col = b5_items:from_list([1, 2, 3, 4, 5, 6]),
    Size = b5_items:size(Col),
    RefElements = b5_items:to_list(Col),

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

    ?assertCanonEqual(none, b5_items:percentile(0.00, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.05, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.1, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.15, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.2, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.25, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.3, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.35, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.4, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.45, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.5, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.55, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.6, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.65, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, b5_items:percentile(0.7, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, b5_items:percentile(0.75, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, b5_items:percentile(0.8, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, b5_items:percentile(0.85, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, b5_items:percentile(0.9, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, b5_items:percentile(0.95, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, b5_items:percentile(1, Col, [{method, nearest_rank}])),

    test_valid_percentile_nearest_rank(Size, RefElements, Col).

%%%%%%%%%%%%%%%

test_percentile_hardcoded4(_Config) ->
    Col = b5_items:from_list([1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 9, 9, 9, 9, 9, 9]),
    Size = b5_items:size(Col),
    RefElements = b5_items:to_list(Col),

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

    ?assertCanonEqual(none, b5_items:percentile(0.00, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 1}, b5_items:percentile(0.05, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.1, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.15, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 2}, b5_items:percentile(0.2, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.25, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.3, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 3}, b5_items:percentile(0.35, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.4, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.45, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 4}, b5_items:percentile(0.5, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, b5_items:percentile(0.55, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 5}, b5_items:percentile(0.6, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 6}, b5_items:percentile(0.65, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 8}, b5_items:percentile(0.7, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, b5_items:percentile(0.75, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, b5_items:percentile(0.8, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, b5_items:percentile(0.85, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, b5_items:percentile(0.9, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, b5_items:percentile(0.95, Col, [{method, nearest_rank}])),
    ?assertCanonEqual({value, 9}, b5_items:percentile(1, Col, [{method, nearest_rank}])),

    test_valid_percentile_nearest_rank(Size, RefElements, Col).

%%%%%%%%%%%%%%%%

test_percentile_rank(_Config) ->
    foreach_test_set(
        fun(_Size, RefElements, Col, RefUqElements, UqCol) ->
            run_percentile_rank(RefElements, Col),
            run_percentile_rank(RefUqElements, UqCol)
        end
    ).

%%%%%%%%%%%%%%%%

test_percentile_rank_hardcoded(_Config) ->
    Col1 = b5_items:from_list([1, 2, 3, 4]),
    List1 = b5_items:to_list(Col1),

    ?assertCanonEqual(0.125, percentile_rank_rounded(1, Col1)),
    ?assertCanonEqual(0.375, percentile_rank_rounded(2, Col1)),
    ?assertCanonEqual(0.625, percentile_rank_rounded(3, Col1)),
    ?assertCanonEqual(0.875, percentile_rank_rounded(4, Col1)),

    run_percentile_rank(List1, Col1),

    %%%%%%%

    Col2 = b5_items:from_list([1, 2, 3, 4, 5]),
    List2 = b5_items:to_list(Col2),

    ?assertCanonEqual(0.1, percentile_rank_rounded(1, Col2)),
    ?assertCanonEqual(0.3, percentile_rank_rounded(2, Col2)),
    ?assertCanonEqual(0.5, percentile_rank_rounded(3, Col2)),
    ?assertCanonEqual(0.7, percentile_rank_rounded(4, Col2)),
    ?assertCanonEqual(0.9, percentile_rank_rounded(5, Col2)),

    run_percentile_rank(List2, Col2),

    %%%%%%

    Col3 = b5_items:from_list([1, 2, 3, 4, 5, 6]),
    List3 = b5_items:to_list(Col3),

    ?assertCanonEqual(0.0833333333, percentile_rank_rounded(1, Col3)),
    ?assertCanonEqual(0.25, percentile_rank_rounded(2, Col3)),
    ?assertCanonEqual(0.4166666667, percentile_rank_rounded(3, Col3)),
    ?assertCanonEqual(0.5833333333, percentile_rank_rounded(4, Col3)),
    ?assertCanonEqual(0.75, percentile_rank_rounded(5, Col3)),
    ?assertCanonEqual(0.9166666667, percentile_rank_rounded(6, Col3)),

    run_percentile_rank(List3, Col3),

    %%%%%%

    % From Wikipedia article example
    Col4 = b5_items:from_list([7, 5, 5, 4, 4, 3, 3, 3, 2, 1]),
    List4 = b5_items:to_list(Col4),

    ?assertCanonEqual(0.05, percentile_rank_rounded(1, Col4)),
    ?assertCanonEqual(0.15, percentile_rank_rounded(2, Col4)),
    ?assertCanonEqual(0.35, percentile_rank_rounded(3, Col4)),
    ?assertCanonEqual(0.60, percentile_rank_rounded(4, Col4)),
    ?assertCanonEqual(0.80, percentile_rank_rounded(5, Col4)),
    ?assertCanonEqual(0.90, percentile_rank_rounded(6, Col4)),
    ?assertCanonEqual(0.95, percentile_rank_rounded(7, Col4)),

    run_percentile_rank(List4, Col4),

    %%%%%%

    Col5 = b5_items:from_list([1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 9, 9, 9, 9, 9, 9]),
    List5 = b5_items:to_list(Col5),

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
%% Helper Functions: shared
%% ------------------------------------------------------------------

foreach_test_set(Fun) ->
    foreach_test_set(Fun, []).

foreach_test_set(Fun, Opts) ->
    foreach_tested_size(
        fun(Size, RefElements, RefUqElements) ->
            Col = b5_items:from_list(maybe_shuffle_elements_for_new_collection(RefElements)),
            UqCol = b5_items:from_list(maybe_shuffle_elements_for_new_collection(RefElements), [
                unique
            ]),

            Fun(Size, RefElements, Col, RefUqElements, UqCol)
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
    RefUqElements = ref_unique_elements(RefElements),
    Fun(Size, RefElements, RefUqElements).

new_ref_elements(Size, NumericOnly, RepetitionChance) ->
    new_ref_elements_recur(Size, NumericOnly, RepetitionChance, _Acc = []).

new_ref_elements_recur(Size, NumericOnly, RepetitionChance, Acc) when Size > 0 ->
    case Acc =/= [] andalso rand:uniform() =< RepetitionChance of
        true ->
            RepeatedElement = randomly_switch_number_type(list_pick_random(Acc)),
            UpdatedAcc = [RepeatedElement | Acc],
            new_ref_elements_recur(Size - 1, NumericOnly, RepetitionChance, UpdatedAcc);
        %
        false ->
            NewElement = new_element(NumericOnly),
            UpdatedAcc = [NewElement | Acc],
            new_ref_elements_recur(Size - 1, NumericOnly, RepetitionChance, UpdatedAcc)
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

ref_unique_elements([V1 | [V2 | T2] = T1]) ->
    if
        V2 == V1 ->
            ref_unique_elements([V1 | T2]);
        %
        V2 > V1 ->
            [V1 | ref_unique_elements(T1)]
    end;
ref_unique_elements(T) ->
    T.

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
    new_collection_from_each_added(List, []).

new_collection_from_each_added(List, Opts) ->
    Col = b5_items:new(Opts),

    IsUnique = proplists:get_value(unique, Opts, false),
    ?assertEqual(not IsUnique, Col =:= b5_items:new()),

    ?assertEqual(0, b5_items:size(Col)),
    ?assertEqual(true, b5_items:is_empty(Col)),

    new_collection_from_each_added_recur(List, Col).

new_collection_from_each_added_recur([Element | Next], Col) ->
    UpdatedCol = b5_items:add(Element, Col),
    new_collection_from_each_added_recur(Next, UpdatedCol);
new_collection_from_each_added_recur([], Col) ->
    Col.

%%%%%%%%%%%%%%%

foreach_existing_element(Fun, RefUqElements, Amount) ->
    Chosen = lists:sublist(list_shuffle(RefUqElements), Amount),

    lists:foreach(
        fun(Element) ->
            Fun(randomly_switch_number_type(Element))
        end,
        Chosen
    ).

foreach_non_existent_element(Fun, RefUqElements, Amount) when Amount > 0 ->
    Element = new_element(),

    case lists:any(fun(E) -> E == Element end, RefUqElements) of
        false ->
            Fun(Element),
            foreach_non_existent_element(Fun, RefUqElements, Amount - 1);
        %
        true ->
            foreach_non_existent_element(Fun, RefUqElements, Amount)
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

%% ------------------------------------------------------------------
%% Helpers: deletion
%% ------------------------------------------------------------------

test_delete_non_existing_keys(Col, RemainingElements, Amount) when Amount > 0 ->
    Element = new_element(),

    case lists:any(fun(E) -> E == Element end, RemainingElements) of
        false ->
            ?assertError({badkey, Element}, b5_items:delete(Element, Col)),
            ?assertEqual(Col, b5_items:delete_any(Element, Col)),

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
    case RefElements of
        [] ->
            Element = new_element(),
            ?assertEqual(none, b5_items:smaller(Element, Col));
        %
        [SingleElement] ->
            ?assertEqual(none, b5_items:smaller(randomly_switch_number_type(SingleElement), Col)),

            LargerElement = element_larger(SingleElement),
            ?assert(b5_items:smaller(LargerElement, Col) == {found, SingleElement}),

            SmallerElement = element_smaller(SingleElement),
            ?assertEqual(none, b5_items:smaller(SmallerElement, Col));
        %
        [FirstElement | Next] ->
            ?assertEqual(none, b5_items:smaller(randomly_switch_number_type(FirstElement), Col)),

            SmallerElement = element_smaller(FirstElement),
            ?assertEqual(none, b5_items:smaller(SmallerElement, Col)),

            run_smaller_recur(FirstElement, Next, Col)
    end.

run_smaller_recur(Expected, [LastElement], Col) ->
    ?assert(b5_items:smaller(randomly_switch_number_type(LastElement), Col) == {found, Expected}),

    LargerElement = element_larger(LastElement),
    ?assert(LargerElement > LastElement),
    ?assert(b5_items:smaller(LargerElement, Col) == {found, LastElement});
run_smaller_recur(Expected, [Element | Next], Col) ->
    ?assert(b5_items:smaller(randomly_switch_number_type(Element), Col) == {found, Expected}),

    case element_in_between(Expected, Element) of
        {found, InBetween} ->
            ?assert(InBetween > Expected),
            ?assert(InBetween < Element),
            ?assert(b5_items:smaller(InBetween, Col) == {found, Expected});
        %
        none ->
            ok
    end,

    run_smaller_recur(Element, Next, Col).

%%%%%%%%%%%%%%%%%

run_larger(RefElements, Col) ->
    case RefElements of
        [] ->
            Element = new_element(),
            ?assertEqual(none, b5_items:larger(Element, Col));
        %
        [SingleElement] ->
            ?assertEqual(none, b5_items:larger(SingleElement, Col)),

            LargerElement = element_larger(SingleElement),
            ?assertEqual(none, b5_items:larger(LargerElement, Col)),

            SmallerElement = element_smaller(SingleElement),
            ?assert(b5_items:larger(SmallerElement, Col) == {found, SingleElement});
        %
        _ ->
            [LastElement | Next] = lists:reverse(RefElements),

            ?assertEqual(none, b5_items:larger(randomly_switch_number_type(LastElement), Col)),

            LargerElement = element_larger(LastElement),
            ?assertEqual(none, b5_items:larger(LargerElement, Col)),

            run_larger_recur(LastElement, Next, Col)
    end.

run_larger_recur(Expected, [FirstElement], Col) ->
    ?assert(b5_items:larger(randomly_switch_number_type(FirstElement), Col) == {found, Expected}),

    SmallerElement = element_smaller(FirstElement),
    ?assert(SmallerElement < FirstElement),
    ?assert(b5_items:larger(SmallerElement, Col) == {found, FirstElement});
run_larger_recur(Expected, [Element | Next], Col) ->
    ?assert(b5_items:larger(randomly_switch_number_type(Element), Col) == {found, Expected}),

    case element_in_between(Element, Expected) of
        {found, InBetween} ->
            ?assert(InBetween < Expected),
            ?assert(InBetween > Element),
            ?assert(b5_items:larger(InBetween, Col) == {found, Expected});
        %
        none ->
            ok
    end,

    run_larger_recur(Element, Next, Col).

%%%%%%%%%%%%%%%%%

run_take_smallest([Expected | Next], Col) ->
    {Taken, Col2} = b5_items:take_smallest(Col),
    ?assert(Taken == Expected),
    ?assertEqual(length(Next), b5_items:size(Col2)),
    run_take_smallest(Next, Col2);
run_take_smallest([], Col) ->
    ?assertError(empty_items, b5_items:take_smallest(Col)).

run_take_largest([Expected | Next], Col) ->
    {Taken, Col2} = b5_items:take_largest(Col),
    ?assert(Taken == Expected),
    ?assertEqual(length(Next), b5_items:size(Col2)),
    run_take_largest(Next, Col2);
run_take_largest([], Col) ->
    ?assertError(empty_items, b5_items:take_largest(Col)).

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
    Iter = b5_items:iterator_from(Elem, Col),
    ?assertEqual(Iter, b5_items:iterator_from(Elem, Col, ordered)),
    Iter.

%%%%%%%%%%%%%%%%%

run_iterator_from_reversed(RefElements, Col) ->
    case lists:reverse(RefElements) of
        [] ->
            Iter = b5_items:iterator_from(new_element(), Col, reversed),
            ?assertEqual([], iterate(Iter));
        %
        [SingleElement] ->
            Iter = b5_items:iterator_from(
                randomly_switch_number_type(SingleElement), Col, reversed
            ),
            ?assertListsCanonEqual([SingleElement], iterate(Iter)),

            SmallerElement = element_smaller(SingleElement),
            Iter2 = b5_items:iterator_from(SmallerElement, Col, reversed),
            ?assertEqual([], iterate(Iter2)),

            LargerElement = element_larger(SingleElement),
            Iter3 = b5_items:iterator_from(LargerElement, Col, reversed),
            ?assertListsCanonEqual([SingleElement], iterate(Iter3));
        %
        [LastElement | _] = ReverseRefElements ->
            LargerElement = element_larger(LastElement),
            Iter = b5_items:iterator_from(LargerElement, Col, reversed),
            ?assertListsCanonEqual(ReverseRefElements, iterate(Iter)),

            run_iterator_from_reversed_recur(ReverseRefElements, Col)
    end.

run_iterator_from_reversed_recur([FirstElement], Col) ->
    run_iterator_from_reversed_first_elements([FirstElement], Col);
run_iterator_from_reversed_recur([Elem2 | [Elem1 | _] = Tail] = List, Col) ->
    Iter = b5_items:iterator_from(Elem2, Col, reversed),
    ?assertListsCanonEqual(List, iterate(Iter)),

    case element_in_between(Elem1, Elem2) of
        {found, InBetween} ->
            ?assert(InBetween > Elem1),
            ?assert(InBetween < Elem2),
            Iter2 = b5_items:iterator_from(InBetween, Col, reversed),
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
    Iter = b5_items:iterator_from(randomly_switch_number_type(Elem), Col, reversed),
    ?assertListsCanonEqual(FirstElements, iterate(Iter)),

    SmallerElement = element_smaller(Elem),
    Iter2 = b5_items:iterator_from(SmallerElement, Col, reversed),
    ?assertEqual([], iterate(Iter2)).

%%%%%%%%%%%%%%%%%

new_iterator(Col) ->
    Iter = b5_items:iterator(Col),
    ?assertEqual(Iter, b5_items:iterator(Col, ordered)),
    Iter.

iterate(Iter) ->
    case b5_items:next(Iter) of
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
            ?assertError({badarg, not_numerical}, b5_items:nth(not_numerical, Col)),

            BelowN = -(rand:uniform(100) - 1),
            BelowFloat = float(BelowN),
            ?assertError({badarg, BelowN}, b5_items:nth(BelowN, Col)),
            ?assertError({badarg, BelowFloat}, b5_items:nth(BelowFloat, Col)),

            BeyondN = Size + rand:uniform(100),
            BeyondFloat = float(BeyondN),
            ?assertError({badarg, BeyondN}, b5_items:nth(BeyondN, Col)),
            ?assertError({badarg, BeyondFloat}, b5_items:nth(BeyondFloat, Col)),

            (Size > 1 andalso
                begin
                    InBetween = rand:uniform(Size - 1) + 0.5,
                    ?assertError({badarg, InBetween}, b5_items:nth(InBetween, Col))
                end),

            (Size =/= 0 andalso
                begin
                    ValidButFloat = float(rand:uniform(Size)),
                    ?assertError({badarg, ValidButFloat}, b5_items:nth(ValidButFloat, Col))
                end)
        end,
        lists:seq(1, 20)
    ).

test_valid_nth(RefElements, Col) ->
    test_valid_nth_recur(1, RefElements, Col).

test_valid_nth_recur(N, [RefElement | Next], Col) ->
    ?assert(b5_items:nth(N, Col) == RefElement),
    test_valid_nth_recur(N + 1, Next, Col);
test_valid_nth_recur(_, [], _) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers: Percentile Bracket - Inclusive
%% ------------------------------------------------------------------

test_invalid_percentile_inclusive(Col) ->
    ?assertError({badarg, -1}, new_percentile_bracket_inclusive(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_bracket_inclusive(2, Col)),
    ?assertError({badarg, 100}, new_percentile_bracket_inclusive(100, Col)),
    ?assertError({badarg, 50}, new_percentile_bracket_inclusive(50, Col)),
    ?assertError({badarg, not_numerical}, new_percentile_bracket_inclusive(not_numerical, Col)),

    ?assertError({badarg, -1}, new_percentile_inclusive(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_inclusive(2, Col)),
    ?assertError({badarg, 100}, new_percentile_inclusive(100, Col)),
    ?assertError({badarg, 50}, new_percentile_inclusive(50, Col)),
    ?assertError({badarg, not_numerical}, new_percentile_inclusive(not_numerical, Col)).

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
    try b5_items:percentile_bracket(Percentile, Col) of
        Bracket ->
            ?assertEqual(
                Bracket, b5_items:percentile_bracket(Percentile, Col, [{method, inclusive}])
            ),
            ?assertEqual(
                Bracket,
                b5_items:percentile_bracket(Percentile, Col, [
                    {method, inclusive}, {method, exclusive}
                ])
            ),
            Bracket
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class, Reason, b5_items:percentile_bracket(Percentile, Col, [{method, inclusive}])
            ),
            ?assertException(
                Class,
                Reason,
                b5_items:percentile_bracket(Percentile, Col, [
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
    try b5_items:percentile(Percentile, Col) of
        Result ->
            ?assertEqual(Result, b5_items:percentile(Percentile, Col, [{method, inclusive}])),
            ?assertEqual(
                Result,
                b5_items:percentile(Percentile, Col, [{method, inclusive}, {method, exclusive}])
            ),
            Result
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class, Reason, b5_items:percentile(Percentile, Col, [{method, inclusive}])
            ),
            ?assertException(
                Class,
                Reason,
                b5_items:percentile(Percentile, Col, [{method, inclusive}, {method, exclusive}])
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
    ?assertError({badarg, not_numerical}, new_percentile_bracket_exclusive(not_numerical, Col)),

    ?assertError({badarg, -1}, new_percentile_exclusive(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_exclusive(2, Col)),
    ?assertError({badarg, 100}, new_percentile_exclusive(100, Col)),
    ?assertError({badarg, 50}, new_percentile_exclusive(50, Col)),
    ?assertError({badarg, not_numerical}, new_percentile_exclusive(not_numerical, Col)).

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
    try b5_items:percentile_bracket(Percentile, Col, [{method, exclusive}]) of
        Bracket ->
            ?assertEqual(
                Bracket,
                b5_items:percentile_bracket(Percentile, Col, [
                    {method, exclusive}, {method, inclusive}
                ])
            ),
            Bracket
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class,
                Reason,
                b5_items:percentile_bracket(Percentile, Col, [
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
    try b5_items:percentile(Percentile, Col, [{method, exclusive}]) of
        Result ->
            ?assertEqual(
                Result,
                b5_items:percentile(Percentile, Col, [{method, exclusive}, {method, inclusive}])
            ),
            Result
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class,
                Reason,
                b5_items:percentile(Percentile, Col, [{method, exclusive}, {method, inclusive}])
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
    ?assertError({badarg, not_numerical}, new_percentile_bracket_nearest_rank(not_numerical, Col)),

    ?assertError({badarg, -1}, new_percentile_nearest_rank(-1, Col)),
    ?assertError({badarg, 2}, new_percentile_nearest_rank(2, Col)),
    ?assertError({badarg, 100}, new_percentile_nearest_rank(100, Col)),
    ?assertError({badarg, 50}, new_percentile_nearest_rank(50, Col)),
    ?assertError({badarg, not_numerical}, new_percentile_nearest_rank(not_numerical, Col)).

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
    try b5_items:percentile_bracket(Percentile, Col, [{method, nearest_rank}]) of
        Bracket ->
            ?assertEqual(
                Bracket,
                b5_items:percentile_bracket(Percentile, Col, [
                    {method, nearest_rank}, {method, inclusive}
                ])
            ),
            Bracket
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class,
                Reason,
                b5_items:percentile_bracket(Percentile, Col, [
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
    try b5_items:percentile(Percentile, Col, [{method, nearest_rank}]) of
        Result ->
            ?assertEqual(
                Result,
                b5_items:percentile(Percentile, Col, [{method, nearest_rank}, {method, inclusive}])
            ),
            Result
    catch
        Class:Reason:Stacktrace ->
            ?assertException(
                Class,
                Reason,
                b5_items:percentile(Percentile, Col, [{method, nearest_rank}, {method, inclusive}])
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
    case b5_items:percentile(Percentile, Col, Opts) of
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
    round_float_precision(b5_items:percentile_rank(Elem, Col)).

%% ------------------------------------------------------------------
%% Helpers: Percentile Rank
%% ------------------------------------------------------------------

run_percentile_rank(RefElements, Col) ->
    case RefElements of
        [] ->
            ?assertError(empty_items, b5_items:percentile_rank(foobar, Col));
        %
        [SingleElement] ->
            ?assertEqual(0.5, b5_items:percentile_rank(SingleElement, Col)),
            ?assertEqual(
                0.5, b5_items:percentile_rank(randomly_switch_number_type(SingleElement), Col)
            ),

            LargerElement = element_larger(SingleElement),
            ?assertEqual(1.0, b5_items:percentile_rank(LargerElement, Col)),

            SmallerElement = element_smaller(SingleElement),
            ?assertEqual(0.0, b5_items:percentile_rank(SmallerElement, Col));
        %
        [FirstElement | Next] ->
            SmallerElement = element_smaller(FirstElement),
            ?assertEqual(0.0, b5_items:percentile_rank(SmallerElement, Col)),

            run_percentile_rank_recur(FirstElement, Next, 0, length(RefElements), Col)
    end.

run_percentile_rank_recur(Elem, Next, CF, Size, Col) ->
    CountRepeated = lists_count_while(fun(E) -> E == Elem end, Next),
    F = 1 + CountRepeated,
    UpdatedCF = CF + F,

    ElemRank = (UpdatedCF - (0.5 * F)) / Size,
    ?assertEqual(ElemRank, b5_items:percentile_rank(Elem, Col)),

    case lists:sublist(Next, CountRepeated + 1, length(Next) - CountRepeated) of
        [NextElem | NextNext] ->
            case element_in_between(Elem, NextElem) of
                {found, InBetween} ->
                    InBetweenRank = UpdatedCF / Size,
                    ?assertEqual(InBetweenRank, b5_items:percentile_rank(InBetween, Col));
                none ->
                    ok
            end,

            run_percentile_rank_recur(NextElem, NextNext, UpdatedCF, Size, Col);
        %
        [] ->
            LargerElem = element_larger(Elem),
            LargerRank = 1.0,
            ?assertEqual(LargerRank, b5_items:percentile_rank(LargerElem, Col))
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
