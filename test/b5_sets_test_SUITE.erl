%% @doc B5 Sets test suite modeled after existing b5_trees and b5_ranks test suites
-module(b5_sets_test_SUITE).

-ifndef(NO_CT_SUITE_BEHAVIOUR).
-behaviour(ct_suite).
-endif.

-include_lib("stdlib/include/assert.hrl").

%% CT exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

%% Test exports - Basic API tests
-export([
    test_new_function/1,
    test_from_list_operations/1,
    test_to_list_operations/1,
    test_singleton_operations/1,
    test_size_is_empty_operations/1
]).

%% Test exports - Membership operations
-export([
    test_add_operations/1,
    test_insert_operations/1,
    test_delete_operations/1,
    test_delete_any_operations/1,
    test_is_member_operations/1,
    test_is_set_operations/1
]).

%% Test exports - Set operations
-export([
    test_union_operations/1,
    test_intersection_operations/1,
    test_difference_operations/1,
    test_is_disjoint_operations/1,
    test_is_subset_operations/1,
    test_is_equal_operations/1
]).

%% Test exports - Iterator operations
-export([
    test_iterator_operations/1,
    test_iterator_ordered_operations/1,
    test_iterator_reversed_operations/1,
    test_iterator_from_operations/1,
    test_iterator_from_ordered_operations/1,
    test_iterator_from_reversed_operations/1
]).

%% Test exports - Range operations
-export([
    test_smaller_larger_operations/1,
    test_smallest_largest_operations/1,
    test_take_smallest_operations/1,
    test_take_largest_operations/1
]).

%% Test exports - Higher-order operations
-export([
    test_fold_operations/1,
    test_map_operations/1,
    test_filter_operations/1,
    test_filtermap_operations/1
]).

%% Test exports - Compatibility operations
-export([
    test_compatibility_aliases/1
]).

%% Test exports - Empty set edge cases
-export([
    test_empty_set_exceptions/1
]).

%% Test constants
-define(TESTED_SET_SIZES,
    (lists:seq(0, 50) ++ lists:seq(55, 200, 5) ++ [997])
).

%% ------------------------------------------------------------------
%% CT Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [
        {basic_api, [parallel], [
            test_new_function,
            test_from_list_operations,
            test_to_list_operations,
            test_singleton_operations,
            test_size_is_empty_operations
        ]},
        {membership_operations, [parallel], [
            test_add_operations,
            test_insert_operations,
            test_delete_operations,
            test_delete_any_operations,
            test_is_member_operations,
            test_is_set_operations
        ]},
        {set_operations, [parallel], [
            test_union_operations,
            test_intersection_operations,
            test_difference_operations,
            test_is_disjoint_operations,
            test_is_subset_operations,
            test_is_equal_operations
        ]},
        {iterator_operations, [parallel], [
            test_iterator_operations,
            test_iterator_ordered_operations,
            test_iterator_reversed_operations,
            test_iterator_from_operations,
            test_iterator_from_ordered_operations,
            test_iterator_from_reversed_operations
        ]},
        {range_operations, [parallel], [
            test_smaller_larger_operations,
            test_smallest_largest_operations,
            test_take_smallest_operations,
            test_take_largest_operations
        ]},
        {higher_order_operations, [parallel], [
            test_fold_operations,
            test_map_operations,
            test_filter_operations,
            test_filtermap_operations
        ]},
        {compatibility_operations, [parallel], [
            test_compatibility_aliases
        ]},
        {empty_set_edge_cases, [parallel], [
            test_empty_set_exceptions
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%% ------------------------------------------------------------------
%% Basic API Tests
%% ------------------------------------------------------------------

test_new_function(_Config) ->
    Set = b5_sets:new(),
    ?assertEqual(0, b5_sets:size(Set)),
    ?assertEqual(Set, b5_sets:empty()),
    ?assertEqual(true, b5_sets:is_empty(Set)),
    ?assertEqual([], b5_sets:to_list(Set)).

test_from_list_operations(_Config) ->
    %% Empty list
    Set1 = b5_sets:from_list([]),
    ?assertEqual(0, b5_sets:size(Set1)),
    ?assertEqual([], b5_sets:to_list(Set1)),

    %% Single element
    Set2 = b5_sets:from_list([42]),
    ?assertEqual(1, b5_sets:size(Set2)),
    ?assertEqual([42], b5_sets:to_list(Set2)),

    %% Multiple elements
    Set3 = b5_sets:from_list([3, 1, 4, 1, 5, 9, 2, 6]),
    ?assertEqual(7, b5_sets:size(Set3)),
    ?assertEqual([1, 2, 3, 4, 5, 6, 9], b5_sets:to_list(Set3)),

    %% Test with different types
    Set4 = b5_sets:from_list([atom, "string", 42, 3.14]),
    ?assertEqual(4, b5_sets:size(Set4)).

test_to_list_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        ?assertEqual(gb_sets:to_list(RefSet), b5_sets:to_list(TestSet))
    end).

test_singleton_operations(_Config) ->
    Set1 = b5_sets:singleton(42),
    ?assertEqual(1, b5_sets:size(Set1)),
    ?assertEqual([42], b5_sets:to_list(Set1)),
    ?assertEqual(true, b5_sets:is_member(42, Set1)),

    Set2 = b5_sets:singleton(atom),
    ?assertEqual(1, b5_sets:size(Set2)),
    ?assertEqual([atom], b5_sets:to_list(Set2)),
    ?assertEqual(true, b5_sets:is_member(atom, Set2)).

test_size_is_empty_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        ExpectedSize = gb_sets:size(RefSet),
        ?assertEqual(ExpectedSize, b5_sets:size(TestSet)),
        ?assertEqual(ExpectedSize =:= 0, b5_sets:is_empty(TestSet))
    end).

%% ------------------------------------------------------------------
%% Membership Operation Tests
%% ------------------------------------------------------------------

test_add_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        %% Add existing elements - should not change set
        foreach_randomly_retyped_element(
            fun(Element) ->
                UpdatedSet = b5_sets:add(Element, TestSet),
                ?assertEqual(TestSet, UpdatedSet)
            end,
            RefSet
        ),

        foreach_non_existent_element(
            fun(NewElement) ->
                UpdatedRef = gb_sets:add(NewElement, RefSet),
                UpdatedSet = b5_sets:add(NewElement, TestSet),
                ?assertEqual(gb_sets:size(UpdatedRef), b5_sets:size(UpdatedSet)),
                ?assertEqual(b5_sets:size(TestSet) + 1, b5_sets:size(UpdatedSet)),
                ?assertEqual(true, b5_sets:is_member(NewElement, UpdatedSet)),
                ?assertEqual(gb_sets:to_list(UpdatedRef), b5_sets:to_list(UpdatedSet))
            end,
            RefSet,
            50
        )
    end).

test_insert_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        foreach_randomly_retyped_element(
            fun(Element) ->
                %% Insert existing elements - should throw error
                ?assertError({key_exists, Element}, b5_sets:insert(Element, TestSet))
            end,
            RefSet
        ),

        foreach_non_existent_element(
            fun(NewElement) ->
                UpdatedRef = gb_sets:insert(NewElement, RefSet),
                UpdatedSet = b5_sets:insert(NewElement, TestSet),
                ?assertEqual(gb_sets:size(UpdatedRef), b5_sets:size(UpdatedSet)),
                ?assertEqual(b5_sets:size(TestSet) + 1, b5_sets:size(UpdatedSet)),
                ?assertEqual(true, b5_sets:is_member(NewElement, UpdatedSet)),
                ?assertEqual(gb_sets:to_list(UpdatedRef), b5_sets:to_list(UpdatedSet))
            end,
            RefSet,
            50
        )
    end).

test_delete_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        %% Delete existing elements
        fold_randomly_retyped_shuffled_elements(
            fun(Element, {RefAcc, TestAcc}) ->
                UpdatedRef = gb_sets:delete(Element, RefAcc),
                UpdatedSet = b5_sets:delete(Element, TestAcc),
                ?assertEqual(gb_sets:size(UpdatedRef), b5_sets:size(UpdatedSet)),
                ?assertEqual(b5_sets:size(TestAcc) - 1, b5_sets:size(UpdatedSet)),
                ?assertEqual(false, b5_sets:is_member(Element, UpdatedSet)),
                ?assertEqual(gb_sets:to_list(UpdatedRef), b5_sets:to_list(UpdatedSet)),
                {UpdatedRef, UpdatedSet}
            end,
            {RefSet, TestSet},
            RefSet
        ),

        foreach_non_existent_element(
            fun(NonExistentElement) ->
                %% Delete non-existent element - should throw error
                ?assertError(
                    {badkey, NonExistentElement}, b5_sets:delete(NonExistentElement, TestSet)
                )
            end,
            RefSet,
            50
        )
    end).

test_delete_any_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        %% Delete existing elements
        fold_randomly_retyped_shuffled_elements(
            fun(Element, {RefAcc, TestAcc}) ->
                UpdatedRef = gb_sets:delete(Element, RefAcc),
                UpdatedSet = b5_sets:delete(Element, TestAcc),
                ?assertEqual(gb_sets:size(UpdatedRef), b5_sets:size(UpdatedSet)),
                ?assertEqual(b5_sets:size(TestAcc) - 1, b5_sets:size(UpdatedSet)),
                ?assertEqual(false, b5_sets:is_member(Element, UpdatedSet)),
                ?assertEqual(gb_sets:to_list(UpdatedRef), b5_sets:to_list(UpdatedSet)),
                {UpdatedRef, UpdatedSet}
            end,
            {RefSet, TestSet},
            RefSet
        ),

        foreach_non_existent_element(
            fun(NonExistentElement) ->
                %% Delete non-existent element - keeps the set as is
                ?assertEqual(TestSet, b5_sets:delete_any(NonExistentElement, TestSet))
            end,
            RefSet,
            50
        )
    end).

test_is_member_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        %% Test existing elements
        foreach_randomly_retyped_element(
            fun(Element) ->
                ?assertEqual(true, b5_sets:is_member(Element, TestSet))
            end,
            RefSet
        ),

        %% Test non-existent elements
        foreach_non_existent_element(
            fun(NonExistentElement) ->
                ?assertEqual(false, b5_sets:is_member(NonExistentElement, TestSet))
            end,
            RefSet,
            50
        )
    end).

test_is_set_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        ?assertEqual(false, b5_sets:is_set(RefSet)),
        ?assertEqual(true, b5_sets:is_set(TestSet))
    end).

%% ------------------------------------------------------------------
%% Set Operation Tests
%% ------------------------------------------------------------------

%%%%%%%%%%%%%
%% Union

test_union_operations(_Config) ->
    foreach_set(
        fun(RefSet1, TestSet1) ->
            % Union with self
            ?assertEqual(
                gb_sets:to_list(RefSet1), b5_sets:to_list(b5_sets:union(TestSet1, TestSet1))
            ),

            foreach_set_op_variant(
                RefSet1,
                fun(RefSet2, TestSet2) ->
                    RefUnion = gb_sets:union(RefSet1, RefSet2),
                    ExpectedElements = gb_sets:to_list(RefUnion),
                    ExpectedSize = length(ExpectedElements),

                    TestUnion = b5_sets:union(TestSet1, TestSet2),
                    TestUnionComm = b5_sets:union(TestSet1, TestSet2),

                    ?assertEqual([], diff_lists(ExpectedElements, b5_sets:to_list(TestUnion))),
                    ?assertEqual([], diff_lists(ExpectedElements, b5_sets:to_list(TestUnionComm))),

                    ?assertEqual(ExpectedSize, b5_sets:size(TestUnion)),
                    ?assertEqual(ExpectedSize, b5_sets:size(TestUnionComm))
                end
            )
        end
    ).

%%%%%%%%%%%%%
%% Intersection

test_intersection_operations(_Config) ->
    foreach_set(
        fun(RefSet1, TestSet1) ->
            % Intersect with self
            ?assertEqual(
                gb_sets:to_list(RefSet1), b5_sets:to_list(b5_sets:intersection(TestSet1, TestSet1))
            ),

            foreach_set_op_variant(
                RefSet1,
                fun(RefSet2, TestSet2) ->
                    RefIntersect = gb_sets:intersection(RefSet1, RefSet2),
                    ExpectedElements = gb_sets:to_list(RefIntersect),
                    ExpectedSize = length(ExpectedElements),

                    TestIntersect = b5_sets:intersection(TestSet1, TestSet2),
                    TestIntersectComm = b5_sets:intersection(TestSet1, TestSet2),

                    ?assertEqual([], diff_lists(ExpectedElements, b5_sets:to_list(TestIntersect))),
                    ?assertEqual(
                        [], diff_lists(ExpectedElements, b5_sets:to_list(TestIntersectComm))
                    ),

                    ?assertEqual(ExpectedSize, b5_sets:size(TestIntersect)),
                    ?assertEqual(ExpectedSize, b5_sets:size(TestIntersectComm))
                end
            )
        end
    ).

%%%%%%%%%%%%%
%% Difference

test_difference_operations(_Config) ->
    foreach_set(
        fun(RefSet1, TestSet1) ->
            % Difference with self
            ?assertEqual([], b5_sets:to_list(b5_sets:difference(TestSet1, TestSet1))),

            foreach_set_op_variant(
                RefSet1,
                fun(RefSet2, TestSet2) ->
                    RefDifferenceA = gb_sets:difference(RefSet1, RefSet2),
                    ExpectedElementsA = gb_sets:to_list(RefDifferenceA),
                    ExpectedSizeA = length(ExpectedElementsA),
                    TestDifferenceA = b5_sets:difference(TestSet1, TestSet2),
                    ?assertEqual(ExpectedElementsA, b5_sets:to_list(TestDifferenceA)),
                    ?assertEqual(ExpectedSizeA, b5_sets:size(TestDifferenceA)),

                    RefDifferenceB = gb_sets:difference(RefSet2, RefSet1),
                    ExpectedElementsB = gb_sets:to_list(RefDifferenceB),
                    ExpectedSizeB = length(ExpectedElementsB),
                    TestDifferenceB = b5_sets:difference(TestSet2, TestSet1),
                    ?assertEqual(ExpectedElementsB, b5_sets:to_list(TestDifferenceB)),
                    ?assertEqual(ExpectedSizeB, b5_sets:size(TestDifferenceB))
                end
            )
        end
    ).

test_is_disjoint_operations(_Config) ->
    foreach_set(
        fun(RefSet1, TestSet1) ->
            % is_disjoint with self
            IsEmpty = b5_sets:is_empty(TestSet1),
            ?assertEqual(IsEmpty, b5_sets:is_disjoint(TestSet1, TestSet1)),

            % is_disjoint with rebuilt variants of self
            foreach_rebuilt_variant(
                TestSet1,
                fun(TestSetVariant) ->
                    ?assertEqual(IsEmpty, b5_sets:is_disjoint(TestSet1, TestSetVariant)),
                    ?assertEqual(IsEmpty, b5_sets:is_disjoint(TestSetVariant, TestSet1))
                end
            ),

            foreach_set_op_variant(
                RefSet1,
                fun(RefSet2, TestSet2) ->
                    ?assertEqual(
                        gb_sets:is_disjoint(RefSet1, RefSet2),
                        b5_sets:is_disjoint(TestSet1, TestSet2)
                    ),
                    ?assertEqual(
                        gb_sets:is_disjoint(RefSet1, RefSet2),
                        b5_sets:is_disjoint(TestSet2, TestSet1)
                    ),
                    ?assertEqual(
                        gb_sets:is_disjoint(RefSet2, RefSet1),
                        b5_sets:is_disjoint(TestSet2, TestSet1)
                    )
                end
            )
        end
    ).

test_is_subset_operations(_Config) ->
    foreach_set(
        fun(RefSet1, TestSet1) ->
            % is_subset with self
            ?assertEqual(true, b5_sets:is_subset(TestSet1, TestSet1)),

            % is_subset with rebuilt variants of self
            foreach_rebuilt_variant(
                TestSet1,
                fun(TestSetVariant) ->
                    ?assertEqual(true, b5_sets:is_subset(TestSet1, TestSetVariant)),
                    ?assertEqual(true, b5_sets:is_subset(TestSetVariant, TestSet1))
                end
            ),

            foreach_set_op_variant(
                RefSet1,
                fun(RefSet2, TestSet2) ->
                    ?assertEqual(
                        gb_sets:is_subset(RefSet1, RefSet2), b5_sets:is_subset(TestSet1, TestSet2)
                    ),
                    ?assertEqual(
                        gb_sets:is_subset(RefSet2, RefSet1), b5_sets:is_subset(TestSet2, TestSet1)
                    )
                end
            )
        end
    ).

test_is_equal_operations(_Config) ->
    foreach_set(
        fun(RefSet1, TestSet1) ->
            % is_equal with self
            ?assertEqual(true, b5_sets:is_equal(TestSet1, TestSet1)),

            % is_subset with rebuilt variants of self
            foreach_rebuilt_variant(
                TestSet1,
                fun(TestSetVariant) ->
                    ?assertEqual(true, b5_sets:is_equal(TestSet1, TestSetVariant)),
                    ?assertEqual(true, b5_sets:is_equal(TestSetVariant, TestSet1))
                end
            ),

            foreach_set_op_variant(
                RefSet1,
                fun(RefSet2, TestSet2) ->
                    ?assertEqual(
                        gb_sets:is_equal(RefSet1, RefSet2), b5_sets:is_equal(TestSet1, TestSet2)
                    ),
                    ?assertEqual(
                        gb_sets:is_equal(RefSet1, RefSet2), b5_sets:is_equal(TestSet2, TestSet1)
                    ),
                    ?assertEqual(
                        gb_sets:is_equal(RefSet2, RefSet1), b5_sets:is_equal(TestSet2, TestSet1)
                    )
                end
            )
        end
    ).

%%%%%%%%%%%%%%

foreach_set_op_variant(RefSet, Fun) ->
    RefSize = gb_sets:size(RefSet),
    RefList = gb_sets:to_list(RefSet),

    %
    % Variants focused on union/2, difference/2, is_disjoint/2, is_subset/2
    %

    lists:foreach(
        fun(Size2) ->
            lists:foreach(
                fun(StartingOffset) ->
                    {RefSet2, TestSet2} = test_set_op_variant(RefSet, Size2, StartingOffset),
                    Fun(RefSet2, TestSet2)
                end,
                lists:usort([
                    -20, -1, 0, 1, Size2 div 2, max(0, Size2 - 1), Size2, Size2 + 1, Size2 + 20
                ])
            )
        end,
        lists:usort([
            0, 1, 5, RefSize div 2, max(0, RefSize - 1), RefSize, RefSize + 1, RefSize + 10
        ])
    ),

    %
    % Variants focused on is_subset/2, is_equal/2
    %

    lists:foreach(
        fun(Where) ->
            HopefullyNewElement =
                case Where of
                    smaller ->
                        hd_or_zero(RefList) - 1;
                    mid ->
                        Min = trunc(hd_or_zero(RefList)),
                        Max = trunc(last_or_zero(RefList)),
                        rand:uniform(max(1, 1 + Max - Min - 1));
                    larger ->
                        last_or_zero(RefList) + 1
                end,

            NewList = shuffle_list(randomly_switch_types([HopefullyNewElement | RefList])),

            RefSet2 = gb_sets:from_list(NewList),
            TestSet2 = b5_sets:from_list(NewList),
            Fun(RefSet2, TestSet2)
        end,
        [smaller, mid, larger]
    ).

test_set_op_variant(RefSet, Size2, StartingOffset) ->
    List2 = prepare_set_op_variant(RefSet, Size2, StartingOffset),
    RefSet2 = gb_sets:from_list(shuffle_list(List2)),
    TestSet2 = b5_sets:from_list(shuffle_list(List2)),
    {RefSet2, TestSet2}.

prepare_set_op_variant(RefSet, Size2, StartingOffset) ->
    if
        Size2 =:= 0 ->
            [];
        %
        StartingOffset < 0 ->
            RefList = gb_sets:to_list(RefSet),
            MinElement = hd_or_zero(RefList),
            PrependSize = min(Size2, -StartingOffset),
            PrependFirst = trunc(MinElement + StartingOffset),
            PrependLast = PrependFirst + PrependSize - 1,
            PrependElements = randomly_switch_types(lists:seq(PrependFirst, PrependLast, +1)),
            PrependElements ++ prepare_set_op_variant_recur(RefList, Size2 - PrependSize);
        %
        StartingOffset >= 0 ->
            RefList = gb_sets:to_list(RefSet),

            case lists:sublist(RefList, StartingOffset + 1) of
                [_ | _] = AdvancedRefList ->
                    prepare_set_op_variant_recur(AdvancedRefList, Size2);
                %
                [] ->
                    LastElement = last_or_zero(RefList),
                    AppendOffset = StartingOffset - gb_sets:size(RefSet),
                    AppendFirst = trunc(LastElement + AppendOffset),
                    AppendLast = AppendFirst + Size2 - 1,
                    AppendElements = randomly_switch_types(lists:seq(AppendFirst, AppendLast, +1)),
                    AppendElements
            end
    end.

hd_or_zero([H | _]) -> H;
hd_or_zero([]) -> 0.

last_or_zero([_ | _] = L) -> lists:last(L);
last_or_zero([]) -> 0.

prepare_set_op_variant_recur([ElemA | [_ | _] = Next], Size2) when Size2 > 0 ->
    case rand:uniform(3) of
        1 ->
            % Pick guaranteed conflicting element
            TestElem = randomly_switch_type(ElemA),
            [TestElem | prepare_set_op_variant_recur(Next, Size2 - 1)];
        %
        _ ->
            % Pick element that's very likely to be new
            TestElem = randomly_switch_type(ElemA + 1.3),
            [TestElem | prepare_set_op_variant_recur(Next, Size2 - 1)]
    end;
prepare_set_op_variant_recur([LastElement], Size2) when Size2 > 0 ->
    AppendFirst =
        case rand:uniform(3) of
            1 ->
                % Pick guaranteed conflicting element
                trunc(LastElement);
            _ ->
                % Pick element that's likely to be new
                trunc(LastElement) + 1
        end,

    AppendLast = AppendFirst + Size2 - 1,
    AppendElements = randomly_switch_types(lists:seq(AppendFirst, AppendLast, +1)),
    AppendElements;
prepare_set_op_variant_recur(RefList, Size2) when RefList =:= [] orelse Size2 =:= 0 ->
    [].

%% ------------------------------------------------------------------
%% Iterator Operation Tests
%% ------------------------------------------------------------------

test_iterator_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        TestIter = b5_sets:iterator(TestSet),
        IteratedElements = iterate_to_list(TestIter),
        ?assertEqual(gb_sets:to_list(RefSet), IteratedElements),

        ?assertEqual(gb_sets:size(RefSet), length(IteratedElements)),
        ?assertEqual(gb_sets:size(RefSet), b5_sets:size(TestSet))
    end).

test_iterator_ordered_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        TestIter = b5_sets:iterator(TestSet, ordered),
        IteratedElements = iterate_to_list(TestIter),
        ?assertEqual(gb_sets:to_list(RefSet), IteratedElements),

        ?assertEqual(gb_sets:size(RefSet), length(IteratedElements)),
        ?assertEqual(gb_sets:size(RefSet), b5_sets:size(TestSet))
    end).

test_iterator_reversed_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        TestIter = b5_sets:iterator(TestSet, reversed),
        IteratedElements = iterate_to_list(TestIter),
        ?assertEqual(gb_sets:to_list(RefSet), lists:reverse(IteratedElements)),

        ?assertEqual(gb_sets:size(RefSet), length(IteratedElements)),
        ?assertEqual(gb_sets:size(RefSet), b5_sets:size(TestSet))
    end).

test_iterator_from_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        RefList = gb_sets:to_list(RefSet),

        WholeFromValues = randomly_switch_types(
            lists:usort(
                [
                    hd_or_zero(RefList) - 10,
                    hd_or_zero(RefList) - 1,
                    last_or_zero(RefList) + 1,
                    last_or_zero(RefList) + 10
                ] ++
                    RefList
            )
        ),

        FromValues =
            (lists:map(
                fun(From) -> From + 0.5 end,
                take_random(WholeFromValues, 10)
            ) ++
                WholeFromValues),

        lists:foreach(
            fun(From) ->
                RefIter = gb_sets:iterator_from(From, RefSet),
                RefIteratedElements = ref_iterate_to_list(RefIter),
                ExpectedList = lists:dropwhile(fun(Elem) -> Elem < From end, RefList),
                ?assertEqual(RefIteratedElements, ExpectedList),

                TestIter = b5_sets:iterator_from(From, TestSet),
                IteratedElements = iterate_to_list(TestIter),
                ?assertEqual(ExpectedList, IteratedElements),

                ?assert(b5_sets:size(TestSet) >= length(IteratedElements))
            end,
            FromValues
        )
    end).

test_iterator_from_ordered_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        RefList = gb_sets:to_list(RefSet),

        WholeFromValues = randomly_switch_types(
            lists:usort(
                [
                    hd_or_zero(RefList) - 10,
                    hd_or_zero(RefList) - 1,
                    last_or_zero(RefList) + 1,
                    last_or_zero(RefList) + 10
                ] ++
                    RefList
            )
        ),

        FromValues =
            (lists:map(
                fun(From) -> From + 0.5 end,
                take_random(WholeFromValues, 10)
            ) ++
                WholeFromValues),

        lists:foreach(
            fun(From) ->
                RefIter = gb_sets:iterator_from(From, RefSet, ordered),
                RefIteratedElements = ref_iterate_to_list(RefIter),
                ExpectedList = lists:dropwhile(fun(Elem) -> Elem < From end, RefList),
                ?assertEqual(RefIteratedElements, ExpectedList),

                TestIter = b5_sets:iterator_from(From, TestSet, ordered),
                IteratedElements = iterate_to_list(TestIter),
                ?assertEqual(ExpectedList, IteratedElements),

                ?assert(b5_sets:size(TestSet) >= length(IteratedElements))
            end,
            FromValues
        )
    end).

test_iterator_from_reversed_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        RefList = gb_sets:to_list(RefSet),

        WholeFromValues = randomly_switch_types(
            lists:usort(
                [
                    hd_or_zero(RefList) - 10,
                    hd_or_zero(RefList) - 1,
                    last_or_zero(RefList) + 1,
                    last_or_zero(RefList) + 10
                ] ++
                    RefList
            )
        ),

        FromValues =
            (lists:map(
                fun(From) -> From + 0.5 end,
                take_random(WholeFromValues, 10)
            ) ++
                WholeFromValues),

        lists:foreach(
            fun(From) ->
                RefIter = gb_sets:iterator_from(From, RefSet, reversed),
                RefIteratedElements = ref_iterate_to_list(RefIter),
                ExpectedList = lists:reverse(
                    lists:takewhile(fun(Elem) -> Elem =< From end, RefList)
                ),
                ?assertEqual(RefIteratedElements, ExpectedList),

                TestIter = b5_sets:iterator_from(From, TestSet, reversed),
                IteratedElements = iterate_to_list(TestIter),
                ?assertEqual(ExpectedList, IteratedElements),

                ?assert(b5_sets:size(TestSet) >= length(IteratedElements))
            end,
            FromValues
        )
    end).

%% ------------------------------------------------------------------
%% Range Operation Tests
%% ------------------------------------------------------------------

test_smaller_larger_operations(_Config) ->
    foreach_set(fun(RefSet, TestSet) ->
        RefList = gb_sets:to_list(RefSet),

        WholePivots = randomly_switch_types(
            lists:usort(
                [
                    hd_or_zero(RefList) - 10,
                    hd_or_zero(RefList) - 1,
                    last_or_zero(RefList) + 1,
                    last_or_zero(RefList) + 10
                ] ++
                    RefList
            )
        ),

        Pivots =
            (lists:map(
                fun(From) -> From + 0.5 end,
                take_random(WholePivots, 10)
            ) ++
                WholePivots),

        lists:foreach(
            fun(Pivot) ->
                RefSmaller = gb_sets:smaller(Pivot, RefSet),
                ExpectedSmaller = expected_smaller(Pivot, RefList),
                ?assertEqual(RefSmaller, ExpectedSmaller),

                TestedSmaller = b5_sets:smaller(Pivot, TestSet),
                ?assertEqual(ExpectedSmaller, TestedSmaller),

                %%%

                RefLarger = gb_sets:larger(Pivot, RefSet),
                ExpectedLarger = expected_larger(Pivot, RefList),
                ?assertEqual(RefLarger, ExpectedLarger),

                TestedLarger = b5_sets:larger(Pivot, TestSet),
                ?assertEqual(ExpectedLarger, TestedLarger)
            end,
            Pivots
        )
    end).

expected_smaller(Pivot, RefList) ->
    case lists:filter(fun(Elem) -> Elem < Pivot end, RefList) of
        [] ->
            none;
        %
        Filtered ->
            {found, lists:last(Filtered)}
    end.

expected_larger(Pivot, RefList) ->
    case lists:dropwhile(fun(Elem) -> Elem =< Pivot end, RefList) of
        [] ->
            none;
        %
        [Elem | _] ->
            {found, Elem}
    end.

%%%

test_smallest_largest_operations(_Config) ->
    foreach_set(
        fun(RefSet, TestSet) ->
            case gb_sets:to_list(RefSet) of
                [] ->
                    ?assertError(function_clause, gb_sets:smallest(RefSet)),
                    ?assertError(function_clause, gb_sets:largest(RefSet)),

                    % FIXME should we return the exact same exception instead? (`function_clause')
                    ?assertError(empty_set, b5_sets:smallest(TestSet)),
                    ?assertError(empty_set, b5_sets:largest(TestSet));
                %
                [ExpectedSmallest | _] = RefList ->
                    ExpectedLargest = lists:last(RefList),

                    ?assertEqual(ExpectedSmallest, b5_sets:smallest(TestSet)),
                    ?assertEqual(ExpectedLargest, b5_sets:largest(TestSet))
            end
        end
    ).

test_take_smallest_operations(_Config) ->
    foreach_set(
        fun(RefSet, TestSet) ->
            lists:foldl(
                fun(_, {RefAcc, TestAcc}) ->
                    case gb_sets:to_list(RefAcc) of
                        [] ->
                            ?assertError(function_clause, gb_sets:take_smallest(RefAcc)),
                            ?assertEqual(0, gb_sets:size(RefAcc)),

                            % FIXME should we return the exact same exception instead? (`function_clause')
                            ?assertError(empty_set, b5_sets:take_smallest(TestAcc)),
                            ?assertEqual(0, b5_sets:size(TestAcc)),
                            finished;
                        %
                        [ExpectedSmallest | ExpectedRemaining] ->
                            ExpectedSize = length(ExpectedRemaining),

                            {RefSmallest, RefUpdated} = gb_sets:take_smallest(RefAcc),
                            ?assertEqual(RefSmallest, ExpectedSmallest),
                            ?assertEqual(gb_sets:to_list(RefUpdated), ExpectedRemaining),
                            ?assertEqual(gb_sets:size(RefUpdated), ExpectedSize),

                            {TestSmallest, TestUpdated} = b5_sets:take_smallest(TestAcc),
                            ?assertEqual(TestSmallest, ExpectedSmallest),
                            ?assertEqual(ExpectedRemaining, b5_sets:to_list(TestUpdated)),
                            ?assertEqual(ExpectedSize, b5_sets:size(TestUpdated)),

                            {RefUpdated, TestUpdated}
                    end
                end,
                {RefSet, TestSet},
                lists:seq(1, gb_sets:size(RefSet) + 1)
            )
        end
    ).

test_take_largest_operations(_Config) ->
    foreach_set(
        fun(RefSet, TestSet) ->
            lists:foldl(
                fun(_, {RefAcc, TestAcc}) ->
                    case gb_sets:fold(fun(Elem, Acc) -> [Elem | Acc] end, [], RefAcc) of
                        [] ->
                            ?assertError(function_clause, gb_sets:take_largest(RefAcc)),
                            ?assertEqual(0, gb_sets:size(RefAcc)),

                            ?assertError(empty_set, b5_sets:take_largest(TestAcc)),
                            ?assertEqual(0, b5_sets:size(TestAcc)),
                            finished;
                        %
                        [ExpectedLargest | ExpectedRemainingReverse] ->
                            ExpectedRemaining = lists:reverse(ExpectedRemainingReverse),
                            ExpectedSize = length(ExpectedRemaining),

                            {RefLargest, RefUpdated} = gb_sets:take_largest(RefAcc),
                            ?assertEqual(RefLargest, ExpectedLargest),
                            ?assertEqual(gb_sets:to_list(RefUpdated), ExpectedRemaining),
                            ?assertEqual(gb_sets:size(RefUpdated), ExpectedSize),

                            {TestLargest, TestUpdated} = b5_sets:take_largest(TestAcc),
                            ?assertEqual(TestLargest, ExpectedLargest),
                            ?assertEqual(ExpectedRemaining, b5_sets:to_list(TestUpdated)),
                            ?assertEqual(ExpectedSize, b5_sets:size(TestUpdated)),

                            {RefUpdated, TestUpdated}
                    end
                end,
                {RefSet, TestSet},
                lists:seq(1, gb_sets:size(RefSet) + 1)
            )
        end
    ).

%% ------------------------------------------------------------------
%% Higher-Order Operation Tests
%% ------------------------------------------------------------------

test_fold_operations(_Config) ->
    foreach_set(
        fun(RefSet, TestSet) ->
            {FoldFun, Acc0} =
                case rand:uniform(3) of
                    1 ->
                        {fun(_Elem, _Acc) -> no end, no};
                    %
                    2 ->
                        {fun(Elem, Acc) -> Elem + Acc end, 0};
                    %
                    3 ->
                        Multiplier = rand:uniform(10),
                        {fun(Elem, Acc) -> [Multiplier * Elem | Acc] end, []}
                end,

            ?assertEqual(
                gb_sets:fold(FoldFun, Acc0, RefSet),
                b5_sets:fold(FoldFun, Acc0, TestSet)
            )
        end
    ).

test_map_operations(_Config) ->
    foreach_set(
        fun(RefSet, TestSet) ->
            MapFun =
                case rand:uniform(3) of
                    1 ->
                        fun(_Elem) -> no end;
                    %
                    2 ->
                        fun(Elem) -> Elem div 100 end;
                    %
                    3 ->
                        fun(Elem) -> Elem * 100.0 end
                end,

            ?assertEqual(
                gb_sets:to_list(gb_sets:map(MapFun, RefSet)),
                b5_sets:to_list(b5_sets:map(MapFun, TestSet))
            )
        end
    ).

test_filter_operations(_Config) ->
    foreach_set(
        fun(RefSet, TestSet) ->
            RefList = gb_sets:to_list(RefSet),

            FilterFun =
                case rand:uniform(3) of
                    1 ->
                        fun(_Elem) -> false end;
                    %
                    2 ->
                        fun(Elem) -> trunc(Elem) rem 2 =:= 0 end;
                    %
                    3 ->
                        FilteredAmount = rand:uniform(length(RefList) + 1) - 1,
                        FilteredElements = gb_sets:from_list(take_random(RefList, FilteredAmount)),
                        fun(Elem) -> gb_sets:is_element(Elem, FilteredElements) end
                end,

            ?assertEqual(
                gb_sets:to_list(gb_sets:filter(FilterFun, RefSet)),
                b5_sets:to_list(b5_sets:filter(FilterFun, TestSet))
            )
        end
    ).

test_filtermap_operations(_Config) ->
    foreach_set(
        fun(RefSet, TestSet) ->
            RefList = gb_sets:to_list(RefSet),

            FilterFun =
                case rand:uniform(3) of
                    1 ->
                        fun(_Elem) -> false end;
                    %
                    2 ->
                        fun(Elem) -> trunc(Elem) rem 2 =:= 0 end;
                    %
                    3 ->
                        FilteredAmount = rand:uniform(length(RefList) + 1) - 1,
                        FilteredList = take_random(RefList, FilteredAmount),
                        FilteredElements = gb_sets:from_list(FilteredList),

                        MappedAmount = rand:uniform(length(FilteredList) + 1) - 1,
                        MappedElements = gb_sets:from_list(take_random(FilteredList, MappedAmount)),

                        fun(Elem) ->
                            case gb_sets:is_element(Elem, FilteredElements) of
                                true ->
                                    case gb_sets:is_element(Elem, MappedElements) of
                                        true ->
                                            {true, Elem div 2};
                                        false ->
                                            true
                                    end;
                                false ->
                                    false
                            end
                        end
                end,

            ?assertEqual(
                gb_sets:to_list(gb_sets:filtermap(FilterFun, RefSet)),
                b5_sets:to_list(b5_sets:filtermap(FilterFun, TestSet))
            )
        end
    ).

%% ------------------------------------------------------------------
%% Compatibility Operation Tests
%% ------------------------------------------------------------------

test_compatibility_aliases(_Config) ->
    Set1 = b5_sets:new(),

    %% add_element/2 should behave like add/2
    Set2 = b5_sets:add_element(42, Set1),
    Set3 = b5_sets:add(42, Set1),
    ?assertEqual(Set2, Set3),

    %% del_element/2 should behave like delete_any/2
    Set4 = b5_sets:del_element(42, Set2),
    Set5 = b5_sets:delete_any(42, Set2),
    ?assertEqual(Set4, Set5),

    %% is_element/2 should behave like is_member/2
    ?assertEqual(b5_sets:is_element(42, Set2), b5_sets:is_member(42, Set2)),
    ?assertEqual(b5_sets:is_element(99, Set2), b5_sets:is_member(99, Set2)),

    %% subtract/2 should behave like difference/2
    SetA = b5_sets:from_list([1, 2, 3]),
    SetB = b5_sets:from_list([2, 3, 4]),
    SubtractResult = b5_sets:subtract(SetA, SetB),
    DifferenceResult = b5_sets:difference(SetA, SetB),
    ?assertEqual(SubtractResult, DifferenceResult).

%% ------------------------------------------------------------------
%% Empty Set Edge Case Tests
%% ------------------------------------------------------------------

test_empty_set_exceptions(_Config) ->
    % TODO we may not need this
    EmptySet = b5_sets:new(),

    %% Test operations on empty sets
    ?assertError({badkey, arbitrary_element}, b5_sets:delete(arbitrary_element, EmptySet)),
    ?assertEqual(none, b5_sets:smaller(arbitrary_element, EmptySet)),
    ?assertEqual(none, b5_sets:larger(arbitrary_element, EmptySet)),
    ?assertError(empty_set, b5_sets:smallest(EmptySet)),
    ?assertError(empty_set, b5_sets:largest(EmptySet)),
    ?assertError(empty_set, b5_sets:take_smallest(EmptySet)),
    ?assertError(empty_set, b5_sets:take_largest(EmptySet)),

    %% Test with various element types
    TestElements = [
        42,
        atom,
        "string",
        [1, 2, 3],
        {tuple, element},
        3.14159,
        #{map => element}
    ],

    lists:foreach(
        fun(Element) ->
            ?assertError({badkey, Element}, b5_sets:delete(Element, EmptySet)),
            ?assertEqual(none, b5_sets:smaller(Element, EmptySet)),
            ?assertEqual(none, b5_sets:larger(Element, EmptySet))
        end,
        TestElements
    ),

    %% Verify these operations work fine on non-empty sets
    NonEmptySet = b5_sets:from_list([1, 2, 3]),

    %% These should work without throwing exceptions

    % smallest has no smaller
    ?assertEqual(none, b5_sets:smaller(1, NonEmptySet)),
    % largest has no larger
    ?assertEqual(none, b5_sets:larger(3, NonEmptySet)),
    ?assertEqual({found, 1}, b5_sets:smaller(2, NonEmptySet)),
    ?assertEqual({found, 3}, b5_sets:larger(2, NonEmptySet)),

    %% These should throw badkey for non-existent elements
    ?assertError({badkey, 99}, b5_sets:delete(99, NonEmptySet)),

    %% These should work for elements that exist
    {Smallest, _} = b5_sets:take_smallest(NonEmptySet),
    ?assertEqual(1, Smallest),

    {Largest, _} = b5_sets:take_largest(NonEmptySet),
    ?assertEqual(3, Largest).

%% ------------------------------------------------------------------
%% Helper Functions
%% ------------------------------------------------------------------

foreach_set(Fun) ->
    lists:foreach(
        fun(Size) ->
            ElementsList = generate_unique_elements_list(Size),
            RefSet = gb_sets:from_list(shuffle_list(ElementsList)),
            TestSet = b5_sets:from_list(shuffle_list(ElementsList)),
            Fun(RefSet, TestSet)
        end,
        ?TESTED_SET_SIZES
    ).

foreach_rebuilt_variant(TestSet, Fun) ->
    lists:foreach(
        fun(_) ->
            List = b5_sets:to_list(TestSet),
            ShuffledRetyped = randomly_switch_types(shuffle_list(List)),
            Fun(b5_sets:from_list(ShuffledRetyped))
        end,
        lists:seq(1, 10)
    ).

generate_unique_elements_list(Size) ->
    generate_unique_elements_list_recur(Size, #{}).

generate_unique_elements_list_recur(Size, Acc) when Size > 0 ->
    New = rand:uniform(10000),

    case is_map_key(New, Acc) of
        false ->
            UpdatedAcc = maps:put(New, value, Acc),
            generate_unique_elements_list_recur(Size - 1, UpdatedAcc);
        %
        true ->
            generate_unique_elements_list_recur(Size, Acc)
    end;
generate_unique_elements_list_recur(0, Acc) ->
    maps:keys(Acc).

generate_new_element_not_in_set(RefSet) ->
    generate_new_element_not_in_set(RefSet, 1000).

generate_new_element_not_in_set(RefSet, Candidate) ->
    case gb_sets:is_member(Candidate, RefSet) of
        true -> generate_new_element_not_in_set(RefSet, Candidate + 1);
        false -> Candidate
    end.

%% @doc Take N random elements from a list
take_random(List, N) when N >= length(List) ->
    List;
take_random(List, N) when N > 0 ->
    Shuffled = shuffle_list(List),
    lists:sublist(Shuffled, N);
take_random(_List, 0) ->
    [].

%% @doc Shuffle a list randomly
shuffle_list(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- List])].

%% @doc Iterate through all elements using iterator
iterate_to_list(Iter) ->
    iterate_to_list(Iter, []).

iterate_to_list(Iter, Acc) ->
    case b5_sets:next(Iter) of
        none -> lists:reverse(Acc);
        {Element, NextIter} -> iterate_to_list(NextIter, [Element | Acc])
    end.

ref_iterate_to_list(Iter) ->
    ref_iterate_to_list(Iter, []).

ref_iterate_to_list(Iter, Acc) ->
    case gb_sets:next(Iter) of
        none -> lists:reverse(Acc);
        {Element, NextIter} -> ref_iterate_to_list(NextIter, [Element | Acc])
    end.

fold_randomly_retyped_shuffled_elements(Fun, Acc0, RefSet) ->
    lists:foldl(
        fun(Element, Acc) ->
            Fun(randomly_switch_type(Element), Acc)
        end,
        Acc0,
        shuffle_list(gb_sets:to_list(RefSet))
    ).

foreach_randomly_retyped_element(Fun, RefSet) ->
    gb_sets:fold(
        fun(Element, _) ->
            Fun(randomly_switch_type(Element)),
            ok
        end,
        ok,
        RefSet
    ).

foreach_non_existent_element(Fun, RefSet, Amount) ->
    lists:foreach(
        fun(_) ->
            Element = randomly_switch_type(generate_new_element_not_in_set(RefSet)),
            Fun(Element)
        end,
        lists:seq(1, Amount)
    ).

randomly_switch_types(List) ->
    lists:map(fun randomly_switch_type/1, List).

randomly_switch_type(Element) ->
    case rand:uniform(2) of
        1 when is_integer(Element) ->
            float(Element);
        %
        1 when is_float(Element) ->
            trunc(Element);
        %
        2 ->
            Element
    end.

diff_lists([A | NextA], [B | NextB]) ->
    if
        A == B ->
            diff_lists(NextA, NextB);
        %
        true ->
            [{different, A, B}]
    end;
diff_lists([], []) ->
    [].
