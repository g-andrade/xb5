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

%% Test exports - Filtermap collision edge cases
-export([
    test_filtermap_collision_handling/1
]).

%% Test exports - Validation edge cases
-export([
    test_validation_edge_cases/1
]).

%% Test constants
-define(REGULAR_SET_SIZES,
    (lists:seq(0, 50) ++ lists:seq(55, 200, 5) ++ [997])
).

%% Node type constants (from b5_sets_node.erl)
-define(LEAF0, leaf0).

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
            test_smaller_larger_operations
            % FIXME
            % test_smallest_largest_operations,
            % test_take_smallest_operations,
            % test_take_largest_operations
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
        ]},
        {filtermap_collision_edge_cases, [parallel], [
            test_filtermap_collision_handling
        ]},
        {validation_edge_cases, [parallel], [
            test_validation_edge_cases
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
    foreach_set2(fun(RefSet, TestSet) ->
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
    foreach_set2(fun(RefSet, TestSet) ->
        ExpectedSize = gb_sets:size(RefSet),
        ?assertEqual(ExpectedSize, b5_sets:size(TestSet)),
        ?assertEqual(ExpectedSize =:= 0, b5_sets:is_empty(TestSet))
    end).

%% ------------------------------------------------------------------
%% Membership Operation Tests
%% ------------------------------------------------------------------

test_add_operations(_Config) ->
    foreach_set2(fun(RefSet, TestSet) ->
        %% Add existing elements - should not change set
        foreach_randomly_retyped_element2(
            fun(Element) ->
                UpdatedSet = b5_sets:add(Element, TestSet),
                ?assertEqual(TestSet, UpdatedSet)
            end,
            RefSet
        ),

        foreach_non_existent_element2(
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
    foreach_set2(fun(RefSet, TestSet) ->
        foreach_randomly_retyped_element2(
            fun(Element) ->
                %% Insert existing elements - should throw error
                ?assertError({key_exists, Element}, b5_sets:insert(Element, TestSet))
            end,
            RefSet
        ),

        foreach_non_existent_element2(
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
    foreach_set2(fun(RefSet, TestSet) ->
        %% Delete existing elements
        fold_randomly_retyped_shuffled_elements2(
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

        foreach_non_existent_element2(
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
    foreach_set2(fun(RefSet, TestSet) ->
        %% Delete existing elements
        fold_randomly_retyped_shuffled_elements2(
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

        foreach_non_existent_element2(
            fun(NonExistentElement) ->
                %% Delete non-existent element - keeps the set as is
                ?assertEqual(TestSet, b5_sets:delete_any(NonExistentElement, TestSet))
            end,
            RefSet,
            50
        )
    end).

test_is_member_operations(_Config) ->
    foreach_set2(fun(RefSet, TestSet) ->
        %% Test existing elements
        foreach_randomly_retyped_element2(
            fun(Element) ->
                ?assertEqual(true, b5_sets:is_member(Element, TestSet))
            end,
            RefSet
        ),

        %% Test non-existent elements
        foreach_non_existent_element2(
            fun(NonExistentElement) ->
                ?assertEqual(false, b5_sets:is_member(NonExistentElement, TestSet))
            end,
            RefSet,
            50
        )
    end).

test_is_set_operations(_Config) ->
    foreach_set2(fun(RefSet, TestSet) ->
        ?assertEqual(false, b5_sets:is_set(RefSet)),
        ?assertEqual(true, b5_sets:is_set(TestSet))
    end).

%% ------------------------------------------------------------------
%% Set Operation Tests
%% ------------------------------------------------------------------

%%%%%%%%%%%%%
%% Union

test_union_operations(_Config) ->
    foreach_set2(
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
    foreach_set2(
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
    foreach_set2(
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
    foreach_set2(
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
    foreach_set2(
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
    foreach_set2(
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
    foreach_set2(fun(RefSet, TestSet) ->
        TestIter = b5_sets:iterator(TestSet),
        IteratedElements = iterate_to_list(TestIter),
        ?assertEqual(gb_sets:to_list(RefSet), IteratedElements),

        ?assertEqual(gb_sets:size(RefSet), length(IteratedElements)),
        ?assertEqual(gb_sets:size(RefSet), b5_sets:size(TestSet))
    end).

test_iterator_ordered_operations(_Config) ->
    foreach_set2(fun(RefSet, TestSet) ->
        TestIter = b5_sets:iterator(TestSet, ordered),
        IteratedElements = iterate_to_list(TestIter),
        ?assertEqual(gb_sets:to_list(RefSet), IteratedElements),

        ?assertEqual(gb_sets:size(RefSet), length(IteratedElements)),
        ?assertEqual(gb_sets:size(RefSet), b5_sets:size(TestSet))
    end).

test_iterator_reversed_operations(_Config) ->
    foreach_set2(fun(RefSet, TestSet) ->
        TestIter = b5_sets:iterator(TestSet, reversed),
        IteratedElements = iterate_to_list(TestIter),
        ?assertEqual(gb_sets:to_list(RefSet), lists:reverse(IteratedElements)),

        ?assertEqual(gb_sets:size(RefSet), length(IteratedElements)),
        ?assertEqual(gb_sets:size(RefSet), b5_sets:size(TestSet))
    end).

test_iterator_from_operations(_Config) ->
    foreach_set2(fun(RefSet, TestSet) ->
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
    foreach_set2(fun(RefSet, TestSet) ->
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
    foreach_set2(fun(RefSet, TestSet) ->
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
    EmptySet = b5_sets:new(),

    %% Empty set operations behavior
    ?assertEqual(none, b5_sets:smaller(42, EmptySet)),
    ?assertEqual(none, b5_sets:larger(42, EmptySet)),
    ?assertError({badkey, 42}, b5_sets:delete(42, EmptySet)),

    Set = b5_sets:from_list([1, 3, 5, 7, 9, 11]),

    %% Smaller operations - find greatest element < given element
    ?assertEqual({found, 3}, b5_sets:smaller(5, Set)),
    ?assertEqual({found, 7}, b5_sets:smaller(9, Set)),
    % No element < 1
    ?assertEqual(none, b5_sets:smaller(1, Set)),
    ?assertEqual({found, 9}, b5_sets:smaller(11, Set)),

    %% Larger operations - find least element > given element
    ?assertEqual({found, 7}, b5_sets:larger(5, Set)),
    ?assertEqual({found, 3}, b5_sets:larger(1, Set)),
    % No element > 11
    ?assertEqual(none, b5_sets:larger(11, Set)),
    ?assertEqual({found, 5}, b5_sets:larger(3, Set)),

    %% Test with non-existent elements

    % 4 not in set, find < 4
    ?assertEqual({found, 3}, b5_sets:smaller(4, Set)),
    % 4 not in set, find > 4
    ?assertEqual({found, 5}, b5_sets:larger(4, Set)),
    % 0 < all elements
    ?assertEqual(none, b5_sets:smaller(0, Set)),
    % 20 > all elements
    ?assertEqual(none, b5_sets:larger(20, Set)),

    %% Property-based testing
    foreach_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
        SortedElements = lists:usort(ElementsList),
        case SortedElements of
            [] ->
                %% Empty sets return none for smaller/larger
                ?assertEqual(none, b5_sets:smaller(test_element, TestSet)),
                ?assertEqual(none, b5_sets:larger(test_element, TestSet));
            [Single] ->
                %% Single element - no smaller/larger elements
                ?assertEqual(none, b5_sets:smaller(Single, TestSet)),
                ?assertEqual(none, b5_sets:larger(Single, TestSet));
            [First | Rest] ->
                %% Test smallest element has no smaller
                ?assertEqual(none, b5_sets:smaller(First, TestSet)),
                %% Test largest element has no larger
                Largest = lists:last(SortedElements),
                ?assertEqual(none, b5_sets:larger(Largest, TestSet)),
                %% Test middle elements have correct smaller/larger
                test_smaller_larger_middle_elements(TestSet, SortedElements)
        end
    end).

test_smallest_largest_operations(_Config) ->
    EmptySet = b5_sets:new(),
    ?assertError(empty_set, b5_sets:smallest(EmptySet)),
    ?assertError(empty_set, b5_sets:largest(EmptySet)),

    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        SortedElements = lists:usort(ElementsList),
        ExpectedSmallest = hd(SortedElements),
        ExpectedLargest = lists:last(SortedElements),

        ?assertEqual(ExpectedSmallest, b5_sets:smallest(Set)),
        ?assertEqual(ExpectedLargest, b5_sets:largest(Set))
    end).

test_take_smallest_operations(_Config) ->
    EmptySet = b5_sets:new(),
    ?assertError(empty_set, b5_sets:take_smallest(EmptySet)),

    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        SortedElements = lists:usort(ElementsList),
        ExpectedSmallest = hd(SortedElements),
        ExpectedRemainingElements = tl(SortedElements),

        {TakenSmallest, RemainingSet} = b5_sets:take_smallest(Set),
        ?assertEqual(ExpectedSmallest, TakenSmallest),
        ?assertEqual(ExpectedRemainingElements, b5_sets:to_list(RemainingSet)),
        ?assertEqual(b5_sets:size(Set) - 1, b5_sets:size(RemainingSet))
    end).

test_take_largest_operations(_Config) ->
    EmptySet = b5_sets:new(),
    ?assertError(empty_set, b5_sets:take_largest(EmptySet)),

    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        SortedElements = lists:usort(ElementsList),
        ExpectedLargest = lists:last(SortedElements),
        ExpectedRemainingElements = lists:droplast(SortedElements),

        {TakenLargest, RemainingSet} = b5_sets:take_largest(Set),
        ?assertEqual(ExpectedLargest, TakenLargest),
        ?assertEqual(ExpectedRemainingElements, b5_sets:to_list(RemainingSet)),
        ?assertEqual(b5_sets:size(Set) - 1, b5_sets:size(RemainingSet))
    end).

%% ------------------------------------------------------------------
%% Higher-Order Operation Tests
%% ------------------------------------------------------------------

test_fold_operations(_Config) ->
    EmptySet = b5_sets:new(),
    ?assertEqual(0, b5_sets:fold(fun(_, Acc) -> Acc + 1 end, 0, EmptySet)),

    Set = b5_sets:from_list([1, 2, 3, 4, 5]),

    %% Basic fold operations
    Sum = b5_sets:fold(fun(Element, Acc) -> Element + Acc end, 0, Set),
    ?assertEqual(15, Sum),

    Count = b5_sets:fold(fun(_, Acc) -> Acc + 1 end, 0, Set),
    ?assertEqual(5, Count),

    Collected = b5_sets:fold(fun(Element, Acc) -> [Element | Acc] end, [], Set),
    ?assertEqual([5, 4, 3, 2, 1], Collected),

    %% Property-based testing with various set sizes
    foreach_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),

        %% Test element counting
        FoldCount = b5_sets:fold(fun(_, Acc) -> Acc + 1 end, 0, TestSet),
        ?assertEqual(length(UniqueElements), FoldCount),

        %% Test sum (only with numeric elements)
        case lists:all(fun(E) -> is_integer(E) end, UniqueElements) of
            true ->
                ExpectedSum = lists:sum(UniqueElements),
                FoldSum = b5_sets:fold(fun(E, Acc) -> E + Acc end, 0, TestSet),
                ?assertEqual(ExpectedSum, FoldSum);
            false ->
                ok
        end,

        %% Test element collection preserves all elements
        FoldList = b5_sets:fold(fun(E, Acc) -> [E | Acc] end, [], TestSet),
        ?assertEqual(UniqueElements, lists:usort(FoldList))
    end).

test_map_operations(_Config) ->
    EmptySet = b5_sets:new(),
    MappedEmpty = b5_sets:map(fun(X) -> X * 2 end, EmptySet),
    ?assertEqual(EmptySet, MappedEmpty),

    Set = b5_sets:from_list([1, 2, 3]),

    %% Basic map operations
    DoubledSet = b5_sets:map(fun(X) -> X * 2 end, Set),
    ?assertEqual([2, 4, 6], b5_sets:to_list(DoubledSet)),

    IdentitySet = b5_sets:map(fun(X) -> X end, Set),
    ?assertEqual(Set, IdentitySet),

    ConstantSet = b5_sets:map(fun(_) -> constant end, Set),
    ?assertEqual([constant], b5_sets:to_list(ConstantSet)),
    ?assertEqual(1, b5_sets:size(ConstantSet)),

    %% Property-based testing with various set sizes
    foreach_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),

        %% Test identity mapping (use logical equality)
        IdentityMapped = b5_sets:map(fun(X) -> X end, TestSet),
        ?assertEqual(b5_sets:to_list(TestSet), b5_sets:to_list(IdentityMapped)),

        %% Test constant mapping
        ConstantMapped = b5_sets:map(fun(_) -> mapped_constant end, TestSet),
        case UniqueElements of
            [] ->
                ?assertEqual(EmptySet, ConstantMapped);
            _ ->
                ?assertEqual([mapped_constant], b5_sets:to_list(ConstantMapped)),
                ?assertEqual(1, b5_sets:size(ConstantMapped))
        end,

        %% Test numeric doubling (only for integer elements)
        case lists:all(fun(E) -> is_integer(E) end, UniqueElements) of
            true ->
                DoubledMapped = b5_sets:map(fun(X) -> X * 2 end, TestSet),
                ExpectedDoubled = lists:usort([X * 2 || X <- UniqueElements]),
                ?assertEqual(ExpectedDoubled, b5_sets:to_list(DoubledMapped));
            false ->
                ok
        end
    end).

test_filter_operations(_Config) ->
    EmptySet = b5_sets:new(),
    FilteredEmpty = b5_sets:filter(fun(_) -> true end, EmptySet),
    ?assertEqual(EmptySet, FilteredEmpty),

    Set = b5_sets:from_list([1, 2, 3, 4, 5, 6]),

    %% Basic filter operations
    EvenSet = b5_sets:filter(fun(X) -> X rem 2 =:= 0 end, Set),
    ?assertEqual([2, 4, 6], b5_sets:to_list(EvenSet)),

    AllSet = b5_sets:filter(fun(_) -> true end, Set),
    ?assertEqual(Set, AllSet),

    NoneSet = b5_sets:filter(fun(_) -> false end, Set),
    ?assertEqual(EmptySet, NoneSet),

    %% Property-based testing with various set sizes
    foreach_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),

        %% Test true predicate (identity) - use logical equality
        AllFiltered = b5_sets:filter(fun(_) -> true end, TestSet),
        ?assertEqual(b5_sets:to_list(TestSet), b5_sets:to_list(AllFiltered)),

        %% Test false predicate (empty result)
        NoneFiltered = b5_sets:filter(fun(_) -> false end, TestSet),
        ?assertEqual(EmptySet, NoneFiltered),

        %% Test numeric even filter (only for integer elements)
        case lists:all(fun(E) -> is_integer(E) end, UniqueElements) of
            true ->
                EvenFiltered = b5_sets:filter(fun(X) -> X rem 2 =:= 0 end, TestSet),
                ExpectedEven = lists:usort([X || X <- UniqueElements, X rem 2 =:= 0]),
                ?assertEqual(ExpectedEven, b5_sets:to_list(EvenFiltered));
            false ->
                ok
        end
    end).

test_filtermap_operations(_Config) ->
    EmptySet = b5_sets:new(),
    FiltermappedEmpty = b5_sets:filtermap(fun(X) -> {true, X * 2} end, EmptySet),
    ?assertEqual(EmptySet, FiltermappedEmpty),

    Set = b5_sets:from_list([1, 2, 3, 4, 5]),

    %% Basic filtermap operations
    EvenDoubledSet = b5_sets:filtermap(
        fun(X) ->
            case X rem 2 =:= 0 of
                true -> {true, X * 2};
                false -> false
            end
        end,
        Set
    ),
    ?assertEqual([4, 8], b5_sets:to_list(EvenDoubledSet)),

    AllDoubledSet = b5_sets:filtermap(fun(X) -> {true, X * 2} end, Set),
    ?assertEqual([2, 4, 6, 8, 10], b5_sets:to_list(AllDoubledSet)),

    NoneSet = b5_sets:filtermap(fun(_) -> false end, Set),
    ?assertEqual(EmptySet, NoneSet),

    %% Property-based testing with various set sizes
    foreach_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),

        %% Test always true filtermap
        AllMapped = b5_sets:filtermap(fun(X) -> {true, {mapped, X}} end, TestSet),
        ExpectedMapped = lists:usort([{mapped, X} || X <- UniqueElements]),
        ?assertEqual(ExpectedMapped, b5_sets:to_list(AllMapped)),

        %% Test always false filtermap
        NoneMapped = b5_sets:filtermap(fun(_) -> false end, TestSet),
        ?assertEqual(EmptySet, NoneMapped),

        %% Test numeric even doubling (only for integer elements)
        case lists:all(fun(E) -> is_integer(E) end, UniqueElements) of
            true ->
                EvenDoubleMapped = b5_sets:filtermap(
                    fun(X) ->
                        case X rem 2 =:= 0 of
                            true -> {true, X * 2};
                            false -> false
                        end
                    end,
                    TestSet
                ),
                ExpectedEvenDoubled = lists:usort([X * 2 || X <- UniqueElements, X rem 2 =:= 0]),
                ?assertEqual(ExpectedEvenDoubled, b5_sets:to_list(EvenDoubleMapped));
            false ->
                ok
        end
    end).

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
%% Filtermap Collision Edge Case Tests
%% ------------------------------------------------------------------

test_filtermap_collision_handling(_Config) ->
    %% Test filtermap when multiple elements map to the same result
    %% This should trigger the collision handling code paths in b5_sets_node

    %% Basic collision test - map multiple elements to same value
    Set1 = b5_sets:from_list([1, 2, 3, 4, 5]),

    %% Map all odd numbers to 'odd' and all even numbers to 'even'
    OddEvenSet = b5_sets:filtermap(
        fun(X) ->
            case X rem 2 of
                % Multiple elements map to 'odd'
                1 -> {true, odd};
                % Multiple elements map to 'even'
                0 -> {true, even}
            end
        end,
        Set1
    ),

    %% Should result in set with just {even, odd}
    Result1 = b5_sets:to_list(OddEvenSet),
    ?assertEqual(2, b5_sets:size(OddEvenSet)),
    ?assert(lists:member(odd, Result1)),
    ?assert(lists:member(even, Result1)),

    %% Test with more complex collision scenarios
    Set2 = b5_sets:from_list([10, 11, 12, 13, 14, 15, 16, 17, 18, 19]),

    %% Map to last digit - multiple collisions
    LastDigitSet = b5_sets:filtermap(
        fun(X) ->
            LastDigit = X rem 10,
            {true, LastDigit}
        end,
        Set2
    ),

    %% Should have digits 0-9
    Result2 = b5_sets:to_list(LastDigitSet),
    ExpectedDigits = lists:seq(0, 9),
    ?assertEqual(ExpectedDigits, Result2),
    ?assertEqual(10, b5_sets:size(LastDigitSet)),

    %% Test collision with larger sets (more likely to hit edge cases)
    LargeSet = b5_sets:from_list(lists:seq(1, 50)),

    %% Map to modulo 7 - creates multiple collisions
    ModuloSet = b5_sets:filtermap(
        fun(X) ->
            Modulo = X rem 7,
            {true, Modulo}
        end,
        LargeSet
    ),

    Result3 = b5_sets:to_list(ModuloSet),
    ExpectedModulos = lists:seq(0, 6),
    ?assertEqual(ExpectedModulos, Result3),
    ?assertEqual(7, b5_sets:size(ModuloSet)),

    %% Test with selective filtering + collisions
    Set3 = b5_sets:from_list([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),

    %% Only map even numbers, but map to collision-prone values
    SelectiveSet = b5_sets:filtermap(
        fun(X) ->
            case X rem 2 =:= 0 of
                % 2->1, 4->2, 6->3, 8->4, 10->5
                true -> {true, X div 2};
                false -> false
            end
        end,
        Set3
    ),

    Result4 = b5_sets:to_list(SelectiveSet),
    ?assertEqual([1, 2, 3, 4, 5], Result4),
    ?assertEqual(5, b5_sets:size(SelectiveSet)),

    %% Test extreme collision case - all elements map to same value
    Set4 = b5_sets:from_list([a, b, c, d, e]),

    AllSameSet = b5_sets:filtermap(
        fun(_) ->
            {true, collision_value}
        end,
        Set4
    ),

    Result5 = b5_sets:to_list(AllSameSet),
    ?assertEqual([collision_value], Result5),
    ?assertEqual(1, b5_sets:size(AllSameSet)),

    %% Test collision with different data types
    MixedSet = b5_sets:from_list([1, 2, atom, "string", {tuple}]),

    TypeMappedSet = b5_sets:filtermap(
        fun(X) ->
            Type =
                case X of
                    I when is_integer(I) -> integer;
                    A when is_atom(A) -> atom;
                    S when is_list(S) -> string;
                    T when is_tuple(T) -> tuple
                end,
            {true, Type}
        end,
        MixedSet
    ),

    Result6 = b5_sets:to_list(TypeMappedSet),
    ?assertEqual(4, b5_sets:size(TypeMappedSet)),
    ?assert(lists:member(integer, Result6)),
    ?assert(lists:member(atom, Result6)),
    ?assert(lists:member(string, Result6)),
    ?assert(lists:member(tuple, Result6)).

%% ------------------------------------------------------------------
%% Validation Edge Case Tests
%% ------------------------------------------------------------------

test_validation_edge_cases(_Config) ->
    %% Test normal sets return true for is_set
    EmptySet = b5_sets:new(),
    ?assert(b5_sets:is_set(EmptySet)),

    NormalSet = b5_sets:from_list([1, 2, 3]),
    ?assert(b5_sets:is_set(NormalSet)),

    %% Test invalid root structures to hit does_root_look_legit fallback case
    %% We need to create invalid b5_sets records to trigger the fallback

    %% Test with bad root type (not a valid node structure)
    InvalidSet1 = {b5_sets, 1, invalid_root},
    ?assertNot(b5_sets:is_set(InvalidSet1)),

    %% Test with mismatched size and root

    % Size 999 but empty root
    InvalidSet2 = {b5_sets, 999, ?LEAF0},
    ?assertNot(b5_sets:is_set(InvalidSet2)),

    %% Test with negative size
    InvalidSet3 = {b5_sets, -1, some_root},
    ?assertNot(b5_sets:is_set(InvalidSet3)),

    %% Test with non-integer size
    InvalidSet4 = {b5_sets, not_a_number, some_root},
    ?assertNot(b5_sets:is_set(InvalidSet4)),

    %% Test with atom as root
    InvalidSet5 = {b5_sets, 0, atom_root},
    ?assertNot(b5_sets:is_set(InvalidSet5)),

    %% Test with list as root
    InvalidSet6 = {b5_sets, 1, [list, as, root]},
    ?assertNot(b5_sets:is_set(InvalidSet6)),

    %% Test with tuple of wrong arity as root
    InvalidSet7 = {b5_sets, 1, {wrong, arity, tuple}},
    ?assertNot(b5_sets:is_set(InvalidSet7)),

    %% Test proper LEAF0 with size 0 (should be valid)
    ValidEmptySet = {b5_sets, 0, ?LEAF0},
    ?assert(b5_sets:is_set(ValidEmptySet)).

%% ------------------------------------------------------------------
%% Helper Functions
%% ------------------------------------------------------------------

foreach_set2(Fun) ->
    lists:foreach(
        fun(Size) ->
            ElementsList = generate_unique_elements_list(Size),
            RefSet = gb_sets:from_list(shuffle_list(ElementsList)),
            TestSet = b5_sets:from_list(shuffle_list(ElementsList)),
            Fun(RefSet, TestSet)
        end,
        ?REGULAR_SET_SIZES
    ).

%% @doc Iterate through set elements for different set sizes
foreach_set(Sizes, Fun) ->
    lists:foreach(
        fun(Size) ->
            ElementsList = lists:sort(generate_unique_elements_list(Size)),
            Set = b5_sets:from_list(ElementsList),
            Fun(Set, ElementsList)
        end,
        Sizes
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

%% @doc Iterate through set pairs for different set sizes
foreach_set_pair(Sizes, Fun) ->
    lists:foreach(
        fun(SizeA) ->
            lists:foreach(
                fun(SizeB) ->
                    ElementsA = lists:sort(generate_unique_elements_list(SizeA)),
                    ElementsB = lists:sort(generate_unique_elements_list(SizeB)),
                    SetA = b5_sets:from_list(ElementsA),
                    SetB = b5_sets:from_list(ElementsB),
                    Fun(SetA, ElementsA, SetB, ElementsB)
                % Test against 10 random sizes
                end,
                take_random(Sizes, 10)
            )
        % Test 15 random sizes as first set
        end,
        take_random(Sizes, 15)
    ).

%% @doc Test smaller/larger for middle elements in sorted list
test_smaller_larger_middle_elements(Set, SortedElements) ->
    lists:foreach(
        fun(I) when I > 1, I < length(SortedElements) ->
            Element = lists:nth(I, SortedElements),
            ExpectedSmaller = lists:nth(I - 1, SortedElements),
            ExpectedLarger = lists:nth(I + 1, SortedElements),
            ?assertEqual({found, ExpectedSmaller}, b5_sets:smaller(Element, Set)),
            ?assertEqual({found, ExpectedLarger}, b5_sets:larger(Element, Set))
        end,
        lists:seq(2, length(SortedElements) - 1)
    ).

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

fold_randomly_retyped_shuffled_elements2(Fun, Acc0, RefSet) ->
    lists:foldl(
        fun(Element, Acc) ->
            Fun(randomly_switch_type(Element), Acc)
        end,
        Acc0,
        shuffle_list(gb_sets:to_list(RefSet))
    ).

foreach_randomly_retyped_element2(Fun, RefSet) ->
    gb_sets:fold(
        fun(Element, _) ->
            Fun(randomly_switch_type(Element)),
            ok
        end,
        ok,
        RefSet
    ).

foreach_non_existent_element2(Fun, RefSet, Amount) ->
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
