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

%% Test constants
-define(REGULAR_SET_SIZES,
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
    EmptySet = b5_sets:new(),
    ?assertEqual([], b5_sets:to_list(EmptySet)),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        Result = b5_sets:to_list(Set),
        Expected = lists:usort(ElementsList),
        ?assertEqual(Expected, Result)
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
    EmptySet = b5_sets:new(),
    ?assertEqual(0, b5_sets:size(EmptySet)),
    ?assertEqual(true, b5_sets:is_empty(EmptySet)),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        ExpectedSize = length(lists:usort(ElementsList)),
        ?assertEqual(ExpectedSize, b5_sets:size(Set)),
        ?assertEqual(ExpectedSize =:= 0, b5_sets:is_empty(Set))
    end).

%% ------------------------------------------------------------------
%% Membership Operation Tests
%% ------------------------------------------------------------------

test_add_operations(_Config) ->
    Set1 = b5_sets:new(),
    Set2 = b5_sets:add(42, Set1),
    ?assertEqual(1, b5_sets:size(Set2)),
    ?assertEqual(true, b5_sets:is_member(42, Set2)),

    %% Adding existing element should not change size
    Set3 = b5_sets:add(42, Set2),
    ?assertEqual(1, b5_sets:size(Set3)),
    ?assertEqual(Set2, Set3),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        %% Add existing elements - should not change set
        lists:foreach(
            fun(Element) ->
                UpdatedSet = b5_sets:add(Element, Set),
                ?assertEqual(Set, UpdatedSet)
            end,
            take_random(ElementsList, 10)
        ),

        %% Add new elements
        NewElement = generate_new_element_not_in(ElementsList),
        UpdatedSet = b5_sets:add(NewElement, Set),
        ?assertEqual(b5_sets:size(Set) + 1, b5_sets:size(UpdatedSet)),
        ?assertEqual(true, b5_sets:is_member(NewElement, UpdatedSet))
    end).

test_insert_operations(_Config) ->
    Set1 = b5_sets:new(),
    Set2 = b5_sets:insert(42, Set1),
    ?assertEqual(1, b5_sets:size(Set2)),
    ?assertEqual(true, b5_sets:is_member(42, Set2)),

    %% Inserting existing element should throw error
    ?assertError({key_exists, 42}, b5_sets:insert(42, Set2)),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        %% Insert existing elements - should throw error
        ExistingElement = lists:nth(rand:uniform(length(ElementsList)), ElementsList),
        ?assertError({key_exists, ExistingElement}, b5_sets:insert(ExistingElement, Set)),

        %% Insert new element
        NewElement = generate_new_element_not_in(ElementsList),
        UpdatedSet = b5_sets:insert(NewElement, Set),
        ?assertEqual(b5_sets:size(Set) + 1, b5_sets:size(UpdatedSet)),
        ?assertEqual(true, b5_sets:is_member(NewElement, UpdatedSet))
    end).

test_delete_operations(_Config) ->
    Set1 = b5_sets:singleton(42),
    Set2 = b5_sets:delete(42, Set1),
    ?assertEqual(0, b5_sets:size(Set2)),
    ?assertEqual(false, b5_sets:is_member(42, Set2)),

    %% Deleting non-existent element should throw error
    ?assertError({badkey, 99}, b5_sets:delete(99, Set1)),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        %% Delete existing elements
        ElementToDelete = lists:nth(rand:uniform(length(ElementsList)), ElementsList),
        UpdatedSet = b5_sets:delete(ElementToDelete, Set),
        ?assertEqual(b5_sets:size(Set) - 1, b5_sets:size(UpdatedSet)),
        ?assertEqual(false, b5_sets:is_member(ElementToDelete, UpdatedSet)),

        %% Delete non-existent element - should throw error
        NonExistentElement = generate_new_element_not_in(ElementsList),
        ?assertError({badkey, NonExistentElement}, b5_sets:delete(NonExistentElement, Set))
    end).

test_delete_any_operations(_Config) ->
    Set1 = b5_sets:singleton(42),
    Set2 = b5_sets:delete_any(42, Set1),
    ?assertEqual(0, b5_sets:size(Set2)),
    ?assertEqual(false, b5_sets:is_member(42, Set2)),

    %% Deleting non-existent element should not change set
    Set3 = b5_sets:delete_any(99, Set1),
    ?assertEqual(Set1, Set3),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        %% Delete existing element
        ElementToDelete = lists:nth(rand:uniform(length(ElementsList)), ElementsList),
        UpdatedSet = b5_sets:delete_any(ElementToDelete, Set),
        ?assertEqual(b5_sets:size(Set) - 1, b5_sets:size(UpdatedSet)),
        ?assertEqual(false, b5_sets:is_member(ElementToDelete, UpdatedSet)),

        %% Delete non-existent element - should not change set
        NonExistentElement = generate_new_element_not_in(ElementsList),
        UnchangedSet = b5_sets:delete_any(NonExistentElement, Set),
        ?assertEqual(Set, UnchangedSet)
    end).

test_is_member_operations(_Config) ->
    EmptySet = b5_sets:new(),
    ?assertEqual(false, b5_sets:is_member(42, EmptySet)),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        %% Test existing elements
        lists:foreach(
            fun(Element) ->
                ?assertEqual(true, b5_sets:is_member(Element, Set))
            end,
            take_random(ElementsList, 10)
        ),

        %% Test non-existent elements
        NonExistentElement = generate_new_element_not_in(ElementsList),
        ?assertEqual(false, b5_sets:is_member(NonExistentElement, Set))
    end).

test_is_set_operations(_Config) ->
    EmptySet = b5_sets:new(),
    ?assertEqual(true, b5_sets:is_set(EmptySet)),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, _ElementsList) ->
        ?assertEqual(true, b5_sets:is_set(Set))
    end).

%% ------------------------------------------------------------------
%% Set Operation Tests
%% ------------------------------------------------------------------

test_union_operations(_Config) ->
    EmptySet = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    Set2 = b5_sets:from_list([3, 4, 5]),

    %% Union with empty set
    ?assertEqual(Set1, b5_sets:union(Set1, EmptySet)),
    ?assertEqual(Set1, b5_sets:union(EmptySet, Set1)),

    %% Union of overlapping sets
    UnionSet = b5_sets:union(Set1, Set2),
    ?assertEqual([1, 2, 3, 4, 5], b5_sets:to_list(UnionSet)),
    ?assertEqual(5, b5_sets:size(UnionSet)),

    %% Union with self
    ?assertEqual(Set1, b5_sets:union(Set1, Set1)),

    %% Union of list of sets
    Set3 = b5_sets:from_list([6, 7]),
    UnionList = b5_sets:union([Set1, Set2, Set3]),
    ?assertEqual([1, 2, 3, 4, 5, 6, 7], b5_sets:to_list(UnionList)),

    %% Union of empty list
    ?assertEqual(b5_sets:new(), b5_sets:union([])).

test_intersection_operations(_Config) ->
    EmptySet = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    Set2 = b5_sets:from_list([3, 4, 5]),

    %% Intersection with empty set
    ?assertEqual(EmptySet, b5_sets:intersection(Set1, EmptySet)),
    ?assertEqual(EmptySet, b5_sets:intersection(EmptySet, Set1)),

    %% Intersection of overlapping sets
    IntersectionSet = b5_sets:intersection(Set1, Set2),
    ?assertEqual([3], b5_sets:to_list(IntersectionSet)),
    ?assertEqual(1, b5_sets:size(IntersectionSet)),

    %% Intersection with self
    ?assertEqual(Set1, b5_sets:intersection(Set1, Set1)),

    %% Intersection of disjoint sets
    Set3 = b5_sets:from_list([6, 7, 8]),
    DisjointIntersection = b5_sets:intersection(Set1, Set3),
    ?assertEqual(EmptySet, DisjointIntersection),

    %% Intersection of list of sets
    Set4 = b5_sets:from_list([2, 3, 4]),
    IntersectionList = b5_sets:intersection([Set1, Set2, Set4]),
    ?assertEqual([3], b5_sets:to_list(IntersectionList)),

    %% Intersection of empty list
    ?assertEqual(b5_sets:new(), b5_sets:intersection([])).

test_difference_operations(_Config) ->
    EmptySet = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    Set2 = b5_sets:from_list([3, 4, 5]),

    %% Difference with empty set
    ?assertEqual(Set1, b5_sets:difference(Set1, EmptySet)),
    ?assertEqual(EmptySet, b5_sets:difference(EmptySet, Set1)),

    %% Difference of overlapping sets
    DifferenceSet = b5_sets:difference(Set1, Set2),
    ?assertEqual([1, 2], b5_sets:to_list(DifferenceSet)),
    ?assertEqual(2, b5_sets:size(DifferenceSet)),

    %% Difference with self
    ?assertEqual(EmptySet, b5_sets:difference(Set1, Set1)),

    %% Difference of disjoint sets
    Set3 = b5_sets:from_list([6, 7, 8]),
    DisjointDifference = b5_sets:difference(Set1, Set3),
    ?assertEqual(Set1, DisjointDifference).

test_is_disjoint_operations(_Config) ->
    EmptySet = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    Set2 = b5_sets:from_list([3, 4, 5]),
    Set3 = b5_sets:from_list([6, 7, 8]),

    %% Empty set is disjoint with everything
    ?assertEqual(true, b5_sets:is_disjoint(EmptySet, Set1)),
    ?assertEqual(true, b5_sets:is_disjoint(Set1, EmptySet)),

    %% Overlapping sets are not disjoint
    ?assertEqual(false, b5_sets:is_disjoint(Set1, Set2)),

    %% Disjoint sets
    ?assertEqual(true, b5_sets:is_disjoint(Set1, Set3)),

    %% Set is not disjoint with itself (unless empty)
    ?assertEqual(false, b5_sets:is_disjoint(Set1, Set1)),
    ?assertEqual(true, b5_sets:is_disjoint(EmptySet, EmptySet)).

test_is_subset_operations(_Config) ->
    EmptySet = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    Set2 = b5_sets:from_list([2, 3]),
    Set3 = b5_sets:from_list([3, 4, 5]),

    %% Empty set is subset of everything
    ?assertEqual(true, b5_sets:is_subset(EmptySet, Set1)),
    ?assertEqual(true, b5_sets:is_subset(EmptySet, EmptySet)),

    %% Proper subset
    ?assertEqual(true, b5_sets:is_subset(Set2, Set1)),

    %% Not a subset
    ?assertEqual(false, b5_sets:is_subset(Set1, Set2)),
    ?assertEqual(false, b5_sets:is_subset(Set1, Set3)),

    %% Set is subset of itself
    ?assertEqual(true, b5_sets:is_subset(Set1, Set1)).

test_is_equal_operations(_Config) ->
    EmptySet1 = b5_sets:new(),
    EmptySet2 = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    % Same elements, different order
    Set2 = b5_sets:from_list([3, 2, 1]),
    Set3 = b5_sets:from_list([1, 2]),

    %% Empty sets are equal
    ?assertEqual(true, b5_sets:is_equal(EmptySet1, EmptySet2)),

    %% Sets with same elements are equal regardless of insertion order
    ?assertEqual(true, b5_sets:is_equal(Set1, Set2)),

    %% Sets with different elements are not equal
    ?assertEqual(false, b5_sets:is_equal(Set1, Set3)),
    ?assertEqual(false, b5_sets:is_equal(EmptySet1, Set1)),

    %% Set is equal to itself
    ?assertEqual(true, b5_sets:is_equal(Set1, Set1)).

%% ------------------------------------------------------------------
%% Iterator Operation Tests
%% ------------------------------------------------------------------

test_iterator_operations(_Config) ->
    EmptySet = b5_sets:new(),
    EmptyIter = b5_sets:iterator(EmptySet),
    ?assertEqual(none, b5_sets:next(EmptyIter)),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        Iter = b5_sets:iterator(Set),
        IteratedElements = iterate_to_list(Iter),
        ExpectedElements = lists:usort(ElementsList),
        ?assertEqual(ExpectedElements, IteratedElements)
    end).

test_iterator_ordered_operations(_Config) ->
    Set = b5_sets:from_list([5, 2, 8, 1, 9, 3]),

    OrderedIter = b5_sets:iterator(Set, ordered),
    OrderedElements = iterate_to_list(OrderedIter),
    ?assertEqual([1, 2, 3, 5, 8, 9], OrderedElements).

test_iterator_reversed_operations(_Config) ->
    Set = b5_sets:from_list([5, 2, 8, 1, 9, 3]),

    ReversedIter = b5_sets:iterator(Set, reversed),
    ReversedElements = iterate_to_list(ReversedIter),
    ?assertEqual([9, 8, 5, 3, 2, 1], ReversedElements).

test_iterator_from_operations(_Config) ->
    Set = b5_sets:from_list([1, 3, 5, 7, 9, 11]),

    %% Iterator from existing element
    Iter1 = b5_sets:iterator_from(5, Set),
    Elements1 = iterate_to_list(Iter1),
    ?assertEqual([5, 7, 9, 11], Elements1),

    %% Iterator from non-existing element (should start from next greater)
    Iter2 = b5_sets:iterator_from(6, Set),
    Elements2 = iterate_to_list(Iter2),
    ?assertEqual([7, 9, 11], Elements2),

    %% Iterator from element larger than all (should be empty)
    Iter3 = b5_sets:iterator_from(20, Set),
    ?assertEqual(none, b5_sets:next(Iter3)).

test_iterator_from_ordered_operations(_Config) ->
    Set = b5_sets:from_list([1, 3, 5, 7, 9, 11]),

    Iter = b5_sets:iterator_from(5, Set, ordered),
    Elements = iterate_to_list(Iter),
    ?assertEqual([5, 7, 9, 11], Elements).

test_iterator_from_reversed_operations(_Config) ->
    Set = b5_sets:from_list([1, 3, 5, 7, 9, 11]),

    Iter = b5_sets:iterator_from(5, Set, reversed),
    Elements = iterate_to_list(Iter),
    ?assertEqual([5, 3, 1], Elements).

%% ------------------------------------------------------------------
%% Range Operation Tests
%% ------------------------------------------------------------------

% FIXME smaller and larger don't do what the code below assumes they do

test_smaller_larger_operations(_Config) ->
    Set = b5_sets:from_list([1, 3, 5, 7, 9, 11]),

    %% Smaller elements
    SmallerSet1 = b5_sets:smaller(5, Set),
    ?assertEqual([1, 3], b5_sets:to_list(SmallerSet1)),

    SmallerSet2 = b5_sets:smaller(1, Set),
    ?assertEqual([], b5_sets:to_list(SmallerSet2)),

    %% Larger elements
    LargerSet1 = b5_sets:larger(5, Set),
    ?assertEqual([7, 9, 11], b5_sets:to_list(LargerSet1)),

    LargerSet2 = b5_sets:larger(11, Set),
    ?assertEqual([], b5_sets:to_list(LargerSet2)).

test_smallest_largest_operations(_Config) ->
    EmptySet = b5_sets:new(),
    ?assertError(empty_set, b5_sets:smallest(EmptySet)),
    ?assertError(empty_set, b5_sets:largest(EmptySet)),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        SortedElements = lists:usort(ElementsList),
        ExpectedSmallest = hd(SortedElements),
        ExpectedLargest = lists:last(SortedElements),

        ?assertEqual(ExpectedSmallest, b5_sets:smallest(Set)),
        ?assertEqual(ExpectedLargest, b5_sets:largest(Set))
    end).

test_take_smallest_operations(_Config) ->
    EmptySet = b5_sets:new(),
    ?assertError(empty_set, b5_sets:take_smallest(EmptySet)),

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
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

    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
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

    %% Sum all elements
    Sum = b5_sets:fold(fun(Element, Acc) -> Element + Acc end, 0, Set),
    ?assertEqual(15, Sum),

    %% Count elements
    Count = b5_sets:fold(fun(_, Acc) -> Acc + 1 end, 0, Set),
    ?assertEqual(5, Count),

    %% Collect elements in reverse order (fold is left-to-right)
    Collected = b5_sets:fold(fun(Element, Acc) -> [Element | Acc] end, [], Set),
    ?assertEqual([5, 4, 3, 2, 1], Collected).

test_map_operations(_Config) ->
    EmptySet = b5_sets:new(),
    MappedEmpty = b5_sets:map(fun(X) -> X * 2 end, EmptySet),
    ?assertEqual(EmptySet, MappedEmpty),

    Set = b5_sets:from_list([1, 2, 3]),

    %% Map to double each element
    DoubledSet = b5_sets:map(fun(X) -> X * 2 end, Set),
    ?assertEqual([2, 4, 6], b5_sets:to_list(DoubledSet)),

    %% Map with identity function
    IdentitySet = b5_sets:map(fun(X) -> X end, Set),
    ?assertEqual(Set, IdentitySet),

    %% Map with constant function (results in singleton or smaller set)
    ConstantSet = b5_sets:map(fun(_) -> constant end, Set),
    ?assertEqual([constant], b5_sets:to_list(ConstantSet)),
    ?assertEqual(1, b5_sets:size(ConstantSet)).

test_filter_operations(_Config) ->
    EmptySet = b5_sets:new(),
    FilteredEmpty = b5_sets:filter(fun(_) -> true end, EmptySet),
    ?assertEqual(EmptySet, FilteredEmpty),

    Set = b5_sets:from_list([1, 2, 3, 4, 5, 6]),

    %% Filter even numbers
    EvenSet = b5_sets:filter(fun(X) -> X rem 2 =:= 0 end, Set),
    ?assertEqual([2, 4, 6], b5_sets:to_list(EvenSet)),

    %% Filter all elements (true predicate)
    AllSet = b5_sets:filter(fun(_) -> true end, Set),
    ?assertEqual(Set, AllSet),

    %% Filter no elements (false predicate)
    NoneSet = b5_sets:filter(fun(_) -> false end, Set),
    ?assertEqual(EmptySet, NoneSet).

test_filtermap_operations(_Config) ->
    EmptySet = b5_sets:new(),
    FiltermappedEmpty = b5_sets:filtermap(fun(X) -> {true, X * 2} end, EmptySet),
    ?assertEqual(EmptySet, FiltermappedEmpty),

    Set = b5_sets:from_list([1, 2, 3, 4, 5]),

    %% Filtermap even numbers to their doubles
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

    %% Filtermap with always true
    AllDoubledSet = b5_sets:filtermap(fun(X) -> {true, X * 2} end, Set),
    ?assertEqual([2, 4, 6, 8, 10], b5_sets:to_list(AllDoubledSet)),

    %% Filtermap with always false
    NoneSet = b5_sets:filtermap(fun(_) -> false end, Set),
    ?assertEqual(EmptySet, NoneSet).

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
%% Helper Functions
%% ------------------------------------------------------------------

%% @doc Iterate through set elements for different set sizes
for_each_set(Sizes, Fun) ->
    lists:foreach(
        fun(Size) ->
            ElementsList = generate_elements_list(Size),
            Set = b5_sets:from_list(ElementsList),
            Fun(Set, ElementsList)
        end,
        Sizes
    ).

%% @doc Generate a list of random elements for testing
generate_elements_list(Size) ->
    [rand:uniform(Size * 10) || _ <- lists:seq(1, Size)].

%% @doc Generate a new element not present in the given list
generate_new_element_not_in(ElementsList) ->
    generate_new_element_not_in(ElementsList, 1000).

generate_new_element_not_in(ElementsList, Candidate) ->
    case lists:member(Candidate, ElementsList) of
        true -> generate_new_element_not_in(ElementsList, Candidate + 1);
        false -> Candidate
    end.

%% @doc Take N random elements from a list
take_random(List, N) when N >= length(List) ->
    List;
take_random(List, N) when N > 0 ->
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- List])],
    lists:sublist(Shuffled, N);
take_random(_List, 0) ->
    [].

%% @doc Iterate through all elements using iterator
iterate_to_list(Iter) ->
    iterate_to_list(Iter, []).

iterate_to_list(Iter, Acc) ->
    case b5_sets:next(Iter) of
        none -> lists:reverse(Acc);
        {Element, NextIter} -> iterate_to_list(NextIter, [Element | Acc])
    end.
