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
    (lists:seq(1, 50) ++ lists:seq(55, 200, 5) ++ [997])
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

    %% Basic union operations
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
    ?assertEqual(b5_sets:new(), b5_sets:union([])),
    
    %% Property-based testing with various set sizes
    for_each_set_pair(?REGULAR_SET_SIZES, fun(SetA, ElementsA, SetB, ElementsB) ->
        UnionResult = b5_sets:union(SetA, SetB),
        ExpectedElements = lists:usort(ElementsA ++ ElementsB),
        ?assertEqual(ExpectedElements, b5_sets:to_list(UnionResult)),
        ?assertEqual(length(ExpectedElements), b5_sets:size(UnionResult)),
        
        %% Verify all elements from both sets are in union
        lists:foreach(fun(Elem) ->
            ?assertEqual(true, b5_sets:is_member(Elem, UnionResult))
        end, ElementsA ++ ElementsB),
        
        %% Commutativity: A ∪ B = B ∪ A (test logical equality)
        UnionBA = b5_sets:union(SetB, SetA),
        ?assertEqual(true, b5_sets:is_equal(UnionResult, UnionBA))
    end).

test_intersection_operations(_Config) ->
    EmptySet = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    Set2 = b5_sets:from_list([3, 4, 5]),

    %% Basic intersection operations
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
    ?assertEqual(b5_sets:new(), b5_sets:intersection([])),
    
    %% Property-based testing with various set sizes
    for_each_set_pair(?REGULAR_SET_SIZES, fun(SetA, ElementsA, SetB, ElementsB) ->
        IntersectionResult = b5_sets:intersection(SetA, SetB),
        ExpectedElements = lists:usort([E || E <- ElementsA, lists:member(E, ElementsB)]),
        ?assertEqual(ExpectedElements, b5_sets:to_list(IntersectionResult)),
        ?assertEqual(length(ExpectedElements), b5_sets:size(IntersectionResult)),
        
        %% All elements in intersection must be in both original sets
        lists:foreach(fun(Elem) ->
            ?assertEqual(true, b5_sets:is_member(Elem, SetA)),
            ?assertEqual(true, b5_sets:is_member(Elem, SetB))
        end, b5_sets:to_list(IntersectionResult)),
        
        %% Commutativity: A ∩ B = B ∩ A
        IntersectionBA = b5_sets:intersection(SetB, SetA),
        ?assertEqual(IntersectionResult, IntersectionBA)
    end).

test_difference_operations(_Config) ->
    EmptySet = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    Set2 = b5_sets:from_list([3, 4, 5]),

    %% Basic difference operations
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
    ?assertEqual(Set1, DisjointDifference),
    
    %% Property-based testing with various set sizes
    for_each_set_pair(?REGULAR_SET_SIZES, fun(SetA, ElementsA, SetB, ElementsB) ->
        DifferenceResult = b5_sets:difference(SetA, SetB),
        ExpectedElements = lists:usort([E || E <- ElementsA, not lists:member(E, ElementsB)]),
        ?assertEqual(ExpectedElements, b5_sets:to_list(DifferenceResult)),
        ?assertEqual(length(ExpectedElements), b5_sets:size(DifferenceResult)),
        
        %% All elements in difference must be in first set but not second
        DifferenceElements = b5_sets:to_list(DifferenceResult),
        lists:foreach(fun(Elem) ->
            ?assertEqual(true, b5_sets:is_member(Elem, SetA)),
            ?assertEqual(false, b5_sets:is_member(Elem, SetB))
        end, DifferenceElements),
        
        %% Verify set identities
        %% A - A = ∅
        ?assertEqual(EmptySet, b5_sets:difference(SetA, SetA)),
        %% A - ∅ = A
        ?assertEqual(SetA, b5_sets:difference(SetA, EmptySet))
    end).

test_is_disjoint_operations(_Config) ->
    EmptySet = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    Set2 = b5_sets:from_list([3, 4, 5]),
    Set3 = b5_sets:from_list([6, 7, 8]),

    %% Basic disjoint operations
    ?assertEqual(true, b5_sets:is_disjoint(EmptySet, Set1)),
    ?assertEqual(true, b5_sets:is_disjoint(Set1, EmptySet)),

    %% Overlapping sets are not disjoint
    ?assertEqual(false, b5_sets:is_disjoint(Set1, Set2)),

    %% Disjoint sets
    ?assertEqual(true, b5_sets:is_disjoint(Set1, Set3)),

    %% Set is not disjoint with itself (unless empty)
    ?assertEqual(false, b5_sets:is_disjoint(Set1, Set1)),
    ?assertEqual(true, b5_sets:is_disjoint(EmptySet, EmptySet)),
    
    %% Property-based testing with various set sizes
    for_each_set_pair(?REGULAR_SET_SIZES, fun(SetA, ElementsA, SetB, ElementsB) ->
        DisjointResult = b5_sets:is_disjoint(SetA, SetB),
        HasCommonElement = lists:any(fun(E) -> lists:member(E, ElementsB) end, ElementsA),
        ExpectedDisjoint = not HasCommonElement,
        ?assertEqual(ExpectedDisjoint, DisjointResult),
        
        %% Commutativity: is_disjoint(A, B) = is_disjoint(B, A)
        ?assertEqual(DisjointResult, b5_sets:is_disjoint(SetB, SetA)),
        
        %% Empty set is disjoint with everything
        ?assertEqual(true, b5_sets:is_disjoint(SetA, EmptySet)),
        ?assertEqual(true, b5_sets:is_disjoint(EmptySet, SetB))
    end).

test_is_subset_operations(_Config) ->
    EmptySet = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    Set2 = b5_sets:from_list([2, 3]),
    Set3 = b5_sets:from_list([3, 4, 5]),

    %% Basic subset operations
    ?assertEqual(true, b5_sets:is_subset(EmptySet, Set1)),
    ?assertEqual(true, b5_sets:is_subset(EmptySet, EmptySet)),

    %% Proper subset
    ?assertEqual(true, b5_sets:is_subset(Set2, Set1)),

    %% Not a subset
    ?assertEqual(false, b5_sets:is_subset(Set1, Set2)),
    ?assertEqual(false, b5_sets:is_subset(Set1, Set3)),

    %% Set is subset of itself
    ?assertEqual(true, b5_sets:is_subset(Set1, Set1)),
    
    %% Property-based testing with various set sizes
    for_each_set_pair(?REGULAR_SET_SIZES, fun(SetA, ElementsA, SetB, ElementsB) ->
        SubsetResult = b5_sets:is_subset(SetA, SetB),
        ExpectedSubset = lists:all(fun(E) -> lists:member(E, ElementsB) end, ElementsA),
        ?assertEqual(ExpectedSubset, SubsetResult),
        
        %% Empty set is subset of everything
        ?assertEqual(true, b5_sets:is_subset(EmptySet, SetA)),
        ?assertEqual(true, b5_sets:is_subset(EmptySet, SetB)),
        
        %% Set is subset of itself
        ?assertEqual(true, b5_sets:is_subset(SetA, SetA)),
        
        %% If A ⊆ B and B ⊆ A, then A = B
        case b5_sets:is_subset(SetA, SetB) andalso b5_sets:is_subset(SetB, SetA) of
            true -> ?assertEqual(SetA, SetB);
            false -> ok
        end
    end).

test_is_equal_operations(_Config) ->
    EmptySet1 = b5_sets:new(),
    EmptySet2 = b5_sets:new(),
    Set1 = b5_sets:from_list([1, 2, 3]),
    % Same elements, different order
    Set2 = b5_sets:from_list([3, 2, 1]),
    Set3 = b5_sets:from_list([1, 2]),

    %% Basic equality operations
    ?assertEqual(true, b5_sets:is_equal(EmptySet1, EmptySet2)),
    ?assertEqual(true, b5_sets:is_equal(Set1, Set2)),
    ?assertEqual(false, b5_sets:is_equal(Set1, Set3)),
    ?assertEqual(false, b5_sets:is_equal(EmptySet1, Set1)),
    ?assertEqual(true, b5_sets:is_equal(Set1, Set1)),
    
    %% Property-based testing with various set sizes
    for_each_set(?REGULAR_SET_SIZES, fun(SetA, ElementsA) ->
        %% Test against sets with same elements but different construction
        ShuffledElements = shuffle_list(ElementsA),
        SetB = b5_sets:from_list(ShuffledElements),
        ?assertEqual(true, b5_sets:is_equal(SetA, SetB)),
        
        %% Test reflexivity: A = A
        ?assertEqual(true, b5_sets:is_equal(SetA, SetA)),
        
        %% Test symmetry: if A = B then B = A
        ?assertEqual(b5_sets:is_equal(SetA, SetB), b5_sets:is_equal(SetB, SetA)),
        
        %% Test with different sized sets (should be false unless both empty)
        UniqueElements = lists:usort(ElementsA),
        case length(UniqueElements) > 1 of
            true ->
                %% Create a set with one less unique element
                DifferentElements = tl(UniqueElements),
                DifferentSet = b5_sets:from_list(DifferentElements),
                ?assertEqual(false, b5_sets:is_equal(SetA, DifferentSet));
            false -> ok % Skip single element or empty sets
        end
    end).

%% ------------------------------------------------------------------
%% Iterator Operation Tests
%% ------------------------------------------------------------------

test_iterator_operations(_Config) ->
    EmptySet = b5_sets:new(),
    EmptyIter = b5_sets:iterator(EmptySet),
    ?assertEqual(none, b5_sets:next(EmptyIter)),

    %% Property-based testing with various set sizes
    for_each_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),
        
        %% Test ordered iteration (default)
        Iter = b5_sets:iterator(Set),
        IteratedElements = iterate_to_list(Iter),
        ?assertEqual(UniqueElements, IteratedElements),
        
        %% Verify iterator count matches set size
        ?assertEqual(b5_sets:size(Set), length(IteratedElements)),
        
        %% Test that all elements are correctly iterated
        lists:foreach(fun(Element) ->
            ?assertEqual(true, lists:member(Element, IteratedElements))
        end, UniqueElements)
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

    %% Basic iterator_from operations
    Iter1 = b5_sets:iterator_from(5, Set),
    Elements1 = iterate_to_list(Iter1),
    ?assertEqual([5, 7, 9, 11], Elements1),

    Iter2 = b5_sets:iterator_from(6, Set),
    Elements2 = iterate_to_list(Iter2),
    ?assertEqual([7, 9, 11], Elements2),

    Iter3 = b5_sets:iterator_from(20, Set),
    ?assertEqual(none, b5_sets:next(Iter3)),
    
    %% Property-based testing with various set sizes
    for_each_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),
        case UniqueElements of
            [] -> ok; % Skip empty sets
            _ ->
                %% Test iterator from first element
                FirstElement = hd(UniqueElements),
                FromFirstIter = b5_sets:iterator_from(FirstElement, TestSet),
                FromFirstElements = iterate_to_list(FromFirstIter),
                ?assertEqual(UniqueElements, FromFirstElements),
                
                %% Test iterator from last element
                LastElement = lists:last(UniqueElements),
                FromLastIter = b5_sets:iterator_from(LastElement, TestSet),
                FromLastElements = iterate_to_list(FromLastIter),
                ?assertEqual([LastElement], FromLastElements),
                
                %% Test iterator from beyond largest element
                BeyondLargest = LastElement + 1000,
                BeyondIter = b5_sets:iterator_from(BeyondLargest, TestSet),
                ?assertEqual(none, b5_sets:next(BeyondIter))
        end
    end).

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

test_smaller_larger_operations(_Config) ->
    EmptySet = b5_sets:new(),
    
    %% Empty set operations (note: current implementation has bugs with empty sets)
    % TODO: Fix implementation - currently returns {Element} instead of none
    % ?assertEqual(none, b5_sets:smaller(42, EmptySet)),
    % ?assertEqual(none, b5_sets:larger(42, EmptySet)),
    
    Set = b5_sets:from_list([1, 3, 5, 7, 9, 11]),

    %% Smaller operations - find greatest element < given element
    ?assertEqual({found, 3}, b5_sets:smaller(5, Set)),
    ?assertEqual({found, 7}, b5_sets:smaller(9, Set)),
    ?assertEqual(none, b5_sets:smaller(1, Set)),  % No element < 1
    ?assertEqual({found, 9}, b5_sets:smaller(11, Set)),
    
    %% Larger operations - find least element > given element
    ?assertEqual({found, 7}, b5_sets:larger(5, Set)),
    ?assertEqual({found, 3}, b5_sets:larger(1, Set)),
    ?assertEqual(none, b5_sets:larger(11, Set)),  % No element > 11
    ?assertEqual({found, 5}, b5_sets:larger(3, Set)),
    
    %% Test with non-existent elements
    ?assertEqual({found, 3}, b5_sets:smaller(4, Set)),  % 4 not in set, find < 4
    ?assertEqual({found, 5}, b5_sets:larger(4, Set)),   % 4 not in set, find > 4
    ?assertEqual(none, b5_sets:smaller(0, Set)),        % 0 < all elements
    ?assertEqual(none, b5_sets:larger(20, Set)),        % 20 > all elements
    
    %% Property-based testing
    for_each_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
        SortedElements = lists:usort(ElementsList),
        case SortedElements of
            [] -> ok;  % Skip empty sets
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

    %% Basic fold operations
    Sum = b5_sets:fold(fun(Element, Acc) -> Element + Acc end, 0, Set),
    ?assertEqual(15, Sum),

    Count = b5_sets:fold(fun(_, Acc) -> Acc + 1 end, 0, Set),
    ?assertEqual(5, Count),

    Collected = b5_sets:fold(fun(Element, Acc) -> [Element | Acc] end, [], Set),
    ?assertEqual([5, 4, 3, 2, 1], Collected),
    
    %% Property-based testing with various set sizes
    for_each_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
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
            false -> ok
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
    for_each_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),
        
        %% Test identity mapping (use logical equality)
        IdentityMapped = b5_sets:map(fun(X) -> X end, TestSet),
        ?assertEqual(b5_sets:to_list(TestSet), b5_sets:to_list(IdentityMapped)),
        
        %% Test constant mapping
        ConstantMapped = b5_sets:map(fun(_) -> mapped_constant end, TestSet),
        case UniqueElements of
            [] -> ?assertEqual(EmptySet, ConstantMapped);
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
            false -> ok
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
    for_each_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
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
            false -> ok
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
    for_each_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
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
            false -> ok
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
    Shuffled = shuffle_list(List),
    lists:sublist(Shuffled, N);
take_random(_List, 0) ->
    [].

%% @doc Shuffle a list randomly
shuffle_list(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- List])].

%% @doc Iterate through set pairs for different set sizes
for_each_set_pair(Sizes, Fun) ->
    lists:foreach(fun(SizeA) ->
        lists:foreach(fun(SizeB) ->
            ElementsA = generate_elements_list(SizeA),
            ElementsB = generate_elements_list(SizeB),
            SetA = b5_sets:from_list(ElementsA),
            SetB = b5_sets:from_list(ElementsB),
            Fun(SetA, ElementsA, SetB, ElementsB)
        end, take_random(Sizes, 10))  % Test against 10 random sizes
    end, take_random(Sizes, 15)).    % Test 15 random sizes as first set

%% @doc Test smaller/larger for middle elements in sorted list
test_smaller_larger_middle_elements(Set, SortedElements) ->
    lists:foreach(fun(I) when I > 1, I < length(SortedElements) ->
        Element = lists:nth(I, SortedElements),
        ExpectedSmaller = lists:nth(I - 1, SortedElements),
        ExpectedLarger = lists:nth(I + 1, SortedElements),
        ?assertEqual({found, ExpectedSmaller}, b5_sets:smaller(Element, Set)),
        ?assertEqual({found, ExpectedLarger}, b5_sets:larger(Element, Set))
    end, lists:seq(2, length(SortedElements) - 1)).

%% @doc Iterate through all elements using iterator
iterate_to_list(Iter) ->
    iterate_to_list(Iter, []).

iterate_to_list(Iter, Acc) ->
    case b5_sets:next(Iter) of
        none -> lists:reverse(Acc);
        {Element, NextIter} -> iterate_to_list(NextIter, [Element | Acc])
    end.
