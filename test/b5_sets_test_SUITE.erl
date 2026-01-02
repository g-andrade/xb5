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

-record(b5_sets, {size, root}).

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
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        Result = b5_sets:to_list(Set),
        Expected = lists:sort(ElementsList),
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
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        ExpectedSize = length(ElementsList),
        ?assertEqual(ExpectedSize, b5_sets:size(Set)),
        ?assertEqual(ExpectedSize =:= 0, b5_sets:is_empty(Set))
    end).

%% ------------------------------------------------------------------
%% Membership Operation Tests
%% ------------------------------------------------------------------

test_add_operations(_Config) ->
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        %% Add existing elements - should not change set
        foreach_randomly_retyped_element(
            fun(Element) ->
                UpdatedSet = b5_sets:add(Element, Set),
                ?assertEqual(Set, UpdatedSet)
            end,
            ElementsList
        ),

        foreach_non_existent_element(
          fun(NewElement) ->
                  UpdatedSet = b5_sets:add(NewElement, Set),
                  ?assertEqual(b5_sets:size(Set) + 1, b5_sets:size(UpdatedSet)),
                  ?assertEqual(true, b5_sets:is_member(NewElement, UpdatedSet))
          end,
          ElementsList,
          50)
    end).

test_insert_operations(_Config) ->
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        foreach_randomly_retyped_element(
          fun (Element) ->
                  %% Insert existing elements - should throw error
                  ?assertError({key_exists, Element}, b5_sets:insert(Element, Set))
          end,
          ElementsList),

        foreach_non_existent_element(
          fun (NewElement) ->
                  UpdatedSet = b5_sets:insert(NewElement, Set),
                  ?assertEqual(b5_sets:size(Set) + 1, b5_sets:size(UpdatedSet)),
                  ?assertEqual(true, b5_sets:is_member(NewElement, UpdatedSet)),
                  ?assertEqual(expected_list_insert(NewElement, ElementsList), b5_sets:to_list(UpdatedSet))
          end,
          ElementsList,
          50)
                                     end).

test_delete_operations(_Config) ->
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        %% Delete existing elements
        fold_randomly_retyped_shuffled_elements(
          fun (Element, {SetAcc, ListAcc}) ->
                  UpdatedSet = b5_sets:delete(Element, SetAcc),
                  ?assertEqual(b5_sets:size(SetAcc) - 1, b5_sets:size(UpdatedSet)),
                  ?assertEqual(false, b5_sets:is_member(Element, UpdatedSet)),

                  UpdatedList = expected_list_del(Element, ListAcc),
                  ?assertEqual(UpdatedList, b5_sets:to_list(UpdatedSet)),
                  {UpdatedSet, UpdatedList}
          end,
          {Set, ElementsList},
          ElementsList),

        foreach_non_existent_element(
          fun (NonExistentElement) ->
                  %% Delete non-existent element - should throw error
                  ?assertError({badkey, NonExistentElement}, b5_sets:delete(NonExistentElement, Set))
          end,
          ElementsList,
          50)
    end).

test_delete_any_operations(_Config) ->
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        %% Delete existing elements
        fold_randomly_retyped_shuffled_elements(
          fun (Element, {SetAcc, ListAcc}) ->
                  UpdatedSet = b5_sets:delete_any(Element, SetAcc),
                  ?assertEqual(b5_sets:size(SetAcc) - 1, b5_sets:size(UpdatedSet)),
                  ?assertEqual(false, b5_sets:is_member(Element, UpdatedSet)),

                  UpdatedList = expected_list_del(Element, ListAcc),
                  ?assertEqual(UpdatedList, b5_sets:to_list(UpdatedSet)),
                  {UpdatedSet, UpdatedList}
          end,
          {Set, ElementsList},
          ElementsList),

        foreach_non_existent_element(
          fun (NonExistentElement) ->
                  %% Delete non-existent element - keeps the set as is
                  ?assertEqual(Set, b5_sets:delete_any(NonExistentElement, Set))
          end,
          ElementsList,
          50)
    end).

test_is_member_operations(_Config) ->
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        %% Test existing elements
        foreach_randomly_retyped_element(
          fun (Element) ->
                  ?assertEqual(true, b5_sets:is_member(Element, Set))
          end,
          ElementsList),

        %% Test non-existent elements
        foreach_non_existent_element(
          fun (NonExistentElement) ->
                  ?assertEqual(false, b5_sets:is_member(NonExistentElement, Set))
          end,
          ElementsList,
          50)
    end).

test_is_set_operations(_Config) ->
    foreach_set(?REGULAR_SET_SIZES, fun(Set, _ElementsList) ->
        ?assertEqual(true, b5_sets:is_set(Set))
    end).

%% ------------------------------------------------------------------
%% Set Operation Tests
%% ------------------------------------------------------------------

%%%%%%%%%%%%%
%% Union

test_union_operations(_Config) ->
    foreach_set(
      ?REGULAR_SET_SIZES, 
      fun (Set, ElementsList) ->
              test_union_no_overlap_lesser(Set, ElementsList),
              test_union_overlap_lesser(Set, ElementsList),
              test_union_overlap_within(Set, ElementsList),
              test_union_no_overlap_greater(Set, ElementsList)
      end).

test_union_no_overlap_lesser(Set, ElementsList) ->
    MinElement = hd_or_zero(ElementsList),

    lists:foreach(
      fun (Size2) ->
              Elements2 = lists:map(fun randomly_switch_type/1, lists:sort(lists:seq(MinElement - 1, MinElement - Size2, -1))),
              Set2 = b5_sets:from_list(Elements2),

              UnionSet = b5_sets:union(Set, Set2),

              UnionList = ordsets:union(ElementsList, Elements2),
              ?assertEqual([], diff_lists(UnionList, b5_sets:to_list(UnionSet))),
              ?assertEqual(ordsets:size(UnionList), b5_sets:size(UnionSet))
              
      end,
      lists:seq(1, 50)).

test_union_overlap_lesser(Set, ElementsList) ->
    MinElement = hd_or_zero(ElementsList),

    lists:foreach(
      fun (Size2) ->
              SizeLess = Size2 div 2,
              ElementsLess = lists:map(fun randomly_switch_type/1, lists:seq(MinElement - 1, MinElement - SizeLess, -1)),

              SizeOverlapping = Size2 - SizeLess,
              ElementsOverlapping = elements_overlapping(ElementsList, SizeOverlapping),

              Elements2 = lists:usort(ElementsLess ++ ElementsOverlapping),

              Set2 = b5_sets:from_list(Elements2),

              UnionSet = b5_sets:union(Set, Set2),

              UnionList = ordsets:union(ElementsList, Elements2),
              ?assertEqual([], diff_lists(UnionList, b5_sets:to_list(UnionSet))),
              ?assertEqual(ordsets:size(UnionList), b5_sets:size(UnionSet))
              
      end,
      lists:seq(1, 50)).

test_union_overlap_within(Set, ElementsList) ->
    lists:foreach(
      fun (Size2) ->
              Elements2 = lists:usort(elements_overlapping(ElementsList, Size2)),

              Set2 = b5_sets:from_list(Elements2),

              UnionSet = b5_sets:union(Set, Set2),

              UnionList = ordsets:union(ElementsList, Elements2),
              ?assertEqual([], diff_lists(UnionList, b5_sets:to_list(UnionSet))),
              ?assertEqual(ordsets:size(UnionList), b5_sets:size(UnionSet))
              
      end,
      lists:seq(1, 50)).

test_union_no_overlap_greater(Set, ElementsList) ->
    MaxElement = last_or_zero(ElementsList),

    lists:foreach(
      fun (Size2) ->
              Elements2 = lists:map(fun randomly_switch_type/1, lists:seq(MaxElement + 1, MaxElement + Size2)),
              Set2 = b5_sets:from_list(Elements2),

              UnionSet = b5_sets:union(Set, Set2),

              UnionList = ordsets:union(ElementsList, Elements2),
              ?assertEqual([], diff_lists(UnionList, b5_sets:to_list(UnionSet))),
              ?assertEqual(ordsets:size(UnionList), b5_sets:size(UnionSet))
              
      end,
      lists:seq(1, 50)).

hd_or_zero([H | _]) -> H;
hd_or_zero([]) -> 0.

last_or_zero([]) -> 0;
last_or_zero(List) -> lists:last(List).

elements_overlapping(ElementsList, SizeOverlapping) ->
    BatchSize = 
        case ElementsList =/= [] of
            true -> max(1, SizeOverlapping div length(ElementsList));
            false -> 1
        end,

    elements_overlapping_recur(ElementsList, BatchSize, SizeOverlapping).

elements_overlapping_recur([A | [B | _] = Next], BatchSize, SizeOverlapping) when SizeOverlapping > 0 ->
    WindowSize = min(BatchSize, B - A),

    Batch = lists:map(fun randomly_switch_type/1, lists:seq(A, A + WindowSize)),

    Batch ++ elements_overlapping_recur(Next, BatchSize, SizeOverlapping - WindowSize);
elements_overlapping_recur([Last], _BatchSize, SizeOverlapping) when SizeOverlapping > 0 ->
    Batch = lists:map(fun randomly_switch_type/1, lists:seq(Last, Last + SizeOverlapping)),
    Batch;
elements_overlapping_recur([], _BatchSize, SizeOverlapping) ->
    Batch = lists:map(fun randomly_switch_type/1, lists:seq(1, SizeOverlapping)),
    Batch;
elements_overlapping_recur(_, _, 0) ->
    [].

%%%%%%%%%%%%%
%% Intersection

test_intersection_operations(_Config) ->
    %% TODO continue from here

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
    foreach_set_pair(?REGULAR_SET_SIZES, fun(SetA, ElementsA, SetB, ElementsB) ->
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
    foreach_set_pair(?REGULAR_SET_SIZES, fun(SetA, ElementsA, SetB, ElementsB) ->
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
    foreach_set_pair(?REGULAR_SET_SIZES, fun(SetA, ElementsA, SetB, ElementsB) ->
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
    foreach_set_pair(?REGULAR_SET_SIZES, fun(SetA, ElementsA, SetB, ElementsB) ->
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
    foreach_set(?REGULAR_SET_SIZES, fun(SetA, ElementsA) ->
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
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
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
    EmptySet = b5_sets:new(),
    EmptyIter = b5_sets:iterator(EmptySet, ordered),
    ?assertEqual(none, b5_sets:next(EmptyIter)),

    %% Property-based testing with various set sizes
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),
        
        %% Test ordered iteration
        Iter = b5_sets:iterator(Set, ordered),
        IteratedElements = iterate_to_list(Iter),
        ?assertEqual(UniqueElements, IteratedElements),
        
        %% Verify iterator count matches set size
        ?assertEqual(b5_sets:size(Set), length(IteratedElements)),
        
        %% Test that all elements are correctly iterated
        lists:foreach(fun(Element) ->
            ?assertEqual(true, lists:member(Element, IteratedElements))
        end, UniqueElements)
    end).

test_iterator_reversed_operations(_Config) ->
    EmptySet = b5_sets:new(),
    EmptyIter = b5_sets:iterator(EmptySet, reversed),
    ?assertEqual(none, b5_sets:next(EmptyIter)),

    %% Property-based testing with various set sizes
    foreach_set(?REGULAR_SET_SIZES, fun(Set, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),
        
        %% Test reversed iteration
        Iter = b5_sets:iterator(Set, reversed),
        IteratedElements = iterate_to_list(Iter),
        ?assertEqual(lists:reverse(UniqueElements), IteratedElements),
        
        %% Verify iterator count matches set size
        ?assertEqual(b5_sets:size(Set), length(IteratedElements)),
        
        %% Test that all elements are correctly iterated
        lists:foreach(fun(Element) ->
            ?assertEqual(true, lists:member(Element, IteratedElements))
        end, UniqueElements)
    end).

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
    foreach_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
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

    %% ordered iterator_from operations
    Iter1 = b5_sets:iterator_from(5, Set, ordered),
    Elements1 = iterate_to_list(Iter1),
    ?assertEqual([5, 7, 9, 11], Elements1),

    Iter2 = b5_sets:iterator_from(6, Set, ordered),
    Elements2 = iterate_to_list(Iter2),
    ?assertEqual([7, 9, 11], Elements2),

    Iter3 = b5_sets:iterator_from(20, Set, ordered),
    ?assertEqual(none, b5_sets:next(Iter3)),
    
    %% Property-based testing with various set sizes
    foreach_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
        UniqueElements = lists:usort(ElementsList),
        case UniqueElements of
            [] -> ok; % Skip empty sets
            _ ->
                %% Test iterator from first element
                FirstElement = hd(UniqueElements),
                FromFirstIter = b5_sets:iterator_from(FirstElement, TestSet, ordered),
                FromFirstElements = iterate_to_list(FromFirstIter),
                ?assertEqual(UniqueElements, FromFirstElements),
                
                %% Test iterator from last element
                LastElement = lists:last(UniqueElements),
                FromLastIter = b5_sets:iterator_from(LastElement, TestSet, ordered),
                FromLastElements = iterate_to_list(FromLastIter),
                ?assertEqual([LastElement], FromLastElements),
                
                %% Test iterator from beyond largest element
                BeyondLargest = LastElement + 1000,
                BeyondIter = b5_sets:iterator_from(BeyondLargest, TestSet, ordered),
                ?assertEqual(none, b5_sets:next(BeyondIter))
        end
    end).

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
    
    %% Empty set operations behavior
    ?assertEqual(none, b5_sets:smaller(42, EmptySet)),
    ?assertEqual(none, b5_sets:larger(42, EmptySet)),
    ?assertError({badkey, 42}, b5_sets:delete(42, EmptySet)),
    
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
    foreach_set(?REGULAR_SET_SIZES, fun(TestSet, ElementsList) ->
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
    
    lists:foreach(fun(Element) ->
        ?assertError({badkey, Element}, b5_sets:delete(Element, EmptySet)),
        ?assertEqual(none, b5_sets:smaller(Element, EmptySet)),
        ?assertEqual(none, b5_sets:larger(Element, EmptySet))
    end, TestElements),
    
    %% Verify these operations work fine on non-empty sets
    NonEmptySet = b5_sets:from_list([1, 2, 3]),
    
    %% These should work without throwing exceptions
    ?assertEqual(none, b5_sets:smaller(1, NonEmptySet)),  % smallest has no smaller
    ?assertEqual(none, b5_sets:larger(3, NonEmptySet)),   % largest has no larger
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
    OddEvenSet = b5_sets:filtermap(fun(X) ->
        case X rem 2 of
            1 -> {true, odd};   % Multiple elements map to 'odd'
            0 -> {true, even}   % Multiple elements map to 'even'
        end
    end, Set1),
    
    %% Should result in set with just {even, odd}
    Result1 = b5_sets:to_list(OddEvenSet),
    ?assertEqual(2, b5_sets:size(OddEvenSet)),
    ?assert(lists:member(odd, Result1)),
    ?assert(lists:member(even, Result1)),
    
    %% Test with more complex collision scenarios
    Set2 = b5_sets:from_list([10, 11, 12, 13, 14, 15, 16, 17, 18, 19]),
    
    %% Map to last digit - multiple collisions
    LastDigitSet = b5_sets:filtermap(fun(X) ->
        LastDigit = X rem 10,
        {true, LastDigit}
    end, Set2),
    
    %% Should have digits 0-9
    Result2 = b5_sets:to_list(LastDigitSet),
    ExpectedDigits = lists:seq(0, 9),
    ?assertEqual(ExpectedDigits, Result2),
    ?assertEqual(10, b5_sets:size(LastDigitSet)),
    
    %% Test collision with larger sets (more likely to hit edge cases)
    LargeSet = b5_sets:from_list(lists:seq(1, 50)),
    
    %% Map to modulo 7 - creates multiple collisions
    ModuloSet = b5_sets:filtermap(fun(X) ->
        Modulo = X rem 7,
        {true, Modulo}
    end, LargeSet),
    
    Result3 = b5_sets:to_list(ModuloSet),
    ExpectedModulos = lists:seq(0, 6),
    ?assertEqual(ExpectedModulos, Result3),
    ?assertEqual(7, b5_sets:size(ModuloSet)),
    
    %% Test with selective filtering + collisions
    Set3 = b5_sets:from_list([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    
    %% Only map even numbers, but map to collision-prone values
    SelectiveSet = b5_sets:filtermap(fun(X) ->
        case X rem 2 =:= 0 of
            true -> {true, X div 2};  % 2->1, 4->2, 6->3, 8->4, 10->5
            false -> false
        end
    end, Set3),
    
    Result4 = b5_sets:to_list(SelectiveSet),
    ?assertEqual([1, 2, 3, 4, 5], Result4),
    ?assertEqual(5, b5_sets:size(SelectiveSet)),
    
    %% Test extreme collision case - all elements map to same value
    Set4 = b5_sets:from_list([a, b, c, d, e]),
    
    AllSameSet = b5_sets:filtermap(fun(_) ->
        {true, collision_value}
    end, Set4),
    
    Result5 = b5_sets:to_list(AllSameSet),
    ?assertEqual([collision_value], Result5),
    ?assertEqual(1, b5_sets:size(AllSameSet)),
    
    %% Test collision with different data types
    MixedSet = b5_sets:from_list([1, 2, atom, "string", {tuple}]),
    
    TypeMappedSet = b5_sets:filtermap(fun(X) ->
        Type = case X of
            I when is_integer(I) -> integer;
            A when is_atom(A) -> atom;
            S when is_list(S) -> string;
            T when is_tuple(T) -> tuple
        end,
        {true, Type}
    end, MixedSet),
    
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
    InvalidSet2 = {b5_sets, 999, ?LEAF0},  % Size 999 but empty root
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
foreach_set_pair(Sizes, Fun) ->
    lists:foreach(fun(SizeA) ->
        lists:foreach(fun(SizeB) ->
            ElementsA = lists:sort(generate_unique_elements_list(SizeA)),
            ElementsB = lists:sort(generate_unique_elements_list(SizeB)),
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

fold_randomly_retyped_shuffled_elements(Fun, Acc0, List) ->
    lists:foldl(
      fun (Element, Acc) ->
              Fun(randomly_switch_type(Element), Acc)
      end,
      Acc0,
      shuffle_list(List)).

foreach_randomly_retyped_element(Fun, List) ->
    lists:foreach(
      fun (Element) ->
              Fun(randomly_switch_type(Element))
      end,
      List).

foreach_non_existent_element(Fun, List, Amount) ->
    lists:foreach(
      fun (_) ->
              Element = randomly_switch_type(generate_new_element_not_in(List)),
              Fun(Element)
      end,
      lists:seq(1, Amount)).

expected_list_del(Element, List) ->
    lists:filter(fun (E) -> E /= Element end, List).

expected_list_insert(Element, List) ->
    lists:sort([Element | List]).

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


invalid_set(Root, Size) ->
    #b5_sets{root = Root, size = Size}.
