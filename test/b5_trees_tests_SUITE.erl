%% @copyright 2025 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(b5_trees_tests_SUITE).

-ifndef(NO_CT_SUITE_BEHAVIOUR).
-behaviour(ct_suite).
-endif.

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% ct_suite Function Exports
%% ------------------------------------------------------------------

-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Basic Operations
%% ------------------------------------------------------------------

-export([test_new/1,
         test_empty/1,
         test_is_empty/1,
         test_size/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Insert Operations
%% ------------------------------------------------------------------

-export([test_insert/1,
         test_insert_with/1,
         test_enter/1,
         test_insert_duplicate_key/1,
         test_insert_with_duplicate_key/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Get Operations
%% ------------------------------------------------------------------

-export([test_get/1,
         test_lookup/1,
         test_is_defined/1,
         test_get_nonexistent/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Update Operations
%% ------------------------------------------------------------------

-export([test_update/1,
         test_update_with/1,
         test_update_with_default/1,
         test_update_nonexistent/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Delete Operations
%% ------------------------------------------------------------------

-export([test_delete/1,
         test_take/1,
         test_take_any/1,
         test_take_largest/1,
         test_take_smallest/1,
         test_delete_nonexistent/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Min/Max Operations
%% ------------------------------------------------------------------

-export([test_smallest/1,
         test_largest/1,
         test_smaller/1,
         test_larger/1,
         test_smallest_empty_tree/1,
         test_largest_empty_tree/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Iteration
%% ------------------------------------------------------------------

-export([test_iterator/1,
         test_iterator_ordered/1,
         test_iterator_reversed/1,
         test_iterator_from/1,
         test_iterator_from_ordered/1,
         test_iterator_from_reversed/1,
         test_next/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Conversion Operations
%% ------------------------------------------------------------------

-export([test_from_list/1,
         test_to_list/1,
         test_keys/1,
         test_values/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Fold Operations
%% ------------------------------------------------------------------

-export([test_foldl/1,
         test_foldr/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Map Operation
%% ------------------------------------------------------------------

-export([test_map/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Validation and Private Functions
%% ------------------------------------------------------------------

-export([test_validate/1,
         test_from_constituent_parts/1,
         test_to_constituent_parts/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Edge Cases and Large Trees
%% ------------------------------------------------------------------

-export([test_large_tree_operations/1,
         test_sequential_insertions/1,
         test_random_operations/1]).

%% ------------------------------------------------------------------
%% Test Case Function Exports - Node Coverage (b5_trees_node)
%% ------------------------------------------------------------------

-export([test_empty_tree_operations/1,
         test_single_element_operations/1,
         test_range_queries_comprehensive/1,
         test_error_conditions/1,
         test_iterator_edge_cases/1,
         test_fold_all_patterns/1,
         test_keys_all_patterns/1,
         test_values_all_patterns/1,
         test_to_list_all_patterns/1,
         test_deep_tree_operations/1,
         test_deletion_rebalancing/1,
         test_internal_node_updates/1,
         test_validation_edge_cases/1,
         test_complex_take_operations/1,
         test_iterator_all_branches/1,
         test_comprehensive_node_patterns/1,
         test_single_key_patterns/1,
         test_four_key_leaf_patterns/1,
         test_boundary_key_operations/1]).

%% ------------------------------------------------------------------
%% ct_suite Function Definitions
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [
        {basic_operations, [parallel], [
            test_new,
            test_empty,
            test_is_empty,
            test_size
        ]},
        {insert_operations, [parallel], [
            test_insert,
            test_insert_with,
            test_enter,
            test_insert_duplicate_key,
            test_insert_with_duplicate_key
        ]},
        {get_operations, [parallel], [
            test_get,
            test_lookup,
            test_is_defined,
            test_get_nonexistent
        ]},
        {update_operations, [parallel], [
            test_update,
            test_update_with,
            test_update_with_default,
            test_update_nonexistent
        ]},
        {delete_operations, [parallel], [
            test_delete,
            test_take,
            test_take_any,
            test_take_largest,
            test_take_smallest,
            test_delete_nonexistent
        ]},
        {min_max_operations, [parallel], [
            test_smallest,
            test_largest,
            test_smaller,
            test_larger,
            test_smallest_empty_tree,
            test_largest_empty_tree
        ]},
        {iteration, [parallel], [
            test_iterator,
            test_iterator_ordered,
            test_iterator_reversed,
            test_iterator_from,
            test_iterator_from_ordered,
            test_iterator_from_reversed,
            test_next
        ]},
        {conversion_operations, [parallel], [
            test_from_list,
            test_to_list,
            test_keys,
            test_values
        ]},
        {fold_operations, [parallel], [
            test_foldl,
            test_foldr
        ]},
        {map_operation, [parallel], [
            test_map
        ]},
        {validation_and_private, [parallel], [
            test_validate,
            test_from_constituent_parts,
            test_to_constituent_parts
        ]},
        {edge_cases, [], [
            test_large_tree_operations,
            test_sequential_insertions,
            test_random_operations
        ]},
        {node_coverage, [parallel], [
            test_empty_tree_operations,
            test_single_element_operations,
            test_range_queries_comprehensive,
            test_error_conditions,
            test_iterator_edge_cases,
            test_fold_all_patterns,
            test_keys_all_patterns,
            test_values_all_patterns,
            test_to_list_all_patterns,
            test_deep_tree_operations,
            test_deletion_rebalancing,
            test_internal_node_updates,
            test_validation_edge_cases,
            test_complex_take_operations,
            test_iterator_all_branches,
            test_comprehensive_node_patterns,
            test_single_key_patterns,
            test_four_key_leaf_patterns,
            test_boundary_key_operations
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCases, Config) ->
    Config.

%% ------------------------------------------------------------------
%% Test Cases - Basic Operations
%% ------------------------------------------------------------------

test_new(_Config) ->
    Tree = b5_trees:new(),
    ?assert(b5_trees:is_empty(Tree)),
    ?assertEqual(0, b5_trees:size(Tree)).

test_empty(_Config) ->
    Tree = b5_trees:empty(),
    ?assert(b5_trees:is_empty(Tree)),
    ?assertEqual(0, b5_trees:size(Tree)).

test_is_empty(_Config) ->
    Tree1 = b5_trees:new(),
    ?assert(b5_trees:is_empty(Tree1)),
    
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    ?assertNot(b5_trees:is_empty(Tree2)).

test_size(_Config) ->
    Tree1 = b5_trees:new(),
    ?assertEqual(0, b5_trees:size(Tree1)),
    
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    ?assertEqual(1, b5_trees:size(Tree2)),
    
    Tree3 = b5_trees:insert(key2, value2, Tree2),
    ?assertEqual(2, b5_trees:size(Tree3)),
    
    Tree4 = b5_trees:delete(key1, Tree3),
    ?assertEqual(1, b5_trees:size(Tree4)).

%% ------------------------------------------------------------------
%% Test Cases - Insert Operations
%% ------------------------------------------------------------------

test_insert(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    
    ?assertEqual(1, b5_trees:size(Tree2)),
    ?assertEqual(value1, b5_trees:get(key1, Tree2)).

test_insert_with(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert_with(key1, fun() -> expensive_value end, Tree1),
    
    ?assertEqual(1, b5_trees:size(Tree2)),
    ?assertEqual(expensive_value, b5_trees:get(key1, Tree2)).

test_enter(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:enter(key1, value1, Tree1),
    ?assertEqual(value1, b5_trees:get(key1, Tree2)),
    
    Tree3 = b5_trees:enter(key1, value2, Tree2),
    ?assertEqual(value2, b5_trees:get(key1, Tree3)),
    ?assertEqual(1, b5_trees:size(Tree3)).

test_insert_duplicate_key(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    
    ?assertError({key_exists, key1}, b5_trees:insert(key1, value2, Tree2)).

test_insert_with_duplicate_key(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    
    ?assertError({key_exists, key1}, b5_trees:insert_with(key1, fun() -> value2 end, Tree2)).

%% ------------------------------------------------------------------
%% Test Cases - Get Operations
%% ------------------------------------------------------------------

test_get(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    Tree3 = b5_trees:insert(key2, value2, Tree2),
    
    ?assertEqual(value1, b5_trees:get(key1, Tree3)),
    ?assertEqual(value2, b5_trees:get(key2, Tree3)).

test_lookup(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    
    ?assertEqual({value, value1}, b5_trees:lookup(key1, Tree2)),
    ?assertEqual(none, b5_trees:lookup(nonexistent, Tree2)).

test_is_defined(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    
    ?assert(b5_trees:is_defined(key1, Tree2)),
    ?assertNot(b5_trees:is_defined(nonexistent, Tree2)).

test_get_nonexistent(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    
    ?assertError({badkey, nonexistent}, b5_trees:get(nonexistent, Tree2)).

%% ------------------------------------------------------------------
%% Test Cases - Update Operations
%% ------------------------------------------------------------------

test_update(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    Tree3 = b5_trees:update(key1, new_value, Tree2),
    
    ?assertEqual(new_value, b5_trees:get(key1, Tree3)),
    ?assertEqual(1, b5_trees:size(Tree3)).

test_update_with(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, 10, Tree1),
    Tree3 = b5_trees:update_with(key1, fun(V) -> V * 2 end, Tree2),
    
    ?assertEqual(20, b5_trees:get(key1, Tree3)).

test_update_with_default(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, 10, Tree1),
    
    Tree3 = b5_trees:update_with(key1, fun(V) -> V * 2 end, default_value, Tree2),
    ?assertEqual(20, b5_trees:get(key1, Tree3)),
    
    Tree4 = b5_trees:update_with(key2, fun(V) -> V * 2 end, default_value, Tree3),
    ?assertEqual(default_value, b5_trees:get(key2, Tree4)),
    ?assertEqual(2, b5_trees:size(Tree4)).

test_update_nonexistent(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    
    ?assertError({badkey, nonexistent}, b5_trees:update(nonexistent, new_value, Tree2)).

%% ------------------------------------------------------------------
%% Test Cases - Delete Operations
%% ------------------------------------------------------------------

test_delete(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    Tree3 = b5_trees:insert(key2, value2, Tree2),
    Tree4 = b5_trees:delete(key1, Tree3),
    
    ?assertEqual(1, b5_trees:size(Tree4)),
    ?assertNot(b5_trees:is_defined(key1, Tree4)),
    ?assert(b5_trees:is_defined(key2, Tree4)).

test_take(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    Tree3 = b5_trees:insert(key2, value2, Tree2),
    
    {Value, Tree4} = b5_trees:take(key1, Tree3),
    ?assertEqual(value1, Value),
    ?assertEqual(1, b5_trees:size(Tree4)),
    ?assertNot(b5_trees:is_defined(key1, Tree4)).

test_take_any(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    
    {Value, Tree3} = b5_trees:take_any(key1, Tree2),
    ?assertEqual(value1, Value),
    ?assertEqual(0, b5_trees:size(Tree3)),
    
    ?assertEqual(error, b5_trees:take_any(nonexistent, Tree2)).

test_take_largest(_Config) ->
    Tree1 = b5_trees:from_list([{1, a}, {2, b}, {3, c}]),
    
    {Key, Value, Tree2} = b5_trees:take_largest(Tree1),
    ?assertEqual(3, Key),
    ?assertEqual(c, Value),
    ?assertEqual(2, b5_trees:size(Tree2)).

test_take_smallest(_Config) ->
    Tree1 = b5_trees:from_list([{1, a}, {2, b}, {3, c}]),
    
    {Key, Value, Tree2} = b5_trees:take_smallest(Tree1),
    ?assertEqual(1, Key),
    ?assertEqual(a, Value),
    ?assertEqual(2, b5_trees:size(Tree2)).

test_delete_nonexistent(_Config) ->
    Tree1 = b5_trees:new(),
    Tree2 = b5_trees:insert(key1, value1, Tree1),
    
    ?assertError({badkey, nonexistent}, b5_trees:delete(nonexistent, Tree2)).

%% ------------------------------------------------------------------
%% Test Cases - Min/Max Operations
%% ------------------------------------------------------------------

test_smallest(_Config) ->
    Tree = b5_trees:from_list([{3, c}, {1, a}, {2, b}]),
    {Key, Value} = b5_trees:smallest(Tree),
    ?assertEqual(1, Key),
    ?assertEqual(a, Value).

test_largest(_Config) ->
    Tree = b5_trees:from_list([{3, c}, {1, a}, {2, b}]),
    {Key, Value} = b5_trees:largest(Tree),
    ?assertEqual(3, Key),
    ?assertEqual(c, Value).

test_smaller(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}, {4, d}]),
    
    ?assertEqual({2, b}, b5_trees:smaller(3, Tree)),
    ?assertEqual({1, a}, b5_trees:smaller(2, Tree)),
    ?assertEqual(none, b5_trees:smaller(1, Tree)),
    ?assertEqual(none, b5_trees:smaller(0, Tree)).

test_larger(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}, {4, d}]),
    
    ?assertEqual({2, b}, b5_trees:larger(1, Tree)),
    ?assertEqual({4, d}, b5_trees:larger(3, Tree)),
    ?assertEqual(none, b5_trees:larger(4, Tree)),
    ?assertEqual(none, b5_trees:larger(5, Tree)).

test_smallest_empty_tree(_Config) ->
    Tree = b5_trees:new(),
    ?assertError(empty_tree, b5_trees:smallest(Tree)).

test_largest_empty_tree(_Config) ->
    Tree = b5_trees:new(),
    ?assertError(empty_tree, b5_trees:largest(Tree)).

%% ------------------------------------------------------------------
%% Test Cases - Iteration
%% ------------------------------------------------------------------

test_iterator(_Config) ->
    Tree = b5_trees:from_list([{3, c}, {1, a}, {2, b}]),
    Iter = b5_trees:iterator(Tree),
    
    {Key1, Value1, Iter2} = b5_trees:next(Iter),
    ?assertEqual(1, Key1),
    ?assertEqual(a, Value1),
    
    {Key2, Value2, Iter3} = b5_trees:next(Iter2),
    ?assertEqual(2, Key2),
    ?assertEqual(b, Value2),
    
    {Key3, Value3, Iter4} = b5_trees:next(Iter3),
    ?assertEqual(3, Key3),
    ?assertEqual(c, Value3),
    
    ?assertEqual(none, b5_trees:next(Iter4)).

test_iterator_ordered(_Config) ->
    Tree = b5_trees:from_list([{3, c}, {1, a}, {2, b}]),
    Iter = b5_trees:iterator(Tree, ordered),
    
    {Key1, _Value1, Iter2} = b5_trees:next(Iter),
    ?assertEqual(1, Key1),
    
    {Key2, _Value2, _Iter3} = b5_trees:next(Iter2),
    ?assertEqual(2, Key2).

test_iterator_reversed(_Config) ->
    Tree = b5_trees:from_list([{3, c}, {1, a}, {2, b}]),
    Iter = b5_trees:iterator(Tree, reversed),
    
    {Key1, _Value1, Iter2} = b5_trees:next(Iter),
    ?assertEqual(3, Key1),
    
    {Key2, _Value2, _Iter3} = b5_trees:next(Iter2),
    ?assertEqual(2, Key2).

test_iterator_from(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}, {3, c}, {4, d}]),
    Iter = b5_trees:iterator_from(2, Tree),
    
    {Key1, Value1, Iter2} = b5_trees:next(Iter),
    ?assertEqual(2, Key1),
    ?assertEqual(b, Value1),
    
    {Key2, Value2, _Iter3} = b5_trees:next(Iter2),
    ?assertEqual(3, Key2),
    ?assertEqual(c, Value2).

test_iterator_from_ordered(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}, {3, c}, {4, d}]),
    Iter = b5_trees:iterator_from(2, Tree, ordered),
    
    {Key, _Value, _Iter2} = b5_trees:next(Iter),
    ?assertEqual(2, Key).

test_iterator_from_reversed(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}, {3, c}, {4, d}]),
    Iter = b5_trees:iterator_from(3, Tree, reversed),
    
    {Key1, _Value1, Iter2} = b5_trees:next(Iter),
    ?assertEqual(3, Key1),
    
    {Key2, _Value2, _Iter3} = b5_trees:next(Iter2),
    ?assertEqual(2, Key2).

test_next(_Config) ->
    Tree = b5_trees:from_list([{1, a}]),
    Iter = b5_trees:iterator(Tree),
    
    {Key, Value, Iter2} = b5_trees:next(Iter),
    ?assertEqual(1, Key),
    ?assertEqual(a, Value),
    
    ?assertEqual(none, b5_trees:next(Iter2)).

%% ------------------------------------------------------------------
%% Test Cases - Conversion Operations
%% ------------------------------------------------------------------

test_from_list(_Config) ->
    List = [{3, c}, {1, a}, {2, b}],
    Tree = b5_trees:from_list(List),
    
    ?assertEqual(3, b5_trees:size(Tree)),
    ?assertEqual(a, b5_trees:get(1, Tree)),
    ?assertEqual(b, b5_trees:get(2, Tree)),
    ?assertEqual(c, b5_trees:get(3, Tree)).

test_to_list(_Config) ->
    Tree = b5_trees:from_list([{3, c}, {1, a}, {2, b}]),
    List = b5_trees:to_list(Tree),
    
    ?assertEqual([{1, a}, {2, b}, {3, c}], List).

test_keys(_Config) ->
    Tree = b5_trees:from_list([{3, c}, {1, a}, {2, b}]),
    Keys = b5_trees:keys(Tree),
    
    ?assertEqual([1, 2, 3], Keys).

test_values(_Config) ->
    Tree = b5_trees:from_list([{3, c}, {1, a}, {2, b}]),
    Values = b5_trees:values(Tree),
    
    ?assertEqual([a, b, c], Values).

%% ------------------------------------------------------------------
%% Test Cases - Fold Operations
%% ------------------------------------------------------------------

test_foldl(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}, {3, c}]),
    
    Result = b5_trees:foldl(
        fun(Key, Value, Acc) ->
            [{Key, Value} | Acc]
        end,
        [],
        Tree
    ),
    
    ?assertEqual([{3, c}, {2, b}, {1, a}], Result).

test_foldr(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}, {3, c}]),
    
    Result = b5_trees:foldr(
        fun(Key, Value, Acc) ->
            [{Key, Value} | Acc]
        end,
        [],
        Tree
    ),
    
    ?assertEqual([{1, a}, {2, b}, {3, c}], Result).

%% ------------------------------------------------------------------
%% Test Cases - Map Operation
%% ------------------------------------------------------------------

test_map(_Config) ->
    Tree1 = b5_trees:from_list([{1, a}, {2, b}, {3, c}]),
    Tree2 = b5_trees:map(fun(_Key, Value) -> {uppercase, Value} end, Tree1),
    
    ?assertEqual({uppercase, a}, b5_trees:get(1, Tree2)),
    ?assertEqual({uppercase, b}, b5_trees:get(2, Tree2)),
    ?assertEqual({uppercase, c}, b5_trees:get(3, Tree2)).

%% ------------------------------------------------------------------
%% Test Cases - Validation and Private Functions
%% ------------------------------------------------------------------

test_validate(_Config) ->
    Tree1 = b5_trees:new(),
    ?assertMatch({ok, _}, b5_trees:validate(Tree1)),
    
    Tree2 = b5_trees:from_list([{1, a}, {2, b}, {3, c}]),
    ?assertMatch({ok, _}, b5_trees:validate(Tree2)).

test_from_constituent_parts(_Config) ->
    Tree1 = b5_trees:from_list([{1, a}, {2, b}]),
    {ok, Parts} = b5_trees:'__to_constituent_parts__'(Tree1),
    Tree2 = b5_trees:'__from_constituent_parts__'(Parts),
    
    ?assertEqual(b5_trees:to_list(Tree1), b5_trees:to_list(Tree2)).

test_to_constituent_parts(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}]),
    Result = b5_trees:'__to_constituent_parts__'(Tree),
    
    ?assertMatch({ok, #{root := _, size := 2}}, Result),
    
    ?assertEqual(error, b5_trees:'__to_constituent_parts__'(invalid_tree)).

%% ------------------------------------------------------------------
%% Test Cases - Edge Cases and Large Trees
%% ------------------------------------------------------------------

test_large_tree_operations(_Config) ->
    List = [{I, I * 2} || I <- lists:seq(1, 1000)],
    Tree1 = b5_trees:from_list(List),
    
    ?assertEqual(1000, b5_trees:size(Tree1)),
    ?assertEqual(500, b5_trees:get(250, Tree1)),
    ?assertMatch({ok, _}, b5_trees:validate(Tree1)),
    
    Tree2 = lists:foldl(
        fun(I, TreeAcc) when I rem 2 =:= 0 ->
            b5_trees:delete(I, TreeAcc);
           (_, TreeAcc) ->
            TreeAcc
        end,
        Tree1,
        lists:seq(1, 1000)
    ),
    
    ?assertEqual(500, b5_trees:size(Tree2)),
    ?assertMatch({ok, _}, b5_trees:validate(Tree2)).

test_sequential_insertions(_Config) ->
    Tree = lists:foldl(
        fun(I, TreeAcc) ->
            b5_trees:insert(I, I * 10, TreeAcc)
        end,
        b5_trees:new(),
        lists:seq(1, 100)
    ),
    
    ?assertEqual(100, b5_trees:size(Tree)),
    ?assertEqual(500, b5_trees:get(50, Tree)),
    ?assertMatch({ok, _}, b5_trees:validate(Tree)).

test_random_operations(_Config) ->
    Tree1 = b5_trees:new(),
    Keys = [rand:uniform(1000) || _ <- lists:seq(1, 100)],
    
    Tree2 = lists:foldl(
        fun(Key, TreeAcc) ->
            b5_trees:enter(Key, Key * 2, TreeAcc)
        end,
        Tree1,
        Keys
    ),
    
    ?assertMatch({ok, _}, b5_trees:validate(Tree2)),
    
    Tree3 = lists:foldl(
        fun(Key, TreeAcc) ->
            case b5_trees:is_defined(Key, TreeAcc) of
                true -> b5_trees:delete(Key, TreeAcc);
                false -> TreeAcc
            end
        end,
        Tree2,
        lists:sublist(Keys, 50)
    ),
    
    ?assertMatch({ok, _}, b5_trees:validate(Tree3)).

%% ------------------------------------------------------------------
%% Test Cases - Node Coverage (b5_trees_node)
%% ------------------------------------------------------------------

test_empty_tree_operations(_Config) ->
    EmptyTree = b5_trees:new(),
    
    ?assertEqual([], b5_trees:keys(EmptyTree)),
    ?assertEqual([], b5_trees:values(EmptyTree)),
    ?assertEqual([], b5_trees:to_list(EmptyTree)),
    
    Result1 = b5_trees:foldl(fun(K, V, Acc) -> [{K, V} | Acc] end, [], EmptyTree),
    ?assertEqual([], Result1),
    
    Result2 = b5_trees:foldr(fun(K, V, Acc) -> [{K, V} | Acc] end, [], EmptyTree),
    ?assertEqual([], Result2),
    
    ?assertEqual(none, b5_trees:smaller(any_key, EmptyTree)),
    ?assertEqual(none, b5_trees:larger(any_key, EmptyTree)),
    
    Iter = b5_trees:iterator(EmptyTree),
    ?assertEqual(none, b5_trees:next(Iter)),
    
    IterReversed = b5_trees:iterator(EmptyTree, reversed),
    ?assertEqual(none, b5_trees:next(IterReversed)),
    
    IterFrom = b5_trees:iterator_from(any_key, EmptyTree),
    ?assertEqual(none, b5_trees:next(IterFrom)).

test_single_element_operations(_Config) ->
    Tree = b5_trees:from_list([{key1, value1}]),
    
    ?assertEqual([key1], b5_trees:keys(Tree)),
    ?assertEqual([value1], b5_trees:values(Tree)),
    ?assertEqual([{key1, value1}], b5_trees:to_list(Tree)),
    
    Result1 = b5_trees:foldl(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Tree),
    ?assertEqual([{key1, value1}], Result1),
    
    Result2 = b5_trees:foldr(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Tree),
    ?assertEqual([{key1, value1}], Result2),
    
    ?assertEqual(none, b5_trees:smaller(key1, Tree)),
    ?assertEqual(none, b5_trees:larger(key1, Tree)),
    ?assertEqual(none, b5_trees:smaller(key0, Tree)),
    ?assertEqual({key1, value1}, b5_trees:larger(key0, Tree)),
    ?assertEqual({key1, value1}, b5_trees:smaller(key2, Tree)),
    ?assertEqual(none, b5_trees:larger(key2, Tree)),
    
    Iter = b5_trees:iterator(Tree),
    {Key, Value, Iter2} = b5_trees:next(Iter),
    ?assertEqual(key1, Key),
    ?assertEqual(value1, Value),
    ?assertEqual(none, b5_trees:next(Iter2)),
    
    IterReversed = b5_trees:iterator(Tree, reversed),
    {KeyR, ValueR, Iter2R} = b5_trees:next(IterReversed),
    ?assertEqual(key1, KeyR),
    ?assertEqual(value1, ValueR),
    ?assertEqual(none, b5_trees:next(Iter2R)),
    
    IterFrom = b5_trees:iterator_from(key1, Tree),
    {KeyF, ValueF, Iter2F} = b5_trees:next(IterFrom),
    ?assertEqual(key1, KeyF),
    ?assertEqual(value1, ValueF),
    ?assertEqual(none, b5_trees:next(Iter2F)),
    
    IterFromBefore = b5_trees:iterator_from(key0, Tree),
    {KeyFB, ValueFB, _} = b5_trees:next(IterFromBefore),
    ?assertEqual(key1, KeyFB),
    ?assertEqual(value1, ValueFB),
    
    IterFromAfter = b5_trees:iterator_from(key2, Tree),
    ?assertEqual(none, b5_trees:next(IterFromAfter)).

test_range_queries_comprehensive(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {3, c}, {5, e}, {7, g}, {9, i}]),
    
    ?assertEqual(none, b5_trees:smaller(1, Tree)),
    ?assertEqual({1, a}, b5_trees:smaller(2, Tree)),
    ?assertEqual({1, a}, b5_trees:smaller(3, Tree)),
    ?assertEqual({3, c}, b5_trees:smaller(4, Tree)),
    ?assertEqual({3, c}, b5_trees:smaller(5, Tree)),
    ?assertEqual({5, e}, b5_trees:smaller(6, Tree)),
    ?assertEqual({5, e}, b5_trees:smaller(7, Tree)),
    ?assertEqual({7, g}, b5_trees:smaller(8, Tree)),
    ?assertEqual({7, g}, b5_trees:smaller(9, Tree)),
    ?assertEqual({9, i}, b5_trees:smaller(10, Tree)),
    
    ?assertEqual({1, a}, b5_trees:larger(0, Tree)),
    ?assertEqual({3, c}, b5_trees:larger(1, Tree)),
    ?assertEqual({3, c}, b5_trees:larger(2, Tree)),
    ?assertEqual({5, e}, b5_trees:larger(3, Tree)),
    ?assertEqual({5, e}, b5_trees:larger(4, Tree)),
    ?assertEqual({7, g}, b5_trees:larger(5, Tree)),
    ?assertEqual({7, g}, b5_trees:larger(6, Tree)),
    ?assertEqual({9, i}, b5_trees:larger(7, Tree)),
    ?assertEqual({9, i}, b5_trees:larger(8, Tree)),
    ?assertEqual(none, b5_trees:larger(9, Tree)),
    ?assertEqual(none, b5_trees:larger(10, Tree)),
    
    EmptyTree = b5_trees:new(),
    ?assertEqual(none, b5_trees:smaller(any_key, EmptyTree)),
    ?assertEqual(none, b5_trees:larger(any_key, EmptyTree)).

test_error_conditions(_Config) ->
    EmptyTree = b5_trees:new(),
    
    ?assertError({badkey, nonexistent}, b5_trees:get(nonexistent, EmptyTree)),
    ?assertError({badkey, nonexistent}, b5_trees:delete(nonexistent, EmptyTree)),
    ?assertError({badkey, nonexistent}, b5_trees:update(nonexistent, new_value, EmptyTree)),
    ?assertError({badkey, nonexistent}, b5_trees:update_with(nonexistent, fun(X) -> X end, EmptyTree)),
    ?assertError({badkey, nonexistent}, b5_trees:take(nonexistent, EmptyTree)),
    
    ?assertError(empty_tree, b5_trees:smallest(EmptyTree)),
    ?assertError(empty_tree, b5_trees:largest(EmptyTree)),
    ?assertError(empty_tree, b5_trees:take_smallest(EmptyTree)),
    ?assertError(empty_tree, b5_trees:take_largest(EmptyTree)),
    
    Tree = b5_trees:from_list([{1, a}, {2, b}]),
    ?assertError({badkey, 999}, b5_trees:get(999, Tree)),
    ?assertError({badkey, 999}, b5_trees:delete(999, Tree)),
    ?assertError({badkey, 999}, b5_trees:update(999, new_value, Tree)),
    ?assertError({badkey, 999}, b5_trees:take(999, Tree)).

test_iterator_edge_cases(_Config) ->
    Tree = b5_trees:from_list([{1, a}, {2, b}, {3, c}]),
    
    IterFromEnd = b5_trees:iterator_from(99, Tree),
    ?assertEqual(none, b5_trees:next(IterFromEnd)),
    
    IterFromStart = b5_trees:iterator_from(0, Tree),
    {Key1, Value1, _} = b5_trees:next(IterFromStart),
    ?assertEqual(1, Key1),
    ?assertEqual(a, Value1),
    
    IterReversedFromEnd = b5_trees:iterator_from(99, Tree, reversed),
    {Key3R, Value3R, _Iter3R} = b5_trees:next(IterReversedFromEnd),
    ?assertEqual(3, Key3R),
    ?assertEqual(c, Value3R),
    
    IterReversedFromStart = b5_trees:iterator_from(0, Tree, reversed),
    ?assertEqual(none, b5_trees:next(IterReversedFromStart)),
    
    IterReversedFrom2 = b5_trees:iterator_from(2, Tree, reversed),
    {Key2R, Value2R, Iter2R} = b5_trees:next(IterReversedFrom2),
    ?assertEqual(2, Key2R),
    ?assertEqual(b, Value2R),
    {Key1R, Value1R, _} = b5_trees:next(Iter2R),
    ?assertEqual(1, Key1R),
    ?assertEqual(a, Value1R).

test_fold_all_patterns(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual([], b5_trees:foldl(fun(K, V, Acc) -> [{K, V} | Acc] end, [], EmptyTree)),
    ?assertEqual([], b5_trees:foldr(fun(K, V, Acc) -> [{K, V} | Acc] end, [], EmptyTree)),
    
    SingleTree = b5_trees:from_list([{key, value}]),
    ?assertEqual([{key, value}], b5_trees:foldl(fun(K, V, Acc) -> [{K, V} | Acc] end, [], SingleTree)),
    ?assertEqual([{key, value}], b5_trees:foldr(fun(K, V, Acc) -> [{K, V} | Acc] end, [], SingleTree)),
    
    MultiTree = b5_trees:from_list([{1, a}, {2, b}, {3, c}, {4, d}, {5, e}]),
    FoldlResult = b5_trees:foldl(fun(K, V, Acc) -> [{K, V} | Acc] end, [], MultiTree),
    ?assertEqual([{5, e}, {4, d}, {3, c}, {2, b}, {1, a}], FoldlResult),
    
    FoldrResult = b5_trees:foldr(fun(K, V, Acc) -> [{K, V} | Acc] end, [], MultiTree),
    ?assertEqual([{1, a}, {2, b}, {3, c}, {4, d}, {5, e}], FoldrResult).

test_keys_all_patterns(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual([], b5_trees:keys(EmptyTree)),
    
    SingleTree = b5_trees:from_list([{key, value}]),
    ?assertEqual([key], b5_trees:keys(SingleTree)),
    
    MultiTree = b5_trees:from_list([{3, c}, {1, a}, {4, d}, {2, b}]),
    ?assertEqual([1, 2, 3, 4], b5_trees:keys(MultiTree)).

test_values_all_patterns(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual([], b5_trees:values(EmptyTree)),
    
    SingleTree = b5_trees:from_list([{key, value}]),
    ?assertEqual([value], b5_trees:values(SingleTree)),
    
    MultiTree = b5_trees:from_list([{3, c}, {1, a}, {4, d}, {2, b}]),
    ?assertEqual([a, b, c, d], b5_trees:values(MultiTree)).

test_to_list_all_patterns(_Config) ->
    EmptyTree = b5_trees:new(),
    ?assertEqual([], b5_trees:to_list(EmptyTree)),
    
    SingleTree = b5_trees:from_list([{key, value}]),
    ?assertEqual([{key, value}], b5_trees:to_list(SingleTree)),
    
    MultiTree = b5_trees:from_list([{3, c}, {1, a}, {4, d}, {2, b}]),
    ?assertEqual([{1, a}, {2, b}, {3, c}, {4, d}], b5_trees:to_list(MultiTree)).

test_deep_tree_operations(_Config) ->
    LargeList = [{I, I * 100} || I <- lists:seq(1, 50)],
    Tree = b5_trees:from_list(LargeList),
    
    Result1 = b5_trees:foldl(fun(K, V, Acc) -> Acc + K + V end, 0, Tree),
    ExpectedSum = lists:sum([I * 101 || I <- lists:seq(1, 50)]),
    ?assertEqual(ExpectedSum, Result1),
    
    Result2 = b5_trees:foldr(fun(K, _V, Acc) -> [K | Acc] end, [], Tree),
    ?assertEqual(lists:seq(1, 50), Result2),
    
    ?assertEqual(lists:seq(1, 50), b5_trees:keys(Tree)),
    ?assertEqual([I * 100 || I <- lists:seq(1, 50)], b5_trees:values(Tree)),
    
    ?assertEqual({1, 100}, b5_trees:smaller(2, Tree)),
    ?assertEqual({50, 5000}, b5_trees:larger(49, Tree)),
    
    Iter = b5_trees:iterator(Tree),
    {FirstKey, FirstVal, _} = b5_trees:next(Iter),
    ?assertEqual(1, FirstKey),
    ?assertEqual(100, FirstVal),
    
    IterFromMiddle = b5_trees:iterator_from(25, Tree),
    {MiddleKey, MiddleVal, _} = b5_trees:next(IterFromMiddle),
    ?assertEqual(25, MiddleKey),
    ?assertEqual(2500, MiddleVal).

test_deletion_rebalancing(_Config) ->
    InitialList = [{I, I * 10} || I <- lists:seq(1, 20)],
    Tree1 = b5_trees:from_list(InitialList),
    
    Tree2 = lists:foldl(
        fun(Key, TreeAcc) ->
            b5_trees:delete(Key, TreeAcc)
        end,
        Tree1,
        [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
    ),
    
    ?assertEqual(10, b5_trees:size(Tree2)),
    ?assertMatch({ok, _}, b5_trees:validate(Tree2)),
    
    Tree3 = lists:foldl(
        fun(Key, TreeAcc) ->
            {_, NewTree} = b5_trees:take(Key, TreeAcc),
            NewTree
        end,
        Tree2,
        [1, 3, 5, 7, 9]
    ),
    
    ?assertEqual(5, b5_trees:size(Tree3)),
    ?assertMatch({ok, _}, b5_trees:validate(Tree3)),
    
    {_SmallestKey, _SmallestVal, Tree4} = b5_trees:take_smallest(Tree3),
    ?assertEqual(4, b5_trees:size(Tree4)),
    
    {_LargestKey, _LargestVal, Tree5} = b5_trees:take_largest(Tree4),
    ?assertEqual(3, b5_trees:size(Tree5)).

test_internal_node_updates(_Config) ->
    LargeList = [{I, "value_prefix" ++ integer_to_list(I)} || I <- lists:seq(1, 30)],
    Tree1 = b5_trees:from_list(LargeList),
    
    Tree2 = lists:foldl(
        fun(Key, TreeAcc) ->
            case Key rem 3 of
                0 -> b5_trees:update(Key, updated_value, TreeAcc);
                1 -> b5_trees:update_with(Key, fun(OldVal) -> {updated, OldVal} end, TreeAcc);
                _ -> TreeAcc
            end
        end,
        Tree1,
        lists:seq(1, 30)
    ),
    
    ?assertEqual(updated_value, b5_trees:get(3, Tree2)),
    ?assertEqual(updated_value, b5_trees:get(6, Tree2)),
    ?assertEqual({updated, "value_prefix1"}, b5_trees:get(1, Tree2)),
    ?assertEqual({updated, "value_prefix4"}, b5_trees:get(4, Tree2)),
    ?assertEqual("value_prefix2", b5_trees:get(2, Tree2)),
    
    Tree3 = lists:foldl(
        fun(Key, TreeAcc) ->
            b5_trees:update_with(Key, fun(V) -> {double_updated, V} end, default_new, TreeAcc)
        end,
        Tree2,
        [31, 32, 33]
    ),
    
    ?assertEqual(default_new, b5_trees:get(31, Tree3)),
    ?assertEqual(default_new, b5_trees:get(32, Tree3)),
    ?assertEqual(default_new, b5_trees:get(33, Tree3)),
    ?assertEqual(33, b5_trees:size(Tree3)).

test_validation_edge_cases(_Config) ->
    EmptyTree = b5_trees:new(),
    {ok, EmptyStats} = b5_trees:validate(EmptyTree),
    ?assertMatch(#{}, EmptyStats),
    
    SingleTree = b5_trees:from_list([{key, value}]),
    {ok, SingleStats} = b5_trees:validate(SingleTree),
    ?assertMatch(#{}, SingleStats),
    
    LargeTree = b5_trees:from_list([{I, I} || I <- lists:seq(1, 100)]),
    {ok, LargeStats} = b5_trees:validate(LargeTree),
    ?assertMatch(#{}, LargeStats),
    
    SequentialTree = lists:foldl(
        fun(I, TreeAcc) ->
            b5_trees:enter(I, value, TreeAcc)
        end,
        b5_trees:new(),
        lists:seq(1, 50)
    ),
    {ok, SeqStats} = b5_trees:validate(SequentialTree),
    ?assertMatch(#{}, SeqStats),
    
    ModifiedTree = lists:foldl(
        fun(I, TreeAcc) when I rem 2 =:= 0 ->
            b5_trees:delete(I, TreeAcc);
           (_, TreeAcc) ->
            TreeAcc
        end,
        SequentialTree,
        lists:seq(1, 50)
    ),
    {ok, ModStats} = b5_trees:validate(ModifiedTree),
    ?assertMatch(#{}, ModStats).

test_complex_take_operations(_Config) ->
    VeryLargeList = [{I, element_value_for(I)} || I <- lists:seq(1, 100)],
    Tree1 = b5_trees:from_list(VeryLargeList),
    
    [begin
        {Value, TreeAfterTake} = b5_trees:take(Key, Tree1),
        ?assertEqual(element_value_for(Key), Value),
        ?assertEqual(99, b5_trees:size(TreeAfterTake)),
        ?assertNot(b5_trees:is_defined(Key, TreeAfterTake))
     end || Key <- [5, 25, 50, 75, 95]],
    
    Tree2 = lists:foldl(
        fun(_Key, TreeAcc) ->
            case b5_trees:size(TreeAcc) > 10 of
                true ->
                    {_KeyTaken, _Val, NewTree} = b5_trees:take_largest(TreeAcc),
                    NewTree;
                false ->
                    TreeAcc
            end
        end,
        Tree1,
        lists:seq(1, 85)
    ),
    
    ?assert(b5_trees:size(Tree2) =< 15),
    ?assertMatch({ok, _}, b5_trees:validate(Tree2)),
    
    Tree3 = lists:foldl(
        fun(_I, TreeAcc) ->
            case b5_trees:size(TreeAcc) > 3 of
                true ->
                    {_Key, _Val, NewTree} = b5_trees:take_smallest(TreeAcc),
                    NewTree;
                false ->
                    TreeAcc
            end
        end,
        Tree2,
        lists:seq(1, 50)
    ),
    
    ?assert(b5_trees:size(Tree3) =< 5),
    ?assertMatch({ok, _}, b5_trees:validate(Tree3)).

test_iterator_all_branches(_Config) ->
    LargeTree = b5_trees:from_list([{I, I * 7} || I <- lists:seq(1, 75)]),
    
    BasicIter = b5_trees:iterator(LargeTree),
    gather_all_iterator_elements(BasicIter, []),
    
    ReversedIter = b5_trees:iterator(LargeTree, reversed),
    gather_all_iterator_elements(ReversedIter, []),
    
    [begin
        Iter = b5_trees:iterator_from(StartKey, LargeTree),
        gather_all_iterator_elements(Iter, []),
        
        IterReversed = b5_trees:iterator_from(StartKey, LargeTree, reversed),
        gather_all_iterator_elements(IterReversed, [])
     end || StartKey <- [0, 1, 10, 37, 38, 39, 74, 75, 76, 100]],
    
    EmptyIter = b5_trees:iterator(b5_trees:new()),
    ?assertEqual(none, b5_trees:next(EmptyIter)).

test_comprehensive_node_patterns(_Config) ->
    Pattern1Tree = lists:foldl(
        fun(I, TreeAcc) -> b5_trees:insert(I, pattern1_value, TreeAcc) end,
        b5_trees:new(),
        [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25]
    ),
    
    test_all_operations_on_tree(Pattern1Tree),
    
    Pattern2Tree = lists:foldl(
        fun(I, TreeAcc) -> b5_trees:enter(I * 2, pattern2_value, TreeAcc) end,
        b5_trees:new(),
        lists:seq(1, 40)
    ),
    
    test_all_operations_on_tree(Pattern2Tree),
    
    Pattern3Tree = b5_trees:from_list([{Key, mixed_pattern} || Key <- lists:seq(1, 85)]),
    
    MixedOpsTree = lists:foldl(
        fun(I, TreeAcc) ->
            case I rem 7 of
                0 -> b5_trees:update(I, updated_by_7, TreeAcc);
                1 -> TreeAcc;
                2 -> b5_trees:delete(I, TreeAcc);
                3 -> b5_trees:enter(I, overwritten, TreeAcc);
                4 -> {_, NewTree} = b5_trees:take_any(I, TreeAcc), NewTree;
                5 -> b5_trees:update_with(I, fun(V) -> {enhanced, V} end, TreeAcc);
                6 -> TreeAcc
            end
        end,
        Pattern3Tree,
        lists:seq(1, 85)
    ),
    
    ?assertMatch({ok, _}, b5_trees:validate(MixedOpsTree)),
    test_all_operations_on_tree(MixedOpsTree).

element_value_for(Key) ->
    {complex_value, Key, Key * 3, "suffix_" ++ integer_to_list(Key)}.

gather_all_iterator_elements(Iter, Acc) ->
    case b5_trees:next(Iter) of
        none -> lists:reverse(Acc);
        {Key, Value, NextIter} ->
            gather_all_iterator_elements(NextIter, [{Key, Value} | Acc])
    end.

test_all_operations_on_tree(Tree) ->
    ?assertMatch({ok, _}, b5_trees:validate(Tree)),
    
    case b5_trees:size(Tree) > 0 of
        true ->
            {_SmallKey, _SmallVal} = b5_trees:smallest(Tree),
            {_LargeKey, _LargeVal} = b5_trees:largest(Tree),
            
            _Keys = b5_trees:keys(Tree),
            _Values = b5_trees:values(Tree),
            _ToList = b5_trees:to_list(Tree),
            
            FirstKey = hd(b5_trees:keys(Tree)),
            _SmallerResult = b5_trees:smaller(FirstKey + 1, Tree),
            _LargerResult = b5_trees:larger(FirstKey - 1, Tree),
            
            b5_trees:foldl(fun(_K, _V, Acc) -> Acc + 1 end, 0, Tree),
            b5_trees:foldr(fun(_K, _V, Acc) -> Acc + 1 end, 0, Tree);
        false ->
            ok
    end.

test_single_key_patterns(_Config) ->
    SingleTree = b5_trees:from_list([{single_key, single_value}]),
    
    ?assertEqual([single_key], b5_trees:keys(SingleTree)),
    ?assertEqual({single_key, single_value}, b5_trees:largest(SingleTree)),
    ?assertEqual({single_key, single_value}, b5_trees:smallest(SingleTree)),
    
    MappedSingle = b5_trees:map(fun(K, V) -> {mapped, K, V} end, SingleTree),
    ?assertEqual({mapped, single_key, single_value}, b5_trees:get(single_key, MappedSingle)),
    
    EmptyMapped = b5_trees:map(fun(K, V) -> {never_called, K, V} end, b5_trees:new()),
    ?assert(b5_trees:is_empty(EmptyMapped)),
    
    ?assertEqual(none, b5_trees:larger(single_key, SingleTree)),
    ?assertEqual(none, b5_trees:smaller(single_key, SingleTree)),
    ?assertEqual({single_key, single_value}, b5_trees:larger(before_single, SingleTree)),
    ?assertEqual({single_key, single_value}, b5_trees:smaller(z_after_single, SingleTree)).

test_four_key_leaf_patterns(_Config) ->
    FourKeyList = [{a, val_a}, {b, val_b}, {c, val_c}, {d, val_d}],
    FourKeyTree = b5_trees:from_list(FourKeyList),
    
    ?assertEqual([a, b, c, d], b5_trees:keys(FourKeyTree)),
    ?assertEqual([val_a, val_b, val_c, val_d], b5_trees:values(FourKeyTree)),
    ?assertEqual(FourKeyList, b5_trees:to_list(FourKeyTree)),
    
    Iter = b5_trees:iterator(FourKeyTree),
    {Key1, Val1, _Iter2} = b5_trees:next(Iter),
    ?assertEqual(a, Key1),
    ?assertEqual(val_a, Val1),
    
    IterReversed = b5_trees:iterator(FourKeyTree, reversed),
    {Key4, Val4, _} = b5_trees:next(IterReversed),
    ?assertEqual(d, Key4),
    ?assertEqual(val_d, Val4),
    
    FoldResult = b5_trees:foldl(fun(K, V, Acc) -> [{K, V} | Acc] end, [], FourKeyTree),
    ?assertEqual([{d, val_d}, {c, val_c}, {b, val_b}, {a, val_a}], FoldResult),
    
    MappedFour = b5_trees:map(fun(K, V) -> {transformed, K, V} end, FourKeyTree),
    ?assertEqual({transformed, b, val_b}, b5_trees:get(b, MappedFour)).

test_boundary_key_operations(_Config) ->
    BoundaryTree = b5_trees:from_list([{10, ten}, {20, twenty}, {30, thirty}, {40, forty}]),
    
    ?assertEqual({10, ten}, b5_trees:smaller(15, BoundaryTree)),
    ?assertEqual({20, twenty}, b5_trees:smaller(25, BoundaryTree)),
    ?assertEqual({30, thirty}, b5_trees:smaller(35, BoundaryTree)),
    ?assertEqual({40, forty}, b5_trees:smaller(50, BoundaryTree)),
    ?assertEqual(none, b5_trees:smaller(5, BoundaryTree)),
    
    ?assertEqual({20, twenty}, b5_trees:larger(15, BoundaryTree)),
    ?assertEqual({30, thirty}, b5_trees:larger(25, BoundaryTree)),
    ?assertEqual({40, forty}, b5_trees:larger(35, BoundaryTree)),
    ?assertEqual(none, b5_trees:larger(45, BoundaryTree)),
    ?assertEqual({10, ten}, b5_trees:larger(5, BoundaryTree)),
    
    SparseTree = b5_trees:from_list([{1, one}, {100, hundred}, {200, two_hundred}, {300, three_hundred}]),
    
    ?assertEqual(none, b5_trees:smaller(1, SparseTree)),
    ?assertEqual({1, one}, b5_trees:smaller(50, SparseTree)),
    ?assertEqual({100, hundred}, b5_trees:smaller(150, SparseTree)),
    ?assertEqual({200, two_hundred}, b5_trees:smaller(250, SparseTree)),
    ?assertEqual({300, three_hundred}, b5_trees:smaller(400, SparseTree)),
    
    ?assertEqual({100, hundred}, b5_trees:larger(1, SparseTree)),
    ?assertEqual({100, hundred}, b5_trees:larger(50, SparseTree)),
    ?assertEqual({200, two_hundred}, b5_trees:larger(100, SparseTree)),
    ?assertEqual({200, two_hundred}, b5_trees:larger(150, SparseTree)),
    ?assertEqual({300, three_hundred}, b5_trees:larger(200, SparseTree)),
    ?assertEqual({300, three_hundred}, b5_trees:larger(250, SparseTree)),
    ?assertEqual(none, b5_trees:larger(300, SparseTree)),
    ?assertEqual(none, b5_trees:larger(400, SparseTree)),
    
    IterFromMissing = b5_trees:iterator_from(150, SparseTree),
    {NextKey, NextVal, _} = b5_trees:next(IterFromMissing),
    ?assertEqual(200, NextKey),
    ?assertEqual(two_hundred, NextVal),
    
    IterFromMissingRev = b5_trees:iterator_from(150, SparseTree, reversed),
    {PrevKey, PrevVal, _} = b5_trees:next(IterFromMissingRev),
    ?assertEqual(100, PrevKey),
    ?assertEqual(hundred, PrevVal).
