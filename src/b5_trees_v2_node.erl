%% @doc Low-level API for working directly with B-tree nodes.
%%
%% This module provides a lower-level interface to B-tree operations that work
%% directly on tree nodes without tracking the overall tree size. This is useful
%% for performant re-wraps in languages like Elixir, where you might want to
%% wrap the tree structure under a struct instead of using Erlang records.
%%
%% The tree size is not tracked by these functions, making them more suitable
%% for cases where you need fine-grained control over the tree structure or
%% when integrating with external size tracking mechanisms.
%%
%% @reference https://en.wikipedia.org/wiki/B-tree
%% @reference https://www.geeksforgeeks.org/dsa/delete-operation-in-b-tree/
%%
%% Note regarding deletion: instead of preemptively rebalancing a node before
%% visiting it, the current implementation allows it to become temporarily
%% unbalanced. When this happens, the node is rebalanced immediately after.
%% This approach showed better performance and resulted in less code bloat.
-module(b5_trees_v2_node).

-export([
    delete/2,
    foldl/3,
    foldr/3,
    get/2,
    insert/4,
    iterator/2,
    iterator_from/3,
    keys/1,
    larger/2,
    largest/1,
    map/2,
    new/0,
    next/1,
    smaller/2,
    smallest/1,
    take/2,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    update/4,
    validate/2,
    values/1
]).

% -include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

% Nodes are tuples (for the most part), with different node types
% having different sizes. Size alone is not enough to differentiate
% between some node types, so we need to add tags. The following
% is the rationale for which node types get tags.
%
% After a few hundred keys, the approx average distribution of node types
% stabilises as follows:
% * INTERNAL2: 13%
% * INTERNAL3: 8%
% * INTERNAL4: 5%
% * LEAF2: 36%
% * LEAF3: 22%
% * LEAF4: 14%
%
%
% INTERNAL1 vs LEAF2 (4 elements):
%   Prioritise LEAF2. INTERNAL1 can only show at the root, whereas LEAF2 is the
%   most common type of node. Cheaper and smaller leaves at the expense of a
%   more expensive root node.
%
-define(INTERNAL1(K1, V1, C1, C2), {internal1, K1, V1, C1, C2}).
-define(LEAF2(K1, K2, V1, V2), {K1, K2, V1, V2}).
%
%
% INTERNAL2 vs LEAF3 (6 elements):
%   Prioritise INTERNAL2. Although LEAF3 is much more common than INTERNAL2,
%   most operations take place over internal nodes.
%
-define(INTERNAL2(K1, K2, Values, C1, C2, C3), {K1, K2, Values, C1, C2, C3}).
-define(LEAF3(K1, K2, K3, V1, V2, V3), {leaf3, K1, K2, K3, V1, V2, V3}).
%
%
% INTERNAL3 vs LEAF4 (8 elements):
%   Prioritise INTERNAL3. Same rationale as in previous case.
%
-define(INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4), {K1, K2, K3, Values, C1, C2, C3, C4}).
-define(LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), {leaf4, K1, K2, K3, K4, V1, V2, V3, V4}).
%
%
% Conflict-free:
%
-define(INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5),
    {K1, K2, K3, K4, Values, C1, C2, C3, C4, C5}
).
-define(LEAF1(K1, V1), {K1, V1}).
-define(LEAF0, leaf0).

%%
%% Regarding `Values' in internal nodes:
%% * INTERNAL4 stores tuples
%% * INTERNAL3 stores tuples
%% * INTERNAL2 stores an improper list
%%
%% INTERNAL2 being the most common internal node type, that gives us a slight
%% edge in some operations.
%%

%%%

-define(TAKEN(TakenPair, UpdatedNode), [TakenPair | UpdatedNode]).

% 3 elements
-define(TAKE_MERGED_FROM_RIGHT(TakenPair, MergedNode), {merged_from_right, TakenPair, MergedNode}).

% 3 elements
-define(TAKE_MERGED_FROM_LEFT(TakenPair, MergedNode), {merged_from_left, TakenPair, MergedNode}).

% 5 elements
-define(TAKE_ROTATED_FROM_LEFT(TakenPair, MergedNode, UpdatedLeft, UpdatedRight), {rotated_from_left, TakenPair, MergedNode, UpdatedLeft, UpdatedRight}).

% 5 elements
-define(TAKE_ROTATED_FROM_RIGHT(TakenPair, MergedNode, UpdatedLeft, UpdatedRight), {rotated_from_right, TakenPair, MergedNode, UpdatedLeft, UpdatedRight}).


%-define(DELETION_HELPER_LEFTMOST(Right, RParentK, RParentV), {Right, RParentK, RParentV}).
%
%-define(DELETION_HELPER_RIGHTMOST(Right, RParentK, RParentV), {rightmost, Right, RParentK, RParentV}).
%
%-define(DELETION_HELPER_MID(Left, LParentK, LParentV, Right, RParentK, RParentV), 
%        {Left, LParentK, LParentV, Right, RParentK, RParentV}).

-define(DHELPER(L, LK, LV, R, RK, RV), L, LK, LV, R, RK, RV).

-define(DHELPER, L, LK, LV, R, RK, RV).

-define(DLEFTMOST(R, RK, RV), ?DHELPER(none, none, none, R, RK, RV)).

-define(DRIGHTMOST(L, LK, LV), ?DHELPER(L, LK, LV, none, none, none)).

%-define(DROOT, ?DHELPER(root_level, none, none, root_level, none, none)).

-define(DSMALLEST(R, RK, RV), R, RK, RV).
-define(DSMALLEST, R, RK, RV).

-define(DLARGEST(L, LK, LV), L, LK, LV).
-define(DLARGEST, L, LK, LV).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------




-opaque t(Key, Value) :: root_only_node(Key, Value) | deep_node(Key, Value).
%% This type represents any tree node structure.
-export_type([t/2]).

-type empty_node() :: ?LEAF0.

%
% The fact that some node types are root-only allows us to optimize the
% recursive case - less potential patterns to match.
%
% Therefore, most of the exported API functions here match on the root-only
% nodes in their entry clauses, but only on deep node types after that.
%

-type root_only_node(Key, Value) ::
    (node_internal1(Key, Value)
    | node_leaf1(Key, Value)
    | empty_node()).

-type deep_node(Key, Value) ::
    (node_internal4(Key, Value)
    | node_internal3(Key, Value)
    | node_internal2(Key, Value)
    | node_leaf4(Key, Value)
    | node_leaf3(Key, Value)
    | node_leaf2(Key, Value)).

% -type non_empty_node(Key, Value) ::
%     (node_internal1(Key, Value)
%     | node_leaf1(Key, Value)
%     | deep_node(Key, Value)).

-type node_internal4(Key, Value) ::
    (?INTERNAL4(
        Key,
        Key,
        Key,
        Key,
        {Value, Value, Value, Value},
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_internal3(Key, Value) ::
    (?INTERNAL3(
        Key,
        Key,
        Key,
        {Value, Value, Value},
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_internal2(Key, Value) ::
    (?INTERNAL2(
        Key,
        Key,
        nonempty_improper_list(Value, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_internal1(Key, Value) ::
    (?INTERNAL1(
        Key,
        Value,
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_leaf4(Key, Value) ::
    (?LEAF4(
        Key,
        Key,
        Key,
        Key,
        Value,
        Value,
        Value,
        Value
    )).

-type node_leaf3(Key, Value) ::
    (?LEAF3(
        Key,
        Key,
        Key,
        Value,
        Value,
        Value
    )).

-type node_leaf2(Key, Value) :: ?LEAF2(Key, Key, Value, Value).

%-type node_leaf1(Key, Value) :: ?LEAF1(Key, Value).
-type node_leaf1(Key, Value) :: nonempty_improper_list(Key, Value).

%%%%%%%%%%%

% Dialyzer got too smart when it reasoned this, but it is indeed true.
% -type node_after_deletion(Key, Value) ::
%     node_internal3(Key, Value)
%     | node_internal2(Key, Value)
%     | node_internal1(Key, Value)
%     | node_leaf2(Key, Value)
%     | node_leaf1(Key, Value).
%
% -type deep_node_after_insertion(Key, Value) ::
%     node_internal4(Key, Value)
%     | node_internal3(Key, Value)
%     | node_leaf4(Key, Value)
%     | node_leaf3(Key, Value).
%
% % Temporary situation before rebalance
% -type unbalanced_node(Key, Value) :: node_internal1(Key, Value).

%%%%%%%%%%%

-type insertion_value_wrap(Value) :: Value | fun(() -> Value).
-type insertion_value_eval() :: eager | lazy.
-export_type([insertion_value_wrap/1, insertion_value_eval/0]).

-type update_value_wrap(Value, UpdatedValue) :: Value | fun((Value) -> UpdatedValue).
-type update_value_eval() :: eager | lazy.
-export_type([update_value_wrap/2, update_value_eval/0]).

%%%%%%%%%%%

% -type split_result(Key, Value) :: internal_split_result(Key, Value) | leaf_split_result(Key, Value).

-type internal_split_result(Key, Value) :: split_result(
    Key, Value, node_internal2(Key, Value), node_internal2(Key, Value)
).

-type leaf_split_result(Key, Value) :: split_result(
    Key, Value, node_leaf2(Key, Value), node_leaf2(Key, Value)
).

-type split_result(Key, Value, SplitL, SplitR) :: {split, Key, Value, SplitL, SplitR}.

%%%%%%%%%%%

-opaque iter(Key, Value) :: forward_iter(Key, Value) | reverse_iter(Key, Value).
-export_type([iter/2]).

% FIXME remove default value, it messes up with Dialyzer
-record(b5_trees_v2_forward_iter, {steps :: [iterator_step(_, _)]}).
-type forward_iter(Key, Value) :: #b5_trees_v2_forward_iter{steps :: [iterator_step(Key, Value)]}.

-record(b5_trees_v2_reverse_iter, {steps :: [iterator_step(_, _)]}).
-type reverse_iter(Key, Value) :: #b5_trees_v2_reverse_iter{steps :: [iterator_step(Key, Value)]}.

-type iterator_step(Key, Value) :: {Key, Value} | {Key, Value, NextChild :: deep_node(Key, Value)}.

%%%%%%%%%%%

-type valid_stats() :: #{
    height := non_neg_integer(),
    node_counts := stats_node_counts()
}.
-export_type([valid_stats/0]).

-type stats_node_counts() :: #{
    internal4 => pos_integer(),
    internal3 => pos_integer(),
    internal2 => pos_integer(),
    internal1 => pos_integer(),
    leaf4 => pos_integer(),
    leaf3 => pos_integer(),
    leaf2 => pos_integer(),
    leaf1 => pos_integer()
}.
-export_type([stats_node_counts/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Removes the node with the specified key from the tree node.
%% Fails with a `{badkey, Key}' exception if the key is not present.
-spec delete(Key, t(Key, Value)) -> t(Key, Value).
delete(Key, Node) ->
    ?TAKEN(_, UpdatedNode) = root_take_key(Key, Node),
    UpdatedNode.

%% @doc Folds the tree node from left to right (smallest key to largest).
%% Returns the final accumulator value.
-spec foldl(fun((Key, Value, Acc1) -> Acc2), Acc0, t(Key, Value)) -> AccN when
    AccN :: Acc2, Acc2 :: Acc1, Acc1 :: Acc0.
foldl(Fun, Acc, ?INTERNAL1(K1, V1, C1, C2)) ->
    Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
    foldl_recur(Fun, Acc2, C2);
foldl(Fun, Acc, ?LEAF1(K1, V1)) ->
    Fun(K1, V1, Acc);
foldl(_Fun, Acc, ?LEAF0) ->
    Acc;
foldl(Fun, Acc, Node) ->
    foldl_recur(Fun, Acc, Node).

%% @doc Folds the tree node from right to left (largest key to smallest).
%% Returns the final accumulator value.
-spec foldr(fun((Key, Value, Acc1) -> Acc2), Acc0, t(Key, Value)) -> AccN when
    AccN :: Acc2, Acc2 :: Acc1, Acc1 :: Acc0.
foldr(Fun, Acc, ?INTERNAL1(K1, V1, C1, C2)) ->
    Acc2 = Fun(K1, V1, foldr_recur(Fun, Acc, C2)),
    foldr_recur(Fun, Acc2, C1);
foldr(Fun, Acc, ?LEAF1(K1, V1)) ->
    Fun(K1, V1, Acc);
foldr(_Fun, Acc, ?LEAF0) ->
    Acc;
foldr(Fun, Acc, Node) ->
    foldr_recur(Fun, Acc, Node).

%% @doc Retrieves the value associated with the specified key.
%% Fails with a `{badkey, Key}' exception if the key is not present.
-spec get(Key, t(Key, Value)) -> Value.
get(Key, ?INTERNAL1(K1, V1, C1, C2)) ->
    get_internal1(Key, K1, V1, C1, C2);
get(Key, ?LEAF1(K1, V1)) ->
    get_leaf1(Key, K1, V1);
get(Key, ?LEAF0) ->
    error_badkey(Key);
get(Key, Node) ->
    get_recur(Key, Node).

%% @doc Inserts a key-value pair into the tree node.
%% Fails with a `{key_exists, Key}' exception if the key already exists.
%% The value can be evaluated eagerly or lazily based on the evaluation strategy.
-spec insert(Key, insertion_value_eval(), insertion_value_wrap(Value), t(Key, Value)) ->
    t(Key, Value).
-dialyzer({no_underspecs, insert/4}).
insert(Key, ValueEval, ValueWrap, ?INTERNAL1(K1, V1, C1, C2)) ->
    insert_internal1(Key, ValueEval, ValueWrap, K1, V1, C1, C2);
insert(Key, ValueEval, ValueWrap, ?LEAF1(K1, V1)) ->
    insert_leaf1(Key, ValueEval, ValueWrap, K1, V1);
insert(Key, ValueEval, ValueWrap, ?LEAF0) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF1(Key, Value);
insert(Key, ValueEval, ValueWrap, Root) ->
    case insert_recur(Key, ValueEval, ValueWrap, Root) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            ?INTERNAL1(SplitK, SplitV, SplitL, SplitR);
        UpdatedRoot ->
            UpdatedRoot
    end.

%% @doc Creates an iterator for traversing the tree node entries.
%% Can iterate in `ordered' (ascending) or `reversed' (descending) direction.
-spec iterator(t(Key, Value), ordered | reversed) -> iter(Key, Value).
iterator(Node, ordered) ->
    #b5_trees_v2_forward_iter{steps = iterator_steps_l(Node)};
iterator(Node, reversed) ->
    #b5_trees_v2_reverse_iter{steps = iterator_steps_r(Node)}.

%% @doc Creates an iterator starting from the first key >= the specified key.
%% Can iterate in `ordered' (ascending) or `reversed' (descending) direction.
-spec iterator_from(Key, t(Key, Value), ordered | reversed) -> iter(Key, Value).
iterator_from(Key, Node, ordered) ->
    #b5_trees_v2_forward_iter{steps = iterator_steps_l_from(Key, Node)};
iterator_from(Key, Node, reversed) ->
    #b5_trees_v2_reverse_iter{steps = iterator_steps_r_from(Key, Node)}.

%% @doc Returns all keys in the tree node as an ordered list.
-spec keys(t(Key, _)) -> [Key].
-dialyzer({no_underspecs, keys/1}).
keys(?INTERNAL1(K1, _, C1, C2)) ->
    Acc2 = [K1 | keys_recur(C2, [])],
    keys_recur(C1, Acc2);
keys(?LEAF1(K1, _)) ->
    [K1];
keys(?LEAF0) ->
    [];
keys(Node) ->
    keys_recur(Node, []).

%% @doc Returns the smallest key-value pair where the key is larger than the given key.
%% Returns `none' if no such key exists.
-spec larger(Key, t(Key, Value)) -> {Key, Value} | none.
larger(Key, ?INTERNAL1(K1, V1, C1, C2)) ->
    case Key < K1 of
        true ->
            case larger_recur(Key, C1) of
                none -> {K1, V1};
                Pair -> Pair
            end;
        _ ->
            larger_recur(Key, C2)
    end;
larger(Key, ?LEAF1(K1, V1)) ->
    case Key < K1 of
        true ->
            {K1, V1};
        _ ->
            none
    end;
larger(_, ?LEAF0) ->
    none;
larger(Key, Node) ->
    larger_recur(Key, Node).

%% @doc Returns the largest key-value pair in the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
-spec largest(t(Key, Value)) -> {Key, Value}.
largest(?INTERNAL1(_, _, _, C2)) ->
    largest_recur(C2);
largest(?LEAF1(K1, V1)) ->
    {K1, V1};
largest(?LEAF0) ->
    error_empty_tree();
largest(Node) ->
    largest_recur(Node).

%% @doc Creates a new empty tree node.
-spec new() -> t(_, _).
new() ->
    % Without this wrapper, Dialyzer gets too clever. `t/2' being an opaque
    % type, it shouldn't...
    b5_trees_util:dialyzer_opaque_term(?LEAF0).

%% @doc Returns the next key-value pair from an iterator.
%% Returns `{Key, Value, NewIter}' or `none' if no more entries remain.
-spec next(iter(Key, Value)) -> {Key, Value, iter(Key, Value)} | none.
next(#b5_trees_v2_forward_iter{steps = Steps} = Iter) ->
    case Steps of
        [{Key, Value} | NextSteps] ->
            UpdatedIter = Iter#b5_trees_v2_forward_iter{steps = NextSteps},
            {Key, Value, UpdatedIter};
        [{Key, Value, NextChild} | NextNextSteps] ->
            UpdatedIter = Iter#b5_trees_v2_forward_iter{
                steps = iterator_steps_l_recur(NextChild, NextNextSteps)
            },
            {Key, Value, UpdatedIter};
        [] ->
            none
    end;
next(#b5_trees_v2_reverse_iter{steps = Steps} = Iter) ->
    case Steps of
        [{Key, Value} | NextSteps] ->
            UpdatedIter = Iter#b5_trees_v2_reverse_iter{steps = NextSteps},
            {Key, Value, UpdatedIter};
        [{Key, Value, NextChild} | NextNextSteps] ->
            UpdatedIter = Iter#b5_trees_v2_reverse_iter{
                steps = iterator_steps_r_recur(NextChild, NextNextSteps)
            },
            {Key, Value, UpdatedIter};
        [] ->
            none
    end.

%% @doc Maps a function over all key-value pairs in the tree node.
%% Returns a new tree node with the same keys and transformed values.
-spec map(fun((Key, Value) -> MappedValue), t(Key, Value)) -> t(Key, MappedValue).
%% erlfmt:ignore A bug in test coverage will show the LEAF1 case wrong
map(Fun, ?INTERNAL1(K1, V1, C1, C2)) ->
    ?INTERNAL1(
        K1,
        Fun(K1, V1),
        map_recur(Fun, C1),
        map_recur(Fun, C2)
    );
map(Fun, ?LEAF1(K1, V1)) ->
    ?LEAF1(K1, Fun(K1, V1));
map(_, ?LEAF0) ->
    ?LEAF0;
map(Fun, Node) ->
    map_recur(Fun, Node).

%% @doc Returns the largest key-value pair where the key is smaller than the given key.
%% Returns `none' if no such key exists.
-spec smaller(Key, t(Key, Value)) -> {Key, Value} | none.
smaller(Key, ?INTERNAL1(K1, V1, C1, C2)) ->
    case Key > K1 of
        true ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Pair -> Pair
            end;
        _ ->
            smaller_recur(Key, C1)
    end;
smaller(Key, ?LEAF1(K1, V1)) ->
    case Key > K1 of
        true ->
            {K1, V1};
        _ ->
            none
    end;
smaller(_, ?LEAF0) ->
    none;
smaller(Key, Node) ->
    smaller_recur(Key, Node).

%% @doc Returns the smallest key-value pair in the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
-spec smallest(t(Key, Value)) -> {Key, Value}.
smallest(?INTERNAL1(_, _, C1, _)) ->
    smallest_recur(C1);
smallest(?LEAF1(K1, V1)) ->
    {K1, V1};
smallest(?LEAF0) ->
    error_empty_tree();
smallest(Node) ->
    smallest_recur(Node).

%% @doc Removes and returns the value associated with the specified key.
%% Fails with a `{badkey, Key}' exception if the key is not present.
%% Returns `{Value, UpdatedNode}'.
-spec take(Key, t(Key, Value)) -> {Value, t(Key, Value)}.
take(Key, Node) ->
    root_take_key(Key, Node).

%% @doc Removes and returns the largest key-value pair from the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
%% Returns `{Key, Value, UpdatedNode}'.
-spec take_largest(t(Key, Value)) ->
    {Key, Value, t(Key, Value)}.
% -dialyzer({no_underspecs, take_largest/1}).
take_largest(Node) ->
    root_take_largest(Node).

%% @doc Removes and returns the smallest key-value pair from the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
%% Returns `{Key, Value, UpdatedNode}'.
-spec take_smallest(t(Key, Value)) ->
    {Key, Value, t(Key, Value)}.
% -dialyzer({no_underspecs, take_smallest/1}).
take_smallest(Node) ->
    root_take_smallest(Node).

%% @doc Converts the tree node into an ordered list of key-value tuples.
-spec to_list(t(Key, Value)) -> [{Key, Value}].
-dialyzer({no_underspecs, to_list/1}).
to_list(?INTERNAL1(K1, V1, C1, C2)) ->
    Acc2 = [{K1, V1} | to_list_recur(C2, [])],
    to_list_recur(C1, Acc2);
to_list(?LEAF1(K1, V1)) ->
    [{K1, V1}];
to_list(?LEAF0) ->
    [];
to_list(Node) ->
    to_list_recur(Node, []).

%% @doc Updates the value associated with the specified key.
%% Fails with a `{badkey, Key}' exception if the key is not present.
%% The new value can be computed eagerly or lazily based on the evaluation strategy.
-spec update(
    Key,
    update_value_eval(),
    update_value_wrap(Value, UpdatedValue),
    t(Key, Value)
) -> t(Key, Value | UpdatedValue).
update(Key, ValueEval, ValueWrap, ?INTERNAL1(K1, V1, C1, C2)) ->
    update_internal1(Key, ValueEval, ValueWrap, K1, V1, C1, C2);
update(Key, ValueEval, ValueWrap, ?LEAF1(K1, V1)) ->
    update_leaf1(Key, ValueEval, ValueWrap, K1, V1);
update(Key, _ValueWrap, _ValueEval, ?LEAF0) ->
    error_badkey(Key);
update(Key, ValueEval, ValueWrap, Node) ->
    update_recur(Key, ValueEval, ValueWrap, Node).

%% @doc Validates the internal structure of the tree node.
%% Takes the expected number of keys and returns validation statistics
%% or an error if the tree structure is inconsistent.
-spec validate(non_neg_integer(), t(_, _)) ->
    {ok, valid_stats()} | {error, term()}.
validate(ExpectedNrOfKeys, Root) ->
    #{
        min_height := MinHeight,
        max_height := MaxHeight,
        node_counts := NodeCounts,
        wrong_depth_counts := WrongDepthCounts
    } = Stats = stats(Root),

    NrOfKeys = count_keys_from_stats(NodeCounts),

    if
        WrongDepthCounts =/= #{} ->
            {error, {root_only_nodes_deep_in_the_tree, WrongDepthCounts}};
        MinHeight =/= MaxHeight ->
            {error, {inconsistent_heights, Stats}};
        NrOfKeys =/= ExpectedNrOfKeys ->
            {error, {inconsistent_nr_of_keys, {expected, ExpectedNrOfKeys}, Stats}};
        true ->
            {ok, #{
                height => MinHeight,
                node_counts => NodeCounts
            }}
    end.

%% @doc Returns all values in the tree node as an ordered list,
%% sorted by their corresponding keys.
-spec values(t(_, Value)) -> [Value].
-dialyzer({no_underspecs, values/1}).
values(?INTERNAL1(_, V1, C1, C2)) ->
    Acc2 = [V1 | values_recur(C2, [])],
    values_recur(C1, Acc2);
values(?LEAF1(_, V1)) ->
    [V1];
values(?LEAF0) ->
    [];
values(Node) ->
    values_recur(Node, []).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Exceptions
%% ------------------------------------------------------------------

-compile({inline, error_badkey/1}).
-spec error_badkey(_) -> no_return().
error_badkey(Key) ->
    error({badkey, Key}).

-compile({inline, error_empty_tree/0}).
-spec error_empty_tree() -> no_return().
error_empty_tree() ->
    error(empty_tree).

-compile({inline, error_key_exists/1}).
-spec error_key_exists(_) -> no_return().
error_key_exists(Key) ->
    error({key_exists, Key}).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Get
%% ------------------------------------------------------------------

-spec get_recur(Key, deep_node(Key, Value)) -> Value.
get_recur(Key, ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)) ->
    get_internal4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
get_recur(Key, ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4)) ->
    get_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4);
get_recur(Key, ?INTERNAL2(K1, K2, Values, C1, C2, C3)) ->
    get_internal2(Key, K1, K2, Values, C1, C2, C3);
get_recur(Key, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    get_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4);
get_recur(Key, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    get_leaf3(Key, K1, K2, K3, V1, V2, V3);
get_recur(Key, ?LEAF2(K1, K2, V1, V2)) ->
    get_leaf2(Key, K1, K2, V1, V2).

-compile({inline, [get_internal4/11]}).
get_internal4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    if
        Key > K2 ->
            if
                Key < K4 ->
                    if
                        Key > K3 ->
                            get_recur(Key, C4);
                        Key < K3 ->
                            get_recur(Key, C3);
                        true ->
                            element(3, Values)
                    end;
                Key > K4 ->
                    get_recur(Key, C5);
                true ->
                    element(4, Values)
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    get_recur(Key, C2);
                Key < K1 ->
                    get_recur(Key, C1);
                true ->
                    element(1, Values)
            end;
        true ->
            element(2, Values)
    end.

-compile({inline, [get_internal3/9]}).
get_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4) ->
    if
        Key > K2 ->
            if
                Key < K3 ->
                    get_recur(Key, C3);
                Key > K3 ->
                    get_recur(Key, C4);
                true ->
                    element(3, Values)
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    get_recur(Key, C2);
                Key < K1 ->
                    get_recur(Key, C1);
                true ->
                    element(1, Values)
            end;
        true ->
            element(2, Values)
    end.

-compile({inline, [get_internal2/7]}).
get_internal2(Key, K1, K2, Values, C1, C2, C3) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    get_recur(Key, C2);
                Key > K2 ->
                    get_recur(Key, C3);
                true ->
                    tl(Values)
            end;
        Key < K1 ->
            get_recur(Key, C1);
        true ->
            hd(Values)
    end.

-compile({inline, [get_internal1/5]}).
get_internal1(Key, K1, V1, C1, C2) ->
    if
        Key < K1 ->
            get_recur(Key, C1);
        Key > K1 ->
            get_recur(Key, C2);
        true ->
            V1
    end.

-compile({inline, [get_leaf4/9]}).
get_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4) ->
    if
        Key > K2 ->
            if
                Key == K3 ->
                    V3;
                Key == K4 ->
                    V4;
                true ->
                    error_badkey(Key)
            end;
        Key < K2 ->
            if
                Key == K1 ->
                    V1;
                true ->
                    error_badkey(Key)
            end;
        true ->
            V2
    end.

-compile({inline, [get_leaf3/7]}).
get_leaf3(Key, K1, K2, K3, V1, V2, V3) ->
    if
        Key < K2 ->
            if
                Key == K1 ->
                    V1;
                true ->
                    error_badkey(Key)
            end;
        Key > K2 ->
            if
                Key == K3 ->
                    V3;
                true ->
                    error_badkey(Key)
            end;
        true ->
            V2
    end.

-compile({inline, [get_leaf2/5]}).
get_leaf2(Key, K1, K2, V1, V2) ->
    if
        Key == K2 ->
            V2;
        Key == K1 ->
            V1;
        true ->
            error_badkey(Key)
    end.

-compile({inline, [get_leaf1/3]}).
get_leaf1(Key, K1, V1) ->
    if
        Key == K1 ->
            V1;
        true ->
            error_badkey(Key)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Insertion
%% ------------------------------------------------------------------

% -spec insert_recur(
%     Key,
%     insertion_value_wrap(Value),
%     insertion_value_eval(),
%     deep_node(Key, Value)
% ) -> deep_node_after_insertion(Key, Value) | split_result(Key, Value).
insert_recur(Key, ValueEval, ValueWrap, ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)) ->
    insert_internal4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
insert_recur(Key, ValueEval, ValueWrap, ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4)) ->
    insert_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4);
insert_recur(Key, ValueEval, ValueWrap, ?INTERNAL2(K1, K2, Values, C1, C2, C3)) ->
    insert_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3);
insert_recur(Key, ValueEval, ValueWrap, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    insert_leaf4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4);
insert_recur(Key, ValueEval, ValueWrap, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    insert_leaf3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
insert_recur(Key, ValueEval, ValueWrap, ?LEAF2(K1, K2, V1, V2)) ->
    insert_leaf2(Key, ValueEval, ValueWrap, K1, K2, V1, V2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal4/13]}).
insert_internal4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    if
        Key > K2 ->
            if
                Key < K4 ->
                    if
                        Key > K3 ->
                            insert_internal4_child4(
                                Key,
                                ValueEval,
                                ValueWrap,
                                K1,
                                K2,
                                K3,
                                K4,
                                Values,
                                C1,
                                C2,
                                C3,
                                C4,
                                C5
                            );
                        Key < K3 ->
                            insert_internal4_child3(
                                Key,
                                ValueEval,
                                ValueWrap,
                                K1,
                                K2,
                                K3,
                                K4,
                                Values,
                                C1,
                                C2,
                                C3,
                                C4,
                                C5
                            );
                        true ->
                            error_key_exists(Key)
                    end;
                Key > K4 ->
                    insert_internal4_child5(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
                    );
                true ->
                    error_key_exists(Key)
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    insert_internal4_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
                    );
                Key < K1 ->
                    insert_internal4_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
                    );
                true ->
                    error_key_exists(Key)
            end;
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal4_child1/13]}).
insert_internal4_child1(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            {V1, V2, V3, V4} = Values,
            internal_split(
                SplitK,
                K1,
                K2,
                K3,
                K4,
                SplitV,
                V1,
                V2,
                V3,
                V4,
                SplitL,
                SplitR,
                C2,
                C3,
                C4,
                C5
            );
        UpdatedC1 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                UpdatedC1,
                C2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child2/13]}).
insert_internal4_child2(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            {V1, V2, V3, V4} = Values,
            internal_split(
                K1,
                SplitK,
                K2,
                K3,
                K4,
                V1,
                SplitV,
                V2,
                V3,
                V4,
                C1,
                SplitL,
                SplitR,
                C3,
                C4,
                C5
            );
        UpdatedC2 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                C1,
                UpdatedC2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child3/13]}).
insert_internal4_child3(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            {V1, V2, V3, V4} = Values,
            internal_split(
                K1,
                K2,
                SplitK,
                K3,
                K4,
                V1,
                V2,
                SplitV,
                V3,
                V4,
                C1,
                C2,
                SplitL,
                SplitR,
                C4,
                C5
            );
        UpdatedC3 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                C1,
                C2,
                UpdatedC3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child4/13]}).
insert_internal4_child4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            {V1, V2, V3, V4} = Values,
            internal_split(
                K1,
                K2,
                K3,
                SplitK,
                K4,
                V1,
                V2,
                V3,
                SplitV,
                V4,
                C1,
                C2,
                C3,
                SplitL,
                SplitR,
                C5
            );
        UpdatedC4 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                C1,
                C2,
                C3,
                UpdatedC4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child5/13]}).
insert_internal4_child5(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    case insert_recur(Key, ValueEval, ValueWrap, C5) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            {V1, V2, V3, V4} = Values,
            internal_split(
                K1,
                K2,
                K3,
                K4,
                SplitK,
                V1,
                V2,
                V3,
                V4,
                SplitV,
                C1,
                C2,
                C3,
                C4,
                SplitL,
                SplitR
            );
        UpdatedC5 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                C1,
                C2,
                C3,
                C4,
                UpdatedC5
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal3/11]}).
insert_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    if
        Key > K2 ->
            if
                Key < K3 ->
                    insert_internal3_child3(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4
                    );
                Key > K3 ->
                    insert_internal3_child4(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4
                    );
                true ->
                    error_key_exists(Key)
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    insert_internal3_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4
                    );
                Key < K1 ->
                    insert_internal3_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4
                    );
                true ->
                    error_key_exists(Key)
            end;
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal3_child1/11]}).
insert_internal3_child1(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                SplitK,
                K1,
                K2,
                K3,
                {SplitV, V1, V2, V3},
                SplitL,
                SplitR,
                C2,
                C3,
                C4
            );
        UpdatedC1 ->
            ?INTERNAL3(K1, K2, K3, Values, UpdatedC1, C2, C3, C4)
    end.

-compile({inline, [insert_internal3_child2/11]}).
insert_internal3_child2(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                SplitK,
                K2,
                K3,
                {V1, SplitV, V2, V3},
                C1,
                SplitL,
                SplitR,
                C3,
                C4
            );
        UpdatedC2 ->
            ?INTERNAL3(K1, K2, K3, Values, C1, UpdatedC2, C3, C4)
    end.

-compile({inline, [insert_internal3_child3/11]}).
insert_internal3_child3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                K2,
                SplitK,
                K3,
                {V1, V2, SplitV, V3},
                C1,
                C2,
                SplitL,
                SplitR,
                C4
            );
        UpdatedC3 ->
            ?INTERNAL3(K1, K2, K3, Values, C1, C2, UpdatedC3, C4)
    end.

-compile({inline, [insert_internal3_child4/11]}).
insert_internal3_child4(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                K2,
                K3,
                SplitK,
                {V1, V2, V3, SplitV},
                C1,
                C2,
                C3,
                SplitL,
                SplitR
            );
        UpdatedC4 ->
            ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, UpdatedC4)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal2/9]}).
insert_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    insert_internal2_child2(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3);
                Key > K2 ->
                    insert_internal2_child3(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3);
                true ->
                    error_key_exists(Key)
            end;
        Key < K1 ->
            insert_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3);
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal2_child1/9]}).
insert_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                SplitK,
                K1,
                K2,
                {SplitV, V1, V2},
                SplitL,
                SplitR,
                C2,
                C3
            );
        UpdatedC1 ->
            ?INTERNAL2(K1, K2, Values, UpdatedC1, C2, C3)
    end.

-compile({inline, [insert_internal2_child2/9]}).
insert_internal2_child2(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                K1,
                SplitK,
                K2,
                {V1, SplitV, V2},
                C1,
                SplitL,
                SplitR,
                C3
            );
        UpdatedC2 ->
            ?INTERNAL2(K1, K2, Values, C1, UpdatedC2, C3)
    end.

-compile({inline, [insert_internal2_child3/9]}).
insert_internal2_child3(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                K1,
                K2,
                SplitK,
                {V1, V2, SplitV},
                C1,
                C2,
                SplitL,
                SplitR
            );
        UpdatedC3 ->
            ?INTERNAL2(K1, K2, Values, C1, C2, UpdatedC3)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal1/7]}).
insert_internal1(Key, ValueEval, ValueWrap, K1, V1, C1, C2) ->
    if
        Key < K1 ->
            insert_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, C1, C2);
        Key > K1 ->
            insert_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, C1, C2);
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal1_child1/7]}).
insert_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, C1, C2) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            ?INTERNAL2(
                SplitK,
                K1,
                [SplitV | V1],
                SplitL,
                SplitR,
                C2
            );
        UpdatedC1 ->
            ?INTERNAL1(
                K1,
                V1,
                UpdatedC1,
                C2
            )
    end.

-compile({inline, [insert_internal1_child2/7]}).
insert_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, C1, C2) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            ?INTERNAL2(
                K1,
                SplitK,
                [V1 | SplitV],
                C1,
                SplitL,
                SplitR
            );
        UpdatedC2 ->
            ?INTERNAL1(
                K1,
                V1,
                C1,
                UpdatedC2
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_leaf4/11]}).
insert_leaf4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4) ->
    if
        Key > K2 ->
            if
                Key < K4 ->
                    if
                        Key > K3 ->
                            insert_leaf4_key4(
                                Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4
                            );
                        Key < K3 ->
                            insert_leaf4_key3(
                                Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4
                            );
                        true ->
                            error_key_exists(Key)
                    end;
                Key > K4 ->
                    insert_leaf4_key5(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4);
                true ->
                    error_key_exists(Key)
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    insert_leaf4_key2(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4);
                Key < K1 ->
                    insert_leaf4_key1(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4);
                true ->
                    error_key_exists(Key)
            end;
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_leaf4_key1/11]}).
insert_leaf4_key1(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    leaf_split(
        Key,
        K1,
        K2,
        K3,
        K4,
        Value,
        V1,
        V2,
        V3,
        V4
    ).

-compile({inline, [insert_leaf4_key2/11]}).
insert_leaf4_key2(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    leaf_split(
        K1,
        Key,
        K2,
        K3,
        K4,
        V1,
        Value,
        V2,
        V3,
        V4
    ).

-compile({inline, [insert_leaf4_key3/11]}).
insert_leaf4_key3(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    leaf_split(
        K1,
        K2,
        Key,
        K3,
        K4,
        V1,
        V2,
        Value,
        V3,
        V4
    ).

-compile({inline, [insert_leaf4_key4/11]}).
insert_leaf4_key4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    leaf_split(
        K1,
        K2,
        K3,
        Key,
        K4,
        V1,
        V2,
        V3,
        Value,
        V4
    ).

-compile({inline, [insert_leaf4_key5/11]}).
insert_leaf4_key5(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    leaf_split(
        K1,
        K2,
        K3,
        K4,
        Key,
        V1,
        V2,
        V3,
        V4,
        Value
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_leaf3/9]}).
insert_leaf3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    insert_leaf3_key1(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
                Key > K1 ->
                    insert_leaf3_key2(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
                true ->
                    error_key_exists(Key)
            end;
        Key > K2 ->
            if
                Key < K3 ->
                    insert_leaf3_key3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
                Key > K3 ->
                    insert_leaf3_key4(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
                true ->
                    error_key_exists(Key)
            end;
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_leaf3_key1/9]}).
insert_leaf3_key1(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF4(Key, K1, K2, K3, Value, V1, V2, V3).

-compile({inline, [insert_leaf3_key2/9]}).
insert_leaf3_key2(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF4(K1, Key, K2, K3, V1, Value, V2, V3).

-compile({inline, [insert_leaf3_key3/9]}).
insert_leaf3_key3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF4(K1, K2, Key, K3, V1, V2, Value, V3).

-compile({inline, [insert_leaf3_key4/9]}).
insert_leaf3_key4(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF4(K1, K2, K3, Key, V1, V2, V3, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_leaf2/7]}).
insert_leaf2(Key, ValueEval, ValueWrap, K1, K2, V1, V2) ->
    if
        Key < K2 ->
            if
                Key > K1 ->
                    insert_leaf2_key2(Key, ValueEval, ValueWrap, K1, K2, V1, V2);
                Key < K1 ->
                    insert_leaf2_key1(Key, ValueEval, ValueWrap, K1, K2, V1, V2);
                true ->
                    error_key_exists(Key)
            end;
        Key > K2 ->
            insert_leaf2_key3(Key, ValueEval, ValueWrap, K1, K2, V1, V2);
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_leaf2_key1/7]}).
insert_leaf2_key1(Key, ValueEval, ValueWrap, K1, K2, V1, V2) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF3(Key, K1, K2, Value, V1, V2).

-compile({inline, [insert_leaf2_key2/7]}).
insert_leaf2_key2(Key, ValueEval, ValueWrap, K1, K2, V1, V2) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF3(K1, Key, K2, V1, Value, V2).

-compile({inline, [insert_leaf2_key3/7]}).
insert_leaf2_key3(Key, ValueEval, ValueWrap, K1, K2, V1, V2) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF3(K1, K2, Key, V1, V2, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_leaf1/5]}).
insert_leaf1(Key, ValueEval, ValueWrap, K1, V1) ->
    if
        Key < K1 ->
            insert_leaf1_key1(Key, ValueEval, ValueWrap, K1, V1);
        Key > K1 ->
            insert_leaf1_key2(Key, ValueEval, ValueWrap, K1, V1);
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_leaf1_key1/5]}).
insert_leaf1_key1(Key, ValueEval, ValueWrap, K1, V1) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF2(Key, K1, Value, V1).

-compile({inline, [insert_leaf1_key2/5]}).
insert_leaf1_key2(Key, ValueEval, ValueWrap, K1, V1) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF2(K1, Key, V1, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec eval_insert_value(insertion_value_eval(), insertion_value_wrap(Value)) -> Value.
eval_insert_value(eager, Value) -> Value;
eval_insert_value(lazy, Fun) -> Fun().

-compile({inline, internal_split/16}).
-spec internal_split(
    K,
    K,
    K,
    K,
    K,
    V,
    V,
    V,
    V,
    V,
    C,
    C,
    C,
    C,
    C,
    C
) -> internal_split_result(K, V) when C :: deep_node(K, V).
internal_split(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6) ->
    SplitK = K3,
    SplitV = V3,
    SplitL = ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, C3),
    SplitR = ?INTERNAL2(K4, K5, [V4 | V5], C4, C5, C6),
    {split, SplitK, SplitV, SplitL, SplitR}.

-compile({inline, leaf_split/10}).
-spec leaf_split(
    K,
    K,
    K,
    K,
    K,
    V,
    V,
    V,
    V,
    V
) -> leaf_split_result(K, V).
leaf_split(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5) ->
    SplitK = K3,
    SplitV = V3,
    SplitL = ?LEAF2(K1, K2, V1, V2),
    SplitR = ?LEAF2(K4, K5, V4, V5),
    {split, SplitK, SplitV, SplitL, SplitR}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Update
%% ------------------------------------------------------------------

-spec update_recur(
    Key,
    update_value_wrap(Value, UpdatedValue),
    update_value_eval(),
    deep_node(Key, Value)
) -> deep_node(Key, Value | UpdatedValue).
update_recur(Key, ValueEval, ValueWrap, ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)) ->
    update_internal4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
update_recur(Key, ValueEval, ValueWrap, ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4)) ->
    update_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4);
update_recur(Key, ValueEval, ValueWrap, ?INTERNAL2(K1, K2, Values, C1, C2, C3)) ->
    update_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3);
update_recur(Key, ValueEval, ValueWrap, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    update_leaf4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4);
update_recur(Key, ValueEval, ValueWrap, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    update_leaf3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
update_recur(Key, ValueEval, ValueWrap, ?LEAF2(K1, K2, V1, V2)) ->
    update_leaf2(Key, ValueEval, ValueWrap, K1, K2, V1, V2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal4/13]}).
update_internal4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    if
        Key > K2 ->
            if
                Key < K4 ->
                    if
                        Key > K3 ->
                            update_internal4_child4(
                                Key,
                                ValueEval,
                                ValueWrap,
                                K1,
                                K2,
                                K3,
                                K4,
                                Values,
                                C1,
                                C2,
                                C3,
                                C4,
                                C5
                            );
                        Key < K3 ->
                            update_internal4_child3(
                                Key,
                                ValueEval,
                                ValueWrap,
                                K1,
                                K2,
                                K3,
                                K4,
                                Values,
                                C1,
                                C2,
                                C3,
                                C4,
                                C5
                            );
                        true ->
                            update_internal4_key3(
                                Key, ValueEval, ValueWrap, K1, K2, K4, Values, C1, C2, C3, C4, C5
                            )
                    end;
                Key > K4 ->
                    update_internal4_child5(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
                    );
                true ->
                    update_internal4_key4(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4, C5
                    )
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    update_internal4_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
                    );
                Key < K1 ->
                    update_internal4_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
                    );
                true ->
                    update_internal4_key1(
                        Key, ValueEval, ValueWrap, K2, K3, K4, Values, C1, C2, C3, C4, C5
                    )
            end;
        true ->
            update_internal4_key2(Key, ValueEval, ValueWrap, K1, K3, K4, Values, C1, C2, C3, C4, C5)
    end.

-compile({inline, [update_internal4_child1/13]}).
update_internal4_child1(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        update_recur(Key, ValueEval, ValueWrap, C1),
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_child2/13]}).
update_internal4_child2(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        C1,
        update_recur(Key, ValueEval, ValueWrap, C2),
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_child3/13]}).
update_internal4_child3(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        C1,
        C2,
        update_recur(Key, ValueEval, ValueWrap, C3),
        C4,
        C5
    ).

-compile({inline, [update_internal4_child4/13]}).
update_internal4_child4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        C1,
        C2,
        C3,
        update_recur(Key, ValueEval, ValueWrap, C4),
        C5
    ).

-compile({inline, [update_internal4_child5/13]}).
update_internal4_child5(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        C1,
        C2,
        C3,
        C4,
        update_recur(Key, ValueEval, ValueWrap, C5)
    ).

%%%

-compile({inline, [update_internal4_key1/12]}).
update_internal4_key1(Key, ValueEval, ValueWrap, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL4(
        Key,
        K2,
        K3,
        K4,
        {Value, V2, V3, V4},
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_key2/12]}).
update_internal4_key2(Key, ValueEval, ValueWrap, K1, K3, K4, Values, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V2),
    ?INTERNAL4(
        K1,
        Key,
        K3,
        K4,
        {V1, Value, V3, V4},
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_key3/12]}).
update_internal4_key3(Key, ValueEval, ValueWrap, K1, K2, K4, Values, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V3),
    ?INTERNAL4(
        K1,
        K2,
        Key,
        K4,
        {V1, V2, Value, V4},
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_key4/12]}).
update_internal4_key4(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V4),
    ?INTERNAL4(
        K1,
        K2,
        K3,
        Key,
        {V1, V2, V3, Value},
        C1,
        C2,
        C3,
        C4,
        C5
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal3/11]}).
update_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    if
        Key > K2 ->
            if
                Key < K3 ->
                    update_internal3_child3(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4
                    );
                Key > K3 ->
                    update_internal3_child4(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4
                    );
                true ->
                    update_internal3_key3(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3, C4)
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    update_internal3_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4
                    );
                Key < K1 ->
                    update_internal3_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4
                    );
                true ->
                    update_internal3_key1(Key, ValueEval, ValueWrap, K2, K3, Values, C1, C2, C3, C4)
            end;
        true ->
            update_internal3_key2(Key, ValueEval, ValueWrap, K1, K3, Values, C1, C2, C3, C4)
    end.

-compile({inline, [update_internal3_child1/11]}).
update_internal3_child1(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        update_recur(Key, ValueEval, ValueWrap, C1),
        C2,
        C3,
        C4
    ).

-compile({inline, [update_internal3_child2/11]}).
update_internal3_child2(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        C1,
        update_recur(Key, ValueEval, ValueWrap, C2),
        C3,
        C4
    ).

-compile({inline, [update_internal3_child3/11]}).
update_internal3_child3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        C1,
        C2,
        update_recur(Key, ValueEval, ValueWrap, C3),
        C4
    ).

-compile({inline, [update_internal3_child4/11]}).
update_internal3_child4(Key, ValueEval, ValueWrap, K1, K2, K3, Values, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        C1,
        C2,
        C3,
        update_recur(Key, ValueEval, ValueWrap, C4)
    ).

%%%

-compile({inline, [update_internal3_key1/10]}).
update_internal3_key1(Key, ValueEval, ValueWrap, K2, K3, Values, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL3(
        Key,
        K2,
        K3,
        {Value, V2, V3},
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, [update_internal3_key2/10]}).
update_internal3_key2(Key, ValueEval, ValueWrap, K1, K3, Values, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V2),
    ?INTERNAL3(
        K1,
        Key,
        K3,
        {V1, Value, V3},
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, [update_internal3_key3/10]}).
update_internal3_key3(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V3),
    ?INTERNAL3(
        K1,
        K2,
        Key,
        {V1, V2, Value},
        C1,
        C2,
        C3,
        C4
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal2/9]}).
update_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    update_internal2_child2(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3);
                Key > K2 ->
                    update_internal2_child3(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3);
                true ->
                    update_internal2_key2(Key, ValueEval, ValueWrap, K1, Values, C1, C2, C3)
            end;
        Key < K1 ->
            update_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3);
        true ->
            update_internal2_key1(Key, ValueEval, ValueWrap, K2, Values, C1, C2, C3)
    end.

-compile({inline, [update_internal2_child1/9]}).
update_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3) ->
    ?INTERNAL2(K1, K2, Values, update_recur(Key, ValueEval, ValueWrap, C1), C2, C3).

-compile({inline, [update_internal2_child2/9]}).
update_internal2_child2(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3) ->
    ?INTERNAL2(K1, K2, Values, C1, update_recur(Key, ValueEval, ValueWrap, C2), C3).

-compile({inline, [update_internal2_child3/9]}).
update_internal2_child3(Key, ValueEval, ValueWrap, K1, K2, Values, C1, C2, C3) ->
    ?INTERNAL2(K1, K2, Values, C1, C2, update_recur(Key, ValueEval, ValueWrap, C3)).

%%%

-compile({inline, [update_internal2_key1/8]}).
update_internal2_key1(Key, ValueEval, ValueWrap, K2, Values, C1, C2, C3) ->
    [V1 | V2] = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL2(Key, K2, [Value | V2], C1, C2, C3).

-compile({inline, [update_internal2_key1/8]}).
update_internal2_key2(Key, ValueEval, ValueWrap, K1, Values, C1, C2, C3) ->
    [V1 | V2] = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V2),
    ?INTERNAL2(K1, Key, [V1 | Value], C1, C2, C3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal1/7]}).
update_internal1(Key, ValueEval, ValueWrap, K1, V1, C1, C2) ->
    if
        Key < K1 ->
            update_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, C1, C2);
        Key > K1 ->
            update_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, C1, C2);
        true ->
            update_internal1_key1(Key, ValueEval, ValueWrap, V1, C1, C2)
    end.

-compile({inline, [update_internal1_child1/7]}).
update_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, C1, C2) ->
    ?INTERNAL1(K1, V1, update_recur(Key, ValueEval, ValueWrap, C1), C2).

-compile({inline, [update_internal1_child2/7]}).
update_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, C1, C2) ->
    ?INTERNAL1(K1, V1, C1, update_recur(Key, ValueEval, ValueWrap, C2)).

-compile({inline, [update_internal1_key1/6]}).
update_internal1_key1(Key, ValueEval, ValueWrap, V1, C1, C2) ->
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL1(Key, Value, C1, C2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_leaf4/11]}).
update_leaf4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4) ->
    if
        Key > K2 ->
            if
                Key == K3 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V3),
                    ?LEAF4(K1, K2, Key, K4, V1, V2, Value, V4);
                Key == K4 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V4),
                    ?LEAF4(K1, K2, K3, Key, V1, V2, V3, Value);
                true ->
                    error_badkey(Key)
            end;
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?LEAF4(K1, Key, K3, K4, V1, Value, V3, V4);
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF4(Key, K2, K3, K4, Value, V2, V3, V4);
        true ->
            error_badkey(Key)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_leaf3/9]}).
update_leaf3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3) ->
    if
        Key < K2 ->
            if
                Key == K1 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF3(Key, K2, K3, Value, V2, V3);
                true ->
                    error_badkey(Key)
            end;
        Key > K2 ->
            if
                Key == K3 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V3),
                    ?LEAF3(K1, K2, Key, V1, V2, Value);
                true ->
                    error_badkey(Key)
            end;
        true ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?LEAF3(K1, Key, K3, V1, Value, V3)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_leaf2/7]}).
update_leaf2(Key, ValueEval, ValueWrap, K1, K2, V1, V2) ->
    if
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?LEAF2(K1, Key, V1, Value);
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF2(Key, K2, Value, V2);
        true ->
            error_badkey(Key)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_leaf1/5]}).
update_leaf1(Key, ValueEval, ValueWrap, K1, V1) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF1(Key, Value);
        true ->
            error_badkey(Key)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec eval_update_value(update_value_eval(), update_value_wrap(PrevValue, Value), PrevValue) ->
    Value.
eval_update_value(eager, Value, _) -> Value;
eval_update_value(lazy, Fun, PrevValue) -> Fun(PrevValue).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Deletion
%% ------------------------------------------------------------------

% root_delete(Key, Node) ->
%     try
%         delete_recur(Key, nil, nil, Node)
%     catch
%         ?INTERNAL1(K1, V1, C1, C2) ->
%             delete_internal1(Key, Node, K1, V1, C1, C2);
%         %
%         ?LEAF1(K1, _) ->
%             delete_leaf1(Key, K1);
%         %
%         ?LEAF0 ->
%             error_badkey(Key)
%     end.
% 
% delete_recur(Key, Parent, Pos, Node) ->
%     case Node of
%         ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%             delete_internal4(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%         %
%         ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4) ->
%             delete_internal3(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%         %
%         ?INTERNAL2(K1, K2, Values, C1, C2, C3) ->
%             delete_internal2(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%         %
%         ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
%             delete_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4);
%         %
%         ?LEAF3(K1, K2, K3, V1, V2, V3) ->
%             delete_leaf3(Key, K1, K2, K3, V1, V2, V3);
%         %
%         ?LEAF2(K1, K2, V1, V2) ->
%             delete_leaf2(Key, Parent, Pos, K1, K2, V1, V2);
%         %
%         _ ->
%             throw(Node)
%     end.
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% 
% -compile({inline, delete_internal4/12}).
% delete_internal4(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     if
%         Key < K3 ->
%             if
%                 Key > K1 ->
%                     if
%                         Key < K2 ->
%                             delete_internal4_child2(
%                                 Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
%                             );
%                         Key > K2 ->
%                             delete_internal4_child3(
%                                 Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
%                             );
%                         true ->
%                             delete_internal4_key2(Node, K1, K3, K4, Values, C1, C2, C3, C4, C5)
%                     end;
%                 Key < K1 ->
%                     delete_internal4_child1(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%                 true ->
%                     delete_internal4_key1(Node, K2, K3, K4, Values, C1, C2, C3, C4, C5)
%             end;
%         Key > K3 ->
%             if
%                 Key < K4 ->
%                     delete_internal4_child4(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%                 Key > K4 ->
%                     delete_internal4_child5(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%                 true ->
%                     delete_internal4_key4(Node, K1, K2, K3, Values, C1, C2, C3, C4, C5)
%             end;
%         true ->
%             delete_internal4_key3(Node, K1, K2, K4, Values, C1, C2, C3, C4, C5)
%     end.
% 
% %%%
% 
% -compile({inline, delete_internal4_child1/12}).
% delete_internal4_child1(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     UpdatedC1 = delete_recur(Key, Node, 1, C1),
%     maybe_rebalance_internal4_child1(K1, K2, K3, K4, Values, UpdatedC1, C2, C3, C4, C5).
% 
% -compile({inline, delete_internal4_child2/12}).
% delete_internal4_child2(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     UpdatedC2 = delete_recur(Key, Node, 2, C2),
%     maybe_rebalance_internal4_child2(K1, K2, K3, K4, Values, C1, UpdatedC2, C3, C4, C5).
% 
% -compile({inline, delete_internal4_child3/12}).
% delete_internal4_child3(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     UpdatedC3 = delete_recur(Key, Node, 3, C3),
%     maybe_rebalance_internal4_child3(K1, K2, K3, K4, Values, C1, C2, UpdatedC3, C4, C5).
% 
% -compile({inline, delete_internal4_child4/12}).
% delete_internal4_child4(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     UpdatedC4 = delete_recur(Key, Node, 4, C4),
%     maybe_rebalance_internal4_child4(K1, K2, K3, K4, Values, C1, C2, C3, UpdatedC4, C5).
% 
% -compile({inline, delete_internal4_child5/12}).
% delete_internal4_child5(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     UpdatedC5 = delete_recur(Key, Node, 5, C5),
%     maybe_rebalance_internal4_child5(K1, K2, K3, K4, Values, C1, C2, C3, C4, UpdatedC5).
% 
% %%%
% 
% delete_internal4_key1(Node, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(Node, 2, C2),
%     {_, V2, V3, V4} = Values,
% 
%     maybe_rebalance_internal4_child2(
%         ReplacementK,
%         K2,
%         K3,
%         K4,
%         {ReplacementV, V2, V3, V4},
%         C1,
%         UpdatedC2,
%         C3,
%         C4,
%         C5
%     ).
% 
% delete_internal4_key2(Node, K1, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(Node, 3, C3),
%     {V1, _, V3, V4} = Values,
% 
%     maybe_rebalance_internal4_child3(
%         K1,
%         ReplacementK,
%         K3,
%         K4,
%         {V1, ReplacementV, V3, V4},
%         C1,
%         C2,
%         UpdatedC3,
%         C4,
%         C5
%     ).
% 
% delete_internal4_key3(Node, K1, K2, K4, Values, C1, C2, C3, C4, C5) ->
%     [[ReplacementK | ReplacementV] | UpdatedC4] = take_smallest_recur(Node, 4, C4),
%     {V1, V2, _, V4} = Values,
% 
%     maybe_rebalance_internal4_child4(
%         K1,
%         K2,
%         ReplacementK,
%         K4,
%         {V1, V2, ReplacementV, V4},
%         C1,
%         C2,
%         C3,
%         UpdatedC4,
%         C5
%     ).
% 
% delete_internal4_key4(Node, K1, K2, K3, Values, C1, C2, C3, C4, C5) ->
%     [[ReplacementK | ReplacementV] | UpdatedC5] = take_smallest_recur(Node, 5, C5),
%     {V1, V2, V3, _} = Values,
% 
%     maybe_rebalance_internal4_child5(
%         K1,
%         K2,
%         K3,
%         ReplacementK,
%         {V1, V2, V3, ReplacementV},
%         C1,
%         C2,
%         C3,
%         C4,
%         UpdatedC5
%     ).
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% 
% delete_internal3(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     if
%         Key < K2 ->
%             if
%                 Key < K1 ->
%                     delete_internal3_child1(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%                 Key > K1 ->
%                     delete_internal3_child2(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%                 true ->
%                     delete_internal3_key1(Node, K2, K3, Values, C1, C2, C3, C4)
%             end;
%         Key > K2 ->
%             if
%                 Key < K3 ->
%                     delete_internal3_child3(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%                 Key > K3 ->
%                     delete_internal3_child4(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%                 true ->
%                     delete_internal3_key3(Node, K1, K2, Values, C1, C2, C3, C4)
%             end;
%         true ->
%             delete_internal3_key2(Node, K1, K3, Values, C1, C2, C3, C4)
%     end.
% 
% %%
% 
% delete_internal3_child1(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     UpdatedC1 = delete_recur(Key, Node, 1, C1),
%     maybe_rebalance_internal3_child1(K1, K2, K3, Values, UpdatedC1, C2, C3, C4).
% 
% delete_internal3_child2(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     UpdatedC2 = delete_recur(Key, Node, 2, C2),
%     maybe_rebalance_internal3_child2(K1, K2, K3, Values, C1, UpdatedC2, C3, C4).
% 
% delete_internal3_child3(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     UpdatedC3 = delete_recur(Key, Node, 3, C3),
%     maybe_rebalance_internal3_child3(K1, K2, K3, Values, C1, C2, UpdatedC3, C4).
% 
% delete_internal3_child4(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     UpdatedC4 = delete_recur(Key, Node, 4, C4),
%     maybe_rebalance_internal3_child4(K1, K2, K3, Values, C1, C2, C3, UpdatedC4).
% 
% %%
% 
% delete_internal3_key1(Node, K2, K3, Values, C1, C2, C3, C4) ->
%     [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(Node, 2, C2),
%     {_, V2, V3} = Values,
% 
%     maybe_rebalance_internal3_child2(
%         ReplacementK,
%         K2,
%         K3,
%         {ReplacementV, V2, V3},
%         C1,
%         UpdatedC2,
%         C3,
%         C4
%     ).
% 
% delete_internal3_key2(Node, K1, K3, Values, C1, C2, C3, C4) ->
%     [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(Node, 3, C3),
%     {V1, _, V3} = Values,
% 
%     maybe_rebalance_internal3_child3(
%         K1,
%         ReplacementK,
%         K3,
%         {V1, ReplacementV, V3},
%         C1,
%         C2,
%         UpdatedC3,
%         C4
%     ).
% 
% delete_internal3_key3(Node, K1, K2, Values, C1, C2, C3, C4) ->
%     [[ReplacementK | ReplacementV] | UpdatedC4] = take_smallest_recur(Node, 4, C4),
%     {V1, V2, _} = Values,
% 
%     maybe_rebalance_internal3_child4(
%         K1,
%         K2,
%         ReplacementK,
%         {V1, V2, ReplacementV},
%         C1,
%         C2,
%         C3,
%         UpdatedC4
%     ).
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% 
% delete_internal2(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     if
%         Key < K2 ->
%             if
%                 Key > K1 ->
%                     delete_internal2_child2(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%                 Key < K1 ->
%                     delete_internal2_child1(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%                 true ->
%                     delete_internal2_key1(Parent, Pos, Node, K2, Values, C1, C2, C3)
%             end;
%         Key > K2 ->
%             delete_internal2_child3(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%         true ->
%             delete_internal2_key2(Parent, Pos, Node, K1, Values, C1, C2, C3)
%     end.
% 
% %%
% 
% delete_internal2_child1(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     UpdatedC1 = delete_recur(Key, Node, 1, C1),
%     maybe_rebalance_internal2_child1(Parent, Pos, K1, K2, Values, UpdatedC1, C2, C3).
% 
% delete_internal2_child2(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     UpdatedC2 = delete_recur(Key, Node, 2, C2),
%     maybe_rebalance_internal2_child2(Parent, Pos, K1, K2, Values, C1, UpdatedC2, C3).
% 
% delete_internal2_child3(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     UpdatedC3 = delete_recur(Key, Node, 3, C3),
%     maybe_rebalance_internal2_child3(Parent, Pos, K1, K2, Values, C1, C2, UpdatedC3).
% 
% %%
% 
% delete_internal2_key1(Parent, Pos, Node, K2, Values, C1, C2, C3) ->
%     [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(Node, 2, C2),
%     [_ | V2] = Values,
% 
%     maybe_rebalance_internal2_child2(
%         Parent,
%         Pos,
%         ReplacementK,
%         K2,
%         [ReplacementV | V2],
%         C1,
%         UpdatedC2,
%         C3
%     ).
% 
% delete_internal2_key2(Parent, Pos, Node, K1, Values, C1, C2, C3) ->
%     [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(Node, 3, C3),
%     [V1 | _] = Values,
% 
%     maybe_rebalance_internal2_child3(
%         Parent,
%         Pos,
%         K1,
%         ReplacementK,
%         [V1 | ReplacementV],
%         C1,
%         C2,
%         UpdatedC3
%     ).
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% 
% delete_internal1(Key, Node, K1, V1, C1, C2) ->
%     if
%         Key < K1 ->
%             delete_internal1_child1(Key, Node, K1, V1, C1, C2);
%         Key > K1 ->
%             delete_internal1_child2(Key, Node, K1, V1, C1, C2);
%         true ->
%             delete_internal1_key1(Node, C1, C2)
%     end.
% 
% %%%
% 
% delete_internal1_child1(Key, Node, K1, V1, C1, C2) ->
%     UpdatedC1 = delete_recur(Key, Node, 1, C1),
%     maybe_rebalance_internal1_child1(K1, V1, UpdatedC1, C2).
% 
% delete_internal1_child2(Key, Node, K1, V1, C1, C2) ->
%     UpdatedC2 = delete_recur(Key, Node, 2, C2),
%     maybe_rebalance_internal1_child2(K1, V1, C1, UpdatedC2).
% 
% delete_internal1_key1(Node, C1, C2) ->
%     % FIXME PROBLEM WITH THIS
%     % * Replacement key goes into the rebalanced child node
%     [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(Node, 2, C2),
%     maybe_rebalance_internal1_child2(
%         ReplacementK,
%         ReplacementV,
%         C1,
%         UpdatedC2
%     ).
% 
% %%%%%%%%%%%%%%%%%%%%%
% 
% delete_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4) ->
%     if
%         Key < K3 ->
%             if
%                 Key == K1 ->
%                     ?LEAF3(K2, K3, K4, V2, V3, V4);
%                 Key == K2 ->
%                     ?LEAF3(K1, K3, K4, V1, V3, V4);
%                 true ->
%                     error_badkey(Key)
%             end;
%         Key == K3 ->
%             ?LEAF3(K1, K2, K4, V1, V2, V4);
%         Key == K4 ->
%             ?LEAF3(K1, K2, K3, V1, V2, V3);
%         true ->
%             error_badkey(Key)
%     end.
% 
% delete_leaf3(Key, K1, K2, K3, V1, V2, V3) ->
%     if
%         Key < K2 ->
%             if
%                 Key == K1 ->
%                     ?LEAF2(K2, K3, V2, V3);
%                 true ->
%                     error_badkey(Key)
%             end;
%         Key > K2 ->
%             if
%                 Key == K3 ->
%                     ?LEAF2(K1, K2, V1, V2);
%                 true ->
%                     error_badkey(Key)
%             end;
%         true ->
%             ?LEAF2(K1, K3, V1, V3)
%     end.
% 
% delete_leaf2(Key, Parent, Pos, K1, K2, V1, V2) ->
%     if
%         Key == K1 ->
%             rebalance_leaf2(Parent, Pos, K2, V2);
%         Key == K2 ->
%             rebalance_leaf2(Parent, Pos, K1, V1);
%         true ->
%             error_badkey(Key)
%     end.
% 
% delete_leaf1(Key, K1) ->
%     if
%         Key == K1 ->
%             ?LEAF0;
%         true ->
%             error_badkey(Key)
%     end.
% 
% %% ------------------------------------------------------------------
% %% Internal Function Definitions: Node Taking - Smallest
% %% ------------------------------------------------------------------
% 
% -compile({inline, root_take_smallest/1}).
% root_take_smallest(Node) ->
%     try
%         take_smallest_recur(nil, nil, Node)
%     catch
%         ?INTERNAL1(K1, V1, C1, C2) ->
%             take_smallest_internal1(Node, K1, V1, C1, C2);
%         %
%         ?LEAF1(K1, V1) ->
%             [[K1 | V1] | ?LEAF0];
%         %
%         ?LEAF0 ->
%             error_empty_tree()
%     end.
% 
% take_smallest_recur(Parent, Pos, Node) ->
%     case Node of
%         ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%             take_smallest_internal4(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%         %
%         ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4) ->
%             take_smallest_internal3(Node, K1, K2, K3, Values, C1, C2, C3, C4);
%         %
%         ?INTERNAL2(K1, K2, Values, C1, C2, C3) ->
%             take_smallest_internal2(Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%         %
%         ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
%             [[K1 | V1] | ?LEAF3(K2, K3, K4, V2, V3, V4)];
%         %
%         ?LEAF3(K1, K2, K3, V1, V2, V3) ->
%             [[K1 | V1] | ?LEAF2(K2, K3, V2, V3)];
%         %
%         ?LEAF2(K1, K2, V1, V2) ->
%             take_smallest_leaf2(Parent, Pos, K1, K2, V1, V2);
%         %
%         _ ->
%             throw(Node)
%     end.
% 
% take_smallest_internal4(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [TakenPair | UpdatedC1] = take_smallest_recur(_Parent = Node, 1, C1),
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child1(K1, K2, K3, K4, Values, UpdatedC1, C2, C3, C4, C5)
%     ].
% 
% take_smallest_internal3(Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     [TakenPair | UpdatedC1] = take_smallest_recur(Node, 1, C1),
%     [TakenPair | maybe_rebalance_internal3_child1(K1, K2, K3, Values, UpdatedC1, C2, C3, C4)].
% 
% take_smallest_internal2(Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     [TakenPair | UpdatedC1] = take_smallest_recur(_Parent = Node, 1, C1),
%     [TakenPair | maybe_rebalance_internal2_child1(Parent, Pos, K1, K2, Values, UpdatedC1, C2, C3)].
% 
% take_smallest_internal1(Node, K1, V1, C1, C2) ->
%     [TakenPair | UpdatedC1] = take_smallest_recur(_Parent = Node, 1, C1),
%     [TakenPair | maybe_rebalance_internal1_child1(K1, V1, UpdatedC1, C2)].
% 
% take_smallest_leaf2(Parent, Pos, K1, K2, V1, V2) ->
%     TakenPair = [K1 | V1],
% 
%     RemainingK = K2,
%     RemainingV = V2,
% 
%     [TakenPair | rebalance_leaf2(Parent, Pos, RemainingK, RemainingV)].
% 
% %% ------------------------------------------------------------------
% %% Internal Function Definitions: Node Taking - Largest
% %% ------------------------------------------------------------------
% 
% -compile({inline, root_take_largest/1}).
% root_take_largest(Node) ->
%     try
%         take_largest_recur(nil, nil, Node)
%     catch
%         ?INTERNAL1(K1, V1, C1, C2) ->
%             take_largest_internal1(Node, K1, V1, C1, C2);
%         %
%         ?LEAF1(K1, V1) ->
%             [[K1 | V1] | ?LEAF0];
%         %
%         ?LEAF0 ->
%             error_empty_tree()
%     end.
% 
% take_largest_recur(Parent, Pos, Node) ->
%     case Node of
%         ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%             take_largest_internal4(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%         %
%         ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4) ->
%             take_largest_internal3(Node, K1, K2, K3, Values, C1, C2, C3, C4);
%         %
%         ?INTERNAL2(K1, K2, Values, C1, C2, C3) ->
%             take_largest_internal2(Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%         %
%         ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
%             [[K4 | V4] | ?LEAF3(K1, K2, K3, V1, V2, V3)];
%         %
%         ?LEAF3(K1, K2, K3, V1, V2, V3) ->
%             [[K3 | V3] | ?LEAF2(K1, K2, V1, V2)];
%         %
%         ?LEAF2(K1, K2, V1, V2) ->
%             take_largest_leaf2(Parent, Pos, K1, K2, V1, V2);
%         %
%         _ ->
%             throw(Node)
%     end.
% 
% take_largest_internal4(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [TakenPair | UpdatedC5] = take_largest_recur(_Parent = Node, 5, C5),
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child5(K1, K2, K3, K4, Values, C1, C2, C3, C4, UpdatedC5)
%     ].
% 
% take_largest_internal3(Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     [TakenPair | UpdatedC4] = take_largest_recur(Node, 4, C4),
%     [TakenPair | maybe_rebalance_internal3_child4(K1, K2, K3, Values, C1, C2, C3, UpdatedC4)].
% 
% take_largest_internal2(Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     [TakenPair | UpdatedC3] = take_largest_recur(_Parent = Node, 3, C3),
%     [TakenPair | maybe_rebalance_internal2_child3(Parent, Pos, K1, K2, Values, C1, C2, UpdatedC3)].
% 
% take_largest_internal1(Node, K1, V1, C1, C2) ->
%     [TakenPair | UpdatedC2] = take_largest_recur(_Parent = Node, 2, C2),
%     [TakenPair | maybe_rebalance_internal1_child2(K1, V1, C1, UpdatedC2)].
% 
% take_largest_leaf2(Parent, Pos, K1, K2, V1, V2) ->
%     TakenPair = [K2 | V2],
% 
%     RemainingK = K1,
%     RemainingV = V1,
% 
%     [TakenPair | rebalance_leaf2(Parent, Pos, RemainingK, RemainingV)].
% 
% %% ------------------------------------------------------------------
% %% Internal Function Definitions: Node Taking - Key
% %% ------------------------------------------------------------------
% 
% root_take(Key, Node) ->
%     try
%         take_recur(Key, nil, nil, Node)
%     catch
%         ?INTERNAL1(K1, V1, C1, C2) ->
%             take_internal1(Key, Node, K1, V1, C1, C2);
%         %
%         ?LEAF1(K1, V1) ->
%             take_leaf1(Key, K1, V1);
%         %
%         ?LEAF0 ->
%             error_badkey(Key)
%     end.
% 
% take_recur(Key, Parent, Pos, Node) ->
%     case Node of
%         ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%             take_internal4(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%         %
%         ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4) ->
%             take_internal3(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%         %
%         ?INTERNAL2(K1, K2, Values, C1, C2, C3) ->
%             take_internal2(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%         %
%         ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
%             take_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4);
%         %
%         ?LEAF3(K1, K2, K3, V1, V2, V3) ->
%             take_leaf3(Key, K1, K2, K3, V1, V2, V3);
%         %
%         ?LEAF2(K1, K2, V1, V2) ->
%             take_leaf2(Key, Parent, Pos, K1, K2, V1, V2);
%         %
%         _ ->
%             throw(Node)
%     end.
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% 
% take_internal4(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     if
%         Key < K3 ->
%             if
%                 Key > K1 ->
%                     if
%                         Key < K2 ->
%                             take_internal4_child2(
%                                 Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
%                             );
%                         Key > K2 ->
%                             take_internal4_child3(
%                                 Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5
%                             );
%                         true ->
%                             take_internal4_key2(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)
%                     end;
%                 Key < K1 ->
%                     take_internal4_child1(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%                 true ->
%                     take_internal4_key1(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)
%             end;
%         Key > K3 ->
%             if
%                 Key < K4 ->
%                     take_internal4_child4(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%                 Key > K4 ->
%                     take_internal4_child5(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
%                 true ->
%                     take_internal4_key4(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)
%             end;
%         true ->
%             take_internal4_key3(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)
%     end.
% 
% %%%
% 
% take_internal4_child1(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [TakenPair | UpdatedC1] = take_recur(Key, Node, 1, C1),
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child1(K1, K2, K3, K4, Values, UpdatedC1, C2, C3, C4, C5)
%     ].
% 
% take_internal4_child2(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [TakenPair | UpdatedC2] = take_recur(Key, Node, 2, C2),
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child2(K1, K2, K3, K4, Values, C1, UpdatedC2, C3, C4, C5)
%     ].
% 
% take_internal4_child3(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [TakenPair | UpdatedC3] = take_recur(Key, Node, 3, C3),
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child3(K1, K2, K3, K4, Values, C1, C2, UpdatedC3, C4, C5)
%     ].
% 
% take_internal4_child4(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [TakenPair | UpdatedC4] = take_recur(Key, Node, 4, C4),
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child4(K1, K2, K3, K4, Values, C1, C2, C3, UpdatedC4, C5)
%     ].
% 
% take_internal4_child5(Key, Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [TakenPair | UpdatedC5] = take_recur(Key, Node, 5, C5),
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child5(K1, K2, K3, K4, Values, C1, C2, C3, C4, UpdatedC5)
%     ].
% 
% %%%
% 
% take_internal4_key1(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(Node, 2, C2),
%     {V1, V2, V3, V4} = Values,
% 
%     TakenPair = [K1 | V1],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child2(
%             ReplacementK,
%             K2,
%             K3,
%             K4,
%             {ReplacementV, V2, V3, V4},
%             C1,
%             UpdatedC2,
%             C3,
%             C4,
%             C5
%         )
%     ].
% 
% take_internal4_key2(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(Node, 3, C3),
%     {V1, V2, V3, V4} = Values,
% 
%     TakenPair = [K2 | V2],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child3(
%             K1,
%             ReplacementK,
%             K3,
%             K4,
%             {V1, ReplacementV, V3, V4},
%             C1,
%             C2,
%             UpdatedC3,
%             C4,
%             C5
%         )
%     ].
% 
% take_internal4_key3(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [[ReplacementK | ReplacementV] | UpdatedC4] = take_smallest_recur(Node, 4, C4),
%     {V1, V2, V3, V4} = Values,
% 
%     TakenPair = [K3 | V3],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child4(
%             K1,
%             K2,
%             ReplacementK,
%             K4,
%             {V1, V2, ReplacementV, V4},
%             C1,
%             C2,
%             C3,
%             UpdatedC4,
%             C5
%         )
%     ].
% 
% take_internal4_key4(Node, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     [[ReplacementK | ReplacementV] | UpdatedC5] = take_smallest_recur(Node, 5, C5),
%     {V1, V2, V3, V4} = Values,
% 
%     TakenPair = [K4 | V4],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal4_child5(
%             K1,
%             K2,
%             K3,
%             ReplacementK,
%             {V1, V2, V3, ReplacementV},
%             C1,
%             C2,
%             C3,
%             C4,
%             UpdatedC5
%         )
%     ].
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% 
% take_internal3(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     if
%         Key < K2 ->
%             if
%                 Key < K1 ->
%                     take_internal3_child1(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%                 Key > K1 ->
%                     take_internal3_child2(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%                 true ->
%                     take_internal3_key1(Node, K1, K2, K3, Values, C1, C2, C3, C4)
%             end;
%         Key > K2 ->
%             if
%                 Key < K3 ->
%                     take_internal3_child3(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%                 Key > K3 ->
%                     take_internal3_child4(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4);
%                 true ->
%                     take_internal3_key3(Node, K1, K2, K3, Values, C1, C2, C3, C4)
%             end;
%         true ->
%             take_internal3_key2(Node, K1, K2, K3, Values, C1, C2, C3, C4)
%     end.
% 
% %%
% 
% take_internal3_child1(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     [TakenPair | UpdatedC1] = take_recur(Key, Node, 1, C1),
%     [TakenPair | maybe_rebalance_internal3_child1(K1, K2, K3, Values, UpdatedC1, C2, C3, C4)].
% 
% take_internal3_child2(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     [TakenPair | UpdatedC2] = take_recur(Key, Node, 2, C2),
%     [TakenPair | maybe_rebalance_internal3_child2(K1, K2, K3, Values, C1, UpdatedC2, C3, C4)].
% 
% take_internal3_child3(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     [TakenPair | UpdatedC3] = take_recur(Key, Node, 3, C3),
%     [TakenPair | maybe_rebalance_internal3_child3(K1, K2, K3, Values, C1, C2, UpdatedC3, C4)].
% 
% take_internal3_child4(Key, Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     [TakenPair | UpdatedC4] = take_recur(Key, Node, 4, C4),
%     [TakenPair | maybe_rebalance_internal3_child4(K1, K2, K3, Values, C1, C2, C3, UpdatedC4)].
% 
% %%
% 
% take_internal3_key1(Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(Node, 2, C2),
%     {V1, V2, V3} = Values,
% 
%     TakenPair = [K1 | V1],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal3_child2(
%             ReplacementK,
%             K2,
%             K3,
%             {ReplacementV, V2, V3},
%             C1,
%             UpdatedC2,
%             C3,
%             C4
%         )
%     ].
% 
% take_internal3_key2(Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(Node, 3, C3),
%     {V1, V2, V3} = Values,
% 
%     TakenPair = [K2 | V2],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal3_child3(
%             K1,
%             ReplacementK,
%             K3,
%             {V1, ReplacementV, V3},
%             C1,
%             C2,
%             UpdatedC3,
%             C4
%         )
%     ].
% 
% take_internal3_key3(Node, K1, K2, K3, Values, C1, C2, C3, C4) ->
%     [[ReplacementK | ReplacementV] | UpdatedC4] = take_smallest_recur(Node, 4, C4),
%     {V1, V2, V3} = Values,
% 
%     TakenPair = [K3 | V3],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal3_child4(
%             K1,
%             K2,
%             ReplacementK,
%             {V1, V2, ReplacementV},
%             C1,
%             C2,
%             C3,
%             UpdatedC4
%         )
%     ].
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% 
% take_internal2(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     if
%         Key < K2 ->
%             if
%                 Key > K1 ->
%                     take_internal2_child2(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%                 Key < K1 ->
%                     take_internal2_child1(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%                 true ->
%                     take_internal2_key1(Parent, Pos, Node, K1, K2, Values, C1, C2, C3)
%             end;
%         Key > K2 ->
%             take_internal2_child3(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3);
%         true ->
%             take_internal2_key2(Parent, Pos, Node, K1, K2, Values, C1, C2, C3)
%     end.
% 
% %%
% 
% take_internal2_child1(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     [TakenPair | UpdatedC1] = take_recur(Key, Node, 1, C1),
%     [TakenPair | maybe_rebalance_internal2_child1(Parent, Pos, K1, K2, Values, UpdatedC1, C2, C3)].
% 
% take_internal2_child2(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     [TakenPair | UpdatedC2] = take_recur(Key, Node, 2, C2),
%     [TakenPair | maybe_rebalance_internal2_child2(Parent, Pos, K1, K2, Values, C1, UpdatedC2, C3)].
% 
% take_internal2_child3(Key, Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     [TakenPair | UpdatedC3] = take_recur(Key, Node, 3, C3),
%     [TakenPair | maybe_rebalance_internal2_child3(Parent, Pos, K1, K2, Values, C1, C2, UpdatedC3)].
% 
% %%
% 
% take_internal2_key1(Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(Node, 2, C2),
%     [V1 | V2] = Values,
% 
%     TakenPair = [K1 | V1],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal2_child2(
%             Parent,
%             Pos,
%             ReplacementK,
%             K2,
%             [ReplacementV | V2],
%             C1,
%             UpdatedC2,
%             C3
%         )
%     ].
% 
% take_internal2_key2(Parent, Pos, Node, K1, K2, Values, C1, C2, C3) ->
%     [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(Node, 3, C3),
%     [V1 | V2] = Values,
% 
%     TakenPair = [K2 | V2],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal2_child3(
%             Parent,
%             Pos,
%             K1,
%             ReplacementK,
%             [V1 | ReplacementV],
%             C1,
%             C2,
%             UpdatedC3
%         )
%     ].
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% 
% take_internal1(Key, Node, K1, V1, C1, C2) ->
%     if
%         Key < K1 ->
%             take_internal1_child1(Key, Node, K1, V1, C1, C2);
%         Key > K1 ->
%             take_internal1_child2(Key, Node, K1, V1, C1, C2);
%         true ->
%             take_internal1_key1(Node, K1, V1, C1, C2)
%     end.
% 
% %%%
% 
% take_internal1_child1(Key, Node, K1, V1, C1, C2) ->
%     [TakenPair | UpdatedC1] = take_recur(Key, Node, 1, C1),
%     [TakenPair | maybe_rebalance_internal1_child1(K1, V1, UpdatedC1, C2)].
% 
% take_internal1_child2(Key, Node, K1, V1, C1, C2) ->
%     [TakenPair | UpdatedC2] = take_recur(Key, Node, 2, C2),
%     [TakenPair | maybe_rebalance_internal1_child2(K1, V1, C1, UpdatedC2)].
% 
% take_internal1_key1(Node, K1, V1, C1, C2) ->
%     [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(Node, 2, C2),
% 
%     TakenPair = [K1 | V1],
% 
%     [
%         TakenPair
%         | maybe_rebalance_internal1_child2(
%             ReplacementK,
%             ReplacementV,
%             C1,
%             UpdatedC2
%         )
%     ].
% 
% %%%%%%%%%%%%%%%%%%%%%
% 
% take_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4) ->
%     if
%         Key < K3 ->
%             if
%                 Key == K1 ->
%                     [[K1 | V1] | ?LEAF3(K2, K3, K4, V2, V3, V4)];
%                 Key == K2 ->
%                     [[K2 | V2] | ?LEAF3(K1, K3, K4, V1, V3, V4)];
%                 true ->
%                     error_badkey(Key)
%             end;
%         Key == K3 ->
%             [[K3 | V3] | ?LEAF3(K1, K2, K4, V1, V2, V4)];
%         Key == K4 ->
%             [[K4 | V4] | ?LEAF3(K1, K2, K3, V1, V2, V3)];
%         true ->
%             error_badkey(Key)
%     end.
% 
% take_leaf3(Key, K1, K2, K3, V1, V2, V3) ->
%     if
%         Key < K2 ->
%             if
%                 Key == K1 ->
%                     [[K1 | V1] | ?LEAF2(K2, K3, V2, V3)];
%                 true ->
%                     error_badkey(Key)
%             end;
%         Key > K2 ->
%             if
%                 Key == K3 ->
%                     [[K3 | V3] | ?LEAF2(K1, K2, V1, V2)];
%                 true ->
%                     error_badkey(Key)
%             end;
%         true ->
%             [[K2 | V2] | ?LEAF2(K1, K3, V1, V3)]
%     end.
% 
% take_leaf2(Key, Parent, Pos, K1, K2, V1, V2) ->
%     if
%         Key == K1 ->
%             [[K1 | V1] | rebalance_leaf2(Parent, Pos, K2, V2)];
%         Key == K2 ->
%             [[K2 | V2] | rebalance_leaf2(Parent, Pos, K1, V1)];
%         true ->
%             error_badkey(Key)
%     end.
% 
% take_leaf1(Key, K1, V1) ->
%     if
%         Key == K1 ->
%             [[K1 | V1] | ?LEAF0];
%         true ->
%             error_badkey(Key)
%     end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Statistics for Validation
%% ------------------------------------------------------------------

stats(Root) ->
    Acc0 = #{
        min_height => 0,
        max_height => 0,
        node_counts => #{},
        wrong_depth_counts => #{}
    },

    case Root of
        ?LEAF0 ->
            Acc0;
        Node ->
            Depth = 1,
            Acc1 = Acc0#{min_height => infinity},
            stats_recur(Node, Depth, Acc1)
    end.

stats_recur(?INTERNAL4(_, _, _, _, _, C1, C2, C3, C4, C5), Depth, Acc) ->
    Acc2 = stats_inc_node_count(Acc, internal4),
    Acc3 = stats_recur(C1, Depth + 1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, Acc4),
    Acc6 = stats_recur(C4, Depth + 1, Acc5),
    Acc7 = stats_recur(C5, Depth + 1, Acc6),
    Acc7;
stats_recur(?INTERNAL3(_, _, _, _, C1, C2, C3, C4), Depth, Acc) ->
    Acc2 = stats_inc_node_count(Acc, internal3),
    Acc3 = stats_recur(C1, Depth + 1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, Acc4),
    Acc6 = stats_recur(C4, Depth + 1, Acc5),
    Acc6;
stats_recur(?INTERNAL2(_, _, _, C1, C2, C3), Depth, Acc) ->
    Acc2 = stats_inc_node_count(Acc, internal2),
    Acc3 = stats_recur(C1, Depth + 1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, Acc4),
    Acc5;
stats_recur(?INTERNAL1(_, _, C1, C2), Depth, Acc) ->
    Acc2 =
        case Depth of
            1 -> Acc;
            _ -> stats_inc_wrong_depth_count(Acc, {internal1, Depth})
        end,
    Acc3 = stats_inc_node_count(Acc2, internal1),
    Acc4 = stats_recur(C1, Depth + 1, Acc3),
    Acc5 = stats_recur(C2, Depth + 1, Acc4),
    Acc5;
stats_recur(?LEAF4(_, _, _, _, _, _, _, _), Depth, Acc) ->
    Acc2 = stats_inc_node_count(Acc, leaf4),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF3(_, _, _, _, _, _), Depth, Acc) ->
    Acc2 = stats_inc_node_count(Acc, leaf3),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF2(_, _, _, _), Depth, Acc) ->
    Acc2 = stats_inc_node_count(Acc, leaf2),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF1(_, _), Depth, Acc) ->
    Acc2 =
        case Depth of
            1 -> Acc;
            _ -> stats_inc_wrong_depth_count(Acc, {leaf1, Depth})
        end,
    Acc3 = stats_inc_node_count(Acc2, leaf1),
    Acc4 = stats_register_leaf_height(Acc3, Depth),
    Acc4.

stats_inc_wrong_depth_count(#{wrong_depth_counts := Counts} = Acc, Pair) ->
    UpdatedCounts = map_inc_counter(Pair, Counts),
    Acc#{wrong_depth_counts := UpdatedCounts}.

stats_inc_node_count(#{node_counts := Counts} = Acc, NodeType) ->
    UpdatedCounts = map_inc_counter(NodeType, Counts),
    Acc#{node_counts := UpdatedCounts}.

map_inc_counter(Key, Map) ->
    maps:update_with(Key, fun(Prev) -> Prev + 1 end, 1, Map).

stats_register_leaf_height(#{min_height := Min, max_height := Max} = Acc, Depth) ->
    Acc#{
        min_height := min(Depth, Min),
        max_height := max(Depth, Max)
    }.

count_keys_from_stats(NodeCounts) ->
    maps:fold(
        fun(NodeType, Count, Acc) ->
            Increment = Count * keys_in_node_type(NodeType),
            Acc + Increment
        end,
        0,
        NodeCounts
    ).

keys_in_node_type(internal4) -> 4;
keys_in_node_type(internal3) -> 3;
keys_in_node_type(internal2) -> 2;
keys_in_node_type(internal1) -> 1;
keys_in_node_type(leaf4) -> 4;
keys_in_node_type(leaf3) -> 3;
keys_in_node_type(leaf2) -> 2;
keys_in_node_type(leaf1) -> 1.

%% ------------------------------------------------------------------
%% Internal Function Definitions: foldl and foldr
%% ------------------------------------------------------------------

-spec foldl_recur(fun((Key, Value, Acc1) -> Acc2), Acc0, deep_node(Key, Value)) -> AccN when
    AccN :: Acc2, Acc2 :: Acc1, Acc1 :: Acc0.
-dialyzer({no_underspecs, foldl_recur/3}).
foldl_recur(Fun, Acc, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, C1, C2, C3, C4, C5)) ->
    Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
    Acc3 = Fun(K2, V2, foldl_recur(Fun, Acc2, C2)),
    Acc4 = Fun(K3, V3, foldl_recur(Fun, Acc3, C3)),
    Acc5 = Fun(K4, V4, foldl_recur(Fun, Acc4, C4)),
    foldl_recur(Fun, Acc5, C5);
foldl_recur(Fun, Acc, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, C1, C2, C3, C4)) ->
    Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
    Acc3 = Fun(K2, V2, foldl_recur(Fun, Acc2, C2)),
    Acc4 = Fun(K3, V3, foldl_recur(Fun, Acc3, C3)),
    foldl_recur(Fun, Acc4, C4);
foldl_recur(Fun, Acc, ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, C3)) ->
    Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
    Acc3 = Fun(K2, V2, foldl_recur(Fun, Acc2, C2)),
    foldl_recur(Fun, Acc3, C3);
foldl_recur(Fun, Acc, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    Acc2 = Fun(K1, V1, Acc),
    Acc3 = Fun(K2, V2, Acc2),
    Acc4 = Fun(K3, V3, Acc3),
    Acc5 = Fun(K4, V4, Acc4),
    Acc5;
foldl_recur(Fun, Acc, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    Acc2 = Fun(K1, V1, Acc),
    Acc3 = Fun(K2, V2, Acc2),
    Acc4 = Fun(K3, V3, Acc3),
    Acc4;
foldl_recur(Fun, Acc, ?LEAF2(K1, K2, V1, V2)) ->
    Acc2 = Fun(K1, V1, Acc),
    Acc3 = Fun(K2, V2, Acc2),
    Acc3.

-spec foldr_recur(fun((Key, Value, Acc1) -> Acc2), Acc0, deep_node(Key, Value)) -> AccN when
    AccN :: Acc2, Acc2 :: Acc1, Acc1 :: Acc0.
-dialyzer({no_underspecs, foldr_recur/3}).
foldr_recur(Fun, Acc, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, C1, C2, C3, C4, C5)) ->
    Acc2 = Fun(K4, V4, foldr_recur(Fun, Acc, C5)),
    Acc3 = Fun(K3, V3, foldr_recur(Fun, Acc2, C4)),
    Acc4 = Fun(K2, V2, foldr_recur(Fun, Acc3, C3)),
    Acc5 = Fun(K1, V1, foldr_recur(Fun, Acc4, C2)),
    foldr_recur(Fun, Acc5, C1);
foldr_recur(Fun, Acc, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, C1, C2, C3, C4)) ->
    Acc2 = Fun(K3, V3, foldr_recur(Fun, Acc, C4)),
    Acc3 = Fun(K2, V2, foldr_recur(Fun, Acc2, C3)),
    Acc4 = Fun(K1, V1, foldr_recur(Fun, Acc3, C2)),
    foldr_recur(Fun, Acc4, C1);
foldr_recur(Fun, Acc, ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, C3)) ->
    Acc2 = Fun(K2, V2, foldr_recur(Fun, Acc, C3)),
    Acc3 = Fun(K1, V1, foldr_recur(Fun, Acc2, C2)),
    foldr_recur(Fun, Acc3, C1);
foldr_recur(Fun, Acc, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    Acc2 = Fun(K4, V4, Acc),
    Acc3 = Fun(K3, V3, Acc2),
    Acc4 = Fun(K2, V2, Acc3),
    Acc5 = Fun(K1, V1, Acc4),
    Acc5;
foldr_recur(Fun, Acc, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    Acc2 = Fun(K3, V3, Acc),
    Acc3 = Fun(K2, V2, Acc2),
    Acc4 = Fun(K1, V1, Acc3),
    Acc4;
foldr_recur(Fun, Acc, ?LEAF2(K1, K2, V1, V2)) ->
    Acc2 = Fun(K2, V2, Acc),
    Acc3 = Fun(K1, V1, Acc2),
    Acc3.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Iterator Steps
%% ------------------------------------------------------------------

-spec iterator_steps_l(t(Key, Value)) -> [iterator_step(Key, Value)].
-dialyzer({no_underspecs, iterator_steps_l/1}).
iterator_steps_l(?INTERNAL1(K1, V1, C1, C2)) ->
    iterator_steps_l_recur(C1, [{K1, V1, C2}]);
iterator_steps_l(?LEAF1(K1, V1)) ->
    [{K1, V1}];
iterator_steps_l(?LEAF0) ->
    [];
iterator_steps_l(Node) ->
    iterator_steps_l_recur(Node, []).

-spec iterator_steps_l_recur(deep_node(Key, Value), [iterator_step(Key, Value)]) ->
    [iterator_step(Key, Value), ...].
iterator_steps_l_recur(Node, Acc) ->
    case Node of
        ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, C1, C2, C3, C4, C5) ->
            iterator_steps_l_recur(C1, [
                {K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc
            ]);
        ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, C1, C2, C3, C4) ->
            iterator_steps_l_recur(C1, [{K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4} | Acc]);
        ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, C3) ->
            iterator_steps_l_recur(C1, [{K1, V1, C2}, {K2, V2, C3} | Acc]);
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4} | Acc];
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            [{K1, V1}, {K2, V2}, {K3, V3} | Acc];
        ?LEAF2(K1, K2, V1, V2) ->
            [{K1, V1}, {K2, V2} | Acc]
    end.

%%%%

-spec iterator_steps_l_from(Key, t(Key, Value)) -> [iterator_step(Key, Value)].
iterator_steps_l_from(Key, ?INTERNAL1(K1, V1, C1, C2)) ->
    if
        Key < K1 ->
            iterator_steps_l_from_recur(Key, C1, [{K1, V1, C2}]);
        Key > K1 ->
            iterator_steps_l_from_recur(Key, C2, []);
        true ->
            [{K1, V1, C2}]
    end;
iterator_steps_l_from(Key, ?LEAF1(K1, V1)) ->
    if
        Key > K1 ->
            [];
        true ->
            [{K1, V1}]
    end;
iterator_steps_l_from(_, ?LEAF0) ->
    [];
iterator_steps_l_from(Key, Node) ->
    iterator_steps_l_from_recur(Key, Node, []).

-spec iterator_steps_l_from_recur(Key, deep_node(Key, Value), [iterator_step(Key, Value)]) ->
    [iterator_step(Key, Value)].
iterator_steps_l_from_recur(
    Key, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, C1, C2, C3, C4, C5), Acc
) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    iterator_steps_l_from_recur(Key, C1, [
                        {K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc
                    ]);
                Key > K1 ->
                    iterator_steps_l_from_recur(Key, C2, [
                        {K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc
                    ]);
                true ->
                    [{K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc]
            end;
        Key > K2 ->
            if
                Key < K3 ->
                    iterator_steps_l_from_recur(Key, C3, [{K3, V3, C4}, {K4, V4, C5} | Acc]);
                Key > K3 ->
                    if
                        Key < K4 ->
                            iterator_steps_l_from_recur(Key, C4, [{K4, V4, C5} | Acc]);
                        Key > K4 ->
                            iterator_steps_l_from_recur(Key, C5, Acc);
                        true ->
                            [{K4, V4, C5} | Acc]
                    end;
                true ->
                    [{K3, V3, C4}, {K4, V4, C5} | Acc]
            end;
        true ->
            [{K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc]
    end;
iterator_steps_l_from_recur(Key, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, C1, C2, C3, C4), Acc) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    iterator_steps_l_from_recur(Key, C1, [
                        {K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4} | Acc
                    ]);
                Key > K1 ->
                    iterator_steps_l_from_recur(Key, C2, [{K2, V2, C3}, {K3, V3, C4} | Acc]);
                true ->
                    [{K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4} | Acc]
            end;
        Key > K2 ->
            if
                Key < K3 ->
                    iterator_steps_l_from_recur(Key, C3, [{K3, V3, C4} | Acc]);
                Key > K3 ->
                    iterator_steps_l_from_recur(Key, C4, Acc);
                true ->
                    [{K3, V3, C4} | Acc]
            end;
        true ->
            [{K2, V2, C3}, {K3, V3, C4} | Acc]
    end;
iterator_steps_l_from_recur(Key, ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, C3), Acc) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    iterator_steps_l_from_recur(Key, C1, [{K1, V1, C2}, {K2, V2, C3} | Acc]);
                Key > K1 ->
                    iterator_steps_l_from_recur(Key, C2, [{K2, V2, C3} | Acc]);
                true ->
                    [{K1, V1, C2}, {K2, V2, C3} | Acc]
            end;
        Key > K2 ->
            iterator_steps_l_from_recur(Key, C3, Acc);
        true ->
            [{K2, V2, C3} | Acc]
    end;
iterator_steps_l_from_recur(Key, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), Acc) ->
    if
        Key > K2 ->
            if
                Key > K3 ->
                    if
                        Key > K4 ->
                            Acc;
                        true ->
                            [{K4, V4} | Acc]
                    end;
                true ->
                    [{K3, V3}, {K4, V4} | Acc]
            end;
        true ->
            if
                Key > K1 ->
                    [{K2, V2}, {K3, V3}, {K4, V4} | Acc];
                true ->
                    [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4} | Acc]
            end
    end;
iterator_steps_l_from_recur(Key, ?LEAF3(K1, K2, K3, V1, V2, V3), Acc) ->
    if
        Key > K2 ->
            if
                Key > K3 ->
                    Acc;
                true ->
                    [{K3, V3} | Acc]
            end;
        true ->
            if
                Key > K1 ->
                    [{K2, V2}, {K3, V3} | Acc];
                true ->
                    [{K1, V1}, {K2, V2}, {K3, V3} | Acc]
            end
    end;
iterator_steps_l_from_recur(Key, ?LEAF2(K1, K2, V1, V2), Acc) ->
    if
        Key > K2 ->
            Acc;
        Key > K1 ->
            [{K2, V2} | Acc];
        true ->
            [{K1, V1}, {K2, V2} | Acc]
    end.

%%%%

-spec iterator_steps_r(t(Key, Value)) -> [iterator_step(Key, Value)].
-dialyzer({no_underspecs, iterator_steps_r/1}).
iterator_steps_r(?INTERNAL1(K1, V1, C1, C2)) ->
    iterator_steps_r_recur(C2, [{K1, V1, C1}]);
iterator_steps_r(?LEAF1(K1, V1)) ->
    [{K1, V1}];
iterator_steps_r(?LEAF0) ->
    [];
iterator_steps_r(Node) ->
    iterator_steps_r_recur(Node, []).

-spec iterator_steps_r_recur(deep_node(Key, Value), [iterator_step(Key, Value)]) ->
    [iterator_step(Key, Value), ...].
iterator_steps_r_recur(Node, Acc) ->
    case Node of
        ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, C1, C2, C3, C4, C5) ->
            iterator_steps_r_recur(C5, [
                {K4, V4, C4}, {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc
            ]);
        ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, C1, C2, C3, C4) ->
            iterator_steps_r_recur(C4, [{K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc]);
        ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, C3) ->
            iterator_steps_r_recur(C3, [{K2, V2, C2}, {K1, V1, C1} | Acc]);
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            [{K4, V4}, {K3, V3}, {K2, V2}, {K1, V1} | Acc];
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            [{K3, V3}, {K2, V2}, {K1, V1} | Acc];
        ?LEAF2(K1, K2, V1, V2) ->
            [{K2, V2}, {K1, V1} | Acc]
    end.

%%%

-spec iterator_steps_r_from(Key, t(Key, Value)) -> [iterator_step(Key, Value)].
iterator_steps_r_from(Key, ?LEAF1(K1, V1)) ->
    if
        Key < K1 ->
            [];
        true ->
            [{K1, V1}]
    end;
iterator_steps_r_from(Key, ?INTERNAL1(K1, V1, C1, C2)) ->
    if
        Key > K1 ->
            iterator_steps_r_from_recur(Key, C2, [{K1, V1, C1}]);
        Key < K1 ->
            iterator_steps_r_from_recur(Key, C1, []);
        true ->
            [{K1, V1, C1}]
    end;
iterator_steps_r_from(_, ?LEAF0) ->
    [];
iterator_steps_r_from(Key, Node) ->
    iterator_steps_r_from_recur(Key, Node, []).

-spec iterator_steps_r_from_recur(Key, deep_node(Key, Value), [iterator_step(Key, Value)]) ->
    [iterator_step(Key, Value)].
iterator_steps_r_from_recur(
    Key, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, C1, C2, C3, C4, C5), Acc
) ->
    if
        Key > K2 ->
            if
                Key > K3 ->
                    if
                        Key > K4 ->
                            iterator_steps_r_from_recur(Key, C5, [
                                {K4, V4, C4}, {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc
                            ]);
                        Key < K4 ->
                            iterator_steps_r_from_recur(Key, C4, [
                                {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc
                            ]);
                        true ->
                            [{K4, V4, C4}, {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc]
                    end;
                Key < K3 ->
                    iterator_steps_r_from_recur(Key, C3, [{K2, V2, C2}, {K1, V1, C1} | Acc]);
                true ->
                    [{K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc]
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    iterator_steps_r_from_recur(Key, C2, [{K1, V1, C1} | Acc]);
                Key < K1 ->
                    iterator_steps_r_from_recur(Key, C1, Acc);
                true ->
                    [{K1, V1, C1} | Acc]
            end;
        true ->
            [{K2, V2, C2}, {K1, V1, C1} | Acc]
    end;
iterator_steps_r_from_recur(Key, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, C1, C2, C3, C4), Acc) ->
    if
        Key > K2 ->
            if
                Key > K3 ->
                    iterator_steps_r_from_recur(Key, C4, [
                        {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc
                    ]);
                Key < K3 ->
                    iterator_steps_r_from_recur(Key, C3, [{K2, V2, C2}, {K1, V1, C1} | Acc]);
                true ->
                    [{K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc]
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    iterator_steps_r_from_recur(Key, C2, [{K1, V1, C1} | Acc]);
                Key < K1 ->
                    iterator_steps_r_from_recur(Key, C1, Acc);
                true ->
                    [{K1, V1, C1} | Acc]
            end;
        true ->
            [{K2, V2, C2}, {K1, V1, C1} | Acc]
    end;
iterator_steps_r_from_recur(Key, ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, C3), Acc) ->
    if
        Key > K1 ->
            if
                Key > K2 ->
                    iterator_steps_r_from_recur(Key, C3, [{K2, V2, C2}, {K1, V1, C1} | Acc]);
                Key < K2 ->
                    iterator_steps_r_from_recur(Key, C2, [{K1, V1, C1} | Acc]);
                true ->
                    [{K2, V2, C2}, {K1, V1, C1} | Acc]
            end;
        Key < K1 ->
            iterator_steps_r_from_recur(Key, C1, Acc);
        true ->
            [{K1, V1, C1} | Acc]
    end;
iterator_steps_r_from_recur(Key, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), Acc) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    Acc;
                true ->
                    [{K1, V1} | Acc]
            end;
        Key < K3 ->
            [{K2, V2}, {K1, V1} | Acc];
        Key < K4 ->
            [{K3, V3}, {K2, V2}, {K1, V1} | Acc];
        true ->
            [{K4, V4}, {K3, V3}, {K2, V2}, {K1, V1} | Acc]
    end;
iterator_steps_r_from_recur(Key, ?LEAF3(K1, K2, K3, V1, V2, V3), Acc) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    Acc;
                true ->
                    [{K1, V1} | Acc]
            end;
        Key < K3 ->
            [{K2, V2}, {K1, V1} | Acc];
        true ->
            [{K3, V3}, {K2, V2}, {K1, V1} | Acc]
    end;
iterator_steps_r_from_recur(Key, ?LEAF2(K1, K2, V1, V2), Acc) ->
    if
        Key < K1 ->
            Acc;
        Key < K2 ->
            [{K1, V1} | Acc];
        true ->
            [{K2, V2}, {K1, V1} | Acc]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: keys/2, values/2, to_list/2, largest/1, smallest/1
%% ------------------------------------------------------------------

-spec keys_recur(deep_node(Key, _), [Key]) -> [Key, ...].
-dialyzer({no_underspecs, keys_recur/2}).
keys_recur(?INTERNAL4(K1, K2, K3, K4, _, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [K4 | keys_recur(C5, Acc)],
    Acc3 = [K3 | keys_recur(C4, Acc2)],
    Acc4 = [K2 | keys_recur(C3, Acc3)],
    Acc5 = [K1 | keys_recur(C2, Acc4)],
    keys_recur(C1, Acc5);
keys_recur(?INTERNAL3(K1, K2, K3, _, C1, C2, C3, C4), Acc) ->
    Acc2 = [K3 | keys_recur(C4, Acc)],
    Acc3 = [K2 | keys_recur(C3, Acc2)],
    Acc4 = [K1 | keys_recur(C2, Acc3)],
    keys_recur(C1, Acc4);
keys_recur(?INTERNAL2(K1, K2, _, C1, C2, C3), Acc) ->
    Acc2 = [K2 | keys_recur(C3, Acc)],
    Acc3 = [K1 | keys_recur(C2, Acc2)],
    keys_recur(C1, Acc3);
keys_recur(?LEAF4(K1, K2, K3, K4, _, _, _, _), Acc) ->
    [K1, K2, K3, K4 | Acc];
keys_recur(?LEAF3(K1, K2, K3, _, _, _), Acc) ->
    [K1, K2, K3 | Acc];
keys_recur(?LEAF2(K1, K2, _, _), Acc) ->
    [K1, K2 | Acc].

-spec values_recur(deep_node(_, Value), [Value]) -> [Value, ...].
-dialyzer({no_underspecs, values_recur/2}).
values_recur(?INTERNAL4(_, _, _, _, {V1, V2, V3, V4}, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [V4 | values_recur(C5, Acc)],
    Acc3 = [V3 | values_recur(C4, Acc2)],
    Acc4 = [V2 | values_recur(C3, Acc3)],
    Acc5 = [V1 | values_recur(C2, Acc4)],
    values_recur(C1, Acc5);
values_recur(?INTERNAL3(_, _, _, {V1, V2, V3}, C1, C2, C3, C4), Acc) ->
    Acc2 = [V3 | values_recur(C4, Acc)],
    Acc3 = [V2 | values_recur(C3, Acc2)],
    Acc4 = [V1 | values_recur(C2, Acc3)],
    values_recur(C1, Acc4);
values_recur(?INTERNAL2(_, _, [V1 | V2], C1, C2, C3), Acc) ->
    Acc2 = [V2 | values_recur(C3, Acc)],
    Acc3 = [V1 | values_recur(C2, Acc2)],
    values_recur(C1, Acc3);
values_recur(?LEAF4(_, _, _, _, V1, V2, V3, V4), Acc) ->
    [V1, V2, V3, V4 | Acc];
values_recur(?LEAF3(_, _, _, V1, V2, V3), Acc) ->
    [V1, V2, V3 | Acc];
values_recur(?LEAF2(_, _, V1, V2), Acc) ->
    [V1, V2 | Acc].

-spec to_list_recur(deep_node(Key, Value), [{Key, Value}]) -> [{Key, Value}, ...].
-dialyzer({no_underspecs, to_list_recur/2}).
to_list_recur(?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [{K4, V4} | to_list_recur(C5, Acc)],
    Acc3 = [{K3, V3} | to_list_recur(C4, Acc2)],
    Acc4 = [{K2, V2} | to_list_recur(C3, Acc3)],
    Acc5 = [{K1, V1} | to_list_recur(C2, Acc4)],
    to_list_recur(C1, Acc5);
to_list_recur(?INTERNAL3(K1, K2, K3, {V1, V2, V3}, C1, C2, C3, C4), Acc) ->
    Acc2 = [{K3, V3} | to_list_recur(C4, Acc)],
    Acc3 = [{K2, V2} | to_list_recur(C3, Acc2)],
    Acc4 = [{K1, V1} | to_list_recur(C2, Acc3)],
    to_list_recur(C1, Acc4);
to_list_recur(?INTERNAL2(K1, K2, [V1 | V2], C1, C2, C3), Acc) ->
    Acc2 = [{K2, V2} | to_list_recur(C3, Acc)],
    Acc3 = [{K1, V1} | to_list_recur(C2, Acc2)],
    to_list_recur(C1, Acc3);
to_list_recur(?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), Acc) ->
    [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4} | Acc];
to_list_recur(?LEAF3(K1, K2, K3, V1, V2, V3), Acc) ->
    [{K1, V1}, {K2, V2}, {K3, V3} | Acc];
to_list_recur(?LEAF2(K1, K2, V1, V2), Acc) ->
    [{K1, V1}, {K2, V2} | Acc].

-spec largest_recur(deep_node(Key, Value)) -> {Key, Value}.
largest_recur(?INTERNAL4(_, _, _, _, _, _, _, _, _, C5)) ->
    largest_recur(C5);
largest_recur(?INTERNAL3(_, _, _, _, _, _, _, C4)) ->
    largest_recur(C4);
largest_recur(?INTERNAL2(_, _, _, _, _, C3)) ->
    largest_recur(C3);
largest_recur(?LEAF4(_, _, _, K4, _, _, _, V4)) ->
    {K4, V4};
largest_recur(?LEAF3(_, _, K3, _, _, V3)) ->
    {K3, V3};
largest_recur(?LEAF2(_, K2, _, V2)) ->
    {K2, V2}.

-spec smallest_recur(deep_node(Key, Value)) -> {Key, Value}.
smallest_recur(?INTERNAL4(_, _, _, _, _, C1, _, _, _, _)) ->
    smallest_recur(C1);
smallest_recur(?INTERNAL3(_, _, _, _, C1, _, _, _)) ->
    smallest_recur(C1);
smallest_recur(?INTERNAL2(_, _, _, C1, _, _)) ->
    smallest_recur(C1);
smallest_recur(?LEAF4(K1, _, _, _, V1, _, _, _)) ->
    {K1, V1};
smallest_recur(?LEAF3(K1, _, _, V1, _, _)) ->
    {K1, V1};
smallest_recur(?LEAF2(K1, _, V1, _)) ->
    {K1, V1}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: larger/2
%% ------------------------------------------------------------------

-spec larger_recur(Key, deep_node(Key, Value)) -> {Key, Value} | none.
larger_recur(Key, ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)) ->
    case Key < K2 of
        true ->
            case Key < K1 of
                true ->
                    case larger_recur(Key, C1) of
                        none -> {K1, element(1, Values)};
                        Pair -> Pair
                    end;
                _ ->
                    case larger_recur(Key, C2) of
                        none -> {K2, element(2, Values)};
                        Pair -> Pair
                    end
            end;
        _ ->
            case Key < K3 of
                true ->
                    case larger_recur(Key, C3) of
                        none -> {K3, element(3, Values)};
                        Pair -> Pair
                    end;
                _ ->
                    case Key < K4 of
                        true ->
                            case larger_recur(Key, C4) of
                                none -> {K4, element(4, Values)};
                                Pair -> Pair
                            end;
                        _ ->
                            larger_recur(Key, C5)
                    end
            end
    end;
larger_recur(Key, ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4)) ->
    case Key < K2 of
        true ->
            case Key < K1 of
                true ->
                    case larger_recur(Key, C1) of
                        none -> {K1, element(1, Values)};
                        Pair -> Pair
                    end;
                _ ->
                    case larger_recur(Key, C2) of
                        none -> {K2, element(2, Values)};
                        Pair -> Pair
                    end
            end;
        _ ->
            case Key < K3 of
                true ->
                    case larger_recur(Key, C3) of
                        none -> {K3, element(3, Values)};
                        Pair -> Pair
                    end;
                _ ->
                    larger_recur(Key, C4)
            end
    end;
larger_recur(Key, ?INTERNAL2(K1, K2, Values, C1, C2, C3)) ->
    case Key < K2 of
        true ->
            case Key < K1 of
                true ->
                    case larger_recur(Key, C1) of
                        none -> {K1, hd(Values)};
                        Pair -> Pair
                    end;
                _ ->
                    case larger_recur(Key, C2) of
                        none -> {K2, tl(Values)};
                        Pair -> Pair
                    end
            end;
        _ ->
            larger_recur(Key, C3)
    end;
larger_recur(Key, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    case Key < K2 of
        true ->
            case Key < K1 of
                true ->
                    {K1, V1};
                _ ->
                    {K2, V2}
            end;
        _ ->
            case Key < K3 of
                true ->
                    {K3, V3};
                _ ->
                    case Key < K4 of
                        true ->
                            {K4, V4};
                        _ ->
                            none
                    end
            end
    end;
larger_recur(Key, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    case Key < K2 of
        true ->
            case Key < K1 of
                true ->
                    {K1, V1};
                _ ->
                    {K2, V2}
            end;
        _ ->
            case Key < K3 of
                true ->
                    {K3, V3};
                _ ->
                    none
            end
    end;
larger_recur(Key, ?LEAF2(K1, K2, V1, V2)) ->
    case Key < K2 of
        true ->
            case Key < K1 of
                true ->
                    {K1, V1};
                _ ->
                    {K2, V2}
            end;
        _ ->
            none
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: smaller/2
%% ------------------------------------------------------------------

-spec smaller_recur(Key, deep_node(Key, Value)) -> {Key, Value} | none.
smaller_recur(Key, ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)) ->
    case Key > K2 of
        true ->
            case Key > K3 of
                true ->
                    case Key > K4 of
                        true ->
                            case smaller_recur(Key, C5) of
                                none -> {K4, element(4, Values)};
                                Pair -> Pair
                            end;
                        _ ->
                            case smaller_recur(Key, C4) of
                                none -> {K3, element(3, Values)};
                                Pair -> Pair
                            end
                    end;
                _ ->
                    case smaller_recur(Key, C3) of
                        none -> {K2, element(2, Values)};
                        Pair -> Pair
                    end
            end;
        _ ->
            case Key > K1 of
                true ->
                    case smaller_recur(Key, C2) of
                        none -> {K1, element(1, Values)};
                        Pair -> Pair
                    end;
                _ ->
                    smaller_recur(Key, C1)
            end
    end;
smaller_recur(Key, ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4)) ->
    case Key > K2 of
        true ->
            case Key > K3 of
                true ->
                    case smaller_recur(Key, C4) of
                        none -> {K3, element(3, Values)};
                        Pair -> Pair
                    end;
                _ ->
                    case smaller_recur(Key, C3) of
                        none -> {K2, element(2, Values)};
                        Pair -> Pair
                    end
            end;
        _ ->
            case Key > K1 of
                true ->
                    case smaller_recur(Key, C2) of
                        none -> {K1, element(1, Values)};
                        Pair -> Pair
                    end;
                _ ->
                    smaller_recur(Key, C1)
            end
    end;
smaller_recur(Key, ?INTERNAL2(K1, K2, Values, C1, C2, C3)) ->
    case Key > K1 of
        true ->
            case Key > K2 of
                true ->
                    case smaller_recur(Key, C3) of
                        none -> {K2, tl(Values)};
                        Pair -> Pair
                    end;
                _ ->
                    case smaller_recur(Key, C2) of
                        none -> {K1, hd(Values)};
                        Pair -> Pair
                    end
            end;
        _ ->
            smaller_recur(Key, C1)
    end;
smaller_recur(Key, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    case Key > K2 of
        true ->
            case Key > K3 of
                true ->
                    case Key > K4 of
                        true ->
                            {K4, V4};
                        _ ->
                            {K3, V3}
                    end;
                _ ->
                    {K2, V2}
            end;
        _ ->
            case Key > K1 of
                true ->
                    {K1, V1};
                _ ->
                    none
            end
    end;
smaller_recur(Key, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    case Key > K2 of
        true ->
            case Key > K3 of
                true ->
                    {K3, V3};
                _ ->
                    {K2, V2}
            end;
        _ ->
            case Key > K1 of
                true ->
                    {K1, V1};
                _ ->
                    none
            end
    end;
smaller_recur(Key, ?LEAF2(K1, K2, V1, V2)) ->
    case Key > K1 of
        true ->
            case Key > K2 of
                true ->
                    {K2, V2};
                _ ->
                    {K1, V1}
            end;
        _ ->
            none
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: map/2
%% ------------------------------------------------------------------

-spec map_recur(fun((Key, Value) -> MappedValue), deep_node(Key, Value)) ->
    deep_node(Key, MappedValue).
-dialyzer({no_underspecs, map_recur/2}).
map_recur(Fun, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, C1, C2, C3, C4, C5)) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        {Fun(K1, V1), Fun(K2, V2), Fun(K3, V3), Fun(K4, V4)},
        map_recur(Fun, C1),
        map_recur(Fun, C2),
        map_recur(Fun, C3),
        map_recur(Fun, C4),
        map_recur(Fun, C5)
    );
map_recur(Fun, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, C1, C2, C3, C4)) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        {Fun(K1, V1), Fun(K2, V2), Fun(K3, V3)},
        map_recur(Fun, C1),
        map_recur(Fun, C2),
        map_recur(Fun, C3),
        map_recur(Fun, C4)
    );
map_recur(Fun, ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, C3)) ->
    ?INTERNAL2(
        K1,
        K2,
        [Fun(K1, V1) | Fun(K2, V2)],
        map_recur(Fun, C1),
        map_recur(Fun, C2),
        map_recur(Fun, C3)
    );
map_recur(Fun, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    ?LEAF4(
        K1,
        K2,
        K3,
        K4,
        Fun(K1, V1),
        Fun(K2, V2),
        Fun(K3, V3),
        Fun(K4, V4)
    );
map_recur(Fun, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    ?LEAF3(
        K1,
        K2,
        K3,
        Fun(K1, V1),
        Fun(K2, V2),
        Fun(K3, V3)
    );
map_recur(Fun, ?LEAF2(K1, K2, V1, V2)) ->
    ?LEAF2(
        K1,
        K2,
        Fun(K1, V1),
        Fun(K2, V2)
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Rebalancing after Deletion
%% ------------------------------------------------------------------

% maybe_rebalance_internal4_child1(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     case C1 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [UpdatedC1 | UpdatedC2] ->
%                     ?INTERNAL4(K1, K2, K3, K4, Values, UpdatedC1, UpdatedC2, C3, C4, C5);
%                 MergedC1C2 ->
%                     RemainingValues = erlang:delete_element(1, Values),
%                     ?INTERNAL3(K2, K3, K4, RemainingValues, MergedC1C2, C3, C4, C5)
%             end;
%         UpdatedC1 ->
%             ?INTERNAL4(K1, K2, K3, K4, Values, UpdatedC1, C2, C3, C4, C5)
%     end.
% 
% maybe_rebalance_internal4_child2(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     case C2 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [from_left, UpdatedC1 | UpdatedC2] ->
%                     ?INTERNAL4(K1, K2, K3, K4, Values, UpdatedC1, UpdatedC2, C3, C4, C5);
%                 [UpdatedC2 | UpdatedC3] ->
%                     ?INTERNAL4(K1, K2, K3, K4, Values, C1, UpdatedC2, UpdatedC3, C4, C5);
%                 MergedC1C2 ->
%                     RemainingValues = erlang:delete_element(1, Values),
%                     ?INTERNAL3(K2, K3, K4, RemainingValues, MergedC1C2, C3, C4, C5)
%             end;
%         UpdatedC2 ->
%             ?INTERNAL4(K1, K2, K3, K4, Values, C1, UpdatedC2, C3, C4, C5)
%     end.
% 
% maybe_rebalance_internal4_child3(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     case C3 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [from_left, UpdatedC2 | UpdatedC3] ->
%                     ?INTERNAL4(K1, K2, K3, K4, Values, C1, UpdatedC2, UpdatedC3, C4, C5);
%                 [UpdatedC3 | UpdatedC4] ->
%                     ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, UpdatedC3, UpdatedC4, C5);
%                 MergedC2C3 ->
%                     RemainingValues = erlang:delete_element(2, Values),
%                     ?INTERNAL3(K1, K3, K4, RemainingValues, C1, MergedC2C3, C4, C5)
%             end;
%         UpdatedC3 ->
%             ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, UpdatedC3, C4, C5)
%     end.
% 
% maybe_rebalance_internal4_child4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     case C4 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [from_left, UpdatedC3 | UpdatedC4] ->
%                     ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, UpdatedC3, UpdatedC4, C5);
%                 [UpdatedC4 | UpdatedC5] ->
%                     ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, UpdatedC4, UpdatedC5);
%                 MergedC3C4 ->
%                     RemainingValues = erlang:delete_element(3, Values),
%                     ?INTERNAL3(K1, K2, K4, RemainingValues, C1, C2, MergedC3C4, C5)
%             end;
%         UpdatedC4 ->
%             ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, UpdatedC4, C5)
%     end.
% 
% maybe_rebalance_internal4_child5(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
%     case C5 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [UpdatedC4 | UpdatedC5] ->
%                     ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, UpdatedC4, UpdatedC5);
%                 MergedC4C5 ->
%                     RemainingValues = erlang:delete_element(4, Values),
%                     ?INTERNAL3(K1, K2, K3, RemainingValues, C1, C2, C3, MergedC4C5)
%             end;
%         UpdatedC5 ->
%             ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, UpdatedC5)
%     end.
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% maybe_rebalance_internal3_child1(K1, K2, K3, Values, C1, C2, C3, C4) ->
%     case C1 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [UpdatedC1 | UpdatedC2] ->
%                     ?INTERNAL3(K1, K2, K3, Values, UpdatedC1, UpdatedC2, C3, C4);
%                 MergedC1C2 ->
%                     {_, V2, V3} = Values,
%                     ?INTERNAL2(K2, K3, [V2 | V3], MergedC1C2, C3, C4)
%             end;
%         UpdatedC1 ->
%             ?INTERNAL3(K1, K2, K3, Values, UpdatedC1, C2, C3, C4)
%     end.
% 
% maybe_rebalance_internal3_child2(K1, K2, K3, Values, C1, C2, C3, C4) ->
%     case C2 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [from_left, UpdatedC1 | UpdatedC2] ->
%                     ?INTERNAL3(K1, K2, K3, Values, UpdatedC1, UpdatedC2, C3, C4);
%                 [UpdatedC2 | UpdatedC3] ->
%                     ?INTERNAL3(K1, K2, K3, Values, C1, UpdatedC2, UpdatedC3, C4);
%                 MergedC1C2 ->
%                     {_, V2, V3} = Values,
%                     ?INTERNAL2(K2, K3, [V2 | V3], MergedC1C2, C3, C4)
%             end;
%         UpdatedC2 ->
%             ?INTERNAL3(K1, K2, K3, Values, C1, UpdatedC2, C3, C4)
%     end.
% 
% maybe_rebalance_internal3_child3(K1, K2, K3, Values, C1, C2, C3, C4) ->
%     case C3 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [from_left, UpdatedC2 | UpdatedC3] ->
%                     ?INTERNAL3(K1, K2, K3, Values, C1, UpdatedC2, UpdatedC3, C4);
%                 [UpdatedC3 | UpdatedC4] ->
%                     ?INTERNAL3(K1, K2, K3, Values, C1, C2, UpdatedC3, UpdatedC4);
%                 MergedC2C3 ->
%                     {V1, _, V3} = Values,
%                     ?INTERNAL2(K1, K3, [V1 | V3], C1, MergedC2C3, C4)
%             end;
%         UpdatedC3 ->
%             ?INTERNAL3(K1, K2, K3, Values, C1, C2, UpdatedC3, C4)
%     end.
% 
% maybe_rebalance_internal3_child4(K1, K2, K3, Values, C1, C2, C3, C4) ->
%     case C4 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [UpdatedC3 | UpdatedC4] ->
%                     ?INTERNAL3(K1, K2, K3, Values, C1, C2, UpdatedC3, UpdatedC4);
%                 MergedC3C4 ->
%                     {V1, V2, _} = Values,
%                     ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, MergedC3C4)
%             end;
%         UpdatedC4 ->
%             ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, UpdatedC4)
%     end.
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% maybe_rebalance_internal2_child1(Parent, Pos, K1, K2, Values, C1, C2, C3) ->
%     case C1 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [UpdatedC1 | UpdatedC2] ->
%                     ?INTERNAL2(K1, K2, Values, UpdatedC1, UpdatedC2, C3);
%                 MergedC1C2 ->
%                     RemainingK = K2,
%                     RemainingV = tl(Values),
%                     RemainingC1 = MergedC1C2,
%                     RemainingC2 = C3,
%                     rebalance_internal2(
%                         Parent, Pos, RemainingK, RemainingV, RemainingC1, RemainingC2
%                     )
%             end;
%         UpdatedC1 ->
%             ?INTERNAL2(K1, K2, Values, UpdatedC1, C2, C3)
%     end.
% 
% maybe_rebalance_internal2_child2(Parent, Pos, K1, K2, Values, C1, C2, C3) ->
%     case C2 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [from_left, UpdatedC1 | UpdatedC2] ->
%                     ?INTERNAL2(K1, K2, Values, UpdatedC1, UpdatedC2, C3);
%                 [UpdatedC2 | UpdatedC3] ->
%                     ?INTERNAL2(K1, K2, Values, C1, UpdatedC2, UpdatedC3);
%                 MergedC1C2 ->
%                     RemainingK = K2,
%                     RemainingV = tl(Values),
%                     RemainingC1 = MergedC1C2,
%                     RemainingC2 = C3,
%                     rebalance_internal2(
%                         Parent, Pos, RemainingK, RemainingV, RemainingC1, RemainingC2
%                     )
%             end;
%         UpdatedC2 ->
%             ?INTERNAL2(K1, K2, Values, C1, UpdatedC2, C3)
%     end.
% 
% maybe_rebalance_internal2_child3(Parent, Pos, K1, K2, Values, C1, C2, C3) ->
%     case C3 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [UpdatedC2 | UpdatedC3] ->
%                     ?INTERNAL2(K1, K2, Values, C1, UpdatedC2, UpdatedC3);
%                 MergedC2C3 ->
%                     RemainingK = K1,
%                     RemainingV = hd(Values),
%                     RemainingC1 = C1,
%                     RemainingC2 = MergedC2C3,
%                     rebalance_internal2(
%                         Parent, Pos, RemainingK, RemainingV, RemainingC1, RemainingC2
%                     )
%             end;
%         UpdatedC3 ->
%             ?INTERNAL2(K1, K2, Values, C1, C2, UpdatedC3)
%     end.
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% maybe_rebalance_internal1_child1(K1, V1, C1, C2) ->
%     case C1 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [UpdatedC1 | UpdatedC2] ->
%                     ?INTERNAL1(K1, V1, UpdatedC1, UpdatedC2);
%                 MergedC1C2 ->
%                     % Height reduction, can only happen on root node
%                     MergedC1C2
%             end;
%         UpdatedC1 ->
%             ?INTERNAL1(K1, V1, UpdatedC1, C2)
%     end.
% 
% maybe_rebalance_internal1_child2(K1, V1, C1, C2) ->
%     case C2 of
%         [rebalanced | RebalanceResult] ->
%             case RebalanceResult of
%                 [UpdatedC1 | UpdatedC2] ->
%                     ?INTERNAL1(K1, V1, UpdatedC1, UpdatedC2);
%                 MergedC1C2 ->
%                     % Height reduction, can only happen on root node
%                     MergedC1C2
%             end;
%         UpdatedC2 ->
%             ?INTERNAL1(K1, V1, C1, UpdatedC2)
%     end.
% 
% %%%%%%%%%%%%%%
% 
% rebalance_internal2(nil, _Pos, RemainingK, RemainingV, RemainingC1, RemainingC2) ->
%     % INTERNAL2 becomes INTERNAl1 at the root
%     ?INTERNAL1(RemainingK, RemainingV, RemainingC1, RemainingC2);
% rebalance_internal2(Parent, Pos, RemainingK, RemainingV, RemainingC1, RemainingC2) ->
%     case Pos of
%         1 ->
%             [
%                 rebalanced
%                 | rebalance_internal2_pos1(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2)
%             ];
%         2 ->
%             [
%                 rebalanced
%                 | rebalance_internal2_pos2(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2)
%             ];
%         3 ->
%             [
%                 rebalanced
%                 | rebalance_internal2_pos3(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2)
%             ];
%         4 ->
%             [
%                 rebalanced
%                 | rebalance_internal2_pos4(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2)
%             ];
%         5 ->
%             [
%                 rebalanced
%                 | rebalance_internal2_pos5(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2)
%             ]
%     end.
% 
% rebalance_internal2_pos1(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2) ->
%     case Parent of
%         ?INTERNAL2(ParentK1, _, ParentValues, _, ParentC2, _) ->
%             %
%             ParentK = ParentK1,
%             ParentV = hd(ParentValues),
%             RSibling = ParentC2,
%             rebalance_internal2_from_right_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 ParentK,
%                 ParentV,
%                 RSibling
%             );
%         %
%         ?INTERNAL3(ParentK1, _, _, ParentValues, _, ParentC2, _, _) ->
%             %
%             ParentK = ParentK1,
%             ParentV = element(1, ParentValues),
%             RSibling = ParentC2,
%             rebalance_internal2_from_right_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 ParentK,
%                 ParentV,
%                 RSibling
%             );
%         %
%         ?INTERNAL4(ParentK1, _, _, _, ParentValues, _, ParentC2, _, _, _) ->
%             %
%             ParentK = ParentK1,
%             ParentV = element(1, ParentValues),
%             RSibling = ParentC2,
%             rebalance_internal2_from_right_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 ParentK,
%                 ParentV,
%                 RSibling
%             );
%         %
%         ?INTERNAL1(ParentK1, ParentV1, _, ParentC2) ->
%             ParentK = ParentK1,
%             ParentV = ParentV1,
%             RSibling = ParentC2,
%             rebalance_internal2_from_right_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 ParentK,
%                 ParentV,
%                 RSibling
%             )
%     end.
% 
% rebalance_internal2_pos2(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2) ->
%     case Parent of
%         ?INTERNAL2(ParentK1, _, ParentValues, ParentC1, _, ParentC3) ->
%             %
%             LParentK = ParentK1,
%             LParentV = hd(ParentValues),
%             LSibling = ParentC1,
%             RSibling = ParentC3,
%             %
%             rebalance_internal2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             );
%         %
%         ?INTERNAL3(ParentK1, _, _, ParentValues, ParentC1, _, ParentC3, _) ->
%             %
%             LParentK = ParentK1,
%             LParentV = element(1, ParentValues),
%             LSibling = ParentC1,
%             RSibling = ParentC3,
%             %
%             rebalance_internal2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             );
%         %
%         ?INTERNAL4(ParentK1, _, _, _, ParentValues, ParentC1, _, ParentC3, _, _) ->
%             %
%             LParentK = ParentK1,
%             LParentV = element(1, ParentValues),
%             LSibling = ParentC1,
%             RSibling = ParentC3,
%             %
%             rebalance_internal2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             );
%         %
%         ?INTERNAL1(ParentK1, ParentV1, ParentC1, _) ->
%             ParentK = ParentK1,
%             ParentV = ParentV1,
%             LSibling = ParentC1,
%             rebalance_internal2_from_left_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 ParentK,
%                 ParentV,
%                 LSibling
%             )
%     end.
% 
% rebalance_internal2_pos3(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2) ->
%     case Parent of
%         ?INTERNAL2(_, ParentK2, ParentValues, _, ParentC2, _) ->
%             %
%             LParentK = ParentK2,
%             LParentV = tl(ParentValues),
%             LSibling = ParentC2,
%             %
%             rebalance_internal2_from_left_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 LParentK,
%                 LParentV,
%                 LSibling
%             );
%         %
%         ?INTERNAL3(_, ParentK2, _, ParentValues, _, ParentC2, _, ParentC4) ->
%             %
%             LParentK = ParentK2,
%             LParentV = element(2, ParentValues),
%             LSibling = ParentC2,
%             RSibling = ParentC4,
%             %
%             rebalance_internal2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             );
%         %
%         ?INTERNAL4(_, ParentK2, _, _, ParentValues, _, ParentC2, _, ParentC4, _) ->
%             %
%             LParentK = ParentK2,
%             LParentV = element(2, ParentValues),
%             LSibling = ParentC2,
%             RSibling = ParentC4,
%             %
%             rebalance_internal2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             )
%     end.
% 
% rebalance_internal2_pos4(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2) ->
%     case Parent of
%         ?INTERNAL3(_, _, ParentK3, ParentValues, _, _, ParentC3, _) ->
%             %
%             LParentK = ParentK3,
%             LParentV = element(3, ParentValues),
%             LSibling = ParentC3,
%             %
%             rebalance_internal2_from_left_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 LParentK,
%                 LParentV,
%                 LSibling
%             );
%         %
%         ?INTERNAL4(_, _, ParentK3, _, ParentValues, _, _, ParentC3, _, ParentC5) ->
%             %
%             LParentK = ParentK3,
%             LParentV = element(3, ParentValues),
%             LSibling = ParentC3,
%             RSibling = ParentC5,
%             %
%             rebalance_internal2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             )
%     end.
% 
% rebalance_internal2_pos5(Parent, RemainingK, RemainingV, RemainingC1, RemainingC2) ->
%     case Parent of
%         ?INTERNAL4(_, _, _, ParentK4, ParentValues, _, _, _, ParentC4, _) ->
%             %
%             LParentK = ParentK4,
%             LParentV = element(4, ParentValues),
%             LSibling = ParentC4,
%             %
%             rebalance_internal2_from_left_sibling(
%                 RemainingK,
%                 RemainingV,
%                 RemainingC1,
%                 RemainingC2,
%                 LParentK,
%                 LParentV,
%                 LSibling
%             )
%     end.
% 
% %%%%%%%%%%%%%%%%%%%%
% 
% rebalance_internal2_from_right_sibling(
%     RemainingK, RemainingV, RemainingC1, RemainingC2, ParentK, ParentV, RSibling
% ) ->
%     case RSibling of
%         ?INTERNAL4(RK1, RK2, RK3, RK4, RValues, RC1, RC2, RC3, RC4, RC5) ->
%             {RV1, RV2, RV3, RV4} = RValues,
% 
%             MovedK = RK1,
%             MovedV = RV1,
%             MovedC = RC1,
% 
%             Rebalanced = ?INTERNAL2(
%                 RemainingK,
%                 MovedK,
%                 [RemainingV | MovedV],
%                 RemainingC1,
%                 RemainingC2,
%                 MovedC
%             ),
% 
%             UpdatedSibling = ?INTERNAL3(
%                 RK2,
%                 RK3,
%                 RK4,
%                 {RV2, RV3, RV4},
%                 RC2,
%                 RC3,
%                 RC4,
%                 RC5
%             ),
%             [Rebalanced | UpdatedSibling];
%         %
%         %
%         ?INTERNAL3(RK1, RK2, RK3, RValues, RC1, RC2, RC3, RC4) ->
%             {RV1, RV2, RV3} = RValues,
% 
%             MovedK = RK1,
%             MovedV = RV1,
%             MovedC = RC1,
% 
%             Rebalanced = ?INTERNAL2(
%                 RemainingK,
%                 MovedK,
%                 [RemainingV | MovedV],
%                 RemainingC1,
%                 RemainingC2,
%                 MovedC
%             ),
% 
%             UpdatedSibling = ?INTERNAL2(
%                 RK2,
%                 RK3,
%                 [RV2 | RV3],
%                 RC2,
%                 RC3,
%                 RC4
%             ),
%             [Rebalanced | UpdatedSibling];
%         %
%         ?INTERNAL2(RK1, RK2, RValues, RC1, RC2, RC3) ->
%             [RV1 | RV2] = RValues,
% 
%             % Merge
%             ?INTERNAL4(
%                 RemainingK,
%                 ParentK,
%                 RK1,
%                 RK2,
%                 {RemainingV, ParentV, RV1, RV2},
%                 RemainingC1,
%                 RemainingC2,
%                 RC1,
%                 RC2,
%                 RC3
%             )
%     end.
% 
% rebalance_internal2_from_left_sibling(
%     RemainingK, RemainingV, RemainingC1, RemainingC2, ParentK, ParentV, LSibling
% ) ->
%     case LSibling of
%         ?INTERNAL4(LK1, LK2, LK3, LK4, LValues, LC1, LC2, LC3, LC4, LC5) ->
%             {LV1, LV2, LV3, LV4} = LValues,
% 
%             MovedK = LK4,
%             MovedV = LV4,
%             MovedC = LC5,
% 
%             Rebalanced = ?INTERNAL2(
%                 MovedK,
%                 RemainingK,
%                 [MovedV | RemainingV],
%                 MovedC,
%                 RemainingC1,
%                 RemainingC2
%             ),
% 
%             UpdatedSibling = ?INTERNAL3(
%                 LK1,
%                 LK2,
%                 LK3,
%                 {LV1, LV2, LV3},
%                 LC1,
%                 LC2,
%                 LC3,
%                 LC4
%             ),
%             [UpdatedSibling | Rebalanced];
%         %
%         %
%         ?INTERNAL3(LK1, LK2, LK3, LValues, LC1, LC2, LC3, LC4) ->
%             {LV1, LV2, LV3} = LValues,
% 
%             MovedK = LK3,
%             MovedV = LV3,
%             MovedC = LC4,
% 
%             Rebalanced = ?INTERNAL2(
%                 MovedK,
%                 RemainingK,
%                 [MovedV | RemainingV],
%                 MovedC,
%                 RemainingC1,
%                 RemainingC2
%             ),
% 
%             UpdatedSibling = ?INTERNAL2(
%                 LK1,
%                 LK2,
%                 [LV1 | LV2],
%                 LC1,
%                 LC2,
%                 LC3
%             ),
%             [UpdatedSibling | Rebalanced];
%         %
%         ?INTERNAL2(LK1, LK2, LValues, LC1, LC2, LC3) ->
%             [LV1 | LV2] = LValues,
% 
%             % Merge
%             ?INTERNAL4(
%                 LK1,
%                 LK2,
%                 ParentK,
%                 RemainingK,
%                 {LV1, LV2, ParentV, RemainingV},
%                 LC1,
%                 LC2,
%                 LC3,
%                 RemainingC1,
%                 RemainingC2
%             )
%     end.
% 
% rebalance_internal2_from_either_sibling(
%     RemainingK,
%     RemainingV,
%     RemainingC1,
%     RemainingC2,
%     LParentK,
%     LParentV,
%     LSibling,
%     RSibling
% ) ->
%     case LSibling of
%         ?INTERNAL4(LK1, LK2, LK3, LK4, LValues, LC1, LC2, LC3, LC4, LC5) ->
%             {LV1, LV2, LV3, LV4} = LValues,
% 
%             MovedK = LK4,
%             MovedV = LV4,
%             MovedC = LC5,
% 
%             Rebalanced = ?INTERNAL2(
%                 MovedK,
%                 RemainingK,
%                 [MovedV | RemainingV],
%                 MovedC,
%                 RemainingC1,
%                 RemainingC2
%             ),
% 
%             UpdatedSibling = ?INTERNAL3(
%                 LK1,
%                 LK2,
%                 LK3,
%                 {LV1, LV2, LV3},
%                 LC1,
%                 LC2,
%                 LC3,
%                 LC4
%             ),
%             [from_left, UpdatedSibling | Rebalanced];
%         %
%         %
%         %
%         ?INTERNAL3(LK1, LK2, LK3, LValues, LC1, LC2, LC3, LC4) ->
%             {LV1, LV2, LV3} = LValues,
% 
%             MovedK = LK3,
%             MovedV = LV3,
%             MovedC = LC4,
% 
%             Rebalanced = ?INTERNAL2(
%                 MovedK,
%                 RemainingK,
%                 [MovedV | RemainingV],
%                 MovedC,
%                 RemainingC1,
%                 RemainingC2
%             ),
% 
%             UpdatedSibling = ?INTERNAL2(
%                 LK1,
%                 LK2,
%                 [LV1 | LV2],
%                 LC1,
%                 LC2,
%                 LC3
%             ),
%             [from_left, UpdatedSibling | Rebalanced];
%         %
%         %
%         %
%         ?INTERNAL2(LK1, LK2, LValues, LC1, LC2, LC3) ->
%             %
%             case RSibling of
%                 ?INTERNAL4(RK1, RK2, RK3, RK4, RValues, RC1, RC2, RC3, RC4, RC5) ->
%                     {RV1, RV2, RV3, RV4} = RValues,
% 
%                     MovedK = RK1,
%                     MovedV = RV1,
%                     MovedC = RC1,
% 
%                     Rebalanced = ?INTERNAL2(
%                         RemainingK,
%                         MovedK,
%                         [RemainingV | MovedV],
%                         RemainingC1,
%                         RemainingC2,
%                         MovedC
%                     ),
% 
%                     UpdatedSibling = ?INTERNAL3(
%                         RK2,
%                         RK3,
%                         RK4,
%                         {RV2, RV3, RV4},
%                         RC2,
%                         RC3,
%                         RC4,
%                         RC5
%                     ),
%                     [Rebalanced | UpdatedSibling];
%                 %
%                 %
%                 ?INTERNAL3(RK1, RK2, RK3, RValues, RC1, RC2, RC3, RC4) ->
%                     {RV1, RV2, RV3} = RValues,
% 
%                     MovedK = RK1,
%                     MovedV = RV1,
%                     MovedC = RC1,
% 
%                     Rebalanced = ?INTERNAL2(
%                         RemainingK,
%                         MovedK,
%                         [RemainingV | MovedV],
%                         RemainingC1,
%                         RemainingC2,
%                         MovedC
%                     ),
% 
%                     UpdatedSibling = ?INTERNAL2(
%                         RK2,
%                         RK3,
%                         [RV2 | RV3],
%                         RC2,
%                         RC3,
%                         RC4
%                     ),
%                     [Rebalanced | UpdatedSibling];
%                 %
%                 %
%                 _ ->
%                     [LV1 | LV2] = LValues,
% 
%                     % Merge with left (we already unpacked it)
%                     ?INTERNAL4(
%                         LK1,
%                         LK2,
%                         LParentK,
%                         RemainingK,
%                         {LV1, LV2, LParentV, RemainingV},
%                         LC1,
%                         LC2,
%                         LC3,
%                         RemainingC1,
%                         RemainingC2
%                     )
%             end
%     end.
% 
% %%%%%%%%%%%%%%%%%%%%
% 
% %%%%
% 
% rebalance_leaf2(nil, _Pos, RemainingK, RemainingV) ->
%     % LEAF2 becomes LEAF1 at the root
%     ?LEAF1(RemainingK, RemainingV);
% rebalance_leaf2(Parent, Pos, RemainingK, RemainingV) ->
%     case Pos of
%         1 ->
%             [rebalanced | rebalance_leaf2_pos1(Parent, RemainingK, RemainingV)];
%         2 ->
%             [rebalanced | rebalance_leaf2_pos2(Parent, RemainingK, RemainingV)];
%         3 ->
%             [rebalanced | rebalance_leaf2_pos3(Parent, RemainingK, RemainingV)];
%         4 ->
%             [rebalanced | rebalance_leaf2_pos4(Parent, RemainingK, RemainingV)];
%         5 ->
%             [rebalanced | rebalance_leaf2_pos5(Parent, RemainingK, RemainingV)]
%     end.
% 
% rebalance_leaf2_pos1(Parent, RemainingK, RemainingV) ->
%     case Parent of
%         ?INTERNAL2(ParentK1, _, ParentValues, _, ParentC2, _) ->
%             %
%             ParentK = ParentK1,
%             ParentV = hd(ParentValues),
%             RSibling = ParentC2,
%             rebalance_leaf2_from_right_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 ParentK,
%                 ParentV,
%                 RSibling
%             );
%         %
%         ?INTERNAL3(ParentK1, _, _, ParentValues, _, ParentC2, _, _) ->
%             %
%             ParentK = ParentK1,
%             ParentV = element(1, ParentValues),
%             RSibling = ParentC2,
%             rebalance_leaf2_from_right_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 ParentK,
%                 ParentV,
%                 RSibling
%             );
%         %
%         ?INTERNAL4(ParentK1, _, _, _, ParentValues, _, ParentC2, _, _, _) ->
%             %
%             ParentK = ParentK1,
%             ParentV = element(1, ParentValues),
%             RSibling = ParentC2,
%             rebalance_leaf2_from_right_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 ParentK,
%                 ParentV,
%                 RSibling
%             );
%         %
%         ?INTERNAL1(ParentK1, ParentV1, _, ParentC2) ->
%             ParentK = ParentK1,
%             ParentV = ParentV1,
%             RSibling = ParentC2,
%             rebalance_leaf2_from_right_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 ParentK,
%                 ParentV,
%                 RSibling
%             )
%     end.
% 
% rebalance_leaf2_pos2(Parent, RemainingK, RemainingV) ->
%     case Parent of
%         ?INTERNAL2(ParentK1, _, ParentValues, ParentC1, _, ParentC3) ->
%             %
%             LParentK = ParentK1,
%             LParentV = hd(ParentValues),
%             LSibling = ParentC1,
%             RSibling = ParentC3,
%             %
%             rebalance_leaf2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             );
%         %
%         ?INTERNAL3(ParentK1, _, _, ParentValues, ParentC1, _, ParentC3, _) ->
%             %
%             LParentK = ParentK1,
%             LParentV = element(1, ParentValues),
%             LSibling = ParentC1,
%             RSibling = ParentC3,
%             %
%             rebalance_leaf2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             );
%         %
%         ?INTERNAL4(ParentK1, _, _, _, ParentValues, ParentC1, _, ParentC3, _, _) ->
%             %
%             LParentK = ParentK1,
%             LParentV = element(1, ParentValues),
%             LSibling = ParentC1,
%             RSibling = ParentC3,
%             %
%             rebalance_leaf2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             );
%         %
%         ?INTERNAL1(ParentK1, ParentV1, ParentC1, _) ->
%             ParentK = ParentK1,
%             ParentV = ParentV1,
%             LSibling = ParentC1,
%             rebalance_leaf2_from_left_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 ParentK,
%                 ParentV,
%                 LSibling
%             )
%     end.
% 
% rebalance_leaf2_pos3(Parent, RemainingK, RemainingV) ->
%     case Parent of
%         ?INTERNAL2(_, ParentK2, ParentValues, _, ParentC2, _) ->
%             %
%             LParentK = ParentK2,
%             LParentV = tl(ParentValues),
%             LSibling = ParentC2,
%             %
%             rebalance_leaf2_from_left_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 LParentK,
%                 LParentV,
%                 LSibling
%             );
%         %
%         ?INTERNAL3(_, ParentK2, _, ParentValues, _, ParentC2, _, ParentC4) ->
%             %
%             LParentK = ParentK2,
%             LParentV = element(2, ParentValues),
%             LSibling = ParentC2,
%             RSibling = ParentC4,
%             %
%             rebalance_leaf2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             );
%         %
%         ?INTERNAL4(_, ParentK2, _, _, ParentValues, _, ParentC2, _, ParentC4, _) ->
%             %
%             LParentK = ParentK2,
%             LParentV = element(2, ParentValues),
%             LSibling = ParentC2,
%             RSibling = ParentC4,
%             %
%             rebalance_leaf2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             )
%     end.
% 
% rebalance_leaf2_pos4(Parent, RemainingK, RemainingV) ->
%     case Parent of
%         ?INTERNAL3(_, _, ParentK3, ParentValues, _, _, ParentC3, _) ->
%             %
%             LParentK = ParentK3,
%             LParentV = element(3, ParentValues),
%             LSibling = ParentC3,
%             %
%             rebalance_leaf2_from_left_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 LParentK,
%                 LParentV,
%                 LSibling
%             );
%         %
%         ?INTERNAL4(_, _, ParentK3, _, ParentValues, _, _, ParentC3, _, ParentC5) ->
%             %
%             LParentK = ParentK3,
%             LParentV = element(3, ParentValues),
%             LSibling = ParentC3,
%             RSibling = ParentC5,
%             %
%             rebalance_leaf2_from_either_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 LParentK,
%                 LParentV,
%                 LSibling,
%                 RSibling
%             )
%     end.
% 
% rebalance_leaf2_pos5(Parent, RemainingK, RemainingV) ->
%     case Parent of
%         ?INTERNAL4(_, _, _, ParentK4, ParentValues, _, _, _, ParentC4, _) ->
%             %
%             LParentK = ParentK4,
%             LParentV = element(4, ParentValues),
%             LSibling = ParentC4,
%             %
%             rebalance_leaf2_from_left_sibling(
%                 RemainingK,
%                 RemainingV,
% 
%                 LParentK,
%                 LParentV,
%                 LSibling
%             )
%     end.
% 
% %%%%%%%%%%%%%%%%%%%%
% 
% rebalance_leaf2_from_right_sibling(
%     RemainingK, RemainingV, ParentK, ParentV, RSibling
% ) ->
%     case RSibling of
%         ?LEAF4(RK1, RK2, RK3, RK4, RV1, RV2, RV3, RV4) ->
%             MovedK = RK1,
%             MovedV = RV1,
% 
%             Rebalanced = ?LEAF2(RemainingK, MovedK, RemainingV, MovedV),
%             UpdatedSibling = ?LEAF3(RK2, RK3, RK4, RV2, RV3, RV4),
%             [Rebalanced | UpdatedSibling];
%         %
%         %
%         ?LEAF3(RK1, RK2, RK3, RV1, RV2, RV3) ->
%             MovedK = RK1,
%             MovedV = RV1,
% 
%             Rebalanced = ?LEAF2(RemainingK, MovedK, RemainingV, MovedV),
%             UpdatedSibling = ?LEAF2(RK2, RK3, RV2, RV3),
%             [Rebalanced | UpdatedSibling];
%         %
%         ?LEAF2(RK1, RK2, RV1, RV2) ->
%             % Merge
%             ?LEAF4(
%                 RemainingK,
%                 ParentK,
%                 RK1,
%                 RK2,
%                 RemainingV,
%                 ParentV,
%                 RV1,
%                 RV2
%             )
%     end.
% 
% rebalance_leaf2_from_left_sibling(
%     RemainingK, RemainingV, ParentK, ParentV, LSibling
% ) ->
%     case LSibling of
%         ?LEAF4(LK1, LK2, LK3, LK4, LV1, LV2, LV3, LV4) ->
%             MovedK = LK4,
%             MovedV = LV4,
% 
%             Rebalanced = ?LEAF2(MovedK, RemainingK, MovedV, RemainingV),
%             UpdatedSibling = ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3),
%             [UpdatedSibling | Rebalanced];
%         %
%         ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3) ->
%             MovedK = LK3,
%             MovedV = LV3,
% 
%             Rebalanced = ?LEAF2(MovedK, RemainingK, MovedV, RemainingV),
%             UpdatedSibling = ?LEAF2(LK1, LK2, LV1, LV2),
%             [UpdatedSibling | Rebalanced];
%         %
%         ?LEAF2(LK1, LK2, LV1, LV2) ->
%             % Merge
%             ?LEAF4(
%                 LK1,
%                 LK2,
%                 ParentK,
%                 RemainingK,
%                 LV1,
%                 LV2,
%                 ParentV,
%                 RemainingV
%             )
%     end.
% 
% rebalance_leaf2_from_either_sibling(
%     RemainingK,
%     RemainingV,
% 
%     LParentK,
%     LParentV,
%     LSibling,
%     RSibling
% ) ->
%     case LSibling of
%         ?LEAF4(LK1, LK2, LK3, LK4, LV1, LV2, LV3, LV4) ->
%             MovedK = LK4,
%             MovedV = LV4,
% 
%             Rebalanced = ?LEAF2(MovedK, RemainingK, MovedV, RemainingV),
%             UpdatedSibling = ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3),
%             [from_left, UpdatedSibling | Rebalanced];
%         %
%         ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3) ->
%             MovedK = LK3,
%             MovedV = LV3,
% 
%             Rebalanced = ?LEAF2(MovedK, RemainingK, MovedV, RemainingV),
%             UpdatedSibling = ?LEAF2(LK1, LK2, LV1, LV2),
%             [from_left, UpdatedSibling | Rebalanced];
%         %
%         ?LEAF2(LK1, LK2, LV1, LV2) ->
%             case RSibling of
%                 ?LEAF4(RK1, RK2, RK3, RK4, RV1, RV2, RV3, RV4) ->
%                     MovedK = RK1,
%                     MovedV = RV1,
% 
%                     Rebalanced = ?LEAF2(RemainingK, MovedK, RemainingV, MovedV),
%                     UpdatedSibling = ?LEAF3(RK2, RK3, RK4, RV2, RV3, RV4),
%                     [Rebalanced | UpdatedSibling];
%                 %
%                 %
%                 ?LEAF3(RK1, RK2, RK3, RV1, RV2, RV3) ->
%                     MovedK = RK1,
%                     MovedV = RV1,
% 
%                     Rebalanced = ?LEAF2(RemainingK, MovedK, RemainingV, MovedV),
%                     UpdatedSibling = ?LEAF2(RK2, RK3, RV2, RV3),
%                     [Rebalanced | UpdatedSibling];
%                 %
%                 _ ->
%                     % Merge with left (we already unpacked it)
%                     ?LEAF4(
%                         LK1,
%                         LK2,
%                         LParentK,
%                         RemainingK,
%                         LV1,
%                         LV2,
%                         LParentV,
%                         RemainingV
%                     )
%             end
%     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - Largest From Root
%% ------------------------------------------------------------------

root_take_largest(Root) ->
    case Root of
        ?INTERNAL1(K1, V1, C1, C2) ->
            root_take_largest_internal1(K1, V1, C1, C2);
        %
        ?LEAF1(K1, V1) ->
            TakenPair = [K1 | V1],
            ?TAKEN(TakenPair, ?LEAF0);
        %
        ?LEAF0 ->
            error_empty_tree();
        %
        Node ->
            take_largest_recur(Node, ?DLARGEST(root_level, none, none))
    end.

take_largest_recur(Node, ?DLARGEST) ->
    case Node of
        ?INTERNAL2(K1, K2, Values, C1, C2, C3) ->
            take_largest_internal2(K1, K2, Values, C1, C2, C3, ?DLARGEST);
        %
        %
        ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4) ->
            take_largest_internal3(K1, K2, K3, Values, C1, C2, C3, C4);
        %
        %
        ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
            take_largest_internal4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
        %
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            TakenPair = [K4 | V4],
            UpdatedNode = ?LEAF3(K1, K2, K3, V1, V2, V3),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            TakenPair = [K3 | V3],
            UpdatedNode = ?LEAF2(K1, K2, V1, V2),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        %
        ?LEAF2(K1, K2, V1, V2) ->
            take_largest_leaf2(K1, K2, V1, V2, ?DLARGEST)
    end.

root_take_largest_internal1(K1, V1, C1, C2) ->
    Result = take_largest_recur(C2, ?DLARGEST(C1, K1, V1)),
    rebalance_internal1_child2(K1, V1, C1, Result).

take_largest_internal2(K1, K2, Values, C1, C2, C3, ?DLARGEST(Left, LParentK, LParentV)) ->
    V2 = tl(Values),
    Result = take_largest_recur(C3, ?DLARGEST(C2, K2, V2)),
    rebalance_internal2_child3(K1, K2, Values, C1, C2, Result, ?DRIGHTMOST(Left, LParentK, LParentV)).

take_largest_internal3(K1, K2, K3, Values, C1, C2, C3, C4) ->
    V3 = element(3, Values),
    Result = take_largest_recur(C4, ?DLARGEST(C3, K3, V3)),
    rebalance_internal3_child4(K1, K2, K3, Values, C1, C2, C3, Result).

take_largest_internal4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    V4 = element(4, Values),
    Result = take_largest_recur(C5, ?DLARGEST(C4, K4, V4)),
    rebalance_internal4_child5(K1, K2, K3, K4, Values, C1, C2, C3, C4, Result).

take_largest_leaf2(K1, K2, V1, V2, ?DLARGEST(Left, LParentK, LParentV)) ->
    TakenPair = [K2 | V2],
    rebalance_leaf2(TakenPair, ?DRIGHTMOST(Left, LParentK, LParentV), K1, V1).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - Smallest
%% ------------------------------------------------------------------

root_take_smallest(Root) ->
    case Root of
        ?INTERNAL1(K1, V1, C1, C2) ->
            root_take_smallest_internal1(K1, V1, C1, C2);
        %
        ?LEAF1(K1, V1) ->
            TakenPair = [K1 | V1],
            ?TAKEN(TakenPair, ?LEAF0);
        %
        ?LEAF0 ->
            error_empty_tree();
        %
        Node ->
            take_smallest_recur(Node, ?DSMALLEST(root_level, none, none))
    end.

take_smallest_recur(Node, ?DSMALLEST) ->
    case Node of
        ?INTERNAL2(K1, K2, Values, LC1, LC2, LC3) ->
            take_smallest_internal2(K1, K2, Values, LC1, LC2, LC3, ?DSMALLEST);
        %
        %
        ?INTERNAL3(K1, K2, K3, Values, LC1, LC2, LC3, LC4) ->
            take_smallest_internal3(K1, K2, K3, Values, LC1, LC2, LC3, LC4);
        %
        %
        ?INTERNAL4(K1, K2, K3, K4, Values, LC1, LC2, LC3, LC4, LC5) ->
            take_smallest_internal4(K1, K2, K3, K4, Values, LC1, LC2, LC3, LC4, LC5);
        %
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            TakenPair = [K1 | V1],
            UpdatedLeft = ?LEAF3(K2, K3, K4, V2, V3, V4),
            ?TAKEN(TakenPair, UpdatedLeft);
        %
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            TakenPair = [K1 | V1],
            UpdatedLeft = ?LEAF2(K2, K3, V2, V3),
            ?TAKEN(TakenPair, UpdatedLeft);
        %
        %
        ?LEAF2(K1, K2, V1, V2) ->
            take_smallest_leaf2(K1, K2, V1, V2, ?DSMALLEST)
    end.

root_take_smallest_internal1(K1, V1, C1, C2) ->
    Result = take_smallest_recur(C1, ?DSMALLEST(C2, K1, V1)),
    rebalance_internal1_child1(K1, V1, Result, C2).

take_smallest_internal2(K1, K2, Values, C1, C2, C3, ?DSMALLEST(Right, ParentK, ParentV)) ->
    V1 = hd(Values),
    Result = take_smallest_recur(C1, ?DSMALLEST(C2, K1, V1)),
    rebalance_internal2_child1(K1, K2, Values, Result, C2, C3,
                               ?DLEFTMOST(Right, ParentK, ParentV)).

take_smallest_internal3(K1, K2, K3, Values, C1, C2, C3, C4) ->
    V1 = element(1, Values),
    Result = take_smallest_recur(C1, ?DSMALLEST(C2, K1, V1)),
    rebalance_internal3_child1(K1, K2, K3, Values, Result, C2, C3, C4).

take_smallest_internal4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    V1 = element(1, Values),
    Result = take_smallest_recur(C1, ?DSMALLEST(C2, K1, V1)),
    rebalance_internal4_child1(K1, K2, K3, K4, Values, Result, C2, C3, C4, C5).

take_smallest_leaf2(K1, K2, V1, V2, ?DSMALLEST(Right, ParentK, ParentV)) ->
    TakenPair = [K1 | V1],
    rebalance_leaf2(TakenPair, ?DLEFTMOST(Right, ParentK, ParentV), K2, V2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - General
%% ------------------------------------------------------------------

root_take_key(Key, Root) ->
    case Root of
        ?INTERNAL1(K1, V1, C1, C2) ->
            root_take_key_internal1(Key, K1, V1, C1, C2);
        %
        ?LEAF1(K1, V1) ->
            root_take_key_leaf1(Key, K1, V1);
        %
        ?LEAF0 ->
            error_badkey(Key);
        %
        Node ->
            take_key_recur(Key, Node, ?DHELPER(root_level, none, none, root_level, none, none))
    end.

take_key_recur(Key, Node, ?DHELPER) ->
    case Node of
        ?INTERNAL2(K1, K2, Values, C1, C2, C3) ->
            take_key_internal2(Key, K1, K2, Values, C1, C2, C3, ?DHELPER);
        %
        %
        ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, C4) ->
            take_key_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4);
        %
        %
        ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
            take_key_internal4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
        %
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            take_key_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4);
        %
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            take_key_leaf3(Key, K1, K2, K3, V1, V2, V3);
        %
        %
        ?LEAF2(K1, K2, V1, V2) ->
            take_key_leaf2(Key, K1, K2, V1, V2, ?DHELPER)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - General - Internal 1
%% ------------------------------------------------------------------

root_take_key_internal1(Key, K1, V1, C1, C2) ->
    if
        Key < K1 ->
            Result = take_key_recur(Key, C1, ?DLEFTMOST(C2, K1, V1)),
            rebalance_internal1_child1(K1, V1, Result, C2);
        %
        Key > K1 ->
            Result = take_key_recur(Key, C2, ?DRIGHTMOST(C1, K1, V1)),
            rebalance_internal1_child2(K1, V1, C1, Result);
        %
        true ->
            root_take_key_internal1_key1(K1, V1, C1, C2)
    end.

root_take_key_internal1_key1(K1, V1, C1, C2) ->
    TakenPair = [K1 | V1],

    % We take a replacement key from C2 as if it's at root level
    ?TAKEN(ReplacementPair, UpdatedC2) = take_smallest_recur(C2, ?DSMALLEST(root_level, none, none)),

    case UpdatedC2 of
        ?INTERNAL1(RK1, RV1, RC1, RC2) ->
            ?TAKEN(TakenPair, root_take_key_internal1_key1_rebalance_internal(ReplacementPair, C1, RK1, RV1, RC1, RC2));
        %
        ?LEAF1(RK1, RV1) ->
            ?TAKEN(TakenPair, root_take_key_internal1_key1_rebalance_leaf(ReplacementPair, C1, RK1, RV1));
        %
        _ ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedRoot = ?INTERNAL1(ReplacementK, ReplacementV, C1, UpdatedC2),
            ?TAKEN(TakenPair, UpdatedRoot)
    end.

root_take_key_internal1_key1_rebalance_internal(ReplacementPair, C1, RK1, RV1, RC1, RC2) ->
    [ReplacementK | ReplacementV] = ReplacementPair,


    case C1 of
        ?INTERNAL2(LK1, LK2, LValues, LC1, LC2, LC3) ->
            [LV1 | LV2] = LValues,
            % Merge node, reducing tree height
            ?INTERNAL4(
                LK1, LK2, ReplacementK, RK1,
                {LV1, LV2, ReplacementV, RV1},
                LC1, LC2, LC3, RC1, RC2
            );
        %
        %
        ?INTERNAL3(LK1, LK2, LK3, LValues, LC1, LC2, LC3, LC4) ->
            {LV1, LV2, LV3} = LValues,

            MovedUpK = LK3,
            MovedUpV = LV3,
            MovedC = LC4,

            % Move key and child
            UpdatedC1 = ?INTERNAL2(
                LK1, LK2,
                [LV1 | LV2],
                LC1, LC2, LC3
            ),

            UpdatedC2 = ?INTERNAL2(
                ReplacementK, RK1,
                [ReplacementV | RV1],
                MovedC, RC1, RC2
            ),

            ?INTERNAL1(MovedUpK, MovedUpV, UpdatedC1, UpdatedC2);
        %
        %
        ?INTERNAL4(LK1, LK2, LK3, LK4, LValues, LC1, LC2, LC3, LC4, LC5) ->
            {LV1, LV2, LV3, LV4} = LValues,

            MovedUpK = LK4,
            MovedUpV = LV4,
            MovedC = LC5,

            % Move key and child
            UpdatedC1 = ?INTERNAL3(
                LK1, LK2, LK3,
                {LV1, LV2, LV3},
                LC1, LC2, LC3, LC4
            ),

            UpdatedC2 = ?INTERNAL2(
                ReplacementK, RK1,
                [ReplacementV | RV1],
                MovedC, RC1, RC2
            ),

            ?INTERNAL1(MovedUpK, MovedUpV, UpdatedC1, UpdatedC2)
    end.

root_take_key_internal1_key1_rebalance_leaf(ReplacementPair, C1, RK1, RV1) ->
    [ReplacementK | ReplacementV] = ReplacementPair,


    case C1 of
        ?LEAF2(LK1, LK2, LV1, LV2) ->
            % Merge node, reducing tree height
            ?LEAF4(LK1, LK2, ReplacementK, RK1,
                   LV1, LV2, ReplacementV, RV1);
        %
        %
        ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3) ->
            MovedUpK = LK3,
            MovedUpV = LV3,

            % Move key
            UpdatedC1 = ?LEAF2(LK1, LK2, LV1, LV2),
            UpdatedC2 = ?LEAF2(ReplacementK, RK1, ReplacementV, RV1),

            ?INTERNAL1(MovedUpK, MovedUpV, UpdatedC1, UpdatedC2);
        %
        %
        ?LEAF4(LK1, LK2, LK3, LK4, LV1, LV2, LV3, LV4) ->
            MovedUpK = LK4,
            MovedUpV = LV4,

            % Move key
            UpdatedC1 = ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3),
            UpdatedC2 = ?LEAF2(ReplacementK, RK1, ReplacementV, RV1),

            ?INTERNAL1(MovedUpK, MovedUpV, UpdatedC1, UpdatedC2)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - General - Internal 2
%% ------------------------------------------------------------------

take_key_internal2(Key, K1, K2, Values, C1, C2, C3, ?DHELPER) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    take_key_internal2_child2(Key, K1, K2, Values, C1, C2, C3, ?DHELPER);
                Key > K2 ->
                    take_key_internal2_child3(Key, K1, K2, Values, C1, C2, C3, ?DHELPER);
                true ->
                    take_key_internal2_key2(K1, K2, Values, C1, C2, C3, ?DHELPER)
            end;
        %
        Key < K1 ->
            take_key_internal2_child1(Key, K1, K2, Values, C1, C2, C3, ?DHELPER);
        %
        true ->
            take_key_internal2_key1(K1, K2, Values, C1, C2, C3, ?DHELPER)
    end.

%%%

take_key_internal2_child1(Key, K1, K2, Values, C1, C2, C3, ?DHELPER) ->
    V1 = hd(Values),
    Result = take_key_recur(Key, C1, ?DLEFTMOST(C2, K1, V1)),
    rebalance_internal2_child1(K1, K2, Values, Result, C2, C3, ?DHELPER).

take_key_internal2_child2(Key, K1, K2, Values, C1, C2, C3, ?DHELPER) ->
    [V1 | V2] = Values,
    Result = take_key_recur(Key, C2, ?DHELPER(C1, K1, V1, C3, K2, V2)),
    rebalance_internal2_child2(K1, K2, Values, C1, Result, C3, ?DHELPER).

take_key_internal2_child3(Key, K1, K2, Values, C1, C2, C3, ?DHELPER) ->
    V2 = tl(Values),
    Result = take_key_recur(Key, C3, ?DRIGHTMOST(C2, K2, V2)),
    rebalance_internal2_child3(K1, K2, Values, C1, C2, Result, ?DHELPER).

%%%

take_key_internal2_key1(K1, K2, Values, C1, C2, C3, ?DHELPER) ->
    [V1 | V2] = Values,
    TakenPair = [K1 | V1],

    case take_smallest_recur(C2, ?DSMALLEST(C3, K2, V2)) of
        ?TAKEN(ReplacementPair, UpdatedC2) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL2(ReplacementK, K2, [ReplacementV | V2], C1, UpdatedC2, C3),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, ReplacementPair, UpdatedC2, UpdatedC3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            [MovedK | MovedV] = MovedUp,
            UpdatedNode = ?INTERNAL2(ReplacementK, MovedK, [ReplacementV | MovedV], C1, UpdatedC2, UpdatedC3),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(ReplacementPair, MergededC2C3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            RemainingK = ReplacementK,
            RemainingV = ReplacementV,
            rebalance_internal2_merged(TakenPair,
                                       ?DHELPER,
                                       RemainingK, RemainingV,
                                       C1, MergededC2C3)
    end.

take_key_internal2_key2(K1, K2, Values, C1, C2, C3, ?DHELPER) ->
    [V1 | V2] = Values,
    TakenPair = [K2 | V2],

    case take_largest_recur(C2, ?DSMALLEST(C1, K1, V1)) of
        ?TAKEN(ReplacementPair, UpdatedC2) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL2(K1, ReplacementK, [V1 | ReplacementV], C1, UpdatedC2, C3),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, ReplacementPair, UpdatedC1, UpdatedC2) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            [MovedK | MovedV] = MovedUp,
            UpdatedNode = ?INTERNAL2(MovedK, ReplacementK, [MovedV | ReplacementV], UpdatedC1, UpdatedC2, C3),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(ReplacementPair, MergededC1C2) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            RemainingK = ReplacementK,
            RemainingV = ReplacementV,
            rebalance_internal2_merged(TakenPair,
                                       ?DHELPER,
                                       RemainingK, RemainingV,
                                       MergededC1C2, C3)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - General - Internal 3
%% ------------------------------------------------------------------

take_key_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    take_key_internal3_child1(Key, K1, K2, K3, Values, C1, C2, C3, C4);
                Key > K1 ->
                    take_key_internal3_child2(Key, K1, K2, K3, Values, C1, C2, C3, C4);
                true ->
                    take_key_internal3_key1(K1, K2, K3, Values, C1, C2, C3, C4)
            end;
        %
        Key > K2 ->
            if
                Key < K3 ->
                    take_key_internal3_child3(Key, K1, K2, K3, Values, C1, C2, C3, C4);
                Key > K3 ->
                    take_key_internal3_child4(Key, K1, K2, K3, Values, C1, C2, C3, C4);
                true ->
                    take_key_internal3_key3(K1, K2, K3, Values, C1, C2, C3, C4)
            end;
        %
        true ->
            take_key_internal3_key2(K1, K2, K3, Values, C1, C2, C3, C4)
    end.

%%%

take_key_internal3_child1(Key, K1, K2, K3, Values, C1, C2, C3, C4) ->
    V1 = element(1, Values),
    Result = take_key_recur(Key, C1, ?DLEFTMOST(C2, K1, V1)),
    rebalance_internal3_child1(K1, K2, K3, Values, Result, C2, C3, C4).

take_key_internal3_child2(Key, K1, K2, K3, Values, C1, C2, C3, C4) ->
    {V1, V2, _} = Values,
    Result = take_key_recur(Key, C2, ?DHELPER(C1, K1, V1, C3, K2, V2)),
    rebalance_internal3_child2(K1, K2, K3, Values, C1, Result, C3, C4).

take_key_internal3_child3(Key, K1, K2, K3, Values, C1, C2, C3, C4) ->
    {_, V2, V3} = Values,
    Result = take_key_recur(Key, C3, ?DHELPER(C2, K2, V2, C4, K3, V3)),
    rebalance_internal3_child3(K1, K2, K3, Values, C1, C2, Result, C4).

take_key_internal3_child4(Key, K1, K2, K3, Values, C1, C2, C3, C4) ->
    V3 = element(3, Values),
    Result = take_key_recur(Key, C4, ?DRIGHTMOST(C3, K3, V3)),
    rebalance_internal3_child4(K1, K2, K3, Values, C1, C2, C3, Result).

%    V3 = element(3, Values),
%    DescendentHelper = ?DELETION_HELPER_RIGHTMOST(C3, K3, V3),
%    Result = take_key_recur(C4, DescendentHelper),
%    rebalance_internal3_child4(K1, K2, K3, Values, C1, C2, C3, Result).

%%%

take_key_internal3_key1(K1, K2, K3, Values, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    TakenPair = [K1 | V1],
    
    case take_smallest_recur(C2, ?DSMALLEST(C3, K2, V2)) of
        ?TAKEN(ReplacementPair, UpdatedC2) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL3(ReplacementK, K2, K3, {ReplacementV, V2, V3}, C1, UpdatedC2, C3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, ReplacementPair, UpdatedC2, UpdatedC3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = {ReplacementV, MovedV, V3},
            UpdatedNode = ?INTERNAL3(ReplacementK, MovedK, K3, UpdatedValues, C1, UpdatedC2, UpdatedC3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(ReplacementPair, MergedC2C3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL2(ReplacementK, K3, [ReplacementV | V3], C1, MergedC2C3, C4),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

take_key_internal3_key2(K1, K2, K3, Values, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    TakenPair = [K2 | V2],
    
    case take_smallest_recur(C3, ?DSMALLEST(C4, K3, V3)) of
        ?TAKEN(ReplacementPair, UpdatedC3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL3(K1, ReplacementK, K3, {V1, ReplacementV, V3}, C1, C2, UpdatedC3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, ReplacementPair, UpdatedC3, UpdatedC4) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = {V1, ReplacementV, MovedV},
            UpdatedNode = ?INTERNAL3(K1, ReplacementK, MovedK, UpdatedValues, C1, C2, UpdatedC3, UpdatedC4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(ReplacementPair, MergedC3C4) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL2(ReplacementK, K2, [ReplacementV | V2], C1, C2, MergedC3C4),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

take_key_internal3_key3(K1, K2, K3, Values, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    TakenPair = [K3 | V3],
    
    case take_largest_recur(C3, ?DLARGEST(C2, K2, V2)) of
        ?TAKEN(ReplacementPair, UpdatedC3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL3(K1, ReplacementK, K3, {V1, ReplacementV, V3}, C1, C2, UpdatedC3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, ReplacementPair, UpdatedC2, UpdatedC3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = {V1, MovedV, ReplacementV},
            UpdatedNode = ?INTERNAL3(K1, MovedK, ReplacementK, UpdatedValues, C1, UpdatedC2, UpdatedC3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(ReplacementPair, MergedC2C3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL2(K1, ReplacementK, [V1 | ReplacementV], C1, MergedC2C3, C4),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - General - Internal 4
%% ------------------------------------------------------------------

take_key_internal4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    if
        Key > K2 ->
            if
                Key < K4 ->
                    if
                        Key > K3 ->
                            take_key_internal4_child4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
                        Key < K3 ->
                            take_key_internal4_child3(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
                        true ->
                            take_key_internal4_key3(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)
                    end;
                %
                Key > K4 ->
                    take_key_internal4_child5(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
                %
                true ->
                    take_key_internal4_key4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)
            end;
        %
        Key < K2 ->
            if
                Key < K1 ->
                    take_key_internal4_child1(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
                Key > K1 ->
                    take_key_internal4_child2(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
                true ->
                    take_key_internal4_key1(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)
            end;
        %
        true ->
            take_key_internal4_key2(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5)
    end.

%%%

take_key_internal4_child1(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    V1 = element(1, Values),
    Result = take_key_recur(Key, C1, ?DLEFTMOST(C2, K1, V1)),
    rebalance_internal4_child1(K1, K2, K3, K4, Values, Result, C2, C3, C4, C5).

take_key_internal4_child2(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    {V1, V2, _, _} = Values,
    Result = take_key_recur(Key, C2, ?DHELPER(C1, K1, V1, C3, K2, V2)),
    rebalance_internal4_child2(K1, K2, K3, K4, Values, C1, Result, C3, C4, C5).

take_key_internal4_child3(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    {_, V2, V3, _} = Values,
    Result = take_key_recur(Key, C3, ?DHELPER(C2, K2, V2, C4, K3, V3)),
    rebalance_internal4_child3(K1, K2, K3, K4, Values, C1, C2, Result, C4, C5).

take_key_internal4_child4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    {_, _, V3, V4} = Values,
    Result = take_key_recur(Key, C4, ?DHELPER(C3, K3, V3, C5, K4, V4)),
    rebalance_internal4_child4(K1, K2, K3, K4, Values, C1, C2, C3, Result, C5).

take_key_internal4_child5(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    V4 = element(4, Values),
    Result = take_key_recur(Key, C5, ?DRIGHTMOST(C4, K4, V4)),
    rebalance_internal4_child5(K1, K2, K3, K4, Values, C1, C2, C3, C4, Result).

%%%

take_key_internal4_key1(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    TakenPair = [K1 | V1],
    
    case take_smallest_recur(C2, ?DSMALLEST(C3, K2, V2)) of
        ?TAKEN(ReplacementPair, UpdatedC2) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL4(ReplacementK, K2, K3, K4, {ReplacementV, V2, V3, V4}, C1, UpdatedC2, C3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, ReplacementPair, UpdatedC2, UpdatedC3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = {ReplacementV, MovedV, V3, V4},
            UpdatedNode = ?INTERNAL4(ReplacementK, MovedK, K3, K4, UpdatedValues, C1, UpdatedC2, UpdatedC3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(ReplacementPair, MergedC2C3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL3(ReplacementK, K3, K4, {ReplacementV, V3, V4}, C1, MergedC2C3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

take_key_internal4_key2(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    TakenPair = [K2 | V2],
    
    case take_smallest_recur(C3, ?DSMALLEST(C4, K3, V3)) of
        ?TAKEN(ReplacementPair, UpdatedC3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL4(K1, ReplacementK, K3, K4, {V1, ReplacementV, V3, V4}, C1, C2, UpdatedC3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, ReplacementPair, UpdatedC3, UpdatedC4) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = {V1, ReplacementV, MovedV, V4},
            UpdatedNode = ?INTERNAL4(K1, ReplacementK, MovedK, K4, UpdatedValues, C1, C2, UpdatedC3, UpdatedC4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(ReplacementPair, MergedC3C4) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL3(K1, ReplacementK, K4, {V1, ReplacementV, V4}, C1, C2, MergedC3C4, C5),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

take_key_internal4_key3(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    TakenPair = [K3 | V3],
    
    case take_largest_recur(C3, ?DLARGEST(C2, K2, V2)) of
        ?TAKEN(ReplacementPair, UpdatedC3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL4(K1, K2, ReplacementK, K4, {V1, V2, ReplacementV, V4}, C1, C2, UpdatedC3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, ReplacementPair, UpdatedC2, UpdatedC3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = {V1, MovedV, ReplacementV, V4},
            UpdatedNode = ?INTERNAL4(K1, MovedK, ReplacementK, K4, UpdatedValues, C1, UpdatedC2, UpdatedC3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(ReplacementPair, MergedC2C3) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL3(K1, ReplacementK, K4, {V1, ReplacementV, V4}, C1, MergedC2C3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

take_key_internal4_key4(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    TakenPair = [K4 | V4],
    
    case take_largest_recur(C4, ?DLARGEST(C3, K3, V3)) of
        ?TAKEN(ReplacementPair, UpdatedC4) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL4(K1, K2, K3, ReplacementK, {V1, V2, V3, ReplacementV}, C1, C2, C3, UpdatedC4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, ReplacementPair, UpdatedC3, UpdatedC4) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = {V1, V2, MovedV, ReplacementV},
            UpdatedNode = ?INTERNAL4(K1, K2, MovedK, ReplacementK, UpdatedValues, C1, C2, UpdatedC3, UpdatedC4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(ReplacementPair, MergedC3C4) ->
            [ReplacementK | ReplacementV] = ReplacementPair,
            UpdatedNode = ?INTERNAL3(K1, K2, ReplacementK, {V1, V2, ReplacementV}, C1, C2, MergedC3C4, C5),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - General - Leaves
%% ------------------------------------------------------------------

take_key_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4) ->
    if
        Key < K3 ->
            if
                Key == K1 ->
                    TakenPair = [K1 | V1],
                    ?TAKEN(TakenPair, ?LEAF3(K2, K3, K4, V2, V3, V4));
                Key == K2 ->
                    TakenPair = [K2 | V2],
                    ?TAKEN(TakenPair, ?LEAF3(K1, K3, K4, V1, V3, V4));
                true ->
                    error_badkey(Key)
            end;
        %
        Key == K3 ->
            TakenPair = [K3 | V3],
            ?TAKEN(TakenPair, ?LEAF3(K1, K2, K4, V1, V2, V4));
        %
        Key == K4 ->
            TakenPair = [K4 | V4],
            ?TAKEN(TakenPair, ?LEAF3(K1, K2, K3, V1, V2, V3));
        %
        true ->
            error_badkey(Key)
    end.

take_key_leaf3(Key, K1, K2, K3, V1, V2, V3) ->
    if
        Key < K2 ->
            if
                Key == K1 ->
                    TakenPair = [K1 | V1],
                    ?TAKEN(TakenPair, ?LEAF2(K2, K3, V2, V3));
                true ->
                    error_badkey(Key)
            end;
        Key > K2 ->
            if
                Key == K3 ->
                    TakenPair = [K3 | V3],
                    ?TAKEN(TakenPair, ?LEAF2(K1, K2, V1, V2));
                true ->
                    error_badkey(Key)
            end;
        true ->
            TakenPair = [K2 | V2],
            ?TAKEN(TakenPair, ?LEAF2(K1, K3, V1, V3))
    end.

take_key_leaf2(Key, K1, K2, V1, V2, ?DHELPER) ->
    if
        Key == K1 ->
            TakenPair = [K1 | V1],
            rebalance_leaf2(TakenPair, ?DHELPER, K2, V2);

        Key == K2 -> 
            TakenPair = [K1 | V1],
            rebalance_leaf2(TakenPair, ?DHELPER, K1, V1);

        true ->
            error_badkey(Key)
    end.

root_take_key_leaf1(Key, K1, V1) ->
    if
        Key == K1 ->
            TakenPair = [K1 | V1],
            ?TAKEN(TakenPair, ?LEAF0);
        true ->
            error_badkey(Key)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance Internal 4
%% ------------------------------------------------------------------

rebalance_internal4_child1(K1, K2, K3, K4, Values, Result, C2, C3, C4, C5) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC1) ->
            UpdatedNode = ?INTERNAL4(K1, K2, K3, K4, Values, UpdatedC1, C2, C3, C4, C5) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(1, Values, MovedV),
            UpdatedNode = ?INTERNAL4(MovedK, K2, K3, K4, UpdatedValues, UpdatedC1, UpdatedC2, C3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC1C2) ->
            UpdatedValues = erlang:delete_element(1, Values),
            UpdatedNode = ?INTERNAL3(K2, K3, K4, UpdatedValues, MergedC1C2, C3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

rebalance_internal4_child2(K1, K2, K3, K4, Values, C1, Result, C3, C4, C5) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC2) ->
            UpdatedNode = ?INTERNAL4(K1, K2, K3, K4, Values, C1, UpdatedC2, C3, C4, C5) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(1, Values, MovedV),
            UpdatedNode = ?INTERNAL4(MovedK, K2, K3, K4, UpdatedValues, UpdatedC1, UpdatedC2, C3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC2, UpdatedC3) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(2, Values, MovedV),
            UpdatedNode = ?INTERNAL4(K1, MovedK, K3, K4, UpdatedValues, C1, UpdatedC2, UpdatedC3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC1C2) ->
            UpdatedValues = erlang:delete_element(1, Values),
            UpdatedNode = ?INTERNAL3(K2, K3, K4, UpdatedValues, MergedC1C2, C3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC2C3) ->
            UpdatedValues = erlang:delete_element(2, Values),
            UpdatedNode = ?INTERNAL3(K1, K3, K4, UpdatedValues, C1, MergedC2C3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

rebalance_internal4_child3(K1, K2, K3, K4, Values, C1, C2, Result, C4, C5) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC3) ->
            UpdatedNode = ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, UpdatedC3, C4, C5) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC2, UpdatedC3) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(2, Values, MovedV),
            UpdatedNode = ?INTERNAL4(K1, MovedK, K3, K4, UpdatedValues, C1, UpdatedC2, UpdatedC3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC3, UpdatedC4) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(3, Values, MovedV),
            UpdatedNode = ?INTERNAL4(K1, K2, MovedK, K4, UpdatedValues, C1, C2, UpdatedC3, UpdatedC4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC2C3) ->
            UpdatedValues = erlang:delete_element(2, Values),
            UpdatedNode = ?INTERNAL3(K1, K3, K4, UpdatedValues, C1, MergedC2C3, C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC3C4) ->
            UpdatedValues = erlang:delete_element(3, Values),
            UpdatedNode = ?INTERNAL3(K1, K2, K4, UpdatedValues, C1, C2, MergedC3C4, C5),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

rebalance_internal4_child4(K1, K2, K3, K4, Values, C1, C2, C3, Result, C5) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC4) ->
            UpdatedNode = ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, UpdatedC4, C5) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC3, UpdatedC4) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(3, Values, MovedV),
            UpdatedNode = ?INTERNAL4(K1, K2, MovedK, K4, UpdatedValues, C1, C2, UpdatedC3, UpdatedC4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC4, UpdatedC5) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(4, Values, MovedV),
            UpdatedNode = ?INTERNAL4(K1, K2, K3, MovedK, UpdatedValues, C1, C2, C3, UpdatedC4, UpdatedC5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC3C4) ->
            UpdatedValues = erlang:delete_element(3, Values),
            UpdatedNode = ?INTERNAL3(K1, K2, K4, UpdatedValues, C1, C2, MergedC3C4, C5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC4C5) ->
            UpdatedValues = erlang:delete_element(4, Values),
            UpdatedNode = ?INTERNAL3(K1, K2, K3, UpdatedValues, C1, C2, C3, MergedC4C5),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

rebalance_internal4_child5(K1, K2, K3, K4, Values, C1, C2, C3, C4, Result) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC5) ->
            UpdatedNode = ?INTERNAL4(K1, K2, K3, K4, Values, C1, C2, C3, C4, UpdatedC5) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC4, UpdatedC5) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(4, Values, MovedV),
            UpdatedNode = ?INTERNAL4(K1, K2, K3, MovedK, UpdatedValues, C1, C2, C3, UpdatedC4, UpdatedC5),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC4C5) ->
            UpdatedValues = erlang:delete_element(4, Values),
            UpdatedNode = ?INTERNAL3(K1, K2, K3, UpdatedValues, C1, C2, C3, MergedC4C5),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance Internal 3
%% ------------------------------------------------------------------

rebalance_internal3_child1(K1, K2, K3, Values, Result, C2, C3, C4) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC1) ->
            UpdatedNode = ?INTERNAL3(K1, K2, K3, Values, UpdatedC1, C2, C3, C4) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(1, Values, MovedV),
            UpdatedNode = ?INTERNAL3(MovedK, K2, K3, UpdatedValues, UpdatedC1, UpdatedC2, C3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC1C2) ->
            {_, V2, V3} = Values,
            UpdatedNode = ?INTERNAL2(K2, K3, [V2 | V3], MergedC1C2, C3, C4),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

rebalance_internal3_child2(K1, K2, K3, Values, C1, Result, C3, C4) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC2) ->
            UpdatedNode = ?INTERNAL3(K1, K2, K3, Values, C1, UpdatedC2, C3, C4) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(1, Values, MovedV),
            UpdatedNode = ?INTERNAL3(MovedK, K2, K3, UpdatedValues, UpdatedC1, UpdatedC2, C3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC2, UpdatedC3) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(2, Values, MovedV),
            UpdatedNode = ?INTERNAL3(K1, MovedK, K3, UpdatedValues, C1, UpdatedC2, UpdatedC3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC2C3) ->
            {V1, _, V3} = Values,
            UpdatedNode = ?INTERNAL2(K1, K3, [V1 | V3], C1, MergedC2C3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC1C2) ->
            {_, V2, V3} = Values,
            UpdatedNode = ?INTERNAL2(K2, K3, [V2 | V3], MergedC1C2, C3, C4),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

rebalance_internal3_child3(K1, K2, K3, Values, C1, C2, Result, C4) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC3) ->
            UpdatedNode = ?INTERNAL3(K1, K2, K3, Values, C1, C2, UpdatedC3, C4) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC2, UpdatedC3) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(2, Values, MovedV),
            UpdatedNode = ?INTERNAL3(K1, MovedK, K3, UpdatedValues, C1, UpdatedC2, UpdatedC3, C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC3, UpdatedC4) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(3, Values, MovedV),
            UpdatedNode = ?INTERNAL3(K1, K2, MovedK, UpdatedValues, C1, C2, UpdatedC3, UpdatedC4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC3C4) ->
            {V1, V2, _} = Values,
            UpdatedNode = ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, MergedC3C4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC2C3) ->
            {V1, _, V3} = Values,
            UpdatedNode = ?INTERNAL2(K1, K3, [V1 | V3], C1, MergedC2C3, C4),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

rebalance_internal3_child4(K1, K2, K3, Values, C1, C2, C3, Result) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC4) ->
            UpdatedNode = ?INTERNAL3(K1, K2, K3, Values, C1, C2, C3, UpdatedC4) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC3, UpdatedC4) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = setelement(3, Values, MovedV),
            UpdatedNode = ?INTERNAL3(K1, K2, MovedK, UpdatedValues, C1, C2, UpdatedC3, UpdatedC4),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC3C4) ->
            {V1, V2, _} = Values,
            UpdatedNode = ?INTERNAL2(K1, K2, [V1 | V2], C1, C2, MergedC3C4),
            ?TAKEN(TakenPair, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance Internal 2
%% ------------------------------------------------------------------

rebalance_internal2_child1(K1, K2, Values, Result, C2, C3, ?DHELPER) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC1) ->
            UpdatedNode = ?INTERNAL2(K1, K2, Values, UpdatedC1, C2, C3) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [_ | V2] = Values,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = [MovedV | V2],
            UpdatedNode = ?INTERNAL2(MovedK, K2, UpdatedValues, UpdatedC1, UpdatedC2, C3),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC1C2) ->
            RemainingK = K2,
            [_ | RemainingV] = Values,
            rebalance_internal2_merged(TakenPair,
                                       ?DHELPER,
                                       RemainingK, RemainingV, 
                                       MergedC1C2, C3)
    end.

rebalance_internal2_child2(K1, K2, Values, C1, Result, C3, ?DHELPER) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC2) ->
            UpdatedNode = ?INTERNAL2(K1, K2, Values, C1, UpdatedC2, C3) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [_ | V2] = Values,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = [MovedV | V2],
            UpdatedNode = ?INTERNAL2(MovedK, K2, UpdatedValues, UpdatedC1, UpdatedC2, C3),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC2, UpdatedC3) ->
            [V1 | _] = Values,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = [V1 | MovedV],
            UpdatedNode = ?INTERNAL2(K1, MovedK, UpdatedValues, C1, UpdatedC2, UpdatedC3),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC1C2) ->
            RemainingK = K2,
            [_ | RemainingV] = Values,
            rebalance_internal2_merged(TakenPair,
                                       ?DHELPER,
                                       RemainingK, RemainingV, 
                                       MergedC1C2, C3);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC2C3) ->
            RemainingK = K1,
            [RemainingV | _] = Values,
            rebalance_internal2_merged(TakenPair,
                                       ?DHELPER,
                                       RemainingK, RemainingV, 
                                       C1, MergedC2C3)
    end.

rebalance_internal2_child3(K1, K2, Values, C1, C2, Result, ?DHELPER) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC3) ->
            UpdatedNode = ?INTERNAL2(K1, K2, Values, C1, C2, UpdatedC3) ,
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC2, UpdatedC3) ->
            [V1 | _] = Values,
            [MovedK | MovedV] = MovedUp,
            UpdatedValues = [V1 | MovedV],
            UpdatedNode = ?INTERNAL2(K1, MovedK, UpdatedValues, C1, UpdatedC2, UpdatedC3),
            ?TAKEN(TakenPair, UpdatedNode);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC2C3) ->
            RemainingK = K1,
            [RemainingV | _] = Values,
            rebalance_internal2_merged(TakenPair,
                                       ?DHELPER,
                                       RemainingK, RemainingV, 
                                       C1, MergedC2C3)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance Internal 2 - Merge
%% ------------------------------------------------------------------

rebalance_internal2_merged(TakenPair,
                           ?DHELPER(Left, LParentK, LParentV, Right, RParentK, RParentV),
                           RemainingK, RemainingV, C1, C2) ->
    if
        Left =:= root_level orelse Right =:= root_level ->
            ?TAKEN(TakenPair, ?INTERNAL1(RemainingK, RemainingV, C1, C2));
        %
        Left =/= none ->
            if
                Right =/= none ->
                      rebalance_internal2_merged_mid(
                        TakenPair,
                        Left, LParentK, LParentV,
                        Right, RParentK, RParentV,
                        RemainingK, RemainingV, C1, C2);
                %
                true ->
                      rebalance_internal2_merged_rightmost(
                        TakenPair,
                        Left, LParentK, LParentV,
                        RemainingK, RemainingV, C1, C2)
            end;
        true ->
            rebalance_internal2_merged_leftmost(
              TakenPair,
              Right, RParentK, RParentV,
              RemainingK, RemainingV, C1, C2)
    end.

rebalance_internal2_merged_mid(
  TakenPair,
  Left, LParentK, LParentV,
  Right, RParentK, RParentV,
  RemainingK, RemainingV, C1, C2
 ) ->
    case Right of
        ?INTERNAL4(RK1, RK2, RK3, RK4, RValues, RC1, RC2, RC3, RC4, RC5) ->
            {RV1, RV2, RV3, RV4} = RValues,
            MovedUp = [RK1 | RV1],
            MovedC = RC1,
            UpdatedNode = ?INTERNAL2(RemainingK, RParentK, [RemainingV | RParentV], C1, C2, MovedC),
            UpdatedRight = ?INTERNAL3(RK2, RK3, RK4, {RV2, RV3, RV4}, RC2, RC3, RC4, RC5),
            ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedNode, UpdatedRight);
        %
        %
        ?INTERNAL3(RK1, RK2, RK3, RValues, RC1, RC2, RC3, RC4) ->
            {RV1, RV2, RV3} = RValues,
            MovedUp = [RK1 | RV1],
            MovedC = RC1,
            UpdatedNode = ?INTERNAL2(RemainingK, RParentK, [RemainingV | RParentV], C1, C2, MovedC),
            UpdatedRight = ?INTERNAL2(RK2, RK3, [RV2 | RV3], RC2, RC3, RC4),
            ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedNode, UpdatedRight);
        %
        %
        ?INTERNAL2(RK1, RK2, RValues, RC1, RC2, RC3) ->
            case Left of
                ?INTERNAL4(LK1, LK2, LK3, LK4, LValues, LC1, LC2, LC3, LC4, LC5) ->
                    {LV1, LV2, LV3, LV4} = LValues,
                    MovedUp = [LK4 | LV4],
                    MovedC = LC5,
                    UpdatedLeft = ?INTERNAL3(LK1, LK2, LK3, {LV1, LV2, LV3}, LC1, LC2, LC3, LC4),
                    UpdatedRight = ?INTERNAL2(LParentK, RemainingK, [LParentV | RemainingV], MovedC, C1, C2),
                    ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedLeft, UpdatedRight);
                %
                %
                ?INTERNAL3(LK1, LK2, LK3, LValues, LC1, LC2, LC3, LC4) ->
                    {LV1, LV2, LV3} = LValues,
                    MovedUp = [LK3 | LV3],
                    MovedC = LC4,
                    UpdatedLeft = ?INTERNAL2(LK1, LK2, [LV1 | LV2], LC1, LC2, LC3),
                    UpdatedRight = ?INTERNAL2(LParentK, RemainingK, [LParentV | RemainingV], MovedC, C1, C2),
                    ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedLeft, UpdatedRight);
                %
                %
                _ ->
                    % Merge with right since we already unpacked it
                    [RV1 | RV2] = RValues,
                    MergedNode = ?INTERNAL4(
                        RemainingK, RParentK, RK1, RK2,
                        {RemainingV, RParentV, RV1, RV2},
                        C1, C2, RC1, RC2, RC3
                    ),
                    ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedNode)
            end
    end.

rebalance_internal2_merged_rightmost(TakenPair,
                                     Left, LParentK, LParentV,
                                     RemainingK, RemainingV, C1, C2) ->
    case Left of
        ?INTERNAL4(LK1, LK2, LK3, LK4, LValues, LC1, LC2, LC3, LC4, LC5) ->
            {LV1, LV2, LV3, LV4} = LValues,
            MovedUp = [LK4 | LV4],
            MovedC = LC5,
            UpdatedLeft = ?INTERNAL3(LK1, LK2, LK3, {LV1, LV2, LV3}, LC1, LC2, LC3, LC4),
            UpdatedRight = ?INTERNAL2(LParentK, RemainingK, [LParentV | RemainingV], MovedC, C1, C2),
            ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedLeft, UpdatedRight);
        %
        %
        ?INTERNAL3(LK1, LK2, LK3, LValues, LC1, LC2, LC3, LC4) ->
            {LV1, LV2, LV3} = LValues,
            MovedUp = [LK3 | LV3],
            MovedC = LC4,
            UpdatedLeft = ?INTERNAL2(LK1, LK2, [LV1 | LV2], LC1, LC2, LC3),
            UpdatedRight = ?INTERNAL2(LParentK, RemainingK, [LParentV | RemainingV], MovedC, C1, C2),
            ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedLeft, UpdatedRight);
        %
        %
        ?INTERNAL2(LK1, LK2, LValues, LC1, LC2, LC3) ->
            % Merge with left
            [LV1 | LV2] = LValues,
            MergedNode = ?INTERNAL4(
                            LK1, LK2, LParentK, RemainingK,
                            {LV1, LV2, LParentV, RemainingV},
                            LC1, LC2, LC3, C1, C2
                           ),
            ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedNode)
    end.

rebalance_internal2_merged_leftmost(TakenPair,
                                    Right, RParentK, RParentV,
                                    RemainingK, RemainingV, C1, C2) ->
    case Right of
        ?INTERNAL4(RK1, RK2, RK3, RK4, RValues, RC1, RC2, RC3, RC4, RC5) ->
            {RV1, RV2, RV3, RV4} = RValues,
            MovedUp = [RK1 | RV1],
            MovedC = RC1,
            UpdatedNode = ?INTERNAL2(RemainingK, RParentK, [RemainingV | RParentV], C1, C2, MovedC),
            UpdatedRight = ?INTERNAL3(RK2, RK3, RK4, {RV2, RV3, RV4}, RC2, RC3, RC4, RC5),
            ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedNode, UpdatedRight);
        %
        %
        ?INTERNAL3(RK1, RK2, RK3, RValues, RC1, RC2, RC3, RC4) ->
            {RV1, RV2, RV3} = RValues,
            MovedUp = [RK1 | RV1],
            MovedC = RC1,
            UpdatedNode = ?INTERNAL2(RemainingK, RParentK, [RemainingV | RParentV], C1, C2, MovedC),
            UpdatedRight = ?INTERNAL2(RK2, RK3, [RV2 | RV3], RC2, RC3, RC4),
            ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedNode, UpdatedRight);
        %
        %
        ?INTERNAL2(RK1, RK2, RValues, RC1, RC2, RC3) ->
            % Merge with right 
            [RV1 | RV2] = RValues,
            MergedNode = ?INTERNAL4(
                            RemainingK, RParentK, RK1, RK2,
                            {RemainingV, RParentV, RV1, RV2},
                            C1, C2, RC1, RC2, RC3
                           ),
            ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance Internal 1
%% ------------------------------------------------------------------

rebalance_internal1_child1(K1, V1, Result, C2) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC1) ->
            UpdatedRoot = ?INTERNAL1(K1, V1, UpdatedC1, C2),
            ?TAKEN(TakenPair, UpdatedRoot);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedRoot = ?INTERNAL1(MovedK, MovedV, UpdatedC1, UpdatedC2),
            ?TAKEN(TakenPair, UpdatedRoot);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedRoot = ?INTERNAL1(MovedK, MovedV, UpdatedC1, UpdatedC2),
            ?TAKEN(TakenPair, UpdatedRoot);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC1C2) ->
            % Height reduction
            ?TAKEN(TakenPair, MergedC1C2);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC1C2) ->
            % Height reduction
            ?TAKEN(TakenPair, MergedC1C2)
    end.

rebalance_internal1_child2(K1, V1, C1, Result) ->
    case Result of
        ?TAKEN(TakenPair, UpdatedC2) ->
            UpdatedRoot = ?INTERNAL1(K1, V1, C1, UpdatedC2),
            ?TAKEN(TakenPair, UpdatedRoot);
        %
        ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedRoot = ?INTERNAL1(MovedK, MovedV, UpdatedC1, UpdatedC2),
            ?TAKEN(TakenPair, UpdatedRoot);
        %
        ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedC1, UpdatedC2) ->
            [MovedK | MovedV] = MovedUp,
            UpdatedRoot = ?INTERNAL1(MovedK, MovedV, UpdatedC1, UpdatedC2),
            ?TAKEN(TakenPair, UpdatedRoot);
        %
        ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedC1C2) ->
            % Height reduction
            ?TAKEN(TakenPair, MergedC1C2);
        %
        ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedC1C2) ->
            % Height reduction
            ?TAKEN(TakenPair, MergedC1C2)
    end.

%%%%%%%%%%%%%%

%deletion_left_sibling(Parent, Position) ->
%    case Parent of
%        ?INTERNAL2(K1, K2, Values, C1, C2, C3) ->
%            Left = C2,
%            K = K2,
%            V = tl(Values),
%            {Left, K, V};
%        %
%        ?INTERNAL3(_, _, K3, Values, _, _, C3, _) ->
%            Left = C3,
%            K = K3,
%            V = element(3, Values),
%            {Left, K, V};
%        %
%        ?INTERNAL4(_, _, _, K4, Values, _, _, _, C4, _) ->
%            Left = C4,
%            K = K4,
%            V = element(4, Values),
%            {Left, K, V}
%    end;
%deletion_left_sibling(Parent, leftmost) ->
%    case Parent of
%        ?INTERNAL2(_, K2, Values, _, C2, _) ->
%            Left = C2,
%            K = K2,
%            V = tl(Values),
%            {Left, K, V};
%        %
%        ?INTERNAL3(_, _, K3, Values, _, _, C3, _) ->
%            Left = C3,
%            K = K3,
%            V = element(3, Values),
%            {Left, K, V};
%        %
%        ?INTERNAL4(_, _, _, K4, Values, _, _, _, C4, _) ->
%            Left = C4,
%            K = K4,
%            V = element(4, Values),
%            {Left, K, V}
%    end;
%

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance Leaf 2
%% ------------------------------------------------------------------

rebalance_leaf2(TakenPair, ?DHELPER(Left, LParentK, LParentV, Right, RParentK, RParentV), RemainingK, RemainingV) ->
    if
        Left =:= root_level orelse Right =:= root_level ->
            ?TAKEN(TakenPair, ?LEAF1(RemainingK, RemainingV));
        %
        Left =/= none ->
            if
                Right =/= none ->
                    rebalance_leaf2_from_either_sibling(TakenPair, 
                                                        Left, LParentK, LParentV,
                                                        Right, RParentK, RParentV,
                                                        RemainingK, RemainingV);
                true ->
                    rebalance_leaf2_from_left_sibling(TakenPair, Left, LParentK, LParentV, RemainingK, RemainingV)
            end;
        %
        true ->
            rebalance_leaf2_from_right_sibling(TakenPair, Right, RParentK, RParentV, RemainingK, RemainingV)
    end.

rebalance_leaf2_from_left_sibling(TakenPair, Left, ParentK, ParentV, RemainingK, RemainingV) ->
    case Left of
        ?LEAF4(LK1, LK2, LK3, LK4, LV1, LV2, LV3, LV4) ->
            MovedUp = [LK4 | LV4],
            UpdatedLeft = ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3),
            UpdatedNode = ?LEAF2(ParentK, RemainingK,
                                 ParentV, RemainingV),
            ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedLeft, UpdatedNode);
        %
        ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3) ->
            MovedUp = [LK3 | LV3],
            UpdatedLeft = ?LEAF2(LK1, LK2, LV1, LV2),
            UpdatedNode = ?LEAF2(ParentK, RemainingK,
                                  ParentV, RemainingV),
            ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedLeft, UpdatedNode);
        %
        ?LEAF2(LK1, LK2, LV1, LV2) ->
            MergedNode = ?LEAF4(LK1, LK2, ParentK, RemainingK,
                                LV1, LV2, ParentV, RemainingV),
            ?TAKE_MERGED_FROM_LEFT(TakenPair, MergedNode)
    end.

rebalance_leaf2_from_right_sibling(TakenPair, Right, ParentK, ParentV, RemainingK, RemainingV) ->
    case Right of
        ?LEAF4(RK1, RK2, RK3, RK4, RV1, RV2, RV3, RV4) ->
            MovedUp = [RK1 | RV1],
            UpdatedLeft = ?LEAF2(RemainingK, ParentK, RemainingV, ParentV),
            UpdatedRight = ?LEAF3(RK2, RK3, RK4, RV2, RV3, RV4),
            ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedLeft, UpdatedRight);
        %
        ?LEAF3(RK1, RK2, RK3, RV1, RV2, RV3) ->
            MovedUp = [RK1 | RV1],
            UpdatedLeft = ?LEAF2(RemainingK, ParentK, RemainingV, ParentV),
            UpdatedRight = ?LEAF2(RK2, RK3, RV2, RV3),
            ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedLeft, UpdatedRight);
        %
        ?LEAF2(RK1, RK2, RV1, RV2) ->
            MergedNode = ?LEAF4(
                            RemainingK, ParentK, RK1, RK2,
                            RemainingV, ParentV, RV1, RV2
                           ),
            ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedNode)
    end.

rebalance_leaf2_from_either_sibling(TakenPair, Left, LParentK, LParentV, 
                                    Right, RParentK, RParentV,
                                    RemainingK, RemainingV) ->
    case Right of
        ?LEAF4(RK1, RK2, RK3, RK4, RV1, RV2, RV3, RV4) ->
            MovedUp = [RK1 | RV1],
            UpdatedLeft = ?LEAF2(RemainingK, LParentK, RemainingV, LParentV),
            UpdatedRight = ?LEAF3(RK2, RK3, RK4, RV2, RV3, RV4),
            ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedLeft, UpdatedRight);
        %
        ?LEAF3(RK1, RK2, RK3, RV1, RV2, RV3) ->
            MovedUp = [RK1 | RV1],
            UpdatedLeft = ?LEAF2(RemainingK, LParentK, RemainingV, LParentV),
            UpdatedRight = ?LEAF2(RK2, RK3, RV2, RV3),
            ?TAKE_ROTATED_FROM_RIGHT(MovedUp, TakenPair, UpdatedLeft, UpdatedRight);
        %
        ?LEAF2(RK1, RK2, RV1, RV2) ->
            case Left of
                ?LEAF4(LK1, LK2, LK3, LK4, LV1, LV2, LV3, LV4) ->
                    MovedUp = [LK4 | LV4],
                    UpdatedLeft = ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3),
                    UpdatedNode = ?LEAF2(RParentK, RemainingK,
                                         RParentV, RemainingV),
                    ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedLeft, UpdatedNode);
                %
                ?LEAF3(LK1, LK2, LK3, LV1, LV2, LV3) ->
                    MovedUp = [LK3 | LV3],
                    UpdatedLeft = ?LEAF2(LK1, LK2, LV1, LV2),
                    UpdatedNode = ?LEAF2(RParentK, RemainingK,
                                         RParentV, RemainingV),
                    ?TAKE_ROTATED_FROM_LEFT(MovedUp, TakenPair, UpdatedLeft, UpdatedNode);
                %
                _ ->
                    % Merge with right since we already unpacked it
                    MergedNode = ?LEAF4(
                                    RemainingK, LParentK, RK1, RK2,
                                    RemainingV, LParentV, RV1, RV2
                                   ),
                    ?TAKE_MERGED_FROM_RIGHT(TakenPair, MergedNode)
            end
    end.

