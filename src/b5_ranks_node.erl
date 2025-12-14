% vim: set redrawtime=10000:
% ^ FIXME

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
-module(b5_ranks_node).

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
    nth/2,
    % range/3,
    rank/2,
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
% between some node types, so we need to add tags.
%
% After a few hundred keys, the approx average distribution of node types
% stabilises as follows:
% * INTERNAL2: 13%
% * INTERNAL3: 8%
% * INTERNAL4: 5%
% * LEAF2: 36%
% * LEAF3: 22%
% * LEAF4: 14%

% 8 elements
-define(INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3), {K1, K2, Values, O1, O2, C1, C2, C3}).

% 4 elements
-define(LEAF2(K1, K2, V1, V2), {K1, K2, V1, V2}).

% 11 elements
-define(INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4),
    {K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4}
).

% 6 elements
-define(LEAF3(K1, K2, K3, V1, V2, V3), {K1, K2, K3, V1, V2, V3}).

% 14 elements
-define(INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5),
    {K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5}
).

% 9 elements
-define(LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), {leaf4, K1, K2, K3, K4, V1, V2, V3, V4}).

% 5 elements
-define(INTERNAL1(K1, V1, O1, C1, C2), {K1, V1, O1, C1, C2}).
-define(INTERNAL1, ?INTERNAL1).

% 2 elements
-define(LEAF1(K1, V1), {K1, V1}).

% empty root
-define(LEAF0, leaf0).

%%
%% Regarding `Values' in internal nodes:
%% * INTERNAL4 stores tuples
%% * INTERNAL3 stores tuples
%% * INTERNAL2 stores an improper list
%%

%%%%%%%%

% 7 elements; cannot clash with any node type.
-define(SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR),
    {split, '_', SplitK, SplitV, SplitLOffset, SplitL, SplitR}
).

%%%%%%%%%

% Any of the following cannot clash with either the largest leaf or internal
% nodes, since those are a merged node.

% list
-define(MID_MERGED(MergedNode), [MergedNode]).
-define(MID_MERGED_MATCH(MergedNode), [MergedNode | _]).

% 5 elements
-define(MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight),
    {UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight}
).
% 6 elements
-define(MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight),
    {from_left, UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight}
).

%%%

% 5 elements
-define(ROTATED(UpK, UpVal, MovedSize, UpdatedLeft, UpdatedRight),
    {UpK, UpVal, MovedSize, UpdatedLeft, UpdatedRight}
).

-define(MERGED(MergedNode), MergedNode).

%%%%%%%

% -define(check_node(Node), check_node(?LINE, Node)).
-define(check_node(Node), Node).

% -define(RANGE_ACC(Amount, Pairs), [Amount | Pairs]).

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

-type non_empty_node(Key, Value) ::
    (node_internal1(Key, Value)
    | node_leaf1(Key, Value)
    | deep_node(Key, Value)).

-type node_internal4(Key, Value) ::
    (?INTERNAL4(
        Key,
        Key,
        Key,
        Key,
        {Value, Value, Value, Value},
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
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
        pos_integer(),
        pos_integer(),
        pos_integer(),
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
        pos_integer(),
        pos_integer(),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_internal1(Key, Value) ::
    (?INTERNAL1(
        Key,
        Value,
        pos_integer(),
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

-type node_leaf1(Key, Value) :: ?LEAF1(Key, Value).

%%%%%%%%%%%

-type node_after_deletion(Key, Value) ::
    node_internal3(Key, Value)
    | node_internal2(Key, Value)
    | node_internal1(Key, Value)
    | node_leaf2(Key, Value)
    | node_leaf1(Key, Value).

-type deep_node_after_insertion(Key, Value) ::
    node_internal4(Key, Value)
    | node_internal3(Key, Value)
    | node_leaf4(Key, Value)
    | node_leaf3(Key, Value).

% Temporary situation before rebalance
-type unbalanced_node(Key, Value) :: node_internal1(Key, Value).

%%%%%%%%%%%

-type insertion_value_wrap(Value) :: Value | fun(() -> Value).
-type insertion_value_eval() :: eager | lazy.
-export_type([insertion_value_wrap/1, insertion_value_eval/0]).

-type update_value_wrap(Value, UpdatedValue) :: Value | fun((Value) -> UpdatedValue).
-type update_value_eval() :: eager | lazy.
-export_type([update_value_wrap/2, update_value_eval/0]).

%%%%%%%%%%%

-type split_result(Key, Value) :: internal_split_result(Key, Value) | leaf_split_result(Key, Value).

-type internal_split_result(Key, Value) :: split_result(
    Key, Value, node_internal2(Key, Value), node_internal2(Key, Value)
).

-type leaf_split_result(Key, Value) :: split_result(
    Key, Value, node_leaf2(Key, Value), node_leaf2(Key, Value)
).

-type split_result(Key, Value, SplitL, SplitR) :: ?SPLIT(Key, Value, pos_integer(), SplitL, SplitR).

%%%%%%%%%%%

-opaque iter(Key, Value) :: forward_iter(Key, Value) | reverse_iter(Key, Value).
-export_type([iter/2]).

-record(b5_ranks_v2_forward_iter, {steps}).
-type forward_iter(Key, Value) :: #b5_ranks_v2_forward_iter{steps :: [iterator_step(Key, Value)]}.

-record(b5_ranks_v2_reverse_iter, {steps}).
-type reverse_iter(Key, Value) :: #b5_ranks_v2_reverse_iter{steps :: [iterator_step(Key, Value)]}.

-type iterator_step(Key, Value) :: {Key, Value} | {Key, Value, NextChild :: deep_node(Key, Value)}.

%%%%%%%%%%%

-type take_result(Key, Value) :: nonempty_improper_list(
    kv_pair(Key, Value),
    t(Key, Value)
).
-export_type([take_result/2]).

-type kv_pair(Key, Value) :: nonempty_improper_list(Key, Value).
-export_type([kv_pair/2]).

-type take_result_before_rebalance(Key, Value) :: nonempty_improper_list(
    kv_pair(Key, Value),
    node_after_deletion(Key, Value) | unbalanced_node(Key, Value)
).

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
    ?check_node(Node),
    root_delete(Key, Node).

%% @doc Folds the tree node from left to right (smallest key to largest).
%% Returns the final accumulator value.
-spec foldl(fun((Key, Value, Acc1) -> Acc2), Acc0, t(Key, Value)) -> AccN when
    AccN :: Acc2, Acc2 :: Acc1, Acc1 :: Acc0.
foldl(Fun, Acc, ?INTERNAL1(K1, V1, _, C1, C2)) ->
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
foldr(Fun, Acc, ?INTERNAL1(K1, V1, _, C1, C2)) ->
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
get(Key, ?INTERNAL1(K1, V1, _, C1, C2)) ->
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
insert(Key, ValueEval, ValueWrap, ?INTERNAL1(K1, V1, O1, C1, C2)) ->
    insert_internal1(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2);
insert(Key, ValueEval, ValueWrap, ?LEAF1(K1, V1)) ->
    insert_leaf1(Key, ValueEval, ValueWrap, K1, V1);
insert(Key, ValueEval, ValueWrap, ?LEAF0) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF1(Key, Value);
insert(Key, ValueEval, ValueWrap, Root) ->
    case insert_recur(Key, ValueEval, ValueWrap, Root) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            ?INTERNAL1(SplitK, SplitV, SplitLOffset, SplitL, SplitR);
        %
        UpdatedRoot ->
            UpdatedRoot
    end.

%% @doc Creates an iterator for traversing the tree node entries.
%% Can iterate in `ordered' (ascending) or `reversed' (descending) direction.
-spec iterator(t(Key, Value), ordered | reversed) -> iter(Key, Value).
iterator(Node, ordered) ->
    #b5_ranks_v2_forward_iter{steps = iterator_steps_l(Node)};
iterator(Node, reversed) ->
    #b5_ranks_v2_reverse_iter{steps = iterator_steps_r(Node)}.

%% @doc Creates an iterator starting from the first key >= the specified key.
%% Can iterate in `ordered' (ascending) or `reversed' (descending) direction.
-spec iterator_from(Key, t(Key, Value), ordered | reversed) -> iter(Key, Value).
iterator_from(Key, Node, ordered) ->
    #b5_ranks_v2_forward_iter{steps = iterator_steps_l_from(Key, Node)};
iterator_from(Key, Node, reversed) ->
    #b5_ranks_v2_reverse_iter{steps = iterator_steps_r_from(Key, Node)}.

%% @doc Returns all keys in the tree node as an ordered list.
-spec keys(t(Key, _)) -> [Key].
keys(?INTERNAL1(K1, _, _, C1, C2)) ->
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
larger(Key, ?INTERNAL1(K1, V1, _, C1, C2)) ->
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
largest(?INTERNAL1(_, _, _, _, C2)) ->
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
next(#b5_ranks_v2_forward_iter{steps = Steps} = Iter) ->
    case Steps of
        [{Key, Value} | NextSteps] ->
            UpdatedIter = Iter#b5_ranks_v2_forward_iter{steps = NextSteps},
            {Key, Value, UpdatedIter};
        [{Key, Value, NextChild} | NextNextSteps] ->
            UpdatedIter = Iter#b5_ranks_v2_forward_iter{
                steps = iterator_steps_l_recur(NextChild, NextNextSteps)
            },
            {Key, Value, UpdatedIter};
        [] ->
            none
    end;
next(#b5_ranks_v2_reverse_iter{steps = Steps} = Iter) ->
    case Steps of
        [{Key, Value} | NextSteps] ->
            UpdatedIter = Iter#b5_ranks_v2_reverse_iter{steps = NextSteps},
            {Key, Value, UpdatedIter};
        [{Key, Value, NextChild} | NextNextSteps] ->
            UpdatedIter = Iter#b5_ranks_v2_reverse_iter{
                steps = iterator_steps_r_recur(NextChild, NextNextSteps)
            },
            {Key, Value, UpdatedIter};
        [] ->
            none
    end.

%% TODO document
-spec nth(pos_integer(), t(Key, Value)) -> kv_pair(Key, Value).
nth(N, ?INTERNAL1(K1, V1, O1, C1, C2)) ->
    nth_internal1(N, K1, V1, O1, C1, C2);
nth(N, ?LEAF1(K1, V1)) ->
    nth_leaf1(N, K1, V1);
nth(N, Node) ->
    nth_recur(N, Node).

%% @doc Maps a function over all key-value pairs in the tree node.
%% Returns a new tree node with the same keys and transformed values.
-spec map(fun((Key, Value) -> MappedValue), t(Key, Value)) -> t(Key, MappedValue).
%% erlfmt:ignore A bug in test coverage will show the LEAF1 case wrong
map(Fun, ?INTERNAL1(K1, V1, O1, C1, C2)) ->
    ?INTERNAL1(
        K1,
        Fun(K1, V1),
        O1,
        map_recur(Fun, C1),
        map_recur(Fun, C2)
    );
map(Fun, ?LEAF1(K1, V1)) ->
    ?LEAF1(K1, Fun(K1, V1));
map(_, ?LEAF0) ->
    ?LEAF0;
map(Fun, Node) ->
    map_recur(Fun, Node).

%% TODO document
%-spec nth(pos_integer(), pos_integer(), t(Key, Value)) -> kv_pair(Key, Value).
%range(M, RevLen, ?IMTERMAL1(K1, V1, O1, C1, C2)) ->
%    range_internal1(M, RevLen, K1, V1, O1, C1, C2);
%range(M, _, ?LEAF1(K1, V1)) ->
%    range_leaf1(M, K1, V1);
%range(M, Mode) ->
%    range_recur(M, Mode).

%% TODO document
rank(Key, ?INTERNAL1(K1, V1, O1, C1, C2)) ->
    rank_internal1(Key, 0, K1, V1, O1, C1, C2);
rank(Key, ?LEAF1(K1, V1)) ->
    rank_leaf1(Key, 0, K1, V1);
rank(Key, ?LEAF0) ->
    error_badkey(Key);
rank(Key, Node) ->
    rank_recur(Key, 0, Node).

%% @doc Returns the largest key-value pair where the key is smaller than the given key.
%% Returns `none' if no such key exists.
-spec smaller(Key, t(Key, Value)) -> {Key, Value} | none.
smaller(Key, ?INTERNAL1(K1, V1, _, C1, C2)) ->
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
smallest(?INTERNAL1(_, _, _, C1, _)) ->
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
-spec take(Key, t(Key, Value)) -> take_result(Key, Value).
take(Key, Node) ->
    root_take(Key, Node).

%% @doc Removes and returns the largest key-value pair from the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
%% Returns `{Key, Value, UpdatedNode}'.
-spec take_largest(t(Key, Value)) -> take_result(Key, Value).
take_largest(Node) ->
    root_take_largest(Node).

%% @doc Removes and returns the smallest key-value pair from the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
%% Returns `{Key, Value, UpdatedNode}'.
-spec take_smallest(t(Key, Value)) -> take_result(Key, Value).
take_smallest(Node) ->
    root_take_smallest(Node).

%% @doc Converts the tree node into an ordered list of key-value tuples.
-spec to_list(t(Key, Value)) -> [{Key, Value}].
to_list(?INTERNAL1(K1, V1, _, C1, C2)) ->
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
update(Key, ValueEval, ValueWrap, ?INTERNAL1(K1, V1, O1, C1, C2)) ->
    update_internal1(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2);
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
    } = Stats = stats(Root, ExpectedNrOfKeys),

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
values(?INTERNAL1(_, V1, _, C1, C2)) ->
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
get_recur(Key, ?INTERNAL2(K1, K2, Values, _, _, C1, C2, C3)) ->
    get_internal2(Key, K1, K2, Values, C1, C2, C3);
get_recur(Key, ?INTERNAL3(K1, K2, K3, Values, _, _, _, C1, C2, C3, C4)) ->
    get_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4);
get_recur(Key, ?INTERNAL4(K1, K2, K3, K4, Values, _, _, _, _, C1, C2, C3, C4, C5)) ->
    get_internal4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
get_recur(Key, ?LEAF2(K1, K2, V1, V2)) ->
    get_leaf2(Key, K1, K2, V1, V2);
get_recur(Key, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    get_leaf3(Key, K1, K2, K3, V1, V2, V3);
get_recur(Key, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    get_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4).

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
%% Internal Function Definitions: Nth
%% ------------------------------------------------------------------

-spec nth_recur(N, deep_node(N, Value)) -> Value.
nth_recur(N, ?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3)) ->
    nth_internal2(N, K1, K2, Values, O1, O2, C1, C2, C3);
nth_recur(N, ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)) ->
    nth_internal3(N, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
nth_recur(N, ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)) ->
    nth_internal4(N, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5);
nth_recur(N, ?LEAF2(K1, K2, V1, V2)) ->
    nth_leaf2(N, K1, K2, V1, V2);
nth_recur(N, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    nth_leaf3(N, K1, K2, K3, V1, V2, V3);
nth_recur(N, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    nth_leaf4(N, K1, K2, K3, K4, V1, V2, V3, V4).

-compile({inline, [nth_internal4/15]}).
nth_internal4(N, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    if
        N > O2 ->
            if
                N < O4 ->
                    if
                        N > O3 ->
                            nth_recur(N - O3, C4);
                        N < O3 ->
                            nth_recur(N - O2, C3);
                        true ->
                            [K3 | element(3, Values)]
                    end;
                N > O4 ->
                    nth_recur(N - O4, C5);
                true ->
                    [K4 | element(4, Values)]
            end;
        %
        N < O2 ->
            if
                N > O1 ->
                    nth_recur(N - O1, C2);
                N < O1 ->
                    nth_recur(N, C1);
                true ->
                    [K1 | element(1, Values)]
            end;
        %
        true ->
            [K2 | element(2, Values)]
    end.

-compile({inline, [nth_internal3/12]}).
nth_internal3(N, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    if
        N > O2 ->
            if
                N < O3 ->
                    nth_recur(N - O2, C3);
                N > O3 ->
                    nth_recur(N - O3, C4);
                true ->
                    [K3 | element(3, Values)]
            end;
        %
        N < O2 ->
            if
                N < O1 ->
                    nth_recur(N, C1);
                N > O1 ->
                    nth_recur(N - O1, C2);
                true ->
                    [K1 | element(1, Values)]
            end;
        %
        true ->
            [K2 | element(2, Values)]
    end.

-compile({inline, [nth_internal2/9]}).
nth_internal2(N, K1, K2, Values, O1, O2, C1, C2, C3) ->
    if
        N > O1 ->
            if
                N < O2 ->
                    nth_recur(N - O1, C2);
                N > O2 ->
                    nth_recur(N - O2, C3);
                true ->
                    [K2 | tl(Values)]
            end;
        %
        N < O1 ->
            nth_recur(N, C1);
        %
        true ->
            [K1 | hd(Values)]
    end.

-compile({inline, [nth_internal1/6]}).
nth_internal1(N, K1, V1, O1, C1, C2) ->
    if
        N < O1 ->
            nth_recur(N, C1);
        %
        N > O1 ->
            nth_recur(N - O1, C2);
        %
        true ->
            [K1 | V1]
    end.

-compile({inline, [nth_leaf4/9]}).
nth_leaf4(N, K1, K2, K3, K4, V1, V2, V3, V4) ->
    case N of
        1 -> [K1 | V1];
        2 -> [K2 | V2];
        3 -> [K3 | V3];
        4 -> [K4 | V4]
    end.

-compile({inline, [nth_leaf3/7]}).
nth_leaf3(N, K1, K2, K3, V1, V2, V3) ->
    case N of
        1 -> [K1 | V1];
        2 -> [K2 | V2];
        3 -> [K3 | V3]
    end.

-compile({inline, [nth_leaf2/5]}).
nth_leaf2(N, K1, K2, V1, V2) ->
    case N of
        1 -> [K1 | V1];
        2 -> [K2 | V2]
    end.

-compile({inline, [nth_leaf1/3]}).
nth_leaf1(N, K1, V1) ->
    case N of
        1 -> [K1 | V1]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: range
%% ------------------------------------------------------------------

% TODO continue from here

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rank
%% ------------------------------------------------------------------

-spec rank_recur(Key, non_neg_integer(), deep_node(Key, Value)) -> Value.
rank_recur(Key, LowerBound, ?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3)) ->
    rank_internal2(Key, LowerBound, K1, K2, Values, O1, O2, C1, C2, C3);
rank_recur(Key, LowerBound, ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)) ->
    rank_internal3(Key, LowerBound, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
rank_recur(Key, LowerBound, ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)) ->
    rank_internal4(Key, LowerBound, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5);
rank_recur(Key, LowerBound, ?LEAF2(K1, K2, V1, V2)) ->
    rank_leaf2(Key, LowerBound, K1, K2, V1, V2);
rank_recur(Key, LowerBound, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    rank_leaf3(Key, LowerBound, K1, K2, K3, V1, V2, V3);
rank_recur(Key, LowerBound, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    rank_leaf4(Key, LowerBound, K1, K2, K3, K4, V1, V2, V3, V4).

-compile({inline, [rank_internal4/16]}).
rank_internal4(Key, LowerBound, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    if
        Key > K2 ->
            if
                Key < K4 ->
                    if
                        Key > K3 ->
                            rank_recur(Key, LowerBound + O3, C4);
                        Key < K3 ->
                            rank_recur(Key, LowerBound + O2, C3);
                        true ->
                            [LowerBound + O3 | element(3, Values)]
                    end;
                Key > K4 ->
                    rank_recur(Key, LowerBound + O4, C5);
                true ->
                    [LowerBound + O4 | element(4, Values)]
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    rank_recur(Key, LowerBound + O1, C2);
                Key < K1 ->
                    rank_recur(Key, LowerBound, C1);
                true ->
                    [LowerBound + O1 | element(1, Values)]
            end;
        true ->
            [LowerBound + O2 | element(2, Values)]
    end.

-compile({inline, [rank_internal3/13]}).
rank_internal3(Key, LowerBound, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    if
        Key > K2 ->
            if
                Key < K3 ->
                    rank_recur(Key, LowerBound + O2, C3);
                Key > K3 ->
                    rank_recur(Key, LowerBound + O3, C4);
                true ->
                    [LowerBound + O3 | element(3, Values)]
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    rank_recur(Key, LowerBound + O1 , C2);
                Key < K1 ->
                    rank_recur(Key, LowerBound, C1);
                true ->
                    [LowerBound + O1 | element(1, Values)]
            end;
        true ->
            [LowerBound + O2 | element(2, Values)]
    end.

-compile({inline, [rank_internal2/10]}).
rank_internal2(Key, LowerBound, K1, K2, Values, O1, O2, C1, C2, C3) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    rank_recur(Key, LowerBound + O1 , C2);
                Key > K2 ->
                    rank_recur(Key, LowerBound + O2, C3);
                true ->
                    [LowerBound + O2 | tl(Values)]
            end;
        Key < K1 ->
            rank_recur(Key, LowerBound, C1);
        true ->
            [LowerBound + O1 | hd(Values)]
    end.

-compile({inline, [rank_internal1/7]}).
rank_internal1(Key, LowerBound, K1, V1, O1, C1, C2) ->
    if
        Key < K1 ->
            rank_recur(Key, LowerBound, C1);
        Key > K1 ->
            rank_recur(Key, LowerBound + O1, C2);
        true ->
            [LowerBound + O1 | V1]
    end.

-compile({inline, [rank_leaf4/10]}).
rank_leaf4(Key, LowerBound, K1, K2, K3, K4, V1, V2, V3, V4) ->
    if
        Key > K2 ->
            if
                Key == K3 ->
                    [LowerBound + 3 | V3];
                Key == K4 ->
                    [LowerBound + 4 | V4];
                true ->
                    error_badkey(Key)
            end;
        Key < K2 ->
            if
                Key == K1 ->
                    [LowerBound + 1 | V1];
                true ->
                    error_badkey(Key)
            end;
        true ->
            [LowerBound + 2 | V2]
    end.

-compile({inline, [rank_leaf3/8]}).
rank_leaf3(Key, LowerBound, K1, K2, K3, V1, V2, V3) ->
    if
        Key < K2 ->
            if
                Key == K1 ->
                    [LowerBound + 1 | V1];
                true ->
                    error_badkey(Key)
            end;
        Key > K2 ->
            if
                Key == K3 ->
                    [LowerBound + 3 | V3];
                true ->
                    error_badkey(Key)
            end;
        true ->
            [LowerBound + 2 | V2]
    end.

-compile({inline, [rank_leaf2/6]}).
rank_leaf2(Key, LowerBound, K1, K2, V1, V2) ->
    if
        Key == K2 ->
            [LowerBound + 2 | V2];
        Key == K1 ->
            [LowerBound + 1 | V1];
        true ->
            error_badkey(Key)
    end.

-compile({inline, [rank_leaf1/4]}).
rank_leaf1(Key, LowerBound, K1, V1) ->
    if
        Key == K1 ->
            [LowerBound + 1 | V1];
        true ->
            error_badkey(Key)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion
%% ------------------------------------------------------------------

-spec insert_recur(
    Key,
    insertion_value_wrap(Value),
    insertion_value_eval(),
    deep_node(Key, Value)
) -> deep_node_after_insertion(Key, Value) | split_result(Key, Value).
insert_recur(Key, ValueEval, ValueWrap, ?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3)) ->
    insert_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3);
insert_recur(Key, ValueEval, ValueWrap, ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)) ->
    insert_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
insert_recur(
    Key,
    ValueEval,
    ValueWrap,
    ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)
) ->
    insert_internal4(
        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
    );
insert_recur(Key, ValueEval, ValueWrap, ?LEAF2(K1, K2, V1, V2)) ->
    insert_leaf2(Key, ValueEval, ValueWrap, K1, K2, V1, V2);
insert_recur(Key, ValueEval, ValueWrap, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    insert_leaf3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
insert_recur(Key, ValueEval, ValueWrap, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    insert_leaf4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal4/17]}).
insert_internal4(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
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
                                O1,
                                O2,
                                O3,
                                O4,
                                C1,
                                C2,
                                C3,
                                C4,
                                C5
                            );
                        %
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
                                O1,
                                O2,
                                O3,
                                O4,
                                C1,
                                C2,
                                C3,
                                C4,
                                C5
                            );
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key > K4 ->
                    insert_internal4_child5(
                        Key,
                        ValueEval,
                        ValueWrap,
                        K1,
                        K2,
                        K3,
                        K4,
                        Values,
                        O1,
                        O2,
                        O3,
                        O4,
                        C1,
                        C2,
                        C3,
                        C4,
                        C5
                    );
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K2 ->
            if
                Key > K1 ->
                    insert_internal4_child2(
                        Key,
                        ValueEval,
                        ValueWrap,
                        K1,
                        K2,
                        K3,
                        K4,
                        Values,
                        O1,
                        O2,
                        O3,
                        O4,
                        C1,
                        C2,
                        C3,
                        C4,
                        C5
                    );
                %
                Key < K1 ->
                    insert_internal4_child1(
                        Key,
                        ValueEval,
                        ValueWrap,
                        K1,
                        K2,
                        K3,
                        K4,
                        Values,
                        O1,
                        O2,
                        O3,
                        O4,
                        C1,
                        C2,
                        C3,
                        C4,
                        C5
                    );
                %
                true ->
                    error_key_exists(Key)
            end;
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal4_child1/17]}).
insert_internal4_child1(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
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
                SplitLOffset,
                O1 + 1,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                SplitL,
                SplitR,
                C2,
                C3,
                C4,
                C5
            );
        %
        UpdatedC1 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                O1 + 1,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                UpdatedC1,
                C2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child2/17]}).
insert_internal4_child2(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
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
                O1,
                O1 + SplitLOffset,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                C1,
                SplitL,
                SplitR,
                C3,
                C4,
                C5
            );
        %
        UpdatedC2 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                O1,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                C1,
                UpdatedC2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child3/17]}).
insert_internal4_child3(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
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
                O1,
                O2,
                O2 + SplitLOffset,
                O3 + 1,
                O4 + 1,
                C1,
                C2,
                SplitL,
                SplitR,
                C4,
                C5
            );
        %
        UpdatedC3 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                O1,
                O2,
                O3 + 1,
                O4 + 1,
                C1,
                C2,
                UpdatedC3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child4/17]}).
insert_internal4_child4(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
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
                O1,
                O2,
                O3,
                O3 + SplitLOffset,
                O4 + 1,
                C1,
                C2,
                C3,
                SplitL,
                SplitR,
                C5
            );
        %
        UpdatedC4 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                O1,
                O2,
                O3,
                O4 + 1,
                C1,
                C2,
                C3,
                UpdatedC4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child5/17]}).
insert_internal4_child5(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C5) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
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
                O1,
                O2,
                O3,
                O4,
                O4 + SplitLOffset,
                C1,
                C2,
                C3,
                C4,
                SplitL,
                SplitR
            );
        %
        UpdatedC5 ->
            ?INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                Values,
                O1,
                O2,
                O3,
                O4,
                C1,
                C2,
                C3,
                C4,
                UpdatedC5
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal3/14]}).
insert_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    if
        Key > K2 ->
            if
                Key < K3 ->
                    insert_internal3_child3(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
                    );
                %
                Key > K3 ->
                    insert_internal3_child4(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
                    );
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K2 ->
            if
                Key > K1 ->
                    insert_internal3_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
                    );
                %
                Key < K1 ->
                    insert_internal3_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
                    );
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal3_child1/14]}).
insert_internal3_child1(
    Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                SplitK,
                K1,
                K2,
                K3,
                {SplitV, V1, V2, V3},
                SplitLOffset,
                O1 + 1,
                O2 + 1,
                O3 + 1,
                SplitL,
                SplitR,
                C2,
                C3,
                C4
            );
        %
        UpdatedC1 ->
            ?INTERNAL3(
                K1,
                K2,
                K3,
                Values,
                O1 + 1,
                O2 + 1,
                O3 + 1,
                UpdatedC1,
                C2,
                C3,
                C4
            )
    end.

-compile({inline, [insert_internal3_child2/14]}).
insert_internal3_child2(
    Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                SplitK,
                K2,
                K3,
                {V1, SplitV, V2, V3},
                O1,
                O1 + SplitLOffset,
                O2 + 1,
                O3 + 1,
                C1,
                SplitL,
                SplitR,
                C3,
                C4
            );
        %
        UpdatedC2 ->
            ?INTERNAL3(
                K1,
                K2,
                K3,
                Values,
                O1,
                O2 + 1,
                O3 + 1,
                C1,
                UpdatedC2,
                C3,
                C4
            )
    end.

-compile({inline, [insert_internal3_child3/14]}).
insert_internal3_child3(
    Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                K2,
                SplitK,
                K3,
                {V1, V2, SplitV, V3},
                O1,
                O2,
                O2 + SplitLOffset,
                O3 + 1,
                C1,
                C2,
                SplitL,
                SplitR,
                C4
            );
        %
        UpdatedC3 ->
            ?INTERNAL3(
                K1,
                K2,
                K3,
                Values,
                O1,
                O2,
                O3 + 1,
                C1,
                C2,
                UpdatedC3,
                C4
            )
    end.

-compile({inline, [insert_internal3_child4/14]}).
insert_internal3_child4(
    Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                K2,
                K3,
                SplitK,
                {V1, V2, V3, SplitV},
                O1,
                O2,
                O3,
                O3 + SplitLOffset,
                C1,
                C2,
                C3,
                SplitL,
                SplitR
            );
        %
        UpdatedC4 ->
            ?INTERNAL3(
                K1,
                K2,
                K3,
                Values,
                O1,
                O2,
                O3,
                C1,
                C2,
                C3,
                UpdatedC4
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal2/11]}).
insert_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    insert_internal2_child2(
                        Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3
                    );
                %
                Key > K2 ->
                    insert_internal2_child3(
                        Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3
                    );
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K1 ->
            insert_internal2_child1(
                Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3
            );
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal2_child1/11]}).
insert_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                SplitK,
                K1,
                K2,
                {SplitV, V1, V2},
                SplitLOffset,
                O1 + 1,
                O2 + 1,
                SplitL,
                SplitR,
                C2,
                C3
            );
        %
        UpdatedC1 ->
            ?INTERNAL2(
                K1,
                K2,
                Values,
                O1 + 1,
                O2 + 1,
                UpdatedC1,
                C2,
                C3
            )
    end.

-compile({inline, [insert_internal2_child2/11]}).
insert_internal2_child2(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                K1,
                SplitK,
                K2,
                {V1, SplitV, V2},
                O1,
                O1 + SplitLOffset,
                O2 + 1,
                C1,
                SplitL,
                SplitR,
                C3
            );
        %
        UpdatedC2 ->
            ?INTERNAL2(
                K1,
                K2,
                Values,
                O1,
                O2 + 1,
                C1,
                UpdatedC2,
                C3
            )
    end.

-compile({inline, [insert_internal2_child3/11]}).
insert_internal2_child3(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                K1,
                K2,
                SplitK,
                {V1, V2, SplitV},
                O1,
                O2,
                O2 + SplitLOffset,
                C1,
                C2,
                SplitL,
                SplitR
            );
        %
        UpdatedC3 ->
            ?INTERNAL2(
                K1,
                K2,
                Values,
                O1,
                O2,
                C1,
                C2,
                UpdatedC3
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal1/8]}).
insert_internal1(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2) ->
    if
        Key < K1 ->
            insert_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2);
        Key > K1 ->
            insert_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2);
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal1_child1/8]}).
insert_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            ?INTERNAL2(
                SplitK,
                K1,
                [SplitV | V1],
                SplitLOffset,
                O1 + 1,
                SplitL,
                SplitR,
                C2
            );
        %
        UpdatedC1 ->
            ?INTERNAL1(
                K1,
                V1,
                O1 + 1,
                UpdatedC1,
                C2
            )
    end.

-compile({inline, [insert_internal1_child2/8]}).
insert_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR) ->
            ?INTERNAL2(
                K1,
                SplitK,
                [V1 | SplitV],
                O1,
                O1 + SplitLOffset,
                C1,
                SplitL,
                SplitR
            );
        %
        UpdatedC2 ->
            ?INTERNAL1(
                K1,
                V1,
                O1,
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

-compile({inline, internal_split/21}).
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
    pos_integer(),
    pos_integer(),
    pos_integer(),
    pos_integer(),
    pos_integer(),
    C,
    C,
    C,
    C,
    C,
    C
) -> internal_split_result(K, V) when C :: deep_node(K, V).
internal_split(
    K1,
    K2,
    K3,
    K4,
    K5,
    V1,
    V2,
    V3,
    V4,
    V5,
    O1,
    O2,
    O3,
    O4,
    O5,
    C1,
    C2,
    C3,
    C4,
    C5,
    C6
) ->
    SplitK = K3,
    SplitV = V3,

    SplitLOffset = O3,
    SplitL = ?INTERNAL2(K1, K2, [V1 | V2], O1, O2, C1, C2, C3),

    RO1 = O4 - O3,
    RO2 = O5 - O3,
    SplitR = ?INTERNAL2(K4, K5, [V4 | V5], RO1, RO2, C4, C5, C6),

    ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR).

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

    SplitLOffset = 3,
    SplitL = ?LEAF2(K1, K2, V1, V2),
    SplitR = ?LEAF2(K4, K5, V4, V5),

    ?SPLIT(SplitK, SplitV, SplitLOffset, SplitL, SplitR).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Update
%% ------------------------------------------------------------------

-spec update_recur(
    Key,
    update_value_wrap(Value, UpdatedValue),
    update_value_eval(),
    deep_node(Key, Value)
) -> deep_node(Key, Value | UpdatedValue).
update_recur(Key, ValueEval, ValueWrap, ?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3)) ->
    update_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3);
update_recur(Key, ValueEval, ValueWrap, ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)) ->
    update_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
update_recur(
    Key,
    ValueEval,
    ValueWrap,
    ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)
) ->
    update_internal4(
        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
    );
update_recur(Key, ValueEval, ValueWrap, ?LEAF2(K1, K2, V1, V2)) ->
    update_leaf2(Key, ValueEval, ValueWrap, K1, K2, V1, V2);
update_recur(Key, ValueEval, ValueWrap, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    update_leaf3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
update_recur(Key, ValueEval, ValueWrap, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    update_leaf4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal4/17]}).
update_internal4(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
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
                                O1,
                                O2,
                                O3,
                                O4,
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
                                O1,
                                O2,
                                O3,
                                O4,
                                C1,
                                C2,
                                C3,
                                C4,
                                C5
                            );
                        true ->
                            update_internal4_key3(
                                Key,
                                ValueEval,
                                ValueWrap,
                                K1,
                                K2,
                                K4,
                                Values,
                                O1,
                                O2,
                                O3,
                                O4,
                                C1,
                                C2,
                                C3,
                                C4,
                                C5
                            )
                    end;
                Key > K4 ->
                    update_internal4_child5(
                        Key,
                        ValueEval,
                        ValueWrap,
                        K1,
                        K2,
                        K3,
                        K4,
                        Values,
                        O1,
                        O2,
                        O3,
                        O4,
                        C1,
                        C2,
                        C3,
                        C4,
                        C5
                    );
                true ->
                    update_internal4_key4(
                        Key,
                        ValueEval,
                        ValueWrap,
                        K1,
                        K2,
                        K3,
                        Values,
                        O1,
                        O2,
                        O3,
                        O4,
                        C1,
                        C2,
                        C3,
                        C4,
                        C5
                    )
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    update_internal4_child2(
                        Key,
                        ValueEval,
                        ValueWrap,
                        K1,
                        K2,
                        K3,
                        K4,
                        Values,
                        O1,
                        O2,
                        O3,
                        O4,
                        C1,
                        C2,
                        C3,
                        C4,
                        C5
                    );
                Key < K1 ->
                    update_internal4_child1(
                        Key,
                        ValueEval,
                        ValueWrap,
                        K1,
                        K2,
                        K3,
                        K4,
                        Values,
                        O1,
                        O2,
                        O3,
                        O4,
                        C1,
                        C2,
                        C3,
                        C4,
                        C5
                    );
                true ->
                    update_internal4_key1(
                        Key,
                        ValueEval,
                        ValueWrap,
                        K2,
                        K3,
                        K4,
                        Values,
                        O1,
                        O2,
                        O3,
                        O4,
                        C1,
                        C2,
                        C3,
                        C4,
                        C5
                    )
            end;
        true ->
            update_internal4_key2(
                Key, ValueEval, ValueWrap, K1, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
            )
    end.

-compile({inline, [update_internal4_child1/17]}).
update_internal4_child1(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        update_recur(Key, ValueEval, ValueWrap, C1),
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_child2/17]}).
update_internal4_child2(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        C1,
        update_recur(Key, ValueEval, ValueWrap, C2),
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_child3/17]}).
update_internal4_child3(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        update_recur(Key, ValueEval, ValueWrap, C3),
        C4,
        C5
    ).

-compile({inline, [update_internal4_child4/17]}).
update_internal4_child4(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        C3,
        update_recur(Key, ValueEval, ValueWrap, C4),
        C5
    ).

-compile({inline, [update_internal4_child5/17]}).
update_internal4_child5(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        C3,
        C4,
        update_recur(Key, ValueEval, ValueWrap, C5)
    ).

%%%

-compile({inline, [update_internal4_key1/16]}).
update_internal4_key1(
    Key, ValueEval, ValueWrap, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL4(
        Key,
        K2,
        K3,
        K4,
        {Value, V2, V3, V4},
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_key2/16]}).
update_internal4_key2(
    Key, ValueEval, ValueWrap, K1, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V2),
    ?INTERNAL4(
        K1,
        Key,
        K3,
        K4,
        {V1, Value, V3, V4},
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_key3/16]}).
update_internal4_key3(
    Key, ValueEval, ValueWrap, K1, K2, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V3),
    ?INTERNAL4(
        K1,
        K2,
        Key,
        K4,
        {V1, V2, Value, V4},
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_key4/16]}).
update_internal4_key4(
    Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V4),
    ?INTERNAL4(
        K1,
        K2,
        K3,
        Key,
        {V1, V2, V3, Value},
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        C3,
        C4,
        C5
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal3/14]}).
update_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    if
        Key > K2 ->
            if
                Key < K3 ->
                    update_internal3_child3(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
                    );
                Key > K3 ->
                    update_internal3_child4(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
                    );
                true ->
                    update_internal3_key3(
                        Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, O3, C1, C2, C3, C4
                    )
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    update_internal3_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
                    );
                Key < K1 ->
                    update_internal3_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
                    );
                true ->
                    update_internal3_key1(
                        Key, ValueEval, ValueWrap, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4
                    )
            end;
        true ->
            update_internal3_key2(
                Key, ValueEval, ValueWrap, K1, K3, Values, O1, O2, O3, C1, C2, C3, C4
            )
    end.

-compile({inline, [update_internal3_child1/14]}).
update_internal3_child1(Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        O1,
        O2,
        O3,
        update_recur(Key, ValueEval, ValueWrap, C1),
        C2,
        C3,
        C4
    ).

-compile({inline, [update_internal3_child2/14]}).
update_internal3_child2(Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        O1,
        O2,
        O3,
        C1,
        update_recur(Key, ValueEval, ValueWrap, C2),
        C3,
        C4
    ).

-compile({inline, [update_internal3_child3/14]}).
update_internal3_child3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        O1,
        O2,
        O3,
        C1,
        C2,
        update_recur(Key, ValueEval, ValueWrap, C3),
        C4
    ).

-compile({inline, [update_internal3_child4/14]}).
update_internal3_child4(Key, ValueEval, ValueWrap, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        O1,
        O2,
        O3,
        C1,
        C2,
        C3,
        update_recur(Key, ValueEval, ValueWrap, C4)
    ).

%%%

-compile({inline, [update_internal3_key1/13]}).
update_internal3_key1(Key, ValueEval, ValueWrap, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL3(
        Key,
        K2,
        K3,
        {Value, V2, V3},
        O1,
        O2,
        O3,
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, [update_internal3_key2/13]}).
update_internal3_key2(Key, ValueEval, ValueWrap, K1, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V2),
    ?INTERNAL3(
        K1,
        Key,
        K3,
        {V1, Value, V3},
        O1,
        O2,
        O3,
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, [update_internal3_key3/13]}).
update_internal3_key3(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, O3, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V3),
    ?INTERNAL3(
        K1,
        K2,
        Key,
        {V1, V2, Value},
        O1,
        O2,
        O3,
        C1,
        C2,
        C3,
        C4
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal2/11]}).
update_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    update_internal2_child2(
                        Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3
                    );
                Key > K2 ->
                    update_internal2_child3(
                        Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3
                    );
                true ->
                    update_internal2_key2(Key, ValueEval, ValueWrap, K1, Values, O1, O2, C1, C2, C3)
            end;
        Key < K1 ->
            update_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3);
        true ->
            update_internal2_key1(Key, ValueEval, ValueWrap, K2, Values, O1, O2, C1, C2, C3)
    end.

-compile({inline, [update_internal2_child1/11]}).
update_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3) ->
    ?INTERNAL2(K1, K2, Values, O1, O2, update_recur(Key, ValueEval, ValueWrap, C1), C2, C3).

-compile({inline, [update_internal2_child2/11]}).
update_internal2_child2(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3) ->
    ?INTERNAL2(K1, K2, Values, O1, O2, C1, update_recur(Key, ValueEval, ValueWrap, C2), C3).

-compile({inline, [update_internal2_child3/11]}).
update_internal2_child3(Key, ValueEval, ValueWrap, K1, K2, Values, O1, O2, C1, C2, C3) ->
    ?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, update_recur(Key, ValueEval, ValueWrap, C3)).

%%%

-compile({inline, [update_internal2_key1/10]}).
update_internal2_key1(Key, ValueEval, ValueWrap, K2, Values, O1, O2, C1, C2, C3) ->
    [V1 | V2] = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL2(Key, K2, [Value | V2], O1, O2, C1, C2, C3).

-compile({inline, [update_internal2_key1/10]}).
update_internal2_key2(Key, ValueEval, ValueWrap, K1, Values, O1, O2, C1, C2, C3) ->
    [V1 | V2] = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V2),
    ?INTERNAL2(K1, Key, [V1 | Value], O1, O2, C1, C2, C3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal1/8]}).
update_internal1(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2) ->
    if
        Key < K1 ->
            update_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2);
        Key > K1 ->
            update_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2);
        true ->
            update_internal1_key1(Key, ValueEval, ValueWrap, V1, O1, C1, C2)
    end.

-compile({inline, [update_internal1_child1/8]}).
update_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2) ->
    ?INTERNAL1(K1, V1, O1, update_recur(Key, ValueEval, ValueWrap, C1), C2).

-compile({inline, [update_internal1_child2/8]}).
update_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, O1, C1, C2) ->
    ?INTERNAL1(K1, V1, O1, C1, update_recur(Key, ValueEval, ValueWrap, C2)).

-compile({inline, [update_internal1_key1/7]}).
update_internal1_key1(Key, ValueEval, ValueWrap, V1, O1, C1, C2) ->
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL1(Key, Value, O1, C1, C2).

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

-spec root_delete(Key, non_empty_node(Key, Value)) -> node_after_deletion(Key, Value).
-compile({inline, root_delete/2}).
root_delete(K, ?INTERNAL1(K1, V1, O1, C1, C2)) ->
    delete_internal1(K, K1, V1, O1, C1, C2);
root_delete(K, ?LEAF1(K1, _)) ->
    delete_leaf1(K, K1);
root_delete(K, ?LEAF0) ->
    error_badkey(K);
root_delete(K, Root) ->
    delete_recur(K, Root).

-spec delete_recur(Key, deep_node(Key, Value)) ->
    node_after_deletion(Key, Value) | unbalanced_node(Key, Value).
delete_recur(K, ?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3)) ->
    delete_internal2(K, K1, K2, Values, O1, O2, C1, C2, C3);
delete_recur(K, ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)) ->
    delete_internal3(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
delete_recur(K, ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)) ->
    delete_internal4(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5);
delete_recur(K, ?LEAF2(K1, K2, V1, V2)) ->
    delete_leaf2(K, K1, K2, V1, V2);
delete_recur(K, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    delete_leaf3(K, K1, K2, K3, V1, V2, V3);
delete_recur(K, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    delete_leaf4(K, K1, K2, K3, K4, V1, V2, V3, V4).

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL4

-compile({inline, delete_internal4/15}).
delete_internal4(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    if
        K > K2 ->
            if
                K < K4 ->
                    if
                        K > K3 ->
                            delete_internal4_child4(
                                K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                            );
                        K < K3 ->
                            delete_internal4_child3(
                                K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                            );
                        true ->
                            delete_internal4_key3(
                                K1, K2, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                            )
                    end;
                %
                K > K4 ->
                    delete_internal4_child5(
                        K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                    );
                %
                true ->
                    delete_internal4_key4(K1, K2, K3, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)
            end;
        %
        K < K2 ->
            if
                K > K1 ->
                    delete_internal4_child2(
                        K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                    );
                K < K1 ->
                    delete_internal4_child1(
                        K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                    );
                true ->
                    delete_internal4_key1(K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)
            end;
        %
        true ->
            delete_internal4_key2(K1, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)
    end.

-compile({inline, [delete_internal4_child1/15]}).
delete_internal4_child1(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    UpdatedC1 = delete_recur(K, C1),

    delete_internal4_rebalance_child1(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        UpdatedC1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [delete_internal4_child2/15]}).
delete_internal4_child2(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    UpdatedC2 = delete_recur(K, C2),

    delete_internal4_rebalance_child2(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        C1,
        UpdatedC2,
        C3,
        C4,
        C5
    ).

-compile({inline, [delete_internal4_child3/15]}).
delete_internal4_child3(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    UpdatedC3 = delete_recur(K, C3),

    delete_internal4_rebalance_child3(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        UpdatedC3,
        C4,
        C5
    ).

-compile({inline, [delete_internal4_child4/15]}).
delete_internal4_child4(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    UpdatedC4 = delete_recur(K, C4),

    delete_internal4_rebalance_child4(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        C3,
        UpdatedC4,
        C5
    ).

-compile({inline, [delete_internal4_child5/15]}).
delete_internal4_child5(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    UpdatedC5 = delete_recur(K, C5),

    delete_internal4_rebalance_child5(
        K1,
        K2,
        K3,
        K4,
        Values,
        O1,
        O2,
        O3,
        O4,
        C1,
        C2,
        C3,
        C4,
        UpdatedC5
    ).

%%% Delete - INTERNAL4 - keys in node

-compile({inline, [delete_internal4_key1/13]}).
delete_internal4_key1(K2, K3, K4, PrevValues, O1, O2, O3, O4, C1, PrevC2, C3, C4, C5) ->
    [[ReplacementK | ReplacementV] | C2] = take_smallest_recur(PrevC2),
    K1 = ReplacementK,
    V1 = ReplacementV,
    {_, V2, V3, V4} = PrevValues,
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,
    UpdatedO4 = O4 - 1,

    case C2 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                UpdatedO2,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                0,
                O1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            delete_internal4_rebalance_child2_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C3,
                C4,
                C5
            );
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                %
                K1,
                V1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            delete_internal4_rebalance_child2_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C3,
                C4,
                C5
            );
        %
        %
        _ ->
            Values = {V1, V2, V3, V4},
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    K3,
                    K4,
                    Values,
                    %
                    O1,
                    UpdatedO2,
                    UpdatedO3,
                    UpdatedO4,
                    %
                    C1,
                    C2,
                    C3,
                    C4,
                    C5
                )
            )
    end.

-compile({inline, [delete_internal4_key2/13]}).
delete_internal4_key2(K1, K3, K4, PrevValues, O1, O2, O3, O4, C1, C2, PrevC3, C4, C5) ->
    [[ReplacementK | ReplacementV] | C3] = take_smallest_recur(PrevC3),
    K2 = ReplacementK,
    V2 = ReplacementV,
    {V1, _, V3, V4} = PrevValues,
    UpdatedO3 = O3 - 1,
    UpdatedO4 = O4 - 1,

    case C3 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                UpdatedO3,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K2,
                V2,
                O1,
                O2,
                C2,
                %
                K3,
                V3,
                C4
            ),

            delete_internal4_rebalance_child3_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                O2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C2,
                C4,
                C5
            );
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K2,
                V2,
                C2,
                K3,
                V3,
                C4
            ),

            delete_internal4_rebalance_child3_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                O2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C2,
                C4,
                C5
            );
        %
        %
        _ ->
            Values = {V1, V2, V3, V4},
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    K3,
                    K4,
                    Values,
                    %
                    O1,
                    O2,
                    UpdatedO3,
                    UpdatedO4,
                    %
                    C1,
                    C2,
                    C3,
                    C4,
                    C5
                )
            )
    end.

-compile({inline, [delete_internal4_key3/13]}).
delete_internal4_key3(K1, K2, K4, PrevValues, O1, O2, O3, O4, C1, C2, C3, PrevC4, C5) ->
    [[ReplacementK | ReplacementV] | C4] = take_smallest_recur(PrevC4),
    K3 = ReplacementK,
    V3 = ReplacementV,
    {V1, V2, _, V4} = PrevValues,
    UpdatedO4 = O4 - 1,

    case C4 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                UpdatedO4,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K3,
                V3,
                O2,
                O3,
                C3,
                %
                K4,
                V4,
                C5
            ),

            delete_internal4_rebalance_child4_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                O2,
                O3,
                UpdatedO4,
                %
                C1,
                C2,
                C3,
                C5
            );
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K3,
                V3,
                C3,
                K4,
                V4,
                C5
            ),

            delete_internal4_rebalance_child4_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                O2,
                O3,
                UpdatedO4,
                %
                C1,
                C2,
                C3,
                C5
            );
        %
        %
        _ ->
            Values = {V1, V2, V3, V4},
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    K3,
                    K4,
                    Values,
                    %
                    O1,
                    O2,
                    O3,
                    UpdatedO4,
                    %
                    C1,
                    C2,
                    C3,
                    C4,
                    C5
                )
            )
    end.

-compile({inline, [delete_internal4_key4/13]}).
delete_internal4_key4(K1, K2, K3, PrevValues, O1, O2, O3, O4, C1, C2, C3, C4, PrevC5) ->
    [[ReplacementK | ReplacementV] | C5] = take_smallest_recur(PrevC5),
    K4 = ReplacementK,
    V4 = ReplacementV,
    {V1, V2, V3, _} = PrevValues,

    case C5 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_rightmost_internal(
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K4,
                V4,
                O4 - O3,
                C4
            ),

            delete_internal4_rebalance_child5_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                O2,
                O3,
                O4,
                %
                C1,
                C2,
                C3
            );
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_rightmost_leaf(
                CK,
                CV,
                K4,
                V4,
                C4
            ),

            delete_internal4_rebalance_child5_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                O2,
                O3,
                O4,
                %
                C1,
                C2,
                C3
            );
        %
        %
        _ ->
            Values = {V1, V2, V3, V4},
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    K3,
                    K4,
                    Values,
                    %
                    O1,
                    O2,
                    O3,
                    O4,
                    %
                    C1,
                    C2,
                    C3,
                    C4,
                    C5
                )
            )
    end.

%%% Delete - INTERNAL4 - rebalance

-compile({inline, [delete_internal4_rebalance_child1/14]}).
delete_internal4_rebalance_child1(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    UpdatedO1 = O1 - 1,
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,
    UpdatedO4 = O4 - 1,

    case C1 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            {V1, V2, V3, V4} = Values,

            Result = rebalance_leftmost_internal(
                UpdatedO1,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                C2
            ),

            delete_internal4_rebalance_child1_finish(
                Result,
                K2,
                K3,
                K4,
                V2,
                V3,
                V4,
                %
                UpdatedO1,
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
                %
                C3,
                C4,
                C5
            );
        %
        ?LEAF1(CK, CV) ->
            {V1, V2, V3, V4} = Values,

            Result = rebalance_leftmost_leaf(
                CK,
                CV,
                K1,
                V1,
                C2
            ),

            delete_internal4_rebalance_child1_finish(
                Result,
                K2,
                K3,
                K4,
                V2,
                V3,
                V4,
                %
                UpdatedO1,
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
                %
                C3,
                C4,
                C5
            );
        %
        _ ->
            ?check_node(
                ?INTERNAL4(K1, K2, K3, K4, Values, UpdatedO1, UpdatedO2, UpdatedO3, UpdatedO4, C1, C2, C3, C4, C5)
            )
    end.

-compile({inline, delete_internal4_rebalance_child1_finish/14}).
delete_internal4_rebalance_child1_finish(
    Result, K2, K3, K4, V2, V3, V4, O1, O2, O3, O4, C3, C4, C5
) ->
    case Result of
        ?ROTATED(UpK, UpVal, MovedSize, UpdatedC1, UpdatedC2) ->
            ?check_node(
                ?INTERNAL4(
                    UpK,
                    K2,
                    K3,
                    K4,
                    {UpVal, V2, V3, V4},
                    %
                    O1 + MovedSize,
                    O2,
                    O3,
                    O4,
                    %
                    UpdatedC1,
                    UpdatedC2,
                    C3,
                    C4,
                    C5
                )
            );
        %
        ?MERGED(MergedC1C2) ->
            ?check_node(
                ?INTERNAL3(
                    K2,
                    K3,
                    K4,
                    {V2, V3, V4},
                    %
                    O2,
                    O3,
                    O4,
                    %
                    MergedC1C2,
                    C3,
                    C4,
                    C5
                )
            )
    end.

%%%%%%%

-compile({inline, [delete_internal4_rebalance_child2/14]}).
delete_internal4_rebalance_child2(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,
    UpdatedO4 = O4 - 1,

    case C2 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            {V1, V2, V3, V4} = Values,
            Result = rebalance_internal_from_either_sibling(
                UpdatedO2,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                0,
                O1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            delete_internal4_rebalance_child2_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C3,
                C4,
                C5
            );
        %
        %
        ?LEAF1(CK, CV) ->
            {V1, V2, V3, V4} = Values,
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K1,
                V1,
                C1,
                K2,
                V2,
                C3
            ),

            delete_internal4_rebalance_child2_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C3,
                C4,
                C5
            );
        %
        %
        _ ->
            ?check_node(
                ?INTERNAL4(K1, K2, K3, K4, Values, O1, UpdatedO2, UpdatedO3, UpdatedO4, C1, C2, C3, C4, C5)
            )
    end.

-compile({inline, delete_internal4_rebalance_child2_finish/17}).
delete_internal4_rebalance_child2_finish(
    Result, K1, K2, K3, K4, V1, V2, V3, V4, O1, O2, O3, O4, C1, C3, C4, C5
) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC1C2) ->
            ?INTERNAL3(
                K2,
                K3,
                K4,
                {V2, V3, V4},
                %
                O2,
                O3,
                O4,
                %
                MergedC1C2,
                C3,
                C4,
                C5
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, RebalancedC2, UpdatedC3) ->
            ?check_node(
                ?INTERNAL4(
                    K1,
                    UpK,
                    K3,
                    K4,
                    {V1, UpVal, V3, V4},
                    %
                    O1,
                    O2 + MovedSize,
                    O3,
                    O4,
                    %
                    C1,
                    RebalancedC2,
                    UpdatedC3,
                    C4,
                    C5
                )
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedC1, RebalancedC2) ->
            ?check_node(
                ?INTERNAL4(
                    UpK,
                    K2,
                    K3,
                    K4,
                    {UpVal, V2, V3, V4},
                    %
                    O1 - MovedSize,
                    O2,
                    O3,
                    O4,
                    %
                    UpdatedC1,
                    RebalancedC2,
                    C3,
                    C4,
                    C5
                )
            )
    end.

%%%%%%%%

-compile({inline, [delete_internal4_rebalance_child3/14]}).
delete_internal4_rebalance_child3(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    UpdatedO3 = O3 - 1,
    UpdatedO4 = O4 - 1,

    case C3 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            {V1, V2, V3, V4} = Values,
            Result = rebalance_internal_from_either_sibling(
                UpdatedO3,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K2,
                V2,
                O1,
                O2,
                C2,
                %
                K3,
                V3,
                C4
            ),

            delete_internal4_rebalance_child3_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                O2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C2,
                C4,
                C5
            );
        %
        %
        ?LEAF1(CK, CV) ->
            {V1, V2, V3, V4} = Values,
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K2,
                V2,
                C2,
                K3,
                V3,
                C4
            ),

            delete_internal4_rebalance_child3_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                O2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C2,
                C4,
                C5
            );
        %
        %
        _ ->
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    K3,
                    K4,
                    Values,
                    %
                    O1,
                    O2,
                    UpdatedO3,
                    UpdatedO4,
                    %
                    C1,
                    C2,
                    C3,
                    C4,
                    C5
                )
            )
    end.

-compile({inline, delete_internal4_rebalance_child3_finish/17}).
delete_internal4_rebalance_child3_finish(
    Result, K1, K2, K3, K4, V1, V2, V3, V4, O1, O2, O3, O4, C1, C2, C4, C5
) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC2C3) ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K3,
                    K4,
                    {V1, V3, V4},
                    %
                    O1,
                    O3,
                    O4,
                    %
                    C1,
                    MergedC2C3,
                    C4,
                    C5
                )
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, RebalancedC3, UpdatedC4) ->
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    UpK,
                    K4,
                    {V1, V2, UpVal, V4},
                    %
                    O1,
                    O2,
                    O3 + MovedSize,
                    O4,
                    %
                    C1,
                    C2,
                    RebalancedC3,
                    UpdatedC4,
                    C5
                )
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedC2, RebalancedC3) ->
            ?check_node(
                ?INTERNAL4(
                    K1,
                    UpK,
                    K3,
                    K4,
                    {V1, UpVal, V3, V4},
                    %
                    O1,
                    O2 - MovedSize,
                    O3,
                    O4,
                    %
                    C1,
                    UpdatedC2,
                    RebalancedC3,
                    C4,
                    C5
                )
            )
    end.

%%%

-compile({inline, [delete_internal4_rebalance_child4/14]}).
delete_internal4_rebalance_child4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    UpdatedO4 = O4 - 1,

    case C4 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            {V1, V2, V3, V4} = Values,
            Result = rebalance_internal_from_either_sibling(
                UpdatedO4,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K3,
                V3,
                O2,
                O3,
                C3,
                %
                K4,
                V4,
                C5
            ),

            delete_internal4_rebalance_child4_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                O2,
                O3,
                UpdatedO4,
                %
                C1,
                C2,
                C3,
                C5
            );
        %
        %
        ?LEAF1(CK, CV) ->
            {V1, V2, V3, V4} = Values,
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K3,
                V3,
                C3,
                K4,
                V4,
                C5
            ),

            delete_internal4_rebalance_child4_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                V1,
                V2,
                V3,
                V4,
                %
                O1,
                O2,
                O3,
                UpdatedO4,
                %
                C1,
                C2,
                C3,
                C5
            );
        %
        %
        _ ->
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    K3,
                    K4,
                    Values,
                    %
                    O1,
                    O2,
                    O3,
                    UpdatedO4,
                    %
                    C1,
                    C2,
                    C3,
                    C4,
                    C5
                )
            )
    end.

-compile({inline, delete_internal4_rebalance_child4_finish/17}).
delete_internal4_rebalance_child4_finish(
    Result, K1, K2, K3, K4, V1, V2, V3, V4, O1, O2, O3, O4, C1, C2, C3, C5
) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC3C4) ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K4,
                    {V1, V2, V4},
                    %
                    O1,
                    O2,
                    O4,
                    %
                    C1,
                    C2,
                    MergedC3C4,
                    C5
                )
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, RebalancedC4, UpdatedC5) ->
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    K3,
                    UpK,
                    {V1, V2, V3, UpVal},
                    %
                    O1,
                    O2,
                    O3,
                    O4 + MovedSize,
                    %
                    C1,
                    C2,
                    C3,
                    RebalancedC4,
                    UpdatedC5
                )
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedC3, RebalancedC4) ->
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    UpK,
                    K4,
                    {V1, V2, UpVal, V4},
                    %
                    O1,
                    O2,
                    O3 - MovedSize,
                    O4,
                    %
                    C1,
                    C2,
                    UpdatedC3,
                    RebalancedC4,
                    C5
                )
            )
    end.

%%%

-compile({inline, [delete_internal4_rebalance_child5/14]}).
delete_internal4_rebalance_child5(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    case C5 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            {V1, V2, V3, V4} = Values,
            Result = rebalance_rightmost_internal(
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K4,
                V4,
                O4 - O3,
                C4
            ),

            delete_internal4_rebalance_child5_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                O2,
                O3,
                O4,
                %
                C1,
                C2,
                C3
            );
        %
        %
        ?LEAF1(CK, CV) ->
            {V1, V2, V3, V4} = Values,
            Result = rebalance_rightmost_leaf(
                CK,
                CV,
                K4,
                V4,
                C4
            ),

            delete_internal4_rebalance_child5_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                O2,
                O3,
                O4,
                %
                C1,
                C2,
                C3
            );
        %
        %
        _ ->
            ?check_node(?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5))
    end.

-compile({inline, delete_internal4_rebalance_child5_finish/14}).
delete_internal4_rebalance_child5_finish(
    Result, K1, K2, K3, V1, V2, V3, O1, O2, O3, O4, C1, C2, C3
) ->
    case Result of
        ?ROTATED(UpK, UpVal, MovedSize, UpdatedC4, RebalancedC5) ->
            ?check_node(
                ?INTERNAL4(
                    K1,
                    K2,
                    K3,
                    UpK,
                    {V1, V2, V3, UpVal},
                    %
                    O1,
                    O2,
                    O3,
                    O4 - MovedSize,
                    %
                    C1,
                    C2,
                    C3,
                    UpdatedC4,
                    RebalancedC5
                )
            );
        %
        ?MERGED(MergedC4C5) ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K3,
                    {V1, V2, V3},
                    %
                    O1,
                    O2,
                    O3,
                    %
                    C1,
                    C2,
                    C3,
                    MergedC4C5
                )
            )
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL3

-compile({inline, delete_internal3/12}).
delete_internal3(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    if
        K > K2 ->
            if
                K > K3 ->
                    delete_internal3_child4(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
                K < K3 ->
                    delete_internal3_child3(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
                true ->
                    delete_internal3_key3(K1, K2, Values, O1, O2, O3, C1, C2, C3, C4)
            end;
        %
        K < K2 ->
            if
                K > K1 ->
                    delete_internal3_child2(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
                K < K1 ->
                    delete_internal3_child1(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
                true ->
                    delete_internal3_key1(K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)
            end;
        %
        true ->
            delete_internal3_key2(K1, K3, Values, O1, O2, O3, C1, C2, C3, C4)
    end.

-compile({inline, [delete_internal3_child1/12]}).
delete_internal3_child1(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    UpdatedC1 = delete_recur(K, C1),

    delete_internal3_rebalance_child1(
        K1,
        K2,
        K3,
        Values,
        O1,
        O2,
        O3,
        UpdatedC1,
        C2,
        C3,
        C4
    ).

-compile({inline, [delete_internal3_child2/12]}).
delete_internal3_child2(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    UpdatedC2 = delete_recur(K, C2),

    delete_internal3_rebalance_child2(
        K1,
        K2,
        K3,
        Values,
        O1,
        O2,
        O3,
        C1,
        UpdatedC2,
        C3,
        C4
    ).

-compile({inline, [delete_internal3_child3/12]}).
delete_internal3_child3(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    UpdatedC3 = delete_recur(K, C3),

    delete_internal3_rebalance_child3(
        K1,
        K2,
        K3,
        Values,
        O1,
        O2,
        O3,
        C1,
        C2,
        UpdatedC3,
        C4
    ).

-compile({inline, [delete_internal3_child4/12]}).
delete_internal3_child4(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    UpdatedC4 = delete_recur(K, C4),

    delete_internal3_rebalance_child4(
        K1,
        K2,
        K3,
        Values,
        O1,
        O2,
        O3,
        C1,
        C2,
        C3,
        UpdatedC4
    ).

%%% Delete - INTERNAL3 - keys in node

-compile({inline, [delete_internal3_key1/10]}).
delete_internal3_key1(K2, K3, PrevValues, O1, O2, O3, C1, PrevC2, C3, C4) ->
    [[ReplacementK | ReplacementV] | C2] = take_smallest_recur(PrevC2),
    K1 = ReplacementK,
    V1 = ReplacementV,
    {_, V2, V3} = PrevValues,
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,

    case C2 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                UpdatedO2 ,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                0,
                O1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            delete_internal3_rebalance_child2_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                %
                C1,
                C3,
                C4
            );
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K1,
                V1,
                C1,
                K2,
                V2,
                C3
            ),

            delete_internal3_rebalance_child2_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                %
                C1,
                C3,
                C4
            );
        %
        %
        _ ->
            Values = {V1, V2, V3},
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K3,
                    Values,
                    %
                    O1,
                    UpdatedO2,
                    UpdatedO3,
                    %
                    C1,
                    C2,
                    C3,
                    C4
                )
            )
    end.

-compile({inline, [delete_internal3_key2/10]}).
delete_internal3_key2(K1, K3, PrevValues, O1, O2, O3, C1, C2, PrevC3, C4) ->
    [[ReplacementK | ReplacementV] | C3] = take_smallest_recur(PrevC3),
    K2 = ReplacementK,
    V2 = ReplacementV,
    {V1, _, V3} = PrevValues,
    UpdatedO3 = O3 - 1,

    case C3 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                UpdatedO3,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K2,
                V2,
                O1,
                O2,
                C2,
                %
                K3,
                V3,
                C4
            ),

            delete_internal3_rebalance_child3_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                O2,
                UpdatedO3,
                %
                C1,
                C2,
                C4
            );
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K2,
                V2,
                C2,
                K3,
                V3,
                C4
            ),

            delete_internal3_rebalance_child3_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                O2,
                UpdatedO3,
                %
                C1,
                C2,
                C4
            );
        %
        %
        _ ->
            Values = {V1, V2, V3},
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K3,
                    Values,
                    %
                    O1,
                    O2,
                    UpdatedO3,
                    %
                    C1,
                    C2,
                    C3,
                    C4
                )
            )
    end.

-compile({inline, [delete_internal3_key3/10]}).
delete_internal3_key3(K1, K2, PrevValues, O1, O2, O3, C1, C2, C3, PrevC4) ->
    [[ReplacementK | ReplacementV] | C4] = take_smallest_recur(PrevC4),
    K3 = ReplacementK,
    V3 = ReplacementV,
    {V1, V2, _} = PrevValues,

    case C4 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_rightmost_internal(
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K3,
                V3,
                O3 - O2,
                C3
            ),

            delete_internal3_rebalance_child4_finish(
                Result,
                K1,
                K2,
                V1,
                V2,
                %
                O1,
                O2,
                O3,
                %
                C1,
                C2
            );
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_rightmost_leaf(
                CK,
                CV,
                K3,
                V3,
                C3
            ),

            delete_internal3_rebalance_child4_finish(
                Result,
                K1,
                K2,
                V1,
                V2,
                %
                O1,
                O2,
                O3,
                %
                C1,
                C2
            );
        %
        %
        _ ->
            Values = {V1, V2, V3},
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K3,
                    Values,
                    %
                    O1,
                    O2,
                    O3,
                    %
                    C1,
                    C2,
                    C3,
                    C4
                )
            )
    end.

%%% Delete - INTERNAL3 - rebalance

-compile({inline, [delete_internal3_rebalance_child1/11]}).
delete_internal3_rebalance_child1(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    UpdatedO1 = O1 - 1,
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,

    case C1 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            {V1, V2, V3} = Values,
            Result = rebalance_leftmost_internal(
                UpdatedO1,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                C2
            ),

            delete_internal3_rebalance_child1_finish(
                Result,
                K2,
                K3,
                V2,
                V3,
                %
                UpdatedO1,
                UpdatedO2,
                UpdatedO3,
                %
                C3,
                C4
            );
        %
        ?LEAF1(CK, CV) ->
            {V1, V2, V3} = Values,
            Result = rebalance_leftmost_leaf(
                CK,
                CV,
                K1,
                V1,
                C2
            ),

            delete_internal3_rebalance_child1_finish(
                Result,
                K2,
                K3,
                V2,
                V3,
                %
                UpdatedO1,
                UpdatedO2,
                UpdatedO3,
                %
                C3,
                C4
            );
        %
        _ ->
            ?check_node(?INTERNAL3(K1, K2, K3, Values, UpdatedO1, UpdatedO2, UpdatedO3, C1, C2, C3, C4))
    end.

-compile({inline, delete_internal3_rebalance_child1_finish/10}).
delete_internal3_rebalance_child1_finish(Result, K2, K3, V2, V3, O1, O2, O3, C3, C4) ->
    case Result of
        ?ROTATED(UpK, UpVal, MovedSize, UpdatedC1, UpdatedC2) ->
            ?INTERNAL3(
                UpK,
                K2,
                K3,
                {UpVal, V2, V3},
                %
                O1 + MovedSize,
                O2,
                O3,
                %
                UpdatedC1,
                UpdatedC2,
                C3,
                C4
            );
        %
        ?MERGED(MergedC1C2) ->
            ?check_node(
                ?INTERNAL2(
                    K2,
                    K3,
                    [V2 | V3],
                    %
                    O2,
                    O3,
                    %
                    MergedC1C2,
                    C3,
                    C4
                )
            )
    end.

%%%

-compile({inline, [delete_internal3_rebalance_child2/11]}).
delete_internal3_rebalance_child2(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,

    case C2 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            {V1, V2, V3} = Values,
            Result = rebalance_internal_from_either_sibling(
                UpdatedO2,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                0,
                O1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            delete_internal3_rebalance_child2_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                %
                C1,
                C3,
                C4
            );
        %
        %
        ?LEAF1(CK, CV) ->
            {V1, V2, V3} = Values,
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K1,
                V1,
                C1,
                K2,
                V2,
                C3
            ),

            delete_internal3_rebalance_child2_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                %
                C1,
                C3,
                C4
            );
        %
        %
        _ ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K3,
                    Values,
                    %
                    O1,
                    UpdatedO2,
                    UpdatedO3,
                    %
                    C1,
                    C2,
                    C3,
                    C4
                )
            )
    end.

-compile({inline, delete_internal3_rebalance_child2_finish/13}).
delete_internal3_rebalance_child2_finish(Result, K1, K2, K3, V1, V2, V3, O1, O2, O3, C1, C3, C4) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC1C2) ->
            ?check_node(
                ?INTERNAL2(
                    K2,
                    K3,
                    [V2 | V3],
                    %
                    O2,
                    O3,
                    %
                    MergedC1C2,
                    C3,
                    C4
                )
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, RebalancedC2, UpdatedC3) ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    UpK,
                    K3,
                    {V1, UpVal, V3},
                    %
                    O1,
                    O2 + MovedSize,
                    O3,
                    %
                    C1,
                    RebalancedC2,
                    UpdatedC3,
                    C4
                )
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedC1, RebalancedC2) ->
            ?check_node(
                ?INTERNAL3(
                    UpK,
                    K2,
                    K3,
                    {UpVal, V2, V3},
                    %
                    O1 - MovedSize,
                    O2,
                    O3,
                    %
                    UpdatedC1,
                    RebalancedC2,
                    C3,
                    C4
                )
            )
    end.

%%%

-compile({inline, [delete_internal3_rebalance_child3/11]}).
delete_internal3_rebalance_child3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    UpdatedO3 = O3 - 1,

    case C3 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            {V1, V2, V3} = Values,
            Result = rebalance_internal_from_either_sibling(
                UpdatedO3,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K2,
                V2,
                O1,
                O2,
                C2,
                %
                K3,
                V3,
                C4
            ),

            delete_internal3_rebalance_child3_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                O2,
                UpdatedO3,
                %
                C1,
                C2,
                C4
            );
        %
        %
        ?LEAF1(CK, CV) ->
            {V1, V2, V3} = Values,
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K2,
                V2,
                C2,
                K3,
                V3,
                C4
            ),

            delete_internal3_rebalance_child3_finish(
                Result,
                K1,
                K2,
                K3,
                V1,
                V2,
                V3,
                %
                O1,
                O2,
                UpdatedO3,
                %
                C1,
                C2,
                C4
            );
        %
        %
        _ ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K3,
                    Values,
                    %
                    O1,
                    O2,
                    UpdatedO3,
                    %
                    C1,
                    C2,
                    C3,
                    C4
                )
            )
    end.

-compile({inline, delete_internal3_rebalance_child3_finish/13}).
delete_internal3_rebalance_child3_finish(Result, K1, K2, K3, V1, V2, V3, O1, O2, O3, C1, C2, C4) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC2C3) ->
            ?check_node(
                ?INTERNAL2(
                    K1,
                    K3,
                    [V1 | V3],
                    %
                    O1,
                    O3,
                    %
                    C1,
                    MergedC2C3,
                    C4
                )
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, RebalancedC3, UpdatedC4) ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    UpK,
                    {V1, V2, UpVal},
                    %
                    O1,
                    O2,
                    O3 + MovedSize,
                    %
                    C1,
                    C2,
                    RebalancedC3,
                    UpdatedC4
                )
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedC2, RebalancedC3) ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    UpK,
                    K3,
                    {V1, UpVal, V3},
                    %
                    O1,
                    O2 - MovedSize,
                    O3,
                    %
                    C1,
                    UpdatedC2,
                    RebalancedC3,
                    C4
                )
            )
    end.

%%%

-compile({inline, [delete_internal3_rebalance_child4/11]}).
delete_internal3_rebalance_child4(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    case C4 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            {V1, V2, V3} = Values,
            Result = rebalance_rightmost_internal(
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K3,
                V3,
                O3 - O2,
                C3
            ),

            delete_internal3_rebalance_child4_finish(Result, K1, K2, V1, V2, O1, O2, O3, C1, C2);
        %
        %
        ?LEAF1(CK, CV) ->
            {V1, V2, V3} = Values,
            Result = rebalance_rightmost_leaf(
                CK,
                CV,
                K3,
                V3,
                C3
            ),

            delete_internal3_rebalance_child4_finish(Result, K1, K2, V1, V2, O1, O2, O3, C1, C2);
        %
        %
        _ ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K3,
                    Values,
                    %
                    O1,
                    O2,
                    O3,
                    %
                    C1,
                    C2,
                    C3,
                    C4
                )
            )
    end.

-compile({inline, delete_internal3_rebalance_child4_finish/10}).
delete_internal3_rebalance_child4_finish(Result, K1, K2, V1, V2, O1, O2, O3, C1, C2) ->
    case Result of
        ?ROTATED(UpK, UpVal, MovedSize, UpdatedC3, RebalancedC4) ->
            ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    UpK,
                    {V1, V2, UpVal},
                    %
                    O1,
                    O2,
                    O3 - MovedSize,
                    %
                    C1,
                    C2,
                    UpdatedC3,
                    RebalancedC4
                )
            );
        %
        ?MERGED(MergedC3C4) ->
            ?check_node(
                ?INTERNAL2(
                    K1,
                    K2,
                    [V1 | V2],
                    %
                    O1,
                    O2,
                    %
                    C1,
                    C2,
                    MergedC3C4
                )
            )
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL2

-compile({inline, delete_internal2/9}).
delete_internal2(K, K1, K2, Values, O1, O2, C1, C2, C3) ->
    if
        K > K1 ->
            if
                K > K2 ->
                    delete_internal2_child3(K, K1, K2, Values, O1, O2, C1, C2, C3);
                K < K2 ->
                    delete_internal2_child2(K, K1, K2, Values, O1, O2, C1, C2, C3);
                true ->
                    delete_internal2_key2(K1, Values, O1, O2, C1, C2, C3)
            end;
        K < K1 ->
            delete_internal2_child1(K, K1, K2, Values, O1, O2, C1, C2, C3);
        true ->
            delete_internal2_key1(K2, Values, O1, O2, C1, C2, C3)
    end.

-compile({inline, [delete_internal2_child1/9]}).
delete_internal2_child1(K, K1, K2, Values, O1, O2, C1, C2, C3) ->
    UpdatedC1 = delete_recur(K, C1),

    delete_internal2_rebalance_child1(
        K1,
        K2,
        Values,
        O1,
        O2,
        UpdatedC1,
        C2,
        C3
    ).

-compile({inline, [delete_internal2_child2/9]}).
delete_internal2_child2(K, K1, K2, Values, O1, O2, C1, C2, C3) ->
    UpdatedC2 = delete_recur(K, C2),

    delete_internal2_rebalance_child2(
        K1,
        K2,
        Values,
        O1,
        O2,
        C1,
        UpdatedC2,
        C3
    ).

-compile({inline, [delete_internal2_child3/9]}).
delete_internal2_child3(K, K1, K2, Values, O1, O2, C1, C2, C3) ->
    UpdatedC3 = delete_recur(K, C3),

    delete_internal2_rebalance_child3(
        K1,
        K2,
        Values,
        O1,
        O2,
        C1,
        C2,
        UpdatedC3
    ).

%%% Delete - INTERNAL2 - keys in node

-compile({inline, [delete_internal2_key1/7]}).
delete_internal2_key1(K2, Values, O1, O2, C1, PrevC2, C3) ->
    [_ | V2] = Values,
    [[ReplacementK | ReplacementV] | C2] = take_smallest_recur(PrevC2),
    K1 = ReplacementK,
    V1 = ReplacementV,
    UpdatedO2 = O2 - 1,

    case C2 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                UpdatedO2,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                0,
                O1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            delete_internal2_rebalance_child2_finish(
                Result,
                K1,
                K2,
                V1,
                V2,
                %
                O1,
                UpdatedO2,
                %
                C1,
                C3
            );
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K1,
                V1,
                C1,
                K2,
                V2,
                C3
            ),

            delete_internal2_rebalance_child2_finish(
                Result,
                K1,
                K2,
                V1,
                V2,
                %
                O1,
                UpdatedO2,
                %
                C1,
                C3
            );
        %
        %
        _ ->
            ?check_node(
                ?INTERNAL2(
                    K1,
                    K2,
                    [V1 | V2],
                    %
                    O1,
                    UpdatedO2,
                    %,
                    C1,
                    C2,
                    C3
                )
            )
    end.

-compile({inline, [delete_internal2_key2/7]}).
delete_internal2_key2(K1, Values, O1, O2, C1, C2, PrevC3) ->
    [V1 | _] = Values,
    [[ReplacementK | ReplacementV] | C3] = take_smallest_recur(PrevC3),
    K2 = ReplacementK,
    V2 = ReplacementV,

    case C3 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_rightmost_internal(
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K2,
                V2,
                O2 - O1,
                C2
            ),

            delete_internal2_rebalance_child3_finish(
                Result,
                K1,
                V1,
                %
                O1,
                O2,
                %
                C1
            );
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_rightmost_leaf(
                CK,
                CV,
                K2,
                V2,
                C2
            ),

            delete_internal2_rebalance_child3_finish(
                Result,
                K1,
                V1,
                %
                O1,
                O2,
                %
                C1
            );
        %
        %
        _ ->
            ?check_node(
                ?INTERNAL2(
                    K1,
                    K2,
                    [V1 | V2],
                    %
                    O1,
                    O2,
                    %
                    C1,
                    C2,
                    C3
                )
            )
    end.

%%% Delete - INTERNAL2 - rebalance

-compile({inline, [delete_internal2_rebalance_child1/8]}).
delete_internal2_rebalance_child1(K1, K2, Values, O1, O2, C1, C2, C3) ->
    UpdatedO1 = O1 - 1,
    UpdatedO2 = O2 - 1,

    case C1 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            [V1 | V2] = Values,
            Result = rebalance_leftmost_internal(
                UpdatedO1,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                C2
            ),

            delete_internal2_rebalance_child1_finish(Result, K2, V2, UpdatedO1, UpdatedO2, C3);
        %
        ?LEAF1(CK, CV) ->
            [V1 | V2] = Values,
            Result = rebalance_leftmost_leaf(
                CK,
                CV,
                K1,
                V1,
                C2
            ),

            delete_internal2_rebalance_child1_finish(Result, K2, V2, UpdatedO1, UpdatedO2, C3);
        %
        _ ->
            ?check_node(?INTERNAL2(K1, K2, Values, UpdatedO1, UpdatedO2, C1, C2, C3))
    end.

-compile({inline, delete_internal2_rebalance_child1_finish/6}).
delete_internal2_rebalance_child1_finish(Result, K2, V2, O1, O2, C3) ->
    case Result of
        ?ROTATED(UpK, UpVal, MovedSize, UpdatedC1, UpdatedC2) ->
            ?check_node(
                ?INTERNAL2(
                    UpK,
                    K2,
                    [UpVal | V2],
                    %
                    O1 + MovedSize,
                    O2,
                    %
                    UpdatedC1,
                    UpdatedC2,
                    C3
                )
            );
        %
        ?MERGED(MergedC1C2) ->
            ?check_node(
                ?INTERNAL1(
                    K2,
                    V2,
                    %
                    O2,
                    %
                    MergedC1C2,
                    C3
                )
            )
    end.

%%%

-compile({inline, [delete_internal2_rebalance_child2/8]}).
delete_internal2_rebalance_child2(K1, K2, Values, O1, O2, C1, C2, C3) ->
    UpdatedO2 = O2 - 1,

    case C2 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            [V1 | V2] = Values,
            Result = rebalance_internal_from_either_sibling(
                UpdatedO2,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                0,
                O1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            delete_internal2_rebalance_child2_finish(
                Result,
                K1,
                K2,
                V1,
                V2,
                %
                O1,
                UpdatedO2,
                %
                C1,
                C3
            );
        %
        %
        ?LEAF1(CK, CV) ->
            [V1 | V2] = Values,
            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                K1,
                V1,
                C1,
                K2,
                V2,
                C3
            ),

            delete_internal2_rebalance_child2_finish(
                Result,
                K1,
                K2,
                V1,
                V2,
                %
                O1,
                UpdatedO2,
                %
                C1,
                C3
            );
        %
        %
        _ ->
            ?check_node(?INTERNAL2(K1, K2, Values, O1, UpdatedO2, C1, C2, C3))
    end.

-compile({inline, delete_internal2_rebalance_child2_finish/9}).
delete_internal2_rebalance_child2_finish(Result, K1, K2, V1, V2, O1, O2, C1, C3) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC1C2) ->
            ?check_node(
                ?INTERNAL1(
                    K2,
                    V2,
                    %
                    O2,
                    %
                    MergedC1C2,
                    C3
                )
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, RebalancedC2, UpdatedC3) ->
            ?check_node(
                ?INTERNAL2(
                    K1,
                    UpK,
                    [V1 | UpVal],
                    %
                    O1,
                    O2 + MovedSize,
                    %
                    C1,
                    RebalancedC2,
                    UpdatedC3
                )
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedC1, RebalancedC2) ->
            ?check_node(
                ?INTERNAL2(
                    UpK,
                    K2,
                    [UpVal | V2],
                    %
                    O1 - MovedSize,
                    O2,
                    %
                    UpdatedC1,
                    RebalancedC2,
                    C3
                )
            )
    end.

%%%

-compile({inline, [delete_internal2_rebalance_child3/8]}).
delete_internal2_rebalance_child3(K1, K2, Values, O1, O2, C1, C2, C3) ->
    case C3 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            [V1 | V2] = Values,
            Result = rebalance_rightmost_internal(
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K2,
                V2,
                O2 - O1,
                C2
            ),

            delete_internal2_rebalance_child3_finish(Result, K1, V1, O1, O2, C1);
        %
        %
        ?LEAF1(CK, CV) ->
            [V1 | V2] = Values,
            Result = rebalance_rightmost_leaf(
                CK,
                CV,
                K2,
                V2,
                C2
            ),

            delete_internal2_rebalance_child3_finish(Result, K1, V1, O1, O2, C1);
        %
        %
        _ ->
            ?check_node(?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3))
    end.

-compile({inline, delete_internal2_rebalance_child3_finish/6}).
delete_internal2_rebalance_child3_finish(Result, K1, V1, O1, O2, C1) ->
    case Result of
        ?ROTATED(UpK, UpVal, MovedSize, UpdatedC2, RebalancedC3) ->
            ?check_node(
                ?INTERNAL2(
                    K1,
                    UpK,
                    [V1 | UpVal],
                    %
                    O1,
                    O2 - MovedSize,
                    %
                    C1,
                    UpdatedC2,
                    RebalancedC3
                )
            );
        %
        ?MERGED(MergedC2C3) ->
            ?check_node(
                ?INTERNAL1(
                    K1,
                    V1,
                    %
                    O1,
                    %
                    C1,
                    MergedC2C3
                )
            )
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL1

-compile({inline, delete_internal1/6}).
delete_internal1(K, K1, V1, O1, C1, C2) ->
    if
        K > K1 ->
            delete_internal1_child2(K, K1, V1, O1, C1, C2);
        K < K1 ->
            delete_internal1_child1(K, K1, V1, O1, C1, C2);
        true ->
            delete_internal1_key1(O1, C1, C2)
    end.

-compile({inline, [delete_internal1_child1/6]}).
delete_internal1_child1(K, K1, V1, O1, C1, C2) ->
    UpdatedC1 = delete_recur(K, C1),

    delete_internal1_rebalance_child1(
        K1,
        V1,
        O1,
        UpdatedC1,
        C2
    ).

-compile({inline, [delete_internal1_child2/6]}).
delete_internal1_child2(K, K1, V1, O1, C1, C2) ->
    UpdatedC2 = delete_recur(K, C2),

    delete_internal1_rebalance_child2(
        K1,
        V1,
        O1,
        C1,
        UpdatedC2
    ).

%%% Delete - INTERNAL1 - key in node

-compile({inline, [delete_internal1_key1/3]}).
delete_internal1_key1(O1, C1, C2) ->
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(C2),

    delete_internal1_rebalance_child2(
        ReplacementK,
        ReplacementV,
        O1,
        C1,
        UpdatedC2
    ).

%%% Delete - INTERNAL1 - rebalance

-compile({inline, [delete_internal1_rebalance_child1/5]}).
delete_internal1_rebalance_child1(K1, V1, O1, C1, C2) ->
    UpdatedO1 = O1 - 1,

    case C1 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_leftmost_internal(
                UpdatedO1,
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                C2
            ),

            delete_internal1_rebalance_child1_finish(Result, UpdatedO1);
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_leftmost_leaf(
                CK,
                CV,
                K1,
                V1,
                C2
            ),

            delete_internal1_rebalance_child1_finish(Result, UpdatedO1);
        %
        _ ->
            ?check_node(?INTERNAL1(K1, V1, UpdatedO1, C1, C2))
    end.

delete_internal1_rebalance_child1_finish(Result, O1) ->
    case Result of
        ?ROTATED(UpK, UpVal, MovedSize, UpdatedC1, UpdatedC2) ->
            ?check_node(?INTERNAL1(UpK, UpVal, O1 + MovedSize, UpdatedC1, UpdatedC2));
        %
        ?MERGED(MergedC1C2) ->
            % This can only happen on root - height is reduced
            ?check_node(MergedC1C2)
    end.

-compile({inline, [delete_internal1_rebalance_child2/5]}).
delete_internal1_rebalance_child2(K1, V1, O1, C1, C2) ->
    case C2 of
        ?INTERNAL1(CK, CV, CLO, CL, CR) ->
            Result = rebalance_rightmost_internal(
                CK,
                CV,
                CLO,
                CL,
                CR,
                %
                K1,
                V1,
                O1,
                C1
            ),

            delete_internal1_rebalance_child2_finish(Result, O1);
        %
        %
        ?LEAF1(CK, CV) ->
            Result = rebalance_rightmost_leaf(
                CK,
                CV,
                K1,
                V1,
                C1
            ),

            delete_internal1_rebalance_child2_finish(Result, O1);
        %
        %
        _ ->
            ?check_node(?INTERNAL1(K1, V1, O1, C1, C2))
    end.

delete_internal1_rebalance_child2_finish(Result, O1) ->
    case Result of
        ?ROTATED(UpK, UpVal, MovedSize, UpdatedC1, UpdatedC2) ->
            ?check_node(?INTERNAL1(UpK, UpVal, O1 - MovedSize, UpdatedC1, UpdatedC2));
        %
        ?MERGED(MergedC1C2) ->
            % This can only happen on root - height is reduced
            ?check_node(MergedC1C2)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - LEAF4

-compile({inline, delete_leaf4/9}).
delete_leaf4(K, K1, K2, K3, K4, V1, V2, V3, V4) ->
    if
        K > K2 ->
            if
                K == K3 ->
                    ?LEAF3(K1, K2, K4, V1, V2, V4);
                K == K4 ->
                    ?LEAF3(K1, K2, K3, V1, V2, V3);
                true ->
                    error_badkey(K)
            end;
        K == K2 ->
            ?LEAF3(K1, K3, K4, V1, V3, V4);
        K == K1 ->
            ?LEAF3(K2, K3, K4, V2, V3, V4);
        true ->
            error_badkey(K)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - LEAF3

-compile({inline, delete_leaf3/7}).
delete_leaf3(K, K1, K2, K3, V1, V2, V3) ->
    if
        K < K2 ->
            if
                K == K1 ->
                    ?LEAF2(K2, K3, V2, V3);
                true ->
                    error_badkey(K)
            end;
        K > K2 ->
            if
                K == K3 ->
                    ?LEAF2(K1, K2, V1, V2);
                true ->
                    error_badkey(K)
            end;
        true ->
            ?LEAF2(K1, K3, V1, V3)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - LEAF2

-compile({inline, delete_leaf2/5}).
delete_leaf2(K, K1, K2, V1, V2) ->
    if
        K == K1 ->
            ?LEAF1(K2, V2);
        K == K2 ->
            ?LEAF1(K1, V1);
        true ->
            error_badkey(K)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - LEAF1

-compile({inline, delete_leaf1/2}).
delete_leaf1(K, K1) ->
    if
        K == K1 ->
            ?LEAF0;
        true ->
            error_badkey(K)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - Smallest
%% ------------------------------------------------------------------

-spec root_take_smallest(non_empty_node(Key, Value)) -> take_result(Key, Value).
-compile({inline, root_take_smallest/1}).
root_take_smallest(?INTERNAL1(K1, V1, O1, C1, C2)) ->
    [TakenPair | UpdatedC1] = take_smallest_recur(C1),

    [
        TakenPair
        | delete_internal1_rebalance_child1(
            K1,
            V1,
            O1,
            UpdatedC1,
            C2
        )
    ];
root_take_smallest(?LEAF1(K1, V1)) ->
    [[K1 | V1] | ?LEAF0];
root_take_smallest(?LEAF0) ->
    error_empty_tree();
root_take_smallest(Node) ->
    take_smallest_recur(Node).

-spec take_smallest_recur(deep_node(Key, Value)) -> take_result_before_rebalance(Key, Value).
take_smallest_recur(?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3)) ->
    [TakenPair | UpdatedC1] = take_smallest_recur(C1),

    [
        TakenPair
        | delete_internal2_rebalance_child1(
            K1,
            K2,
            Values,
            O1,
            O2,
            UpdatedC1,
            C2,
            C3
        )
    ];
take_smallest_recur(?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)) ->
    [TakenPair | UpdatedC1] = take_smallest_recur(C1),

    [
        TakenPair
        | delete_internal3_rebalance_child1(
            K1,
            K2,
            K3,
            Values,
            O1,
            O2,
            O3,
            UpdatedC1,
            C2,
            C3,
            C4
        )
    ];
take_smallest_recur(?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)) ->
    [TakenPair | UpdatedC1] = take_smallest_recur(C1),

    [
        TakenPair
        | delete_internal4_rebalance_child1(
            K1,
            K2,
            K3,
            K4,
            Values,
            O1,
            O2,
            O3,
            O4,
            UpdatedC1,
            C2,
            C3,
            C4,
            C5
        )
    ];
take_smallest_recur(?LEAF2(K1, K2, V1, V2)) ->
    [[K1 | V1] | ?LEAF1(K2, V2)];
take_smallest_recur(?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    [[K1 | V1] | ?LEAF2(K2, K3, V2, V3)];
take_smallest_recur(?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    [[K1 | V1] | ?LEAF3(K2, K3, K4, V2, V3, V4)].

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - Largest
%% ------------------------------------------------------------------

-spec root_take_largest(non_empty_node(Key, Value)) -> take_result(Key, Value).
-compile({inline, root_take_largest/1}).
root_take_largest(?INTERNAL1(K1, V1, O1, C1, C2)) ->
    [TakenPair | UpdatedC2] = take_largest_recur(C2),

    [
        TakenPair
        | delete_internal1_rebalance_child2(
            K1,
            V1,
            O1,
            C1,
            UpdatedC2
        )
    ];
root_take_largest(?LEAF1(K1, V1)) ->
    [[K1 | V1] | ?LEAF0];
root_take_largest(?LEAF0) ->
    error_empty_tree();
root_take_largest(Node) ->
    take_largest_recur(Node).

-spec take_largest_recur(deep_node(Key, Value)) -> take_result_before_rebalance(Key, Value).
take_largest_recur(?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3)) ->
    [TakenPair | UpdatedC3] = take_largest_recur(C3),

    [
        TakenPair
        | delete_internal2_rebalance_child3(
            K1,
            K2,
            Values,
            O1,
            O2,
            C1,
            C2,
            UpdatedC3
        )
    ];
take_largest_recur(?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)) ->
    [TakenPair | UpdatedC4] = take_largest_recur(C4),

    [
        TakenPair
        | delete_internal3_rebalance_child4(
            K1,
            K2,
            K3,
            Values,
            O1,
            O2,
            O3,
            C1,
            C2,
            C3,
            UpdatedC4
        )
    ];
take_largest_recur(?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)) ->
    [TakenPair | UpdatedC5] = take_largest_recur(C5),

    [
        TakenPair
        | delete_internal4_rebalance_child5(
            K1,
            K2,
            K3,
            K4,
            Values,
            O1,
            O2,
            O3,
            O4,
            C1,
            C2,
            C3,
            C4,
            UpdatedC5
        )
    ];
take_largest_recur(?LEAF2(K1, K2, V1, V2)) ->
    [[K2 | V2] | ?LEAF1(K1, V1)];
take_largest_recur(?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    [[K3 | V3] | ?LEAF2(K1, K2, V1, V2)];
take_largest_recur(?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    [[K4 | V4] | ?LEAF3(K1, K2, K3, V1, V2, V3)].

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - Key
%% ------------------------------------------------------------------

-spec root_take(Key, non_empty_node(Key, Value)) -> take_result(Key, Value).
root_take(K, ?INTERNAL1(K1, V1, O1, C1, C2)) ->
    take_internal1(K, K1, V1, O1, C1, C2);
root_take(K, ?LEAF1(K1, V1)) ->
    take_leaf1(K, K1, V1);
root_take(K, ?LEAF0) ->
    error_badkey(K);
root_take(K, Root) ->
    take_recur(K, Root).

-spec take_recur(Key, deep_node(Key, Value)) -> take_result_before_rebalance(Key, Value).
take_recur(K, ?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3)) ->
    take_internal2(K, K1, K2, Values, O1, O2, C1, C2, C3);
take_recur(K, ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)) ->
    take_internal3(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
take_recur(K, ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)) ->
    take_internal4(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5);
take_recur(K, ?LEAF2(K1, K2, V1, V2)) ->
    take_leaf2(K, K1, K2, V1, V2);
take_recur(K, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    take_leaf3(K, K1, K2, K3, V1, V2, V3);
take_recur(K, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    take_leaf4(K, K1, K2, K3, K4, V1, V2, V3, V4).

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL4

-compile({inline, take_internal4/15}).
take_internal4(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    if
        K > K2 ->
            if
                K < K4 ->
                    if
                        K > K3 ->
                            take_internal4_child4(
                                K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                            );
                        K < K3 ->
                            take_internal4_child3(
                                K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                            );
                        true ->
                            take_internal4_key3(
                                K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                            )
                    end;
                K > K4 ->
                    take_internal4_child5(
                        K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                    );
                true ->
                    take_internal4_key4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)
            end;
        K < K2 ->
            if
                K > K1 ->
                    take_internal4_child2(
                        K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                    );
                K < K1 ->
                    take_internal4_child1(
                        K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5
                    );
                true ->
                    take_internal4_key1(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)
            end;
        true ->
            take_internal4_key2(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)
    end.

-compile({inline, [take_internal4_child1/15]}).
take_internal4_child1(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC1] = take_recur(K, C1),

    [
        TakenPair
        | delete_internal4_rebalance_child1(
            K1,
            K2,
            K3,
            K4,
            Values,
            O1,
            O2,
            O3,
            O4,
            UpdatedC1,
            C2,
            C3,
            C4,
            C5
        )
    ].

-compile({inline, [take_internal4_child2/15]}).
take_internal4_child2(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC2] = take_recur(K, C2),

    [
        TakenPair
        | delete_internal4_rebalance_child2(
            K1,
            K2,
            K3,
            K4,
            Values,
            O1,
            O2,
            O3,
            O4,
            C1,
            UpdatedC2,
            C3,
            C4,
            C5
        )
    ].

-compile({inline, [take_internal4_child3/15]}).
take_internal4_child3(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC3] = take_recur(K, C3),

    [
        TakenPair
        | delete_internal4_rebalance_child3(
            K1,
            K2,
            K3,
            K4,
            Values,
            O1,
            O2,
            O3,
            O4,
            C1,
            C2,
            UpdatedC3,
            C4,
            C5
        )
    ].

-compile({inline, [take_internal4_child4/15]}).
take_internal4_child4(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC4] = take_recur(K, C4),

    [
        TakenPair
        | delete_internal4_rebalance_child4(
            K1,
            K2,
            K3,
            K4,
            Values,
            O1,
            O2,
            O3,
            O4,
            C1,
            C2,
            C3,
            UpdatedC4,
            C5
        )
    ].

-compile({inline, [take_internal4_child5/15]}).
take_internal4_child5(K, K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC5] = take_recur(K, C5),

    [
        TakenPair
        | delete_internal4_rebalance_child5(
            K1,
            K2,
            K3,
            K4,
            Values,
            O1,
            O2,
            O3,
            O4,
            C1,
            C2,
            C3,
            C4,
            UpdatedC5
        )
    ].

%%% Take - INTERNAL4 - keys in node

-compile({inline, [take_internal4_key1/14]}).
take_internal4_key1(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    TakenPair = [K1 | element(1, Values)],
    [TakenPair | delete_internal4_key1(K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)].

-compile({inline, [take_internal4_key2/14]}).
take_internal4_key2(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    TakenPair = [K2 | element(2, Values)],
    [TakenPair | delete_internal4_key2(K1, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)].

-compile({inline, [take_internal4_key3/14]}).
take_internal4_key3(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    TakenPair = [K3 | element(3, Values)],
    [TakenPair | delete_internal4_key3(K1, K2, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)].

-compile({inline, [take_internal4_key4/14]}).
take_internal4_key4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
    TakenPair = [K4 | element(4, Values)],
    [TakenPair | delete_internal4_key4(K1, K2, K3, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5)].

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL3

-compile({inline, take_internal3/12}).
take_internal3(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    if
        K > K2 ->
            if
                K > K3 ->
                    take_internal3_child4(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
                K < K3 ->
                    take_internal3_child3(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
                true ->
                    take_internal3_key3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)
            end;
        K < K2 ->
            if
                K > K1 ->
                    take_internal3_child2(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
                K < K1 ->
                    take_internal3_child1(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4);
                true ->
                    take_internal3_key1(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)
            end;
        true ->
            take_internal3_key2(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)
    end.

-compile({inline, [take_internal3_child1/12]}).
take_internal3_child1(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC1] = take_recur(K, C1),

    [
        TakenPair
        | delete_internal3_rebalance_child1(
            K1,
            K2,
            K3,
            Values,
            O1,
            O2,
            O3,
            UpdatedC1,
            C2,
            C3,
            C4
        )
    ].

-compile({inline, [take_internal3_child2/12]}).
take_internal3_child2(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC2] = take_recur(K, C2),

    [
        TakenPair
        | delete_internal3_rebalance_child2(
            K1,
            K2,
            K3,
            Values,
            O1,
            O2,
            O3,
            C1,
            UpdatedC2,
            C3,
            C4
        )
    ].

-compile({inline, [take_internal3_child3/12]}).
take_internal3_child3(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC3] = take_recur(K, C3),

    [
        TakenPair
        | delete_internal3_rebalance_child3(
            K1,
            K2,
            K3,
            Values,
            O1,
            O2,
            O3,
            C1,
            C2,
            UpdatedC3,
            C4
        )
    ].

-compile({inline, [take_internal3_child4/12]}).
take_internal3_child4(K, K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC4] = take_recur(K, C4),

    [
        TakenPair
        | delete_internal3_rebalance_child4(
            K1,
            K2,
            K3,
            Values,
            O1,
            O2,
            O3,
            C1,
            C2,
            C3,
            UpdatedC4
        )
    ].

%%% Take - INTERNAL3 - keys in node

-compile({inline, [take_internal3_key1/11]}).
take_internal3_key1(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    TakenPair = [K1 | element(1, Values)],
    [TakenPair | delete_internal3_key1(K2, K3, Values, O1, O2, O3, C1, C2, C3, C4)].

-compile({inline, [take_internal3_key2/11]}).
take_internal3_key2(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    TakenPair = [K2 | element(2, Values)],
    [TakenPair | delete_internal3_key2(K1, K3, Values, O1, O2, O3, C1, C2, C3, C4)].

-compile({inline, [take_internal3_key3/11]}).
take_internal3_key3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
    TakenPair = [K3 | element(3, Values)],
    [TakenPair | delete_internal3_key3(K1, K2, Values, O1, O2, O3, C1, C2, C3, C4)].

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL2

-compile({inline, take_internal2/9}).
take_internal2(K, K1, K2, Values, O1, O2, C1, C2, C3) ->
    if
        K > K1 ->
            if
                K > K2 ->
                    take_internal2_child3(K, K1, K2, Values, O1, O2, C1, C2, C3);
                K < K2 ->
                    take_internal2_child2(K, K1, K2, Values, O1, O2, C1, C2, C3);
                true ->
                    take_internal2_key2(K1, K2, Values, O1, O2, C1, C2, C3)
            end;
        K < K1 ->
            take_internal2_child1(K, K1, K2, Values, O1, O2, C1, C2, C3);
        true ->
            take_internal2_key1(K1, K2, Values, O1, O2, C1, C2, C3)
    end.

-compile({inline, [take_internal2_child1/9]}).
take_internal2_child1(K, K1, K2, Values, O1, O2, C1, C2, C3) ->
    [TakenPair | UpdatedC1] = take_recur(K, C1),

    [
        TakenPair
        | delete_internal2_rebalance_child1(
            K1,
            K2,
            Values,
            O1,
            O2,
            UpdatedC1,
            C2,
            C3
        )
    ].

-compile({inline, [take_internal2_child2/9]}).
take_internal2_child2(K, K1, K2, Values, O1, O2, C1, C2, C3) ->
    [TakenPair | UpdatedC2] = take_recur(K, C2),

    [
        TakenPair
        | delete_internal2_rebalance_child2(
            K1,
            K2,
            Values,
            O1,
            O2,
            C1,
            UpdatedC2,
            C3
        )
    ].

-compile({inline, [take_internal2_child3/9]}).
take_internal2_child3(K, K1, K2, Values, O1, O2, C1, C2, C3) ->
    [TakenPair | UpdatedC3] = take_recur(K, C3),

    [
        TakenPair
        | delete_internal2_rebalance_child3(
            K1,
            K2,
            Values,
            O1,
            O2,
            C1,
            C2,
            UpdatedC3
        )
    ].

%%% Take - INTERNAL2 - keys in node

-compile({inline, [take_internal2_key1/8]}).
take_internal2_key1(K1, K2, Values, O1, O2, C1, C2, C3) ->
    TakenPair = [K1 | hd(Values)],
    [TakenPair | delete_internal2_key1(K2, Values, O1, O2, C1, C2, C3)].

-compile({inline, [take_internal2_key2/8]}).
take_internal2_key2(K1, K2, Values, O1, O2, C1, C2, C3) ->
    TakenPair = [K2 | tl(Values)],
    [TakenPair | delete_internal2_key2(K1, Values, O1, O2, C1, C2, C3)].

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL1

-compile({inline, take_internal1/6}).
take_internal1(K, K1, V1, O1, C1, C2) ->
    if
        K > K1 ->
            take_internal1_child2(K, K1, V1, O1, C1, C2);
        K < K1 ->
            take_internal1_child1(K, K1, V1, O1, C1, C2);
        true ->
            take_internal1_key1(K1, V1, O1, C1, C2)
    end.

-compile({inline, [take_internal1_child1/6]}).
take_internal1_child1(K, K1, V1, O1, C1, C2) ->
    [TakenPair | UpdatedC1] = take_recur(K, C1),

    [
        TakenPair
        | delete_internal1_rebalance_child1(
            K1,
            V1,
            O1,
            UpdatedC1,
            C2
        )
    ].

-compile({inline, [take_internal1_child2/6]}).
take_internal1_child2(K, K1, V1, O1, C1, C2) ->
    [TakenPair | UpdatedC2] = take_recur(K, C2),

    [
        TakenPair
        | delete_internal1_rebalance_child2(
            K1,
            V1,
            O1,
            C1,
            UpdatedC2
        )
    ].

%%% Take - INTERNAL1 - key in node

-compile({inline, [take_internal1_key1/5]}).
take_internal1_key1(K1, V1, O1, C1, C2) ->
    TakenPair = [K1 | V1],
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(C2),

    [
        TakenPair
        | delete_internal1_rebalance_child2(
            ReplacementK,
            ReplacementV,
            O1,
            C1,
            UpdatedC2
        )
    ].

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF4

-compile({inline, take_leaf4/9}).
take_leaf4(K, K1, K2, K3, K4, V1, V2, V3, V4) ->
    if
        K > K2 ->
            if
                K == K3 ->
                    [[K3 | V3] | ?LEAF3(K1, K2, K4, V1, V2, V4)];
                K == K4 ->
                    [[K4 | V4] | ?LEAF3(K1, K2, K3, V1, V2, V3)];
                true ->
                    error_badkey(K)
            end;
        K == K2 ->
            [[K2 | V2] | ?LEAF3(K1, K3, K4, V1, V3, V4)];
        K == K1 ->
            [[K1 | V1] | ?LEAF3(K2, K3, K4, V2, V3, V4)];
        true ->
            error_badkey(K)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF3

-compile({inline, take_leaf3/7}).
take_leaf3(K, K1, K2, K3, V1, V2, V3) ->
    if
        K < K2 ->
            if
                K == K1 ->
                    [[K1 | V1] | ?LEAF2(K2, K3, V2, V3)];
                true ->
                    error_badkey(K)
            end;
        K > K2 ->
            if
                K == K3 ->
                    [[K3 | V3] | ?LEAF2(K1, K2, V1, V2)];
                true ->
                    error_badkey(K)
            end;
        true ->
            [[K2 | V2] | ?LEAF2(K1, K3, V1, V3)]
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF2

-compile({inline, take_leaf2/5}).
take_leaf2(K, K1, K2, V1, V2) ->
    if
        K == K1 ->
            [[K1 | V1] | ?LEAF1(K2, V2)];
        K == K2 ->
            [[K2 | V2] | ?LEAF1(K1, V1)];
        true ->
            error_badkey(K)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF1

-compile({inline, take_leaf1/3}).
take_leaf1(K, K1, V1) ->
    if
        K == K1 ->
            [[K1 | V1] | ?LEAF0];
        true ->
            error_badkey(K)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Statistics for Validation
%% ------------------------------------------------------------------

stats(Root, ExpectedNrOfKeys) ->
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
            stats_recur(Node, Depth, ExpectedNrOfKeys, Acc1)
    end.

stats_recur(
    ?INTERNAL4(_, _, _, _, _, O1, O2, O3, O4, C1, C2, C3, C4, C5), Depth, ExpectedNrOfKeys, Acc
) ->
    S1 = O1 - 1,
    S2 = O2 - O1 - 1,
    S3 = O3 - O2 - 1,
    S4 = O4 - O3 - 1,
    S5 = ExpectedNrOfKeys - O4,

    Acc2 = stats_inc_node_count(Acc, internal4),
    Acc3 = stats_recur(C1, Depth + 1, S1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, S2, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, S3, Acc4),
    Acc6 = stats_recur(C4, Depth + 1, S4, Acc5),
    Acc7 = stats_recur(C5, Depth + 1, S5, Acc6),
    Acc7;
stats_recur(?INTERNAL3(_, _, _, _, O1, O2, O3, C1, C2, C3, C4), Depth, ExpectedNrOfKeys, Acc) ->
    S1 = O1 - 1,
    S2 = O2 - O1 - 1,
    S3 = O3 - O2 - 1,
    S4 = ExpectedNrOfKeys - O3,

    Acc2 = stats_inc_node_count(Acc, internal3),
    Acc3 = stats_recur(C1, Depth + 1, S1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, S2, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, S3, Acc4),
    Acc6 = stats_recur(C4, Depth + 1, S4, Acc5),
    Acc6;
stats_recur(?INTERNAL2(_, _, _, O1, O2, C1, C2, C3), Depth, ExpectedNrOfKeys, Acc) ->
    S1 = O1 - 1,
    S2 = O2 - O1 - 1,
    S3 = ExpectedNrOfKeys - O2,

    Acc2 = stats_inc_node_count(Acc, internal2),
    Acc3 = stats_recur(C1, Depth + 1, S1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, S2, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, S3, Acc4),
    Acc5;
stats_recur(?INTERNAL1(_, _, O1, C1, C2), Depth, ExpectedNrOfKeys, Acc) ->
    S1 = O1 - 1,
    S2 = ExpectedNrOfKeys - O1,

    Acc2 =
        case Depth of
            1 -> Acc;
            _ -> stats_inc_wrong_depth_count(Acc, {internal1, Depth})
        end,
    Acc3 = stats_inc_node_count(Acc2, internal1),
    Acc4 = stats_recur(C1, Depth + 1, S1, Acc3),
    Acc5 = stats_recur(C2, Depth + 1, S2, Acc4),
    Acc5;
stats_recur(?LEAF4(_, _, _, _, _, _, _, _), Depth, ExpectedNrOfKeys, Acc) ->
    4 = ExpectedNrOfKeys,

    Acc2 = stats_inc_node_count(Acc, leaf4),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF3(_, _, _, _, _, _), Depth, ExpectedNrOfKeys, Acc) ->
    3 = ExpectedNrOfKeys,

    Acc2 = stats_inc_node_count(Acc, leaf3),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF2(_, _, _, _), Depth, ExpectedNrOfKeys, Acc) ->
    2 = ExpectedNrOfKeys,

    Acc2 = stats_inc_node_count(Acc, leaf2),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF1(_, _), Depth, ExpectedNrOfKeys, Acc) ->
    1 = ExpectedNrOfKeys,

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
foldl_recur(Fun, Acc, ?INTERNAL2(K1, K2, [V1 | V2], _, _, C1, C2, C3)) ->
    Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
    Acc3 = Fun(K2, V2, foldl_recur(Fun, Acc2, C2)),
    foldl_recur(Fun, Acc3, C3);
foldl_recur(Fun, Acc, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, _, _, C1, C2, C3, C4)) ->
    Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
    Acc3 = Fun(K2, V2, foldl_recur(Fun, Acc2, C2)),
    Acc4 = Fun(K3, V3, foldl_recur(Fun, Acc3, C3)),
    foldl_recur(Fun, Acc4, C4);
foldl_recur(Fun, Acc, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, _, _, _, C1, C2, C3, C4, C5)) ->
    Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
    Acc3 = Fun(K2, V2, foldl_recur(Fun, Acc2, C2)),
    Acc4 = Fun(K3, V3, foldl_recur(Fun, Acc3, C3)),
    Acc5 = Fun(K4, V4, foldl_recur(Fun, Acc4, C4)),
    foldl_recur(Fun, Acc5, C5);
foldl_recur(Fun, Acc, ?LEAF2(K1, K2, V1, V2)) ->
    Acc2 = Fun(K1, V1, Acc),
    Acc3 = Fun(K2, V2, Acc2),
    Acc3;
foldl_recur(Fun, Acc, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    Acc2 = Fun(K1, V1, Acc),
    Acc3 = Fun(K2, V2, Acc2),
    Acc4 = Fun(K3, V3, Acc3),
    Acc4;
foldl_recur(Fun, Acc, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    Acc2 = Fun(K1, V1, Acc),
    Acc3 = Fun(K2, V2, Acc2),
    Acc4 = Fun(K3, V3, Acc3),
    Acc5 = Fun(K4, V4, Acc4),
    Acc5.

-spec foldr_recur(fun((Key, Value, Acc1) -> Acc2), Acc0, deep_node(Key, Value)) -> AccN when
    AccN :: Acc2, Acc2 :: Acc1, Acc1 :: Acc0.
-dialyzer({no_underspecs, foldr_recur/3}).
foldr_recur(Fun, Acc, ?INTERNAL2(K1, K2, [V1 | V2], _, _, C1, C2, C3)) ->
    Acc2 = Fun(K2, V2, foldr_recur(Fun, Acc, C3)),
    Acc3 = Fun(K1, V1, foldr_recur(Fun, Acc2, C2)),
    foldr_recur(Fun, Acc3, C1);
foldr_recur(Fun, Acc, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, _, _, C1, C2, C3, C4)) ->
    Acc2 = Fun(K3, V3, foldr_recur(Fun, Acc, C4)),
    Acc3 = Fun(K2, V2, foldr_recur(Fun, Acc2, C3)),
    Acc4 = Fun(K1, V1, foldr_recur(Fun, Acc3, C2)),
    foldr_recur(Fun, Acc4, C1);
foldr_recur(Fun, Acc, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, _, _, _, C1, C2, C3, C4, C5)) ->
    Acc2 = Fun(K4, V4, foldr_recur(Fun, Acc, C5)),
    Acc3 = Fun(K3, V3, foldr_recur(Fun, Acc2, C4)),
    Acc4 = Fun(K2, V2, foldr_recur(Fun, Acc3, C3)),
    Acc5 = Fun(K1, V1, foldr_recur(Fun, Acc4, C2)),
    foldr_recur(Fun, Acc5, C1);
foldr_recur(Fun, Acc, ?LEAF2(K1, K2, V1, V2)) ->
    Acc2 = Fun(K2, V2, Acc),
    Acc3 = Fun(K1, V1, Acc2),
    Acc3;
foldr_recur(Fun, Acc, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    Acc2 = Fun(K3, V3, Acc),
    Acc3 = Fun(K2, V2, Acc2),
    Acc4 = Fun(K1, V1, Acc3),
    Acc4;
foldr_recur(Fun, Acc, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    Acc2 = Fun(K4, V4, Acc),
    Acc3 = Fun(K3, V3, Acc2),
    Acc4 = Fun(K2, V2, Acc3),
    Acc5 = Fun(K1, V1, Acc4),
    Acc5.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Iterator steps, ordered
%% ------------------------------------------------------------------

-spec iterator_steps_l(t(Key, Value)) -> [iterator_step(Key, Value)].
iterator_steps_l(?INTERNAL1(K1, V1, _, C1, C2)) ->
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
        ?INTERNAL2(K1, K2, [V1 | V2], _, _, C1, C2, C3) ->
            iterator_steps_l_recur(C1, [{K1, V1, C2}, {K2, V2, C3} | Acc]);
        %
        ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, _, _, C1, C2, C3, C4) ->
            iterator_steps_l_recur(C1, [{K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4} | Acc]);
        %
        ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, _, _, _, C1, C2, C3, C4, C5) ->
            iterator_steps_l_recur(C1, [
                {K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc
            ]);
        %
        ?LEAF2(K1, K2, V1, V2) ->
            [{K1, V1}, {K2, V2} | Acc];
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            [{K1, V1}, {K2, V2}, {K3, V3} | Acc];
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4} | Acc]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Iterator steps, ordered from
%% ------------------------------------------------------------------

-spec iterator_steps_l_from(Key, t(Key, Value)) -> [iterator_step(Key, Value)].
iterator_steps_l_from(Key, ?INTERNAL1(K1, V1, _, C1, C2)) ->
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
iterator_steps_l_from_recur(Key, Node, Acc) ->
    case Node of
        ?INTERNAL2(K1, K2, Values, _, _, C1, C2, C3) ->
            iterator_steps_l_from_recur_internal2(Key, K1, K2, Values, C1, C2, C3, Acc);
        %
        ?INTERNAL3(K1, K2, K3, Values, _, _, _, C1, C2, C3, C4) ->
            iterator_steps_l_from_recur_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4, Acc);
        %
        ?INTERNAL4(K1, K2, K3, K4, Values, _, _, _, _, C1, C2, C3, C4, C5) ->
            iterator_steps_l_from_recur_internal4(
                Key,
                K1,
                K2,
                K3,
                K4,
                Values,
                C1,
                C2,
                C3,
                C4,
                C5,
                Acc
            );
        %
        ?LEAF2(K1, K2, V1, V2) ->
            iterator_steps_l_from_recur_leaf2(Key, K1, K2, V1, V2, Acc);
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            iterator_steps_l_from_recur_leaf3(Key, K1, K2, K3, V1, V2, V3, Acc);
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            iterator_steps_l_from_recur_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4, Acc)
    end.

%%%

-compile({inline, iterator_steps_l_from_recur_internal4/12}).
iterator_steps_l_from_recur_internal4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5, Acc) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    {V1, V2, V3, V4} = Values,
                    iterator_steps_l_from_recur(Key, C1, [
                        {K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc
                    ]);
                %
                Key > K1 ->
                    {_, V2, V3, V4} = Values,
                    iterator_steps_l_from_recur(Key, C2, [
                        {K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc
                    ]);
                %
                true ->
                    {V1, V2, V3, V4} = Values,
                    [{K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc]
            end;
        %
        Key > K2 ->
            if
                Key < K3 ->
                    {_, _, V3, V4} = Values,
                    iterator_steps_l_from_recur(Key, C3, [{K3, V3, C4}, {K4, V4, C5} | Acc]);
                %
                Key > K3 ->
                    if
                        Key < K4 ->
                            V4 = element(4, Values),
                            iterator_steps_l_from_recur(Key, C4, [{K4, V4, C5} | Acc]);
                        Key > K4 ->
                            iterator_steps_l_from_recur(Key, C5, Acc);
                        true ->
                            V4 = element(4, Values),
                            [{K4, V4, C5} | Acc]
                    end;
                %
                true ->
                    {_, _, V3, V4} = Values,
                    [{K3, V3, C4}, {K4, V4, C5} | Acc]
            end;
        %
        true ->
            {_, V2, V3, V4} = Values,
            [{K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc]
    end.

-compile({inline, iterator_steps_l_from_recur_internal3/10}).
iterator_steps_l_from_recur_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4, Acc) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    {V1, V2, V3} = Values,
                    iterator_steps_l_from_recur(Key, C1, [
                        {K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4} | Acc
                    ]);
                %
                Key > K1 ->
                    {_, V2, V3} = Values,
                    iterator_steps_l_from_recur(Key, C2, [{K2, V2, C3}, {K3, V3, C4} | Acc]);
                %
                true ->
                    {V1, V2, V3} = Values,
                    [{K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4} | Acc]
            end;
        %
        Key > K2 ->
            if
                Key < K3 ->
                    V3 = element(3, Values),
                    iterator_steps_l_from_recur(Key, C3, [{K3, V3, C4} | Acc]);
                %
                Key > K3 ->
                    iterator_steps_l_from_recur(Key, C4, Acc);
                %
                true ->
                    V3 = element(3, Values),
                    [{K3, V3, C4} | Acc]
            end;
        %
        true ->
            {_, V2, V3} = Values,
            [{K2, V2, C3}, {K3, V3, C4} | Acc]
    end.

-compile({inline, iterator_steps_l_from_recur_internal2/8}).
iterator_steps_l_from_recur_internal2(Key, K1, K2, Values, C1, C2, C3, Acc) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    [V1 | V2] = Values,
                    iterator_steps_l_from_recur(Key, C1, [{K1, V1, C2}, {K2, V2, C3} | Acc]);
                %
                Key > K1 ->
                    V2 = tl(Values),
                    iterator_steps_l_from_recur(Key, C2, [{K2, V2, C3} | Acc]);
                %
                true ->
                    [V1 | V2] = Values,
                    [{K1, V1, C2}, {K2, V2, C3} | Acc]
            end;
        %
        Key > K2 ->
            iterator_steps_l_from_recur(Key, C3, Acc);
        %
        true ->
            V2 = tl(Values),
            [{K2, V2, C3} | Acc]
    end.

-compile({inline, iterator_steps_l_from_recur_leaf4/10}).
iterator_steps_l_from_recur_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4, Acc) ->
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
                %
                true ->
                    [{K3, V3}, {K4, V4} | Acc]
            end;
        true ->
            if
                Key > K1 ->
                    [{K2, V2}, {K3, V3}, {K4, V4} | Acc];
                %
                true ->
                    [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4} | Acc]
            end
    end.

-compile({inline, iterator_steps_l_from_recur_leaf3/8}).
iterator_steps_l_from_recur_leaf3(Key, K1, K2, K3, V1, V2, V3, Acc) ->
    if
        Key > K2 ->
            if
                Key > K3 ->
                    Acc;
                true ->
                    [{K3, V3} | Acc]
            end;
        %
        true ->
            if
                Key > K1 ->
                    [{K2, V2}, {K3, V3} | Acc];
                true ->
                    [{K1, V1}, {K2, V2}, {K3, V3} | Acc]
            end
    end.

-compile({inline, iterator_steps_l_from_recur_leaf2/6}).
iterator_steps_l_from_recur_leaf2(Key, K1, K2, V1, V2, Acc) ->
    if
        Key > K2 ->
            Acc;
        %
        Key > K1 ->
            [{K2, V2} | Acc];
        %
        true ->
            [{K1, V1}, {K2, V2} | Acc]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Iterator steps, reversed
%% ------------------------------------------------------------------

-spec iterator_steps_r(t(Key, Value)) -> [iterator_step(Key, Value)].
iterator_steps_r(?INTERNAL1(K1, V1, _, C1, C2)) ->
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
        ?INTERNAL2(K1, K2, [V1 | V2], _, _, C1, C2, C3) ->
            iterator_steps_r_recur(C3, [{K2, V2, C2}, {K1, V1, C1} | Acc]);
        %
        ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, _, _, C1, C2, C3, C4) ->
            iterator_steps_r_recur(C4, [{K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc]);
        %
        ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, _, _, _, C1, C2, C3, C4, C5) ->
            iterator_steps_r_recur(C5, [
                {K4, V4, C4}, {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc
            ]);
        %
        ?LEAF2(K1, K2, V1, V2) ->
            [{K2, V2}, {K1, V1} | Acc];
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            [{K3, V3}, {K2, V2}, {K1, V1} | Acc];
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            [{K4, V4}, {K3, V3}, {K2, V2}, {K1, V1} | Acc]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Iterator steps, reversed from
%% ------------------------------------------------------------------

-spec iterator_steps_r_from(Key, t(Key, Value)) -> [iterator_step(Key, Value)].
iterator_steps_r_from(Key, ?LEAF1(K1, V1)) ->
    if
        Key < K1 ->
            [];
        true ->
            [{K1, V1}]
    end;
iterator_steps_r_from(Key, ?INTERNAL1(K1, V1, _, C1, C2)) ->
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
iterator_steps_r_from_recur(Key, Node, Acc) ->
    case Node of
        ?INTERNAL2(K1, K2, Values, _, _, C1, C2, C3) ->
            iterator_steps_r_from_recur_internal2(Key, K1, K2, Values, C1, C2, C3, Acc);
        %
        ?INTERNAL3(K1, K2, K3, Values, _, _, _, C1, C2, C3, C4) ->
            iterator_steps_r_from_recur_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4, Acc);
        %
        ?INTERNAL4(K1, K2, K3, K4, Values, _, _, _, _, C1, C2, C3, C4, C5) ->
            iterator_steps_r_from_recur_internal4(
                Key,
                K1,
                K2,
                K3,
                K4,
                Values,
                C1,
                C2,
                C3,
                C4,
                C5,
                Acc
            );
        %
        ?LEAF2(K1, K2, V1, V2) ->
            iterator_steps_r_from_recur_leaf2(Key, K1, K2, V1, V2, Acc);
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            iterator_steps_r_from_recur_leaf3(Key, K1, K2, K3, V1, V2, V3, Acc);
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            iterator_steps_r_from_recur_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4, Acc)
    end.

%%%

-compile({inline, iterator_steps_r_from_recur_internal4/12}).
iterator_steps_r_from_recur_internal4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5, Acc) ->
    if
        Key > K2 ->
            if
                Key > K3 ->
                    if
                        Key > K4 ->
                            {V1, V2, V3, V4} = Values,
                            iterator_steps_r_from_recur(Key, C5, [
                                {K4, V4, C4}, {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc
                            ]);
                        %
                        Key < K4 ->
                            {V1, V2, V3, _} = Values,
                            iterator_steps_r_from_recur(Key, C4, [
                                {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc
                            ]);
                        %
                        true ->
                            {V1, V2, V3, V4} = Values,
                            [{K4, V4, C4}, {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc]
                    end;
                %
                Key < K3 ->
                    {V1, V2, _, _} = Values,
                    iterator_steps_r_from_recur(Key, C3, [{K2, V2, C2}, {K1, V1, C1} | Acc]);
                %
                true ->
                    {V1, V2, V3, _} = Values,
                    [{K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc]
            end;
        %
        Key < K2 ->
            if
                Key > K1 ->
                    V1 = element(1, Values),
                    iterator_steps_r_from_recur(Key, C2, [{K1, V1, C1} | Acc]);
                %
                Key < K1 ->
                    iterator_steps_r_from_recur(Key, C1, Acc);
                %
                true ->
                    V1 = element(1, Values),
                    [{K1, V1, C1} | Acc]
            end;
        %
        true ->
            {V1, V2, _, _} = Values,
            [{K2, V2, C2}, {K1, V1, C1} | Acc]
    end.

-compile({inline, iterator_steps_r_from_recur_internal3/10}).
iterator_steps_r_from_recur_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4, Acc) ->
    if
        Key > K2 ->
            if
                Key > K3 ->
                    {V1, V2, V3} = Values,
                    iterator_steps_r_from_recur(Key, C4, [
                        {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc
                    ]);
                %
                Key < K3 ->
                    {V1, V2, _} = Values,
                    iterator_steps_r_from_recur(Key, C3, [{K2, V2, C2}, {K1, V1, C1} | Acc]);
                %
                true ->
                    {V1, V2, V3} = Values,
                    [{K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc]
            end;
        %
        Key < K2 ->
            if
                Key > K1 ->
                    V1 = element(1, Values),
                    iterator_steps_r_from_recur(Key, C2, [{K1, V1, C1} | Acc]);
                %
                Key < K1 ->
                    iterator_steps_r_from_recur(Key, C1, Acc);
                %
                true ->
                    V1 = element(1, Values),
                    [{K1, V1, C1} | Acc]
            end;
        %
        true ->
            {V1, V2, _} = Values,
            [{K2, V2, C2}, {K1, V1, C1} | Acc]
    end.

-compile({inline, iterator_steps_r_from_recur_internal2/8}).
iterator_steps_r_from_recur_internal2(Key, K1, K2, Values, C1, C2, C3, Acc) ->
    if
        Key > K1 ->
            if
                Key > K2 ->
                    [V1 | V2] = Values,
                    iterator_steps_r_from_recur(Key, C3, [{K2, V2, C2}, {K1, V1, C1} | Acc]);
                %
                Key < K2 ->
                    V1 = hd(Values),
                    iterator_steps_r_from_recur(Key, C2, [{K1, V1, C1} | Acc]);
                %
                true ->
                    [V1 | V2] = Values,
                    [{K2, V2, C2}, {K1, V1, C1} | Acc]
            end;
        %
        Key < K1 ->
            iterator_steps_r_from_recur(Key, C1, Acc);
        %
        true ->
            V1 = hd(Values),
            [{K1, V1, C1} | Acc]
    end.

-compile({inline, iterator_steps_r_from_recur_leaf4/10}).
iterator_steps_r_from_recur_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4, Acc) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    Acc;
                true ->
                    [{K1, V1} | Acc]
            end;
        %
        Key < K3 ->
            [{K2, V2}, {K1, V1} | Acc];
        %
        Key < K4 ->
            [{K3, V3}, {K2, V2}, {K1, V1} | Acc];
        %
        true ->
            [{K4, V4}, {K3, V3}, {K2, V2}, {K1, V1} | Acc]
    end.

-compile({inline, iterator_steps_r_from_recur_leaf3/8}).
iterator_steps_r_from_recur_leaf3(Key, K1, K2, K3, V1, V2, V3, Acc) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    Acc;
                %
                true ->
                    [{K1, V1} | Acc]
            end;
        %
        Key < K3 ->
            [{K2, V2}, {K1, V1} | Acc];
        %
        true ->
            [{K3, V3}, {K2, V2}, {K1, V1} | Acc]
    end.

-compile({inline, iterator_steps_r_from_recur_leaf2/6}).
iterator_steps_r_from_recur_leaf2(Key, K1, K2, V1, V2, Acc) ->
    if
        Key < K1 ->
            Acc;
        %
        Key < K2 ->
            [{K1, V1} | Acc];
        %
        true ->
            [{K2, V2}, {K1, V1} | Acc]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: keys/1
%% ------------------------------------------------------------------

-spec keys_recur(deep_node(Key, _), [Key]) -> [Key, ...].
keys_recur(?INTERNAL2(K1, K2, _, _, _, C1, C2, C3), Acc) ->
    Acc2 = [K2 | keys_recur(C3, Acc)],
    Acc3 = [K1 | keys_recur(C2, Acc2)],
    keys_recur(C1, Acc3);
keys_recur(?INTERNAL3(K1, K2, K3, _, _, _, _, C1, C2, C3, C4), Acc) ->
    Acc2 = [K3 | keys_recur(C4, Acc)],
    Acc3 = [K2 | keys_recur(C3, Acc2)],
    Acc4 = [K1 | keys_recur(C2, Acc3)],
    keys_recur(C1, Acc4);
keys_recur(?INTERNAL4(K1, K2, K3, K4, _, _, _, _, _, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [K4 | keys_recur(C5, Acc)],
    Acc3 = [K3 | keys_recur(C4, Acc2)],
    Acc4 = [K2 | keys_recur(C3, Acc3)],
    Acc5 = [K1 | keys_recur(C2, Acc4)],
    keys_recur(C1, Acc5);
keys_recur(?LEAF2(K1, K2, _, _), Acc) ->
    [K1, K2 | Acc];
keys_recur(?LEAF3(K1, K2, K3, _, _, _), Acc) ->
    [K1, K2, K3 | Acc];
keys_recur(?LEAF4(K1, K2, K3, K4, _, _, _, _), Acc) ->
    [K1, K2, K3, K4 | Acc].

%% ------------------------------------------------------------------
%% Internal Function Definitions: values/1
%% ------------------------------------------------------------------

-spec values_recur(deep_node(_, Value), [Value]) -> [Value, ...].
-dialyzer({no_underspecs, values_recur/2}).
values_recur(?INTERNAL2(_, _, [V1 | V2], _, _, C1, C2, C3), Acc) ->
    Acc2 = [V2 | values_recur(C3, Acc)],
    Acc3 = [V1 | values_recur(C2, Acc2)],
    values_recur(C1, Acc3);
values_recur(?INTERNAL3(_, _, _, {V1, V2, V3}, _, _, _, C1, C2, C3, C4), Acc) ->
    Acc2 = [V3 | values_recur(C4, Acc)],
    Acc3 = [V2 | values_recur(C3, Acc2)],
    Acc4 = [V1 | values_recur(C2, Acc3)],
    values_recur(C1, Acc4);
values_recur(?INTERNAL4(_, _, _, _, {V1, V2, V3, V4}, _, _, _, _, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [V4 | values_recur(C5, Acc)],
    Acc3 = [V3 | values_recur(C4, Acc2)],
    Acc4 = [V2 | values_recur(C3, Acc3)],
    Acc5 = [V1 | values_recur(C2, Acc4)],
    values_recur(C1, Acc5);
values_recur(?LEAF2(_, _, V1, V2), Acc) ->
    [V1, V2 | Acc];
values_recur(?LEAF3(_, _, _, V1, V2, V3), Acc) ->
    [V1, V2, V3 | Acc];
values_recur(?LEAF4(_, _, _, _, V1, V2, V3, V4), Acc) ->
    [V1, V2, V3, V4 | Acc].

%% ------------------------------------------------------------------
%% Internal Function Definitions: to_list/1
%% ------------------------------------------------------------------

-spec to_list_recur(deep_node(Key, Value), [{Key, Value}]) -> [{Key, Value}, ...].
-dialyzer({no_underspecs, to_list_recur/2}).
to_list_recur(?INTERNAL2(K1, K2, [V1 | V2], _, _, C1, C2, C3), Acc) ->
    Acc2 = [{K2, V2} | to_list_recur(C3, Acc)],
    Acc3 = [{K1, V1} | to_list_recur(C2, Acc2)],
    to_list_recur(C1, Acc3);
to_list_recur(?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, _, _, C1, C2, C3, C4), Acc) ->
    Acc2 = [{K3, V3} | to_list_recur(C4, Acc)],
    Acc3 = [{K2, V2} | to_list_recur(C3, Acc2)],
    Acc4 = [{K1, V1} | to_list_recur(C2, Acc3)],
    to_list_recur(C1, Acc4);
to_list_recur(?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, _, _, _, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [{K4, V4} | to_list_recur(C5, Acc)],
    Acc3 = [{K3, V3} | to_list_recur(C4, Acc2)],
    Acc4 = [{K2, V2} | to_list_recur(C3, Acc3)],
    Acc5 = [{K1, V1} | to_list_recur(C2, Acc4)],
    to_list_recur(C1, Acc5);
to_list_recur(?LEAF2(K1, K2, V1, V2), Acc) ->
    [{K1, V1}, {K2, V2} | Acc];
to_list_recur(?LEAF3(K1, K2, K3, V1, V2, V3), Acc) ->
    [{K1, V1}, {K2, V2}, {K3, V3} | Acc];
to_list_recur(?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), Acc) ->
    [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4} | Acc].

%% ------------------------------------------------------------------
%% Internal Function Definitions: largest/1
%% ------------------------------------------------------------------

-spec largest_recur(deep_node(Key, Value)) -> {Key, Value}.
largest_recur(?INTERNAL2(_, _, _, _, _, _, _, C3)) ->
    largest_recur(C3);
largest_recur(?INTERNAL3(_, _, _, _, _, _, _, _, _, _, C4)) ->
    largest_recur(C4);
largest_recur(?INTERNAL4(_, _, _, _, _, _, _, _, _, _, _, _, _, C5)) ->
    largest_recur(C5);
largest_recur(?LEAF2(_, K2, _, V2)) ->
    {K2, V2};
largest_recur(?LEAF3(_, _, K3, _, _, V3)) ->
    {K3, V3};
largest_recur(?LEAF4(_, _, _, K4, _, _, _, V4)) ->
    {K4, V4}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: smallest/1
%% ------------------------------------------------------------------

-spec smallest_recur(deep_node(Key, Value)) -> {Key, Value}.
smallest_recur(?INTERNAL2(_, _, _, _, _, C1, _, _)) ->
    smallest_recur(C1);
smallest_recur(?INTERNAL3(_, _, _, _, _, _, _, C1, _, _, _)) ->
    smallest_recur(C1);
smallest_recur(?INTERNAL4(_, _, _, _, _, _, _, _, _, C1, _, _, _, _)) ->
    smallest_recur(C1);
smallest_recur(?LEAF2(K1, _, V1, _)) ->
    {K1, V1};
smallest_recur(?LEAF3(K1, _, _, V1, _, _)) ->
    {K1, V1};
smallest_recur(?LEAF4(K1, _, _, _, V1, _, _, _)) ->
    {K1, V1}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: larger/2
%% ------------------------------------------------------------------

-spec larger_recur(Key, deep_node(Key, Value)) -> {Key, Value} | none.
larger_recur(Key, ?INTERNAL2(K1, K2, Values, _, _, C1, C2, C3)) ->
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
larger_recur(Key, ?INTERNAL3(K1, K2, K3, Values, _, _, _, C1, C2, C3, C4)) ->
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
larger_recur(Key, ?INTERNAL4(K1, K2, K3, K4, Values, _, _, _, _, C1, C2, C3, C4, C5)) ->
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
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: map/2
%% ------------------------------------------------------------------

-spec map_recur(fun((Key, Value) -> MappedValue), deep_node(Key, Value)) ->
    deep_node(Key, MappedValue).
-dialyzer({no_underspecs, map_recur/2}).
map_recur(Fun, ?INTERNAL2(K1, K2, [V1 | V2], O1, O2, C1, C2, C3)) ->
    ?INTERNAL2(
        K1,
        K2,
        [Fun(K1, V1) | Fun(K2, V2)],
        O1,
        O2,
        map_recur(Fun, C1),
        map_recur(Fun, C2),
        map_recur(Fun, C3)
    );
map_recur(Fun, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, O1, O2, O3, C1, C2, C3, C4)) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        {Fun(K1, V1), Fun(K2, V2), Fun(K3, V3)},
        O1,
        O2,
        O3,
        map_recur(Fun, C1),
        map_recur(Fun, C2),
        map_recur(Fun, C3),
        map_recur(Fun, C4)
    );
map_recur(Fun, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, O1, O2, O3, O4, C1, C2, C3, C4, C5)) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        {Fun(K1, V1), Fun(K2, V2), Fun(K3, V3), Fun(K4, V4)},
        O1,
        O2,
        O3,
        O4,
        map_recur(Fun, C1),
        map_recur(Fun, C2),
        map_recur(Fun, C3),
        map_recur(Fun, C4),
        map_recur(Fun, C5)
    );
map_recur(Fun, ?LEAF2(K1, K2, V1, V2)) ->
    ?LEAF2(
        K1,
        K2,
        Fun(K1, V1),
        Fun(K2, V2)
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
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: smaller/2
%% ------------------------------------------------------------------

-spec smaller_recur(Key, deep_node(Key, Value)) -> {Key, Value} | none.
smaller_recur(Key, ?INTERNAL2(K1, K2, Values, _, _, C1, C2, C3)) ->
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
smaller_recur(Key, ?INTERNAL3(K1, K2, K3, Values, _, _, _, C1, C2, C3, C4)) ->
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
smaller_recur(Key, ?INTERNAL4(K1, K2, K3, K4, Values, _, _, _, _, C1, C2, C3, C4, C5)) ->
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
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its right sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_leftmost_internal/7}).
rebalance_leftmost_internal(
    CSizePlusOne, CKey, CValue, CLeftOffset, CLeft, CRight, ParentK, ParentV, Right
) ->
    _ = ?check_node(Right),

    case Right of
        ?INTERNAL2(K1, K2, Values, O1, O2, C1, C2, C3) ->
            [V1 | V2] = Values,

            MergedNodeO1 = CLeftOffset,
            MergedNodeO2 = CSizePlusOne,
            MergedNodeO3 = MergedNodeO2 + O1,
            MergedNodeO4 = MergedNodeO2 + O2,

            MergedNode = ?check_node(
                ?INTERNAL4(
                    CKey,
                    ParentK,
                    K1,
                    K2,
                    {CValue, ParentV, V1, V2},
                    %
                    MergedNodeO1,
                    MergedNodeO2,
                    MergedNodeO3,
                    MergedNodeO4,
                    %
                    CLeft,
                    CRight,
                    C1,
                    C2,
                    C3
                )
            ),

            MergedNode;
        %
        %
        %
        ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
            {V1, V2, V3} = Values,

            UpK = K1,
            UpVal = V1,
            MovedC = C1,

            UpdatedNodeO1 = CLeftOffset,
            UpdatedNodeO2 = CSizePlusOne,

            UpdatedNode = ?check_node(
                ?INTERNAL2(
                    CKey,
                    ParentK,
                    [CValue | ParentV],
                    %
                    UpdatedNodeO1,
                    UpdatedNodeO2,
                    %
                    CLeft,
                    CRight,
                    MovedC
                )
            ),

            UpdatedRight = ?check_node(
                ?INTERNAL2(
                    K2,
                    K3,
                    %
                    [V2 | V3],
                    O2 - O1,
                    O3 - O1,
                    %
                    C2,
                    C3,
                    C4
                )
            ),

            MovedSize = O1,
            ?ROTATED(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight);
        %
        %
        %
        ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
            {V1, V2, V3, V4} = Values,

            UpK = K1,
            UpVal = V1,
            MovedC = C1,

            UpdatedNodeO1 = CLeftOffset,
            UpdatedNodeO2 = CSizePlusOne,

            UpdatedNode = ?check_node(
                ?INTERNAL2(
                    CKey,
                    ParentK,
                    [CValue | ParentV],
                    %
                    UpdatedNodeO1,
                    UpdatedNodeO2,
                    %
                    CLeft,
                    CRight,
                    MovedC
                )
            ),

            UpdatedRight = ?check_node(
                ?INTERNAL3(
                    K2,
                    K3,
                    K4,
                    {V2, V3, V4},
                    %
                    O2 - O1,
                    O3 - O1,
                    O4 - O1,
                    %
                    C2,
                    C3,
                    C4,
                    C5
                )
            ),

            MovedSize = O1,
            ?ROTATED(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight)
        %
        %
    end.

%-compile({inline, rebalance_leftmost_leaf/5}).
rebalance_leftmost_leaf(CKey, CValue, ParentK, ParentV, Right) ->
    case Right of
        ?LEAF2(K1, K2, V1, V2) ->
            MergedNode = ?LEAF4(
                CKey,
                ParentK,
                K1,
                K2,
                CValue,
                ParentV,
                V1,
                V2
            ),

            MergedNode;
        %
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            UpK = K1,
            UpVal = V1,
            MovedSize = 1,

            UpdatedNode = ?LEAF2(CKey, ParentK, CValue, ParentV),
            UpdatedRight = ?LEAF2(K2, K3, V2, V3),

            ?ROTATED(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight);
        %
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            UpK = K1,
            UpVal = V1,
            MovedSize = 1,

            UpdatedNode = ?LEAF2(CKey, ParentK, CValue, ParentV),
            UpdatedRight = ?LEAF3(K2, K3, K4, V2, V3, V4),

            ?ROTATED(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight)
        %
        %
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its left sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_rightmost_internal/7}).
rebalance_rightmost_internal(
    CKey,
    CValue,
    CLeftOffset,
    CLeft,
    CRight,
    %
    ParentK,
    ParentV,
    LeftSizePlusOne,
    Left
) ->
    _ = ?check_node(Left),

    case Left of
        ?INTERNAL2(LK1, LK2, LValues, LO1, LO2, LC1, LC2, LC3) ->
            [LV1 | LV2] = LValues,

            MergedNodeO1 = LO1,
            MergedNodeO2 = LO2,
            MergedNodeO3 = LeftSizePlusOne,
            MergedNodeO4 = MergedNodeO3 + CLeftOffset,

            MergedNode = ?check_node(
                ?INTERNAL4(
                    LK1,
                    LK2,
                    ParentK,
                    CKey,
                    {LV1, LV2, ParentV, CValue},
                    %
                    MergedNodeO1,
                    MergedNodeO2,
                    MergedNodeO3,
                    MergedNodeO4,
                    %
                    LC1,
                    LC2,
                    LC3,
                    CLeft,
                    CRight
                )
            ),

            MergedNode;
        %
        %
        %
        ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
            {V1, V2, V3} = Values,

            UpK = K3,
            UpVal = V3,
            MovedC = C4,

            UpdatedNodeO1 = MovedSize = LeftSizePlusOne - O3,
            UpdatedNodeO2 = UpdatedNodeO1 + CLeftOffset,

            UpdatedNode = ?check_node(
                ?INTERNAL2(
                    ParentK,
                    CKey,
                    [ParentV | CValue],
                    %
                    UpdatedNodeO1,
                    UpdatedNodeO2,
                    %
                    MovedC,
                    CLeft,
                    CRight
                )
            ),

            UpdatedLeft = ?check_node(
                ?INTERNAL2(
                    K1,
                    K2,
                    [V1 | V2],
                    %
                    O1,
                    O2,
                    %
                    C1,
                    C2,
                    C3
                )
            ),

            ?ROTATED(UpK, UpVal, MovedSize, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
            {V1, V2, V3, V4} = Values,

            UpK = K4,
            UpVal = V4,
            MovedC = C5,

            UpdatedNodeO1 = MovedSize = LeftSizePlusOne - O4,
            UpdatedNodeO2 = UpdatedNodeO1 + CLeftOffset,

            UpdatedNode = ?check_node(
                ?INTERNAL2(
                    ParentK,
                    CKey,
                    [ParentV | CValue],
                    %
                    UpdatedNodeO1,
                    UpdatedNodeO2,
                    %
                    MovedC,
                    CLeft,
                    CRight
                )
            ),

            UpdatedLeft = ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K3,
                    {V1, V2, V3},
                    %
                    O1,
                    O2,
                    O3,
                    %
                    C1,
                    C2,
                    C3,
                    C4
                )
            ),

            ?ROTATED(UpK, UpVal, MovedSize, UpdatedLeft, UpdatedNode)
        %
        %
    end.

%-compile({inline, rebalance_rightmost_leaf/5}).
rebalance_rightmost_leaf(
    CKey,
    CValue,
    ParentK,
    ParentV,
    Left
) ->
    case Left of
        ?LEAF2(LK1, LK2, LV1, LV2) ->
            MergedNode = ?LEAF4(
                LK1,
                LK2,
                ParentK,
                CKey,
                LV1,
                LV2,
                ParentV,
                CValue
            ),

            MergedNode;
        %
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            UpK = K3,
            UpVal = V3,
            MovedSize = 1,

            UpdatedNode = ?LEAF2(ParentK, CKey, ParentV, CValue),
            UpdatedLeft = ?LEAF2(K1, K2, V1, V2),

            ?ROTATED(UpK, UpVal, MovedSize, UpdatedLeft, UpdatedNode);
        %
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            UpK = K4,
            UpVal = V4,
            MovedSize = 1,

            UpdatedNode = ?LEAF2(ParentK, CKey, ParentV, CValue),
            UpdatedLeft = ?LEAF3(K1, K2, K3, V1, V2, V3),

            ?ROTATED(UpK, UpVal, MovedSize, UpdatedLeft, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from either left/right sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_either_sibling/10}).
rebalance_internal_from_either_sibling(
    COffset,
    CKey,
    CValue,
    CLeftOffset,
    CLeft,
    CRight,
    %
    LParentK,
    LParentV,
    LeftLeftOffset,
    LeftOffset,
    Left,
    %
    RParentK,
    RParentV,
    Right
) ->
    _ = ?check_node(Left),
    _ = ?check_node(Right),

    case Left of
        ?INTERNAL2(LK1, LK2, LValues, LO1, LO2, LC1, LC2, LC3) ->
            %
            %
            case Right of
                ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
                    {V1, V2, V3} = Values,

                    UpK = K1,
                    UpVal = V1,
                    MovedC = C1,

                    CSizePlusOne = COffset - LeftOffset,

                    UpdatedNodeO1 = CLeftOffset,
                    UpdatedNodeO2 = CSizePlusOne,

                    UpdatedNode = ?check_node(
                        ?INTERNAL2(
                            CKey,
                            RParentK,
                            [CValue | RParentV],
                            %
                            UpdatedNodeO1,
                            UpdatedNodeO2,
                            %
                            CLeft,
                            CRight,
                            MovedC
                        )
                    ),

                    UpdatedRight = ?check_node(
                        ?INTERNAL2(
                            K2,
                            K3,
                            [V2 | V3],
                            %
                            O2 - O1,
                            O3 - O1,
                            %
                            C2,
                            C3,
                            C4
                        )
                    ),

                    MovedSize = O1,
                    ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight);
                %
                %
                ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
                    {V1, V2, V3, V4} = Values,

                    UpK = K1,
                    UpVal = V1,
                    MovedC = C1,

                    CSizePlusOne = COffset - LeftOffset,

                    UpdatedNodeO1 = CLeftOffset,
                    UpdatedNodeO2 = CSizePlusOne,

                    UpdatedNode = ?check_node(
                        ?INTERNAL2(
                            CKey,
                            RParentK,
                            [CValue | RParentV],
                            %
                            UpdatedNodeO1,
                            UpdatedNodeO2,
                            %
                            CLeft,
                            CRight,
                            MovedC
                        )
                    ),

                    UpdatedRight = ?check_node(
                        ?INTERNAL3(
                            K2,
                            K3,
                            K4,
                            {V2, V3, V4},
                            %
                            O2 - O1,
                            O3 - O1,
                            O4 - O1,
                            %
                            C2,
                            C3,
                            C4,
                            C5
                        )
                    ),

                    MovedSize = O1,
                    ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight);
                %
                %
                _ ->
                    % Merge with left since we already unpacked it
                    [LV1 | LV2] = LValues,

                    MergedNodeO1 = LO1,
                    MergedNodeO2 = LO2,
                    MergedNodeO3 = LeftOffset - LeftLeftOffset,
                    MergedNodeO4 = MergedNodeO3 + CLeftOffset,

                    MergedNode = ?check_node(?INTERNAL4(
                        LK1,
                        LK2,
                        LParentK,
                        CKey,
                        {LV1, LV2, LParentV, CValue},
                        %
                        MergedNodeO1,
                        MergedNodeO2,
                        MergedNodeO3,
                        MergedNodeO4,
                        %
                        LC1,
                        LC2,
                        LC3,
                        CLeft,
                        CRight
                    )),

                    ?MID_MERGED(MergedNode)
            end;
        %
        %
        %
        %
        ?INTERNAL3(K1, K2, K3, Values, O1, O2, O3, C1, C2, C3, C4) ->
            {V1, V2, V3} = Values,

            UpK = K3,
            UpVal = V3,
            MovedC = C4,

            LeftSizePlusOne = LeftOffset - LeftLeftOffset,

            UpdatedNodeO1 = MovedSize = (LeftSizePlusOne - O3),
            UpdatedNodeO2 = UpdatedNodeO1 + CLeftOffset,

            UpdatedNode = ?check_node(
                ?INTERNAL2(
                    LParentK,
                    CKey,
                    [LParentV | CValue],
                    %
                    UpdatedNodeO1,
                    UpdatedNodeO2,
                    %
                    MovedC,
                    CLeft,
                    CRight
                )
            ),

            UpdatedLeft = ?check_node(
                ?INTERNAL2(
                    K1,
                    K2,
                    [V1 | V2],
                    %
                    O1,
                    O2,
                    %
                    C1,
                    C2,
                    C3
                )
            ),

            ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedLeft, UpdatedNode);
        %
        %
        %
        %
        ?INTERNAL4(K1, K2, K3, K4, Values, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
            {V1, V2, V3, V4} = Values,

            UpK = K4,
            UpVal = V4,
            MovedC = C5,

            LeftSizePlusOne = LeftOffset - LeftLeftOffset,

            UpdatedNodeO1 = MovedSize = (LeftSizePlusOne - O4),
            UpdatedNodeO2 = UpdatedNodeO1 + CLeftOffset,

            UpdatedNode = ?check_node(
                ?INTERNAL2(
                    LParentK,
                    CKey,
                    [LParentV | CValue],
                    %
                    UpdatedNodeO1,
                    UpdatedNodeO2,
                    %
                    MovedC,
                    CLeft,
                    CRight
                )
            ),

            UpdatedLeft = ?check_node(
                ?INTERNAL3(
                    K1,
                    K2,
                    K3,
                    {V1, V2, V3},
                    %
                    O1,
                    O2,
                    O3,
                    %
                    C1,
                    C2,
                    C3,
                    C4
                )
            ),

            ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedLeft, UpdatedNode)
    end.

% -compile({inline, rebalance_leaf_from_either_sibling/8}).
rebalance_leaf_from_either_sibling(
    CKey,
    CValue,
    LParentK,
    LParentV,
    Left,
    RParentK,
    RParentV,
    Right
) ->
    case Left of
        ?LEAF2(LK1, LK2, LV1, LV2) ->
            case Right of
                ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
                    UpK = K1,
                    UpVal = V1,
                    MovedSize = 1,

                    UpdatedNode = ?LEAF2(CKey, RParentK, CValue, RParentV),
                    UpdatedRight = ?LEAF3(K2, K3, K4, V2, V3, V4),

                    ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight);
                %
                %
                ?LEAF3(K1, K2, K3, V1, V2, V3) ->
                    UpK = K1,
                    UpVal = V1,
                    MovedSize = 1,

                    UpdatedNode = ?LEAF2(CKey, RParentK, CValue, RParentV),
                    UpdatedRight = ?LEAF2(K2, K3, V2, V3),

                    ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, MovedSize, UpdatedNode, UpdatedRight);
                %
                %
                _ ->
                    % Merge with left since we already unpacked it
                    MergedNode = ?LEAF4(
                        LK1,
                        LK2,
                        LParentK,
                        CKey,
                        LV1,
                        LV2,
                        LParentV,
                        CValue
                    ),

                    ?MID_MERGED(MergedNode)
            end;
        %
        %
        %
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            UpK = K4,
            UpVal = V4,
            MovedSize = 1,

            UpdatedNode = ?LEAF2(LParentK, CKey, LParentV, CValue),
            UpdatedLeft = ?LEAF3(K1, K2, K3, V1, V2, V3),

            ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            UpK = K3,
            UpVal = V3,
            MovedSize = 1,

            UpdatedNode = ?LEAF2(LParentK, CKey, LParentV, CValue),
            UpdatedLeft = ?LEAF2(K1, K2, V1, V2),

            ?MID_ROTATED_FROM_LEFT(UpK, UpVal, MovedSize, UpdatedLeft, UpdatedNode)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% check_node(Line, ?INTERNAL1(_, _, O1, C1, _) = Node) ->
%     S1 = O1 - 1,
%     check_sizes(Line, Node, [{s1, S1, C1}]);
% check_node(Line, ?INTERNAL2(_, _, _, O1, O2, C1, C2, _) = Node) ->
%     S1 = O1 - 1,
%     S2 = O2 - O1 - 1,
%     check_sizes(Line, Node, [{s1, S1, C1}, {s2, S2, C2}]);
% check_node(Line, ?INTERNAL3(_, _, _, _, O1, O2, O3, C1, C2, C3, _) = Node) ->
%     S1 = O1 - 1,
%     S2 = O2 - O1 - 1,
%     S3 = O3 - O2 - 1,
%     check_sizes(Line, Node, [{s1, S1, C1}, {s2, S2, C2}, {s3, S3, C3}]);
% check_node(Line, ?INTERNAL4(_, _, _, _, _, O1, O2, O3, O4, C1, C2, C3, C4, _) = Node) ->
%     S1 = O1 - 1,
%     S2 = O2 - O1 - 1,
%     S3 = O3 - O2 - 1,
%     S4 = O4 - O3 - 1,
%     check_sizes(Line, Node, [{s1, S1, C1}, {s2, S2, C2}, {s3, S3, C3}, {s4, S4, C4}]);
% check_node(_, ?LEAF4(_, _, _, _, _, _, _, _) = Node) ->
%     Node;
% check_node(_, ?LEAF3(_, _, _, _, _, _) = Node) ->
%     Node;
% check_node(_, ?LEAF2(_, _, _, _) = Node) ->
%     Node;
% check_node(_, ?LEAF1(_, _) = Node) ->
%     Node;
% check_node(_, ?LEAF0 = Node) ->
%     Node.
% 
% check_sizes(Line, Node, Targets) ->
%     case lists:filtermap(fun check_size/1, Targets) of
%         [] ->
%             Node;
% 
%         BadTargets ->
%             error({bad_sizes, {line, Line}, BadTargets})
%     end.
% 
% check_size({Tag, Size, Node}) ->
%     CountedKeys = length(keys_recur(Node, [])),
%     
%     case CountedKeys =/= Size of
%         true ->
%             {true, {Tag, {expected, Size}, {but_measured, CountedKeys}, Node}};
%         %
%         false ->
%             false
%     end.
