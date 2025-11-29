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
    delete_key/2,
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
    smaller/2,
    smallest/1,
    take_key/2,
    take_largest/1,
    take_nth/2,
    take_smallest/1,
    to_list/1,
    update/4,
    validate/2,
    values/1
]).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

% 11 elements
-define(INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5),
    {K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5}
).

% 9 elements
-define(INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4),
    {K1, K2, K3, Values, Sizes, C1, C2, C3, C4}
).

% 7 elements
-define(INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3),
    {K1, K2, Values, Sizes, C1, C2, C3}
).

% 5 elements
-define(INTERNAL1(K1, V1, Sizes, C1, C2), {K1, V1, Sizes, C1, C2}).

% 8 elements
-define(LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), {K1, K2, K3, K4, V1, V2, V3, V4}).

% 6 elements
-define(LEAF3(K1, K2, K3, V1, V2, V3), {K1, K2, K3, V1, V2, V3}).

% 4 elements
-define(LEAF2(K1, K2, V1, V2), {K1, K2, V1, V2}).

-define(LEAF1(K1, V1), [K1 | V1]).

-define(LEAF0, leaf0).

%%

% 2 elements
-define(SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR),
    {split, {SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR}}
).

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
        internal4_sizes(),
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
        internal3_sizes(),
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
        internal2_sizes(),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_internal1(Key, Value) ::
    (?INTERNAL1(
        Key,
        Value,
        internal1_sizes(),
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

-type internal4_sizes() :: 'TODO'.
-type internal3_sizes() :: 'TODO'.
-type internal2_sizes() :: 'TODO'.
-type internal1_sizes() :: 'TODO'.

%%%%%%%%%%%

% Dialyzer got too smart when it reasoned this, but it is indeed true.
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

-type split_result(Key, Value, SplitL, SplitR) :: {split, Key, Value, SplitL, SplitR}.

%%%%%%%%%%%

-opaque iter(Key, Value) :: forward_iter(Key, Value) | reverse_iter(Key, Value).
-export_type([iter/2]).

% FIXME remove default value, it messes up with Dialyzer
-record(b5_ranks_forward_iter, {steps :: [iterator_step(_, _)]}).
-type forward_iter(Key, Value) :: #b5_ranks_forward_iter{steps :: [iterator_step(Key, Value)]}.

-record(b5_ranks_reverse_iter, {steps :: [iterator_step(_, _)]}).
-type reverse_iter(Key, Value) :: #b5_ranks_reverse_iter{steps :: [iterator_step(Key, Value)]}.

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
-spec delete_key(Key, t(Key, Value)) -> t(Key, Value).
delete_key(Key, Node) ->
    delete(Key, Node).

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
-dialyzer({no_underspecs, insert/4}).
insert(Key, ValueEval, ValueWrap, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    insert_internal1(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2);
insert(Key, ValueEval, ValueWrap, ?LEAF1(K1, V1)) ->
    insert_leaf1(Key, ValueEval, ValueWrap, K1, V1);
insert(Key, ValueEval, ValueWrap, ?LEAF0) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF1(Key, Value);
insert(Key, ValueEval, ValueWrap, Root) ->
    case insert_recur(Key, ValueEval, ValueWrap, Root) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            Sizes = internal1_sizes_pack(SplitLSize, SplitRSize),
            ?INTERNAL1(SplitK, SplitV, Sizes, SplitL, SplitR);
        UpdatedRoot ->
            UpdatedRoot
    end.

nth(N, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    [S1 | _] = internal1_sizes_unpack(Sizes),

    Pos1 = S1 + 1,

    if
        N < Pos1 ->
            nth(N, C1);
        N > Pos1 ->
            nth(N - Pos1, C2);
        true ->
            {K1, V1}
    end;
nth(N, ?LEAF1(K1, V1)) ->
    case N of
        1 ->
            {K1, V1}
    end;
nth(N, Node) ->
    nth_recur(N, Node).

%% @doc Creates an iterator for traversing the tree node entries.
%% Can iterate in `ordered' (ascending) or `reversed' (descending) direction.
-spec iterator(t(Key, Value), ordered | reversed) -> iter(Key, Value).
iterator(Node, ordered) ->
    #b5_ranks_forward_iter{steps = iterator_steps_l(Node)};
iterator(Node, reversed) ->
    #b5_ranks_reverse_iter{steps = iterator_steps_r(Node)}.

%% @doc Creates an iterator starting from the first key >= the specified key.
%% Can iterate in `ordered' (ascending) or `reversed' (descending) direction.
-spec iterator_from(Key, t(Key, Value), ordered | reversed) -> iter(Key, Value).
iterator_from(Key, Node, ordered) ->
    #b5_ranks_forward_iter{steps = iterator_steps_l_from(Key, Node)};
iterator_from(Key, Node, reversed) ->
    #b5_ranks_reverse_iter{steps = iterator_steps_r_from(Key, Node)}.

%% @doc Returns all keys in the tree node as an ordered list.
-spec keys(t(Key, _)) -> [Key].
-dialyzer({no_underspecs, keys/1}).
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
next(#b5_ranks_forward_iter{steps = Steps} = Iter) ->
    case Steps of
        [{Key, Value} | NextSteps] ->
            UpdatedIter = Iter#b5_ranks_forward_iter{steps = NextSteps},
            {Key, Value, UpdatedIter};
        [{Key, Value, NextChild} | NextNextSteps] ->
            UpdatedIter = Iter#b5_ranks_forward_iter{
                steps = iterator_steps_l_recur(NextChild, NextNextSteps)
            },
            {Key, Value, UpdatedIter};
        [] ->
            none
    end;
next(#b5_ranks_reverse_iter{steps = Steps} = Iter) ->
    case Steps of
        [{Key, Value} | NextSteps] ->
            UpdatedIter = Iter#b5_ranks_reverse_iter{steps = NextSteps},
            {Key, Value, UpdatedIter};
        [{Key, Value, NextChild} | NextNextSteps] ->
            UpdatedIter = Iter#b5_ranks_reverse_iter{
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
map(Fun, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    ?INTERNAL1(
        K1,
        Fun(K1, V1),
        Sizes,
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
-spec take_key(Key, t(Key, Value)) -> {Value, t(Key, Value)}.
take_key(Key, Node) ->
    {{_, Value}, UpdatedNode} = root_take(key, Key, Node),
    {Value, UpdatedNode}.

%% @doc Removes and returns the largest key-value pair from the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
%% Returns `{Key, Value, UpdatedNode}'.
-spec take_largest(t(Key, Value)) ->
    {Key, Value, t(Key, Value)}.
% -dialyzer({no_underspecs, take_largest/1}).
take_largest(Node) ->
    {{K, V}, UpdatedNode} = root_take_largest(Node),
    {K, V, UpdatedNode}.

take_nth(N, Node) ->
    {{Key, Value}, UpdatedNode} = root_take(nth, N, Node),
    {Key, Value, UpdatedNode}.

%% @doc Removes and returns the smallest key-value pair from the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
%% Returns `{Key, Value, UpdatedNode}'.
-spec take_smallest(t(Key, Value)) ->
    {Key, Value, t(Key, Value)}.
% -dialyzer({no_underspecs, take_smallest/1}).
take_smallest(Node) ->
    {{K, V}, UpdatedNode} = root_take_smallest(Node),
    {K, V, UpdatedNode}.

%% @doc Converts the tree node into an ordered list of key-value tuples.
-spec to_list(t(Key, Value)) -> [{Key, Value}].
-dialyzer({no_underspecs, to_list/1}).
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
update(Key, ValueEval, ValueWrap, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    update_internal1(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2);
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
-dialyzer({no_underspecs, values/1}).
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
get_recur(Key, ?INTERNAL4(K1, K2, K3, K4, Values, _, C1, C2, C3, C4, C5)) ->
    get_internal4(Key, K1, K2, K3, K4, Values, C1, C2, C3, C4, C5);
get_recur(Key, ?INTERNAL3(K1, K2, K3, Values, _, C1, C2, C3, C4)) ->
    get_internal3(Key, K1, K2, K3, Values, C1, C2, C3, C4);
get_recur(Key, ?INTERNAL2(K1, K2, Values, _, C1, C2, C3)) ->
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

-spec insert_recur(
    Key,
    insertion_value_wrap(Value),
    insertion_value_eval(),
    deep_node(Key, Value)
) -> deep_node_after_insertion(Key, Value) | split_result(Key, Value).
insert_recur(
    Key, ValueEval, ValueWrap, ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
) ->
    insert_internal4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
insert_recur(Key, ValueEval, ValueWrap, ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)) ->
    insert_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
insert_recur(Key, ValueEval, ValueWrap, ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3)) ->
    insert_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3);
insert_recur(Key, ValueEval, ValueWrap, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    insert_leaf4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4);
insert_recur(Key, ValueEval, ValueWrap, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    insert_leaf3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
insert_recur(Key, ValueEval, ValueWrap, ?LEAF2(K1, K2, V1, V2)) ->
    insert_leaf2(Key, ValueEval, ValueWrap, K1, K2, V1, V2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal4/14]}).
insert_internal4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
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
                                Sizes,
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
                                Sizes,
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
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    error_key_exists(Key)
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    insert_internal4_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                Key < K1 ->
                    insert_internal4_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    error_key_exists(Key)
            end;
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal4_child1/14]}).
insert_internal4_child1(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            {_, S2, S3, S4, S5} = internal4_sizes_unpack(Sizes),
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
                SplitLSize,
                SplitRSize,
                S2,
                S3,
                S4,
                S5,
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
                internal4_sizes_update1(Sizes, +1),
                UpdatedC1,
                C2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child2/14]}).
insert_internal4_child2(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            {S1, _, S3, S4, S5} = internal4_sizes_unpack(Sizes),
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
                S1,
                SplitLSize,
                SplitRSize,
                S3,
                S4,
                S5,
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
                internal4_sizes_update2(Sizes, +1),
                C1,
                UpdatedC2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child3/14]}).
insert_internal4_child3(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            {S1, S2, _, S4, S5} = internal4_sizes_unpack(Sizes),
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
                S1,
                S2,
                SplitLSize,
                SplitRSize,
                S4,
                S5,
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
                internal4_sizes_update3(Sizes, +1),
                C1,
                C2,
                UpdatedC3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child4/14]}).
insert_internal4_child4(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            {S1, S2, S3, _, S5} = internal4_sizes_unpack(Sizes),
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
                S1,
                S2,
                S3,
                SplitLSize,
                SplitRSize,
                S5,
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
                internal4_sizes_update4(Sizes, +1),
                C1,
                C2,
                C3,
                UpdatedC4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child5/14]}).
insert_internal4_child5(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(Key, ValueEval, ValueWrap, C5) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            {S1, S2, S3, S4, _} = internal4_sizes_unpack(Sizes),
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
                S1,
                S2,
                S3,
                S4,
                SplitLSize,
                SplitRSize,
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
                internal4_sizes_update5(Sizes, +1),
                C1,
                C2,
                C3,
                C4,
                UpdatedC5
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal3/12]}).
insert_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    if
        Key > K2 ->
            if
                Key < K3 ->
                    insert_internal3_child3(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                Key > K3 ->
                    insert_internal3_child4(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    error_key_exists(Key)
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    insert_internal3_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                Key < K1 ->
                    insert_internal3_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    error_key_exists(Key)
            end;
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal3_child1/12]}).
insert_internal3_child1(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                SplitK,
                K1,
                K2,
                K3,
                {SplitV, V1, V2, V3},
                internal3_sizes_split1(Sizes, SplitLSize, SplitRSize),
                SplitL,
                SplitR,
                C2,
                C3,
                C4
            );
        UpdatedC1 ->
            ?INTERNAL3(
                K1,
                K2,
                K3,
                Values,
                internal3_sizes_update1(Sizes, +1),
                UpdatedC1,
                C2,
                C3,
                C4
            )
    end.

-compile({inline, [insert_internal3_child2/12]}).
insert_internal3_child2(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                SplitK,
                K2,
                K3,
                {V1, SplitV, V2, V3},
                internal3_sizes_split2(Sizes, SplitLSize, SplitRSize),
                C1,
                SplitL,
                SplitR,
                C3,
                C4
            );
        UpdatedC2 ->
            ?INTERNAL3(
                K1,
                K2,
                K3,
                Values,
                internal3_sizes_update2(Sizes, +1),
                C1,
                UpdatedC2,
                C3,
                C4
            )
    end.

-compile({inline, [insert_internal3_child3/12]}).
insert_internal3_child3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                K2,
                SplitK,
                K3,
                {V1, V2, SplitV, V3},
                internal3_sizes_split3(Sizes, SplitLSize, SplitRSize),
                C1,
                C2,
                SplitL,
                SplitR,
                C4
            );
        UpdatedC3 ->
            ?INTERNAL3(
                K1,
                K2,
                K3,
                Values,
                internal3_sizes_update3(Sizes, +1),
                C1,
                C2,
                UpdatedC3,
                C4
            )
    end.

-compile({inline, [insert_internal3_child4/12]}).
insert_internal3_child4(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                K2,
                K3,
                SplitK,
                {V1, V2, V3, SplitV},
                internal3_sizes_split4(Sizes, SplitLSize, SplitRSize),
                C1,
                C2,
                C3,
                SplitL,
                SplitR
            );
        UpdatedC4 ->
            ?INTERNAL3(
                K1,
                K2,
                K3,
                Values,
                internal3_sizes_update4(Sizes, +1),
                C1,
                C2,
                C3,
                UpdatedC4
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal2/10]}).
insert_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    insert_internal2_child2(
                        Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3
                    );
                Key > K2 ->
                    insert_internal2_child3(
                        Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3
                    );
                true ->
                    error_key_exists(Key)
            end;
        Key < K1 ->
            insert_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3);
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal2_child1/10]}).
insert_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                SplitK,
                K1,
                K2,
                {SplitV, V1, V2},
                internal2_sizes_split1(Sizes, SplitLSize, SplitRSize),
                SplitL,
                SplitR,
                C2,
                C3
            );
        UpdatedC1 ->
            ?INTERNAL2(
                K1,
                K2,
                Values,
                internal2_sizes_update1(Sizes, +1),
                UpdatedC1,
                C2,
                C3
            )
    end.

-compile({inline, [insert_internal2_child2/10]}).
insert_internal2_child2(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                K1,
                SplitK,
                K2,
                {V1, SplitV, V2},
                internal2_sizes_split2(Sizes, SplitLSize, SplitRSize),
                C1,
                SplitL,
                SplitR,
                C3
            );
        UpdatedC2 ->
            ?INTERNAL2(
                K1,
                K2,
                Values,
                internal2_sizes_update2(Sizes, +1),
                C1,
                UpdatedC2,
                C3
            )
    end.

-compile({inline, [insert_internal2_child3/10]}).
insert_internal2_child3(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                K1,
                K2,
                SplitK,
                {V1, V2, SplitV},
                internal2_sizes_split3(Sizes, SplitLSize, SplitRSize),
                C1,
                C2,
                SplitL,
                SplitR
            );
        UpdatedC3 ->
            ?INTERNAL2(
                K1,
                K2,
                Values,
                internal2_sizes_update3(Sizes, +1),
                C1,
                C2,
                UpdatedC3
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal1/8]}).
insert_internal1(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2) ->
    if
        Key < K1 ->
            insert_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2);
        Key > K1 ->
            insert_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2);
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal1_child1/8]}).
insert_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            ?INTERNAL2(
                SplitK,
                K1,
                [SplitV | V1],
                internal1_sizes_split1(Sizes, SplitLSize, SplitRSize),
                SplitL,
                SplitR,
                C2
            );
        UpdatedC1 ->
            ?INTERNAL1(
                K1,
                V1,
                internal1_sizes_update1(Sizes, +1),
                UpdatedC1,
                C2
            )
    end.

-compile({inline, [insert_internal1_child2/8]}).
insert_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            ?INTERNAL2(
                K1,
                SplitK,
                [V1 | SplitV],
                internal1_sizes_split2(Sizes, SplitLSize, SplitRSize),
                C1,
                SplitL,
                SplitR
            );
        UpdatedC2 ->
            ?INTERNAL1(
                K1,
                V1,
                internal1_sizes_update2(Sizes, +1),
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

-compile({inline, internal_split/22}).
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
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    C1,
    C2,
    C3,
    C4,
    C5,
    C6
) ->
    SplitK = K3,
    SplitV = V3,

    SplitLSize = S1 + S2 + S3 + 2,
    SizesL = internal2_sizes_pack(S1, S2, S3),
    SplitL = ?INTERNAL2(K1, K2, [V1 | V2], SizesL, C1, C2, C3),

    SplitRSize = S4 + S5 + S6 + 2,
    SizesR = internal2_sizes_pack(S4, S5, S6),
    SplitR = ?INTERNAL2(K4, K5, [V4 | V5], SizesR, C4, C5, C6),

    ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR).

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

    SplitLSize = 2,
    SplitL = ?LEAF2(K1, K2, V1, V2),

    SplitRSize = 2,
    SplitR = ?LEAF2(K4, K5, V4, V5),

    ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Update
%% ------------------------------------------------------------------

-spec update_recur(
    Key,
    update_value_wrap(Value, UpdatedValue),
    update_value_eval(),
    deep_node(Key, Value)
) -> deep_node(Key, Value | UpdatedValue).
update_recur(
    Key, ValueEval, ValueWrap, ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
) ->
    update_internal4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
update_recur(Key, ValueEval, ValueWrap, ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)) ->
    update_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
update_recur(Key, ValueEval, ValueWrap, ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3)) ->
    update_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3);
update_recur(Key, ValueEval, ValueWrap, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    update_leaf4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4);
update_recur(Key, ValueEval, ValueWrap, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    update_leaf3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
update_recur(Key, ValueEval, ValueWrap, ?LEAF2(K1, K2, V1, V2)) ->
    update_leaf2(Key, ValueEval, ValueWrap, K1, K2, V1, V2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal4/14]}).
update_internal4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
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
                                Sizes,
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
                                Sizes,
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
                                Sizes,
                                C1,
                                C2,
                                C3,
                                C4,
                                C5
                            )
                    end;
                Key > K4 ->
                    update_internal4_child5(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    update_internal4_key4(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4, C5
                    )
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    update_internal4_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                Key < K1 ->
                    update_internal4_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    update_internal4_key1(
                        Key, ValueEval, ValueWrap, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    )
            end;
        true ->
            update_internal4_key2(
                Key, ValueEval, ValueWrap, K1, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
            )
    end.

-compile({inline, [update_internal4_child1/14]}).
update_internal4_child1(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        Sizes,
        update_recur(Key, ValueEval, ValueWrap, C1),
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_child2/14]}).
update_internal4_child2(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        Sizes,
        C1,
        update_recur(Key, ValueEval, ValueWrap, C2),
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_child3/14]}).
update_internal4_child3(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        Sizes,
        C1,
        C2,
        update_recur(Key, ValueEval, ValueWrap, C3),
        C4,
        C5
    ).

-compile({inline, [update_internal4_child4/14]}).
update_internal4_child4(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        Sizes,
        C1,
        C2,
        C3,
        update_recur(Key, ValueEval, ValueWrap, C4),
        C5
    ).

-compile({inline, [update_internal4_child5/14]}).
update_internal4_child5(
    Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        Values,
        Sizes,
        C1,
        C2,
        C3,
        C4,
        update_recur(Key, ValueEval, ValueWrap, C5)
    ).

%%%

-compile({inline, [update_internal4_key1/13]}).
update_internal4_key1(Key, ValueEval, ValueWrap, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL4(
        Key,
        K2,
        K3,
        K4,
        {Value, V2, V3, V4},
        Sizes,
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_key2/13]}).
update_internal4_key2(Key, ValueEval, ValueWrap, K1, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V2),
    ?INTERNAL4(
        K1,
        Key,
        K3,
        K4,
        {V1, Value, V3, V4},
        Sizes,
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_key3/13]}).
update_internal4_key3(Key, ValueEval, ValueWrap, K1, K2, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V3),
    ?INTERNAL4(
        K1,
        K2,
        Key,
        K4,
        {V1, V2, Value, V4},
        Sizes,
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [update_internal4_key4/13]}).
update_internal4_key4(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4, C5) ->
    {V1, V2, V3, V4} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V4),
    ?INTERNAL4(
        K1,
        K2,
        K3,
        Key,
        {V1, V2, V3, Value},
        Sizes,
        C1,
        C2,
        C3,
        C4,
        C5
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal3/12]}).
update_internal3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    if
        Key > K2 ->
            if
                Key < K3 ->
                    update_internal3_child3(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                Key > K3 ->
                    update_internal3_child4(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    update_internal3_key3(
                        Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3, C4
                    )
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    update_internal3_child2(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                Key < K1 ->
                    update_internal3_child1(
                        Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    update_internal3_key1(
                        Key, ValueEval, ValueWrap, K2, K3, Values, Sizes, C1, C2, C3, C4
                    )
            end;
        true ->
            update_internal3_key2(Key, ValueEval, ValueWrap, K1, K3, Values, Sizes, C1, C2, C3, C4)
    end.

-compile({inline, [update_internal3_child1/12]}).
update_internal3_child1(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        Sizes,
        update_recur(Key, ValueEval, ValueWrap, C1),
        C2,
        C3,
        C4
    ).

-compile({inline, [update_internal3_child2/12]}).
update_internal3_child2(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        Sizes,
        C1,
        update_recur(Key, ValueEval, ValueWrap, C2),
        C3,
        C4
    ).

-compile({inline, [update_internal3_child3/12]}).
update_internal3_child3(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        Sizes,
        C1,
        C2,
        update_recur(Key, ValueEval, ValueWrap, C3),
        C4
    ).

-compile({inline, [update_internal3_child4/12]}).
update_internal3_child4(Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        Values,
        Sizes,
        C1,
        C2,
        C3,
        update_recur(Key, ValueEval, ValueWrap, C4)
    ).

%%%

-compile({inline, [update_internal3_key1/11]}).
update_internal3_key1(Key, ValueEval, ValueWrap, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL3(
        Key,
        K2,
        K3,
        {Value, V2, V3},
        Sizes,
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, [update_internal3_key2/11]}).
update_internal3_key2(Key, ValueEval, ValueWrap, K1, K3, Values, Sizes, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V2),
    ?INTERNAL3(
        K1,
        Key,
        K3,
        {V1, Value, V3},
        Sizes,
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, [update_internal3_key3/11]}).
update_internal3_key3(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3, C4) ->
    {V1, V2, V3} = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V3),
    ?INTERNAL3(
        K1,
        K2,
        Key,
        {V1, V2, Value},
        Sizes,
        C1,
        C2,
        C3,
        C4
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal2/10]}).
update_internal2(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    update_internal2_child2(
                        Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3
                    );
                Key > K2 ->
                    update_internal2_child3(
                        Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3
                    );
                true ->
                    update_internal2_key2(Key, ValueEval, ValueWrap, K1, Values, Sizes, C1, C2, C3)
            end;
        Key < K1 ->
            update_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3);
        true ->
            update_internal2_key1(Key, ValueEval, ValueWrap, K2, Values, Sizes, C1, C2, C3)
    end.

-compile({inline, [update_internal2_child1/10]}).
update_internal2_child1(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    ?INTERNAL2(K1, K2, Values, Sizes, update_recur(Key, ValueEval, ValueWrap, C1), C2, C3).

-compile({inline, [update_internal2_child2/10]}).
update_internal2_child2(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    ?INTERNAL2(K1, K2, Values, Sizes, C1, update_recur(Key, ValueEval, ValueWrap, C2), C3).

-compile({inline, [update_internal2_child3/10]}).
update_internal2_child3(Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, update_recur(Key, ValueEval, ValueWrap, C3)).

%%%

-compile({inline, [update_internal2_key1/9]}).
update_internal2_key1(Key, ValueEval, ValueWrap, K2, Values, Sizes, C1, C2, C3) ->
    [V1 | V2] = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL2(Key, K2, [Value | V2], Sizes, C1, C2, C3).

-compile({inline, [update_internal2_key1/9]}).
update_internal2_key2(Key, ValueEval, ValueWrap, K1, Values, Sizes, C1, C2, C3) ->
    [V1 | V2] = Values,
    Value = eval_update_value(ValueEval, ValueWrap, V2),
    ?INTERNAL2(K1, Key, [V1 | Value], Sizes, C1, C2, C3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [update_internal1/8]}).
update_internal1(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2) ->
    if
        Key < K1 ->
            update_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2);
        Key > K1 ->
            update_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2);
        true ->
            update_internal1_key1(Key, ValueEval, ValueWrap, V1, Sizes, C1, C2)
    end.

-compile({inline, [update_internal1_child1/8]}).
update_internal1_child1(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2) ->
    ?INTERNAL1(K1, V1, Sizes, update_recur(Key, ValueEval, ValueWrap, C1), C2).

-compile({inline, [update_internal1_child2/8]}).
update_internal1_child2(Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2) ->
    ?INTERNAL1(K1, V1, Sizes, C1, update_recur(Key, ValueEval, ValueWrap, C2)).

-compile({inline, [update_internal1_key1/7]}).
update_internal1_key1(Key, ValueEval, ValueWrap, V1, Sizes, C1, C2) ->
    Value = eval_update_value(ValueEval, ValueWrap, V1),
    ?INTERNAL1(Key, Value, Sizes, C1, C2).

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

-spec delete(Key, non_empty_node(Key, Value)) -> node_after_deletion(Key, Value).
delete(K, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    delete_internal1(K, K1, V1, Sizes, C1, C2);
delete(K, ?LEAF1(K1, _)) ->
    delete_leaf1(K, K1);
delete(K, ?LEAF0) ->
    error_badkey(K);
delete(K, Root) ->
    delete_recur(K, Root).

-spec delete_recur(Key, deep_node(Key, Value)) ->
    node_after_deletion(Key, Value) | unbalanced_node(Key, Value).
delete_recur(K, ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)) ->
    delete_internal4(K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
delete_recur(K, ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)) ->
    delete_internal3(K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
delete_recur(K, ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3)) ->
    delete_internal2(K, K1, K2, Values, Sizes, C1, C2, C3);
delete_recur(K, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    delete_leaf4(K, K1, K2, K3, K4, V1, V2, V3, V4);
delete_recur(K, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    delete_leaf3(K, K1, K2, K3, V1, V2, V3);
delete_recur(K, ?LEAF2(K1, K2, V1, V2)) ->
    delete_leaf2(K, K1, K2, V1, V2).

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL4

-compile({inline, delete_internal4/12}).
delete_internal4(K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    if
        K > K2 ->
            if
                K < K4 ->
                    if
                        K > K3 ->
                            delete_internal4_child4(
                                K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            );
                        K < K3 ->
                            delete_internal4_child3(
                                K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            );
                        true ->
                            delete_internal4_key3(K1, K2, K4, Values, Sizes, C1, C2, C3, C4, C5)
                    end;
                K > K4 ->
                    delete_internal4_child5(K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
                true ->
                    delete_internal4_key4(K1, K2, K3, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        K < K2 ->
            if
                K > K1 ->
                    delete_internal4_child2(K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
                K < K1 ->
                    delete_internal4_child1(K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
                true ->
                    delete_internal4_key1(K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        true ->
            delete_internal4_key2(K1, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
    end.

-compile({inline, [delete_internal4_child1/12]}).
delete_internal4_child1(K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC1 = delete_recur(K, C1),

    delete_internal4_rebalance_child1(
        K1,
        K2,
        K3,
        K4,
        V1,
        V2,
        V3,
        V4,
        Sizes,
        UpdatedC1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, [delete_internal4_child2/12]}).
delete_internal4_child2(K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC2 = delete_recur(K, C2),

    delete_internal4_rebalance_child2(
        K1,
        K2,
        K3,
        K4,
        V1,
        V2,
        V3,
        V4,
        Sizes,
        C1,
        UpdatedC2,
        C3,
        C4,
        C5
    ).

-compile({inline, [delete_internal4_child3/12]}).
delete_internal4_child3(K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC3 = delete_recur(K, C3),

    delete_internal4_rebalance_child3(
        K1,
        K2,
        K3,
        K4,
        V1,
        V2,
        V3,
        V4,
        Sizes,
        C1,
        C2,
        UpdatedC3,
        C4,
        C5
    ).

-compile({inline, [delete_internal4_child4/12]}).
delete_internal4_child4(K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC4 = delete_recur(K, C4),

    delete_internal4_rebalance_child4(
        K1,
        K2,
        K3,
        K4,
        V1,
        V2,
        V3,
        V4,
        Sizes,
        C1,
        C2,
        C3,
        UpdatedC4,
        C5
    ).

-compile({inline, [delete_internal4_child5/12]}).
delete_internal4_child5(K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC5 = delete_recur(K, C5),

    delete_internal4_rebalance_child5(
        K1,
        K2,
        K3,
        K4,
        V1,
        V2,
        V3,
        V4,
        Sizes,
        C1,
        C2,
        C3,
        C4,
        UpdatedC5
    ).

%%% Delete - INTERNAL4 - keys in node

-compile({inline, [delete_internal4_key1/10]}).
delete_internal4_key1(K2, K3, K4, {_, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    {{ReplacementK, ReplacementV}, UpdatedC2} = take_smallest_recur(C2),

    delete_internal4_rebalance_child2(
        ReplacementK,
        K2,
        K3,
        K4,
        ReplacementV,
        V2,
        V3,
        V4,
        Sizes,
        C1,
        UpdatedC2,
        C3,
        C4,
        C5
    ).

-compile({inline, [delete_internal4_key2/10]}).
delete_internal4_key2(K1, K3, K4, {V1, _, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    {{ReplacementK, ReplacementV}, UpdatedC3} = take_smallest_recur(C3),

    delete_internal4_rebalance_child3(
        K1,
        ReplacementK,
        K3,
        K4,
        V1,
        ReplacementV,
        V3,
        V4,
        Sizes,
        C1,
        C2,
        UpdatedC3,
        C4,
        C5
    ).

-compile({inline, [delete_internal4_key3/10]}).
delete_internal4_key3(K1, K2, K4, {V1, V2, _, V4}, Sizes, C1, C2, C3, C4, C5) ->
    {{ReplacementK, ReplacementV}, UpdatedC4} = take_smallest_recur(C4),

    delete_internal4_rebalance_child4(
        K1,
        K2,
        ReplacementK,
        K4,
        V1,
        V2,
        ReplacementV,
        V4,
        Sizes,
        C1,
        C2,
        C3,
        UpdatedC4,
        C5
    ).

-compile({inline, [delete_internal4_key4/10]}).
delete_internal4_key4(K1, K2, K3, {V1, V2, V3, _}, Sizes, C1, C2, C3, C4, C5) ->
    {{ReplacementK, ReplacementV}, UpdatedC5} = take_smallest_recur(C5),

    delete_internal4_rebalance_child5(
        K1,
        K2,
        K3,
        ReplacementK,
        V1,
        V2,
        V3,
        ReplacementV,
        Sizes,
        C1,
        C2,
        C3,
        C4,
        UpdatedC5
    ).

%%% Delete - INTERNAL4 - rebalance

-compile({inline, [delete_internal4_rebalance_child1/14]}).
delete_internal4_rebalance_child1(K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5) ->
    case maybe_rebalance_left(C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal4_sizes_update1(Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {UpK, UpVal, UpdatedC1, UpdatedC2, MovedSize} ->
            UpdatedSizes = internal4_sizes_move_left12(Sizes, MovedSize),
            ?INTERNAL4(
                UpK, K2, K3, K4, {UpVal, V2, V3, V4}, UpdatedSizes, UpdatedC1, UpdatedC2, C3, C4, C5
            );
        {merged, MergedC1C2} ->
            UpdatedSizes = internal4_sizes_merge12(Sizes),
            ?INTERNAL3(K2, K3, K4, {V2, V3, V4}, UpdatedSizes, MergedC1C2, C3, C4, C5)
    end.

-compile({inline, [delete_internal4_rebalance_child2/14]}).
delete_internal4_rebalance_child2(K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5) ->
    case maybe_rebalance_mid(C1, K1, V1, C2, K2, V2, C3) of
        no ->
            UpdatedSizes = internal4_sizes_update2(Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {from_left, {UpK, UpVal, UpdatedC1, RebalancedC2, MovedSize}} ->
            UpdatedSizes = internal4_sizes_move_right12(Sizes, MovedSize),
            ?INTERNAL4(
                UpK,
                K2,
                K3,
                K4,
                {UpVal, V2, V3, V4},
                UpdatedSizes,
                UpdatedC1,
                RebalancedC2,
                C3,
                C4,
                C5
            );
        {from_right, {UpK, UpVal, RebalancedC2, UpdatedC3, MovedSize}} ->
            UpdatedSizes = internal4_sizes_move_left23(Sizes, MovedSize),
            ?INTERNAL4(
                K1,
                UpK,
                K3,
                K4,
                {V1, UpVal, V3, V4},
                UpdatedSizes,
                C1,
                RebalancedC2,
                UpdatedC3,
                C4,
                C5
            );
        {from_left, {merged, MergedC1C2}} ->
            UpdatedSizes = internal4_sizes_merge12(Sizes),
            ?INTERNAL3(K2, K3, K4, {V2, V3, V4}, UpdatedSizes, MergedC1C2, C3, C4, C5)
    end.

-compile({inline, [delete_internal4_rebalance_child3/14]}).
delete_internal4_rebalance_child3(K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5) ->
    case maybe_rebalance_mid(C2, K2, V2, C3, K3, V3, C4) of
        no ->
            UpdatedSizes = internal4_sizes_update3(Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {from_left, {UpK, UpVal, UpdatedC2, RebalancedC3, MovedSize}} ->
            UpdatedSizes = internal4_sizes_move_right23(Sizes, MovedSize),
            ?INTERNAL4(
                K1,
                UpK,
                K3,
                K4,
                {V1, UpVal, V3, V4},
                UpdatedSizes,
                C1,
                UpdatedC2,
                RebalancedC3,
                C4,
                C5
            );
        {from_right, {UpK, UpVal, RebalancedC3, UpdatedC4, MovedSize}} ->
            UpdatedSizes = internal4_sizes_move_left34(Sizes, MovedSize),
            ?INTERNAL4(
                K1,
                K2,
                UpK,
                K4,
                {V1, V2, UpVal, V4},
                UpdatedSizes,
                C1,
                C2,
                RebalancedC3,
                UpdatedC4,
                C5
            );
        {from_left, {merged, MergedC2C3}} ->
            UpdatedSizes = internal4_sizes_merge23(Sizes),
            ?INTERNAL3(K1, K3, K4, {V1, V3, V4}, UpdatedSizes, C1, MergedC2C3, C4, C5)
    end.

-compile({inline, [delete_internal4_rebalance_child4/14]}).
delete_internal4_rebalance_child4(K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5) ->
    case maybe_rebalance_mid(C3, K3, V3, C4, K4, V4, C5) of
        no ->
            UpdatedSizes = internal4_sizes_update4(Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {from_left, {UpK, UpVal, UpdatedC3, RebalancedC4, MovedSize}} ->
            UpdatedSizes = internal4_sizes_move_right34(Sizes, MovedSize),
            ?INTERNAL4(
                K1,
                K2,
                UpK,
                K4,
                {V1, V2, UpVal, V4},
                UpdatedSizes,
                C1,
                C2,
                UpdatedC3,
                RebalancedC4,
                C5
            );
        {from_right, {UpK, UpVal, RebalancedC4, UpdatedC5, MovedSize}} ->
            UpdatedSizes = internal4_sizes_move_left45(Sizes, MovedSize),
            ?INTERNAL4(
                K1,
                K2,
                K3,
                UpK,
                {V1, V2, V3, UpVal},
                UpdatedSizes,
                C1,
                C2,
                C3,
                RebalancedC4,
                UpdatedC5
            );
        {from_left, {merged, MergedC3C4}} ->
            UpdatedSizes = internal4_sizes_merge34(Sizes),
            ?INTERNAL3(K1, K2, K4, {V1, V2, V4}, UpdatedSizes, C1, C2, MergedC3C4, C5)
    end.

-compile({inline, [delete_internal4_rebalance_child5/14]}).
delete_internal4_rebalance_child5(K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5) ->
    case maybe_rebalance_right(C4, K4, V4, C5) of
        no ->
            UpdatedSizes = internal4_sizes_update5(Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {UpK, UpVal, UpdatedC4, RebalancedC5, MovedSize} ->
            UpdatedSizes = internal4_sizes_move_right45(Sizes, MovedSize),
            ?INTERNAL4(
                K1,
                K2,
                K3,
                UpK,
                {V1, V2, V3, UpVal},
                UpdatedSizes,
                C1,
                C2,
                C3,
                UpdatedC4,
                RebalancedC5
            );
        {merged, MergedC4C5} ->
            UpdatedSizes = internal4_sizes_merge45(Sizes),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, MergedC4C5)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL3

-compile({inline, delete_internal3/10}).
delete_internal3(K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    if
        K > K2 ->
            if
                K > K3 ->
                    delete_internal3_child4(K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                K < K3 ->
                    delete_internal3_child3(K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                true ->
                    delete_internal3_key3(K1, K2, Values, Sizes, C1, C2, C3, C4)
            end;
        K < K2 ->
            if
                K > K1 ->
                    delete_internal3_child2(K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                K < K1 ->
                    delete_internal3_child1(K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                true ->
                    delete_internal3_key1(K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        true ->
            delete_internal3_key2(K1, K3, Values, Sizes, C1, C2, C3, C4)
    end.

-compile({inline, [delete_internal3_child1/10]}).
delete_internal3_child1(K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    UpdatedC1 = delete_recur(K, C1),

    delete_internal3_rebalance_child1(
        K1,
        K2,
        K3,
        V1,
        V2,
        V3,
        Sizes,
        UpdatedC1,
        C2,
        C3,
        C4
    ).

-compile({inline, [delete_internal3_child2/10]}).
delete_internal3_child2(K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    UpdatedC2 = delete_recur(K, C2),

    delete_internal3_rebalance_child2(
        K1,
        K2,
        K3,
        V1,
        V2,
        V3,
        Sizes,
        C1,
        UpdatedC2,
        C3,
        C4
    ).

-compile({inline, [delete_internal3_child3/10]}).
delete_internal3_child3(K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    UpdatedC3 = delete_recur(K, C3),

    delete_internal3_rebalance_child3(
        K1,
        K2,
        K3,
        V1,
        V2,
        V3,
        Sizes,
        C1,
        C2,
        UpdatedC3,
        C4
    ).

-compile({inline, [delete_internal3_child4/10]}).
delete_internal3_child4(K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    UpdatedC4 = delete_recur(K, C4),

    delete_internal3_rebalance_child4(
        K1,
        K2,
        K3,
        V1,
        V2,
        V3,
        Sizes,
        C1,
        C2,
        C3,
        UpdatedC4
    ).

%%% Delete - INTERNAL3 - keys in node

-compile({inline, [delete_internal3_key1/8]}).
delete_internal3_key1(K2, K3, {_, V2, V3}, Sizes, C1, C2, C3, C4) ->
    {{ReplacementK, ReplacementV}, UpdatedC2} = take_smallest_recur(C2),

    delete_internal3_rebalance_child2(
        ReplacementK,
        K2,
        K3,
        ReplacementV,
        V2,
        V3,
        Sizes,
        C1,
        UpdatedC2,
        C3,
        C4
    ).

-compile({inline, [delete_internal3_key2/8]}).
delete_internal3_key2(K1, K3, {V1, _, V3}, Sizes, C1, C2, C3, C4) ->
    {{ReplacementK, ReplacementV}, UpdatedC3} = take_smallest_recur(C3),

    delete_internal3_rebalance_child3(
        K1,
        ReplacementK,
        K3,
        V1,
        ReplacementV,
        V3,
        Sizes,
        C1,
        C2,
        UpdatedC3,
        C4
    ).

-compile({inline, [delete_internal3_key3/8]}).
delete_internal3_key3(K1, K2, {V1, V2, _}, Sizes, C1, C2, C3, C4) ->
    {{ReplacementK, ReplacementV}, UpdatedC4} = take_smallest_recur(C4),

    delete_internal3_rebalance_child4(
        K1,
        K2,
        ReplacementK,
        V1,
        V2,
        ReplacementV,
        Sizes,
        C1,
        C2,
        C3,
        UpdatedC4
    ).

%%% Delete - INTERNAL3 - rebalance

-compile({inline, [delete_internal3_rebalance_child1/11]}).
delete_internal3_rebalance_child1(K1, K2, K3, V1, V2, V3, Sizes, C1, C2, C3, C4) ->
    case maybe_rebalance_left(C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal3_sizes_update1(Sizes, -1),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, C4);
        {UpK, UpVal, UpdatedC1, UpdatedC2, MovedSize} ->
            UpdatedSizes = internal3_sizes_move_left12(Sizes, MovedSize),
            ?INTERNAL3(UpK, K2, K3, {UpVal, V2, V3}, UpdatedSizes, UpdatedC1, UpdatedC2, C3, C4);
        {merged, MergedC1C2} ->
            UpdatedSizes = internal3_sizes_merge12(Sizes),
            ?INTERNAL2(K2, K3, [V2 | V3], UpdatedSizes, MergedC1C2, C3, C4)
    end.

-compile({inline, [delete_internal3_rebalance_child2/11]}).
delete_internal3_rebalance_child2(K1, K2, K3, V1, V2, V3, Sizes, C1, C2, C3, C4) ->
    case maybe_rebalance_mid(C1, K1, V1, C2, K2, V2, C3) of
        no ->
            UpdatedSizes = internal3_sizes_update2(Sizes, -1),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, C4);
        {from_left, {UpK, UpVal, UpdatedC1, RebalancedC2, MovedSize}} ->
            UpdatedSizes = internal3_sizes_move_right12(Sizes, MovedSize),
            ?INTERNAL3(UpK, K2, K3, {UpVal, V2, V3}, UpdatedSizes, UpdatedC1, RebalancedC2, C3, C4);
        {from_right, {UpK, UpVal, RebalancedC2, UpdatedC3, MovedSize}} ->
            UpdatedSizes = internal3_sizes_move_left23(Sizes, MovedSize),
            ?INTERNAL3(K1, UpK, K3, {V1, UpVal, V3}, UpdatedSizes, C1, RebalancedC2, UpdatedC3, C4);
        {from_left, {merged, MergedC1C2}} ->
            UpdatedSizes = internal3_sizes_merge12(Sizes),
            ?INTERNAL2(K2, K3, [V2 | V3], UpdatedSizes, MergedC1C2, C3, C4)
    end.

-compile({inline, [delete_internal3_rebalance_child3/11]}).
delete_internal3_rebalance_child3(K1, K2, K3, V1, V2, V3, Sizes, C1, C2, C3, C4) ->
    case maybe_rebalance_mid(C2, K2, V2, C3, K3, V3, C4) of
        no ->
            UpdatedSizes = internal3_sizes_update3(Sizes, -1),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, C4);
        {from_left, {UpK, UpVal, UpdatedC2, RebalancedC3, MovedSize}} ->
            UpdatedSizes = internal3_sizes_move_right23(Sizes, MovedSize),
            ?INTERNAL3(K1, UpK, K3, {V1, UpVal, V3}, UpdatedSizes, C1, UpdatedC2, RebalancedC3, C4);
        {from_right, {UpK, UpVal, RebalancedC3, UpdatedC4, MovedSize}} ->
            UpdatedSizes = internal3_sizes_move_left34(Sizes, MovedSize),
            ?INTERNAL3(K1, K2, UpK, {V1, V2, UpVal}, UpdatedSizes, C1, C2, RebalancedC3, UpdatedC4);
        {from_left, {merged, MergedC2C3}} ->
            UpdatedSizes = internal3_sizes_merge23(Sizes),
            ?INTERNAL2(K1, K3, [V1 | V3], UpdatedSizes, C1, MergedC2C3, C4)
    end.

-compile({inline, [delete_internal3_rebalance_child4/11]}).
delete_internal3_rebalance_child4(K1, K2, K3, V1, V2, V3, Sizes, C1, C2, C3, C4) ->
    case maybe_rebalance_right(C3, K3, V3, C4) of
        no ->
            UpdatedSizes = internal3_sizes_update4(Sizes, -1),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, C4);
        {UpK, UpVal, UpdatedC3, RebalancedC4, MovedSize} ->
            UpdatedSizes = internal3_sizes_move_right34(Sizes, MovedSize),
            ?INTERNAL3(K1, K2, UpK, {V1, V2, UpVal}, UpdatedSizes, C1, C2, UpdatedC3, RebalancedC4);
        {merged, MergedC3C4} ->
            UpdatedSizes = internal3_sizes_merge34(Sizes),
            ?INTERNAL2(K1, K2, [V1 | V2], UpdatedSizes, C1, C2, MergedC3C4)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL2

-compile({inline, delete_internal2/8}).
delete_internal2(K, K1, K2, Values, Sizes, C1, C2, C3) ->
    if
        K > K1 ->
            if
                K > K2 ->
                    delete_internal2_child3(K, K1, K2, Values, Sizes, C1, C2, C3);
                K < K2 ->
                    delete_internal2_child2(K, K1, K2, Values, Sizes, C1, C2, C3);
                true ->
                    delete_internal2_key2(K1, Values, Sizes, C1, C2, C3)
            end;
        K < K1 ->
            delete_internal2_child1(K, K1, K2, Values, Sizes, C1, C2, C3);
        true ->
            delete_internal2_key1(K2, Values, Sizes, C1, C2, C3)
    end.

-compile({inline, [delete_internal2_child1/8]}).
delete_internal2_child1(K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    UpdatedC1 = delete_recur(K, C1),

    delete_internal2_rebalance_child1(
        K1,
        K2,
        V1,
        V2,
        Sizes,
        UpdatedC1,
        C2,
        C3
    ).

-compile({inline, [delete_internal2_child2/8]}).
delete_internal2_child2(K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    UpdatedC2 = delete_recur(K, C2),

    delete_internal2_rebalance_child2(
        K1,
        K2,
        V1,
        V2,
        Sizes,
        C1,
        UpdatedC2,
        C3
    ).

-compile({inline, [delete_internal2_child3/8]}).
delete_internal2_child3(K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    UpdatedC3 = delete_recur(K, C3),

    delete_internal2_rebalance_child3(
        K1,
        K2,
        V1,
        V2,
        Sizes,
        C1,
        C2,
        UpdatedC3
    ).

%%% Delete - INTERNAL2 - keys in node

-compile({inline, [delete_internal2_key1/6]}).
delete_internal2_key1(K2, [_ | V2], Sizes, C1, C2, C3) ->
    {{ReplacementK, ReplacementV}, UpdatedC2} = take_smallest_recur(C2),

    delete_internal2_rebalance_child2(
        ReplacementK,
        K2,
        ReplacementV,
        V2,
        Sizes,
        C1,
        UpdatedC2,
        C3
    ).

-compile({inline, [delete_internal2_key2/6]}).
delete_internal2_key2(K1, [V1 | _], Sizes, C1, C2, C3) ->
    {{ReplacementK, ReplacementV}, UpdatedC3} = take_smallest_recur(C3),

    delete_internal2_rebalance_child3(
        K1,
        ReplacementK,
        V1,
        ReplacementV,
        Sizes,
        C1,
        C2,
        UpdatedC3
    ).

%%% Delete - INTERNAL2 - rebalance

-compile({inline, [delete_internal2_rebalance_child1/8]}).
delete_internal2_rebalance_child1(K1, K2, V1, V2, Sizes, C1, C2, C3) ->
    case maybe_rebalance_left(C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal2_sizes_update1(Sizes, -1),
            ?INTERNAL2(K1, K2, [V1 | V2], UpdatedSizes, C1, C2, C3);
        {UpK, UpVal, UpdatedC1, UpdatedC2, MovedSize} ->
            UpdatedSizes = internal2_sizes_move_left12(Sizes, MovedSize),
            ?INTERNAL2(UpK, K2, [UpVal | V2], UpdatedSizes, UpdatedC1, UpdatedC2, C3);
        {merged, MergedC1C2} ->
            UpdatedSizes = internal2_sizes_merge12(Sizes),
            ?INTERNAL1(K2, V2, UpdatedSizes, MergedC1C2, C3)
    end.

-compile({inline, [delete_internal2_rebalance_child2/8]}).
delete_internal2_rebalance_child2(K1, K2, V1, V2, Sizes, C1, C2, C3) ->
    case maybe_rebalance_mid(C1, K1, V1, C2, K2, V2, C3) of
        no ->
            UpdatedSizes = internal2_sizes_update2(Sizes, -1),
            ?INTERNAL2(K1, K2, [V1 | V2], UpdatedSizes, C1, C2, C3);
        {from_left, {UpK, UpVal, UpdatedC1, RebalancedC2, MovedSize}} ->
            UpdatedSizes = internal2_sizes_move_right12(Sizes, MovedSize),
            ?INTERNAL2(UpK, K2, [UpVal | V2], UpdatedSizes, UpdatedC1, RebalancedC2, C3);
        {from_right, {UpK, UpVal, RebalancedC2, UpdatedC3, MovedSize}} ->
            UpdatedSizes = internal2_sizes_move_left23(Sizes, MovedSize),
            ?INTERNAL2(K1, UpK, [V1 | UpVal], UpdatedSizes, C1, RebalancedC2, UpdatedC3);
        {from_left, {merged, MergedC1C2}} ->
            UpdatedSizes = internal2_sizes_merge12(Sizes),
            ?INTERNAL1(K2, V2, UpdatedSizes, MergedC1C2, C3)
    end.

-compile({inline, [delete_internal2_rebalance_child3/8]}).
delete_internal2_rebalance_child3(K1, K2, V1, V2, Sizes, C1, C2, C3) ->
    case maybe_rebalance_right(C2, K2, V2, C3) of
        no ->
            UpdatedSizes = internal2_sizes_update3(Sizes, -1),
            ?INTERNAL2(K1, K2, [V1 | V2], UpdatedSizes, C1, C2, C3);
        {UpK, UpVal, UpdatedC2, RebalancedC3, MovedSize} ->
            UpdatedSizes = internal2_sizes_move_right23(Sizes, MovedSize),
            ?INTERNAL2(K1, UpK, [V1 | UpVal], UpdatedSizes, C1, UpdatedC2, RebalancedC3);
        {merged, MergedC2C3} ->
            UpdatedSizes = internal2_sizes_merge23(Sizes),
            ?INTERNAL1(K1, V1, UpdatedSizes, C1, MergedC2C3)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL1

-compile({inline, delete_internal1/6}).
delete_internal1(K, K1, V1, Sizes, C1, C2) ->
    if
        K > K1 ->
            delete_internal1_child2(K, K1, V1, Sizes, C1, C2);
        K < K1 ->
            delete_internal1_child1(K, K1, V1, Sizes, C1, C2);
        true ->
            delete_internal1_key1(Sizes, C1, C2)
    end.

-compile({inline, [delete_internal1_child1/6]}).
delete_internal1_child1(K, K1, V1, Sizes, C1, C2) ->
    UpdatedC1 = delete_recur(K, C1),

    delete_internal1_rebalance_child1(
        K1,
        V1,
        Sizes,
        UpdatedC1,
        C2
    ).

-compile({inline, [delete_internal1_child2/6]}).
delete_internal1_child2(K, K1, V1, Sizes, C1, C2) ->
    UpdatedC2 = delete_recur(K, C2),

    delete_internal1_rebalance_child2(
        K1,
        V1,
        Sizes,
        C1,
        UpdatedC2
    ).

%%% Delete - INTERNAL1 - key in node

-compile({inline, [delete_internal1_key1/3]}).
delete_internal1_key1(Sizes, C1, C2) ->
    {{ReplacementK, ReplacementV}, UpdatedC2} = take_smallest_recur(C2),

    delete_internal1_rebalance_child2(
        ReplacementK,
        ReplacementV,
        Sizes,
        C1,
        UpdatedC2
    ).

%%% Delete - INTERNAL1 - rebalance

-compile({inline, [delete_internal1_rebalance_child1/5]}).
delete_internal1_rebalance_child1(K1, V1, Sizes, C1, C2) ->
    case maybe_rebalance_left(C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal1_sizes_update1(Sizes, -1),
            ?INTERNAL1(K1, V1, UpdatedSizes, C1, C2);
        {UpK, UpVal, UpdatedC1, UpdatedC2, MovedSize} ->
            UpdatedSizes = internal1_sizes_move_left12(Sizes, MovedSize),
            ?INTERNAL1(UpK, UpVal, UpdatedSizes, UpdatedC1, UpdatedC2);
        {merged, MergedC1C2} ->
            % This can only happen on root - height is reduced
            MergedC1C2
    end.

-compile({inline, [delete_internal1_rebalance_child2/5]}).
delete_internal1_rebalance_child2(K1, V1, Sizes, C1, C2) ->
    case maybe_rebalance_right(C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal1_sizes_update2(Sizes, -1),
            ?INTERNAL1(K1, V1, UpdatedSizes, C1, C2);
        {UpK, UpVal, UpdatedC1, RebalancedC2, MovedSize} ->
            UpdatedSizes = internal1_sizes_move_right12(Sizes, MovedSize),
            ?INTERNAL1(UpK, UpVal, UpdatedSizes, UpdatedC1, RebalancedC2);
        {merged, MergedC1C2} ->
            % This can only happen on root - height is reduced
            MergedC1C2
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

root_take_smallest(?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    {TakenPair, UpdatedC1} = take_smallest_recur(C1),

    {TakenPair,
        delete_internal1_rebalance_child1(
            K1,
            V1,
            Sizes,
            UpdatedC1,
            C2
        )};
root_take_smallest(?LEAF1(K1, V1)) ->
    {{K1, V1}, ?LEAF0};
root_take_smallest(?LEAF0) ->
    error_empty_tree();
root_take_smallest(Node) ->
    take_smallest_recur(Node).

take_smallest_recur(?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)) ->
    {TakenPair, UpdatedC1} = take_smallest_recur(C1),
    {V1, V2, V3, V4} = Values,

    {TakenPair,
        delete_internal4_rebalance_child1(
            K1,
            K2,
            K3,
            K4,
            V1,
            V2,
            V3,
            V4,
            Sizes,
            UpdatedC1,
            C2,
            C3,
            C4,
            C5
        )};
take_smallest_recur(?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)) ->
    {TakenPair, UpdatedC1} = take_smallest_recur(C1),
    {V1, V2, V3} = Values,

    {TakenPair,
        delete_internal3_rebalance_child1(
            K1,
            K2,
            K3,
            V1,
            V2,
            V3,
            Sizes,
            UpdatedC1,
            C2,
            C3,
            C4
        )};
take_smallest_recur(?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3)) ->
    {TakenPair, UpdatedC1} = take_smallest_recur(C1),
    [V1 | V2] = Values,

    {TakenPair,
        delete_internal2_rebalance_child1(
            K1,
            K2,
            V1,
            V2,
            Sizes,
            UpdatedC1,
            C2,
            C3
        )};
take_smallest_recur(?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    {{K1, V1}, ?LEAF3(K2, K3, K4, V2, V3, V4)};
take_smallest_recur(?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    {{K1, V1}, ?LEAF2(K2, K3, V2, V3)};
take_smallest_recur(?LEAF2(K1, K2, V1, V2)) ->
    {{K1, V1}, ?LEAF1(K2, V2)}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - Largest
%% ------------------------------------------------------------------

root_take_largest(?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    {TakenPair, UpdatedC2} = take_largest_recur(C2),

    {TakenPair,
        delete_internal1_rebalance_child2(
            K1,
            V1,
            Sizes,
            C1,
            UpdatedC2
        )};
root_take_largest(?LEAF1(K1, V1)) ->
    {{K1, V1}, ?LEAF0};
root_take_largest(?LEAF0) ->
    error_empty_tree();
root_take_largest(Node) ->
    take_largest_recur(Node).

take_largest_recur(?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)) ->
    {TakenPair, UpdatedC5} = take_largest_recur(C5),
    {V1, V2, V3, V4} = Values,

    {TakenPair,
        delete_internal4_rebalance_child5(
            K1,
            K2,
            K3,
            K4,
            V1,
            V2,
            V3,
            V4,
            Sizes,
            C1,
            C2,
            C3,
            C4,
            UpdatedC5
        )};
take_largest_recur(?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)) ->
    {TakenPair, UpdatedC4} = take_largest_recur(C4),
    {V1, V2, V3} = Values,

    {TakenPair,
        delete_internal3_rebalance_child4(
            K1,
            K2,
            K3,
            V1,
            V2,
            V3,
            Sizes,
            C1,
            C2,
            C3,
            UpdatedC4
        )};
take_largest_recur(?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3)) ->
    {TakenPair, UpdatedC3} = take_largest_recur(C3),
    [V1 | V2] = Values,

    {TakenPair,
        delete_internal2_rebalance_child3(
            K1,
            K2,
            V1,
            V2,
            Sizes,
            C1,
            C2,
            UpdatedC3
        )};
take_largest_recur(?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    {{K4, V4}, ?LEAF3(K1, K2, K3, V1, V2, V3)};
take_largest_recur(?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    {{K3, V3}, ?LEAF2(K1, K2, V1, V2)};
take_largest_recur(?LEAF2(K1, K2, V1, V2)) ->
    {{K2, V2}, ?LEAF1(K1, V1)}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - Key
%% ------------------------------------------------------------------

-spec root_take(todo, Key, non_empty_node(Key, Value)) ->
    {{Key, Value}, node_after_deletion(Key, Value)}.
root_take(Type, K, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    take_internal1(Type, K, K1, V1, Sizes, C1, C2);
root_take(Type, K, ?LEAF1(K1, V1)) ->
    take_leaf1(Type, K, K1, V1);
root_take(key, K, ?LEAF0) ->
    error_badkey(K);
root_take(Type, K, Root) ->
    take_recur(Type, K, Root).

-spec take_recur(todo, Key | nil, deep_node(Key, Value)) ->
    {{Key, Value}, node_after_deletion(Key, Value) | unbalanced_node(Key, Value)}.
take_recur(Type, K, ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)) ->
    take_internal4(Type, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
take_recur(Type, K, ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)) ->
    take_internal3(Type, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
take_recur(Type, K, ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3)) ->
    take_internal2(Type, K, K1, K2, Values, Sizes, C1, C2, C3);
take_recur(Type, K, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    take_leaf4(Type, K, K1, K2, K3, K4, V1, V2, V3, V4);
take_recur(Type, K, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    take_leaf3(Type, K, K1, K2, K3, V1, V2, V3);
take_recur(Type, K, ?LEAF2(K1, K2, V1, V2)) ->
    take_leaf2(Type, K, K1, K2, V1, V2).

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL4

-compile({inline, take_internal4/13}).
take_internal4(key, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    if
        K > K2 ->
            if
                K < K4 ->
                    if
                        K > K3 ->
                            take_internal4_child4(
                                key, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            );
                        K < K3 ->
                            take_internal4_child3(
                                key, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            );
                        true ->
                            take_internal4_key3(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
                    end;
                K > K4 ->
                    take_internal4_child5(
                        key, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    take_internal4_key4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        K < K2 ->
            if
                K > K1 ->
                    take_internal4_child2(
                        key, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                K < K1 ->
                    take_internal4_child1(
                        key, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    take_internal4_key1(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        true ->
            take_internal4_key2(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
    end;
take_internal4(nth, N, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    {S1, S2, S3, S4, _} = internal4_sizes_unpack(Sizes),

    Pos3 = S1 + S2 + S3 + 3,

    if
        N < Pos3 ->
            Pos1 = S1 + 1,

            if
                N > Pos1 ->
                    Pos2 = Pos1 + S2 + 1,

                    if
                        N < Pos2 ->
                            take_internal4_child2(
                                nth, N - Pos1, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            );
                        N > Pos2 ->
                            take_internal4_child3(
                                nth, N - Pos2, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            );
                        true ->
                            take_internal4_key2(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
                    end;
                N < Pos1 ->
                    take_internal4_child1(
                        nth, N, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    take_internal4_key3(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        N > Pos3 ->
            Pos4 = Pos3 + S4 + 1,

            if
                N < Pos4 ->
                    take_internal4_child4(
                        nth, N - Pos3, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                N > Pos4 ->
                    take_internal4_child5(
                        nth, N - Pos4, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    take_internal4_key4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        true ->
            take_internal4_key3(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
    end.

%%

-compile({inline, [take_internal4_child1/13]}).
take_internal4_child1(Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    {TakenPair, UpdatedC1} = take_recur(Type, K, C1),

    {TakenPair,
        delete_internal4_rebalance_child1(
            K1,
            K2,
            K3,
            K4,
            V1,
            V2,
            V3,
            V4,
            Sizes,
            UpdatedC1,
            C2,
            C3,
            C4,
            C5
        )}.

-compile({inline, [take_internal4_child2/13]}).
take_internal4_child2(Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    {TakenPair, UpdatedC2} = take_recur(Type, K, C2),

    {TakenPair,
        delete_internal4_rebalance_child2(
            K1,
            K2,
            K3,
            K4,
            V1,
            V2,
            V3,
            V4,
            Sizes,
            C1,
            UpdatedC2,
            C3,
            C4,
            C5
        )}.

-compile({inline, [take_internal4_child3/13]}).
take_internal4_child3(Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    {TakenPair, UpdatedC3} = take_recur(Type, K, C3),

    {TakenPair,
        delete_internal4_rebalance_child3(
            K1,
            K2,
            K3,
            K4,
            V1,
            V2,
            V3,
            V4,
            Sizes,
            C1,
            C2,
            UpdatedC3,
            C4,
            C5
        )}.

-compile({inline, [take_internal4_child4/13]}).
take_internal4_child4(Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    {TakenPair, UpdatedC4} = take_recur(Type, K, C4),

    {TakenPair,
        delete_internal4_rebalance_child4(
            K1,
            K2,
            K3,
            K4,
            V1,
            V2,
            V3,
            V4,
            Sizes,
            C1,
            C2,
            C3,
            UpdatedC4,
            C5
        )}.

-compile({inline, [take_internal4_child5/13]}).
take_internal4_child5(Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    {TakenPair, UpdatedC5} = take_recur(Type, K, C5),

    {TakenPair,
        delete_internal4_rebalance_child5(
            K1,
            K2,
            K3,
            K4,
            V1,
            V2,
            V3,
            V4,
            Sizes,
            C1,
            C2,
            C3,
            C4,
            UpdatedC5
        )}.

%%% Take - INTERNAL4 - keys in node

-compile({inline, [take_internal4_key1/11]}).
take_internal4_key1(K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    TakenPair = {K1, V1},
    {{ReplacementK, ReplacementV}, UpdatedC2} = take_smallest_recur(C2),

    {TakenPair,
        delete_internal4_rebalance_child2(
            ReplacementK,
            K2,
            K3,
            K4,
            ReplacementV,
            V2,
            V3,
            V4,
            Sizes,
            C1,
            UpdatedC2,
            C3,
            C4,
            C5
        )}.

-compile({inline, [take_internal4_key2/11]}).
take_internal4_key2(K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    TakenPair = {K2, V2},
    {{ReplacementK, ReplacementV}, UpdatedC3} = take_smallest_recur(C3),

    {TakenPair,
        delete_internal4_rebalance_child3(
            K1,
            ReplacementK,
            K3,
            K4,
            V1,
            ReplacementV,
            V3,
            V4,
            Sizes,
            C1,
            C2,
            UpdatedC3,
            C4,
            C5
        )}.

-compile({inline, [take_internal4_key3/11]}).
take_internal4_key3(K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    TakenPair = {K3, V3},
    {{ReplacementK, ReplacementV}, UpdatedC4} = take_smallest_recur(C4),

    {TakenPair,
        delete_internal4_rebalance_child4(
            K1,
            K2,
            ReplacementK,
            K4,
            V1,
            V2,
            ReplacementV,
            V4,
            Sizes,
            C1,
            C2,
            C3,
            UpdatedC4,
            C5
        )}.

-compile({inline, [take_internal4_key4/11]}).
take_internal4_key4(K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    TakenPair = {K4, V4},
    {{ReplacementK, ReplacementV}, UpdatedC5} = take_smallest_recur(C5),

    {TakenPair,
        delete_internal4_rebalance_child5(
            K1,
            K2,
            K3,
            ReplacementK,
            V1,
            V2,
            V3,
            ReplacementV,
            Sizes,
            C1,
            C2,
            C3,
            C4,
            UpdatedC5
        )}.

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL3

-compile({inline, take_internal3/11}).
take_internal3(key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    if
        K > K2 ->
            if
                K > K3 ->
                    take_internal3_child4(key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                K < K3 ->
                    take_internal3_child3(key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                true ->
                    take_internal3_key3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        K < K2 ->
            if
                K > K1 ->
                    take_internal3_child2(key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                K < K1 ->
                    take_internal3_child1(key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                true ->
                    take_internal3_key1(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        true ->
            take_internal3_key2(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
    end;
take_internal3(nth, N, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    {S1, S2, S3, _} = internal3_sizes_unpack(Sizes),

    Pos2 = S1 + S2 + 2,

    if
        N < Pos2 ->
            Pos1 = S1 + 1,

            if
                N < Pos1 ->
                    take_internal3_child1(nth, N, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                N > Pos1 ->
                    take_internal3_child2(nth, N - Pos1, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                true ->
                    take_internal3_key1(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        N > Pos2 ->
            Pos3 = Pos2 + S3 + 1,

            if
                N < Pos3 ->
                    take_internal3_child3(nth, N - Pos2, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                N > Pos3 ->
                    take_internal3_child4(nth, N - Pos3, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                true ->
                    take_internal3_key3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        true ->
            take_internal3_key2(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
    end.

%%

-compile({inline, [take_internal3_child1/11]}).
take_internal3_child1(Type, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    {TakenPair, UpdatedC1} = take_recur(Type, K, C1),

    {TakenPair,
        delete_internal3_rebalance_child1(
            K1,
            K2,
            K3,
            V1,
            V2,
            V3,
            Sizes,
            UpdatedC1,
            C2,
            C3,
            C4
        )}.

-compile({inline, [take_internal3_child2/11]}).
take_internal3_child2(Type, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    {TakenPair, UpdatedC2} = take_recur(Type, K, C2),

    {TakenPair,
        delete_internal3_rebalance_child2(
            K1,
            K2,
            K3,
            V1,
            V2,
            V3,
            Sizes,
            C1,
            UpdatedC2,
            C3,
            C4
        )}.

-compile({inline, [take_internal3_child3/11]}).
take_internal3_child3(Type, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    {TakenPair, UpdatedC3} = take_recur(Type, K, C3),

    {TakenPair,
        delete_internal3_rebalance_child3(
            K1,
            K2,
            K3,
            V1,
            V2,
            V3,
            Sizes,
            C1,
            C2,
            UpdatedC3,
            C4
        )}.

-compile({inline, [take_internal3_child4/11]}).
take_internal3_child4(Type, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    {TakenPair, UpdatedC4} = take_recur(Type, K, C4),

    {TakenPair,
        delete_internal3_rebalance_child4(
            K1,
            K2,
            K3,
            V1,
            V2,
            V3,
            Sizes,
            C1,
            C2,
            C3,
            UpdatedC4
        )}.

%%% Take - INTERNAL3 - keys in node

-compile({inline, [take_internal3_key1/9]}).
take_internal3_key1(K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    TakenPair = {K1, V1},
    {{ReplacementK, ReplacementV}, UpdatedC2} = take_smallest_recur(C2),

    {TakenPair,
        delete_internal3_rebalance_child2(
            ReplacementK,
            K2,
            K3,
            ReplacementV,
            V2,
            V3,
            Sizes,
            C1,
            UpdatedC2,
            C3,
            C4
        )}.

-compile({inline, [take_internal3_key2/9]}).
take_internal3_key2(K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    TakenPair = {K2, V2},
    {{ReplacementK, ReplacementV}, UpdatedC3} = take_smallest_recur(C3),

    {TakenPair,
        delete_internal3_rebalance_child3(
            K1,
            ReplacementK,
            K3,
            V1,
            ReplacementV,
            V3,
            Sizes,
            C1,
            C2,
            UpdatedC3,
            C4
        )}.

-compile({inline, [take_internal3_key3/9]}).
take_internal3_key3(K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    TakenPair = {K3, V3},
    {{ReplacementK, ReplacementV}, UpdatedC4} = take_smallest_recur(C4),

    {TakenPair,
        delete_internal3_rebalance_child4(
            K1,
            K2,
            ReplacementK,
            V1,
            V2,
            ReplacementV,
            Sizes,
            C1,
            C2,
            C3,
            UpdatedC4
        )}.

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL2

-compile({inline, take_internal2/9}).
take_internal2(key, K, K1, K2, Values, Sizes, C1, C2, C3) ->
    if
        K > K1 ->
            if
                K > K2 ->
                    take_internal2_child3(key, K, K1, K2, Values, Sizes, C1, C2, C3);
                K < K2 ->
                    take_internal2_child2(key, K, K1, K2, Values, Sizes, C1, C2, C3);
                true ->
                    take_internal2_key2(K1, K2, Values, Sizes, C1, C2, C3)
            end;
        K < K1 ->
            take_internal2_child1(key, K, K1, K2, Values, Sizes, C1, C2, C3);
        true ->
            take_internal2_key1(K1, K2, Values, Sizes, C1, C2, C3)
    end;
take_internal2(nth, N, K1, K2, Values, Sizes, C1, C2, C3) ->
    {S1, S2, _} = internal2_sizes_unpack(Sizes),

    Pos1 = S1 + 1,

    if
        N > Pos1 ->
            Pos2 = Pos1 + S2 + 1,

            if
                N < Pos2 ->
                    take_internal2_child2(nth, N - Pos1, K1, K2, Values, Sizes, C1, C2, C3);
                N > Pos2 ->
                    take_internal2_child3(nth, N - Pos2, K1, K2, Values, Sizes, C1, C2, C3);
                true ->
                    take_internal2_key2(K1, K2, Values, Sizes, C1, C2, C3)
            end;
        N < Pos1 ->
            take_internal2_child1(nth, N, K1, K2, Values, Sizes, C1, C2, C3);
        true ->
            take_internal2_key1(K1, K2, Values, Sizes, C1, C2, C3)
    end.

-compile({inline, [take_internal2_child1/9]}).
take_internal2_child1(Type, K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    {TakenPair, UpdatedC1} = take_recur(Type, K, C1),

    {TakenPair,
        delete_internal2_rebalance_child1(
            K1,
            K2,
            V1,
            V2,
            Sizes,
            UpdatedC1,
            C2,
            C3
        )}.

-compile({inline, [take_internal2_child2/9]}).
take_internal2_child2(Type, K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    {TakenPair, UpdatedC2} = take_recur(Type, K, C2),

    {TakenPair,
        delete_internal2_rebalance_child2(
            K1,
            K2,
            V1,
            V2,
            Sizes,
            C1,
            UpdatedC2,
            C3
        )}.

-compile({inline, [take_internal2_child3/9]}).
take_internal2_child3(Type, K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    {TakenPair, UpdatedC3} = take_recur(Type, K, C3),

    {TakenPair,
        delete_internal2_rebalance_child3(
            K1,
            K2,
            V1,
            V2,
            Sizes,
            C1,
            C2,
            UpdatedC3
        )}.

%%% Take - INTERNAL2 - keys in node

-compile({inline, [take_internal2_key1/7]}).
take_internal2_key1(K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    TakenPair = {K1, V1},
    {{ReplacementK, ReplacementV}, UpdatedC2} = take_smallest_recur(C2),

    {TakenPair,
        delete_internal2_rebalance_child2(
            ReplacementK,
            K2,
            ReplacementV,
            V2,
            Sizes,
            C1,
            UpdatedC2,
            C3
        )}.

-compile({inline, [take_internal2_key2/7]}).
take_internal2_key2(K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    TakenPair = {K2, V2},
    {{ReplacementK, ReplacementV}, UpdatedC3} = take_smallest_recur(C3),

    {TakenPair,
        delete_internal2_rebalance_child3(
            K1,
            ReplacementK,
            V1,
            ReplacementV,
            Sizes,
            C1,
            C2,
            UpdatedC3
        )}.

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL1

-compile({inline, take_internal1/7}).
take_internal1(key, K, K1, V1, Sizes, C1, C2) ->
    if
        K > K1 ->
            take_internal1_child2(key, K, K1, V1, Sizes, C1, C2);
        K < K1 ->
            take_internal1_child1(key, K, K1, V1, Sizes, C1, C2);
        true ->
            take_internal1_key1(K1, V1, Sizes, C1, C2)
    end;
take_internal1(nth, N, K1, V1, Sizes, C1, C2) ->
    [S1 | _] = internal1_sizes_unpack(Sizes),

    Pos1 = S1 + 1,

    if
        N < Pos1 ->
            take_internal1_child1(nth, N, K1, V1, Sizes, C1, C2);
        N > Pos1 ->
            take_internal1_child2(nth, N - Pos1, K1, V1, Sizes, C1, C2);
        true ->
            take_internal1_key1(K1, V1, Sizes, C1, C2)
    end.

-compile({inline, [take_internal1_child1/7]}).
take_internal1_child1(Type, K, K1, V1, Sizes, C1, C2) ->
    {TakenPair, UpdatedC1} = take_recur(Type, K, C1),

    {TakenPair,
        delete_internal1_rebalance_child1(
            K1,
            V1,
            Sizes,
            UpdatedC1,
            C2
        )}.

-compile({inline, [take_internal1_child2/7]}).
take_internal1_child2(Type, K, K1, V1, Sizes, C1, C2) ->
    {TakenPair, UpdatedC2} = take_recur(Type, K, C2),

    {TakenPair,
        delete_internal1_rebalance_child2(
            K1,
            V1,
            Sizes,
            C1,
            UpdatedC2
        )}.

%%% Take - INTERNAL1 - key in node

-compile({inline, [take_internal1_key1/5]}).
take_internal1_key1(K1, V1, Sizes, C1, C2) ->
    TakenPair = {K1, V1},
    {{ReplacementK, ReplacementV}, UpdatedC2} = take_smallest_recur(C2),

    {TakenPair,
        delete_internal1_rebalance_child2(
            ReplacementK,
            ReplacementV,
            Sizes,
            C1,
            UpdatedC2
        )}.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF4

-compile({inline, take_leaf4/10}).
take_leaf4(key, K, K1, K2, K3, K4, V1, V2, V3, V4) ->
    if
        K > K2 ->
            if
                K == K3 ->
                    {{K3, V3}, ?LEAF3(K1, K2, K4, V1, V2, V4)};
                K == K4 ->
                    {{K4, V4}, ?LEAF3(K1, K2, K3, V1, V2, V3)};
                true ->
                    error_badkey(K)
            end;
        K == K2 ->
            {{K2, V2}, ?LEAF3(K1, K3, K4, V1, V3, V4)};
        K == K1 ->
            {{K1, V1}, ?LEAF3(K2, K3, K4, V2, V3, V4)};
        true ->
            error_badkey(K)
    end;
take_leaf4(nth, N, K1, K2, K3, K4, V1, V2, V3, V4) ->
    case N of
        1 -> {{K1, V1}, ?LEAF3(K2, K3, K4, V2, V3, V4)};
        2 -> {{K2, V2}, ?LEAF3(K1, K3, K4, V1, V3, V4)};
        3 -> {{K3, V3}, ?LEAF3(K1, K2, K4, V1, V2, V4)};
        4 -> {{K4, V4}, ?LEAF3(K1, K2, K3, V1, V2, V3)}
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF3

-compile({inline, take_leaf3/8}).
take_leaf3(key, K, K1, K2, K3, V1, V2, V3) ->
    if
        K < K2 ->
            if
                K == K1 ->
                    {{K1, V1}, ?LEAF2(K2, K3, V2, V3)};
                true ->
                    error_badkey(K)
            end;
        K > K2 ->
            if
                K == K3 ->
                    {{K3, V3}, ?LEAF2(K1, K2, V1, V2)};
                true ->
                    error_badkey(K)
            end;
        true ->
            {{K2, V2}, ?LEAF2(K1, K3, V1, V3)}
    end;
take_leaf3(nth, N, K1, K2, K3, V1, V2, V3) ->
    case N of
        1 -> {{K1, V1}, ?LEAF2(K2, K3, V2, V3)};
        2 -> {{K2, V2}, ?LEAF2(K1, K3, V1, V3)};
        3 -> {{K3, V3}, ?LEAF2(K1, K2, V1, V2)}
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF2

-compile({inline, take_leaf2/6}).
take_leaf2(key, K, K1, K2, V1, V2) ->
    if
        K == K1 ->
            {{K1, V1}, ?LEAF1(K2, V2)};
        K == K2 ->
            {{K2, V2}, ?LEAF1(K1, V1)};
        true ->
            error_badkey(K)
    end;
take_leaf2(nth, N, K1, K2, V1, V2) ->
    case N of
        1 ->
            {{K1, V1}, ?LEAF1(K2, V2)};
        2 ->
            {{K2, V2}, ?LEAF1(K1, V1)}
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF1

-compile({inline, take_leaf1/4}).
take_leaf1(key, K, K1, V1) ->
    if
        K == K1 ->
            TakenPair = {K1, V1},
            {TakenPair, ?LEAF0};
        true ->
            error_badkey(K)
    end;
take_leaf1(nth, 1, K1, V1) ->
    TakenPair = {K1, V1},
    {TakenPair, ?LEAF0}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Rebalance
%% ------------------------------------------------------------------

%%%%%%%%%
%%%%%%%%%
%%% Rebalance from Right into Left

-compile({inline, maybe_rebalance_left/4}).
maybe_rebalance_left(Left, ParentK, ParentV, Right) ->
    maybe_rebalance_left(Left, ParentK, ParentV, Right, _Merge = true).

-compile({inline, maybe_rebalance_left/5}).
maybe_rebalance_left(Left, ParentK, ParentV, Right, Merge) ->
    case Left of
        ?INTERNAL1(K1, V1, Sizes, C1, C2) ->
            rebalance_left_internal1(K1, V1, Sizes, C1, C2, ParentK, ParentV, Right, Merge);
        ?LEAF1(K1, V1) ->
            rebalance_left_leaf1(K1, V1, ParentK, ParentV, Right, Merge);
        _ ->
            no
    end.

-compile({inline, [rebalance_left_internal1/9]}).
rebalance_left_internal1(LeftK1, LeftV1, LeftSizes, LeftC1, LeftC2, ParentK, ParentV, Right, Merge) ->
    case Right of
        ?INTERNAL4(
            RightK1,
            RightK2,
            RightK3,
            RightK4,
            RightValues,
            RightSizes,
            RightC1,
            RightC2,
            RightC3,
            RightC4,
            RightC5
        ) ->
            rebalance_left_internal1_internal4(
                LeftK1,
                LeftV1,
                LeftSizes,
                LeftC1,
                LeftC2,
                ParentK,
                ParentV,
                RightK1,
                RightK2,
                RightK3,
                RightK4,
                RightValues,
                RightSizes,
                RightC1,
                RightC2,
                RightC3,
                RightC4,
                RightC5
            );
        ?INTERNAL3(
            RightK1,
            RightK2,
            RightK3,
            RightValues,
            RightSizes,
            RightC1,
            RightC2,
            RightC3,
            RightC4
        ) ->
            rebalance_left_internal1_internal3(
                LeftK1,
                LeftV1,
                LeftSizes,
                LeftC1,
                LeftC2,
                ParentK,
                ParentV,
                RightK1,
                RightK2,
                RightK3,
                RightValues,
                RightSizes,
                RightC1,
                RightC2,
                RightC3,
                RightC4
            );
        ?INTERNAL2(RightK1, RightK2, RightValues, RightSizes, RightC1, RightC2, RightC3) ->
            rebalance_left_internal1_internal2(
                LeftK1,
                LeftV1,
                LeftSizes,
                LeftC1,
                LeftC2,
                ParentK,
                ParentV,
                RightK1,
                RightK2,
                RightValues,
                RightSizes,
                RightC1,
                RightC2,
                RightC3,
                Merge
            )
    end.

-compile({inline, rebalance_left_internal1_internal4/18}).
rebalance_left_internal1_internal4(
    LeftK1,
    LeftV1,
    LeftSizes,
    LeftC1,
    LeftC2,
    ParentK,
    ParentV,
    RightK1,
    RightK2,
    RightK3,
    RightK4,
    RightValues,
    RightSizes,
    RightC1,
    RightC2,
    RightC3,
    RightC4,
    RightC5
) ->
    {RightV1, RightV2, RightV3, RightV4} = RightValues,

    [LeftS1 | LeftS2] = internal1_sizes_unpack(LeftSizes),
    {RightS1, RightS2, RightS3, RightS4, RightS5} = internal4_sizes_unpack(RightSizes),

    UpK = RightK1,
    UpVal = RightV1,
    MovedChild = RightC1,
    MovedSize = RightS1,

    UpdatedLeft = ?INTERNAL2(
        LeftK1,
        ParentK,
        [LeftV1 | ParentV],
        internal2_sizes_pack(LeftS1, LeftS2, MovedSize),
        LeftC1,
        LeftC2,
        MovedChild
    ),

    UpdatedRight = ?INTERNAL3(
        RightK2,
        RightK3,
        RightK4,
        {RightV2, RightV3, RightV4},
        internal3_sizes_pack(RightS2, RightS3, RightS4, RightS5),
        RightC2,
        RightC3,
        RightC4,
        RightC5
    ),

    {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize}.

-compile({inline, rebalance_left_internal1_internal3/16}).
rebalance_left_internal1_internal3(
    LeftK1,
    LeftV1,
    LeftSizes,
    LeftC1,
    LeftC2,
    ParentK,
    ParentV,
    RightK1,
    RightK2,
    RightK3,
    RightValues,
    RightSizes,
    RightC1,
    RightC2,
    RightC3,
    RightC4
) ->
    {RightV1, RightV2, RightV3} = RightValues,

    [LeftS1 | LeftS2] = internal1_sizes_unpack(LeftSizes),
    {RightS1, RightS2, RightS3, RightS4} = internal3_sizes_unpack(RightSizes),

    UpK = RightK1,
    UpVal = RightV1,
    MovedChild = RightC1,
    MovedSize = RightS1,

    UpdatedLeft = ?INTERNAL2(
        LeftK1,
        ParentK,
        [LeftV1 | ParentV],
        internal2_sizes_pack(LeftS1, LeftS2, MovedSize),
        LeftC1,
        LeftC2,
        MovedChild
    ),

    UpdatedRight = ?INTERNAL2(
        RightK2,
        RightK3,
        [RightV2 | RightV3],
        internal2_sizes_pack(RightS2, RightS3, RightS4),
        RightC2,
        RightC3,
        RightC4
    ),
    {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize}.

-compile({inline, rebalance_left_internal1_internal2/15}).
rebalance_left_internal1_internal2(
    LeftK1,
    LeftV1,
    LeftSizes,
    LeftC1,
    LeftC2,
    ParentK,
    ParentV,
    RightK1,
    RightK2,
    RightValues,
    RightSizes,
    RightC1,
    RightC2,
    RightC3,
    Merge
) ->
    [RightV1 | RightV2] = RightValues,

    [LeftS1 | LeftS2] = internal1_sizes_unpack(LeftSizes),
    {RightS1, RightS2, RightS3} = internal2_sizes_unpack(RightSizes),

    if
        Merge ->
            {merged,
                ?INTERNAL4(
                    LeftK1,
                    ParentK,
                    RightK1,
                    RightK2,
                    {LeftV1, ParentV, RightV1, RightV2},
                    internal4_sizes_pack(LeftS1, LeftS2, RightS1, RightS2, RightS3),
                    LeftC1,
                    LeftC2,
                    RightC1,
                    RightC2,
                    RightC3
                )};
        true ->
            no
    end.

-compile({inline, [rebalance_left_leaf1/6]}).
rebalance_left_leaf1(LeftK1, LeftV1, ParentK, ParentV, Right, Merge) ->
    % Why is MovedSize 0:
    % * We deleted a key in the left node
    % * Then we're gonna move a key from right to left
    %
    % So, left's size doesn't change. Right does - by -1 - which is what
    % the deletion rebalance functions assume (one additional key lost).
    %
    % Therefore we return 0 and everything works as expected.

    case Right of
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            UpK = K1,
            UpVal = V1,
            UpdatedLeft = ?LEAF2(LeftK1, ParentK, LeftV1, ParentV),
            UpdatedRight = ?LEAF3(K2, K3, K4, V2, V3, V4),
            MovedSize = 0,
            {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize};
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            UpK = K1,
            UpVal = V1,
            UpdatedLeft = ?LEAF2(LeftK1, ParentK, LeftV1, ParentV),
            UpdatedRight = ?LEAF2(K2, K3, V2, V3),
            MovedSize = 0,
            {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize};
        ?LEAF2(K1, K2, V1, V2) ->
            if
                Merge ->
                    {merged,
                        ?LEAF4(
                            LeftK1,
                            ParentK,
                            K1,
                            K2,
                            LeftV1,
                            ParentV,
                            V1,
                            V2
                        )};
                true ->
                    no
            end
    end.

%%%%%%%%%
%%%%%%%%%
%%% Rebalance from Left into Right

-compile({inline, maybe_rebalance_right/4}).
maybe_rebalance_right(Left, ParentK, ParentV, Right) ->
    case Right of
        ?INTERNAL1(RightK1, RightV1, RightSizes, RightC1, RightC2) ->
            rebalance_right_internal1(
                RightK1, RightV1, RightSizes, RightC1, RightC2, ParentK, ParentV, Left
            );
        ?LEAF1(RightK1, RightV1) ->
            rebalance_right_leaf1(RightK1, RightV1, ParentK, ParentV, Left);
        _ ->
            no
    end.

-compile({inline, [rebalance_right_internal1/8]}).
rebalance_right_internal1(RightK1, RightV1, RightSizes, RightC1, RightC2, ParentK, ParentV, Left) ->
    case Left of
        ?INTERNAL4(
            LeftK1,
            LeftK2,
            LeftK3,
            LeftK4,
            LeftValues,
            LeftSizes,
            LeftC1,
            LeftC2,
            LeftC3,
            LeftC4,
            LeftC5
        ) ->
            rebalance_right_internal1_internal4(
                RightK1,
                RightV1,
                RightSizes,
                RightC1,
                RightC2,
                ParentK,
                ParentV,
                LeftK1,
                LeftK2,
                LeftK3,
                LeftK4,
                LeftValues,
                LeftSizes,
                LeftC1,
                LeftC2,
                LeftC3,
                LeftC4,
                LeftC5
            );
        ?INTERNAL3(
            LeftK1, LeftK2, LeftK3, LeftValues, LeftSizes, LeftC1, LeftC2, LeftC3, LeftC4
        ) ->
            rebalance_right_internal1_internal3(
                RightK1,
                RightV1,
                RightSizes,
                RightC1,
                RightC2,
                ParentK,
                ParentV,
                LeftK1,
                LeftK2,
                LeftK3,
                LeftValues,
                LeftSizes,
                LeftC1,
                LeftC2,
                LeftC3,
                LeftC4
            );
        ?INTERNAL2(LeftK1, LeftK2, LeftValues, LeftSizes, LeftC1, LeftC2, LeftC3) ->
            rebalance_right_internal1_internal2(
                RightK1,
                RightV1,
                RightSizes,
                RightC1,
                RightC2,
                ParentK,
                ParentV,
                LeftK1,
                LeftK2,
                LeftValues,
                LeftSizes,
                LeftC1,
                LeftC2,
                LeftC3
            )
    end.

-compile({inline, rebalance_right_internal1_internal4/18}).
rebalance_right_internal1_internal4(
    RightK1,
    RightV1,
    RightSizes,
    RightC1,
    RightC2,
    ParentK,
    ParentV,
    LeftK1,
    LeftK2,
    LeftK3,
    LeftK4,
    LeftValues,
    LeftSizes,
    LeftC1,
    LeftC2,
    LeftC3,
    LeftC4,
    LeftC5
) ->
    {LeftV1, LeftV2, LeftV3, LeftV4} = LeftValues,

    {LeftS1, LeftS2, LeftS3, LeftS4, LeftS5} = internal4_sizes_unpack(LeftSizes),
    [RightS1 | RightS2] = internal1_sizes_unpack(RightSizes),

    UpK = LeftK4,
    UpVal = LeftV4,
    MovedChild = LeftC5,
    MovedSize = LeftS5,

    UpdatedLeft = ?INTERNAL3(
        LeftK1,
        LeftK2,
        LeftK3,
        {LeftV1, LeftV2, LeftV3},
        internal3_sizes_pack(LeftS1, LeftS2, LeftS3, LeftS4),
        LeftC1,
        LeftC2,
        LeftC3,
        LeftC4
    ),

    UpdatedRight =
        ?INTERNAL2(
            ParentK,
            RightK1,
            [ParentV | RightV1],
            internal2_sizes_pack(MovedSize, RightS1, RightS2),
            MovedChild,
            RightC1,
            RightC2
        ),

    {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize}.

-compile({inline, rebalance_right_internal1_internal3/16}).
rebalance_right_internal1_internal3(
    RightK1,
    RightV1,
    RightSizes,
    RightC1,
    RightC2,
    ParentK,
    ParentV,
    LeftK1,
    LeftK2,
    LeftK3,
    LeftValues,
    LeftSizes,
    LeftC1,
    LeftC2,
    LeftC3,
    LeftC4
) ->
    {LeftV1, LeftV2, LeftV3} = LeftValues,

    {LeftS1, LeftS2, LeftS3, LeftS4} = internal3_sizes_unpack(LeftSizes),
    [RightS1 | RightS2] = internal1_sizes_unpack(RightSizes),

    UpK = LeftK3,
    UpVal = LeftV3,
    MovedChild = LeftC4,
    MovedSize = LeftS4,

    UpdatedLeft = ?INTERNAL2(
        LeftK1,
        LeftK2,
        [LeftV1 | LeftV2],
        internal2_sizes_pack(LeftS1, LeftS2, LeftS3),
        LeftC1,
        LeftC2,
        LeftC3
    ),

    UpdatedRight =
        ?INTERNAL2(
            ParentK,
            RightK1,
            [ParentV | RightV1],
            internal2_sizes_pack(MovedSize, RightS1, RightS2),
            MovedChild,
            RightC1,
            RightC2
        ),

    {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize}.

-compile({inline, rebalance_right_internal1_internal2/14}).
rebalance_right_internal1_internal2(
    RightK1,
    RightV1,
    RightSizes,
    RightC1,
    RightC2,
    ParentK,
    ParentV,
    LeftK1,
    LeftK2,
    LeftValues,
    LeftSizes,
    LeftC1,
    LeftC2,
    LeftC3
) ->
    [LeftV1 | LeftV2] = LeftValues,

    {LeftS1, LeftS2, LeftS3} = internal2_sizes_unpack(LeftSizes),
    [RightS1 | RightS2] = internal1_sizes_unpack(RightSizes),

    {merged,
        ?INTERNAL4(
            LeftK1,
            LeftK2,
            ParentK,
            RightK1,
            {LeftV1, LeftV2, ParentV, RightV1},
            internal4_sizes_pack(LeftS1, LeftS2, LeftS3, RightS1, RightS2),
            LeftC1,
            LeftC2,
            LeftC3,
            RightC1,
            RightC2
        )}.

-compile({inline, [rebalance_right_leaf1/5]}).
rebalance_right_leaf1(RightK1, RightV1, ParentK, ParentV, Left) ->
    % Why is MovedSize 0:
    % * We deleted a key in the right node
    % * Then we're gonna move a key from left to right
    %
    % So, right's size doesn't change. Left does - by -1 - which is what
    % the deletion rebalance functions assume (one additional key lost).
    %
    % Therefore we return 0 and everything works as expected.

    case Left of
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            UpK = K4,
            UpVal = V4,
            UpdatedLeft = ?LEAF3(K1, K2, K3, V1, V2, V3),
            UpdatedRight = ?LEAF2(ParentK, RightK1, ParentV, RightV1),
            MovedSize = 0,
            {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize};
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            UpK = K3,
            UpVal = V3,
            UpdatedLeft = ?LEAF2(K1, K2, V1, V2),
            UpdatedRight = ?LEAF2(ParentK, RightK1, ParentV, RightV1),
            MovedSize = 0,
            {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize};
        ?LEAF2(K1, K2, V1, V2) ->
            {merged,
                ?LEAF4(
                    K1,
                    K2,
                    ParentK,
                    RightK1,
                    V1,
                    V2,
                    ParentV,
                    RightV1
                )}
    end.

%%%%%%%%%
%%%%%%%%%
%%% Rebalance from either Left or Right into a Mid child

-compile({inline, maybe_rebalance_mid/7}).
maybe_rebalance_mid(Left, ParentK1, ParentV1, Mid, ParentK2, ParentV2, Right) ->
    case maybe_rebalance_left(Mid, ParentK2, ParentV2, Right, false) of
        no ->
            case maybe_rebalance_right(Left, ParentK1, ParentV1, Mid) of
                no ->
                    no;
                Rebalanced ->
                    {from_left, Rebalanced}
            end;
        Rebalanced ->
            {from_right, Rebalanced}
    end.

%%%%%%%%%
%%%%%%%%%
%%% Lowering of root height

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

% TODO Validate sizes
stats_recur(?INTERNAL4(_, _, _, _, _, Sizes, C1, C2, C3, C4, C5), Depth, ExpectedNrOfKeys, Acc) ->
    {S1, S2, S3, S4, S5} = internal4_sizes_unpack(Sizes),
    ?assertEqual(S1 + S2 + S3 + S4 + S5 + 4, ExpectedNrOfKeys),

    Acc2 = stats_inc_node_count(Acc, internal4),
    Acc3 = stats_recur(C1, Depth + 1, S1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, S2, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, S3, Acc4),
    Acc6 = stats_recur(C4, Depth + 1, S4, Acc5),
    Acc7 = stats_recur(C5, Depth + 1, S5, Acc6),
    Acc7;
stats_recur(?INTERNAL3(_, _, _, _, Sizes, C1, C2, C3, C4), Depth, ExpectedNrOfKeys, Acc) ->
    {S1, S2, S3, S4} = internal3_sizes_unpack(Sizes),
    ?assertEqual(S1 + S2 + S3 + S4 + 3, ExpectedNrOfKeys),

    Acc2 = stats_inc_node_count(Acc, internal3),
    Acc3 = stats_recur(C1, Depth + 1, S1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, S2, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, S3, Acc4),
    Acc6 = stats_recur(C4, Depth + 1, S4, Acc5),
    Acc6;
stats_recur(?INTERNAL2(_, _, _, Sizes, C1, C2, C3), Depth, ExpectedNrOfKeys, Acc) ->
    {S1, S2, S3} = internal2_sizes_unpack(Sizes),
    ?assertEqual(S1 + S2 + S3 + 2, ExpectedNrOfKeys),

    Acc2 = stats_inc_node_count(Acc, internal2),
    Acc3 = stats_recur(C1, Depth + 1, S1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, S2, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, S3, Acc4),
    Acc5;
stats_recur(?INTERNAL1(_, _, Sizes, C1, C2), Depth, ExpectedNrOfKeys, Acc) ->
    [S1 | S2] = internal2_sizes_unpack(Sizes),
    ?assertEqual(S1 + S2 + 1, ExpectedNrOfKeys),

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
    ?assertEqual(ExpectedNrOfKeys, 4),

    Acc2 = stats_inc_node_count(Acc, leaf4),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF3(_, _, _, _, _, _), Depth, ExpectedNrOfKeys, Acc) ->
    ?assertEqual(ExpectedNrOfKeys, 3),

    Acc2 = stats_inc_node_count(Acc, leaf3),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF2(_, _, _, _), Depth, ExpectedNrOfKeys, Acc) ->
    ?assertEqual(ExpectedNrOfKeys, 2),

    Acc2 = stats_inc_node_count(Acc, leaf2),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF1(_, _), Depth, ExpectedNrOfKeys, Acc) ->
    ?assertEqual(ExpectedNrOfKeys, 1),

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
foldl_recur(Fun, Acc, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, C1, C2, C3, C4, C5)) ->
    Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
    Acc3 = Fun(K2, V2, foldl_recur(Fun, Acc2, C2)),
    Acc4 = Fun(K3, V3, foldl_recur(Fun, Acc3, C3)),
    Acc5 = Fun(K4, V4, foldl_recur(Fun, Acc4, C4)),
    foldl_recur(Fun, Acc5, C5);
foldl_recur(Fun, Acc, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, C1, C2, C3, C4)) ->
    Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
    Acc3 = Fun(K2, V2, foldl_recur(Fun, Acc2, C2)),
    Acc4 = Fun(K3, V3, foldl_recur(Fun, Acc3, C3)),
    foldl_recur(Fun, Acc4, C4);
foldl_recur(Fun, Acc, ?INTERNAL2(K1, K2, [V1 | V2], _, C1, C2, C3)) ->
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
foldr_recur(Fun, Acc, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, C1, C2, C3, C4, C5)) ->
    Acc2 = Fun(K4, V4, foldr_recur(Fun, Acc, C5)),
    Acc3 = Fun(K3, V3, foldr_recur(Fun, Acc2, C4)),
    Acc4 = Fun(K2, V2, foldr_recur(Fun, Acc3, C3)),
    Acc5 = Fun(K1, V1, foldr_recur(Fun, Acc4, C2)),
    foldr_recur(Fun, Acc5, C1);
foldr_recur(Fun, Acc, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, C1, C2, C3, C4)) ->
    Acc2 = Fun(K3, V3, foldr_recur(Fun, Acc, C4)),
    Acc3 = Fun(K2, V2, foldr_recur(Fun, Acc2, C3)),
    Acc4 = Fun(K1, V1, foldr_recur(Fun, Acc3, C2)),
    foldr_recur(Fun, Acc4, C1);
foldr_recur(Fun, Acc, ?INTERNAL2(K1, K2, [V1 | V2], _, C1, C2, C3)) ->
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
        ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, C1, C2, C3, C4, C5) ->
            iterator_steps_l_recur(C1, [
                {K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4}, {K4, V4, C5} | Acc
            ]);
        ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, C1, C2, C3, C4) ->
            iterator_steps_l_recur(C1, [{K1, V1, C2}, {K2, V2, C3}, {K3, V3, C4} | Acc]);
        ?INTERNAL2(K1, K2, [V1 | V2], _, C1, C2, C3) ->
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
iterator_steps_l_from_recur(
    Key, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, C1, C2, C3, C4, C5), Acc
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
iterator_steps_l_from_recur(Key, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, C1, C2, C3, C4), Acc) ->
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
iterator_steps_l_from_recur(Key, ?INTERNAL2(K1, K2, [V1 | V2], _, C1, C2, C3), Acc) ->
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
        ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, C1, C2, C3, C4, C5) ->
            iterator_steps_r_recur(C5, [
                {K4, V4, C4}, {K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc
            ]);
        ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, C1, C2, C3, C4) ->
            iterator_steps_r_recur(C4, [{K3, V3, C3}, {K2, V2, C2}, {K1, V1, C1} | Acc]);
        ?INTERNAL2(K1, K2, [V1 | V2], _, C1, C2, C3) ->
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
iterator_steps_r_from_recur(
    Key, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, C1, C2, C3, C4, C5), Acc
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
iterator_steps_r_from_recur(Key, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, C1, C2, C3, C4), Acc) ->
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
iterator_steps_r_from_recur(Key, ?INTERNAL2(K1, K2, [V1 | V2], _, C1, C2, C3), Acc) ->
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
keys_recur(?INTERNAL4(K1, K2, K3, K4, _, _, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [K4 | keys_recur(C5, Acc)],
    Acc3 = [K3 | keys_recur(C4, Acc2)],
    Acc4 = [K2 | keys_recur(C3, Acc3)],
    Acc5 = [K1 | keys_recur(C2, Acc4)],
    keys_recur(C1, Acc5);
keys_recur(?INTERNAL3(K1, K2, K3, _, _, C1, C2, C3, C4), Acc) ->
    Acc2 = [K3 | keys_recur(C4, Acc)],
    Acc3 = [K2 | keys_recur(C3, Acc2)],
    Acc4 = [K1 | keys_recur(C2, Acc3)],
    keys_recur(C1, Acc4);
keys_recur(?INTERNAL2(K1, K2, _, _, C1, C2, C3), Acc) ->
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
values_recur(?INTERNAL4(_, _, _, _, {V1, V2, V3, V4}, _, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [V4 | values_recur(C5, Acc)],
    Acc3 = [V3 | values_recur(C4, Acc2)],
    Acc4 = [V2 | values_recur(C3, Acc3)],
    Acc5 = [V1 | values_recur(C2, Acc4)],
    values_recur(C1, Acc5);
values_recur(?INTERNAL3(_, _, _, {V1, V2, V3}, _, C1, C2, C3, C4), Acc) ->
    Acc2 = [V3 | values_recur(C4, Acc)],
    Acc3 = [V2 | values_recur(C3, Acc2)],
    Acc4 = [V1 | values_recur(C2, Acc3)],
    values_recur(C1, Acc4);
values_recur(?INTERNAL2(_, _, [V1 | V2], _, C1, C2, C3), Acc) ->
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
to_list_recur(?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, _, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [{K4, V4} | to_list_recur(C5, Acc)],
    Acc3 = [{K3, V3} | to_list_recur(C4, Acc2)],
    Acc4 = [{K2, V2} | to_list_recur(C3, Acc3)],
    Acc5 = [{K1, V1} | to_list_recur(C2, Acc4)],
    to_list_recur(C1, Acc5);
to_list_recur(?INTERNAL3(K1, K2, K3, {V1, V2, V3}, _, C1, C2, C3, C4), Acc) ->
    Acc2 = [{K3, V3} | to_list_recur(C4, Acc)],
    Acc3 = [{K2, V2} | to_list_recur(C3, Acc2)],
    Acc4 = [{K1, V1} | to_list_recur(C2, Acc3)],
    to_list_recur(C1, Acc4);
to_list_recur(?INTERNAL2(K1, K2, [V1 | V2], _, C1, C2, C3), Acc) ->
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
largest_recur(?INTERNAL4(_, _, _, _, _, _, _, _, _, _, C5)) ->
    largest_recur(C5);
largest_recur(?INTERNAL3(_, _, _, _, _, _, _, _, C4)) ->
    largest_recur(C4);
largest_recur(?INTERNAL2(_, _, _, _, _, _, C3)) ->
    largest_recur(C3);
largest_recur(?LEAF4(_, _, _, K4, _, _, _, V4)) ->
    {K4, V4};
largest_recur(?LEAF3(_, _, K3, _, _, V3)) ->
    {K3, V3};
largest_recur(?LEAF2(_, K2, _, V2)) ->
    {K2, V2}.

-spec smallest_recur(deep_node(Key, Value)) -> {Key, Value}.
smallest_recur(?INTERNAL4(_, _, _, _, _, _, C1, _, _, _, _)) ->
    smallest_recur(C1);
smallest_recur(?INTERNAL3(_, _, _, _, _, C1, _, _, _)) ->
    smallest_recur(C1);
smallest_recur(?INTERNAL2(_, _, _, _, C1, _, _)) ->
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
larger_recur(Key, ?INTERNAL4(K1, K2, K3, K4, Values, _, C1, C2, C3, C4, C5)) ->
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
larger_recur(Key, ?INTERNAL3(K1, K2, K3, Values, _, C1, C2, C3, C4)) ->
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
larger_recur(Key, ?INTERNAL2(K1, K2, Values, _, C1, C2, C3)) ->
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
smaller_recur(Key, ?INTERNAL4(K1, K2, K3, K4, Values, _, C1, C2, C3, C4, C5)) ->
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
smaller_recur(Key, ?INTERNAL3(K1, K2, K3, Values, _, C1, C2, C3, C4)) ->
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
smaller_recur(Key, ?INTERNAL2(K1, K2, Values, _, C1, C2, C3)) ->
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
map_recur(Fun, ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5)) ->
    ?INTERNAL4(
        K1,
        K2,
        K3,
        K4,
        {Fun(K1, V1), Fun(K2, V2), Fun(K3, V3), Fun(K4, V4)},
        Sizes,
        map_recur(Fun, C1),
        map_recur(Fun, C2),
        map_recur(Fun, C3),
        map_recur(Fun, C4),
        map_recur(Fun, C5)
    );
map_recur(Fun, ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4)) ->
    ?INTERNAL3(
        K1,
        K2,
        K3,
        {Fun(K1, V1), Fun(K2, V2), Fun(K3, V3)},
        Sizes,
        map_recur(Fun, C1),
        map_recur(Fun, C2),
        map_recur(Fun, C3),
        map_recur(Fun, C4)
    );
map_recur(Fun, ?INTERNAL2(K1, K2, [V1 | V2], Sizes, C1, C2, C3)) ->
    ?INTERNAL2(
        K1,
        K2,
        [Fun(K1, V1) | Fun(K2, V2)],
        Sizes,
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
%% Internal Function Definitions: nth/4
%% ------------------------------------------------------------------

nth_recur(N, ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)) ->
    {S1, S2, S3, S4, _} = internal4_sizes_unpack(Sizes),

    Pos3 = S1 + S2 + S3 + 3,

    if
        N < Pos3 ->
            Pos1 = S1 + 1,

            if
                N > Pos1 ->
                    Pos2 = Pos1 + S2 + 1,

                    if
                        N < Pos2 ->
                            nth_recur(N - Pos1, C2);
                        N > Pos2 ->
                            nth_recur(N - Pos2, C3);
                        true ->
                            {K2, element(2, Values)}
                    end;
                N < Pos1 ->
                    nth_recur(N, C1);
                true ->
                    {K1, element(1, Values)}
            end;
        N > Pos3 ->
            Pos4 = Pos3 + S4 + 1,

            if
                N < Pos4 ->
                    nth_recur(N - Pos3, C4);
                N > Pos4 ->
                    nth_recur(N - Pos4, C5);
                true ->
                    {K4, element(4, Values)}
            end;
        true ->
            {K3, element(3, Values)}
    end;
nth_recur(N, ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4)) ->
    {S1, S2, S3, _} = internal3_sizes_unpack(Sizes),

    Pos2 = S1 + S2 + 2,

    if
        N < Pos2 ->
            Pos1 = S1 + 1,

            if
                N < Pos1 ->
                    nth_recur(N, C1);
                N > Pos1 ->
                    nth_recur(N - Pos1, C2);
                true ->
                    {K1, element(1, Values)}
            end;
        N > Pos2 ->
            Pos3 = Pos2 + S3 + 1,

            if
                N < Pos3 ->
                    nth_recur(N - Pos2, C3);
                N > Pos3 ->
                    nth_recur(N - Pos3, C4);
                true ->
                    {K3, element(3, Values)}
            end;
        true ->
            {K2, element(2, Values)}
    end;
nth_recur(N, ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3)) ->
    {S1, S2, _} = internal2_sizes_unpack(Sizes),

    Pos1 = S1 + 1,

    if
        N > Pos1 ->
            Pos2 = Pos1 + S2 + 1,

            if
                N < Pos2 ->
                    nth_recur(N - Pos1, C2);
                N > Pos2 ->
                    nth_recur(N - Pos2, C3);
                true ->
                    {K2, tl(Values)}
            end;
        N < Pos1 ->
            nth_recur(N, C1);
        true ->
            {K1, hd(Values)}
    end;
nth_recur(N, ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    case N of
        1 -> {K1, V1};
        2 -> {K2, V2};
        3 -> {K3, V3};
        4 -> {K4, V4}
    end;
nth_recur(N, ?LEAF3(K1, K2, K3, V1, V2, V3)) ->
    case N of
        1 -> {K1, V1};
        2 -> {K2, V2};
        3 -> {K3, V3}
    end;
nth_recur(N, ?LEAF2(K1, K2, V1, V2)) ->
    case N of
        1 -> {K1, V1};
        2 -> {K2, V2}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Size Packing for INTERNAL4
%% ------------------------------------------------------------------

-compile({inline, internal4_sizes_pack/5}).
internal4_sizes_pack(S1, S2, S3, S4, S5) ->
    {S1, S2, S3, S4, S5}.

-compile({inline, internal4_sizes_unpack/1}).
internal4_sizes_unpack(Sizes) ->
    Sizes.

%%%

-compile({inline, internal4_sizes_update1/2}).
internal4_sizes_update1({S1, S2, S3, S4, S5}, Inc) ->
    {S1 + Inc, S2, S3, S4, S5}.

-compile({inline, internal4_sizes_update2/2}).
internal4_sizes_update2({S1, S2, S3, S4, S5}, Inc) ->
    {S1, S2 + Inc, S3, S4, S5}.

-compile({inline, internal4_sizes_update3/2}).
internal4_sizes_update3({S1, S2, S3, S4, S5}, Inc) ->
    {S1, S2, S3 + Inc, S4, S5}.

-compile({inline, internal4_sizes_update4/2}).
internal4_sizes_update4({S1, S2, S3, S4, S5}, Inc) ->
    {S1, S2, S3, S4 + Inc, S5}.

-compile({inline, internal4_sizes_update5/2}).
internal4_sizes_update5({S1, S2, S3, S4, S5}, Inc) ->
    {S1, S2, S3, S4, S5 + Inc}.

%%%

-compile({inline, internal4_sizes_move_left12/2}).
internal4_sizes_move_left12({S1, S2, S3, S4, S5}, MovedSize) ->
    {
        S1 + MovedSize,
        S2 - MovedSize - 1,
        S3,
        S4,
        S5
    }.

-compile({inline, internal4_sizes_move_left23/2}).
internal4_sizes_move_left23({S1, S2, S3, S4, S5}, MovedSize) ->
    {
        S1,
        S2 + MovedSize,
        S3 - MovedSize - 1,
        S4,
        S5
    }.

-compile({inline, internal4_sizes_move_left34/2}).
internal4_sizes_move_left34({S1, S2, S3, S4, S5}, MovedSize) ->
    {
        S1,
        S2,
        S3 + MovedSize,
        S4 - MovedSize - 1,
        S5
    }.

-compile({inline, internal4_sizes_move_left45/2}).
internal4_sizes_move_left45({S1, S2, S3, S4, S5}, MovedSize) ->
    {
        S1,
        S2,
        S3,
        S4 + MovedSize,
        S5 - MovedSize - 1
    }.

%%%

-compile({inline, internal4_sizes_move_right12/2}).
internal4_sizes_move_right12({S1, S2, S3, S4, S5}, MovedSize) ->
    {
        S1 - MovedSize - 1,
        S2 + MovedSize,
        S3,
        S4,
        S5
    }.

-compile({inline, internal4_sizes_move_right23/2}).
internal4_sizes_move_right23({S1, S2, S3, S4, S5}, MovedSize) ->
    {
        S1,
        S2 - MovedSize - 1,
        S3 + MovedSize,
        S4,
        S5
    }.

-compile({inline, internal4_sizes_move_right34/2}).
internal4_sizes_move_right34({S1, S2, S3, S4, S5}, MovedSize) ->
    {
        S1,
        S2,
        S3 - MovedSize - 1,
        S4 + MovedSize,
        S5
    }.

-compile({inline, internal4_sizes_move_right45/2}).
internal4_sizes_move_right45({S1, S2, S3, S4, S5}, MovedSize) ->
    {
        S1,
        S2,
        S3,
        S4 - MovedSize - 1,
        S5 + MovedSize
    }.

%%%

-compile({inline, internal4_sizes_merge12/1}).
internal4_sizes_merge12({S1, S2, S3, S4, S5}) ->
    internal3_sizes_pack(
        S1 + S2,
        S3,
        S4,
        S5
    ).

-compile({inline, internal4_sizes_merge23/1}).
internal4_sizes_merge23({S1, S2, S3, S4, S5}) ->
    internal3_sizes_pack(
        S1,
        S2 + S3,
        S4,
        S5
    ).

-compile({inline, internal4_sizes_merge34/1}).
internal4_sizes_merge34({S1, S2, S3, S4, S5}) ->
    internal3_sizes_pack(
        S1,
        S2,
        S3 + S4,
        S5
    ).

-compile({inline, internal4_sizes_merge45/1}).
internal4_sizes_merge45({S1, S2, S3, S4, S5}) ->
    internal3_sizes_pack(
        S1,
        S2,
        S3,
        S4 + S5
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Size Packing for INTERNAL3
%% ------------------------------------------------------------------

-compile({inline, internal3_sizes_pack/4}).
internal3_sizes_pack(S1, S2, S3, S4) ->
    {S1, S2, S3, S4}.

-compile({inline, internal3_sizes_unpack/1}).
internal3_sizes_unpack(Sizes) ->
    Sizes.

%%%

-compile({inline, internal3_sizes_split1/3}).
internal3_sizes_split1({_, S2, S3, S4}, LSize, RSize) ->
    internal4_sizes_pack(LSize, RSize, S2, S3, S4).

-compile({inline, internal3_sizes_split2/3}).
internal3_sizes_split2({S1, _, S3, S4}, LSize, RSize) ->
    internal4_sizes_pack(S1, LSize, RSize, S3, S4).

-compile({inline, internal3_sizes_split3/3}).
internal3_sizes_split3({S1, S2, _, S4}, LSize, RSize) ->
    internal4_sizes_pack(S1, S2, LSize, RSize, S4).

-compile({inline, internal3_sizes_split4/3}).
internal3_sizes_split4({S1, S2, S3, _}, LSize, RSize) ->
    internal4_sizes_pack(S1, S2, S3, LSize, RSize).

%%%

-compile({inline, internal3_sizes_update1/2}).
internal3_sizes_update1({S1, S2, S3, S4}, Inc) ->
    {S1 + Inc, S2, S3, S4}.

-compile({inline, internal3_sizes_update2/2}).
internal3_sizes_update2({S1, S2, S3, S4}, Inc) ->
    {S1, S2 + Inc, S3, S4}.

-compile({inline, internal3_sizes_update3/2}).
internal3_sizes_update3({S1, S2, S3, S4}, Inc) ->
    {S1, S2, S3 + Inc, S4}.

-compile({inline, internal3_sizes_update4/2}).
internal3_sizes_update4({S1, S2, S3, S4}, Inc) ->
    {S1, S2, S3, S4 + Inc}.

%%%

-compile({inline, internal3_sizes_move_left12/2}).
internal3_sizes_move_left12({S1, S2, S3, S4}, MovedSize) ->
    {
        S1 + MovedSize,
        S2 - MovedSize - 1,
        S3,
        S4
    }.

-compile({inline, internal3_sizes_move_left23/2}).
internal3_sizes_move_left23({S1, S2, S3, S4}, MovedSize) ->
    {
        S1,
        S2 + MovedSize,
        S3 - MovedSize - 1,
        S4
    }.

-compile({inline, internal3_sizes_move_left34/2}).
internal3_sizes_move_left34({S1, S2, S3, S4}, MovedSize) ->
    {
        S1,
        S2,
        S3 + MovedSize,
        S4 - MovedSize - 1
    }.

%%%

-compile({inline, internal3_sizes_move_right12/2}).
internal3_sizes_move_right12({S1, S2, S3, S4}, MovedSize) ->
    {
        S1 - MovedSize - 1,
        S2 + MovedSize,
        S3,
        S4
    }.

-compile({inline, internal3_sizes_move_right23/2}).
internal3_sizes_move_right23({S1, S2, S3, S4}, MovedSize) ->
    {
        S1,
        S2 - MovedSize - 1,
        S3 + MovedSize,
        S4
    }.

-compile({inline, internal3_sizes_move_right34/2}).
internal3_sizes_move_right34({S1, S2, S3, S4}, MovedSize) ->
    {
        S1,
        S2,
        S3 - MovedSize - 1,
        S4 + MovedSize
    }.

%%%

-compile({inline, internal3_sizes_merge12/1}).
internal3_sizes_merge12({S1, S2, S3, S4}) ->
    internal2_sizes_pack(
        S1 + S2,
        S3,
        S4
    ).

-compile({inline, internal3_sizes_merge23/1}).
internal3_sizes_merge23({S1, S2, S3, S4}) ->
    internal2_sizes_pack(
        S1,
        S2 + S3,
        S4
    ).

-compile({inline, internal3_sizes_merge34/1}).
internal3_sizes_merge34({S1, S2, S3, S4}) ->
    internal2_sizes_pack(
        S1,
        S2,
        S3 + S4
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Size Packing for INTERNAL2
%% ------------------------------------------------------------------

-compile({inline, internal2_sizes_pack/3}).
internal2_sizes_pack(S1, S2, S3) ->
    {S1, S2, S3}.

-compile({inline, internal2_sizes_unpack/1}).
internal2_sizes_unpack(Sizes) ->
    Sizes.

%%%

-compile({inline, internal2_sizes_split1/3}).
internal2_sizes_split1({_, S2, S3}, LSize, RSize) ->
    internal3_sizes_pack(LSize, RSize, S2, S3).

-compile({inline, internal2_sizes_split2/3}).
internal2_sizes_split2({S1, _, S3}, LSize, RSize) ->
    internal3_sizes_pack(S1, LSize, RSize, S3).

-compile({inline, internal2_sizes_split3/3}).
internal2_sizes_split3({S1, S2, _}, LSize, RSize) ->
    internal3_sizes_pack(S1, S2, LSize, RSize).

%%%

-compile({inline, internal2_sizes_update1/2}).
internal2_sizes_update1({S1, S2, S3}, Inc) ->
    {S1 + Inc, S2, S3}.

-compile({inline, internal2_sizes_update2/2}).
internal2_sizes_update2({S1, S2, S3}, Inc) ->
    {S1, S2 + Inc, S3}.

-compile({inline, internal2_sizes_update3/2}).
internal2_sizes_update3({S1, S2, S3}, Inc) ->
    {S1, S2, S3 + Inc}.

%%%

-compile({inline, internal2_sizes_move_left12/2}).
internal2_sizes_move_left12({S1, S2, S3}, MovedSize) ->
    {
        S1 + MovedSize,
        S2 - MovedSize - 1,
        S3
    }.

-compile({inline, internal2_sizes_move_left23/2}).
internal2_sizes_move_left23({S1, S2, S3}, MovedSize) ->
    {
        S1,
        S2 + MovedSize,
        S3 - MovedSize - 1
    }.

%%%

-compile({inline, internal2_sizes_move_right12/2}).
internal2_sizes_move_right12({S1, S2, S3}, MovedSize) ->
    {
        S1 - MovedSize - 1,
        S2 + MovedSize,
        S3
    }.

-compile({inline, internal2_sizes_move_right23/2}).
internal2_sizes_move_right23({S1, S2, S3}, MovedSize) ->
    {
        S1,
        S2 - MovedSize - 1,
        S3 + MovedSize
    }.

%%%

-compile({inline, internal2_sizes_merge12/1}).
internal2_sizes_merge12({S1, S2, S3}) ->
    internal1_sizes_pack(
        S1 + S2,
        S3
    ).

-compile({inline, internal2_sizes_merge23/1}).
internal2_sizes_merge23({S1, S2, S3}) ->
    internal1_sizes_pack(
        S1,
        S2 + S3
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Size Packing for INTERNAL1
%% ------------------------------------------------------------------

-compile({inline, internal1_sizes_pack/2}).
internal1_sizes_pack(S1, S2) ->
    [S1 | S2].

-compile({inline, internal1_sizes_unpack/1}).
internal1_sizes_unpack(Sizes) ->
    Sizes.

%%%

-compile({inline, internal1_sizes_split1/3}).
internal1_sizes_split1([_ | S2], LSize, RSize) ->
    internal2_sizes_pack(LSize, RSize, S2).

-compile({inline, internal1_sizes_split2/3}).
internal1_sizes_split2([S1 | _], LSize, RSize) ->
    internal2_sizes_pack(S1, LSize, RSize).

%%%

-compile({inline, internal1_sizes_update1/2}).
internal1_sizes_update1([S1 | S2], Inc) ->
    [S1 + Inc | S2].

-compile({inline, internal1_sizes_update2/2}).
internal1_sizes_update2([S1 | S2], Inc) ->
    [S1 | S2 + Inc].

%%%

-compile({inline, internal1_sizes_move_left12/2}).
internal1_sizes_move_left12([S1 | S2], MovedSize) ->
    [
        (S1 + MovedSize)
        | (S2 - MovedSize - 1)
    ].

%%%

-compile({inline, internal1_sizes_move_right12/2}).
internal1_sizes_move_right12([S1 | S2], MovedSize) ->
    [
        (S1 - MovedSize - 1)
        | (S2 + MovedSize)
    ].
