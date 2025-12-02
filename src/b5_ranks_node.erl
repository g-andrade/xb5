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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    delete/3,
    foldl/3,
    foldr/3,
    get/2,
    insert/5,
    iterator/2,
    iterator_from/3,
    keys/1,
    larger/2,
    largest/1,
    map/2,
    new/0,
    next/1,
    nth/3,
    smaller/2,
    smallest/1,
    take/3,
    take_largest/2,
    take_nth/3,
    take_smallest/2,
    to_list/1,
    update/4,
    validate/3,
    values/1
]).

%-compile([export_all]).

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

%%

-define(PACKED_MASK(B), ((1 bsl B) - 1)).

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

% % Dialyzer got too smart when it reasoned this, but it is indeed true.
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

% FIXME wrong specs
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
-spec delete(Key, b5_ranks_h2b:t(), t(Key, Value)) ->
    {height_decreased, t(Key, Value)} | t(Key, Value).
delete(Key, H2B, Node) ->
    case H2B of
        [B | NextH2B] ->
            root_delete_internal(B, NextH2B, Key, Node);
        _ ->
            root_delete_leaf(Key, Node)
    end.

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
-spec insert(
    Key,
    insertion_value_eval(),
    insertion_value_wrap(Value),
    b5_ranks_h2b:t(),
    t(Key, Value)
) ->
    {height_increased, b5_ranks_h2b:t(), t(Key, Value)} | t(Key, Value).
-dialyzer({no_underspecs, insert/5}).
insert(Key, ValueEval, ValueWrap, H2B, Root) ->
    case H2B of
        [B | NextH2B] ->
            root_insert_internal(B, NextH2B, Key, ValueEval, ValueWrap, Root);
        _ ->
            root_insert_leaf(Key, ValueEval, ValueWrap, Root)
    end.

nth(N, H2B, Root) ->
    case H2B of
        [B | NextH2B] ->
            root_nth_internal(B, NextH2B, N, Root);
        [] ->
            root_nth_leaf(N, Root)
    end.

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
%cjww-spec take(Key, t(Key, Value)) -> {Value, t(Key, Value)}.
take(Key, H2B, Node) ->
    case H2B of
        [B | NextH2B] ->
            root_take_internal(B, NextH2B, key, Key, Node);
        _ ->
            root_take_leaf(key, Key, Node)
    end.

%% @doc Removes and returns the largest key-value pair from the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
%% Returns `{Key, Value, UpdatedNode}'.
%-spec take_largest(b5_ranks_h2b:t(), t(Key, Value)) ->
%    (
%     {height_reduced, nonempty_improper_list(Key, Value), t(Key, Value)}
%     | nonempty_improper_list(nonempty_improper_list(Key, Value), t(Key, Value))
%    ).
% -dialyzer({no_underspecs, take_largest/1}).
take_largest(H2B, Node) ->
    case H2B of
        [B | NextH2B] ->
            root_take_largest_internal(B, NextH2B, Node);
        _ ->
            root_take_largest_leaf(Node)
    end.

take_nth(N, H2B, Node) ->
    case H2B of
        [B | NextH2B] ->
            root_take_internal(B, NextH2B, nth, N, Node);
        _ ->
            root_take_leaf(nth, N, Node)
    end.

%% @doc Removes and returns the smallest key-value pair from the tree node.
%% Fails with an `empty_tree' exception if the node is empty.
%% Returns `{Key, Value, UpdatedNode}'.
%-spec take_smallest(t(Key, Value)) ->
%    (
%     {height_reduced, nonempty_improper_list(Key, Value), t(Key, Value)}
%     | nonempty_improper_list(nonempty_improper_list(Key, Value), t(Key, Value))
%    ).
% -dialyzer({no_underspecs, take_smallest/1}).
take_smallest(H2B, Node) ->
    case H2B of
        [B | NextH2B] ->
            root_take_smallest_internal(B, NextH2B, Node);
        _ ->
            root_take_smallest_leaf(Node)
    end.

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
-spec validate(non_neg_integer(), b5_ranks_h2b:t(), t(_, _)) ->
    {ok, valid_stats()} | {error, term()}.
validate(ExpectedNrOfKeys, H2B, Root) ->
    #{
        min_height := MinHeight,
        max_height := MaxHeight,
        node_counts := NodeCounts,
        wrong_depth_counts := WrongDepthCounts
    } = Stats = stats(Root, H2B, ExpectedNrOfKeys),

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

% -spec insert_recur(
%     Key,
%     insertion_value_wrap(Value),
%     insertion_value_eval(),
%     deep_node(Key, Value)
% ) -> deep_node_after_insertion(Key, Value) | split_result(Key, Value).

root_insert_internal(B, H2B, Key, ValueEval, ValueWrap, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    insert_internal1(B, H2B, Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2);
root_insert_internal(B, H2B, Key, ValueEval, ValueWrap, Root) ->
    case insert_general_internal(B, H2B, Key, ValueEval, ValueWrap, Root) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            Sizes = internal1_sizes_pack(B, SplitLSize, SplitRSize),
            UpdatedH2B = b5_ranks_h2b:increase_height([B | H2B]),
            {height_increased, UpdatedH2B, ?INTERNAL1(SplitK, SplitV, Sizes, SplitL, SplitR)};
        UpdatedRoot ->
            UpdatedRoot
    end.

root_insert_leaf(Key, ValueEval, ValueWrap, ?LEAF1(K1, V1)) ->
    insert_leaf1(Key, ValueEval, ValueWrap, K1, V1);
root_insert_leaf(Key, ValueEval, ValueWrap, ?LEAF0) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF1(Key, Value);
root_insert_leaf(Key, ValueEval, ValueWrap, Root) ->
    case insert_general_leaf(Key, ValueEval, ValueWrap, Root) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            UpdatedH2B = [B] = b5_ranks_h2b:increase_height([]),
            Sizes = internal1_sizes_pack(B, SplitLSize, SplitRSize),
            {height_increased, UpdatedH2B, ?INTERNAL1(SplitK, SplitV, Sizes, SplitL, SplitR)};
        UpdatedRoot ->
            UpdatedRoot
    end.

insert_recur(H2B, Key, ValueEval, ValueWrap, Node) ->
    case H2B of
        [B | NextH2B] ->
            insert_general_internal(B, NextH2B, Key, ValueEval, ValueWrap, Node);
        %
        _ ->
            insert_general_leaf(Key, ValueEval, ValueWrap, Node)
    end.

insert_general_internal(B, H2B, Key, ValueEval, ValueWrap, Node) ->
    case Node of
        ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
            insert_internal4(
                B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
            );
        %
        ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
            insert_internal3(
                B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
            );
        %
        ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3) ->
            insert_internal2(B, H2B, Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3)
    end.

insert_general_leaf(Key, ValueEval, ValueWrap, Node) ->
    case Node of
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            insert_leaf4(Key, ValueEval, ValueWrap, K1, K2, K3, K4, V1, V2, V3, V4);
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            insert_leaf3(Key, ValueEval, ValueWrap, K1, K2, K3, V1, V2, V3);
        %
        ?LEAF2(K1, K2, V1, V2) ->
            insert_leaf2(Key, ValueEval, ValueWrap, K1, K2, V1, V2)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal4/16]}).
insert_internal4(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    if
        Key > K2 ->
            if
                Key < K4 ->
                    if
                        Key > K3 ->
                            insert_internal4_child4(
                                B,
                                H2B,
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
                                B,
                                H2B,
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
                        B,
                        H2B,
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
        Key < K2 ->
            if
                Key > K1 ->
                    insert_internal4_child2(
                        B,
                        H2B,
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
                Key < K1 ->
                    insert_internal4_child1(
                        B,
                        H2B,
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
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal4_child1/16]}).
insert_internal4_child1(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            [LeftSizes | RightSizes] = internal4_sizes_split1(B, Sizes, SplitLSize, SplitRSize),
            internal_split(
                B,
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
                LeftSizes,
                RightSizes,
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
                internal4_sizes_update1(B, Sizes, +1),
                UpdatedC1,
                C2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child2/16]}).
insert_internal4_child2(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            [LeftSizes | RightSizes] = internal4_sizes_split2(B, Sizes, SplitLSize, SplitRSize),
            internal_split(
                B,
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
                LeftSizes, RightSizes,
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
                internal4_sizes_update2(B, Sizes, +1),
                C1,
                UpdatedC2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child3/16]}).
insert_internal4_child3(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C3) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            [LeftSizes | RightSizes] = internal4_sizes_split3(B, Sizes, SplitLSize, SplitRSize),
            internal_split(
                B,
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
                LeftSizes, RightSizes,
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
                internal4_sizes_update3(B, Sizes, +1),
                C1,
                C2,
                UpdatedC3,
                C4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child4/16]}).
insert_internal4_child4(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C4) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            [LeftSizes | RightSizes] = internal4_sizes_split4(B, Sizes, SplitLSize, SplitRSize),
            internal_split(
                B,
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
                LeftSizes, RightSizes,
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
                internal4_sizes_update4(B, Sizes, +1),
                C1,
                C2,
                C3,
                UpdatedC4,
                C5
            )
    end.

-compile({inline, [insert_internal4_child5/16]}).
insert_internal4_child5(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C5) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3, V4} = Values,
            [LeftSizes | RightSizes] = internal4_sizes_split5(B, Sizes, SplitLSize, SplitRSize),
            internal_split(
                B,
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
                LeftSizes, RightSizes,
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
                internal4_sizes_update5(B, Sizes, +1),
                C1,
                C2,
                C3,
                C4,
                UpdatedC5
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal3/14]}).
insert_internal3(B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    if
        Key > K2 ->
            if
                Key < K3 ->
                    insert_internal3_child3(
                        B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                Key > K3 ->
                    insert_internal3_child4(
                        B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    error_key_exists(Key)
            end;
        Key < K2 ->
            if
                Key > K1 ->
                    insert_internal3_child2(
                        B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                Key < K1 ->
                    insert_internal3_child1(
                        B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    error_key_exists(Key)
            end;
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal3_child1/14]}).
insert_internal3_child1(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                SplitK,
                K1,
                K2,
                K3,
                {SplitV, V1, V2, V3},
                internal3_sizes_split1(B, Sizes, SplitLSize, SplitRSize),
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
                internal3_sizes_update1(B, Sizes, +1),
                UpdatedC1,
                C2,
                C3,
                C4
            )
    end.

-compile({inline, [insert_internal3_child2/14]}).
insert_internal3_child2(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                SplitK,
                K2,
                K3,
                {V1, SplitV, V2, V3},
                internal3_sizes_split2(B, Sizes, SplitLSize, SplitRSize),
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
                internal3_sizes_update2(B, Sizes, +1),
                C1,
                UpdatedC2,
                C3,
                C4
            )
    end.

-compile({inline, [insert_internal3_child3/14]}).
insert_internal3_child3(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C3) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                K2,
                SplitK,
                K3,
                {V1, V2, SplitV, V3},
                internal3_sizes_split3(B, Sizes, SplitLSize, SplitRSize),
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
                internal3_sizes_update3(B, Sizes, +1),
                C1,
                C2,
                UpdatedC3,
                C4
            )
    end.

-compile({inline, [insert_internal3_child4/14]}).
insert_internal3_child4(
    B, H2B, Key, ValueEval, ValueWrap, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C4) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            {V1, V2, V3} = Values,
            ?INTERNAL4(
                K1,
                K2,
                K3,
                SplitK,
                {V1, V2, V3, SplitV},
                internal3_sizes_split4(B, Sizes, SplitLSize, SplitRSize),
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
                internal3_sizes_update4(B, Sizes, +1),
                C1,
                C2,
                C3,
                UpdatedC4
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal2/12]}).
insert_internal2(B, H2B, Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    if
        Key > K1 ->
            if
                Key < K2 ->
                    insert_internal2_child2(
                        B, H2B, Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3
                    );
                Key > K2 ->
                    insert_internal2_child3(
                        B, H2B, Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3
                    );
                true ->
                    error_key_exists(Key)
            end;
        Key < K1 ->
            insert_internal2_child1(
                B, H2B, Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3
            );
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal2_child1/12]}).
insert_internal2_child1(B, H2B, Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                SplitK,
                K1,
                K2,
                {SplitV, V1, V2},
                internal2_sizes_split1(B, Sizes, SplitLSize, SplitRSize),
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
                internal2_sizes_update1(B, Sizes, +1),
                UpdatedC1,
                C2,
                C3
            )
    end.

-compile({inline, [insert_internal2_child2/12]}).
insert_internal2_child2(B, H2B, Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                K1,
                SplitK,
                K2,
                {V1, SplitV, V2},
                internal2_sizes_split2(B, Sizes, SplitLSize, SplitRSize),
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
                internal2_sizes_update2(B, Sizes, +1),
                C1,
                UpdatedC2,
                C3
            )
    end.

-compile({inline, [insert_internal2_child3/12]}).
insert_internal2_child3(B, H2B, Key, ValueEval, ValueWrap, K1, K2, Values, Sizes, C1, C2, C3) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C3) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            [V1 | V2] = Values,
            ?INTERNAL3(
                K1,
                K2,
                SplitK,
                {V1, V2, SplitV},
                internal2_sizes_split3(B, Sizes, SplitLSize, SplitRSize),
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
                internal2_sizes_update3(B, Sizes, +1),
                C1,
                C2,
                UpdatedC3
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal1/10]}).
insert_internal1(B, H2B, Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2) ->
    if
        Key < K1 ->
            insert_internal1_child1(B, H2B, Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2);
        Key > K1 ->
            insert_internal1_child2(B, H2B, Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2);
        true ->
            error_key_exists(Key)
    end.

-compile({inline, [insert_internal1_child1/10]}).
insert_internal1_child1(B, H2B, Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C1) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            ?INTERNAL2(
                SplitK,
                K1,
                [SplitV | V1],
                internal1_sizes_split1(B, Sizes, SplitLSize, SplitRSize),
                SplitL,
                SplitR,
                C2
            );
        UpdatedC1 ->
            ?INTERNAL1(
                K1,
                V1,
                internal1_sizes_update1(B, Sizes, +1),
                UpdatedC1,
                C2
            )
    end.

-compile({inline, [insert_internal1_child2/10]}).
insert_internal1_child2(B, H2B, Key, ValueEval, ValueWrap, K1, V1, Sizes, C1, C2) ->
    case insert_recur(H2B, Key, ValueEval, ValueWrap, C2) of
        ?SPLIT(SplitK, SplitV, SplitLSize, SplitL, SplitRSize, SplitR) ->
            ?INTERNAL2(
                K1,
                SplitK,
                [V1 | SplitV],
                internal1_sizes_split2(B, Sizes, SplitLSize, SplitRSize),
                C1,
                SplitL,
                SplitR
            );
        UpdatedC2 ->
            ?INTERNAL1(
                K1,
                V1,
                internal1_sizes_update2(B, Sizes, +1),
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

-compile({inline, internal_split/19}).
-spec internal_split(
    pos_integer(),
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
    internal2_sizes(),
    internal2_sizes(),
    C,
    C,
    C,
    C,
    C,
    C
) -> internal_split_result(K, V) when C :: deep_node(K, V).
internal_split(
    B,
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
    SizesL,
    SizesR,
    C1,
    C2,
    C3,
    C4,
    C5,
    C6
) ->
    SplitK = K3,
    SplitV = V3,

    SplitLSize = internal2_sizes_sum(B, SizesL) + 2,
    SplitL = ?INTERNAL2(K1, K2, [V1 | V2], SizesL, C1, C2, C3),

    SplitRSize = internal2_sizes_sum(B, SizesR) + 2,
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

root_delete_internal(B, H2B, Key, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    delete_internal1(B, H2B, Key, K1, V1, Sizes, C1, C2);
root_delete_internal(B, H2B, Key, Root) ->
    delete_general_internal(B, H2B, Key, Root).

root_delete_leaf(Key, ?LEAF1(K1, _)) ->
    delete_leaf1(Key, K1);
root_delete_leaf(Key, ?LEAF0) ->
    error_badkey(Key);
root_delete_leaf(Key, Root) ->
    delete_general_leaf(Key, Root).

delete_recur(H2B, Key, Node) ->
    case H2B of
        [B | NextH2B] ->
            delete_general_internal(B, NextH2B, Key, Node);
        %
        _ ->
            delete_general_leaf(Key, Node)
    end.

delete_general_internal(B, H2B, Key, Node) ->
    case Node of
        ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
            delete_internal4(B, H2B, Key, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
        %
        ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
            delete_internal3(B, H2B, Key, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
        %
        ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3) ->
            delete_internal2(B, H2B, Key, K1, K2, Values, Sizes, C1, C2, C3)
    end.

delete_general_leaf(Key, Node) ->
    case Node of
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            delete_leaf4(Key, K1, K2, K3, K4, V1, V2, V3, V4);
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            delete_leaf3(Key, K1, K2, K3, V1, V2, V3);
        %
        ?LEAF2(K1, K2, V1, V2) ->
            delete_leaf2(Key, K1, K2, V1, V2)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL4

-compile({inline, delete_internal4/14}).
delete_internal4(B, H2B, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    if
        K > K2 ->
            if
                K < K4 ->
                    if
                        K > K3 ->
                            delete_internal4_child4(
                                B, H2B, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            );
                        K < K3 ->
                            delete_internal4_child3(
                                B, H2B, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            );
                        true ->
                            delete_internal4_key3(
                                B, H2B, K1, K2, K4, Values, Sizes, C1, C2, C3, C4, C5
                            )
                    end;
                K > K4 ->
                    delete_internal4_child5(
                        B, H2B, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    delete_internal4_key4(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        K < K2 ->
            if
                K > K1 ->
                    delete_internal4_child2(
                        B, H2B, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                K < K1 ->
                    delete_internal4_child1(
                        B, H2B, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                    );
                true ->
                    delete_internal4_key1(B, H2B, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        true ->
            delete_internal4_key2(B, H2B, K1, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
    end.

-compile({inline, [delete_internal4_child1/14]}).
delete_internal4_child1(B, H2B, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC1 = delete_recur(H2B, K, C1),

    delete_internal4_rebalance_child1(
        B,
        H2B,
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

-compile({inline, [delete_internal4_child2/14]}).
delete_internal4_child2(B, H2B, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC2 = delete_recur(H2B, K, C2),

    delete_internal4_rebalance_child2(
        B,
        H2B,
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

-compile({inline, [delete_internal4_child3/14]}).
delete_internal4_child3(B, H2B, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC3 = delete_recur(H2B, K, C3),

    delete_internal4_rebalance_child3(
        B,
        H2B,
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

-compile({inline, [delete_internal4_child4/14]}).
delete_internal4_child4(B, H2B, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC4 = delete_recur(H2B, K, C4),

    delete_internal4_rebalance_child4(
        B,
        H2B,
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

-compile({inline, [delete_internal4_child5/14]}).
delete_internal4_child5(B, H2B, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    UpdatedC5 = delete_recur(H2B, K, C5),

    delete_internal4_rebalance_child5(
        B,
        H2B,
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

-compile({inline, [delete_internal4_key1/12]}).
delete_internal4_key1(B, H2B, K2, K3, K4, {_, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(H2B, C2),

    delete_internal4_rebalance_child2(
        B,
        H2B,
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

-compile({inline, [delete_internal4_key2/12]}).
delete_internal4_key2(B, H2B, K1, K3, K4, {V1, _, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(H2B, C3),

    delete_internal4_rebalance_child3(
        B,
        H2B,
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

-compile({inline, [delete_internal4_key3/12]}).
delete_internal4_key3(B, H2B, K1, K2, K4, {V1, V2, _, V4}, Sizes, C1, C2, C3, C4, C5) ->
    [[ReplacementK | ReplacementV] | UpdatedC4] = take_smallest_recur(H2B, C4),

    delete_internal4_rebalance_child4(
        B,
        H2B,
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

-compile({inline, [delete_internal4_key4/12]}).
delete_internal4_key4(B, H2B, K1, K2, K3, {V1, V2, V3, _}, Sizes, C1, C2, C3, C4, C5) ->
    [[ReplacementK | ReplacementV] | UpdatedC5] = take_smallest_recur(H2B, C5),

    delete_internal4_rebalance_child5(
        B,
        H2B,
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

-compile({inline, [delete_internal4_rebalance_child1/16]}).
delete_internal4_rebalance_child1(
    B, H2B, K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5
) ->
    case maybe_rebalance_left(H2B, C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal4_sizes_update1(B, Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {UpK, UpVal, UpdatedC1, UpdatedC2, MovedSize} ->
            UpdatedSizes = internal4_sizes_update12(B, Sizes, MovedSize, -MovedSize - 1),
            ?INTERNAL4(
                UpK, K2, K3, K4, {UpVal, V2, V3, V4}, UpdatedSizes, UpdatedC1, UpdatedC2, C3, C4, C5
            );
        {merged, MergedC1C2} ->
            UpdatedSizes = internal4_sizes_merge12(B, Sizes),
            ?INTERNAL3(K2, K3, K4, {V2, V3, V4}, UpdatedSizes, MergedC1C2, C3, C4, C5)
    end.

-compile({inline, [delete_internal4_rebalance_child2/16]}).
delete_internal4_rebalance_child2(
    B, H2B, K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5
) ->
    case maybe_rebalance_mid(H2B, C1, K1, V1, C2, K2, V2, C3) of
        no ->
            UpdatedSizes = internal4_sizes_update2(B, Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {from_left, {UpK, UpVal, UpdatedC1, RebalancedC2, MovedSize}} ->
            UpdatedSizes = internal4_sizes_update12(B, Sizes, -MovedSize - 1, MovedSize),
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
            UpdatedSizes = internal4_sizes_update23(B, Sizes, MovedSize, -MovedSize - 1),
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
            UpdatedSizes = internal4_sizes_merge12(B, Sizes),
            ?INTERNAL3(K2, K3, K4, {V2, V3, V4}, UpdatedSizes, MergedC1C2, C3, C4, C5)
    end.

-compile({inline, [delete_internal4_rebalance_child3/16]}).
delete_internal4_rebalance_child3(
    B, H2B, K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5
) ->
    case maybe_rebalance_mid(H2B, C2, K2, V2, C3, K3, V3, C4) of
        no ->
            UpdatedSizes = internal4_sizes_update3(B, Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {from_left, {UpK, UpVal, UpdatedC2, RebalancedC3, MovedSize}} ->
            UpdatedSizes = internal4_sizes_update23(B, Sizes, -MovedSize - 1, MovedSize),
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
            UpdatedSizes = internal4_sizes_update34(B, Sizes, MovedSize, -MovedSize - 1),
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
            UpdatedSizes = internal4_sizes_merge23(B, Sizes),
            ?INTERNAL3(K1, K3, K4, {V1, V3, V4}, UpdatedSizes, C1, MergedC2C3, C4, C5)
    end.

-compile({inline, [delete_internal4_rebalance_child4/16]}).
delete_internal4_rebalance_child4(
    B, H2B, K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5
) ->
    case maybe_rebalance_mid(H2B, C3, K3, V3, C4, K4, V4, C5) of
        no ->
            UpdatedSizes = internal4_sizes_update4(B, Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {from_left, {UpK, UpVal, UpdatedC3, RebalancedC4, MovedSize}} ->
            UpdatedSizes = internal4_sizes_update34(B, Sizes, -MovedSize - 1, MovedSize),
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
            UpdatedSizes = internal4_sizes_update45(B, Sizes, MovedSize, -MovedSize - 1),
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
            UpdatedSizes = internal4_sizes_merge34(B, Sizes),
            ?INTERNAL3(K1, K2, K4, {V1, V2, V4}, UpdatedSizes, C1, C2, MergedC3C4, C5)
    end.

-compile({inline, [delete_internal4_rebalance_child5/16]}).
delete_internal4_rebalance_child5(
    B, H2B, K1, K2, K3, K4, V1, V2, V3, V4, Sizes, C1, C2, C3, C4, C5
) ->
    case maybe_rebalance_right(H2B, C4, K4, V4, C5) of
        no ->
            UpdatedSizes = internal4_sizes_update5(B, Sizes, -1),
            ?INTERNAL4(K1, K2, K3, K4, {V1, V2, V3, V4}, UpdatedSizes, C1, C2, C3, C4, C5);
        {UpK, UpVal, UpdatedC4, RebalancedC5, MovedSize} ->
            UpdatedSizes = internal4_sizes_update45(B, Sizes, -MovedSize - 1, MovedSize),
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
            UpdatedSizes = internal4_sizes_merge45(B, Sizes),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, MergedC4C5)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL3

-compile({inline, delete_internal3/12}).
delete_internal3(B, H2B, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    if
        K > K2 ->
            if
                K > K3 ->
                    delete_internal3_child4(B, H2B, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                K < K3 ->
                    delete_internal3_child3(B, H2B, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                true ->
                    delete_internal3_key3(B, H2B, K1, K2, Values, Sizes, C1, C2, C3, C4)
            end;
        K < K2 ->
            if
                K > K1 ->
                    delete_internal3_child2(B, H2B, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                K < K1 ->
                    delete_internal3_child1(B, H2B, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
                true ->
                    delete_internal3_key1(B, H2B, K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        true ->
            delete_internal3_key2(B, H2B, K1, K3, Values, Sizes, C1, C2, C3, C4)
    end.

-compile({inline, [delete_internal3_child1/12]}).
delete_internal3_child1(B, H2B, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    UpdatedC1 = delete_recur(H2B, K, C1),

    delete_internal3_rebalance_child1(
        B,
        H2B,
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

-compile({inline, [delete_internal3_child2/12]}).
delete_internal3_child2(B, H2B, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    UpdatedC2 = delete_recur(H2B, K, C2),

    delete_internal3_rebalance_child2(
        B,
        H2B,
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

-compile({inline, [delete_internal3_child3/12]}).
delete_internal3_child3(B, H2B, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    UpdatedC3 = delete_recur(H2B, K, C3),

    delete_internal3_rebalance_child3(
        B,
        H2B,
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

-compile({inline, [delete_internal3_child4/12]}).
delete_internal3_child4(B, H2B, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    UpdatedC4 = delete_recur(H2B, K, C4),

    delete_internal3_rebalance_child4(
        B,
        H2B,
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

-compile({inline, [delete_internal3_key1/10]}).
delete_internal3_key1(B, H2B, K2, K3, {_, V2, V3}, Sizes, C1, C2, C3, C4) ->
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(H2B, C2),

    delete_internal3_rebalance_child2(
        B,
        H2B,
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

-compile({inline, [delete_internal3_key2/10]}).
delete_internal3_key2(B, H2B, K1, K3, {V1, _, V3}, Sizes, C1, C2, C3, C4) ->
    [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(H2B, C3),

    delete_internal3_rebalance_child3(
        B,
        H2B,
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

-compile({inline, [delete_internal3_key3/10]}).
delete_internal3_key3(B, H2B, K1, K2, {V1, V2, _}, Sizes, C1, C2, C3, C4) ->
    [[ReplacementK | ReplacementV] | UpdatedC4] = take_smallest_recur(H2B, C4),

    delete_internal3_rebalance_child4(
        B,
        H2B,
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

-compile({inline, [delete_internal3_rebalance_child1/13]}).
delete_internal3_rebalance_child1(B, H2B, K1, K2, K3, V1, V2, V3, Sizes, C1, C2, C3, C4) ->
    case maybe_rebalance_left(H2B, C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal3_sizes_update1(B, Sizes, -1),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, C4);
        {UpK, UpVal, UpdatedC1, UpdatedC2, MovedSize} ->
            UpdatedSizes = internal3_sizes_update12(B, Sizes, MovedSize, -MovedSize - 1),
            ?INTERNAL3(UpK, K2, K3, {UpVal, V2, V3}, UpdatedSizes, UpdatedC1, UpdatedC2, C3, C4);
        {merged, MergedC1C2} ->
            UpdatedSizes = internal3_sizes_merge12(B, Sizes),
            ?INTERNAL2(K2, K3, [V2 | V3], UpdatedSizes, MergedC1C2, C3, C4)
    end.

-compile({inline, [delete_internal3_rebalance_child2/13]}).
delete_internal3_rebalance_child2(B, H2B, K1, K2, K3, V1, V2, V3, Sizes, C1, C2, C3, C4) ->
    case maybe_rebalance_mid(H2B, C1, K1, V1, C2, K2, V2, C3) of
        no ->
            UpdatedSizes = internal3_sizes_update2(B, Sizes, -1),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, C4);
        {from_left, {UpK, UpVal, UpdatedC1, RebalancedC2, MovedSize}} ->
            UpdatedSizes = internal3_sizes_update12(B, Sizes, -MovedSize - 1, MovedSize),
            ?INTERNAL3(UpK, K2, K3, {UpVal, V2, V3}, UpdatedSizes, UpdatedC1, RebalancedC2, C3, C4);
        {from_right, {UpK, UpVal, RebalancedC2, UpdatedC3, MovedSize}} ->
            UpdatedSizes = internal3_sizes_update23(B, Sizes, MovedSize, -MovedSize - 1),
            ?INTERNAL3(K1, UpK, K3, {V1, UpVal, V3}, UpdatedSizes, C1, RebalancedC2, UpdatedC3, C4);
        {from_left, {merged, MergedC1C2}} ->
            UpdatedSizes = internal3_sizes_merge12(B, Sizes),
            ?INTERNAL2(K2, K3, [V2 | V3], UpdatedSizes, MergedC1C2, C3, C4)
    end.

-compile({inline, [delete_internal3_rebalance_child3/13]}).
delete_internal3_rebalance_child3(B, H2B, K1, K2, K3, V1, V2, V3, Sizes, C1, C2, C3, C4) ->
    case maybe_rebalance_mid(H2B, C2, K2, V2, C3, K3, V3, C4) of
        no ->
            UpdatedSizes = internal3_sizes_update3(B, Sizes, -1),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, C4);
        {from_left, {UpK, UpVal, UpdatedC2, RebalancedC3, MovedSize}} ->
            UpdatedSizes = internal3_sizes_update23(B, Sizes, -MovedSize - 1, MovedSize),
            ?INTERNAL3(K1, UpK, K3, {V1, UpVal, V3}, UpdatedSizes, C1, UpdatedC2, RebalancedC3, C4);
        {from_right, {UpK, UpVal, RebalancedC3, UpdatedC4, MovedSize}} ->
            UpdatedSizes = internal3_sizes_update34(B, Sizes, MovedSize, -MovedSize - 1),
            ?INTERNAL3(K1, K2, UpK, {V1, V2, UpVal}, UpdatedSizes, C1, C2, RebalancedC3, UpdatedC4);
        {from_left, {merged, MergedC2C3}} ->
            UpdatedSizes = internal3_sizes_merge23(B, Sizes),
            ?INTERNAL2(K1, K3, [V1 | V3], UpdatedSizes, C1, MergedC2C3, C4)
    end.

-compile({inline, [delete_internal3_rebalance_child4/13]}).
delete_internal3_rebalance_child4(B, H2B, K1, K2, K3, V1, V2, V3, Sizes, C1, C2, C3, C4) ->
    case maybe_rebalance_right(H2B, C3, K3, V3, C4) of
        no ->
            UpdatedSizes = internal3_sizes_update4(B, Sizes, -1),
            ?INTERNAL3(K1, K2, K3, {V1, V2, V3}, UpdatedSizes, C1, C2, C3, C4);
        {UpK, UpVal, UpdatedC3, RebalancedC4, MovedSize} ->
            UpdatedSizes = internal3_sizes_update34(B, Sizes, -MovedSize - 1, MovedSize),
            ?INTERNAL3(K1, K2, UpK, {V1, V2, UpVal}, UpdatedSizes, C1, C2, UpdatedC3, RebalancedC4);
        {merged, MergedC3C4} ->
            UpdatedSizes = internal3_sizes_merge34(B, Sizes),
            ?INTERNAL2(K1, K2, [V1 | V2], UpdatedSizes, C1, C2, MergedC3C4)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL2

-compile({inline, delete_internal2/10}).
delete_internal2(B, H2B, K, K1, K2, Values, Sizes, C1, C2, C3) ->
    if
        K > K1 ->
            if
                K > K2 ->
                    delete_internal2_child3(B, H2B, K, K1, K2, Values, Sizes, C1, C2, C3);
                K < K2 ->
                    delete_internal2_child2(B, H2B, K, K1, K2, Values, Sizes, C1, C2, C3);
                true ->
                    delete_internal2_key2(B, H2B, K1, Values, Sizes, C1, C2, C3)
            end;
        K < K1 ->
            delete_internal2_child1(B, H2B, K, K1, K2, Values, Sizes, C1, C2, C3);
        true ->
            delete_internal2_key1(B, H2B, K2, Values, Sizes, C1, C2, C3)
    end.

-compile({inline, [delete_internal2_child1/10]}).
delete_internal2_child1(B, H2B, K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    UpdatedC1 = delete_recur(H2B, K, C1),

    delete_internal2_rebalance_child1(
        B,
        H2B,
        K1,
        K2,
        V1,
        V2,
        Sizes,
        UpdatedC1,
        C2,
        C3
    ).

-compile({inline, [delete_internal2_child2/10]}).
delete_internal2_child2(B, H2B, K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    UpdatedC2 = delete_recur(H2B, K, C2),

    delete_internal2_rebalance_child2(
        B,
        H2B,
        K1,
        K2,
        V1,
        V2,
        Sizes,
        C1,
        UpdatedC2,
        C3
    ).

-compile({inline, [delete_internal2_child3/10]}).
delete_internal2_child3(B, H2B, K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    UpdatedC3 = delete_recur(H2B, K, C3),

    delete_internal2_rebalance_child3(
        B,
        H2B,
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

-compile({inline, [delete_internal2_key1/8]}).
delete_internal2_key1(B, H2B, K2, [_ | V2], Sizes, C1, C2, C3) ->
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(H2B, C2),

    delete_internal2_rebalance_child2(
        B,
        H2B,
        ReplacementK,
        K2,
        ReplacementV,
        V2,
        Sizes,
        C1,
        UpdatedC2,
        C3
    ).

-compile({inline, [delete_internal2_key2/8]}).
delete_internal2_key2(B, H2B, K1, [V1 | _], Sizes, C1, C2, C3) ->
    [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(H2B, C3),

    delete_internal2_rebalance_child3(
        B,
        H2B,
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

-compile({inline, [delete_internal2_rebalance_child1/10]}).
delete_internal2_rebalance_child1(B, H2B, K1, K2, V1, V2, Sizes, C1, C2, C3) ->
    case maybe_rebalance_left(H2B, C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal2_sizes_update1(B, Sizes, -1),
            ?INTERNAL2(K1, K2, [V1 | V2], UpdatedSizes, C1, C2, C3);
        {UpK, UpVal, UpdatedC1, UpdatedC2, MovedSize} ->
            UpdatedSizes = internal2_sizes_update12(B, Sizes, MovedSize, -MovedSize - 1),
            ?INTERNAL2(UpK, K2, [UpVal | V2], UpdatedSizes, UpdatedC1, UpdatedC2, C3);
        {merged, MergedC1C2} ->
            UpdatedSizes = internal2_sizes_merge12(B, Sizes),
            ?INTERNAL1(K2, V2, UpdatedSizes, MergedC1C2, C3)
    end.

-compile({inline, [delete_internal2_rebalance_child2/10]}).
delete_internal2_rebalance_child2(B, H2B, K1, K2, V1, V2, Sizes, C1, C2, C3) ->
    case maybe_rebalance_mid(H2B, C1, K1, V1, C2, K2, V2, C3) of
        no ->
            UpdatedSizes = internal2_sizes_update2(B, Sizes, -1),
            ?INTERNAL2(K1, K2, [V1 | V2], UpdatedSizes, C1, C2, C3);
        {from_left, {UpK, UpVal, UpdatedC1, RebalancedC2, MovedSize}} ->
            UpdatedSizes = internal2_sizes_update12(B, Sizes, -MovedSize - 1, MovedSize),
            ?INTERNAL2(UpK, K2, [UpVal | V2], UpdatedSizes, UpdatedC1, RebalancedC2, C3);
        {from_right, {UpK, UpVal, RebalancedC2, UpdatedC3, MovedSize}} ->
            UpdatedSizes = internal2_sizes_update23(B, Sizes, MovedSize, -MovedSize - 1),
            ?INTERNAL2(K1, UpK, [V1 | UpVal], UpdatedSizes, C1, RebalancedC2, UpdatedC3);
        {from_left, {merged, MergedC1C2}} ->
            UpdatedSizes = internal2_sizes_merge12(B, Sizes),
            ?INTERNAL1(K2, V2, UpdatedSizes, MergedC1C2, C3)
    end.

-compile({inline, [delete_internal2_rebalance_child3/10]}).
delete_internal2_rebalance_child3(B, H2B, K1, K2, V1, V2, Sizes, C1, C2, C3) ->
    case maybe_rebalance_right(H2B, C2, K2, V2, C3) of
        no ->
            UpdatedSizes = internal2_sizes_update3(B, Sizes, -1),
            ?INTERNAL2(K1, K2, [V1 | V2], UpdatedSizes, C1, C2, C3);
        {UpK, UpVal, UpdatedC2, RebalancedC3, MovedSize} ->
            UpdatedSizes = internal2_sizes_update23(B, Sizes, -MovedSize - 1, MovedSize),
            ?INTERNAL2(K1, UpK, [V1 | UpVal], UpdatedSizes, C1, UpdatedC2, RebalancedC3);
        {merged, MergedC2C3} ->
            UpdatedSizes = internal2_sizes_merge23(B, Sizes),
            ?INTERNAL1(K1, V1, UpdatedSizes, C1, MergedC2C3)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Delete - INTERNAL1

-compile({inline, delete_internal1/8}).
delete_internal1(B, H2B, K, K1, V1, Sizes, C1, C2) ->
    if
        K > K1 ->
            delete_internal1_child2(B, H2B, K, K1, V1, Sizes, C1, C2);
        K < K1 ->
            delete_internal1_child1(B, H2B, K, K1, V1, Sizes, C1, C2);
        true ->
            delete_internal1_key1(B, H2B, Sizes, C1, C2)
    end.

-compile({inline, [delete_internal1_child1/8]}).
delete_internal1_child1(B, H2B, K, K1, V1, Sizes, C1, C2) ->
    UpdatedC1 = delete_recur(H2B, K, C1),

    delete_internal1_rebalance_child1(
        B,
        H2B,
        K1,
        V1,
        Sizes,
        UpdatedC1,
        C2
    ).

-compile({inline, [delete_internal1_child2/8]}).
delete_internal1_child2(B, H2B, K, K1, V1, Sizes, C1, C2) ->
    UpdatedC2 = delete_recur(H2B, K, C2),

    delete_internal1_rebalance_child2(
        B,
        H2B,
        K1,
        V1,
        Sizes,
        C1,
        UpdatedC2
    ).

%%% Delete - INTERNAL1 - key in node

-compile({inline, [delete_internal1_key1/5]}).
delete_internal1_key1(B, H2B, Sizes, C1, C2) ->
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(H2B, C2),

    delete_internal1_rebalance_child2(
        B,
        H2B,
        ReplacementK,
        ReplacementV,
        Sizes,
        C1,
        UpdatedC2
    ).

%%% Delete - INTERNAL1 - rebalance

-compile({inline, [delete_internal1_rebalance_child1/7]}).
delete_internal1_rebalance_child1(B, H2B, K1, V1, Sizes, C1, C2) ->
    case maybe_rebalance_left(H2B, C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal1_sizes_update1(B, Sizes, -1),
            ?INTERNAL1(K1, V1, UpdatedSizes, C1, C2);
        {UpK, UpVal, UpdatedC1, UpdatedC2, MovedSize} ->
            UpdatedSizes = internal1_sizes_update12(B, Sizes, MovedSize, -MovedSize - 1),
            ?INTERNAL1(UpK, UpVal, UpdatedSizes, UpdatedC1, UpdatedC2);
        {merged, MergedC1C2} ->
            % This can only happen on root
            {height_decreased, MergedC1C2}
    end.

-compile({inline, [delete_internal1_rebalance_child2/7]}).
delete_internal1_rebalance_child2(B, H2B, K1, V1, Sizes, C1, C2) ->
    case maybe_rebalance_right(H2B, C1, K1, V1, C2) of
        no ->
            UpdatedSizes = internal1_sizes_update2(B, Sizes, -1),
            ?INTERNAL1(K1, V1, UpdatedSizes, C1, C2);
        {UpK, UpVal, UpdatedC1, RebalancedC2, MovedSize} ->
            UpdatedSizes = internal1_sizes_update12(B, Sizes, -MovedSize - 1, MovedSize),
            ?INTERNAL1(UpK, UpVal, UpdatedSizes, UpdatedC1, RebalancedC2);
        {merged, MergedC1C2} ->
            % This can only happen on root - height is reduced
            {height_decreased, MergedC1C2}
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

root_take_smallest_internal(B, H2B, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    take_smallest_internal1(B, H2B, K1, V1, Sizes, C1, C2);
root_take_smallest_internal(B, H2B, Root) ->
    take_smallest_general_internal(B, H2B, Root).

root_take_smallest_leaf(?LEAF1(K1, V1)) ->
    [[K1 | V1] | ?LEAF0];
root_take_smallest_leaf(?LEAF0) ->
    error_empty_tree();
root_take_smallest_leaf(Root) ->
    take_smallest_general_leaf(Root).

take_smallest_recur(H2B, Node) ->
    case H2B of
        [B | NextH2B] ->
            take_smallest_general_internal(B, NextH2B, Node);
        %
        _ ->
            take_smallest_general_leaf(Node)
    end.

take_smallest_general_internal(B, H2B, Node) ->
    case Node of
        ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
            take_smallest_internal4(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
        %
        ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
            take_smallest_internal3(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
        %
        ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3) ->
            take_smallest_internal2(B, H2B, K1, K2, Values, Sizes, C1, C2, C3)
    end.

take_smallest_general_leaf(Node) ->
    case Node of
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            [[K1 | V1] | ?LEAF3(K2, K3, K4, V2, V3, V4)];
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            [[K1 | V1] | ?LEAF2(K2, K3, V2, V3)];
        %
        ?LEAF2(K1, K2, V1, V2) ->
            [[K1 | V1] | ?LEAF1(K2, V2)]
    end.

%%%%%%%%

-compile({inline, take_smallest_internal4/13}).
take_smallest_internal4(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC1] = take_smallest_recur(H2B, C1),
    {V1, V2, V3, V4} = Values,

    [
        TakenPair
        | delete_internal4_rebalance_child1(
            B,
            H2B,
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
        )
    ].

-compile({inline, take_smallest_internal3/11}).
take_smallest_internal3(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC1] = take_smallest_recur(H2B, C1),
    {V1, V2, V3} = Values,

    [
        TakenPair
        | delete_internal3_rebalance_child1(
            B,
            H2B,
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
        )
    ].

-compile({inline, take_smallest_internal2/9}).
take_smallest_internal2(B, H2B, K1, K2, Values, Sizes, C1, C2, C3) ->
    [TakenPair | UpdatedC1] = take_smallest_recur(H2B, C1),
    [V1 | V2] = Values,

    [
        TakenPair
        | delete_internal2_rebalance_child1(
            B,
            H2B,
            K1,
            K2,
            V1,
            V2,
            Sizes,
            UpdatedC1,
            C2,
            C3
        )
    ].

-compile({inline, take_smallest_internal1/7}).
take_smallest_internal1(B, H2B, K1, V1, Sizes, C1, C2) ->
    [TakenPair | UpdatedC1] = take_smallest_recur(H2B, C1),

    case
        delete_internal1_rebalance_child1(
            B,
            H2B,
            K1,
            V1,
            Sizes,
            UpdatedC1,
            C2
        )
    of
        {height_decreased, UpdatedRoot} ->
            {height_decreased, TakenPair, UpdatedRoot};
        UpdatedRoot ->
            [TakenPair | UpdatedRoot]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - Largest
%% ------------------------------------------------------------------

root_take_largest_internal(B, H2B, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    take_largest_internal1(B, H2B, K1, V1, Sizes, C1, C2);
root_take_largest_internal(B, H2B, Root) ->
    take_largest_general_internal(B, H2B, Root).

root_take_largest_leaf(?LEAF1(K1, V1)) ->
    [[K1 | V1] | ?LEAF0];
root_take_largest_leaf(?LEAF0) ->
    error_empty_tree();
root_take_largest_leaf(Root) ->
    take_largest_general_leaf(Root).

take_largest_recur(H2B, Node) ->
    case H2B of
        [B | NextH2B] ->
            take_largest_general_internal(B, NextH2B, Node);
        %
        _ ->
            take_largest_general_leaf(Node)
    end.

take_largest_general_internal(B, H2B, Node) ->
    case Node of
        ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
            take_largest_internal4(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
        %
        ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
            take_largest_internal3(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
        %
        ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3) ->
            take_largest_internal2(B, H2B, K1, K2, Values, Sizes, C1, C2, C3)
    end.

take_largest_general_leaf(Node) ->
    case Node of
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            [[K4 | V4] | ?LEAF3(K1, K2, K3, V1, V2, V3)];
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            [[K3 | V3] | ?LEAF2(K1, K2, V1, V2)];
        %
        ?LEAF2(K1, K2, V1, V2) ->
            [[K2 | V2] | ?LEAF1(K1, V1)]
    end.

%%%%%%%%

-compile({inline, take_largest_internal4/13}).
take_largest_internal4(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC5] = take_largest_recur(H2B, C5),
    {V1, V2, V3, V4} = Values,

    [
        TakenPair
        | delete_internal4_rebalance_child5(
            B,
            H2B,
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
        )
    ].

-compile({inline, take_largest_internal3/11}).
take_largest_internal3(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC4] = take_largest_recur(H2B, C4),
    {V1, V2, V3} = Values,

    [
        TakenPair
        | delete_internal3_rebalance_child4(
            B,
            H2B,
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
        )
    ].

-compile({inline, take_largest_internal2/9}).
take_largest_internal2(B, H2B, K1, K2, Values, Sizes, C1, C2, C3) ->
    [TakenPair | UpdatedC3] = take_largest_recur(H2B, C3),
    [V1 | V2] = Values,

    [
        TakenPair
        | delete_internal2_rebalance_child3(
            B,
            H2B,
            K1,
            K2,
            V1,
            V2,
            Sizes,
            C1,
            C2,
            UpdatedC3
        )
    ].

-compile({inline, take_largest_internal1/7}).
take_largest_internal1(B, H2B, K1, V1, Sizes, C1, C2) ->
    [TakenPair | UpdatedC2] = take_largest_recur(H2B, C2),

    case
        delete_internal1_rebalance_child2(
            B,
            H2B,
            K1,
            V1,
            Sizes,
            C1,
            UpdatedC2
        )
    of
        {height_decreased, UpdatedRoot} ->
            {height_decreased, TakenPair, UpdatedRoot};
        UpdatedRoot ->
            [TakenPair | UpdatedRoot]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Taking - Key
%% ------------------------------------------------------------------

root_take_internal(B, H2B, Type, Key, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    take_internal1(B, H2B, Type, Key, K1, V1, Sizes, C1, C2);
root_take_internal(B, H2B, Type, Key, Root) ->
    take_general_internal(B, H2B, Type, Key, Root).

root_take_leaf(Type, Key, ?LEAF1(K1, V1)) ->
    take_leaf1(Type, Key, K1, V1);
root_take_leaf(key, Key, ?LEAF0) ->
    error_badkey(Key);
root_take_leaf(Type, Key, Root) ->
    take_general_leaf(Type, Key, Root).

take_recur(H2B, Type, Key, Node) ->
    case H2B of
        [B | NextH2B] ->
            take_general_internal(B, NextH2B, Type, Key, Node);
        %
        _ ->
            take_general_leaf(Type, Key, Node)
    end.

take_general_internal(B, H2B, Type, Key, Node) ->
    case Node of
        ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
            take_internal4(B, H2B, Type, Key, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
        %
        ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
            take_internal3(B, H2B, Type, Key, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
        %
        ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3) ->
            take_internal2(B, H2B, Type, Key, K1, K2, Values, Sizes, C1, C2, C3)
    end.

take_general_leaf(Type, Key, Node) ->
    case Node of
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            take_leaf4(Type, Key, K1, K2, K3, K4, V1, V2, V3, V4);
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            take_leaf3(Type, Key, K1, K2, K3, V1, V2, V3);
        %
        ?LEAF2(K1, K2, V1, V2) ->
            take_leaf2(Type, Key, K1, K2, V1, V2)
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL4

-compile({inline, take_internal4/15}).
take_internal4(B, H2B, key, K, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    if
        K > K2 ->
            if
                K < K4 ->
                    if
                        K > K3 ->
                            take_internal4_child4(
                                B,
                                H2B,
                                key,
                                K,
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
                        K < K3 ->
                            take_internal4_child3(
                                B,
                                H2B,
                                key,
                                K,
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
                            take_internal4_key3(
                                B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            )
                    end;
                K > K4 ->
                    take_internal4_child5(
                        B,
                        H2B,
                        key,
                        K,
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
                    take_internal4_key4(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        K < K2 ->
            if
                K > K1 ->
                    take_internal4_child2(
                        B,
                        H2B,
                        key,
                        K,
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
                K < K1 ->
                    take_internal4_child1(
                        B,
                        H2B,
                        key,
                        K,
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
                    take_internal4_key1(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        true ->
            take_internal4_key2(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
    end;
take_internal4(B, H2B, nth, N, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    {S1, S2, S3, S4, _} = internal4_sizes_unpack(B, Sizes),

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
                                B,
                                H2B,
                                nth,
                                N - Pos1,
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
                        N > Pos2 ->
                            take_internal4_child3(
                                B,
                                H2B,
                                nth,
                                N - Pos2,
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
                            take_internal4_key2(
                                B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5
                            )
                    end;
                N < Pos1 ->
                    take_internal4_child1(
                        B,
                        H2B,
                        nth,
                        N,
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
                    take_internal4_key3(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        N > Pos3 ->
            Pos4 = Pos3 + S4 + 1,

            if
                N < Pos4 ->
                    take_internal4_child4(
                        B,
                        H2B,
                        nth,
                        N - Pos3,
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
                N > Pos4 ->
                    take_internal4_child5(
                        B,
                        H2B,
                        nth,
                        N - Pos4,
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
                    take_internal4_key4(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
            end;
        true ->
            take_internal4_key3(B, H2B, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5)
    end.

%%

-compile({inline, [take_internal4_child1/15]}).
take_internal4_child1(B, H2B, Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC1] = take_recur(H2B, Type, K, C1),

    [
        TakenPair
        | delete_internal4_rebalance_child1(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal4_child2/15]}).
take_internal4_child2(B, H2B, Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC2] = take_recur(H2B, Type, K, C2),

    [
        TakenPair
        | delete_internal4_rebalance_child2(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal4_child3/15]}).
take_internal4_child3(B, H2B, Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC3] = take_recur(H2B, Type, K, C3),

    [
        TakenPair
        | delete_internal4_rebalance_child3(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal4_child4/15]}).
take_internal4_child4(B, H2B, Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC4] = take_recur(H2B, Type, K, C4),

    [
        TakenPair
        | delete_internal4_rebalance_child4(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal4_child5/15]}).
take_internal4_child5(B, H2B, Type, K, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    [TakenPair | UpdatedC5] = take_recur(H2B, Type, K, C5),

    [
        TakenPair
        | delete_internal4_rebalance_child5(
            B,
            H2B,
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
        )
    ].

%%% Take - INTERNAL4 - keys in node

-compile({inline, [take_internal4_key1/13]}).
take_internal4_key1(B, H2B, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    TakenPair = [K1 | V1],
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(H2B, C2),

    [
        TakenPair
        | delete_internal4_rebalance_child2(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal4_key2/13]}).
take_internal4_key2(B, H2B, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    TakenPair = [K2 | V2],
    [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(H2B, C3),

    [
        TakenPair
        | delete_internal4_rebalance_child3(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal4_key3/13]}).
take_internal4_key3(B, H2B, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    TakenPair = [K3 | V3],
    [[ReplacementK | ReplacementV] | UpdatedC4] = take_smallest_recur(H2B, C4),

    [
        TakenPair
        | delete_internal4_rebalance_child4(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal4_key4/13]}).
take_internal4_key4(B, H2B, K1, K2, K3, K4, {V1, V2, V3, V4}, Sizes, C1, C2, C3, C4, C5) ->
    TakenPair = [K4 | V4],
    [[ReplacementK | ReplacementV] | UpdatedC5] = take_smallest_recur(H2B, C5),

    [
        TakenPair
        | delete_internal4_rebalance_child5(
            B,
            H2B,
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
        )
    ].

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL3

-compile({inline, take_internal3/13}).
take_internal3(B, H2B, key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    if
        K > K2 ->
            if
                K > K3 ->
                    take_internal3_child4(
                        B, H2B, key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                K < K3 ->
                    take_internal3_child3(
                        B, H2B, key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    take_internal3_key3(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        K < K2 ->
            if
                K > K1 ->
                    take_internal3_child2(
                        B, H2B, key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                K < K1 ->
                    take_internal3_child1(
                        B, H2B, key, K, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    take_internal3_key1(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        true ->
            take_internal3_key2(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
    end;
take_internal3(B, H2B, nth, N, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    {S1, S2, S3, _} = internal3_sizes_unpack(B, Sizes),

    Pos2 = S1 + S2 + 2,

    if
        N < Pos2 ->
            Pos1 = S1 + 1,

            if
                N < Pos1 ->
                    take_internal3_child1(
                        B, H2B, nth, N, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                N > Pos1 ->
                    take_internal3_child2(
                        B, H2B, nth, N - Pos1, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    take_internal3_key1(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        N > Pos2 ->
            Pos3 = Pos2 + S3 + 1,

            if
                N < Pos3 ->
                    take_internal3_child3(
                        B, H2B, nth, N - Pos2, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                N > Pos3 ->
                    take_internal3_child4(
                        B, H2B, nth, N - Pos3, K1, K2, K3, Values, Sizes, C1, C2, C3, C4
                    );
                true ->
                    take_internal3_key3(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
            end;
        true ->
            take_internal3_key2(B, H2B, K1, K2, K3, Values, Sizes, C1, C2, C3, C4)
    end.

%%

-compile({inline, [take_internal3_child1/13]}).
take_internal3_child1(B, H2B, Type, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC1] = take_recur(H2B, Type, K, C1),

    [
        TakenPair
        | delete_internal3_rebalance_child1(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal3_child2/13]}).
take_internal3_child2(B, H2B, Type, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC2] = take_recur(H2B, Type, K, C2),

    [
        TakenPair
        | delete_internal3_rebalance_child2(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal3_child3/13]}).
take_internal3_child3(B, H2B, Type, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC3] = take_recur(H2B, Type, K, C3),

    [
        TakenPair
        | delete_internal3_rebalance_child3(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal3_child4/13]}).
take_internal3_child4(B, H2B, Type, K, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    [TakenPair | UpdatedC4] = take_recur(H2B, Type, K, C4),

    [
        TakenPair
        | delete_internal3_rebalance_child4(
            B,
            H2B,
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
        )
    ].

%%% Take - INTERNAL3 - keys in node

-compile({inline, [take_internal3_key1/11]}).
take_internal3_key1(B, H2B, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    TakenPair = [K1 | V1],
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(H2B, C2),

    [
        TakenPair
        | delete_internal3_rebalance_child2(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal3_key2/11]}).
take_internal3_key2(B, H2B, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    TakenPair = [K2 | V2],
    [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(H2B, C3),

    [
        TakenPair
        | delete_internal3_rebalance_child3(
            B,
            H2B,
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
        )
    ].

-compile({inline, [take_internal3_key3/11]}).
take_internal3_key3(B, H2B, K1, K2, K3, {V1, V2, V3}, Sizes, C1, C2, C3, C4) ->
    TakenPair = [K3 | V3],
    [[ReplacementK | ReplacementV] | UpdatedC4] = take_smallest_recur(H2B, C4),

    [
        TakenPair
        | delete_internal3_rebalance_child4(
            B,
            H2B,
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
        )
    ].

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL2

-compile({inline, take_internal2/11}).
take_internal2(B, H2B, key, K, K1, K2, Values, Sizes, C1, C2, C3) ->
    if
        K > K1 ->
            if
                K > K2 ->
                    take_internal2_child3(B, H2B, key, K, K1, K2, Values, Sizes, C1, C2, C3);
                K < K2 ->
                    take_internal2_child2(B, H2B, key, K, K1, K2, Values, Sizes, C1, C2, C3);
                true ->
                    take_internal2_key2(B, H2B, K1, K2, Values, Sizes, C1, C2, C3)
            end;
        K < K1 ->
            take_internal2_child1(B, H2B, key, K, K1, K2, Values, Sizes, C1, C2, C3);
        true ->
            take_internal2_key1(B, H2B, K1, K2, Values, Sizes, C1, C2, C3)
    end;
take_internal2(B, H2B, nth, N, K1, K2, Values, Sizes, C1, C2, C3) ->
    {S1, S2, _} = internal2_sizes_unpack(B, Sizes),

    Pos1 = S1 + 1,

    if
        N > Pos1 ->
            Pos2 = Pos1 + S2 + 1,

            if
                N < Pos2 ->
                    take_internal2_child2(B, H2B, nth, N - Pos1, K1, K2, Values, Sizes, C1, C2, C3);
                N > Pos2 ->
                    take_internal2_child3(B, H2B, nth, N - Pos2, K1, K2, Values, Sizes, C1, C2, C3);
                true ->
                    take_internal2_key2(B, H2B, K1, K2, Values, Sizes, C1, C2, C3)
            end;
        N < Pos1 ->
            take_internal2_child1(B, H2B, nth, N, K1, K2, Values, Sizes, C1, C2, C3);
        true ->
            take_internal2_key1(B, H2B, K1, K2, Values, Sizes, C1, C2, C3)
    end.

-compile({inline, [take_internal2_child1/11]}).
take_internal2_child1(B, H2B, Type, K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    [TakenPair | UpdatedC1] = take_recur(H2B, Type, K, C1),

    [
        TakenPair
        | delete_internal2_rebalance_child1(
            B,
            H2B,
            K1,
            K2,
            V1,
            V2,
            Sizes,
            UpdatedC1,
            C2,
            C3
        )
    ].

-compile({inline, [take_internal2_child2/11]}).
take_internal2_child2(B, H2B, Type, K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    [TakenPair | UpdatedC2] = take_recur(H2B, Type, K, C2),

    [
        TakenPair
        | delete_internal2_rebalance_child2(
            B,
            H2B,
            K1,
            K2,
            V1,
            V2,
            Sizes,
            C1,
            UpdatedC2,
            C3
        )
    ].

-compile({inline, [take_internal2_child3/11]}).
take_internal2_child3(B, H2B, Type, K, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    [TakenPair | UpdatedC3] = take_recur(H2B, Type, K, C3),

    [
        TakenPair
        | delete_internal2_rebalance_child3(
            B,
            H2B,
            K1,
            K2,
            V1,
            V2,
            Sizes,
            C1,
            C2,
            UpdatedC3
        )
    ].

%%% Take - INTERNAL2 - keys in node

-compile({inline, [take_internal2_key1/9]}).
take_internal2_key1(B, H2B, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    TakenPair = [K1 | V1],
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(H2B, C2),

    [
        TakenPair
        | delete_internal2_rebalance_child2(
            B,
            H2B,
            ReplacementK,
            K2,
            ReplacementV,
            V2,
            Sizes,
            C1,
            UpdatedC2,
            C3
        )
    ].

-compile({inline, [take_internal2_key2/9]}).
take_internal2_key2(B, H2B, K1, K2, [V1 | V2], Sizes, C1, C2, C3) ->
    TakenPair = [K2 | V2],
    [[ReplacementK | ReplacementV] | UpdatedC3] = take_smallest_recur(H2B, C3),

    [
        TakenPair
        | delete_internal2_rebalance_child3(
            B,
            H2B,
            K1,
            ReplacementK,
            V1,
            ReplacementV,
            Sizes,
            C1,
            C2,
            UpdatedC3
        )
    ].

%%%%%%%%%
%%%%%%%%%
%%% Take - INTERNAL1

-compile({inline, take_internal1/9}).
take_internal1(B, H2B, key, K, K1, V1, Sizes, C1, C2) ->
    if
        K > K1 ->
            take_internal1_child2(B, H2B, key, K, K1, V1, Sizes, C1, C2);
        K < K1 ->
            take_internal1_child1(B, H2B, key, K, K1, V1, Sizes, C1, C2);
        true ->
            take_internal1_key1(B, H2B, K1, V1, Sizes, C1, C2)
    end;
take_internal1(B, H2B, nth, N, K1, V1, Sizes, C1, C2) ->
    [S1 | _] = internal1_sizes_unpack(B, Sizes),

    Pos1 = S1 + 1,

    if
        N < Pos1 ->
            take_internal1_child1(B, H2B, nth, N, K1, V1, Sizes, C1, C2);
        N > Pos1 ->
            take_internal1_child2(B, H2B, nth, N - Pos1, K1, V1, Sizes, C1, C2);
        true ->
            take_internal1_key1(B, H2B, K1, V1, Sizes, C1, C2)
    end.

-compile({inline, [take_internal1_child1/9]}).
take_internal1_child1(B, H2B, Type, K, K1, V1, Sizes, C1, C2) ->
    [TakenPair | UpdatedC1] = take_recur(H2B, Type, K, C1),

    case
        delete_internal1_rebalance_child1(
            B,
            H2B,
            K1,
            V1,
            Sizes,
            UpdatedC1,
            C2
        )
    of
        {height_decreased, UpdatedRoot} ->
            {height_decreased, TakenPair, UpdatedRoot};
        UpdatedRoot ->
            [TakenPair | UpdatedRoot]
    end.

-compile({inline, [take_internal1_child2/9]}).
take_internal1_child2(B, H2B, Type, K, K1, V1, Sizes, C1, C2) ->
    [TakenPair | UpdatedC2] = take_recur(H2B, Type, K, C2),

    case
        delete_internal1_rebalance_child2(
            B,
            H2B,
            K1,
            V1,
            Sizes,
            C1,
            UpdatedC2
        )
    of
        {height_decreased, UpdatedRoot} ->
            {height_decreased, TakenPair, UpdatedRoot};
        UpdatedRoot ->
            [TakenPair | UpdatedRoot]
    end.

%%% Take - INTERNAL1 - key in node

-compile({inline, [take_internal1_key1/7]}).
take_internal1_key1(B, H2B, K1, V1, Sizes, C1, C2) ->
    TakenPair = [K1 | V1],
    [[ReplacementK | ReplacementV] | UpdatedC2] = take_smallest_recur(H2B, C2),

    case
        delete_internal1_rebalance_child2(
            B,
            H2B,
            ReplacementK,
            ReplacementV,
            Sizes,
            C1,
            UpdatedC2
        )
    of
        {height_decreased, UpdatedRoot} ->
            {height_decreased, TakenPair, UpdatedRoot};
        UpdatedRoot ->
            [TakenPair | UpdatedRoot]
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF4

-compile({inline, take_leaf4/10}).
take_leaf4(key, K, K1, K2, K3, K4, V1, V2, V3, V4) ->
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
    end;
take_leaf4(nth, N, K1, K2, K3, K4, V1, V2, V3, V4) ->
    case N of
        1 -> [[K1 | V1] | ?LEAF3(K2, K3, K4, V2, V3, V4)];
        2 -> [[K2 | V2] | ?LEAF3(K1, K3, K4, V1, V3, V4)];
        3 -> [[K3 | V3] | ?LEAF3(K1, K2, K4, V1, V2, V4)];
        4 -> [[K4 | V4] | ?LEAF3(K1, K2, K3, V1, V2, V3)]
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
    end;
take_leaf3(nth, N, K1, K2, K3, V1, V2, V3) ->
    case N of
        1 -> [[K1 | V1] | ?LEAF2(K2, K3, V2, V3)];
        2 -> [[K2 | V2] | ?LEAF2(K1, K3, V1, V3)];
        3 -> [[K3 | V3] | ?LEAF2(K1, K2, V1, V2)]
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF2

-compile({inline, take_leaf2/6}).
take_leaf2(key, K, K1, K2, V1, V2) ->
    if
        K == K1 ->
            [[K1 | V1] | ?LEAF1(K2, V2)];
        K == K2 ->
            [[K2 | V2] | ?LEAF1(K1, V1)];
        true ->
            error_badkey(K)
    end;
take_leaf2(nth, N, K1, K2, V1, V2) ->
    case N of
        1 ->
            [[K1 | V1] | ?LEAF1(K2, V2)];
        2 ->
            [[K2 | V2] | ?LEAF1(K1, V1)]
    end.

%%%%%%%%%
%%%%%%%%%
%%% Take - LEAF1

-compile({inline, take_leaf1/4}).
take_leaf1(key, K, K1, V1) ->
    if
        K == K1 ->
            TakenPair = [K1 | V1],
            [TakenPair | ?LEAF0];
        true ->
            error_badkey(K)
    end;
take_leaf1(nth, 1, K1, V1) ->
    TakenPair = [K1 | V1],
    [TakenPair | ?LEAF0].

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Rebalance
%% ------------------------------------------------------------------

%%%%%%%%%
%%%%%%%%%
%%% Rebalance from Right into Left

-compile({inline, maybe_rebalance_left/5}).
maybe_rebalance_left(H2B, Left, ParentK, ParentV, Right) ->
    maybe_rebalance_left(H2B, Left, ParentK, ParentV, Right, _Merge = true).

-compile({inline, maybe_rebalance_left/6}).
maybe_rebalance_left([B | _], Left, ParentK, ParentV, Right, Merge) ->
    maybe_rebalance_left_internal(B, Left, ParentK, ParentV, Right, Merge);
maybe_rebalance_left(_, Left, ParentK, ParentV, Right, Merge) ->
    maybe_rebalance_left_leaf(Left, ParentK, ParentV, Right, Merge).

-compile({inline, maybe_rebalance_left_internal/6}).
maybe_rebalance_left_internal(B, Left, ParentK, ParentV, Right, Merge) ->
    case Left of
        ?INTERNAL1(K1, V1, Sizes, C1, C2) ->
            rebalance_left_internal1(B, K1, V1, Sizes, C1, C2, ParentK, ParentV, Right, Merge);
        _ ->
            no
    end.

-compile({inline, [rebalance_left_internal1/10]}).
rebalance_left_internal1(
    B, LeftK1, LeftV1, LeftSizes, LeftC1, LeftC2, ParentK, ParentV, Right, Merge
) ->
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
                B,
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
                B,
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
                B,
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

-compile({inline, rebalance_left_internal1_internal4/19}).
rebalance_left_internal1_internal4(
    B,
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

    {NewLeftSizes, NewRightSizes, MovedSize} = internal1_sizes_rebalance_left_with_internal4(B, LeftSizes, RightSizes),

    UpK = RightK1,
    UpVal = RightV1,
    MovedChild = RightC1,

    UpdatedLeft = ?INTERNAL2(
        LeftK1,
        ParentK,
        [LeftV1 | ParentV],
        % internal2_sizes_pack(B, LeftS1, LeftS2, MovedSize),
        NewLeftSizes,
        LeftC1,
        LeftC2,
        MovedChild
    ),

    UpdatedRight = ?INTERNAL3(
        RightK2,
        RightK3,
        RightK4,
        {RightV2, RightV3, RightV4},
        % internal3_sizes_pack(B, RightS2, RightS3, RightS4, RightS5),
        NewRightSizes,
        RightC2,
        RightC3,
        RightC4,
        RightC5
    ),

    {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize}.

-compile({inline, rebalance_left_internal1_internal3/17}).
rebalance_left_internal1_internal3(
    B,
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

%    [LeftS1 | LeftS2] = internal1_sizes_unpack(B, LeftSizes),
%    {RightS1, RightS2, RightS3, RightS4} = internal3_sizes_unpack(B, RightSizes),
    {NewLeftSizes, NewRightSizes, MovedSize} = internal1_sizes_rebalance_left_with_internal3(B, LeftSizes, RightSizes),

    UpK = RightK1,
    UpVal = RightV1,
    MovedChild = RightC1,

    UpdatedLeft = ?INTERNAL2(
        LeftK1,
        ParentK,
        [LeftV1 | ParentV],
        % internal2_sizes_pack(B, LeftS1, LeftS2, MovedSize),
        NewLeftSizes,
        LeftC1,
        LeftC2,
        MovedChild
    ),

    UpdatedRight = ?INTERNAL2(
        RightK2,
        RightK3,
        [RightV2 | RightV3],
        % internal2_sizes_pack(B, RightS2, RightS3, RightS4),
        NewRightSizes,
        RightC2,
        RightC3,
        RightC4
    ),
    {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize}.

-compile({inline, rebalance_left_internal1_internal2/16}).
rebalance_left_internal1_internal2(
    B,
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

    if
        Merge ->
            [RightV1 | RightV2] = RightValues,

            MergedSizes = internal1_sizes_rebalance_left_merge_with_internal2(B, LeftSizes, RightSizes),

            {merged,
                ?INTERNAL4(
                    LeftK1,
                    ParentK,
                    RightK1,
                    RightK2,
                    {LeftV1, ParentV, RightV1, RightV2},
                    % internal4_sizes_pack(B, LeftS1, LeftS2, RightS1, RightS2, RightS3),
                    MergedSizes,
                    LeftC1,
                    LeftC2,
                    RightC1,
                    RightC2,
                    RightC3
                )};
        true ->
            no
    end.

%%%

-compile({inline, [maybe_rebalance_left_leaf/5]}).
maybe_rebalance_left_leaf(Left, ParentK, ParentV, Right, Merge) ->
    case Left of
        ?LEAF1(K1, V1) ->
            rebalance_left_leaf1(K1, V1, ParentK, ParentV, Right, Merge);
        _ ->
            no
    end.

-compile({inline, [rebalance_left_leaf1/6]}).
rebalance_left_leaf1(LeftK1, LeftV1, ParentK, ParentV, Right, Merge) ->
    % Why MovedSize is 0:
    % * We deleted a key in the left node
    % * Then we're gonna move a key from right to left
    %
    % So, left's size doesn't change. Right does -- by -1 -- which is what the
    % deletion rebalance functions assume (one additional key lost).
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

-compile({inline, maybe_rebalance_right/5}).
maybe_rebalance_right([B | _], Left, ParentK, ParentV, Right) ->
    maybe_rebalance_right_internal(B, Left, ParentK, ParentV, Right);
maybe_rebalance_right(_, Left, ParentK, ParentV, Right) ->
    maybe_rebalance_right_leaf(Left, ParentK, ParentV, Right).

-compile({inline, maybe_rebalance_right_internal/5}).
maybe_rebalance_right_internal(B, Left, ParentK, ParentV, Right) ->
    case Right of
        ?INTERNAL1(RightK1, RightV1, RightSizes, RightC1, RightC2) ->
            rebalance_right_internal1(
                B, RightK1, RightV1, RightSizes, RightC1, RightC2, ParentK, ParentV, Left
            );
        _ ->
            no
    end.

-compile({inline, [rebalance_right_internal1/9]}).
rebalance_right_internal1(
    B, RightK1, RightV1, RightSizes, RightC1, RightC2, ParentK, ParentV, Left
) ->
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
                B,
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
                B,
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
                B,
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

-compile({inline, rebalance_right_internal1_internal4/19}).
rebalance_right_internal1_internal4(
    B,
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

    {NewLeftSizes, NewRightSizes, MovedSize} = internal1_sizes_rebalance_right_with_internal4(B, LeftSizes, RightSizes),

    UpK = LeftK4,
    UpVal = LeftV4,
    MovedChild = LeftC5,

    UpdatedLeft = ?INTERNAL3(
        LeftK1,
        LeftK2,
        LeftK3,
        {LeftV1, LeftV2, LeftV3},
        % internal3_sizes_pack(B, LeftS1, LeftS2, LeftS3, LeftS4),
        NewLeftSizes,
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
            NewRightSizes,
            % internal2_sizes_pack(B, MovedSize, RightS1, RightS2),
            MovedChild,
            RightC1,
            RightC2
        ),

    {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize}.

-compile({inline, rebalance_right_internal1_internal3/17}).
rebalance_right_internal1_internal3(
    B,
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

    {NewLeftSizes, NewRightSizes, MovedSize} = internal1_sizes_rebalance_right_with_internal3(B, LeftSizes, RightSizes),

    UpK = LeftK3,
    UpVal = LeftV3,
    MovedChild = LeftC4,

    UpdatedLeft = ?INTERNAL2(
        LeftK1,
        LeftK2,
        [LeftV1 | LeftV2],
        NewLeftSizes,
        % internal2_sizes_pack(B, LeftS1, LeftS2, LeftS3),
        LeftC1,
        LeftC2,
        LeftC3
    ),

    UpdatedRight =
        ?INTERNAL2(
            ParentK,
            RightK1,
            [ParentV | RightV1],
            NewRightSizes,
            % internal2_sizes_pack(B, MovedSize, RightS1, RightS2),
            MovedChild,
            RightC1,
            RightC2
        ),

    {UpK, UpVal, UpdatedLeft, UpdatedRight, MovedSize}.

-compile({inline, rebalance_right_internal1_internal2/15}).
rebalance_right_internal1_internal2(
    B,
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

    MergedSizes = internal1_sizes_rebalance_right_merge_with_internal2(B, LeftSizes, RightSizes),

    {merged,
        ?INTERNAL4(
            LeftK1,
            LeftK2,
            ParentK,
            RightK1,
            {LeftV1, LeftV2, ParentV, RightV1},
            MergedSizes,
            %internal4_sizes_pack(B, LeftS1, LeftS2, LeftS3, RightS1, RightS2),
            LeftC1,
            LeftC2,
            LeftC3,
            RightC1,
            RightC2
        )}.

-compile({inline, maybe_rebalance_right_leaf/4}).
maybe_rebalance_right_leaf(Left, ParentK, ParentV, Right) ->
    case Right of
        ?LEAF1(RightK1, RightV1) ->
            rebalance_right_leaf1(RightK1, RightV1, ParentK, ParentV, Left);
        _ ->
            no
    end.

-compile({inline, [rebalance_right_leaf1/5]}).
rebalance_right_leaf1(RightK1, RightV1, ParentK, ParentV, Left) ->
    % Why MovedSize is 0:
    % * We deleted a key in the right node
    % * Then we're gonna move a key from left to right
    %
    % So, right's size doesn't change. Left does -- by -1 -- which is what the
    % deletion rebalance functions assume (one additional key lost).
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

-compile({inline, maybe_rebalance_mid/8}).
maybe_rebalance_mid([B | _], Left, ParentK1, ParentV1, Mid, ParentK2, ParentV2, Right) ->
    case maybe_rebalance_left_internal(B, Mid, ParentK2, ParentV2, Right, false) of
        no ->
            case maybe_rebalance_right_internal(B, Left, ParentK1, ParentV1, Mid) of
                no ->
                    no;
                Rebalanced ->
                    {from_left, Rebalanced}
            end;
        Rebalanced ->
            {from_right, Rebalanced}
    end;
maybe_rebalance_mid(_, Left, ParentK1, ParentV1, Mid, ParentK2, ParentV2, Right) ->
    case maybe_rebalance_left_leaf(Mid, ParentK2, ParentV2, Right, false) of
        no ->
            case maybe_rebalance_right_leaf(Left, ParentK1, ParentV1, Mid) of
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

stats(Root, H2B, ExpectedNrOfKeys) ->
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
            stats_recur(Node, Depth, H2B, ExpectedNrOfKeys, Acc1)
    end.

stats_recur(
    ?INTERNAL4(_, _, _, _, _, Sizes, C1, C2, C3, C4, C5), Depth, [B | H2B], ExpectedNrOfKeys, Acc
) ->
    {S1, S2, S3, S4, S5} = internal4_sizes_unpack(B, Sizes),
    ?assertEqual(S1 + S2 + S3 + S4 + S5 + 4, ExpectedNrOfKeys),

    Acc2 = stats_inc_node_count(Acc, internal4),
    Acc3 = stats_recur(C1, Depth + 1, H2B, S1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, H2B, S2, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, H2B, S3, Acc4),
    Acc6 = stats_recur(C4, Depth + 1, H2B, S4, Acc5),
    Acc7 = stats_recur(C5, Depth + 1, H2B, S5, Acc6),
    Acc7;
stats_recur(?INTERNAL3(_, _, _, _, Sizes, C1, C2, C3, C4), Depth, [B | H2B], ExpectedNrOfKeys, Acc) ->
    {S1, S2, S3, S4} = internal3_sizes_unpack(B, Sizes),
    ?assertEqual(S1 + S2 + S3 + S4 + 3, ExpectedNrOfKeys),

    Acc2 = stats_inc_node_count(Acc, internal3),
    Acc3 = stats_recur(C1, Depth + 1, H2B, S1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, H2B, S2, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, H2B, S3, Acc4),
    Acc6 = stats_recur(C4, Depth + 1, H2B, S4, Acc5),
    Acc6;
stats_recur(?INTERNAL2(_, _, _, Sizes, C1, C2, C3), Depth, [B | H2B], ExpectedNrOfKeys, Acc) ->
    {S1, S2, S3} = internal2_sizes_unpack(B, Sizes),
    ?assertEqual(S1 + S2 + S3 + 2, ExpectedNrOfKeys),

    Acc2 = stats_inc_node_count(Acc, internal2),
    Acc3 = stats_recur(C1, Depth + 1, H2B, S1, Acc2),
    Acc4 = stats_recur(C2, Depth + 1, H2B, S2, Acc3),
    Acc5 = stats_recur(C3, Depth + 1, H2B, S3, Acc4),
    Acc5;
stats_recur(?INTERNAL1(_, _, Sizes, C1, C2), Depth, [B | H2B], ExpectedNrOfKeys, Acc) ->
    [S1 | S2] = internal1_sizes_unpack(B, Sizes),
    ?assertEqual(S1 + S2 + 1, ExpectedNrOfKeys),

    Acc2 =
        case Depth of
            1 -> Acc;
            _ -> stats_inc_wrong_depth_count(Acc, {internal1, Depth})
        end,
    Acc3 = stats_inc_node_count(Acc2, internal1),
    Acc4 = stats_recur(C1, Depth + 1, H2B, S1, Acc3),
    Acc5 = stats_recur(C2, Depth + 1, H2B, S2, Acc4),
    Acc5;
stats_recur(?LEAF4(_, _, _, _, _, _, _, _), Depth, [], ExpectedNrOfKeys, Acc) ->
    ?assertEqual(ExpectedNrOfKeys, 4),

    Acc2 = stats_inc_node_count(Acc, leaf4),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF3(_, _, _, _, _, _), Depth, [], ExpectedNrOfKeys, Acc) ->
    ?assertEqual(ExpectedNrOfKeys, 3),

    Acc2 = stats_inc_node_count(Acc, leaf3),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF2(_, _, _, _), Depth, [], ExpectedNrOfKeys, Acc) ->
    ?assertEqual(ExpectedNrOfKeys, 2),

    Acc2 = stats_inc_node_count(Acc, leaf2),
    Acc3 = stats_register_leaf_height(Acc2, Depth),
    Acc3;
stats_recur(?LEAF1(_, _), Depth, [], ExpectedNrOfKeys, Acc) ->
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

root_nth_internal(B, H2B, N, ?INTERNAL1(K1, V1, Sizes, C1, C2)) ->
    nth_internal1(B, H2B, N, K1, V1, Sizes, C1, C2);
root_nth_internal(B, H2B, N, Root) ->
    nth_general_internal(B, H2B, N, Root).

root_nth_leaf(N, ?LEAF1(K1, V1)) ->
    nth_leaf1(N, K1, V1);
root_nth_leaf(N, ?LEAF0) ->
    error_badkey(N);
root_nth_leaf(N, Root) ->
    nth_general_leaf(N, Root).

nth_recur(H2B, N, Node) ->
    case H2B of
        [B | NextH2B] ->
            nth_general_internal(B, NextH2B, N, Node);
        %
        _ ->
            nth_general_leaf(N, Node)
    end.

nth_general_internal(B, H2B, N, Node) ->
    case Node of
        ?INTERNAL4(K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
            nth_internal4(B, H2B, N, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5);
        %
        ?INTERNAL3(K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
            nth_internal3(B, H2B, N, K1, K2, K3, Values, Sizes, C1, C2, C3, C4);
        %
        ?INTERNAL2(K1, K2, Values, Sizes, C1, C2, C3) ->
            nth_internal2(B, H2B, N, K1, K2, Values, Sizes, C1, C2, C3)
    end.

nth_general_leaf(N, Node) ->
    case Node of
        ?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) ->
            nth_leaf4(N, K1, K2, K3, K4, V1, V2, V3, V4);
        %
        ?LEAF3(K1, K2, K3, V1, V2, V3) ->
            nth_leaf3(N, K1, K2, K3, V1, V2, V3);
        %
        ?LEAF2(K1, K2, V1, V2) ->
            nth_leaf2(N, K1, K2, V1, V2)
    end.

-compile({inline, nth_internal4/14}).
nth_internal4(B, H2B, N, K1, K2, K3, K4, Values, Sizes, C1, C2, C3, C4, C5) ->
    {S1, S2, S3, S4, _} = internal4_sizes_unpack(B, Sizes),

    Pos3 = S1 + S2 + S3 + 3,

    if
        N < Pos3 ->
            Pos1 = S1 + 1,

            if
                N > Pos1 ->
                    Pos2 = Pos1 + S2 + 1,

                    if
                        N < Pos2 ->
                            nth_recur(H2B, N - Pos1, C2);
                        N > Pos2 ->
                            nth_recur(H2B, N - Pos2, C3);
                        true ->
                            {K2, element(2, Values)}
                    end;
                N < Pos1 ->
                    nth_recur(H2B, N, C1);
                true ->
                    {K1, element(1, Values)}
            end;
        N > Pos3 ->
            Pos4 = Pos3 + S4 + 1,

            if
                N < Pos4 ->
                    nth_recur(H2B, N - Pos3, C4);
                N > Pos4 ->
                    nth_recur(H2B, N - Pos4, C5);
                true ->
                    {K4, element(4, Values)}
            end;
        true ->
            {K3, element(3, Values)}
    end.

-compile({inline, nth_internal3/12}).
nth_internal3(B, H2B, N, K1, K2, K3, Values, Sizes, C1, C2, C3, C4) ->
    {S1, S2, S3, _} = internal3_sizes_unpack(B, Sizes),

    Pos2 = S1 + S2 + 2,

    if
        N < Pos2 ->
            Pos1 = S1 + 1,

            if
                N < Pos1 ->
                    nth_recur(H2B, N, C1);
                N > Pos1 ->
                    nth_recur(H2B, N - Pos1, C2);
                true ->
                    {K1, element(1, Values)}
            end;
        N > Pos2 ->
            Pos3 = Pos2 + S3 + 1,

            if
                N < Pos3 ->
                    nth_recur(H2B, N - Pos2, C3);
                N > Pos3 ->
                    nth_recur(H2B, N - Pos3, C4);
                true ->
                    {K3, element(3, Values)}
            end;
        true ->
            {K2, element(2, Values)}
    end.

-compile({inline, nth_internal2/10}).
nth_internal2(B, H2B, N, K1, K2, Values, Sizes, C1, C2, C3) ->
    {S1, S2, _} = internal2_sizes_unpack(B, Sizes),

    Pos1 = S1 + 1,

    if
        N > Pos1 ->
            Pos2 = Pos1 + S2 + 1,

            if
                N < Pos2 ->
                    nth_recur(H2B, N - Pos1, C2);
                N > Pos2 ->
                    nth_recur(H2B, N - Pos2, C3);
                true ->
                    {K2, tl(Values)}
            end;
        N < Pos1 ->
            nth_recur(H2B, N, C1);
        true ->
            {K1, hd(Values)}
    end.

-compile({inline, nth_internal1/8}).
nth_internal1(B, H2B, N, K1, V1, Sizes, C1, C2) ->
    [S1 | _] = internal1_sizes_unpack(B, Sizes),

    Pos1 = S1 + 1,

    if
        N < Pos1 ->
            nth_recur(H2B, N, C1);
        N > Pos1 ->
            nth_recur(H2B, N - Pos1, C2);
        true ->
            {K1, V1}
    end.

-compile({inline, nth_leaf4/9}).
nth_leaf4(N, K1, K2, K3, K4, V1, V2, V3, V4) ->
    case N of
        1 -> {K1, V1};
        2 -> {K2, V2};
        3 -> {K3, V3};
        4 -> {K4, V4}
    end.

-compile({inline, nth_leaf3/7}).
nth_leaf3(N, K1, K2, K3, V1, V2, V3) ->
    case N of
        1 -> {K1, V1};
        2 -> {K2, V2};
        3 -> {K3, V3}
    end.

-compile({inline, nth_leaf2/5}).
nth_leaf2(N, K1, K2, V1, V2) ->
    case N of
        1 -> {K1, V1};
        2 -> {K2, V2}
    end.

-compile({inline, nth_leaf1/3}).
nth_leaf1(1, K1, V1) ->
    {K1, V1}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Size Packing for INTERNAL4
%% ------------------------------------------------------------------

%% Layouts:
%% INTERNAL4: [<<S2, S1>>, <<S4, S3>> | S5]

-ifdef(TEST).
-compile({inline, internal4_sizes_pack/6}).
internal4_sizes_pack(B, S1, S2, S3, S4, S5) ->
    S21 = S1 bor (S2 bsl B),
    S43 = S3 bor (S4 bsl B),
    [S21, S43 | S5].
-endif.

-compile({inline, internal4_sizes_unpack/2}).
internal4_sizes_unpack(B, [S21, S43 | S5]) ->
    Mask = ?PACKED_MASK(B),
    S1 = S21 band Mask,
    S2 = S21 bsr B,
    S3 = S43 band Mask,
    S4 = S43 bsr B,
    {S1, S2, S3, S4, S5}.

%%%

-compile({inline, internal4_sizes_split1/4}).
internal4_sizes_split1(B, [S21, S43 | S5], LSize, RSize) ->
    Mask = ?PACKED_MASK(B),

    Left1 = LSize,
    Left32 = RSize bor (S21 band (Mask bsl B)),

    Right1 = S43 band Mask,
    Right32 = (S43 bsr B) bor (S5 bsl B),

    LeftSizes = [Left1 | Left32],
    RightSizes = [Right1 | Right32],
    [LeftSizes | RightSizes].

-compile({inline, internal4_sizes_split2/4}).
internal4_sizes_split2(B, [S21, S43 | S5], LSize, RSize) ->
    Mask = ?PACKED_MASK(B),

    Left1 = S21 band Mask,
    Left32 = LSize bor (RSize bsl B),

    Right1 = S43 band Mask,
    Right32 = (S43 bsr B) bor (S5 bsl B),

    LeftSizes = [Left1 | Left32],
    RightSizes = [Right1 | Right32],
    [LeftSizes | RightSizes].

-compile({inline, internal4_sizes_split3/4}).
internal4_sizes_split3(B, [S21, S43 | S5], LSize, RSize) ->
    Mask = ?PACKED_MASK(B),

    Left1 = S21 band Mask,
    Left32 = (S21 bsr B) bor (LSize bsl B),

    Right1 = RSize,
    Right32 = (S43 bsr B) bor (S5 bsl B),

    LeftSizes = [Left1 | Left32],
    RightSizes = [Right1 | Right32],
    [LeftSizes | RightSizes].

-compile({inline, internal4_sizes_split4/4}).
internal4_sizes_split4(B, [S21, S43 | S5], LSize, RSize) ->
    Mask = ?PACKED_MASK(B),

    Left1 = S21 band Mask,
    Left32 = (S21 bsr B) bor ((S43 band Mask) bsl B),

    Right1 = LSize,
    Right32 = RSize bor (S5 bsl B),

    LeftSizes = [Left1 | Left32],
    RightSizes = [Right1 | Right32],
    [LeftSizes | RightSizes].

-compile({inline, internal4_sizes_split5/4}).
internal4_sizes_split5(B, [S21, S43 | _], LSize, RSize) ->
    Mask = ?PACKED_MASK(B),

    Left1 = S21 band Mask,
    Left32 = (S21 bsr B) bor ((S43 band Mask) bsl B),

    Right1 = S43 bsr B,
    Right32 = LSize bor (RSize bsl B),

    LeftSizes = [Left1 | Left32],
    RightSizes = [Right1 | Right32],
    [LeftSizes | RightSizes].

%%%

-compile({inline, internal4_sizes_update1/3}).
internal4_sizes_update1(_, [S21 | Tail], Inc) ->
    [S21 + Inc | Tail].

-compile({inline, internal4_sizes_update2/3}).
internal4_sizes_update2(B, [S21 | Tail], Inc) ->
    [S21 + (Inc bsl B) | Tail].

-compile({inline, internal4_sizes_update3/3}).
internal4_sizes_update3(_, [S21, S43 | S5], Inc) ->
    [S21, S43 + Inc | S5].

-compile({inline, internal4_sizes_update4/3}).
internal4_sizes_update4(B, [S21, S43 | S5], Inc) ->
    [S21, S43 + (Inc bsl B) | S5].

-compile({inline, internal4_sizes_update5/3}).
internal4_sizes_update5(_, [S21, S43 | S5], Inc) ->
    [S21, S43 | S5 + Inc].

%%%

-compile({inline, internal4_sizes_update12/4}).
internal4_sizes_update12(B, [S21 | Tail], Inc1, Inc2) ->
    [S21 + Inc1 + (Inc2 bsl B) | Tail].

-compile({inline, internal4_sizes_update23/4}).
internal4_sizes_update23(B, [S21, S43 | S5], Inc2, Inc3) ->
    [S21 + (Inc2 bsl B), S43 + Inc3 | S5].

-compile({inline, internal4_sizes_update34/4}).
internal4_sizes_update34(B, [S21, S43 | S5], Inc3, Inc4) ->
    [S21, S43 + Inc3 + (Inc4 bsl B) | S5].

-compile({inline, internal4_sizes_update45/4}).
internal4_sizes_update45(B, [S21, S43 | S5], Inc4, Inc5) ->
    [S21, S43 + (Inc4 bsl B) | S5 + Inc5].

%%%

%%%

-compile({inline, internal4_sizes_merge12/2}).
internal4_sizes_merge12(B, [S21, S43 | S5]) ->
    Mask = ?PACKED_MASK(B),
    NewS1 = (S21 band Mask) + (S21 bsr B),
    NewS2 = S43 band Mask,
    NewS3 = S43 bsr B,
    NewS4 = S5,

    NewS21 = NewS1 bor (NewS2 bsl B),
    NewS43 = NewS3 bor (NewS4 bsl B),
    [NewS21 | NewS43].


-compile({inline, internal4_sizes_merge23/2}).
internal4_sizes_merge23(B, [S21, S43 | S5]) ->
    Mask = ?PACKED_MASK(B),

    NewS1 = S21 band Mask,
    NewS2 = (S21 bsr B) + (S43 band Mask),
    NewS3 = S43 bsr B,
    NewS4 = S5,

    NewS21 = NewS1 bor (NewS2 bsl B),
    NewS43 = NewS3 bor (NewS4 bsl B),
    [NewS21 | NewS43].


-compile({inline, internal4_sizes_merge34/2}).
internal4_sizes_merge34(B, [S21, S43 | S5]) ->
    Mask = ?PACKED_MASK(B),

    NewS3 = (S43 bsr B) + (S43 band Mask),
    NewS4 = S5,

    NewS21 = S21,
    NewS43 = NewS3 bor (NewS4 bsl B),
    [NewS21 | NewS43].

-compile({inline, internal4_sizes_merge45/2}).
internal4_sizes_merge45(B, [S21, S43 | S5]) ->
    NewS21 = S21,
    NewS43 = S43 + (S5 bsl B),
    [NewS21 | NewS43].

%% ------------------------------------------------------------------
%% Internal Function Definitions: Size Packing for INTERNAL3
%% ------------------------------------------------------------------

-compile({inline, internal3_sizes_pack/5}).
internal3_sizes_pack(B, S1, S2, S3, S4) ->
    S21 = S1 bor (S2 bsl B),
    S43 = S3 bor (S4 bsl B),
    [S21 | S43].

-compile({inline, internal3_sizes_unpack/2}).
internal3_sizes_unpack(B, [S21 | S43]) ->
    Mask = ?PACKED_MASK(B),

    S1 = S21 band Mask,
    S2 = S21 bsr B,
    S3 = S43 band Mask,
    S4 = S43 bsr B,

    {S1, S2, S3, S4}.

%%%

-compile({inline, internal3_sizes_split1/4}).
internal3_sizes_split1(B, [S21 | S43], LSize, RSize) ->
    Mask = ?PACKED_MASK(B),

%    NewS1 = LSize,
%    NewS2 = RSize,
%    NewS3 = S21 bsr B,
%    NewS4 = S43 band Mask,
%    NewS5 = S43 bsr B,
%
%    internal4_sizes_pack(B, NewS1, NewS2, NewS3, NewS4, NewS5).

    NewS21 = LSize bor (RSize bsl B),
    NewS43 = (S21 bsr B) bor ((S43 band Mask) bsl B),
    NewS5 = S43 bsr B,
    [NewS21, NewS43 | NewS5].

-compile({inline, internal3_sizes_split2/4}).
internal3_sizes_split2(B, [S21 | S43], LSize, RSize) ->
    Mask = ?PACKED_MASK(B),

%    NewS1 = S21 band Mask,
%    NewS2 = LSize,
%    NewS3 = RSize,
%    NewS4 = S43 band Mask,
%    NewS5 = S43 bsr B,
%
%    internal4_sizes_pack(B, NewS1, NewS2, NewS3, NewS4, NewS5).

    NewS21 = (S21 band Mask) bor (LSize bsl B),
    NewS43 = RSize bor ((S43 band Mask) bsl B),
    NewS5 = S43 bsr B,
    [NewS21, NewS43 | NewS5].

-compile({inline, internal3_sizes_split3/4}).
internal3_sizes_split3(B, [S21 | S43], LSize, RSize) ->
%    NewS1 = S21 band Mask,
%    NewS2 = S21 bsr B,
%    NewS3 = LSize,
%    NewS4 = RSize,
%    NewS5 = S43 bsr B,
%
%    internal4_sizes_pack(B, NewS1, NewS2, NewS3, NewS4, NewS5).

    NewS21 = S21,
    NewS43 = LSize bor (RSize bsl B),
    NewS5 = S43 bsr B,
    [NewS21, NewS43 | NewS5].

-compile({inline, internal3_sizes_split4/4}).
internal3_sizes_split4(B, [S21 | S43], LSize, RSize) ->
    Mask = ?PACKED_MASK(B),

%    NewS1 = S21 band Mask,
%    NewS2 = S21 bsr B,
%    NewS3 = S43 band Mask,
%    NewS4 = LSize,
%    NewS5 = RSize,
%
%    internal4_sizes_pack(B, NewS1, NewS2, NewS3, NewS4, NewS5).

    NewS21 = S21,
    NewS43 = (S43 band Mask) bor (LSize bsl B),
    NewS5 = RSize,
    [NewS21, NewS43 | NewS5].

%%%

-compile({inline, internal3_sizes_update1/3}).
internal3_sizes_update1(_, [S21 | S43], Inc) ->
    [S21 + Inc | S43].

-compile({inline, internal3_sizes_update2/3}).
internal3_sizes_update2(B, [S21 | S43], Inc) ->
    [S21 + (Inc bsl B) | S43].

-compile({inline, internal3_sizes_update3/3}).
internal3_sizes_update3(_, [S21 | S43], Inc) ->
    [S21 | S43 + Inc].

-compile({inline, internal3_sizes_update4/3}).
internal3_sizes_update4(B, [S21 | S43], Inc) ->
    [S21 | S43 + (Inc bsl B)].

%%%

-compile({inline, internal3_sizes_update12/4}).
internal3_sizes_update12(B, [S21 | S43], Inc1, Inc2) ->
    [
     S21 + Inc1 + (Inc2 bsl B)
     | S43
    ].

-compile({inline, internal3_sizes_update23/4}).
internal3_sizes_update23(B, [S21 | S43], Inc2, Inc3) ->
    [
     S21 + (Inc2 bsl B)
     | S43 + Inc3
    ].


-compile({inline, internal3_sizes_update34/4}).
internal3_sizes_update34(B, [S21 | S43], Inc3, Inc4) ->
    [
     S21
     | S43 + Inc3 + (Inc4 bsl B)
    ].


%%%

-compile({inline, internal3_sizes_merge12/2}).
internal3_sizes_merge12(B, [S21 | S43]) ->
    Mask = ?PACKED_MASK(B),
    NewS1 = (S21 band Mask) + (S21 bsr B),
    % NewS2 = S43 band Mask,
    % NewS3 = S43 bsr B,
    NewS32 = S43,
    [NewS1 | NewS32].

-compile({inline, internal3_sizes_merge23/2}).
internal3_sizes_merge23(B, [S21 | S43]) ->
    Mask = ?PACKED_MASK(B),
    NewS1 = S21 band Mask,
    % NewS2 = (S21 bsr B) + (S43 band Mask),
    % NewS3 = S43 bsr B,
    NewS32 = S43 + (S21 bsr B),
    [NewS1 | NewS32].

-compile({inline, internal3_sizes_merge34/2}).
internal3_sizes_merge34(B, [S21 | S43]) ->
    Mask = ?PACKED_MASK(B),
    NewS1 = S21 band Mask,
    NewS2 = S21 bsr B,
    NewS3 = (S43 band Mask) + (S43 bsr B),
    NewS32 = NewS2 bor (NewS3 bsl B),
    [NewS1 | NewS32].

%% ------------------------------------------------------------------
%% Internal Function Definitions: Size Packing for INTERNAL2
%% ------------------------------------------------------------------

%% Layouts:
%% INTERNAL3: [<<S4, S1>>, <<S3, S2>>]
%%
%% INTERNAL2: [<<S1>>, <<S3, S2>>]

-compile({inline, internal2_sizes_pack/4}).
internal2_sizes_pack(B, S1, S2, S3) ->
    S32 = S2 bor (S3 bsl B),
    [S1 | S32].

-compile({inline, internal2_sizes_unpack/2}).
internal2_sizes_unpack(B, [S1 | S32]) ->
    S2 = S32 band ?PACKED_MASK(B),
    S3 = S32 bsr B,
    {S1, S2, S3}.

-compile({inline, internal2_sizes_sum/2}).
internal2_sizes_sum(B, [S1 | S32]) ->
    S1 + (S32 band ?PACKED_MASK(B)) + (S32 bsr B).

%%%

-compile({inline, internal2_sizes_split1/4}).
internal2_sizes_split1(B, [_ | S32], LSize, RSize) ->
    % NewS1 = LSize,
    % NewS2 = RSize,
    % NewS3 = S32 band ?PACKED_MASK(B),
    % NewS4 = S32 bsr B,
    % internal3_sizes_pack(B, NewS1, NewS2, NewS3, NewS4).

    NewS21 = LSize bor (RSize bsl B),
    NewS43 = S32,
    [NewS21 | NewS43].

-compile({inline, internal2_sizes_split2/4}).
internal2_sizes_split2(B, [S1 | S32], LSize, RSize) ->
    NewS1 = S1,
    NewS2 = LSize,
    NewS3 = RSize,
    NewS4 = S32 bsr B,
    internal3_sizes_pack(B, NewS1, NewS2, NewS3, NewS4).

-compile({inline, internal2_sizes_split3/4}).
internal2_sizes_split3(B, [S1 | S32], LSize, RSize) ->
    NewS1 = S1,
    NewS2 = S32 band ?PACKED_MASK(B),
    NewS3 = LSize,
    NewS4 = RSize,
    internal3_sizes_pack(B, NewS1, NewS2, NewS3, NewS4).

%%%

-compile({inline, internal2_sizes_update1/3}).
internal2_sizes_update1(_, [S1 | S32], Inc) ->
    [S1 + Inc | S32].

-compile({inline, internal2_sizes_update2/3}).
internal2_sizes_update2(_, [S1 | S32], Inc) ->
    [S1 | S32 + Inc].

-compile({inline, internal2_sizes_update3/3}).
internal2_sizes_update3(B, [S1 | S32], Inc) ->
    [S1 | S32 + (Inc bsl B)].

%%%

-compile({inline, internal2_sizes_update12/4}).
internal2_sizes_update12(_, [S1 | S32], Inc1, Inc2) ->
    [S1 + Inc1 | S32 + Inc2].

-compile({inline, internal2_sizes_update23/4}).
internal2_sizes_update23(B, [S1 | S32], Inc2, Inc3) ->
    [S1 | S32 + Inc2 + (Inc3 bsl B)].

%%%

-compile({inline, internal2_sizes_merge12/2}).
internal2_sizes_merge12(B, [S1 | S32]) ->
    NewS1 = S1 + (S32 band ?PACKED_MASK(B)),
    NewS2 = S32 bsr B,
    [NewS1 | NewS2].

-compile({inline, internal2_sizes_merge23/2}).
internal2_sizes_merge23(B, [S1 | S32]) ->
    NewS2 = (S32 band ?PACKED_MASK(B)) + (S32 bsr B),
    [S1 | NewS2].

%% ------------------------------------------------------------------
%% Internal Function Definitions: Size Packing for INTERNAL1
%% ------------------------------------------------------------------

-compile({inline, internal1_sizes_pack/3}).
internal1_sizes_pack(_, S1, S2) ->
    [S1 | S2].

-compile({inline, internal1_sizes_unpack/2}).
internal1_sizes_unpack(_, Sizes) ->
    Sizes.

%%%

-compile({inline, internal1_sizes_split1/4}).
internal1_sizes_split1(B, [_ | S2], LSize, RSize) ->
    internal2_sizes_pack(B, LSize, RSize, S2).

-compile({inline, internal1_sizes_split2/4}).
internal1_sizes_split2(B, [S1 | _], LSize, RSize) ->
    internal2_sizes_pack(B, S1, LSize, RSize).

%%%

-compile({inline, internal1_sizes_update1/3}).
internal1_sizes_update1(_, [S1 | S2], Inc) ->
    [S1 + Inc | S2].

-compile({inline, internal1_sizes_update2/3}).
internal1_sizes_update2(_, [S1 | S2], Inc) ->
    [S1 | S2 + Inc].

%%%

-compile({inline, internal1_sizes_update12/4}).
internal1_sizes_update12(_, [S1 | S2], Inc1, Inc2) ->
    [S1 + Inc1 | S2 + Inc2].

%%%

-compile({inline, internal1_sizes_rebalance_left_with_internal4/3}).
internal1_sizes_rebalance_left_with_internal4(B, [Left1 | Left2], [Right21, Right43 | Right5]) ->
    Mask = ?PACKED_MASK(B),

    MovedSize = Right21 band Mask,

    NewLeft1 = Left1,
    NewLeft32 = Left2 bor (MovedSize bsl B),
    NewLeftSizes = [NewLeft1 | NewLeft32],

    NewRight21 = (Right21 bsr B) bor ((Right43 band Mask) bsl B),
    NewRight43 = (Right43 bsr B) bor (Right5 bsl B),
    NewRightSizes = [NewRight21 | NewRight43],

    {NewLeftSizes, NewRightSizes, MovedSize}.

-compile({inline, internal1_sizes_rebalance_left_with_internal3/3}).
internal1_sizes_rebalance_left_with_internal3(B, [Left1 | Left2], [Right21 | Right43]) ->
    Mask = ?PACKED_MASK(B),

    MovedSize = Right21 band Mask,

    NewLeft1 = Left1,
    NewLeft32 = Left2 bor (MovedSize bsl B),
    NewLeftSizes = [NewLeft1 | NewLeft32],

    NewRight1 = Right21 bsr B,
    NewRight32 = Right43,
    NewRightSizes = [NewRight1 | NewRight32],

    {NewLeftSizes, NewRightSizes, MovedSize}.

-compile({inline, internal1_sizes_rebalance_left_merge_with_internal2/3}).
internal1_sizes_rebalance_left_merge_with_internal2(B, [Left1 | Left2], [Right1 | Right32]) ->
    % TODO test
    Mask = ?PACKED_MASK(B),

    Merged21 = Left1 bor (Left2 bsl B),
    Merged43 = Right1 bor ((Right32 band Mask) bsl B),
    Merged5 = Right32 bsr B,

    [Merged21, Merged43 | Merged5].

%%%

-compile({inline, internal1_sizes_rebalance_right_with_internal4/3}).
internal1_sizes_rebalance_right_with_internal4(B, [Left21, Left43 | Left5], [Right1 | Right2]) ->
    MovedSize = Left5,

    NewLeftSizes = [Left21 | Left43],

    NewRight1 = MovedSize,
    NewRight32 = Right1 bor (Right2 bsl B),
    NewRightSizes = [NewRight1 | NewRight32],

    {NewLeftSizes, NewRightSizes, MovedSize}.

-compile({inline, internal1_sizes_rebalance_right_with_internal3/3}).
internal1_sizes_rebalance_right_with_internal3(B, [Left21 | Left43], [Right1 | Right2]) ->
    Mask = ?PACKED_MASK(B),

    MovedSize = Left43 bsr B,

    NewLeft1 = Left21 band Mask,
    NewLeft32 = (Left21 bsr B) bor ((Left43 band Mask) bsl B),
    NewLeftSizes = [NewLeft1 | NewLeft32],

    NewRight1 = MovedSize,
    NewRight32 = Right1 bor (Right2 bsl B),
    NewRightSizes = [NewRight1 | NewRight32],

    {NewLeftSizes, NewRightSizes, MovedSize}.

-compile({inline, internal1_sizes_rebalance_right_merge_with_internal2/3}).
internal1_sizes_rebalance_right_merge_with_internal2(B, [Left1 | Left32], [Right1 | Right2]) ->
    % TODO test
    Mask = ?PACKED_MASK(B),

    Merged21 = Left1 bor ((Left32 band Mask) bsl B),
    Merged43 = (Left32 bsr B) bor (Right1 bsl B),
    Merged5 = Right2,

    [Merged21, Merged43 | Merged5].

%% ------------------------------------------------------------------
%% Unit Test Definitions
%% ------------------------------------------------------------------
-ifdef(TEST).

%%%%%%
%%%%%% Internal 4
%%%%%%

internal4_sizes_repack_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, Packed),
           {S1, S2, S3, S4, S5}
        )
      end).

internal4_sizes_split1_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5, LSize, RSize]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        [SplitLSizes | SplitRSizes] = internal4_sizes_split1(B, Packed, LSize, RSize),
        ?assertEqual({LSize, RSize, S2}, internal2_sizes_unpack(B, SplitLSizes)),
        ?assertEqual({S3, S4, S5}, internal2_sizes_unpack(B, SplitRSizes))
      end).

internal4_sizes_split2_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5, LSize, RSize]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        [SplitLSizes | SplitRSizes] = internal4_sizes_split2(B, Packed, LSize, RSize),
        ?assertEqual({S1, LSize, RSize}, internal2_sizes_unpack(B, SplitLSizes)),
        ?assertEqual({S3, S4, S5}, internal2_sizes_unpack(B, SplitRSizes))
      end).

internal4_sizes_split3_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5, LSize, RSize]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        [SplitLSizes | SplitRSizes] = internal4_sizes_split3(B, Packed, LSize, RSize),
        ?assertEqual({S1, S2, LSize}, internal2_sizes_unpack(B, SplitLSizes)),
        ?assertEqual({RSize, S4, S5}, internal2_sizes_unpack(B, SplitRSizes))
      end).

internal4_sizes_split4_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5, LSize, RSize]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        [SplitLSizes | SplitRSizes] = internal4_sizes_split4(B, Packed, LSize, RSize),
        ?assertEqual({S1, S2, S3}, internal2_sizes_unpack(B, SplitLSizes)),
        ?assertEqual({LSize, RSize, S5}, internal2_sizes_unpack(B, SplitRSizes))
      end).

internal4_sizes_split5_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5, LSize, RSize]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        [SplitLSizes | SplitRSizes] = internal4_sizes_split5(B, Packed, LSize, RSize),
        ?assertEqual({S1, S2, S3}, internal2_sizes_unpack(B, SplitLSizes)),
        ?assertEqual({S4, LSize, RSize}, internal2_sizes_unpack(B, SplitRSizes))
      end).

internal4_sizes_update1_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        Inc = random_inc(S1),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, internal4_sizes_update1(B, Packed, Inc)),
           {S1 + Inc, S2, S3, S4, S5}
        )
      end).

internal4_sizes_update2_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        Inc = random_inc(S2),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, internal4_sizes_update2(B, Packed, Inc)),
           {S1, S2 + Inc, S3, S4, S5}
        )
      end).

internal4_sizes_update3_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        Inc = random_inc(S3),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, internal4_sizes_update3(B, Packed, Inc)),
           {S1, S2, S3 + Inc, S4, S5}
        )
      end).

internal4_sizes_update4_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        Inc = random_inc(S4),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, internal4_sizes_update4(B, Packed, Inc)),
           {S1, S2, S3, S4 + Inc, S5}
        )
      end).

internal4_sizes_update5_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        Inc = random_inc(S5),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, internal4_sizes_update5(B, Packed, Inc)),
           {S1, S2, S3, S4, S5 + Inc}
        )
      end).

internal4_sizes_update12_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        Inc = random_inc_many([S1, S2]),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, internal4_sizes_update12(B, Packed, Inc, -Inc)),
           {S1 + Inc, S2 - Inc, S3, S4, S5}
        )
      end).

internal4_sizes_update23_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        Inc = random_inc_many([S2, S3]),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, internal4_sizes_update23(B, Packed, Inc, -Inc)),
           {S1, S2 + Inc, S3 - Inc, S4, S5}
        )
      end).

internal4_sizes_update34_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        Inc = random_inc_many([S3, S4]),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, internal4_sizes_update34(B, Packed, Inc, -Inc)),
           {S1, S2, S3 + Inc, S4 - Inc, S5}
        )
      end).

internal4_sizes_update45_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        Inc = random_inc_many([S4, S5]),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal4_sizes_unpack(B, internal4_sizes_update45(B, Packed, Inc, -Inc)),
           {S1, S2, S3, S4 + Inc, S5 - Inc}
        )
      end).

internal4_sizes_merge12_test() ->
    run_test_repeatedly(
      fun () ->
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal3_sizes_unpack(B, internal4_sizes_merge12(B, Packed)),
           {S1 + S2, S3, S4, S5}
        )
      end).

internal4_sizes_merge23_test() ->
    run_test_repeatedly(
      fun () ->
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal3_sizes_unpack(B, internal4_sizes_merge23(B, Packed)),
           {S1, S2 + S3, S4, S5}
        )
      end).

internal4_sizes_merge34_test() ->
    run_test_repeatedly(
      fun () ->
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal3_sizes_unpack(B, internal4_sizes_merge34(B, Packed)),
           {S1, S2, S3 + S4, S5}
        )
      end).

internal4_sizes_merge45_test() ->
    run_test_repeatedly(
      fun () ->
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        S5 = random_size(RB),
        B = test_b([S1, S2, S3, S4, S5]),
        Packed = internal4_sizes_pack(B, S1, S2, S3, S4, S5),

        ?assertEqual(
           internal3_sizes_unpack(B, internal4_sizes_merge45(B, Packed)),
           {S1, S2, S3, S4 + S5}
        )
      end).

%%%%%%
%%%%%% Internal 3
%%%%%%

internal3_sizes_repack_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal3_sizes_unpack(B, Packed),
           {S1, S2, S3, S4}
        )
      end).

internal3_sizes_split1_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, S4, LSize, RSize]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal4_sizes_unpack(B, internal3_sizes_split1(B, Packed, LSize, RSize)),
           {LSize, RSize, S2, S3, S4}
        )
      end).

internal3_sizes_split2_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, S4, LSize, RSize]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal4_sizes_unpack(B, internal3_sizes_split2(B, Packed, LSize, RSize)),
           {S1, LSize, RSize, S3, S4}
        )
      end).

internal3_sizes_split3_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, S4, LSize, RSize]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal4_sizes_unpack(B, internal3_sizes_split3(B, Packed, LSize, RSize)),
           {S1, S2, LSize, RSize, S4}
        )
      end).

internal3_sizes_split4_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, S4, LSize, RSize]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal4_sizes_unpack(B, internal3_sizes_split4(B, Packed, LSize, RSize)),
           {S1, S2, S3, LSize, RSize}
        )
      end).

internal3_sizes_update1_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        Inc = random_inc(S1),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal3_sizes_unpack(B, internal3_sizes_update1(B, Packed, Inc)),
           {S1 + Inc, S2, S3, S4}
        )
      end).

internal3_sizes_update2_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        Inc = random_inc(S2),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal3_sizes_unpack(B, internal3_sizes_update2(B, Packed, Inc)),
           {S1, S2 + Inc, S3, S4}
        )
      end).

internal3_sizes_update3_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        Inc = random_inc(S3),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal3_sizes_unpack(B, internal3_sizes_update3(B, Packed, Inc)),
           {S1, S2, S3 + Inc, S4}
        )
      end).

internal3_sizes_update4_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        Inc = random_inc(S4),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal3_sizes_unpack(B, internal3_sizes_update4(B, Packed, Inc)),
           {S1, S2, S3, S4 + Inc}
        )
      end).

internal3_sizes_update12_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        Inc = random_inc_many([S1, S2]),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal3_sizes_unpack(B, internal3_sizes_update12(B, Packed, Inc, -Inc)),
           {S1 + Inc, S2 - Inc, S3, S4}
        )
      end).

internal3_sizes_update23_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        Inc = random_inc_many([S2, S3]),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal3_sizes_unpack(B, internal3_sizes_update23(B, Packed, Inc, -Inc)),
           {S1, S2 + Inc, S3 - Inc, S4}
        )
      end).

internal3_sizes_update34_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        Inc = random_inc_many([S3, S4]),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal3_sizes_unpack(B, internal3_sizes_update34(B, Packed, Inc, -Inc)),
           {S1, S2, S3 + Inc, S4 - Inc}
        )
      end).

internal3_sizes_merge12_test() ->
    run_test_repeatedly(
      fun () ->
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal2_sizes_unpack(B, internal3_sizes_merge12(B, Packed)),
           {S1 + S2, S3, S4}
        )
      end).

internal3_sizes_merge23_test() ->
    run_test_repeatedly(
      fun () ->
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal2_sizes_unpack(B, internal3_sizes_merge23(B, Packed)),
           {S1, S2 + S3, S4}
        )
      end).

internal3_sizes_merge34_test() ->
    run_test_repeatedly(
      fun () ->
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        S4 = random_size(RB),
        B = test_b([S1, S2, S3, S4]),
        Packed = internal3_sizes_pack(B, S1, S2, S3, S4),

        ?assertEqual(
           internal2_sizes_unpack(B, internal3_sizes_merge34(B, Packed)),
           {S1, S2, S3 + S4}
        )
      end).

%%%%%%
%%%%%% Internal 2
%%%%%%

internal2_sizes_repack_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        B = test_b([S1, S2, S3]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal2_sizes_unpack(B, Packed),
           {S1, S2, S3}
        )
      end).

internal2_sizes_split1_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, LSize, RSize]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal3_sizes_unpack(B, internal2_sizes_split1(B, Packed, LSize, RSize)),
           {LSize, RSize, S2, S3}
        )
      end).

internal2_sizes_split2_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, LSize, RSize]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal3_sizes_unpack(B, internal2_sizes_split2(B, Packed, LSize, RSize)),
           {S1, LSize, RSize, S3}
        )
      end).

internal2_sizes_split3_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, S3, LSize, RSize]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal3_sizes_unpack(B, internal2_sizes_split3(B, Packed, LSize, RSize)),
           {S1, S2, LSize, RSize}
        )
      end).

internal2_sizes_update1_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        Inc = random_inc(S1),
        B = test_b([S1, S2, S3]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal2_sizes_unpack(B, internal2_sizes_update1(B, Packed, Inc)),
           {S1 + Inc, S2, S3}
        )
      end).

internal2_sizes_update2_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        Inc = random_inc(S2),
        B = test_b([S1, S2, S3]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal2_sizes_unpack(B, internal2_sizes_update2(B, Packed, Inc)),
           {S1, S2 + Inc, S3}
        )
      end).

internal2_sizes_update3_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        Inc = random_inc(S3),
        B = test_b([S1, S2, S3]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal2_sizes_unpack(B, internal2_sizes_update3(B, Packed, Inc)),
           {S1, S2, S3 + Inc}
        )
      end).

internal2_sizes_update12_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        Inc = random_inc_many([S1, S2]),
        B = test_b([S1, S2, S3]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal2_sizes_unpack(B, internal2_sizes_update12(B, Packed, Inc, -Inc)),
           {S1 + Inc, S2 - Inc, S3}
        )
      end).

internal2_sizes_update23_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        Inc = random_inc_many([S2, S3]),
        B = test_b([S1, S2, S3]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal2_sizes_unpack(B, internal2_sizes_update23(B, Packed, Inc, -Inc)),
           {S1, S2 + Inc, S3 - Inc}
        )
      end).

internal2_sizes_merge12_test() ->
    run_test_repeatedly(
      fun () ->
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        B = test_b([S1, S2, S3]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal1_sizes_unpack(B, internal2_sizes_merge12(B, Packed)),
           [S1 + S2 | S3]
        )
      end).

internal2_sizes_merge23_test() ->
    run_test_repeatedly(
      fun () ->
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        S3 = random_size(RB),
        B = test_b([S1, S2, S3]),
        Packed = internal2_sizes_pack(B, S1, S2, S3),

        ?assertEqual(
           internal1_sizes_unpack(B, internal2_sizes_merge23(B, Packed)),
           [S1 | S2 + S3]
        )
      end).

%%%%%%
%%%%%% Internal 1
%%%%%%

internal1_sizes_repack_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        B = test_b([S1, S2]),
        Packed = internal1_sizes_pack(B, S1, S2),

        ?assertEqual(
           internal1_sizes_unpack(B, Packed),
           [S1 | S2]
        )
      end).

internal1_sizes_split1_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, LSize, RSize]),
        Packed = internal1_sizes_pack(B, S1, S2),

        ?assertEqual(
           internal2_sizes_unpack(B, internal1_sizes_split1(B, Packed, LSize, RSize)),
           {LSize, RSize, S2}
        )
      end).

internal1_sizes_split2_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        LSize = random_size(RB),
        RSize = random_size(RB),
        B = test_b([S1, S2, LSize, RSize]),
        Packed = internal1_sizes_pack(B, S1, S2),

        ?assertEqual(
           internal2_sizes_unpack(B, internal1_sizes_split2(B, Packed, LSize, RSize)),
           {S1, LSize, RSize}
        )
      end).

internal1_sizes_update1_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        Inc = random_inc(S1),
        B = test_b([S1, S2]),
        Packed = internal1_sizes_pack(B, S1, S2),

        ?assertEqual(
           internal1_sizes_unpack(B, internal1_sizes_update1(B, Packed, Inc)),
           [S1 + Inc | S2]
        )
      end).

internal1_sizes_update2_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        Inc = random_inc(S2),
        B = test_b([S1, S2]),
        Packed = internal1_sizes_pack(B, S1, S2),

        ?assertEqual(
           internal1_sizes_unpack(B, internal1_sizes_update2(B, Packed, Inc)),
           [S1 | S2 + Inc]
        )
      end).

internal1_sizes_update12_test() ->
    run_test_repeatedly(
      fun () -> 
        RB = random_bitsize(),
        S1 = random_size(RB),
        S2 = random_size(RB),
        Inc = random_inc_many([S1, S2]),
        B = test_b([S1, S2]),
        Packed = internal1_sizes_pack(B, S1, S2),

        ?assertEqual(
           internal1_sizes_unpack(B, internal1_sizes_update12(B, Packed, Inc, -Inc)),
           [S1 + Inc | S2 - Inc]
        )
      end).

%%%%%%%%%%%%%%%%%
%%% Rebalancing left

internal1_sizes_rebalance_left_with_internal4_test() ->
    run_test_repeatedly(
      fun () ->
              RB = random_bitsize(),
              LeftS1 = random_size(RB),
              LeftS2 = random_size(RB),

              RightS1 = random_size(RB),
              RightS2 = random_size(RB),
              RightS3 = random_size(RB),
              RightS4 = random_size(RB),
              RightS5 = random_size(RB),

              B = test_b([LeftS1, LeftS2, RightS1, RightS2, RightS3, RightS4, RightS5]),
              Left = internal1_sizes_pack(B, LeftS1, LeftS2),
              Right = internal4_sizes_pack(B, RightS1, RightS2, RightS3, RightS4, RightS5),

              {NewLeft, NewRight, MovedSize} = internal1_sizes_rebalance_left_with_internal4(B, Left, Right),

              ?assertEqual(
                 internal2_sizes_unpack(B, NewLeft),
                 {LeftS1, LeftS2, RightS1}
              ),

              ?assertEqual(
                 internal3_sizes_unpack(B, NewRight),
                 {RightS2, RightS3, RightS4, RightS5}
              ),

              ?assertEqual(MovedSize, RightS1)
      end).

internal1_sizes_rebalance_left_with_internal3_test() ->
    run_test_repeatedly(
      fun () ->
              RB = random_bitsize(),
              LeftS1 = random_size(RB),
              LeftS2 = random_size(RB),

              RightS1 = random_size(RB),
              RightS2 = random_size(RB),
              RightS3 = random_size(RB),
              RightS4 = random_size(RB),

              B = test_b([LeftS1, LeftS2, RightS1, RightS2, RightS3, RightS4]),
              Left = internal1_sizes_pack(B, LeftS1, LeftS2),
              Right = internal3_sizes_pack(B, RightS1, RightS2, RightS3, RightS4),

              {NewLeft, NewRight, MovedSize} = internal1_sizes_rebalance_left_with_internal3(B, Left, Right),

              ?assertEqual(
                 internal2_sizes_unpack(B, NewLeft),
                 {LeftS1, LeftS2, RightS1}
              ),

              ?assertEqual(
                 internal2_sizes_unpack(B, NewRight),
                 {RightS2, RightS3, RightS4}
              ),

              ?assertEqual(MovedSize, RightS1)
      end).

internal1_sizes_rebalance_left_merge_with_internal2_test() ->
    run_test_repeatedly(
      fun () ->
              RB = random_bitsize(),
              LeftS1 = random_size(RB),
              LeftS2 = random_size(RB),

              RightS1 = random_size(RB),
              RightS2 = random_size(RB),
              RightS3 = random_size(RB),

              B = test_b([LeftS1, LeftS2, RightS1, RightS2, RightS3]),
              Left = internal1_sizes_pack(B, LeftS1, LeftS2),
              Right = internal2_sizes_pack(B, RightS1, RightS2, RightS3),

              MergedSizes = internal1_sizes_rebalance_left_merge_with_internal2(B, Left, Right),

              ?assertEqual(
                 internal4_sizes_unpack(B, MergedSizes),
                 {LeftS1, LeftS2, RightS1, RightS2, RightS3}
              )
      end).

%%%%%%%%%%%%%%%%%
%%% Rebalancing right

internal1_sizes_rebalance_right_with_internal4_test() ->
    run_test_repeatedly(
      fun () ->
              RB = random_bitsize(),
              LeftS1 = random_size(RB),
              LeftS2 = random_size(RB),
              LeftS3 = random_size(RB),
              LeftS4 = random_size(RB),
              LeftS5 = random_size(RB),

              RightS1 = random_size(RB),
              RightS2 = random_size(RB),

              B = test_b([LeftS1, LeftS2, LeftS3, LeftS4, LeftS5, RightS1, RightS2]),
              Left = internal4_sizes_pack(B, LeftS1, LeftS2, LeftS3, LeftS4, LeftS5),
              Right = internal1_sizes_pack(B, RightS1, RightS2),

              {NewLeft, NewRight, MovedSize} = internal1_sizes_rebalance_right_with_internal4(B, Left, Right),

              ?assertEqual(
                 internal3_sizes_unpack(B, NewLeft),
                 {LeftS1, LeftS2, LeftS3, LeftS4}
              ),

              ?assertEqual(
                 internal2_sizes_unpack(B, NewRight),
                 {LeftS5, RightS1, RightS2}
              ),

              ?assertEqual(MovedSize, LeftS5)
      end).

internal1_sizes_rebalance_right_with_internal3_test() ->
    run_test_repeatedly(
      fun () ->
              RB = random_bitsize(),
              LeftS1 = random_size(RB),
              LeftS2 = random_size(RB),
              LeftS3 = random_size(RB),
              LeftS4 = random_size(RB),

              RightS1 = random_size(RB),
              RightS2 = random_size(RB),

              B = test_b([LeftS1, LeftS2, LeftS3, LeftS4, RightS1, RightS2]),
              Left = internal3_sizes_pack(B, LeftS1, LeftS2, LeftS3, LeftS4),
              Right = internal1_sizes_pack(B, RightS1, RightS2),

              {NewLeft, NewRight, MovedSize} = internal1_sizes_rebalance_right_with_internal3(B, Left, Right),

              ?assertEqual(
                 internal2_sizes_unpack(B, NewLeft),
                 {LeftS1, LeftS2, LeftS3}
              ),

              ?assertEqual(
                 internal2_sizes_unpack(B, NewRight),
                 {LeftS4, RightS1, RightS2}
              ),

              ?assertEqual(MovedSize, LeftS4)
      end).

internal1_sizes_rebalance_right_with_internal2_test() ->
    run_test_repeatedly(
      fun () ->
              RB = random_bitsize(),
              LeftS1 = random_size(RB),
              LeftS2 = random_size(RB),
              LeftS3 = random_size(RB),

              RightS1 = random_size(RB),
              RightS2 = random_size(RB),

              B = test_b([LeftS1, LeftS2, LeftS3, RightS1, RightS2]),
              Left = internal2_sizes_pack(B, LeftS1, LeftS2, LeftS3),
              Right = internal1_sizes_pack(B, RightS1, RightS2),

              MergedSizes = internal1_sizes_rebalance_right_merge_with_internal2(B, Left, Right),

              ?assertEqual(
                 internal4_sizes_unpack(B, MergedSizes),
                 {LeftS1, LeftS2, LeftS3, RightS1, RightS2}
              )
      end).

%%%%%%%%%%%%%%%%%%

random_bitsize() ->
    6 + rand:uniform(100).

random_size(Bitsize) ->
    rand:uniform((1 bsl Bitsize) - 1).

test_b(List) ->
    ceil(1 + math:log2(lists:max(List) + 1)).

random_inc(S) ->
    Inc = rand:uniform(S) - (S div 2),
    ?assert(S + Inc > 0),
    Inc.

random_inc_many(List) ->
    Min = lists:min(List),
    rand:uniform(Min) - (Min div 2).

run_test_repeatedly(Fun) ->
    run_test_repeatedly_recur(Fun, 500).

run_test_repeatedly_recur(Fun, N) when N > 0 ->
    Fun(),
    run_test_repeatedly_recur(Fun, N - 1);
run_test_repeatedly_recur(_, 0) ->
    ok.

-endif. % -ifdef(TEST).
