% TODO document
-module(b5_trees_node).

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
    structural_stats/1,
    take/2,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    update/4,
    values/1
]).

%% ------------------------------------------------------------------
%% Macro Definitions: Nodes
%% ------------------------------------------------------------------

% 7 elements
-define(INTERNAL2(K1, K2, V1, V2, C1, C2, C3), {K1, K2, V1, V2, C1, C2, C3}).
-define(INTERNAL2_MATCH(K1, K2, V1, V2, C1, C2, C3), {K1, K2, V1, V2, C1, C2, C3}).
-define(INTERNAL2_MATCH_ALL, {K1, K2, V1, V2, C1, C2, C3}).

% 4 elements
-define(LEAF2(K1, K2, V1, V2), {K1, K2, V1, V2}).
-define(LEAF2_MATCH(K1, K2, V1, V2), {K1, K2, V1, V2}).
-define(LEAF2_MATCH_ALL, {K1, K2, V1, V2}).

% 10 elements
-define(INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4),
    {K1, K2, K3, V1, V2, V3, C1, C2, C3, C4}
).
-define(INTERNAL3_MATCH(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4),
    {K1, K2, K3, V1, V2, V3, C1, C2, C3, C4}
).
-define(INTERNAL3_MATCH_ALL, {K1, K2, K3, V1, V2, V3, C1, C2, C3, C4}).

% 6 elements
-define(LEAF3(K1, K2, K3, V1, V2, V3), {K1, K2, K3, V1, V2, V3}).
-define(LEAF3_MATCH(K1, K2, K3, V1, V2, V3), {K1, K2, K3, V1, V2, V3}).
-define(LEAF3_MATCH_ALL, {K1, K2, K3, V1, V2, V3}).

% 13 elements
-define(INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5),
    {K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5}
).
-define(INTERNAL4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5),
    {K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5}
).
-define(INTERNAL4_MATCH_ALL,
    {K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5}
).

% 8 elements
-define(LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), {K1, K2, K3, K4, V1, V2, V3, V4}).
-define(LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4), {K1, K2, K3, K4, V1, V2, V3, V4}).
-define(LEAF4_MATCH_ALL, {K1, K2, K3, K4, V1, V2, V3, V4}).

% 5 elements
-define(INTERNAL1(K1, V1, C1, C2), {internal1, K1, V1, C1, C2}).
-define(INTERNAL1_MATCH(K1, V1, C1, C2), {_, K1, V1, C1, C2}).
-define(INTERNAL1_MATCH_ALL, {_, K1, V1, C1, C2}).

% improper list
-define(LEAF1(K1, V1), [K1 | V1]).
-define(LEAF1_MATCH(K1, V1), [K1 | V1]).
-define(LEAF1_MATCH_ALL, [K1 | V1]).

% empty root
-define(LEAF0, leaf0).
-define(LEAF0_MATCH, leaf0).
-define(LEAF0_MATCH_ALL, leaf0).

%%%%%%%%

%-define(INTERNAL2_VALUES(V1, V2), [V1 | V2]).
%-define(INTERNAL2_VALUES_MATCH_ALL, [V1 | V2]).
%-define(INTERNAL2_VALUES_GET2(Values), tl(Values)).
%-define(INTERNAL2_VALUES_GET1(Values), hd(Values)).
%
%-define(INTERNAL3_VALUES(V1, V2, V3), {V1, V2, V3}).
%-define(INTERNAL3_VALUES_MATCH_ALL, {V1, V2, V3}).
%-define(INTERNAL3_VALUES_GET1(Values), element(1, Values)).
%-define(INTERNAL3_VALUES_GET2(Values), element(2, Values)).
%-define(INTERNAL3_VALUES_GET3(Values), element(3, Values)).

%-define(INTERNAL4_VALUES(V1, V2, V3, V4), {V1, V2, V3, V4}).
%-define(INTERNAL4_VALUES_MATCH_ALL, {V1, V2, V3, V4}).
%-define(INTERNAL4_VALUES_GET1(Values), element(1, Values)).
%-define(INTERNAL4_VALUES_GET2(Values), element(2, Values)).
%-define(INTERNAL4_VALUES_GET3(Values), element(3, Values)).
%-define(INTERNAL4_VALUES_GET4(Values), element(4, Values)).

%%%%%%%%

% 5 elements; cannot clash with any node type.
-define(SPLIT(SplitK, SplitV, SplitL, SplitR), {split, SplitK, SplitV, SplitL, SplitR}).
-define(SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR), {split, SplitK, SplitV, SplitL, SplitR}).

%%%%%%%%%

% Any of the following cannot clash with either the largest leaf or internal
% nodes, since those are a merged node.

% list
-define(MID_MERGED(MergedNode), [MergedNode]).
-define(MID_MERGED_MATCH(MergedNode), [MergedNode | _]).

% 4 elements
-define(MID_ROTATED_FROM_RIGHT(UpKey, UpValue, UpdatedNode, UpdatedRight),
    {UpKey, UpValue, UpdatedNode, UpdatedRight}
).
% 5 elements
-define(MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedNode, UpdatedRight),
    {from_left, UpKey, UpValue, UpdatedNode, UpdatedRight}
).

%%%

% 4 elements
-define(ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedRight),
    {UpKey, UpValue, UpdatedLeft, UpdatedRight}
).

-define(MERGED(MergedNode), (MergedNode)).

%%%%%%%

-define(ITER_PAIR(Key, Value), ?LEAF1(Key, Value)).
-define(REV_ITER_TAG, reversed).

%% ------------------------------------------------------------------
%% Macro Definitions: Boilerplate Helpers
%% ------------------------------------------------------------------

%% ?INTERNAL4

-define(INTERNAL4_ARGS, K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARITY, 13).
-define(INTERNAL4_ARITY_PLUS1, 14).
-define(INTERNAL4_ARITY_PLUS2, 15).
-define(INTERNAL4_ARITY_PLUS3, 16).
-define(INTERNAL4_ARITY_MINUS3, 10).

-define(INTERNAL4_C1(UpdatedC1),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, UpdatedC1, C2, C3, C4, C5)
).
-define(INTERNAL4_C2(UpdatedC2),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_C3(UpdatedC3),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_C4(UpdatedC4),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_C5(UpdatedC5),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, UpdatedC5)
).

-define(INTERNAL4_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL4_C1(K1, K2, K3, K4, V1, V2, V3, V4, UpdatedC1, C2, C3, C4, C5)
).
-define(INTERNAL4_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL4_C2(K1, K2, K3, K4, V1, V2, V3, V4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL4_C3(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_C4_REBALANCE(UpdatedC4),
    rebalance_INTERNAL4_C4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_C5_REBALANCE(UpdatedC5),
    rebalance_INTERNAL4_C5(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, UpdatedC5)
).

-define(INTERNAL4_ARGS_IGN_K1, _, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K2, K1, _, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K3, K1, K2, _, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K4, K1, K2, K3, _, V1, V2, V3, V4, C1, C2, C3, C4, C5).

-define(INTERNAL4_ARGS_IGN_K1_V1, _, K2, K3, K4, _, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K2_V2, K1, _, K3, K4, V1, _, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K3_V3, K1, K2, _, K4, V1, V2, _, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K4_V4, K1, K2, K3, _, V1, V2, V3, _, C1, C2, C3, C4, C5).

%% ?INTERNAL3

-define(INTERNAL3_ARGS, K1, K2, K3, V1, V2, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARITY, 10).
-define(INTERNAL3_ARITY_PLUS1, 11).
-define(INTERNAL3_ARITY_PLUS2, 12).
-define(INTERNAL3_ARITY_PLUS3, 13).
-define(INTERNAL3_ARITY_MINUS3, 7).

-define(INTERNAL3_C1(UpdatedC1), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, UpdatedC1, C2, C3, C4)).
-define(INTERNAL3_C2(UpdatedC2), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, UpdatedC2, C3, C4)).
-define(INTERNAL3_C3(UpdatedC3), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, UpdatedC3, C4)).
-define(INTERNAL3_C4(UpdatedC4), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, UpdatedC4)).

-define(INTERNAL3_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL3_C1(K1, K2, K3, V1, V2, V3, UpdatedC1, C2, C3, C4)
).
-define(INTERNAL3_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL3_C2(K1, K2, K3, V1, V2, V3, C1, UpdatedC2, C3, C4)
).
-define(INTERNAL3_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL3_C3(K1, K2, K3, V1, V2, V3, C1, C2, UpdatedC3, C4)
).
-define(INTERNAL3_C4_REBALANCE(UpdatedC4),
    rebalance_INTERNAL3_C4(K1, K2, K3, V1, V2, V3, C1, C2, C3, UpdatedC4)
).

-define(INTERNAL3_ARGS_IGN_K1, _, K2, K3, V1, V2, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_K2, K1, _, K3, V1, V2, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_K3, K1, K2, _, V1, V2, V3, C1, C2, C3, C4).

-define(INTERNAL3_ARGS_IGN_K1_V1, _, K2, K3, _, V2, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_K2_V2, K1, _, K3, V1, _, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_K3_V3, K1, K2, _, V1, V2, _, C1, C2, C3, C4).

%% ?INTERNAL2

-define(INTERNAL2_ARGS, K1, K2, V1, V2, C1, C2, C3).
-define(INTERNAL2_ARITY, 7).
-define(INTERNAL2_ARITY_PLUS1, 8).
-define(INTERNAL2_ARITY_PLUS2, 9).
-define(INTERNAL2_ARITY_PLUS3, 10).
-define(INTERNAL2_ARITY_MINUS3, 4).

-define(INTERNAL2_C1(UpdatedC1), ?new_INTERNAL2(K1, K2, V1, V2, UpdatedC1, C2, C3)).
-define(INTERNAL2_C2(UpdatedC2), ?new_INTERNAL2(K1, K2, V1, V2, C1, UpdatedC2, C3)).
-define(INTERNAL2_C3(UpdatedC3), ?new_INTERNAL2(K1, K2, V1, V2, C1, C2, UpdatedC3)).

-define(INTERNAL2_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL2_C1(K1, K2, V1, V2, UpdatedC1, C2, C3)
).
-define(INTERNAL2_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL2_C2(K1, K2, V1, V2, C1, UpdatedC2, C3)
).
-define(INTERNAL2_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL2_C3(K1, K2, V1, V2, C1, C2, UpdatedC3)
).

-define(INTERNAL2_ARGS_IGN_K1, _, K2, V1, V2, C1, C2, C3).
-define(INTERNAL2_ARGS_IGN_K2, K1, _, V1, V2, C1, C2, C3).

-define(INTERNAL2_ARGS_IGN_K1_V1, _, K2, _, V2, C1, C2, C3).
-define(INTERNAL2_ARGS_IGN_K2_V2, K1, _, V1, _, C1, C2, C3).

%% ?INTERNAL1

-define(INTERNAL1_ARGS, K1, V1, C1, C2).
-define(INTERNAL1_ARITY, 4).
-define(INTERNAL1_ARITY_PLUS1, 5).
-define(INTERNAL1_ARITY_PLUS2, 6).
-define(INTERNAL1_ARITY_PLUS3, 7).

-define(INTERNAL1_C1(UpdatedC1), ?new_INTERNAL1(K1, V1, UpdatedC1, C2)).
-define(INTERNAL1_C2(UpdatedC2), ?new_INTERNAL1(K1, V1, C1, UpdatedC2)).

-define(INTERNAL1_C1_REBALANCE(UpdatedC1), rebalance_INTERNAL1_C1(K1, V1, UpdatedC1, C2)).
-define(INTERNAL1_C2_REBALANCE(UpdatedC2), rebalance_INTERNAL1_C2(K1, V1, C1, UpdatedC2)).

-define(INTERNAL1_K1_C2_REBALANCE(ReplacementK, ReplacementV, UpdatedC2),
    rebalance_INTERNAL1_C2(ReplacementK, ReplacementV, C1, UpdatedC2)
).

-define(INTERNAL1_ARGS_IGN_K1, _, V1, C1, C2).
-define(INTERNAL1_ARGS_IGN_K1_V1, _, _, C1, C2).

%% ?LEAF4

-define(LEAF4_ARGS, K1, K2, K3, K4, V1, V2, V3, V4).
-define(LEAF4_ARITY, 8).
-define(LEAF4_ARITY_PLUS1, 9).
-define(LEAF4_ARITY_PLUS2, 10).
-define(LEAF4_ARITY_PLUS3, 11).

%% ?LEAF3

-define(LEAF3_ARGS, K1, K2, K3, V1, V2, V3).
-define(LEAF3_ARITY, 6).
-define(LEAF3_ARITY_PLUS1, 7).
-define(LEAF3_ARITY_PLUS2, 8).
-define(LEAF3_ARITY_PLUS3, 9).

%% ?LEAF2

-define(LEAF2_ARGS, K1, K2, V1, V2).
-define(LEAF2_ARITY, 4).
-define(LEAF2_ARITY_PLUS1, 5).
-define(LEAF2_ARITY_PLUS2, 6).
-define(LEAF2_ARITY_PLUS3, 7).

%% ?LEAF1

-define(LEAF1_ARGS, K1, V1).
-define(LEAF1_ARITY, 2).
-define(LEAF1_ARITY_PLUS1, 3).
-define(LEAF1_ARITY_PLUS2, 4).
-define(LEAF1_ARITY_PLUS3, 5).

%%

-define(TAKEN(Pair, UpdatedNode), [Pair | UpdatedNode]).

-define(TAKEN_PAIR(K, V, UpdatedNode), ?TAKEN([K | V], UpdatedNode)).

%%

% defined(TEST)).
-define(NODE_CHECK_ENABLED, false).

-if(?NODE_CHECK_ENABLED).
-define(CHECK_NODE(Node), check_node(?LINE, Node)).
-define(CHECK_NODE_RECUR(Node), check_node_recur(?LINE, Node)).
-else.
-define(CHECK_NODE(Node), Node).
-define(CHECK_NODE_RECUR(Node), Node).
-endif.

%

-define(new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5),
    ?CHECK_NODE_RECUR(?INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5))
).

-define(new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4),
    ?CHECK_NODE_RECUR(?INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4))
).

-define(new_INTERNAL2(K1, K2, V1, V2, C1, C2, C3),
    ?CHECK_NODE_RECUR(?INTERNAL2(K1, K2, V1, V2, C1, C2, C3))
).

-define(new_INTERNAL1(K1, V1, C1, C2), ?CHECK_NODE(?INTERNAL1(K1, V1, C1, C2))).

%

-define(new_LEAF4(K1, K2, K3, K4, V1, V2, V3, V4),
    ?CHECK_NODE_RECUR(?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4))
).

-define(new_LEAF3(K1, K2, K3, V1, V2, V3), ?CHECK_NODE_RECUR(?LEAF3(K1, K2, K3, V1, V2, V3))).

-define(new_LEAF2(K1, K2, V1, V2), ?CHECK_NODE_RECUR(?LEAF2(K1, K2, V1, V2))).

-define(new_LEAF1(K1, V1), ?CHECK_NODE(?LEAF1(K1, V1))).

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
    (node_INTERNAL1(Key, Value)
    | node_LEAF1(Key, Value)
    | empty_node()).

-type deep_node(Key, Value) ::
    (node_INTERNAL4(Key, Value)
    | node_INTERNAL3(Key, Value)
    | node_INTERNAL2(Key, Value)
    | node_LEAF4(Key, Value)
    | node_LEAF3(Key, Value)
    | node_LEAF2(Key, Value)).

%-type non_empty_node(Key, Value) ::
%    (node_INTERNAL1(Key, Value)
%    | node_LEAF1(Key, Value)
%    | deep_node(Key, Value)).

-type node_INTERNAL4(Key, Value) ::
    (?INTERNAL4(
        Key,
        Key,
        Key,
        Key,
        Value,
        Value,
        Value,
        Value,
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_INTERNAL3(Key, Value) ::
    (?INTERNAL3(
        Key,
        Key,
        Key,
        Value,
        Value,
        Value,
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_INTERNAL2(Key, Value) ::
    (?INTERNAL2(
        Key,
        Key,
        Value,
        Value,
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_INTERNAL1(Key, Value) ::
    (?INTERNAL1(
        Key,
        Value,
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_LEAF4(Key, Value) ::
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

-type node_LEAF3(Key, Value) ::
    (?LEAF3(
        Key,
        Key,
        Key,
        Value,
        Value,
        Value
    )).

-type node_LEAF2(Key, Value) :: ?LEAF2(Key, Key, Value, Value).

-type node_LEAF1(Key, Value) :: nonempty_improper_list(Key, Value).

%%%%%%%%%%%

% -type node_after_deletion(Key) ::
%     node_INTERNAL3(Key)
%     | node_INTERNAL2(Key)
%     | node_INTERNAL1(Key)
%     | node_LEAF2(Key)
%     | node_LEAF1(Key).
%
% -type deep_node_after_insertion(Key) ::
%     node_INTERNAL4(Key)
%     | node_INTERNAL3(Key)
%     | node_LEAF4(Key)
%     | node_LEAF3(Key).
%
% % Temporary situation before rebalance
% -type unbalanced_node(Key) :: node_INTERNAL1(Key).

%%%%%%%%%%%

% -type split_result(Key) :: split_internal_result(Key) | leaf_split_result(Key).
%
% -type split_internal_result(Key) :: split_result(
%     Key, node_INTERNAL2(Key), node_INTERNAL2(Key)
% ).
%
% -type leaf_split_result(Key) :: split_result(
%     Key, node_LEAF2(Key), node_LEAF2(Key)
% ).
%
% -type split_result(Key, SplitL, SplitR) :: ?SPLIT(Key, SplitL, SplitR).

%%%%%%%%%%%

% -opaque iter(Key) :: forward_iter(Key) | reverse_iter(Key).
% -export_type([iter/1]).
%
% -type forward_iter(Key) :: [iterator_step(Key)].
% -type reverse_iter(Key) :: nonempty_improper_list(reversed, [iterator_step(Key)]).

% -type iterator_step(Key) :: kv_pair(Key) | deep_node(Key).

%%%%%%%%%%%

%%%%%%%%%%%%

-record(stats_acc, {
    count_internal4,
    count_internal3,
    count_internal2,
    count_internal1,
    count_leaf4,
    count_leaf3,
    count_leaf2,
    count_leaf1,
    height
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

delete(Key, Root) ->
    delete_recur(Key, Root).

foldl(Fun, Acc, Root) ->
    foldl_recur(Fun, Acc, Root).

foldr(Fun, Acc, Root) ->
    foldr_recur(Fun, Acc, Root).

get(Key, Root) ->
    get_recur(Key, Root).

insert(Key, ValueEval, ValueWrap, Root) ->
    case insert_recur(Key, ValueEval, ValueWrap, Root) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            ?new_INTERNAL1(SplitK, SplitV, SplitL, SplitR);
        %
        UpdatedRoot ->
            UpdatedRoot
    end.

iterator(Root, ordered) ->
    fwd_iterator(Root);
iterator(Root, reversed) ->
    Acc = rev_iterator(Root),
    [?REV_ITER_TAG | Acc].

iterator_from(Key, Root, ordered) ->
    bound_fwd_iterator(Key, Root);
iterator_from(Key, Root, reversed) ->
    Acc = bound_rev_iterator(Key, Root),
    [?REV_ITER_TAG | Acc].

keys(Root) ->
    keys_recur(Root, []).

larger(Key, Root) ->
    larger_recur(Key, Root).

% TODO continue from here: we're moving root-only definitions so that they're allowed anywhere in the tree
largest(?INTERNAL1_MATCH(_, _, _, C2)) ->
    largest_recur(C2);
largest(?LEAF1_MATCH(K1, V1)) ->
    {K1, V1};
largest(?LEAF0_MATCH) ->
    error_empty_tree();
largest(Root) ->
    largest_recur(Root).

map(Fun, ?INTERNAL1_MATCH_ALL) ->
    ?new_INTERNAL1(
        K1,
        %
        Fun(K1, V1),
        %
        map_recur(Fun, C1),
        map_recur(Fun, C2)
    );
map(Fun, ?LEAF1_MATCH_ALL) ->
    ?new_LEAF1(K1, Fun(K1, V1));
map(_Fun, ?LEAF0_MATCH) ->
    ?LEAF0;
map(Fun, Root) ->
    map_recur(Fun, Root).

new() ->
    ?LEAF0.

next([Head | Tail]) ->
    next(Head, Tail);
next([]) ->
    none.

smaller(Key, ?INTERNAL1_MATCH_ALL) ->
    smaller_INTERNAL1(Key, ?INTERNAL1_ARGS);
smaller(Key, ?LEAF1_MATCH_ALL) ->
    smaller_LEAF1(Key, ?LEAF1_ARGS);
smaller(_Key, ?LEAF0_MATCH_ALL) ->
    none;
smaller(Key, Root) ->
    smaller_recur(Key, Root).

smallest(?INTERNAL1_MATCH(_, _, C1, _)) ->
    smallest_recur(C1);
smallest(?LEAF1_MATCH(K1, V1)) ->
    {K1, V1};
smallest(?LEAF0_MATCH) ->
    error_empty_tree();
smallest(Root) ->
    smallest_recur(Root).

structural_stats(Root) ->
    Acc = #stats_acc{
        count_internal4 = 0,
        count_internal3 = 0,
        count_internal2 = 0,
        count_internal1 = 0,
        count_leaf4 = 0,
        count_leaf3 = 0,
        count_leaf2 = 0,
        count_leaf1 = 0,
        height = 0
    },

    case Root of
        ?INTERNAL1_MATCH(_, _, C1, C2) ->
            Height = 1,
            Acc2 = structural_stats_inc(#stats_acc.count_internal1, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            return_structural_stats(Acc4);
        %
        ?LEAF1_MATCH(_, _) ->
            Height = 1,
            Acc2 = structural_stats_inc(#stats_acc.count_leaf1, Acc),
            Acc3 = structural_stats_set_height(Height, Acc2),
            return_structural_stats(Acc3);
        %
        ?LEAF0_MATCH ->
            return_structural_stats(Acc);
        %
        _ ->
            Height = 1,
            Acc2 = structural_stats_recur(Root, Acc, Height),
            return_structural_stats(Acc2)
    end.

take(Key, ?INTERNAL1_MATCH_ALL) ->
    take_INTERNAL1(Key, ?INTERNAL1_ARGS);
take(Key, ?LEAF1_MATCH_ALL) ->
    take_LEAF1(Key, ?LEAF1_ARGS);
take(Key, ?LEAF0_MATCH_ALL) ->
    error_badkey(Key);
take(Key, Root) ->
    take_recur(Key, Root).

take_largest(?INTERNAL1_MATCH_ALL) ->
    take_largest_INTERNAL1(?INTERNAL1_ARGS);
take_largest(?LEAF1_MATCH_ALL) ->
    take_largest_LEAF1(?LEAF1_ARGS);
take_largest(?LEAF0_MATCH_ALL) ->
    error_empty_tree();
take_largest(Root) ->
    take_largest_recur(Root).

take_smallest(?INTERNAL1_MATCH_ALL) ->
    take_smallest_INTERNAL1(?INTERNAL1_ARGS);
take_smallest(?LEAF1_MATCH_ALL) ->
    take_smallest_LEAF1(?LEAF1_ARGS);
take_smallest(?LEAF0_MATCH_ALL) ->
    error_empty_tree();
take_smallest(Root) ->
    take_smallest_recur(Root).

to_list(?INTERNAL1_MATCH_ALL) ->
    Acc2 = to_list_recur(C2, []),
    Acc3 = [{K1, V1} | Acc2],
    to_list_recur(C1, Acc3);
to_list(?LEAF1_MATCH_ALL) ->
    [{K1, V1}];
to_list(?LEAF0_MATCH_ALL) ->
    [];
to_list(Root) ->
    to_list_recur(Root, []).

update(Key, ValueEval, ValueWrap, ?INTERNAL1_MATCH_ALL) ->
    update_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
update(Key, ValueEval, ValueWrap, ?LEAF1_MATCH_ALL) ->
    update_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
update(Key, _ValueEval, _ValueWrap, ?LEAF0_MATCH_ALL) ->
    error_badkey(Key);
update(Key, ValueEval, ValueWrap, Root) ->
    update_recur(Key, ValueEval, ValueWrap, Root).

values(?INTERNAL1_MATCH(_, V1, C1, C2)) ->
    values_recur(C1, [V1 | values_recur(C2, [])]);
values(?LEAF1_MATCH(_, V1)) ->
    [V1];
values(?LEAF0_MATCH) ->
    [];
values(Root) ->
    values_recur(Root, []).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Exceptions
%% ------------------------------------------------------------------

-compile({inline, error_badkey/1}).
error_badkey(Key) ->
    error({badkey, Key}).

-compile({inline, error_empty_tree/0}).
error_empty_tree() ->
    error(empty_tree).

-compile({inline, error_key_exists/1}).
error_key_exists(Key) ->
    error({key_exists, Key}).

%% ------------------------------------------------------------------
%% Internal Function Definitions: delete/2
%% ------------------------------------------------------------------

delete_recur(Key, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            delete_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            delete_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            delete_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            delete_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            delete_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            delete_LEAF4(Key, ?LEAF4_ARGS);
        %
        %
        %
        ?INTERNAL1_MATCH_ALL ->
            delete_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF1_MATCH(K1, _) ->
            delete_LEAF1(Key, K1);
        %
        ?LEAF0_MATCH_ALL ->
            error_badkey(Key)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, delete_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            delete_INTERNAL4_C4(Key, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            delete_INTERNAL4_C3(Key, ?INTERNAL4_ARGS);
                        %
                        true ->
                            delete_INTERNAL4_K3(?INTERNAL4_ARGS)
                    end;
                %
                Key > K4 ->
                    delete_INTERNAL4_C5(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_INTERNAL4_K4(?INTERNAL4_ARGS)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    delete_INTERNAL4_C1(Key, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    delete_INTERNAL4_C2(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_INTERNAL4_K1(?INTERNAL4_ARGS)
            end;
        %
        true ->
            delete_INTERNAL4_K2(?INTERNAL4_ARGS)
    end.

-compile({inline, delete_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C1(Key, ?INTERNAL4_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),

    ?INTERNAL4_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C2(Key, ?INTERNAL4_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),

    ?INTERNAL4_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C3(Key, ?INTERNAL4_ARGS) ->
    UpdatedC3 = delete_recur(Key, C3),

    ?INTERNAL4_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C4(Key, ?INTERNAL4_ARGS) ->
    UpdatedC4 = delete_recur(Key, C4),

    ?INTERNAL4_C4_REBALANCE(UpdatedC4).

-compile({inline, delete_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C5(Key, ?INTERNAL4_ARGS) ->
    UpdatedC5 = delete_recur(Key, C5),

    ?INTERNAL4_C5_REBALANCE(UpdatedC5).

%%

-compile({inline, delete_INTERNAL4_K1 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_K1(?INTERNAL4_ARGS_IGN_K1_V1) ->
    % ?INTERNAL4_VALUES(_, V2, V3, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    rebalance_INTERNAL4_C2(
        ReplacementK,
        K2,
        K3,
        K4,
        %
        ReplacementV,
        V2,
        V3,
        V4,
        %
        C1,
        UpdatedC2,
        C3,
        C4,
        C5
    ).

-compile({inline, delete_INTERNAL4_K2 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_K2(?INTERNAL4_ARGS_IGN_K2_V2) ->
    % ?INTERNAL4_VALUES(V1, _, V3, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_largest_recur(C2),

    rebalance_INTERNAL4_C2(
        K1,
        ReplacementK,
        K3,
        K4,
        %
        V1,
        ReplacementV,
        V3,
        V4,
        %
        C1,
        UpdatedC2,
        C3,
        C4,
        C5
    ).

-compile({inline, delete_INTERNAL4_K3 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_K3(?INTERNAL4_ARGS_IGN_K3_V3) ->
    % ?INTERNAL4_VALUES(V1, V2, _, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_smallest_recur(C4),

    rebalance_INTERNAL4_C4(
        K1,
        K2,
        ReplacementK,
        K4,
        %
        V1,
        V2,
        ReplacementV,
        V4,
        %
        C1,
        C2,
        C3,
        UpdatedC4,
        C5
    ).

-compile({inline, delete_INTERNAL4_K4 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_K4(?INTERNAL4_ARGS_IGN_K4_V4) ->
    % ?INTERNAL4_VALUES(V1, V2, V3, _) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_largest_recur(C4),

    rebalance_INTERNAL4_C4(
        K1,
        K2,
        K3,
        ReplacementK,
        %
        V1,
        V2,
        V3,
        ReplacementV,
        %
        C1,
        C2,
        C3,
        UpdatedC4,
        C5
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, delete_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    delete_INTERNAL3_C1(Key, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    delete_INTERNAL3_C2(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_INTERNAL3_K1(?INTERNAL3_ARGS)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    delete_INTERNAL3_C3(Key, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    delete_INTERNAL3_C4(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_INTERNAL3_K3(?INTERNAL3_ARGS)
            end;
        %
        true ->
            delete_INTERNAL3_K2(?INTERNAL3_ARGS)
    end.

-compile({inline, delete_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C1(Key, ?INTERNAL3_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),

    ?INTERNAL3_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C2(Key, ?INTERNAL3_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),

    ?INTERNAL3_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C3(Key, ?INTERNAL3_ARGS) ->
    UpdatedC3 = delete_recur(Key, C3),

    ?INTERNAL3_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C4(Key, ?INTERNAL3_ARGS) ->
    UpdatedC4 = delete_recur(Key, C4),

    ?INTERNAL3_C4_REBALANCE(UpdatedC4).

%%

-compile({inline, delete_INTERNAL3_K1 / ?INTERNAL3_ARITY}).
delete_INTERNAL3_K1(?INTERNAL3_ARGS_IGN_K1_V1) ->
    % ?INTERNAL3_VALUES(_, V2, V3) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    rebalance_INTERNAL3_C2(
        ReplacementK,
        K2,
        K3,
        %
        ReplacementV,
        V2,
        V3,
        %
        C1,
        UpdatedC2,
        C3,
        C4
    ).

-compile({inline, delete_INTERNAL3_K2 / ?INTERNAL3_ARITY}).
delete_INTERNAL3_K2(?INTERNAL3_ARGS_IGN_K2_V2) ->
    % ?INTERNAL3_VALUES(V1, _, V3) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) = take_smallest_recur(C3),

    rebalance_INTERNAL3_C3(
        K1,
        ReplacementK,
        K3,
        %
        V1,
        ReplacementV,
        V3,
        %
        C1,
        C2,
        UpdatedC3,
        C4
    ).

-compile({inline, delete_INTERNAL3_K3 / ?INTERNAL3_ARITY}).
delete_INTERNAL3_K3(?INTERNAL3_ARGS_IGN_K3_V3) ->
    % ?INTERNAL3_VALUES(V1, V2, _) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) = take_largest_recur(C3),

    rebalance_INTERNAL3_C3(
        K1,
        K2,
        ReplacementK,
        %
        V1,
        V2,
        ReplacementV,
        %
        C1,
        C2,
        UpdatedC3,
        C4
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, delete_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    delete_INTERNAL2_C2(Key, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    delete_INTERNAL2_C3(Key, ?INTERNAL2_ARGS);
                %
                true ->
                    delete_INTERNAL2_K2(?INTERNAL2_ARGS)
            end;
        %
        Key < K1 ->
            delete_INTERNAL2_C1(Key, ?INTERNAL2_ARGS);
        %
        true ->
            delete_INTERNAL2_K1(?INTERNAL2_ARGS)
    end.

-compile({inline, delete_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2_C1(Key, ?INTERNAL2_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),

    ?INTERNAL2_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2_C2(Key, ?INTERNAL2_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),

    ?INTERNAL2_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2_C3(Key, ?INTERNAL2_ARGS) ->
    UpdatedC3 = delete_recur(Key, C3),

    ?INTERNAL2_C3_REBALANCE(UpdatedC3).

%%

-compile({inline, delete_INTERNAL2_K1 / ?INTERNAL2_ARITY}).
delete_INTERNAL2_K1(?INTERNAL2_ARGS_IGN_K1_V1) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    rebalance_INTERNAL2_C2(
        ReplacementK,
        K2,
        %
        ReplacementV,
        V2,
        %
        C1,
        UpdatedC2,
        C3
    ).

-compile({inline, delete_INTERNAL2_K2 / ?INTERNAL2_ARITY}).
delete_INTERNAL2_K2(?INTERNAL2_ARGS_IGN_K2_V2) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_largest_recur(C2),

    rebalance_INTERNAL2_C2(
        K1,
        ReplacementK,
        %
        V1,
        ReplacementV,
        %
        C1,
        UpdatedC2,
        C3
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, delete_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
delete_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            delete_INTERNAL1_C1(Key, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            delete_INTERNAL1_C2(Key, ?INTERNAL1_ARGS);
        %
        true ->
            delete_INTERNAL1_K1(?INTERNAL1_ARGS)
    end.

-compile({inline, delete_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
delete_INTERNAL1_C1(Key, ?INTERNAL1_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),
    ?INTERNAL1_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
delete_INTERNAL1_C2(Key, ?INTERNAL1_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),
    ?INTERNAL1_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL1_K1 / ?INTERNAL1_ARITY}).
delete_INTERNAL1_K1(?INTERNAL1_ARGS_IGN_K1_V1) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    ?INTERNAL1_K1_C2_REBALANCE(ReplacementK, ReplacementV, UpdatedC2).

%%
%% ?LEAF4
%%

-compile({inline, delete_LEAF4 / ?LEAF4_ARITY_PLUS1}).
delete_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key == K1 ->
            ?new_LEAF3(K2, K3, K4, V2, V3, V4);
        %
        Key == K2 ->
            ?new_LEAF3(K1, K3, K4, V1, V3, V4);
        %
        Key == K3 ->
            ?new_LEAF3(K1, K2, K4, V1, V2, V4);
        %
        Key == K4 ->
            ?new_LEAF3(K1, K2, K3, V1, V2, V3);
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF3
%%

-compile({inline, delete_LEAF3 / ?LEAF3_ARITY_PLUS1}).
delete_LEAF3(Key, ?LEAF3_ARGS) ->
    if
        Key == K1 ->
            ?new_LEAF2(K2, K3, V2, V3);
        %
        Key == K2 ->
            ?new_LEAF2(K1, K3, V1, V3);
        %
        Key == K3 ->
            ?new_LEAF2(K1, K2, V1, V2);
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF2
%%

-compile({inline, delete_LEAF2 / ?LEAF2_ARITY_PLUS1}).
delete_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key == K1 ->
            ?new_LEAF1(K2, V2);
        %
        Key == K2 ->
            ?new_LEAF1(K1, V1);
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF1
%%

-compile({inline, delete_LEAF1/2}).
delete_LEAF1(Key, K1) ->
    if
        Key == K1 ->
            ?LEAF0;
        %
        true ->
            error_badkey(Key)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: foldl/3
%% ------------------------------------------------------------------

foldl_recur(Fun, Acc, Node) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            Fun(K2, V2, Fun(K1, V1, Acc));
        %
        ?LEAF3_MATCH_ALL ->
            Fun(K3, V3, Fun(K2, V2, Fun(K1, V1, Acc)));
        %
        ?LEAF4_MATCH_ALL ->
            Fun(K4, V4, Fun(K3, V3, Fun(K2, V2, Fun(K1, V1, Acc))));
        %
        ?INTERNAL2_MATCH_ALL ->
            % ?INTERNAL2_VALUES_MATCH_ALL = Values,
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            _Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3);
        %
        ?INTERNAL3_MATCH_ALL ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3),
            _Acc5 = foldl_recur(Fun, Fun(K3, V3, Acc4), C4);
        %
        ?INTERNAL4_MATCH_ALL ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3),
            Acc5 = foldl_recur(Fun, Fun(K3, V3, Acc4), C4),
            _Acc6 = foldl_recur(Fun, Fun(K4, V4, Acc5), C5);
        %
        %
        %
        ?INTERNAL1_MATCH_ALL ->
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = Fun(K1, V1, Acc2),
            foldl_recur(Fun, Acc3, C2);
        %
        ?LEAF1_MATCH_ALL ->
            Fun(K1, V1, Acc);
        %
        ?LEAF0_MATCH_ALL ->
            Acc
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: foldr/3
%% ------------------------------------------------------------------

foldr_recur(Fun, Acc, Node) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            Fun(K1, V1, Fun(K2, V2, Acc));
        %
        ?LEAF3_MATCH_ALL ->
            Fun(K1, V1, Fun(K2, V2, Fun(K3, V3, Acc)));
        %
        ?LEAF4_MATCH_ALL ->
            Fun(K1, V1, Fun(K2, V2, Fun(K3, V3, Fun(K4, V4, Acc))));
        %
        ?INTERNAL2_MATCH_ALL ->
            % ?INTERNAL2_VALUES_MATCH_ALL = Values,
            Acc2 = foldr_recur(Fun, Acc, C3),
            Acc3 = foldr_recur(Fun, Fun(K2, V2, Acc2), C2),
            _Acc4 = foldr_recur(Fun, Fun(K1, V1, Acc3), C1);
        %
        ?INTERNAL3_MATCH_ALL ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,
            Acc2 = foldr_recur(Fun, Acc, C4),
            Acc3 = foldr_recur(Fun, Fun(K3, V3, Acc2), C3),
            Acc4 = foldr_recur(Fun, Fun(K2, V2, Acc3), C2),
            _Acc5 = foldr_recur(Fun, Fun(K1, V1, Acc4), C1);
        %
        ?INTERNAL4_MATCH_ALL ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = foldr_recur(Fun, Acc, C5),
            Acc3 = foldr_recur(Fun, Fun(K4, V4, Acc2), C4),
            Acc4 = foldr_recur(Fun, Fun(K3, V3, Acc3), C3),
            Acc5 = foldr_recur(Fun, Fun(K2, V2, Acc4), C2),
            _Acc6 = foldr_recur(Fun, Fun(K1, V1, Acc5), C1);
        %
        %
        %
        ?INTERNAL1_MATCH_ALL ->
            Acc2 = foldr_recur(Fun, Acc, C2),
            Acc3 = Fun(K1, V1, Acc2),
            foldr_recur(Fun, Acc3, C1);
        %
        ?LEAF1_MATCH_ALL ->
            Fun(K1, V1, Acc);
        %
        ?LEAF0_MATCH_ALL ->
            Acc
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: get/2
%% ------------------------------------------------------------------

get_recur(Key, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            get_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            get_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            get_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            get_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            get_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            get_LEAF4(Key, ?LEAF4_ARGS);
        %
        %
        %
        ?INTERNAL1_MATCH_ALL ->
            get_INTERNAL1(Key, ?INTERNAL1_ARGS);
        ?LEAF1_MATCH_ALL ->
            get_LEAF1(Key, ?LEAF1_ARGS);
        ?LEAF0_MATCH_ALL ->
            error_badkey(Key)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, get_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
get_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            get_recur(Key, C4);
                        %
                        Key < K3 ->
                            get_recur(Key, C3);
                        %
                        true ->
                            V3
                    end;
                %
                Key > K4 ->
                    get_recur(Key, C5);
                %
                true ->
                    V4
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    get_recur(Key, C1);
                %
                Key > K1 ->
                    get_recur(Key, C2);
                %
                true ->
                    V1
            end;
        %
        true ->
            V2
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, get_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
get_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    get_recur(Key, C1);
                %
                Key > K1 ->
                    get_recur(Key, C2);
                %
                true ->
                    V1
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    get_recur(Key, C3);
                %
                Key > K3 ->
                    get_recur(Key, C4);
                %
                true ->
                    V3
            end;
        %
        true ->
            V2
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, get_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
get_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    get_recur(Key, C2);
                %
                Key > K2 ->
                    get_recur(Key, C3);
                %
                true ->
                    V2
            end;
        %
        Key < K1 ->
            get_recur(Key, C1);
        %
        true ->
            V1
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, get_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
get_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            get_recur(Key, C1);
        %
        Key > K1 ->
            get_recur(Key, C2);
        %
        true ->
            V1
    end.

%%
%% ?LEAF4
%%

-compile({inline, get_LEAF4 / ?LEAF4_ARITY_PLUS1}).
get_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        Key == K2 ->
            V2;
        %
        Key == K3 ->
            V3;
        %
        Key == K4 ->
            V4;
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF3
%%

-compile({inline, get_LEAF3 / ?LEAF3_ARITY_PLUS1}).
get_LEAF3(Key, ?LEAF3_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        Key == K2 ->
            V2;
        %
        Key == K3 ->
            V3;
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF2
%%

-compile({inline, get_LEAF2 / ?LEAF2_ARITY_PLUS1}).
get_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        Key == K2 ->
            V2;
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF1
%%

-compile({inline, get_LEAF1 / ?LEAF1_ARITY_PLUS1}).
get_LEAF1(Key, ?LEAF1_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        true ->
            error_badkey(Key)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: insert/2
%% ------------------------------------------------------------------

insert_recur(Key, ValueEval, ValueWrap, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            insert_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            insert_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            insert_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            insert_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            insert_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            insert_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
        %
        %
        %
        ?INTERNAL1_MATCH_ALL ->
            insert_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        ?LEAF1_MATCH_ALL ->
            insert_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
        ?LEAF0_MATCH_ALL ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_LEAF1(Key, Value)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, insert_INTERNAL4 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            insert_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            insert_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key > K4 ->
                    insert_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    insert_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            split_internal(
                SplitK,
                K1,
                K2,
                K3,
                K4,
                %
                SplitV,
                V1,
                V2,
                V3,
                V4,
                %
                SplitL,
                SplitR,
                C2,
                C3,
                C4,
                C5
            );
        %
        UpdatedC1 ->
            ?INTERNAL4_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                SplitK,
                K2,
                K3,
                K4,
                %
                V1,
                SplitV,
                V2,
                V3,
                V4,
                %
                C1,
                SplitL,
                SplitR,
                C3,
                C4,
                C5
            );
        %
        UpdatedC2 ->
            ?INTERNAL4_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                K2,
                SplitK,
                K3,
                K4,
                %
                V1,
                V2,
                SplitV,
                V3,
                V4,
                %
                C1,
                C2,
                SplitL,
                SplitR,
                C4,
                C5
            );
        %
        UpdatedC3 ->
            ?INTERNAL4_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                K2,
                K3,
                SplitK,
                K4,
                %
                V1,
                V2,
                V3,
                SplitV,
                V4,
                %
                C1,
                C2,
                C3,
                SplitL,
                SplitR,
                C5
            );
        %
        UpdatedC4 ->
            ?INTERNAL4_C4(UpdatedC4)
    end.

-compile({inline, insert_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C5) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                K2,
                K3,
                K4,
                SplitK,
                %
                V1,
                V2,
                V3,
                V4,
                SplitV,
                %
                C1,
                C2,
                C3,
                C4,
                SplitL,
                SplitR
            );
        %
        UpdatedC5 ->
            ?INTERNAL4_C5(UpdatedC5)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, insert_INTERNAL3 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    insert_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    insert_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    insert_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL4(
                SplitK,
                K1,
                K2,
                K3,
                %
                SplitV,
                V1,
                V2,
                V3,
                %
                SplitL,
                SplitR,
                C2,
                C3,
                C4
            );
        %
        UpdatedC1 ->
            ?INTERNAL3_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL4(
                K1,
                SplitK,
                K2,
                K3,
                %
                V1,
                SplitV,
                V2,
                V3,
                %
                C1,
                SplitL,
                SplitR,
                C3,
                C4
            );
        %
        UpdatedC2 ->
            ?INTERNAL3_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL4(
                K1,
                K2,
                SplitK,
                K3,
                %
                V1,
                V2,
                SplitV,
                V3,
                %
                C1,
                C2,
                SplitL,
                SplitR,
                C4
            );
        %
        UpdatedC3 ->
            ?INTERNAL3_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL4(
                K1,
                K2,
                K3,
                SplitK,
                %
                V1,
                V2,
                V3,
                SplitV,
                %
                C1,
                C2,
                C3,
                SplitL,
                SplitR
            );
        %
        UpdatedC4 ->
            ?INTERNAL3_C4(UpdatedC4)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, insert_INTERNAL2 / ?INTERNAL2_ARITY_PLUS3}).
insert_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    insert_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    insert_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K1 ->
            insert_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS3}).
insert_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL2_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL3(
                SplitK,
                K1,
                K2,
                %
                SplitV,
                V1,
                V2,
                %
                SplitL,
                SplitR,
                C2,
                C3
            );
        %
        UpdatedC1 ->
            ?INTERNAL2_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS3}).
insert_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL2_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL3(
                K1,
                SplitK,
                K2,
                %
                V1,
                SplitV,
                V2,
                %
                C1,
                SplitL,
                SplitR,
                C3
            );
        %
        UpdatedC2 ->
            ?INTERNAL2_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS3}).
insert_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL2_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL3(
                K1,
                K2,
                SplitK,
                %
                V1,
                V2,
                SplitV,
                %
                C1,
                C2,
                SplitL,
                SplitR
            );
        %
        UpdatedC3 ->
            ?INTERNAL2_C3(UpdatedC3)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, insert_INTERNAL1 / ?INTERNAL1_ARITY_PLUS3}).
insert_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            insert_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            insert_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS3}).
insert_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            ?new_INTERNAL2(
                SplitK,
                K1,
                %
                SplitV,
                V1,
                %
                SplitL,
                SplitR,
                C2
            );
        %
        UpdatedC1 ->
            ?INTERNAL1_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS3}).
insert_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            ?new_INTERNAL2(
                K1,
                SplitK,
                %
                V1,
                SplitV,
                %
                C1,
                SplitL,
                SplitR
            );
        %
        UpdatedC2 ->
            ?INTERNAL1_C2(UpdatedC2)
    end.

%%
%% ?LEAF4
%%

-compile({inline, insert_LEAF4 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            insert_LEAF4_POS4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                        %
                        Key < K3 ->
                            insert_LEAF4_POS3(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key > K4 ->
                    insert_LEAF4_POS5(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_LEAF4_POS1(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                %
                Key > K1 ->
                    insert_LEAF4_POS2(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF4_POS1 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS1(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        Value,
        V1,
        V2,
        V3,
        V4
    ).

-compile({inline, insert_LEAF4_POS2 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS2(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        Key,
        K2,
        K3,
        K4,
        %
        V1,
        Value,
        V2,
        V3,
        V4
    ).

-compile({inline, insert_LEAF4_POS3 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS3(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        K2,
        Key,
        K3,
        K4,
        %
        V1,
        V2,
        Value,
        V3,
        V4
    ).

-compile({inline, insert_LEAF4_POS4 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        K2,
        K3,
        Key,
        K4,
        %
        V1,
        V2,
        V3,
        Value,
        V4
    ).

-compile({inline, insert_LEAF4_POS5 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS5(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        K2,
        K3,
        K4,
        Key,
        %
        V1,
        V2,
        V3,
        V4,
        Value
    ).

%%
%% ?LEAF3
%%

-compile({inline, insert_LEAF3 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_LEAF3_POS1(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
                %
                Key > K1 ->
                    insert_LEAF3_POS2(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    insert_LEAF3_POS3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
                %
                Key > K3 ->
                    insert_LEAF3_POS4(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF3_POS1 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3_POS1(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF4(
        Key,
        K1,
        K2,
        K3,
        %
        Value,
        V1,
        V2,
        V3
    ).

-compile({inline, insert_LEAF3_POS2 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3_POS2(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF4(
        K1,
        Key,
        K2,
        K3,
        %
        V1,
        Value,
        V2,
        V3
    ).

-compile({inline, insert_LEAF3_POS3 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3_POS3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF4(
        K1,
        K2,
        Key,
        K3,
        %
        V1,
        V2,
        Value,
        V3
    ).

-compile({inline, insert_LEAF3_POS4 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3_POS4(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF4(
        K1,
        K2,
        K3,
        Key,
        %
        V1,
        V2,
        V3,
        Value
    ).

%%
%% ?LEAF2
%%

-compile({inline, insert_LEAF2 / ?LEAF2_ARITY_PLUS3}).
insert_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    insert_LEAF2_POS2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
                %
                Key > K2 ->
                    insert_LEAF2_POS3(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K1 ->
            insert_LEAF2_POS1(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF2_POS1 / ?LEAF2_ARITY_PLUS3}).
insert_LEAF2_POS1(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF3(
        Key,
        K1,
        K2,
        %
        Value,
        V1,
        V2
    ).

-compile({inline, insert_LEAF2_POS2 / ?LEAF2_ARITY_PLUS3}).
insert_LEAF2_POS2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF3(
        K1,
        Key,
        K2,
        %
        V1,
        Value,
        V2
    ).

-compile({inline, insert_LEAF2_POS3 / ?LEAF2_ARITY_PLUS3}).
insert_LEAF2_POS3(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF3(
        K1,
        K2,
        Key,
        %
        V1,
        V2,
        Value
    ).

%%
%% ?LEAF1
%%

-compile({inline, insert_LEAF1 / ?LEAF1_ARITY_PLUS3}).
insert_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    if
        Key < K1 ->
            insert_LEAF1_POS1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
        %
        Key > K1 ->
            insert_LEAF1_POS2(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF1_POS1 / ?LEAF1_ARITY_PLUS3}).
insert_LEAF1_POS1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF2(Key, K1, Value, V1).

-compile({inline, insert_LEAF1_POS2 / ?LEAF1_ARITY_PLUS3}).
insert_LEAF1_POS2(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF2(K1, Key, V1, Value).

%%%

%-spec eval_insert_value(insertion_value_eval(), insertion_value_wrap(Value)) -> Value.
-compile({inline, eval_insert_value/2}).
eval_insert_value(Type, Wrap) ->
    case Type of
        eager ->
            Wrap;
        %
        lazy ->
            Wrap()
    end.

%%
%% Split
%%

-compile({inline, split_internal/16}).
split_internal(
    K1,
    K2,
    K3,
    K4,
    K5,
    %
    V1,
    V2,
    V3,
    V4,
    V5,
    %
    C1,
    C2,
    C3,
    C4,
    C5,
    C6
) ->
    SplitK = K3,
    SplitV = V3,

    SplitL = ?new_INTERNAL2(K1, K2, V1, V2, C1, C2, C3),
    SplitR = ?new_INTERNAL2(K4, K5, V4, V5, C4, C5, C6),

    ?SPLIT(SplitK, SplitV, SplitL, SplitR).

-compile({inline, split_leaf/10}).
split_leaf(
    K1,
    K2,
    K3,
    K4,
    K5,
    %
    V1,
    V2,
    V3,
    V4,
    V5
) ->
    SplitK = K3,
    SplitV = V3,

    SplitL = ?new_LEAF2(K1, K2, V1, V2),
    SplitR = ?new_LEAF2(K4, K5, V4, V5),

    ?SPLIT(SplitK, SplitV, SplitL, SplitR).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - forward
%% ------------------------------------------------------------------

fwd_iterator(Root) ->
    Acc = [],
    fwd_iterator_recur(Root, Acc).

fwd_iterator_recur(?LEAF2_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2) | Acc],
    Acc2;
fwd_iterator_recur(?LEAF3_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3) | Acc],
    Acc2;
fwd_iterator_recur(?LEAF4_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc],
    Acc2;
fwd_iterator_recur(?INTERNAL2_MATCH_ALL, Acc) ->
    %?INTERNAL2_VALUES_MATCH_ALL = Values,
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        C2,
        ?ITER_PAIR(K2, V2),
        C3
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?INTERNAL3_MATCH_ALL, Acc) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        C2,
        ?ITER_PAIR(K2, V2),
        C3,
        ?ITER_PAIR(K3, V3),
        C4
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?INTERNAL4_MATCH_ALL, Acc) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        C2,
        ?ITER_PAIR(K2, V2),
        C3,
        ?ITER_PAIR(K3, V3),
        C4,
        ?ITER_PAIR(K4, V4),
        C5
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
%
%
%
fwd_iterator_recur(?INTERNAL1_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K1, V1), C2 | Acc],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?LEAF1_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K1, V1) | Acc],
    Acc2;
fwd_iterator_recur(?LEAF0_MATCH, Acc) ->
    Acc.

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - reverse
%% ------------------------------------------------------------------

rev_iterator(Root) ->
    Acc = [],
    rev_iterator_recur(Root, Acc).

rev_iterator_recur(?LEAF2_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc],
    Acc2;
rev_iterator_recur(?LEAF3_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc],
    Acc2;
rev_iterator_recur(?LEAF4_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K4, V4), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc],
    Acc2;
rev_iterator_recur(?INTERNAL2_MATCH_ALL, Acc) ->
    %?INTERNAL2_VALUES_MATCH_ALL = Values,
    Acc2 = [
        ?ITER_PAIR(K2, V2),
        C2,
        ?ITER_PAIR(K1, V1),
        C1
        | Acc
    ],
    rev_iterator_recur(C3, Acc2);
rev_iterator_recur(?INTERNAL3_MATCH_ALL, Acc) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,
    Acc2 = [
        ?ITER_PAIR(K3, V3),
        C3,
        ?ITER_PAIR(K2, V2),
        C2,
        ?ITER_PAIR(K1, V1),
        C1
        | Acc
    ],
    rev_iterator_recur(C4, Acc2);
rev_iterator_recur(?INTERNAL4_MATCH_ALL, Acc) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,
    Acc2 = [
        ?ITER_PAIR(K4, V4),
        C4,
        ?ITER_PAIR(K3, V3),
        C3,
        ?ITER_PAIR(K2, V2),
        C2,
        ?ITER_PAIR(K1, V1),
        C1
        | Acc
    ],
    rev_iterator_recur(C5, Acc2);
%
%
%
rev_iterator_recur(?INTERNAL1_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
    rev_iterator_recur(C2, Acc2);
rev_iterator_recur(?LEAF1_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K1, V1) | Acc],
    Acc2;
rev_iterator_recur(?LEAF0_MATCH, Acc) ->
    Acc.

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_fwd_iterator(Key, Root) ->
    Acc = [],
    bound_fwd_iterator_recur(Key, Root, Acc).

bound_fwd_iterator_recur(Key, Node, Acc) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            bound_fwd_iterator_LEAF2(Key, ?LEAF2_ARGS, Acc);
        %
        ?LEAF3_MATCH_ALL ->
            bound_fwd_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            bound_fwd_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc);
        %
        ?INTERNAL2_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc);
        %
        %
        %
        ?INTERNAL1_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS, Acc);
        %
        ?LEAF1_MATCH_ALL ->
            bound_fwd_iterator_LEAF1(Key, ?LEAF1_ARGS, Acc);
        %
        ?LEAF0_MATCH ->
            Acc
    end.

%% INTERNAL4

-compile({inline, bound_fwd_iterator_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc) ->
    if
        Key > K4 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Key, C5, Acc)
            end;
        %
        Key > K3 ->
            Acc2 = [?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C4, Acc2);
        %
        Key > K2 ->
            % ?INTERNAL4_VALUES(_, _, V3, V4) = Values,
            Acc2 = [?ITER_PAIR(K3, V3), C4, ?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C3, Acc2);
        %
        Key > K1 ->
            % ?INTERNAL4_VALUES(_, V2, V3, V4) = Values,
            Acc2 = [?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4, ?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2);
        %
        true ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = [
                ?ITER_PAIR(K1, V1),
                C2,
                ?ITER_PAIR(K2, V2),
                C3,
                ?ITER_PAIR(K3, V3),
                C4,
                ?ITER_PAIR(K4, V4),
                C5
                | Acc
            ],
            bound_fwd_iterator_recur(Key, C1, Acc2)
    end.

%% INTERNAL3

-compile({inline, bound_fwd_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc) ->
    if
        Key > K3 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Key, C4, Acc)
            end;
        %
        Key > K2 ->
            Acc2 = [?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C3, Acc2);
        %
        Key > K1 ->
            % ?INTERNAL3_VALUES(_, V2, V3) = Values,
            Acc2 = [?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2);
        %
        true ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,
            Acc2 = [?ITER_PAIR(K1, V1), C2, ?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C1, Acc2)
    end.

%% INTERNAL2

-compile({inline, bound_fwd_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS, Acc) ->
    if
        Key > K2 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Key, C3, Acc)
            end;
        %
        Key > K1 ->
            Acc2 = [?ITER_PAIR(K2, V2), C3 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2);
        %
        true ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,
            Acc2 = [?ITER_PAIR(K1, V1), C2, ?ITER_PAIR(K2, V2), C3 | Acc],
            bound_fwd_iterator_recur(Key, C1, Acc2)
    end.

%% INTERNAL1

-compile({inline, bound_fwd_iterator_INTERNAL1 / ?INTERNAL1_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS, Acc) ->
    if
        Key > K1 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Key, C2, Acc)
            end;
        %
        true ->
            Acc2 = [?ITER_PAIR(K1, V1), C2 | Acc],
            bound_fwd_iterator_recur(Key, C1, Acc2)
    end.

%% LEAF4

-compile({inline, bound_fwd_iterator_LEAF4 / ?LEAF4_ARITY_PLUS2}).
bound_fwd_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc) ->
    if
        Key > K4 ->
            Acc;
        %
        Key > K3 ->
            [?ITER_PAIR(K4, V4) | Acc];
        %
        Key > K2 ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc];
        %
        Key > K1 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc];
        %
        true ->
            [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc]
    end.

%% LEAF3

-compile({inline, bound_fwd_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_fwd_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc) ->
    if
        Key > K3 ->
            Acc;
        %
        Key > K2 ->
            [?ITER_PAIR(K3, V3) | Acc];
        %
        Key > K1 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3) | Acc];
        %
        true ->
            [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3) | Acc]
    end.

%% LEAF2

-compile({inline, bound_fwd_iterator_LEAF2 / ?LEAF2_ARITY_PLUS2}).
bound_fwd_iterator_LEAF2(Key, ?LEAF2_ARGS, Acc) ->
    if
        Key > K2 ->
            Acc;
        %
        Key > K1 ->
            [?ITER_PAIR(K2, V2) | Acc];
        %
        true ->
            [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2) | Acc]
    end.

%% LEAF1

-compile({inline, bound_fwd_iterator_LEAF1 / ?LEAF1_ARITY_PLUS2}).
bound_fwd_iterator_LEAF1(Key, ?LEAF1_ARGS, Acc) ->
    if
        Key > K1 ->
            Acc;
        %
        true ->
            [?ITER_PAIR(K1, V1) | Acc]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_rev_iterator(Key, Root) ->
    Acc = [],
    bound_rev_iterator_recur(Key, Root, Acc).

bound_rev_iterator_recur(Key, Node, Acc) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            bound_rev_iterator_LEAF2(Key, ?LEAF2_ARGS, Acc);
        %
        ?LEAF3_MATCH_ALL ->
            bound_rev_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            bound_rev_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc);
        %
        ?INTERNAL2_MATCH_ALL ->
            bound_rev_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            bound_rev_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            bound_rev_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc);
        %
        %
        %
        ?INTERNAL1_MATCH_ALL ->
            bound_rev_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS, Acc);
        %
        ?LEAF1_MATCH_ALL ->
            bound_rev_iterator_LEAF1(Key, ?LEAF1_ARGS, Acc);
        %
        ?LEAF0_MATCH ->
            Acc
    end.

%% INTERNAL4

-compile({inline, bound_rev_iterator_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc) ->
    if
        Key < K1 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Key, C1, Acc)
            end;
        %
        Key < K2 ->
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2);
        %
        Key < K3 ->
            % ?INTERNAL4_VALUES(V1, V2, _, _) = Values,
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2);
        %
        Key < K4 ->
            % ?INTERNAL4_VALUES(V1, V2, V3, _) = Values,
            Acc2 = [?ITER_PAIR(K3, V3), C3, ?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C4, Acc2);
        %
        true ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = [
                ?ITER_PAIR(K4, V4),
                C4,
                ?ITER_PAIR(K3, V3),
                C3,
                ?ITER_PAIR(K2, V2),
                C2,
                ?ITER_PAIR(K1, V1),
                C1
                | Acc
            ],
            bound_rev_iterator_recur(Key, C5, Acc2)
    end.

%% INTERNAL3

-compile({inline, bound_rev_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc) ->
    if
        Key < K1 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Key, C1, Acc)
            end;
        %
        Key < K2 ->
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2);
        %
        Key < K3 ->
            % ?INTERNAL3_VALUES(V1, V2, _) = Values,
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2);
        %
        true ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,
            Acc2 = [?ITER_PAIR(K3, V3), C3, ?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C4, Acc2)
    end.

%% INTERNAL2

-compile({inline, bound_rev_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS, Acc) ->
    if
        Key < K1 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Key, C1, Acc)
            end;
        %
        Key < K2 ->
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2);
        %
        true ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2)
    end.

%% INTERNAL1

-compile({inline, bound_rev_iterator_INTERNAL1 / ?INTERNAL1_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS, Acc) ->
    if
        Key < K1 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Key, C1, Acc)
            end;
        %
        true ->
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2)
    end.

%% LEAF4

-compile({inline, bound_rev_iterator_LEAF4 / ?LEAF4_ARITY_PLUS2}).
bound_rev_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc) ->
    if
        Key < K1 ->
            Acc;
        %
        Key < K2 ->
            [?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K3 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K4 ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        true ->
            [?ITER_PAIR(K4, V4), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc]
    end.

%% LEAF3

-compile({inline, bound_rev_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_rev_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc) ->
    if
        Key < K1 ->
            Acc;
        %
        Key < K2 ->
            [?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K3 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        true ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc]
    end.

%% LEAF2

-compile({inline, bound_rev_iterator_LEAF2 / ?LEAF2_ARITY_PLUS2}).
bound_rev_iterator_LEAF2(Key, ?LEAF2_ARGS, Acc) ->
    if
        Key < K1 ->
            Acc;
        %
        Key < K2 ->
            [?ITER_PAIR(K1, V1) | Acc];
        %
        true ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc]
    end.

%% LEAF1

-compile({inline, bound_rev_iterator_LEAF1 / ?LEAF1_ARITY_PLUS2}).
bound_rev_iterator_LEAF1(Key, ?LEAF1_ARGS, Acc) ->
    if
        Key < K1 ->
            Acc;
        %
        true ->
            [?ITER_PAIR(K1, V1) | Acc]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: keys/1
%% ------------------------------------------------------------------

keys_recur(Node, Acc) ->
    case Node of
        ?LEAF2_MATCH(K1, K2, _, _) ->
            [K1, K2 | Acc];
        %
        ?LEAF3_MATCH(K1, K2, K3, _, _, _) ->
            [K1, K2, K3 | Acc];
        %
        ?LEAF4_MATCH(K1, K2, K3, K4, _, _, _, _) ->
            [K1, K2, K3, K4 | Acc];
        %
        ?INTERNAL2_MATCH(K1, K2, _, _, C1, C2, C3) ->
            Acc2 = [K2 | keys_recur(C3, Acc)],
            Acc3 = [K1 | keys_recur(C2, Acc2)],
            keys_recur(C1, Acc3);
        %
        ?INTERNAL3_MATCH(K1, K2, K3, _, _, _, C1, C2, C3, C4) ->
            Acc2 = [K3 | keys_recur(C4, Acc)],
            Acc3 = [K2 | keys_recur(C3, Acc2)],
            Acc4 = [K1 | keys_recur(C2, Acc3)],
            keys_recur(C1, Acc4);
        %
        ?INTERNAL4_MATCH(K1, K2, K3, K4, _, _, _, _, C1, C2, C3, C4, C5) ->
            Acc2 = [K4 | keys_recur(C5, Acc)],
            Acc3 = [K3 | keys_recur(C4, Acc2)],
            Acc4 = [K2 | keys_recur(C3, Acc3)],
            Acc5 = [K1 | keys_recur(C2, Acc4)],
            keys_recur(C1, Acc5);
        %
        %
        %
        ?INTERNAL1_MATCH(K1, _, C1, C2) ->
            Acc2 = [K1 | keys_recur(C2, Acc)],
            keys_recur(C1, Acc2);
        %
        ?LEAF1_MATCH(K1, _) ->
            [K1 | Acc];
        %
        ?LEAF0_MATCH ->
            Acc
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: larger/2
%% ------------------------------------------------------------------

larger_recur(Key, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            larger_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            larger_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            larger_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            larger_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            larger_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            larger_LEAF4(Key, ?LEAF4_ARGS);
        %
        %
        %
        ?INTERNAL1_MATCH_ALL ->
            larger_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            larger_LEAF1(Key, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH_ALL ->
            none
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, larger_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
larger_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key < K2 ->
            case Key < K1 of
                true ->
                    case larger_recur(Key, C1) of
                        none -> {K1, V1};
                        Found -> Found
                    end;
                _ ->
                    case larger_recur(Key, C2) of
                        none -> {K2, V2};
                        Found -> Found
                    end
            end;
        %
        Key < K3 ->
            case larger_recur(Key, C3) of
                none -> {K3, V3};
                Found -> Found
            end;
        %
        Key < K4 ->
            case larger_recur(Key, C4) of
                none -> {K4, V4};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C5)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, larger_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
larger_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            case Key < K1 of
                true ->
                    case larger_recur(Key, C1) of
                        none -> {K1, V1};
                        Found -> Found
                    end;
                _ ->
                    case larger_recur(Key, C2) of
                        none -> {K2, V2};
                        Found -> Found
                    end
            end;
        %
        Key < K3 ->
            case larger_recur(Key, C3) of
                none -> {K3, V3};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C4)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, larger_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
larger_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key < K1 ->
            case larger_recur(Key, C1) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        Key < K2 ->
            case larger_recur(Key, C2) of
                none -> {K2, V2};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C3)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, larger_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
larger_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            case larger_recur(Key, C1) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C2)
    end.

%%
%% ?LEAF4
%%

-compile({inline, larger_LEAF4 / ?LEAF4_ARITY_PLUS1}).
larger_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key < K2 ->
            case Key < K1 of
                true ->
                    {K1, V1};
                _ ->
                    {K2, V2}
            end;
        %
        Key < K3 ->
            {K3, V3};
        %
        Key < K4 ->
            {K4, V4};
        %
        true ->
            none
    end.

%%
%% ?LEAF3
%%

-compile({inline, larger_LEAF3 / ?LEAF3_ARITY_PLUS1}).
larger_LEAF3(Key, ?LEAF3_ARGS) ->
    if
        Key < K2 ->
            case Key < K1 of
                true ->
                    {K1, V1};
                _ ->
                    {K2, V2}
            end;
        %
        Key < K3 ->
            {K3, V3};
        %
        true ->
            none
    end.

%%
%% ?LEAF2
%%

-compile({inline, larger_LEAF2 / ?LEAF2_ARITY_PLUS1}).
larger_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key < K1 ->
            {K1, V1};
        %
        Key < K2 ->
            {K2, V2};
        %
        true ->
            none
    end.

%%
%% ?LEAF1
%%

-compile({inline, larger_LEAF1 / ?LEAF1_ARITY_PLUS1}).
larger_LEAF1(Key, ?LEAF1_ARGS) ->
    case Key < K1 of
        true ->
            {K1, V1};
        %
        _ ->
            none
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: largest/1
%% ------------------------------------------------------------------

largest_recur(Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH(_, _, _, _, _, _, C3) ->
            largest_recur(C3);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, _, _, _, C4) ->
            largest_recur(C4);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, _, _, _, _, C5) ->
            largest_recur(C5);
        %
        ?LEAF2_MATCH(_, K2, _, V2) ->
            {K2, V2};
        %
        ?LEAF3_MATCH(_, _, K3, _, _, V3) ->
            {K3, V3};
        %
        ?LEAF4_MATCH(_, _, _, K4, _, _, _, V4) ->
            {K4, V4}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: map/2
%% ------------------------------------------------------------------

map_recur(Fun, Node) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            ?new_LEAF2(
                K1,
                K2,
                %
                Fun(K1, V1),
                Fun(K2, V2)
            );
        %
        %
        ?LEAF3_MATCH_ALL ->
            ?new_LEAF3(
                K1,
                K2,
                K3,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3)
            );
        %
        %
        ?LEAF4_MATCH_ALL ->
            ?new_LEAF4(
                K1,
                K2,
                K3,
                K4,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                Fun(K4, V4)
            );
        %
        %
        ?INTERNAL2_MATCH_ALL ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL2(
                K1,
                K2,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                %
                map_recur(Fun, C1),
                map_recur(Fun, C2),
                map_recur(Fun, C3)
            );
        %
        %
        ?INTERNAL3_MATCH_ALL ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL3(
                K1,
                K2,
                K3,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                %
                map_recur(Fun, C1),
                map_recur(Fun, C2),
                map_recur(Fun, C3),
                map_recur(Fun, C4)
            );
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                Fun(K4, V4),
                %
                map_recur(Fun, C1),
                map_recur(Fun, C2),
                map_recur(Fun, C3),
                map_recur(Fun, C4),
                map_recur(Fun, C5)
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: next/1
%% ------------------------------------------------------------------

next(?REV_ITER_TAG, Tail) ->
    rev_next(Tail);
next(Head, Tail) ->
    fwd_next(Head, Tail).

fwd_next(Head, Tail) ->
    case Head of
        ?ITER_PAIR(Key, Value) ->
            Iter2 = Tail,
            {Key, Value, Iter2};
        %
        Node ->
            [?ITER_PAIR(Key, Value) | NewTail] = fwd_iterator_recur(Node, Tail),
            Iter2 = NewTail,
            {Key, Value, Iter2}
    end.

rev_next([Head | Tail]) ->
    rev_next(Head, Tail);
rev_next([]) ->
    none.

rev_next(Head, Tail) ->
    case Head of
        ?ITER_PAIR(Key, Value) ->
            Iter2 = [?REV_ITER_TAG | Tail],
            {Key, Value, Iter2};
        %
        Node ->
            [?ITER_PAIR(Key, Value) | NewTail] = rev_iterator_recur(Node, Tail),
            Iter2 = [?REV_ITER_TAG | NewTail],
            {Key, Value, Iter2}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: smaller/2
%% ------------------------------------------------------------------

smaller_recur(Key, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            smaller_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            smaller_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            smaller_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            smaller_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            smaller_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            smaller_LEAF4(Key, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, smaller_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
smaller_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K3 ->
            case Key > K4 of
                true ->
                    case smaller_recur(Key, C5) of
                        none -> {K4, V4};
                        Found -> Found
                    end;
                _ ->
                    case smaller_recur(Key, C4) of
                        none -> {K3, V3};
                        Found -> Found
                    end
            end;
        %
        Key > K2 ->
            case smaller_recur(Key, C3) of
                none -> {K2, V2};
                Found -> Found
            end;
        %
        Key > K1 ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Key, C1)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, smaller_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
smaller_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key > K2 ->
            case Key > K3 of
                true ->
                    case smaller_recur(Key, C4) of
                        none -> {K3, V3};
                        Found -> Found
                    end;
                _ ->
                    case smaller_recur(Key, C3) of
                        none -> {K2, V2};
                        Found -> Found
                    end
            end;
        %
        Key > K1 ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Key, C1)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, smaller_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
smaller_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K2 ->
            case smaller_recur(Key, C3) of
                none -> {K2, V2};
                Found -> Found
            end;
        %
        Key > K1 ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Key, C1)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, smaller_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
smaller_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    case Key > K1 of
        true ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        _ ->
            smaller_recur(Key, C1)
    end.

%%
%% ?LEAF4
%%

-compile({inline, smaller_LEAF4 / ?LEAF4_ARITY_PLUS1}).
smaller_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key > K3 ->
            case Key > K4 of
                true ->
                    {K4, V4};
                _ ->
                    {K3, V3}
            end;
        %
        Key > K2 ->
            {K2, V2};
        %
        Key > K1 ->
            {K1, V1};
        %
        true ->
            none
    end.

%%
%% ?LEAF3
%%

-compile({inline, smaller_LEAF3 / ?LEAF3_ARITY_PLUS1}).
smaller_LEAF3(Key, ?LEAF3_ARGS) ->
    if
        Key > K2 ->
            case Key > K3 of
                true ->
                    {K3, V3};
                _ ->
                    {K2, V2}
            end;
        %
        Key > K1 ->
            {K1, V1};
        %
        true ->
            none
    end.

%%
%% ?LEAF2
%%

-compile({inline, smaller_LEAF2 / ?LEAF2_ARITY_PLUS1}).
smaller_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key > K2 ->
            {K2, V2};
        %
        Key > K1 ->
            {K1, V1};
        %
        true ->
            none
    end.

%%
%% ?LEAF1
%%

-compile({inline, smaller_LEAF1 / ?LEAF1_ARITY_PLUS1}).
smaller_LEAF1(Key, ?LEAF1_ARGS) ->
    case Key > K1 of
        true ->
            {K1, V1};
        %
        _ ->
            none
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: smallest/1
%% ------------------------------------------------------------------

smallest_recur(Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH(_, _, _, _, C1, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, C1, _, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, C1, _, _, _, _) ->
            smallest_recur(C1);
        %
        ?LEAF2_MATCH(K1, _, V1, _) ->
            {K1, V1};
        %
        ?LEAF3_MATCH(K1, _, _, V1, _, _) ->
            {K1, V1};
        %
        ?LEAF4_MATCH(K1, _, _, _, V1, _, _, _) ->
            {K1, V1}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: structural_stats/1
%% ------------------------------------------------------------------

structural_stats_recur(Node, Acc, Height) ->
    case Node of
        ?LEAF2_MATCH(_, _, _, _) ->
            Acc2 = structural_stats_inc(#stats_acc.count_leaf2, Acc),
            structural_stats_set_height(Height, Acc2);
        %
        ?LEAF3_MATCH(_, _, _, _, _, _) ->
            Acc2 = structural_stats_inc(#stats_acc.count_leaf3, Acc),
            structural_stats_set_height(Height, Acc2);
        %
        ?LEAF4_MATCH(_, _, _, _, _, _, _, _) ->
            Acc2 = structural_stats_inc(#stats_acc.count_leaf4, Acc),
            structural_stats_set_height(Height, Acc2);
        %
        ?INTERNAL2_MATCH(_, _, _, _, C1, C2, C3) ->
            Acc2 = structural_stats_inc(#stats_acc.count_internal2, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            _Acc5 = structural_stats_recur(C3, Acc4, Height + 1);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, C1, C2, C3, C4) ->
            Acc2 = structural_stats_inc(#stats_acc.count_internal3, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            _Acc6 = structural_stats_recur(C4, Acc5, Height + 1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, C1, C2, C3, C4, C5) ->
            Acc2 = structural_stats_inc(#stats_acc.count_internal4, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            Acc6 = structural_stats_recur(C4, Acc5, Height + 1),
            _Acc7 = structural_stats_recur(C5, Acc6, Height + 1)
    end.

structural_stats_set_height(Height, #stats_acc{height = RecordHeight} = Acc) ->
    case RecordHeight of
        _ when RecordHeight < Height ->
            Acc#stats_acc{height = Height};
        %
        _ when RecordHeight =:= Height ->
            Acc
    end.

-compile({inline, structural_stats_inc/2}).
structural_stats_inc(Pos, #stats_acc{} = Acc) ->
    setelement(Pos, Acc, element(Pos, Acc) + 1).

return_structural_stats(#stats_acc{} = Acc) ->
    NodeCounts = node_counts(Acc),
    NodePercentages = node_percentages(NodeCounts),
    TotalKeys = total_keys(NodeCounts),
    KeyPercentages = key_percentages(NodeCounts, TotalKeys),

    [
        {height, Acc#stats_acc.height},
        {node_counts, NodeCounts},
        {node_percentages, NodePercentages},
        {total_keys, TotalKeys},
        {key_percentages, KeyPercentages}
    ].

node_counts(#stats_acc{} = Acc) ->
    [
        {internal4, Acc#stats_acc.count_internal4},
        {internal3, Acc#stats_acc.count_internal3},
        {internal2, Acc#stats_acc.count_internal2},
        {internal1, Acc#stats_acc.count_internal1},
        {leaf4, Acc#stats_acc.count_leaf4},
        {leaf3, Acc#stats_acc.count_leaf3},
        {leaf2, Acc#stats_acc.count_leaf2},
        {leaf1, Acc#stats_acc.count_leaf1}
    ].

node_percentages(NodeCounts) ->
    Sum = lists:foldl(
        fun({_Type, Count}, Acc) ->
            Acc + Count
        end,
        0,
        NodeCounts
    ),

    %%%

    case Sum of
        0 ->
            lists:map(fun({Type, _Count}) -> {Type, 0.0} end, NodeCounts);
        %
        TotalCount ->
            lists:map(
                fun({Type, Count}) ->
                    {Type, round_percentage(100.0 * Count / TotalCount)}
                end,
                NodeCounts
            )
    end.

total_keys(NodeCounts) ->
    lists:foldl(
        fun({NodeType, Count}, Acc) ->
            Acc + (Count * total_keys_in_node_type(NodeType))
        end,
        0,
        NodeCounts
    ).

key_percentages(NodeCounts, 0 = _TotalKeys) ->
    lists:map(
        fun({NodeType, 0}) ->
            {NodeType, 0.0}
        end,
        NodeCounts
    );
key_percentages(NodeCounts, TotalKeys) ->
    lists:map(
        fun({NodeType, Count}) ->
            TotalNodeTypeKeys = Count * total_keys_in_node_type(NodeType),
            {NodeType, round_percentage(100.0 * TotalNodeTypeKeys / TotalKeys)}
        end,
        NodeCounts
    ).

round_percentage(Percentage) ->
    binary_to_float(float_to_binary(Percentage, [{decimals, 1}])).

total_keys_in_node_type(internal4) -> 4;
total_keys_in_node_type(internal3) -> 3;
total_keys_in_node_type(internal2) -> 2;
total_keys_in_node_type(internal1) -> 1;
total_keys_in_node_type(leaf4) -> 4;
total_keys_in_node_type(leaf3) -> 3;
total_keys_in_node_type(leaf2) -> 2;
total_keys_in_node_type(leaf1) -> 1.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take/2
%% ------------------------------------------------------------------

take_recur(Key, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            take_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            take_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            take_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            take_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            take_LEAF4(Key, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, take_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            take_INTERNAL4_C4(Key, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            take_INTERNAL4_C3(Key, ?INTERNAL4_ARGS);
                        %
                        true ->
                            take_INTERNAL4_K3(?INTERNAL4_ARGS)
                    end;
                %
                Key > K4 ->
                    take_INTERNAL4_C5(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    take_INTERNAL4_K4(?INTERNAL4_ARGS)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    take_INTERNAL4_C1(Key, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    take_INTERNAL4_C2(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    take_INTERNAL4_K1(?INTERNAL4_ARGS)
            end;
        %
        true ->
            take_INTERNAL4_K2(?INTERNAL4_ARGS)
    end.

-compile({inline, take_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C1(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),

    ?TAKEN(Pair, ?INTERNAL4_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C2(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),

    ?TAKEN(Pair, ?INTERNAL4_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C3(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC3) = take_recur(Key, C3),

    ?TAKEN(Pair, ?INTERNAL4_C3_REBALANCE(UpdatedC3)).

-compile({inline, take_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C4(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC4) = take_recur(Key, C4),

    ?TAKEN(Pair, ?INTERNAL4_C4_REBALANCE(UpdatedC4)).

-compile({inline, take_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C5(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC5) = take_recur(Key, C5),

    ?TAKEN(Pair, ?INTERNAL4_C5_REBALANCE(UpdatedC5)).

%%

-compile({inline, take_INTERNAL4_K1 / ?INTERNAL4_ARITY}).
take_INTERNAL4_K1(?INTERNAL4_ARGS) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    ?TAKEN_PAIR(
        K1,
        V1,
        rebalance_INTERNAL4_C2(
            ReplacementK,
            K2,
            K3,
            K4,
            %
            ReplacementV,
            V2,
            V3,
            V4,
            %
            C1,
            UpdatedC2,
            C3,
            C4,
            C5
        )
    ).

-compile({inline, take_INTERNAL4_K2 / ?INTERNAL4_ARITY}).
take_INTERNAL4_K2(?INTERNAL4_ARGS) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_largest_recur(C2),

    ?TAKEN_PAIR(
        K2,
        V2,
        rebalance_INTERNAL4_C2(
            K1,
            ReplacementK,
            K3,
            K4,
            %
            V1,
            ReplacementV,
            V3,
            V4,
            %
            C1,
            UpdatedC2,
            C3,
            C4,
            C5
        )
    ).

-compile({inline, take_INTERNAL4_K3 / ?INTERNAL4_ARITY}).
take_INTERNAL4_K3(?INTERNAL4_ARGS) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) = take_largest_recur(C3),

    ?TAKEN_PAIR(
        K3,
        V3,
        rebalance_INTERNAL4_C3(
            K1,
            K2,
            ReplacementK,
            K4,
            %
            V1,
            V2,
            ReplacementV,
            V4,
            %
            C1,
            C2,
            UpdatedC3,
            C4,
            C5
        )
    ).

-compile({inline, take_INTERNAL4_K4 / ?INTERNAL4_ARITY}).
take_INTERNAL4_K4(?INTERNAL4_ARGS) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_largest_recur(C4),

    ?TAKEN_PAIR(
        K4,
        V4,
        rebalance_INTERNAL4_C4(
            K1,
            K2,
            K3,
            ReplacementK,
            %
            V1,
            V2,
            V3,
            ReplacementV,
            %
            C1,
            C2,
            C3,
            UpdatedC4,
            C5
        )
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, take_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    take_INTERNAL3_C1(Key, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    take_INTERNAL3_C2(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    take_INTERNAL3_K1(?INTERNAL3_ARGS)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    take_INTERNAL3_C3(Key, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    take_INTERNAL3_C4(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    take_INTERNAL3_K3(?INTERNAL3_ARGS)
            end;
        %
        true ->
            take_INTERNAL3_K2(?INTERNAL3_ARGS)
    end.

-compile({inline, take_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3_C1(Key, ?INTERNAL3_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),

    ?TAKEN(Pair, ?INTERNAL3_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3_C2(Key, ?INTERNAL3_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),

    ?TAKEN(Pair, ?INTERNAL3_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3_C3(Key, ?INTERNAL3_ARGS) ->
    ?TAKEN(Pair, UpdatedC3) = take_recur(Key, C3),

    ?TAKEN(Pair, ?INTERNAL3_C3_REBALANCE(UpdatedC3)).

-compile({inline, take_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3_C4(Key, ?INTERNAL3_ARGS) ->
    ?TAKEN(Pair, UpdatedC4) = take_recur(Key, C4),

    ?TAKEN(Pair, ?INTERNAL3_C4_REBALANCE(UpdatedC4)).

%%

-compile({inline, take_INTERNAL3_K1 / ?INTERNAL3_ARITY}).
take_INTERNAL3_K1(?INTERNAL3_ARGS) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    ?TAKEN_PAIR(
        K1,
        V1,
        rebalance_INTERNAL3_C2(
            ReplacementK,
            K2,
            K3,
            %
            ReplacementV,
            V2,
            V3,
            %
            C1,
            UpdatedC2,
            C3,
            C4
        )
    ).

-compile({inline, take_INTERNAL3_K2 / ?INTERNAL3_ARITY}).
take_INTERNAL3_K2(?INTERNAL3_ARGS) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) = take_smallest_recur(C3),

    ?TAKEN_PAIR(
        K2,
        V2,
        rebalance_INTERNAL3_C3(
            K1,
            ReplacementK,
            K3,
            %
            V1,
            ReplacementV,
            V3,
            %
            C1,
            C2,
            UpdatedC3,
            C4
        )
    ).

-compile({inline, take_INTERNAL3_K3 / ?INTERNAL3_ARITY}).
take_INTERNAL3_K3(?INTERNAL3_ARGS) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) = take_largest_recur(C3),

    ?TAKEN_PAIR(
        K3,
        V3,
        rebalance_INTERNAL3_C3(
            K1,
            K2,
            ReplacementK,
            %
            V1,
            V2,
            ReplacementV,
            %
            C1,
            C2,
            UpdatedC3,
            C4
        )
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, take_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
take_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    take_INTERNAL2_C2(Key, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    take_INTERNAL2_C3(Key, ?INTERNAL2_ARGS);
                %
                true ->
                    take_INTERNAL2_K2(?INTERNAL2_ARGS)
            end;
        %
        Key < K1 ->
            take_INTERNAL2_C1(Key, ?INTERNAL2_ARGS);
        %
        true ->
            take_INTERNAL2_K1(?INTERNAL2_ARGS)
    end.

-compile({inline, take_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
take_INTERNAL2_C1(Key, ?INTERNAL2_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),

    ?TAKEN(Pair, ?INTERNAL2_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
take_INTERNAL2_C2(Key, ?INTERNAL2_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),

    ?TAKEN(Pair, ?INTERNAL2_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
take_INTERNAL2_C3(Key, ?INTERNAL2_ARGS) ->
    ?TAKEN(Pair, UpdatedC3) = take_recur(Key, C3),

    ?TAKEN(Pair, ?INTERNAL2_C3_REBALANCE(UpdatedC3)).

%%

-compile({inline, take_INTERNAL2_K1 / ?INTERNAL2_ARITY}).
take_INTERNAL2_K1(?INTERNAL2_ARGS) ->
    %?INTERNAL2_VALUES_MATCH_ALL = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    ?TAKEN_PAIR(
        K1,
        V1,
        rebalance_INTERNAL2_C2(
            ReplacementK,
            K2,
            %
            ReplacementV,
            V2,
            %
            C1,
            UpdatedC2,
            C3
        )
    ).

-compile({inline, take_INTERNAL2_K2 / ?INTERNAL2_ARITY}).
take_INTERNAL2_K2(?INTERNAL2_ARGS) ->
    %?INTERNAL2_VALUES_MATCH_ALL = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) = take_smallest_recur(C3),

    ?TAKEN_PAIR(
        K2,
        V2,
        rebalance_INTERNAL2_C3(
            K1,
            ReplacementK,
            %
            V1,
            ReplacementV,
            %
            C1,
            C2,
            UpdatedC3
        )
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, take_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
take_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            take_INTERNAL1_C1(Key, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            take_INTERNAL1_C2(Key, ?INTERNAL1_ARGS);
        %
        true ->
            take_INTERNAL1_K1(?INTERNAL1_ARGS)
    end.

-compile({inline, take_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
take_INTERNAL1_C1(Key, ?INTERNAL1_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),
    ?TAKEN(Pair, ?INTERNAL1_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
take_INTERNAL1_C2(Key, ?INTERNAL1_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),
    ?TAKEN(Pair, ?INTERNAL1_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL1_K1 / ?INTERNAL1_ARITY}).
take_INTERNAL1_K1(?INTERNAL1_ARGS) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    ?TAKEN_PAIR(K1, V1, ?INTERNAL1_K1_C2_REBALANCE(ReplacementK, ReplacementV, UpdatedC2)).

%%
%% ?LEAF4
%%

-compile({inline, take_LEAF4 / ?LEAF4_ARITY_PLUS1}).
take_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF3(K2, K3, K4, V2, V3, V4));
        %
        Key == K2 ->
            ?TAKEN_PAIR(K2, V2, ?new_LEAF3(K1, K3, K4, V1, V3, V4));
        %
        Key == K3 ->
            ?TAKEN_PAIR(K3, V3, ?new_LEAF3(K1, K2, K4, V1, V2, V4));
        %
        Key == K4 ->
            ?TAKEN_PAIR(K4, V4, ?new_LEAF3(K1, K2, K3, V1, V2, V3));
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF3
%%

-compile({inline, take_LEAF3 / ?LEAF3_ARITY_PLUS1}).
take_LEAF3(Key, ?LEAF3_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF2(K2, K3, V2, V3));
        %
        Key == K2 ->
            ?TAKEN_PAIR(K2, V2, ?new_LEAF2(K1, K3, V1, V3));
        %
        Key == K3 ->
            ?TAKEN_PAIR(K3, V3, ?new_LEAF2(K1, K2, V1, V2));
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF2
%%

-compile({inline, take_LEAF2 / ?LEAF2_ARITY_PLUS1}).
take_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF1(K2, V2));
        %
        Key == K2 ->
            ?TAKEN_PAIR(K2, V2, ?new_LEAF1(K1, V1));
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF1
%%

-compile({inline, take_LEAF1 / ?LEAF1_ARITY_PLUS1}).
take_LEAF1(Key, ?LEAF1_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?LEAF0);
        %
        true ->
            error_badkey(Key)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_largest/2
%% ------------------------------------------------------------------

take_largest_recur(Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            take_largest_INTERNAL2(?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            take_largest_INTERNAL3(?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_largest_INTERNAL4(?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            take_largest_LEAF2(?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            take_largest_LEAF3(?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            take_largest_LEAF4(?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, take_largest_INTERNAL4 / ?INTERNAL4_ARITY}).
take_largest_INTERNAL4(?INTERNAL4_ARGS) ->
    ?TAKEN(Taken, UpdatedC5) = take_largest_recur(C5),
    ?TAKEN(Taken, ?INTERNAL4_C5_REBALANCE(UpdatedC5)).

%%
%% ?INTERNAL3
%%

-compile({inline, take_largest_INTERNAL3 / ?INTERNAL3_ARITY}).
take_largest_INTERNAL3(?INTERNAL3_ARGS) ->
    ?TAKEN(Taken, UpdatedC4) = take_largest_recur(C4),
    ?TAKEN(Taken, ?INTERNAL3_C4_REBALANCE(UpdatedC4)).

%%
%% ?INTERNAL2
%%

-compile({inline, take_largest_INTERNAL2 / ?INTERNAL2_ARITY}).
take_largest_INTERNAL2(?INTERNAL2_ARGS) ->
    ?TAKEN(Taken, UpdatedC3) = take_largest_recur(C3),
    ?TAKEN(Taken, ?INTERNAL2_C3_REBALANCE(UpdatedC3)).

%%
%% ?INTERNAL1
%%

-compile({inline, take_largest_INTERNAL1 / ?INTERNAL1_ARITY}).
take_largest_INTERNAL1(?INTERNAL1_ARGS) ->
    ?TAKEN(Taken, UpdatedC2) = take_largest_recur(C2),
    ?TAKEN(Taken, ?INTERNAL1_C2_REBALANCE(UpdatedC2)).

%%
%% ?LEAF4
%%

-compile({inline, take_largest_LEAF4 / ?LEAF4_ARITY}).
take_largest_LEAF4(?LEAF4_ARGS) ->
    ?TAKEN_PAIR(K4, V4, ?new_LEAF3(K1, K2, K3, V1, V2, V3)).

%%
%% ?LEAF3
%%

-compile({inline, take_largest_LEAF3 / ?LEAF3_ARITY}).
take_largest_LEAF3(?LEAF3_ARGS) ->
    ?TAKEN_PAIR(K3, V3, ?new_LEAF2(K1, K2, V1, V2)).

%%
%% ?LEAF2
%%

-compile({inline, take_largest_LEAF2 / ?LEAF2_ARITY}).
take_largest_LEAF2(?LEAF2_ARGS) ->
    ?TAKEN_PAIR(K2, V2, ?new_LEAF1(K1, V1)).

%%
%% ?LEAF1
%%

-compile({inline, take_largest_LEAF1 / ?LEAF1_ARITY}).
take_largest_LEAF1(?LEAF1_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?LEAF0).

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_smallest/2
%% ------------------------------------------------------------------

take_smallest_recur(Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            take_smallest_INTERNAL2(?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            take_smallest_INTERNAL3(?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_smallest_INTERNAL4(?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            take_smallest_LEAF2(?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            take_smallest_LEAF3(?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            take_smallest_LEAF4(?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, take_smallest_INTERNAL4 / ?INTERNAL4_ARITY}).
take_smallest_INTERNAL4(?INTERNAL4_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL4_C1_REBALANCE(UpdatedC1)).

%%
%% ?INTERNAL3
%%

-compile({inline, take_smallest_INTERNAL3 / ?INTERNAL3_ARITY}).
take_smallest_INTERNAL3(?INTERNAL3_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL3_C1_REBALANCE(UpdatedC1)).

%%
%% ?INTERNAL2
%%

-compile({inline, take_smallest_INTERNAL2 / ?INTERNAL2_ARITY}).
take_smallest_INTERNAL2(?INTERNAL2_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL2_C1_REBALANCE(UpdatedC1)).

%%
%% ?INTERNAL1
%%

-compile({inline, take_smallest_INTERNAL1 / ?INTERNAL1_ARITY}).
take_smallest_INTERNAL1(?INTERNAL1_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL1_C1_REBALANCE(UpdatedC1)).

%%
%% ?LEAF4
%%

-compile({inline, take_smallest_LEAF4 / ?LEAF4_ARITY}).
take_smallest_LEAF4(?LEAF4_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?new_LEAF3(K2, K3, K4, V2, V3, V4)).

%%
%% ?LEAF3
%%

-compile({inline, take_smallest_LEAF3 / ?LEAF3_ARITY}).
take_smallest_LEAF3(?LEAF3_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?new_LEAF2(K2, K3, V2, V3)).

%%
%% ?LEAF2
%%

-compile({inline, take_smallest_LEAF2 / ?LEAF2_ARITY}).
take_smallest_LEAF2(?LEAF2_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?new_LEAF1(K2, V2)).

%%
%% ?LEAF1
%%

-compile({inline, take_smallest_LEAF1 / ?LEAF1_ARITY}).
take_smallest_LEAF1(?LEAF1_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?LEAF0).

%% ------------------------------------------------------------------
%% Internal Function Definitions: to_list/1
%% ------------------------------------------------------------------

to_list_recur(Node, Acc) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            [{K1, V1}, {K2, V2} | Acc];
        %
        ?LEAF3_MATCH_ALL ->
            [{K1, V1}, {K2, V2}, {K3, V3} | Acc];
        %
        ?LEAF4_MATCH_ALL ->
            [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4} | Acc];
        %
        ?INTERNAL2_MATCH_ALL ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,
            Acc2 = to_list_recur(C3, Acc),
            Acc3 = to_list_recur(C2, [{K2, V2} | Acc2]),
            _Acc4 = to_list_recur(C1, [{K1, V1} | Acc3]);
        %
        ?INTERNAL3_MATCH_ALL ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,
            Acc2 = to_list_recur(C4, Acc),
            Acc3 = to_list_recur(C3, [{K3, V3} | Acc2]),
            Acc4 = to_list_recur(C2, [{K2, V2} | Acc3]),
            _Acc5 = to_list_recur(C1, [{K1, V1} | Acc4]);
        %
        ?INTERNAL4_MATCH_ALL ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = to_list_recur(C5, Acc),
            Acc3 = to_list_recur(C4, [{K4, V4} | Acc2]),
            Acc4 = to_list_recur(C3, [{K3, V3} | Acc3]),
            Acc5 = to_list_recur(C2, [{K2, V2} | Acc4]),
            _Acc6 = to_list_recur(C1, [{K1, V1} | Acc5])
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: update/2
%% ------------------------------------------------------------------

update_recur(Key, ValueEval, ValueWrap, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            update_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            update_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            update_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            update_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            update_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            update_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, update_INTERNAL4 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            update_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            update_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        true ->
                            update_INTERNAL4_K3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
                    end;
                %
                Key > K4 ->
                    update_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    update_INTERNAL4_K4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    update_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    update_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    update_INTERNAL4_K1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
            end;
        %
        true ->
            update_INTERNAL4_K2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
    end.

-compile({inline, update_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL4_C1(UpdatedC1).

-compile({inline, update_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL4_C2(UpdatedC2).

-compile({inline, update_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC3 = update_recur(Key, ValueEval, ValueWrap, C3),
    ?INTERNAL4_C3(UpdatedC3).

-compile({inline, update_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC4 = update_recur(Key, ValueEval, ValueWrap, C4),
    ?INTERNAL4_C4(UpdatedC4).

-compile({inline, update_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC5 = update_recur(Key, ValueEval, ValueWrap, C5),
    ?INTERNAL4_C5(UpdatedC5).

%%

-compile({inline, update_INTERNAL4_K1 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_K1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K1) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL4(
        Key,
        K2,
        K3,
        K4,
        %
        Value,
        V2,
        V3,
        V4,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, update_INTERNAL4_K2 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_K2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K2) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V2),

    ?new_INTERNAL4(
        K1,
        Key,
        K3,
        K4,
        %
        V1,
        Value,
        V3,
        V4,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, update_INTERNAL4_K3 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_K3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K3) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V3),

    ?new_INTERNAL4(
        K1,
        K2,
        Key,
        K4,
        %
        V1,
        V2,
        Value,
        V4,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, update_INTERNAL4_K4 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_K4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K4) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V4),

    ?new_INTERNAL4(
        K1,
        K2,
        K3,
        Key,
        %
        V1,
        V2,
        V3,
        Value,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, update_INTERNAL3 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    update_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    update_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    update_INTERNAL3_K1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    update_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    update_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    update_INTERNAL3_K3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
            end;
        %
        true ->
            update_INTERNAL3_K2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
    end.

-compile({inline, update_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL3_C1(UpdatedC1).

-compile({inline, update_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL3_C2(UpdatedC2).

-compile({inline, update_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    UpdatedC3 = update_recur(Key, ValueEval, ValueWrap, C3),
    ?INTERNAL3_C3(UpdatedC3).

-compile({inline, update_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    UpdatedC4 = update_recur(Key, ValueEval, ValueWrap, C4),
    ?INTERNAL3_C4(UpdatedC4).

%%

-compile({inline, update_INTERNAL3_K1 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_K1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS_IGN_K1) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL3(
        Key,
        K2,
        K3,
        %
        Value,
        V2,
        V3,
        %
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, update_INTERNAL3_K2 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_K2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS_IGN_K2) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V2),

    ?new_INTERNAL3(
        K1,
        Key,
        K3,
        %
        V1,
        Value,
        V3,
        %
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, update_INTERNAL3_K3 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_K3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS_IGN_K3) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V3),

    ?new_INTERNAL3(
        K1,
        K2,
        Key,
        %
        V1,
        V2,
        Value,
        %
        C1,
        C2,
        C3,
        C4
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, update_INTERNAL2 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    update_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    update_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                true ->
                    update_INTERNAL2_K2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS)
            end;
        %
        Key < K1 ->
            update_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        true ->
            update_INTERNAL2_K1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS)
    end.

-compile({inline, update_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL2_C1(UpdatedC1).

-compile({inline, update_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL2_C2(UpdatedC2).

-compile({inline, update_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    UpdatedC3 = update_recur(Key, ValueEval, ValueWrap, C3),
    ?INTERNAL2_C3(UpdatedC3).

%%

-compile({inline, update_INTERNAL2_K1 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_K1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS_IGN_K1) ->
    %?INTERNAL2_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL2(
        Key,
        K2,
        %
        Value,
        V2,
        %
        C1,
        C2,
        C3
    ).

-compile({inline, update_INTERNAL2_K2 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_K2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS_IGN_K2) ->
    %?INTERNAL2_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V2),

    ?new_INTERNAL2(
        K1,
        Key,
        %
        V1,
        Value,
        %
        C1,
        C2,
        C3
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, update_INTERNAL1 / ?INTERNAL1_ARITY_PLUS3}).
update_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            update_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            update_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        true ->
            update_INTERNAL1_K1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS)
    end.

-compile({inline, update_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS3}).
update_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL1_C1(UpdatedC1).

-compile({inline, update_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS3}).
update_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL1_C2(UpdatedC2).

%%

-compile({inline, update_INTERNAL1_K1 / ?INTERNAL1_ARITY_PLUS3}).
update_INTERNAL1_K1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS_IGN_K1) ->
    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL1(
        Key,
        %
        Value,
        %
        C1,
        C2
    ).

%%
%% ?LEAF4
%%

-compile({inline, update_LEAF4 / ?LEAF4_ARITY_PLUS3}).
update_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF4(
                Key,
                K2,
                K3,
                K4,
                %
                Value,
                V2,
                V3,
                V4
            );
        %
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?new_LEAF4(
                K1,
                Key,
                K3,
                K4,
                %
                V1,
                Value,
                V3,
                V4
            );
        %
        Key == K3 ->
            Value = eval_update_value(ValueEval, ValueWrap, V3),
            ?new_LEAF4(
                K1,
                K2,
                Key,
                K4,
                %
                V1,
                V2,
                Value,
                V4
            );
        %
        Key == K4 ->
            Value = eval_update_value(ValueEval, ValueWrap, V4),
            ?new_LEAF4(
                K1,
                K2,
                K3,
                Key,
                %
                V1,
                V2,
                V3,
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF3
%%

-compile({inline, update_LEAF3 / ?LEAF3_ARITY_PLUS3}).
update_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF3(
                Key,
                K2,
                K3,
                %
                Value,
                V2,
                V3
            );
        %
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?new_LEAF3(
                K1,
                Key,
                K3,
                %
                V1,
                Value,
                V3
            );
        %
        Key == K3 ->
            Value = eval_update_value(ValueEval, ValueWrap, V3),
            ?new_LEAF3(
                K1,
                K2,
                Key,
                %
                V1,
                V2,
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF2
%%

-compile({inline, update_LEAF2 / ?LEAF2_ARITY_PLUS3}).
update_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF2(
                Key,
                K2,
                %
                Value,
                V2
            );
        %
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?new_LEAF2(
                K1,
                Key,
                %
                V1,
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF1
%%

-compile({inline, update_LEAF1 / ?LEAF1_ARITY_PLUS3}).
update_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF1(
                Key,
                %
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%%

-compile({inline, eval_update_value/3}).
eval_update_value(Type, Wrap, Prev) ->
    case Type of
        eager ->
            Wrap;
        %
        lazy ->
            Wrap(Prev)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: values/1
%% ------------------------------------------------------------------

values_recur(Node, Acc) ->
    case Node of
        ?LEAF2_MATCH(_, _, V1, V2) ->
            [V1, V2 | Acc];
        %
        ?LEAF3_MATCH(_, _, _, V1, V2, V3) ->
            [V1, V2, V3 | Acc];
        %
        ?LEAF4_MATCH(_, _, _, _, V1, V2, V3, V4) ->
            [V1, V2, V3, V4 | Acc];
        %
        ?INTERNAL2_MATCH(_, _, V1, V2, C1, C2, C3) ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,
            Acc2 = [V2 | values_recur(C3, Acc)],
            Acc3 = [V1 | values_recur(C2, Acc2)],
            values_recur(C1, Acc3);
        %
        ?INTERNAL3_MATCH(_, _, _, V1, V2, V3, C1, C2, C3, C4) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,
            Acc2 = [V3 | values_recur(C4, Acc)],
            Acc3 = [V2 | values_recur(C3, Acc2)],
            Acc4 = [V1 | values_recur(C2, Acc3)],
            values_recur(C1, Acc4);
        %
        ?INTERNAL4_MATCH(_, _, _, _, V1, V2, V3, V4, C1, C2, C3, C4, C5) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = [V4 | values_recur(C5, Acc)],
            Acc3 = [V3 | values_recur(C4, Acc2)],
            Acc4 = [V2 | values_recur(C3, Acc3)],
            Acc5 = [V1 | values_recur(C2, Acc4)],
            values_recur(C1, Acc5)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL4
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL4_C1 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C1(?INTERNAL4_ARGS) ->
    case C1 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_right_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K1,
                V1,
                C2
            ),

            rebalance_INTERNAL4_C1_finish(
                Result,
                K2,
                K3,
                K4,
                %
                V2,
                V3,
                V4,
                %
                C3,
                C4,
                C5
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            Result = rebalance_leaf_from_right_sibling(
                CK,
                CV,
                %
                K1,
                V1,
                C2
            ),

            rebalance_INTERNAL4_C1_finish(
                Result,
                K2,
                K3,
                K4,
                %
                V2,
                V3,
                V4,
                %
                C3,
                C4,
                C5
            );
        %
        UpdatedC1 ->
            ?INTERNAL4_C1(UpdatedC1)
    end.

-compile({inline, rebalance_INTERNAL4_C1_finish / ?INTERNAL4_ARITY_MINUS3}).
rebalance_INTERNAL4_C1_finish(
    Result,
    K2,
    K3,
    K4,
    %
    V2,
    V3,
    V4,
    %
    C3,
    C4,
    C5
) ->
    case Result of
        ?ROTATED(UpKey, UpValue, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL4(
                UpKey,
                K2,
                K3,
                K4,
                %
                UpValue,
                V2,
                V3,
                V4,
                %
                UpdatedC1,
                UpdatedC2,
                C3,
                C4,
                C5
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL3(
                K2,
                K3,
                K4,
                %
                V2,
                V3,
                V4,
                %
                MergedC1C2,
                C3,
                C4,
                C5
            )
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL4_C2 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C2(?INTERNAL4_ARGS) ->
    case C2 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_either_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K1,
                V1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            rebalance_INTERNAL4_C2_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                %
                V1,
                V2,
                V3,
                V4,
                %
                C1,
                C3,
                C4,
                C5
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

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

            rebalance_INTERNAL4_C2_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                %
                V1,
                V2,
                V3,
                V4,
                %
                C1,
                C3,
                C4,
                C5
            );
        %
        UpdatedC2 ->
            ?INTERNAL4_C2(UpdatedC2)
    end.

-compile({inline, rebalance_INTERNAL4_C2_finish / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C2_finish(
    Result,
    K1,
    K2,
    K3,
    K4,
    %
    V1,
    V2,
    V3,
    V4,
    %
    C1,
    C3,
    C4,
    C5
) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC1C2) ->
            ?new_INTERNAL3(
                K2,
                K3,
                K4,
                %
                V2,
                V3,
                V4,
                %
                MergedC1C2,
                C3,
                C4,
                C5
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, RebalancedC2, UpdatedC3) ->
            ?new_INTERNAL4(
                K1,
                UpKey,
                K3,
                K4,
                %
                V1,
                UpValue,
                V3,
                V4,
                %
                C1,
                RebalancedC2,
                UpdatedC3,
                C4,
                C5
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL4(
                UpKey,
                K2,
                K3,
                K4,
                %
                UpValue,
                V2,
                V3,
                V4,
                %
                UpdatedC1,
                RebalancedC2,
                C3,
                C4,
                C5
            )
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C3(?INTERNAL4_ARGS) ->
    case C3 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_either_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K2,
                V2,
                C2,
                %
                K3,
                V3,
                C4
            ),

            rebalance_INTERNAL4_C3_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                %
                V1,
                V2,
                V3,
                V4,
                %
                C1,
                C2,
                C4,
                C5
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                %
                K2,
                V2,
                C2,
                %
                K3,
                V3,
                C4
            ),

            rebalance_INTERNAL4_C3_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                %
                V1,
                V2,
                V3,
                V4,
                %
                C1,
                C2,
                C4,
                C5
            );
        %
        UpdatedC3 ->
            ?INTERNAL4_C3(UpdatedC3)
    end.

-compile({inline, rebalance_INTERNAL4_C3_finish / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C3_finish(
    Result,
    K1,
    K2,
    K3,
    K4,
    %
    V1,
    V2,
    V3,
    V4,
    %
    C1,
    C2,
    C4,
    C5
) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC2C3) ->
            ?new_INTERNAL3(
                K1,
                K3,
                K4,
                %
                V1,
                V3,
                V4,
                %
                C1,
                MergedC2C3,
                C4,
                C5
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, RebalancedC3, UpdatedC4) ->
            ?new_INTERNAL4(
                K1,
                K2,
                UpKey,
                K4,
                %
                V1,
                V2,
                UpValue,
                V4,
                %
                C1,
                C2,
                RebalancedC3,
                UpdatedC4,
                C5
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedC2, RebalancedC3) ->
            ?new_INTERNAL4(
                K1,
                UpKey,
                K3,
                K4,
                %
                V1,
                UpValue,
                V3,
                V4,
                %
                C1,
                UpdatedC2,
                RebalancedC3,
                C4,
                C5
            )
    end.

%%
%% C4
%%

-compile({inline, rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C4(?INTERNAL4_ARGS) ->
    case C4 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_either_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K3,
                V3,
                C3,
                %
                K4,
                V4,
                C5
            ),

            rebalance_INTERNAL4_C4_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                %
                V1,
                V2,
                V3,
                V4,
                %
                C1,
                C2,
                C3,
                C5
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                %
                K3,
                V3,
                C3,
                %
                K4,
                V4,
                C5
            ),

            rebalance_INTERNAL4_C4_finish(
                Result,
                K1,
                K2,
                K3,
                K4,
                %
                V1,
                V2,
                V3,
                V4,
                %
                C1,
                C2,
                C3,
                C5
            );
        %
        UpdatedC4 ->
            ?INTERNAL4_C4(UpdatedC4)
    end.

-compile({inline, rebalance_INTERNAL4_C4_finish / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C4_finish(
    Result,
    K1,
    K2,
    K3,
    K4,
    %
    V1,
    V2,
    V3,
    V4,
    %
    C1,
    C2,
    C3,
    C5
) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC3C4) ->
            ?new_INTERNAL3(
                K1,
                K2,
                K4,
                %
                V1,
                V2,
                V4,
                %
                C1,
                C2,
                MergedC3C4,
                C5
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, RebalancedC4, UpdatedC5) ->
            ?new_INTERNAL4(
                K1,
                K2,
                K3,
                UpKey,
                %
                V1,
                V2,
                V3,
                UpValue,
                %
                C1,
                C2,
                C3,
                RebalancedC4,
                UpdatedC5
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedC3, RebalancedC4) ->
            ?new_INTERNAL4(
                K1,
                K2,
                UpKey,
                K4,
                %
                V1,
                V2,
                UpValue,
                V4,
                %
                C1,
                C2,
                UpdatedC3,
                RebalancedC4,
                C5
            )
    end.

%%
%% C5
%%

-compile({inline, rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C5(?INTERNAL4_ARGS) ->
    case C5 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_left_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K4,
                V4,
                C4
            ),

            rebalance_INTERNAL4_C5_finish(
                Result,
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C2,
                C3
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            Result = rebalance_leaf_from_left_sibling(
                CK,
                CV,
                %
                K4,
                V4,
                C4
            ),

            rebalance_INTERNAL4_C5_finish(
                Result,
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C2,
                C3
            );
        %
        UpdatedC5 ->
            ?INTERNAL4_C5(UpdatedC5)
    end.

-compile({inline, rebalance_INTERNAL4_C5_finish / ?INTERNAL4_ARITY_MINUS3}).
rebalance_INTERNAL4_C5_finish(
    Result,
    K1,
    K2,
    K3,
    %
    V1,
    V2,
    V3,
    %
    C1,
    C2,
    C3
) ->
    case Result of
        ?ROTATED(UpKey, UpValue, UpdatedC4, RebalancedC5) ->
            ?new_INTERNAL4(
                K1,
                K2,
                K3,
                UpKey,
                %
                V1,
                V2,
                V3,
                UpValue,
                %
                C1,
                C2,
                C3,
                UpdatedC4,
                RebalancedC5
            );
        %
        ?MERGED(MergedC4C5) ->
            ?new_INTERNAL3(
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C2,
                C3,
                MergedC4C5
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL3
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL3_C1 / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C1(?INTERNAL3_ARGS) ->
    case C1 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_right_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K1,
                V1,
                C2
            ),

            rebalance_INTERNAL3_C1_finish(
                Result,
                K2,
                K3,
                %
                V2,
                V3,
                %
                C3,
                C4
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            Result = rebalance_leaf_from_right_sibling(
                CK,
                CV,
                %
                K1,
                V1,
                C2
            ),

            rebalance_INTERNAL3_C1_finish(
                Result,
                K2,
                K3,
                %
                V2,
                V3,
                %
                C3,
                C4
            );
        %
        UpdatedC1 ->
            ?INTERNAL3_C1(UpdatedC1)
    end.

-compile({inline, rebalance_INTERNAL3_C1_finish / ?INTERNAL3_ARITY_MINUS3}).
rebalance_INTERNAL3_C1_finish(
    Result,
    K2,
    K3,
    %
    V2,
    V3,
    %
    C3,
    C4
) ->
    case Result of
        ?ROTATED(UpKey, UpValue, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL3(
                UpKey,
                K2,
                K3,
                %
                UpValue,
                V2,
                V3,
                %
                UpdatedC1,
                UpdatedC2,
                C3,
                C4
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL2(
                K2,
                K3,
                %
                V2,
                V3,
                %
                MergedC1C2,
                C3,
                C4
            )
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL3_C2 / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C2(?INTERNAL3_ARGS) ->
    case C2 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_either_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K1,
                V1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            rebalance_INTERNAL3_C2_finish(
                Result,
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C3,
                C4
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

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

            rebalance_INTERNAL3_C2_finish(
                Result,
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C3,
                C4
            );
        %
        UpdatedC2 ->
            ?INTERNAL3_C2(UpdatedC2)
    end.

-compile({inline, rebalance_INTERNAL3_C2_finish / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C2_finish(
    Result,
    K1,
    K2,
    K3,
    %
    V1,
    V2,
    V3,
    %
    C1,
    C3,
    C4
) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC1C2) ->
            ?new_INTERNAL2(
                K2,
                K3,
                %
                V2,
                V3,
                %
                MergedC1C2,
                C3,
                C4
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, RebalancedC2, UpdatedC3) ->
            ?new_INTERNAL3(
                K1,
                UpKey,
                K3,
                %
                V1,
                UpValue,
                V3,
                %
                C1,
                RebalancedC2,
                UpdatedC3,
                C4
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL3(
                UpKey,
                K2,
                K3,
                %
                UpValue,
                V2,
                V3,
                %
                UpdatedC1,
                RebalancedC2,
                C3,
                C4
            )
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C3(?INTERNAL3_ARGS) ->
    case C3 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_either_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K2,
                V2,
                C2,
                %
                K3,
                V3,
                C4
            ),

            rebalance_INTERNAL3_C3_finish(
                Result,
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C2,
                C4
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            Result = rebalance_leaf_from_either_sibling(
                CK,
                CV,
                %
                K2,
                V2,
                C2,
                %
                K3,
                V3,
                C4
            ),

            rebalance_INTERNAL3_C3_finish(
                Result,
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C2,
                C4
            );
        %
        UpdatedC3 ->
            ?INTERNAL3_C3(UpdatedC3)
    end.

-compile({inline, rebalance_INTERNAL3_C3_finish / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C3_finish(
    Result,
    K1,
    K2,
    K3,
    %
    V1,
    V2,
    V3,
    %
    C1,
    C2,
    C4
) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC2C3) ->
            ?new_INTERNAL2(
                K1,
                K3,
                %
                V1,
                V3,
                %
                C1,
                MergedC2C3,
                C4
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, RebalancedC3, UpdatedC4) ->
            ?new_INTERNAL3(
                K1,
                K2,
                UpKey,
                %
                V1,
                V2,
                UpValue,
                %
                C1,
                C2,
                RebalancedC3,
                UpdatedC4
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedC2, RebalancedC3) ->
            ?new_INTERNAL3(
                K1,
                UpKey,
                K3,
                %
                V1,
                UpValue,
                V3,
                %
                C1,
                UpdatedC2,
                RebalancedC3,
                C4
            )
    end.

%%
%% C4
%%

-compile({inline, rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C4(?INTERNAL3_ARGS) ->
    case C4 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_left_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K3,
                V3,
                C3
            ),

            rebalance_INTERNAL3_C4_finish(
                Result,
                K1,
                K2,
                %
                V1,
                V2,
                %
                C1,
                C2
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            Result = rebalance_leaf_from_left_sibling(
                CK,
                CV,
                %
                K3,
                V3,
                C3
            ),

            rebalance_INTERNAL3_C4_finish(
                Result,
                K1,
                K2,
                %
                V1,
                V2,
                %
                C1,
                C2
            );
        %
        UpdatedC4 ->
            ?INTERNAL3_C4(UpdatedC4)
    end.

-compile({inline, rebalance_INTERNAL3_C4_finish / ?INTERNAL3_ARITY_MINUS3}).
rebalance_INTERNAL3_C4_finish(
    Result,
    K1,
    K2,
    %
    V1,
    V2,
    %
    C1,
    C2
) ->
    case Result of
        ?ROTATED(UpKey, UpValue, UpdatedC3, RebalancedC4) ->
            ?new_INTERNAL3(
                K1,
                K2,
                UpKey,
                %
                V1,
                V2,
                UpValue,
                %
                C1,
                C2,
                UpdatedC3,
                RebalancedC4
            );
        %
        ?MERGED(MergedC3C4) ->
            ?new_INTERNAL2(
                K1,
                K2,
                %
                V1,
                V2,
                %
                C1,
                C2,
                MergedC3C4
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL2
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL2_C1 / ?INTERNAL2_ARITY}).
rebalance_INTERNAL2_C1(?INTERNAL2_ARGS) ->
    case C1 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_right_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K1,
                V1,
                C2
            ),

            rebalance_INTERNAL2_C1_finish(Result, K2, V2, C3);
        %
        ?LEAF1_MATCH(CK, CV) ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,

            Result = rebalance_leaf_from_right_sibling(
                CK,
                CV,
                %
                K1,
                V1,
                C2
            ),

            rebalance_INTERNAL2_C1_finish(Result, K2, V2, C3);
        %
        UpdatedC1 ->
            ?INTERNAL2_C1(UpdatedC1)
    end.

-compile({inline, rebalance_INTERNAL2_C1_finish / ?INTERNAL2_ARITY_MINUS3}).
rebalance_INTERNAL2_C1_finish(Result, K2, V2, C3) ->
    case Result of
        ?ROTATED(UpKey, UpValue, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL2(
                UpKey,
                K2,
                %
                UpValue,
                V2,
                %
                UpdatedC1,
                UpdatedC2,
                C3
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL1(K2, V2, MergedC1C2, C3)
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL2_C2 / ?INTERNAL2_ARITY}).
rebalance_INTERNAL2_C2(?INTERNAL2_ARGS) ->
    case C2 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_either_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K1,
                V1,
                C1,
                %
                K2,
                V2,
                C3
            ),

            rebalance_INTERNAL2_C2_finish(
                Result,
                K1,
                K2,
                %
                V1,
                V2,
                %
                C1,
                C3
            );
        %
        ?LEAF1_MATCH(CK, CV) ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,

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

            rebalance_INTERNAL2_C2_finish(
                Result,
                K1,
                K2,
                %
                V1,
                V2,
                %
                C1,
                C3
            );
        %
        UpdatedC2 ->
            ?INTERNAL2_C2(UpdatedC2)
    end.

-compile({inline, rebalance_INTERNAL2_C2_finish / ?INTERNAL2_ARITY}).
rebalance_INTERNAL2_C2_finish(
    Result,
    K1,
    K2,
    %
    V1,
    V2,
    %
    C1,
    C3
) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC1C2) ->
            ?new_INTERNAL1(
                K2,
                %
                V2,
                %
                MergedC1C2,
                C3
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, RebalancedC2, UpdatedC3) ->
            ?new_INTERNAL2(
                K1,
                UpKey,
                %
                V1,
                UpValue,
                %
                C1,
                RebalancedC2,
                UpdatedC3
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL2(
                UpKey,
                K2,
                %
                UpValue,
                V2,
                %
                UpdatedC1,
                RebalancedC2,
                C3
            )
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY}).
rebalance_INTERNAL2_C3(?INTERNAL2_ARGS) ->
    case C3 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,

            Result = rebalance_internal_from_left_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K2,
                V2,
                C2
            ),

            rebalance_INTERNAL2_C3_finish(Result, K1, V1, C1);
        %
        ?LEAF1_MATCH(CK, CV) ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,

            Result = rebalance_leaf_from_left_sibling(
                CK,
                CV,
                %
                K2,
                V2,
                C2
            ),

            rebalance_INTERNAL2_C3_finish(Result, K1, V1, C1);
        %
        UpdatedC3 ->
            ?INTERNAL2_C3(UpdatedC3)
    end.

-compile({inline, rebalance_INTERNAL2_C3_finish / ?INTERNAL2_ARITY_MINUS3}).
rebalance_INTERNAL2_C3_finish(Result, K1, V1, C1) ->
    case Result of
        ?ROTATED(UpKey, UpValue, UpdatedC2, RebalancedC3) ->
            ?new_INTERNAL2(
                K1,
                UpKey,
                %
                V1,
                UpValue,
                %
                C1,
                UpdatedC2,
                RebalancedC3
            );
        %
        ?MERGED(MergedC2C3) ->
            ?new_INTERNAL1(
                K1,
                %
                V1,
                %
                C1,
                MergedC2C3
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL1
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL1_C1 / ?INTERNAL1_ARITY}).
rebalance_INTERNAL1_C1(?INTERNAL1_ARGS) ->
    case C1 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            Result = rebalance_internal_from_right_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K1,
                V1,
                C2
            ),

            rebalance_INTERNAL1_finish(Result);
        %
        ?LEAF1_MATCH(CK, CV) ->
            Result = rebalance_leaf_from_right_sibling(
                CK,
                CV,
                %
                K1,
                V1,
                C2
            ),

            rebalance_INTERNAL1_finish(Result);
        %
        UpdatedC1 ->
            ?INTERNAL1_C1(UpdatedC1)
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL1_C2 / ?INTERNAL1_ARITY}).
rebalance_INTERNAL1_C2(?INTERNAL1_ARGS) ->
    case C2 of
        ?INTERNAL1_MATCH(CK, CV, CL, CR) ->
            Result = rebalance_internal_from_left_sibling(
                CK,
                CV,
                CL,
                CR,
                %
                K1,
                V1,
                C1
            ),

            rebalance_INTERNAL1_finish(Result);
        %
        ?LEAF1_MATCH(CK, CV) ->
            Result = rebalance_leaf_from_left_sibling(
                CK,
                CV,
                %
                K1,
                V1,
                C1
            ),

            rebalance_INTERNAL1_finish(Result);
        %
        UpdatedC2 ->
            ?INTERNAL1_C2(UpdatedC2)
    end.

%-compile({inline, rebalance_INTERNAL1_finish/?INTERNAL1_ARITY_MINUS2}).
rebalance_INTERNAL1_finish(Result) ->
    case Result of
        ?ROTATED(UpKey, UpValue, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL1(UpKey, UpValue, UpdatedC1, UpdatedC2);
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen at root - height is reduced
            MergedC1C2
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its right sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_right_sibling/7}).
rebalance_internal_from_right_sibling(
    CKey,
    CValue,
    CLeft,
    CRight,
    %
    ParentK,
    ParentV,
    Right
) ->
    case Right of
        ?INTERNAL2_MATCH_ALL ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,

            MergedNode = ?new_INTERNAL4(
                CKey,
                ParentK,
                K1,
                K2,
                %
                CValue,
                ParentV,
                V1,
                V2,
                %
                CLeft,
                CRight,
                C1,
                C2,
                C3
            ),

            MergedNode;
        %
        %
        %
        ?INTERNAL3_MATCH_ALL ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            UpKey = K1,
            UpValue = V1,
            MovedC = C1,

            UpdatedNode = ?new_INTERNAL2(
                CKey,
                ParentK,
                %
                CValue,
                ParentV,
                %
                CLeft,
                CRight,
                MovedC
            ),

            UpdatedRight = ?new_INTERNAL2(
                K2,
                K3,
                %
                V2,
                V3,
                %
                C2,
                C3,
                C4
            ),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight);
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            UpKey = K1,
            UpValue = V1,
            MovedC = C1,

            UpdatedNode = ?new_INTERNAL2(
                CKey,
                ParentK,
                %
                CValue,
                ParentV,
                %
                CLeft,
                CRight,
                MovedC
            ),

            UpdatedRight = ?new_INTERNAL3(
                K2,
                K3,
                K4,
                %
                V2,
                V3,
                V4,
                %
                C2,
                C3,
                C4,
                C5
            ),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight)
        %
        %
    end.

%-compile({inline, rebalance_leaf_from_right_sibling/5}).
rebalance_leaf_from_right_sibling(CKey, CValue, ParentK, ParentV, Right) ->
    case Right of
        ?LEAF2_MATCH_ALL ->
            MergedNode = ?new_LEAF4(
                CKey,
                ParentK,
                K1,
                K2,
                %
                CValue,
                ParentV,
                V1,
                V2
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpKey = K1,
            UpValue = V1,

            UpdatedNode = ?new_LEAF2(CKey, ParentK, CValue, ParentV),
            UpdatedRight = ?new_LEAF2(K2, K3, V2, V3),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpKey = K1,
            UpValue = V1,

            UpdatedNode = ?new_LEAF2(CKey, ParentK, CValue, ParentV),
            UpdatedRight = ?new_LEAF3(K2, K3, K4, V2, V3, V4),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight)
        %
        %
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its left sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_left_sibling/7}).
rebalance_internal_from_left_sibling(
    CKey,
    CValue,
    CLeft,
    CRight,
    %
    ParentK,
    ParentV,
    Left
) ->
    case Left of
        ?INTERNAL2_MATCH_ALL ->
            %?INTERNAL2_VALUES_MATCH_ALL = Values,

            MergedNode = ?new_INTERNAL4(
                K1,
                K2,
                ParentK,
                CKey,
                %
                V1,
                V2,
                ParentV,
                CValue,
                %
                C1,
                C2,
                C3,
                CLeft,
                CRight
            ),

            MergedNode;
        %
        %
        %
        ?INTERNAL3_MATCH_ALL ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            UpKey = K3,
            UpValue = V3,
            MovedC = C4,

            UpdatedNode = ?new_INTERNAL2(
                ParentK,
                CKey,
                %
                ParentV,
                CValue,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?new_INTERNAL2(
                K1,
                K2,
                %
                V1,
                V2,
                C1,
                C2,
                C3
            ),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            UpKey = K4,
            UpValue = V4,
            MovedC = C5,

            UpdatedNode = ?new_INTERNAL2(
                ParentK,
                CKey,
                %
                ParentV,
                CValue,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?new_INTERNAL3(
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C2,
                C3,
                C4
            ),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode)
        %
        %
    end.

%-compile({inline, rebalance_leaf_from_left_sibling/5}).
rebalance_leaf_from_left_sibling(
    CKey,
    CValue,
    ParentK,
    ParentV,
    Left
) ->
    case Left of
        ?LEAF2_MATCH_ALL ->
            MergedNode = ?new_LEAF4(
                K1,
                K2,
                ParentK,
                CKey,
                %
                V1,
                V2,
                ParentV,
                CValue
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpKey = K3,
            UpValue = V3,

            UpdatedNode = ?new_LEAF2(ParentK, CKey, ParentV, CValue),
            UpdatedLeft = ?new_LEAF2(K1, K2, V1, V2),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpKey = K4,
            UpValue = V4,

            UpdatedNode = ?new_LEAF2(ParentK, CKey, ParentV, CValue),
            UpdatedLeft = ?new_LEAF3(K1, K2, K3, V1, V2, V3),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from either left/right sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_either_sibling/10}).
rebalance_internal_from_either_sibling(
    CKey,
    CValue,
    CLeft,
    CRight,
    %
    LParentK,
    LParentV,
    Left,
    %
    RParentK,
    RParentV,
    Right
) ->
    case Left of
        ?INTERNAL2_MATCH(LK1, LK2, LV1, LV2, LC1, LC2, LC3) ->
            %
            %
            case Right of
                ?INTERNAL3_MATCH_ALL ->
                    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

                    UpKey = K1,
                    UpValue = V1,
                    MovedC = C1,

                    UpdatedNode = ?new_INTERNAL2(
                        CKey,
                        RParentK,
                        %
                        CValue,
                        RParentV,
                        %
                        CLeft,
                        CRight,
                        MovedC
                    ),

                    UpdatedRight = ?new_INTERNAL2(
                        K2,
                        K3,
                        %
                        V2,
                        V3,
                        %
                        C2,
                        C3,
                        C4
                    ),

                    ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, UpdatedNode, UpdatedRight);
                %
                %
                ?INTERNAL4_MATCH_ALL ->
                    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

                    UpKey = K1,
                    UpValue = V1,
                    MovedC = C1,

                    UpdatedNode = ?new_INTERNAL2(
                        CKey,
                        RParentK,
                        %
                        CValue,
                        RParentV,
                        %
                        CLeft,
                        CRight,
                        MovedC
                    ),

                    UpdatedRight = ?new_INTERNAL3(
                        K2,
                        K3,
                        K4,
                        %
                        V2,
                        V3,
                        V4,
                        %
                        C2,
                        C3,
                        C4,
                        C5
                    ),

                    ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, UpdatedNode, UpdatedRight);
                %
                %
                _ ->
                    % ?INTERNAL2_VALUES(LV1, LV2) = LValues,

                    % Merge with left since we already unpacked it
                    MergedNode = ?new_INTERNAL4(
                        LK1,
                        LK2,
                        LParentK,
                        CKey,
                        %
                        LV1,
                        LV2,
                        LParentV,
                        CValue,
                        %
                        LC1,
                        LC2,
                        LC3,
                        CLeft,
                        CRight
                    ),

                    ?MID_MERGED(MergedNode)
            end;
        %
        %
        %
        %
        ?INTERNAL3_MATCH_ALL ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            UpKey = K3,
            UpValue = V3,
            MovedC = C4,

            UpdatedNode = ?new_INTERNAL2(
                LParentK,
                CKey,
                %
                LParentV,
                CValue,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?new_INTERNAL2(
                K1,
                K2,
                %
                V1,
                V2,
                %
                C1,
                C2,
                C3
            ),

            ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            UpKey = K4,
            UpValue = V4,
            MovedC = C5,

            UpdatedNode = ?new_INTERNAL2(
                LParentK,
                CKey,
                %
                LParentV,
                CValue,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?new_INTERNAL3(
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C2,
                C3,
                C4
            ),

            ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedLeft, UpdatedNode)
    end.

% -compile({inline, rebalance_leaf_from_either_sibling/8}).
rebalance_leaf_from_either_sibling(
    CKey,
    CValue,
    %
    LParentK,
    LParentV,
    Left,
    %
    RParentK,
    RParentV,
    Right
) ->
    case Left of
        ?LEAF2_MATCH(LK1, LK2, LV1, LV2) ->
            %
            case Right of
                ?LEAF4_MATCH_ALL ->
                    UpKey = K1,
                    UpValue = V1,

                    UpdatedNode = ?new_LEAF2(CKey, RParentK, CValue, RParentV),
                    UpdatedRight = ?new_LEAF3(K2, K3, K4, V2, V3, V4),

                    ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, UpdatedNode, UpdatedRight);
                %
                %
                ?LEAF3_MATCH_ALL ->
                    UpKey = K1,
                    UpValue = V1,

                    UpdatedNode = ?new_LEAF2(CKey, RParentK, CValue, RParentV),
                    UpdatedRight = ?new_LEAF2(K2, K3, V2, V3),

                    ?MID_ROTATED_FROM_RIGHT(UpKey, UpValue, UpdatedNode, UpdatedRight);
                %
                %
                _ ->
                    % Merge with left since we already unpacked it
                    MergedNode = ?new_LEAF4(
                        LK1,
                        LK2,
                        LParentK,
                        CKey,
                        %
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
        ?LEAF3_MATCH_ALL ->
            UpKey = K3,
            UpValue = V3,

            UpdatedNode = ?new_LEAF2(LParentK, CKey, LParentV, CValue),
            UpdatedLeft = ?new_LEAF2(K1, K2, V1, V2),

            ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpKey = K4,
            UpValue = V4,

            UpdatedNode = ?new_LEAF2(LParentK, CKey, LParentV, CValue),
            UpdatedLeft = ?new_LEAF3(K1, K2, K3, V1, V2, V3),

            ?MID_ROTATED_FROM_LEFT(UpKey, UpValue, UpdatedLeft, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Well-Formedness Checks
%% ------------------------------------------------------------------

-if(?NODE_CHECK_ENABLED).

check_node(LineNumber, Node) ->
    Type = node_type(Node),
    List = to_list(Node),
    MissortedKeys = check_node_keys(List),

    case MissortedKeys of
        [] ->
            Node;
        %
        [_ | _] ->
            fail_node_check(LineNumber, Type, {missorted_keys, MissortedKeys})
    end.

node_type(Node) ->
    case Node of
        ?INTERNAL1(_, _, _, _) ->
            'INTERNAL1';
        %
        ?LEAF1(_, _) ->
            'LEAF1';
        %
        _ ->
            recur_node_type(Node)
    end.

check_node_recur(LineNumber, Node) ->
    Type = recur_node_type(Node),
    List = to_list_recur(Node, []),
    MissortedKeys = check_node_keys(List),

    case MissortedKeys of
        [] ->
            Node;
        %
        [_ | _] ->
            fail_node_check(LineNumber, Type, {missorted_keys, MissortedKeys})
    end.

recur_node_type(Node) ->
    case Node of
        ?INTERNAL2(_, _, _, _, _, _, _) ->
            'INTERNAL2';
        %
        ?INTERNAL3(_, _, _, _, _, _, _, _, _, _) ->
            'INTERNAL3';
        %
        ?INTERNAL4(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
            'INTERNAL4';
        %
        ?LEAF2(_, _, _, _) ->
            'LEAF2';
        %
        ?LEAF3(_, _, _, _, _, _) ->
            'LEAF3';
        %
        ?LEAF4(_, _, _, _, _, _, _, _) ->
            'LEAF4'
    end.

fail_node_check(LineNumber, Type, Reason) ->
    error(
        {bad_node, [
            {line, LineNumber},
            {type, Type},
            {reason, Reason}
        ]}
    ).

check_node_keys([H | T]) ->
    check_node_keys(H, T);
check_node_keys([]) ->
    [].

check_node_keys(K1, [K2 | Next]) ->
    case K1 >= K2 of
        false ->
            check_node_keys(K2, Next);
        %
        true ->
            [{K1, K2} | check_node_keys(K2, Next)]
    end;
check_node_keys(_, []) ->
    [].

% -if(?NODE_CHECK_ENABLED).
-endif.
