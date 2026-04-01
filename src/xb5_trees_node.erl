-module(xb5_trees_node).

-moduledoc """
API for operating over `m:xb5_trees` internal nodes directly.

> ℹ️
> You're likely looking for `m:xb5_trees`.

<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
""".

-include("src/xb5_search_helpers.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    delete_att/2,
    elixir_reduce/3,
    foldl/3,
    foldr/3,
    from_orddict/2,
    get_att/4,
    insert_att/4,
    intersect/4,
    intersect_with/5,
    iterator/2,
    iterator_from/3,
    is_equal/4,
    keys/1,
    larger/2,
    largest/1,
    map/2,
    merge/4,
    merge_with/5,
    new/0,
    next/1,
    smaller/2,
    smallest/1,
    structural_stats/1,
    take_att/2,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    to_rev_list/1,
    update_att/4,
    values/1
]).

-ignore_xref([
    elixir_reduce/3,
    to_rev_list/1
]).

%% ------------------------------------------------------------------
%% Static Check Tweaks
%% ------------------------------------------------------------------

-hank([
    {unnecessary_function_arguments, [
        {get_found, 2, 1},
        {get_not_found, 1, 1},
        {intersect_get_not_found, 1, 1},
        {merge_pick_first, 3, 1},
        {merge_pick_first, 3, 3},
        {merge_pick_second, 3, 1},
        {merge_pick_second, 3, 2}
    ]}
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

% 2 elements
-define(LEAF1(K1, V1), {K1, V1}).
-define(LEAF1_MATCH(K1, V1), {K1, V1}).
-define(LEAF1_MATCH_ALL, {K1, V1}).

% empty root
-define(LEAF0, leaf0).

%%%%%%%%

% Cannot clash with any node type.

-define(SPLIT(Pos, Args), [Pos | Args]).
-define(SPLIT_MATCH(Pos, Args), [Pos | Args]).

%%%%%%%%%

% Any of the following cannot clash with either the largest leaf or internal
% nodes, since those are a merged node.

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

-define(INTERNAL4_UPD_C1(UpdatedC1),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, UpdatedC1, C2, C3, C4, C5)
).
-define(INTERNAL4_UPD_C2(UpdatedC2),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_UPD_C3(UpdatedC3),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_UPD_C4(UpdatedC4),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_UPD_C5(UpdatedC5),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, UpdatedC5)
).

-define(INTERNAL4_C1_REBALANCE(UpdatedC1),
    del_rebalance_INTERNAL4_C1(K1, K2, K3, K4, V1, V2, V3, V4, UpdatedC1, C2, C3, C4, C5)
).
-define(INTERNAL4_C2_REBALANCE(UpdatedC2),
    del_rebalance_INTERNAL4_C2(K1, K2, K3, K4, V1, V2, V3, V4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_C3_REBALANCE(UpdatedC3),
    del_rebalance_INTERNAL4_C3(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_C4_REBALANCE(UpdatedC4),
    del_rebalance_INTERNAL4_C4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_C5_REBALANCE(UpdatedC5),
    del_rebalance_INTERNAL4_C5(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, UpdatedC5)
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

-define(INTERNAL3_UPD_C1(UpdatedC1), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, UpdatedC1, C2, C3, C4)).
-define(INTERNAL3_UPD_C2(UpdatedC2), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, UpdatedC2, C3, C4)).
-define(INTERNAL3_UPD_C3(UpdatedC3), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, UpdatedC3, C4)).
-define(INTERNAL3_UPD_C4(UpdatedC4), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, UpdatedC4)).

-define(INTERNAL3_C1_REBALANCE(UpdatedC1),
    del_rebalance_INTERNAL3_C1(K1, K2, K3, V1, V2, V3, UpdatedC1, C2, C3, C4)
).
-define(INTERNAL3_C2_REBALANCE(UpdatedC2),
    del_rebalance_INTERNAL3_C2(K1, K2, K3, V1, V2, V3, C1, UpdatedC2, C3, C4)
).
-define(INTERNAL3_C3_REBALANCE(UpdatedC3),
    del_rebalance_INTERNAL3_C3(K1, K2, K3, V1, V2, V3, C1, C2, UpdatedC3, C4)
).
-define(INTERNAL3_C4_REBALANCE(UpdatedC4),
    del_rebalance_INTERNAL3_C4(K1, K2, K3, V1, V2, V3, C1, C2, C3, UpdatedC4)
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

-define(INTERNAL2_UPD_C1(UpdatedC1), ?new_INTERNAL2(K1, K2, V1, V2, UpdatedC1, C2, C3)).
-define(INTERNAL2_UPD_C2(UpdatedC2), ?new_INTERNAL2(K1, K2, V1, V2, C1, UpdatedC2, C3)).
-define(INTERNAL2_UPD_C3(UpdatedC3), ?new_INTERNAL2(K1, K2, V1, V2, C1, C2, UpdatedC3)).

-define(INTERNAL2_C1_REBALANCE(UpdatedC1),
    del_rebalance_INTERNAL2_C1(K1, K2, V1, V2, UpdatedC1, C2, C3)
).
-define(INTERNAL2_C2_REBALANCE(UpdatedC2),
    del_rebalance_INTERNAL2_C2(K1, K2, V1, V2, C1, UpdatedC2, C3)
).
-define(INTERNAL2_C3_REBALANCE(UpdatedC3),
    del_rebalance_INTERNAL2_C3(K1, K2, V1, V2, C1, C2, UpdatedC3)
).

-define(INTERNAL2_ARGS_IGN_K1, _, K2, V1, V2, C1, C2, C3).
-define(INTERNAL2_ARGS_IGN_K2, K1, _, V1, V2, C1, C2, C3).

-define(INTERNAL2_ARGS_IGN_K1_V1, _, K2, _, V2, C1, C2, C3).
-define(INTERNAL2_ARGS_IGN_K2_V2, K1, _, V1, _, C1, C2, C3).

%% ?INTERNAL1

-define(INTERNAL1_ARGS, K1, V1, C1, C2).
-define(INTERNAL1_ARITY, 4).
-define(INTERNAL1_ARITY_PLUS1, 5).
-define(INTERNAL1_ARITY_PLUS3, 7).

-define(INTERNAL1_UPD_C1(UpdatedC1), ?new_INTERNAL1(K1, V1, UpdatedC1, C2)).
-define(INTERNAL1_UPD_C2(UpdatedC2), ?new_INTERNAL1(K1, V1, C1, UpdatedC2)).

-define(INTERNAL1_C1_REBALANCE(UpdatedC1), del_rebalance_INTERNAL1_C1(K1, V1, UpdatedC1, C2)).
-define(INTERNAL1_C2_REBALANCE(UpdatedC2), del_rebalance_INTERNAL1_C2(K1, V1, C1, UpdatedC2)).

-define(INTERNAL1_K1_C2_REBALANCE(ReplacementK, ReplacementV, UpdatedC2),
    del_rebalance_INTERNAL1_C2(ReplacementK, ReplacementV, C1, UpdatedC2)
).

-define(INTERNAL1_ARGS_IGN_K1, _, V1, C1, C2).
-define(INTERNAL1_ARGS_IGN_K1_V1, _, _, C1, C2).

%% ?LEAF4

-define(LEAF4_ARGS, K1, K2, K3, K4, V1, V2, V3, V4).
-define(LEAF4_ARITY_PLUS1, 9).
-define(LEAF4_ARITY_PLUS2, 10).
-define(LEAF4_ARITY_PLUS3, 11).

%% ?LEAF3

-define(LEAF3_ARGS, K1, K2, K3, V1, V2, V3).
-define(LEAF3_ARITY_PLUS1, 7).
-define(LEAF3_ARITY_PLUS2, 8).
-define(LEAF3_ARITY_PLUS3, 9).

%% ?LEAF2

-define(LEAF2_ARGS, K1, K2, V1, V2).
-define(LEAF2_ARITY_PLUS1, 5).
-define(LEAF2_ARITY_PLUS2, 6).
-define(LEAF2_ARITY_PLUS3, 7).

%% ?LEAF1

-define(LEAF1_ARGS, K1, V1).
-define(LEAF1_ARITY_PLUS1, 3).
-define(LEAF1_ARITY_PLUS3, 5).

%%

-define(TAKEN(Pair, UpdatedNode), [Pair | UpdatedNode]).

-define(TAKEN_PAIR(K, V, UpdatedNode), ?TAKEN([K | V], UpdatedNode)).

%%

% defined(TEST)).
-define(NODE_CHECK_ENABLED, false).

-if(?NODE_CHECK_ENABLED).
-define(CHECK_NODE(Node), check_node(?LINE, Node, top)).
-define(CHECK_NODE_RECUR(Node), check_node(?LINE, Node, recur)).
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

-type nonempty_node(Key, Value) ::
    (node_INTERNAL1(Key, Value)
    | node_LEAF1(Key, Value)
    | deep_node(Key, Value)).

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

-type node_LEAF1(Key, Value) :: ?LEAF1(Key, Value).

%%%%%%%%%%%

-type split_internal_result(Key, Value) :: split_result(
    Key, Value, node_INTERNAL2(Key, Value), node_INTERNAL2(Key, Value)
).

-type split_leaf_result(Key, Value) :: split_result(
    Key, Value, node_LEAF2(Key, Value), node_LEAF2(Key, Value)
).

-type split_result(Key, Value, SplitL, SplitR) :: {split, Key, Value, SplitL, SplitR}.

%%%%%%%%%%%

-type take_result(Key, Value) :: nonempty_improper_list(kv_pair(Key, Value), t(Key, Value)).
-export_type([take_result/2]).

%%%%%%%%%%%

-opaque iter(Key, Value) :: forward_iter(Key, Value) | reverse_iter(Key, Value).
-export_type([iter/2]).

-type forward_iter(Key, Value) :: [iterator_step(Key, Value)].
-type reverse_iter(Key, Value) :: nonempty_improper_list(reversed, [iterator_step(Key, Value)]).

-type iterator_step(Key, Value) :: kv_pair(Key, Value) | deep_node(Key, Value).

%%%%%%%%%%%

-type kv_pair(Key, Value) :: nonempty_improper_list(Key, Value).

%%%%%%%%%%%%

%%%%%%%%%%%%

-type elixir_reducer_acc(ElemAcc) :: {cont, ElemAcc} | {halt, ElemAcc} | {suspend, ElemAcc}.
-export_type([elixir_reducer_acc/1]).

-type elixir_reducer_result(ElemAcc) ::
    {done, ElemAcc}
    | {halted, ElemAcc}
    | {suspended, ElemAcc, elixir_reducer_continuation(ElemAcc)}.
-export_type([elixir_reducer_result/1]).

-type elixir_reducer_continuation(ElemAcc) :: fun(
    (elixir_reducer_acc(ElemAcc)) -> elixir_reducer_result(ElemAcc)
).
-export_type([elixir_reducer_continuation/1]).

-type elixir_reducer(Key, Value, ElemAcc) :: fun(
    ({Key, Value}, ElemAcc) -> elixir_reducer_acc(ElemAcc)
).
-export_type([elixir_reducer/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec delete_att(Key, t(Key, Value)) -> badkey | t(Key, Value).
delete_att(Key, ?INTERNAL1_MATCH_ALL) ->
    delete_att_INTERNAL1(Key, ?INTERNAL1_ARGS);
delete_att(Key, ?LEAF1_MATCH(K1, _)) ->
    delete_att_LEAF1(Key, K1);
delete_att(_Key, ?LEAF0) ->
    badkey;
delete_att(Key, Root) ->
    delete_att_recur(Key, Root).

-spec elixir_reduce(Fun, Acc, Root) -> Result when
    Fun :: elixir_reducer(Key, Value, ElemAcc),
    Acc :: elixir_reducer_acc(ElemAcc),
    Root :: t(Key, Value),
    Result :: elixir_reducer_result(ElemAcc).
elixir_reduce(Fun, Acc, Root) ->
    Iter = fwd_iterator(Root),
    elixir_reduce_recur(Fun, Acc, Iter).

-spec foldl(fun((Key, Value, Acc2) -> Acc1), Acc0, t(Key, Value)) -> AccN when
    Acc0 :: term(),
    Acc1 :: term(),
    Acc2 :: term(),
    AccN :: term().
foldl(Fun, Acc, ?INTERNAL1_MATCH_ALL) ->
    Acc2 = foldl_recur(Fun, Acc, C1),
    Acc3 = Fun(K1, V1, Acc2),
    foldl_recur(Fun, Acc3, C2);
foldl(Fun, Acc, ?LEAF1_MATCH_ALL) ->
    Fun(K1, V1, Acc);
foldl(_Fun, Acc, ?LEAF0) ->
    Acc;
foldl(Fun, Acc, Root) ->
    foldl_recur(Fun, Acc, Root).

-spec foldr(fun((Key, Value, Acc2) -> Acc1), Acc0, t(Key, Value)) -> AccN when
    Acc0 :: term(),
    Acc1 :: term(),
    Acc2 :: term(),
    AccN :: term().
foldr(Fun, Acc, ?INTERNAL1_MATCH_ALL) ->
    Acc2 = foldr_recur(Fun, Acc, C2),
    Acc3 = Fun(K1, V1, Acc2),
    foldr_recur(Fun, Acc3, C1);
foldr(Fun, Acc, ?LEAF1_MATCH_ALL) ->
    Fun(K1, V1, Acc);
foldr(_Fun, Acc, ?LEAF0) ->
    Acc;
foldr(Fun, Acc, Root) ->
    foldr_recur(Fun, Acc, Root).

-spec from_orddict(orddict:orddict(Key, Value), non_neg_integer()) -> t(Key, Value).
from_orddict([], 0) ->
    ?LEAF0;
from_orddict([{K1, V1}], 1) ->
    ?new_LEAF1(K1, V1);
from_orddict(L, S) ->
    [BatchOffset | BatchSize] = xb5_utils:bulk_construction_params(S),
    AtRoot = true,

    [Root | []] = from_orddict_recur(L, S, BatchOffset, BatchSize, AtRoot),
    Root.

-spec get_att(Key, t(Key, Value), fun((Key, Value) -> Found), fun((Key) -> NotFound)) ->
    Found | NotFound.
get_att(Key, ?INTERNAL1_MATCH_ALL, Found, NotFound) ->
    get_att_INTERNAL1(Key, ?INTERNAL1_ARGS, Found, NotFound);
get_att(Key, ?LEAF1_MATCH_ALL, Found, NotFound) ->
    get_att_LEAF1(Key, ?LEAF1_ARGS, Found, NotFound);
get_att(Key, ?LEAF0, _Found, NotFound) ->
    NotFound(Key);
get_att(Key, Root, Found, NotFound) ->
    get_att_recur(Key, Root, Found, NotFound).

-spec insert_att
    (Key, eager, Value, t(Key, Value)) -> key_exists | t(Key, Value);
    (Key, lazy, fun(() -> Value), t(Key, Value)) -> key_exists | t(Key, Value).
insert_att(Key, ValueEval, ValueWrap, ?INTERNAL1_MATCH_ALL) ->
    insert_att_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
insert_att(Key, ValueEval, ValueWrap, ?LEAF1_MATCH_ALL) ->
    insert_att_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
insert_att(Key, ValueEval, ValueWrap, ?LEAF0) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    xb5_utils:dialyzer_opaque_term(?new_LEAF1(Key, Value));
insert_att(Key, ValueEval, ValueWrap, Root) ->
    case insert_att_recur(Key, ValueEval, ValueWrap, Root) of
        ?SPLIT_MATCH(Pos, Args) ->
            insert_split_root(Pos, Args, Root);
        %
        UpdatedRoot ->
            UpdatedRoot
    end.

-spec intersect(non_neg_integer(), t(KA, VA), non_neg_integer(), t(KB, VB)) ->
    nonempty_improper_list(NewSize, t(KA | KB, VA | VB))
when
    NewSize :: non_neg_integer().
intersect(Size1, Root1, Size2, Root2) ->
    Fun1 = fun merge_pick_second/3,
    Fun2 = fun merge_pick_first/3,
    intersect_with2(Fun1, Size1, Root1, Fun2, Size2, Root2).

-spec intersect_with(IntersectFun, non_neg_integer(), t(KA, VA), non_neg_integer(), t(KB, VB)) ->
    nonempty_improper_list(NewSize, t(KA | KB, IntersectedV))
when
    IntersectFun :: fun((KA | KB, VA, VB) -> IntersectedV),
    NewSize :: non_neg_integer().
intersect_with(Fun, Size1, Root1, Size2, Root2) ->
    Fun1 = Fun,
    Fun2 = fun(Key, Value2, Value1) -> Fun(Key, Value1, Value2) end,
    intersect_with2(Fun1, Size1, Root1, Fun2, Size2, Root2).

-spec iterator(t(Key, Value), ordered | reversed) -> iter(Key, Value).
iterator(Root, ordered) ->
    fwd_iterator(Root);
iterator(Root, reversed) ->
    Acc = rev_iterator(Root),
    [?REV_ITER_TAG | Acc].

-spec iterator_from(Key, t(Key, Value), ordered | reversed) -> iter(Key, Value).
iterator_from(Key, Root, ordered) ->
    bound_fwd_iterator(Key, Root);
iterator_from(Key, Root, reversed) ->
    Acc = bound_rev_iterator(Key, Root),
    [?REV_ITER_TAG | Acc].

-spec is_equal(non_neg_integer(), t(_, _), non_neg_integer(), t(_, _)) -> boolean().
is_equal(Size1, Root1, Size2, Root2) ->
    case Size1 =:= Size2 of
        true ->
            Iter2 = fwd_iterator(Root2),
            is_equal_root(Root1, Iter2) =:= [];
        %
        _ ->
            false
    end.

-spec keys(t(Key, _)) -> [Key].
keys(?INTERNAL1_MATCH(K1, _, C1, C2)) ->
    keys_recur(C1, [K1 | keys_recur(C2, [])]);
keys(?LEAF1_MATCH(K1, _)) ->
    [K1];
keys(?LEAF0) ->
    [];
keys(Root) ->
    keys_recur(Root, []).

-spec larger(Key, t(Key, Value)) -> {Key, Value} | none.
larger(Key, ?INTERNAL1_MATCH_ALL) ->
    larger_INTERNAL1(Key, ?INTERNAL1_ARGS);
larger(Key, ?LEAF1_MATCH_ALL) ->
    larger_LEAF1(Key, ?LEAF1_ARGS);
larger(_Key, ?LEAF0) ->
    none;
larger(Key, Root) ->
    larger_recur(Key, Root).

-spec largest(t(Key, Value)) -> {Key, Value}.
largest(?INTERNAL1_MATCH(_, _, _, C2)) ->
    largest_recur(C2);
largest(?LEAF1_MATCH(K1, V1)) ->
    {K1, V1};
largest(Root) ->
    largest_recur(Root).

-spec map(fun((Key, Value) -> MappedValue), t(Key, Value)) -> t(Key, MappedValue).
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
map(_Fun, ?LEAF0) ->
    ?LEAF0;
map(Fun, Root) ->
    map_recur(Fun, Root).

-spec merge(non_neg_integer(), t(KA, VA), non_neg_integer(), t(KB, VB)) ->
    nonempty_improper_list(NewSize, t(KA | KB, VA | VB))
when
    NewSize :: non_neg_integer().
merge(Size1, Root1, Size2, Root2) ->
    Fun1 = fun merge_pick_second/3,
    Fun2 = fun merge_pick_first/3,
    merge_with2(Fun1, Size1, Root1, Fun2, Size2, Root2).

-spec merge_with(MergeFun, non_neg_integer(), t(KA, VA), non_neg_integer(), t(KB, VB)) ->
    nonempty_improper_list(NewSize, t(KA | KB, MergedV))
when
    MergeFun :: fun((KA | KB, VA, VB) -> MergedV),
    NewSize :: non_neg_integer().
merge_with(Fun, Size1, Root1, Size2, Root2) ->
    Fun1 = Fun,
    Fun2 = fun(Key, Value2, Value1) -> Fun(Key, Value1, Value2) end,
    merge_with2(Fun1, Size1, Root1, Fun2, Size2, Root2).

-spec new() -> t(term(), term()).
new() ->
    ?LEAF0.

-spec next(iter(Key, Value)) -> {Key, Value, iter(Key, Value)} | none.
next([Head | Tail]) ->
    next(Head, Tail);
next([]) ->
    none.

-spec smaller(Key, t(Key, Value)) -> {Key, Value} | none.
smaller(Key, ?INTERNAL1_MATCH_ALL) ->
    smaller_INTERNAL1(Key, ?INTERNAL1_ARGS);
smaller(Key, ?LEAF1_MATCH_ALL) ->
    smaller_LEAF1(Key, ?LEAF1_ARGS);
smaller(_Key, ?LEAF0) ->
    none;
smaller(Key, Root) ->
    smaller_recur(Key, Root).

-spec smallest(t(Key, Value)) -> {Key, Value}.
smallest(?INTERNAL1_MATCH(_, _, C1, _)) ->
    smallest_recur(C1);
smallest(?LEAF1_MATCH(K1, V1)) ->
    {K1, V1};
smallest(Root) ->
    smallest_recur(Root).

-spec structural_stats(t(_, _)) -> xb5_structural_stats:t().
structural_stats(Root) ->
    Acc = xb5_structural_stats:new(),

    case Root of
        ?INTERNAL1_MATCH(_, _, C1, C2) ->
            Height = 1,
            Acc2 = xb5_structural_stats:inc_count(internal1, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            xb5_structural_stats:return(Acc4);
        %
        ?LEAF1_MATCH(_, _) ->
            Height = 1,
            Acc2 = xb5_structural_stats:inc_count(leaf1, Acc),
            Acc3 = xb5_structural_stats:set_height(Height, Acc2),
            xb5_structural_stats:return(Acc3);
        %
        ?LEAF0 ->
            xb5_structural_stats:return(Acc);
        %
        _ ->
            Height = 1,
            Acc2 = structural_stats_recur(Root, Acc, Height),
            xb5_structural_stats:return(Acc2)
    end.

-spec take_att(Key, t(Key, Value)) -> badkey | take_result(Key, Value) | no_return().
take_att(Key, ?INTERNAL1_MATCH_ALL) ->
    take_att_INTERNAL1(Key, ?INTERNAL1_ARGS);
take_att(Key, ?LEAF1_MATCH_ALL) ->
    take_att_LEAF1(Key, ?LEAF1_ARGS);
take_att(_Key, ?LEAF0) ->
    badkey;
take_att(Key, Root) ->
    take_att_recur(Key, Root).

-spec take_largest(t(Key, Value)) -> take_result(Key, Value).
take_largest(?INTERNAL1_MATCH_ALL) ->
    ?TAKEN(Taken, UpdatedC2) = take_largest_recur(C2),
    ?TAKEN(Taken, ?INTERNAL1_C2_REBALANCE(UpdatedC2));
take_largest(?LEAF1_MATCH_ALL) ->
    ?TAKEN_PAIR(K1, V1, ?LEAF0);
take_largest(Root) ->
    take_largest_recur(Root).

-spec take_smallest(t(Key, Value)) -> take_result(Key, Value).
take_smallest(?INTERNAL1_MATCH_ALL) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL1_C1_REBALANCE(UpdatedC1));
take_smallest(?LEAF1_MATCH_ALL) ->
    ?TAKEN_PAIR(K1, V1, ?LEAF0);
take_smallest(Root) ->
    take_smallest_recur(Root).

-spec to_list(t(Key, Value)) -> [{Key, Value}].
to_list(?INTERNAL1_MATCH_ALL) ->
    Acc2 = to_list_recur(C2, []),
    Acc3 = [{K1, V1} | Acc2],
    to_list_recur(C1, Acc3);
to_list(?LEAF1_MATCH_ALL) ->
    [{K1, V1}];
to_list(?LEAF0) ->
    [];
to_list(Root) ->
    to_list_recur(Root, []).

-spec to_rev_list(t(Key, Value)) -> [{Key, Value}].
to_rev_list(?INTERNAL1_MATCH_ALL) ->
    Acc2 = to_rev_list_recur(C1, []),
    Acc3 = [{K1, V1} | Acc2],
    to_rev_list_recur(C2, Acc3);
to_rev_list(?LEAF1_MATCH_ALL) ->
    [{K1, V1}];
to_rev_list(?LEAF0) ->
    [];
to_rev_list(Root) ->
    to_rev_list_recur(Root, []).

-spec update_att
    (Key, eager, Value, t(Key, _)) -> badkey | t(Key, Value);
    (Key, lazy, fun((PrevValue) -> Value), t(Key, PrevValue)) -> badkey | t(Key, Value).
update_att(Key, ValueEval, ValueWrap, ?INTERNAL1_MATCH_ALL) ->
    update_att_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
update_att(Key, ValueEval, ValueWrap, ?LEAF1_MATCH_ALL) ->
    update_att_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
update_att(_Key, _ValueEval, _ValueWrap, ?LEAF0) ->
    xb5_utils:dialyzer_opaque_term(badkey);
update_att(Key, ValueEval, ValueWrap, Root) ->
    update_att_recur(Key, ValueEval, ValueWrap, Root).

-spec values(t(_, Value)) -> [Value].
values(?INTERNAL1_MATCH(_, V1, C1, C2)) ->
    values_recur(C1, [V1 | values_recur(C2, [])]);
values(?LEAF1_MATCH(_, V1)) ->
    [V1];
values(?LEAF0) ->
    [];
values(Root) ->
    values_recur(Root, []).

%% ------------------------------------------------------------------
%% Internal Function Definitions: delete_att/2
%% ------------------------------------------------------------------

delete_att_recur(Key, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            delete_att_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            delete_att_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            delete_att_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            delete_att_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            delete_att_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            delete_att_LEAF4(Key, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, delete_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    ?GAP_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        delete_att_INTERNAL4_K1(?INTERNAL4_ARGS),
        delete_att_INTERNAL4_K2(?INTERNAL4_ARGS),
        delete_att_INTERNAL4_K3(?INTERNAL4_ARGS),
        delete_att_INTERNAL4_K4(?INTERNAL4_ARGS),
        %
        delete_att_INTERNAL4_C1(Key, ?INTERNAL4_ARGS),
        delete_att_INTERNAL4_C2(Key, ?INTERNAL4_ARGS),
        delete_att_INTERNAL4_C3(Key, ?INTERNAL4_ARGS),
        delete_att_INTERNAL4_C4(Key, ?INTERNAL4_ARGS),
        delete_att_INTERNAL4_C5(Key, ?INTERNAL4_ARGS)
    ).

-compile({inline, delete_att_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C1(Key, ?INTERNAL4_ARGS) ->
    UpdatedC1 = delete_att_recur(Key, C1),

    ?INTERNAL4_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_att_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C2(Key, ?INTERNAL4_ARGS) ->
    UpdatedC2 = delete_att_recur(Key, C2),

    ?INTERNAL4_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_att_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C3(Key, ?INTERNAL4_ARGS) ->
    UpdatedC3 = delete_att_recur(Key, C3),

    ?INTERNAL4_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_att_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C4(Key, ?INTERNAL4_ARGS) ->
    UpdatedC4 = delete_att_recur(Key, C4),

    ?INTERNAL4_C4_REBALANCE(UpdatedC4).

-compile({inline, delete_att_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C5(Key, ?INTERNAL4_ARGS) ->
    UpdatedC5 = delete_att_recur(Key, C5),

    ?INTERNAL4_C5_REBALANCE(UpdatedC5).

%%

-compile({inline, delete_att_INTERNAL4_K1 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_K1(?INTERNAL4_ARGS_IGN_K1_V1) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    del_rebalance_INTERNAL4_C2(
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

-compile({inline, delete_att_INTERNAL4_K2 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_K2(?INTERNAL4_ARGS_IGN_K2_V2) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_largest_recur(C2),

    del_rebalance_INTERNAL4_C2(
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

-compile({inline, delete_att_INTERNAL4_K3 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_K3(?INTERNAL4_ARGS_IGN_K3_V3) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_smallest_recur(C4),

    del_rebalance_INTERNAL4_C4(
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

-compile({inline, delete_att_INTERNAL4_K4 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_K4(?INTERNAL4_ARGS_IGN_K4_V4) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_largest_recur(C4),

    del_rebalance_INTERNAL4_C4(
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

-compile({inline, delete_att_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    ?GAP_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        delete_att_INTERNAL3_K1(?INTERNAL3_ARGS),
        delete_att_INTERNAL3_K2(?INTERNAL3_ARGS),
        delete_att_INTERNAL3_K3(?INTERNAL3_ARGS),
        %
        delete_att_INTERNAL3_C1(Key, ?INTERNAL3_ARGS),
        delete_att_INTERNAL3_C2(Key, ?INTERNAL3_ARGS),
        delete_att_INTERNAL3_C3(Key, ?INTERNAL3_ARGS),
        delete_att_INTERNAL3_C4(Key, ?INTERNAL3_ARGS)
    ).

-compile({inline, delete_att_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3_C1(Key, ?INTERNAL3_ARGS) ->
    UpdatedC1 = delete_att_recur(Key, C1),

    ?INTERNAL3_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_att_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3_C2(Key, ?INTERNAL3_ARGS) ->
    UpdatedC2 = delete_att_recur(Key, C2),

    ?INTERNAL3_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_att_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3_C3(Key, ?INTERNAL3_ARGS) ->
    UpdatedC3 = delete_att_recur(Key, C3),

    ?INTERNAL3_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_att_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3_C4(Key, ?INTERNAL3_ARGS) ->
    UpdatedC4 = delete_att_recur(Key, C4),

    ?INTERNAL3_C4_REBALANCE(UpdatedC4).

%%

-compile({inline, delete_att_INTERNAL3_K1 / ?INTERNAL3_ARITY}).
delete_att_INTERNAL3_K1(?INTERNAL3_ARGS_IGN_K1_V1) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    del_rebalance_INTERNAL3_C2(
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

-compile({inline, delete_att_INTERNAL3_K2 / ?INTERNAL3_ARITY}).
delete_att_INTERNAL3_K2(?INTERNAL3_ARGS_IGN_K2_V2) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) = take_smallest_recur(C3),

    del_rebalance_INTERNAL3_C3(
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

-compile({inline, delete_att_INTERNAL3_K3 / ?INTERNAL3_ARITY}).
delete_att_INTERNAL3_K3(?INTERNAL3_ARGS_IGN_K3_V3) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) = take_largest_recur(C3),

    del_rebalance_INTERNAL3_C3(
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

-compile({inline, delete_att_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
delete_att_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    ?GAP_SEARCH2(
        Key,
        K1,
        K2,
        %
        delete_att_INTERNAL2_K1(?INTERNAL2_ARGS),
        delete_att_INTERNAL2_K2(?INTERNAL2_ARGS),
        %
        delete_att_INTERNAL2_C1(Key, ?INTERNAL2_ARGS),
        delete_att_INTERNAL2_C2(Key, ?INTERNAL2_ARGS),
        delete_att_INTERNAL2_C3(Key, ?INTERNAL2_ARGS)
    ).

-compile({inline, delete_att_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
delete_att_INTERNAL2_C1(Key, ?INTERNAL2_ARGS) ->
    UpdatedC1 = delete_att_recur(Key, C1),

    ?INTERNAL2_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_att_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
delete_att_INTERNAL2_C2(Key, ?INTERNAL2_ARGS) ->
    UpdatedC2 = delete_att_recur(Key, C2),

    ?INTERNAL2_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_att_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
delete_att_INTERNAL2_C3(Key, ?INTERNAL2_ARGS) ->
    UpdatedC3 = delete_att_recur(Key, C3),

    ?INTERNAL2_C3_REBALANCE(UpdatedC3).

%%

-compile({inline, delete_att_INTERNAL2_K1 / ?INTERNAL2_ARITY}).
delete_att_INTERNAL2_K1(?INTERNAL2_ARGS_IGN_K1_V1) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    del_rebalance_INTERNAL2_C2(
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

-compile({inline, delete_att_INTERNAL2_K2 / ?INTERNAL2_ARITY}).
delete_att_INTERNAL2_K2(?INTERNAL2_ARGS_IGN_K2_V2) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_largest_recur(C2),

    del_rebalance_INTERNAL2_C2(
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

-compile({inline, delete_att_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
delete_att_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    ?GAP_SEARCH1(
        Key,
        K1,
        %
        delete_att_INTERNAL1_K1(?INTERNAL1_ARGS),
        %
        delete_att_INTERNAL1_C1(Key, ?INTERNAL1_ARGS),
        delete_att_INTERNAL1_C2(Key, ?INTERNAL1_ARGS)
    ).

-compile({inline, delete_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
delete_att_INTERNAL1_C1(Key, ?INTERNAL1_ARGS) ->
    UpdatedC1 = delete_att_recur(Key, C1),
    ?INTERNAL1_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
delete_att_INTERNAL1_C2(Key, ?INTERNAL1_ARGS) ->
    UpdatedC2 = delete_att_recur(Key, C2),
    ?INTERNAL1_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_att_INTERNAL1_K1 / ?INTERNAL1_ARITY}).
delete_att_INTERNAL1_K1(?INTERNAL1_ARGS_IGN_K1_V1) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    ?INTERNAL1_K1_C2_REBALANCE(ReplacementK, ReplacementV, UpdatedC2).

%%
%% Leaves
%%

-compile({inline, delete_att_LEAF4 / ?LEAF4_ARITY_PLUS1}).
delete_att_LEAF4(Key, ?LEAF4_ARGS) ->
    ?EXACT_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        ?new_LEAF3(K2, K3, K4, V2, V3, V4),
        ?new_LEAF3(K1, K3, K4, V1, V3, V4),
        ?new_LEAF3(K1, K2, K4, V1, V2, V4),
        ?new_LEAF3(K1, K2, K3, V1, V2, V3),
        badkey
    ).

-compile({inline, delete_att_LEAF3 / ?LEAF3_ARITY_PLUS1}).
delete_att_LEAF3(Key, ?LEAF3_ARGS) ->
    ?EXACT_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        ?new_LEAF2(K2, K3, V2, V3),
        ?new_LEAF2(K1, K3, V1, V3),
        ?new_LEAF2(K1, K2, V1, V2),
        badkey
    ).

-compile({inline, delete_att_LEAF2 / ?LEAF2_ARITY_PLUS1}).
delete_att_LEAF2(Key, ?LEAF2_ARGS) ->
    ?EXACT_SEARCH2(
        Key,
        K1,
        K2,
        ?new_LEAF1(K2, V2),
        ?new_LEAF1(K1, V1),
        badkey
    ).

-compile({inline, delete_att_LEAF1/2}).
delete_att_LEAF1(Key, K1) ->
    ?EXACT_SEARCH1(
        Key,
        K1,
        ?LEAF0,
        badkey
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: elixir_reduce/3
%% ------------------------------------------------------------------

elixir_reduce_recur(Fun, {cont, ElemAcc}, [Head | Tail]) ->
    case Head of
        ?ITER_PAIR(Key, Value) ->
            Next = Tail,
            Acc2 = Fun({Key, Value}, ElemAcc),
            elixir_reduce_recur(Fun, Acc2, Next);
        %
        Node ->
            [?ITER_PAIR(Key, Value) | Next] = fwd_iterator_recur(Node, Tail),
            Acc2 = Fun({Key, Value}, ElemAcc),
            elixir_reduce_recur(Fun, Acc2, Next)
    end;
elixir_reduce_recur(_Fun, {cont, ElemAcc}, []) ->
    {done, ElemAcc};
elixir_reduce_recur(_Fun, {halt, ElemAcc}, _) ->
    {halted, ElemAcc};
elixir_reduce_recur(Fun, {suspend, ElemAcc}, Iter) ->
    {suspended, ElemAcc, fun(Acc) -> elixir_reduce_recur(Fun, Acc, Iter) end}.

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
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            _Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3);
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3),
            _Acc5 = foldl_recur(Fun, Fun(K3, V3, Acc4), C4);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3),
            Acc5 = foldl_recur(Fun, Fun(K3, V3, Acc4), C4),
            _Acc6 = foldl_recur(Fun, Fun(K4, V4, Acc5), C5)
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
            Acc2 = foldr_recur(Fun, Acc, C3),
            Acc3 = foldr_recur(Fun, Fun(K2, V2, Acc2), C2),
            _Acc4 = foldr_recur(Fun, Fun(K1, V1, Acc3), C1);
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = foldr_recur(Fun, Acc, C4),
            Acc3 = foldr_recur(Fun, Fun(K3, V3, Acc2), C3),
            Acc4 = foldr_recur(Fun, Fun(K2, V2, Acc3), C2),
            _Acc5 = foldr_recur(Fun, Fun(K1, V1, Acc4), C1);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = foldr_recur(Fun, Acc, C5),
            Acc3 = foldr_recur(Fun, Fun(K4, V4, Acc2), C4),
            Acc4 = foldr_recur(Fun, Fun(K3, V3, Acc3), C3),
            Acc5 = foldr_recur(Fun, Fun(K2, V2, Acc4), C2),
            _Acc6 = foldr_recur(Fun, Fun(K1, V1, Acc5), C1)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: from_orddict/2
%% ------------------------------------------------------------------

from_orddict_recur(L, S, BatchOffset, BatchSize, AtRoot) when S >= 5 ->
    ChildrenBatchOffset = BatchOffset - BatchSize,
    ChildrenBatchSize = BatchSize bsr 2,

    case (S - BatchOffset) div BatchSize of
        2 ->
            S1 = S2 = BatchSize - 1,
            [S3 | S4] = from_orddict_right_children_sizes(S - (BatchSize bsl 1), BatchSize),
            from_orddict_INTERNAL3(L, S1, S2, S3, S4, ChildrenBatchOffset, ChildrenBatchSize);
        %
        3 ->
            S1 = S2 = S3 = BatchSize - 1,
            [S4 | S5] = from_orddict_right_children_sizes(S - (BatchSize * 3), BatchSize),
            from_orddict_INTERNAL4(L, S1, S2, S3, S4, S5, ChildrenBatchOffset, ChildrenBatchSize);
        %
        Quotient when Quotient =:= 1 orelse (Quotient =:= 0 andalso not AtRoot) ->
            S1 = BatchSize - 1,
            [S2 | S3] = from_orddict_right_children_sizes(S - BatchSize, BatchSize),
            from_orddict_INTERNAL2(L, S1, S2, S3, ChildrenBatchOffset, ChildrenBatchSize);
        %
        0 ->
            [S1 | S2] = from_orddict_right_children_sizes(S, BatchSize),
            from_orddict_INTERNAL1(L, S1, S2, ChildrenBatchOffset, ChildrenBatchSize)
    end;
from_orddict_recur(L, 4, _, _, _) ->
    [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4} | Next] = L,
    [?new_LEAF4(K1, K2, K3, K4, V1, V2, V3, V4) | Next];
from_orddict_recur(L, 3, _, _, _) ->
    [{K1, V1}, {K2, V2}, {K3, V3} | Next] = L,
    [?new_LEAF3(K1, K2, K3, V1, V2, V3) | Next];
from_orddict_recur(L, 2, _, _, _) ->
    [{K1, V1}, {K2, V2} | Next] = L,
    [?new_LEAF2(K1, K2, V1, V2) | Next].

from_orddict_right_children_sizes(RemainingSize, BatchSize) ->
    case RemainingSize bsr 1 < BatchSize of
        true ->
            SLeft = ((BatchSize * 3) bsr 2) - 1,
            SRight = RemainingSize - SLeft - 1,
            [SLeft | SRight];
        %
        false ->
            SLeft = BatchSize - 1,
            SRight = RemainingSize - SLeft - 1,
            [SLeft | SRight]
    end.

-compile({inline, from_orddict_INTERNAL1/5}).
from_orddict_INTERNAL1(L, S1, S2, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [{K1, V1} | L2]] = from_orddict_recur(
        L,
        S1,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C2 | []] = from_orddict_recur(L2, S2, ChildrenBatchOffset, ChildrenBatchSize, false),

    Node = ?new_INTERNAL1(K1, V1, C1, C2),

    [Node | []].

-compile({inline, from_orddict_INTERNAL2/6}).
from_orddict_INTERNAL2(L, S1, S2, S3, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [{K1, V1} | L2]] = from_orddict_recur(
        L,
        S1,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C2 | [{K2, V2} | L3]] = from_orddict_recur(
        L2,
        S2,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C3 | L4] = from_orddict_recur(L3, S3, ChildrenBatchOffset, ChildrenBatchSize, false),

    Node = ?new_INTERNAL2(K1, K2, V1, V2, C1, C2, C3),

    [Node | L4].

-compile({inline, from_orddict_INTERNAL3/7}).
from_orddict_INTERNAL3(L, S1, S2, S3, S4, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [{K1, V1} | L2]] = from_orddict_recur(
        L,
        S1,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C2 | [{K2, V2} | L3]] = from_orddict_recur(
        L2,
        S2,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C3 | [{K3, V3} | L4]] = from_orddict_recur(
        L3,
        S3,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C4 | L5] = from_orddict_recur(L4, S4, ChildrenBatchOffset, ChildrenBatchSize, false),

    Node = ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4),

    [Node | L5].

-compile({inline, from_orddict_INTERNAL4/8}).
from_orddict_INTERNAL4(L, S1, S2, S3, S4, S5, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [{K1, V1} | L2]] = from_orddict_recur(
        L,
        S1,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C2 | [{K2, V2} | L3]] = from_orddict_recur(
        L2,
        S2,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C3 | [{K3, V3} | L4]] = from_orddict_recur(
        L3,
        S3,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C4 | [{K4, V4} | L5]] = from_orddict_recur(
        L4,
        S4,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C5 | L6] = from_orddict_recur(
        L5,
        S5,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),

    Node = ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5),

    [Node | L6].

%% ------------------------------------------------------------------
%% Internal Function Definitions: get/2
%% ------------------------------------------------------------------

get_att_recur(Key, Node, Found, NotFound) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            get_att_INTERNAL2(Key, ?INTERNAL2_ARGS, Found, NotFound);
        %
        ?INTERNAL3_MATCH_ALL ->
            get_att_INTERNAL3(Key, ?INTERNAL3_ARGS, Found, NotFound);
        %
        ?INTERNAL4_MATCH_ALL ->
            get_att_INTERNAL4(Key, ?INTERNAL4_ARGS, Found, NotFound);
        %
        ?LEAF2_MATCH_ALL ->
            get_att_LEAF2(Key, ?LEAF2_ARGS, Found, NotFound);
        %
        ?LEAF3_MATCH_ALL ->
            get_att_LEAF3(Key, ?LEAF3_ARGS, Found, NotFound);
        %
        ?LEAF4_MATCH_ALL ->
            get_att_LEAF4(Key, ?LEAF4_ARGS, Found, NotFound)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, get_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS3}).
get_att_INTERNAL4(Key, ?INTERNAL4_ARGS, Found, NotFound) ->
    ?GAP_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        Found(K1, V1),
        Found(K2, V2),
        Found(K3, V3),
        Found(K4, V4),
        %
        get_att_recur(Key, C1, Found, NotFound),
        get_att_recur(Key, C2, Found, NotFound),
        get_att_recur(Key, C3, Found, NotFound),
        get_att_recur(Key, C4, Found, NotFound),
        get_att_recur(Key, C5, Found, NotFound)
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, get_att_INTERNAL3 / ?INTERNAL3_ARITY_PLUS3}).
get_att_INTERNAL3(Key, ?INTERNAL3_ARGS, Found, NotFound) ->
    ?GAP_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        Found(K1, V1),
        Found(K2, V2),
        Found(K3, V3),
        %
        get_att_recur(Key, C1, Found, NotFound),
        get_att_recur(Key, C2, Found, NotFound),
        get_att_recur(Key, C3, Found, NotFound),
        get_att_recur(Key, C4, Found, NotFound)
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, get_att_INTERNAL2 / ?INTERNAL2_ARITY_PLUS3}).
get_att_INTERNAL2(Key, ?INTERNAL2_ARGS, Found, NotFound) ->
    ?GAP_SEARCH2(
        Key,
        K1,
        K2,
        %
        Found(K1, V1),
        Found(K2, V2),
        %
        get_att_recur(Key, C1, Found, NotFound),
        get_att_recur(Key, C2, Found, NotFound),
        get_att_recur(Key, C3, Found, NotFound)
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, get_att_INTERNAL1 / ?INTERNAL1_ARITY_PLUS3}).
get_att_INTERNAL1(Key, ?INTERNAL1_ARGS, Found, NotFound) ->
    ?GAP_SEARCH1(
        Key,
        K1,
        %
        Found(K1, V1),
        %
        get_att_recur(Key, C1, Found, NotFound),
        get_att_recur(Key, C2, Found, NotFound)
    ).

%%
%% Leaves
%%

-compile({inline, get_att_LEAF4 / ?LEAF4_ARITY_PLUS3}).
get_att_LEAF4(Key, ?LEAF4_ARGS, Found, NotFound) ->
    ?EXACT_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        Found(K1, V1),
        Found(K2, V2),
        Found(K3, V3),
        Found(K4, V4),
        NotFound(Key)
    ).

-compile({inline, get_att_LEAF3 / ?LEAF3_ARITY_PLUS3}).
get_att_LEAF3(Key, ?LEAF3_ARGS, Found, NotFound) ->
    ?EXACT_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        Found(K1, V1),
        Found(K2, V2),
        Found(K3, V3),
        NotFound(Key)
    ).

-compile({inline, get_att_LEAF2 / ?LEAF2_ARITY_PLUS3}).
get_att_LEAF2(Key, ?LEAF2_ARGS, Found, NotFound) ->
    ?EXACT_SEARCH2(
        Key,
        K1,
        K2,
        %
        Found(K1, V1),
        Found(K2, V2),
        NotFound(Key)
    ).

%%
%% ?LEAF1
%%

-compile({inline, get_att_LEAF1 / ?LEAF1_ARITY_PLUS3}).
get_att_LEAF1(Key, ?LEAF1_ARGS, Found, NotFound) ->
    ?EXACT_SEARCH1(
        Key,
        K1,
        %
        Found(K1, V1),
        NotFound(Key)
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: insert_att/2
%% ------------------------------------------------------------------

insert_att_recur(Key, ValueEval, ValueWrap, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            insert_att_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            insert_att_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            insert_att_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            insert_att_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            insert_att_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH(K1, K2, K3, K4, _, _, _, _) ->
            insert_att_LEAF4(Key, ValueEval, ValueWrap, K1, K2, K3, K4)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, insert_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    ?GAP_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        key_exists,
        key_exists,
        key_exists,
        key_exists,
        %
        insert_att_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        insert_att_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        insert_att_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        insert_att_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        insert_att_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
    ).

-compile({inline, insert_att_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C1),
    ins_rebalance_INTERNAL4_C1(Result, ?INTERNAL4_ARGS).

-compile({inline, insert_att_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C2),
    ins_rebalance_INTERNAL4_C2(Result, ?INTERNAL4_ARGS).

-compile({inline, insert_att_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C3),
    ins_rebalance_INTERNAL4_C3(Result, ?INTERNAL4_ARGS).

-compile({inline, insert_att_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C4),
    ins_rebalance_INTERNAL4_C4(Result, ?INTERNAL4_ARGS).

-compile({inline, insert_att_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C5),
    ins_rebalance_INTERNAL4_C5(Result, ?INTERNAL4_ARGS).

%%
%% ?INTERNAL3
%%

-compile({inline, insert_att_INTERNAL3 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    ?GAP_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        key_exists,
        key_exists,
        key_exists,
        %
        insert_att_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS),
        insert_att_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS),
        insert_att_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS),
        insert_att_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
    ).

-compile({inline, insert_att_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C1),
    ins_rebalance_INTERNAL3_C1(Result, ?INTERNAL3_ARGS).

-compile({inline, insert_att_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C2),
    ins_rebalance_INTERNAL3_C2(Result, ?INTERNAL3_ARGS).

-compile({inline, insert_att_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C3),
    ins_rebalance_INTERNAL3_C3(Result, ?INTERNAL3_ARGS).

-compile({inline, insert_att_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C4),
    ins_rebalance_INTERNAL3_C4(Result, ?INTERNAL3_ARGS).

%%
%% ?INTERNAL2
%%

-compile({inline, insert_att_INTERNAL2 / ?INTERNAL2_ARITY_PLUS3}).
insert_att_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    ?GAP_SEARCH2(
        Key,
        K1,
        K2,
        %
        key_exists,
        key_exists,
        %
        insert_att_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS),
        insert_att_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS),
        insert_att_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS)
    ).

-compile({inline, insert_att_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS3}).
insert_att_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C1),
    ins_rebalance_INTERNAL2_C1(Result, ?INTERNAL2_ARGS).

-compile({inline, insert_att_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS3}).
insert_att_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C2),
    ins_rebalance_INTERNAL2_C2(Result, ?INTERNAL2_ARGS).

-compile({inline, insert_att_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS3}).
insert_att_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C3),
    ins_rebalance_INTERNAL2_C3(Result, ?INTERNAL2_ARGS).

%%
%% ?INTERNAL1
%%

-compile({inline, insert_att_INTERNAL1 / ?INTERNAL1_ARITY_PLUS3}).
insert_att_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    ?GAP_SEARCH1(
        Key,
        K1,
        %
        key_exists,
        %
        insert_att_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS),
        insert_att_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS)
    ).

-compile({inline, insert_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS3}).
insert_att_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C1),
    ins_rebalance_INTERNAL1_C1(Result, ?INTERNAL1_ARGS).

-compile({inline, insert_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS3}).
insert_att_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    Result = insert_att_recur(Key, ValueEval, ValueWrap, C2),
    ins_rebalance_INTERNAL1_C2(Result, ?INTERNAL1_ARGS).

%%
%% Leaves
%%

-compile({inline, insert_att_LEAF4/7}).
insert_att_LEAF4(Key, ValueEval, ValueWrap, K1, K2, K3, K4) ->
    ?GAP_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        key_exists,
        key_exists,
        key_exists,
        key_exists,
        %
        ?SPLIT(1, [Key | eval_insert_value(ValueEval, ValueWrap)]),
        ?SPLIT(2, [Key | eval_insert_value(ValueEval, ValueWrap)]),
        ?SPLIT(3, [Key | eval_insert_value(ValueEval, ValueWrap)]),
        ?SPLIT(4, [Key | eval_insert_value(ValueEval, ValueWrap)]),
        ?SPLIT(5, [Key | eval_insert_value(ValueEval, ValueWrap)])
    ).

-compile({inline, insert_att_LEAF3 / ?LEAF3_ARITY_PLUS3}).
insert_att_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    ?GAP_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        key_exists,
        key_exists,
        key_exists,
        %
        ?new_LEAF4(
            Key,
            K1,
            K2,
            K3,
            eval_insert_value(ValueEval, ValueWrap),
            V1,
            V2,
            V3
        ),
        %
        ?new_LEAF4(
            K1,
            Key,
            K2,
            K3,
            V1,
            eval_insert_value(ValueEval, ValueWrap),
            V2,
            V3
        ),
        %
        ?new_LEAF4(
            K1,
            K2,
            Key,
            K3,
            V1,
            V2,
            eval_insert_value(ValueEval, ValueWrap),
            V3
        ),
        %
        ?new_LEAF4(
            K1,
            K2,
            K3,
            Key,
            V1,
            V2,
            V3,
            eval_insert_value(ValueEval, ValueWrap)
        )
    ).

-compile({inline, insert_att_LEAF2 / ?LEAF2_ARITY_PLUS3}).
insert_att_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    ?GAP_SEARCH2(
        Key,
        K1,
        K2,
        %
        key_exists,
        key_exists,
        %
        ?new_LEAF3(
            Key,
            K1,
            K2,
            eval_insert_value(ValueEval, ValueWrap),
            V1,
            V2
        ),
        %
        ?new_LEAF3(
            K1,
            Key,
            K2,
            V1,
            eval_insert_value(ValueEval, ValueWrap),
            V2
        ),
        %
        ?new_LEAF3(
            K1,
            K2,
            Key,
            V1,
            V2,
            eval_insert_value(ValueEval, ValueWrap)
        )
    ).

-compile({inline, insert_att_LEAF1 / ?LEAF1_ARITY_PLUS3}).
insert_att_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    ?GAP_SEARCH1(
        Key,
        K1,
        %
        key_exists,
        %
        ?new_LEAF2(
            Key,
            K1,
            eval_insert_value(ValueEval, ValueWrap),
            V1
        ),
        %
        ?new_LEAF2(
            K1,
            Key,
            V1,
            eval_insert_value(ValueEval, ValueWrap)
        )
    ).

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

-spec split_internal(
    K,
    K,
    K,
    K,
    K,
    %
    V,
    V,
    V,
    V,
    V,
    %
    C,
    C,
    C,
    C,
    C,
    C
) -> split_internal_result(K, V) when
    C :: nonempty_node(K, V).
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

    {split, SplitK, SplitV, SplitL, SplitR}.

-spec split_leaf(
    K,
    K,
    K,
    K,
    K,
    %
    V,
    V,
    V,
    V,
    V
) -> split_leaf_result(K, V).
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

    {split, SplitK, SplitV, SplitL, SplitR}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: intersect/4
%% ------------------------------------------------------------------

intersect_with2(Fun1, Size1, Root1, Fun2, Size2, Root2) when Size2 < Size1 ->
    intersect_root(Fun2, Size2, Root2, Fun1, Size1, Root1);
intersect_with2(Fun1, Size1, Root1, Fun2, Size2, Root2) ->
    intersect_root(Fun1, Size1, Root1, Fun2, Size2, Root2).

intersect_root(_, 0, _, _, _, _) ->
    [0 | ?LEAF0];
intersect_root(Fun1, _Size1, Root1, Fun2, Size2, Root2) when Size2 < 10 ->
    List1 = to_rev_list(Root1),
    List2 = to_rev_list(Root2),
    intersect_2(Fun1, List1, Fun2, List2);
intersect_root(Fun1, Size1, Root1, Fun2, Size2, Root2) ->
    ThresholdSize = Size1 * round(math:log2(Size2)),

    if
        Size2 < ThresholdSize ->
            List1 = to_rev_list(Root1),
            List2 = to_rev_list(Root2),
            intersect_2(Fun1, List1, Fun2, List2);
        %
        true ->
            case Fun1 =:= fun merge_pick_second/3 of
                true ->
                    Keys1 = lists:reverse(keys(Root1)),
                    intersect_1_a(Keys1, Root2, 0, []);
                %
                _ ->
                    List1 = to_rev_list(Root1),
                    intersect_1_b(Fun1, List1, Root2, 0, [])
            end
    end.

%%

intersect_1_a([Key | Next], Root2, AccSize, Acc) ->
    case get_att(Key, Root2, fun intersect_get_found/2, fun intersect_get_not_found/1) of
        {_, _} = Pair ->
            % We want to keep the second value, which is the one already in Root
            intersect_1_a(Next, Root2, AccSize + 1, [Pair | Acc]);
        %
        nil ->
            intersect_1_a(Next, Root2, AccSize, Acc)
    end;
intersect_1_a([], _Root2, AccSize, Acc) ->
    [AccSize | from_orddict(Acc, AccSize)].

intersect_get_found(Key, Value) -> {Key, Value}.
intersect_get_not_found(_Key) -> nil.

%%

intersect_1_b(Fun1, [{Key, ValueA} | Next], Root2, AccSize, Acc) ->
    case get_att(Key, Root2, fun intersect_get_found/2, fun intersect_get_not_found/1) of
        {_, ValueB} ->
            IntersectedValue = Fun1(Key, ValueA, ValueB),
            intersect_1_b(Fun1, Next, Root2, AccSize + 1, [{Key, IntersectedValue} | Acc]);
        %
        nil ->
            intersect_1_b(Fun1, Next, Root2, AccSize, Acc)
    end;
intersect_1_b(_Fun1, [], _Root2, AccSize, Acc) ->
    [AccSize | from_orddict(Acc, AccSize)].

%%

intersect_2(Fun1, List1, Fun2, List2) ->
    intersect_2(Fun1, List1, Fun2, List2, 0, []).

intersect_2(
    Fun1,
    [{Key1, Value1} | Next1] = List1,
    Fun2,
    [{Key2, Value2} | Next2] = List2,
    AccSize,
    Acc
) ->
    if
        Key1 > Key2 ->
            intersect_2(Fun1, Next1, Fun2, List2, AccSize, Acc);
        %
        Key1 < Key2 ->
            intersect_2(Fun2, Next2, Fun1, List1, AccSize, Acc);
        %
        true ->
            IntersectedValue = Fun1(Key1, Value1, Value2),
            intersect_2(Fun1, Next1, Fun2, Next2, AccSize + 1, [{Key1, IntersectedValue} | Acc])
    end;
intersect_2(_Fun1, [], _Fun2, _, AccSize, Acc) ->
    [AccSize | from_orddict(Acc, AccSize)];
intersect_2(_Fun1, _, _Fun2, [], AccSize, Acc) ->
    [AccSize | from_orddict(Acc, AccSize)].

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - forward
%% ------------------------------------------------------------------

fwd_iterator(?INTERNAL1_MATCH_ALL) ->
    Acc = [?ITER_PAIR(K1, V1), C2],
    fwd_iterator_recur(C1, Acc);
fwd_iterator(?LEAF1_MATCH_ALL) ->
    Iter = [?ITER_PAIR(K1, V1)],
    Iter;
fwd_iterator(?LEAF0) ->
    Iter = [],
    Iter;
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
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        C2,
        ?ITER_PAIR(K2, V2),
        C3
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?INTERNAL3_MATCH_ALL, Acc) ->
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
    fwd_iterator_recur(C1, Acc2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - reverse
%% ------------------------------------------------------------------

rev_iterator(?INTERNAL1_MATCH_ALL) ->
    Acc = [?ITER_PAIR(K1, V1), C1],
    rev_iterator_recur(C2, Acc);
rev_iterator(?LEAF1_MATCH_ALL) ->
    Iter = [?ITER_PAIR(K1, V1)],
    Iter;
rev_iterator(?LEAF0) ->
    Iter = [],
    Iter;
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
    Acc2 = [
        ?ITER_PAIR(K2, V2),
        C2,
        ?ITER_PAIR(K1, V1),
        C1
        | Acc
    ],
    rev_iterator_recur(C3, Acc2);
rev_iterator_recur(?INTERNAL3_MATCH_ALL, Acc) ->
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
    rev_iterator_recur(C5, Acc2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_fwd_iterator(Key, Root) ->
    case Root of
        ?INTERNAL1_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            bound_fwd_iterator_LEAF1(Key, ?LEAF1_ARGS);
        %
        ?LEAF0 ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            bound_fwd_iterator_recur(Key, Root, Acc)
    end.

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
            bound_fwd_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc)
    end.

%% INTERNAL4

-compile({inline, bound_fwd_iterator_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc) ->
    ?SMALLER_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        % Key =< K1
        begin
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
        end,
        %
        % Key =< K2
        begin
            Acc2 = [?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4, ?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2)
        end,
        %
        % Key =< K3
        begin
            Acc2 = [?ITER_PAIR(K3, V3), C4, ?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C3, Acc2)
        end,
        %
        % Key =< K4
        begin
            Acc2 = [?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C4, Acc2)
        end,
        %
        % Key > K4
        case Acc of
            [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_fwd_iterator_recur(Key, C5, Acc)
        end
    ).

%% INTERNAL3

-compile({inline, bound_fwd_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc) ->
    ?SMALLER_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        % Key =< K1
        begin
            Acc2 = [?ITER_PAIR(K1, V1), C2, ?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C1, Acc2)
        end,
        %
        % Key =< K2
        begin
            Acc2 = [?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2)
        end,
        %
        % Key =< K3
        begin
            Acc2 = [?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C3, Acc2)
        end,
        %
        % Key > K3
        case Acc of
            [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_fwd_iterator_recur(Key, C4, Acc)
        end
    ).

%% INTERNAL2

-compile({inline, bound_fwd_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS, Acc) ->
    ?SMALLER_SEARCH2(
        Key,
        K1,
        K2,
        %
        % Key =< K1
        begin
            Acc2 = [?ITER_PAIR(K1, V1), C2, ?ITER_PAIR(K2, V2), C3 | Acc],
            bound_fwd_iterator_recur(Key, C1, Acc2)
        end,
        %
        % Key =< K2
        begin
            Acc2 = [?ITER_PAIR(K2, V2), C3 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2)
        end,
        %
        % Key > K2
        case Acc of
            [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_fwd_iterator_recur(Key, C3, Acc)
        end
    ).

%% INTERNAL1

-compile({inline, bound_fwd_iterator_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
bound_fwd_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            Acc = [?ITER_PAIR(K1, V1), C2],
            bound_fwd_iterator_recur(Key, C1, Acc);
        %
        Key > K1 ->
            Acc = [],
            bound_fwd_iterator_recur(Key, C2, Acc);
        %
        true ->
            _Acc = [?ITER_PAIR(K1, V1), C2]
    end.

%% LEAF4

-compile({inline, bound_fwd_iterator_LEAF4 / ?LEAF4_ARITY_PLUS2}).
bound_fwd_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc) ->
    ?SMALLER_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        % Key =< K1
        [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc],
        % Key =< K2
        [?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc],
        % Key =< K3
        [?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc],
        % Key =< K4
        [?ITER_PAIR(K4, V4) | Acc],
        % Key > K4
        Acc
    ).

%% LEAF3

-compile({inline, bound_fwd_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_fwd_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc) ->
    ?SMALLER_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        % Key =< K1
        [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3) | Acc],
        % Key =< K2
        [?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3) | Acc],
        % Key =< K3
        [?ITER_PAIR(K3, V3) | Acc],
        % Key > K4
        Acc
    ).

%% LEAF2

-compile({inline, bound_fwd_iterator_LEAF2 / ?LEAF2_ARITY_PLUS2}).
bound_fwd_iterator_LEAF2(Key, ?LEAF2_ARGS, Acc) ->
    ?SMALLER_SEARCH2(
        Key,
        K1,
        K2,
        % Key =< K1
        [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2) | Acc],
        % Key =< K2
        [?ITER_PAIR(K2, V2) | Acc],
        % Key > K2
        Acc
    ).

%% LEAF1

-compile({inline, bound_fwd_iterator_LEAF1 / ?LEAF1_ARITY_PLUS1}).
bound_fwd_iterator_LEAF1(Key, ?LEAF1_ARGS) ->
    ?SMALLER_SEARCH1(
        Key,
        K1,
        % Key =< K1
        [?ITER_PAIR(K1, V1)],
        % Key > K1
        []
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_rev_iterator(Key, Root) ->
    case Root of
        ?INTERNAL1_MATCH_ALL ->
            bound_rev_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            bound_rev_iterator_LEAF1(Key, ?LEAF1_ARGS);
        %
        ?LEAF0 ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            bound_rev_iterator_recur(Key, Root, Acc)
    end.

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
            bound_rev_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc)
    end.

%% INTERNAL4

-compile({inline, bound_rev_iterator_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc) ->
    ?LARGER_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        % Key < K1
        case Acc of
            [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_rev_iterator_recur(Key, C1, Acc)
        end,
        %
        % Key < K2
        begin
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2)
        end,
        %
        % Key < K3
        begin
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2)
        end,
        %
        % Key < K4
        begin
            Acc2 = [?ITER_PAIR(K3, V3), C3, ?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C4, Acc2)
        end,
        %
        % Key >= K4
        begin
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
        end
    ).

%% INTERNAL3

-compile({inline, bound_rev_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc) ->
    ?LARGER_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        % Key < K1
        case Acc of
            [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_rev_iterator_recur(Key, C1, Acc)
        end,
        %
        % Key < K2
        begin
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2)
        end,
        %
        % Key < K3
        begin
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2)
        end,
        %
        % Key >= K3
        begin
            Acc2 = [?ITER_PAIR(K3, V3), C3, ?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C4, Acc2)
        end
    ).

%% INTERNAL2

-compile({inline, bound_rev_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS, Acc) ->
    ?LARGER_SEARCH2(
        Key,
        K1,
        K2,
        %
        % Key < K1
        case Acc of
            [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_rev_iterator_recur(Key, C1, Acc)
        end,
        %
        % Key < K2
        begin
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2)
        end,
        %
        % Key >= K2
        begin
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2)
        end
    ).

%% INTERNAL1

-compile({inline, bound_rev_iterator_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
bound_rev_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key > K1 ->
            Acc = [?ITER_PAIR(K1, V1), C1],
            bound_rev_iterator_recur(Key, C2, Acc);
        %
        Key < K1 ->
            Acc = [],
            bound_rev_iterator_recur(Key, C1, Acc);
        %
        true ->
            _Acc = [?ITER_PAIR(K1, V1), C1]
    end.

%% LEAF4

-compile({inline, bound_rev_iterator_LEAF4 / ?LEAF4_ARITY_PLUS2}).
bound_rev_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc) ->
    ?LARGER_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        % Key < K1
        Acc,
        % Key < K2
        [?ITER_PAIR(K1, V1) | Acc],
        % Key < K3
        [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc],
        % Key < K4
        [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc],
        % Key >= K4
        [?ITER_PAIR(K4, V4), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc]
    ).

%% LEAF3

-compile({inline, bound_rev_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_rev_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc) ->
    ?LARGER_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        % Key < K1
        Acc,
        % Key < K2
        [?ITER_PAIR(K1, V1) | Acc],
        % Key < K3
        [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc],
        % Key >= K3
        [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc]
    ).

%% LEAF2

-compile({inline, bound_rev_iterator_LEAF2 / ?LEAF2_ARITY_PLUS2}).
bound_rev_iterator_LEAF2(Key, ?LEAF2_ARGS, Acc) ->
    ?LARGER_SEARCH2(
        Key,
        K1,
        K2,
        % Key < K1
        Acc,
        % Key < K2
        [?ITER_PAIR(K1, V1) | Acc],
        % Key >= K2
        [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc]
    ).

%% LEAF1

-compile({inline, bound_rev_iterator_LEAF1 / ?LEAF1_ARITY_PLUS1}).
bound_rev_iterator_LEAF1(Key, ?LEAF1_ARGS) ->
    ?LARGER_SEARCH1(
        Key,
        K1,
        % Key < K1
        [],
        % Key >= K1
        [?ITER_PAIR(K1, V1)]
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_equal/2
%% ------------------------------------------------------------------

is_equal_root(?INTERNAL1_MATCH_ALL, Iter) ->
    case is_equal_internal_pair(K1, V1, C1, Iter) of
        false ->
            false;
        %
        Iter2 ->
            is_equal_recur(C2, Iter2)
    end;
is_equal_root(?LEAF1_MATCH_ALL, Iter) ->
    next_pair_is_equal(K1, V1, Iter);
is_equal_root(?LEAF0, []) ->
    [];
is_equal_root(Root, Iter) ->
    is_equal_recur(Root, Iter).

is_equal_recur(?LEAF2_MATCH_ALL, Iter) ->
    is_equal_leaf_batch2(?LEAF2_ARGS, Iter);
is_equal_recur(?LEAF3_MATCH_ALL, Iter) ->
    is_equal_leaf_batch3(?LEAF3_ARGS, Iter);
is_equal_recur(?LEAF4_MATCH_ALL, Iter) ->
    case next_pair_is_equal(K1, V1, Iter) of
        false ->
            false;
        %
        Iter2 ->
            is_equal_leaf_batch3(K2, K3, K4, V2, V3, V4, Iter2)
    end;
is_equal_recur(?INTERNAL2_MATCH_ALL, Iter) ->
    is_equal_internal_batch2(K1, K2, V1, V2, C1, C2, C3, Iter);
is_equal_recur(?INTERNAL3_MATCH_ALL, Iter) ->
    is_equal_internal_batch3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4, Iter);
is_equal_recur(?INTERNAL4_MATCH_ALL, Iter) ->
    case is_equal_internal_pair(K1, V1, C1, Iter) of
        false ->
            false;
        %
        Iter2 ->
            is_equal_internal_batch3(K2, K3, K4, V2, V3, V4, C2, C3, C4, C5, Iter2)
    end.

%%%%%

-compile({inline, is_equal_internal_batch3 / ?INTERNAL3_ARITY_PLUS1}).
is_equal_internal_batch3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4, Iter) ->
    case is_equal_internal_pair(K1, V1, C1, Iter) of
        false ->
            false;
        %
        Iter2 ->
            is_equal_internal_batch2(K2, K3, V2, V3, C2, C3, C4, Iter2)
    end.

-compile({inline, is_equal_internal_batch2 / ?INTERNAL2_ARITY_PLUS1}).
is_equal_internal_batch2(K1, K2, V1, V2, C1, C2, C3, Iter) ->
    case is_equal_internal_pair(K1, V1, C1, Iter) of
        false ->
            false;
        %
        Iter2 ->
            case is_equal_internal_pair(K2, V2, C2, Iter2) of
                false ->
                    false;
                %
                Iter3 ->
                    is_equal_recur(C3, Iter3)
            end
    end.

-compile({inline, is_equal_internal_pair/4}).
is_equal_internal_pair(K1, V1, C1, Iter) ->
    case is_equal_recur(C1, Iter) of
        false ->
            false;
        %
        Iter2 ->
            next_pair_is_equal(K1, V1, Iter2)
    end.

%%%%%

-compile({inline, is_equal_leaf_batch3 / ?LEAF3_ARITY_PLUS1}).
is_equal_leaf_batch3(K1, K2, K3, V1, V2, V3, Iter) ->
    case next_pair_is_equal(K1, V1, Iter) of
        false ->
            false;
        %
        Iter2 ->
            is_equal_leaf_batch2(K2, K3, V2, V3, Iter2)
    end.

-compile({inline, is_equal_leaf_batch2 / ?LEAF2_ARITY_PLUS1}).
is_equal_leaf_batch2(K1, K2, V1, V2, Iter) ->
    case next_pair_is_equal(K1, V1, Iter) of
        false ->
            false;
        %
        Iter2 ->
            next_pair_is_equal(K2, V2, Iter2)
    end.

%%%%%%

next_pair_is_equal(KeyA, ValueA, [Head | Tail]) ->
    case Head of
        ?ITER_PAIR(KeyB, ValueB) ->
            KeyA == KeyB andalso ValueA =:= ValueB andalso Tail;
        %
        Node ->
            [?ITER_PAIR(KeyB, ValueB) | NewTail] = fwd_iterator_recur(Node, Tail),
            KeyA == KeyB andalso ValueA =:= ValueB andalso NewTail
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
            keys_recur(C1, Acc5)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: larger/2
%% ------------------------------------------------------------------

larger_recur(Key, Node) ->
    case Node of
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
            larger_LEAF4(Key, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, larger_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
larger_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    ?LARGER_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        % Key < K1
        case larger_recur(Key, C1) of
            none -> {K1, V1};
            Found -> Found
        end,
        %
        % Key < K2
        case larger_recur(Key, C2) of
            none -> {K2, V2};
            Found -> Found
        end,
        %
        % Key < K3
        case larger_recur(Key, C3) of
            none -> {K3, V3};
            Found -> Found
        end,
        %
        % Key < K4
        case larger_recur(Key, C4) of
            none -> {K4, V4};
            Found -> Found
        end,
        %
        % Key >= K4
        larger_recur(Key, C5)
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, larger_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
larger_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    ?LARGER_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        % Key < K1
        case larger_recur(Key, C1) of
            none -> {K1, V1};
            Found -> Found
        end,
        %
        % Key < K2
        case larger_recur(Key, C2) of
            none -> {K2, V2};
            Found -> Found
        end,
        %
        % Key < K3
        case larger_recur(Key, C3) of
            none -> {K3, V3};
            Found -> Found
        end,
        %
        % Key >= K3
        larger_recur(Key, C4)
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, larger_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
larger_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    ?LARGER_SEARCH2(
        Key,
        K1,
        K2,
        %
        % Key < K1
        case larger_recur(Key, C1) of
            none -> {K1, V1};
            Found -> Found
        end,
        %
        % Key < K2
        case larger_recur(Key, C2) of
            none -> {K2, V2};
            Found -> Found
        end,
        %
        % Key >= K2
        larger_recur(Key, C3)
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, larger_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
larger_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    ?LARGER_SEARCH1(
        Key,
        K1,
        %
        % Key < K1
        case larger_recur(Key, C1) of
            none -> {K1, V1};
            Found -> Found
        end,
        %
        % Key >= K1
        larger_recur(Key, C2)
    ).

%%
%% Leaves
%%

-compile({inline, larger_LEAF4 / ?LEAF4_ARITY_PLUS1}).
larger_LEAF4(Key, ?LEAF4_ARGS) ->
    ?LARGER_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        % Key < K1
        {K1, V1},
        % Key < K2
        {K2, V2},
        % Key < K3
        {K3, V3},
        % Key < K4
        {K4, V4},
        % Key >= K4
        none
    ).

-compile({inline, larger_LEAF3 / ?LEAF3_ARITY_PLUS1}).
larger_LEAF3(Key, ?LEAF3_ARGS) ->
    ?LARGER_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        % Key < K1
        {K1, V1},
        % Key < K2
        {K2, V2},
        % Key < K3
        {K3, V3},
        % Key >= K3
        none
    ).

-compile({inline, larger_LEAF2 / ?LEAF2_ARITY_PLUS1}).
larger_LEAF2(Key, ?LEAF2_ARGS) ->
    ?LARGER_SEARCH2(
        Key,
        K1,
        K2,
        % Key < K1
        {K1, V1},
        % Key < K2
        {K2, V2},
        % Key >= K2
        none
    ).

-compile({inline, larger_LEAF1 / ?LEAF1_ARITY_PLUS1}).
larger_LEAF1(Key, ?LEAF1_ARGS) ->
    ?LARGER_SEARCH1(
        Key,
        K1,
        % Key < K1
        {K1, V1},
        % Key >= K1
        none
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: largest/1
%% ------------------------------------------------------------------

largest_recur(Node) ->
    case Node of
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
%% Internal Function Definitions: merge/2
%% ------------------------------------------------------------------

merge_pick_first(_Key, ValueA, _ValueB) -> ValueA.

merge_pick_second(_Key, _ValueA, ValueB) -> ValueB.

%%

merge_with2(Fun1, Size1, Root1, Fun2, Size2, Root2) when Size2 < Size1 ->
    merge_root(Fun2, Size2, Root2, Fun1, Size1, Root1);
merge_with2(Fun1, Size1, Root1, Fun2, Size2, Root2) ->
    merge_root(Fun1, Size1, Root1, Fun2, Size2, Root2).

merge_root(_, 0, _, _, Size2, Root2) ->
    [Size2 | Root2];
merge_root(Fun1, Size1, Root1, Fun2, Size2, Root2) when Size2 < 10 ->
    List1 = to_rev_list(Root1),
    List2 = to_rev_list(Root2),
    merge_2(Fun1, List1, Fun2, List2, Size1 + Size2);
merge_root(Fun1, Size1, Root1, Fun2, Size2, Root2) ->
    ThresholdSize = Size1 * round(math:log2(Size2)),

    if
        Size2 < ThresholdSize ->
            List1 = to_rev_list(Root1),
            List2 = to_rev_list(Root2),
            merge_2(Fun1, List1, Fun2, List2, Size1 + Size2);
        %
        true ->
            case Fun1 =:= fun merge_pick_second/3 of
                true ->
                    List1 = to_rev_list(Root1),
                    merge_1_a(List1, Size2, Root2);
                %
                _ ->
                    List1 = to_rev_list(Root1),
                    merge_1_b(Fun1, List1, Size2, Root2)
            end
    end.

%%

merge_1_a([{Key, Value} | Next], Size2, Root2) ->
    case insert_att(Key, eager, Value, Root2) of
        key_exists ->
            % We want to keep the second value, which is the one already in Root
            merge_1_a(Next, Size2, Root2);
        %
        UpdatedRoot2 ->
            merge_1_a(Next, Size2 + 1, UpdatedRoot2)
    end;
merge_1_a([], Size2, Root2) ->
    [Size2 | Root2].

%%

merge_1_b(Fun1, [{Key, ValueA} | Next], Size2, Root2) ->
    case insert_att(Key, eager, ValueA, Root2) of
        key_exists ->
            ValueB = get(Key, Root2),
            MergeValue = Fun1(Key, ValueA, ValueB),
            UpdatedRoot2 = update_att(Key, eager, MergeValue, Root2),
            merge_1_b(Fun1, Next, Size2, UpdatedRoot2);
        %
        UpdatedRoot2 ->
            merge_1_b(Fun1, Next, Size2 + 1, UpdatedRoot2)
    end;
merge_1_b(_Fun1, [], Size2, Root2) ->
    [Size2 | Root2].

get(Key, Root2) ->
    get_att(Key, Root2, fun get_found/2, fun get_not_found/1).

get_found(_Key, Value) ->
    Value.

-spec get_not_found(_) -> no_return().
get_not_found(_Key) ->
    error('This path is supposed to be unreachable').

%%

merge_2(Fun1, List1, Fun2, List2, AccSize) ->
    merge_2(Fun1, List1, Fun2, List2, AccSize, []).

merge_2(
    Fun1,
    [{Key1, Value1} = Pair1 | Next1] = List1,
    Fun2,
    [{Key2, Value2} = Pair2 | Next2] = List2,
    AccSize,
    Acc
) ->
    if
        Key1 > Key2 ->
            merge_2(Fun1, Next1, Fun2, List2, AccSize, [Pair1 | Acc]);
        %
        Key1 < Key2 ->
            merge_2(Fun2, Next2, Fun1, List1, AccSize, [Pair2 | Acc]);
        %
        true ->
            MergeValue = Fun1(Key1, Value1, Value2),
            merge_2(Fun1, Next1, Fun2, Next2, AccSize - 1, [{Key1, MergeValue} | Acc])
    end;
merge_2(_Fun1, [], _Fun2, List2, AccSize, Acc) ->
    FinalAcc = lists:reverse(List2, Acc),
    [AccSize | from_orddict(FinalAcc, AccSize)];
merge_2(_Fun1, List1, _Fun2, [], AccSize, Acc) ->
    FinalAcc = lists:reverse(List1, Acc),
    [AccSize | from_orddict(FinalAcc, AccSize)].

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
    ?SMALLER_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        % Key =< K1
        smaller_recur(Key, C1),
        %
        % Key =< K2
        case smaller_recur(Key, C2) of
            none -> {K1, V1};
            Found -> Found
        end,
        %
        % Key =< K3
        case smaller_recur(Key, C3) of
            none -> {K2, V2};
            Found -> Found
        end,
        %
        % Key =< K4
        case smaller_recur(Key, C4) of
            none -> {K3, V3};
            Found -> Found
        end,
        %
        % Key > K4
        case smaller_recur(Key, C5) of
            none -> {K4, V4};
            Found -> Found
        end
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, smaller_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
smaller_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    ?SMALLER_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        % Key =< K1
        smaller_recur(Key, C1),
        %
        % Key =< K2
        case smaller_recur(Key, C2) of
            none -> {K1, V1};
            Found -> Found
        end,
        %
        % Key =< K3
        case smaller_recur(Key, C3) of
            none -> {K2, V2};
            Found -> Found
        end,
        %
        % Key > K3
        case smaller_recur(Key, C4) of
            none -> {K3, V3};
            Found -> Found
        end
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, smaller_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
smaller_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    ?SMALLER_SEARCH2(
        Key,
        K1,
        K2,
        %
        % Key =< K1
        smaller_recur(Key, C1),
        %
        % Key =< K2
        case smaller_recur(Key, C2) of
            none -> {K1, V1};
            Found -> Found
        end,
        %
        % Key > K2
        case smaller_recur(Key, C3) of
            none -> {K2, V2};
            Found -> Found
        end
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, smaller_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
smaller_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    ?SMALLER_SEARCH1(
        Key,
        K1,
        %
        % Key =< K1
        smaller_recur(Key, C1),
        %
        % Key > K1
        case smaller_recur(Key, C2) of
            none -> {K1, V1};
            Found -> Found
        end
    ).

%%
%% ?LEAF4
%%

-compile({inline, smaller_LEAF4 / ?LEAF4_ARITY_PLUS1}).
smaller_LEAF4(Key, ?LEAF4_ARGS) ->
    ?SMALLER_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        % Key =< K1
        none,
        % Key =< K2
        {K1, V1},
        % Key =< K3
        {K2, V2},
        % Key =< K4
        {K3, V3},
        % Key > K4
        {K4, V4}
    ).

%%
%% ?LEAF3
%%

-compile({inline, smaller_LEAF3 / ?LEAF3_ARITY_PLUS1}).
smaller_LEAF3(Key, ?LEAF3_ARGS) ->
    ?SMALLER_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        % Key =< K1
        none,
        % Key =< K2
        {K1, V1},
        % Key =< K3
        {K2, V2},
        % Key > K3
        {K3, V3}
    ).

%%
%% ?LEAF2
%%

-compile({inline, smaller_LEAF2 / ?LEAF2_ARITY_PLUS1}).
smaller_LEAF2(Key, ?LEAF2_ARGS) ->
    ?SMALLER_SEARCH2(
        Key,
        K1,
        K2,
        % Key =< K1
        none,
        % Key =< K2
        {K1, V1},
        % Key > K2
        {K2, V2}
    ).

%%
%% ?LEAF1
%%

-compile({inline, smaller_LEAF1 / ?LEAF1_ARITY_PLUS1}).
smaller_LEAF1(Key, ?LEAF1_ARGS) ->
    ?SMALLER_SEARCH1(
        Key,
        K1,
        % Key =< K1
        none,
        % Key > K1
        {K1, V1}
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: smallest/1
%% ------------------------------------------------------------------

smallest_recur(Node) ->
    case Node of
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
            Acc2 = xb5_structural_stats:inc_count(leaf2, Acc),
            xb5_structural_stats:set_height(Height, Acc2);
        %
        ?LEAF3_MATCH(_, _, _, _, _, _) ->
            Acc2 = xb5_structural_stats:inc_count(leaf3, Acc),
            xb5_structural_stats:set_height(Height, Acc2);
        %
        ?LEAF4_MATCH(_, _, _, _, _, _, _, _) ->
            Acc2 = xb5_structural_stats:inc_count(leaf4, Acc),
            xb5_structural_stats:set_height(Height, Acc2);
        %
        ?INTERNAL2_MATCH(_, _, _, _, C1, C2, C3) ->
            Acc2 = xb5_structural_stats:inc_count(internal2, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            _Acc5 = structural_stats_recur(C3, Acc4, Height + 1);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, C1, C2, C3, C4) ->
            Acc2 = xb5_structural_stats:inc_count(internal3, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            _Acc6 = structural_stats_recur(C4, Acc5, Height + 1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, C1, C2, C3, C4, C5) ->
            Acc2 = xb5_structural_stats:inc_count(internal4, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            Acc6 = structural_stats_recur(C4, Acc5, Height + 1),
            _Acc7 = structural_stats_recur(C5, Acc6, Height + 1)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_att/2
%% ------------------------------------------------------------------

take_att_recur(Key, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            take_att_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            take_att_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_att_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            take_att_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            take_att_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            take_att_LEAF4(Key, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, take_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    ?GAP_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        take_att_INTERNAL4_K1(?INTERNAL4_ARGS),
        take_att_INTERNAL4_K2(?INTERNAL4_ARGS),
        take_att_INTERNAL4_K3(?INTERNAL4_ARGS),
        take_att_INTERNAL4_K4(?INTERNAL4_ARGS),
        %
        take_att_INTERNAL4_C1(Key, ?INTERNAL4_ARGS),
        take_att_INTERNAL4_C2(Key, ?INTERNAL4_ARGS),
        take_att_INTERNAL4_C3(Key, ?INTERNAL4_ARGS),
        take_att_INTERNAL4_C4(Key, ?INTERNAL4_ARGS),
        take_att_INTERNAL4_C5(Key, ?INTERNAL4_ARGS)
    ).

-compile({inline, take_att_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C1(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C1) of
        ?TAKEN(Pair, UpdatedC1) ->
            ?TAKEN(Pair, ?INTERNAL4_C1_REBALANCE(UpdatedC1));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C2(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C2) of
        ?TAKEN(Pair, UpdatedC2) ->
            ?TAKEN(Pair, ?INTERNAL4_C2_REBALANCE(UpdatedC2));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C3(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C3) of
        ?TAKEN(Pair, UpdatedC3) ->
            ?TAKEN(Pair, ?INTERNAL4_C3_REBALANCE(UpdatedC3));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C4(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C4) of
        ?TAKEN(Pair, UpdatedC4) ->
            ?TAKEN(Pair, ?INTERNAL4_C4_REBALANCE(UpdatedC4));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C5(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C5) of
        ?TAKEN(Pair, UpdatedC5) ->
            ?TAKEN(Pair, ?INTERNAL4_C5_REBALANCE(UpdatedC5));
        %
        badkey ->
            badkey
    end.

%%

-compile({inline, take_att_INTERNAL4_K1 / ?INTERNAL4_ARITY}).
take_att_INTERNAL4_K1(?INTERNAL4_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_att_INTERNAL4_K1(?INTERNAL4_ARGS)).

-compile({inline, take_att_INTERNAL4_K2 / ?INTERNAL4_ARITY}).
take_att_INTERNAL4_K2(?INTERNAL4_ARGS) ->
    ?TAKEN_PAIR(K2, V2, delete_att_INTERNAL4_K2(?INTERNAL4_ARGS)).

-compile({inline, take_att_INTERNAL4_K3 / ?INTERNAL4_ARITY}).
take_att_INTERNAL4_K3(?INTERNAL4_ARGS) ->
    ?TAKEN_PAIR(K3, V3, delete_att_INTERNAL4_K3(?INTERNAL4_ARGS)).

-compile({inline, take_att_INTERNAL4_K4 / ?INTERNAL4_ARITY}).
take_att_INTERNAL4_K4(?INTERNAL4_ARGS) ->
    ?TAKEN_PAIR(K4, V4, delete_att_INTERNAL4_K4(?INTERNAL4_ARGS)).

%%
%% ?INTERNAL3
%%

-compile({inline, take_att_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
take_att_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    ?GAP_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        take_att_INTERNAL3_K1(?INTERNAL3_ARGS),
        take_att_INTERNAL3_K2(?INTERNAL3_ARGS),
        take_att_INTERNAL3_K3(?INTERNAL3_ARGS),
        %
        take_att_INTERNAL3_C1(Key, ?INTERNAL3_ARGS),
        take_att_INTERNAL3_C2(Key, ?INTERNAL3_ARGS),
        take_att_INTERNAL3_C3(Key, ?INTERNAL3_ARGS),
        take_att_INTERNAL3_C4(Key, ?INTERNAL3_ARGS)
    ).

-compile({inline, take_att_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
take_att_INTERNAL3_C1(Key, ?INTERNAL3_ARGS) ->
    case take_att_recur(Key, C1) of
        ?TAKEN(Pair, UpdatedC1) ->
            ?TAKEN(Pair, ?INTERNAL3_C1_REBALANCE(UpdatedC1));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
take_att_INTERNAL3_C2(Key, ?INTERNAL3_ARGS) ->
    case take_att_recur(Key, C2) of
        ?TAKEN(Pair, UpdatedC2) ->
            ?TAKEN(Pair, ?INTERNAL3_C2_REBALANCE(UpdatedC2));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
take_att_INTERNAL3_C3(Key, ?INTERNAL3_ARGS) ->
    case take_att_recur(Key, C3) of
        ?TAKEN(Pair, UpdatedC3) ->
            ?TAKEN(Pair, ?INTERNAL3_C3_REBALANCE(UpdatedC3));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
take_att_INTERNAL3_C4(Key, ?INTERNAL3_ARGS) ->
    case take_att_recur(Key, C4) of
        ?TAKEN(Pair, UpdatedC4) ->
            ?TAKEN(Pair, ?INTERNAL3_C4_REBALANCE(UpdatedC4));
        %
        badkey ->
            badkey
    end.

%%

-compile({inline, take_att_INTERNAL3_K1 / ?INTERNAL3_ARITY}).
take_att_INTERNAL3_K1(?INTERNAL3_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_att_INTERNAL3_K1(?INTERNAL3_ARGS)).

-compile({inline, take_att_INTERNAL3_K2 / ?INTERNAL3_ARITY}).
take_att_INTERNAL3_K2(?INTERNAL3_ARGS) ->
    ?TAKEN_PAIR(K2, V2, delete_att_INTERNAL3_K2(?INTERNAL3_ARGS)).

-compile({inline, take_att_INTERNAL3_K3 / ?INTERNAL3_ARITY}).
take_att_INTERNAL3_K3(?INTERNAL3_ARGS) ->
    ?TAKEN_PAIR(K3, V3, delete_att_INTERNAL3_K3(?INTERNAL3_ARGS)).

%%
%% ?INTERNAL2
%%

-compile({inline, take_att_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
take_att_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    ?GAP_SEARCH2(
        Key,
        K1,
        K2,
        %
        take_att_INTERNAL2_K1(?INTERNAL2_ARGS),
        take_att_INTERNAL2_K2(?INTERNAL2_ARGS),
        %
        take_att_INTERNAL2_C1(Key, ?INTERNAL2_ARGS),
        take_att_INTERNAL2_C2(Key, ?INTERNAL2_ARGS),
        take_att_INTERNAL2_C3(Key, ?INTERNAL2_ARGS)
    ).

-compile({inline, take_att_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
take_att_INTERNAL2_C1(Key, ?INTERNAL2_ARGS) ->
    case take_att_recur(Key, C1) of
        ?TAKEN(Pair, UpdatedC1) ->
            ?TAKEN(Pair, ?INTERNAL2_C1_REBALANCE(UpdatedC1));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
take_att_INTERNAL2_C2(Key, ?INTERNAL2_ARGS) ->
    case take_att_recur(Key, C2) of
        ?TAKEN(Pair, UpdatedC2) ->
            ?TAKEN(Pair, ?INTERNAL2_C2_REBALANCE(UpdatedC2));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
take_att_INTERNAL2_C3(Key, ?INTERNAL2_ARGS) ->
    case take_att_recur(Key, C3) of
        ?TAKEN(Pair, UpdatedC3) ->
            ?TAKEN(Pair, ?INTERNAL2_C3_REBALANCE(UpdatedC3));
        %
        badkey ->
            badkey
    end.

%%

-compile({inline, take_att_INTERNAL2_K1 / ?INTERNAL2_ARITY}).
take_att_INTERNAL2_K1(?INTERNAL2_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_att_INTERNAL2_K1(?INTERNAL2_ARGS)).

-compile({inline, take_att_INTERNAL2_K2 / ?INTERNAL2_ARITY}).
take_att_INTERNAL2_K2(?INTERNAL2_ARGS) ->
    ?TAKEN_PAIR(K2, V2, delete_att_INTERNAL2_K2(?INTERNAL2_ARGS)).

%%
%% ?INTERNAL1
%%

-compile({inline, take_att_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
take_att_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    ?GAP_SEARCH1(
        Key,
        K1,
        %
        take_att_INTERNAL1_K1(?INTERNAL1_ARGS),
        %
        take_att_INTERNAL1_C1(Key, ?INTERNAL1_ARGS),
        take_att_INTERNAL1_C2(Key, ?INTERNAL1_ARGS)
    ).

-compile({inline, take_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
take_att_INTERNAL1_C1(Key, ?INTERNAL1_ARGS) ->
    case take_att_recur(Key, C1) of
        ?TAKEN(Pair, UpdatedC1) ->
            ?TAKEN(Pair, ?INTERNAL1_C1_REBALANCE(UpdatedC1));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
take_att_INTERNAL1_C2(Key, ?INTERNAL1_ARGS) ->
    case take_att_recur(Key, C2) of
        ?TAKEN(Pair, UpdatedC2) ->
            ?TAKEN(Pair, ?INTERNAL1_C2_REBALANCE(UpdatedC2));
        %
        badkey ->
            badkey
    end.

-compile({inline, take_att_INTERNAL1_K1 / ?INTERNAL1_ARITY}).
take_att_INTERNAL1_K1(?INTERNAL1_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_att_INTERNAL1_K1(?INTERNAL1_ARGS)).

%%
%% Leaves
%%

-compile({inline, take_att_LEAF4 / ?LEAF4_ARITY_PLUS1}).
take_att_LEAF4(Key, ?LEAF4_ARGS) ->
    ?EXACT_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        ?TAKEN_PAIR(K1, V1, ?new_LEAF3(K2, K3, K4, V2, V3, V4)),
        ?TAKEN_PAIR(K2, V2, ?new_LEAF3(K1, K3, K4, V1, V3, V4)),
        ?TAKEN_PAIR(K3, V3, ?new_LEAF3(K1, K2, K4, V1, V2, V4)),
        ?TAKEN_PAIR(K4, V4, ?new_LEAF3(K1, K2, K3, V1, V2, V3)),
        badkey
    ).

%%
%% ?LEAF3
%%

-compile({inline, take_att_LEAF3 / ?LEAF3_ARITY_PLUS1}).
take_att_LEAF3(Key, ?LEAF3_ARGS) ->
    ?EXACT_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        ?TAKEN_PAIR(K1, V1, ?new_LEAF2(K2, K3, V2, V3)),
        ?TAKEN_PAIR(K2, V2, ?new_LEAF2(K1, K3, V1, V3)),
        ?TAKEN_PAIR(K3, V3, ?new_LEAF2(K1, K2, V1, V2)),
        badkey
    ).

-compile({inline, take_att_LEAF2 / ?LEAF2_ARITY_PLUS1}).
take_att_LEAF2(Key, ?LEAF2_ARGS) ->
    ?EXACT_SEARCH2(
        Key,
        K1,
        K2,
        ?TAKEN_PAIR(K1, V1, ?new_LEAF1(K2, V2)),
        ?TAKEN_PAIR(K2, V2, ?new_LEAF1(K1, V1)),
        badkey
    ).

-compile({inline, take_att_LEAF1 / ?LEAF1_ARITY_PLUS1}).
take_att_LEAF1(Key, ?LEAF1_ARGS) ->
    ?EXACT_SEARCH1(
        Key,
        K1,
        ?TAKEN_PAIR(K1, V1, ?LEAF0),
        badkey
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_largest/2
%% ------------------------------------------------------------------

take_largest_recur(Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC3) = take_largest_recur(C3),
            ?TAKEN(Taken, ?INTERNAL2_C3_REBALANCE(UpdatedC3));
        %
        ?INTERNAL3_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC4) = take_largest_recur(C4),
            ?TAKEN(Taken, ?INTERNAL3_C4_REBALANCE(UpdatedC4));
        %
        ?INTERNAL4_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC5) = take_largest_recur(C5),
            ?TAKEN(Taken, ?INTERNAL4_C5_REBALANCE(UpdatedC5));
        %
        ?LEAF2_MATCH_ALL ->
            ?TAKEN_PAIR(K2, V2, ?new_LEAF1(K1, V1));
        %
        ?LEAF3_MATCH_ALL ->
            ?TAKEN_PAIR(K3, V3, ?new_LEAF2(K1, K2, V1, V2));
        %
        ?LEAF4_MATCH_ALL ->
            ?TAKEN_PAIR(K4, V4, ?new_LEAF3(K1, K2, K3, V1, V2, V3))
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_smallest/2
%% ------------------------------------------------------------------

take_smallest_recur(Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
            ?TAKEN(Taken, ?INTERNAL2_C1_REBALANCE(UpdatedC1));
        %
        ?INTERNAL3_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
            ?TAKEN(Taken, ?INTERNAL3_C1_REBALANCE(UpdatedC1));
        %
        ?INTERNAL4_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
            ?TAKEN(Taken, ?INTERNAL4_C1_REBALANCE(UpdatedC1));
        %
        ?LEAF2_MATCH_ALL ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF1(K2, V2));
        %
        ?LEAF3_MATCH_ALL ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF2(K2, K3, V2, V3));
        %
        ?LEAF4_MATCH_ALL ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF3(K2, K3, K4, V2, V3, V4))
    end.

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
            Acc2 = to_list_recur(C3, Acc),
            Acc3 = to_list_recur(C2, [{K2, V2} | Acc2]),
            _Acc4 = to_list_recur(C1, [{K1, V1} | Acc3]);
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = to_list_recur(C4, Acc),
            Acc3 = to_list_recur(C3, [{K3, V3} | Acc2]),
            Acc4 = to_list_recur(C2, [{K2, V2} | Acc3]),
            _Acc5 = to_list_recur(C1, [{K1, V1} | Acc4]);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = to_list_recur(C5, Acc),
            Acc3 = to_list_recur(C4, [{K4, V4} | Acc2]),
            Acc4 = to_list_recur(C3, [{K3, V3} | Acc3]),
            Acc5 = to_list_recur(C2, [{K2, V2} | Acc4]),
            _Acc6 = to_list_recur(C1, [{K1, V1} | Acc5])
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: to_rev_list/1
%% ------------------------------------------------------------------

to_rev_list_recur(Node, Acc) ->
    % TODO test
    case Node of
        ?LEAF2_MATCH_ALL ->
            [{K2, V2}, {K1, V1} | Acc];
        %
        ?LEAF3_MATCH_ALL ->
            [{K3, V3}, {K2, V2}, {K1, V1} | Acc];
        %
        ?LEAF4_MATCH_ALL ->
            [{K4, V4}, {K3, V3}, {K2, V2}, {K1, V1} | Acc];
        %
        ?INTERNAL2_MATCH_ALL ->
            Acc2 = to_rev_list_recur(C1, Acc),
            Acc3 = to_rev_list_recur(C2, [{K1, V1} | Acc2]),
            _Acc4 = to_rev_list_recur(C3, [{K2, V2} | Acc3]);
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = to_rev_list_recur(C1, Acc),
            Acc3 = to_rev_list_recur(C2, [{K1, V1} | Acc2]),
            Acc4 = to_rev_list_recur(C3, [{K2, V2} | Acc3]),
            _Acc5 = to_rev_list_recur(C4, [{K3, V3} | Acc4]);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = to_rev_list_recur(C1, Acc),
            Acc3 = to_rev_list_recur(C2, [{K1, V1} | Acc2]),
            Acc4 = to_rev_list_recur(C3, [{K2, V2} | Acc3]),
            Acc5 = to_rev_list_recur(C4, [{K3, V3} | Acc4]),
            _Acc6 = to_rev_list_recur(C5, [{K4, V4} | Acc5])
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: update_att/2
%% ------------------------------------------------------------------

update_att_recur(Key, ValueEval, ValueWrap, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            update_att_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            update_att_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            update_att_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            update_att_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            update_att_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            update_att_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, update_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    ?GAP_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        update_att_INTERNAL4_K1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        update_att_INTERNAL4_K2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        update_att_INTERNAL4_K3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        update_att_INTERNAL4_K4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        %
        update_att_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        update_att_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        update_att_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        update_att_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS),
        update_att_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
    ).

-compile({inline, update_att_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C1) of
        badkey ->
            badkey;
        %
        UpdatedC1 ->
            ?INTERNAL4_UPD_C1(UpdatedC1)
    end.

-compile({inline, update_att_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C2) of
        badkey ->
            badkey;
        %
        UpdatedC2 ->
            ?INTERNAL4_UPD_C2(UpdatedC2)
    end.

-compile({inline, update_att_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C3) of
        badkey ->
            badkey;
        %
        UpdatedC3 ->
            ?INTERNAL4_UPD_C3(UpdatedC3)
    end.

-compile({inline, update_att_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C4) of
        badkey ->
            badkey;
        %
        UpdatedC4 ->
            ?INTERNAL4_UPD_C4(UpdatedC4)
    end.

-compile({inline, update_att_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C5) of
        badkey ->
            badkey;
        %
        UpdatedC5 ->
            ?INTERNAL4_UPD_C5(UpdatedC5)
    end.

%%

-compile({inline, update_att_INTERNAL4_K1 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_K1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K1) ->
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

-compile({inline, update_att_INTERNAL4_K2 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_K2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K2) ->
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

-compile({inline, update_att_INTERNAL4_K3 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_K3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K3) ->
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

-compile({inline, update_att_INTERNAL4_K4 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_K4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K4) ->
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

-compile({inline, update_att_INTERNAL3 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    ?GAP_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        update_att_INTERNAL3_K1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS),
        update_att_INTERNAL3_K2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS),
        update_att_INTERNAL3_K3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS),
        %
        update_att_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS),
        update_att_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS),
        update_att_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS),
        update_att_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
    ).

-compile({inline, update_att_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C1) of
        badkey ->
            badkey;
        %
        UpdatedC1 ->
            ?INTERNAL3_UPD_C1(UpdatedC1)
    end.

-compile({inline, update_att_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C2) of
        badkey ->
            badkey;
        %
        UpdatedC2 ->
            ?INTERNAL3_UPD_C2(UpdatedC2)
    end.

-compile({inline, update_att_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C3) of
        badkey ->
            badkey;
        %
        UpdatedC3 ->
            ?INTERNAL3_UPD_C3(UpdatedC3)
    end.

-compile({inline, update_att_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C4) of
        badkey ->
            badkey;
        %
        UpdatedC4 ->
            ?INTERNAL3_UPD_C4(UpdatedC4)
    end.

%%

-compile({inline, update_att_INTERNAL3_K1 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_K1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS_IGN_K1) ->
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

-compile({inline, update_att_INTERNAL3_K2 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_K2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS_IGN_K2) ->
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

-compile({inline, update_att_INTERNAL3_K3 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_K3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS_IGN_K3) ->
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

-compile({inline, update_att_INTERNAL2 / ?INTERNAL2_ARITY_PLUS3}).
update_att_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    ?GAP_SEARCH2(
        Key,
        K1,
        K2,
        %
        update_att_INTERNAL2_K1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS),
        update_att_INTERNAL2_K2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS),
        %
        update_att_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS),
        update_att_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS),
        update_att_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS)
    ).

-compile({inline, update_att_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS3}).
update_att_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C1) of
        badkey ->
            badkey;
        %
        UpdatedC1 ->
            ?INTERNAL2_UPD_C1(UpdatedC1)
    end.

-compile({inline, update_att_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS3}).
update_att_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C2) of
        badkey ->
            badkey;
        %
        UpdatedC2 ->
            ?INTERNAL2_UPD_C2(UpdatedC2)
    end.

-compile({inline, update_att_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS3}).
update_att_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C3) of
        badkey ->
            badkey;
        %
        UpdatedC3 ->
            ?INTERNAL2_UPD_C3(UpdatedC3)
    end.

%%

-compile({inline, update_att_INTERNAL2_K1 / ?INTERNAL2_ARITY_PLUS3}).
update_att_INTERNAL2_K1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS_IGN_K1) ->
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

-compile({inline, update_att_INTERNAL2_K2 / ?INTERNAL2_ARITY_PLUS3}).
update_att_INTERNAL2_K2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS_IGN_K2) ->
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

-compile({inline, update_att_INTERNAL1 / ?INTERNAL1_ARITY_PLUS3}).
update_att_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    ?GAP_SEARCH1(
        Key,
        K1,
        %
        update_att_INTERNAL1_K1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS),
        %
        update_att_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS),
        update_att_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS)
    ).

-compile({inline, update_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS3}).
update_att_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C1) of
        badkey ->
            badkey;
        %
        UpdatedC1 ->
            ?INTERNAL1_UPD_C1(UpdatedC1)
    end.

-compile({inline, update_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS3}).
update_att_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C2) of
        badkey ->
            badkey;
        %
        UpdatedC2 ->
            ?INTERNAL1_UPD_C2(UpdatedC2)
    end.

%%

-compile({inline, update_att_INTERNAL1_K1 / ?INTERNAL1_ARITY_PLUS3}).
update_att_INTERNAL1_K1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS_IGN_K1) ->
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

-compile({inline, update_att_LEAF4 / ?LEAF4_ARITY_PLUS3}).
update_att_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    ?EXACT_SEARCH4(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        %
        ?new_LEAF4(
            Key,
            K2,
            K3,
            K4,
            %
            eval_update_value(ValueEval, ValueWrap, V1),
            V2,
            V3,
            V4
        ),
        %
        %
        ?new_LEAF4(
            K1,
            Key,
            K3,
            K4,
            %
            V1,
            eval_update_value(ValueEval, ValueWrap, V2),
            V3,
            V4
        ),
        %
        %
        ?new_LEAF4(
            K1,
            K2,
            Key,
            K4,
            %
            V1,
            V2,
            eval_update_value(ValueEval, ValueWrap, V3),
            V4
        ),
        %
        %
        ?new_LEAF4(
            K1,
            K2,
            K3,
            Key,
            %
            V1,
            V2,
            V3,
            eval_update_value(ValueEval, ValueWrap, V4)
        ),
        %
        %
        badkey
    ).

%%
%% ?LEAF3
%%

-compile({inline, update_att_LEAF3 / ?LEAF3_ARITY_PLUS3}).
update_att_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    ?EXACT_SEARCH3(
        Key,
        K1,
        K2,
        K3,
        %
        %
        ?new_LEAF3(
            Key,
            K2,
            K3,
            %
            eval_update_value(ValueEval, ValueWrap, V1),
            V2,
            V3
        ),
        %
        %
        ?new_LEAF3(
            K1,
            Key,
            K3,
            %
            V1,
            eval_update_value(ValueEval, ValueWrap, V2),
            V3
        ),
        %
        %
        ?new_LEAF3(
            K1,
            K2,
            Key,
            %
            V1,
            V2,
            eval_update_value(ValueEval, ValueWrap, V3)
        ),
        %
        %
        badkey
    ).

%%
%% ?LEAF2
%%

-compile({inline, update_att_LEAF2 / ?LEAF2_ARITY_PLUS3}).
update_att_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    ?EXACT_SEARCH2(
        Key,
        K1,
        K2,
        %
        %
        ?new_LEAF2(
            Key,
            K2,
            %
            eval_update_value(ValueEval, ValueWrap, V1),
            V2
        ),
        %
        %
        ?new_LEAF2(
            K1,
            Key,
            %
            V1,
            eval_update_value(ValueEval, ValueWrap, V2)
        ),
        %
        %
        badkey
    ).

%%
%% ?LEAF1
%%

-compile({inline, update_att_LEAF1 / ?LEAF1_ARITY_PLUS3}).
update_att_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    ?EXACT_SEARCH1(
        Key,
        K1,
        %
        %
        ?new_LEAF1(
            Key,
            %
            eval_update_value(ValueEval, ValueWrap, V1)
        ),
        %
        %
        badkey
    ).

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
            Acc2 = [V2 | values_recur(C3, Acc)],
            Acc3 = [V1 | values_recur(C2, Acc2)],
            values_recur(C1, Acc3);
        %
        ?INTERNAL3_MATCH(_, _, _, V1, V2, V3, C1, C2, C3, C4) ->
            Acc2 = [V3 | values_recur(C4, Acc)],
            Acc3 = [V2 | values_recur(C3, Acc2)],
            Acc4 = [V1 | values_recur(C2, Acc3)],
            values_recur(C1, Acc4);
        %
        ?INTERNAL4_MATCH(_, _, _, _, V1, V2, V3, V4, C1, C2, C3, C4, C5) ->
            Acc2 = [V4 | values_recur(C5, Acc)],
            Acc3 = [V3 | values_recur(C4, Acc2)],
            Acc4 = [V2 | values_recur(C3, Acc3)],
            Acc5 = [V1 | values_recur(C2, Acc4)],
            values_recur(C1, Acc5)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Insertion Rebalance handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing INTERNAL4
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
ins_rebalance_INTERNAL4_C1(Result, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_right_sibling_maybe(C1, Pos, Args, K1, V1, C2) of
                {UpKey, UpValue, UpdatedC1, UpdatedC2} ->
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
                Split ->
                    ?SPLIT(1, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC1 ->
            ?INTERNAL4_UPD_C1(UpdatedC1)
    end.

-compile({inline, ins_rebalance_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
ins_rebalance_INTERNAL4_C2(Result, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C2, Pos, Args, K1, V1, C1) of
                {UpKey, UpValue, UpdatedC1, UpdatedC2} ->
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
                Split ->
                    ?SPLIT(2, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC2 ->
            ?INTERNAL4_UPD_C2(UpdatedC2)
    end.

-compile({inline, ins_rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
ins_rebalance_INTERNAL4_C3(Result, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C3, Pos, Args, K2, V2, C2) of
                {UpKey, UpValue, UpdatedC2, UpdatedC3} ->
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
                        UpdatedC3,
                        C4,
                        C5
                    );
                %
                Split ->
                    ?SPLIT(3, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC3 ->
            ?INTERNAL4_UPD_C3(UpdatedC3)
    end.

-compile({inline, ins_rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
ins_rebalance_INTERNAL4_C4(Result, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C4, Pos, Args, K3, V3, C3) of
                {UpKey, UpValue, UpdatedC3, UpdatedC4} ->
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
                        UpdatedC4,
                        C5
                    );
                %
                Split ->
                    ?SPLIT(4, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC4 ->
            ?INTERNAL4_UPD_C4(UpdatedC4)
    end.

-compile({inline, ins_rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
ins_rebalance_INTERNAL4_C5(Result, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C5, Pos, Args, K4, V4, C4) of
                {UpKey, UpValue, UpdatedC4, UpdatedC5} ->
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
                        UpdatedC5
                    );
                %
                Split ->
                    ?SPLIT(5, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC5 ->
            ?INTERNAL4_UPD_C5(UpdatedC5)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing INTERNAL3
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
ins_rebalance_INTERNAL3_C1(Result, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_right_sibling_maybe(C1, Pos, Args, K1, V1, C2) of
                {UpKey, UpValue, UpdatedC1, UpdatedC2} ->
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
                {split, SplitK, SplitV, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC1 ->
            ?INTERNAL3_UPD_C1(UpdatedC1)
    end.

-compile({inline, ins_rebalance_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
ins_rebalance_INTERNAL3_C2(Result, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C2, Pos, Args, K1, V1, C1) of
                {UpKey, UpValue, UpdatedC1, UpdatedC2} ->
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
                {split, SplitK, SplitV, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC2 ->
            ?INTERNAL3_UPD_C2(UpdatedC2)
    end.

-compile({inline, ins_rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
ins_rebalance_INTERNAL3_C3(Result, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C3, Pos, Args, K2, V2, C2) of
                {UpKey, UpValue, UpdatedC2, UpdatedC3} ->
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
                        UpdatedC3,
                        C4
                    );
                %
                {split, SplitK, SplitV, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC3 ->
            ?INTERNAL3_UPD_C3(UpdatedC3)
    end.

-compile({inline, ins_rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
ins_rebalance_INTERNAL3_C4(Result, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C4, Pos, Args, K3, V3, C3) of
                {UpKey, UpValue, UpdatedC3, UpdatedC4} ->
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
                        UpdatedC4
                    );
                %
                {split, SplitK, SplitV, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC4 ->
            ?INTERNAL3_UPD_C4(UpdatedC4)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing INTERNAL2
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
ins_rebalance_INTERNAL2_C1(Result, ?INTERNAL2_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_right_sibling_maybe(C1, Pos, Args, K1, V1, C2) of
                {UpKey, UpValue, UpdatedC1, UpdatedC2} ->
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
                {split, SplitK, SplitV, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC1 ->
            ?INTERNAL2_UPD_C1(UpdatedC1)
    end.

-compile({inline, ins_rebalance_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
ins_rebalance_INTERNAL2_C2(Result, ?INTERNAL2_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C2, Pos, Args, K1, V1, C1) of
                {UpKey, UpValue, UpdatedC1, UpdatedC2} ->
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
                {split, SplitK, SplitV, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC2 ->
            ?INTERNAL2_UPD_C2(UpdatedC2)
    end.

-compile({inline, ins_rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
ins_rebalance_INTERNAL2_C3(Result, ?INTERNAL2_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C3, Pos, Args, K2, V2, C2) of
                {UpKey, UpValue, UpdatedC2, UpdatedC3} ->
                    ?new_INTERNAL2(
                        K1,
                        UpKey,
                        %
                        V1,
                        UpValue,
                        %
                        C1,
                        UpdatedC2,
                        UpdatedC3
                    );
                %
                {split, SplitK, SplitV, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC3 ->
            ?INTERNAL2_UPD_C3(UpdatedC3)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing INTERNAL1
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
ins_rebalance_INTERNAL1_C1(Result, ?INTERNAL1_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_right_sibling_maybe(C1, Pos, Args, K1, V1, C2) of
                {UpKey, UpValue, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL1(UpKey, UpValue, UpdatedC1, UpdatedC2);
                %
                {split, SplitK, SplitV, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC1 ->
            ?new_INTERNAL1(K1, V1, UpdatedC1, C2)
    end.

-compile({inline, ins_rebalance_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
ins_rebalance_INTERNAL1_C2(Result, ?INTERNAL1_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(C2, Pos, Args, K1, V1, C1) of
                {UpKey, UpValue, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL1(UpKey, UpValue, UpdatedC1, UpdatedC2);
                %
                {split, SplitK, SplitV, SplitL, SplitR} ->
                    ?new_INTERNAL2(
                        K1,
                        SplitK,
                        %
                        V1,
                        SplitV,
                        C1,
                        SplitL,
                        SplitR
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC2 ->
            ?new_INTERNAL1(K1, V1, C1, UpdatedC2)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Node Split
%% ------------------------------------------------------------------

insert_split_root(Pos, [NewKey | NewValue], Root) ->
    ?LEAF4_MATCH_ALL = Root,

    case Pos of
        1 ->
            ?new_INTERNAL1(
                K2,
                V2,
                ?new_LEAF2(NewKey, K1, NewValue, V1),
                ?new_LEAF2(K3, K4, V3, V4)
            );
        %
        2 ->
            ?new_INTERNAL1(
                K2,
                V2,
                ?new_LEAF2(K1, NewKey, V1, NewValue),
                ?new_LEAF2(K3, K4, V3, V4)
            );
        %
        3 ->
            ?new_INTERNAL1(
                NewKey,
                NewValue,
                ?new_LEAF2(K1, K2, V1, V2),
                ?new_LEAF2(K3, K4, V3, V4)
            );
        %
        4 ->
            ?new_INTERNAL1(
                K3,
                V3,
                ?new_LEAF2(K1, K2, V1, V2),
                ?new_LEAF2(NewKey, K4, NewValue, V4)
            );
        %
        5 ->
            ?new_INTERNAL1(
                K3,
                V3,
                ?new_LEAF2(K1, K2, V1, V2),
                ?new_LEAF2(K4, NewKey, V4, NewValue)
            )
    end;
insert_split_root(Pos, {split, SplitK, SplitV, SplitL, SplitR}, Root) ->
    ?INTERNAL4_MATCH_ALL = Root,

    case Pos of
        1 ->
            insert_split_root_internal(
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
        2 ->
            insert_split_root_internal(
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
        3 ->
            insert_split_root_internal(
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
        4 ->
            insert_split_root_internal(
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
        5 ->
            insert_split_root_internal(
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
            )
    end.

-spec insert_split_root_internal(
    K,
    K,
    K,
    K,
    K,
    %
    V,
    V,
    V,
    V,
    V,
    %
    C,
    C,
    C,
    C,
    C,
    C
) -> node_INTERNAL1(K, V) when
    C :: nonempty_node(K, V).
-compile({inline, insert_split_root_internal/16}).
insert_split_root_internal(
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

    SplitL = ?new_INTERNAL2(
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

    SplitR = ?new_INTERNAL2(
        K4,
        K5,
        %
        V4,
        V5,
        %
        C4,
        C5,
        C6
    ),

    ?new_INTERNAL1(SplitK, SplitV, SplitL, SplitR).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing from left sibling
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_into_left_sibling_maybe/6}).
ins_rebalance_into_left_sibling_maybe(Node, Pos, Args, ParentK, ParentV, Left) ->
    case Args of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            ins_rebalance_into_left_internal_maybe(
                Node, Pos, SplitK, SplitV, SplitL, SplitR, ParentK, ParentV, Left
            );
        %
        [NewKey | NewValue] ->
            ins_rebalance_into_left_leaf_maybe(Node, Pos, NewKey, NewValue, ParentK, ParentV, Left)
    end.

-compile({inline, ins_rebalance_into_left_internal_maybe/9}).
ins_rebalance_into_left_internal_maybe(
    ?INTERNAL4_MATCH_ALL, Pos, SplitK, SplitV, SplitL, SplitR, ParentK, ParentV, Left
) ->
    case Pos of
        1 ->
            ins_rebalance_into_left_internal(
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
                C5,
                %
                ParentK,
                ParentV,
                Left
            );
        %
        2 ->
            ins_rebalance_into_left_internal(
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
                C5,
                %
                ParentK,
                ParentV,
                Left
            );
        %
        3 ->
            ins_rebalance_into_left_internal(
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
                C5,
                %
                ParentK,
                ParentV,
                Left
            );
        %
        4 ->
            ins_rebalance_into_left_internal(
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
                C5,
                %
                ParentK,
                ParentV,
                Left
            );
        %
        5 ->
            ins_rebalance_into_left_internal(
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
                SplitR,
                %
                ParentK,
                ParentV,
                Left
            )
    end.

-compile({inline, ins_rebalance_into_left_internal/19}).
ins_rebalance_into_left_internal(
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
    C6,
    %
    ParentK,
    ParentV,
    Left
) ->
    case Left of
        ?INTERNAL2_MATCH(
            LK1,
            LK2,
            %
            LV1,
            LV2,
            %
            LC1,
            LC2,
            LC3
        ) ->
            UpKey = K1,
            UpValue = V1,

            UpdatedLeft = ?new_INTERNAL3(
                LK1,
                LK2,
                ParentK,
                %
                LV1,
                LV2,
                ParentV,
                %
                LC1,
                LC2,
                LC3,
                C1
            ),

            UpdatedNode = ?new_INTERNAL4(
                K2,
                K3,
                K4,
                K5,
                %
                V2,
                V3,
                V4,
                V5,
                %
                C2,
                C3,
                C4,
                C5,
                C6
            ),

            {UpKey, UpValue, UpdatedLeft, UpdatedNode};
        %
        %
        %
        %
        _ ->
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
            )
    end.

-compile({inline, ins_rebalance_into_left_leaf_maybe/7}).
ins_rebalance_into_left_leaf_maybe(Node, Pos, NewKey, NewValue, ParentK, ParentV, Left) ->
    case Left of
        ?LEAF2_MATCH(LK1, LK2, LV1, LV2) ->
            UpdatedLeft = ?new_LEAF3(LK1, LK2, ParentK, LV1, LV2, ParentV),
            ins_rebalance_into_left_leaf(Node, Pos, NewKey, NewValue, UpdatedLeft);
        %
        _ ->
            ins_rebalance_split_leaf(Node, Pos, NewKey, NewValue)
    end.

-compile({inline, ins_rebalance_into_left_leaf/5}).
ins_rebalance_into_left_leaf(?LEAF4_MATCH_ALL = Node, Pos, NewKey, NewValue, UpdatedLeft) ->
    case Pos of
        1 ->
            UpKey = NewKey,
            UpValue = NewValue,
            UpdatedNode = Node,
            {UpKey, UpValue, UpdatedLeft, UpdatedNode};
        %
        2 ->
            UpKey = K1,
            UpValue = V1,
            UpdatedNode = ?new_LEAF4(NewKey, K2, K3, K4, NewValue, V2, V3, V4),
            {UpKey, UpValue, UpdatedLeft, UpdatedNode};
        %
        3 ->
            UpKey = K1,
            UpValue = V1,
            UpdatedNode = ?new_LEAF4(K2, NewKey, K3, K4, V2, NewValue, V3, V4),
            {UpKey, UpValue, UpdatedLeft, UpdatedNode};
        %
        4 ->
            UpKey = K1,
            UpValue = V1,
            UpdatedNode = ?new_LEAF4(K2, K3, NewKey, K4, V2, V3, NewValue, V4),
            {UpKey, UpValue, UpdatedLeft, UpdatedNode};
        %
        5 ->
            UpKey = K1,
            UpValue = V1,
            UpdatedNode = ?new_LEAF4(K2, K3, K4, NewKey, V2, V3, V4, NewValue),
            {UpKey, UpValue, UpdatedLeft, UpdatedNode}
    end.

-compile({inline, ins_rebalance_split_leaf/4}).
ins_rebalance_split_leaf(?LEAF4_MATCH_ALL, Pos, NewKey, NewValue) ->
    case Pos of
        1 ->
            split_leaf(
                NewKey,
                K1,
                K2,
                K3,
                K4,
                NewValue,
                V1,
                V2,
                V3,
                V4
            );
        %
        2 ->
            split_leaf(
                K1,
                NewKey,
                K2,
                K3,
                K4,
                V1,
                NewValue,
                V2,
                V3,
                V4
            );
        %
        3 ->
            split_leaf(
                K1,
                K2,
                NewKey,
                K3,
                K4,
                V1,
                V2,
                NewValue,
                V3,
                V4
            );
        %
        4 ->
            split_leaf(
                K1,
                K2,
                K3,
                NewKey,
                K4,
                V1,
                V2,
                V3,
                NewValue,
                V4
            );
        %
        5 ->
            split_leaf(
                K1,
                K2,
                K3,
                K4,
                NewKey,
                V1,
                V2,
                V3,
                V4,
                NewValue
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing from right sibling
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_into_right_sibling_maybe/6}).
ins_rebalance_into_right_sibling_maybe(Node, Pos, Args, ParentK, ParentV, Right) ->
    case Args of
        {split, SplitK, SplitV, SplitL, SplitR} ->
            ins_rebalance_into_right_internal_maybe(
                Node, Pos, SplitK, SplitV, SplitL, SplitR, ParentK, ParentV, Right
            );
        %
        [NewKey | NewValue] ->
            ins_rebalance_into_right_leaf_maybe(
                Node, Pos, NewKey, NewValue, ParentK, ParentV, Right
            )
    end.

-compile({inline, ins_rebalance_into_right_internal_maybe/9}).
ins_rebalance_into_right_internal_maybe(
    ?INTERNAL4_MATCH_ALL, Pos, SplitK, SplitV, SplitL, SplitR, ParentK, ParentV, Right
) ->
    case Pos of
        1 ->
            ins_rebalance_into_right_internal(
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
                C5,
                %
                ParentK,
                ParentV,
                Right
            );
        %
        2 ->
            ins_rebalance_into_right_internal(
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
                C5,
                %
                ParentK,
                ParentV,
                Right
            );
        %
        3 ->
            ins_rebalance_into_right_internal(
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
                C5,
                %
                ParentK,
                ParentV,
                Right
            );
        %
        4 ->
            ins_rebalance_into_right_internal(
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
                C5,
                %
                ParentK,
                ParentV,
                Right
            );
        %
        5 ->
            ins_rebalance_into_right_internal(
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
                SplitR,
                %
                ParentK,
                ParentV,
                Right
            )
    end.

-compile({inline, ins_rebalance_into_right_internal/19}).
ins_rebalance_into_right_internal(
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
    C6,
    %
    ParentK,
    ParentV,
    Right
) ->
    case Right of
        ?INTERNAL2_MATCH(
            RK1,
            RK2,
            %
            RV1,
            RV2,
            %
            RC1,
            RC2,
            RC3
        ) ->
            UpKey = K5,
            UpValue = V5,

            UpdatedNode = ?new_INTERNAL4(
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
                C4,
                C5
            ),

            UpdatedRight = ?new_INTERNAL3(
                ParentK,
                RK1,
                RK2,
                %
                ParentV,
                RV1,
                RV2,
                %
                C6,
                RC1,
                RC2,
                RC3
            ),

            {UpKey, UpValue, UpdatedNode, UpdatedRight};
        %
        %
        %
        %
        _ ->
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
            )
    end.

-compile({inline, ins_rebalance_into_right_leaf_maybe/7}).
ins_rebalance_into_right_leaf_maybe(Node, Pos, NewKey, NewValue, ParentK, ParentV, Right) ->
    case Right of
        ?LEAF2_MATCH(RK1, RK2, RV1, RV2) ->
            UpdatedRight = ?new_LEAF3(ParentK, RK1, RK2, ParentV, RV1, RV2),
            ins_rebalance_into_right_leaf(Node, Pos, NewKey, NewValue, UpdatedRight);
        %
        _ ->
            ins_rebalance_split_leaf(Node, Pos, NewKey, NewValue)
    end.

-compile({inline, ins_rebalance_into_right_leaf/5}).
ins_rebalance_into_right_leaf(?LEAF4_MATCH_ALL = Node, Pos, NewKey, NewValue, UpdatedRight) ->
    case Pos of
        1 ->
            UpKey = K4,
            UpValue = V4,
            UpdatedNode = ?new_LEAF4(NewKey, K1, K2, K3, NewValue, V1, V2, V3),
            {UpKey, UpValue, UpdatedNode, UpdatedRight};
        %
        2 ->
            UpKey = K4,
            UpValue = V4,
            UpdatedNode = ?new_LEAF4(K1, NewKey, K2, K3, V1, NewValue, V2, V3),
            {UpKey, UpValue, UpdatedNode, UpdatedRight};
        %
        3 ->
            UpKey = K4,
            UpValue = V4,
            UpdatedNode = ?new_LEAF4(K1, K2, NewKey, K3, V1, V2, NewValue, V3),
            {UpKey, UpValue, UpdatedNode, UpdatedRight};
        %
        4 ->
            UpKey = K4,
            UpValue = V4,
            UpdatedNode = ?new_LEAF4(K1, K2, K3, NewKey, V1, V2, V3, NewValue),
            {UpKey, UpValue, UpdatedNode, UpdatedRight};
        %
        5 ->
            UpKey = NewKey,
            UpValue = NewValue,
            UpdatedNode = Node,
            {UpKey, UpValue, UpdatedNode, UpdatedRight}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Deletion Rebalance handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------
%% Internal Function Definitions: Deletion - Rebalancing INTERNAL4
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, del_rebalance_INTERNAL4_C1 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C1(?INTERNAL4_ARGS) ->
    case del_rebalance_maybe_from_right_sibling(C1, K1, V1, C2) of
        balanced ->
            ?INTERNAL4_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
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

-compile({inline, del_rebalance_INTERNAL4_C2 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C2(?INTERNAL4_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C2,
            %
            K1,
            V1,
            C1
        )
    of
        balanced ->
            ?INTERNAL4_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
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
%% C3
%%

-compile({inline, del_rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C3(?INTERNAL4_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C3,
            %
            K2,
            V2,
            C2
        )
    of
        balanced ->
            ?INTERNAL4_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpKey, UpValue, UpdatedC2, RebalancedC3) ->
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
            );
        %
        ?MERGED(MergedC2C3) ->
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
            )
    end.

%%
%% C4
%%

-compile({inline, del_rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C4(?INTERNAL4_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C4,
            %
            K3,
            V3,
            C3
        )
    of
        balanced ->
            ?INTERNAL4_UPD_C4(C4);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpKey, UpValue, UpdatedC3, RebalancedC4) ->
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
            );
        %
        ?MERGED(MergedC3C4) ->
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
            )
    end.

%%
%% C5
%%

-compile({inline, del_rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C5(?INTERNAL4_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C5, K4, V4, C4) of
        balanced ->
            ?INTERNAL4_UPD_C5(C5);
        %
        badkey ->
            badkey;
        %
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
%% Internal Function Definitions: Deletion - Rebalancing INTERNAL3
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, del_rebalance_INTERNAL3_C1 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C1(?INTERNAL3_ARGS) ->
    case del_rebalance_maybe_from_right_sibling(C1, K1, V1, C2) of
        balanced ->
            ?INTERNAL3_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
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

-compile({inline, del_rebalance_INTERNAL3_C2 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C2(?INTERNAL3_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C2,
            %
            K1,
            V1,
            C1
        )
    of
        balanced ->
            ?INTERNAL3_UPD_C2(C2);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
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
%% C3
%%

-compile({inline, del_rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C3(?INTERNAL3_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C3,
            %
            K2,
            V2,
            C2
        )
    of
        balanced ->
            ?INTERNAL3_UPD_C3(C3);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpKey, UpValue, UpdatedC2, RebalancedC3) ->
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
            );
        %
        ?MERGED(MergedC2C3) ->
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
            )
    end.

%%
%% C4
%%

-compile({inline, del_rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C4(?INTERNAL3_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C4, K3, V3, C3) of
        balanced ->
            ?INTERNAL3_UPD_C4(C4);
        %
        badkey ->
            badkey;
        %
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
%% Internal Function Definitions: Deletion - Rebalancing INTERNAL2
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, del_rebalance_INTERNAL2_C1 / ?INTERNAL2_ARITY}).
del_rebalance_INTERNAL2_C1(?INTERNAL2_ARGS) ->
    case del_rebalance_maybe_from_right_sibling(C1, K1, V1, C2) of
        balanced ->
            ?INTERNAL2_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
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

-compile({inline, del_rebalance_INTERNAL2_C2 / ?INTERNAL2_ARITY}).
del_rebalance_INTERNAL2_C2(?INTERNAL2_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C2,
            %
            K1,
            V1,
            C1
        )
    of
        balanced ->
            ?INTERNAL2_UPD_C2(C2);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
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
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL1(
                K2,
                %
                V2,
                %
                MergedC1C2,
                C3
            )
    end.

%%
%% C3
%%

-compile({inline, del_rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY}).
del_rebalance_INTERNAL2_C3(?INTERNAL2_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C3, K2, V2, C2) of
        balanced ->
            ?INTERNAL2_UPD_C3(C3);
        %
        badkey ->
            badkey;
        %
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
%% Internal Function Definitions: Deletion - Rebalancing INTERNAL1
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, del_rebalance_INTERNAL1_C1 / ?INTERNAL1_ARITY}).
del_rebalance_INTERNAL1_C1(?INTERNAL1_ARGS) ->
    case del_rebalance_maybe_from_right_sibling(C1, K1, V1, C2) of
        balanced ->
            ?INTERNAL1_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL1(UpKey, UpValue, UpdatedC1, UpdatedC2);
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen at root - height is reduced
            ?CHECK_NODE(MergedC1C2)
    end.

%%
%% C2
%%

-compile({inline, del_rebalance_INTERNAL1_C2 / ?INTERNAL1_ARITY}).
del_rebalance_INTERNAL1_C2(?INTERNAL1_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C2, K1, V1, C1) of
        balanced ->
            ?INTERNAL1_UPD_C2(C2);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL1(UpKey, UpValue, UpdatedC1, UpdatedC2);
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen at root - height is reduced
            ?CHECK_NODE(MergedC1C2)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Deletion - Rebalance a node from its right sibling
%% ------------------------------------------------------------------

-compile({inline, del_rebalance_maybe_from_right_sibling/4}).
del_rebalance_maybe_from_right_sibling(Child, RParentK, RParentV, Right) ->
    case Child of
        ?INTERNAL1_MATCH(CKey, CValue, CLeft, CRight) ->
            del_rebalance_internal_from_right_sibling(
                CKey,
                CValue,
                CLeft,
                CRight,
                %
                RParentK,
                RParentV,
                Right
            );
        %
        ?LEAF1_MATCH(CKey, CValue) ->
            del_rebalance_leaf_from_right_sibling(
                CKey,
                CValue,
                %
                RParentK,
                RParentV,
                Right
            );
        %
        badkey ->
            badkey;
        %
        _ ->
            balanced
    end.

%-compile({inline, del_rebalance_internal_from_right_sibling/7}).
del_rebalance_internal_from_right_sibling(
    CKey,
    CValue,
    CLeft,
    CRight,
    %
    RParentK,
    RParentV,
    Right
) ->
    case Right of
        ?INTERNAL2_MATCH_ALL ->
            MergedNode = ?new_INTERNAL4(
                CKey,
                RParentK,
                K1,
                K2,
                %
                CValue,
                RParentV,
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

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight);
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
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

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight)
        %
        %
    end.

%-compile({inline, del_rebalance_leaf_from_right_sibling/5}).
del_rebalance_leaf_from_right_sibling(CKey, CValue, RParentK, RParentV, Right) ->
    case Right of
        ?LEAF2_MATCH_ALL ->
            MergedNode = ?new_LEAF4(
                CKey,
                RParentK,
                K1,
                K2,
                %
                CValue,
                RParentV,
                V1,
                V2
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpKey = K1,
            UpValue = V1,

            UpdatedNode = ?new_LEAF2(CKey, RParentK, CValue, RParentV),
            UpdatedRight = ?new_LEAF2(K2, K3, V2, V3),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpKey = K1,
            UpValue = V1,

            UpdatedNode = ?new_LEAF2(CKey, RParentK, CValue, RParentV),
            UpdatedRight = ?new_LEAF3(K2, K3, K4, V2, V3, V4),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight)
        %
        %
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Deletion - Rebalance a node from its left sibling
%% ------------------------------------------------------------------

-compile({inline, del_rebalance_maybe_from_left_sibling/4}).
del_rebalance_maybe_from_left_sibling(Child, LParentK, LParentV, Left) ->
    case Child of
        ?INTERNAL1_MATCH(CKey, CValue, CLeft, CRight) ->
            del_rebalance_internal_from_left_sibling(
                CKey,
                CValue,
                CLeft,
                CRight,
                %
                LParentK,
                LParentV,
                Left
            );
        %
        ?LEAF1_MATCH(CKey, CValue) ->
            del_rebalance_leaf_from_left_sibling(
                CKey,
                CValue,
                %
                LParentK,
                LParentV,
                Left
            );
        %
        badkey ->
            badkey;
        %
        _ ->
            balanced
    end.

%-compile({inline, del_rebalance_internal_from_left_sibling/7}).
del_rebalance_internal_from_left_sibling(
    CKey,
    CValue,
    CLeft,
    CRight,
    %
    LParentK,
    LParentV,
    Left
) ->
    case Left of
        ?INTERNAL2_MATCH_ALL ->
            MergedNode = ?new_INTERNAL4(
                K1,
                K2,
                LParentK,
                CKey,
                %
                V1,
                V2,
                LParentV,
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
                C1,
                C2,
                C3
            ),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
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

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode)
        %
        %
    end.

%-compile({inline, del_rebalance_leaf_from_left_sibling/5}).
del_rebalance_leaf_from_left_sibling(
    CKey,
    CValue,
    LParentK,
    LParentV,
    Left
) ->
    case Left of
        ?LEAF2_MATCH_ALL ->
            MergedNode = ?new_LEAF4(
                K1,
                K2,
                LParentK,
                CKey,
                %
                V1,
                V2,
                LParentV,
                CValue
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpKey = K3,
            UpValue = V3,

            UpdatedNode = ?new_LEAF2(LParentK, CKey, LParentV, CValue),
            UpdatedLeft = ?new_LEAF2(K1, K2, V1, V2),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpKey = K4,
            UpValue = V4,

            UpdatedNode = ?new_LEAF2(LParentK, CKey, LParentV, CValue),
            UpdatedLeft = ?new_LEAF3(K1, K2, K3, V1, V2, V3),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Well-Formedness Checks
%% ------------------------------------------------------------------

-if(?NODE_CHECK_ENABLED).

check_node(LineNumber, Node, Variant) ->
    try do_check_node(LineNumber, Node, Variant) of
        ok ->
            Node
    catch
        error:{bad_node, _} = Reason:Stacktrace ->
            erlang:raise(error, Reason, Stacktrace);
        %
        Class:Reason:Stacktrace ->
            fail_node_check(LineNumber, unknown, Node, {Class, Reason, Stacktrace})
    end.

do_check_node(LineNumber, Node, Variant) ->
    Type = node_type(Node, Variant),
    check_node_keys(LineNumber, Type, Node),
    ok.

node_type(Node, top) ->
    node_type(Node);
node_type(Node, recur) ->
    recur_node_type(Node).

check_node_keys(LineNumber, Type, Node) ->
    Keys = keys(Node),
    MissortedKeys = check_node_keys(Keys),

    case MissortedKeys of
        [] ->
            Node;
        %
        [_ | _] ->
            fail_node_check(LineNumber, Type, Node, {missorted_keys, MissortedKeys})
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

fail_node_check(LineNumber, Type, Node, Reason) ->
    error(
        {bad_node, [
            {line, LineNumber},
            {type, Type},
            {reason, Reason},
            {node, Node}
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
