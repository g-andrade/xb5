% TODO document
-module(b5_trees_v2_node).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    delete_att/2,
    foldl/3,
    foldr/3,
    get/2,
    insert_att/4,
    is_defined/2,
    iterator/2,
    iterator_from/3,
    keys/1,
    larger/2,
    largest/1,
    lookup/2,
    map/2,
    new/0,
    next/1,
    smaller/2,
    smallest/1,
    structural_stats/1,
    take_att/2,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    update_att/4,
    values/1
]).

%% ------------------------------------------------------------------
%% Macro Definitions: Nodes
%% ------------------------------------------------------------------

% 7 elements
-define(INTERNAL2(K1, K2, V1, V2, C1, C2, C3), {K1, K2, V1, V2, C1, C2, C3}).
-define(INTERNAL2_MATCH(K1, K2, V1, V2, C1, C2, C3), {K1, K2, V1, V2, C1, C2, C3}).
-define(INTERNAL2_MATCH_ALL, {K1, K2, V1, V2, C1, C2, C3}).

% 10 elements
-define(INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4),
    {K1, K2, K3, V1, V2, V3, C1, C2, C3, C4}
).
-define(INTERNAL3_MATCH(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4),
    {K1, K2, K3, V1, V2, V3, C1, C2, C3, C4}
).
-define(INTERNAL3_MATCH_ALL, {K1, K2, K3, V1, V2, V3, C1, C2, C3, C4}).

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

% 4 elements
-define(INTERNAL1(K1, V1, C1, C2), {K1, V1, C1, C2}).
-define(INTERNAL1_MATCH(K1, V1, C1, C2), {K1, V1, C1, C2}).
-define(INTERNAL1_MATCH_ALL, {K1, V1, C1, C2}).

% empty root
-define(EMPTY_ROOT, leaf0).

%%%%%%%%

% Cannot clash with any node type.

-define(SPLIT(Pos, Args), [Pos | Args]).
-define(SPLIT_MATCH(Pos, Args), [Pos | Args]).

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

-define(ITER_PAIR(Key, Value), [Key | Value]).
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

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-opaque t(Key, Value) :: root_only_node(Key, Value) | deep_node(Key, Value).
%% This type represents any tree node structure.
-export_type([t/2]).

-type empty_node() :: ?EMPTY_ROOT.

%
% The fact that some node types are root-only allows us to optimize the
% recursive case - less potential patterns to match.
%
% Therefore, most of the exported API functions here match on the root-only
% nodes in their entry clauses, but only on deep node types after that.
%

-type root_only_node(Key, Value) ::
    (node_INTERNAL1(Key, Value)
    | empty_node()).

-type deep_node(Key, Value) ::
    (node_INTERNAL4(Key, Value)
    | node_INTERNAL3(Key, Value)
    | node_INTERNAL2(Key, Value)).

-type nonempty_node(Key, Value) ::
    (node_INTERNAL1(Key, Value)
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

%%%%%%%%%%%

-type split_internal_result(Key, Value) :: split_result(
    Key, Value, node_INTERNAL2(Key, Value), node_INTERNAL2(Key, Value)
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

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec delete_att(Key, t(Key, Value)) -> none | t(Key, Value).
delete_att(Key, ?INTERNAL1_MATCH_ALL) ->
    delete_att_INTERNAL1(Key, ?INTERNAL1_ARGS);
delete_att(_Key, ?EMPTY_ROOT) ->
    none;
delete_att(Key, Root) ->
    delete_att_recur(Key, Root).

-spec foldl(fun((Key, Value, Acc2) -> Acc1), Acc0, t(Key, Value)) -> AccN when
    Acc0 :: term(),
    Acc1 :: term(),
    Acc2 :: term(),
    AccN :: term().
foldl(Fun, Acc, ?INTERNAL1_MATCH_ALL) ->
    Acc2 = foldl_recur(Fun, Acc, C1),
    Acc3 = Fun(K1, V1, Acc2),
    foldl_recur(Fun, Acc3, C2);
foldl(_Fun, Acc, ?EMPTY_ROOT) ->
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
foldr(_Fun, Acc, ?EMPTY_ROOT) ->
    Acc;
foldr(Fun, Acc, Root) ->
    foldr_recur(Fun, Acc, Root).

-spec get(Key, t(Key, Value)) -> Value | no_return().
get(Key, ?INTERNAL1_MATCH_ALL) ->
    get_INTERNAL1(Key, ?INTERNAL1_ARGS);
get(Key, ?EMPTY_ROOT) ->
    error_badkey(Key);
get(Key, Root) ->
    get_recur(Key, Root).

-spec insert_att
    (Key, eager, Value, t(Key, Value)) -> none | t(Key, Value);
    (Key, lazy, fun(() -> Value), t(Key, Value)) -> none | t(Key, Value).
insert_att(Key, ValueEval, ValueWrap, ?INTERNAL1_MATCH_ALL) ->
    insert_att_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
insert_att(Key, ValueEval, ValueWrap, ?EMPTY_ROOT) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?new_INTERNAL1(Key, Value, nil, nil);
insert_att(Key, ValueEval, ValueWrap, Root) ->
    case insert_att_recur(Key, ValueEval, ValueWrap, Root) of
        ?SPLIT_MATCH(Pos, Args) ->
            insert_split_root(Pos, Args, Root);
        %
        UpdatedRoot ->
            UpdatedRoot
    end.

-spec is_defined(Key, t(Key, _)) -> boolean().
is_defined(Key, ?INTERNAL1_MATCH(K1, _, C1, C2)) ->
    is_defined_INTERNAL1(Key, K1, C1, C2);
is_defined(_Key, ?EMPTY_ROOT) ->
    false;
is_defined(Key, Root) ->
    is_defined_recur(Key, Root).

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

-spec keys(t(Key, _)) -> [Key].
keys(?INTERNAL1_MATCH(K1, _, C1, C2)) ->
    keys_recur(C1, [K1 | keys_recur(C2, [])]);
keys(?EMPTY_ROOT) ->
    [];
keys(Root) ->
    keys_recur(Root, []).

-spec larger(Key, t(Key, Value)) -> {Key, Value} | none.
larger(Key, ?INTERNAL1_MATCH_ALL) ->
    larger_INTERNAL1(Key, ?INTERNAL1_ARGS);
larger(_Key, ?EMPTY_ROOT) ->
    none;
larger(Key, Root) ->
    larger_recur(Key, Root).

-spec largest(t(Key, Value)) -> {Key, Value}.
largest(?INTERNAL1_MATCH(K1, V1, _, C2)) ->
    largest_continue(K1, V1, C2);
largest(Root) ->
    largest_recur(Root).

-spec lookup(Key, t(Key, Value)) -> {value, Value} | none.
lookup(Key, ?INTERNAL1_MATCH_ALL) ->
    lookup_INTERNAL1(Key, ?INTERNAL1_ARGS);
lookup(_Key, ?EMPTY_ROOT) ->
    none;
lookup(Key, Root) ->
    lookup_recur(Key, Root).

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
map(_Fun, ?EMPTY_ROOT) ->
    ?EMPTY_ROOT;
map(Fun, Root) ->
    map_recur(Fun, Root).

-spec new() -> t(term(), term()).
new() ->
    ?EMPTY_ROOT.

next([Head | Tail]) ->
    next(Head, Tail);
next([]) ->
    none.

-spec smaller(Key, t(Key, Value)) -> {Key, Value} | none.
smaller(Key, ?INTERNAL1_MATCH_ALL) ->
    smaller_INTERNAL1(Key, ?INTERNAL1_ARGS);
smaller(_Key, ?EMPTY_ROOT) ->
    none;
smaller(Key, Root) ->
    smaller_recur(Key, Root).

-spec smallest(t(Key, Value)) -> {Key, Value}.
smallest(?INTERNAL1_MATCH(K1, V1, C1, _)) ->
    smallest_continue(K1, V1, C1);
smallest(Root) ->
    smallest_recur(Root).

-spec structural_stats(t(_, _)) -> b5_structural_stats:t().
structural_stats(Root) ->
    Acc = b5_structural_stats:new(),

    case Root of
        ?INTERNAL1_MATCH(_, _, C1, C2) ->
            Height = 1,
            Acc2 = b5_structural_stats:inc_count(internal1, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            b5_structural_stats:return(Acc4);
        %
        ?EMPTY_ROOT ->
            b5_structural_stats:return(Acc);
        %
        _ ->
            Height = 1,
            Acc2 = structural_stats_recur(Root, Acc, Height),
            b5_structural_stats:return(Acc2)
    end.

-spec take_att(Key, t(Key, Value)) -> none | take_result(Key, Value) | no_return().
take_att(Key, ?INTERNAL1_MATCH_ALL) ->
    take_att_INTERNAL1(Key, ?INTERNAL1_ARGS);
take_att(_Key, ?EMPTY_ROOT) ->
    none;
take_att(Key, Root) ->
    take_att_recur(Key, Root).

-spec take_largest(t(Key, Value)) -> take_result(Key, Value).
take_largest(?INTERNAL1_MATCH_ALL) ->
    take_largest_INTERNAL1(?INTERNAL1_ARGS);
take_largest(Root) ->
    take_largest_recur(Root).

-spec take_smallest(t(Key, Value)) -> take_result(Key, Value).
take_smallest(?INTERNAL1_MATCH_ALL) ->
    take_smallest_INTERNAL1(?INTERNAL1_ARGS);
take_smallest(Root) ->
    take_smallest_recur(Root).

-spec to_list(t(Key, Value)) -> [{Key, Value}].
to_list(?INTERNAL1_MATCH_ALL) ->
    Acc2 = to_list_recur(C2, []),
    Acc3 = [{K1, V1} | Acc2],
    to_list_recur(C1, Acc3);
to_list(?EMPTY_ROOT) ->
    [];
to_list(Root) ->
    to_list_recur(Root, []).

-spec update_att
    (Key, eager, Value, t(Key, _)) -> none | t(Key, Value);
    (Key, lazy, fun((PrevValue) -> Value), t(Key, PrevValue)) -> none | t(Key, Value).
update_att(Key, ValueEval, ValueWrap, ?INTERNAL1_MATCH_ALL) ->
    update_att_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
update_att(_Key, _ValueEval, _ValueWrap, ?EMPTY_ROOT) ->
    none;
update_att(Key, ValueEval, ValueWrap, Root) ->
    update_att_recur(Key, ValueEval, ValueWrap, Root).

-spec values(t(_, Value)) -> [Value].
values(?INTERNAL1_MATCH(_, V1, C1, C2)) ->
    values_recur(C1, [V1 | values_recur(C2, [])]);
values(?EMPTY_ROOT) ->
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
        nil ->
            none
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, delete_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            delete_att_INTERNAL4_C4(Key, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            delete_att_INTERNAL4_C3(Key, ?INTERNAL4_ARGS);
                        %
                        true ->
                            delete_att_INTERNAL4_K3(?INTERNAL4_ARGS)
                    end;
                %
                Key > K4 ->
                    delete_att_INTERNAL4_C5(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_att_INTERNAL4_K4(?INTERNAL4_ARGS)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    delete_att_INTERNAL4_C1(Key, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    delete_att_INTERNAL4_C2(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_att_INTERNAL4_K1(?INTERNAL4_ARGS)
            end;
        %
        true ->
            delete_att_INTERNAL4_K2(?INTERNAL4_ARGS)
    end.

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
    case take_smallest_recur(C2) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) ->
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
            );
        %
        none ->
            ?new_INTERNAL3(
                K2,
                K3,
                K4,
                %
                V2,
                V3,
                V4,
                %
                nil,
                nil,
                nil,
                nil
            )
    end.

-compile({inline, delete_att_INTERNAL4_K2 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_K2(?INTERNAL4_ARGS_IGN_K2_V2) ->
    case take_largest_recur(C2) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) ->
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
            );
        %
        none ->
            ?new_INTERNAL3(
                K1,
                K3,
                K4,
                %
                V1,
                V3,
                V4,
                %
                nil,
                nil,
                nil,
                nil
            )
    end.

-compile({inline, delete_att_INTERNAL4_K3 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_K3(?INTERNAL4_ARGS_IGN_K3_V3) ->
    case take_smallest_recur(C4) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) ->
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
            );
        %
        none ->
            ?new_INTERNAL3(
                K1,
                K2,
                K4,
                %
                V1,
                V2,
                V4,
                %
                nil,
                nil,
                nil,
                nil
            )
    end.

-compile({inline, delete_att_INTERNAL4_K4 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_K4(?INTERNAL4_ARGS_IGN_K4_V4) ->
    case take_largest_recur(C4) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) ->
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
            );
        %
        none ->
            ?new_INTERNAL3(
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                nil,
                nil,
                nil,
                nil
            )
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, delete_att_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    delete_att_INTERNAL3_C1(Key, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    delete_att_INTERNAL3_C2(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_att_INTERNAL3_K1(?INTERNAL3_ARGS)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    delete_att_INTERNAL3_C3(Key, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    delete_att_INTERNAL3_C4(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_att_INTERNAL3_K3(?INTERNAL3_ARGS)
            end;
        %
        true ->
            delete_att_INTERNAL3_K2(?INTERNAL3_ARGS)
    end.

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
    case take_smallest_recur(C2) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) ->
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
            );
        %
        none ->
            ?new_INTERNAL2(
                K2,
                K3,
                %
                V2,
                V3,
                %
                nil,
                nil,
                nil
            )
    end.

-compile({inline, delete_att_INTERNAL3_K2 / ?INTERNAL3_ARITY}).
delete_att_INTERNAL3_K2(?INTERNAL3_ARGS_IGN_K2_V2) ->
    case take_smallest_recur(C3) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) ->
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
            );
        %
        none ->
            ?new_INTERNAL2(
                K1,
                K3,
                %
                V1,
                V3,
                %
                nil,
                nil,
                nil
            )
    end.

-compile({inline, delete_att_INTERNAL3_K3 / ?INTERNAL3_ARITY}).
delete_att_INTERNAL3_K3(?INTERNAL3_ARGS_IGN_K3_V3) ->
    case take_largest_recur(C3) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) ->
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
            );
        %
        none ->
            ?new_INTERNAL2(
                K1,
                K2,
                %
                V1,
                V2,
                %
                nil,
                nil,
                nil
            )
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, delete_att_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
delete_att_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    delete_att_INTERNAL2_C2(Key, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    delete_att_INTERNAL2_C3(Key, ?INTERNAL2_ARGS);
                %
                true ->
                    delete_att_INTERNAL2_K2(?INTERNAL2_ARGS)
            end;
        %
        Key < K1 ->
            delete_att_INTERNAL2_C1(Key, ?INTERNAL2_ARGS);
        %
        true ->
            delete_att_INTERNAL2_K1(?INTERNAL2_ARGS)
    end.

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
    case take_smallest_recur(C2) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) ->
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
            );
        %
        none ->
            ?new_INTERNAL1(K2, V2, nil, nil)
    end.

-compile({inline, delete_att_INTERNAL2_K2 / ?INTERNAL2_ARITY}).
delete_att_INTERNAL2_K2(?INTERNAL2_ARGS_IGN_K2_V2) ->
    case take_largest_recur(C2) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) ->
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
            );
        %
        none ->
            ?new_INTERNAL1(K1, V1, nil, nil)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, delete_att_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
delete_att_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            delete_att_INTERNAL1_C1(Key, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            delete_att_INTERNAL1_C2(Key, ?INTERNAL1_ARGS);
        %
        true ->
            delete_att_INTERNAL1_K1(?INTERNAL1_ARGS)
    end.

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
    case take_smallest_recur(C2) of
        ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) ->
            ?INTERNAL1_K1_C2_REBALANCE(ReplacementK, ReplacementV, UpdatedC2);
        %
        none ->
            ?EMPTY_ROOT
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: foldl/3
%% ------------------------------------------------------------------

foldl_recur(Fun, Acc, Node) ->
    case Node of
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
            _Acc6 = foldl_recur(Fun, Fun(K4, V4, Acc5), C5);
        %
        nil ->
            Acc
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: foldr/3
%% ------------------------------------------------------------------

foldr_recur(Fun, Acc, Node) ->
    case Node of
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
            _Acc6 = foldr_recur(Fun, Fun(K1, V1, Acc5), C1);
        %
        nil ->
            Acc
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: get/2
%% ------------------------------------------------------------------

get_recur(Key, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            get_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            get_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            get_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        nil ->
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

-compile({inline, error_badkey/1}).
-spec error_badkey(term()) -> no_return().
error_badkey(Key) ->
    error({badkey, Key}).

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
            insert_att_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, insert_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            insert_att_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            insert_att_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        true ->
                            none
                    end;
                %
                Key > K4 ->
                    insert_att_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    none
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_att_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    insert_att_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    none
            end;
        %
        true ->
            none
    end.

-compile({inline, insert_att_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case C1 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?SPLIT(1, [Key | Value]);
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C1),
            ins_rebalance_INTERNAL4_C1(Result, ?INTERNAL4_ARGS)
    end.

-compile({inline, insert_att_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case C2 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?SPLIT(2, [Key | Value]);
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C2),
            ins_rebalance_INTERNAL4_C2(Result, ?INTERNAL4_ARGS)
    end.

-compile({inline, insert_att_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case C3 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?SPLIT(3, [Key | Value]);
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C3),
            ins_rebalance_INTERNAL4_C3(Result, ?INTERNAL4_ARGS)
    end.

-compile({inline, insert_att_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case C4 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?SPLIT(4, [Key | Value]);
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C4),
            ins_rebalance_INTERNAL4_C4(Result, ?INTERNAL4_ARGS)
    end.

-compile({inline, insert_att_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS3}).
insert_att_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case C5 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?SPLIT(5, [Key | Value]);
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C5),
            ins_rebalance_INTERNAL4_C5(Result, ?INTERNAL4_ARGS)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, insert_att_INTERNAL3 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_att_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    insert_att_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    none
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    insert_att_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    insert_att_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    none
            end;
        %
        true ->
            none
    end.

-compile({inline, insert_att_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case C1 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_INTERNAL4(
                Key,
                K1,
                K2,
                K3,
                %
                Value,
                V1,
                V2,
                V3,
                %
                nil,
                nil,
                nil,
                nil,
                nil
            );
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C1),
            ins_rebalance_INTERNAL3_C1(Result, ?INTERNAL3_ARGS)
    end.

-compile({inline, insert_att_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case C2 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_INTERNAL4(
                K1,
                Key,
                K2,
                K3,
                %
                V1,
                Value,
                V2,
                V3,
                %
                nil,
                nil,
                nil,
                nil,
                nil
            );
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C2),
            ins_rebalance_INTERNAL3_C2(Result, ?INTERNAL3_ARGS)
    end.

-compile({inline, insert_att_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case C3 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_INTERNAL4(
                K1,
                K2,
                Key,
                K3,
                %
                V1,
                V2,
                Value,
                V3,
                %
                nil,
                nil,
                nil,
                nil,
                nil
            );
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C3),
            ins_rebalance_INTERNAL3_C3(Result, ?INTERNAL3_ARGS)
    end.

-compile({inline, insert_att_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS3}).
insert_att_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case C4 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
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
                nil,
                nil,
                nil,
                nil,
                nil
            );
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C4),
            ins_rebalance_INTERNAL3_C4(Result, ?INTERNAL3_ARGS)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, insert_att_INTERNAL2 / ?INTERNAL2_ARITY_PLUS3}).
insert_att_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    insert_att_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    insert_att_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                true ->
                    none
            end;
        %
        Key < K1 ->
            insert_att_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        true ->
            none
    end.

-compile({inline, insert_att_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS3}).
insert_att_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case C1 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_INTERNAL3(
                Key,
                K1,
                K2,
                %
                Value,
                V1,
                V2,
                %
                nil,
                nil,
                nil,
                nil
            );
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C1),
            ins_rebalance_INTERNAL2_C1(Result, ?INTERNAL2_ARGS)
    end.

-compile({inline, insert_att_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS3}).
insert_att_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case C2 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_INTERNAL3(
                K1,
                Key,
                K2,
                %
                V1,
                Value,
                V2,
                %
                nil,
                nil,
                nil,
                nil
            );
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C2),
            ins_rebalance_INTERNAL2_C2(Result, ?INTERNAL2_ARGS)
    end.

-compile({inline, insert_att_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS3}).
insert_att_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case C3 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_INTERNAL3(
                K1,
                K2,
                Key,
                %
                V1,
                V2,
                Value,
                %
                nil,
                nil,
                nil,
                nil
            );
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C3),
            ins_rebalance_INTERNAL2_C3(Result, ?INTERNAL2_ARGS)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, insert_att_INTERNAL1 / ?INTERNAL1_ARITY_PLUS3}).
insert_att_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            insert_att_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            insert_att_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        true ->
            none
    end.

-compile({inline, insert_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS3}).
insert_att_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case C1 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_INTERNAL2(
                Key,
                K1,
                %
                Value,
                V1,
                %
                nil,
                nil,
                nil
            );
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C1),
            ins_rebalance_INTERNAL1_C1(Result, ?INTERNAL1_ARGS)
    end.

-compile({inline, insert_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS3}).
insert_att_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case C2 of
        nil ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_INTERNAL2(
                K1,
                Key,
                %
                V1,
                Value,
                %
                nil,
                nil,
                nil
            );
        %
        _ ->
            Result = insert_att_recur(Key, ValueEval, ValueWrap, C2),
            ins_rebalance_INTERNAL1_C2(Result, ?INTERNAL1_ARGS)
    end.

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
        nil,
        nil,
        nil,
        nil,
        nil,
        nil
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_defined/2
%% ------------------------------------------------------------------

is_defined_recur(Key, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH(K1, K2, _, _, C1, C2, C3) ->
            is_defined_INTERNAL2(Key, K1, K2, C1, C2, C3);
        %
        ?INTERNAL3_MATCH(K1, K2, K3, _, _, _, C1, C2, C3, C4) ->
            is_defined_INTERNAL3(Key, K1, K2, K3, C1, C2, C3, C4);
        %
        ?INTERNAL4_MATCH(K1, K2, K3, K4, _, _, _, _, C1, C2, C3, C4, C5) ->
            is_defined_INTERNAL4(Key, K1, K2, K3, K4, C1, C2, C3, C4, C5);
        %
        nil ->
            false
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, is_defined_INTERNAL4/10}).
is_defined_INTERNAL4(Key, K1, K2, K3, K4, C1, C2, C3, C4, C5) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            is_defined_recur(Key, C4);
                        %
                        Key < K3 ->
                            is_defined_recur(Key, C3);
                        %
                        true ->
                            true
                    end;
                %
                Key > K4 ->
                    is_defined_recur(Key, C5);
                %
                true ->
                    true
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    is_defined_recur(Key, C1);
                %
                Key > K1 ->
                    is_defined_recur(Key, C2);
                %
                true ->
                    true
            end;
        %
        true ->
            true
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, is_defined_INTERNAL3/8}).
is_defined_INTERNAL3(Key, K1, K2, K3, C1, C2, C3, C4) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    is_defined_recur(Key, C1);
                %
                Key > K1 ->
                    is_defined_recur(Key, C2);
                %
                true ->
                    true
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    is_defined_recur(Key, C3);
                %
                Key > K3 ->
                    is_defined_recur(Key, C4);
                %
                true ->
                    true
            end;
        %
        true ->
            true
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, is_defined_INTERNAL2/6}).
is_defined_INTERNAL2(Key, K1, K2, C1, C2, C3) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    is_defined_recur(Key, C2);
                %
                Key > K2 ->
                    is_defined_recur(Key, C3);
                %
                true ->
                    true
            end;
        %
        Key < K1 ->
            is_defined_recur(Key, C1);
        %
        true ->
            true
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, is_defined_INTERNAL1/4}).
is_defined_INTERNAL1(Key, K1, C1, C2) ->
    if
        Key < K1 ->
            is_defined_recur(Key, C1);
        %
        Key > K1 ->
            is_defined_recur(Key, C2);
        %
        true ->
            true
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - forward
%% ------------------------------------------------------------------

fwd_iterator(?INTERNAL1_MATCH_ALL) ->
    Acc = [?ITER_PAIR(K1, V1), C2],
    fwd_iterator_recur(C1, Acc);
fwd_iterator(?EMPTY_ROOT) ->
    Iter = [],
    Iter;
fwd_iterator(Root) ->
    Acc = [],
    fwd_iterator_recur(Root, Acc).

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
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(nil, Acc) ->
    Acc.

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - reverse
%% ------------------------------------------------------------------

rev_iterator(?INTERNAL1_MATCH_ALL) ->
    Acc = [?ITER_PAIR(K1, V1), C1],
    rev_iterator_recur(C2, Acc);
rev_iterator(?EMPTY_ROOT) ->
    Iter = [],
    Iter;
rev_iterator(Root) ->
    Acc = [],
    rev_iterator_recur(Root, Acc).

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
    rev_iterator_recur(C5, Acc2);
rev_iterator_recur(nil, Acc) ->
    Acc.

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_fwd_iterator(Key, Root) ->
    case Root of
        ?INTERNAL1_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?EMPTY_ROOT ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            bound_fwd_iterator_recur(Key, Root, Acc)
    end.

bound_fwd_iterator_recur(Key, Node, Acc) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc);
        %
        nil ->
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
            Acc2 = [?ITER_PAIR(K3, V3), C4, ?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C3, Acc2);
        %
        Key > K1 ->
            Acc2 = [?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4, ?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2);
        %
        true ->
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
            Acc2 = [?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2);
        %
        true ->
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
            Acc2 = [?ITER_PAIR(K1, V1), C2, ?ITER_PAIR(K2, V2), C3 | Acc],
            bound_fwd_iterator_recur(Key, C1, Acc2)
    end.

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

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_rev_iterator(Key, Root) ->
    case Root of
        ?INTERNAL1_MATCH_ALL ->
            bound_rev_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?EMPTY_ROOT ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            bound_rev_iterator_recur(Key, Root, Acc)
    end.

bound_rev_iterator_recur(Key, Node, Acc) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            bound_rev_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            bound_rev_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            bound_rev_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc);
        %
        nil ->
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
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2);
        %
        Key < K4 ->
            Acc2 = [?ITER_PAIR(K3, V3), C3, ?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C4, Acc2);
        %
        true ->
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
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2);
        %
        true ->
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
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2)
    end.

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

%% ------------------------------------------------------------------
%% Internal Function Definitions: keys/1
%% ------------------------------------------------------------------

keys_recur(Node, Acc) ->
    case Node of
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
        nil ->
            Acc
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
        nil ->
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

%% ------------------------------------------------------------------
%% Internal Function Definitions: largest/1
%% ------------------------------------------------------------------

-compile({inline, largest_continue/3}).
largest_continue(K, V, C) ->
    case C of
        nil ->
            {K, V};
        %
        _ ->
            largest_recur(C)
    end.

largest_recur(Node) ->
    case Node of
        ?INTERNAL2_MATCH(_, K2, _, V2, _, _, C3) ->
            largest_continue(K2, V2, C3);
        %
        ?INTERNAL3_MATCH(_, _, K3, _, _, V3, _, _, _, C4) ->
            largest_continue(K3, V3, C4);
        %
        ?INTERNAL4_MATCH(_, _, _, K4, _, _, _, V4, _, _, _, _, C5) ->
            largest_continue(K4, V4, C5)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: lookup/2
%% ------------------------------------------------------------------

lookup_recur(Key, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            lookup_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            lookup_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            lookup_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        nil ->
            none
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, lookup_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
lookup_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            lookup_recur(Key, C4);
                        %
                        Key < K3 ->
                            lookup_recur(Key, C3);
                        %
                        true ->
                            {value, V3}
                    end;
                %
                Key > K4 ->
                    lookup_recur(Key, C5);
                %
                true ->
                    {value, V4}
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    lookup_recur(Key, C1);
                %
                Key > K1 ->
                    lookup_recur(Key, C2);
                %
                true ->
                    {value, V1}
            end;
        %
        true ->
            {value, V2}
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, lookup_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
lookup_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    lookup_recur(Key, C1);
                %
                Key > K1 ->
                    lookup_recur(Key, C2);
                %
                true ->
                    {value, V1}
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    lookup_recur(Key, C3);
                %
                Key > K3 ->
                    lookup_recur(Key, C4);
                %
                true ->
                    {value, V3}
            end;
        %
        true ->
            {value, V2}
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, lookup_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
lookup_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    lookup_recur(Key, C2);
                %
                Key > K2 ->
                    lookup_recur(Key, C3);
                %
                true ->
                    {value, V2}
            end;
        %
        Key < K1 ->
            lookup_recur(Key, C1);
        %
        true ->
            {value, V1}
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, lookup_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
lookup_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            lookup_recur(Key, C1);
        %
        Key > K1 ->
            lookup_recur(Key, C2);
        %
        true ->
            {value, V1}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: map/2
%% ------------------------------------------------------------------

map_recur(Fun, Node) ->
    case Node of
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
            );
        %
        %
        nil ->
            nil
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
            case fwd_iterator_recur(Node, Tail) of
                [?ITER_PAIR(Key, Value) | NewTail] ->
                    Iter2 = NewTail,
                    {Key, Value, Iter2};
                %
                [] ->
                    none
            end
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
            case rev_iterator_recur(Node, Tail) of
                [?ITER_PAIR(Key, Value) | NewTail] ->
                    Iter2 = [?REV_ITER_TAG | NewTail],
                    {Key, Value, Iter2};
                %
                [] ->
                    none
            end
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
        nil ->
            none
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

%% ------------------------------------------------------------------
%% Internal Function Definitions: smallest/1
%% ------------------------------------------------------------------

-compile({inline, smallest_continue/3}).
smallest_continue(K, V, C) ->
    case C of
        nil ->
            {K, V};
        %
        _ ->
            smallest_recur(C)
    end.

smallest_recur(Node) ->
    case Node of
        ?INTERNAL2_MATCH(K1, _, V1, _, C1, _, _) ->
            smallest_continue(K1, V1, C1);
        %
        ?INTERNAL3_MATCH(K1, _, _, V1, _, _, C1, _, _, _) ->
            smallest_continue(K1, V1, C1);
        %
        ?INTERNAL4_MATCH(K1, _, _, _, V1, _, _, _, C1, _, _, _, _) ->
            smallest_continue(K1, V1, C1)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: structural_stats/1
%% ------------------------------------------------------------------

structural_stats_recur(Node, Acc, Height) ->
    case Node of
        ?INTERNAL2_MATCH(_, _, _, _, C1, C2, C3) ->
            Acc2 = b5_structural_stats:inc_count(internal2, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            _Acc5 = structural_stats_recur(C3, Acc4, Height + 1);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, C1, C2, C3, C4) ->
            Acc2 = b5_structural_stats:inc_count(internal3, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            _Acc6 = structural_stats_recur(C4, Acc5, Height + 1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, C1, C2, C3, C4, C5) ->
            Acc2 = b5_structural_stats:inc_count(internal4, Acc),
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
        nil ->
            none
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, take_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            take_att_INTERNAL4_C4(Key, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            take_att_INTERNAL4_C3(Key, ?INTERNAL4_ARGS);
                        %
                        true ->
                            take_att_INTERNAL4_K3(?INTERNAL4_ARGS)
                    end;
                %
                Key > K4 ->
                    take_att_INTERNAL4_C5(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    take_att_INTERNAL4_K4(?INTERNAL4_ARGS)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    take_att_INTERNAL4_C1(Key, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    take_att_INTERNAL4_C2(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    take_att_INTERNAL4_K1(?INTERNAL4_ARGS)
            end;
        %
        true ->
            take_att_INTERNAL4_K2(?INTERNAL4_ARGS)
    end.

-compile({inline, take_att_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C1(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C1) of
        ?TAKEN(Pair, UpdatedC1) ->
            ?TAKEN(Pair, ?INTERNAL4_C1_REBALANCE(UpdatedC1));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C2(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C2) of
        ?TAKEN(Pair, UpdatedC2) ->
            ?TAKEN(Pair, ?INTERNAL4_C2_REBALANCE(UpdatedC2));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C3(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C3) of
        ?TAKEN(Pair, UpdatedC3) ->
            ?TAKEN(Pair, ?INTERNAL4_C3_REBALANCE(UpdatedC3));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C4(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C4) of
        ?TAKEN(Pair, UpdatedC4) ->
            ?TAKEN(Pair, ?INTERNAL4_C4_REBALANCE(UpdatedC4));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
take_att_INTERNAL4_C5(Key, ?INTERNAL4_ARGS) ->
    case take_att_recur(Key, C5) of
        ?TAKEN(Pair, UpdatedC5) ->
            ?TAKEN(Pair, ?INTERNAL4_C5_REBALANCE(UpdatedC5));
        %
        none ->
            none
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
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    take_att_INTERNAL3_C1(Key, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    take_att_INTERNAL3_C2(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    take_att_INTERNAL3_K1(?INTERNAL3_ARGS)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    take_att_INTERNAL3_C3(Key, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    take_att_INTERNAL3_C4(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    take_att_INTERNAL3_K3(?INTERNAL3_ARGS)
            end;
        %
        true ->
            take_att_INTERNAL3_K2(?INTERNAL3_ARGS)
    end.

-compile({inline, take_att_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
take_att_INTERNAL3_C1(Key, ?INTERNAL3_ARGS) ->
    case take_att_recur(Key, C1) of
        ?TAKEN(Pair, UpdatedC1) ->
            ?TAKEN(Pair, ?INTERNAL3_C1_REBALANCE(UpdatedC1));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
take_att_INTERNAL3_C2(Key, ?INTERNAL3_ARGS) ->
    case take_att_recur(Key, C2) of
        ?TAKEN(Pair, UpdatedC2) ->
            ?TAKEN(Pair, ?INTERNAL3_C2_REBALANCE(UpdatedC2));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
take_att_INTERNAL3_C3(Key, ?INTERNAL3_ARGS) ->
    case take_att_recur(Key, C3) of
        ?TAKEN(Pair, UpdatedC3) ->
            ?TAKEN(Pair, ?INTERNAL3_C3_REBALANCE(UpdatedC3));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
take_att_INTERNAL3_C4(Key, ?INTERNAL3_ARGS) ->
    case take_att_recur(Key, C4) of
        ?TAKEN(Pair, UpdatedC4) ->
            ?TAKEN(Pair, ?INTERNAL3_C4_REBALANCE(UpdatedC4));
        %
        none ->
            none
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
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    take_att_INTERNAL2_C2(Key, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    take_att_INTERNAL2_C3(Key, ?INTERNAL2_ARGS);
                %
                true ->
                    take_att_INTERNAL2_K2(?INTERNAL2_ARGS)
            end;
        %
        Key < K1 ->
            take_att_INTERNAL2_C1(Key, ?INTERNAL2_ARGS);
        %
        true ->
            take_att_INTERNAL2_K1(?INTERNAL2_ARGS)
    end.

-compile({inline, take_att_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
take_att_INTERNAL2_C1(Key, ?INTERNAL2_ARGS) ->
    case take_att_recur(Key, C1) of
        ?TAKEN(Pair, UpdatedC1) ->
            ?TAKEN(Pair, ?INTERNAL2_C1_REBALANCE(UpdatedC1));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
take_att_INTERNAL2_C2(Key, ?INTERNAL2_ARGS) ->
    case take_att_recur(Key, C2) of
        ?TAKEN(Pair, UpdatedC2) ->
            ?TAKEN(Pair, ?INTERNAL2_C2_REBALANCE(UpdatedC2));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
take_att_INTERNAL2_C3(Key, ?INTERNAL2_ARGS) ->
    case take_att_recur(Key, C3) of
        ?TAKEN(Pair, UpdatedC3) ->
            ?TAKEN(Pair, ?INTERNAL2_C3_REBALANCE(UpdatedC3));
        %
        none ->
            none
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
    if
        Key < K1 ->
            take_att_INTERNAL1_C1(Key, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            take_att_INTERNAL1_C2(Key, ?INTERNAL1_ARGS);
        %
        true ->
            take_att_INTERNAL1_K1(?INTERNAL1_ARGS)
    end.

-compile({inline, take_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
take_att_INTERNAL1_C1(Key, ?INTERNAL1_ARGS) ->
    case take_att_recur(Key, C1) of
        ?TAKEN(Pair, UpdatedC1) ->
            ?TAKEN(Pair, ?INTERNAL1_C1_REBALANCE(UpdatedC1));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
take_att_INTERNAL1_C2(Key, ?INTERNAL1_ARGS) ->
    case take_att_recur(Key, C2) of
        ?TAKEN(Pair, UpdatedC2) ->
            ?TAKEN(Pair, ?INTERNAL1_C2_REBALANCE(UpdatedC2));
        %
        none ->
            none
    end.

-compile({inline, take_att_INTERNAL1_K1 / ?INTERNAL1_ARITY}).
take_att_INTERNAL1_K1(?INTERNAL1_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_att_INTERNAL1_K1(?INTERNAL1_ARGS)).

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_largest/2
%% ------------------------------------------------------------------

take_largest_recur(Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            take_largest_INTERNAL2(?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            take_largest_INTERNAL3(?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_largest_INTERNAL4(?INTERNAL4_ARGS);
        %
        nil ->
            none
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, take_largest_INTERNAL4 / ?INTERNAL4_ARITY}).
take_largest_INTERNAL4(?INTERNAL4_ARGS) ->
    case take_largest_recur(C5) of
        ?TAKEN(Taken, UpdatedC5) ->
            ?TAKEN(Taken, ?INTERNAL4_C5_REBALANCE(UpdatedC5));
        %
        none ->
            ?TAKEN_PAIR(
                K4,
                V4,
                ?new_INTERNAL3(
                    K1,
                    K2,
                    K3,
                    %
                    V1,
                    V2,
                    V3,
                    %
                    nil,
                    nil,
                    nil,
                    nil
                )
            )
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, take_largest_INTERNAL3 / ?INTERNAL3_ARITY}).
take_largest_INTERNAL3(?INTERNAL3_ARGS) ->
    case take_largest_recur(C4) of
        ?TAKEN(Taken, UpdatedC4) ->
            ?TAKEN(Taken, ?INTERNAL3_C4_REBALANCE(UpdatedC4));
        %
        none ->
            ?TAKEN_PAIR(
                K3,
                V3,
                ?new_INTERNAL2(
                    K1,
                    K2,
                    %
                    V1,
                    V2,
                    %
                    nil,
                    nil,
                    nil
                )
            )
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, take_largest_INTERNAL2 / ?INTERNAL2_ARITY}).
take_largest_INTERNAL2(?INTERNAL2_ARGS) ->
    case take_largest_recur(C3) of
        ?TAKEN(Taken, UpdatedC3) ->
            ?TAKEN(Taken, ?INTERNAL2_C3_REBALANCE(UpdatedC3));
        %
        none ->
            ?TAKEN_PAIR(
                K2,
                V2,
                ?new_INTERNAL1(
                    K1,
                    %
                    V1,
                    %
                    nil,
                    nil
                )
            )
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, take_largest_INTERNAL1 / ?INTERNAL1_ARITY}).
take_largest_INTERNAL1(?INTERNAL1_ARGS) ->
    case take_largest_recur(C2) of
        ?TAKEN(Taken, UpdatedC2) ->
            ?TAKEN(Taken, ?INTERNAL1_C2_REBALANCE(UpdatedC2));
        %
        none ->
            ?TAKEN_PAIR(K1, V1, ?EMPTY_ROOT)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_smallest/2
%% ------------------------------------------------------------------

take_smallest_recur(Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            take_smallest_INTERNAL2(?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            take_smallest_INTERNAL3(?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_smallest_INTERNAL4(?INTERNAL4_ARGS);
        %
        nil ->
            none
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, take_smallest_INTERNAL4 / ?INTERNAL4_ARITY}).
take_smallest_INTERNAL4(?INTERNAL4_ARGS) ->
    case take_smallest_recur(C1) of
        none ->
            ?TAKEN_PAIR(
                K1,
                V1,
                ?new_INTERNAL3(
                    K2,
                    K3,
                    K4,
                    %
                    V2,
                    V3,
                    V4,
                    %
                    nil,
                    nil,
                    nil,
                    nil
                )
            );
        %
        ?TAKEN(Taken, UpdatedC1) ->
            ?TAKEN(Taken, ?INTERNAL4_C1_REBALANCE(UpdatedC1))
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, take_smallest_INTERNAL3 / ?INTERNAL3_ARITY}).
take_smallest_INTERNAL3(?INTERNAL3_ARGS) ->
    case take_smallest_recur(C1) of
        ?TAKEN(Taken, UpdatedC1) ->
            ?TAKEN(Taken, ?INTERNAL3_C1_REBALANCE(UpdatedC1));
        %
        none ->
            ?TAKEN_PAIR(
                K1,
                V1,
                ?new_INTERNAL2(
                    K2,
                    K3,
                    %
                    V2,
                    V3,
                    %
                    nil,
                    nil,
                    nil
                )
            )
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, take_smallest_INTERNAL2 / ?INTERNAL2_ARITY}).
take_smallest_INTERNAL2(?INTERNAL2_ARGS) ->
    case take_smallest_recur(C1) of
        ?TAKEN(Taken, UpdatedC1) ->
            ?TAKEN(Taken, ?INTERNAL2_C1_REBALANCE(UpdatedC1));
        %
        none ->
            ?TAKEN_PAIR(
                K1,
                V1,
                ?new_INTERNAL1(
                    K2,
                    %
                    V2,
                    %
                    nil,
                    nil
                )
            )
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, take_smallest_INTERNAL1 / ?INTERNAL1_ARITY}).
take_smallest_INTERNAL1(?INTERNAL1_ARGS) ->
    case take_smallest_recur(C1) of
        ?TAKEN(Taken, UpdatedC1) ->
            ?TAKEN(Taken, ?INTERNAL1_C1_REBALANCE(UpdatedC1));
        %
        none ->
            ?TAKEN_PAIR(K1, V1, ?EMPTY_ROOT)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: to_list/1
%% ------------------------------------------------------------------

to_list_recur(Node, Acc) ->
    case Node of
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
            _Acc6 = to_list_recur(C1, [{K1, V1} | Acc5]);
        %
        nil ->
            Acc
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
        nil ->
            none
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, update_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            update_att_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            update_att_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        true ->
                            update_att_INTERNAL4_K3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
                    end;
                %
                Key > K4 ->
                    update_att_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    update_att_INTERNAL4_K4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    update_att_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    update_att_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    update_att_INTERNAL4_K1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
            end;
        %
        true ->
            update_att_INTERNAL4_K2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
    end.

-compile({inline, update_att_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C1) of
        none ->
            none;
        %
        UpdatedC1 ->
            ?INTERNAL4_UPD_C1(UpdatedC1)
    end.

-compile({inline, update_att_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C2) of
        none ->
            none;
        %
        UpdatedC2 ->
            ?INTERNAL4_UPD_C2(UpdatedC2)
    end.

-compile({inline, update_att_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C3) of
        none ->
            none;
        %
        UpdatedC3 ->
            ?INTERNAL4_UPD_C3(UpdatedC3)
    end.

-compile({inline, update_att_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C4) of
        none ->
            none;
        %
        UpdatedC4 ->
            ?INTERNAL4_UPD_C4(UpdatedC4)
    end.

-compile({inline, update_att_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS3}).
update_att_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C5) of
        none ->
            none;
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
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    update_att_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    update_att_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    update_att_INTERNAL3_K1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    update_att_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    update_att_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    update_att_INTERNAL3_K3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
            end;
        %
        true ->
            update_att_INTERNAL3_K2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
    end.

-compile({inline, update_att_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C1) of
        none ->
            none;
        %
        UpdatedC1 ->
            ?INTERNAL3_UPD_C1(UpdatedC1)
    end.

-compile({inline, update_att_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C2) of
        none ->
            none;
        %
        UpdatedC2 ->
            ?INTERNAL3_UPD_C2(UpdatedC2)
    end.

-compile({inline, update_att_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C3) of
        none ->
            none;
        %
        UpdatedC3 ->
            ?INTERNAL3_UPD_C3(UpdatedC3)
    end.

-compile({inline, update_att_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS3}).
update_att_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C4) of
        none ->
            none;
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
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    update_att_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    update_att_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                true ->
                    update_att_INTERNAL2_K2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS)
            end;
        %
        Key < K1 ->
            update_att_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        true ->
            update_att_INTERNAL2_K1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS)
    end.

-compile({inline, update_att_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS3}).
update_att_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C1) of
        none ->
            none;
        %
        UpdatedC1 ->
            ?INTERNAL2_UPD_C1(UpdatedC1)
    end.

-compile({inline, update_att_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS3}).
update_att_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C2) of
        none ->
            none;
        %
        UpdatedC2 ->
            ?INTERNAL2_UPD_C2(UpdatedC2)
    end.

-compile({inline, update_att_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS3}).
update_att_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C3) of
        none ->
            none;
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
    if
        Key < K1 ->
            update_att_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            update_att_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        true ->
            update_att_INTERNAL1_K1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS)
    end.

-compile({inline, update_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS3}).
update_att_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C1) of
        none ->
            none;
        %
        UpdatedC1 ->
            ?INTERNAL1_UPD_C1(UpdatedC1)
    end.

-compile({inline, update_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS3}).
update_att_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case update_att_recur(Key, ValueEval, ValueWrap, C2) of
        none ->
            none;
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
            values_recur(C1, Acc5);
        %
        nil ->
            Acc
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
        %
        UpdatedC2 ->
            ?new_INTERNAL1(K1, V1, C1, UpdatedC2)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Node Split
%% ------------------------------------------------------------------

insert_split_root(Pos, [NewKey | NewValue], Root) ->
    ?INTERNAL4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4, _, _, _, _, _) = Root,

    case Pos of
        1 ->
            ?new_INTERNAL1(
                K2,
                V2,
                ?new_INTERNAL2(NewKey, K1, NewValue, V1, nil, nil, nil),
                ?new_INTERNAL2(K3, K4, V3, V4, nil, nil, nil)
            );
        %
        2 ->
            ?new_INTERNAL1(
                K2,
                V2,
                ?new_INTERNAL2(K1, NewKey, V1, NewValue, nil, nil, nil),
                ?new_INTERNAL2(K3, K4, V3, V4, nil, nil, nil)
            );
        %
        3 ->
            ?new_INTERNAL1(
                NewKey,
                NewValue,
                ?new_INTERNAL2(K1, K2, V1, V2, nil, nil, nil),
                ?new_INTERNAL2(K3, K4, V3, V4, nil, nil, nil)
            );
        %
        4 ->
            ?new_INTERNAL1(
                K3,
                V3,
                ?new_INTERNAL2(K1, K2, V1, V2, nil, nil, nil),
                ?new_INTERNAL2(NewKey, K4, NewValue, V4, nil, nil, nil)
            );
        %
        5 ->
            ?new_INTERNAL1(
                K3,
                V3,
                ?new_INTERNAL2(K1, K2, V1, V2, nil, nil, nil),
                ?new_INTERNAL2(K4, NewKey, V4, NewValue, nil, nil, nil)
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
        ?INTERNAL2_MATCH(LK1, LK2, LV1, LV2, _, _, _) ->
            UpdatedLeft = ?new_INTERNAL3(LK1, LK2, ParentK, LV1, LV2, ParentV, nil, nil, nil, nil),
            ins_rebalance_into_left_leaf(Node, Pos, NewKey, NewValue, UpdatedLeft);
        %
        _ ->
            ins_rebalance_split_leaf(Node, Pos, NewKey, NewValue)
    end.

-compile({inline, ins_rebalance_into_left_leaf/5}).
ins_rebalance_into_left_leaf(
    ?INTERNAL4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4, _, _, _, _, _) = Node,
    Pos,
    NewKey,
    NewValue,
    UpdatedLeft
) ->
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
            UpdatedNode = ?new_INTERNAL4(
                NewKey, K2, K3, K4, NewValue, V2, V3, V4, nil, nil, nil, nil, nil
            ),
            {UpKey, UpValue, UpdatedLeft, UpdatedNode};
        %
        3 ->
            UpKey = K1,
            UpValue = V1,
            UpdatedNode = ?new_INTERNAL4(
                K2, NewKey, K3, K4, V2, NewValue, V3, V4, nil, nil, nil, nil, nil
            ),
            {UpKey, UpValue, UpdatedLeft, UpdatedNode};
        %
        4 ->
            UpKey = K1,
            UpValue = V1,
            UpdatedNode = ?new_INTERNAL4(
                K2, K3, NewKey, K4, V2, V3, NewValue, V4, nil, nil, nil, nil, nil
            ),
            {UpKey, UpValue, UpdatedLeft, UpdatedNode};
        %
        5 ->
            UpKey = K1,
            UpValue = V1,
            UpdatedNode = ?new_INTERNAL4(
                K2, K3, K4, NewKey, V2, V3, V4, NewValue, nil, nil, nil, nil, nil
            ),
            {UpKey, UpValue, UpdatedLeft, UpdatedNode}
    end.

-compile({inline, ins_rebalance_split_leaf/4}).
ins_rebalance_split_leaf(
    ?INTERNAL4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4, _, _, _, _, _), Pos, NewKey, NewValue
) ->
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
        ?INTERNAL2_MATCH(RK1, RK2, RV1, RV2, _, _, _) ->
            UpdatedRight = ?new_INTERNAL3(ParentK, RK1, RK2, ParentV, RV1, RV2, nil, nil, nil, nil),
            ins_rebalance_into_right_leaf(Node, Pos, NewKey, NewValue, UpdatedRight);
        %
        _ ->
            ins_rebalance_split_leaf(Node, Pos, NewKey, NewValue)
    end.

-compile({inline, ins_rebalance_into_right_leaf/5}).
ins_rebalance_into_right_leaf(
    ?INTERNAL4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4, _, _, _, _, _) = Node,
    Pos,
    NewKey,
    NewValue,
    UpdatedRight
) ->
    case Pos of
        1 ->
            UpKey = K4,
            UpValue = V4,
            UpdatedNode = ?new_INTERNAL4(
                NewKey, K1, K2, K3, NewValue, V1, V2, V3, nil, nil, nil, nil, nil
            ),
            {UpKey, UpValue, UpdatedNode, UpdatedRight};
        %
        2 ->
            UpKey = K4,
            UpValue = V4,
            UpdatedNode = ?new_INTERNAL4(
                K1, NewKey, K2, K3, V1, NewValue, V2, V3, nil, nil, nil, nil, nil
            ),
            {UpKey, UpValue, UpdatedNode, UpdatedRight};
        %
        3 ->
            UpKey = K4,
            UpValue = V4,
            UpdatedNode = ?new_INTERNAL4(
                K1, K2, NewKey, K3, V1, V2, NewValue, V3, nil, nil, nil, nil, nil
            ),
            {UpKey, UpValue, UpdatedNode, UpdatedRight};
        %
        4 ->
            UpKey = K4,
            UpValue = V4,
            UpdatedNode = ?new_INTERNAL4(
                K1, K2, K3, NewKey, V1, V2, V3, NewValue, nil, nil, nil, nil, nil
            ),
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
        none ->
            none;
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
        del_rebalance_maybe_from_either_sibling(
            C2,
            %
            K1,
            V1,
            C1,
            %
            K2,
            V2,
            C3
        )
    of
        balanced ->
            ?INTERNAL4_UPD_C2(C2);
        %
        none ->
            none;
        %
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

-compile({inline, del_rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C3(?INTERNAL4_ARGS) ->
    case
        del_rebalance_maybe_from_either_sibling(
            C3,
            %
            K2,
            V2,
            C2,
            %
            K3,
            V3,
            C4
        )
    of
        balanced ->
            ?INTERNAL4_UPD_C3(C3);
        %
        none ->
            none;
        %
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

-compile({inline, del_rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C4(?INTERNAL4_ARGS) ->
    case
        del_rebalance_maybe_from_either_sibling(
            C4,
            %
            K3,
            V3,
            C3,
            %
            K4,
            V4,
            C5
        )
    of
        balanced ->
            ?INTERNAL4_UPD_C4(C4);
        %
        none ->
            none;
        %
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

-compile({inline, del_rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C5(?INTERNAL4_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C5, K4, V4, C4) of
        balanced ->
            ?INTERNAL4_UPD_C5(C5);
        %
        none ->
            none;
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
        none ->
            none;
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
        del_rebalance_maybe_from_either_sibling(
            C2,
            %
            K1,
            V1,
            C1,
            %
            K2,
            V2,
            C3
        )
    of
        balanced ->
            ?INTERNAL3_UPD_C2(C2);
        %
        none ->
            none;
        %
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

-compile({inline, del_rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C3(?INTERNAL3_ARGS) ->
    case
        del_rebalance_maybe_from_either_sibling(
            C3,
            %
            K2,
            V2,
            C2,
            %
            K3,
            V3,
            C4
        )
    of
        balanced ->
            ?INTERNAL3_UPD_C3(C3);
        %
        none ->
            none;
        %
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

-compile({inline, del_rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C4(?INTERNAL3_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C4, K3, V3, C3) of
        balanced ->
            ?INTERNAL3_UPD_C4(C4);
        %
        none ->
            none;
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
        none ->
            none;
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
        del_rebalance_maybe_from_either_sibling(
            C2,
            %
            K1,
            V1,
            C1,
            %
            K2,
            V2,
            C3
        )
    of
        balanced ->
            ?INTERNAL2_UPD_C2(C2);
        %
        none ->
            none;
        %
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

-compile({inline, del_rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY}).
del_rebalance_INTERNAL2_C3(?INTERNAL2_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C3, K2, V2, C2) of
        balanced ->
            ?INTERNAL2_UPD_C3(C3);
        %
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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
        none ->
            none;
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

%% ------------------------------------------------------------------
%% Internal Function Definitions: Deletion - Rebalance a node from either left/right sibling
%% ------------------------------------------------------------------

-compile({inline, del_rebalance_maybe_from_either_sibling/7}).
del_rebalance_maybe_from_either_sibling(
    Child,
    %
    LParentK,
    LParentV,
    Left,
    %
    RParentK,
    RParentV,
    Right
) ->
    case Child of
        ?INTERNAL1_MATCH(CKey, CValue, CLeft, CRight) ->
            del_rebalance_internal_from_either_sibling(
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
            );
        %
        none ->
            none;
        %
        _ ->
            balanced
    end.

%-compile({inline, del_rebalance_internal_from_either_sibling/10}).
del_rebalance_internal_from_either_sibling(
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
            'INTERNAL4'
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
