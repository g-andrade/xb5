-module(xb5_structural_stats).

-moduledoc """
Structural statistics about a B-tree.

This is primarily intended for debugging and testing.
""".

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    new/0,
    set_height/2,
    inc_count/2,
    return/1
]).

%% ------------------------------------------------------------------
%% Linter Tweaks
%% ------------------------------------------------------------------

-elvis([
    % We really don't want to show `acc/0` in the docs.
    {elvis_style, export_used_types, disable}
]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type t() :: [stat(), ...].
-export_type([t/0]).

-type stat() ::
    ({height, non_neg_integer()}
    | {node_counts, count_per_node()}
    | {node_percentages, percent_per_node()}
    | {total_keys, non_neg_integer()}
    | {key_percentages, percent_per_node()})
    | {avg_keys_per_node, undefined | float()}
    | {avg_keys_per_internal_node, undefined | float()}
    | {avg_keys_per_leaf_node, undefined | float()}.
-export_type([stat/0]).

-type count_per_node() :: [{node_type(), non_neg_integer()}, ...].
-export_type([count_per_node/0]).

-type percent_per_node() :: [{node_type(), number()}, ...].
-export_type([percent_per_node/0]).

-type node_type() ::
    (internal4
    | internal3
    | internal2
    | internal1
    | leaf4
    | leaf3
    | leaf2
    | leaf1).
-export_type([node_type/0]).

-type acc() :: #{
    node_counters := #{node_type() := non_neg_integer()},
    height := non_neg_integer()
}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-dialyzer({no_underspecs, new/0}).

-doc false.
-spec new() -> acc().
new() ->
    #{
        node_counters => #{
            internal4 => 0,
            internal3 => 0,
            internal2 => 0,
            internal1 => 0,
            leaf4 => 0,
            leaf3 => 0,
            leaf2 => 0,
            leaf1 => 0
        },
        height => 0
    }.

-doc false.
-spec set_height(pos_integer(), acc()) -> acc().
set_height(Height, #{height := RecordHeight} = Acc) ->
    case RecordHeight of
        _ when RecordHeight < Height ->
            Acc#{height := Height};
        %
        _ when RecordHeight =:= Height ->
            Acc
    end.

-doc false.
-spec inc_count(node_type(), acc()) -> acc().
inc_count(NodeType, #{node_counters := NodeCounters} = Acc) ->
    Count = map_get(NodeType, NodeCounters),
    Acc#{node_counters := NodeCounters#{NodeType := Count + 1}}.

-doc false.
-spec return(acc()) -> t().
return(#{node_counters := NodeCounters, height := Height}) ->
    NodeCounts = node_counts(NodeCounters),
    NodePercentages = node_percentages(NodeCounts),
    TotalKeys = total_keys(NodeCounts),
    KeyPercentages = key_percentages(NodeCounts, TotalKeys),
    {AvgKeysPerNode, AvgKeysPerInternalNode, AvgKeysPerLeafNode} = avg_keys_per_node(NodeCounts),

    [
        {height, Height},
        {node_counts, NodeCounts},
        {node_percentages, NodePercentages},
        {total_keys, TotalKeys},
        {key_percentages, KeyPercentages},
        {avg_keys_per_node, AvgKeysPerNode},
        {avg_keys_per_internal_node, AvgKeysPerInternalNode},
        {avg_keys_per_leaf_node, AvgKeysPerLeafNode}
    ].

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

node_counts(NodeCounters) ->
    SortedNodeTypes = sort_node_types(maps:keys(NodeCounters)),

    lists:map(
        fun(NodeType) ->
            {NodeType, map_get(NodeType, NodeCounters)}
        end,
        SortedNodeTypes
    ).

sort_node_types(NodeTypes) ->
    WithSortingKeys = node_types_with_sorting_keys(NodeTypes),
    Sorted = lists:keysort(2, WithSortingKeys),
    lists:map(fun({NodeType, _}) -> NodeType end, Sorted).

node_types_with_sorting_keys(NodeTypes) ->
    lists:map(fun(NodeType) -> {NodeType, node_type_sorting_key(NodeType)} end, NodeTypes).

node_type_sorting_key(NodeType) ->
    case atom_to_list(NodeType) of
        "internal" ++ ArityStr ->
            [1 | -list_to_integer(ArityStr)];
        %
        "leaf" ++ ArityStr ->
            [2 | -list_to_integer(ArityStr)]
    end.

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

avg_keys_per_node(NodeCounts) ->
    {KeySum, NodeSum, InternalKSum, InternalNSum} =
        lists:foldl(
            fun({NodeType, Count}, {KeySum, NodeSum, InternalKSum, InternalNSum}) ->
                KeysInNodeType = total_keys_in_node_type(NodeType),
                KeysInc = Count * KeysInNodeType,

                KeySum2 = KeySum + KeysInc,
                NodeSum2 = NodeSum + Count,

                {InternalKSum2, InternalNSum2} =
                    case is_node_type_internal(NodeType) of
                        true ->
                            {
                                InternalKSum + KeysInc,
                                InternalNSum + Count
                            };
                        false ->
                            {InternalKSum, InternalNSum}
                    end,

                {KeySum2, NodeSum2, InternalKSum2, InternalNSum2}
            end,
            {0, 0, 0, 0},
            NodeCounts
        ),

    %%%

    LeafKSum = KeySum - InternalKSum,
    LeafNSum = NodeSum - InternalNSum,

    %%%

    {
        avg(KeySum, NodeSum),
        avg(InternalKSum, InternalNSum),
        avg(LeafKSum, LeafNSum)
    }.

avg(KeySum, _) when KeySum =:= 0 ->
    undefined;
avg(KeySum, NodeSum) ->
    KeySum / NodeSum.

is_node_type_internal(NodeType) ->
    case atom_to_binary(NodeType) of
        <<"internal", _>> ->
            true;
        %
        <<"leaf", _>> ->
            false
    end.

-spec total_keys_in_node_type(node_type()) -> 1..4.
total_keys_in_node_type(internal4) -> 4;
total_keys_in_node_type(internal3) -> 3;
total_keys_in_node_type(internal2) -> 2;
total_keys_in_node_type(internal1) -> 1;
total_keys_in_node_type(leaf4) -> 4;
total_keys_in_node_type(leaf3) -> 3;
total_keys_in_node_type(leaf2) -> 2;
total_keys_in_node_type(leaf1) -> 1.
