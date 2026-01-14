% TODO document
-module(b5_structural_stats).

-include("src/b5_structural_stats.hrl").

-export([
    new/0,
    set_height/2,
    inc/2,
    return/1
]).

%% Types

-type t() :: [stat(), ...].
-export_type([t/0]).

-type stat() ::
    ({height, non_neg_integer()}
    | {node_counts, count_per_node()}
    | {node_percentages, percent_per_node()}
    | {total_keys, non_neg_integer()}
    | {key_percentages, percent_per_node()}).
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

-opaque acc() :: #stats_acc{
    count_internal4 :: non_neg_integer(),
    count_internal3 :: non_neg_integer(),
    count_internal2 :: non_neg_integer(),
    count_internal1 :: non_neg_integer(),
    count_leaf4 :: non_neg_integer(),
    count_leaf3 :: non_neg_integer(),
    count_leaf2 :: non_neg_integer(),
    count_leaf1 :: non_neg_integer(),
    height :: non_neg_integer()
}.
-export_type([acc/0]).

%% API

-spec new() -> acc().
new() ->
    #stats_acc{
        count_internal4 = 0,
        count_internal3 = 0,
        count_internal2 = 0,
        count_internal1 = 0,
        count_leaf4 = 0,
        count_leaf3 = 0,
        count_leaf2 = 0,
        count_leaf1 = 0,
        height = 0
    }.

-spec set_height(pos_integer(), acc()) -> acc().
set_height(Height, #stats_acc{height = RecordHeight} = Acc) ->
    case RecordHeight of
        _ when RecordHeight < Height ->
            Acc#stats_acc{height = Height};
        %
        _ when RecordHeight =:= Height ->
            Acc
    end.

-spec inc(pos_integer(), acc()) -> acc().
inc(Pos, #stats_acc{} = Acc) ->
    setelement(Pos, Acc, element(Pos, Acc) + 1).

-spec return(acc()) -> t().
return(#stats_acc{} = Acc) ->
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

%% Internal

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
