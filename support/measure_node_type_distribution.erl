-module(measure_node_type_distribution).

-define(ALL_NODE_TYPES, [internal4, internal3, internal2, internal1, leaf4, leaf3, leaf2, leaf1]).

-export([run/0]).

run() ->
    AllTreeSizes = all_tree_sizes(1, 20000),
    SumsPerSize = pmap:pmap(fun collect_data_for_size/1, AllTreeSizes),
    Rows = accumulate_data_rows(SumsPerSize, #{}),

    CsvHeaders = lists:join($,, [$n | lists:map(fun integer_to_binary/1, AllTreeSizes)]),

    CsvRows = lists:map(
        fun ({NodeType, Samples}) ->
            lists:join($,, [atom_to_binary(NodeType) | lists:map(fun float_to_binary/1, Samples)])
        end,
        Rows
    ),

    CsvData = lists:join($\n, [CsvHeaders | CsvRows]),

    file:write_file("data.csv", CsvData).

all_tree_sizes(Size, Max) when Size =< Max ->
    [Size | all_tree_sizes(next_size(Size), Max)];
all_tree_sizes(_, _) ->
    [].

next_size(Size) ->
    if
        Size < 10 ->
            Size + 1;

        Size < 100 ->
            Size + 10;

        Size < 1000 ->
            Size + 100;

        Size < 10000 ->
            Size + 1000;

        Size < 100000 ->
            Size + 10000
    end.

collect_data_for_size(TreeSize) ->
    Sums =
    lists:foldr(
      fun (_, Acc) ->
              KeyValues = shuffled( new_desc_sorted_kvs(TreeSize) ),
              Tree = b5_trees:from_list(KeyValues),
              true = (b5_trees:size(Tree) =:= TreeSize),

              {ok, #{node_counts := NodeCounts}} = b5_trees:validate(Tree),

              maps:fold(
                fun (NodeType, Count, SubAcc) ->
                        update_counter(NodeType, +Count, SubAcc)
                end,
                Acc,
                NodeCounts)
      end,
      #{},
      lists:seq(1, 100)),

    {TreeSize, Sums}.

new_desc_sorted_kvs(N) when N > 0 ->
    [{N, value} | new_desc_sorted_kvs(N - 1)];
new_desc_sorted_kvs(0) ->
    [].

shuffled(List) ->
    Weighed = lists:map(fun (Elem) -> {Elem, rand:uniform()} end, List),
    Sorted = lists:keysort(2, Weighed),
    lists:map(fun ({Elem, _}) -> Elem end, Sorted).

update_counter(Key, Delta, Counters) when Delta > 0 ->
    maps:update_with(Key, fun (Prev) -> Prev + Delta end, Delta, Counters).

accumulate_data_rows([{_TreeSize, Sums} | Next], Acc) ->
    Total = maps:fold(fun (_, Count, SubAcc) -> SubAcc + Count end, 0, Sums),

    UpdatedAcc = 
    lists:foldl(
      fun (NodeType, SubAcc) ->
              NodeCount = maps:get(NodeType, Sums, 0),
              AvgPerc = 100 * NodeCount / Total,
              NodeAcc = maps:get(NodeType, Acc, []),
              NewNodeAcc = [AvgPerc | NodeAcc],
              maps:put(NodeType, NewNodeAcc, SubAcc)
      end,
      Acc,
      ?ALL_NODE_TYPES),

    accumulate_data_rows(Next, UpdatedAcc);
accumulate_data_rows([], Acc) ->
    Fixed = 
    maps:map(
      fun (_NodeType, RevSamples) ->
              lists:reverse(RevSamples)
      end,
      Acc),

    lists:keysort(1, maps:to_list(Fixed)).
