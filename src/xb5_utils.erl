-module(xb5_utils).

-ifdef(E48).
-moduledoc false.
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    dialyzer_opaque_term/1,
    bulk_construction_params/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec dialyzer_opaque_term(term()) -> term().
dialyzer_opaque_term(V) ->
    V.

%% Seeds bulk construction of a tree of `S' elements from already-ordered
%% input: xb5_sets_node:from_ordset/2, xb5_trees_node:from_orddict/2 and
%% xb5_bag_node:from_ordered_list/2 all start here, then recurse with the
%% pair returned.
%%
%% The arithmetic counts in base 4, because bulk construction aims for nodes
%% of 3 keys and 4 children rather than the maximum 4 and 5 - the key density
%% the README argues for under "Key density". Writing the levels out:
%%
%%     S range     BatchOffset   BatchSize   BatchSize - 1
%%     < 5         1             1           0
%%     5 .. 20     5             4           3
%%     21 .. 84    21            16          15
%%     85 .. 340   85            64          63
%%     341 ..      341           256         255
%%
%% `BatchSize - 1' is 4^(h+1) - 1, the size of a perfect subtree of height h
%% in which every node holds 3 keys and 4 children. It is the size handed to
%% each fully loaded child.
%%
%% `BatchOffset' is (4^(h+1) - 1) / 3, which is at once the node count of that
%% perfect subtree and the smallest `S' for which this level is chosen. The
%% two coincide only because the target density is exactly 3.
%%
%% So the loop below returns the deepest level whose perfect subtree still
%% fits inside `S'. Feeding it exactly 4^(h+1) - 1 elements yields a tree of
%% precisely 3.000 keys per node and (4^(h+1) - 1) / 3 nodes; the worst case
%% is `S' equal to a BatchOffset, the point at which a new level is forced and
%% the root is left an INTERNAL1.
-spec bulk_construction_params(non_neg_integer()) ->
    nonempty_improper_list(BatchOffset, BatchSize)
when
    BatchOffset :: pos_integer(), BatchSize :: pos_integer().
bulk_construction_params(S) ->
    bulk_construction_params_recur(S, 1, 1).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

bulk_construction_params_recur(S, Offset, BatchSize) ->
    NextBatchSize = BatchSize bsl 2,
    NextOffset = Offset + NextBatchSize,

    case S < NextOffset of
        true ->
            [Offset | BatchSize];
        %
        _ ->
            bulk_construction_params_recur(S, NextOffset, NextBatchSize)
    end.
