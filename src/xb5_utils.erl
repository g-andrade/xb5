%% @private
-module(xb5_utils).

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
