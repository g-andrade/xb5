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
bulk_construction_params(S) when S < 5 ->
    % 2 bits
    [1 | 1];
bulk_construction_params(S) when S < 21 ->
    % 5 bits
    [5 | 4];
bulk_construction_params(S) when S < 85 ->
    % 7 bits
    [21 | 16];
bulk_construction_params(S) when S < 341 ->
    % 9 bits
    [85 | 64];
bulk_construction_params(S) when S < 1365 ->
    % 11 bits
    [341 | 256];
bulk_construction_params(S) when S < 5461 ->
    % 13 bits
    [1365 | 1024];
bulk_construction_params(S) when S < 21845 ->
    % 15 bits
    [5461 | 4096];
bulk_construction_params(S) when S < 87381 ->
    % 17 bits
    [21845 | 16384];
bulk_construction_params(S) when S < 349525 ->
    % 19 bits
    [87381 | 65536];
bulk_construction_params(S) when S < 1398101 ->
    % 21 bits
    [349525 | 262144];
bulk_construction_params(S) when S < 5592405 ->
    % 23 bits
    [1398101 | 1048576];
bulk_construction_params(S) when S < 22369621 ->
    % 25 bits
    [5592405 | 4194304];
bulk_construction_params(S) when S < 89478485 ->
    % 27 bits
    [22369621 | 16777216];
bulk_construction_params(S) when S < 357913941 ->
    % 29 bits
    [89478485 | 67108864];
bulk_construction_params(S) when S < 1431655765 ->
    % 31 bits
    [357913941 | 268435456];
bulk_construction_params(S) when S < 5726623061 ->
    % 33 bits
    [1431655765 | 1073741824];
bulk_construction_params(S) when S < 22906492245 ->
    % 35 bits
    [5726623061 | 4294967296];
bulk_construction_params(S) when S < 91625968981 ->
    % 37 bits
    [22906492245 | 17179869184];
bulk_construction_params(S) when S < 366503875925 ->
    % 39 bits
    [91625968981 | 68719476736];
bulk_construction_params(S) when S < 1466015503701 ->
    % 41 bits
    [366503875925 | 274877906944];
bulk_construction_params(S) when S < 5864062014805 ->
    % 43 bits
    [1466015503701 | 1099511627776];
bulk_construction_params(S) when S < 23456248059221 ->
    % 45 bits
    [5864062014805 | 4398046511104];
bulk_construction_params(S) when S < 93824992236885 ->
    % 47 bits
    [23456248059221 | 17592186044416];
bulk_construction_params(S) when S < 375299968947541 ->
    % 49 bits
    [93824992236885 | 70368744177664];
bulk_construction_params(S) when S < 1501199875790165 ->
    % 51 bits
    [375299968947541 | 281474976710656];
bulk_construction_params(S) when S < 6004799503160661 ->
    % 53 bits
    [1501199875790165 | 1125899906842624];
bulk_construction_params(S) when S < 24019198012642645 ->
    % 55 bits
    [6004799503160661 | 4503599627370496];
bulk_construction_params(S) when S < 96076792050570581 ->
    % 57 bits
    [24019198012642645 | 18014398509481984];
bulk_construction_params(S) when S < 384307168202282325 ->
    % 59 bits
    [96076792050570581 | 72057594037927936];
bulk_construction_params(S) when S < 1537228672809129301 ->
    % 61 bits
    [384307168202282325 | 288230376151711744];
bulk_construction_params(S) when S < 6148914691236517205 ->
    % 63 bits
    [1537228672809129301 | 1152921504606846976];
bulk_construction_params(S) when S < 24595658764946068821 ->
    % 65 bits
    [6148914691236517205 | 4611686018427387904].

%bulk_construction_params_fun() ->
%    Name = "bulk_construction_params",
%    Variants = compute_bulk_construction_params(),
%
%    Clauses = lists:map(
%        fun (Variant) -> bulk_construction_params_fun_clause(Name, Variant) end,
%        Variants
%    ),
%
%    iolist_to_binary([
%        [
%            "-spec ",
%            Name,
%            "(non_neg_integer()) -> nonempty_improper_list(BatchOffset, BatchSize)\n"
%            "    when BatchOffset :: pos_integer(), BatchSize :: pos_integer().\n"
%        ],
%        lists:join(";\n", Clauses),
%        ".\n"
%    ]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%compute_bulk_construction_params() ->
%    compute_bulk_construction_params_recur(1, 1).
%
%compute_bulk_construction_params_recur(BatchOffset, BatchSize) when BatchOffset < 1 bsl 64 ->
%    Params = [BatchOffset | BatchSize],
%    NextBatchSize = 4 * BatchSize,
%    NextOffset = BatchOffset + NextBatchSize,
%    [{NextOffset, Params} | compute_bulk_construction_params_recur(NextOffset, NextBatchSize)];
%compute_bulk_construction_params_recur(_BatchOffset, _BatchSize) ->
%    [].
%
%bulk_construction_params_fun_clause(Name, {NextOffset, Params}) ->
%    BitSize = ceil(math:log2(NextOffset - 1)),
%    io_lib:format(
%        "~s(S) when S < ~p ->~n"
%        "    % ~p bits~n"
%        "    ~p",
%        [Name, NextOffset, BitSize, Params]).
