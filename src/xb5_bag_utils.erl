-module(xb5_bag_utils).

-moduledoc """
Additional utils for operating over `m:xb5_bag` contents.
""".

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    percentile/5,
    percentile_bracket/4,
    percentile_rank/3
]).

-ifdef(TEST).
-ignore_xref([
    test/0,
    linear_interpolation_test/0
]).
-endif.

%% ------------------------------------------------------------------
%% Linter Tweaks
%% ------------------------------------------------------------------

-elvis([
    % Large URLs below require this
    {elvis_text_style, line_length, #{limit => 131}}
]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-ifdef(E48).
-doc "A percentile value between 0.0 (0th) and 1.0 (100th).".
-endif.
-type percentile() :: 0 | 1 | float().
-export_type([percentile/0]).

%%

-ifdef(E48).
-doc "An option for percentile functions. Currently only `{method, Method}` is supported; see `t:percentile_bracket_method/0`.".
-endif.
-type percentile_bracket_opt() ::
    ({method, percentile_bracket_method()}).
-export_type([percentile_bracket_opt/0]).

%%

-ifdef(E48).
-doc """
The method used to calculate a percentile bracket.

- `inclusive` (default) -- Pos = 1 + (N - 1) * P. Equivalent to Excel
  [PERCENTILE.INC](https://support.microsoft.com/en-us/office/percentile-inc-function-680f9539-45eb-410b-9a5e-c1355e5fe2ed)
  (Hyndman-Fan Type 7). Covers the full `[0.0, 1.0]` range.

- `exclusive` -- Pos = (N + 1) * P. Equivalent to Excel
  [PERCENTILE.EXC](https://support.microsoft.com/en-us/office/percentile-exc-function-bbaa7204-e9e1-4010-85bf-c31dc5dce4ba)
  (Hyndman-Fan Type 6). Returns `none` for percentiles outside the
  representable range.

- `nearest_rank` -- Pos = ceil(P * N). As described in
  [Wikipedia](https://en.wikipedia.org/wiki/Percentile#The_nearest-rank_method).
  Always returns an exact element (no interpolation). Returns `none` for
  percentile `0`.
""".
-endif.
-type percentile_bracket_method() ::
    (inclusive
    | exclusive
    | nearest_rank).

-export_type([percentile_bracket_method/0]).

%%

-ifdef(E48).
-doc """
The result of a percentile bracket calculation.

- `{exact, Element}` -- the percentile falls exactly on an element.
- `{between, A, B, T}` -- the percentile falls between elements A and B, where `T`
  is a float in `(0.0, 1.0)` representing the interpolation factor.
- `none` -- the percentile cannot be calculated (empty bag, or out of
  range for the chosen method).
""".
-endif.
-type percentile_bracket(Element) ::
    ({exact, Element}
    | {between, Element, Element, T :: float()}
    | none).
-export_type([percentile_bracket/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-ifdef(E48).
-doc """
Calculates a percentile value in O(log n) time, using linear interpolation if
the method is either `inclusive` or `exclusive` (default is `inclusive`, see
`t:percentile_bracket_method/0`) and the bracket doesn't fall on one exact
element. Returns `{value, Result}` or `none`.

Raises a `{bracket_value_not_a_number, #{value => Bound, bracket => Bracket}}`
error if linear interpolation is required but the bracketing elements are not
numbers.
""".
-endif.
-spec percentile(Percentile, Size, Root, ValueFun, Opts) -> ValueWrap | none when
    Percentile :: percentile(),
    Size :: non_neg_integer(),
    Root :: xb5_bag_node:t(Element),
    ValueFun :: fun((Element | InterpolationResult) -> ValueWrap),
    InterpolationResult :: number(),
    Opts :: [percentile_bracket_opt()].

percentile(Percentile, Size, Root, ValueFun, Opts) ->
    Bracket = percentile_bracket(Percentile, Size, Root, Opts),
    linear_interpolated_percentile(Bracket, ValueFun).

%%

-ifdef(E48).
-doc """
Returns the percentile bracket for `Percentile` in `Root` node in O(log n)
time, using the method selected (default is `inclusive`; see
`t:percentile_bracket_method/0`).

Returns `{exact, Element}` when the percentile falls exactly on an element,
`{between, A, B, T}` when it falls between two elements, or `none` if the bag
is empty.
""".
-endif.
-spec percentile_bracket(Percentile, Size, Root, Opts) -> Bracket when
    Percentile :: percentile(),
    Size :: non_neg_integer(),
    Root :: xb5_bag_node:t(Element),
    Opts :: [percentile_bracket_opt()],
    Bracket :: percentile_bracket(Element).

percentile_bracket(Percentile, Size, Root, Opts) when
    is_number(Percentile), Percentile >= 0.0, Percentile =< 1.0
->
    case Size of
        0 ->
            none;
        %
        _ ->
            Method = proplists:get_value(method, Opts, inclusive),
            Pos = percentile_bracket_pos(Percentile, Size, Method),
            percentile_bracket_for_pos(Percentile, Pos, Size, Root, Method)
    end;
percentile_bracket(Percentile, _Size, _Root, _Opts) ->
    error({badarg, Percentile}).

-ifdef(E48).
-doc """
Returns the [percentile rank](https://en.wikipedia.org/wiki/Percentile_rank)
of `Element` in non-empty `Root` as a float in `[0.0, 1.0]`, in O(log n) time.

`Element` does not have to be in the bag.
""".
-endif.
-spec percentile_rank(Element, Size, Root) -> Rank when
    Size :: pos_integer(),
    Root :: xb5_bag_node:t(Element),
    Rank :: float().

percentile_rank(Elem, Size, Root) ->
    % As described in Wikipedia:
    % https://en.wikipedia.org/wiki/Percentile_rank
    SmallerPair = xb5_bag_node:rank_smaller(Elem, Root),
    LargerPair = xb5_bag_node:rank_larger(Elem, Root),

    [CF | F] = percentile_rank_params(SmallerPair, LargerPair, Size),

    (CF - 0.5 * F) / Size.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

percentile_bracket_pos(Percentile, Size, inclusive) ->
    1 + (Size - 1) * Percentile;
percentile_bracket_pos(Percentile, Size, exclusive) ->
    (Size + 1) * Percentile;
percentile_bracket_pos(Percentile, Size, nearest_rank) ->
    ceil(Percentile * Size).

percentile_bracket_for_pos(Percentile, Pos, Size, Root, Method) ->
    RankA = floor(Pos),
    RankB = ceil(Pos),

    if
        RankA < 1 orelse RankB > Size ->
            none;
        %
        RankA =:= RankB ->
            Elem = xb5_bag_node:nth(RankA, Root),
            {exact, Elem};
        %
        true ->
            [A | B] = xb5_bag_node:nth_and_nthp1(RankA, Root),

            case A == B of
                true ->
                    {exact, A};
                %
                _ ->
                    PercA = percentile_bracket_perc(RankA, Size, Method),
                    PercB = percentile_bracket_perc(RankB, Size, Method),

                    PercRange = PercB - PercA,
                    T = (Percentile - PercA) / PercRange,

                    {between, A, B, T}
            end
    end.

percentile_bracket_perc(Rank, Size, inclusive) ->
    (Rank - 1) / (Size - 1);
percentile_bracket_perc(Rank, Size, exclusive) ->
    Rank / (Size + 1).

linear_interpolated_percentile({exact, ExactElem}, ValueFun) ->
    ValueFun(ExactElem);
linear_interpolated_percentile({between, A, B, T} = Bracket, ValueFun) ->
    if
        not is_number(A) ->
            error({bracket_value_not_a_number, #{value => A, bracket => Bracket}});
        %
        not is_number(B) ->
            error({bracket_value_not_a_number, #{value => B, bracket => Bracket}});
        %
        true ->
            ValueFun(linear_interpolation(A, B, T))
    end;
linear_interpolated_percentile(none, _ValueFun) ->
    none.

linear_interpolation(A, B, T) when is_integer(A), is_integer(B) ->
    Diff = B - A,

    FloatA = float(A),
    FloatDiff = float(Diff),

    if
        FloatA /= A orelse FloatDiff /= Diff ->
            % Lost of precision, fallback to all integer math
            Magnitude = 1024,
            IntT = round(T * Magnitude),
            A + (IntT * Diff div Magnitude);
        %
        true ->
            FloatA + (T * FloatDiff)
    end;
linear_interpolation(A, B, T) ->
    % If B is a very large integer and A a small float we may lose precision,
    % no way I could think around it.
    A + (T * (B - A)).

%%%%%%%%

percentile_rank_params(none, none, Size) ->
    CF = Size,
    F = Size,
    [CF | F];
percentile_rank_params(none, [LargerRank | _], _) ->
    F = LargerRank - 1,
    CF = F,
    [CF | F];
percentile_rank_params([SmallerRank | _], none, Size) ->
    F = Size - SmallerRank,
    CF = Size,
    [CF | F];
percentile_rank_params([SmallerRank | _], [LargerRank | _], _) ->
    CF = LargerRank - 1,
    F = LargerRank - SmallerRank - 1,
    [CF | F].

%% ------------------------------------------------------------------
%% Unit Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

linear_interpolation_test() ->
    %
    % Avoid loss of precision when converting both A and Diff to float
    %
    L1A = (1 bsl 70) - 100,
    L1B = (1 bsl 70),
    ?assertEqual((1 bsl 70) - 96, linear_interpolation(L1A, L1B, 0.05)),
    ?assertEqual((1 bsl 70) - 50, linear_interpolation(L1A, L1B, 0.50)),
    ?assertEqual((1 bsl 70) - 5, linear_interpolation(L1A, L1B, 0.95)),

    %
    % Avoid loss of precision when converting Diff to float
    %
    L2A = (1 bsl 40),
    L2B = (1 bsl 100),
    ?assertEqual((1 bsl 98) + (1 bsl 39) + (1 bsl 38), linear_interpolation(L2A, L2B, 0.25)),
    ?assertEqual((1 bsl 99) + (1 bsl 39), linear_interpolation(L2A, L2B, 0.50)),
    ?assertEqual((1 bsl 99) + (1 bsl 98) + (1 bsl 38), linear_interpolation(L2A, L2B, 0.75)),

    %
    % No loss of precision to avoid, both large integers represent as floats
    %
    L3A = (1 bsl 69),
    L3B = (1 bsl 70),
    ?assertEqual(precise_float((1 bsl 69) + (1 bsl 67)), linear_interpolation(L3A, L3B, 0.25)),
    ?assertEqual(precise_float((1 bsl 69) + (1 bsl 68)), linear_interpolation(L3A, L3B, 0.50)),
    ?assertEqual(precise_float((1 bsl 69) + 3 * (1 bsl 67)), linear_interpolation(L3A, L3B, 0.75)),

    %
    % Floats
    %
    L4A = 1.3235e2,
    L4B = 9.332343e300,
    ?assertEqual(9.332343e100, linear_interpolation(L4A, L4B, 1.0e-200)),
    ?assertEqual(2.33308575e300, linear_interpolation(L4A, L4B, 0.25)),
    ?assertEqual(4.6661715e300, linear_interpolation(L4A, L4B, 0.50)),
    ?assertEqual(6.99925725e300, linear_interpolation(L4A, L4B, 0.75)),
    ?assertEqual(9.332342999999999e300, linear_interpolation(L4A, L4B, 0.9999999999999999)),

    %
    % Mixed float / very large integer that loses precision
    %
    L5A = math:pow(10, 20),
    L5B = int_pow(10, 30),
    ?assert(L5B /= float(L5B)),

    ?assertEqual(2.50000000075e29, linear_interpolation(L5A, L5B, 0.25)),
    ?assertEqual(5.0000000004999996e29, linear_interpolation(L5A, L5B, 0.50)),
    ?assertEqual(7.50000000025e29, linear_interpolation(L5A, L5B, 0.75)).

precise_float(Integer) when is_integer(Integer) ->
    Float = float(Integer),
    ?assert(Float == Integer),
    Float.

int_pow(_Base, 0) ->
    1;
int_pow(Base, Exponent) when Exponent > 0 ->
    Base * int_pow(Base, Exponent - 1).

-endif.
