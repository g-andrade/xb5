-module(b5_ranks_h2b).

% Max keys computed by the recursive formula:
% 4 + 5 * max_keys(Height - 1)

-ifdef(TEST).
-define(HEIGHT_TO_BITSIZE, {
    % max keys 4

    % 4,
    small,
    % max keys 24

    % 6,
    small,
    % max keys 124

    % 8,
    small,
    % max keys 624

    % 11,
    small,
    % max keys 3124

    % 13,
    small,
    % max keys 15624

    % 15,
    small,
    % max keys 78124

    % 18,
    big,
    % max keys 390624

    % 20,
    big,
    % max keys 1953124

    % 22,
    big,
    % max keys 9765624

    % 25,
    big,
    % max keys 48828124

    % 27,
    big,
    % max keys 244140624

    % 29,
    big,
    % max keys 1220703124

    % 32,
    big,
    % max keys 6103515624

    % 34,
    big,
    % max keys 30517578124

    % 36,
    big,
    % max keys 152587890624

    % 39,
    big,
    % max keys 762939453124

    % 41,
    big,
    % max keys 3814697265624

    % 43,
    big,
    % max keys 19073486328124

    % 46,
    big,
    % max keys 95367431640624

    % 48,
    big,
    % max keys 476837158203124

    % 50,
    big,
    % max keys 2384185791015624

    % 53,
    big,
    % max keys 11920928955078124

    % 55,
    big,
    % max keys 59604644775390624

    % 57,
    big,
    % max keys 298023223876953124

    % 60,
    big,
    % max keys 1490116119384765624

    % 62,
    big,
    % max keys 7450580596923828124

    % 64
    big
}).
-else.
-define(HEIGHT_TO_BITSIZE, {
    % max keys 4

    % 4,
    small,
    % max keys 24

    % 6,
    small,
    % max keys 124

    % 8,
    small,
    % max keys 624

    % 11,
    small,
    % max keys 3124

    % 13,
    small,
    % max keys 15624

    % 15,
    small,
    % max keys 78124

    % 18,
    small,
    % max keys 390624

    % 20,
    small,
    % max keys 1953124

    % 22,
    small,
    % max keys 9765624

    % 25,
    small,
    % max keys 48828124

    % 27,
    small,
    % max keys 244140624

    % 29,
    big,
    % max keys 1220703124

    % 32,
    big,
    % max keys 6103515624

    % 34,
    big,
    % max keys 30517578124

    % 36,
    big,
    % max keys 152587890624

    % 39,
    big,
    % max keys 762939453124

    % 41,
    big,
    % max keys 3814697265624

    % 43,
    big,
    % max keys 19073486328124

    % 46,
    big,
    % max keys 95367431640624

    % 48,
    big,
    % max keys 476837158203124

    % 50,
    big,
    % max keys 2384185791015624

    % 53,
    big,
    % max keys 11920928955078124

    % 55,
    big,
    % max keys 59604644775390624

    % 57,
    big,
    % max keys 298023223876953124

    % 60,
    big,
    % max keys 1490116119384765624

    % 62,
    big,
    % max keys 7450580596923828124

    % 64
    big
}).
-endif.

-export([increase_height/1, decrease_height/1, height_to_b/1]).

-type t() :: [pos_integer()].
-export_type([t/0]).

increase_height(HeightBits) ->
    NewHeight = length(HeightBits) + 1,
    NewBitSize = element(NewHeight, ?HEIGHT_TO_BITSIZE),
    [NewBitSize | HeightBits].

decrease_height([_ | RemainingHeightBits]) ->
    RemainingHeightBits.

height_to_b(Height) ->
    % FIXME
    % case Height =< 11 of
    case Height =< 0 of
        true ->
            small;
        _ ->
            big
    end.
