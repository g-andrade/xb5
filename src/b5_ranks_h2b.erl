-module(b5_ranks_h2b).

% Max keys computed by the recursive formula:
% 4 + 5 * max_keys(Height - 1)

-define(HEIGHT_TO_BITSIZE, {
    % max keys 4
    3,
    % max keys 24
    5,
    % max keys 124
    7,
    % max keys 624
    10,
    % max keys 3124
    12,
    % max keys 15624
    14,
    % max keys 78124
    17,
    % max keys 390624
    19,
    % max keys 1953124
    21,
    % max keys 9765624
    24,
    % max keys 48828124
    26,
    % max keys 244140624
    28,
    % max keys 1220703124
    31,
    % max keys 6103515624
    33,
    % max keys 30517578124
    35,
    % max keys 152587890624
    38,
    % max keys 762939453124
    40,
    % max keys 3814697265624
    42,
    % max keys 19073486328124
    45,
    % max keys 95367431640624
    47,
    % max keys 476837158203124
    49,
    % max keys 2384185791015624
    52,
    % max keys 11920928955078124
    54,
    % max keys 59604644775390624
    56,
    % max keys 298023223876953124
    59,
    % max keys 1490116119384765624
    61,
    % max keys 7450580596923828124
    63
}).

-export([increase_height/1, decrease_height/1]).

-type t() :: [pos_integer()].
-export_type([t/0]).

increase_height(HeightBits) ->
    NewHeight = length(HeightBits) + 1,
    NewBitSize = element(NewHeight, ?HEIGHT_TO_BITSIZE),
    [NewBitSize | HeightBits].

decrease_height([_ | RemainingHeightBits]) ->
    RemainingHeightBits.
