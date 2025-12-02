-module(b5_ranks_h2b).

% Max keys computed by the recursive formula:
% 4 + 5 * max_keys(Height - 1)

-define(HEIGHT_TO_BITSIZE, {
    % max keys 4
    4,
    % max keys 24
    6,
    % max keys 124
    8,
    % max keys 624
    11,
    % max keys 3124
    13,
    % max keys 15624
    15,
    % max keys 78124
    18,
    % max keys 390624
    20,
    % max keys 1953124
    22,
    % max keys 9765624
    25,
    % max keys 48828124
    27,
    % max keys 244140624
    29,
    % max keys 1220703124
    32,
    % max keys 6103515624
    34,
    % max keys 30517578124
    36,
    % max keys 152587890624
    39,
    % max keys 762939453124
    41,
    % max keys 3814697265624
    43,
    % max keys 19073486328124
    46,
    % max keys 95367431640624
    48,
    % max keys 476837158203124
    50,
    % max keys 2384185791015624
    53,
    % max keys 11920928955078124
    55,
    % max keys 59604644775390624
    57,
    % max keys 298023223876953124
    60,
    % max keys 1490116119384765624
    62,
    % max keys 7450580596923828124
    64
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
