% TODO document
-module(b5_trees_node).

-export([
    delete/2,
    foldl/3,
    foldr/3,
    get/2,
    insert/4,
    iterator/2,
    iterator_from/3,
    keys/1,
    larger/2,
    largest/1,
    map/2,
    new/0,
    next/1,
    smaller/2,
    smallest/1,
    structural_stats/1,
    take/2,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    update/4,
    values/1
]).

%% ------------------------------------------------------------------
%% Macro Definitions: Nodes
%% ------------------------------------------------------------------

% 10 elements
-define(INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4),
    {K1, K2, K3, V1, V2, V3, C1, C2, C3, C4}
).
-define(INTERNAL3_MATCH(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4),
    {K1, K2, K3, V1, V2, V3, C1, C2, C3, C4}
).
-define(INTERNAL3_MATCH_ALL, {K1, K2, K3, V1, V2, V3, C1, C2, C3, C4}).

% 6 elements
-define(LEAF3(K1, K2, K3, V1, V2, V3), {K1, K2, K3, V1, V2, V3}).
-define(LEAF3_MATCH(K1, K2, K3, V1, V2, V3), {K1, K2, K3, V1, V2, V3}).
-define(LEAF3_MATCH_ALL, {K1, K2, K3, V1, V2, V3}).

% 13 elements
-define(INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5),
    {K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5}
).
-define(INTERNAL4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5),
    {K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5}
).
-define(INTERNAL4_MATCH_ALL,
    {K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5}
).

% 8 elements
-define(LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), {K1, K2, K3, K4, V1, V2, V3, V4}).
-define(LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4), {K1, K2, K3, K4, V1, V2, V3, V4}).
-define(LEAF4_MATCH_ALL, {K1, K2, K3, K4, V1, V2, V3, V4}).

% 16 elements
-define(INTERNAL5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6),
    {K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6}
).
-define(INTERNAL5_MATCH(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6),
    {K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6}
).
-define(INTERNAL5_MATCH_ALL,
    {K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6}
).

% 11 elements
-define(LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5),
    {leaf5, K1, K2, K3, K4, K5, V1, V2, V3, V4, V5}
).
-define(LEAF5_MATCH(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5),
    {_, K1, K2, K3, K4, K5, V1, V2, V3, V4, V5}
).
-define(LEAF5_MATCH_ALL, {_, K1, K2, K3, K4, K5, V1, V2, V3, V4, V5}).

% 19 elements
-define(INTERNAL6(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7),
    {K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7}
).
-define(INTERNAL6_MATCH(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7),
    {K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7}
).
-define(INTERNAL6_MATCH_ALL,
    {K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7}
).

% 12 elements
-define(LEAF6(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6),
    {K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6}
).
-define(LEAF6_MATCH(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6),
    {K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6}
).
-define(LEAF6_MATCH_ALL, {K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6}).

% 7 elements
-define(INTERNAL2(K1, K2, V1, V2, C1, C2, C3), {K1, K2, V1, V2, C1, C2, C3}).
-define(INTERNAL2_MATCH(K1, K2, V1, V2, C1, C2, C3), {K1, K2, V1, V2, C1, C2, C3}).
-define(INTERNAL2_MATCH_ALL, {K1, K2, V1, V2, C1, C2, C3}).

% 4 elements
-define(LEAF2(K1, K2, V1, V2), {K1, K2, V1, V2}).
-define(LEAF2_MATCH(K1, K2, V1, V2), {K1, K2, V1, V2}).
-define(LEAF2_MATCH_ALL, {K1, K2, V1, V2}).

% 5 elements
-define(INTERNAL1(K1, V1, C1, C2), {internal1, K1, V1, C1, C2}).
-define(INTERNAL1_MATCH(K1, V1, C1, C2), {_, K1, V1, C1, C2}).
-define(INTERNAL1_MATCH_ALL, {_, K1, V1, C1, C2}).

% 2 elements
-define(LEAF1(K1, V1), {K1, V1}).
-define(LEAF1_MATCH(K1, V1), {K1, V1}).
-define(LEAF1_MATCH_ALL, {K1, V1}).

% empty root
-define(LEAF0, leaf0).
-define(LEAF0_MATCH, leaf0).
-define(LEAF0_MATCH_ALL, leaf0).

%%%%%%%%

%-define(INTERNAL2_VALUES(V1, V2), [V1 | V2]).
%-define(INTERNAL2_VALUES_MATCH_ALL, [V1 | V2]).
%-define(INTERNAL2_VALUES_GET2(Values), tl(Values)).
%-define(INTERNAL2_VALUES_GET1(Values), hd(Values)).
%
%-define(INTERNAL3_VALUES(V1, V2, V3), {V1, V2, V3}).
%-define(INTERNAL3_VALUES_MATCH_ALL, {V1, V2, V3}).
%-define(INTERNAL3_VALUES_GET1(Values), element(1, Values)).
%-define(INTERNAL3_VALUES_GET2(Values), element(2, Values)).
%-define(INTERNAL3_VALUES_GET3(Values), element(3, Values)).

%-define(INTERNAL4_VALUES(V1, V2, V3, V4), {V1, V2, V3, V4}).
%-define(INTERNAL4_VALUES_MATCH_ALL, {V1, V2, V3, V4}).
%-define(INTERNAL4_VALUES_GET1(Values), element(1, Values)).
%-define(INTERNAL4_VALUES_GET2(Values), element(2, Values)).
%-define(INTERNAL4_VALUES_GET3(Values), element(3, Values)).
%-define(INTERNAL4_VALUES_GET4(Values), element(4, Values)).

%%%%%%%%

% 5 elements; cannot clash with any node type.
-define(SPLIT(SplitK, SplitV, SplitL, SplitR), {split, SplitK, SplitV, SplitL, SplitR}).
-define(SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR), {split, SplitK, SplitV, SplitL, SplitR}).

%%%%%%%%%

-define(NEEDS_REBALANCE(Pos, MergedChild), [Pos | MergedChild]).

%%%%%%%%%

% 4 elements
-define(ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedRight),
    {UpKey, UpValue, UpdatedLeft, UpdatedRight}
).

-define(MERGED(MergedNode), (MergedNode)).

%%%%%%%

-define(ITER_PAIR(Key, Value), ?LEAF1(Key, Value)).
-define(REV_ITER_TAG, reversed).

%% ------------------------------------------------------------------
%% Macro Definitions: Boilerplate Helpers
%% ------------------------------------------------------------------

%% ?INTERNAL6

-define(INTERNAL6_ARGS, K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7).
-define(INTERNAL6_ARITY, 19).
-define(INTERNAL6_ARITY_PLUS1, 20).
-define(INTERNAL6_ARITY_PLUS2, 21).
-define(INTERNAL6_ARITY_PLUS3, 22).

-define(INTERNAL6_C1(UpdatedC1),
    ?new_INTERNAL6(
        K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, UpdatedC1, C2, C3, C4, C5, C6, C7
    )
).
-define(INTERNAL6_C2(UpdatedC2),
    ?new_INTERNAL6(
        K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, UpdatedC2, C3, C4, C5, C6, C7
    )
).
-define(INTERNAL6_C3(UpdatedC3),
    ?new_INTERNAL6(
        K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, UpdatedC3, C4, C5, C6, C7
    )
).
-define(INTERNAL6_C4(UpdatedC4),
    ?new_INTERNAL6(
        K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, UpdatedC4, C5, C6, C7
    )
).
-define(INTERNAL6_C5(UpdatedC5),
    ?new_INTERNAL6(
        K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, UpdatedC5, C6, C7
    )
).
-define(INTERNAL6_C6(UpdatedC6),
    ?new_INTERNAL6(
        K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, UpdatedC6, C7
    )
).
-define(INTERNAL6_C7(UpdatedC7),
    ?new_INTERNAL6(
        K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, UpdatedC7
    )
).

-define(INTERNAL6_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL6_C1(
        UpdatedC1, ?INTERNAL6_ARGS
    )
).
-define(INTERNAL6_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL6_C2(
        UpdatedC2, ?INTERNAL6_ARGS
    )
).
-define(INTERNAL6_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL6_C3(
        UpdatedC3, ?INTERNAL6_ARGS
    )
).
-define(INTERNAL6_C4_REBALANCE(UpdatedC4),
    rebalance_INTERNAL6_C4(
        UpdatedC4, ?INTERNAL6_ARGS
    )
).
-define(INTERNAL6_C5_REBALANCE(UpdatedC5),
    rebalance_INTERNAL6_C5(
        UpdatedC5, ?INTERNAL6_ARGS
    )
).
-define(INTERNAL6_C6_REBALANCE(UpdatedC6),
    rebalance_INTERNAL6_C6(
        UpdatedC6, ?INTERNAL6_ARGS
    )
).
-define(INTERNAL6_C7_REBALANCE(UpdatedC7),
    rebalance_INTERNAL6_C7(
        UpdatedC7, ?INTERNAL6_ARGS
    )
).

-define(INTERNAL6_ARGS_IGN_K1,
    _, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K2,
    K1, _, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K3,
    K1, K2, _, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K4,
    K1, K2, K3, _, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K5,
    K1, K2, K3, K4, _, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K6,
    K1, K2, K3, K4, K5, _, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7
).

-define(INTERNAL6_ARGS_IGN_K1_V1,
    _, K2, K3, K4, K5, K6, _, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K2_V2,
    K1, _, K3, K4, K5, K6, V1, _, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K3_V3,
    K1, K2, _, K4, K5, K6, V1, V2, _, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K4_V4,
    K1, K2, K3, _, K5, K6, V1, V2, V3, _, V5, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K5_V5,
    K1, K2, K3, K4, _, K6, V1, V2, V3, V4, _, V6, C1, C2, C3, C4, C5, C6, C7
).
-define(INTERNAL6_ARGS_IGN_K6_V6,
    K1, K2, K3, K4, K5, _, V1, V2, V3, V4, V5, _, C1, C2, C3, C4, C5, C6, C7
).

%% ?INTERNAL5

-define(INTERNAL5_ARGS, K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6).
-define(INTERNAL5_ARITY, 16).
-define(INTERNAL5_ARITY_PLUS1, 17).
-define(INTERNAL5_ARITY_PLUS2, 18).
-define(INTERNAL5_ARITY_PLUS3, 19).

-define(INTERNAL5_C1(UpdatedC1),
    ?new_INTERNAL5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, UpdatedC1, C2, C3, C4, C5, C6)
).
-define(INTERNAL5_C2(UpdatedC2),
    ?new_INTERNAL5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, UpdatedC2, C3, C4, C5, C6)
).
-define(INTERNAL5_C3(UpdatedC3),
    ?new_INTERNAL5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, UpdatedC3, C4, C5, C6)
).
-define(INTERNAL5_C4(UpdatedC4),
    ?new_INTERNAL5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, UpdatedC4, C5, C6)
).
-define(INTERNAL5_C5(UpdatedC5),
    ?new_INTERNAL5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, UpdatedC5, C6)
).
-define(INTERNAL5_C6(UpdatedC6),
    ?new_INTERNAL5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, UpdatedC6)
).

-define(INTERNAL5_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL5_C1(
        UpdatedC1, ?INTERNAL5_ARGS
    )
).
-define(INTERNAL5_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL5_C2(
        UpdatedC2, ?INTERNAL5_ARGS
    )
).
-define(INTERNAL5_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL5_C3(
        UpdatedC3, ?INTERNAL5_ARGS
    )
).
-define(INTERNAL5_C4_REBALANCE(UpdatedC4),
    rebalance_INTERNAL5_C4(
        UpdatedC4, ?INTERNAL5_ARGS
    )
).
-define(INTERNAL5_C5_REBALANCE(UpdatedC5),
    rebalance_INTERNAL5_C5(
        UpdatedC5, ?INTERNAL5_ARGS
    )
).
-define(INTERNAL5_C6_REBALANCE(UpdatedC6),
    rebalance_INTERNAL5_C6(
        UpdatedC6, ?INTERNAL5_ARGS
    )
).

-define(INTERNAL5_ARGS_IGN_K1, _, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6).
-define(INTERNAL5_ARGS_IGN_K2, K1, _, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6).
-define(INTERNAL5_ARGS_IGN_K3, K1, K2, _, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6).
-define(INTERNAL5_ARGS_IGN_K4, K1, K2, K3, _, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6).
-define(INTERNAL5_ARGS_IGN_K5, K1, K2, K3, K4, _, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6).

-define(INTERNAL5_ARGS_IGN_K1_V1, _, K2, K3, K4, K5, _, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6).
-define(INTERNAL5_ARGS_IGN_K2_V2, K1, _, K3, K4, K5, V1, _, V3, V4, V5, C1, C2, C3, C4, C5, C6).
-define(INTERNAL5_ARGS_IGN_K3_V3, K1, K2, _, K4, K5, V1, V2, _, V4, V5, C1, C2, C3, C4, C5, C6).
-define(INTERNAL5_ARGS_IGN_K4_V4, K1, K2, K3, _, K5, V1, V2, V3, _, V5, C1, C2, C3, C4, C5, C6).
-define(INTERNAL5_ARGS_IGN_K5_V5, K1, K2, K3, K4, _, V1, V2, V3, V4, _, C1, C2, C3, C4, C5, C6).

%% ?INTERNAL4

-define(INTERNAL4_ARGS, K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARITY, 13).
-define(INTERNAL4_ARITY_PLUS1, 14).
-define(INTERNAL4_ARITY_PLUS2, 15).
-define(INTERNAL4_ARITY_PLUS3, 16).

-define(INTERNAL4_C1(UpdatedC1),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, UpdatedC1, C2, C3, C4, C5)
).
-define(INTERNAL4_C2(UpdatedC2),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_C3(UpdatedC3),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_C4(UpdatedC4),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_C5(UpdatedC5),
    ?new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, UpdatedC5)
).

-define(INTERNAL4_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL4_C1(UpdatedC1, ?INTERNAL4_ARGS)
).
-define(INTERNAL4_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL4_C2(UpdatedC2, ?INTERNAL4_ARGS)
).
-define(INTERNAL4_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL4_C3(UpdatedC3, ?INTERNAL4_ARGS)
).
-define(INTERNAL4_C4_REBALANCE(UpdatedC4),
    rebalance_INTERNAL4_C4(UpdatedC4, ?INTERNAL4_ARGS)
).
-define(INTERNAL4_C5_REBALANCE(UpdatedC5),
    rebalance_INTERNAL4_C5(UpdatedC5, ?INTERNAL4_ARGS)
).

-define(INTERNAL4_ARGS_IGN_K1, _, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K2, K1, _, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K3, K1, K2, _, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K4, K1, K2, K3, _, V1, V2, V3, V4, C1, C2, C3, C4, C5).

-define(INTERNAL4_ARGS_IGN_K1_V1, _, K2, K3, K4, _, V2, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K2_V2, K1, _, K3, K4, V1, _, V3, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K3_V3, K1, K2, _, K4, V1, V2, _, V4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_K4_V4, K1, K2, K3, _, V1, V2, V3, _, C1, C2, C3, C4, C5).

%% ?INTERNAL3

-define(INTERNAL3_ARGS, K1, K2, K3, V1, V2, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARITY, 10).
-define(INTERNAL3_ARITY_PLUS1, 11).
-define(INTERNAL3_ARITY_PLUS2, 12).
-define(INTERNAL3_ARITY_PLUS3, 13).
-define(INTERNAL3_ARITY_PLUS5, 15).

-define(INTERNAL3_C1(UpdatedC1), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, UpdatedC1, C2, C3, C4)).
-define(INTERNAL3_C2(UpdatedC2), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, UpdatedC2, C3, C4)).
-define(INTERNAL3_C3(UpdatedC3), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, UpdatedC3, C4)).
-define(INTERNAL3_C4(UpdatedC4), ?new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, UpdatedC4)).

-define(INTERNAL3_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL3_C1(UpdatedC1, ?INTERNAL3_ARGS)
).
-define(INTERNAL3_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL3_C2(UpdatedC2, ?INTERNAL3_ARGS)
).
-define(INTERNAL3_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL3_C3(UpdatedC3, ?INTERNAL3_ARGS)
).
-define(INTERNAL3_C4_REBALANCE(UpdatedC4),
    rebalance_INTERNAL3_C4(UpdatedC4, ?INTERNAL3_ARGS)
).

-define(INTERNAL3_ARGS_IGN_K1, _, K2, K3, V1, V2, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_K2, K1, _, K3, V1, V2, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_K3, K1, K2, _, V1, V2, V3, C1, C2, C3, C4).

-define(INTERNAL3_ARGS_IGN_K1_V1, _, K2, K3, _, V2, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_K2_V2, K1, _, K3, V1, _, V3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_K3_V3, K1, K2, _, V1, V2, _, C1, C2, C3, C4).

%% ?INTERNAL2

-define(INTERNAL2_ARGS, K1, K2, V1, V2, C1, C2, C3).
-define(INTERNAL2_ARITY, 7).
-define(INTERNAL2_ARITY_PLUS1, 8).
-define(INTERNAL2_ARITY_PLUS3, 10).

-define(INTERNAL2_C1(UpdatedC1), ?new_INTERNAL2(K1, K2, V1, V2, UpdatedC1, C2, C3)).
-define(INTERNAL2_C2(UpdatedC2), ?new_INTERNAL2(K1, K2, V1, V2, C1, UpdatedC2, C3)).
-define(INTERNAL2_C3(UpdatedC3), ?new_INTERNAL2(K1, K2, V1, V2, C1, C2, UpdatedC3)).

-define(INTERNAL2_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL2_C1(UpdatedC1, ?INTERNAL2_ARGS)
).
-define(INTERNAL2_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL2_C2(UpdatedC2, ?INTERNAL2_ARGS)
).
-define(INTERNAL2_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL2_C3(UpdatedC3, ?INTERNAL2_ARGS)
).

-define(INTERNAL2_ARGS_IGN_K1, _, K2, V1, V2, C1, C2, C3).
-define(INTERNAL2_ARGS_IGN_K2, K1, _, V1, V2, C1, C2, C3).

-define(INTERNAL2_ARGS_IGN_K1_V1, _, K2, _, V2, C1, C2, C3).
-define(INTERNAL2_ARGS_IGN_K2_V2, K1, _, V1, _, C1, C2, C3).

%% ?INTERNAL1

-define(INTERNAL1_ARGS, K1, V1, C1, C2).
-define(INTERNAL1_ARITY, 4).
-define(INTERNAL1_ARITY_PLUS1, 5).
-define(INTERNAL1_ARITY_PLUS3, 7).

-define(INTERNAL1_C1(UpdatedC1), ?new_INTERNAL1(K1, V1, UpdatedC1, C2)).
-define(INTERNAL1_C2(UpdatedC2), ?new_INTERNAL1(K1, V1, C1, UpdatedC2)).

-define(INTERNAL1_C1_REBALANCE(UpdatedC1), rebalance_INTERNAL1_C1(UpdatedC1, ?INTERNAL1_ARGS)).
-define(INTERNAL1_C2_REBALANCE(UpdatedC2), rebalance_INTERNAL1_C2(UpdatedC2, ?INTERNAL1_ARGS)).

-define(INTERNAL1_ARGS_IGN_K1, _, V1, C1, C2).
-define(INTERNAL1_ARGS_IGN_K1_V1, _, _, C1, C2).

%% ?LEAF6

-define(LEAF6_ARGS, K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6).
-define(LEAF6_ARITY, 12).
-define(LEAF6_ARITY_PLUS1, 13).
-define(LEAF6_ARITY_PLUS2, 14).
-define(LEAF6_ARITY_PLUS3, 15).

%% ?LEAF5

-define(LEAF5_ARGS, K1, K2, K3, K4, K5, V1, V2, V3, V4, V5).
-define(LEAF5_ARITY, 10).
-define(LEAF5_ARITY_PLUS1, 11).
-define(LEAF5_ARITY_PLUS2, 12).
-define(LEAF5_ARITY_PLUS3, 13).

%% ?LEAF4

-define(LEAF4_ARGS, K1, K2, K3, K4, V1, V2, V3, V4).
-define(LEAF4_ARITY, 8).
-define(LEAF4_ARITY_PLUS1, 9).
-define(LEAF4_ARITY_PLUS2, 10).
-define(LEAF4_ARITY_PLUS3, 11).

%% ?LEAF3

-define(LEAF3_ARGS, K1, K2, K3, V1, V2, V3).
% -define(LEAF3_ARITY, 6).
-define(LEAF3_ARITY_PLUS1, 7).
-define(LEAF3_ARITY_PLUS2, 8).
-define(LEAF3_ARITY_PLUS3, 9).
-define(LEAF3_ARITY_PLUS4, 10).

%% ?LEAF2

-define(LEAF2_ARGS, K1, K2, V1, V2).
-define(LEAF2_ARITY, 4).
-define(LEAF2_ARITY_PLUS1, 5).
-define(LEAF2_ARITY_PLUS3, 7).

%% ?LEAF1

-define(LEAF1_ARGS, K1, V1).
-define(LEAF1_ARITY, 2).
-define(LEAF1_ARITY_PLUS1, 3).
-define(LEAF1_ARITY_PLUS3, 5).

%%

-define(TAKEN(Pair, UpdatedNode), [Pair | UpdatedNode]).

-define(TAKEN_PAIR(K, V, UpdatedNode), ?TAKEN([K | V], UpdatedNode)).

%%

% defined(TEST)).
-define(NODE_CHECK_ENABLED, false).

-if(?NODE_CHECK_ENABLED).
-define(CHECK_NODE(Node), check_node(?LINE, Node)).
-define(CHECK_NODE_RECUR(Node), check_node_recur(?LINE, Node)).
-else.
-define(CHECK_NODE(Node), Node).
-define(CHECK_NODE_RECUR(Node), Node).
-endif.

%

-define(new_INTERNAL6(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7),
    ?CHECK_NODE_RECUR(
        ?INTERNAL6(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7)
    )
).

-define(new_INTERNAL5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6),
    ?CHECK_NODE_RECUR(?INTERNAL5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6))
).

-define(new_INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5),
    ?CHECK_NODE_RECUR(?INTERNAL4(K1, K2, K3, K4, V1, V2, V3, V4, C1, C2, C3, C4, C5))
).

-define(new_INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4),
    ?CHECK_NODE_RECUR(?INTERNAL3(K1, K2, K3, V1, V2, V3, C1, C2, C3, C4))
).

-define(new_INTERNAL2(K1, K2, V1, V2, C1, C2, C3),
    ?CHECK_NODE(?INTERNAL2(K1, K2, V1, V2, C1, C2, C3))
).

-define(new_INTERNAL1(K1, V1, C1, C2), ?CHECK_NODE(?INTERNAL1(K1, V1, C1, C2))).

%

-define(new_LEAF6(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6),
    ?CHECK_NODE_RECUR(?LEAF6(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6))
).

-define(new_LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5),
    ?CHECK_NODE_RECUR(?LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5))
).

-define(new_LEAF4(K1, K2, K3, K4, V1, V2, V3, V4),
    ?CHECK_NODE_RECUR(?LEAF4(K1, K2, K3, K4, V1, V2, V3, V4))
).

-define(new_LEAF3(K1, K2, K3, V1, V2, V3), ?CHECK_NODE_RECUR(?LEAF3(K1, K2, K3, V1, V2, V3))).

-define(new_LEAF2(K1, K2, V1, V2), ?CHECK_NODE(?LEAF2(K1, K2, V1, V2))).

-define(new_LEAF1(K1, V1), ?CHECK_NODE(?LEAF1(K1, V1))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-opaque t(Key, Value) :: root_only_node(Key, Value) | deep_node(Key, Value).
%% This type represents any tree node structure.
-export_type([t/2]).

-type empty_node() :: ?LEAF0.

%
% The fact that some node types are root-only allows us to optimize the
% recursive case - less potential patterns to match.
%
% Therefore, most of the exported API functions here match on the root-only
% nodes in their entry clauses, but only on deep node types after that.
%

-type root_only_node(Key, Value) ::
    (node_INTERNAL1(Key, Value)
    | node_LEAF1(Key, Value)
    | empty_node()).

-type deep_node(Key, Value) ::
    (node_INTERNAL4(Key, Value)
    | node_INTERNAL3(Key, Value)
    | node_INTERNAL2(Key, Value)
    | node_LEAF4(Key, Value)
    | node_LEAF3(Key, Value)
    | node_LEAF2(Key, Value)).

%-type non_empty_node(Key, Value) ::
%    (node_INTERNAL1(Key, Value)
%    | node_LEAF1(Key, Value)
%    | deep_node(Key, Value)).

-type node_INTERNAL4(Key, Value) ::
    (?INTERNAL4(
        Key,
        Key,
        Key,
        Key,
        Value,
        Value,
        Value,
        Value,
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_INTERNAL3(Key, Value) ::
    (?INTERNAL3(
        Key,
        Key,
        Key,
        Value,
        Value,
        Value,
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_INTERNAL2(Key, Value) ::
    (?INTERNAL2(
        Key,
        Key,
        Value,
        Value,
        deep_node(Key, Value),
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_INTERNAL1(Key, Value) ::
    (?INTERNAL1(
        Key,
        Value,
        deep_node(Key, Value),
        deep_node(Key, Value)
    )).

-type node_LEAF4(Key, Value) ::
    (?LEAF4(
        Key,
        Key,
        Key,
        Key,
        Value,
        Value,
        Value,
        Value
    )).

-type node_LEAF3(Key, Value) ::
    (?LEAF3(
        Key,
        Key,
        Key,
        Value,
        Value,
        Value
    )).

-type node_LEAF2(Key, Value) :: ?LEAF2(Key, Key, Value, Value).

-type node_LEAF1(Key, Value) :: ?LEAF1(Key, Value).

%%%%%%%%%%%

% -type node_after_deletion(Key) ::
%     node_INTERNAL3(Key)
%     | node_INTERNAL2(Key)
%     | node_INTERNAL1(Key)
%     | node_LEAF2(Key)
%     | node_LEAF1(Key).
%
% -type deep_node_after_insertion(Key) ::
%     node_INTERNAL4(Key)
%     | node_INTERNAL3(Key)
%     | node_LEAF4(Key)
%     | node_LEAF3(Key).
%
% % Temporary situation before rebalance
% -type unbalanced_node(Key) :: node_INTERNAL1(Key).

%%%%%%%%%%%

% -type split_result(Key) :: split_internal_result(Key) | leaf_split_result(Key).
%
% -type split_internal_result(Key) :: split_result(
%     Key, node_INTERNAL2(Key), node_INTERNAL2(Key)
% ).
%
% -type leaf_split_result(Key) :: split_result(
%     Key, node_LEAF2(Key), node_LEAF2(Key)
% ).
%
% -type split_result(Key, SplitL, SplitR) :: ?SPLIT(Key, SplitL, SplitR).

%%%%%%%%%%%

% -opaque iter(Key) :: forward_iter(Key) | reverse_iter(Key).
% -export_type([iter/1]).
%
% -type forward_iter(Key) :: [iterator_step(Key)].
% -type reverse_iter(Key) :: nonempty_improper_list(reversed, [iterator_step(Key)]).

% -type iterator_step(Key) :: kv_pair(Key) | deep_node(Key).

%%%%%%%%%%%

%%%%%%%%%%%%

-record(stats_acc, {
    count_internal6,
    count_internal5,
    count_internal4,
    count_internal3,
    count_internal2,
    count_internal1,
    count_leaf6,
    count_leaf5,
    count_leaf4,
    count_leaf3,
    count_leaf2,
    count_leaf1,
    height
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

delete(Key, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            delete_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            delete_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            delete_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF1_MATCH(K1, _) ->
            delete_LEAF1(Key, K1);
        %
        ?LEAF0_MATCH_ALL ->
            error_badkey(Key);
        %
        _ ->
            UpdatedRoot = delete_recur(Key, Root),
            maybe_rebalance_root(UpdatedRoot, Root)
    end.

foldl(Fun, Acc, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
            Acc3 = Fun(K2, V2, foldl_recur(Fun, Acc2, C2)),
            foldl_recur(Fun, Acc3, C3);
        %
        ?INTERNAL1_MATCH_ALL ->
            Acc2 = Fun(K1, V1, foldl_recur(Fun, Acc, C1)),
            foldl_recur(Fun, Acc2, C2);
        %
        ?LEAF2_MATCH_ALL ->
            Fun(K2, V2, Fun(K1, V1, Acc));
        %
        ?LEAF1_MATCH_ALL ->
            Fun(K1, V1, Acc);
        %
        ?LEAF0_MATCH_ALL ->
            Acc;
        %
        _ ->
            foldl_recur(Fun, Acc, Root)
    end.

foldr(Fun, Acc, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            Acc2 = Fun(K2, V2, foldr_recur(Fun, Acc, C3)),
            Acc3 = Fun(K1, V1, foldr_recur(Fun, Acc2, C2)),
            foldr_recur(Fun, Acc3, C1);
        %
        ?INTERNAL1_MATCH_ALL ->
            Acc2 = Fun(K1, V1, foldr_recur(Fun, Acc, C2)),
            foldr_recur(Fun, Acc2, C1);
        %
        ?LEAF2_MATCH_ALL ->
            Fun(K1, V1, Fun(K2, V2, Acc));
        %
        ?LEAF1_MATCH_ALL ->
            Fun(K1, V1, Acc);
        %
        ?LEAF0_MATCH_ALL ->
            Acc;
        %
        _ ->
            foldr_recur(Fun, Acc, Root)
    end.

get(Key, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            get_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            get_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            get_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            get_LEAF1(Key, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH_ALL ->
            error_badkey(Key);
        %
        _ ->
            get_recur(Key, Root)
    end.

insert(Key, ValueEval, ValueWrap, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            insert_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            insert_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            insert_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            insert_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH_ALL ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?new_LEAF1(Key, Value);
        %
        _ ->
            case insert_recur(Key, ValueEval, ValueWrap, Root) of
                ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
                    ?new_INTERNAL1(SplitK, SplitV, SplitL, SplitR);
                %
                UpdatedRoot ->
                    UpdatedRoot
            end
    end.

iterator(Root, ordered) ->
    fwd_iterator(Root);
iterator(Root, reversed) ->
    Acc = rev_iterator(Root),
    [?REV_ITER_TAG | Acc].

iterator_from(Key, Root, ordered) ->
    bound_fwd_iterator(Key, Root);
iterator_from(Key, Root, reversed) ->
    Acc = bound_rev_iterator(Key, Root),
    [?REV_ITER_TAG | Acc].

keys(Root) ->
    case Root of
        ?INTERNAL2_MATCH(K1, K2, _, _, C1, C2, C3) ->
            Acc = [],
            Acc2 = [K2 | keys_recur(C3, Acc)],
            Acc3 = [K1 | keys_recur(C2, Acc2)],
            keys_recur(C1, Acc3);
        %
        ?INTERNAL1_MATCH(K1, _, C1, C2) ->
            keys_recur(C1, [K1 | keys_recur(C2, [])]);
        %
        ?LEAF2_MATCH(K1, K2, _, _) ->
            [K1, K2];
        %
        ?LEAF1_MATCH(K1, _) ->
            [K1];
        %
        ?LEAF0_MATCH ->
            [];
        %
        _ ->
            keys_recur(Root, [])
    end.

larger(Key, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            larger_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            larger_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            larger_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            larger_LEAF1(Key, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH_ALL ->
            none;
        %
        _ ->
            larger_recur(Key, Root)
    end.

largest(Root) ->
    case Root of
        ?INTERNAL2_MATCH(_, _, _, _, _, _, C3) ->
            largest_recur(C3);
        %
        ?INTERNAL1_MATCH(_, _, _, C2) ->
            largest_recur(C2);
        %
        ?LEAF2_MATCH(_, K2, _, V2) ->
            {K2, V2};
        %
        ?LEAF1_MATCH(K1, V1) ->
            {K1, V1};
        %
        ?LEAF0_MATCH_ALL ->
            error_empty_tree();
        %
        _ ->
            largest_recur(Root)
    end.

map(Fun, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            ?new_INTERNAL2(
                K1,
                K2,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                %
                map_recur(Fun, C1),
                map_recur(Fun, C2),
                map_recur(Fun, C3)
            );
        %
        %
        ?INTERNAL1_MATCH_ALL ->
            ?new_INTERNAL1(
                K1,
                %
                Fun(K1, V1),
                %
                map_recur(Fun, C1),
                map_recur(Fun, C2)
            );
        %
        %
        ?LEAF2_MATCH_ALL ->
            ?new_LEAF2(
                K1,
                K2,
                %
                Fun(K1, V1),
                Fun(K2, V2)
            );
        %
        %
        ?LEAF1_MATCH_ALL ->
            ?new_LEAF1(K1, Fun(K1, V1));
        %
        %
        ?LEAF0_MATCH ->
            ?LEAF0;
        %
        %
        _ ->
            map_recur(Fun, Root)
    end.

new() ->
    ?LEAF0.

next([Head | Tail]) ->
    next(Head, Tail);
next([]) ->
    none.

smaller(Key, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            smaller_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            smaller_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            smaller_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            smaller_LEAF1(Key, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH_ALL ->
            none;
        %
        _ ->
            smaller_recur(Key, Root)
    end.

smallest(Root) ->
    case Root of
        ?INTERNAL2_MATCH(_, _, _, _, C1, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL1_MATCH(_, _, C1, _) ->
            smallest_recur(C1);
        %
        ?LEAF2_MATCH(K1, _, V1, _) ->
            {K1, V1};
        %
        ?LEAF1_MATCH(K1, V1) ->
            {K1, V1};
        %
        ?LEAF0_MATCH_ALL ->
            error_empty_tree();
        %
        _ ->
            smallest_recur(Root)
    end.

structural_stats(Root) ->
    Acc = #stats_acc{
        count_internal6 = 0,
        count_internal5 = 0,
        count_internal4 = 0,
        count_internal3 = 0,
        count_internal2 = 0,
        count_internal1 = 0,
        count_leaf6 = 0,
        count_leaf5 = 0,
        count_leaf4 = 0,
        count_leaf3 = 0,
        count_leaf2 = 0,
        count_leaf1 = 0,
        height = 0
    },

    case Root of
        ?INTERNAL1_MATCH(_, _, C1, C2) ->
            Height = 1,
            Acc2 = structural_stats_inc(#stats_acc.count_internal1, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            return_structural_stats(Acc4);
        %
        ?INTERNAL2_MATCH(_, _, _, _, C1, C2, C3) ->
            Height = 1,
            Acc2 = structural_stats_inc(#stats_acc.count_internal2, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            return_structural_stats(Acc5);
        %
        ?LEAF1_MATCH(_, _) ->
            Height = 1,
            Acc2 = structural_stats_inc(#stats_acc.count_leaf1, Acc),
            Acc3 = structural_stats_set_height(Height, Acc2),
            return_structural_stats(Acc3);
        %
        ?LEAF2_MATCH(_, _, _, _) ->
            Height = 1,
            Acc2 = structural_stats_inc(#stats_acc.count_leaf2, Acc),
            Acc3 = structural_stats_set_height(Height, Acc2),
            return_structural_stats(Acc3);
        %
        ?LEAF0_MATCH ->
            return_structural_stats(Acc);
        %
        _ ->
            Height = 1,
            Acc2 = structural_stats_recur(Root, Acc, Height),
            return_structural_stats(Acc2)
    end.

take(Key, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            take_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            take_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            take_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            take_LEAF1(Key, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH_ALL ->
            error_badkey(Key);
        %
        _ ->
            Taken = take_recur(Key, Root),
            maybe_rebalance_root_on_take(Taken, Root)
    end.

take_largest(Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            take_largest_INTERNAL2(?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            take_largest_INTERNAL1(?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            take_largest_LEAF2(?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            take_largest_LEAF1(?LEAF1_ARGS);
        %
        ?LEAF0_MATCH_ALL ->
            error_empty_tree();
        %
        _ ->
            Taken = take_largest_recur(Root),
            maybe_rebalance_root_on_take(Taken, Root)
    end.

take_smallest(Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            take_smallest_INTERNAL2(?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            take_smallest_INTERNAL1(?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            take_smallest_LEAF2(?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            take_smallest_LEAF1(?LEAF1_ARGS);
        %
        ?LEAF0_MATCH_ALL ->
            error_empty_tree();
        %
        _ ->
            Taken = take_smallest_recur(Root),
            maybe_rebalance_root_on_take(Taken, Root)
    end.

to_list(Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            Acc2 = to_list_recur(C3, []),
            Acc3 = to_list_recur(C2, [{K2, V2} | Acc2]),
            _Acc4 = to_list_recur(C1, [{K1, V1} | Acc3]);
        %
        ?INTERNAL1_MATCH_ALL ->
            Acc2 = to_list_recur(C2, []),
            Acc3 = [{K1, V1} | Acc2],
            to_list_recur(C1, Acc3);
        %
        ?LEAF2_MATCH_ALL ->
            [{K1, V1}, {K2, V2}];
        %
        ?LEAF1_MATCH_ALL ->
            [{K1, V1}];
        %
        ?LEAF0_MATCH_ALL ->
            [];
        %
        _ ->
            to_list_recur(Root, [])
    end.

update(Key, ValueEval, ValueWrap, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            update_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            update_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            update_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            update_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH_ALL ->
            error_badkey(Key);
        %
        _ ->
            update_recur(Key, ValueEval, ValueWrap, Root)
    end.

values(Root) ->
    case Root of
        ?INTERNAL2_MATCH(_, _, V1, V2, C1, C2, C3) ->
            Acc2 = [V2 | values_recur(C3, [])],
            Acc3 = [V1 | values_recur(C2, Acc2)],
            values_recur(C1, Acc3);
        %
        ?INTERNAL1_MATCH(_, V1, C1, C2) ->
            values_recur(C1, [V1 | values_recur(C2, [])]);
        %
        ?LEAF2_MATCH(_, _, V1, V2) ->
            [V1, V2];
        %
        ?LEAF1_MATCH(_, V1) ->
            [V1];
        %
        ?LEAF0_MATCH ->
            [];
        %
        _ ->
            values_recur(Root, [])
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Exceptions
%% ------------------------------------------------------------------

-compile({inline, error_badkey/1}).
error_badkey(Key) ->
    error({badkey, Key}).

-compile({inline, error_empty_tree/0}).
error_empty_tree() ->
    error(empty_tree).

-compile({inline, error_key_exists/1}).
error_key_exists(Key) ->
    error({key_exists, Key}).

%% ------------------------------------------------------------------
%% Internal Function Definitions: delete/2
%% ------------------------------------------------------------------

delete_recur(Key, Node) ->
    case Node of
        ?INTERNAL3_MATCH_ALL ->
            delete_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            delete_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?INTERNAL5_MATCH_ALL ->
            delete_INTERNAL5(Key, ?INTERNAL5_ARGS);
        %
        ?INTERNAL6_MATCH_ALL ->
            delete_INTERNAL6(Key, ?INTERNAL6_ARGS);
        %
        ?LEAF3_MATCH(K1, K2, K3, _, _, _) ->
            delete_LEAF3(Key, K1, K2, K3);
        %
        ?LEAF4_MATCH_ALL ->
            delete_LEAF4(Key, ?LEAF4_ARGS);
        %
        ?LEAF5_MATCH_ALL ->
            delete_LEAF5(Key, ?LEAF5_ARGS);
        %
        ?LEAF6_MATCH_ALL ->
            delete_LEAF6(Key, ?LEAF6_ARGS)
    end.

%%
%% ?INTERNAL6
%%

-compile({inline, delete_INTERNAL6 / ?INTERNAL6_ARITY_PLUS1}).
delete_INTERNAL6(Key, ?INTERNAL6_ARGS) ->
    if
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            delete_INTERNAL6_C5(Key, ?INTERNAL6_ARGS);
                        %
                        Key < K4 ->
                            delete_INTERNAL6_C4(Key, ?INTERNAL6_ARGS);
                        %
                        true ->
                            delete_INTERNAL6_K4(?INTERNAL6_ARGS)
                    end;
                %
                Key > K5 ->
                    %
                    if
                        Key < K6 ->
                            delete_INTERNAL6_C6(Key, ?INTERNAL6_ARGS);
                        %
                        Key > K6 ->
                            delete_INTERNAL6_C7(Key, ?INTERNAL6_ARGS);
                        %
                        true ->
                            delete_INTERNAL6_K6(?INTERNAL6_ARGS)
                    end;
                %
                true ->
                    delete_INTERNAL6_K5(?INTERNAL6_ARGS)
            end;
        %
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            delete_INTERNAL6_C2(Key, ?INTERNAL6_ARGS);
                        %
                        Key > K2 ->
                            delete_INTERNAL6_C3(Key, ?INTERNAL6_ARGS);
                        %
                        true ->
                            delete_INTERNAL6_K2(?INTERNAL6_ARGS)
                    end;
                %
                Key < K1 ->
                    delete_INTERNAL6_C1(Key, ?INTERNAL6_ARGS);
                %
                true ->
                    delete_INTERNAL6_K1(?INTERNAL6_ARGS)
            end;
        %
        true ->
            delete_INTERNAL6_K3(?INTERNAL6_ARGS)
    end.

-compile({inline, delete_INTERNAL6_C1 / ?INTERNAL6_ARITY_PLUS1}).
delete_INTERNAL6_C1(Key, ?INTERNAL6_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),

    ?INTERNAL6_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL6_C2 / ?INTERNAL6_ARITY_PLUS1}).
delete_INTERNAL6_C2(Key, ?INTERNAL6_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),

    ?INTERNAL6_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL6_C3 / ?INTERNAL6_ARITY_PLUS1}).
delete_INTERNAL6_C3(Key, ?INTERNAL6_ARGS) ->
    UpdatedC3 = delete_recur(Key, C3),

    ?INTERNAL6_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL6_C4 / ?INTERNAL6_ARITY_PLUS1}).
delete_INTERNAL6_C4(Key, ?INTERNAL6_ARGS) ->
    UpdatedC4 = delete_recur(Key, C4),

    ?INTERNAL6_C4_REBALANCE(UpdatedC4).

-compile({inline, delete_INTERNAL6_C5 / ?INTERNAL6_ARITY_PLUS1}).
delete_INTERNAL6_C5(Key, ?INTERNAL6_ARGS) ->
    UpdatedC5 = delete_recur(Key, C5),

    ?INTERNAL6_C5_REBALANCE(UpdatedC5).

-compile({inline, delete_INTERNAL6_C6 / ?INTERNAL6_ARITY_PLUS1}).
delete_INTERNAL6_C6(Key, ?INTERNAL6_ARGS) ->
    UpdatedC6 = delete_recur(Key, C6),

    ?INTERNAL6_C6_REBALANCE(UpdatedC6).

-compile({inline, delete_INTERNAL6_C7 / ?INTERNAL6_ARITY_PLUS1}).
delete_INTERNAL6_C7(Key, ?INTERNAL6_ARGS) ->
    UpdatedC7 = delete_recur(Key, C7),

    ?INTERNAL6_C7_REBALANCE(UpdatedC7).

%%

-compile({inline, delete_INTERNAL6_K1 / ?INTERNAL6_ARITY}).
delete_INTERNAL6_K1(?INTERNAL6_ARGS_IGN_K1_V1) ->
    % ?INTERNAL6_VALUES(_, V2, V3, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    rebalance_INTERNAL6_C2(
        UpdatedC2,
        %
        ReplacementK,
        K2,
        K3,
        K4,
        K5,
        K6,
        %
        ReplacementV,
        V2,
        V3,
        V4,
        V5,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, delete_INTERNAL6_K2 / ?INTERNAL6_ARITY}).
delete_INTERNAL6_K2(?INTERNAL6_ARGS_IGN_K2_V2) ->
    % ?INTERNAL6_VALUES(V1, _, V3, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_largest_recur(C2),

    rebalance_INTERNAL6_C2(
        UpdatedC2,
        %
        K1,
        ReplacementK,
        K3,
        K4,
        K5,
        K6,
        %
        V1,
        ReplacementV,
        V3,
        V4,
        V5,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, delete_INTERNAL6_K3 / ?INTERNAL6_ARITY}).
delete_INTERNAL6_K3(?INTERNAL6_ARGS_IGN_K3_V3) ->
    % ?INTERNAL6_VALUES(V1, V2, _, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_smallest_recur(C4),

    rebalance_INTERNAL6_C4(
        UpdatedC4,
        %
        K1,
        K2,
        ReplacementK,
        K4,
        K5,
        K6,
        %
        V1,
        V2,
        ReplacementV,
        V4,
        V5,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, delete_INTERNAL6_K4 / ?INTERNAL6_ARITY}).
delete_INTERNAL6_K4(?INTERNAL6_ARGS_IGN_K4_V4) ->
    % ?INTERNAL6_VALUES(V1, V2, V3, _) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_largest_recur(C4),

    rebalance_INTERNAL6_C4(
        UpdatedC4,
        %
        K1,
        K2,
        K3,
        ReplacementK,
        K5,
        K6,
        %
        V1,
        V2,
        V3,
        ReplacementV,
        V5,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, delete_INTERNAL6_K5 / ?INTERNAL6_ARITY}).
delete_INTERNAL6_K5(?INTERNAL6_ARGS_IGN_K5_V5) ->
    % ?INTERNAL6_VALUES(V1, V2, V3, _) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC5) = take_largest_recur(C5),

    rebalance_INTERNAL6_C5(
        UpdatedC5,
        %
        K1,
        K2,
        K3,
        K4,
        ReplacementK,
        K6,
        %
        V1,
        V2,
        V3,
        V4,
        ReplacementV,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, delete_INTERNAL6_K6 / ?INTERNAL6_ARITY}).
delete_INTERNAL6_K6(?INTERNAL6_ARGS_IGN_K6_V6) ->
    % ?INTERNAL6_VALUES(V1, V2, V3, _) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC6) = take_largest_recur(C6),

    rebalance_INTERNAL6_C6(
        UpdatedC6,
        %
        K1,
        K2,
        K3,
        K4,
        K5,
        ReplacementK,
        %
        V1,
        V2,
        V3,
        V4,
        V5,
        ReplacementV,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

%%
%% ?INTERNAL5
%%

-compile({inline, delete_INTERNAL5 / ?INTERNAL5_ARITY_PLUS1}).
delete_INTERNAL5(Key, ?INTERNAL5_ARGS) ->
    if
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            delete_INTERNAL5_C2(Key, ?INTERNAL5_ARGS);
                        %
                        Key > K2 ->
                            delete_INTERNAL5_C3(Key, ?INTERNAL5_ARGS);
                        %
                        true ->
                            delete_INTERNAL5_K2(?INTERNAL5_ARGS)
                    end;
                %
                Key < K1 ->
                    delete_INTERNAL5_C1(Key, ?INTERNAL5_ARGS);
                %
                true ->
                    delete_INTERNAL5_K1(?INTERNAL5_ARGS)
            end;
        %
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            delete_INTERNAL5_C5(Key, ?INTERNAL5_ARGS);
                        %
                        Key < K4 ->
                            delete_INTERNAL5_C4(Key, ?INTERNAL5_ARGS);
                        %
                        true ->
                            delete_INTERNAL5_K4(?INTERNAL5_ARGS)
                    end;
                %
                Key > K5 ->
                    delete_INTERNAL5_C6(Key, ?INTERNAL5_ARGS);
                %
                true ->
                    delete_INTERNAL5_K5(?INTERNAL5_ARGS)
            end;
        %
        true ->
            delete_INTERNAL5_K3(?INTERNAL5_ARGS)
    end.

-compile({inline, delete_INTERNAL5_C1 / ?INTERNAL5_ARITY_PLUS1}).
delete_INTERNAL5_C1(Key, ?INTERNAL5_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),

    ?INTERNAL5_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL5_C2 / ?INTERNAL5_ARITY_PLUS1}).
delete_INTERNAL5_C2(Key, ?INTERNAL5_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),

    ?INTERNAL5_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL5_C3 / ?INTERNAL5_ARITY_PLUS1}).
delete_INTERNAL5_C3(Key, ?INTERNAL5_ARGS) ->
    UpdatedC3 = delete_recur(Key, C3),

    ?INTERNAL5_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL5_C4 / ?INTERNAL5_ARITY_PLUS1}).
delete_INTERNAL5_C4(Key, ?INTERNAL5_ARGS) ->
    UpdatedC4 = delete_recur(Key, C4),

    ?INTERNAL5_C4_REBALANCE(UpdatedC4).

-compile({inline, delete_INTERNAL5_C5 / ?INTERNAL5_ARITY_PLUS1}).
delete_INTERNAL5_C5(Key, ?INTERNAL5_ARGS) ->
    UpdatedC5 = delete_recur(Key, C5),

    ?INTERNAL5_C5_REBALANCE(UpdatedC5).

-compile({inline, delete_INTERNAL5_C6 / ?INTERNAL5_ARITY_PLUS1}).
delete_INTERNAL5_C6(Key, ?INTERNAL5_ARGS) ->
    UpdatedC6 = delete_recur(Key, C6),

    ?INTERNAL5_C6_REBALANCE(UpdatedC6).

%%

-compile({inline, delete_INTERNAL5_K1 / ?INTERNAL5_ARITY}).
delete_INTERNAL5_K1(?INTERNAL5_ARGS_IGN_K1_V1) ->
    % ?INTERNAL5_VALUES(_, V2, V3, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    rebalance_INTERNAL5_C2(
        UpdatedC2,
        %
        ReplacementK,
        K2,
        K3,
        K4,
        K5,
        %
        ReplacementV,
        V2,
        V3,
        V4,
        V5,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

-compile({inline, delete_INTERNAL5_K2 / ?INTERNAL5_ARITY}).
delete_INTERNAL5_K2(?INTERNAL5_ARGS_IGN_K2_V2) ->
    % ?INTERNAL5_VALUES(V1, _, V3, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_largest_recur(C2),

    rebalance_INTERNAL5_C2(
        UpdatedC2,
        %
        K1,
        ReplacementK,
        K3,
        K4,
        K5,
        %
        V1,
        ReplacementV,
        V3,
        V4,
        V5,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

-compile({inline, delete_INTERNAL5_K3 / ?INTERNAL5_ARITY}).
delete_INTERNAL5_K3(?INTERNAL5_ARGS_IGN_K3_V3) ->
    % ?INTERNAL5_VALUES(V1, V2, _, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_smallest_recur(C4),

    rebalance_INTERNAL5_C4(
        UpdatedC4,
        %
        K1,
        K2,
        ReplacementK,
        K4,
        K5,
        %
        V1,
        V2,
        ReplacementV,
        V4,
        V5,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

-compile({inline, delete_INTERNAL5_K4 / ?INTERNAL5_ARITY}).
delete_INTERNAL5_K4(?INTERNAL5_ARGS_IGN_K4_V4) ->
    % ?INTERNAL5_VALUES(V1, V2, V3, _) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_largest_recur(C4),

    rebalance_INTERNAL5_C4(
        UpdatedC4,
        %
        K1,
        K2,
        K3,
        ReplacementK,
        K5,
        %
        V1,
        V2,
        V3,
        ReplacementV,
        V5,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

-compile({inline, delete_INTERNAL5_K5 / ?INTERNAL5_ARITY}).
delete_INTERNAL5_K5(?INTERNAL5_ARGS_IGN_K5_V5) ->
    % ?INTERNAL5_VALUES(V1, V2, V3, _) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC5) = take_largest_recur(C5),

    rebalance_INTERNAL5_C5(
        UpdatedC5,
        %
        K1,
        K2,
        K3,
        K4,
        ReplacementK,
        %
        V1,
        V2,
        V3,
        V4,
        ReplacementV,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

%%
%% ?INTERNAL4
%%

-compile({inline, delete_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            delete_INTERNAL4_C4(Key, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            delete_INTERNAL4_C3(Key, ?INTERNAL4_ARGS);
                        %
                        true ->
                            delete_INTERNAL4_K3(?INTERNAL4_ARGS)
                    end;
                %
                Key > K4 ->
                    delete_INTERNAL4_C5(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_INTERNAL4_K4(?INTERNAL4_ARGS)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    delete_INTERNAL4_C1(Key, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    delete_INTERNAL4_C2(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_INTERNAL4_K1(?INTERNAL4_ARGS)
            end;
        %
        true ->
            delete_INTERNAL4_K2(?INTERNAL4_ARGS)
    end.

-compile({inline, delete_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C1(Key, ?INTERNAL4_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),

    ?INTERNAL4_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C2(Key, ?INTERNAL4_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),

    ?INTERNAL4_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C3(Key, ?INTERNAL4_ARGS) ->
    UpdatedC3 = delete_recur(Key, C3),

    ?INTERNAL4_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C4(Key, ?INTERNAL4_ARGS) ->
    UpdatedC4 = delete_recur(Key, C4),

    ?INTERNAL4_C4_REBALANCE(UpdatedC4).

-compile({inline, delete_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C5(Key, ?INTERNAL4_ARGS) ->
    UpdatedC5 = delete_recur(Key, C5),

    ?INTERNAL4_C5_REBALANCE(UpdatedC5).

%%

-compile({inline, delete_INTERNAL4_K1 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_K1(?INTERNAL4_ARGS_IGN_K1_V1) ->
    % ?INTERNAL4_VALUES(_, V2, V3, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    rebalance_INTERNAL4_C2(
        UpdatedC2,
        %
        ReplacementK,
        K2,
        K3,
        K4,
        %
        ReplacementV,
        V2,
        V3,
        V4,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, delete_INTERNAL4_K2 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_K2(?INTERNAL4_ARGS_IGN_K2_V2) ->
    % ?INTERNAL4_VALUES(V1, _, V3, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_largest_recur(C2),

    rebalance_INTERNAL4_C2(
        UpdatedC2,
        %
        K1,
        ReplacementK,
        K3,
        K4,
        %
        V1,
        ReplacementV,
        V3,
        V4,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, delete_INTERNAL4_K3 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_K3(?INTERNAL4_ARGS_IGN_K3_V3) ->
    % ?INTERNAL4_VALUES(V1, V2, _, V4) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_smallest_recur(C4),

    rebalance_INTERNAL4_C4(
        UpdatedC4,
        %
        K1,
        K2,
        ReplacementK,
        K4,
        %
        V1,
        V2,
        ReplacementV,
        V4,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, delete_INTERNAL4_K4 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_K4(?INTERNAL4_ARGS_IGN_K4_V4) ->
    % ?INTERNAL4_VALUES(V1, V2, V3, _) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_largest_recur(C4),

    rebalance_INTERNAL4_C4(
        UpdatedC4,
        %
        K1,
        K2,
        K3,
        ReplacementK,
        %
        V1,
        V2,
        V3,
        ReplacementV,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, delete_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    delete_INTERNAL3_C1(Key, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    delete_INTERNAL3_C2(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_INTERNAL3_K1(?INTERNAL3_ARGS)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    delete_INTERNAL3_C3(Key, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    delete_INTERNAL3_C4(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_INTERNAL3_K3(?INTERNAL3_ARGS)
            end;
        %
        true ->
            delete_INTERNAL3_K2(?INTERNAL3_ARGS)
    end.

-compile({inline, delete_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C1(Key, ?INTERNAL3_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),

    ?INTERNAL3_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C2(Key, ?INTERNAL3_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),

    ?INTERNAL3_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C3(Key, ?INTERNAL3_ARGS) ->
    UpdatedC3 = delete_recur(Key, C3),

    ?INTERNAL3_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C4(Key, ?INTERNAL3_ARGS) ->
    UpdatedC4 = delete_recur(Key, C4),

    ?INTERNAL3_C4_REBALANCE(UpdatedC4).

%%

-compile({inline, delete_INTERNAL3_K1 / ?INTERNAL3_ARITY}).
delete_INTERNAL3_K1(?INTERNAL3_ARGS_IGN_K1_V1) ->
    % ?INTERNAL3_VALUES(_, V2, V3) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    rebalance_INTERNAL3_C2(
        UpdatedC2,
        %
        ReplacementK,
        K2,
        K3,
        %
        ReplacementV,
        V2,
        V3,
        %
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, delete_INTERNAL3_K2 / ?INTERNAL3_ARITY}).
delete_INTERNAL3_K2(?INTERNAL3_ARGS_IGN_K2_V2) ->
    % ?INTERNAL3_VALUES(V1, _, V3) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC3) = take_smallest_recur(C3),

    rebalance_INTERNAL3_C3(
        UpdatedC3,
        %
        K1,
        ReplacementK,
        K3,
        %
        V1,
        ReplacementV,
        V3,
        %
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, delete_INTERNAL3_K3 / ?INTERNAL3_ARITY}).
delete_INTERNAL3_K3(?INTERNAL3_ARGS_IGN_K3_V3) ->
    % ?INTERNAL3_VALUES(V1, V2, _) = Values,

    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC4) = take_smallest_recur(C4),

    rebalance_INTERNAL3_C4(
        UpdatedC4,
        %
        K1,
        K2,
        ReplacementK,
        %
        V1,
        V2,
        ReplacementV,
        %
        C1,
        C2,
        C3,
        C4
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, delete_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    delete_INTERNAL2_C2(Key, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    delete_INTERNAL2_C3(Key, ?INTERNAL2_ARGS);
                %
                true ->
                    delete_INTERNAL2_K2(?INTERNAL2_ARGS)
            end;
        %
        Key < K1 ->
            delete_INTERNAL2_C1(Key, ?INTERNAL2_ARGS);
        %
        true ->
            delete_INTERNAL2_K1(?INTERNAL2_ARGS)
    end.

-compile({inline, delete_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2_C1(Key, ?INTERNAL2_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),

    ?INTERNAL2_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2_C2(Key, ?INTERNAL2_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),

    ?INTERNAL2_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2_C3(Key, ?INTERNAL2_ARGS) ->
    UpdatedC3 = delete_recur(Key, C3),

    ?INTERNAL2_C3_REBALANCE(UpdatedC3).

%%

-compile({inline, delete_INTERNAL2_K1 / ?INTERNAL2_ARITY}).
delete_INTERNAL2_K1(?INTERNAL2_ARGS_IGN_K1_V1) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    rebalance_INTERNAL2_C2(
        UpdatedC2,
        %
        ReplacementK,
        K2,
        %
        ReplacementV,
        V2,
        %
        C1,
        C2,
        C3
    ).

-compile({inline, delete_INTERNAL2_K2 / ?INTERNAL2_ARITY}).
delete_INTERNAL2_K2(?INTERNAL2_ARGS_IGN_K2_V2) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_largest_recur(C2),

    rebalance_INTERNAL2_C2(
        UpdatedC2,
        %
        K1,
        ReplacementK,
        %
        V1,
        ReplacementV,
        %
        C1,
        C2,
        C3
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, delete_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
delete_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            delete_INTERNAL1_C1(Key, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            delete_INTERNAL1_C2(Key, ?INTERNAL1_ARGS);
        %
        true ->
            delete_INTERNAL1_K1(?INTERNAL1_ARGS)
    end.

-compile({inline, delete_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
delete_INTERNAL1_C1(Key, ?INTERNAL1_ARGS) ->
    UpdatedC1 = delete_recur(Key, C1),
    ?INTERNAL1_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
delete_INTERNAL1_C2(Key, ?INTERNAL1_ARGS) ->
    UpdatedC2 = delete_recur(Key, C2),
    ?INTERNAL1_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL1_K1 / ?INTERNAL1_ARITY}).
delete_INTERNAL1_K1(?INTERNAL1_ARGS_IGN_K1_V1) ->
    ?TAKEN_PAIR(ReplacementK, ReplacementV, UpdatedC2) = take_smallest_recur(C2),

    rebalance_INTERNAL1_C2(
        UpdatedC2,
        %
        ReplacementK,
        %
        ReplacementV,
        %
        C1,
        C2
    ).

%%
%% ?LEAF6
%%

-compile({inline, delete_LEAF6 / ?LEAF6_ARITY_PLUS1}).
delete_LEAF6(Key, ?LEAF6_ARGS) ->
    if
        Key == K1 ->
            ?new_LEAF5(K2, K3, K4, K5, K6, V2, V3, V4, V5, V6);
        %
        Key == K2 ->
            ?new_LEAF5(K1, K3, K4, K5, K6, V1, V3, V4, V5, V6);
        %
        Key == K3 ->
            ?new_LEAF5(K1, K2, K4, K5, K6, V1, V2, V4, V5, V6);
        %
        Key == K4 ->
            ?new_LEAF5(K1, K2, K3, K5, K6, V1, V2, V3, V5, V6);
        %
        Key == K5 ->
            ?new_LEAF5(K1, K2, K3, K4, K6, V1, V2, V3, V4, V6);
        %
        Key == K6 ->
            ?new_LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5);
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF5
%%

-compile({inline, delete_LEAF5 / ?LEAF5_ARITY_PLUS1}).
delete_LEAF5(Key, ?LEAF5_ARGS) ->
    if
        Key == K1 ->
            ?new_LEAF4(K2, K3, K4, K5, V2, V3, V4, V5);
        %
        Key == K2 ->
            ?new_LEAF4(K1, K3, K4, K5, V1, V3, V4, V5);
        %
        Key == K3 ->
            ?new_LEAF4(K1, K2, K4, K5, V1, V2, V4, V5);
        %
        Key == K4 ->
            ?new_LEAF4(K1, K2, K3, K5, V1, V2, V3, V5);
        %
        Key == K5 ->
            ?new_LEAF4(K1, K2, K3, K4, V1, V2, V3, V4);
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF4
%%

-compile({inline, delete_LEAF4 / ?LEAF4_ARITY_PLUS1}).
delete_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key == K1 ->
            ?new_LEAF3(K2, K3, K4, V2, V3, V4);
        %
        Key == K2 ->
            ?new_LEAF3(K1, K3, K4, V1, V3, V4);
        %
        Key == K3 ->
            ?new_LEAF3(K1, K2, K4, V1, V2, V4);
        %
        Key == K4 ->
            ?new_LEAF3(K1, K2, K3, V1, V2, V3);
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF3
%%

-compile({inline, delete_LEAF3/4}).
delete_LEAF3(Key, K1, K2, K3) ->
    if
        Key == K1 ->
            ?NEEDS_REBALANCE(1, nil);
        %
        Key == K2 ->
            ?NEEDS_REBALANCE(2, nil);
        %
        Key == K3 ->
            ?NEEDS_REBALANCE(3, nil);
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF2
%%

-compile({inline, delete_LEAF2 / ?LEAF2_ARITY_PLUS1}).
delete_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key == K1 ->
            ?new_LEAF1(K2, V2);
        %
        Key == K2 ->
            ?new_LEAF1(K1, V1);
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF1
%%

-compile({inline, delete_LEAF1/2}).
delete_LEAF1(Key, K1) ->
    if
        Key == K1 ->
            ?LEAF0;
        %
        true ->
            error_badkey(Key)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: foldl/3
%% ------------------------------------------------------------------

foldl_recur(Fun, Acc, Node) ->
    case Node of
        ?LEAF3_MATCH_ALL ->
            Fun(K3, V3, Fun(K2, V2, Fun(K1, V1, Acc)));
        %
        ?LEAF4_MATCH_ALL ->
            Fun(K4, V4, Fun(K3, V3, Fun(K2, V2, Fun(K1, V1, Acc))));
        %
        ?LEAF5_MATCH_ALL ->
            Fun(K5, V5, Fun(K4, V4, Fun(K3, V3, Fun(K2, V2, Fun(K1, V1, Acc)))));
        %
        ?LEAF6_MATCH_ALL ->
            Fun(K6, V6, Fun(K5, V5, Fun(K4, V4, Fun(K3, V3, Fun(K2, V2, Fun(K1, V1, Acc))))));
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3),
            _Acc5 = foldl_recur(Fun, Fun(K3, V3, Acc4), C4);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3),
            Acc5 = foldl_recur(Fun, Fun(K3, V3, Acc4), C4),
            _Acc6 = foldl_recur(Fun, Fun(K4, V4, Acc5), C5);
        %
        ?INTERNAL5_MATCH_ALL ->
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3),
            Acc5 = foldl_recur(Fun, Fun(K3, V3, Acc4), C4),
            Acc6 = foldl_recur(Fun, Fun(K4, V4, Acc5), C5),
            _Acc7 = foldl_recur(Fun, Fun(K5, V5, Acc6), C6);
        %
        ?INTERNAL6_MATCH_ALL ->
            Acc2 = foldl_recur(Fun, Acc, C1),
            Acc3 = foldl_recur(Fun, Fun(K1, V1, Acc2), C2),
            Acc4 = foldl_recur(Fun, Fun(K2, V2, Acc3), C3),
            Acc5 = foldl_recur(Fun, Fun(K3, V3, Acc4), C4),
            Acc6 = foldl_recur(Fun, Fun(K4, V4, Acc5), C5),
            Acc7 = foldl_recur(Fun, Fun(K5, V5, Acc6), C6),
            _Acc8 = foldl_recur(Fun, Fun(K6, V6, Acc7), C7)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: foldr/3
%% ------------------------------------------------------------------

foldr_recur(Fun, Acc, Node) ->
    case Node of
        ?LEAF3_MATCH_ALL ->
            Fun(K1, V1, Fun(K2, V2, Fun(K3, V3, Acc)));
        %
        ?LEAF4_MATCH_ALL ->
            Fun(K1, V1, Fun(K2, V2, Fun(K3, V3, Fun(K4, V4, Acc))));
        %
        ?LEAF5_MATCH_ALL ->
            Fun(K1, V1, Fun(K2, V2, Fun(K3, V3, Fun(K4, V4, Fun(K5, V5, Acc)))));
        %
        ?LEAF6_MATCH_ALL ->
            Fun(K1, V1, Fun(K2, V2, Fun(K3, V3, Fun(K4, V4, Fun(K5, V5, Fun(K6, V6, Acc))))));
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = foldr_recur(Fun, Acc, C4),
            Acc3 = foldr_recur(Fun, Fun(K3, V3, Acc2), C3),
            Acc4 = foldr_recur(Fun, Fun(K2, V2, Acc3), C2),
            _Acc5 = foldr_recur(Fun, Fun(K1, V1, Acc4), C1);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = foldr_recur(Fun, Acc, C5),
            Acc3 = foldr_recur(Fun, Fun(K4, V4, Acc2), C4),
            Acc4 = foldr_recur(Fun, Fun(K3, V3, Acc3), C3),
            Acc5 = foldr_recur(Fun, Fun(K2, V2, Acc4), C2),
            _Acc6 = foldr_recur(Fun, Fun(K1, V1, Acc5), C1);
        %
        ?INTERNAL5_MATCH_ALL ->
            Acc2 = foldr_recur(Fun, Acc, C6),
            Acc3 = foldr_recur(Fun, Fun(K5, V5, Acc2), C5),
            Acc4 = foldr_recur(Fun, Fun(K4, V4, Acc3), C4),
            Acc5 = foldr_recur(Fun, Fun(K3, V3, Acc4), C3),
            Acc6 = foldr_recur(Fun, Fun(K2, V2, Acc5), C2),
            _Acc7 = foldr_recur(Fun, Fun(K1, V1, Acc6), C1);
        %
        ?INTERNAL6_MATCH_ALL ->
            Acc2 = foldr_recur(Fun, Acc, C7),
            Acc3 = foldr_recur(Fun, Fun(K6, V6, Acc2), C6),
            Acc4 = foldr_recur(Fun, Fun(K5, V5, Acc3), C5),
            Acc5 = foldr_recur(Fun, Fun(K4, V4, Acc4), C4),
            Acc6 = foldr_recur(Fun, Fun(K3, V3, Acc5), C3),
            Acc7 = foldr_recur(Fun, Fun(K2, V2, Acc6), C2),
            _Acc8 = foldr_recur(Fun, Fun(K1, V1, Acc7), C1)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: get/2
%% ------------------------------------------------------------------

get_recur(Key, Node) ->
    case Node of
        ?INTERNAL3_MATCH_ALL ->
            get_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            get_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?INTERNAL5_MATCH_ALL ->
            get_INTERNAL5(Key, ?INTERNAL5_ARGS);
        %
        ?INTERNAL6_MATCH_ALL ->
            get_INTERNAL6(Key, ?INTERNAL6_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            get_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            get_LEAF4(Key, ?LEAF4_ARGS);
        %
        ?LEAF5_MATCH_ALL ->
            get_LEAF5(Key, ?LEAF5_ARGS);
        %
        ?LEAF6_MATCH_ALL ->
            get_LEAF6(Key, ?LEAF6_ARGS)
    end.

%%
%% ?INTERNAL6
%%

-compile({inline, get_INTERNAL6 / ?INTERNAL6_ARITY_PLUS1}).
get_INTERNAL6(Key, ?INTERNAL6_ARGS) ->
    if
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            get_recur(Key, C5);
                        %
                        Key < K4 ->
                            get_recur(Key, C4);
                        %
                        true ->
                            V4
                    end;
                %
                Key > K5 ->
                    %
                    if
                        Key < K6 ->
                            get_recur(Key, C6);
                        %
                        Key > K6 ->
                            get_recur(Key, C7);
                        %
                        true ->
                            V6
                    end;
                %
                true ->
                    V5
            end;
        %
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            get_recur(Key, C2);
                        %
                        Key > K2 ->
                            get_recur(Key, C3);
                        %
                        true ->
                            V2
                    end;
                %
                Key < K1 ->
                    get_recur(Key, C1);
                %
                true ->
                    V1
            end;
        %
        true ->
            V3
    end.

%%
%% ?INTERNAL5
%%

-compile({inline, get_INTERNAL5 / ?INTERNAL5_ARITY_PLUS1}).
get_INTERNAL5(Key, ?INTERNAL5_ARGS) ->
    if
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            get_recur(Key, C2);
                        %
                        Key > K2 ->
                            get_recur(Key, C3);
                        %
                        true ->
                            V2
                    end;
                %
                Key < K1 ->
                    get_recur(Key, C1);
                %
                true ->
                    V1
            end;
        %
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            get_recur(Key, C5);
                        %
                        Key < K4 ->
                            get_recur(Key, C4);
                        %
                        true ->
                            V4
                    end;
                %
                Key > K5 ->
                    get_recur(Key, C6);
                %
                true ->
                    V5
            end;
        %
        true ->
            V3
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, get_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
get_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            get_recur(Key, C4);
                        %
                        Key < K3 ->
                            get_recur(Key, C3);
                        %
                        true ->
                            V3
                    end;
                %
                Key > K4 ->
                    get_recur(Key, C5);
                %
                true ->
                    V4
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    get_recur(Key, C1);
                %
                Key > K1 ->
                    get_recur(Key, C2);
                %
                true ->
                    V1
            end;
        %
        true ->
            V2
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, get_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
get_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    get_recur(Key, C1);
                %
                Key > K1 ->
                    get_recur(Key, C2);
                %
                true ->
                    V1
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    get_recur(Key, C3);
                %
                Key > K3 ->
                    get_recur(Key, C4);
                %
                true ->
                    V3
            end;
        %
        true ->
            V2
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, get_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
get_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    get_recur(Key, C2);
                %
                Key > K2 ->
                    get_recur(Key, C3);
                %
                true ->
                    V2
            end;
        %
        Key < K1 ->
            get_recur(Key, C1);
        %
        true ->
            V1
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, get_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
get_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            get_recur(Key, C1);
        %
        Key > K1 ->
            get_recur(Key, C2);
        %
        true ->
            V1
    end.

%%
%% ?LEAF6
%%

-compile({inline, get_LEAF6 / ?LEAF6_ARITY_PLUS1}).
get_LEAF6(Key, ?LEAF6_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        Key == K2 ->
            V2;
        %
        Key == K3 ->
            V3;
        %
        Key == K4 ->
            V4;
        %
        Key == K5 ->
            V5;
        %
        Key == K6 ->
            V6;
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF5
%%

-compile({inline, get_LEAF5 / ?LEAF5_ARITY_PLUS1}).
get_LEAF5(Key, ?LEAF5_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        Key == K2 ->
            V2;
        %
        Key == K3 ->
            V3;
        %
        Key == K4 ->
            V4;
        %
        Key == K5 ->
            V5;
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF4
%%

-compile({inline, get_LEAF4 / ?LEAF4_ARITY_PLUS1}).
get_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        Key == K2 ->
            V2;
        %
        Key == K3 ->
            V3;
        %
        Key == K4 ->
            V4;
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF3
%%

-compile({inline, get_LEAF3 / ?LEAF3_ARITY_PLUS1}).
get_LEAF3(Key, ?LEAF3_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        Key == K2 ->
            V2;
        %
        Key == K3 ->
            V3;
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF2
%%

-compile({inline, get_LEAF2 / ?LEAF2_ARITY_PLUS1}).
get_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        Key == K2 ->
            V2;
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF1
%%

-compile({inline, get_LEAF1 / ?LEAF1_ARITY_PLUS1}).
get_LEAF1(Key, ?LEAF1_ARGS) ->
    if
        Key == K1 ->
            V1;
        %
        true ->
            error_badkey(Key)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: insert/2
%% ------------------------------------------------------------------

insert_recur(Key, ValueEval, ValueWrap, Node) ->
    case Node of
        ?INTERNAL3_MATCH_ALL ->
            insert_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            insert_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
        %
        ?INTERNAL5_MATCH_ALL ->
            insert_INTERNAL5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
        %
        ?INTERNAL6_MATCH_ALL ->
            insert_INTERNAL6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            insert_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            insert_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
        %
        ?LEAF5_MATCH_ALL ->
            insert_LEAF5(Key, ValueEval, ValueWrap, ?LEAF5_ARGS);
        %
        ?LEAF6_MATCH_ALL ->
            insert_LEAF6(Key, ValueEval, ValueWrap, ?LEAF6_ARGS)
    end.

%%
%% ?INTERNAL6
%%

-compile({inline, insert_INTERNAL6 / ?INTERNAL6_ARITY_PLUS3}).
insert_INTERNAL6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    if
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            insert_INTERNAL6_C5(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        Key < K4 ->
                            insert_INTERNAL6_C4(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key > K5 ->
                    %
                    if
                        Key < K6 ->
                            insert_INTERNAL6_C6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        Key > K6 ->
                            insert_INTERNAL6_C7(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            insert_INTERNAL6_C2(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        Key > K2 ->
                            insert_INTERNAL6_C3(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key < K1 ->
                    insert_INTERNAL6_C1(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL6_C1 / ?INTERNAL6_ARITY_PLUS3}).
insert_INTERNAL6_C1(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL6_VALUES_MATCH_ALL = Values,

            split_internal(
                SplitK,
                K1,
                K2,
                K3,
                K4,
                K5,
                K6,
                %
                SplitV,
                V1,
                V2,
                V3,
                V4,
                V5,
                V6,
                %
                SplitL,
                SplitR,
                C2,
                C3,
                C4,
                C5,
                C6,
                C7
            );
        %
        UpdatedC1 ->
            ?INTERNAL6_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL6_C2 / ?INTERNAL6_ARITY_PLUS3}).
insert_INTERNAL6_C2(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL6_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                SplitK,
                K2,
                K3,
                K4,
                K5,
                K6,
                %
                V1,
                SplitV,
                V2,
                V3,
                V4,
                V5,
                V6,
                %
                C1,
                SplitL,
                SplitR,
                C3,
                C4,
                C5,
                C6,
                C7
            );
        %
        UpdatedC2 ->
            ?INTERNAL6_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL6_C3 / ?INTERNAL6_ARITY_PLUS3}).
insert_INTERNAL6_C3(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL6_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                K2,
                SplitK,
                K3,
                K4,
                K5,
                K6,
                %
                V1,
                V2,
                SplitV,
                V3,
                V4,
                V5,
                V6,
                %
                C1,
                C2,
                SplitL,
                SplitR,
                C4,
                C5,
                C6,
                C7
            );
        %
        UpdatedC3 ->
            ?INTERNAL6_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL6_C4 / ?INTERNAL6_ARITY_PLUS3}).
insert_INTERNAL6_C4(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL6_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                K2,
                K3,
                SplitK,
                K4,
                K5,
                K6,
                %
                V1,
                V2,
                V3,
                SplitV,
                V4,
                V5,
                V6,
                %
                C1,
                C2,
                C3,
                SplitL,
                SplitR,
                C5,
                C6,
                C7
            );
        %
        UpdatedC4 ->
            ?INTERNAL6_C4(UpdatedC4)
    end.

-compile({inline, insert_INTERNAL6_C5 / ?INTERNAL6_ARITY_PLUS3}).
insert_INTERNAL6_C5(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C5) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL6_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                K2,
                K3,
                K4,
                SplitK,
                K5,
                K6,
                %
                V1,
                V2,
                V3,
                V4,
                SplitV,
                V5,
                V6,
                %
                C1,
                C2,
                C3,
                C4,
                SplitL,
                SplitR,
                C6,
                C7
            );
        %
        UpdatedC5 ->
            ?INTERNAL6_C5(UpdatedC5)
    end.

-compile({inline, insert_INTERNAL6_C6 / ?INTERNAL6_ARITY_PLUS3}).
insert_INTERNAL6_C6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C6) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL6_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                K2,
                K3,
                K4,
                K5,
                SplitK,
                K6,
                %
                V1,
                V2,
                V3,
                V4,
                V5,
                SplitV,
                V6,
                %
                C1,
                C2,
                C3,
                C4,
                C5,
                SplitL,
                SplitR,
                C7
            );
        %
        UpdatedC6 ->
            ?INTERNAL6_C6(UpdatedC6)
    end.

-compile({inline, insert_INTERNAL6_C7 / ?INTERNAL6_ARITY_PLUS3}).
insert_INTERNAL6_C7(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C7) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL6_VALUES_MATCH_ALL = Values,

            split_internal(
                K1,
                K2,
                K3,
                K4,
                K5,
                K6,
                SplitK,
                %
                V1,
                V2,
                V3,
                V4,
                V5,
                V6,
                SplitV,
                %
                C1,
                C2,
                C3,
                C4,
                C5,
                C6,
                SplitL,
                SplitR
            );
        %
        UpdatedC7 ->
            ?INTERNAL6_C7(UpdatedC7)
    end.

%%
%% ?INTERNAL5
%%

-compile({inline, insert_INTERNAL5 / ?INTERNAL5_ARITY_PLUS3}).
insert_INTERNAL5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    if
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            insert_INTERNAL5_C2(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                        %
                        Key > K2 ->
                            insert_INTERNAL5_C3(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key < K1 ->
                    insert_INTERNAL5_C1(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            insert_INTERNAL5_C5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                        %
                        Key < K4 ->
                            insert_INTERNAL5_C4(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key > K5 ->
                    insert_INTERNAL5_C6(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL5_C1 / ?INTERNAL5_ARITY_PLUS3}).
insert_INTERNAL5_C1(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL5_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL6(
                SplitK,
                K1,
                K2,
                K3,
                K4,
                K5,
                %
                SplitV,
                V1,
                V2,
                V3,
                V4,
                V5,
                %
                SplitL,
                SplitR,
                C2,
                C3,
                C4,
                C5,
                C6
            );
        %
        UpdatedC1 ->
            ?INTERNAL5_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL5_C2 / ?INTERNAL5_ARITY_PLUS3}).
insert_INTERNAL5_C2(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL5_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL6(
                K1,
                SplitK,
                K2,
                K3,
                K4,
                K5,
                %
                V1,
                SplitV,
                V2,
                V3,
                V4,
                V5,
                %
                C1,
                SplitL,
                SplitR,
                C3,
                C4,
                C5,
                C6
            );
        %
        UpdatedC2 ->
            ?INTERNAL5_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL5_C3 / ?INTERNAL5_ARITY_PLUS3}).
insert_INTERNAL5_C3(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL5_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL6(
                K1,
                K2,
                SplitK,
                K3,
                K4,
                K5,
                %
                V1,
                V2,
                SplitV,
                V3,
                V4,
                V5,
                %
                C1,
                C2,
                SplitL,
                SplitR,
                C4,
                C5,
                C6
            );
        %
        UpdatedC3 ->
            ?INTERNAL5_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL5_C4 / ?INTERNAL5_ARITY_PLUS3}).
insert_INTERNAL5_C4(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL5_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL6(
                K1,
                K2,
                K3,
                SplitK,
                K4,
                K5,
                %
                V1,
                V2,
                V3,
                SplitV,
                V4,
                V5,
                %
                C1,
                C2,
                C3,
                SplitL,
                SplitR,
                C5,
                C6
            );
        %
        UpdatedC4 ->
            ?INTERNAL5_C4(UpdatedC4)
    end.

-compile({inline, insert_INTERNAL5_C5 / ?INTERNAL5_ARITY_PLUS3}).
insert_INTERNAL5_C5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C5) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL5_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL6(
                K1,
                K2,
                K3,
                K4,
                SplitK,
                K5,
                %
                V1,
                V2,
                V3,
                V4,
                SplitV,
                V5,
                %
                C1,
                C2,
                C3,
                C4,
                SplitL,
                SplitR,
                C6
            );
        %
        UpdatedC5 ->
            ?INTERNAL5_C5(UpdatedC5)
    end.

-compile({inline, insert_INTERNAL5_C6 / ?INTERNAL5_ARITY_PLUS3}).
insert_INTERNAL5_C6(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C6) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL5_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL6(
                K1,
                K2,
                K3,
                K4,
                K5,
                SplitK,
                %
                V1,
                V2,
                V3,
                V4,
                V5,
                SplitV,
                %
                C1,
                C2,
                C3,
                C4,
                C5,
                SplitL,
                SplitR
            );
        %
        UpdatedC6 ->
            ?INTERNAL5_C6(UpdatedC6)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, insert_INTERNAL4 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            insert_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            insert_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key > K4 ->
                    insert_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    insert_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL5(
                SplitK,
                K1,
                K2,
                K3,
                K4,
                %
                SplitV,
                V1,
                V2,
                V3,
                V4,
                %
                SplitL,
                SplitR,
                C2,
                C3,
                C4,
                C5
            );
        %
        UpdatedC1 ->
            ?INTERNAL4_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL5(
                K1,
                SplitK,
                K2,
                K3,
                K4,
                %
                V1,
                SplitV,
                V2,
                V3,
                V4,
                %
                C1,
                SplitL,
                SplitR,
                C3,
                C4,
                C5
            );
        %
        UpdatedC2 ->
            ?INTERNAL4_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL5(
                K1,
                K2,
                SplitK,
                K3,
                K4,
                %
                V1,
                V2,
                SplitV,
                V3,
                V4,
                %
                C1,
                C2,
                SplitL,
                SplitR,
                C4,
                C5
            );
        %
        UpdatedC3 ->
            ?INTERNAL4_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL5(
                K1,
                K2,
                K3,
                SplitK,
                K4,
                %
                V1,
                V2,
                V3,
                SplitV,
                V4,
                %
                C1,
                C2,
                C3,
                SplitL,
                SplitR,
                C5
            );
        %
        UpdatedC4 ->
            ?INTERNAL4_C4(UpdatedC4)
    end.

-compile({inline, insert_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS3}).
insert_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C5) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL5(
                K1,
                K2,
                K3,
                K4,
                SplitK,
                %
                V1,
                V2,
                V3,
                V4,
                SplitV,
                %
                C1,
                C2,
                C3,
                C4,
                SplitL,
                SplitR
            );
        %
        UpdatedC5 ->
            ?INTERNAL4_C5(UpdatedC5)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, insert_INTERNAL3 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    insert_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    insert_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    insert_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL4(
                SplitK,
                K1,
                K2,
                K3,
                %
                SplitV,
                V1,
                V2,
                V3,
                %
                SplitL,
                SplitR,
                C2,
                C3,
                C4
            );
        %
        UpdatedC1 ->
            ?INTERNAL3_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL4(
                K1,
                SplitK,
                K2,
                K3,
                %
                V1,
                SplitV,
                V2,
                V3,
                %
                C1,
                SplitL,
                SplitR,
                C3,
                C4
            );
        %
        UpdatedC2 ->
            ?INTERNAL3_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL4(
                K1,
                K2,
                SplitK,
                K3,
                %
                V1,
                V2,
                SplitV,
                V3,
                %
                C1,
                C2,
                SplitL,
                SplitR,
                C4
            );
        %
        UpdatedC3 ->
            ?INTERNAL3_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS3}).
insert_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C4) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL4(
                K1,
                K2,
                K3,
                SplitK,
                %
                V1,
                V2,
                V3,
                SplitV,
                %
                C1,
                C2,
                C3,
                SplitL,
                SplitR
            );
        %
        UpdatedC4 ->
            ?INTERNAL3_C4(UpdatedC4)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, insert_INTERNAL2 / ?INTERNAL2_ARITY_PLUS3}).
insert_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    insert_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    insert_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K1 ->
            insert_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS3}).
insert_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL2_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL3(
                SplitK,
                K1,
                K2,
                %
                SplitV,
                V1,
                V2,
                %
                SplitL,
                SplitR,
                C2,
                C3
            );
        %
        UpdatedC1 ->
            ?INTERNAL2_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS3}).
insert_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL2_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL3(
                K1,
                SplitK,
                K2,
                %
                V1,
                SplitV,
                V2,
                %
                C1,
                SplitL,
                SplitR,
                C3
            );
        %
        UpdatedC2 ->
            ?INTERNAL2_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS3}).
insert_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C3) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            % ?INTERNAL2_VALUES_MATCH_ALL = Values,

            ?new_INTERNAL3(
                K1,
                K2,
                SplitK,
                %
                V1,
                V2,
                SplitV,
                %
                C1,
                C2,
                SplitL,
                SplitR
            );
        %
        UpdatedC3 ->
            ?INTERNAL2_C3(UpdatedC3)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, insert_INTERNAL1 / ?INTERNAL1_ARITY_PLUS3}).
insert_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            insert_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            insert_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS3}).
insert_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C1) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            ?new_INTERNAL2(
                SplitK,
                K1,
                %
                SplitV,
                V1,
                %
                SplitL,
                SplitR,
                C2
            );
        %
        UpdatedC1 ->
            ?INTERNAL1_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS3}).
insert_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    case insert_recur(Key, ValueEval, ValueWrap, C2) of
        ?SPLIT_MATCH(SplitK, SplitV, SplitL, SplitR) ->
            ?new_INTERNAL2(
                K1,
                SplitK,
                %
                V1,
                SplitV,
                %
                C1,
                SplitL,
                SplitR
            );
        %
        UpdatedC2 ->
            ?INTERNAL1_C2(UpdatedC2)
    end.

%%
%% ?LEAF6
%%

-compile({inline, insert_LEAF6 / ?LEAF6_ARITY_PLUS3}).
insert_LEAF6(Key, ValueEval, ValueWrap, ?LEAF6_ARGS) ->
    if
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            insert_LEAF6_POS5(Key, ValueEval, ValueWrap, ?LEAF6_ARGS);
                        %
                        Key < K4 ->
                            insert_LEAF6_POS4(Key, ValueEval, ValueWrap, ?LEAF6_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key > K5 ->
                    %
                    if
                        Key < K6 ->
                            insert_LEAF6_POS6(Key, ValueEval, ValueWrap, ?LEAF6_ARGS);
                        %
                        Key > K6 ->
                            insert_LEAF6_POS7(Key, ValueEval, ValueWrap, ?LEAF6_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            insert_LEAF6_POS2(Key, ValueEval, ValueWrap, ?LEAF6_ARGS);
                        %
                        Key > K2 ->
                            insert_LEAF6_POS3(Key, ValueEval, ValueWrap, ?LEAF6_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key < K1 ->
                    insert_LEAF6_POS1(Key, ValueEval, ValueWrap, ?LEAF6_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF6_POS1 / ?LEAF6_ARITY_PLUS3}).
insert_LEAF6_POS1(Key, ValueEval, ValueWrap, ?LEAF6_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        Key,
        K1,
        K2,
        K3,
        K4,
        K5,
        K6,
        %
        Value,
        V1,
        V2,
        V3,
        V4,
        V5,
        V6
    ).

-compile({inline, insert_LEAF6_POS2 / ?LEAF6_ARITY_PLUS3}).
insert_LEAF6_POS2(Key, ValueEval, ValueWrap, ?LEAF6_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        Key,
        K2,
        K3,
        K4,
        K5,
        K6,
        %
        V1,
        Value,
        V2,
        V3,
        V4,
        V5,
        V6
    ).

-compile({inline, insert_LEAF6_POS3 / ?LEAF6_ARITY_PLUS3}).
insert_LEAF6_POS3(Key, ValueEval, ValueWrap, ?LEAF6_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        K2,
        Key,
        K3,
        K4,
        K5,
        K6,
        %
        V1,
        V2,
        Value,
        V3,
        V4,
        V5,
        V6
    ).

-compile({inline, insert_LEAF6_POS4 / ?LEAF6_ARITY_PLUS3}).
insert_LEAF6_POS4(Key, ValueEval, ValueWrap, ?LEAF6_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        K2,
        K3,
        Key,
        K4,
        K5,
        K6,
        %
        V1,
        V2,
        V3,
        Value,
        V4,
        V5,
        V6
    ).

-compile({inline, insert_LEAF6_POS5 / ?LEAF6_ARITY_PLUS3}).
insert_LEAF6_POS5(Key, ValueEval, ValueWrap, ?LEAF6_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        K2,
        K3,
        K4,
        Key,
        K5,
        K6,
        %
        V1,
        V2,
        V3,
        V4,
        Value,
        V5,
        V6
    ).

-compile({inline, insert_LEAF6_POS6 / ?LEAF6_ARITY_PLUS3}).
insert_LEAF6_POS6(Key, ValueEval, ValueWrap, ?LEAF6_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        K2,
        K3,
        K4,
        K5,
        Key,
        K6,
        %
        V1,
        V2,
        V3,
        V4,
        V5,
        Value,
        V6
    ).

-compile({inline, insert_LEAF6_POS7 / ?LEAF6_ARITY_PLUS3}).
insert_LEAF6_POS7(Key, ValueEval, ValueWrap, ?LEAF6_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    split_leaf(
        K1,
        K2,
        K3,
        K4,
        K5,
        K6,
        Key,
        %
        V1,
        V2,
        V3,
        V4,
        V5,
        V6,
        Value
    ).
%%
%% ?LEAF5
%%

-compile({inline, insert_LEAF5 / ?LEAF5_ARITY_PLUS3}).
insert_LEAF5(Key, ValueEval, ValueWrap, ?LEAF5_ARGS) ->
    if
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            insert_LEAF5_POS2(Key, ValueEval, ValueWrap, ?LEAF5_ARGS);
                        %
                        Key > K2 ->
                            insert_LEAF5_POS3(Key, ValueEval, ValueWrap, ?LEAF5_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key < K1 ->
                    insert_LEAF5_POS1(Key, ValueEval, ValueWrap, ?LEAF5_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            insert_LEAF5_POS5(Key, ValueEval, ValueWrap, ?LEAF5_ARGS);
                        %
                        Key < K4 ->
                            insert_LEAF5_POS4(Key, ValueEval, ValueWrap, ?LEAF5_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key > K5 ->
                    insert_LEAF5_POS6(Key, ValueEval, ValueWrap, ?LEAF5_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF5_POS1 / ?LEAF5_ARITY_PLUS3}).
insert_LEAF5_POS1(Key, ValueEval, ValueWrap, ?LEAF5_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF6(
        Key,
        K1,
        K2,
        K3,
        K4,
        K5,
        %
        Value,
        V1,
        V2,
        V3,
        V4,
        V5
    ).

-compile({inline, insert_LEAF5_POS2 / ?LEAF5_ARITY_PLUS3}).
insert_LEAF5_POS2(Key, ValueEval, ValueWrap, ?LEAF5_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF6(
        K1,
        Key,
        K2,
        K3,
        K4,
        K5,
        %
        V1,
        Value,
        V2,
        V3,
        V4,
        V5
    ).

-compile({inline, insert_LEAF5_POS3 / ?LEAF5_ARITY_PLUS3}).
insert_LEAF5_POS3(Key, ValueEval, ValueWrap, ?LEAF5_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF6(
        K1,
        K2,
        Key,
        K3,
        K4,
        K5,
        %
        V1,
        V2,
        Value,
        V3,
        V4,
        V5
    ).

-compile({inline, insert_LEAF5_POS4 / ?LEAF5_ARITY_PLUS3}).
insert_LEAF5_POS4(Key, ValueEval, ValueWrap, ?LEAF5_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF6(
        K1,
        K2,
        K3,
        Key,
        K4,
        K5,
        %
        V1,
        V2,
        V3,
        Value,
        V4,
        V5
    ).

-compile({inline, insert_LEAF5_POS5 / ?LEAF5_ARITY_PLUS3}).
insert_LEAF5_POS5(Key, ValueEval, ValueWrap, ?LEAF5_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF6(
        K1,
        K2,
        K3,
        K4,
        Key,
        K5,
        %
        V1,
        V2,
        V3,
        V4,
        Value,
        V5
    ).

-compile({inline, insert_LEAF5_POS6 / ?LEAF5_ARITY_PLUS3}).
insert_LEAF5_POS6(Key, ValueEval, ValueWrap, ?LEAF5_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF6(
        K1,
        K2,
        K3,
        K4,
        K5,
        Key,
        %
        V1,
        V2,
        V3,
        V4,
        V5,
        Value
    ).

%%
%% ?LEAF4
%%

-compile({inline, insert_LEAF4 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            insert_LEAF4_POS4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                        %
                        Key < K3 ->
                            insert_LEAF4_POS3(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                        %
                        true ->
                            error_key_exists(Key)
                    end;
                %
                Key > K4 ->
                    insert_LEAF4_POS5(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_LEAF4_POS1(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                %
                Key > K1 ->
                    insert_LEAF4_POS2(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF4_POS1 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS1(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF5(
        Key,
        K1,
        K2,
        K3,
        K4,
        %
        Value,
        V1,
        V2,
        V3,
        V4
    ).

-compile({inline, insert_LEAF4_POS2 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS2(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF5(
        K1,
        Key,
        K2,
        K3,
        K4,
        %
        V1,
        Value,
        V2,
        V3,
        V4
    ).

-compile({inline, insert_LEAF4_POS3 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS3(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF5(
        K1,
        K2,
        Key,
        K3,
        K4,
        %
        V1,
        V2,
        Value,
        V3,
        V4
    ).

-compile({inline, insert_LEAF4_POS4 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF5(
        K1,
        K2,
        K3,
        Key,
        K4,
        %
        V1,
        V2,
        V3,
        Value,
        V4
    ).

-compile({inline, insert_LEAF4_POS5 / ?LEAF4_ARITY_PLUS3}).
insert_LEAF4_POS5(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF5(
        K1,
        K2,
        K3,
        K4,
        Key,
        %
        V1,
        V2,
        V3,
        V4,
        Value
    ).

%%
%% ?LEAF3
%%

-compile({inline, insert_LEAF3 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    insert_LEAF3_POS1(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
                %
                Key > K1 ->
                    insert_LEAF3_POS2(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    insert_LEAF3_POS3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
                %
                Key > K3 ->
                    insert_LEAF3_POS4(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF3_POS1 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3_POS1(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF4(
        Key,
        K1,
        K2,
        K3,
        %
        Value,
        V1,
        V2,
        V3
    ).

-compile({inline, insert_LEAF3_POS2 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3_POS2(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF4(
        K1,
        Key,
        K2,
        K3,
        %
        V1,
        Value,
        V2,
        V3
    ).

-compile({inline, insert_LEAF3_POS3 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3_POS3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF4(
        K1,
        K2,
        Key,
        K3,
        %
        V1,
        V2,
        Value,
        V3
    ).

-compile({inline, insert_LEAF3_POS4 / ?LEAF3_ARITY_PLUS3}).
insert_LEAF3_POS4(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF4(
        K1,
        K2,
        K3,
        Key,
        %
        V1,
        V2,
        V3,
        Value
    ).

%%
%% ?LEAF2
%%

-compile({inline, insert_LEAF2 / ?LEAF2_ARITY_PLUS3}).
insert_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    insert_LEAF2_POS2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
                %
                Key > K2 ->
                    insert_LEAF2_POS3(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
                %
                true ->
                    error_key_exists(Key)
            end;
        %
        Key < K1 ->
            insert_LEAF2_POS1(Key, ValueEval, ValueWrap, ?LEAF2_ARGS);
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF2_POS1 / ?LEAF2_ARITY_PLUS3}).
insert_LEAF2_POS1(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF3(
        Key,
        K1,
        K2,
        %
        Value,
        V1,
        V2
    ).

-compile({inline, insert_LEAF2_POS2 / ?LEAF2_ARITY_PLUS3}).
insert_LEAF2_POS2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF3(
        K1,
        Key,
        K2,
        %
        V1,
        Value,
        V2
    ).

-compile({inline, insert_LEAF2_POS3 / ?LEAF2_ARITY_PLUS3}).
insert_LEAF2_POS3(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF3(
        K1,
        K2,
        Key,
        %
        V1,
        V2,
        Value
    ).

%%
%% ?LEAF1
%%

-compile({inline, insert_LEAF1 / ?LEAF1_ARITY_PLUS3}).
insert_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    if
        Key < K1 ->
            insert_LEAF1_POS1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
        %
        Key > K1 ->
            insert_LEAF1_POS2(Key, ValueEval, ValueWrap, ?LEAF1_ARGS);
        %
        true ->
            error_key_exists(Key)
    end.

-compile({inline, insert_LEAF1_POS1 / ?LEAF1_ARITY_PLUS3}).
insert_LEAF1_POS1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF2(Key, K1, Value, V1).

-compile({inline, insert_LEAF1_POS2 / ?LEAF1_ARITY_PLUS3}).
insert_LEAF1_POS2(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    Value = eval_insert_value(ValueEval, ValueWrap),

    ?new_LEAF2(K1, Key, V1, Value).

%%%

%-spec eval_insert_value(insertion_value_eval(), insertion_value_wrap(Value)) -> Value.
-compile({inline, eval_insert_value/2}).
eval_insert_value(Type, Wrap) ->
    case Type of
        eager ->
            Wrap;
        %
        lazy ->
            Wrap()
    end.

%%
%% Split
%%

-compile({inline, split_internal/22}).
split_internal(
    K1,
    K2,
    K3,
    K4,
    K5,
    K6,
    K7,
    %
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
    %
    C1,
    C2,
    C3,
    C4,
    C5,
    C6,
    C7,
    C8
) ->
    SplitK = K4,
    SplitV = V4,

    SplitL = ?new_INTERNAL3(
        K1,
        K2,
        K3,
        %
        V1,
        V2,
        V3,
        %
        C1,
        C2,
        C3,
        C4
    ),

    SplitR = ?new_INTERNAL3(
        K5,
        K6,
        K7,
        %
        V5,
        V6,
        V7,
        %
        C5,
        C6,
        C7,
        C8
    ),

    ?SPLIT(SplitK, SplitV, SplitL, SplitR).

-compile({inline, split_leaf/14}).
split_leaf(
    K1,
    K2,
    K3,
    K4,
    K5,
    K6,
    K7,
    %
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7
) ->
    SplitK = K4,
    SplitV = V4,

    SplitL = ?new_LEAF3(
        K1,
        K2,
        K3,
        %
        V1,
        V2,
        V3
    ),

    SplitR = ?new_LEAF3(
        K5,
        K6,
        K7,
        %
        V5,
        V6,
        V7
    ),

    ?SPLIT(SplitK, SplitV, SplitL, SplitR).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - forward
%% ------------------------------------------------------------------

fwd_iterator(Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            Acc = [
                ?ITER_PAIR(K1, V1),
                C2,
                ?ITER_PAIR(K2, V2),
                C3
            ],
            fwd_iterator_recur(C1, Acc);
        %
        ?INTERNAL1_MATCH_ALL ->
            Acc = [?ITER_PAIR(K1, V1), C2],
            fwd_iterator_recur(C1, Acc);
        %
        ?LEAF2_MATCH_ALL ->
            Iter = [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2)],
            Iter;
        %
        ?LEAF1_MATCH_ALL ->
            Iter = [?ITER_PAIR(K1, V1)],
            Iter;
        %
        ?LEAF0_MATCH ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            fwd_iterator_recur(Root, Acc)
    end.

fwd_iterator_recur(?LEAF3_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3) | Acc],
    Acc2;
fwd_iterator_recur(?LEAF4_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc],
    Acc2;
fwd_iterator_recur(?LEAF5_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        ?ITER_PAIR(K2, V2),
        ?ITER_PAIR(K3, V3),
        ?ITER_PAIR(K4, V4),
        ?ITER_PAIR(K5, V5)
        | Acc
    ],
    Acc2;
fwd_iterator_recur(?LEAF6_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        ?ITER_PAIR(K2, V2),
        ?ITER_PAIR(K3, V3),
        ?ITER_PAIR(K4, V4),
        ?ITER_PAIR(K5, V5),
        ?ITER_PAIR(K6, V6)
        | Acc
    ],
    Acc2;
fwd_iterator_recur(?INTERNAL3_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        C2,
        ?ITER_PAIR(K2, V2),
        C3,
        ?ITER_PAIR(K3, V3),
        C4
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?INTERNAL4_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        C2,
        ?ITER_PAIR(K2, V2),
        C3,
        ?ITER_PAIR(K3, V3),
        C4,
        ?ITER_PAIR(K4, V4),
        C5
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?INTERNAL5_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        C2,
        ?ITER_PAIR(K2, V2),
        C3,
        ?ITER_PAIR(K3, V3),
        C4,
        ?ITER_PAIR(K4, V4),
        C5,
        ?ITER_PAIR(K5, V5),
        C6
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?INTERNAL6_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K1, V1),
        C2,
        ?ITER_PAIR(K2, V2),
        C3,
        ?ITER_PAIR(K3, V3),
        C4,
        ?ITER_PAIR(K4, V4),
        C5,
        ?ITER_PAIR(K5, V5),
        C6,
        ?ITER_PAIR(K6, V6),
        C7
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - reverse
%% ------------------------------------------------------------------

rev_iterator(Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            Acc = [
                ?ITER_PAIR(K2, V2),
                C2,
                ?ITER_PAIR(K1, V1),
                C1
            ],
            rev_iterator_recur(C3, Acc);
        %
        ?INTERNAL1_MATCH_ALL ->
            Acc = [?ITER_PAIR(K1, V1), C1],
            rev_iterator_recur(C2, Acc);
        %
        ?LEAF2_MATCH_ALL ->
            Iter = [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1)],
            Iter;
        %
        ?LEAF1_MATCH_ALL ->
            Iter = [?ITER_PAIR(K1, V1)],
            Iter;
        %
        ?LEAF0_MATCH ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            rev_iterator_recur(Root, Acc)
    end.

rev_iterator_recur(?LEAF3_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc],
    Acc2;
rev_iterator_recur(?LEAF4_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_PAIR(K4, V4), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc],
    Acc2;
rev_iterator_recur(?LEAF5_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K5, V5),
        ?ITER_PAIR(K4, V4),
        ?ITER_PAIR(K3, V3),
        ?ITER_PAIR(K2, V2),
        ?ITER_PAIR(K1, V1)
        | Acc
    ],
    Acc2;
rev_iterator_recur(?LEAF6_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K6, V6),
        ?ITER_PAIR(K5, V5),
        ?ITER_PAIR(K4, V4),
        ?ITER_PAIR(K3, V3),
        ?ITER_PAIR(K2, V2),
        ?ITER_PAIR(K1, V1)
        | Acc
    ],
    Acc2;
rev_iterator_recur(?INTERNAL3_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K3, V3),
        C3,
        ?ITER_PAIR(K2, V2),
        C2,
        ?ITER_PAIR(K1, V1),
        C1
        | Acc
    ],
    rev_iterator_recur(C4, Acc2);
rev_iterator_recur(?INTERNAL4_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K4, V4),
        C4,
        ?ITER_PAIR(K3, V3),
        C3,
        ?ITER_PAIR(K2, V2),
        C2,
        ?ITER_PAIR(K1, V1),
        C1
        | Acc
    ],
    rev_iterator_recur(C5, Acc2);
rev_iterator_recur(?INTERNAL5_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K5, V5),
        C5,
        ?ITER_PAIR(K4, V4),
        C4,
        ?ITER_PAIR(K3, V3),
        C3,
        ?ITER_PAIR(K2, V2),
        C2,
        ?ITER_PAIR(K1, V1),
        C1
        | Acc
    ],
    rev_iterator_recur(C6, Acc2);
rev_iterator_recur(?INTERNAL6_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_PAIR(K6, V6),
        C6,
        ?ITER_PAIR(K5, V5),
        C5,
        ?ITER_PAIR(K4, V4),
        C4,
        ?ITER_PAIR(K3, V3),
        C3,
        ?ITER_PAIR(K2, V2),
        C2,
        ?ITER_PAIR(K1, V1),
        C1
        | Acc
    ],
    rev_iterator_recur(C7, Acc2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_fwd_iterator(Key, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            bound_fwd_iterator_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            bound_fwd_iterator_LEAF1(Key, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            bound_fwd_iterator_recur(Key, Root, Acc)
    end.

bound_fwd_iterator_recur(Key, Node, Acc) ->
    case Node of
        ?LEAF3_MATCH_ALL ->
            bound_fwd_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            bound_fwd_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc);
        %
        ?LEAF5_MATCH_ALL ->
            bound_fwd_iterator_LEAF5(Key, ?LEAF5_ARGS, Acc);
        %
        ?LEAF6_MATCH_ALL ->
            bound_fwd_iterator_LEAF6(Key, ?LEAF6_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc);
        %
        ?INTERNAL5_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL5(Key, ?INTERNAL5_ARGS, Acc);
        %
        ?INTERNAL6_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL6(Key, ?INTERNAL6_ARGS, Acc)
    end.

%% INTERNAL6

-compile({inline, bound_fwd_iterator_INTERNAL6 / ?INTERNAL6_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL6(Key, ?INTERNAL6_ARGS, Acc) ->
    if
        Key > K6 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Key, C7, Acc)
            end;
        %
        Key > K5 ->
            Acc2 = [?ITER_PAIR(K6, V6), C7 | Acc],
            bound_fwd_iterator_recur(Key, C6, Acc2);
        %
        Key > K4 ->
            Acc2 = [?ITER_PAIR(K5, V5), C6, ?ITER_PAIR(K6, V6), C7 | Acc],
            bound_fwd_iterator_recur(Key, C5, Acc2);
        %
        Key > K3 ->
            Acc2 = [?ITER_PAIR(K4, V4), C5, ?ITER_PAIR(K5, V5), C6, ?ITER_PAIR(K6, V6), C7 | Acc],
            bound_fwd_iterator_recur(Key, C4, Acc2);
        %
        Key > K2 ->
            Acc2 = [
                ?ITER_PAIR(K3, V3),
                C4,
                ?ITER_PAIR(K4, V4),
                C5,
                ?ITER_PAIR(K5, V5),
                C6,
                ?ITER_PAIR(K6, V6),
                C7
                | Acc
            ],
            bound_fwd_iterator_recur(Key, C3, Acc2);
        %
        Key > K1 ->
            Acc2 = [
                ?ITER_PAIR(K2, V2),
                C3,
                ?ITER_PAIR(K3, V3),
                C4,
                ?ITER_PAIR(K4, V4),
                C5,
                ?ITER_PAIR(K5, V5),
                C6,
                ?ITER_PAIR(K6, V6),
                C7
                | Acc
            ],
            bound_fwd_iterator_recur(Key, C2, Acc2);
        %
        true ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = [
                ?ITER_PAIR(K1, V1),
                C2,
                ?ITER_PAIR(K2, V2),
                C3,
                ?ITER_PAIR(K3, V3),
                C4,
                ?ITER_PAIR(K4, V4),
                C5,
                ?ITER_PAIR(K5, V5),
                C6,
                ?ITER_PAIR(K6, V6),
                C7
                | Acc
            ],
            bound_fwd_iterator_recur(Key, C1, Acc2)
    end.

%% INTERNAL5

-compile({inline, bound_fwd_iterator_INTERNAL5 / ?INTERNAL5_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL5(Key, ?INTERNAL5_ARGS, Acc) ->
    if
        Key > K5 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Key, C6, Acc)
            end;
        %
        Key > K4 ->
            Acc2 = [?ITER_PAIR(K5, V5), C6 | Acc],
            bound_fwd_iterator_recur(Key, C5, Acc2);
        %
        Key > K3 ->
            Acc2 = [?ITER_PAIR(K4, V4), C5, ?ITER_PAIR(K5, V5), C6 | Acc],
            bound_fwd_iterator_recur(Key, C4, Acc2);
        %
        Key > K2 ->
            Acc2 = [?ITER_PAIR(K3, V3), C4, ?ITER_PAIR(K4, V4), C5, ?ITER_PAIR(K5, V5), C6 | Acc],
            bound_fwd_iterator_recur(Key, C3, Acc2);
        %
        Key > K1 ->
            Acc2 = [
                ?ITER_PAIR(K2, V2),
                C3,
                ?ITER_PAIR(K3, V3),
                C4,
                ?ITER_PAIR(K4, V4),
                C5,
                ?ITER_PAIR(K5, V5),
                C6
                | Acc
            ],
            bound_fwd_iterator_recur(Key, C2, Acc2);
        %
        true ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = [
                ?ITER_PAIR(K1, V1),
                C2,
                ?ITER_PAIR(K2, V2),
                C3,
                ?ITER_PAIR(K3, V3),
                C4,
                ?ITER_PAIR(K4, V4),
                C5,
                ?ITER_PAIR(K5, V5),
                C6
                | Acc
            ],
            bound_fwd_iterator_recur(Key, C1, Acc2)
    end.

%% INTERNAL4

-compile({inline, bound_fwd_iterator_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc) ->
    if
        Key > K4 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Key, C5, Acc)
            end;
        %
        Key > K3 ->
            Acc2 = [?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C4, Acc2);
        %
        Key > K2 ->
            % ?INTERNAL4_VALUES(_, _, V3, V4) = Values,
            Acc2 = [?ITER_PAIR(K3, V3), C4, ?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C3, Acc2);
        %
        Key > K1 ->
            % ?INTERNAL4_VALUES(_, V2, V3, V4) = Values,
            Acc2 = [?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4, ?ITER_PAIR(K4, V4), C5 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2);
        %
        true ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = [
                ?ITER_PAIR(K1, V1),
                C2,
                ?ITER_PAIR(K2, V2),
                C3,
                ?ITER_PAIR(K3, V3),
                C4,
                ?ITER_PAIR(K4, V4),
                C5
                | Acc
            ],
            bound_fwd_iterator_recur(Key, C1, Acc2)
    end.

%% INTERNAL3

-compile({inline, bound_fwd_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc) ->
    if
        Key > K3 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Key, C4, Acc)
            end;
        %
        Key > K2 ->
            Acc2 = [?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C3, Acc2);
        %
        Key > K1 ->
            % ?INTERNAL3_VALUES(_, V2, V3) = Values,
            Acc2 = [?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C2, Acc2);
        %
        true ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,
            Acc2 = [?ITER_PAIR(K1, V1), C2, ?ITER_PAIR(K2, V2), C3, ?ITER_PAIR(K3, V3), C4 | Acc],
            bound_fwd_iterator_recur(Key, C1, Acc2)
    end.

%% INTERNAL2

-compile({inline, bound_fwd_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
bound_fwd_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K2 ->
            Acc = [],
            bound_fwd_iterator_recur(Key, C3, Acc);
        %
        Key > K1 ->
            Acc = [?ITER_PAIR(K2, V2), C3],
            bound_fwd_iterator_recur(Key, C2, Acc);
        %
        true ->
            Acc = [?ITER_PAIR(K1, V1), C2, ?ITER_PAIR(K2, V2), C3],
            bound_fwd_iterator_recur(Key, C1, Acc)
    end.

%% INTERNAL1

-compile({inline, bound_fwd_iterator_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
bound_fwd_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            Acc = [?ITER_PAIR(K1, V1), C2],
            bound_fwd_iterator_recur(Key, C1, Acc);
        %
        Key > K1 ->
            Acc = [],
            bound_fwd_iterator_recur(Key, C2, Acc);
        %
        true ->
            _Acc = [?ITER_PAIR(K1, V1), C2]
    end.

%% LEAF6

-compile({inline, bound_fwd_iterator_LEAF6 / ?LEAF6_ARITY_PLUS2}).
bound_fwd_iterator_LEAF6(Key, ?LEAF6_ARGS, Acc) ->
    if
        Key > K6 ->
            Acc;
        %
        Key > K5 ->
            [?ITER_PAIR(K6, V6) | Acc];
        %
        Key > K4 ->
            [?ITER_PAIR(K5, V5), ?ITER_PAIR(K6, V6) | Acc];
        %
        Key > K3 ->
            [?ITER_PAIR(K4, V4), ?ITER_PAIR(K5, V5), ?ITER_PAIR(K6, V6) | Acc];
        %
        Key > K2 ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4), ?ITER_PAIR(K5, V5), ?ITER_PAIR(K6, V6) | Acc];
        %
        Key > K1 ->
            [
                ?ITER_PAIR(K2, V2),
                ?ITER_PAIR(K3, V3),
                ?ITER_PAIR(K4, V4),
                ?ITER_PAIR(K5, V5),
                ?ITER_PAIR(K6, V6)
                | Acc
            ];
        %
        true ->
            [
                ?ITER_PAIR(K1, V1),
                ?ITER_PAIR(K2, V2),
                ?ITER_PAIR(K3, V3),
                ?ITER_PAIR(K4, V4),
                ?ITER_PAIR(K5, V5),
                ?ITER_PAIR(K6, V6)
                | Acc
            ]
    end.

%% LEAF5

-compile({inline, bound_fwd_iterator_LEAF5 / ?LEAF5_ARITY_PLUS2}).
bound_fwd_iterator_LEAF5(Key, ?LEAF5_ARGS, Acc) ->
    if
        Key > K5 ->
            Acc;
        %
        Key > K4 ->
            [?ITER_PAIR(K5, V5) | Acc];
        %
        Key > K3 ->
            [?ITER_PAIR(K4, V4), ?ITER_PAIR(K5, V5) | Acc];
        %
        Key > K2 ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4), ?ITER_PAIR(K5, V5) | Acc];
        %
        Key > K1 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4), ?ITER_PAIR(K5, V5) | Acc];
        %
        true ->
            [
                ?ITER_PAIR(K1, V1),
                ?ITER_PAIR(K2, V2),
                ?ITER_PAIR(K3, V3),
                ?ITER_PAIR(K4, V4),
                ?ITER_PAIR(K5, V5)
                | Acc
            ]
    end.

%% LEAF4

-compile({inline, bound_fwd_iterator_LEAF4 / ?LEAF4_ARITY_PLUS2}).
bound_fwd_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc) ->
    if
        Key > K4 ->
            Acc;
        %
        Key > K3 ->
            [?ITER_PAIR(K4, V4) | Acc];
        %
        Key > K2 ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc];
        %
        Key > K1 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc];
        %
        true ->
            [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K4, V4) | Acc]
    end.

%% LEAF3

-compile({inline, bound_fwd_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_fwd_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc) ->
    if
        Key > K3 ->
            Acc;
        %
        Key > K2 ->
            [?ITER_PAIR(K3, V3) | Acc];
        %
        Key > K1 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3) | Acc];
        %
        true ->
            [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K3, V3) | Acc]
    end.

%% LEAF2

-compile({inline, bound_fwd_iterator_LEAF2 / ?LEAF2_ARITY_PLUS1}).
bound_fwd_iterator_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key > K2 ->
            [];
        %
        Key > K1 ->
            [?ITER_PAIR(K2, V2)];
        %
        true ->
            [?ITER_PAIR(K1, V1), ?ITER_PAIR(K2, V2)]
    end.

%% LEAF1

-compile({inline, bound_fwd_iterator_LEAF1 / ?LEAF1_ARITY_PLUS1}).
bound_fwd_iterator_LEAF1(Key, ?LEAF1_ARGS) ->
    if
        Key > K1 ->
            [];
        %
        true ->
            [?ITER_PAIR(K1, V1)]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_rev_iterator(Key, Root) ->
    case Root of
        ?INTERNAL2_MATCH_ALL ->
            bound_rev_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS);
        %
        ?INTERNAL1_MATCH_ALL ->
            bound_rev_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            bound_rev_iterator_LEAF2(Key, ?LEAF2_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            bound_rev_iterator_LEAF1(Key, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            bound_rev_iterator_recur(Key, Root, Acc)
    end.

bound_rev_iterator_recur(Key, Node, Acc) ->
    case Node of
        ?LEAF3_MATCH_ALL ->
            bound_rev_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            bound_rev_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc);
        %
        ?LEAF5_MATCH_ALL ->
            bound_rev_iterator_LEAF5(Key, ?LEAF5_ARGS, Acc);
        %
        ?LEAF6_MATCH_ALL ->
            bound_rev_iterator_LEAF6(Key, ?LEAF6_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            bound_rev_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            bound_rev_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc);
        %
        ?INTERNAL5_MATCH_ALL ->
            bound_rev_iterator_INTERNAL5(Key, ?INTERNAL5_ARGS, Acc);
        %
        ?INTERNAL6_MATCH_ALL ->
            bound_rev_iterator_INTERNAL6(Key, ?INTERNAL6_ARGS, Acc)
    end.

%% INTERNAL6

-compile({inline, bound_rev_iterator_INTERNAL6 / ?INTERNAL6_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL6(Key, ?INTERNAL6_ARGS, Acc) ->
    if
        Key < K1 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Key, C1, Acc)
            end;
        %
        Key < K2 ->
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2);
        %
        Key < K3 ->
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2);
        %
        Key < K4 ->
            Acc2 = [?ITER_PAIR(K3, V3), C3, ?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C4, Acc2);
        %
        Key < K5 ->
            Acc2 = [
                ?ITER_PAIR(K4, V4),
                C4,
                ?ITER_PAIR(K3, V3),
                C3,
                ?ITER_PAIR(K2, V2),
                C2,
                ?ITER_PAIR(K1, V1),
                C1
                | Acc
            ],
            bound_rev_iterator_recur(Key, C5, Acc2);
        %
        Key < K6 ->
            Acc2 = [
                ?ITER_PAIR(K5, V5),
                C5,
                ?ITER_PAIR(K4, V4),
                C4,
                ?ITER_PAIR(K3, V3),
                C3,
                ?ITER_PAIR(K2, V2),
                C2,
                ?ITER_PAIR(K1, V1),
                C1
                | Acc
            ],
            bound_rev_iterator_recur(Key, C6, Acc2);
        %
        true ->
            Acc2 = [
                ?ITER_PAIR(K6, V6),
                C6,
                ?ITER_PAIR(K5, V5),
                C5,
                ?ITER_PAIR(K4, V4),
                C4,
                ?ITER_PAIR(K3, V3),
                C3,
                ?ITER_PAIR(K2, V2),
                C2,
                ?ITER_PAIR(K1, V1),
                C1
                | Acc
            ],
            bound_rev_iterator_recur(Key, C7, Acc2)
    end.

%% INTERNAL5

-compile({inline, bound_rev_iterator_INTERNAL5 / ?INTERNAL5_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL5(Key, ?INTERNAL5_ARGS, Acc) ->
    if
        Key < K1 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Key, C1, Acc)
            end;
        %
        Key < K2 ->
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2);
        %
        Key < K3 ->
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2);
        %
        Key < K4 ->
            Acc2 = [?ITER_PAIR(K3, V3), C3, ?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C4, Acc2);
        %
        Key < K5 ->
            Acc2 = [
                ?ITER_PAIR(K4, V4),
                C4,
                ?ITER_PAIR(K3, V3),
                C3,
                ?ITER_PAIR(K2, V2),
                C2,
                ?ITER_PAIR(K1, V1),
                C1
                | Acc
            ],
            bound_rev_iterator_recur(Key, C5, Acc2);
        %
        true ->
            Acc2 = [
                ?ITER_PAIR(K5, V5),
                C5,
                ?ITER_PAIR(K4, V4),
                C4,
                ?ITER_PAIR(K3, V3),
                C3,
                ?ITER_PAIR(K2, V2),
                C2,
                ?ITER_PAIR(K1, V1),
                C1
                | Acc
            ],
            bound_rev_iterator_recur(Key, C6, Acc2)
    end.

%% INTERNAL4

-compile({inline, bound_rev_iterator_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL4(Key, ?INTERNAL4_ARGS, Acc) ->
    if
        Key < K1 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Key, C1, Acc)
            end;
        %
        Key < K2 ->
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2);
        %
        Key < K3 ->
            % ?INTERNAL4_VALUES(V1, V2, _, _) = Values,
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2);
        %
        Key < K4 ->
            % ?INTERNAL4_VALUES(V1, V2, V3, _) = Values,
            Acc2 = [?ITER_PAIR(K3, V3), C3, ?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C4, Acc2);
        %
        true ->
            % ?INTERNAL4_VALUES_MATCH_ALL = Values,
            Acc2 = [
                ?ITER_PAIR(K4, V4),
                C4,
                ?ITER_PAIR(K3, V3),
                C3,
                ?ITER_PAIR(K2, V2),
                C2,
                ?ITER_PAIR(K1, V1),
                C1
                | Acc
            ],
            bound_rev_iterator_recur(Key, C5, Acc2)
    end.

%% INTERNAL3

-compile({inline, bound_rev_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL3(Key, ?INTERNAL3_ARGS, Acc) ->
    if
        Key < K1 ->
            case Acc of
                [?ITER_PAIR(AccNextKey, _) | _] when AccNextKey == Key ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Key, C1, Acc)
            end;
        %
        Key < K2 ->
            Acc2 = [?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C2, Acc2);
        %
        Key < K3 ->
            % ?INTERNAL3_VALUES(V1, V2, _) = Values,
            Acc2 = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C3, Acc2);
        %
        true ->
            % ?INTERNAL3_VALUES_MATCH_ALL = Values,
            Acc2 = [?ITER_PAIR(K3, V3), C3, ?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1 | Acc],
            bound_rev_iterator_recur(Key, C4, Acc2)
    end.

%% INTERNAL2

-compile({inline, bound_rev_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
bound_rev_iterator_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key < K1 ->
            Acc = [],
            bound_rev_iterator_recur(Key, C1, Acc);
        %
        Key < K2 ->
            Acc = [?ITER_PAIR(K1, V1), C1],
            bound_rev_iterator_recur(Key, C2, Acc);
        %
        true ->
            Acc = [?ITER_PAIR(K2, V2), C2, ?ITER_PAIR(K1, V1), C1],
            bound_rev_iterator_recur(Key, C3, Acc)
    end.

%% INTERNAL1

-compile({inline, bound_rev_iterator_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
bound_rev_iterator_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key > K1 ->
            Acc = [?ITER_PAIR(K1, V1), C1],
            bound_rev_iterator_recur(Key, C2, Acc);
        %
        Key < K1 ->
            Acc = [],
            bound_rev_iterator_recur(Key, C1, Acc);
        %
        true ->
            _Acc = [?ITER_PAIR(K1, V1), C1]
    end.

%% LEAF6

-compile({inline, bound_rev_iterator_LEAF6 / ?LEAF6_ARITY_PLUS2}).
bound_rev_iterator_LEAF6(Key, ?LEAF6_ARGS, Acc) ->
    if
        Key < K1 ->
            Acc;
        %
        Key < K2 ->
            [?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K3 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K4 ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K5 ->
            [?ITER_PAIR(K4, V4), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K6 ->
            [
                ?ITER_PAIR(K5, V5),
                ?ITER_PAIR(K4, V4),
                ?ITER_PAIR(K3, V3),
                ?ITER_PAIR(K2, V2),
                ?ITER_PAIR(K1, V1)
                | Acc
            ];
        %
        true ->
            [
                ?ITER_PAIR(K6, V6),
                ?ITER_PAIR(K5, V5),
                ?ITER_PAIR(K4, V4),
                ?ITER_PAIR(K3, V3),
                ?ITER_PAIR(K2, V2),
                ?ITER_PAIR(K1, V1)
                | Acc
            ]
    end.

%% LEAF5

-compile({inline, bound_rev_iterator_LEAF5 / ?LEAF5_ARITY_PLUS2}).
bound_rev_iterator_LEAF5(Key, ?LEAF5_ARGS, Acc) ->
    if
        Key < K1 ->
            Acc;
        %
        Key < K2 ->
            [?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K3 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K4 ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K5 ->
            [?ITER_PAIR(K4, V4), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        true ->
            [
                ?ITER_PAIR(K5, V5),
                ?ITER_PAIR(K4, V4),
                ?ITER_PAIR(K3, V3),
                ?ITER_PAIR(K2, V2),
                ?ITER_PAIR(K1, V1)
                | Acc
            ]
    end.

%% LEAF4

-compile({inline, bound_rev_iterator_LEAF4 / ?LEAF4_ARITY_PLUS2}).
bound_rev_iterator_LEAF4(Key, ?LEAF4_ARGS, Acc) ->
    if
        Key < K1 ->
            Acc;
        %
        Key < K2 ->
            [?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K3 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K4 ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        true ->
            [?ITER_PAIR(K4, V4), ?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc]
    end.

%% LEAF3

-compile({inline, bound_rev_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_rev_iterator_LEAF3(Key, ?LEAF3_ARGS, Acc) ->
    if
        Key < K1 ->
            Acc;
        %
        Key < K2 ->
            [?ITER_PAIR(K1, V1) | Acc];
        %
        Key < K3 ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc];
        %
        true ->
            [?ITER_PAIR(K3, V3), ?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1) | Acc]
    end.

%% LEAF2

-compile({inline, bound_rev_iterator_LEAF2 / ?LEAF2_ARITY_PLUS1}).
bound_rev_iterator_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key < K1 ->
            [];
        %
        Key < K2 ->
            [?ITER_PAIR(K1, V1)];
        %
        true ->
            [?ITER_PAIR(K2, V2), ?ITER_PAIR(K1, V1)]
    end.

%% LEAF1

-compile({inline, bound_rev_iterator_LEAF1 / ?LEAF1_ARITY_PLUS1}).
bound_rev_iterator_LEAF1(Key, ?LEAF1_ARGS) ->
    if
        Key < K1 ->
            [];
        %
        true ->
            [?ITER_PAIR(K1, V1)]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: keys/1
%% ------------------------------------------------------------------

keys_recur(Node, Acc) ->
    case Node of
        ?LEAF3_MATCH(K1, K2, K3, _, _, _) ->
            [K1, K2, K3 | Acc];
        %
        ?LEAF4_MATCH(K1, K2, K3, K4, _, _, _, _) ->
            [K1, K2, K3, K4 | Acc];
        %
        ?LEAF5_MATCH(K1, K2, K3, K4, K5, _, _, _, _, _) ->
            [K1, K2, K3, K4, K5 | Acc];
        %
        ?LEAF6_MATCH(K1, K2, K3, K4, K5, K6, _, _, _, _, _, _) ->
            [K1, K2, K3, K4, K5, K6 | Acc];
        %
        ?INTERNAL3_MATCH(K1, K2, K3, _, _, _, C1, C2, C3, C4) ->
            Acc2 = [K3 | keys_recur(C4, Acc)],
            Acc3 = [K2 | keys_recur(C3, Acc2)],
            Acc4 = [K1 | keys_recur(C2, Acc3)],
            keys_recur(C1, Acc4);
        %
        ?INTERNAL4_MATCH(K1, K2, K3, K4, _, _, _, _, C1, C2, C3, C4, C5) ->
            Acc2 = [K4 | keys_recur(C5, Acc)],
            Acc3 = [K3 | keys_recur(C4, Acc2)],
            Acc4 = [K2 | keys_recur(C3, Acc3)],
            Acc5 = [K1 | keys_recur(C2, Acc4)],
            keys_recur(C1, Acc5);
        %
        ?INTERNAL5_MATCH(K1, K2, K3, K4, K5, _, _, _, _, _, C1, C2, C3, C4, C5, C6) ->
            Acc2 = [K5 | keys_recur(C6, Acc)],
            Acc3 = [K4 | keys_recur(C5, Acc2)],
            Acc4 = [K3 | keys_recur(C4, Acc3)],
            Acc5 = [K2 | keys_recur(C3, Acc4)],
            Acc6 = [K1 | keys_recur(C2, Acc5)],
            keys_recur(C1, Acc6);
        %
        ?INTERNAL6_MATCH(K1, K2, K3, K4, K5, K6, _, _, _, _, _, _, C1, C2, C3, C4, C5, C6, C7) ->
            Acc2 = [K6 | keys_recur(C7, Acc)],
            Acc3 = [K5 | keys_recur(C6, Acc2)],
            Acc4 = [K4 | keys_recur(C5, Acc3)],
            Acc5 = [K3 | keys_recur(C4, Acc4)],
            Acc6 = [K2 | keys_recur(C3, Acc5)],
            Acc7 = [K1 | keys_recur(C2, Acc6)],
            keys_recur(C1, Acc7)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: larger/2
%% ------------------------------------------------------------------

larger_recur(Key, Node) ->
    case Node of
        ?INTERNAL3_MATCH_ALL ->
            larger_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            larger_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?INTERNAL5_MATCH_ALL ->
            larger_INTERNAL5(Key, ?INTERNAL5_ARGS);
        %
        ?INTERNAL6_MATCH_ALL ->
            larger_INTERNAL6(Key, ?INTERNAL6_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            larger_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            larger_LEAF4(Key, ?LEAF4_ARGS);
        %
        ?LEAF5_MATCH_ALL ->
            larger_LEAF5(Key, ?LEAF5_ARGS);
        %
        ?LEAF6_MATCH_ALL ->
            larger_LEAF6(Key, ?LEAF6_ARGS)
    end.

%%
%% ?INTERNAL6
%%

-compile({inline, larger_INTERNAL6 / ?INTERNAL6_ARITY_PLUS1}).
larger_INTERNAL6(Key, ?INTERNAL6_ARGS) ->
    if
        Key < K3 ->
            case Key < K2 of
                true ->
                    case Key < K1 of
                        true ->
                            case larger_recur(Key, C1) of
                                none -> {K1, V1};
                                Found -> Found
                            end;
                        _ ->
                            case larger_recur(Key, C2) of
                                none -> {K2, V2};
                                Found -> Found
                            end
                    end;
                %
                _ ->
                    case larger_recur(Key, C3) of
                        none -> {K3, V3};
                        Found -> Found
                    end
            end;
        %
        Key < K4 ->
            case larger_recur(Key, C4) of
                none -> {K4, V4};
                Found -> Found
            end;
        %
        Key < K5 ->
            case larger_recur(Key, C5) of
                none -> {K5, V5};
                Found -> Found
            end;
        %
        Key < K6 ->
            case larger_recur(Key, C6) of
                none -> {K6, V6};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C7)
    end.

%%
%% ?INTERNAL5
%%

-compile({inline, larger_INTERNAL5 / ?INTERNAL5_ARITY_PLUS1}).
larger_INTERNAL5(Key, ?INTERNAL5_ARGS) ->
    if
        Key < K3 ->
            case Key < K2 of
                true ->
                    case Key < K1 of
                        true ->
                            case larger_recur(Key, C1) of
                                none -> {K1, V1};
                                Found -> Found
                            end;
                        _ ->
                            case larger_recur(Key, C2) of
                                none -> {K2, V2};
                                Found -> Found
                            end
                    end;
                %
                _ ->
                    case larger_recur(Key, C3) of
                        none -> {K3, V3};
                        Found -> Found
                    end
            end;
        %
        Key < K4 ->
            case larger_recur(Key, C4) of
                none -> {K4, V4};
                Found -> Found
            end;
        %
        Key < K5 ->
            case larger_recur(Key, C5) of
                none -> {K5, V5};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C6)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, larger_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
larger_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key < K2 ->
            case Key < K1 of
                true ->
                    case larger_recur(Key, C1) of
                        none -> {K1, V1};
                        Found -> Found
                    end;
                _ ->
                    case larger_recur(Key, C2) of
                        none -> {K2, V2};
                        Found -> Found
                    end
            end;
        %
        Key < K3 ->
            case larger_recur(Key, C3) of
                none -> {K3, V3};
                Found -> Found
            end;
        %
        Key < K4 ->
            case larger_recur(Key, C4) of
                none -> {K4, V4};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C5)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, larger_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
larger_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            case Key < K1 of
                true ->
                    case larger_recur(Key, C1) of
                        none -> {K1, V1};
                        Found -> Found
                    end;
                _ ->
                    case larger_recur(Key, C2) of
                        none -> {K2, V2};
                        Found -> Found
                    end
            end;
        %
        Key < K3 ->
            case larger_recur(Key, C3) of
                none -> {K3, V3};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C4)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, larger_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
larger_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key < K1 ->
            case larger_recur(Key, C1) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        Key < K2 ->
            case larger_recur(Key, C2) of
                none -> {K2, V2};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C3)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, larger_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
larger_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            case larger_recur(Key, C1) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Key, C2)
    end.

%%
%% ?LEAF6
%%

-compile({inline, larger_LEAF6 / ?LEAF6_ARITY_PLUS1}).
larger_LEAF6(Key, ?LEAF6_ARGS) ->
    if
        Key < K3 ->
            case Key < K2 of
                true ->
                    case Key < K1 of
                        true ->
                            {K1, V1};
                        _ ->
                            {K2, V2}
                    end;
                %
                _ ->
                    {K3, V3}
            end;
        %
        Key < K4 ->
            {K4, V4};
        %
        Key < K5 ->
            {K5, V5};
        %
        Key < K6 ->
            {K6, V6};
        %
        true ->
            none
    end.

%%
%% ?LEAF5
%%

-compile({inline, larger_LEAF5 / ?LEAF5_ARITY_PLUS1}).
larger_LEAF5(Key, ?LEAF5_ARGS) ->
    if
        Key < K3 ->
            case Key < K2 of
                true ->
                    case Key < K1 of
                        true ->
                            {K1, V1};
                        _ ->
                            {K2, V2}
                    end;
                %
                _ ->
                    {K3, V3}
            end;
        %
        Key < K4 ->
            {K4, V4};
        %
        Key < K5 ->
            {K5, V5};
        %
        true ->
            none
    end.

%%
%% ?LEAF4
%%

-compile({inline, larger_LEAF4 / ?LEAF4_ARITY_PLUS1}).
larger_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key < K2 ->
            case Key < K1 of
                true ->
                    {K1, V1};
                _ ->
                    {K2, V2}
            end;
        %
        Key < K3 ->
            {K3, V3};
        %
        Key < K4 ->
            {K4, V4};
        %
        true ->
            none
    end.

%%
%% ?LEAF3
%%

-compile({inline, larger_LEAF3 / ?LEAF3_ARITY_PLUS1}).
larger_LEAF3(Key, ?LEAF3_ARGS) ->
    if
        Key < K2 ->
            case Key < K1 of
                true ->
                    {K1, V1};
                _ ->
                    {K2, V2}
            end;
        %
        Key < K3 ->
            {K3, V3};
        %
        true ->
            none
    end.

%%
%% ?LEAF2
%%

-compile({inline, larger_LEAF2 / ?LEAF2_ARITY_PLUS1}).
larger_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key < K1 ->
            {K1, V1};
        %
        Key < K2 ->
            {K2, V2};
        %
        true ->
            none
    end.

%%
%% ?LEAF1
%%

-compile({inline, larger_LEAF1 / ?LEAF1_ARITY_PLUS1}).
larger_LEAF1(Key, ?LEAF1_ARGS) ->
    case Key < K1 of
        true ->
            {K1, V1};
        %
        _ ->
            none
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: largest/1
%% ------------------------------------------------------------------

largest_recur(Node) ->
    case Node of
        ?INTERNAL3_MATCH(_, _, _, _, _, _, _, _, _, C4) ->
            largest_recur(C4);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, _, _, _, _, C5) ->
            largest_recur(C5);
        %
        ?INTERNAL5_MATCH(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, C6) ->
            largest_recur(C6);
        %
        ?INTERNAL6_MATCH(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, C7) ->
            largest_recur(C7);
        %
        ?LEAF3_MATCH(_, _, K3, _, _, V3) ->
            {K3, V3};
        %
        ?LEAF4_MATCH(_, _, _, K4, _, _, _, V4) ->
            {K4, V4};
        %
        ?LEAF5_MATCH(_, _, _, _, K5, _, _, _, _, V5) ->
            {K5, V5};
        %
        ?LEAF6_MATCH(_, _, _, _, _, K6, _, _, _, _, _, V6) ->
            {K6, V6}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: map/2
%% ------------------------------------------------------------------

map_recur(Fun, Node) ->
    case Node of
        ?LEAF3_MATCH_ALL ->
            ?new_LEAF3(
                K1,
                K2,
                K3,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3)
            );
        %
        %
        ?LEAF4_MATCH_ALL ->
            ?new_LEAF4(
                K1,
                K2,
                K3,
                K4,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                Fun(K4, V4)
            );
        %
        %
        ?LEAF5_MATCH_ALL ->
            ?new_LEAF5(
                K1,
                K2,
                K3,
                K4,
                K5,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                Fun(K4, V4),
                Fun(K5, V5)
            );
        %
        %
        ?LEAF6_MATCH_ALL ->
            ?new_LEAF6(
                K1,
                K2,
                K3,
                K4,
                K5,
                K6,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                Fun(K4, V4),
                Fun(K5, V5),
                Fun(K6, V6)
            );
        %
        %
        ?INTERNAL3_MATCH_ALL ->
            ?new_INTERNAL3(
                K1,
                K2,
                K3,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                %
                map_recur(Fun, C1),
                map_recur(Fun, C2),
                map_recur(Fun, C3),
                map_recur(Fun, C4)
            );
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            ?new_INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                Fun(K4, V4),
                %
                map_recur(Fun, C1),
                map_recur(Fun, C2),
                map_recur(Fun, C3),
                map_recur(Fun, C4),
                map_recur(Fun, C5)
            );
        %
        %
        ?INTERNAL5_MATCH_ALL ->
            ?new_INTERNAL5(
                K1,
                K2,
                K3,
                K4,
                K5,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                Fun(K4, V4),
                Fun(K5, V5),
                %
                map_recur(Fun, C1),
                map_recur(Fun, C2),
                map_recur(Fun, C3),
                map_recur(Fun, C4),
                map_recur(Fun, C5),
                map_recur(Fun, C6)
            );
        %
        %
        ?INTERNAL6_MATCH_ALL ->
            ?new_INTERNAL6(
                K1,
                K2,
                K3,
                K4,
                K5,
                K6,
                %
                Fun(K1, V1),
                Fun(K2, V2),
                Fun(K3, V3),
                Fun(K4, V4),
                Fun(K5, V5),
                Fun(K6, V6),
                %
                map_recur(Fun, C1),
                map_recur(Fun, C2),
                map_recur(Fun, C3),
                map_recur(Fun, C4),
                map_recur(Fun, C5),
                map_recur(Fun, C6),
                map_recur(Fun, C7)
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: next/1
%% ------------------------------------------------------------------

next(?REV_ITER_TAG, Tail) ->
    rev_next(Tail);
next(Head, Tail) ->
    fwd_next(Head, Tail).

fwd_next(Head, Tail) ->
    case Head of
        ?ITER_PAIR(Key, Value) ->
            Iter2 = Tail,
            {Key, Value, Iter2};
        %
        Node ->
            [?ITER_PAIR(Key, Value) | NewTail] = fwd_iterator_recur(Node, Tail),
            Iter2 = NewTail,
            {Key, Value, Iter2}
    end.

rev_next([Head | Tail]) ->
    rev_next(Head, Tail);
rev_next([]) ->
    none.

rev_next(Head, Tail) ->
    case Head of
        ?ITER_PAIR(Key, Value) ->
            Iter2 = [?REV_ITER_TAG | Tail],
            {Key, Value, Iter2};
        %
        Node ->
            [?ITER_PAIR(Key, Value) | NewTail] = rev_iterator_recur(Node, Tail),
            Iter2 = [?REV_ITER_TAG | NewTail],
            {Key, Value, Iter2}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: smaller/2
%% ------------------------------------------------------------------

smaller_recur(Key, Node) ->
    case Node of
        ?INTERNAL3_MATCH_ALL ->
            smaller_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            smaller_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?INTERNAL5_MATCH_ALL ->
            smaller_INTERNAL5(Key, ?INTERNAL5_ARGS);
        %
        ?INTERNAL6_MATCH_ALL ->
            smaller_INTERNAL6(Key, ?INTERNAL6_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            smaller_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            smaller_LEAF4(Key, ?LEAF4_ARGS);
        %
        ?LEAF5_MATCH_ALL ->
            smaller_LEAF5(Key, ?LEAF5_ARGS);
        %
        ?LEAF6_MATCH_ALL ->
            smaller_LEAF6(Key, ?LEAF6_ARGS)
    end.

%%
%% ?INTERNAL6
%%

-compile({inline, smaller_INTERNAL6 / ?INTERNAL6_ARITY_PLUS1}).
smaller_INTERNAL6(Key, ?INTERNAL6_ARGS) ->
    if
        Key > K4 ->
            case Key > K5 of
                true ->
                    case Key > K6 of
                        true ->
                            case smaller_recur(Key, C7) of
                                none -> {K6, V6};
                                Found -> Found
                            end;
                        _ ->
                            case smaller_recur(Key, C6) of
                                none -> {K5, V5};
                                Found -> Found
                            end
                    end;
                %
                _ ->
                    case smaller_recur(Key, C5) of
                        none -> {K4, V4};
                        Found -> Found
                    end
            end;
        %
        Key > K3 ->
            case smaller_recur(Key, C4) of
                none -> {K3, V3};
                Found -> Found
            end;
        %
        Key > K2 ->
            case smaller_recur(Key, C3) of
                none -> {K2, V2};
                Found -> Found
            end;
        %
        Key > K1 ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Key, C1)
    end.

%%
%% ?INTERNAL5
%%

-compile({inline, smaller_INTERNAL5 / ?INTERNAL5_ARITY_PLUS1}).
smaller_INTERNAL5(Key, ?INTERNAL5_ARGS) ->
    if
        Key > K3 ->
            case Key > K4 of
                true ->
                    case Key > K5 of
                        true ->
                            case smaller_recur(Key, C6) of
                                none -> {K5, V5};
                                Found -> Found
                            end;
                        %
                        _ ->
                            case smaller_recur(Key, C5) of
                                none -> {K4, V4};
                                Found -> Found
                            end
                    end;
                %
                _ ->
                    case smaller_recur(Key, C4) of
                        none -> {K3, V3};
                        Found -> Found
                    end
            end;
        %
        Key > K2 ->
            case smaller_recur(Key, C3) of
                none -> {K2, V2};
                Found -> Found
            end;
        %
        Key > K1 ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Key, C1)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, smaller_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
smaller_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K3 ->
            case Key > K4 of
                true ->
                    case smaller_recur(Key, C5) of
                        none -> {K4, V4};
                        Found -> Found
                    end;
                _ ->
                    case smaller_recur(Key, C4) of
                        none -> {K3, V3};
                        Found -> Found
                    end
            end;
        %
        Key > K2 ->
            case smaller_recur(Key, C3) of
                none -> {K2, V2};
                Found -> Found
            end;
        %
        Key > K1 ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Key, C1)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, smaller_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
smaller_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key > K2 ->
            case Key > K3 of
                true ->
                    case smaller_recur(Key, C4) of
                        none -> {K3, V3};
                        Found -> Found
                    end;
                _ ->
                    case smaller_recur(Key, C3) of
                        none -> {K2, V2};
                        Found -> Found
                    end
            end;
        %
        Key > K1 ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Key, C1)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, smaller_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
smaller_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K2 ->
            case smaller_recur(Key, C3) of
                none -> {K2, V2};
                Found -> Found
            end;
        %
        Key > K1 ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Key, C1)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, smaller_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
smaller_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    case Key > K1 of
        true ->
            case smaller_recur(Key, C2) of
                none -> {K1, V1};
                Found -> Found
            end;
        %
        _ ->
            smaller_recur(Key, C1)
    end.

%%
%% ?LEAF6
%%

-compile({inline, smaller_LEAF6 / ?LEAF6_ARITY_PLUS1}).
smaller_LEAF6(Key, ?LEAF6_ARGS) ->
    if
        Key > K4 ->
            case Key > K5 of
                true ->
                    case Key > K6 of
                        true ->
                            {K6, V6};
                        _ ->
                            {K5, V5}
                    end;
                %
                _ ->
                    {K4, V4}
            end;
        %
        Key > K3 ->
            {K3, V3};
        %
        Key > K2 ->
            {K2, V2};
        %
        Key > K1 ->
            {K1, V1};
        %
        true ->
            none
    end.

%%
%% ?LEAF5
%%

-compile({inline, smaller_LEAF5 / ?LEAF5_ARITY_PLUS1}).
smaller_LEAF5(Key, ?LEAF5_ARGS) ->
    if
        Key > K3 ->
            case Key > K4 of
                true ->
                    case Key > K5 of
                        true ->
                            {K5, V5};
                        %
                        _ ->
                            {K4, V4}
                    end;
                %
                _ ->
                    {K3, V3}
            end;
        %
        Key > K2 ->
            {K2, V2};
        %
        Key > K1 ->
            {K1, V1};
        %
        true ->
            none
    end.

%%
%% ?LEAF4
%%

-compile({inline, smaller_LEAF4 / ?LEAF4_ARITY_PLUS1}).
smaller_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key > K3 ->
            case Key > K4 of
                true ->
                    {K4, V4};
                _ ->
                    {K3, V3}
            end;
        %
        Key > K2 ->
            {K2, V2};
        %
        Key > K1 ->
            {K1, V1};
        %
        true ->
            none
    end.

%%
%% ?LEAF3
%%

-compile({inline, smaller_LEAF3 / ?LEAF3_ARITY_PLUS1}).
smaller_LEAF3(Key, ?LEAF3_ARGS) ->
    if
        Key > K2 ->
            case Key > K3 of
                true ->
                    {K3, V3};
                _ ->
                    {K2, V2}
            end;
        %
        Key > K1 ->
            {K1, V1};
        %
        true ->
            none
    end.

%%
%% ?LEAF2
%%

-compile({inline, smaller_LEAF2 / ?LEAF2_ARITY_PLUS1}).
smaller_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key > K2 ->
            {K2, V2};
        %
        Key > K1 ->
            {K1, V1};
        %
        true ->
            none
    end.

%%
%% ?LEAF1
%%

-compile({inline, smaller_LEAF1 / ?LEAF1_ARITY_PLUS1}).
smaller_LEAF1(Key, ?LEAF1_ARGS) ->
    case Key > K1 of
        true ->
            {K1, V1};
        %
        _ ->
            none
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: smallest/1
%% ------------------------------------------------------------------

smallest_recur(Node) ->
    case Node of
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, C1, _, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, C1, _, _, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL5_MATCH(_, _, _, _, _, _, _, _, _, _, C1, _, _, _, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL6_MATCH(_, _, _, _, _, _, _, _, _, _, _, _, C1, _, _, _, _, _, _) ->
            smallest_recur(C1);
        %
        ?LEAF3_MATCH(K1, _, _, V1, _, _) ->
            {K1, V1};
        %
        ?LEAF4_MATCH(K1, _, _, _, V1, _, _, _) ->
            {K1, V1};
        %
        ?LEAF5_MATCH(K1, _, _, _, _, V1, _, _, _, _) ->
            {K1, V1};
        %
        ?LEAF6_MATCH(K1, _, _, _, _, _, V1, _, _, _, _, _) ->
            {K1, V1}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: structural_stats/1
%% ------------------------------------------------------------------

structural_stats_recur(Node, Acc, Height) ->
    case Node of
        %
        ?LEAF3_MATCH(_, _, _, _, _, _) ->
            Acc2 = structural_stats_inc(#stats_acc.count_leaf3, Acc),
            structural_stats_set_height(Height, Acc2);
        %
        ?LEAF4_MATCH(_, _, _, _, _, _, _, _) ->
            Acc2 = structural_stats_inc(#stats_acc.count_leaf4, Acc),
            structural_stats_set_height(Height, Acc2);
        %
        ?LEAF5_MATCH(_, _, _, _, _, _, _, _, _, _) ->
            Acc2 = structural_stats_inc(#stats_acc.count_leaf5, Acc),
            structural_stats_set_height(Height, Acc2);
        %
        ?LEAF6_MATCH(_, _, _, _, _, _, _, _, _, _, _, _) ->
            Acc2 = structural_stats_inc(#stats_acc.count_leaf6, Acc),
            structural_stats_set_height(Height, Acc2);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, C1, C2, C3, C4) ->
            Acc2 = structural_stats_inc(#stats_acc.count_internal3, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            _Acc6 = structural_stats_recur(C4, Acc5, Height + 1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, C1, C2, C3, C4, C5) ->
            Acc2 = structural_stats_inc(#stats_acc.count_internal4, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            Acc6 = structural_stats_recur(C4, Acc5, Height + 1),
            _Acc7 = structural_stats_recur(C5, Acc6, Height + 1);
        %
        ?INTERNAL5_MATCH(_, _, _, _, _, _, _, _, _, _, C1, C2, C3, C4, C5, C6) ->
            Acc2 = structural_stats_inc(#stats_acc.count_internal5, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            Acc6 = structural_stats_recur(C4, Acc5, Height + 1),
            Acc7 = structural_stats_recur(C5, Acc6, Height + 1),
            _Acc8 = structural_stats_recur(C6, Acc7, Height + 1);
        %
        ?INTERNAL6_MATCH(_, _, _, _, _, _, _, _, _, _, _, _, C1, C2, C3, C4, C5, C6, C7) ->
            Acc2 = structural_stats_inc(#stats_acc.count_internal6, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            Acc6 = structural_stats_recur(C4, Acc5, Height + 1),
            Acc7 = structural_stats_recur(C5, Acc6, Height + 1),
            Acc8 = structural_stats_recur(C6, Acc7, Height + 1),
            _Acc9 = structural_stats_recur(C7, Acc8, Height + 1)
    end.

structural_stats_set_height(Height, #stats_acc{height = RecordHeight} = Acc) ->
    case RecordHeight of
        _ when RecordHeight < Height ->
            Acc#stats_acc{height = Height};
        %
        _ when RecordHeight =:= Height ->
            Acc
    end.

-compile({inline, structural_stats_inc/2}).
structural_stats_inc(Pos, #stats_acc{} = Acc) ->
    setelement(Pos, Acc, element(Pos, Acc) + 1).

return_structural_stats(#stats_acc{} = Acc) ->
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

node_counts(#stats_acc{} = Acc) ->
    [
        {internal6, Acc#stats_acc.count_internal6},
        {internal5, Acc#stats_acc.count_internal5},
        {internal4, Acc#stats_acc.count_internal4},
        {internal3, Acc#stats_acc.count_internal3},
        {internal2, Acc#stats_acc.count_internal2},
        {internal1, Acc#stats_acc.count_internal1},
        {leaf6, Acc#stats_acc.count_leaf6},
        {leaf5, Acc#stats_acc.count_leaf5},
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

total_keys_in_node_type(internal6) -> 6;
total_keys_in_node_type(internal5) -> 5;
total_keys_in_node_type(internal4) -> 4;
total_keys_in_node_type(internal3) -> 3;
total_keys_in_node_type(internal2) -> 2;
total_keys_in_node_type(internal1) -> 1;
total_keys_in_node_type(leaf6) -> 6;
total_keys_in_node_type(leaf5) -> 5;
total_keys_in_node_type(leaf4) -> 4;
total_keys_in_node_type(leaf3) -> 3;
total_keys_in_node_type(leaf2) -> 2;
total_keys_in_node_type(leaf1) -> 1.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take/2
%% ------------------------------------------------------------------

take_recur(Key, Node) ->
    case Node of
        ?INTERNAL3_MATCH_ALL ->
            take_INTERNAL3(Key, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_INTERNAL4(Key, ?INTERNAL4_ARGS);
        %
        ?INTERNAL5_MATCH_ALL ->
            take_INTERNAL5(Key, ?INTERNAL5_ARGS);
        %
        ?INTERNAL6_MATCH_ALL ->
            take_INTERNAL6(Key, ?INTERNAL6_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            take_LEAF3(Key, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            take_LEAF4(Key, ?LEAF4_ARGS);
        %
        ?LEAF5_MATCH_ALL ->
            take_LEAF5(Key, ?LEAF5_ARGS);
        %
        ?LEAF6_MATCH_ALL ->
            take_LEAF6(Key, ?LEAF6_ARGS)
    end.

%%
%% ?INTERNAL6
%%

-compile({inline, take_INTERNAL6 / ?INTERNAL6_ARITY_PLUS1}).
take_INTERNAL6(Key, ?INTERNAL6_ARGS) ->
    if
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            take_INTERNAL6_C5(Key, ?INTERNAL6_ARGS);
                        %
                        Key < K4 ->
                            take_INTERNAL6_C4(Key, ?INTERNAL6_ARGS);
                        %
                        true ->
                            take_INTERNAL6_K4(?INTERNAL6_ARGS)
                    end;
                %
                Key > K5 ->
                    %
                    if
                        Key < K6 ->
                            take_INTERNAL6_C6(Key, ?INTERNAL6_ARGS);
                        %
                        Key > K6 ->
                            take_INTERNAL6_C7(Key, ?INTERNAL6_ARGS);
                        %
                        true ->
                            take_INTERNAL6_K6(?INTERNAL6_ARGS)
                    end;
                %
                true ->
                    take_INTERNAL6_K5(?INTERNAL6_ARGS)
            end;
        %
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            take_INTERNAL6_C2(Key, ?INTERNAL6_ARGS);
                        %
                        Key > K2 ->
                            take_INTERNAL6_C3(Key, ?INTERNAL6_ARGS);
                        %
                        true ->
                            take_INTERNAL6_K2(?INTERNAL6_ARGS)
                    end;
                %
                Key < K1 ->
                    take_INTERNAL6_C1(Key, ?INTERNAL6_ARGS);
                %
                true ->
                    take_INTERNAL6_K1(?INTERNAL6_ARGS)
            end;
        %
        true ->
            take_INTERNAL6_K3(?INTERNAL6_ARGS)
    end.

-compile({inline, take_INTERNAL6_C1 / ?INTERNAL6_ARITY_PLUS1}).
take_INTERNAL6_C1(Key, ?INTERNAL6_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),

    ?TAKEN(Pair, ?INTERNAL6_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL6_C2 / ?INTERNAL6_ARITY_PLUS1}).
take_INTERNAL6_C2(Key, ?INTERNAL6_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),

    ?TAKEN(Pair, ?INTERNAL6_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL6_C3 / ?INTERNAL6_ARITY_PLUS1}).
take_INTERNAL6_C3(Key, ?INTERNAL6_ARGS) ->
    ?TAKEN(Pair, UpdatedC3) = take_recur(Key, C3),

    ?TAKEN(Pair, ?INTERNAL6_C3_REBALANCE(UpdatedC3)).

-compile({inline, take_INTERNAL6_C4 / ?INTERNAL6_ARITY_PLUS1}).
take_INTERNAL6_C4(Key, ?INTERNAL6_ARGS) ->
    ?TAKEN(Pair, UpdatedC4) = take_recur(Key, C4),

    ?TAKEN(Pair, ?INTERNAL6_C4_REBALANCE(UpdatedC4)).

-compile({inline, take_INTERNAL6_C5 / ?INTERNAL6_ARITY_PLUS1}).
take_INTERNAL6_C5(Key, ?INTERNAL6_ARGS) ->
    ?TAKEN(Pair, UpdatedC5) = take_recur(Key, C5),

    ?TAKEN(Pair, ?INTERNAL6_C5_REBALANCE(UpdatedC5)).

-compile({inline, take_INTERNAL6_C6 / ?INTERNAL6_ARITY_PLUS1}).
take_INTERNAL6_C6(Key, ?INTERNAL6_ARGS) ->
    ?TAKEN(Pair, UpdatedC6) = take_recur(Key, C6),

    ?TAKEN(Pair, ?INTERNAL6_C6_REBALANCE(UpdatedC6)).

-compile({inline, take_INTERNAL6_C7 / ?INTERNAL6_ARITY_PLUS1}).
take_INTERNAL6_C7(Key, ?INTERNAL6_ARGS) ->
    ?TAKEN(Pair, UpdatedC7) = take_recur(Key, C7),

    ?TAKEN(Pair, ?INTERNAL6_C7_REBALANCE(UpdatedC7)).

%%

-compile({inline, take_INTERNAL6_K1 / ?INTERNAL6_ARITY}).
take_INTERNAL6_K1(?INTERNAL6_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_INTERNAL6_K1(?INTERNAL6_ARGS)).

-compile({inline, take_INTERNAL6_K2 / ?INTERNAL6_ARITY}).
take_INTERNAL6_K2(?INTERNAL6_ARGS) ->
    ?TAKEN_PAIR(K2, V2, delete_INTERNAL6_K2(?INTERNAL6_ARGS)).

-compile({inline, take_INTERNAL6_K3 / ?INTERNAL6_ARITY}).
take_INTERNAL6_K3(?INTERNAL6_ARGS) ->
    ?TAKEN_PAIR(K3, V3, delete_INTERNAL6_K3(?INTERNAL6_ARGS)).

-compile({inline, take_INTERNAL6_K4 / ?INTERNAL6_ARITY}).
take_INTERNAL6_K4(?INTERNAL6_ARGS) ->
    ?TAKEN_PAIR(K4, V4, delete_INTERNAL6_K4(?INTERNAL6_ARGS)).

-compile({inline, take_INTERNAL6_K5 / ?INTERNAL6_ARITY}).
take_INTERNAL6_K5(?INTERNAL6_ARGS) ->
    ?TAKEN_PAIR(K5, V5, delete_INTERNAL6_K5(?INTERNAL6_ARGS)).

-compile({inline, take_INTERNAL6_K6 / ?INTERNAL6_ARITY}).
take_INTERNAL6_K6(?INTERNAL6_ARGS) ->
    ?TAKEN_PAIR(K6, V6, delete_INTERNAL6_K6(?INTERNAL6_ARGS)).

%%
%% ?INTERNAL5
%%

-compile({inline, take_INTERNAL5 / ?INTERNAL5_ARITY_PLUS1}).
take_INTERNAL5(Key, ?INTERNAL5_ARGS) ->
    if
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            take_INTERNAL5_C2(Key, ?INTERNAL5_ARGS);
                        %
                        Key > K2 ->
                            take_INTERNAL5_C3(Key, ?INTERNAL5_ARGS);
                        %
                        true ->
                            take_INTERNAL5_K2(?INTERNAL5_ARGS)
                    end;
                %
                Key < K1 ->
                    take_INTERNAL5_C1(Key, ?INTERNAL5_ARGS);
                %
                true ->
                    take_INTERNAL5_K1(?INTERNAL5_ARGS)
            end;
        %
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            take_INTERNAL5_C5(Key, ?INTERNAL5_ARGS);
                        %
                        Key < K4 ->
                            take_INTERNAL5_C4(Key, ?INTERNAL5_ARGS);
                        %
                        true ->
                            take_INTERNAL5_K4(?INTERNAL5_ARGS)
                    end;
                %
                Key > K5 ->
                    take_INTERNAL5_C6(Key, ?INTERNAL5_ARGS);
                %
                true ->
                    take_INTERNAL5_K5(?INTERNAL5_ARGS)
            end;
        %
        true ->
            take_INTERNAL5_K3(?INTERNAL5_ARGS)
    end.

-compile({inline, take_INTERNAL5_C1 / ?INTERNAL5_ARITY_PLUS1}).
take_INTERNAL5_C1(Key, ?INTERNAL5_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),

    ?TAKEN(Pair, ?INTERNAL5_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL5_C2 / ?INTERNAL5_ARITY_PLUS1}).
take_INTERNAL5_C2(Key, ?INTERNAL5_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),

    ?TAKEN(Pair, ?INTERNAL5_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL5_C3 / ?INTERNAL5_ARITY_PLUS1}).
take_INTERNAL5_C3(Key, ?INTERNAL5_ARGS) ->
    ?TAKEN(Pair, UpdatedC3) = take_recur(Key, C3),

    ?TAKEN(Pair, ?INTERNAL5_C3_REBALANCE(UpdatedC3)).

-compile({inline, take_INTERNAL5_C4 / ?INTERNAL5_ARITY_PLUS1}).
take_INTERNAL5_C4(Key, ?INTERNAL5_ARGS) ->
    ?TAKEN(Pair, UpdatedC4) = take_recur(Key, C4),

    ?TAKEN(Pair, ?INTERNAL5_C4_REBALANCE(UpdatedC4)).

-compile({inline, take_INTERNAL5_C5 / ?INTERNAL5_ARITY_PLUS1}).
take_INTERNAL5_C5(Key, ?INTERNAL5_ARGS) ->
    ?TAKEN(Pair, UpdatedC5) = take_recur(Key, C5),

    ?TAKEN(Pair, ?INTERNAL5_C5_REBALANCE(UpdatedC5)).

-compile({inline, take_INTERNAL5_C6 / ?INTERNAL5_ARITY_PLUS1}).
take_INTERNAL5_C6(Key, ?INTERNAL5_ARGS) ->
    ?TAKEN(Pair, UpdatedC6) = take_recur(Key, C6),

    ?TAKEN(Pair, ?INTERNAL5_C6_REBALANCE(UpdatedC6)).

%%

-compile({inline, take_INTERNAL5_K1 / ?INTERNAL5_ARITY}).
take_INTERNAL5_K1(?INTERNAL5_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_INTERNAL5_K1(?INTERNAL5_ARGS)).

-compile({inline, take_INTERNAL5_K2 / ?INTERNAL5_ARITY}).
take_INTERNAL5_K2(?INTERNAL5_ARGS) ->
    ?TAKEN_PAIR(K2, V2, delete_INTERNAL5_K2(?INTERNAL5_ARGS)).

-compile({inline, take_INTERNAL5_K3 / ?INTERNAL5_ARITY}).
take_INTERNAL5_K3(?INTERNAL5_ARGS) ->
    ?TAKEN_PAIR(K3, V3, delete_INTERNAL5_K3(?INTERNAL5_ARGS)).

-compile({inline, take_INTERNAL5_K4 / ?INTERNAL5_ARITY}).
take_INTERNAL5_K4(?INTERNAL5_ARGS) ->
    ?TAKEN_PAIR(K4, V4, delete_INTERNAL5_K4(?INTERNAL5_ARGS)).

-compile({inline, take_INTERNAL5_K5 / ?INTERNAL5_ARITY}).
take_INTERNAL5_K5(?INTERNAL5_ARGS) ->
    ?TAKEN_PAIR(K5, V5, delete_INTERNAL5_K5(?INTERNAL5_ARGS)).

%%
%% ?INTERNAL4
%%

-compile({inline, take_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4(Key, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            take_INTERNAL4_C4(Key, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            take_INTERNAL4_C3(Key, ?INTERNAL4_ARGS);
                        %
                        true ->
                            take_INTERNAL4_K3(?INTERNAL4_ARGS)
                    end;
                %
                Key > K4 ->
                    take_INTERNAL4_C5(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    take_INTERNAL4_K4(?INTERNAL4_ARGS)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    take_INTERNAL4_C1(Key, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    take_INTERNAL4_C2(Key, ?INTERNAL4_ARGS);
                %
                true ->
                    take_INTERNAL4_K1(?INTERNAL4_ARGS)
            end;
        %
        true ->
            take_INTERNAL4_K2(?INTERNAL4_ARGS)
    end.

-compile({inline, take_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C1(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),

    ?TAKEN(Pair, ?INTERNAL4_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C2(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),

    ?TAKEN(Pair, ?INTERNAL4_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C3(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC3) = take_recur(Key, C3),

    ?TAKEN(Pair, ?INTERNAL4_C3_REBALANCE(UpdatedC3)).

-compile({inline, take_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C4(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC4) = take_recur(Key, C4),

    ?TAKEN(Pair, ?INTERNAL4_C4_REBALANCE(UpdatedC4)).

-compile({inline, take_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
take_INTERNAL4_C5(Key, ?INTERNAL4_ARGS) ->
    ?TAKEN(Pair, UpdatedC5) = take_recur(Key, C5),

    ?TAKEN(Pair, ?INTERNAL4_C5_REBALANCE(UpdatedC5)).

%%

-compile({inline, take_INTERNAL4_K1 / ?INTERNAL4_ARITY}).
take_INTERNAL4_K1(?INTERNAL4_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_INTERNAL4_K1(?INTERNAL4_ARGS)).

-compile({inline, take_INTERNAL4_K2 / ?INTERNAL4_ARITY}).
take_INTERNAL4_K2(?INTERNAL4_ARGS) ->
    ?TAKEN_PAIR(K2, V2, delete_INTERNAL4_K2(?INTERNAL4_ARGS)).

-compile({inline, take_INTERNAL4_K3 / ?INTERNAL4_ARITY}).
take_INTERNAL4_K3(?INTERNAL4_ARGS) ->
    ?TAKEN_PAIR(K3, V3, delete_INTERNAL4_K3(?INTERNAL4_ARGS)).

-compile({inline, take_INTERNAL4_K4 / ?INTERNAL4_ARITY}).
take_INTERNAL4_K4(?INTERNAL4_ARGS) ->
    ?TAKEN_PAIR(K4, V4, delete_INTERNAL4_K4(?INTERNAL4_ARGS)).

%%
%% ?INTERNAL3
%%

-compile({inline, take_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3(Key, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    take_INTERNAL3_C1(Key, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    take_INTERNAL3_C2(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    take_INTERNAL3_K1(?INTERNAL3_ARGS)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    take_INTERNAL3_C3(Key, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    take_INTERNAL3_C4(Key, ?INTERNAL3_ARGS);
                %
                true ->
                    take_INTERNAL3_K3(?INTERNAL3_ARGS)
            end;
        %
        true ->
            take_INTERNAL3_K2(?INTERNAL3_ARGS)
    end.

-compile({inline, take_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3_C1(Key, ?INTERNAL3_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),

    ?TAKEN(Pair, ?INTERNAL3_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3_C2(Key, ?INTERNAL3_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),

    ?TAKEN(Pair, ?INTERNAL3_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3_C3(Key, ?INTERNAL3_ARGS) ->
    ?TAKEN(Pair, UpdatedC3) = take_recur(Key, C3),

    ?TAKEN(Pair, ?INTERNAL3_C3_REBALANCE(UpdatedC3)).

-compile({inline, take_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
take_INTERNAL3_C4(Key, ?INTERNAL3_ARGS) ->
    ?TAKEN(Pair, UpdatedC4) = take_recur(Key, C4),

    ?TAKEN(Pair, ?INTERNAL3_C4_REBALANCE(UpdatedC4)).

%%

-compile({inline, take_INTERNAL3_K1 / ?INTERNAL3_ARITY}).
take_INTERNAL3_K1(?INTERNAL3_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_INTERNAL3_K1(?INTERNAL3_ARGS)).

-compile({inline, take_INTERNAL3_K2 / ?INTERNAL3_ARITY}).
take_INTERNAL3_K2(?INTERNAL3_ARGS) ->
    ?TAKEN_PAIR(K2, V2, delete_INTERNAL3_K2(?INTERNAL3_ARGS)).

-compile({inline, take_INTERNAL3_K3 / ?INTERNAL3_ARITY}).
take_INTERNAL3_K3(?INTERNAL3_ARGS) ->
    ?TAKEN_PAIR(K3, V3, delete_INTERNAL3_K3(?INTERNAL3_ARGS)).

%%
%% ?INTERNAL2
%%

-compile({inline, take_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
take_INTERNAL2(Key, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    take_INTERNAL2_C2(Key, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    take_INTERNAL2_C3(Key, ?INTERNAL2_ARGS);
                %
                true ->
                    take_INTERNAL2_K2(?INTERNAL2_ARGS)
            end;
        %
        Key < K1 ->
            take_INTERNAL2_C1(Key, ?INTERNAL2_ARGS);
        %
        true ->
            take_INTERNAL2_K1(?INTERNAL2_ARGS)
    end.

-compile({inline, take_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
take_INTERNAL2_C1(Key, ?INTERNAL2_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),

    ?TAKEN(Pair, ?INTERNAL2_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
take_INTERNAL2_C2(Key, ?INTERNAL2_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),

    ?TAKEN(Pair, ?INTERNAL2_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
take_INTERNAL2_C3(Key, ?INTERNAL2_ARGS) ->
    ?TAKEN(Pair, UpdatedC3) = take_recur(Key, C3),

    ?TAKEN(Pair, ?INTERNAL2_C3_REBALANCE(UpdatedC3)).

%%

-compile({inline, take_INTERNAL2_K1 / ?INTERNAL2_ARITY}).
take_INTERNAL2_K1(?INTERNAL2_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_INTERNAL2_K1(?INTERNAL2_ARGS)).

-compile({inline, take_INTERNAL2_K2 / ?INTERNAL2_ARITY}).
take_INTERNAL2_K2(?INTERNAL2_ARGS) ->
    ?TAKEN_PAIR(K2, V2, delete_INTERNAL2_K2(?INTERNAL2_ARGS)).

%%
%% ?INTERNAL1
%%

-compile({inline, take_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
take_INTERNAL1(Key, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            take_INTERNAL1_C1(Key, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            take_INTERNAL1_C2(Key, ?INTERNAL1_ARGS);
        %
        true ->
            take_INTERNAL1_K1(?INTERNAL1_ARGS)
    end.

-compile({inline, take_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
take_INTERNAL1_C1(Key, ?INTERNAL1_ARGS) ->
    ?TAKEN(Pair, UpdatedC1) = take_recur(Key, C1),
    ?TAKEN(Pair, ?INTERNAL1_C1_REBALANCE(UpdatedC1)).

-compile({inline, take_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
take_INTERNAL1_C2(Key, ?INTERNAL1_ARGS) ->
    ?TAKEN(Pair, UpdatedC2) = take_recur(Key, C2),
    ?TAKEN(Pair, ?INTERNAL1_C2_REBALANCE(UpdatedC2)).

-compile({inline, take_INTERNAL1_K1 / ?INTERNAL1_ARITY}).
take_INTERNAL1_K1(?INTERNAL1_ARGS) ->
    ?TAKEN_PAIR(K1, V1, delete_INTERNAL1_K1(?INTERNAL1_ARGS)).

%%
%% ?LEAF6
%%

-compile({inline, take_LEAF6 / ?LEAF6_ARITY_PLUS1}).
take_LEAF6(Key, ?LEAF6_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF5(K2, K3, K4, K5, K6, V2, V3, V4, V5, V6));
        %
        Key == K2 ->
            ?TAKEN_PAIR(K2, V2, ?new_LEAF5(K1, K3, K4, K5, K6, V1, V3, V4, V5, V6));
        %
        Key == K3 ->
            ?TAKEN_PAIR(K3, V3, ?new_LEAF5(K1, K2, K4, K5, K6, V1, V2, V4, V5, V6));
        %
        Key == K4 ->
            ?TAKEN_PAIR(K4, V4, ?new_LEAF5(K1, K2, K3, K5, K6, V1, V2, V3, V5, V6));
        %
        Key == K5 ->
            ?TAKEN_PAIR(K5, V5, ?new_LEAF5(K1, K2, K3, K4, K6, V1, V2, V3, V4, V6));
        %
        Key == K6 ->
            ?TAKEN_PAIR(K6, V6, ?new_LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5));
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF5
%%

-compile({inline, take_LEAF5 / ?LEAF5_ARITY_PLUS1}).
take_LEAF5(Key, ?LEAF5_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF4(K2, K3, K4, K5, V2, V3, V4, V5));
        %
        Key == K2 ->
            ?TAKEN_PAIR(K2, V2, ?new_LEAF4(K1, K3, K4, K5, V1, V3, V4, V5));
        %
        Key == K3 ->
            ?TAKEN_PAIR(K3, V3, ?new_LEAF4(K1, K2, K4, K5, V1, V2, V4, V5));
        %
        Key == K4 ->
            ?TAKEN_PAIR(K4, V4, ?new_LEAF4(K1, K2, K3, K5, V1, V2, V3, V5));
        %
        Key == K5 ->
            ?TAKEN_PAIR(K5, V5, ?new_LEAF4(K1, K2, K3, K4, V1, V2, V3, V4));
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF4
%%

-compile({inline, take_LEAF4 / ?LEAF4_ARITY_PLUS1}).
take_LEAF4(Key, ?LEAF4_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF3(K2, K3, K4, V2, V3, V4));
        %
        Key == K2 ->
            ?TAKEN_PAIR(K2, V2, ?new_LEAF3(K1, K3, K4, V1, V3, V4));
        %
        Key == K3 ->
            ?TAKEN_PAIR(K3, V3, ?new_LEAF3(K1, K2, K4, V1, V2, V4));
        %
        Key == K4 ->
            ?TAKEN_PAIR(K4, V4, ?new_LEAF3(K1, K2, K3, V1, V2, V3));
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF3
%%

-compile({inline, take_LEAF3 / ?LEAF3_ARITY_PLUS1}).
take_LEAF3(Key, ?LEAF3_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?NEEDS_REBALANCE(1, nil));
        %
        Key == K2 ->
            ?TAKEN_PAIR(K2, V2, ?NEEDS_REBALANCE(2, nil));
        %
        Key == K3 ->
            ?TAKEN_PAIR(K3, V3, ?NEEDS_REBALANCE(3, nil));
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF2
%%

-compile({inline, take_LEAF2 / ?LEAF2_ARITY_PLUS1}).
take_LEAF2(Key, ?LEAF2_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?new_LEAF1(K2, V2));
        %
        Key == K2 ->
            ?TAKEN_PAIR(K2, V2, ?new_LEAF1(K1, V1));
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF1
%%

-compile({inline, take_LEAF1 / ?LEAF1_ARITY_PLUS1}).
take_LEAF1(Key, ?LEAF1_ARGS) ->
    if
        Key == K1 ->
            ?TAKEN_PAIR(K1, V1, ?LEAF0);
        %
        true ->
            error_badkey(Key)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_largest/2
%% ------------------------------------------------------------------

take_largest_recur(Node) ->
    case Node of
        ?INTERNAL3_MATCH_ALL ->
            take_largest_INTERNAL3(?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_largest_INTERNAL4(?INTERNAL4_ARGS);
        %
        ?INTERNAL5_MATCH_ALL ->
            take_largest_INTERNAL5(?INTERNAL5_ARGS);
        %
        ?INTERNAL6_MATCH_ALL ->
            take_largest_INTERNAL6(?INTERNAL6_ARGS);
        %
        ?LEAF3_MATCH(_, _, K3, _, _, V3) ->
            take_largest_LEAF3(K3, V3);
        %
        ?LEAF4_MATCH_ALL ->
            take_largest_LEAF4(?LEAF4_ARGS);
        %
        ?LEAF5_MATCH_ALL ->
            take_largest_LEAF5(?LEAF5_ARGS);
        %
        ?LEAF6_MATCH_ALL ->
            take_largest_LEAF6(?LEAF6_ARGS)
    end.

%%
%% ?INTERNAL6
%%

-compile({inline, take_largest_INTERNAL6 / ?INTERNAL6_ARITY}).
take_largest_INTERNAL6(?INTERNAL6_ARGS) ->
    ?TAKEN(Taken, UpdatedC7) = take_largest_recur(C7),
    ?TAKEN(Taken, ?INTERNAL6_C7_REBALANCE(UpdatedC7)).

%%
%% ?INTERNAL5
%%

-compile({inline, take_largest_INTERNAL5 / ?INTERNAL5_ARITY}).
take_largest_INTERNAL5(?INTERNAL5_ARGS) ->
    ?TAKEN(Taken, UpdatedC6) = take_largest_recur(C6),
    ?TAKEN(Taken, ?INTERNAL5_C6_REBALANCE(UpdatedC6)).

%%
%% ?INTERNAL4
%%

-compile({inline, take_largest_INTERNAL4 / ?INTERNAL4_ARITY}).
take_largest_INTERNAL4(?INTERNAL4_ARGS) ->
    ?TAKEN(Taken, UpdatedC5) = take_largest_recur(C5),
    ?TAKEN(Taken, ?INTERNAL4_C5_REBALANCE(UpdatedC5)).

%%
%% ?INTERNAL3
%%

-compile({inline, take_largest_INTERNAL3 / ?INTERNAL3_ARITY}).
take_largest_INTERNAL3(?INTERNAL3_ARGS) ->
    ?TAKEN(Taken, UpdatedC4) = take_largest_recur(C4),
    ?TAKEN(Taken, ?INTERNAL3_C4_REBALANCE(UpdatedC4)).

%%
%% ?INTERNAL2
%%

-compile({inline, take_largest_INTERNAL2 / ?INTERNAL2_ARITY}).
take_largest_INTERNAL2(?INTERNAL2_ARGS) ->
    ?TAKEN(Taken, UpdatedC3) = take_largest_recur(C3),
    ?TAKEN(Taken, ?INTERNAL2_C3_REBALANCE(UpdatedC3)).

%%
%% ?INTERNAL1
%%

-compile({inline, take_largest_INTERNAL1 / ?INTERNAL1_ARITY}).
take_largest_INTERNAL1(?INTERNAL1_ARGS) ->
    ?TAKEN(Taken, UpdatedC2) = take_largest_recur(C2),
    ?TAKEN(Taken, ?INTERNAL1_C2_REBALANCE(UpdatedC2)).

%%
%% ?LEAF6
%%

-compile({inline, take_largest_LEAF6 / ?LEAF6_ARITY}).
take_largest_LEAF6(?LEAF6_ARGS) ->
    ?TAKEN_PAIR(K6, V6, ?new_LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5)).

%%
%% ?LEAF5
%%

-compile({inline, take_largest_LEAF5 / ?LEAF5_ARITY}).
take_largest_LEAF5(?LEAF5_ARGS) ->
    ?TAKEN_PAIR(K5, V5, ?new_LEAF4(K1, K2, K3, K4, V1, V2, V3, V4)).

%%
%% ?LEAF4
%%

-compile({inline, take_largest_LEAF4 / ?LEAF4_ARITY}).
take_largest_LEAF4(?LEAF4_ARGS) ->
    ?TAKEN_PAIR(K4, V4, ?new_LEAF3(K1, K2, K3, V1, V2, V3)).

%%
%% ?LEAF3
%%

-compile({inline, take_largest_LEAF3/2}).
take_largest_LEAF3(K3, V3) ->
    ?TAKEN_PAIR(K3, V3, ?NEEDS_REBALANCE(3, nil)).

%%
%% ?LEAF2
%%

-compile({inline, take_largest_LEAF2 / ?LEAF2_ARITY}).
take_largest_LEAF2(?LEAF2_ARGS) ->
    ?TAKEN_PAIR(K2, V2, ?new_LEAF1(K1, V1)).

%%
%% ?LEAF1
%%

-compile({inline, take_largest_LEAF1 / ?LEAF1_ARITY}).
take_largest_LEAF1(?LEAF1_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?LEAF0).

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_smallest/2
%% ------------------------------------------------------------------

take_smallest_recur(Node) ->
    case Node of
        ?INTERNAL3_MATCH_ALL ->
            take_smallest_INTERNAL3(?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_smallest_INTERNAL4(?INTERNAL4_ARGS);
        %
        ?INTERNAL5_MATCH_ALL ->
            take_smallest_INTERNAL5(?INTERNAL5_ARGS);
        %
        ?INTERNAL6_MATCH_ALL ->
            take_smallest_INTERNAL6(?INTERNAL6_ARGS);
        %
        ?LEAF3_MATCH(K1, _, _, V1, _, _) ->
            take_smallest_LEAF3(K1, V1);
        %
        ?LEAF4_MATCH_ALL ->
            take_smallest_LEAF4(?LEAF4_ARGS);
        %
        ?LEAF5_MATCH_ALL ->
            take_smallest_LEAF5(?LEAF5_ARGS);
        %
        ?LEAF6_MATCH_ALL ->
            take_smallest_LEAF6(?LEAF6_ARGS)
    end.

%%
%% ?INTERNAL6
%%

-compile({inline, take_smallest_INTERNAL6 / ?INTERNAL6_ARITY}).
take_smallest_INTERNAL6(?INTERNAL6_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL6_C1_REBALANCE(UpdatedC1)).

%%
%% ?INTERNAL5
%%

-compile({inline, take_smallest_INTERNAL5 / ?INTERNAL5_ARITY}).
take_smallest_INTERNAL5(?INTERNAL5_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL5_C1_REBALANCE(UpdatedC1)).

%%
%% ?INTERNAL4
%%

-compile({inline, take_smallest_INTERNAL4 / ?INTERNAL4_ARITY}).
take_smallest_INTERNAL4(?INTERNAL4_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL4_C1_REBALANCE(UpdatedC1)).

%%
%% ?INTERNAL3
%%

-compile({inline, take_smallest_INTERNAL3 / ?INTERNAL3_ARITY}).
take_smallest_INTERNAL3(?INTERNAL3_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL3_C1_REBALANCE(UpdatedC1)).

%%
%% ?INTERNAL2
%%

-compile({inline, take_smallest_INTERNAL2 / ?INTERNAL2_ARITY}).
take_smallest_INTERNAL2(?INTERNAL2_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL2_C1_REBALANCE(UpdatedC1)).

%%
%% ?INTERNAL1
%%

-compile({inline, take_smallest_INTERNAL1 / ?INTERNAL1_ARITY}).
take_smallest_INTERNAL1(?INTERNAL1_ARGS) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL1_C1_REBALANCE(UpdatedC1)).

%%
%% ?LEAF6
%%

-compile({inline, take_smallest_LEAF6 / ?LEAF6_ARITY}).
take_smallest_LEAF6(?LEAF6_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?new_LEAF5(K2, K3, K4, K5, K6, V2, V3, V4, V5, V6)).

%%
%% ?LEAF5
%%

-compile({inline, take_smallest_LEAF5 / ?LEAF5_ARITY}).
take_smallest_LEAF5(?LEAF5_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?new_LEAF4(K2, K3, K4, K5, V2, V3, V4, V5)).

%%
%% ?LEAF4
%%

-compile({inline, take_smallest_LEAF4 / ?LEAF4_ARITY}).
take_smallest_LEAF4(?LEAF4_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?new_LEAF3(K2, K3, K4, V2, V3, V4)).

%%
%% ?LEAF3
%%

-compile({inline, take_smallest_LEAF3/2}).
take_smallest_LEAF3(K1, V1) ->
    ?TAKEN_PAIR(K1, V1, ?NEEDS_REBALANCE(1, nil)).

%%
%% ?LEAF2
%%

-compile({inline, take_smallest_LEAF2 / ?LEAF2_ARITY}).
take_smallest_LEAF2(?LEAF2_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?new_LEAF1(K2, V2)).

%%
%% ?LEAF1
%%

-compile({inline, take_smallest_LEAF1 / ?LEAF1_ARITY}).
take_smallest_LEAF1(?LEAF1_ARGS) ->
    ?TAKEN_PAIR(K1, V1, ?LEAF0).

%% ------------------------------------------------------------------
%% Internal Function Definitions: to_list/1
%% ------------------------------------------------------------------

to_list_recur(Node, Acc) ->
    case Node of
        ?LEAF3_MATCH_ALL ->
            [{K1, V1}, {K2, V2}, {K3, V3} | Acc];
        %
        ?LEAF4_MATCH_ALL ->
            [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4} | Acc];
        %
        ?LEAF5_MATCH_ALL ->
            [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4}, {K5, V5} | Acc];
        %
        ?LEAF6_MATCH_ALL ->
            [{K1, V1}, {K2, V2}, {K3, V3}, {K4, V4}, {K5, V5}, {K6, V6} | Acc];
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = to_list_recur(C4, Acc),
            Acc3 = to_list_recur(C3, [{K3, V3} | Acc2]),
            Acc4 = to_list_recur(C2, [{K2, V2} | Acc3]),
            _Acc5 = to_list_recur(C1, [{K1, V1} | Acc4]);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = to_list_recur(C5, Acc),
            Acc3 = to_list_recur(C4, [{K4, V4} | Acc2]),
            Acc4 = to_list_recur(C3, [{K3, V3} | Acc3]),
            Acc5 = to_list_recur(C2, [{K2, V2} | Acc4]),
            _Acc6 = to_list_recur(C1, [{K1, V1} | Acc5]);
        %
        ?INTERNAL5_MATCH_ALL ->
            Acc2 = to_list_recur(C6, Acc),
            Acc3 = to_list_recur(C5, [{K5, V5} | Acc2]),
            Acc4 = to_list_recur(C4, [{K4, V4} | Acc3]),
            Acc5 = to_list_recur(C3, [{K3, V3} | Acc4]),
            Acc6 = to_list_recur(C2, [{K2, V2} | Acc5]),
            _Acc7 = to_list_recur(C1, [{K1, V1} | Acc6]);
        %
        ?INTERNAL6_MATCH_ALL ->
            Acc2 = to_list_recur(C7, Acc),
            Acc3 = to_list_recur(C6, [{K6, V6} | Acc2]),
            Acc4 = to_list_recur(C5, [{K5, V5} | Acc3]),
            Acc5 = to_list_recur(C4, [{K4, V4} | Acc4]),
            Acc6 = to_list_recur(C3, [{K3, V3} | Acc5]),
            Acc7 = to_list_recur(C2, [{K2, V2} | Acc6]),
            _Acc8 = to_list_recur(C1, [{K1, V1} | Acc7])
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: update/2
%% ------------------------------------------------------------------

update_recur(Key, ValueEval, ValueWrap, Node) ->
    case Node of
        ?INTERNAL3_MATCH_ALL ->
            update_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            update_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
        %
        ?INTERNAL5_MATCH_ALL ->
            update_INTERNAL5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
        %
        ?INTERNAL6_MATCH_ALL ->
            update_INTERNAL6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            update_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            update_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS);
        %
        ?LEAF5_MATCH_ALL ->
            update_LEAF5(Key, ValueEval, ValueWrap, ?LEAF5_ARGS);
        %
        ?LEAF6_MATCH_ALL ->
            update_LEAF6(Key, ValueEval, ValueWrap, ?LEAF6_ARGS)
    end.

%%
%% ?INTERNAL6
%%

-compile({inline, update_INTERNAL6 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    if
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            update_INTERNAL6_C5(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        Key < K4 ->
                            update_INTERNAL6_C4(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        true ->
                            update_INTERNAL6_K4(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS)
                    end;
                %
                Key > K5 ->
                    %
                    if
                        Key < K6 ->
                            update_INTERNAL6_C6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        Key > K6 ->
                            update_INTERNAL6_C7(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        true ->
                            update_INTERNAL6_K6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS)
                    end;
                %
                true ->
                    update_INTERNAL6_K5(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS)
            end;
        %
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            update_INTERNAL6_C2(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        Key > K2 ->
                            update_INTERNAL6_C3(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                        %
                        true ->
                            update_INTERNAL6_K2(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS)
                    end;
                %
                Key < K1 ->
                    update_INTERNAL6_C1(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS);
                %
                true ->
                    update_INTERNAL6_K1(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS)
            end;
        %
        true ->
            update_INTERNAL6_K3(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS)
    end.

-compile({inline, update_INTERNAL6_C1 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_C1(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL6_C1(UpdatedC1).

-compile({inline, update_INTERNAL6_C2 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_C2(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL6_C2(UpdatedC2).

-compile({inline, update_INTERNAL6_C3 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_C3(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    UpdatedC3 = update_recur(Key, ValueEval, ValueWrap, C3),
    ?INTERNAL6_C3(UpdatedC3).

-compile({inline, update_INTERNAL6_C4 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_C4(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    UpdatedC4 = update_recur(Key, ValueEval, ValueWrap, C4),
    ?INTERNAL6_C4(UpdatedC4).

-compile({inline, update_INTERNAL6_C5 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_C5(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    UpdatedC5 = update_recur(Key, ValueEval, ValueWrap, C5),
    ?INTERNAL6_C5(UpdatedC5).

-compile({inline, update_INTERNAL6_C6 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_C6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    UpdatedC6 = update_recur(Key, ValueEval, ValueWrap, C6),
    ?INTERNAL6_C6(UpdatedC6).

-compile({inline, update_INTERNAL6_C7 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_C7(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS) ->
    UpdatedC7 = update_recur(Key, ValueEval, ValueWrap, C7),
    ?INTERNAL6_C7(UpdatedC7).

%%

-compile({inline, update_INTERNAL6_K1 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_K1(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS_IGN_K1) ->
    % ?INTERNAL6_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL6(
        Key,
        K2,
        K3,
        K4,
        K5,
        K6,
        %
        Value,
        V2,
        V3,
        V4,
        V5,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, update_INTERNAL6_K2 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_K2(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS_IGN_K2) ->
    % ?INTERNAL6_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V2),

    ?new_INTERNAL6(
        K1,
        Key,
        K3,
        K4,
        K5,
        K6,
        %
        V1,
        Value,
        V3,
        V4,
        V5,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, update_INTERNAL6_K3 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_K3(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS_IGN_K3) ->
    % ?INTERNAL6_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V3),

    ?new_INTERNAL6(
        K1,
        K2,
        Key,
        K4,
        K5,
        K6,
        %
        V1,
        V2,
        Value,
        V4,
        V5,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, update_INTERNAL6_K4 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_K4(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS_IGN_K4) ->
    % ?INTERNAL6_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V4),

    ?new_INTERNAL6(
        K1,
        K2,
        K3,
        Key,
        K5,
        K6,
        %
        V1,
        V2,
        V3,
        Value,
        V5,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, update_INTERNAL6_K5 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_K5(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS_IGN_K5) ->
    % ?INTERNAL6_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V5),

    ?new_INTERNAL6(
        K1,
        K2,
        K3,
        K4,
        Key,
        K6,
        %
        V1,
        V2,
        V3,
        V4,
        Value,
        V6,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

-compile({inline, update_INTERNAL6_K6 / ?INTERNAL6_ARITY_PLUS3}).
update_INTERNAL6_K6(Key, ValueEval, ValueWrap, ?INTERNAL6_ARGS_IGN_K6) ->
    % ?INTERNAL6_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V6),

    ?new_INTERNAL6(
        K1,
        K2,
        K3,
        K4,
        K5,
        Key,
        %
        V1,
        V2,
        V3,
        V4,
        V5,
        Value,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6,
        C7
    ).

%%
%% ?INTERNAL5
%%

-compile({inline, update_INTERNAL5 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    if
        Key < K3 ->
            %
            if
                Key > K1 ->
                    %
                    if
                        Key < K2 ->
                            update_INTERNAL5_C2(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                        %
                        Key > K2 ->
                            update_INTERNAL5_C3(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                        %
                        true ->
                            update_INTERNAL5_K2(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS)
                    end;
                %
                Key < K1 ->
                    update_INTERNAL5_C1(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                %
                true ->
                    update_INTERNAL5_K1(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS)
            end;
        %
        Key > K3 ->
            if
                Key < K5 ->
                    %
                    if
                        Key > K4 ->
                            update_INTERNAL5_C5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                        %
                        Key < K4 ->
                            update_INTERNAL5_C4(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                        %
                        true ->
                            update_INTERNAL5_K4(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS)
                    end;
                %
                Key > K5 ->
                    update_INTERNAL5_C6(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS);
                %
                true ->
                    update_INTERNAL5_K5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS)
            end;
        %
        true ->
            update_INTERNAL5_K3(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS)
    end.

-compile({inline, update_INTERNAL5_C1 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_C1(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL5_C1(UpdatedC1).

-compile({inline, update_INTERNAL5_C2 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_C2(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL5_C2(UpdatedC2).

-compile({inline, update_INTERNAL5_C3 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_C3(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    UpdatedC3 = update_recur(Key, ValueEval, ValueWrap, C3),
    ?INTERNAL5_C3(UpdatedC3).

-compile({inline, update_INTERNAL5_C4 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_C4(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    UpdatedC4 = update_recur(Key, ValueEval, ValueWrap, C4),
    ?INTERNAL5_C4(UpdatedC4).

-compile({inline, update_INTERNAL5_C5 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_C5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    UpdatedC5 = update_recur(Key, ValueEval, ValueWrap, C5),
    ?INTERNAL5_C5(UpdatedC5).

-compile({inline, update_INTERNAL5_C6 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_C6(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS) ->
    UpdatedC6 = update_recur(Key, ValueEval, ValueWrap, C6),
    ?INTERNAL5_C6(UpdatedC6).

%%

-compile({inline, update_INTERNAL5_K1 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_K1(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS_IGN_K1) ->
    % ?INTERNAL5_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL5(
        Key,
        K2,
        K3,
        K4,
        K5,
        %
        Value,
        V2,
        V3,
        V4,
        V5,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

-compile({inline, update_INTERNAL5_K2 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_K2(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS_IGN_K2) ->
    % ?INTERNAL5_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V2),

    ?new_INTERNAL5(
        K1,
        Key,
        K3,
        K4,
        K5,
        %
        V1,
        Value,
        V3,
        V4,
        V5,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

-compile({inline, update_INTERNAL5_K3 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_K3(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS_IGN_K3) ->
    % ?INTERNAL5_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V3),

    ?new_INTERNAL5(
        K1,
        K2,
        Key,
        K4,
        K5,
        %
        V1,
        V2,
        Value,
        V4,
        V5,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

-compile({inline, update_INTERNAL5_K4 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_K4(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS_IGN_K4) ->
    % ?INTERNAL5_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V4),

    ?new_INTERNAL5(
        K1,
        K2,
        K3,
        Key,
        K5,
        %
        V1,
        V2,
        V3,
        Value,
        V5,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

-compile({inline, update_INTERNAL5_K5 / ?INTERNAL5_ARITY_PLUS3}).
update_INTERNAL5_K5(Key, ValueEval, ValueWrap, ?INTERNAL5_ARGS_IGN_K5) ->
    % ?INTERNAL5_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V5),

    ?new_INTERNAL5(
        K1,
        K2,
        K3,
        K4,
        Key,
        %
        V1,
        V2,
        V3,
        V4,
        Value,
        %
        C1,
        C2,
        C3,
        C4,
        C5,
        C6
    ).

%%
%% ?INTERNAL4
%%

-compile({inline, update_INTERNAL4 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    if
        Key > K2 ->
            %
            if
                Key < K4 ->
                    %
                    if
                        Key > K3 ->
                            update_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        Key < K3 ->
                            update_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                        %
                        true ->
                            update_INTERNAL4_K3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
                    end;
                %
                Key > K4 ->
                    update_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    update_INTERNAL4_K4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
            end;
        %
        Key < K2 ->
            %
            if
                Key < K1 ->
                    update_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                Key > K1 ->
                    update_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS);
                %
                true ->
                    update_INTERNAL4_K1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
            end;
        %
        true ->
            update_INTERNAL4_K2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS)
    end.

-compile({inline, update_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL4_C1(UpdatedC1).

-compile({inline, update_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL4_C2(UpdatedC2).

-compile({inline, update_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC3 = update_recur(Key, ValueEval, ValueWrap, C3),
    ?INTERNAL4_C3(UpdatedC3).

-compile({inline, update_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC4 = update_recur(Key, ValueEval, ValueWrap, C4),
    ?INTERNAL4_C4(UpdatedC4).

-compile({inline, update_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_C5(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS) ->
    UpdatedC5 = update_recur(Key, ValueEval, ValueWrap, C5),
    ?INTERNAL4_C5(UpdatedC5).

%%

-compile({inline, update_INTERNAL4_K1 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_K1(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K1) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL4(
        Key,
        K2,
        K3,
        K4,
        %
        Value,
        V2,
        V3,
        V4,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, update_INTERNAL4_K2 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_K2(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K2) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V2),

    ?new_INTERNAL4(
        K1,
        Key,
        K3,
        K4,
        %
        V1,
        Value,
        V3,
        V4,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, update_INTERNAL4_K3 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_K3(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K3) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V3),

    ?new_INTERNAL4(
        K1,
        K2,
        Key,
        K4,
        %
        V1,
        V2,
        Value,
        V4,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

-compile({inline, update_INTERNAL4_K4 / ?INTERNAL4_ARITY_PLUS3}).
update_INTERNAL4_K4(Key, ValueEval, ValueWrap, ?INTERNAL4_ARGS_IGN_K4) ->
    % ?INTERNAL4_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V4),

    ?new_INTERNAL4(
        K1,
        K2,
        K3,
        Key,
        %
        V1,
        V2,
        V3,
        Value,
        %
        C1,
        C2,
        C3,
        C4,
        C5
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, update_INTERNAL3 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    if
        Key < K2 ->
            %
            if
                Key < K1 ->
                    update_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K1 ->
                    update_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    update_INTERNAL3_K1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
            end;
        %
        Key > K2 ->
            %
            if
                Key < K3 ->
                    update_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                Key > K3 ->
                    update_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS);
                %
                true ->
                    update_INTERNAL3_K3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
            end;
        %
        true ->
            update_INTERNAL3_K2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS)
    end.

-compile({inline, update_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_C1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL3_C1(UpdatedC1).

-compile({inline, update_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_C2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL3_C2(UpdatedC2).

-compile({inline, update_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_C3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    UpdatedC3 = update_recur(Key, ValueEval, ValueWrap, C3),
    ?INTERNAL3_C3(UpdatedC3).

-compile({inline, update_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_C4(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS) ->
    UpdatedC4 = update_recur(Key, ValueEval, ValueWrap, C4),
    ?INTERNAL3_C4(UpdatedC4).

%%

-compile({inline, update_INTERNAL3_K1 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_K1(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS_IGN_K1) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL3(
        Key,
        K2,
        K3,
        %
        Value,
        V2,
        V3,
        %
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, update_INTERNAL3_K2 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_K2(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS_IGN_K2) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V2),

    ?new_INTERNAL3(
        K1,
        Key,
        K3,
        %
        V1,
        Value,
        V3,
        %
        C1,
        C2,
        C3,
        C4
    ).

-compile({inline, update_INTERNAL3_K3 / ?INTERNAL3_ARITY_PLUS3}).
update_INTERNAL3_K3(Key, ValueEval, ValueWrap, ?INTERNAL3_ARGS_IGN_K3) ->
    % ?INTERNAL3_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V3),

    ?new_INTERNAL3(
        K1,
        K2,
        Key,
        %
        V1,
        V2,
        Value,
        %
        C1,
        C2,
        C3,
        C4
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, update_INTERNAL2 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    if
        Key > K1 ->
            %
            if
                Key < K2 ->
                    update_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                Key > K2 ->
                    update_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
                %
                true ->
                    update_INTERNAL2_K2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS)
            end;
        %
        Key < K1 ->
            update_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS);
        %
        true ->
            update_INTERNAL2_K1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS)
    end.

-compile({inline, update_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_C1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL2_C1(UpdatedC1).

-compile({inline, update_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_C2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL2_C2(UpdatedC2).

-compile({inline, update_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_C3(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS) ->
    UpdatedC3 = update_recur(Key, ValueEval, ValueWrap, C3),
    ?INTERNAL2_C3(UpdatedC3).

%%

-compile({inline, update_INTERNAL2_K1 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_K1(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS_IGN_K1) ->
    %?INTERNAL2_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL2(
        Key,
        K2,
        %
        Value,
        V2,
        %
        C1,
        C2,
        C3
    ).

-compile({inline, update_INTERNAL2_K2 / ?INTERNAL2_ARITY_PLUS3}).
update_INTERNAL2_K2(Key, ValueEval, ValueWrap, ?INTERNAL2_ARGS_IGN_K2) ->
    %?INTERNAL2_VALUES_MATCH_ALL = Values,

    Value = eval_update_value(ValueEval, ValueWrap, V2),

    ?new_INTERNAL2(
        K1,
        Key,
        %
        V1,
        Value,
        %
        C1,
        C2,
        C3
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, update_INTERNAL1 / ?INTERNAL1_ARITY_PLUS3}).
update_INTERNAL1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    if
        Key < K1 ->
            update_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        Key > K1 ->
            update_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS);
        %
        true ->
            update_INTERNAL1_K1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS)
    end.

-compile({inline, update_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS3}).
update_INTERNAL1_C1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    UpdatedC1 = update_recur(Key, ValueEval, ValueWrap, C1),
    ?INTERNAL1_C1(UpdatedC1).

-compile({inline, update_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS3}).
update_INTERNAL1_C2(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS) ->
    UpdatedC2 = update_recur(Key, ValueEval, ValueWrap, C2),
    ?INTERNAL1_C2(UpdatedC2).

%%

-compile({inline, update_INTERNAL1_K1 / ?INTERNAL1_ARITY_PLUS3}).
update_INTERNAL1_K1(Key, ValueEval, ValueWrap, ?INTERNAL1_ARGS_IGN_K1) ->
    Value = eval_update_value(ValueEval, ValueWrap, V1),

    ?new_INTERNAL1(
        Key,
        %
        Value,
        %
        C1,
        C2
    ).

%%
%% ?LEAF6
%%

-compile({inline, update_LEAF6 / ?LEAF6_ARITY_PLUS3}).
update_LEAF6(Key, ValueEval, ValueWrap, ?LEAF6_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF6(
                Key,
                K2,
                K3,
                K4,
                K5,
                K6,
                %
                Value,
                V2,
                V3,
                V4,
                V5,
                V6
            );
        %
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?new_LEAF6(
                K1,
                Key,
                K3,
                K4,
                K5,
                K6,
                %
                V1,
                Value,
                V3,
                V4,
                V5,
                V6
            );
        %
        Key == K3 ->
            Value = eval_update_value(ValueEval, ValueWrap, V3),
            ?new_LEAF6(
                K1,
                K2,
                Key,
                K4,
                K5,
                K6,
                %
                V1,
                V2,
                Value,
                V4,
                V5,
                V6
            );
        %
        Key == K4 ->
            Value = eval_update_value(ValueEval, ValueWrap, V4),
            ?new_LEAF6(
                K1,
                K2,
                K3,
                Key,
                K5,
                K6,
                %
                V1,
                V2,
                V3,
                Value,
                V5,
                V6
            );
        %
        Key == K5 ->
            Value = eval_update_value(ValueEval, ValueWrap, V5),
            ?new_LEAF6(
                K1,
                K2,
                K3,
                K4,
                Key,
                K6,
                %
                V1,
                V2,
                V3,
                V4,
                Value,
                V6
            );
        %
        Key == K6 ->
            Value = eval_update_value(ValueEval, ValueWrap, V6),
            ?new_LEAF6(
                K1,
                K2,
                K3,
                K4,
                K5,
                Key,
                %
                V1,
                V2,
                V3,
                V4,
                V5,
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF5
%%

-compile({inline, update_LEAF5 / ?LEAF5_ARITY_PLUS3}).
update_LEAF5(Key, ValueEval, ValueWrap, ?LEAF5_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF5(
                Key,
                K2,
                K3,
                K4,
                K5,
                %
                Value,
                V2,
                V3,
                V4,
                V5
            );
        %
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?new_LEAF5(
                K1,
                Key,
                K3,
                K4,
                K5,
                %
                V1,
                Value,
                V3,
                V4,
                V5
            );
        %
        Key == K3 ->
            Value = eval_update_value(ValueEval, ValueWrap, V3),
            ?new_LEAF5(
                K1,
                K2,
                Key,
                K4,
                K5,
                %
                V1,
                V2,
                Value,
                V4,
                V5
            );
        %
        Key == K4 ->
            Value = eval_update_value(ValueEval, ValueWrap, V4),
            ?new_LEAF5(
                K1,
                K2,
                K3,
                Key,
                K5,
                %
                V1,
                V2,
                V3,
                Value,
                V5
            );
        %
        Key == K5 ->
            Value = eval_update_value(ValueEval, ValueWrap, V5),
            ?new_LEAF5(
                K1,
                K2,
                K3,
                K4,
                Key,
                %
                V1,
                V2,
                V3,
                V4,
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF4
%%

-compile({inline, update_LEAF4 / ?LEAF4_ARITY_PLUS3}).
update_LEAF4(Key, ValueEval, ValueWrap, ?LEAF4_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF4(
                Key,
                K2,
                K3,
                K4,
                %
                Value,
                V2,
                V3,
                V4
            );
        %
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?new_LEAF4(
                K1,
                Key,
                K3,
                K4,
                %
                V1,
                Value,
                V3,
                V4
            );
        %
        Key == K3 ->
            Value = eval_update_value(ValueEval, ValueWrap, V3),
            ?new_LEAF4(
                K1,
                K2,
                Key,
                K4,
                %
                V1,
                V2,
                Value,
                V4
            );
        %
        Key == K4 ->
            Value = eval_update_value(ValueEval, ValueWrap, V4),
            ?new_LEAF4(
                K1,
                K2,
                K3,
                Key,
                %
                V1,
                V2,
                V3,
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF3
%%

-compile({inline, update_LEAF3 / ?LEAF3_ARITY_PLUS3}).
update_LEAF3(Key, ValueEval, ValueWrap, ?LEAF3_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF3(
                Key,
                K2,
                K3,
                %
                Value,
                V2,
                V3
            );
        %
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?new_LEAF3(
                K1,
                Key,
                K3,
                %
                V1,
                Value,
                V3
            );
        %
        Key == K3 ->
            Value = eval_update_value(ValueEval, ValueWrap, V3),
            ?new_LEAF3(
                K1,
                K2,
                Key,
                %
                V1,
                V2,
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF2
%%

-compile({inline, update_LEAF2 / ?LEAF2_ARITY_PLUS3}).
update_LEAF2(Key, ValueEval, ValueWrap, ?LEAF2_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF2(
                Key,
                K2,
                %
                Value,
                V2
            );
        %
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V2),
            ?new_LEAF2(
                K1,
                Key,
                %
                V1,
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%
%% ?LEAF1
%%

-compile({inline, update_LEAF1 / ?LEAF1_ARITY_PLUS3}).
update_LEAF1(Key, ValueEval, ValueWrap, ?LEAF1_ARGS) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?new_LEAF1(
                Key,
                %
                Value
            );
        %
        true ->
            error_badkey(Key)
    end.

%%%

-compile({inline, eval_update_value/3}).
eval_update_value(Type, Wrap, Prev) ->
    case Type of
        eager ->
            Wrap;
        %
        lazy ->
            Wrap(Prev)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: values/1
%% ------------------------------------------------------------------

values_recur(Node, Acc) ->
    case Node of
        ?LEAF3_MATCH(_, _, _, V1, V2, V3) ->
            [V1, V2, V3 | Acc];
        %
        ?LEAF4_MATCH(_, _, _, _, V1, V2, V3, V4) ->
            [V1, V2, V3, V4 | Acc];
        %
        ?LEAF5_MATCH(_, _, _, _, _, V1, V2, V3, V4, V5) ->
            [V1, V2, V3, V4, V5 | Acc];
        %
        ?LEAF6_MATCH(_, _, _, _, _, _, V1, V2, V3, V4, V5, V6) ->
            [V1, V2, V3, V4, V5, V6 | Acc];
        %
        ?INTERNAL3_MATCH(_, _, _, V1, V2, V3, C1, C2, C3, C4) ->
            Acc2 = [V3 | values_recur(C4, Acc)],
            Acc3 = [V2 | values_recur(C3, Acc2)],
            Acc4 = [V1 | values_recur(C2, Acc3)],
            values_recur(C1, Acc4);
        %
        ?INTERNAL4_MATCH(_, _, _, _, V1, V2, V3, V4, C1, C2, C3, C4, C5) ->
            Acc2 = [V4 | values_recur(C5, Acc)],
            Acc3 = [V3 | values_recur(C4, Acc2)],
            Acc4 = [V2 | values_recur(C3, Acc3)],
            Acc5 = [V1 | values_recur(C2, Acc4)],
            values_recur(C1, Acc5);
        %
        ?INTERNAL5_MATCH(_, _, _, _, _, V1, V2, V3, V4, V5, C1, C2, C3, C4, C5, C6) ->
            Acc2 = [V5 | values_recur(C6, Acc)],
            Acc3 = [V4 | values_recur(C5, Acc2)],
            Acc4 = [V3 | values_recur(C4, Acc3)],
            Acc5 = [V2 | values_recur(C3, Acc4)],
            Acc6 = [V1 | values_recur(C2, Acc5)],
            values_recur(C1, Acc6);
        %
        ?INTERNAL6_MATCH(_, _, _, _, _, _, V1, V2, V3, V4, V5, V6, C1, C2, C3, C4, C5, C6, C7) ->
            Acc2 = [V6 | values_recur(C7, Acc)],
            Acc3 = [V5 | values_recur(C6, Acc2)],
            Acc4 = [V4 | values_recur(C5, Acc3)],
            Acc5 = [V3 | values_recur(C4, Acc4)],
            Acc6 = [V2 | values_recur(C3, Acc5)],
            Acc7 = [V1 | values_recur(C2, Acc6)],
            values_recur(C1, Acc7)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL3 / LEAF3 root
%% ------------------------------------------------------------------

-compile({inline, maybe_rebalance_root/2}).
maybe_rebalance_root(UpdatedRoot, PrevRoot) ->
    case UpdatedRoot of
        ?NEEDS_REBALANCE(Pos, MergedChild) ->
            rebalance_root(Pos, MergedChild, PrevRoot);
        %
        _ ->
            UpdatedRoot
    end.

-compile({inline, maybe_rebalance_root_on_take/2}).
maybe_rebalance_root_on_take(?TAKEN(Pair, UpdatedRoot) = Taken, PrevRoot) ->
    case UpdatedRoot of
        ?NEEDS_REBALANCE(Pos, MergedChild) ->
            ?TAKEN(Pair, rebalance_root(Pos, MergedChild, PrevRoot));
        %
        _ ->
            Taken
    end.

-compile({inline, rebalance_root/3}).
rebalance_root(Pos, MergedChild, PrevRoot) ->
    case PrevRoot of
        ?INTERNAL3_MATCH_ALL ->
            rebalance_root_INTERNAL3(Pos, MergedChild, ?INTERNAL3_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            rebalance_root_LEAF3(Pos, ?LEAF3_ARGS)
    end.

-compile({inline, rebalance_root_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
rebalance_root_INTERNAL3(Pos, MergedChild, ?INTERNAL3_ARGS) ->
    case Pos of
        1 ->
            ?new_INTERNAL2(
                K2,
                K3,
                %
                V2,
                V3,
                %
                MergedChild,
                C3,
                C4
            );
        %
        2 ->
            ?new_INTERNAL2(
                K1,
                K3,
                %
                V1,
                V3,
                %
                C1,
                MergedChild,
                C4
            );
        %
        3 ->
            ?new_INTERNAL2(
                K1,
                K2,
                %
                V1,
                V2,
                %
                C1,
                C2,
                MergedChild
            )
    end.

-compile({inline, rebalance_root_LEAF3 / ?LEAF3_ARITY_PLUS1}).
rebalance_root_LEAF3(Pos, ?LEAF3_ARGS) ->
    case Pos of
        1 ->
            ?new_LEAF2(K2, K3, V2, V3);
        %
        2 ->
            ?new_LEAF2(K1, K3, V1, V3);
        %
        3 ->
            ?new_LEAF2(K1, K2, V1, V2)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL6
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL6_C1 / ?INTERNAL6_ARITY_PLUS1}).
rebalance_INTERNAL6_C1(UpdatedC1, ?INTERNAL6_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC1, C1, K1, V1, C2) of
        no ->
            ?INTERNAL6_C1(UpdatedC1);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC1, UpdatedC2) ->
            ?new_INTERNAL6(
                UpKey,
                K2,
                K3,
                K4,
                K5,
                K6,
                %
                UpValue,
                V2,
                V3,
                V4,
                V5,
                V6,
                %
                RebalancedC1,
                UpdatedC2,
                C3,
                C4,
                C5,
                C6,
                C7
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL5(
                K2,
                K3,
                K4,
                K5,
                K6,
                %
                V2,
                V3,
                V4,
                V5,
                V6,
                %
                MergedC1C2,
                C3,
                C4,
                C5,
                C6,
                C7
            )
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL6_C2 / ?INTERNAL6_ARITY_PLUS1}).
rebalance_INTERNAL6_C2(UpdatedC2, ?INTERNAL6_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC2, C2, K1, V1, C1) of
        no ->
            ?INTERNAL6_C2(UpdatedC2);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL6(
                UpKey,
                K2,
                K3,
                K4,
                K5,
                K6,
                %
                UpValue,
                V2,
                V3,
                V4,
                V5,
                V6,
                %
                UpdatedC1,
                RebalancedC2,
                C3,
                C4,
                C5,
                C6,
                C7
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL5(
                K2,
                K3,
                K4,
                K5,
                K6,
                %
                V2,
                V3,
                V4,
                V5,
                V6,
                %
                MergedC1C2,
                C3,
                C4,
                C5,
                C6,
                C7
            )
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL6_C3 / ?INTERNAL6_ARITY_PLUS1}).
rebalance_INTERNAL6_C3(UpdatedC3, ?INTERNAL6_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC3, C3, K3, V3, C4) of
        no ->
            ?INTERNAL6_C3(UpdatedC3);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC3, UpdatedC4) ->
            ?new_INTERNAL6(
                K1,
                K2,
                UpKey,
                K4,
                K5,
                K6,
                %
                V1,
                V2,
                UpValue,
                V4,
                V5,
                V6,
                %
                C1,
                C2,
                RebalancedC3,
                UpdatedC4,
                C5,
                C6,
                C7
            );
        %
        ?MERGED(MergedC3C4) ->
            ?new_INTERNAL5(
                K1,
                K2,
                K4,
                K5,
                K6,
                %
                V1,
                V2,
                V4,
                V5,
                V6,
                %
                C1,
                C2,
                MergedC3C4,
                C5,
                C6,
                C7
            )
    end.

%%
%% C4
%%

-compile({inline, rebalance_INTERNAL6_C4 / ?INTERNAL6_ARITY_PLUS1}).
rebalance_INTERNAL6_C4(UpdatedC4, ?INTERNAL6_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC4, C4, K3, V3, C3) of
        no ->
            ?INTERNAL6_C4(UpdatedC4);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC3, RebalancedC4) ->
            ?new_INTERNAL6(
                K1,
                K2,
                UpKey,
                K4,
                K5,
                K6,
                %
                V1,
                V2,
                UpValue,
                V4,
                V5,
                V6,
                %
                C1,
                C2,
                UpdatedC3,
                RebalancedC4,
                C5,
                C6,
                C7
            );
        %
        ?MERGED(MergedC3C4) ->
            ?new_INTERNAL5(
                K1,
                K2,
                K4,
                K5,
                K6,
                %
                V1,
                V2,
                V4,
                V5,
                V6,
                %
                C1,
                C2,
                MergedC3C4,
                C5,
                C6,
                C7
            )
    end.

%%
%% C5
%%

-compile({inline, rebalance_INTERNAL6_C5 / ?INTERNAL6_ARITY_PLUS1}).
rebalance_INTERNAL6_C5(UpdatedC5, ?INTERNAL6_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC5, C5, K5, V5, C6) of
        no ->
            ?INTERNAL6_C5(UpdatedC5);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC5, UpdatedC6) ->
            ?new_INTERNAL6(
                K1,
                K2,
                K3,
                K4,
                UpKey,
                K6,
                %
                V1,
                V2,
                V3,
                V4,
                UpValue,
                V6,
                %
                C1,
                C2,
                C3,
                C4,
                RebalancedC5,
                UpdatedC6,
                C7
            );
        %
        ?MERGED(MergedC5C6) ->
            ?new_INTERNAL5(
                K1,
                K2,
                K3,
                K4,
                K6,
                %
                V1,
                V2,
                V3,
                V4,
                V6,
                %
                C1,
                C2,
                C3,
                C4,
                MergedC5C6,
                C7
            )
    end.

%%
%% C6
%%

-compile({inline, rebalance_INTERNAL6_C6 / ?INTERNAL6_ARITY_PLUS1}).
rebalance_INTERNAL6_C6(UpdatedC6, ?INTERNAL6_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC6, C6, K5, V5, C5) of
        no ->
            ?INTERNAL6_C6(UpdatedC6);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC5, RebalancedC6) ->
            ?new_INTERNAL6(
                K1,
                K2,
                K3,
                K4,
                UpKey,
                K6,
                %
                V1,
                V2,
                V3,
                V4,
                UpValue,
                V6,
                %
                C1,
                C2,
                C3,
                C4,
                UpdatedC5,
                RebalancedC6,
                C7
            );
        %
        ?MERGED(MergedC5C6) ->
            ?new_INTERNAL5(
                K1,
                K2,
                K3,
                K4,
                K6,
                %
                V1,
                V2,
                V3,
                V4,
                V6,
                %
                C1,
                C2,
                C3,
                C4,
                MergedC5C6,
                C7
            )
    end.

%%
%% C7
%%

-compile({inline, rebalance_INTERNAL6_C7 / ?INTERNAL6_ARITY_PLUS1}).
rebalance_INTERNAL6_C7(UpdatedC7, ?INTERNAL6_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC7, C7, K6, V6, C6) of
        no ->
            ?INTERNAL6_C7(UpdatedC7);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC6, RebalancedC7) ->
            ?new_INTERNAL6(
                K1,
                K2,
                K3,
                K4,
                K5,
                UpKey,
                %
                V1,
                V2,
                V3,
                V4,
                V5,
                UpValue,
                %
                C1,
                C2,
                C3,
                C4,
                C5,
                UpdatedC6,
                RebalancedC7
            );
        %
        ?MERGED(MergedC6C7) ->
            ?new_INTERNAL5(
                K1,
                K2,
                K3,
                K4,
                K5,
                %
                V1,
                V2,
                V3,
                V4,
                V5,
                %
                C1,
                C2,
                C3,
                C4,
                C5,
                MergedC6C7
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL5
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL5_C1 / ?INTERNAL5_ARITY_PLUS1}).
rebalance_INTERNAL5_C1(UpdatedC1, ?INTERNAL5_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC1, C1, K1, V1, C2) of
        no ->
            ?INTERNAL5_C1(UpdatedC1);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC1, UpdatedC2) ->
            ?new_INTERNAL5(
                UpKey,
                K2,
                K3,
                K4,
                K5,
                %
                UpValue,
                V2,
                V3,
                V4,
                V5,
                %
                RebalancedC1,
                UpdatedC2,
                C3,
                C4,
                C5,
                C6
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL4(
                K2,
                K3,
                K4,
                K5,
                %
                V2,
                V3,
                V4,
                V5,
                %
                MergedC1C2,
                C3,
                C4,
                C5,
                C6
            )
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL5_C2 / ?INTERNAL5_ARITY_PLUS1}).
rebalance_INTERNAL5_C2(UpdatedC2, ?INTERNAL5_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC2, C2, K1, V1, C1) of
        no ->
            ?INTERNAL5_C2(UpdatedC2);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL5(
                UpKey,
                K2,
                K3,
                K4,
                K5,
                %
                UpValue,
                V2,
                V3,
                V4,
                V5,
                %
                UpdatedC1,
                RebalancedC2,
                C3,
                C4,
                C5,
                C6
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL4(
                K2,
                K3,
                K4,
                K5,
                %
                V2,
                V3,
                V4,
                V5,
                %
                MergedC1C2,
                C3,
                C4,
                C5,
                C6
            )
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL5_C3 / ?INTERNAL5_ARITY_PLUS1}).
rebalance_INTERNAL5_C3(UpdatedC3, ?INTERNAL5_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC3, C3, K3, V3, C4) of
        no ->
            ?INTERNAL5_C3(UpdatedC3);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC3, UpdatedC4) ->
            ?new_INTERNAL5(
                K1,
                K2,
                UpKey,
                K4,
                K5,
                %
                V1,
                V2,
                UpValue,
                V4,
                V5,
                %
                C1,
                C2,
                RebalancedC3,
                UpdatedC4,
                C5,
                C6
            );
        %
        ?MERGED(MergedC3C4) ->
            ?new_INTERNAL4(
                K1,
                K2,
                K4,
                K5,
                %
                V1,
                V2,
                V4,
                V5,
                %
                C1,
                C2,
                MergedC3C4,
                C5,
                C6
            )
    end.

%%
%% C4
%%

-compile({inline, rebalance_INTERNAL5_C4 / ?INTERNAL5_ARITY_PLUS1}).
rebalance_INTERNAL5_C4(UpdatedC4, ?INTERNAL5_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC4, C4, K3, V3, C3) of
        no ->
            ?INTERNAL5_C4(UpdatedC4);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC3, RebalancedC4) ->
            ?new_INTERNAL5(
                K1,
                K2,
                UpKey,
                K4,
                K5,
                %
                V1,
                V2,
                UpValue,
                V4,
                V5,
                %
                C1,
                C2,
                UpdatedC3,
                RebalancedC4,
                C5,
                C6
            );
        %
        ?MERGED(MergedC3C4) ->
            ?new_INTERNAL4(
                K1,
                K2,
                K4,
                K5,
                %
                V1,
                V2,
                V4,
                V5,
                %
                C1,
                C2,
                MergedC3C4,
                C5,
                C6
            )
    end.

%%
%% C5
%%

-compile({inline, rebalance_INTERNAL5_C5 / ?INTERNAL5_ARITY_PLUS1}).
rebalance_INTERNAL5_C5(UpdatedC5, ?INTERNAL5_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC5, C5, K5, V5, C6) of
        no ->
            ?INTERNAL5_C5(UpdatedC5);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC5, UpdatedC6) ->
            ?new_INTERNAL5(
                K1,
                K2,
                K3,
                K4,
                UpKey,
                %
                V1,
                V2,
                V3,
                V4,
                UpValue,
                %
                C1,
                C2,
                C3,
                C4,
                RebalancedC5,
                UpdatedC6
            );
        %
        ?MERGED(MergedC5C6) ->
            ?new_INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                %
                V1,
                V2,
                V3,
                V4,
                %
                C1,
                C2,
                C3,
                C4,
                MergedC5C6
            )
    end.

%%
%% C6
%%

-compile({inline, rebalance_INTERNAL5_C6 / ?INTERNAL5_ARITY_PLUS1}).
rebalance_INTERNAL5_C6(UpdatedC6, ?INTERNAL5_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC6, C6, K5, V5, C5) of
        no ->
            ?INTERNAL5_C6(UpdatedC6);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC5, RebalancedC6) ->
            ?new_INTERNAL5(
                K1,
                K2,
                K3,
                K4,
                UpKey,
                %
                V1,
                V2,
                V3,
                V4,
                UpValue,
                %
                C1,
                C2,
                C3,
                C4,
                UpdatedC5,
                RebalancedC6
            );
        %
        ?MERGED(MergedC5C6) ->
            ?new_INTERNAL4(
                K1,
                K2,
                K3,
                K4,
                %
                V1,
                V2,
                V3,
                V4,
                %
                C1,
                C2,
                C3,
                C4,
                MergedC5C6
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL4
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
rebalance_INTERNAL4_C1(UpdatedC1, ?INTERNAL4_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC1, C1, K1, V1, C2) of
        no ->
            ?INTERNAL4_C1(UpdatedC1);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC1, UpdatedC2) ->
            ?new_INTERNAL4(
                UpKey,
                K2,
                K3,
                K4,
                %
                UpValue,
                V2,
                V3,
                V4,
                %
                RebalancedC1,
                UpdatedC2,
                C3,
                C4,
                C5
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL3(
                K2,
                K3,
                K4,
                %
                V2,
                V3,
                V4,
                %
                MergedC1C2,
                C3,
                C4,
                C5
            )
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
rebalance_INTERNAL4_C2(UpdatedC2, ?INTERNAL4_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC2, C2, K1, V1, C1) of
        no ->
            ?INTERNAL4_C2(UpdatedC2);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL4(
                UpKey,
                K2,
                K3,
                K4,
                %
                UpValue,
                V2,
                V3,
                V4,
                %
                UpdatedC1,
                RebalancedC2,
                C3,
                C4,
                C5
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL3(
                K2,
                K3,
                K4,
                %
                V2,
                V3,
                V4,
                %
                MergedC1C2,
                C3,
                C4,
                C5
            )
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
rebalance_INTERNAL4_C3(UpdatedC3, ?INTERNAL4_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC3, C3, K3, V3, C4) of
        no ->
            ?INTERNAL4_C3(UpdatedC3);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC3, UpdatedC4) ->
            ?new_INTERNAL4(
                K1,
                K2,
                UpKey,
                K4,
                %
                V1,
                V2,
                UpValue,
                V4,
                %
                C1,
                C2,
                RebalancedC3,
                UpdatedC4,
                C5
            );
        %
        ?MERGED(MergedC3C4) ->
            ?new_INTERNAL3(
                K1,
                K2,
                K4,
                %
                V1,
                V2,
                V4,
                %
                C1,
                C2,
                MergedC3C4,
                C5
            )
    end.

%%
%% C4
%%

-compile({inline, rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
rebalance_INTERNAL4_C4(UpdatedC4, ?INTERNAL4_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC4, C4, K3, V3, C3) of
        no ->
            ?INTERNAL4_C4(UpdatedC4);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC3, RebalancedC4) ->
            ?new_INTERNAL4(
                K1,
                K2,
                UpKey,
                K4,
                %
                V1,
                V2,
                UpValue,
                V4,
                %
                C1,
                C2,
                UpdatedC3,
                RebalancedC4,
                C5
            );
        %
        ?MERGED(MergedC3C4) ->
            ?new_INTERNAL3(
                K1,
                K2,
                K4,
                %
                V1,
                V2,
                V4,
                %
                C1,
                C2,
                MergedC3C4,
                C5
            )
    end.

%%
%% C5
%%

-compile({inline, rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
rebalance_INTERNAL4_C5(UpdatedC5, ?INTERNAL4_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC5, C5, K4, V4, C4) of
        no ->
            ?INTERNAL4_C5(UpdatedC5);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC4, RebalancedC5) ->
            ?new_INTERNAL4(
                K1,
                K2,
                K3,
                UpKey,
                %
                V1,
                V2,
                V3,
                UpValue,
                %
                C1,
                C2,
                C3,
                UpdatedC4,
                RebalancedC5
            );
        %
        ?MERGED(MergedC4C5) ->
            ?new_INTERNAL3(
                K1,
                K2,
                K3,
                %
                V1,
                V2,
                V3,
                %
                C1,
                C2,
                C3,
                MergedC4C5
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL3
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
rebalance_INTERNAL3_C1(UpdatedC1, ?INTERNAL3_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC1, C1, K1, V1, C2) of
        no ->
            ?INTERNAL3_C1(UpdatedC1);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC1, UpdatedC2) ->
            ?new_INTERNAL3(
                UpKey,
                K2,
                K3,
                %
                UpValue,
                V2,
                V3,
                %
                RebalancedC1,
                UpdatedC2,
                C3,
                C4
            );
        %
        ?MERGED(MergedC1C2) ->
            ?NEEDS_REBALANCE(1, MergedC1C2)
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
rebalance_INTERNAL3_C2(UpdatedC2, ?INTERNAL3_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC2, C2, K1, V1, C1) of
        no ->
            ?INTERNAL3_C2(UpdatedC2);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL3(
                UpKey,
                K2,
                K3,
                %
                UpValue,
                V2,
                V3,
                %
                UpdatedC1,
                RebalancedC2,
                C3,
                C4
            );
        %
        ?MERGED(MergedC1C2) ->
            ?NEEDS_REBALANCE(1, MergedC1C2)
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
rebalance_INTERNAL3_C3(UpdatedC3, ?INTERNAL3_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC3, C3, K2, V2, C2) of
        no ->
            ?INTERNAL3_C3(UpdatedC3);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC2, RebalancedC3) ->
            ?new_INTERNAL3(
                K1,
                UpKey,
                K3,
                %
                V1,
                UpValue,
                V3,
                %
                C1,
                UpdatedC2,
                RebalancedC3,
                C4
            );
        %
        ?MERGED(MergedC2C3) ->
            ?NEEDS_REBALANCE(2, MergedC2C3)
    end.

%%
%% C4
%%

-compile({inline, rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
rebalance_INTERNAL3_C4(UpdatedC4, ?INTERNAL3_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC4, C4, K3, V3, C3) of
        no ->
            ?INTERNAL3_C4(UpdatedC4);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC3, RebalancedC4) ->
            ?new_INTERNAL3(
                K1,
                K2,
                UpKey,
                %
                V1,
                V2,
                UpValue,
                %
                C1,
                C2,
                UpdatedC3,
                RebalancedC4
            );
        %
        ?MERGED(MergedC3C4) ->
            ?NEEDS_REBALANCE(3, MergedC3C4)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL2
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
rebalance_INTERNAL2_C1(UpdatedC1, ?INTERNAL2_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC1, C1, K1, V1, C2) of
        no ->
            ?INTERNAL2_C1(UpdatedC1);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC1, UpdatedC2) ->
            ?new_INTERNAL2(
                UpKey,
                K2,
                %
                UpValue,
                V2,
                %
                RebalancedC1,
                UpdatedC2,
                C3
            );
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen on root
            ?new_INTERNAL1(
                K2,
                %
                V2,
                %
                MergedC1C2,
                C3
            )
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
rebalance_INTERNAL2_C2(UpdatedC2, ?INTERNAL2_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC2, C2, K1, V1, C1) of
        no ->
            ?INTERNAL2_C2(UpdatedC2);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL2(
                UpKey,
                K2,
                %
                UpValue,
                V2,
                %
                UpdatedC1,
                RebalancedC2,
                C3
            );
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen on root
            ?new_INTERNAL1(
                K2,
                %
                V2,
                %
                MergedC1C2,
                C3
            )
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
rebalance_INTERNAL2_C3(UpdatedC3, ?INTERNAL2_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC3, C3, K2, V2, C2) of
        no ->
            ?INTERNAL2_C3(UpdatedC3);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC2, RebalancedC3) ->
            ?new_INTERNAL2(
                K1,
                UpKey,
                %
                V1,
                UpValue,
                %
                C1,
                UpdatedC2,
                RebalancedC3
            );
        %
        ?MERGED(MergedC2C3) ->
            % Can only happen on root
            ?new_INTERNAL1(
                K1,
                %
                V1,
                %
                C1,
                MergedC2C3
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL1
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
rebalance_INTERNAL1_C1(UpdatedC1, ?INTERNAL1_ARGS) ->
    case maybe_rebalance_from_right_sibling(UpdatedC1, C1, K1, V1, C2) of
        no ->
            ?INTERNAL1_C1(UpdatedC1);
        %
        ?ROTATED(UpKey, UpValue, RebalancedC1, UpdatedC2) ->
            ?new_INTERNAL1(UpKey, UpValue, RebalancedC1, UpdatedC2);
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen on root - height is reduced
            MergedC1C2
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
rebalance_INTERNAL1_C2(UpdatedC2, ?INTERNAL1_ARGS) ->
    case maybe_rebalance_from_left_sibling(UpdatedC2, C2, K1, V1, C1) of
        no ->
            ?INTERNAL1_C2(UpdatedC2);
        %
        ?ROTATED(UpKey, UpValue, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL1(UpKey, UpValue, UpdatedC1, RebalancedC2);
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen on root - height is reduced
            MergedC1C2
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its right sibling
%% ------------------------------------------------------------------

maybe_rebalance_from_right_sibling(Node, PrevNode, ParentK, ParentV, Right) ->
    case Node of
        ?NEEDS_REBALANCE(Pos, MergedChild) ->
            case PrevNode of
                ?INTERNAL3_MATCH_ALL ->
                    rebalance_internal3_from_right_sibling(
                        Pos, MergedChild, ?INTERNAL3_ARGS, ParentK, ParentV, Right
                    );
                %
                ?LEAF3_MATCH_ALL ->
                    rebalance_leaf3_from_right_sibling(Pos, ?LEAF3_ARGS, ParentK, ParentV, Right)
            end;
        %
        _ ->
            no
    end.

-compile({inline, rebalance_internal3_from_right_sibling / ?INTERNAL3_ARITY_PLUS5}).
rebalance_internal3_from_right_sibling(Pos, MergedChild, ?INTERNAL3_ARGS, ParentK, ParentV, Right) ->
    case Pos of
        1 ->
            rebalance_internal2_from_right_sibling(
                K2,
                K3,
                %
                V2,
                V3,
                %
                MergedChild,
                C3,
                C4,
                %
                ParentK,
                ParentV,
                Right
            );
        %
        2 ->
            rebalance_internal2_from_right_sibling(
                K1,
                K3,
                %,
                V1,
                V3,
                %
                C1,
                MergedChild,
                C4,
                %
                ParentK,
                ParentV,
                Right
            );
        %
        3 ->
            rebalance_internal2_from_right_sibling(
                K1,
                K2,
                %,
                V1,
                V2,
                %
                C1,
                C2,
                MergedChild,
                %
                ParentK,
                ParentV,
                Right
            )
    end.

-compile({inline, rebalance_internal2_from_right_sibling / ?INTERNAL2_ARITY_PLUS3}).
rebalance_internal2_from_right_sibling(?INTERNAL2_ARGS, ParentK, ParentV, Right) ->
    case Right of
        ?INTERNAL3_MATCH(
            RK1,
            RK2,
            RK3,
            %
            RV1,
            RV2,
            RV3,
            %
            RC1,
            RC2,
            RC3,
            RC4
        ) ->
            MergedNode =
                ?new_INTERNAL6(
                    K1,
                    K2,
                    ParentK,
                    RK1,
                    RK2,
                    RK3,
                    %
                    V1,
                    V2,
                    ParentV,
                    RV1,
                    RV2,
                    RV3,
                    %
                    C1,
                    C2,
                    C3,
                    RC1,
                    RC2,
                    RC3,
                    RC4
                ),

            ?MERGED(MergedNode);
        %
        %
        %
        %
        ?INTERNAL4_MATCH(
            RK1,
            RK2,
            RK3,
            RK4,
            %
            RV1,
            RV2,
            RV3,
            RV4,
            %
            RC1,
            RC2,
            RC3,
            RC4,
            RC5
        ) ->
            UpKey = RK1,
            UpValue = RV1,
            MovedChild = RC1,

            UpdatedNode = ?new_INTERNAL3(
                K1,
                K2,
                ParentK,
                %
                V1,
                V2,
                ParentV,
                %
                C1,
                C2,
                C3,
                MovedChild
            ),

            UpdatedRight = ?new_INTERNAL3(
                RK2,
                RK3,
                RK4,
                %
                RV2,
                RV3,
                RV4,
                %
                RC2,
                RC3,
                RC4,
                RC5
            ),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight);
        %
        %
        %
        %
        ?INTERNAL5_MATCH(
            RK1,
            RK2,
            RK3,
            RK4,
            RK5,
            %
            RV1,
            RV2,
            RV3,
            RV4,
            RV5,
            %
            RC1,
            RC2,
            RC3,
            RC4,
            RC5,
            RC6
        ) ->
            UpKey = RK1,
            UpValue = RV1,
            MovedChild = RC1,

            UpdatedNode = ?new_INTERNAL3(
                K1,
                K2,
                ParentK,
                %
                V1,
                V2,
                ParentV,
                %
                C1,
                C2,
                C3,
                MovedChild
            ),

            UpdatedRight = ?new_INTERNAL4(
                RK2,
                RK3,
                RK4,
                RK5,
                %
                RV2,
                RV3,
                RV4,
                RV5,
                %
                RC2,
                RC3,
                RC4,
                RC5,
                RC6
            ),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight);
        %
        %
        %
        %
        ?INTERNAL6_MATCH(
            RK1,
            RK2,
            RK3,
            RK4,
            RK5,
            RK6,
            %
            RV1,
            RV2,
            RV3,
            RV4,
            RV5,
            RV6,
            %
            RC1,
            RC2,
            RC3,
            RC4,
            RC5,
            RC6,
            RC7
        ) ->
            UpKey = RK1,
            UpValue = RV1,
            MovedChild = RC1,

            UpdatedNode = ?new_INTERNAL3(
                K1,
                K2,
                ParentK,
                %
                V1,
                V2,
                ParentV,
                %
                C1,
                C2,
                C3,
                MovedChild
            ),

            UpdatedRight = ?new_INTERNAL5(
                RK2,
                RK3,
                RK4,
                RK5,
                RK6,
                %
                RV2,
                RV3,
                RV4,
                RV5,
                RV6,
                %
                RC2,
                RC3,
                RC4,
                RC5,
                RC6,
                RC7
            ),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight)
    end.

-compile({inline, rebalance_leaf3_from_right_sibling / ?LEAF3_ARITY_PLUS4}).
rebalance_leaf3_from_right_sibling(Pos, ?LEAF3_ARGS, ParentK, ParentV, Right) ->
    case Pos of
        1 ->
            rebalance_leaf2_from_right_sibling(
                K2,
                K3,
                %
                V2,
                V3,
                %
                ParentK,
                ParentV,
                Right
            );
        %
        2 ->
            rebalance_leaf2_from_right_sibling(
                K1,
                K3,
                %
                V1,
                V3,
                %
                ParentK,
                ParentV,
                Right
            );
        3 ->
            rebalance_leaf2_from_right_sibling(
                K1,
                K2,
                %
                V1,
                V2,
                %
                ParentK,
                ParentV,
                Right
            )
    end.

-compile({inline, rebalance_leaf2_from_right_sibling / ?LEAF2_ARITY_PLUS3}).
rebalance_leaf2_from_right_sibling(?LEAF2_ARGS, ParentK, ParentV, Right) ->
    case Right of
        ?LEAF3_MATCH(
            RK1,
            RK2,
            RK3,
            %
            RV1,
            RV2,
            RV3
        ) ->
            MergedNode =
                ?new_LEAF6(
                    K1,
                    K2,
                    ParentK,
                    RK1,
                    RK2,
                    RK3,
                    %
                    V1,
                    V2,
                    ParentV,
                    RV1,
                    RV2,
                    RV3
                ),

            ?MERGED(MergedNode);
        %
        %
        %
        %
        ?LEAF4_MATCH(
            RK1,
            RK2,
            RK3,
            RK4,
            %
            RV1,
            RV2,
            RV3,
            RV4
        ) ->
            UpKey = RK1,
            UpValue = RV1,

            UpdatedNode = ?new_LEAF3(
                K1,
                K2,
                ParentK,
                %
                V1,
                V2,
                ParentV
            ),

            UpdatedRight = ?new_LEAF3(
                RK2,
                RK3,
                RK4,
                %
                RV2,
                RV3,
                RV4
            ),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight);
        %
        %
        %
        %
        ?LEAF5_MATCH(
            RK1,
            RK2,
            RK3,
            RK4,
            RK5,
            %
            RV1,
            RV2,
            RV3,
            RV4,
            RV5
        ) ->
            UpKey = RK1,
            UpValue = RV1,

            UpdatedNode = ?new_LEAF3(
                K1,
                K2,
                ParentK,
                %
                V1,
                V2,
                ParentV
            ),

            UpdatedRight = ?new_LEAF4(
                RK2,
                RK3,
                RK4,
                RK5,
                %
                RV2,
                RV3,
                RV4,
                RV5
            ),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight);
        %
        %
        %
        %
        ?LEAF6_MATCH(
            RK1,
            RK2,
            RK3,
            RK4,
            RK5,
            RK6,
            %
            RV1,
            RV2,
            RV3,
            RV4,
            RV5,
            RV6
        ) ->
            UpKey = RK1,
            UpValue = RV1,

            UpdatedNode = ?new_LEAF3(
                K1,
                K2,
                ParentK,
                %
                V1,
                V2,
                ParentV
            ),

            UpdatedRight = ?new_LEAF5(
                RK2,
                RK3,
                RK4,
                RK5,
                RK6,
                %
                RV2,
                RV3,
                RV4,
                RV5,
                RV6
            ),

            ?ROTATED(UpKey, UpValue, UpdatedNode, UpdatedRight)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its left sibling
%% ------------------------------------------------------------------

maybe_rebalance_from_left_sibling(Node, PrevNode, ParentK, ParentV, Left) ->
    case Node of
        ?NEEDS_REBALANCE(Pos, MergedChild) ->
            case PrevNode of
                ?INTERNAL3_MATCH_ALL ->
                    rebalance_internal3_from_left_sibling(
                        Pos, MergedChild, ?INTERNAL3_ARGS, ParentK, ParentV, Left
                    );
                %
                ?LEAF3_MATCH_ALL ->
                    rebalance_leaf3_from_left_sibling(Pos, ?LEAF3_ARGS, ParentK, ParentV, Left)
            end;
        %
        _ ->
            no
    end.

-compile({inline, rebalance_internal3_from_left_sibling / ?INTERNAL3_ARITY_PLUS5}).
rebalance_internal3_from_left_sibling(Pos, MergedChild, ?INTERNAL3_ARGS, ParentK, ParentV, Left) ->
    case Pos of
        1 ->
            rebalance_internal2_from_left_sibling(
                K2,
                K3,
                %
                V2,
                V3,
                %
                MergedChild,
                C3,
                C4,
                %
                ParentK,
                ParentV,
                Left
            );
        %
        2 ->
            rebalance_internal2_from_left_sibling(
                K1,
                K3,
                %,
                V1,
                V3,
                %
                C1,
                MergedChild,
                C4,
                %
                ParentK,
                ParentV,
                Left
            );
        %
        3 ->
            rebalance_internal2_from_left_sibling(
                K1,
                K2,
                %,
                V1,
                V2,
                %
                C1,
                C2,
                MergedChild,
                %
                ParentK,
                ParentV,
                Left
            )
    end.

-compile({inline, rebalance_internal2_from_left_sibling / ?INTERNAL2_ARITY_PLUS3}).
rebalance_internal2_from_left_sibling(?INTERNAL2_ARGS, ParentK, ParentV, Left) ->
    case Left of
        ?INTERNAL3_MATCH(
            LK1,
            LK2,
            LK3,
            %
            LV1,
            LV2,
            LV3,
            %
            LC1,
            LC2,
            LC3,
            LC4
        ) ->
            MergedNode =
                ?new_INTERNAL6(
                    LK1,
                    LK2,
                    LK3,
                    ParentK,
                    K1,
                    K2,
                    %
                    LV1,
                    LV2,
                    LV3,
                    ParentV,
                    V1,
                    V2,
                    %
                    LC1,
                    LC2,
                    LC3,
                    LC4,
                    C1,
                    C2,
                    C3
                ),

            ?MERGED(MergedNode);
        %
        %
        %
        %
        ?INTERNAL4_MATCH(
            LK1,
            LK2,
            LK3,
            LK4,
            %
            LV1,
            LV2,
            LV3,
            LV4,
            %
            LC1,
            LC2,
            LC3,
            LC4,
            LC5
        ) ->
            UpKey = LK4,
            UpValue = LV4,
            MovedChild = LC5,

            UpdatedNode = ?new_INTERNAL3(
                ParentK,
                K1,
                K2,
                %
                ParentV,
                V1,
                V2,
                %
                MovedChild,
                C1,
                C2,
                C3
            ),

            UpdatedLeft = ?new_INTERNAL3(
                LK1,
                LK2,
                LK3,
                %
                LV1,
                LV2,
                LV3,
                %
                LC1,
                LC2,
                LC3,
                LC4
            ),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        %
        %
        ?INTERNAL5_MATCH(
            LK1,
            LK2,
            LK3,
            LK4,
            LK5,
            %
            LV1,
            LV2,
            LV3,
            LV4,
            LV5,
            %
            LC1,
            LC2,
            LC3,
            LC4,
            LC5,
            LC6
        ) ->
            UpKey = LK5,
            UpValue = LV5,
            MovedChild = LC6,

            UpdatedNode = ?new_INTERNAL3(
                ParentK,
                K1,
                K2,
                %
                ParentV,
                V1,
                V2,
                %
                MovedChild,
                C1,
                C2,
                C3
            ),

            UpdatedLeft = ?new_INTERNAL4(
                LK1,
                LK2,
                LK3,
                LK4,
                %
                LV1,
                LV2,
                LV3,
                LV4,
                %
                LC1,
                LC2,
                LC3,
                LC4,
                LC5
            ),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        %
        %
        ?INTERNAL6_MATCH(
            LK1,
            LK2,
            LK3,
            LK4,
            LK5,
            LK6,
            %
            LV1,
            LV2,
            LV3,
            LV4,
            LV5,
            LV6,
            %
            LC1,
            LC2,
            LC3,
            LC4,
            LC5,
            LC6,
            LC7
        ) ->
            UpKey = LK6,
            UpValue = LV6,
            MovedChild = LC7,

            UpdatedNode = ?new_INTERNAL3(
                ParentK,
                K1,
                K2,
                %
                ParentV,
                V1,
                V2,
                %
                MovedChild,
                C1,
                C2,
                C3
            ),

            UpdatedLeft = ?new_INTERNAL5(
                LK1,
                LK2,
                LK3,
                LK4,
                LK5,
                %
                LV1,
                LV2,
                LV3,
                LV4,
                LV5,
                %
                LC1,
                LC2,
                LC3,
                LC4,
                LC5,
                LC6
            ),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode)
    end.

-compile({inline, rebalance_leaf3_from_left_sibling / ?LEAF3_ARITY_PLUS4}).
rebalance_leaf3_from_left_sibling(Pos, ?LEAF3_ARGS, ParentK, ParentV, Left) ->
    case Pos of
        1 ->
            rebalance_leaf2_from_left_sibling(
                K2,
                K3,
                %
                V2,
                V3,
                %
                ParentK,
                ParentV,
                Left
            );
        %
        2 ->
            rebalance_leaf2_from_left_sibling(
                K1,
                K3,
                %
                V1,
                V3,
                %
                ParentK,
                ParentV,
                Left
            );
        3 ->
            rebalance_leaf2_from_left_sibling(
                K1,
                K2,
                %
                V1,
                V2,
                %
                ParentK,
                ParentV,
                Left
            )
    end.

-compile({inline, rebalance_leaf2_from_left_sibling / ?LEAF2_ARITY_PLUS3}).
rebalance_leaf2_from_left_sibling(?LEAF2_ARGS, ParentK, ParentV, Left) ->
    case Left of
        ?LEAF3_MATCH(
            LK1,
            LK2,
            LK3,
            %
            LV1,
            LV2,
            LV3
        ) ->
            MergedNode =
                ?new_LEAF6(
                    LK1,
                    LK2,
                    LK3,
                    ParentK,
                    K1,
                    K2,
                    %
                    LV1,
                    LV2,
                    LV3,
                    ParentV,
                    V1,
                    V2
                ),

            ?MERGED(MergedNode);
        %
        %
        %
        %
        ?LEAF4_MATCH(
            LK1,
            LK2,
            LK3,
            LK4,
            %
            LV1,
            LV2,
            LV3,
            LV4
        ) ->
            UpKey = LK4,
            UpValue = LV4,

            UpdatedNode = ?new_LEAF3(
                ParentK,
                K1,
                K2,
                %
                ParentV,
                V1,
                V2
            ),

            UpdatedLeft = ?new_LEAF3(
                LK1,
                LK2,
                LK3,
                %
                LV1,
                LV2,
                LV3
            ),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        %
        %
        ?LEAF5_MATCH(
            LK1,
            LK2,
            LK3,
            LK4,
            LK5,
            %
            LV1,
            LV2,
            LV3,
            LV4,
            LV5
        ) ->
            UpKey = LK5,
            UpValue = LV5,

            UpdatedNode = ?new_LEAF3(
                ParentK,
                K1,
                K2,
                %
                ParentV,
                V1,
                V2
            ),

            UpdatedLeft = ?new_LEAF4(
                LK1,
                LK2,
                LK3,
                LK4,
                %
                LV1,
                LV2,
                LV3,
                LV4
            ),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode);
        %
        %
        %
        %
        ?LEAF6_MATCH(
            LK1,
            LK2,
            LK3,
            LK4,
            LK5,
            LK6,
            %
            LV1,
            LV2,
            LV3,
            LV4,
            LV5,
            LV6
        ) ->
            UpKey = LK6,
            UpValue = LV6,

            UpdatedNode = ?new_LEAF3(
                ParentK,
                K1,
                K2,
                %
                ParentV,
                V1,
                V2
            ),

            UpdatedLeft = ?new_LEAF5(
                LK1,
                LK2,
                LK3,
                LK4,
                LK5,
                %
                LV1,
                LV2,
                LV3,
                LV4,
                LV5
            ),

            ?ROTATED(UpKey, UpValue, UpdatedLeft, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Well-Formedness Checks
%% ------------------------------------------------------------------

-if(?NODE_CHECK_ENABLED).

check_node(LineNumber, Node) ->
    Type = node_type(Node),
    List = to_list(Node),
    MissortedKeys = check_node_keys(List),

    case MissortedKeys of
        [] ->
            Node;
        %
        [_ | _] ->
            fail_node_check(LineNumber, Type, {missorted_keys, MissortedKeys})
    end.

node_type(Node) ->
    case Node of
        ?INTERNAL2(_, _, _, _, _, _, _) ->
            'INTERNAL2';
        %
        ?INTERNAL1(_, _, _, _) ->
            'INTERNAL1';
        %
        ?LEAF2(_, _, _, _) ->
            'LEAF2';
        %
        ?LEAF1(_, _) ->
            'LEAF1';
        %
        _ ->
            recur_node_type(Node)
    end.

check_node_recur(LineNumber, Node) ->
    Type = recur_node_type(Node),

    try to_list_recur(Node, []) of
        List ->
            MissortedKeys = check_node_keys(List),

            case MissortedKeys of
                [] ->
                    Node;
                %
                [_ | _] ->
                    fail_node_check(LineNumber, Type, {missorted_keys, MissortedKeys})
            end
    catch
        Class:Reason:Stacktrace ->
            fail_node_check(LineNumber, Type, {error, to_list_recur, {Class, Reason, Stacktrace}})
    end.

recur_node_type(Node) ->
    case Node of
        ?INTERNAL3(_, _, _, _, _, _, _, _, _, _) ->
            'INTERNAL3';
        %
        ?INTERNAL4(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
            'INTERNAL4';
        %
        ?INTERNAL5(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
            'INTERNAL5';
        %
        ?INTERNAL6(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
            'INTERNAL6';
        %
        ?LEAF3(_, _, _, _, _, _) ->
            'LEAF3';
        %
        ?LEAF4(_, _, _, _, _, _, _, _) ->
            'LEAF4';
        %
        ?LEAF5(_, _, _, _, _, _, _, _, _, _) ->
            'LEAF5';
        %
        ?LEAF6(_, _, _, _, _, _, _, _, _, _, _, _) ->
            'LEAF6'
    end.

fail_node_check(LineNumber, Type, Reason) ->
    error(
        {bad_node, [
            {line, LineNumber},
            {type, Type},
            {reason, Reason}
        ]}
    ).

check_node_keys([H | T]) ->
    check_node_keys(H, T);
check_node_keys([]) ->
    [].

check_node_keys(K1, [K2 | Next]) ->
    case K1 >= K2 of
        false ->
            check_node_keys(K2, Next);
        %
        true ->
            [{K1, K2} | check_node_keys(K2, Next)]
    end;
check_node_keys(_, []) ->
    [].

% -if(?NODE_CHECK_ENABLED).
-endif.
