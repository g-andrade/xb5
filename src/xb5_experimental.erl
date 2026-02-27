-module(xb5_experimental).

-include("src/helpers.hrl").

-export([
    from_list/1,
    insert/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 9 elements
-define(INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, C5), {E1, E2, E3, E4, C1, C2, C3, C4, C5}).
-define(INTERNAL4_MATCH_ALL, {E1, E2, E3, E4, C1, C2, C3, C4, C5}).

% 7 elements
-define(INTERNAL3(E1, E2, E3, C1, C2, C3, C4), {E1, E2, E3, C1, C2, C3, C4}).
-define(INTERNAL3_MATCH_ALL, {E1, E2, E3, C1, C2, C3, C4}).

% 6 elements
-define(INTERNAL2(E1, E2, C1, C2, C3), {internal2, E1, E2, C1, C2, C3}).
-define(INTERNAL2_MATCH_ALL, {_, E1, E2, C1, C2, C3}).

% 4 elements
-define(INTERNAL1(E1, C1, C2), {internal1, E1, C1, C2}).
-define(INTERNAL1_MATCH_ALL, {_, E1, C1, C2}).

% 5 elements
-define(LEAF4(E1, E2, E3, E4), {leaf4, E1, E2, E3, E4}).
-define(LEAF4_MATCH_ALL, {_, E1, E2, E3, E4}).

% 3 elements
-define(LEAF3(E1, E2, E3), {E1, E2, E3}).
-define(LEAF3_MATCH_ALL, {E1, E2, E3}).

% 2 elements
-define(LEAF2(E1, E2), {E1, E2}).
-define(LEAF2_MATCH_ALL, {E1, E2}).

% 1 element
-define(LEAF1(E1), {E1}).
-define(LEAF1_MATCH_ALL, {E1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SPLIT(SplitE, SplitL, SplitR), {split, SplitE, SplitL, SplitR}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(INTERNAL4_ARGS, E1, E2, E3, E4, C1, C2, C3, C4, C5).

-define(INTERNAL3_ARGS, E1, E2, E3, C1, C2, C3, C4).

-define(INTERNAL2_ARGS, E1, E2, C1, C2, C3).

-define(INTERNAL1_ARGS, E1, C1, C2).

-define(LEAF4_ARGS, E1, E2, E3, E4).

-define(LEAF3_ARGS, E1, E2, E3).

-define(LEAF2_ARGS, E1, E2).

-define(LEAF1_ARGS, E1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(INTERNAL4_ARITY_PLUS1, 10).

-define(INTERNAL3_ARITY_PLUS1, 8).

-define(INTERNAL2_ARITY_PLUS1, 6).

-define(INTERNAL1_ARITY_PLUS1, 4).

-define(LEAF4_ARITY_PLUS1, 5).

-define(LEAF3_ARITY_PLUS1, 4).

-define(LEAF2_ARITY_PLUS1, 3).

-define(LEAF1_ARITY_PLUS1, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NODE_CHECK_ENABLED, false).

-if(?NODE_CHECK_ENABLED).
-define(CHECK_NODE(Node), check_node(?LINE, Node, top)).
-define(CHECK_NODE_RECUR(Node), check_node(?LINE, Node, recur)).
-else.
-define(CHECK_NODE(Node), Node).
-define(CHECK_NODE_RECUR(Node), Node).
-endif.


-define(new_INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, C5),
    ?CHECK_NODE_RECUR(?INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, C5))
).

-define(new_INTERNAL3(E1, E2, E3, C1, C2, C3, C4),
    ?CHECK_NODE_RECUR(?INTERNAL3(E1, E2, E3, C1, C2, C3, C4))
).

-define(new_INTERNAL2(E1, E2, C1, C2, C3),
    ?CHECK_NODE_RECUR(?INTERNAL2(E1, E2, C1, C2, C3))
).

-define(new_INTERNAL1(E1, C1, C2), ?CHECK_NODE(?INTERNAL1(E1, C1, C2))).

%

-define(new_LEAF4(E1, E2, E3, E4),
    ?CHECK_NODE_RECUR(?LEAF4(E1, E2, E3, E4))
).

-define(new_LEAF3(E1, E2, E3), ?CHECK_NODE_RECUR(?LEAF3(E1, E2, E3))).

-define(new_LEAF2(E1, E2), ?CHECK_NODE_RECUR(?LEAF2(E1, E2))).

-define(new_LEAF1(E1), ?CHECK_NODE(?LEAF1(E1))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(INTERNAL4_UPD_C1(UpdatedC1),
    ?new_INTERNAL4(E1, E2, E3, E4, UpdatedC1, C2, C3, C4, C5)
).
-define(INTERNAL4_UPD_C2(UpdatedC2),
    ?new_INTERNAL4(E1, E2, E3, E4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_UPD_C3(UpdatedC3),
    ?new_INTERNAL4(E1, E2, E3, E4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_UPD_C4(UpdatedC4),
    ?new_INTERNAL4(E1, E2, E3, E4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_UPD_C5(UpdatedC5),
    ?new_INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, UpdatedC5)
).

-define(INTERNAL3_UPD_C1(UpdatedC1), ?new_INTERNAL3(E1, E2, E3, UpdatedC1, C2, C3, C4)).
-define(INTERNAL3_UPD_C2(UpdatedC2), ?new_INTERNAL3(E1, E2, E3, C1, UpdatedC2, C3, C4)).
-define(INTERNAL3_UPD_C3(UpdatedC3), ?new_INTERNAL3(E1, E2, E3, C1, C2, UpdatedC3, C4)).
-define(INTERNAL3_UPD_C4(UpdatedC4), ?new_INTERNAL3(E1, E2, E3, C1, C2, C3, UpdatedC4)).

-define(INTERNAL2_UPD_C1(UpdatedC1), ?new_INTERNAL2(E1, E2, UpdatedC1, C2, C3)).
-define(INTERNAL2_UPD_C2(UpdatedC2), ?new_INTERNAL2(E1, E2, C1, UpdatedC2, C3)).
-define(INTERNAL2_UPD_C3(UpdatedC3), ?new_INTERNAL2(E1, E2, C1, C2, UpdatedC3)).

-define(INTERNAL1_UPD_C1(UpdatedC1), ?new_INTERNAL1(E1, UpdatedC1, C2)).
-define(INTERNAL1_UPD_C2(UpdatedC2), ?new_INTERNAL1(E1, C1, UpdatedC2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API

from_list(L) ->
    lists:foldl(fun insert/2, nil, L).

insert(Elem, Node) ->
    insert_recur(Elem, Node).

%% ------------------------------------------------------------------
%% Internal Function Definitions: insert_recur/2
%% ------------------------------------------------------------------

insert_recur(Elem, Node) ->
    case Node of
        ?INTERNAL4_MATCH_ALL ->
            ?GAP_SEARCH4(
               Elem,
               E1,
               E2,
               E3,
               E4,
               key_exists,
               key_exists,
               key_exists,
               key_exists,
               insert_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS),
               insert_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS),
               insert_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS),
               insert_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS),
               insert_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS)
              );
        %
        ?INTERNAL3_MATCH_ALL ->
            ?GAP_SEARCH3(
               Elem,
               E1,
               E2,
               E3,
               key_exists,
               key_exists,
               key_exists,
               insert_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS),
               insert_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS),
               insert_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS),
               insert_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS)
              );
        %
        ?INTERNAL2_MATCH_ALL ->
            ?GAP_SEARCH2(
               Elem,
               E1,
               E2,
               key_exists,
               key_exists,
               insert_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS),
               insert_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS),
               insert_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS)
              );
        %
        ?INTERNAL1_MATCH_ALL ->
            ?GAP_SEARCH1(
               Elem,
               E1,
               key_exists,
               insert_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS),
               insert_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS)
              );
        %
        ?LEAF4_MATCH_ALL ->
            ?GAP_SEARCH4(
               Elem,
               E1, E2, E3, E4,
               key_exists,
               key_exists,
               key_exists,
               key_exists,
               put_LEAF4_POS1(Elem, ?LEAF4_ARGS),
               put_LEAF4_POS2(Elem, ?LEAF4_ARGS),
               put_LEAF4_POS3(Elem, ?LEAF4_ARGS),
               put_LEAF4_POS4(Elem, ?LEAF4_ARGS),
               put_LEAF4_POS5(Elem, ?LEAF4_ARGS)
              );
        %
        ?LEAF3_MATCH_ALL ->
            ?GAP_SEARCH3(
               Elem,
               E1, E2, E3,
               key_exists,
               key_exists,
               key_exists,
               ?new_LEAF4(Elem, E1, E2, E3),
               ?new_LEAF4(E1, Elem, E2, E3),
               ?new_LEAF4(E1, E2, Elem, E3),
               ?new_LEAF4(E1, E2, E3, Elem)
              );
        %
        ?LEAF2_MATCH_ALL ->
            ?GAP_SEARCH2(
               Elem,
               E1, E2,
               key_exists,
               key_exists,
               ?new_LEAF3(Elem, E1, E2),
               ?new_LEAF3(E1, Elem, E2),
               ?new_LEAF3(E1, E2, Elem)
              );
        %
        ?LEAF1_MATCH_ALL ->
            ?GAP_SEARCH1(
               Elem,
               E1,
               key_exists,
               ?new_LEAF2(Elem, E1),
               ?new_LEAF2(E1, Elem)
              );
        %
        nil ->
            ?new_LEAF1(Elem)
    end.

%%%%%%%%%%%%
%% INTERNAL4
%%

-compile({inline, insert_INTERNAL4_C1/?INTERNAL4_ARITY_PLUS1}).
insert_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS) ->
    case insert(Elem, C1) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            split_internal(
              SplitE, E1, E2, E3, E4,
              %
              SplitL, SplitR, C2, C3, C4, C5);
        %
        UpdatedC1 ->
            ?INTERNAL4_UPD_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL4_C2/?INTERNAL4_ARITY_PLUS1}).
insert_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS) ->
    case insert(Elem, C2) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            split_internal(
              E1, SplitE, E2, E3, E4,
              %
              C1, SplitL, SplitR, C3, C4, C5);
        %
        UpdatedC2 ->
            ?INTERNAL4_UPD_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL4_C3/?INTERNAL4_ARITY_PLUS1}).
insert_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS) ->
    case insert(Elem, C3) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            split_internal(
              E1, E2, SplitE, E3, E4,
              %
              C1, C2, SplitL, SplitR, C4, C5);
        %
        UpdatedC3 ->
            ?INTERNAL4_UPD_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL4_C4/?INTERNAL4_ARITY_PLUS1}).
insert_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS) ->
    case insert(Elem, C4) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            split_internal(
              E1, E2, E3, SplitE, E4,
              %
              C1, C2, C3, SplitL, SplitR, C5);
        %
        UpdatedC4 ->
            ?INTERNAL4_UPD_C4(UpdatedC4)
    end.

-compile({inline, insert_INTERNAL4_C5/?INTERNAL4_ARITY_PLUS1}).
insert_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS) ->
    case insert(Elem, C5) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            split_internal(
              E1, E2, E3, E4, SplitE,
              %
              C1, C2, C3, C4, SplitL, SplitR);
        %
        UpdatedC5 ->
            ?INTERNAL4_UPD_C5(UpdatedC5)
    end.

%%%%%%%%%%%%
%% INTERNAL3
%%

-compile({inline, insert_INTERNAL3_C1/?INTERNAL3_ARITY_PLUS1}).
insert_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS) ->
    case insert(Elem, C1) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL4(
               SplitE, E1, E2, E3,
               %
               SplitL, SplitR, C2, C3, C4);
        %
        UpdatedC1 ->
            ?INTERNAL3_UPD_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL3_C2/?INTERNAL3_ARITY_PLUS1}).
insert_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS) ->
    case insert(Elem, C2) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL4(
               E1, SplitE, E2, E3,
               %
               C1, SplitL, SplitR, C3, C4);
        %
        UpdatedC2 ->
            ?INTERNAL3_UPD_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL3_C3/?INTERNAL3_ARITY_PLUS1}).
insert_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS) ->
    case insert(Elem, C3) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL4(
               E1, E2, SplitE, E3,
               %
               C1, C2, SplitL, SplitR, C4);
        %
        UpdatedC3 ->
            ?INTERNAL3_UPD_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL3_C4/?INTERNAL3_ARITY_PLUS1}).
insert_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS) ->
    case insert(Elem, C4) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL4(
               E1, E2, E3, SplitE,
               %
               C1, C2, C3, SplitL, SplitR);
        %
        UpdatedC4 ->
            ?INTERNAL3_UPD_C4(UpdatedC4)
    end.

%%%%%%%%%%%%
%% INTERNAL2
%%

-compile({inline, insert_INTERNAL2_C1/?INTERNAL2_ARITY_PLUS1}).
insert_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS) ->
    case insert(Elem, C1) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL3(
               SplitE, E1, E2,
               %
               SplitL, SplitR, C2, C3);
        %
        UpdatedC1 ->
            ?INTERNAL2_UPD_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL2_C2/?INTERNAL2_ARITY_PLUS1}).
insert_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS) ->
    case insert(Elem, C2) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL3(
               E1, SplitE, E2,
               %
               C1, SplitL, SplitR, C2);
        %
        UpdatedC2 ->
            ?INTERNAL2_UPD_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL2_C3/?INTERNAL2_ARITY_PLUS1}).
insert_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS) ->
    case insert(Elem, C3) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL3(
               E1, E2, SplitE,
               %
               C1, C2, SplitL, SplitR);
        %
        UpdatedC3 ->
            ?INTERNAL2_UPD_C3(UpdatedC3)
    end.

%%%%%%%%%%%%
%% INTERNAL1
%%

-compile({inline, insert_INTERNAL1_C1/?INTERNAL1_ARITY_PLUS1}).
insert_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    case insert(Elem, C1) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL2(
               SplitE, E1,
               %
               SplitL, SplitR, C2);
        %
        UpdatedC1 ->
            ?INTERNAL1_UPD_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL1_C2/?INTERNAL1_ARITY_PLUS1}).
insert_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS) ->
    case insert(Elem, C2) of
        key_exists ->
            key_exists;
        %
        ?SPLIT(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL2(
               E1, SplitE,
               %
               C1, SplitL, SplitR);
        %
        UpdatedC2 ->
            ?INTERNAL1_UPD_C2(UpdatedC2)
    end.

%%%%%%%%%%%%
%% LEAF4
%%

-compile({inline, put_LEAF4_POS1/?LEAF4_ARITY_PLUS1}).
put_LEAF4_POS1(Elem, ?LEAF4_ARGS) ->
    split_leaf(Elem, E1, E2, E3, E4).

-compile({inline, put_LEAF4_POS2/?LEAF4_ARITY_PLUS1}).
put_LEAF4_POS2(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, Elem, E2, E3, E4).

-compile({inline, put_LEAF4_POS3/?LEAF4_ARITY_PLUS1}).
put_LEAF4_POS3(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, Elem, E3, E4).

-compile({inline, put_LEAF4_POS4/?LEAF4_ARITY_PLUS1}).
put_LEAF4_POS4(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, E3, Elem, E4).

-compile({inline, put_LEAF4_POS5/?LEAF4_ARITY_PLUS1}).
put_LEAF4_POS5(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, E3, E4, Elem).

%%%

-compile({inline, split_internal/11}).
split_internal(E1, E2, E3, E4, E5, C1, C2, C3, C4, C5, C6) ->
    SplitE = E3,
    SplitL = ?new_INTERNAL2(E1, E2, C1, C2, C3),
    SplitR = ?new_INTERNAL2(E4, E5, C4, C5, C6),
    ?SPLIT(SplitE, SplitL, SplitR).

-compile({inline, split_leaf/5}).
split_leaf(E1, E2, E3, E4, E5) ->
    SplitE = E3,
    SplitL = ?new_LEAF2(E1, E2),
    SplitR = ?new_LEAF2(E4, E5),
    ?SPLIT(SplitE, SplitL, SplitR).
