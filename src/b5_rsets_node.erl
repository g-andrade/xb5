%% @doc Low-level API for working directly with B-tree nodes.
%%
%% This module provides a lower-level interface to B-tree operations that work
%% directly on tree nodes without tracking the overall tree size. This is useful
%% for performant re-wraps in languages like Elixir, where you might want to
%% wrap the tree structure under a struct instead of using Erlang records.
%%
%% The tree size is not tracked by these functions, making them more suitable
%% for cases where you need fine-grained control over the tree structure or
%% when integrating with external size tracking mechanisms.
%%
%% @reference https://en.wikipedia.org/wiki/B-tree
%% @reference https://www.geeksforgeeks.org/dsa/delete-operation-in-b-tree/
%%
%% Note regarding deletion: instead of preemptively rebalancing a node before
%% visiting it, the current implementation allows it to become temporarily
%% unbalanced. When this happens, the node is rebalanced immediately after.
%% This approach showed better performance and resulted in less code bloat.
-module(b5_rsets_node).

-export([
    test_fun/1,
    enter/2,
    insert/2,
    is_element/2,
    new/0,
    nth/2,
    size/1,
    to_list/1,
    union/2
]).

% -include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

% 13 elements
-define(INTERNAL4(E1, E2, E3, E4,
                  S1, S2, S3, S4, S5,
                  C1, C2, C3, C4, C5),
        {
         E1, E2, E3, E4,
         S1, S2, S3, S4, S5,
         C1, C2, C3, C4, C5
        }).

% 10 elements
-define(INTERNAL3(E1, E2, E3,
                  S1, S2, S3, S4,
                  C1, C2, C3, C4),
        {E1, E2, E3,
         S1, S2, S3, S4,
         C1, C2, C3, C4}).

% 7 elements
-define(INTERNAL2(E1, E2,
                  S1, S2, S3,
                  C1, C2, C3),
        {
         E1, E2,
         S1, S2, S3,
         C1, C2, C3}).

% 5 elements
-define(INTERNAL1(E1,
                  S1, S2,
                  C1, C2),
        {E1,
         S1, S2,
         C1, C2}).

% 4 elements
-define(LEAF4(E1, E2, E3, E4), {E1, E2, E3, E4}).

% 3 elements
-define(LEAF3(E1, E2, E3), {E1, E2, E3}).

% improper list
-define(LEAF2(E1, E2), [E1 | E2]).

% 1 element
-define(LEAF1(E1), {E1}).

-define(LEAF0, leaf0).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-opaque t(Element) :: root_only_node(Element) | deep_node(Element).
%% This type represents any tree node structure.
-export_type([t/1]).

-type empty_node() :: ?LEAF0.

%
% The fact that some node types are root-only allows us to optimize the
% recursive case - less potential patterns to match.
%
% Therefore, most of the exported API functions here match on the root-only
% nodes in their entry clauses, but only on deep node types after that.
%

-type root_only_node(Element) ::
    (node_internal1(Element)
    | node_leaf1(Element)
    | empty_node()).

-type deep_node(Element) ::
    (node_internal4(Element)
    | node_internal3(Element)
    | node_internal2(Element)
    | node_leaf4(Element)
    | node_leaf3(Element)
    | node_leaf2(Element)).

-type non_empty_node(Element) ::
    (node_internal1(Element)
    | node_leaf1(Element)
    | deep_node(Element)).

-type node_internal4(Element) ::
    (?INTERNAL4(
        Element,
        Element,
        Element,
        Element,
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        deep_node(Element),
        deep_node(Element),
        deep_node(Element),
        deep_node(Element),
        deep_node(Element)
    )).

-type node_internal3(Element) ::
    (?INTERNAL3(
        Element,
        Element,
        Element,
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
        deep_node(Element),
        deep_node(Element),
        deep_node(Element),
        deep_node(Element)
    )).

-type node_internal2(Element) ::
    (?INTERNAL2(
        Element,
        Element,
        pos_integer(),
        pos_integer(),
        pos_integer(),
        deep_node(Element),
        deep_node(Element),
        deep_node(Element)
    )).

-type node_internal1(Element) ::
    (?INTERNAL1(
        Element,
        pos_integer(),
        pos_integer(),
        deep_node(Element),
        deep_node(Element)
    )).

-type node_leaf4(Element) ::
    (?LEAF4(
        Element,
        Element,
        Element,
        Element
    )).

-type node_leaf3(Element) ::
    (?LEAF3(
        Element,
        Element,
        Element
    )).

%-type node_leaf2(Element) :: ?LEAF2(Element).
-type node_leaf2(Element) :: nonempty_improper_list(Element, Element).

-type node_leaf1(Element) :: ?LEAF1(Element).

%%%%%%%%%%%

% % Dialyzer got too smart when it reasoned this, but it is indeed true.
% -type node_after_deletion(Element) ::
%     node_internal3(Element)
%     | node_internal2(Element)
%     | node_internal1(Element)
%     | node_leaf2(Element)
%     | node_leaf1(Element).
%
% -type deep_node_after_insertion(Element) ::
%     node_internal4(Element)
%     | node_internal3(Element)
%     | node_leaf4(Element)
%     | node_leaf3(Element).
%
% % Temporary situation before rebalance
% -type unbalanced_node(Element) :: node_internal1(Element).
%
% %%%%%%%%%%%
%
% -type split_result(Element) :: internal_split_result(Element) | leaf_split_result(Element).
%
% -type internal_split_result(Element) :: split_result(
%     Element, node_internal2(Element), node_internal2(Element)
% ).
%
% -type leaf_split_result(Element) :: split_result(
%     Element, node_leaf2(Element), node_leaf2(Element)
% ).
%
% -type split_result(Element, SplitL, SplitR) :: {split, Element, SplitL, SplitR}.

%%%%%%%%%%%

-opaque iter(Element) :: forward_iter(Element) | reverse_iter(Element).
-export_type([iter/1]).

-record(b5_rsets_forward_iter, {steps :: [iterator_step(_)]}).
-type forward_iter(Element) :: #b5_rsets_forward_iter{steps :: [iterator_step(Element)]}.

-record(b5_rsets_reverse_iter, {steps :: [iterator_step(_)]}).
-type reverse_iter(Element) :: #b5_rsets_reverse_iter{steps :: [iterator_step(Element)]}.

-type iterator_step(Element) :: {Element} | {Element, NextChild :: deep_node(Element)}.

%%%%%%%%%%%

-type valid_stats() :: #{
    height := non_neg_integer(),
    node_counts := stats_node_counts()
}.
-export_type([valid_stats/0]).

-type stats_node_counts() :: #{
    internal4 => pos_integer(),
    internal3 => pos_integer(),
    internal2 => pos_integer(),
    internal1 => pos_integer(),
    leaf4 => pos_integer(),
    leaf3 => pos_integer(),
    leaf2 => pos_integer(),
    leaf1 => pos_integer()
}.
-export_type([stats_node_counts/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

test_fun(List) ->
    lists:foldl(
      fun insert/2,
      ?LEAF0,
      List).

enter(Element, Tree) ->
    try
        insert(Element, Tree)
    catch
        error:{element_exists, E} when E =:= Element ->
            Tree
    end.

-spec insert(Element, t(Element)) -> t(Element).
insert(Element, ?INTERNAL1(E1, S1, S2, C1, C2)) ->
    insert_internal1(Element, E1, S1, S2, C1, C2);
insert(Element, ?LEAF1(E1)) ->
    insert_leaf1(Element, E1);
insert(Element, ?LEAF0) ->
    ?LEAF1(Element);
insert(Element, Node) ->
    case insert_recur(Element, Node) of
        {split, E1, S1, S2, C1, C2} ->
            internal1(E1, S1, S2, C1, C2);

        UpdatedNode ->
            UpdatedNode
    end.

-spec is_element(Element, t(Element)) -> boolean().
is_element(Element, ?INTERNAL1(E1, _, _, C1, C2)) ->
    is_element_internal1(Element, E1, C1, C2);
is_element(Element, ?LEAF1(E1)) ->
    Element == E1;
is_element(_, ?LEAF0) ->
    false;
is_element(Element, Node) ->
    is_element_recur(Element, Node).

new() ->
    ?LEAF0.

nth(N, Node) when is_integer(N) andalso N > 0 ->
    Low = 1,
    High = node_size(Node),

    if
        N > High ->
            error_badarg(N);

        true ->
            nth_valid(N, Low, High, Node)
    end.

-spec size(t(_)) -> non_neg_integer().
size(Node) ->
    node_size(Node).

-spec to_list(t(Element)) -> [Element].
to_list(?INTERNAL1(E1, _, _, C1, C2)) ->
    Acc = [E1 | to_list_recur(C2, [])],
    to_list_recur(C1, Acc);
to_list(?LEAF1(E1)) ->
    [E1];
to_list(?LEAF0) ->
    0;
to_list(Node) ->
    to_list_recur(Node, []).

-spec union(t(Element1), t(Element2)) -> t(Element1 | Element2).
union(Set1, Set2) ->
    case node_size(Set2) < node_size(Set1) of
        true ->
            union_from_smallest(Set2, Set1);

        _ ->
            union_from_smallest(Set1, Set2)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: constructors
%% ------------------------------------------------------------------

-compile({inline, internal4/14}).
internal4(E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5) ->
    % ?assertMatch(_ when is_integer(E1), E1),
    % ?assertMatch(_ when is_integer(E2), E2),
    % ?assertMatch(_ when is_integer(E3), E3),
    % ?assertMatch(_ when is_integer(E4), E4),

    % ?assertMatch(_ when is_integer(S1) andalso S1 > 0, S1),
    % ?assertMatch(_ when is_integer(S2) andalso S2 > 0, S2),
    % ?assertMatch(_ when is_integer(S3) andalso S3 > 0, S3),
    % ?assertMatch(_ when is_integer(S4) andalso S4 > 0, S4),
    % ?assertMatch(_ when is_integer(S5) andalso S5 > 0, S5),

    % ?assertMatch(DNS1 when DNS1 > 0, deep_node_size(C1)),
    % ?assertMatch(DNS2 when DNS2 > 0, deep_node_size(C2)),
    % ?assertMatch(DNS3 when DNS3 > 0, deep_node_size(C3)),
    % ?assertMatch(DNS4 when DNS4 > 0, deep_node_size(C4)),
    % ?assertMatch(DNS5 when DNS5 > 0, deep_node_size(C5)),

    % ?assertMatch({C1L, C2S} when C1L < E1 andalso E1 < C2S, {largest(C1), smallest(C2)}),
    % ?assertMatch({C2L, C3S} when C2L < E2 andalso E2 < C3S, {largest(C2), smallest(C3)}),
    % ?assertMatch({C3L, C4S} when C3L < E3 andalso E3 < C4S, {largest(C3), smallest(C4)}),
    % ?assertMatch({C4L, C5S} when C4L < E4 andalso E4 < C5S, {largest(C4), smallest(C5)}),

    ?INTERNAL4(
       E1, E2, E3, E4,
       S1, S2, S3, S4, S5,
       C1, C2, C3, C4, C5).

-compile({inline, internal3/11}).
internal3(E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4) ->
    % ?assertMatch(_ when is_integer(E1), E1),
    % ?assertMatch(_ when is_integer(E2), E2),
    % ?assertMatch(_ when is_integer(E3), E3),

    % ?assertMatch(_ when is_integer(S1) andalso S1 > 0, S1),
    % ?assertMatch(_ when is_integer(S2) andalso S2 > 0, S2),
    % ?assertMatch(_ when is_integer(S3) andalso S3 > 0, S3),
    % ?assertMatch(_ when is_integer(S4) andalso S4 > 0, S4),

    % ?assertMatch(DNS1 when DNS1 > 0, deep_node_size(C1)),
    % ?assertMatch(DNS2 when DNS2 > 0, deep_node_size(C2)),
    % ?assertMatch(DNS3 when DNS3 > 0, deep_node_size(C3)),
    % ?assertMatch(DNS4 when DNS4 > 0, deep_node_size(C4)),

    % ?assertMatch({C1L, C2S} when C1L < E1 andalso E1 < C2S, {largest(C1), smallest(C2)}),
    % ?assertMatch({C2L, C3S} when C2L < E2 andalso E2 < C3S, {largest(C2), smallest(C3)}),
    % ?assertMatch({C3L, C4S} when C3L < E3 andalso E3 < C4S, {largest(C3), smallest(C4)}),

    ?INTERNAL3(
       E1, E2, E3,
       S1, S2, S3, S4,
       C1, C2, C3, C4).

-compile({inline, internal2/8}).
internal2(E1, E2, S1, S2, S3, C1, C2, C3) ->
    % ?assertMatch(_ when is_integer(E1), E1),
    % ?assertMatch(_ when is_integer(E2), E2),

    % ?assertMatch(_ when is_integer(S1) andalso S1 > 0, S1),
    % ?assertMatch(_ when is_integer(S2) andalso S2 > 0, S2),
    % ?assertMatch(_ when is_integer(S3) andalso S3 > 0, S3),

    % ?assertMatch(DNS1 when DNS1 > 0, deep_node_size(C1)),
    % ?assertMatch(DNS2 when DNS2 > 0, deep_node_size(C2)),
    % ?assertMatch(DNS3 when DNS3 > 0, deep_node_size(C3)),

    % ?assertMatch({C1L, C2S} when C1L < E1 andalso E1 < C2S, {largest(C1), smallest(C2)}),
    % ?assertMatch({C2L, C3S} when C2L < E2 andalso E2 < C3S, {largest(C2), smallest(C3)}),

    ?INTERNAL2(
       E1, E2,
       S1, S2, S3,
       C1, C2, C3).

-compile({inline, internal1/5}).
internal1(E1, S1, S2, C1, C2) ->
    % ?assertMatch(_ when is_integer(E1), E1),

    % ?assertMatch(_ when is_integer(S1) andalso S1 > 0, S1),
    % ?assertMatch(_ when is_integer(S2) andalso S2 > 0, S2),

    % ?assertMatch(DNS1 when DNS1 > 0, deep_node_size(C1)),
    % ?assertMatch(DNS2 when DNS2 > 0, deep_node_size(C2)),

    % ?assertMatch({C1L, C2S} when C1L < E1 andalso E1 < C2S, {largest(C1), smallest(C2)}),

    ?INTERNAL1(
       E1,
       S1, S2,
       C1, C2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: insert
%% ------------------------------------------------------------------

insert_recur(Element, ?INTERNAL4(E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5)) ->
    insert_internal4(Element,
                     E1, E2, E3, E4,
                     S1, S2, S3, S4, S5,
                     C1, C2, C3, C4, C5
                    );
insert_recur(Element, ?INTERNAL3(E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4)) ->
    insert_internal3(Element,
                     E1, E2, E3,
                     S1, S2, S3, S4,
                     C1, C2, C3, C4);
insert_recur(Element, ?INTERNAL2(E1, E2, S1, S2, S3, C1, C2, C3)) ->
    insert_internal2(Element,
                     E1, E2,
                     S1, S2, S3,
                     C1, C2, C3);
insert_recur(Element, ?LEAF4(E1, E2, E3, E4)) ->
    insert_leaf4(Element, E1, E2, E3, E4);
insert_recur(Element, ?LEAF3(E1, E2, E3)) ->
    insert_leaf3(Element, E1, E2, E3);
insert_recur(Element, ?LEAF2(E1, E2)) ->
    insert_leaf2(Element, E1, E2).

%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal4/15]}).
insert_internal4(Element, E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5) ->
    if
        Element > E2 ->
            if
                Element < E4 ->
                    if
                        Element > E3 ->
                            insert_internal4_child4(Element, E1, E2, E3, E4,
                                                    S1, S2, S3, S4, S5,
                                                    C1, C2, C3, C4, C5);

                        Element < E3 ->
                            insert_internal4_child3(Element, E1, E2, E3, E4,
                                                    S1, S2, S3, S4, S5,
                                                    C1, C2, C3, C4, C5);

                        true ->
                            error_element_exists(Element)
                    end;

                Element > E4 ->
                    insert_internal4_child5(Element, E1, E2, E3, E4,
                                            S1, S2, S3, S4, S5,
                                            C1, C2, C3, C4, C5);

                true ->
                    error_element_exists(Element)
            end;

        Element < E2 ->
            if
                Element > E1 ->
                    insert_internal4_child2(Element, E1, E2, E3, E4,
                                            S1, S2, S3, S4, S5,
                                            C1, C2, C3, C4, C5);

                Element < E1 ->
                    insert_internal4_child1(Element, E1, E2, E3, E4,
                                            S1, S2, S3, S4, S5,
                                            C1, C2, C3, C4, C5);

                true ->
                    error_element_exists(Element)
            end;

        true ->
            error_element_exists(Element)
    end.

-compile({inline, [insert_internal4_child1/15]}).
insert_internal4_child1(Element, E1, E2, E3, E4,
                        S1, S2, S3, S4, S5,
                        C1, C2, C3, C4, C5) ->
    case insert_recur(Element, C1) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal_split(
              SplitE, E1, E2, E3, E4,
              SizeL, SizeR, S2, S3, S4, S5,
              SplitL, SplitR, C2, C3, C4, C5);

        UpdatedC1 ->
            internal4(
               E1, E2, E3, E4,
               S1 + 1, S2, S3, S4, S5,
               UpdatedC1, C2, C3, C4, C5)
    end.

-compile({inline, [insert_internal4_child2/15]}).
insert_internal4_child2(Element, E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5) ->
    case insert_recur(Element, C2) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal_split(
              E1, SplitE, E2, E3, E4,
              S1, SizeL, SizeR, S3, S4, S5,
              C1, SplitL, SplitR, C3, C4, C5);

        UpdatedC2 ->
            internal4(
               E1, E2, E3, E4,
               S1, S2 + 1, S3, S4, S5,
               C1, UpdatedC2, C3, C4, C5)
    end.

-compile({inline, [insert_internal4_child3/15]}).
insert_internal4_child3(Element, E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5) ->
    case insert_recur(Element, C3) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal_split(
              E1, E2, SplitE, E3, E4,
              S1, S2, SizeL, SizeR, S4, S5,
              C1, C2, SplitL, SplitR, C4, C5);

        UpdatedC3 ->
            internal4(
               E1, E2, E3, E4,
               S1, S2, S3 + 1, S4, S5,
               C1, C2, UpdatedC3, C4, C5)
    end.

-compile({inline, [insert_internal4_child4/15]}).
insert_internal4_child4(Element, E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5) ->
    case insert_recur(Element, C4) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal_split(
              E1, E2, E3, SplitE, E4,
              S1, S2, S3, SizeL, SizeR, S5,
              C1, C2, C3, SplitL, SplitR, C5);

        UpdatedC4 ->
            internal4(
               E1, E2, E3, E4,
               S1, S2, S3, S4 + 1, S5,
               C1, C2, C3, UpdatedC4, C5)
    end.

-compile({inline, [insert_internal4_child5/15]}).
insert_internal4_child5(Element, E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5) ->
    case insert_recur(Element, C5) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal_split(
              E1, E2, E3, E4, SplitE,
              S1, S2, S3, S4, SizeL, SizeR,
              C1, C2, C3, C4, SplitL, SplitR);

        UpdatedC5 ->
            internal4(
               E1, E2, E3, E4,
               S1, S2, S3, S4, S5 + 1,
               C1, C2, C3, C4, UpdatedC5)
    end.

%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal3/12]}).
insert_internal3(Element, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4) ->
    if
        Element > E2 ->
            if
                Element < E3 ->
                    insert_internal3_child3(Element, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4);

                Element > E3 ->
                    insert_internal3_child4(Element, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4);

                true ->
                    error_element_exists(Element)
            end;

        Element < E2 ->
            if
                Element < E1 ->
                    insert_internal3_child1(Element, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4);

                Element > E1 ->
                    insert_internal3_child2(Element, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4);

                true ->
                    error_element_exists(Element)
            end;

        true ->
            error_element_exists(Element)
    end.

-compile({inline, [insert_internal3_child1/12]}).
insert_internal3_child1(Element, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4) ->
    case insert_recur(Element, C1) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal4(
               SplitE, E1, E2, E3,
               SizeL, SizeR, S2, S3, S4,
               SplitL, SplitR, C2, C3, C4);

        UpdatedC1 ->
            internal3(
               E1, E2, E3,
               S1 + 1, S2, S3, S4,
               UpdatedC1, C2, C3, C4)
    end.

-compile({inline, [insert_internal3_child2/12]}).
insert_internal3_child2(Element, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4) ->
    case insert_recur(Element, C2) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal4(
               E1, SplitE, E2, E3,
               S1, SizeL, SizeR, S3, S4,
               C1, SplitL, SplitR, C3, C4);

        UpdatedC2 ->
            internal3(
               E1, E2, E3,
               S1, S2 + 1, S3, S4,
               C1, UpdatedC2, C3, C4)
    end.

-compile({inline, [insert_internal3_child3/12]}).
insert_internal3_child3(Element, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4) ->
    case insert_recur(Element, C3) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal4(
               E1, E2, SplitE, E3,
               S1, S2, SizeL, SizeR, S4,
               C1, C2, SplitL, SplitR, C4);

        UpdatedC3 ->
            internal3(
               E1, E2, E3,
               S1, S2, S3 + 1, S4,
               C1, C2, UpdatedC3, C4)
    end.

-compile({inline, [insert_internal3_child4/12]}).
insert_internal3_child4(Element, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4) ->
    case insert_recur(Element, C4) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal4(
               E1, E2, E3, SplitE,
               S1, S2, S3, SizeL, SizeR,
               C1, C2, C3, SplitL, SplitR);

        UpdatedC4 ->
            internal3(
               E1, E2, E3,
               S1, S2, S3, S4 + 1,
               C1, C2, C3, UpdatedC4)
    end.

%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal2/9]}).
insert_internal2(Element, E1, E2, S1, S2, S3, C1, C2, C3) ->
    if
        Element > E1 ->
            if
                Element < E2 ->
                    insert_internal2_child2(Element, E1, E2, S1, S2, S3, C1, C2, C3);

                Element > E2 ->
                    insert_internal2_child3(Element, E1, E2, S1, S2, S3, C1, C2, C3);

                true ->
                    error_element_exists(Element)
            end;

        Element < E1 ->
            insert_internal2_child1(Element, E1, E2, S1, S2, S3, C1, C2, C3);

        true ->
            error_element_exists(Element)
    end.

-compile({inline, [insert_internal2_child1/9]}).
insert_internal2_child1(Element, E1, E2, S1, S2, S3, C1, C2, C3) ->
    case insert_recur(Element, C1) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal3(
               SplitE, E1, E2,
               SizeL, SizeR, S2, S3,
               SplitL, SplitR, C2, C3);

        UpdatedC1 ->
            internal2(
               E1, E2,
               S1 + 1, S2, S3,
               UpdatedC1, C2, C3)
    end.

-compile({inline, [insert_internal2_child2/9]}).
insert_internal2_child2(Element, E1, E2, S1, S2, S3, C1, C2, C3) ->
    case insert_recur(Element, C2) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal3(
               E1, SplitE, E2,
               S1, SizeL, SizeR, S3,
               C1, SplitL, SplitR, C3);

        UpdatedC2 ->
            internal2(
               E1, E2,
               S1, S2 + 1, S3,
               C1, UpdatedC2, C3)
    end.

-compile({inline, [insert_internal2_child3/9]}).
insert_internal2_child3(Element, E1, E2, S1, S2, S3, C1, C2, C3) ->
    case insert_recur(Element, C3) of
        {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
            internal3(
               E1, E2, SplitE,
               S1, S2, SizeL, SizeR,
               C1, C2, SplitL, SplitR);

        UpdatedC3 ->
            internal2(
               E1, E2,
               S1, S2, S3 + 1,
               C1, C2, UpdatedC3)
    end.

%%%%%%%%%%%%%%%%%%%%%

-compile({inline, [insert_internal1/6]}).
insert_internal1(Element, E1, S1, S2, C1, C2) ->
    if
        Element < E1 ->
            case insert_recur(Element, C1) of
                {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
                    internal2(
                       SplitE, E1,
                       SizeL, SizeR, S2,
                       SplitL, SplitR, C2);

                UpdatedC1 ->
                    internal1(
                       E1,
                       S1 + 1, S2,
                       UpdatedC1, C2)
            end;

        Element > E1 ->
            case insert_recur(Element, C2) of
                {split, SplitE, SizeL, SizeR, SplitL, SplitR} ->
                    internal2(
                       E1, SplitE,
                       S1, SizeL, SizeR,
                       C1, SplitL, SplitR);

                UpdatedC2 ->
                    internal1(
                       E1,
                       S1, S2 + 1,
                       C1, UpdatedC2)
            end;

        true ->
            error_element_exists(Element)
    end.

-compile({inline, [insert_leaf4/5]}).
insert_leaf4(Element, E1, E2, E3, E4) ->
    if
        Element > E2 ->
            if
                Element < E4 ->
                    if
                        Element > E3 ->
                            leaf_split(E1, E2, E3, Element, E4);

                        Element < E3 ->
                            leaf_split(E1, E2, Element, E3, E4);

                        true ->
                            error_element_exists(Element)
                    end;

                Element > E4 ->
                    leaf_split(E1, E2, E3, E4, Element);

                true ->
                    error_element_exists(Element)
            end;

        Element < E2 ->
            if
                Element > E1 ->
                    leaf_split(E1, Element, E2, E3, E4);

                Element < E1 ->
                    leaf_split(Element, E1, E2, E3, E4);

                true ->
                    error_element_exists(Element)
            end;

        true ->
            error_element_exists(Element)
    end.

-compile({inline, [insert_leaf3/4]}).
insert_leaf3(Element, E1, E2, E3) ->
    if
        Element < E2 ->
            if
                Element < E1 ->
                    ?LEAF4(Element, E1, E2, E3);

                Element > E1 ->
                    ?LEAF4(E1, Element, E2, E3);

                true ->
                    error_element_exists(Element)
            end;

        Element > E2 ->
            if
                Element < E3 ->
                    ?LEAF4(E1, E2, Element, E3);

                Element > E3 ->
                    ?LEAF4(E1, E2, E3, Element);

                true ->
                    error_element_exists(Element)
            end
    end.

-compile({inline, [insert_leaf2/3]}).
insert_leaf2(Element, E1, E2) ->
    if
        Element < E2 ->
            if
                Element > E1 ->
                    ?LEAF3(E1, Element, E2);

                Element < E1 ->
                    ?LEAF3(Element, E1, E2);

                true ->
                    error_element_exists(Element)
            end;

        Element > E2 ->
            ?LEAF3(E1, E2, Element);

        true ->
            error_element_exists(Element)
    end.


-compile({inline, [insert_leaf1/2]}).
insert_leaf1(Element, E1) ->
    if
        Element < E1 ->
            ?LEAF2(Element, E1);

        Element > E1 ->
            ?LEAF2(E1, Element);

        true ->
            error_element_exists(Element)
    end.

-compile({inline, [error_element_exists/1]}).
error_element_exists(Element) ->
    error({element_exists, Element}).

-compile({inline, [internal_split/17]}).
internal_split(E1, E2, E3, E4, E5,
               S1, S2, S3, S4, S5, S6,
               C1, C2, C3, C4, C5, C6) ->
    SplitE = E3,

    SizeL = 2 + S1 + S2 + S3,
    SplitL = internal2(E1, E2, S1, S2, S3, C1, C2, C3),

    SizeR = 2 + S4 + S5 + S6,
    SplitR = internal2(E4, E5, S4, S5, S6, C4, C5, C6),

    % ?assertMatch({C1L, C2S} when C1L < E1 andalso E1 < C2S, {largest(C1), smallest(C2)}),
    % ?assertMatch({C2L, C3S} when C2L < E2 andalso E2 < C3S, {largest(C2), smallest(C3)}),
    % ?assertMatch({C3L, C4S} when C3L < E3 andalso E3 < C4S, {largest(C3), smallest(C4)}),
    % ?assertMatch({C4L, C5S} when C4L < E4 andalso E4 < C5S, {largest(C4), smallest(C5)}),
    % ?assertMatch({C5L, C6S} when C5L < E5 andalso E5 < C6S, {largest(C5), smallest(C6)}),

    {split, SplitE, SizeL, SizeR, SplitL, SplitR}.

-compile({inline, [leaf_split/5]}).
leaf_split(E1, E2, E3, E4, E5) ->
    SplitE = E3,

    SizeL = 2,
    SplitL = ?LEAF2(E1, E2),

    SizeR = 2,
    SplitR = ?LEAF2(E4, E5),

    % ?assertMatch({C1L, C2S} when C1L < SplitE andalso SplitE < C2S,
    %                              {largest(SplitL), smallest(SplitR)}),

    {split, SplitE, SizeL, SizeR, SplitL, SplitR}.

node_size(?INTERNAL1(_, S1, S2, _, _)) ->
    1 + S1 + S2;
node_size(?LEAF1(_)) ->
    1;
node_size(?LEAF0) ->
    0;
node_size(Node) ->
    deep_node_size(Node).

deep_node_size(?INTERNAL4(_, _, _, _, S1, S2, S3, S4, S5, _, _, _, _, _)) ->
    4 + S1 + S2 + S3 + S4 + S5;
deep_node_size(?INTERNAL3(_, _, _, S1, S2, S3, S4, _, _, _, _)) ->
    3 + S1 + S2 + S3 + S4;
deep_node_size(?INTERNAL2(_, _, S1, S2, S3, _, _, _)) ->
    2 + S1 + S2 + S3;
deep_node_size(?LEAF4(_, _, _, _)) -> 4;
deep_node_size(?LEAF3(_, _, _)) -> 3;
deep_node_size(?LEAF2(_, _)) -> 2.

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_element
%% ------------------------------------------------------------------

is_element_recur(Element, ?INTERNAL4(E1, E2, E3, E4, _, _, _, _, _, C1, C2, C3, C4, C5)) ->
    if
        Element > E2 ->
            is_element_internal2(Element, E3, E4, C3, C4, C5);

        Element < E2 ->
            is_element_internal1(Element, E1, C1, C2);

        true ->
            true
    end;
is_element_recur(Element, ?INTERNAL3(E1, E2, E3, _, _, _, _, C1, C2, C3, C4)) ->
    if
        Element > E2 ->
            is_element_internal1(Element, E3, C3, C4);

        Element < E2 ->
            is_element_internal1(Element, E1, C1, C2);

        true ->
            true
    end;
is_element_recur(Element, ?INTERNAL2(E1, E2, _, _, _, C1, C2, C3)) ->
    is_element_internal2(Element, E1, E2, C1, C2, C3);
is_element_recur(Element, ?LEAF4(E1, E2, E3, E4)) ->
    if
        Element > E2 ->
            Element == E3 orelse Element == E4;

        Element < E2 ->
            Element == E1;

        true ->
            true
    end;
is_element_recur(Element, ?LEAF3(E1, E2, E3)) ->
    if
        Element > E2 ->
            Element == E3;

        Element < E2 ->
            Element == E1;

        true ->
            true
    end;
is_element_recur(Element, ?LEAF2(E1, E2)) ->
    Element == E1 orelse Element == E2.

-compile({inline, [is_element_internal2/6]}).
is_element_internal2(Element, E1, E2, C1, C2, C3) ->
    if
        Element > E1 ->
            is_element_internal1(Element, E2, C2, C3);

        Element < E1 ->
            is_element_recur(Element, C1);

        true ->
            true
    end.

-compile({inline, [is_element_internal1/4]}).
is_element_internal1(Element, E1, C1, C2) ->
    if
        Element < E1 ->
            is_element_recur(Element, C1);

        Element > E1 ->
            is_element_recur(Element, C2);

        true ->
            true
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: nth
%% ------------------------------------------------------------------

nth_valid(N, Low, High, ?INTERNAL1(E1, S1, S2, C1, C2)) ->
    nth_internal1(N, Low, High, E1, S1, S2, C1, C2);
nth_valid(_, _, _, ?LEAF1(E1)) ->
    E1;
nth_valid(N, Low, High, Node) ->
    nth_recur(N, Low, High, Node).

nth_recur(N, Low, High, ?INTERNAL4(E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5)) ->
    nth_internal4(N, Low, High, E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5);
nth_recur(N, Low, High, ?INTERNAL3(E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4)) ->
    nth_internal3(N, Low, High, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4);
nth_recur(N, Low, High, ?INTERNAL2(E1, E2, S1, S2, S3, C1, C2, C3)) ->
    nth_internal2(N, Low, High, E1, E2, S1, S2, S3, C1, C2, C3);
nth_recur(N, Low, High, ?LEAF4(E1, E2, E3, E4)) ->
    if
        N =:= Low ->
            E1;

        N =:= High ->
            E4;

        N =:= High - 1 ->
            E3;

        % N == Low + 1 ->
        true ->
            E2
    end;
nth_recur(N, Low, High, ?LEAF3(E1, E2, E3)) ->
    if
        N =:= Low ->
            E1;

        N =:= High ->
            E3;

        % N == Low + 1 ->
        true ->
            E2
    end;
nth_recur(N, Low, _, ?LEAF2(E1, E2)) ->
    if
        N =:= Low ->
            E1;

        % N == High ->
        true ->
            E2
    end.

-compile({inline, nth_internal4/17}).
nth_internal4(N, Low, High, E1, E2, E3, E4, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5) ->
    % ?assert(Low < High),
    % ?assertEqual(High - Low, S1 + S2 + S3 + S4 + S5 + 3),

    Mid2 = Low + S1 + S2 + 1,

    if
        N < Mid2 ->
            NewHigh = Mid2 - 1,
            nth_internal1(N, Low, NewHigh, E1, S1, S2, C1, C2);

        N > Mid2 ->
            NewLow = Mid2 + 1,
            nth_internal2(N, NewLow, High, E3, E4, S3, S4, S5, C3, C4, C5);

        true ->
            E2
    end.

-compile({inline, nth_internal3/14}).
nth_internal3(N, Low, High, E1, E2, E3, S1, S2, S3, S4, C1, C2, C3, C4) ->
    % ?assert(Low < High),
    % ?assertEqual(High - Low, S1 + S2 + S3 + S4 + 2),

    Mid2 = Low + S1 + S2 + 1,

    if
        N < Mid2 ->
            NewHigh = Mid2 - 1,
            nth_internal1(N, Low, NewHigh, E1, S1, S2, C1, C2);

        N > Mid2 ->
            NewLow = Mid2 + 1,
            nth_internal1(N, NewLow, High, E3, S3, S4, C3, C4);

        true ->
            E2
    end.

-compile({inline, nth_internal2/11}).
nth_internal2(N, Low, High, E1, E2, S1, S2, _, C1, C2, C3) ->
    % ?assert(Low < High),
    % ?assertEqual(High - Low, S1 + S2 + S3 + 1),

    Mid1 = Low + S1,

    if
        N > Mid1 ->
            Mid2 = Mid1 + S2 + 1,

            if
                N < Mid2 ->
                    NewLow = Mid1 + 1,
                    NewHigh = Mid2 - 1,
                    nth_recur(N, NewLow, NewHigh, C2);

                N > Mid2 ->
                    NewLow = Mid2 + 1,
                    nth_recur(N, NewLow, High, C3);

                true ->
                    E2
            end;

        N < Mid1 ->
            NewHigh = Mid1 - 1,
            nth_recur(N, Low, NewHigh, C1);

        true ->
            E1
    end.

-compile({inline, nth_internal1/8}).
nth_internal1(N, Low, High, E1, S1, _, C1, C2) ->
    % ?assert(Low < High),
    % ?assertEqual(High - Low, S1 + S2),

    Mid = Low + S1,

    if
        N < Mid ->
            NewHigh = Mid - 1,
            nth_recur(N, Low, NewHigh, C1);

        N > Mid ->
            NewLow = Mid + 1,
            nth_recur(N, NewLow, High, C2);

        true ->
            E1
    end.

error_badarg(V) ->
    error({badarg, V}).

%% ------------------------------------------------------------------
%% Internal Function Definitions: packed size operations
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Internal Function Definitions: to_list
%% ------------------------------------------------------------------

to_list_recur(?INTERNAL4(E1, E2, E3, E4, _, _, _, _, _, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = [E4 | to_list_recur(C5, Acc)],
    Acc3 = [E3 | to_list_recur(C4, Acc2)],
    Acc4 = [E2 | to_list_recur(C3, Acc3)],
    Acc5 = [E1 | to_list_recur(C2, Acc4)],
    to_list_recur(C1, Acc5);
to_list_recur(?INTERNAL3(E1, E2, E3, _, _, _, _, C1, C2, C3, C4), Acc) ->
    Acc2 = [E3 | to_list_recur(C4, Acc)],
    Acc3 = [E2 | to_list_recur(C3, Acc2)],
    Acc4 = [E1 | to_list_recur(C2, Acc3)],
    to_list_recur(C1, Acc4);
to_list_recur(?INTERNAL2(E1, E2, _, _, _, C1, C2, C3), Acc) ->
    Acc2 = [E2 | to_list_recur(C3, Acc)],
    Acc3 = [E1 | to_list_recur(C2, Acc2)],
    to_list_recur(C1, Acc3);
to_list_recur(?LEAF4(E1, E2, E3, E4), Acc) ->
    [E1, E2, E3, E4 | Acc];
to_list_recur(?LEAF3(E1, E2, E3), Acc) ->
    [E1, E2, E3 | Acc];
to_list_recur(?LEAF2(E1, E2), Acc) ->
    [E1, E2 | Acc].

%%%%%%%%%%%%%

% smallest(?INTERNAL4(_, _, _, _, _, _, _, _, _, C1, _, _, _, _)) ->
%     smallest(C1);
% smallest(?INTERNAL3(_, _, _, _, _, _, _, C1, _, _, _)) ->
%     smallest(C1);
% smallest(?INTERNAL2(_, _, _, _, _, C1, _, _)) ->
%     smallest(C1);
% smallest(?LEAF4(E1, _, _, _)) ->
%     E1;
% smallest(?LEAF3(E1, _, _)) ->
%     E1;
% smallest(?LEAF2(E1, _)) ->
%     E1.
% 
% largest(?INTERNAL4(_, _, _, _, _, _, _, _, _, _, _, _, _, C5)) ->
%     largest(C5);
% largest(?INTERNAL3(_, _, _, _, _, _, _, _, _, _, C4)) ->
%     largest(C4);
% largest(?INTERNAL2(_, _, _, _, _, _, _, C3)) ->
%     largest(C3);
% largest(?LEAF4(_, _, _, E4)) ->
%     E4;
% largest(?LEAF3(_, _, E3)) ->
%     E3;
% largest(?LEAF2(_, E2)) ->
%     E2.

%% ------------------------------------------------------------------
%% Internal Function Definitions: union
%% ------------------------------------------------------------------

union_from_smallest(?INTERNAL1(E1, _, _, C1, C2), Acc) ->
    Acc2 = union_recur(C1, Acc),
    Acc3 = enter(E1, Acc2),
    union_recur(C2, Acc3);
union_from_smallest(?LEAF1(E1), Acc) ->
    enter(E1, Acc);
union_from_smallest(?LEAF0, Acc) ->
    Acc;
union_from_smallest(Node, Acc) ->
    union_recur(Node, Acc).

union_recur(?INTERNAL4(E1, E2, E3, E4, _, _, _, _, _, C1, C2, C3, C4, C5), Acc) ->
    Acc2 = enter(E1, union_recur(C1, Acc)),
    Acc3 = enter(E2, union_recur(C2, Acc2)),
    Acc4 = enter(E3, union_recur(C3, Acc3)),
    Acc5 = enter(E4, union_recur(C4, Acc4)),
    union_recur(C5, Acc5);
union_recur(?INTERNAL3(E1, E2, E3, _, _, _, _, C1, C2, C3, C4), Acc) ->
    Acc2 = enter(E1, union_recur(C1, Acc)),
    Acc3 = enter(E2, union_recur(C2, Acc2)),
    Acc4 = enter(E3, union_recur(C3, Acc3)),
    union_recur(C4, Acc4);
union_recur(?INTERNAL2(E1, E2, _, _, _, C1, C2, C3), Acc) ->
    Acc2 = enter(E1, union_recur(C1, Acc)),
    Acc3 = enter(E2, union_recur(C2, Acc2)),
    union_recur(C3, Acc3);
union_recur(?LEAF4(E1, E2, E3, E4), Acc) ->
    enter(E4, enter(E3, enter(E2, enter(E1, Acc))));
union_recur(?LEAF3(E1, E2, E3), Acc) ->
    enter(E3, enter(E2, enter(E1, Acc)));
union_recur(?LEAF2(E1, E2), Acc) ->
    enter(E2, enter(E1, Acc)).
