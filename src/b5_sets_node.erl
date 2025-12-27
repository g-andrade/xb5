%% FIXME
%% -% vim: set redrawtime=10000:

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
-module(b5_sets_node).

-export([
    % add/2,
    % balance/1,
    % delete/2,
    % delete_any/2,
    % difference/2,
    % empty/0,
    % filter/2,
    % filtermap/2,
    % fold/3,
    % from_list/1,
    % from_ordset/1,
    insert/2,
    % intersection/1,
    % intersection/2,
    % is_disjoint/2,
    % is_empty/1,
    % is_equal/2,
    is_member/2,
    % is_set/1,
    % is_subset/2,
    % iterator/1,
    % iterator/2,
    % iterator_from/2,
    % iterator_from/3,
    % larger/2,
    % largest/1,
    % map/2,
    % next/1,
    % singleton/1,
    % size/1,
    % smaller/2,
    % smallest/1,
    % take_largest/1,
    take_smallest/1,
    % to_list/1,
    % union/1,
    % union/2
         new/0 % FIXME
        ]).

% -include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% Macro Definitions: Nodes
%% ------------------------------------------------------------------

% 6 elements
-define(INTERNAL2(E1, E2, C1, C2, C3), {internal2, E1, E2, C1, C2, C3}).
-define(INTERNAL2_MATCH(E1, E2, C1, C2, C3), {_, E1, E2, C1, C2, C3}).
-define(INTERNAL2_MATCH_ALL, {_, E1, E2, C1, C2, C3}).

% improper list
-define(LEAF2(E1, E2), [E1 | E2]).
-define(LEAF2_MATCH(E1, E2), [E1 | E2]).
-define(LEAF2_MATCH_ALL, [E1 | E2]).

% 7 elements
-define(INTERNAL3(E1, E2, E3, C1, C2, C3, C4), {E1, E2, E3, C1, C2, C3, C4}).
-define(INTERNAL3_MATCH(E1, E2, E3, C1, C2, C3, C4), {E1, E2, E3, C1, C2, C3, C4}).
-define(INTERNAL3_MATCH_ALL, {E1, E2, E3, C1, C2, C3, C4}).

% 3 elements
-define(LEAF3(E1, E2, E3), {E1, E2, E3}).
-define(LEAF3_MATCH(E1, E2, E3), {E1, E2, E3}).
-define(LEAF3_MATCH_ALL, {E1, E2, E3}).

% 9 elements
-define(INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, C5),
    {E1, E2, E3, E4, C1, C2, C3, C4, C5}
).
-define(INTERNAL4_MATCH(E1, E2, E3, E4, C1, C2, C3, C4, C5),
    {E1, E2, E3, E4, C1, C2, C3, C4, C5}
).
-define(INTERNAL4_MATCH_ALL,
    {E1, E2, E3, E4, C1, C2, C3, C4, C5}
).

% 5 elements
-define(LEAF4(E1, E2, E3, E4), {leaf4, E1, E2, E3, E4}).
-define(LEAF4_MATCH(E1, E2, E3, E4), {_, E1, E2, E3, E4}).
-define(LEAF4_MATCH_ALL, {_, E1, E2, E3, E4}).

% 4 elements
-define(INTERNAL1(E1, C1, C2), {internal1, E1, C1, C2}).
-define(INTERNAL1_MATCH(E1, C1, C2), {_, E1, C1, C2}).
-define(INTERNAL1_MATCH_ALL, {_, E1, C1, C2}).

% 1 element
-define(LEAF1(E1), {E1}).
-define(LEAF1_MATCH(E1), {E1}).
-define(LEAF1_MATCH_ALL, {E1}).

% empty root
-define(LEAF0, leaf0).
-define(LEAF0_MATCH, leaf0).
-define(LEAF0_MATCH_ALL, leaf0).

%%%%%%%%

% 4 elements; cannot clash with any node type.
-define(SPLIT(SplitE, SplitL, SplitR), {split, SplitE, SplitL, SplitR}).
-define(SPLIT_MATCH(SplitE, SplitL, SplitR), {split, SplitE, SplitL, SplitR}).

%%%%%%%%%

% Any of the following cannot clash with either the largest leaf or internal
% nodes, since those are a merged node.

% list
-define(MID_MERGED(MergedNode), [MergedNode]).
-define(MID_MERGED_MATCH(MergedNode), [MergedNode | _]).

% 3 elements
-define(MID_ROTATED_FROM_RIGHT(UpElem, UpdatedNode, UpdatedRight),
    {UpElem, UpdatedNode, UpdatedRight}
).
% 4 elements
-define(MID_ROTATED_FROM_LEFT(UpElem, UpdatedNode, UpdatedRight),
    {from_left, UpElem, UpdatedNode, UpdatedRight}
).

%%%

% 3 elements
-define(ROTATED(UpElem, UpdatedLeft, UpdatedRight), {UpElem, UpdatedLeft, UpdatedRight}).

-define(MERGED(MergedNode), (MergedNode)).

%%%%%%%

% Cannot collide with any node definition
-define(ITER_KV(Elem), [Elem | Value]).

%% ------------------------------------------------------------------
%% Macro Definitions: Boilerplate Helpers
%% ------------------------------------------------------------------

%% ?INTERNAL4

-define(INTERNAL4_ARGS, E1, E2, E3, E4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARITY, 9).
-define(INTERNAL4_ARITY_P1, 10).

-define(INTERNAL4_C1(UpdatedC1), ?INTERNAL4(E1, E2, E3, E4, UpdatedC1, C2, C3, C4, C5)).
-define(INTERNAL4_C2(UpdatedC2), ?INTERNAL4(E1, E2, E3, E4, C1, UpdatedC2, C3, C4, C5)).
-define(INTERNAL4_C3(UpdatedC3), ?INTERNAL4(E1, E2, E3, E4, C1, C2, UpdatedC3, C4, C5)).
-define(INTERNAL4_C4(UpdatedC4), ?INTERNAL4(E1, E2, E3, E4, C1, C2, C3, UpdatedC4, C5)).
-define(INTERNAL4_C5(UpdatedC5), ?INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, UpdatedC5)).

-define(INTERNAL4_C1_REBALANCE(UpdatedC1), rebalance_INTERNAL4_C1(E1, E2, E3, E4, UpdatedC1, C2, C3, C4, C5)).
-define(INTERNAL4_C2_REBALANCE(UpdatedC2), rebalance_INTERNAL4_C2(E1, E2, E3, E4, C1, UpdatedC2, C3, C4, C5)).
-define(INTERNAL4_C3_REBALANCE(UpdatedC3), rebalance_INTERNAL4_C3(E1, E2, E3, E4, C1, C2, UpdatedC3, C4, C5)).
-define(INTERNAL4_C4_REBALANCE(UpdatedC4), rebalance_INTERNAL4_C4(E1, E2, E3, E4, C1, C2, C3, UpdatedC4, C5)).
-define(INTERNAL4_C5_REBALANCE(UpdatedC5), rebalance_INTERNAL4_C5(E1, E2, E3, E4, C1, C2, C3, C4, UpdatedC5)).

%% ?INTERNAL3

-define(INTERNAL3_ARGS, E1, E2, E3, C1, C2, C3, C4).
-define(INTERNAL3_ARITY, 7).
-define(INTERNAL3_ARITY_P1, 8).

-define(INTERNAL3_C1(UpdatedC1), ?INTERNAL3(E1, E2, E3, UpdatedC1, C2, C3, C4)).
-define(INTERNAL3_C2(UpdatedC2), ?INTERNAL3(E1, E2, E3, C1, UpdatedC2, C3, C4)).
-define(INTERNAL3_C3(UpdatedC3), ?INTERNAL3(E1, E2, E3, C1, C2, UpdatedC3, C4)).
-define(INTERNAL3_C4(UpdatedC4), ?INTERNAL3(E1, E2, E3, C1, C2, C3, UpdatedC4)).

-define(INTERNAL3_C1_REBALANCE(UpdatedC1), rebalance_INTERNAL3_C1(E1, E2, E3, UpdatedC1, C2, C3, C4)).
-define(INTERNAL3_C2_REBALANCE(UpdatedC2), rebalance_INTERNAL3_C2(E1, E2, E3, C1, UpdatedC2, C3, C4)).
-define(INTERNAL3_C3_REBALANCE(UpdatedC3), rebalance_INTERNAL3_C3(E1, E2, E3, C1, C2, UpdatedC3, C4)).
-define(INTERNAL3_C4_REBALANCE(UpdatedC4), rebalance_INTERNAL3_C4(E1, E2, E3, C1, C2, C3, UpdatedC4)).

%% ?INTERNAL2

-define(INTERNAL2_ARGS, E1, E2, C1, C2, C3).
-define(INTERNAL2_ARITY, 5).
-define(INTERNAL2_ARITY_P1, 6).

-define(INTERNAL2_C1(UpdatedC1), ?INTERNAL2(E1, E2, UpdatedC1, C2, C3)).
-define(INTERNAL2_C2(UpdatedC2), ?INTERNAL2(E1, E2, C1, UpdatedC2, C3)).
-define(INTERNAL2_C3(UpdatedC3), ?INTERNAL2(E1, E2, C1, C2, UpdatedC3)).

-define(INTERNAL2_C1_REBALANCE(UpdatedC1), rebalance_INTERNAL2_C1(E1, E2, UpdatedC1, C2, C3)).
-define(INTERNAL2_C2_REBALANCE(UpdatedC2), rebalance_INTERNAL2_C2(E1, E2, C1, UpdatedC2, C3)).
-define(INTERNAL2_C3_REBALANCE(UpdatedC3), rebalance_INTERNAL2_C3(E1, E2, C1, C2, UpdatedC3)).

%% ?INTERNAL1

-define(INTERNAL1_ARGS, E1, C1, C2).
-define(INTERNAL1_ARITY, 3).
-define(INTERNAL1_ARITY_P1, 4).

-define(INTERNAL1_C1(UpdatedC1), ?INTERNAL1(E1, UpdatedC1, C2)).
-define(INTERNAL1_C2(UpdatedC2), ?INTERNAL1(E1, C1, UpdatedC2)).

-define(INTERNAL1_C1_REBALANCE(UpdatedC1), rebalance_INTERNAL1_C1(E1, UpdatedC1, C2)).
-define(INTERNAL1_C2_REBALANCE(UpdatedC2), rebalance_INTERNAL1_C2(E1, C1, UpdatedC2)).

%% ?LEAF4

-define(LEAF4_ARGS, E1, E2, E3, E4).
-define(LEAF4_ARITY, 4).
-define(LEAF4_ARITY_P1, 5).

%% ?LEAF3

-define(LEAF3_ARGS, E1, E2, E3).
-define(LEAF3_ARITY, 3).
-define(LEAF3_ARITY_P1, 4).

%% ?LEAF2

-define(LEAF2_ARGS, E1, E2).
-define(LEAF2_ARITY, 2).
-define(LEAF2_ARITY_P1, 3).

%% ?LEAF1

-define(LEAF1_ARGS, E1).
-define(LEAF1_ARITY, 1).
-define(LEAF1_ARITY_P1, 2).

%%

-define(TAKEN(Taken, UpdatedNode), [Taken | UpdatedNode]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-opaque t(Elem) :: root_only_node(Elem) | deep_node(Elem).
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

-type root_only_node(Elem) ::
    (node_INTERNAL1(Elem)
    | node_LEAF1(Elem)
    | empty_node()).

-type deep_node(Elem) ::
    (node_INTERNAL4(Elem)
    | node_INTERNAL3(Elem)
    | node_INTERNAL2(Elem)
    | node_LEAF4(Elem)
    | node_LEAF3(Elem)
    | node_LEAF2(Elem)).

%-type non_empty_node(Elem) ::
%    (node_INTERNAL1(Elem)
%    | node_LEAF1(Elem)
%    | deep_node(Elem)).

-type node_INTERNAL4(Elem) ::
    (?INTERNAL4(
        Elem,
        Elem,
        Elem,
        Elem,
        deep_node(Elem),
        deep_node(Elem),
        deep_node(Elem),
        deep_node(Elem),
        deep_node(Elem)
    )).

-type node_INTERNAL3(Elem) ::
    (?INTERNAL3(
        Elem,
        Elem,
        Elem,
        deep_node(Elem),
        deep_node(Elem),
        deep_node(Elem),
        deep_node(Elem)
    )).

-type node_INTERNAL2(Elem) ::
    (?INTERNAL2(
        Elem,
        Elem,
        deep_node(Elem),
        deep_node(Elem),
        deep_node(Elem)
    )).

-type node_INTERNAL1(Elem) ::
    (?INTERNAL1(
        Elem,
        deep_node(Elem),
        deep_node(Elem)
    )).

-type node_LEAF4(Elem) ::
    (?LEAF4(
        Elem,
        Elem,
        Elem,
        Elem
    )).

-type node_LEAF3(Elem) ::
    (?LEAF3(
        Elem,
        Elem,
        Elem
    )).

-type node_LEAF2(Elem) :: nonempty_improper_list(Elem, Elem).

-type node_LEAF1(Elem) :: ?LEAF1(Elem).

%%%%%%%%%%%

% -type node_after_deletion(Elem) ::
%     node_INTERNAL3(Elem)
%     | node_INTERNAL2(Elem)
%     | node_INTERNAL1(Elem)
%     | node_LEAF2(Elem)
%     | node_LEAF1(Elem).
%
% -type deep_node_after_insertion(Elem) ::
%     node_INTERNAL4(Elem)
%     | node_INTERNAL3(Elem)
%     | node_LEAF4(Elem)
%     | node_LEAF3(Elem).
%
% % Temporary situation before rebalance
% -type unbalanced_node(Elem) :: node_INTERNAL1(Elem).

%%%%%%%%%%%

% -type split_result(Elem) :: split_internal_result(Elem) | leaf_split_result(Elem).
%
% -type split_internal_result(Elem) :: split_result(
%     Elem, node_INTERNAL2(Elem), node_INTERNAL2(Elem)
% ).
%
% -type leaf_split_result(Elem) :: split_result(
%     Elem, node_LEAF2(Elem), node_LEAF2(Elem)
% ).
%
% -type split_result(Elem, SplitL, SplitR) :: ?SPLIT(Elem, SplitL, SplitR).

%%%%%%%%%%%

% -opaque iter(Elem) :: forward_iter(Elem) | reverse_iter(Elem).
% -export_type([iter/1]).
%
% -type forward_iter(Elem) :: [iterator_step(Elem)].
% -type reverse_iter(Elem) :: nonempty_improper_list(reversed, [iterator_step(Elem)]).

% -type iterator_step(Elem) :: kv_pair(Elem) | deep_node(Elem).

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

insert(Elem, ?INTERNAL1_MATCH_ALL) ->
    insert_INTERNAL1(Elem, ?INTERNAL1_ARGS);
insert(Elem, ?LEAF1_MATCH_ALL) ->
    insert_LEAF1(Elem, ?LEAF1_ARGS);
insert(Elem, ?LEAF0_MATCH_ALL) ->
    ?LEAF1(Elem);
insert(Elem, Root) ->
    case insert_recur(Elem, Root) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL1(SplitE, SplitL, SplitR);
        %
        UpdatedRoot ->
            UpdatedRoot
    end.

is_member(Elem, ?INTERNAL1_MATCH_ALL) ->
    is_member_INTERNAL1(Elem, ?INTERNAL1_ARGS);
is_member(Elem, ?LEAF1_MATCH_ALL) ->
    is_member_LEAF1(Elem, ?LEAF1_ARGS);
is_member(Elem, ?LEAF0_MATCH_ALL) ->
    ?LEAF1(Elem);
is_member(Elem, Root) ->
    is_member_recur(Elem, Root).

new() ->
    ?LEAF0.

take_smallest(?INTERNAL1_MATCH_ALL) ->
    take_smallest_INTERNAL1(?INTERNAL1_ARGS);
take_smallest(?LEAF1_MATCH_ALL) ->
    take_smallest_LEAF1(?LEAF1_ARGS);
take_smallest(?LEAF0_MATCH_ALL) ->
    error_empty_set();
take_smallest(Root) ->
    case take_smallest_recur(Root) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL1(SplitE, SplitL, SplitR);
        %
        UpdatedRoot ->
            UpdatedRoot
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Exceptions
%% ------------------------------------------------------------------

-compile({inline, error_empty_set/0}).
error_empty_set() ->
    error(empty_set).

-compile({inline, error_key_exists/1}).
error_key_exists(Elem) ->
    error({key_exists, Elem}).

%% ------------------------------------------------------------------
%% Internal Function Definitions: insert/2
%% ------------------------------------------------------------------

insert_recur(Elem, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            insert_INTERNAL2(Elem, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            insert_INTERNAL3(Elem, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            insert_INTERNAL4(Elem, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            insert_LEAF2(Elem, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            insert_LEAF3(Elem, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            insert_LEAF4(Elem, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, insert_INTERNAL4/?INTERNAL4_ARITY_P1}).
insert_INTERNAL4(Elem, ?INTERNAL4_ARGS) ->
    if
        Elem > E2 ->
            %
            if
                Elem < E4 ->
                    %
                    if
                        Elem > E3 ->
                            insert_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS);
                        %
                        Elem < E3 ->
                            insert_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS);
                        %
                        true ->
                            error_key_exists(Elem)
                    end;
                %
                Elem > E4 ->
                    insert_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    insert_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS);
                %
                Elem > E1 ->
                    insert_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        true ->
            error_key_exists(Elem)
    end.

-compile({inline, insert_INTERNAL4_C1/?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
              SplitE, E1, E2, E3, E4,
              %
              SplitL, SplitR, C2, C3, C4, C5
            );
        %
        UpdatedC1 ->
            ?INTERNAL4_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL4_C2/?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
              E1, SplitE, E2, E3, E4,
              %
              C1, SplitL, SplitR, C3, C4, C5
            );
        %
        UpdatedC2 ->
            ?INTERNAL4_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL4_C3/?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C3) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
              E1, E2, SplitE, E3, E4,
              %
              C1, C2, SplitL, SplitR, C4, C5
            );
        %
        UpdatedC3 ->
            ?INTERNAL4_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL4_C4/?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C4) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
              E1, E2, E3, SplitE, E4,
              %
              C1, C2, C3, SplitL, SplitR, C5
            );
        %
        UpdatedC4 ->
            ?INTERNAL4_C4(UpdatedC4)
    end.

-compile({inline, insert_INTERNAL4_C5/?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C5) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
              E1, E2, E3, E4, SplitE,
              %
              C1, C2, C3, C4, SplitL, SplitR
            );
        %
        UpdatedC5 ->
            ?INTERNAL4_C5(UpdatedC5)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, insert_INTERNAL3/?INTERNAL3_ARITY_P1}).
insert_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    if
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    insert_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS);
                %
                Elem > E1 ->
                    insert_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        Elem > E2 ->
            %
            if
                Elem < E3 ->
                    insert_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS);
                %
                Elem > E3 ->
                    insert_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        true ->
            error_key_exists(Elem)
    end.


-compile({inline, insert_INTERNAL3_C1/?INTERNAL3_ARITY_P1}).
insert_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL4(
              SplitE, E1, E2, E3,
              %
              SplitL, SplitR, C2, C3, C4
            );
        %
        UpdatedC1 ->
            ?INTERNAL3_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL3_C2/?INTERNAL3_ARITY_P1}).
insert_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL4(
              E1, SplitE, E2, E3,
              %
              C1, SplitL, SplitR, C3, C4
            );
        %
        UpdatedC2 ->
            ?INTERNAL3_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL3_C3/?INTERNAL3_ARITY_P1}).
insert_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C3) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL4(
              E1, E2, SplitE, E3,
              %
              C1, C2, SplitL, SplitR, C4
            );
        %
        UpdatedC3 ->
            ?INTERNAL3_C3(UpdatedC3)
    end.

-compile({inline, insert_INTERNAL3_C4/?INTERNAL3_ARITY_P1}).
insert_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C4) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL4(
              E1, E2, E3, SplitE,
              %
              C1, C2, C3, SplitL, SplitR
            );
        %
        UpdatedC4 ->
            ?INTERNAL3_C4(UpdatedC4)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, insert_INTERNAL2/?INTERNAL2_ARITY_P1}).
insert_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    if
        Elem > E1 ->
            %
            if
                Elem < E2 ->
                    insert_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS);
                %
                Elem > E2 ->
                    insert_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        Elem < E1 ->
            insert_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS);
        %
        true ->
            error_key_exists(Elem)
    end.


-compile({inline, insert_INTERNAL2_C1/?INTERNAL2_ARITY_P1}).
insert_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL3(
              SplitE, E1, E2,
              %
              SplitL, SplitR, C2, C3
            );
        %
        UpdatedC1 ->
            ?INTERNAL2_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL2_C2/?INTERNAL2_ARITY_P1}).
insert_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL3(
              E1, SplitE, E2,
              %
              C1, SplitL, SplitR, C3
            );
        %
        UpdatedC2 ->
            ?INTERNAL2_C2(UpdatedC2)
    end.

-compile({inline, insert_INTERNAL2_C3/?INTERNAL2_ARITY_P1}).
insert_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS) ->
    case insert_recur(Elem, C3) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL3(
              E1, E2, SplitE,
              %
              C1, C2, SplitL, SplitR
            );
        %
        UpdatedC3 ->
            ?INTERNAL2_C3(UpdatedC3)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, insert_INTERNAL1/?INTERNAL1_ARITY_P1}).
insert_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem < E1 ->
            insert_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS);
        %
        Elem > E1 ->
            insert_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS);
        %
        true ->
            error_key_exists(Elem)
    end.

-compile({inline, insert_INTERNAL1_C1/?INTERNAL1_ARITY_P1}).
insert_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL2(
              SplitE, E1,
              %
              SplitL, SplitR, C2
            );
        %
        UpdatedC1 ->
            ?INTERNAL1_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL1_C2/?INTERNAL1_ARITY_P1}).
insert_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL2(
              E1, SplitE,
              %
              C1, SplitL, SplitR
            );
        %
        UpdatedC2 ->
            ?INTERNAL1_C2(UpdatedC2)
    end.

%%
%% ?LEAF4
%%

-compile({inline, insert_LEAF4/?LEAF4_ARITY_P1}).
insert_LEAF4(Elem, ?LEAF4_ARGS) ->
    if
        Elem > E2 ->
            %
            if
                Elem < E4 ->
                    %
                    if
                        Elem > E3 ->
                            insert_LEAF4_POS4(Elem, ?LEAF4_ARGS);
                        %
                        Elem < E3 ->
                            insert_LEAF4_POS3(Elem, ?LEAF4_ARGS);
                        %
                        true ->
                            error_key_exists(Elem)
                    end;
                %
                Elem > E4 ->
                    insert_LEAF4_POS5(Elem, ?LEAF4_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    insert_LEAF4_POS1(Elem, ?LEAF4_ARGS);
                %
                Elem > E1 ->
                    insert_LEAF4_POS2(Elem, ?LEAF4_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        true ->
            error_key_exists(Elem)
    end.

-compile({inline, insert_LEAF4_POS1/?LEAF4_ARITY_P1}).
insert_LEAF4_POS1(Elem, ?LEAF4_ARGS) ->
    split_leaf(Elem, E1, E2, E3, E4).

-compile({inline, insert_LEAF4_POS2/?LEAF4_ARITY_P1}).
insert_LEAF4_POS2(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, Elem, E2, E3, E4).

-compile({inline, insert_LEAF4_POS3/?LEAF4_ARITY_P1}).
insert_LEAF4_POS3(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, Elem, E3, E4).

-compile({inline, insert_LEAF4_POS4/?LEAF4_ARITY_P1}).
insert_LEAF4_POS4(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, E3, Elem, E4).

-compile({inline, insert_LEAF4_POS5/?LEAF4_ARITY_P1}).
insert_LEAF4_POS5(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, E3, E4, Elem).

%%
%% ?LEAF3
%%

-compile({inline, insert_LEAF3/?LEAF3_ARITY_P1}).
insert_LEAF3(Elem, ?LEAF3_ARGS) ->
    if
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    insert_LEAF3_POS1(Elem, ?LEAF3_ARGS);
                %
                Elem > E1 ->
                    insert_LEAF3_POS2(Elem, ?LEAF3_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        Elem > E2 ->
            %
            if
                Elem < E3 ->
                    insert_LEAF3_POS3(Elem, ?LEAF3_ARGS);
                %
                Elem > E3 ->
                    insert_LEAF3_POS4(Elem, ?LEAF3_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        true ->
            error_key_exists(Elem)
    end.


-compile({inline, insert_LEAF3_POS1/?LEAF3_ARITY_P1}).
insert_LEAF3_POS1(Elem, ?LEAF3_ARGS) ->
    ?LEAF4(Elem, E1, E2, E3).

-compile({inline, insert_LEAF3_POS2/?LEAF3_ARITY_P1}).
insert_LEAF3_POS2(Elem, ?LEAF3_ARGS) ->
    ?LEAF4(E1, Elem, E2, E3).

-compile({inline, insert_LEAF3_POS3/?LEAF3_ARITY_P1}).
insert_LEAF3_POS3(Elem, ?LEAF3_ARGS) ->
    ?LEAF4(E1, E2, Elem, E3).

-compile({inline, insert_LEAF3_POS4/?LEAF3_ARITY_P1}).
insert_LEAF3_POS4(Elem, ?LEAF3_ARGS) ->
    ?LEAF4(E1, E2, E3, Elem).

%%
%% ?LEAF2
%%

-compile({inline, insert_LEAF2/?LEAF2_ARITY_P1}).
insert_LEAF2(Elem, ?LEAF2_ARGS) ->
    if
        Elem > E1 ->
            %
            if
                Elem < E2 ->
                    insert_LEAF2_POS2(Elem, ?LEAF2_ARGS);
                %
                Elem > E2 ->
                    insert_LEAF2_POS3(Elem, ?LEAF2_ARGS);
                %
                true ->
                    error_key_exists(Elem)
            end;
        %
        Elem < E1 ->
            insert_LEAF2_POS1(Elem, ?LEAF2_ARGS);
        %
        true ->
            error_key_exists(Elem)
    end.


-compile({inline, insert_LEAF2_POS1/?LEAF2_ARITY_P1}).
insert_LEAF2_POS1(Elem, ?LEAF2_ARGS) ->
    ?LEAF3(Elem, E1, E2).

-compile({inline, insert_LEAF2_POS2/?LEAF2_ARITY_P1}).
insert_LEAF2_POS2(Elem, ?LEAF2_ARGS) ->
    ?LEAF3(E1, Elem, E2).

-compile({inline, insert_LEAF2_POS3/?LEAF2_ARITY_P1}).
insert_LEAF2_POS3(Elem, ?LEAF2_ARGS) ->
    ?LEAF3(E1, E2, Elem).

%%
%% ?LEAF1
%%

-compile({inline, insert_LEAF1/?LEAF1_ARITY_P1}).
insert_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem < E1 ->
            insert_LEAF1_POS1(Elem, ?LEAF1_ARGS);
        %
        Elem > E1 ->
            insert_LEAF1_POS2(Elem, ?LEAF1_ARGS);
        %
        true ->
            error_key_exists(Elem)
    end.

-compile({inline, insert_LEAF1_POS1/?LEAF1_ARITY_P1}).
insert_LEAF1_POS1(Elem, ?LEAF1_ARGS) ->
    ?LEAF2(Elem, E1).

-compile({inline, insert_LEAF1_POS2/?LEAF1_ARITY_P1}).
insert_LEAF1_POS2(Elem, ?LEAF1_ARGS) ->
    ?LEAF2(E1, Elem).

%%
%% Split
%%

-compile({inline, split_internal/11}).
split_internal(
  E1, E2, E3, E4, E5,
  %
  C1, C2, C3, C4, C5, C6
 ) ->
    SplitE = E3,

    SplitL = ?INTERNAL2(E1, E2, C1, C2, C3),
    SplitR = ?INTERNAL2(E4, E5, C4, C5, C6),

    ?SPLIT(SplitE, SplitL, SplitR).

-compile({inline, split_leaf/5}).
split_leaf(
  E1, E2, E3, E4, E5
 ) ->
    SplitE = E3,

    SplitL = ?LEAF2(E1, E2),
    SplitR = ?LEAF2(E4, E5),

    ?SPLIT(SplitE, SplitL, SplitR).

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_member/2
%% ------------------------------------------------------------------

is_member_recur(Elem, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            is_member_INTERNAL2(Elem, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            is_member_INTERNAL3(Elem, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            is_member_INTERNAL4(Elem, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            is_member_LEAF2(Elem, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            is_member_LEAF3(Elem, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            is_member_LEAF4(Elem, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, is_member_INTERNAL4/?INTERNAL4_ARITY_P1}).
is_member_INTERNAL4(Elem, ?INTERNAL4_ARGS) ->
    if
        Elem > E2 ->
            %
            if
                Elem < E4 ->
                    %
                    if
                        Elem > E3 ->
                            is_member_recur(Elem, C4);
                        %
                        Elem < E3 ->
                            is_member_recur(Elem, C3);
                        %
                        true ->
                            true
                    end;
                %
                Elem > E4 ->
                    is_member_recur(Elem, C5);
                %
                true ->
                    true
            end;
        %
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    is_member_recur(Elem, C1);
                %
                Elem > E1 ->
                    is_member_recur(Elem, C2);
                %
                true ->
                    true
            end;
        %
        true ->
            true
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, is_member_INTERNAL3/?INTERNAL3_ARITY_P1}).
is_member_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    if
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    is_member_recur(Elem, C1);
                %
                Elem > E1 ->
                    is_member_recur(Elem, C2);
                %
                true ->
                    true
            end;
        %
        Elem > E2 ->
            %
            if
                Elem < E3 ->
                    is_member_recur(Elem, C3);
                %
                Elem > E3 ->
                    is_member_recur(Elem, C4);
                %
                true ->
                    true
            end;
        %
        true ->
            true
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, is_member_INTERNAL2/?INTERNAL2_ARITY_P1}).
is_member_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    if
        Elem > E1 ->
            %
            if
                Elem < E2 ->
                    is_member_recur(Elem, C2);
                %
                Elem > E2 ->
                    is_member_recur(Elem, C3);
                %
                true ->
                    true
            end;
        %
        Elem < E1 ->
            is_member_recur(Elem, C1);
        %
        true ->
            true
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, is_member_INTERNAL1/?INTERNAL1_ARITY_P1}).
is_member_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem < E1 ->
            is_member_recur(Elem, C1);
        %
        Elem > E1 ->
            is_member_recur(Elem, C2);
        %
        true ->
            true
    end.

%%
%% ?LEAF4
%%

-compile({inline, is_member_LEAF4/?LEAF4_ARITY_P1}).
is_member_LEAF4(Elem, ?LEAF4_ARGS) ->
    if
        Elem > E2 ->
            Elem == E3 orelse Elem == E4;
        %
        true ->
            Elem == E2 orelse Elem == E1
    end.

%%
%% ?LEAF3
%%

-compile({inline, is_member_LEAF3/?LEAF3_ARITY_P1}).
is_member_LEAF3(Elem, ?LEAF3_ARGS) ->
    if
        Elem < E2 ->
            Elem == E1;
        %
        Elem > E2 ->
            Elem == E3;
        %
        true ->
            true
    end.

%%
%% ?LEAF2
%%

-compile({inline, is_member_LEAF2/?LEAF2_ARITY_P1}).
is_member_LEAF2(Elem, ?LEAF2_ARGS) ->
    Elem == E1 orelse Elem == E2.

%%
%% ?LEAF1
%%

-compile({inline, is_member_LEAF1/?LEAF1_ARITY_P1}).
is_member_LEAF1(Elem, ?LEAF1_ARGS) ->
    Elem == E1.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_smallest/2
%% ------------------------------------------------------------------

take_smallest_recur(Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            take_smallest_INTERNAL2(?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            take_smallest_INTERNAL3(?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_smallest_INTERNAL4(?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            take_smallest_LEAF2(?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            take_smallest_LEAF3(?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            take_smallest_LEAF4(?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, take_smallest_INTERNAL4/?INTERNAL4_ARITY}).
take_smallest_INTERNAL4(?INTERNAL4_ARGS) ->
    [Taken | UpdatedC1] = take_smallest_recur(C1),
    [Taken | ?INTERNAL4_C1_REBALANCE(UpdatedC1)].

%%
%% ?LEAF4
%%

-compile({inline, take_smallest_LEAF4/?LEAF4_ARITY}).
take_smallest_LEAF4(?LEAF4_ARGS) ->
    ?TAKEN(E1, ?LEAF3(E2, E3, E4)).

%%
%% ?LEAF3
%%

-compile({inline, take_smallest_LEAF3/?LEAF3_ARITY}).
take_smallest_LEAF3(?LEAF3_ARGS) ->
    ?TAKEN(E1, ?LEAF2(E2, E3)).

%%
%% ?LEAF2
%%

-compile({inline, take_smallest_LEAF2/?LEAF2_ARITY}).
take_smallest_LEAF2(?LEAF2_ARGS) ->
    ?TAKEN(E1, ?LEAF1(E2)).

%%
%% ?LEAF1
%%

-compile({inline, take_smallest_LEAF1/?LEAF1_ARITY}).
take_smallest_LEAF1(?LEAF1_ARGS) ->
    ?TAKEN(E1, ?LEAF0).

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing Child Nodes
%% ------------------------------------------------------------------

%%
%% INTERNAL4
%%

-compile({inline, rebalance_INTERNAL4_C1/?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C1(?INTERNAL4_ARGS) ->
    % TODO continue from here!!

    case C1 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_right_sibling(
                CE,
                CL,
                CR,
                E1,
                C2
            ),

            rebalance_INTERNAL4_C1_finish(Result, E2, E3, E4, C3, C4, C5);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_right_sibling(
                CE,
                E1,
                C2
            ),

            delete_internal4_rebalance_child1_finish(Result, E2, E3, E4, C3, C4, C5);
        %
        UpdatedC1 ->
            ?INTERNAL4_C1(UpdatedC1)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its right sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_right_sibling/7}).
rebalance_internal_from_right_sibling(CElem, CLeft, CRight, ParentE, Right) ->
    case Right of
        ?INTERNAL2_MATCH(K1, K2, Values, C1, C2, C3) ->
            [V1 | V2] = Values,

            MergedNode = ?INTERNAL4(
                CKey,
                ParentK,
                K1,
                K2,
                {CValue, ParentV, V1, V2},
                CLeft,
                CRight,
                C1,
                C2,
                C3
            ),

            MergedNode;
        %
        %
        %
        ?INTERNAL3_MATCH(K1, K2, K3, Values, C1, C2, C3, C4) ->
            {V1, V2, V3} = Values,

            UpK = K1,
            UpVal = V1,
            MovedC = C1,

            UpdatedNode = ?INTERNAL2(
                CKey,
                ParentK,
                [CValue | ParentV],
                CLeft,
                CRight,
                MovedC
            ),

            UpdatedRight = ?INTERNAL2(
                K2,
                K3,
                [V2 | V3],
                C2,
                C3,
                C4
            ),

            ?ROTATED(UpK, UpVal, UpdatedNode, UpdatedRight);
        %
        %
        %
        ?INTERNAL4_MATCH(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
            {V1, V2, V3, V4} = Values,

            UpK = K1,
            UpVal = V1,
            MovedC = C1,

            UpdatedNode = ?INTERNAL2(
                CKey,
                ParentK,
                [CValue | ParentV],
                CLeft,
                CRight,
                MovedC
            ),

            UpdatedRight = ?INTERNAL3(
                K2,
                K3,
                K4,
                {V2, V3, V4},
                C2,
                C3,
                C4,
                C5
            ),

            ?ROTATED(UpK, UpVal, UpdatedNode, UpdatedRight)
        %
        %
    end.

%-compile({inline, rebalance_leaf_from_right_sibling/5}).
rebalance_leaf_from_right_sibling(CElem, ParentE, Right) ->
    case Right of
        ?LEAF2_MATCH(K1, K2, V1, V2) ->
            MergedNode = ?LEAF4(
                CKey,
                ParentK,
                K1,
                K2,
                CValue,
                ParentV,
                V1,
                V2
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH(K1, K2, K3, V1, V2, V3) ->
            UpK = K1,
            UpVal = V1,

            UpdatedNode = ?LEAF2(CKey, ParentK, CValue, ParentV),
            UpdatedRight = ?LEAF2(K2, K3, V2, V3),

            ?ROTATED(UpK, UpVal, UpdatedNode, UpdatedRight);
        %
        %
        ?LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4) ->
            UpK = K1,
            UpVal = V1,

            UpdatedNode = ?LEAF2(CKey, ParentK, CValue, ParentV),
            UpdatedRight = ?LEAF3(K2, K3, K4, V2, V3, V4),

            ?ROTATED(UpK, UpVal, UpdatedNode, UpdatedRight)
        %
        %
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its left sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_left_sibling/7}).
rebalance_internal_from_left_sibling(
    CKey,
    CValue,
    CLeft,
    CRight,
    ParentK,
    ParentV,
    Left
) ->
    case Left of
        ?INTERNAL2_MATCH(LK1, LK2, LValues, LC1, LC2, LC3) ->
            [LV1 | LV2] = LValues,

            MergedNode = ?INTERNAL4(
                LK1,
                LK2,
                ParentK,
                CKey,
                {LV1, LV2, ParentV, CValue},
                LC1,
                LC2,
                LC3,
                CLeft,
                CRight
            ),

            MergedNode;
        %
        %
        %
        ?INTERNAL3_MATCH(K1, K2, K3, Values, C1, C2, C3, C4) ->
            {V1, V2, V3} = Values,

            UpK = K3,
            UpVal = V3,
            MovedC = C4,

            UpdatedNode = ?INTERNAL2(
                ParentK,
                CKey,
                [ParentV | CValue],
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?INTERNAL2(
                K1,
                K2,
                [V1 | V2],
                C1,
                C2,
                C3
            ),

            ?ROTATED(UpK, UpVal, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?INTERNAL4_MATCH(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
            {V1, V2, V3, V4} = Values,

            UpK = K4,
            UpVal = V4,
            MovedC = C5,

            UpdatedNode = ?INTERNAL2(
                ParentK,
                CKey,
                [ParentV | CValue],
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?INTERNAL3(
                K1,
                K2,
                K3,
                {V1, V2, V3},
                C1,
                C2,
                C3,
                C4
            ),

            ?ROTATED(UpK, UpVal, UpdatedLeft, UpdatedNode)
        %
        %
    end.

%-compile({inline, rebalance_leaf_from_left_sibling/5}).
rebalance_leaf_from_left_sibling(
    CKey,
    CValue,
    ParentK,
    ParentV,
    Left
) ->
    case Left of
        ?LEAF2_MATCH(LK1, LK2, LV1, LV2) ->
            MergedNode = ?LEAF4(
                LK1,
                LK2,
                ParentK,
                CKey,
                LV1,
                LV2,
                ParentV,
                CValue
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH(K1, K2, K3, V1, V2, V3) ->
            UpK = K3,
            UpVal = V3,

            UpdatedNode = ?LEAF2(ParentK, CKey, ParentV, CValue),
            UpdatedLeft = ?LEAF2(K1, K2, V1, V2),

            ?ROTATED(UpK, UpVal, UpdatedLeft, UpdatedNode);
        %
        %
        ?LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4) ->
            UpK = K4,
            UpVal = V4,

            UpdatedNode = ?LEAF2(ParentK, CKey, ParentV, CValue),
            UpdatedLeft = ?LEAF3(K1, K2, K3, V1, V2, V3),

            ?ROTATED(UpK, UpVal, UpdatedLeft, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from either left/right sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_either_sibling/10}).
rebalance_internal_from_either_sibling(
    CKey,
    CValue,
    CLeft,
    CRight,
    LParentK,
    LParentV,
    Left,
    RParentK,
    RParentV,
    Right
) ->
    case Left of
        ?INTERNAL2_MATCH(LK1, LK2, LValues, LC1, LC2, LC3) ->
            %
            %
            case Right of
                ?INTERNAL3_MATCH(K1, K2, K3, Values, C1, C2, C3, C4) ->
                    {V1, V2, V3} = Values,

                    UpK = K1,
                    UpVal = V1,
                    MovedC = C1,

                    UpdatedNode = ?INTERNAL2(
                        CKey,
                        RParentK,
                        [CValue | RParentV],
                        CLeft,
                        CRight,
                        MovedC
                    ),

                    UpdatedRight = ?INTERNAL2(
                        K2,
                        K3,
                        [V2 | V3],
                        C2,
                        C3,
                        C4
                    ),

                    ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, UpdatedNode, UpdatedRight);
                %
                %
                ?INTERNAL4_MATCH(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
                    {V1, V2, V3, V4} = Values,

                    UpK = K1,
                    UpVal = V1,
                    MovedC = C1,

                    UpdatedNode = ?INTERNAL2(
                        CKey,
                        RParentK,
                        [CValue | RParentV],
                        CLeft,
                        CRight,
                        MovedC
                    ),

                    UpdatedRight = ?INTERNAL3(
                        K2,
                        K3,
                        K4,
                        {V2, V3, V4},
                        C2,
                        C3,
                        C4,
                        C5
                    ),

                    ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, UpdatedNode, UpdatedRight);
                %
                %
                _ ->
                    % Merge with left since we already unpacked it
                    [LV1 | LV2] = LValues,

                    MergedNode = ?INTERNAL4(
                        LK1,
                        LK2,
                        LParentK,
                        CKey,
                        {LV1, LV2, LParentV, CValue},
                        LC1,
                        LC2,
                        LC3,
                        CLeft,
                        CRight
                    ),

                    ?MID_MERGED(MergedNode)
            end;
        %
        %
        %
        %
        ?INTERNAL3_MATCH(K1, K2, K3, Values, C1, C2, C3, C4) ->
            {V1, V2, V3} = Values,

            UpK = K3,
            UpVal = V3,
            MovedC = C4,

            UpdatedNode = ?INTERNAL2(
                LParentK,
                CKey,
                [LParentV | CValue],
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?INTERNAL2(
                K1,
                K2,
                [V1 | V2],
                C1,
                C2,
                C3
            ),

            ?MID_ROTATED_FROM_LEFT(UpK, UpVal, UpdatedLeft, UpdatedNode);
        %
        %
        %
        %
        ?INTERNAL4_MATCH(K1, K2, K3, K4, Values, C1, C2, C3, C4, C5) ->
            {V1, V2, V3, V4} = Values,

            UpK = K4,
            UpVal = V4,
            MovedC = C5,

            UpdatedNode = ?INTERNAL2(
                LParentK,
                CKey,
                [LParentV | CValue],
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?INTERNAL3(
                K1,
                K2,
                K3,
                {V1, V2, V3},
                C1,
                C2,
                C3,
                C4
            ),

            ?MID_ROTATED_FROM_LEFT(UpK, UpVal, UpdatedLeft, UpdatedNode)
    end.

% -compile({inline, rebalance_leaf_from_either_sibling/8}).
rebalance_leaf_from_either_sibling(
    CKey,
    CValue,
    LParentK,
    LParentV,
    Left,
    RParentK,
    RParentV,
    Right
) ->
    case Left of
        ?LEAF2_MATCH(LK1, LK2, LV1, LV2) ->
            case Right of
                ?LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4) ->
                    UpK = K1,
                    UpVal = V1,

                    UpdatedNode = ?LEAF2(CKey, RParentK, CValue, RParentV),
                    UpdatedRight = ?LEAF3(K2, K3, K4, V2, V3, V4),

                    ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, UpdatedNode, UpdatedRight);
                %
                %
                ?LEAF3_MATCH(K1, K2, K3, V1, V2, V3) ->
                    UpK = K1,
                    UpVal = V1,

                    UpdatedNode = ?LEAF2(CKey, RParentK, CValue, RParentV),
                    UpdatedRight = ?LEAF2(K2, K3, V2, V3),

                    ?MID_ROTATED_FROM_RIGHT(UpK, UpVal, UpdatedNode, UpdatedRight);
                %
                %
                _ ->
                    % Merge with left since we already unpacked it
                    MergedNode = ?LEAF4(
                        LK1,
                        LK2,
                        LParentK,
                        CKey,
                        LV1,
                        LV2,
                        LParentV,
                        CValue
                    ),

                    ?MID_MERGED(MergedNode)
            end;
        %
        %
        %
        ?LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4) ->
            UpK = K4,
            UpVal = V4,

            UpdatedNode = ?LEAF2(LParentK, CKey, LParentV, CValue),
            UpdatedLeft = ?LEAF3(K1, K2, K3, V1, V2, V3),

            ?MID_ROTATED_FROM_LEFT(UpK, UpVal, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?LEAF3_MATCH(K1, K2, K3, V1, V2, V3) ->
            UpK = K3,
            UpVal = V3,

            UpdatedNode = ?LEAF2(LParentK, CKey, LParentV, CValue),
            UpdatedLeft = ?LEAF2(K1, K2, V1, V2),

            ?MID_ROTATED_FROM_LEFT(UpK, UpVal, UpdatedLeft, UpdatedNode)
    end.
