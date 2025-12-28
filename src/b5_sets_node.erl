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
    delete/2,
    difference/2,
    % filter/2,
    % filtermap/2,
    % fold/3,
    insert/2,
    % intersection/1,
    intersection/2,
    is_disjoint/4,
    % is_equal/2,
    is_member/2,
    % is_set/1,
    is_subset/2,
    iterator/2,
    iterator_from/3,
    % larger/2,
    largest/1,
    % map/2,
    new/0,
    next/1,
    % singleton/1,
    % size/1,
    % smaller/2,
    smallest/1,
    take_largest/1,
    take_smallest/1,
    % to_list/1,
    % union/1,
    union/4
]).

% -include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% Macro Definitions: Nodes
%% ------------------------------------------------------------------

% 6 elements
-define(INTERNAL2(E1, E2, C1, C2, C3), {internal2, E1, E2, C1, C2, C3}).
-define(INTERNAL2_MATCH(E1, E2, C1, C2, C3), {_, E1, E2, C1, C2, C3}).
-define(INTERNAL2_MATCH_ALL, {_, E1, E2, C1, C2, C3}).

% 2 elements
-define(LEAF2(E1, E2), {E1, E2}).
-define(LEAF2_MATCH(E1, E2), {E1, E2}).
-define(LEAF2_MATCH_ALL, {E1, E2}).

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
-define(ITER_ELEM(Elem), [Elem]).
-define(REV_ITER_TAG, reversed).

%% ------------------------------------------------------------------
%% Macro Definitions: Boilerplate Helpers
%% ------------------------------------------------------------------

%% ?INTERNAL4

-define(INTERNAL4_ARGS, E1, E2, E3, E4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARITY, 9).
-define(INTERNAL4_ARITY_P1, 10).
-define(INTERNAL4_ARITY_P2, 11).
-define(INTERNAL4_ARITY_M2, 7).

-define(INTERNAL4_C1(UpdatedC1), ?INTERNAL4(E1, E2, E3, E4, UpdatedC1, C2, C3, C4, C5)).
-define(INTERNAL4_C2(UpdatedC2), ?INTERNAL4(E1, E2, E3, E4, C1, UpdatedC2, C3, C4, C5)).
-define(INTERNAL4_C3(UpdatedC3), ?INTERNAL4(E1, E2, E3, E4, C1, C2, UpdatedC3, C4, C5)).
-define(INTERNAL4_C4(UpdatedC4), ?INTERNAL4(E1, E2, E3, E4, C1, C2, C3, UpdatedC4, C5)).
-define(INTERNAL4_C5(UpdatedC5), ?INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, UpdatedC5)).

-define(INTERNAL4_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL4_C1(E1, E2, E3, E4, UpdatedC1, C2, C3, C4, C5)
).
-define(INTERNAL4_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL4_C2(E1, E2, E3, E4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL4_C3(E1, E2, E3, E4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_C4_REBALANCE(UpdatedC4),
    rebalance_INTERNAL4_C4(E1, E2, E3, E4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_C5_REBALANCE(UpdatedC5),
    rebalance_INTERNAL4_C5(E1, E2, E3, E4, C1, C2, C3, C4, UpdatedC5)
).

-define(INTERNAL4_E1_C2_REBALANCE(ReplacementE, UpdatedC2),
    rebalance_INTERNAL4_C2(ReplacementE, E2, E3, E4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_E2_C3_REBALANCE(ReplacementE, UpdatedC3),
    rebalance_INTERNAL4_C3(E1, ReplacementE, E3, E4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_E3_C4_REBALANCE(ReplacementE, UpdatedC4),
    rebalance_INTERNAL4_C4(E1, E2, ReplacementE, E4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_E4_C5_REBALANCE(ReplacementE, UpdatedC5),
    rebalance_INTERNAL4_C5(E1, E2, E3, ReplacementE, C1, C2, C3, C4, UpdatedC5)
).

-define(INTERNAL4_ARGS_IGN_E1, _, E2, E3, E4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_E2, E1, _, E3, E4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_E3, E1, E2, _, E4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_E4, E1, E2, E3, _, C1, C2, C3, C4, C5).

%% ?INTERNAL3

-define(INTERNAL3_ARGS, E1, E2, E3, C1, C2, C3, C4).
-define(INTERNAL3_ARITY, 7).
-define(INTERNAL3_ARITY_P1, 8).
-define(INTERNAL3_ARITY_P2, 9).
-define(INTERNAL3_ARITY_M2, 5).

-define(INTERNAL3_C1(UpdatedC1), ?INTERNAL3(E1, E2, E3, UpdatedC1, C2, C3, C4)).
-define(INTERNAL3_C2(UpdatedC2), ?INTERNAL3(E1, E2, E3, C1, UpdatedC2, C3, C4)).
-define(INTERNAL3_C3(UpdatedC3), ?INTERNAL3(E1, E2, E3, C1, C2, UpdatedC3, C4)).
-define(INTERNAL3_C4(UpdatedC4), ?INTERNAL3(E1, E2, E3, C1, C2, C3, UpdatedC4)).

-define(INTERNAL3_C1_REBALANCE(UpdatedC1),
    rebalance_INTERNAL3_C1(E1, E2, E3, UpdatedC1, C2, C3, C4)
).
-define(INTERNAL3_C2_REBALANCE(UpdatedC2),
    rebalance_INTERNAL3_C2(E1, E2, E3, C1, UpdatedC2, C3, C4)
).
-define(INTERNAL3_C3_REBALANCE(UpdatedC3),
    rebalance_INTERNAL3_C3(E1, E2, E3, C1, C2, UpdatedC3, C4)
).
-define(INTERNAL3_C4_REBALANCE(UpdatedC4),
    rebalance_INTERNAL3_C4(E1, E2, E3, C1, C2, C3, UpdatedC4)
).

-define(INTERNAL3_E1_C2_REBALANCE(ReplacementE, UpdatedC2),
    rebalance_INTERNAL3_C2(ReplacementE, E2, E3, C1, UpdatedC2, C3, C4)
).
-define(INTERNAL3_E2_C3_REBALANCE(ReplacementE, UpdatedC3),
    rebalance_INTERNAL3_C3(E1, ReplacementE, E3, C1, C2, UpdatedC3, C4)
).
-define(INTERNAL3_E3_C4_REBALANCE(ReplacementE, UpdatedC4),
    rebalance_INTERNAL3_C4(E1, E2, ReplacementE, C1, C2, C3, UpdatedC4)
).

-define(INTERNAL3_ARGS_IGN_E1, _, E2, E3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_E2, E1, _, E3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_E3, E1, E2, _, C1, C2, C3, C4).

%% ?INTERNAL2

-define(INTERNAL2_ARGS, E1, E2, C1, C2, C3).
-define(INTERNAL2_ARITY, 5).
-define(INTERNAL2_ARITY_P1, 6).
-define(INTERNAL2_ARITY_P2, 7).
-define(INTERNAL2_ARITY_M2, 3).

-define(INTERNAL2_C1(UpdatedC1), ?INTERNAL2(E1, E2, UpdatedC1, C2, C3)).
-define(INTERNAL2_C2(UpdatedC2), ?INTERNAL2(E1, E2, C1, UpdatedC2, C3)).
-define(INTERNAL2_C3(UpdatedC3), ?INTERNAL2(E1, E2, C1, C2, UpdatedC3)).

-define(INTERNAL2_C1_REBALANCE(UpdatedC1), rebalance_INTERNAL2_C1(E1, E2, UpdatedC1, C2, C3)).
-define(INTERNAL2_C2_REBALANCE(UpdatedC2), rebalance_INTERNAL2_C2(E1, E2, C1, UpdatedC2, C3)).
-define(INTERNAL2_C3_REBALANCE(UpdatedC3), rebalance_INTERNAL2_C3(E1, E2, C1, C2, UpdatedC3)).

-define(INTERNAL2_E1_C2_REBALANCE(ReplacementE, UpdatedC2),
    rebalance_INTERNAL2_C2(ReplacementE, E2, C1, UpdatedC2, C3)
).
-define(INTERNAL2_E2_C3_REBALANCE(ReplacementE, UpdatedC3),
    rebalance_INTERNAL2_C3(E1, ReplacementE, C1, C2, UpdatedC3)
).

-define(INTERNAL2_ARGS_IGN_E1, _, E2, C1, C2, C3).
-define(INTERNAL2_ARGS_IGN_E2, E1, _, C1, C2, C3).

%% ?INTERNAL1

-define(INTERNAL1_ARGS, E1, C1, C2).
-define(INTERNAL1_ARITY, 3).
-define(INTERNAL1_ARITY_P1, 4).
-define(INTERNAL1_ARITY_M2, 1).

-define(INTERNAL1_C1(UpdatedC1), ?INTERNAL1(E1, UpdatedC1, C2)).
-define(INTERNAL1_C2(UpdatedC2), ?INTERNAL1(E1, C1, UpdatedC2)).

-define(INTERNAL1_C1_REBALANCE(UpdatedC1), rebalance_INTERNAL1_C1(E1, UpdatedC1, C2)).
-define(INTERNAL1_C2_REBALANCE(UpdatedC2), rebalance_INTERNAL1_C2(E1, C1, UpdatedC2)).

-define(INTERNAL1_E1_C2_REBALANCE(ReplacementE, UpdatedC2),
    rebalance_INTERNAL1_C2(ReplacementE, C1, UpdatedC2)
).

-define(INTERNAL1_ARGS_IGN_E1, _, C1, C2).

%% ?LEAF4

-define(LEAF4_ARGS, E1, E2, E3, E4).
-define(LEAF4_ARITY, 4).
-define(LEAF4_ARITY_P1, 5).
-define(LEAF4_ARITY_P2, 6).
-define(LEAF4_ARITY_M1, 3).

%% ?LEAF3

-define(LEAF3_ARGS, E1, E2, E3).
-define(LEAF3_ARITY, 3).
-define(LEAF3_ARITY_P1, 4).
-define(LEAF3_ARITY_P2, 5).
-define(LEAF3_ARITY_M1, 2).

%% ?LEAF2

-define(LEAF2_ARGS, E1, E2).
-define(LEAF2_ARITY, 2).
-define(LEAF2_ARITY_P1, 3).
-define(LEAF2_ARITY_P2, 4).
-define(LEAF2_ARITY_M1, 1).

%% ?LEAF1

-define(LEAF1_ARGS, E1).
-define(LEAF1_ARITY, 1).
-define(LEAF1_ARITY_P1, 2).
-define(LEAF1_ARITY_M1, 0).

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

delete(Elem, ?INTERNAL1_MATCH_ALL) ->
    delete_INTERNAL1(Elem, ?INTERNAL1_ARGS);
delete(Elem, ?LEAF1_MATCH_ALL) ->
    delete_LEAF1(Elem, ?LEAF1_ARGS);
delete(Elem, ?LEAF0_MATCH_ALL) ->
    ?LEAF1(Elem);
delete(Elem, Root) ->
    delete_recur(Elem, Root).

-spec difference(t(Elem), t(Elem)) -> nonempty_improper_list(non_neg_integer(), t(Elem)).
difference(Root1, Root2) ->
    RemovedCount = 0,

    try smallest(Root1) of
        MinElem ->
            MaxElem = largest(Root1),
            Iter = iterator_from(MinElem, Root2, ordered),
            difference_recur(Iter, Root1, RemovedCount, MaxElem)
    catch
        error:empty_set ->
            [RemovedCount | Root1]
    end.

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

-spec intersection(t(Elem), t(Elem)) -> nonempty_improper_list(non_neg_integer(), t(Elem)).
intersection(Root1, Root2) ->
    NewRoot = new(),
    Count = 0,

    try max(smallest(Root1), smallest(Root2)) of
        MinElem ->
            Iter1 = bound_fwd_iterator(MinElem, Root1),
            Iter2 = bound_fwd_iterator(MinElem, Root2),
            intersection_recur(Iter1, Iter2, NewRoot, Count)
    catch
        error:empty_set ->
            [Count | NewRoot]
    end.

is_disjoint(Root1, Size1, Root2, Size2) ->
    case Size1 < Size2 of
        true ->
            is_disjoint_root(Root1, Root2);
        %
        _ ->
            is_disjoint_root(Root2, Root1)
    end.

is_member(Elem, ?INTERNAL1_MATCH_ALL) ->
    is_member_INTERNAL1(Elem, ?INTERNAL1_ARGS);
is_member(Elem, ?LEAF1_MATCH_ALL) ->
    is_member_LEAF1(Elem, ?LEAF1_ARGS);
is_member(Elem, ?LEAF0_MATCH_ALL) ->
    ?LEAF1(Elem);
is_member(Elem, Root) ->
    is_member_recur(Elem, Root).

is_subset(Root1, Root2) ->
    Iter1 = fwd_iterator(Root1),
    is_subset_recur(Iter1, Root2).

iterator(Root, ordered) ->
    fwd_iterator(Root);
iterator(Root, reversed) ->
    Acc = rev_iterator(Root),
    [?REV_ITER_TAG | Acc].

iterator_from(Elem, Root, ordered) ->
    bound_fwd_iterator(Elem, Root).

largest(?INTERNAL1_MATCH(_, _, C2)) ->
    largest_recur(C2);
largest(?LEAF1_MATCH(E1)) ->
    E1;
largest(?LEAF0_MATCH) ->
    error_empty_set();
largest(Root) ->
    largest_recur(Root).

new() ->
    ?LEAF0.

next([Head | Tail]) ->
    next(Head, Tail);
next([]) ->
    none.

smallest(?INTERNAL1_MATCH(_, C1, _)) ->
    smallest_recur(C1);
smallest(?LEAF1_MATCH(E1)) ->
    E1;
smallest(?LEAF0_MATCH) ->
    error_empty_set();
smallest(Root) ->
    smallest_recur(Root).

take_largest(?INTERNAL1_MATCH_ALL) ->
    take_largest_INTERNAL1(?INTERNAL1_ARGS);
take_largest(?LEAF1_MATCH_ALL) ->
    take_largest_LEAF1(?LEAF1_ARGS);
take_largest(?LEAF0_MATCH_ALL) ->
    error_empty_set();
take_largest(Root) ->
    take_largest_recur(Root).

take_smallest(?INTERNAL1_MATCH_ALL) ->
    take_smallest_INTERNAL1(?INTERNAL1_ARGS);
take_smallest(?LEAF1_MATCH_ALL) ->
    take_smallest_LEAF1(?LEAF1_ARGS);
take_smallest(?LEAF0_MATCH_ALL) ->
    error_empty_set();
take_smallest(Root) ->
    take_smallest_recur(Root).

union(Root1, Size1, Root2, Size2) ->
    case Size1 > Size2 of
        true ->
            union_root(Root1, Size1, Root2);
        %
        _ ->
            union_root(Root2, Size2, Root1)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Exceptions
%% ------------------------------------------------------------------

-compile({inline, error_badkey/1}).
error_badkey(Elem) ->
    error({badkey, Elem}).

-compile({inline, error_empty_set/0}).
error_empty_set() ->
    error(empty_set).

-compile({inline, error_key_exists/1}).
error_key_exists(Elem) ->
    error({key_exists, Elem}).

%% ------------------------------------------------------------------
%% Internal Function Definitions: delete/2
%% ------------------------------------------------------------------

delete_recur(Elem, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            delete_INTERNAL2(Elem, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            delete_INTERNAL3(Elem, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            delete_INTERNAL4(Elem, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            delete_LEAF2(Elem, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            delete_LEAF3(Elem, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            delete_LEAF4(Elem, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, delete_INTERNAL4 / ?INTERNAL4_ARITY_P1}).
delete_INTERNAL4(Elem, ?INTERNAL4_ARGS) ->
    if
        Elem > E2 ->
            %
            if
                Elem < E4 ->
                    %
                    if
                        Elem > E3 ->
                            delete_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS);
                        %
                        Elem < E3 ->
                            delete_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS);
                        %
                        true ->
                            delete_INTERNAL4_E3(?INTERNAL4_ARGS)
                    end;
                %
                Elem > E4 ->
                    delete_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_INTERNAL4_E4(?INTERNAL4_ARGS)
            end;
        %
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    delete_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS);
                %
                Elem > E1 ->
                    delete_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_INTERNAL4_E1(?INTERNAL4_ARGS)
            end;
        %
        true ->
            delete_INTERNAL4_E2(?INTERNAL4_ARGS)
    end.

-compile({inline, delete_INTERNAL4_C1 / ?INTERNAL4_ARITY_P1}).
delete_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC1 = delete_recur(Elem, C1),

    ?INTERNAL4_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL4_C2 / ?INTERNAL4_ARITY_P1}).
delete_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC2 = delete_recur(Elem, C2),

    ?INTERNAL4_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL4_C3 / ?INTERNAL4_ARITY_P1}).
delete_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC3 = delete_recur(Elem, C3),

    ?INTERNAL4_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL4_C4 / ?INTERNAL4_ARITY_P1}).
delete_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC4 = delete_recur(Elem, C4),

    ?INTERNAL4_C4_REBALANCE(UpdatedC4).

-compile({inline, delete_INTERNAL4_C5 / ?INTERNAL4_ARITY_P1}).
delete_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC5 = delete_recur(Elem, C5),

    ?INTERNAL4_C5_REBALANCE(UpdatedC5).

%%

-compile({inline, delete_INTERNAL4_E1 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_E1(?INTERNAL4_ARGS_IGN_E1) ->
    [ReplacementE | UpdatedC2] = take_smallest_recur(C2),

    ?INTERNAL4_E1_C2_REBALANCE(ReplacementE, UpdatedC2).

-compile({inline, delete_INTERNAL4_E2 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_E2(?INTERNAL4_ARGS_IGN_E2) ->
    [ReplacementE | UpdatedC3] = take_smallest_recur(C3),

    ?INTERNAL4_E2_C3_REBALANCE(ReplacementE, UpdatedC3).

-compile({inline, delete_INTERNAL4_E3 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_E3(?INTERNAL4_ARGS_IGN_E3) ->
    [ReplacementE | UpdatedC4] = take_smallest_recur(C4),

    ?INTERNAL4_E3_C4_REBALANCE(ReplacementE, UpdatedC4).

-compile({inline, delete_INTERNAL4_E4 / ?INTERNAL4_ARITY}).
delete_INTERNAL4_E4(?INTERNAL4_ARGS_IGN_E4) ->
    [ReplacementE | UpdatedC5] = take_smallest_recur(C5),

    ?INTERNAL4_E4_C5_REBALANCE(ReplacementE, UpdatedC5).

%%
%% ?INTERNAL3
%%

-compile({inline, delete_INTERNAL3 / ?INTERNAL3_ARITY_P1}).
delete_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    if
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    delete_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS);
                %
                Elem > E1 ->
                    delete_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_INTERNAL3_E1(?INTERNAL3_ARGS)
            end;
        %
        Elem > E2 ->
            %
            if
                Elem < E3 ->
                    delete_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS);
                %
                Elem > E3 ->
                    delete_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_INTERNAL3_E3(?INTERNAL3_ARGS)
            end;
        %
        true ->
            delete_INTERNAL3_E2(?INTERNAL3_ARGS)
    end.

-compile({inline, delete_INTERNAL3_C1 / ?INTERNAL3_ARITY_P1}).
delete_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC1 = delete_recur(Elem, C1),

    ?INTERNAL3_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL3_C2 / ?INTERNAL3_ARITY_P1}).
delete_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC2 = delete_recur(Elem, C2),

    ?INTERNAL3_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL3_C3 / ?INTERNAL3_ARITY_P1}).
delete_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC3 = delete_recur(Elem, C3),

    ?INTERNAL3_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL3_C4 / ?INTERNAL3_ARITY_P1}).
delete_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC4 = delete_recur(Elem, C4),

    ?INTERNAL3_C4_REBALANCE(UpdatedC4).

%%

-compile({inline, delete_INTERNAL3_E1 / ?INTERNAL3_ARITY}).
delete_INTERNAL3_E1(?INTERNAL3_ARGS_IGN_E1) ->
    [ReplacementE | UpdatedC2] = take_smallest_recur(C2),

    ?INTERNAL3_E1_C2_REBALANCE(ReplacementE, UpdatedC2).

-compile({inline, delete_INTERNAL3_E2 / ?INTERNAL3_ARITY}).
delete_INTERNAL3_E2(?INTERNAL3_ARGS_IGN_E2) ->
    [ReplacementE | UpdatedC3] = take_smallest_recur(C3),

    ?INTERNAL3_E2_C3_REBALANCE(ReplacementE, UpdatedC3).

-compile({inline, delete_INTERNAL3_E3 / ?INTERNAL3_ARITY}).
delete_INTERNAL3_E3(?INTERNAL3_ARGS_IGN_E3) ->
    [ReplacementE | UpdatedC4] = take_smallest_recur(C4),

    ?INTERNAL3_E3_C4_REBALANCE(ReplacementE, UpdatedC4).

%%
%% ?INTERNAL2
%%

-compile({inline, delete_INTERNAL2 / ?INTERNAL2_ARITY_P1}).
delete_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    if
        Elem > E1 ->
            %
            if
                Elem < E2 ->
                    delete_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS);
                %
                Elem > E2 ->
                    delete_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS);
                %
                true ->
                    delete_INTERNAL2_E2(?INTERNAL2_ARGS)
            end;
        %
        Elem < E1 ->
            delete_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS);
        %
        true ->
            delete_INTERNAL2_E1(?INTERNAL2_ARGS)
    end.

-compile({inline, delete_INTERNAL2_C1 / ?INTERNAL2_ARITY_P1}).
delete_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS) ->
    UpdatedC1 = delete_recur(Elem, C1),

    ?INTERNAL2_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL2_C2 / ?INTERNAL2_ARITY_P1}).
delete_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS) ->
    UpdatedC2 = delete_recur(Elem, C2),

    ?INTERNAL2_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL2_C3 / ?INTERNAL2_ARITY_P1}).
delete_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS) ->
    UpdatedC3 = delete_recur(Elem, C3),

    ?INTERNAL2_C3_REBALANCE(UpdatedC3).

%%

-compile({inline, delete_INTERNAL2_E1 / ?INTERNAL2_ARITY}).
delete_INTERNAL2_E1(?INTERNAL2_ARGS_IGN_E1) ->
    [ReplacementE | UpdatedC2] = take_smallest_recur(C2),

    ?INTERNAL2_E1_C2_REBALANCE(ReplacementE, UpdatedC2).

-compile({inline, delete_INTERNAL2_E2 / ?INTERNAL2_ARITY}).
delete_INTERNAL2_E2(?INTERNAL2_ARGS_IGN_E2) ->
    [ReplacementE | UpdatedC3] = take_smallest_recur(C3),

    ?INTERNAL2_E2_C3_REBALANCE(ReplacementE, UpdatedC3).

%%
%% ?INTERNAL1
%%

-compile({inline, delete_INTERNAL1 / ?INTERNAL1_ARITY_P1}).
delete_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem < E1 ->
            delete_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS);
        %
        Elem > E1 ->
            delete_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS);
        %
        true ->
            delete_INTERNAL1_E1(?INTERNAL1_ARGS)
    end.

-compile({inline, delete_INTERNAL1_C1 / ?INTERNAL1_ARITY_P1}).
delete_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    UpdatedC1 = delete_recur(Elem, C1),
    ?INTERNAL1_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL1_C2 / ?INTERNAL1_ARITY_P1}).
delete_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS) ->
    UpdatedC2 = delete_recur(Elem, C2),
    ?INTERNAL1_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL1_E1 / ?INTERNAL1_ARITY}).
delete_INTERNAL1_E1(?INTERNAL1_ARGS_IGN_E1) ->
    [ReplacementE | UpdatedC2] = take_smallest_recur(C2),
    ?INTERNAL1_E1_C2_REBALANCE(ReplacementE, UpdatedC2).

%%
%% ?LEAF4
%%

-compile({inline, delete_LEAF4 / ?LEAF4_ARITY_P1}).
delete_LEAF4(Elem, ?LEAF4_ARGS) ->
    if
        Elem == E1 ->
            ?LEAF3(E2, E3, E4);
        %
        Elem == E2 ->
            ?LEAF3(E1, E3, E4);
        %
        Elem == E3 ->
            ?LEAF3(E1, E2, E4);
        %
        Elem == E4 ->
            ?LEAF3(E1, E2, E3);
        %
        true ->
            error_badkey(Elem)
    end.

%%
%% ?LEAF3
%%

-compile({inline, delete_LEAF3 / ?LEAF3_ARITY_P1}).
delete_LEAF3(Elem, ?LEAF3_ARGS) ->
    if
        Elem == E1 ->
            ?LEAF2(E2, E3);
        %
        Elem == E2 ->
            ?LEAF2(E1, E3);
        %
        Elem == E3 ->
            ?LEAF2(E1, E2);
        %
        true ->
            error_badkey(Elem)
    end.

%%
%% ?LEAF2
%%

-compile({inline, delete_LEAF2 / ?LEAF2_ARITY_P1}).
delete_LEAF2(Elem, ?LEAF2_ARGS) ->
    if
        Elem == E1 ->
            ?LEAF1(E2);
        %
        Elem == E2 ->
            ?LEAF1(E1);
        %
        true ->
            error_badkey(Elem)
    end.

%%
%% ?LEAF1
%%

-compile({inline, delete_LEAF1 / ?LEAF1_ARITY_P1}).
delete_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem == E1 ->
            ?LEAF0;
        %
        true ->
            error_badkey(Elem)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: difference/2
%% ------------------------------------------------------------------

difference_recur([Head | Tail], Root, RemovedCount, MaxElem) ->
    case Head of
        ?ITER_ELEM(Elem) ->
            Next = Tail,
            difference_recur(Elem, Next, Root, RemovedCount, MaxElem);
        %
        Node ->
            [?ITER_ELEM(Elem) | Next] = fwd_iterator_recur(Node, Tail),
            difference_recur(Elem, Next, Root, RemovedCount, MaxElem)
    end;
difference_recur([], Root, RemovedCount, _MaxElem) ->
    [RemovedCount | Root].

difference_recur(Elem, Next, Root, RemovedCount, MaxElem) ->
    try delete(Elem, Root) of
        UpdatedRoot ->
            UpdatedCount = RemovedCount + 1,
            difference_recur(Next, UpdatedRoot, UpdatedCount, MaxElem)
    catch
        error:{badkey, K} when K =:= Elem ->
            %
            case Elem =< MaxElem of
                true ->
                    difference_recur(Next, Root, RemovedCount, MaxElem);
                %
                _ ->
                    % No more elements can be removed, no point in continuing
                    [RemovedCount | Root]
            end
    end.

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

-compile({inline, insert_INTERNAL4 / ?INTERNAL4_ARITY_P1}).
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

-compile({inline, insert_INTERNAL4_C1 / ?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
                SplitE,
                E1,
                E2,
                E3,
                E4,
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

-compile({inline, insert_INTERNAL4_C2 / ?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
                E1,
                SplitE,
                E2,
                E3,
                E4,
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

-compile({inline, insert_INTERNAL4_C3 / ?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C3) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
                E1,
                E2,
                SplitE,
                E3,
                E4,
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

-compile({inline, insert_INTERNAL4_C4 / ?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C4) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
                E1,
                E2,
                E3,
                SplitE,
                E4,
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

-compile({inline, insert_INTERNAL4_C5 / ?INTERNAL4_ARITY_P1}).
insert_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS) ->
    case insert_recur(Elem, C5) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            split_internal(
                E1,
                E2,
                E3,
                E4,
                SplitE,
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

-compile({inline, insert_INTERNAL3 / ?INTERNAL3_ARITY_P1}).
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

-compile({inline, insert_INTERNAL3_C1 / ?INTERNAL3_ARITY_P1}).
insert_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL4(
                SplitE,
                E1,
                E2,
                E3,
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

-compile({inline, insert_INTERNAL3_C2 / ?INTERNAL3_ARITY_P1}).
insert_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL4(
                E1,
                SplitE,
                E2,
                E3,
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

-compile({inline, insert_INTERNAL3_C3 / ?INTERNAL3_ARITY_P1}).
insert_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C3) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL4(
                E1,
                E2,
                SplitE,
                E3,
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

-compile({inline, insert_INTERNAL3_C4 / ?INTERNAL3_ARITY_P1}).
insert_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C4) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL4(
                E1,
                E2,
                E3,
                SplitE,
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

-compile({inline, insert_INTERNAL2 / ?INTERNAL2_ARITY_P1}).
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

-compile({inline, insert_INTERNAL2_C1 / ?INTERNAL2_ARITY_P1}).
insert_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL3(
                SplitE,
                E1,
                E2,
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

-compile({inline, insert_INTERNAL2_C2 / ?INTERNAL2_ARITY_P1}).
insert_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL3(
                E1,
                SplitE,
                E2,
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

-compile({inline, insert_INTERNAL2_C3 / ?INTERNAL2_ARITY_P1}).
insert_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS) ->
    case insert_recur(Elem, C3) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL3(
                E1,
                E2,
                SplitE,
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

-compile({inline, insert_INTERNAL1 / ?INTERNAL1_ARITY_P1}).
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

-compile({inline, insert_INTERNAL1_C1 / ?INTERNAL1_ARITY_P1}).
insert_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL2(
                SplitE,
                E1,
                %
                SplitL,
                SplitR,
                C2
            );
        %
        UpdatedC1 ->
            ?INTERNAL1_C1(UpdatedC1)
    end.

-compile({inline, insert_INTERNAL1_C2 / ?INTERNAL1_ARITY_P1}).
insert_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?INTERNAL2(
                E1,
                SplitE,
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
%% ?LEAF4
%%

-compile({inline, insert_LEAF4 / ?LEAF4_ARITY_P1}).
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

-compile({inline, insert_LEAF4_POS1 / ?LEAF4_ARITY_P1}).
insert_LEAF4_POS1(Elem, ?LEAF4_ARGS) ->
    split_leaf(Elem, E1, E2, E3, E4).

-compile({inline, insert_LEAF4_POS2 / ?LEAF4_ARITY_P1}).
insert_LEAF4_POS2(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, Elem, E2, E3, E4).

-compile({inline, insert_LEAF4_POS3 / ?LEAF4_ARITY_P1}).
insert_LEAF4_POS3(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, Elem, E3, E4).

-compile({inline, insert_LEAF4_POS4 / ?LEAF4_ARITY_P1}).
insert_LEAF4_POS4(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, E3, Elem, E4).

-compile({inline, insert_LEAF4_POS5 / ?LEAF4_ARITY_P1}).
insert_LEAF4_POS5(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, E3, E4, Elem).

%%
%% ?LEAF3
%%

-compile({inline, insert_LEAF3 / ?LEAF3_ARITY_P1}).
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

-compile({inline, insert_LEAF3_POS1 / ?LEAF3_ARITY_P1}).
insert_LEAF3_POS1(Elem, ?LEAF3_ARGS) ->
    ?LEAF4(Elem, E1, E2, E3).

-compile({inline, insert_LEAF3_POS2 / ?LEAF3_ARITY_P1}).
insert_LEAF3_POS2(Elem, ?LEAF3_ARGS) ->
    ?LEAF4(E1, Elem, E2, E3).

-compile({inline, insert_LEAF3_POS3 / ?LEAF3_ARITY_P1}).
insert_LEAF3_POS3(Elem, ?LEAF3_ARGS) ->
    ?LEAF4(E1, E2, Elem, E3).

-compile({inline, insert_LEAF3_POS4 / ?LEAF3_ARITY_P1}).
insert_LEAF3_POS4(Elem, ?LEAF3_ARGS) ->
    ?LEAF4(E1, E2, E3, Elem).

%%
%% ?LEAF2
%%

-compile({inline, insert_LEAF2 / ?LEAF2_ARITY_P1}).
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

-compile({inline, insert_LEAF2_POS1 / ?LEAF2_ARITY_P1}).
insert_LEAF2_POS1(Elem, ?LEAF2_ARGS) ->
    ?LEAF3(Elem, E1, E2).

-compile({inline, insert_LEAF2_POS2 / ?LEAF2_ARITY_P1}).
insert_LEAF2_POS2(Elem, ?LEAF2_ARGS) ->
    ?LEAF3(E1, Elem, E2).

-compile({inline, insert_LEAF2_POS3 / ?LEAF2_ARITY_P1}).
insert_LEAF2_POS3(Elem, ?LEAF2_ARGS) ->
    ?LEAF3(E1, E2, Elem).

%%
%% ?LEAF1
%%

-compile({inline, insert_LEAF1 / ?LEAF1_ARITY_P1}).
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

-compile({inline, insert_LEAF1_POS1 / ?LEAF1_ARITY_P1}).
insert_LEAF1_POS1(Elem, ?LEAF1_ARGS) ->
    ?LEAF2(Elem, E1).

-compile({inline, insert_LEAF1_POS2 / ?LEAF1_ARITY_P1}).
insert_LEAF1_POS2(Elem, ?LEAF1_ARGS) ->
    ?LEAF2(E1, Elem).

%%
%% Split
%%

-compile({inline, split_internal/11}).
split_internal(
    E1,
    E2,
    E3,
    E4,
    E5,
    %
    C1,
    C2,
    C3,
    C4,
    C5,
    C6
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
%% Internal Function Definitions: intersection/1
%% ------------------------------------------------------------------

intersection_recur([Head1 | Tail1], Iter2, Root, Count) ->
    case Head1 of
        ?ITER_ELEM(Elem1) ->
            Next1 = Tail1,
            intersection_recur(Elem1, Next1, Iter2, Root, Count);
        %
        Node1 ->
            [?ITER_ELEM(Elem1) | Next1] = fwd_iterator_recur(Node1, Tail1),
            intersection_recur(Elem1, Next1, Iter2, Root, Count)
    end;
intersection_recur([], _Iter2, Root, Count) ->
    [Count | Root].

intersection_recur(Elem1, Next1, [Head2 | Tail2], Root, Count) ->
    case Head2 of
        ?ITER_ELEM(Elem2) ->
            Next2 = Tail2,
            intersection_recur(Elem1, Next1, Elem2, Next2, Root, Count);
        %
        Node2 ->
            [?ITER_ELEM(Elem2) | Next2] = fwd_iterator_recur(Node2, Tail2),
            intersection_recur(Elem1, Next1, Elem2, Next2, Root, Count)
    end;
intersection_recur(_Elem1, _Next1, [], Root, Count) ->
    [Count | Root].

intersection_recur(Elem1, Next1, Elem2, Next2, Root, Count) ->
    if
        Elem1 < Elem2 ->
            intersection_recur(Elem2, Next2, Next1, Root, Count);
        %
        Elem1 > Elem2 ->
            intersection_recur(Elem1, Next1, Next2, Root, Count);
        %
        true ->
            UpdatedRoot = insert(Elem1, Root),
            UpdatedCount = Count + 1,
            intersection_recur(Next1, Next2, UpdatedRoot, UpdatedCount)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_disjoint/2
%% ------------------------------------------------------------------

is_disjoint_root(Root1, Root2) ->
    MinElem = smallest(Root2),
    MaxElem = largest(Root2),
    Iter = bound_fwd_iterator(MinElem, Root1),
    is_disjoint_recur(Iter, Root2, MaxElem).

is_disjoint_recur([Head | Tail], Root2, MaxElem) ->
    case Head of
        ?ITER_ELEM(Elem) ->
            Next = Tail,
            is_disjoint_recur(Elem, Next, Root2, MaxElem);
        %
        Node ->
            [?ITER_ELEM(Elem) | Next] = fwd_iterator_recur(Node, Tail),
            is_disjoint_recur(Elem, Next, Root2, MaxElem)
    end;
is_disjoint_recur([], _Root2, _MaxElem) ->
    true.

is_disjoint_recur(Elem, Next, Root2, MaxElem) ->
    case is_member(Elem, Root2) of
        true ->
            false;
        %
        _ when Elem > MaxElem ->
            true;
        %
        _ ->
            is_disjoint_recur(Next, Root2, MaxElem)
    end.

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

-compile({inline, is_member_INTERNAL4 / ?INTERNAL4_ARITY_P1}).
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

-compile({inline, is_member_INTERNAL3 / ?INTERNAL3_ARITY_P1}).
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

-compile({inline, is_member_INTERNAL2 / ?INTERNAL2_ARITY_P1}).
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

-compile({inline, is_member_INTERNAL1 / ?INTERNAL1_ARITY_P1}).
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

-compile({inline, is_member_LEAF4 / ?LEAF4_ARITY_P1}).
is_member_LEAF4(Elem, ?LEAF4_ARGS) ->
    (Elem == E1 orelse
        Elem == E2 orelse
        Elem == E3 orelse
        Elem == E4).

%%
%% ?LEAF3
%%

-compile({inline, is_member_LEAF3 / ?LEAF3_ARITY_P1}).
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

-compile({inline, is_member_LEAF2 / ?LEAF2_ARITY_P1}).
is_member_LEAF2(Elem, ?LEAF2_ARGS) ->
    Elem == E1 orelse Elem == E2.

%%
%% ?LEAF1
%%

-compile({inline, is_member_LEAF1 / ?LEAF1_ARITY_P1}).
is_member_LEAF1(Elem, ?LEAF1_ARGS) ->
    Elem == E1.

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_subset/2
%% ------------------------------------------------------------------

is_subset_recur([Head | Tail], Root2) ->
    case Head of
        ?ITER_ELEM(Elem) ->
            Next = Tail,
            is_subset_recur(Elem, Next, Root2);
        %
        Node ->
            [?ITER_ELEM(Elem) | Next] = fwd_iterator_recur(Node, Tail),
            is_subset_recur(Elem, Next, Root2)
    end;
is_subset_recur([], _Root2) ->
    true.

is_subset_recur(Elem, Next, Root2) ->
    is_member(Elem, Root2) andalso is_subset_recur(Next, Root2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - forward
%% ------------------------------------------------------------------

fwd_iterator(?INTERNAL1_MATCH(E1, C1, C2)) ->
    Acc = [?ITER_ELEM(E1), C2],
    fwd_iterator_recur(C1, Acc);
fwd_iterator(?LEAF1_MATCH(E1)) ->
    Iter = [?ITER_ELEM(E1)],
    Iter;
fwd_iterator(?LEAF0_MATCH) ->
    Iter = [],
    Iter;
fwd_iterator(Root) ->
    Acc = [],
    fwd_iterator_recur(Root, Acc).

fwd_iterator_recur(?LEAF2_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_ELEM(E1), ?ITER_ELEM(E2) | Acc],
    Acc2;
fwd_iterator_recur(?LEAF3_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_ELEM(E1), ?ITER_ELEM(E2), ?ITER_ELEM(E3) | Acc],
    Acc2;
fwd_iterator_recur(?LEAF4_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_ELEM(E1), ?ITER_ELEM(E2), ?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc],
    Acc2;
fwd_iterator_recur(?INTERNAL2_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_ELEM(E1),
        C2,
        ?ITER_ELEM(E2),
        C3
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?INTERNAL3_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_ELEM(E1),
        C2,
        ?ITER_ELEM(E2),
        C3,
        ?ITER_ELEM(E3),
        C4
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?INTERNAL4_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_ELEM(E1),
        C2,
        ?ITER_ELEM(E2),
        C3,
        ?ITER_ELEM(E3),
        C4,
        ?ITER_ELEM(E4),
        C5
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - reverse
%% ------------------------------------------------------------------

rev_iterator(?INTERNAL1_MATCH(E1, C1, C2)) ->
    Acc = [?ITER_ELEM(E1), C1],
    rev_iterator_recur(C2, Acc);
rev_iterator(?LEAF1_MATCH(E1)) ->
    Iter = [?ITER_ELEM(E1)],
    Iter;
rev_iterator(?LEAF0_MATCH) ->
    Iter = [],
    Iter;
rev_iterator(Root) ->
    Acc = [],
    rev_iterator_recur(Root, Acc).

rev_iterator_recur(?LEAF2_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc],
    Acc2;
rev_iterator_recur(?LEAF3_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_ELEM(E3), ?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc],
    Acc2;
rev_iterator_recur(?LEAF4_MATCH_ALL, Acc) ->
    Acc2 = [?ITER_ELEM(E4), ?ITER_ELEM(E3), ?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc],
    Acc2;
rev_iterator_recur(?INTERNAL2_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_ELEM(E2),
        C2,
        ?ITER_ELEM(E1),
        C1
        | Acc
    ],
    rev_iterator_recur(C3, Acc2);
rev_iterator_recur(?INTERNAL3_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_ELEM(E3),
        C3,
        ?ITER_ELEM(E2),
        C2,
        ?ITER_ELEM(E1),
        C1
        | Acc
    ],
    rev_iterator_recur(C4, Acc2);
rev_iterator_recur(?INTERNAL4_MATCH_ALL, Acc) ->
    Acc2 = [
        ?ITER_ELEM(E4),
        C4,
        ?ITER_ELEM(E3),
        C3,
        ?ITER_ELEM(E2),
        C2,
        ?ITER_ELEM(E1),
        C1
        | Acc
    ],
    rev_iterator_recur(C5, Acc2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_fwd_iterator(Elem, Root) ->
    case Root of
        ?INTERNAL1_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL1(Elem, ?INTERNAL1_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            bound_fwd_iterator_LEAF1(Elem, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            bound_fwd_iterator_recur(Elem, Root, Acc)
    end.

bound_fwd_iterator_recur(Elem, Node, Acc) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            bound_fwd_iterator_LEAF2(Elem, ?LEAF2_ARGS, Acc);
        %
        ?LEAF3_MATCH_ALL ->
            bound_fwd_iterator_LEAF3(Elem, ?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            bound_fwd_iterator_LEAF4(Elem, ?LEAF4_ARGS, Acc);
        %
        ?INTERNAL2_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            bound_fwd_iterator_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc)
    end.

%% INTERNAL4

-compile({inline, bound_fwd_iterator_INTERNAL4 / ?INTERNAL4_ARITY_P2}).
bound_fwd_iterator_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc) ->
    case Acc of
        _ when Elem =< E1 ->
            Acc2 = [
                ?ITER_ELEM(E1),
                C2,
                ?ITER_ELEM(E2),
                C3,
                ?ITER_ELEM(E3),
                C4,
                ?ITER_ELEM(E4),
                C5
                | Acc
            ],

            bound_fwd_iterator_recur(Elem, C1, Acc2);
        %
        _ when Elem =< E2 ->
            Acc2 = [
                ?ITER_ELEM(E2),
                C3,
                ?ITER_ELEM(E3),
                C4,
                ?ITER_ELEM(E4),
                C5
                | Acc
            ],

            bound_fwd_iterator_recur(Elem, C2, Acc2);
        %
        _ when Elem =< E3 ->
            Acc2 = [
                ?ITER_ELEM(E3),
                C4,
                ?ITER_ELEM(E4),
                C5
                | Acc
            ],

            bound_fwd_iterator_recur(Elem, C3, Acc2);
        %
        _ when Elem =< E4 ->
            Acc2 = [
                ?ITER_ELEM(E4),
                C5
                | Acc
            ],

            bound_fwd_iterator_recur(Elem, C4, Acc2);
        %
        [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
            % We overshot when recursing from this node's parent, stop here
            % since no more elements can possibly be included.
            Acc;
        %
        _ ->
            bound_fwd_iterator_recur(Elem, C5, Acc)
    end.

%% INTERNAL3

-compile({inline, bound_fwd_iterator_INTERNAL3 / ?INTERNAL3_ARITY_P2}).
bound_fwd_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc) ->
    case Acc of
        _ when Elem =< E1 ->
            Acc2 = [
                ?ITER_ELEM(E1),
                C2,
                ?ITER_ELEM(E2),
                C3,
                ?ITER_ELEM(E3),
                C4
                | Acc
            ],

            bound_fwd_iterator_recur(Elem, C1, Acc2);
        %
        _ when Elem =< E2 ->
            Acc2 = [
                ?ITER_ELEM(E2),
                C3,
                ?ITER_ELEM(E3),
                C4
                | Acc
            ],

            bound_fwd_iterator_recur(Elem, C2, Acc2);
        %
        _ when Elem =< E3 ->
            Acc2 = [
                ?ITER_ELEM(E3),
                C4
                | Acc
            ],

            bound_fwd_iterator_recur(Elem, C3, Acc2);
        %
        [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
            % We overshot when recursing from this node's parent, stop here
            % since no more elements can possibly be included.
            Acc;
        %
        _ ->
            bound_fwd_iterator_recur(Elem, C4, Acc)
    end.

%% INTERNAL2

-compile({inline, bound_fwd_iterator_INTERNAL2 / ?INTERNAL2_ARITY_P2}).
bound_fwd_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc) ->
    case Acc of
        _ when Elem =< E1 ->
            Acc2 = [
                ?ITER_ELEM(E1),
                C2,
                ?ITER_ELEM(E2),
                C3
                | Acc
            ],

            bound_fwd_iterator_recur(Elem, C1, Acc2);
        %
        _ when Elem =< E2 ->
            Acc2 = [
                ?ITER_ELEM(E2),
                C3
                | Acc
            ],

            bound_fwd_iterator_recur(Elem, C2, Acc2);
        %
        [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
            % We overshot when recursing from this node's parent, stop here
            % since no more elements can possibly be included.
            Acc;
        %
        _ ->
            bound_fwd_iterator_recur(Elem, C3, Acc)
    end.

%% INTERNAL1

-compile({inline, bound_fwd_iterator_INTERNAL1 / ?INTERNAL1_ARITY_P1}).
bound_fwd_iterator_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem < E1 ->
            Acc = [?ITER_ELEM(E1), C2],
            bound_fwd_iterator_recur(Elem, C1, Acc);
        %
        Elem > E1 ->
            Acc = [],
            bound_fwd_iterator_recur(Elem, C2, Acc);
        %
        true ->
            Acc = [?ITER_ELEM(E1)],
            bound_fwd_iterator_recur(Elem, C2, Acc)
    end.

%% LEAF4

-compile({inline, bound_fwd_iterator_LEAF4 / ?LEAF4_ARITY_P2}).
bound_fwd_iterator_LEAF4(Elem, ?LEAF4_ARGS, Acc) ->
    if
        Elem =< E1 ->
            [?ITER_ELEM(E1), ?ITER_ELEM(E2), ?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc];
        %
        Elem =< E2 ->
            [?ITER_ELEM(E2), ?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc];
        %
        Elem =< E3 ->
            [?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc];
        %
        Elem =< E4 ->
            [?ITER_ELEM(E4) | Acc];
        %
        true ->
            Acc
    end.

%% LEAF3

-compile({inline, bound_fwd_iterator_LEAF3 / ?LEAF3_ARITY_P2}).
bound_fwd_iterator_LEAF3(Elem, ?LEAF3_ARGS, Acc) ->
    if
        Elem =< E1 ->
            [?ITER_ELEM(E1), ?ITER_ELEM(E2), ?ITER_ELEM(E3) | Acc];
        %
        Elem =< E2 ->
            [?ITER_ELEM(E2), ?ITER_ELEM(E3) | Acc];
        %
        Elem =< E3 ->
            [?ITER_ELEM(E3) | Acc];
        %
        true ->
            Acc
    end.

%% LEAF2

-compile({inline, bound_fwd_iterator_LEAF2 / ?LEAF2_ARITY_P2}).
bound_fwd_iterator_LEAF2(Elem, ?LEAF2_ARGS, Acc) ->
    if
        Elem =< E1 ->
            [?ITER_ELEM(E1), ?ITER_ELEM(E2) | Acc];
        %
        Elem =< E2 ->
            [?ITER_ELEM(E2) | Acc];
        %
        true ->
            Acc
    end.

%% LEAF1

-compile({inline, bound_fwd_iterator_LEAF1 / ?LEAF1_ARITY_P1}).
bound_fwd_iterator_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem =< E1 ->
            [?ITER_ELEM(E1)];
        %
        true ->
            []
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: largest/1
%% ------------------------------------------------------------------

largest_recur(Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH(_, _, _, _, C3) ->
            largest_recur(C3);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, C4) ->
            largest_recur(C4);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, C5) ->
            largest_recur(C5);
        %
        ?LEAF2_MATCH(_, E2) ->
            E2;
        %
        ?LEAF3_MATCH(_, _, E3) ->
            E3;
        %
        ?LEAF4_MATCH(_, _, _, E4) ->
            E4
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
        ?ITER_ELEM(Elem) ->
            Iter2 = Tail,
            {Elem, Iter2};
        %
        Node ->
            [?ITER_ELEM(Elem) | Next] = fwd_iterator_recur(Node, Tail),
            Iter2 = Next,
            {Elem, Iter2}
    end.

rev_next([Head | Tail]) ->
    case Head of
        ?ITER_ELEM(Elem) ->
            Iter2 = [?REV_ITER_TAG | Tail],
            {Elem, Iter2};
        %
        Node ->
            [?ITER_ELEM(Elem) | Next] = rev_iterator_recur(Node, Tail),
            Iter2 = [?REV_ITER_TAG | Next],
            {Elem, Iter2}
    end;
rev_next([]) ->
    none.

%% ------------------------------------------------------------------
%% Internal Function Definitions: smallest/1
%% ------------------------------------------------------------------

smallest_recur(Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH(_, _, C1, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL3_MATCH(_, _, _, C1, _, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, C1, _, _, _, _) ->
            smallest_recur(C1);
        %
        ?LEAF2_MATCH(E1, _) ->
            E1;
        %
        ?LEAF3_MATCH(E1, _, _) ->
            E1;
        %
        ?LEAF4_MATCH(E1, _, _, _) ->
            E1
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_largest/2
%% ------------------------------------------------------------------

take_largest_recur(Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            take_largest_INTERNAL2(?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            take_largest_INTERNAL3(?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            take_largest_INTERNAL4(?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            take_largest_LEAF2(?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            take_largest_LEAF3(?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            take_largest_LEAF4(?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, take_largest_INTERNAL4 / ?INTERNAL4_ARITY}).
take_largest_INTERNAL4(?INTERNAL4_ARGS) ->
    [Taken | UpdatedC5] = take_largest_recur(C5),
    [Taken | ?INTERNAL4_C5_REBALANCE(UpdatedC5)].

%%
%% ?INTERNAL3
%%

-compile({inline, take_largest_INTERNAL3 / ?INTERNAL3_ARITY}).
take_largest_INTERNAL3(?INTERNAL3_ARGS) ->
    [Taken | UpdatedC4] = take_largest_recur(C4),
    [Taken | ?INTERNAL3_C4_REBALANCE(UpdatedC4)].

%%
%% ?INTERNAL2
%%

-compile({inline, take_largest_INTERNAL2 / ?INTERNAL2_ARITY}).
take_largest_INTERNAL2(?INTERNAL2_ARGS) ->
    [Taken | UpdatedC3] = take_largest_recur(C3),
    [Taken | ?INTERNAL2_C3_REBALANCE(UpdatedC3)].

%%
%% ?INTERNAL1
%%

-compile({inline, take_largest_INTERNAL1 / ?INTERNAL1_ARITY}).
take_largest_INTERNAL1(?INTERNAL1_ARGS) ->
    [Taken | UpdatedC2] = take_largest_recur(C2),
    [Taken | ?INTERNAL1_C2_REBALANCE(UpdatedC2)].

%%
%% ?LEAF4
%%

-compile({inline, take_largest_LEAF4 / ?LEAF4_ARITY}).
take_largest_LEAF4(?LEAF4_ARGS) ->
    ?TAKEN(E4, ?LEAF3(E1, E2, E3)).

%%
%% ?LEAF3
%%

-compile({inline, take_largest_LEAF3 / ?LEAF3_ARITY}).
take_largest_LEAF3(?LEAF3_ARGS) ->
    ?TAKEN(E3, ?LEAF2(E1, E2)).

%%
%% ?LEAF2
%%

-compile({inline, take_largest_LEAF2 / ?LEAF2_ARITY}).
take_largest_LEAF2(?LEAF2_ARGS) ->
    ?TAKEN(E2, ?LEAF1(E1)).

%%
%% ?LEAF1
%%

-compile({inline, take_largest_LEAF1 / ?LEAF1_ARITY}).
take_largest_LEAF1(?LEAF1_ARGS) ->
    ?TAKEN(E1, ?LEAF0).

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

-compile({inline, take_smallest_INTERNAL4 / ?INTERNAL4_ARITY}).
take_smallest_INTERNAL4(?INTERNAL4_ARGS) ->
    [Taken | UpdatedC1] = take_smallest_recur(C1),
    [Taken | ?INTERNAL4_C1_REBALANCE(UpdatedC1)].

%%
%% ?INTERNAL3
%%

-compile({inline, take_smallest_INTERNAL3 / ?INTERNAL3_ARITY}).
take_smallest_INTERNAL3(?INTERNAL3_ARGS) ->
    [Taken | UpdatedC1] = take_smallest_recur(C1),
    [Taken | ?INTERNAL3_C1_REBALANCE(UpdatedC1)].

%%
%% ?INTERNAL2
%%

-compile({inline, take_smallest_INTERNAL2 / ?INTERNAL2_ARITY}).
take_smallest_INTERNAL2(?INTERNAL2_ARGS) ->
    [Taken | UpdatedC1] = take_smallest_recur(C1),
    [Taken | ?INTERNAL2_C1_REBALANCE(UpdatedC1)].

%%
%% ?INTERNAL1
%%

-compile({inline, take_smallest_INTERNAL1 / ?INTERNAL1_ARITY}).
take_smallest_INTERNAL1(?INTERNAL1_ARGS) ->
    [Taken | UpdatedC1] = take_smallest_recur(C1),
    [Taken | ?INTERNAL1_C1_REBALANCE(UpdatedC1)].

%%
%% ?LEAF4
%%

-compile({inline, take_smallest_LEAF4 / ?LEAF4_ARITY}).
take_smallest_LEAF4(?LEAF4_ARGS) ->
    ?TAKEN(E1, ?LEAF3(E2, E3, E4)).

%%
%% ?LEAF3
%%

-compile({inline, take_smallest_LEAF3 / ?LEAF3_ARITY}).
take_smallest_LEAF3(?LEAF3_ARGS) ->
    ?TAKEN(E1, ?LEAF2(E2, E3)).

%%
%% ?LEAF2
%%

-compile({inline, take_smallest_LEAF2 / ?LEAF2_ARITY}).
take_smallest_LEAF2(?LEAF2_ARGS) ->
    ?TAKEN(E1, ?LEAF1(E2)).

%%
%% ?LEAF1
%%

-compile({inline, take_smallest_LEAF1 / ?LEAF1_ARITY}).
take_smallest_LEAF1(?LEAF1_ARGS) ->
    ?TAKEN(E1, ?LEAF0).

%% ------------------------------------------------------------------
%% Internal Function Definitions: union/2
%% ------------------------------------------------------------------

union_root(Root1, Size1, Root2) ->
    Acc = [Size1 | Root1],
    Target = Root2,

    case Target of
        ?INTERNAL1_MATCH_ALL ->
            union_INTERNAL1(?INTERNAL1_ARGS, Acc);
        %
        ?LEAF1_MATCH_ALL ->
            union_LEAF1(?LEAF1_ARGS, Acc);
        %
        ?LEAF0 ->
            Acc;
        %
        _ ->
            union_recur(Target, Acc)
    end.

union_recur(Node, Acc) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            union_LEAF2(?LEAF2_ARGS, Acc);
        %
        ?LEAF3_MATCH_ALL ->
            union_LEAF3(?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            union_LEAF4(?LEAF4_ARGS, Acc);
        %
        ?INTERNAL2_MATCH_ALL ->
            union_INTERNAL2(?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            union_INTERNAL3(?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            union_INTERNAL4(?INTERNAL4_ARGS, Acc)
    end.

%% INTERNAL4

-compile({inline, union_INTERNAL4 / ?INTERNAL4_ARITY_P1}).
union_INTERNAL4(?INTERNAL4_ARGS, Acc) ->
    Acc2 = union_recur(C1, Acc),
    Acc3 = union_elem(E1, Acc2),

    Acc4 = union_recur(C2, Acc3),
    Acc5 = union_elem(E2, Acc4),

    Acc6 = union_recur(C3, Acc5),
    Acc7 = union_elem(E3, Acc6),

    Acc8 = union_recur(C4, Acc7),
    Acc9 = union_elem(E4, Acc8),

    union_recur(C5, Acc9).

%% INTERNAL3

-compile({inline, union_INTERNAL3 / ?INTERNAL3_ARITY_P1}).
union_INTERNAL3(?INTERNAL3_ARGS, Acc) ->
    Acc2 = union_recur(C1, Acc),
    Acc3 = union_elem(E1, Acc2),

    Acc4 = union_recur(C2, Acc3),
    Acc5 = union_elem(E2, Acc4),

    Acc6 = union_recur(C3, Acc5),
    Acc7 = union_elem(E3, Acc6),

    union_recur(C4, Acc7).

%% INTERNAL2

-compile({inline, union_INTERNAL2 / ?INTERNAL2_ARITY_P1}).
union_INTERNAL2(?INTERNAL2_ARGS, Acc) ->
    Acc2 = union_recur(C1, Acc),
    Acc3 = union_elem(E1, Acc2),

    Acc4 = union_recur(C2, Acc3),
    Acc5 = union_elem(E2, Acc4),

    union_recur(C3, Acc5).

%% INTERNAL1

-compile({inline, union_INTERNAL1 / ?INTERNAL1_ARITY_P1}).
union_INTERNAL1(?INTERNAL1_ARGS, Acc) ->
    Acc2 = union_recur(C1, Acc),
    Acc3 = union_elem(E1, Acc2),

    union_recur(C2, Acc3).

%% LEAF4

-compile({inline, union_LEAF4 / ?LEAF4_ARITY_P1}).
union_LEAF4(?LEAF4_ARGS, Acc) ->
    Acc2 = union_elem(E1, Acc),
    Acc3 = union_elem(E2, Acc2),
    Acc4 = union_elem(E3, Acc3),
    union_elem(E4, Acc4).

%% LEAF3

-compile({inline, union_LEAF3 / ?LEAF3_ARITY_P1}).
union_LEAF3(?LEAF3_ARGS, Acc) ->
    Acc2 = union_elem(E1, Acc),
    Acc3 = union_elem(E2, Acc2),
    union_elem(E3, Acc3).

%% LEAF2

-compile({inline, union_LEAF2 / ?LEAF2_ARITY_P1}).
union_LEAF2(?LEAF2_ARGS, Acc) ->
    Acc2 = union_elem(E1, Acc),
    union_elem(E2, Acc2).

%% LEAF1

-compile({inline, union_LEAF1 / ?LEAF1_ARITY_P1}).
union_LEAF1(?LEAF1_ARGS, Acc) ->
    union_elem(E1, Acc).

%% Individual elements

-compile({inline, union_elem/2}).
union_elem(Elem, [Count | Root] = Acc) ->
    try insert(Elem, Root) of
        UpdatedRoot ->
            [Count + 1 | UpdatedRoot]
    catch
        error:{key_exists, K} when K =:= Elem ->
            Acc
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL4
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL4_C1 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C1(?INTERNAL4_ARGS) ->
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

            rebalance_INTERNAL4_C1_finish(Result, E2, E3, E4, C3, C4, C5);
        %
        UpdatedC1 ->
            ?INTERNAL4_C1(UpdatedC1)
    end.

-compile({inline, rebalance_INTERNAL4_C1_finish / ?INTERNAL4_ARITY_M2}).
rebalance_INTERNAL4_C1_finish(Result, E2, E3, E4, C3, C4, C5) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?INTERNAL4(UpElem, E2, E3, E4, UpdatedC1, UpdatedC2, C3, C4, C5);
        %
        ?MERGED(MergedC1C2) ->
            ?INTERNAL3(E2, E3, E4, MergedC1C2, C3, C4, C5)
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL4_C2 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C2(?INTERNAL4_ARGS) ->
    case C2 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                CE,
                CL,
                CR,
                %
                E1,
                C1,
                %
                E2,
                C3
            ),

            rebalance_INTERNAL4_C2_finish(Result, E1, E2, E3, E4, C1, C3, C4, C5);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_either_sibling(
                CE,
                %
                E1,
                C1,
                %
                E2,
                C3
            ),

            rebalance_INTERNAL4_C2_finish(Result, E1, E2, E3, E4, C1, C3, C4, C5);
        %
        UpdatedC2 ->
            ?INTERNAL4_C2(UpdatedC2)
    end.

-compile({inline, rebalance_INTERNAL4_C2_finish / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C2_finish(Result, E1, E2, E3, E4, C1, C3, C4, C5) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC1C2) ->
            ?INTERNAL3(
                E2,
                E3,
                E4,
                %
                MergedC1C2,
                C3,
                C4,
                C5
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpElem, RebalancedC2, UpdatedC3) ->
            ?INTERNAL4(
                E1,
                UpElem,
                E3,
                E4,
                %
                C1,
                RebalancedC2,
                UpdatedC3,
                C4,
                C5
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedC1, RebalancedC2) ->
            ?INTERNAL4(
                UpElem,
                E2,
                E3,
                E4,
                %
                UpdatedC1,
                RebalancedC2,
                C3,
                C4,
                C5
            )
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C3(?INTERNAL4_ARGS) ->
    case C3 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                CE,
                CL,
                CR,
                %
                E2,
                C2,
                %
                E3,
                C4
            ),

            rebalance_INTERNAL4_C3_finish(Result, E1, E2, E3, E4, C1, C2, C4, C5);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_either_sibling(
                CE,
                %
                E2,
                C2,
                %
                E3,
                C4
            ),

            rebalance_INTERNAL4_C3_finish(Result, E1, E2, E3, E4, C1, C2, C4, C5);
        %
        UpdatedC3 ->
            ?INTERNAL4_C3(UpdatedC3)
    end.

-compile({inline, rebalance_INTERNAL4_C3_finish / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C3_finish(Result, E1, E2, E3, E4, C1, C2, C4, C5) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC2C3) ->
            ?INTERNAL3(
                E1,
                E3,
                E4,
                %
                C1,
                MergedC2C3,
                C4,
                C5
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpElem, RebalancedC3, UpdatedC4) ->
            ?INTERNAL4(
                E1,
                E2,
                UpElem,
                E4,
                %
                C1,
                C2,
                RebalancedC3,
                UpdatedC4,
                C5
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedC2, RebalancedC3) ->
            ?INTERNAL4(
                E1,
                UpElem,
                E3,
                E4,
                %
                C1,
                UpdatedC2,
                RebalancedC3,
                C4,
                C5
            )
    end.

%%
%% C4
%%

-compile({inline, rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C4(?INTERNAL4_ARGS) ->
    case C4 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                CE,
                CL,
                CR,
                %
                E3,
                C3,
                %
                E4,
                C5
            ),

            rebalance_INTERNAL4_C4_finish(Result, E1, E2, E3, E4, C1, C2, C3, C5);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_either_sibling(
                CE,
                %
                E3,
                C3,
                %
                E4,
                C5
            ),

            rebalance_INTERNAL4_C4_finish(Result, E1, E2, E3, E4, C1, C2, C3, C5);
        %
        UpdatedC4 ->
            ?INTERNAL4_C4(UpdatedC4)
    end.

-compile({inline, rebalance_INTERNAL4_C4_finish / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C4_finish(Result, E1, E2, E3, E4, C1, C2, C3, C5) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC3C4) ->
            ?INTERNAL3(
                E1,
                E2,
                E4,
                %
                C1,
                C2,
                MergedC3C4,
                C5
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpElem, RebalancedC4, UpdatedC5) ->
            ?INTERNAL4(
                E1,
                E2,
                E3,
                UpElem,
                %
                C1,
                C2,
                C3,
                RebalancedC4,
                UpdatedC5
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedC3, RebalancedC4) ->
            ?INTERNAL4(
                E1,
                E2,
                UpElem,
                E4,
                %
                C1,
                C2,
                UpdatedC3,
                RebalancedC4,
                C5
            )
    end.

%%
%% C5
%%

-compile({inline, rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY}).
rebalance_INTERNAL4_C5(?INTERNAL4_ARGS) ->
    case C5 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_left_sibling(
                CE,
                CL,
                CR,
                %
                E4,
                C4
            ),

            rebalance_INTERNAL4_C5_finish(Result, E1, E2, E3, C1, C2, C3);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_left_sibling(
                CE,
                %
                E4,
                C4
            ),

            rebalance_INTERNAL4_C5_finish(Result, E1, E2, E3, C1, C2, C3);
        %
        UpdatedC5 ->
            ?INTERNAL4_C5(UpdatedC5)
    end.

-compile({inline, rebalance_INTERNAL4_C5_finish / ?INTERNAL4_ARITY_M2}).
rebalance_INTERNAL4_C5_finish(Result, E1, E2, E3, C1, C2, C3) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC4, RebalancedC5) ->
            ?INTERNAL4(
                E1,
                E2,
                E3,
                UpElem,
                %
                C1,
                C2,
                C3,
                UpdatedC4,
                RebalancedC5
            );
        %
        ?MERGED(MergedC4C5) ->
            ?INTERNAL3(
                E1,
                E2,
                E3,
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

-compile({inline, rebalance_INTERNAL3_C1 / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C1(?INTERNAL3_ARGS) ->
    case C1 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_right_sibling(
                CE,
                CL,
                CR,
                E1,
                C2
            ),

            rebalance_INTERNAL3_C1_finish(Result, E2, E3, C3, C4);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_right_sibling(
                CE,
                E1,
                C2
            ),

            rebalance_INTERNAL3_C1_finish(Result, E2, E3, C3, C4);
        %
        UpdatedC1 ->
            ?INTERNAL3_C1(UpdatedC1)
    end.

-compile({inline, rebalance_INTERNAL3_C1_finish / ?INTERNAL3_ARITY_M2}).
rebalance_INTERNAL3_C1_finish(Result, E2, E3, C3, C4) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?INTERNAL3(UpElem, E2, E3, UpdatedC1, UpdatedC2, C3, C4);
        %
        ?MERGED(MergedC1C2) ->
            ?INTERNAL2(E2, E3, MergedC1C2, C3, C4)
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL3_C2 / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C2(?INTERNAL3_ARGS) ->
    case C2 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                CE,
                CL,
                CR,
                %
                E1,
                C1,
                %
                E2,
                C3
            ),

            rebalance_INTERNAL3_C2_finish(Result, E1, E2, E3, C1, C3, C4);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_either_sibling(
                CE,
                %
                E1,
                C1,
                %
                E2,
                C3
            ),

            rebalance_INTERNAL3_C2_finish(Result, E1, E2, E3, C1, C3, C4);
        %
        UpdatedC2 ->
            ?INTERNAL3_C2(UpdatedC2)
    end.

-compile({inline, rebalance_INTERNAL3_C2_finish / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C2_finish(Result, E1, E2, E3, C1, C3, C4) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC1C2) ->
            ?INTERNAL2(
                E2,
                E3,
                %
                MergedC1C2,
                C3,
                C4
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpElem, RebalancedC2, UpdatedC3) ->
            ?INTERNAL3(
                E1,
                UpElem,
                E3,
                %
                C1,
                RebalancedC2,
                UpdatedC3,
                C4
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedC1, RebalancedC2) ->
            ?INTERNAL3(
                UpElem,
                E2,
                E3,
                %
                UpdatedC1,
                RebalancedC2,
                C3,
                C4
            )
    end.

%%
%% C3
%%

-compile({inline, rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C3(?INTERNAL3_ARGS) ->
    case C3 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                CE,
                CL,
                CR,
                %
                E2,
                C2,
                %
                E3,
                C4
            ),

            rebalance_INTERNAL3_C3_finish(Result, E1, E2, E3, C1, C2, C4);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_either_sibling(
                CE,
                %
                E2,
                C2,
                %
                E3,
                C4
            ),

            rebalance_INTERNAL3_C3_finish(Result, E1, E2, E3, C1, C2, C4);
        %
        UpdatedC3 ->
            ?INTERNAL3_C3(UpdatedC3)
    end.

-compile({inline, rebalance_INTERNAL3_C3_finish / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C3_finish(Result, E1, E2, E3, C1, C2, C4) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC2C3) ->
            ?INTERNAL2(
                E1,
                E3,
                %
                C1,
                MergedC2C3,
                C4
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpElem, RebalancedC3, UpdatedC4) ->
            ?INTERNAL3(
                E1,
                E2,
                UpElem,
                %
                C1,
                C2,
                RebalancedC3,
                UpdatedC4
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedC2, RebalancedC3) ->
            ?INTERNAL3(
                E1,
                UpElem,
                E3,
                %
                C1,
                UpdatedC2,
                RebalancedC3,
                C4
            )
    end.

%%
%% C4
%%

-compile({inline, rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY}).
rebalance_INTERNAL3_C4(?INTERNAL3_ARGS) ->
    case C4 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_left_sibling(
                CE,
                CL,
                CR,
                %
                E3,
                C3
            ),

            rebalance_INTERNAL3_C4_finish(Result, E1, E2, C1, C2);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_left_sibling(
                CE,
                %
                E3,
                C3
            ),

            rebalance_INTERNAL3_C4_finish(Result, E1, E2, C1, C2);
        %
        UpdatedC4 ->
            ?INTERNAL3_C4(UpdatedC4)
    end.

-compile({inline, rebalance_INTERNAL3_C4_finish / ?INTERNAL3_ARITY_M2}).
rebalance_INTERNAL3_C4_finish(Result, E1, E2, C1, C2) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC3, RebalancedC4) ->
            ?INTERNAL3(
                E1,
                E2,
                UpElem,
                %
                C1,
                C2,
                UpdatedC3,
                RebalancedC4
            );
        %
        ?MERGED(MergedC3C4) ->
            ?INTERNAL2(
                E1,
                E2,
                %
                C1,
                C2,
                MergedC3C4
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalancing INTERNAL2
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, rebalance_INTERNAL2_C1 / ?INTERNAL2_ARITY}).
rebalance_INTERNAL2_C1(?INTERNAL2_ARGS) ->
    case C1 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_right_sibling(
                CE,
                CL,
                CR,
                E1,
                C2
            ),

            rebalance_INTERNAL2_C1_finish(Result, E2, C3);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_right_sibling(
                CE,
                E1,
                C2
            ),

            rebalance_INTERNAL2_C1_finish(Result, E2, C3);
        %
        UpdatedC1 ->
            ?INTERNAL2_C1(UpdatedC1)
    end.

-compile({inline, rebalance_INTERNAL2_C1_finish / ?INTERNAL2_ARITY_M2}).
rebalance_INTERNAL2_C1_finish(Result, E2, C3) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?INTERNAL2(UpElem, E2, UpdatedC1, UpdatedC2, C3);
        %
        ?MERGED(MergedC1C2) ->
            ?INTERNAL1(E2, MergedC1C2, C3)
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL2_C2 / ?INTERNAL2_ARITY}).
rebalance_INTERNAL2_C2(?INTERNAL2_ARGS) ->
    case C2 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_either_sibling(
                CE,
                CL,
                CR,
                %
                E1,
                C1,
                %
                E2,
                C3
            ),

            rebalance_INTERNAL2_C2_finish(Result, E1, E2, C1, C3);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_either_sibling(
                CE,
                %
                E1,
                C1,
                %
                E2,
                C3
            ),

            rebalance_INTERNAL2_C2_finish(Result, E1, E2, C1, C3);
        %
        UpdatedC2 ->
            ?INTERNAL2_C2(UpdatedC2)
    end.

-compile({inline, rebalance_INTERNAL2_C2_finish / ?INTERNAL2_ARITY}).
rebalance_INTERNAL2_C2_finish(Result, E1, E2, C1, C3) ->
    case Result of
        ?MID_MERGED_MATCH(MergedC1C2) ->
            ?INTERNAL1(
                E2,
                %
                MergedC1C2,
                C3
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpElem, RebalancedC2, UpdatedC3) ->
            ?INTERNAL2(
                E1,
                UpElem,
                %
                C1,
                RebalancedC2,
                UpdatedC3
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedC1, RebalancedC2) ->
            ?INTERNAL2(
                UpElem,
                E2,
                %
                UpdatedC1,
                RebalancedC2,
                C3
            )
    end.

%%
%% C4
%%

-compile({inline, rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY}).
rebalance_INTERNAL2_C3(?INTERNAL2_ARGS) ->
    case C3 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_left_sibling(
                CE,
                CL,
                CR,
                %
                E2,
                C2
            ),

            rebalance_INTERNAL2_C3_finish(Result, E1, C1);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_left_sibling(
                CE,
                %
                E2,
                C2
            ),

            rebalance_INTERNAL2_C3_finish(Result, E1, C1);
        %
        UpdatedC3 ->
            ?INTERNAL2_C3(UpdatedC3)
    end.

-compile({inline, rebalance_INTERNAL2_C3_finish / ?INTERNAL2_ARITY_M2}).
rebalance_INTERNAL2_C3_finish(Result, E1, C1) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC2, RebalancedC3) ->
            ?INTERNAL2(
                E1,
                UpElem,
                %
                C1,
                UpdatedC2,
                RebalancedC3
            );
        %
        ?MERGED(MergedC2C3) ->
            ?INTERNAL1(
                E1,
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

-compile({inline, rebalance_INTERNAL1_C1 / ?INTERNAL1_ARITY}).
rebalance_INTERNAL1_C1(?INTERNAL1_ARGS) ->
    case C1 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_right_sibling(
                CE,
                CL,
                CR,
                E1,
                C2
            ),

            rebalance_INTERNAL1_finish(Result);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_right_sibling(
                CE,
                E1,
                C2
            ),

            rebalance_INTERNAL1_finish(Result);
        %
        UpdatedC1 ->
            ?INTERNAL1_C1(UpdatedC1)
    end.

%%
%% C2
%%

-compile({inline, rebalance_INTERNAL1_C2 / ?INTERNAL1_ARITY}).
rebalance_INTERNAL1_C2(?INTERNAL1_ARGS) ->
    case C2 of
        ?INTERNAL1_MATCH(CE, CL, CR) ->
            Result = rebalance_internal_from_left_sibling(
                CE,
                CL,
                CR,
                %
                E1,
                C1
            ),

            rebalance_INTERNAL1_finish(Result);
        %
        ?LEAF1_MATCH(CE) ->
            Result = rebalance_leaf_from_left_sibling(
                CE,
                E1,
                C1
            ),

            rebalance_INTERNAL1_finish(Result);
        %
        UpdatedC2 ->
            ?INTERNAL1_C2(UpdatedC2)
    end.

%-compile({inline, rebalance_INTERNAL1_finish/?INTERNAL1_ARITY_M2}).
rebalance_INTERNAL1_finish(Result) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?INTERNAL1(UpElem, UpdatedC1, UpdatedC2);
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen at root - height is reduced
            MergedC1C2
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its right sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_right_sibling/7}).
rebalance_internal_from_right_sibling(
    CElem,
    CLeft,
    CRight,
    %
    ParentE,
    Right
) ->
    case Right of
        %?INTERNAL2_MATCH(E1, E2, Values, C1, C2, C3) ->
        ?INTERNAL2_MATCH_ALL ->
            MergedNode = ?INTERNAL4(
                CElem,
                ParentE,
                E1,
                E2,
                %
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
        ?INTERNAL3_MATCH_ALL ->
            UpElem = E1,
            MovedC = C1,

            UpdatedNode = ?INTERNAL2(
                CElem,
                ParentE,
                %
                CLeft,
                CRight,
                MovedC
            ),

            UpdatedRight = ?INTERNAL2(
                E2,
                E3,
                %
                C2,
                C3,
                C4
            ),

            ?ROTATED(UpElem, UpdatedNode, UpdatedRight);
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            UpElem = E1,
            MovedC = C1,

            UpdatedNode = ?INTERNAL2(
                CElem,
                ParentE,
                %
                CLeft,
                CRight,
                MovedC
            ),

            UpdatedRight = ?INTERNAL3(
                E2,
                E3,
                E4,
                %
                C2,
                C3,
                C4,
                C5
            ),

            ?ROTATED(UpElem, UpdatedNode, UpdatedRight)
        %
        %
    end.

%-compile({inline, rebalance_leaf_from_right_sibling/5}).
rebalance_leaf_from_right_sibling(CElem, ParentE, Right) ->
    case Right of
        ?LEAF2_MATCH_ALL ->
            MergedNode = ?LEAF4(
                CElem,
                ParentE,
                E1,
                E2
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpElem = E1,

            UpdatedNode = ?LEAF2(CElem, ParentE),
            UpdatedRight = ?LEAF2(E2, E3),

            ?ROTATED(UpElem, UpdatedNode, UpdatedRight);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E1,

            UpdatedNode = ?LEAF2(CElem, ParentE),
            UpdatedRight = ?LEAF3(E2, E3, E4),

            ?ROTATED(UpElem, UpdatedNode, UpdatedRight)
        %
        %
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from its left sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_left_sibling/7}).
rebalance_internal_from_left_sibling(
    CElem,
    CLeft,
    CRight,
    %
    ParentE,
    Left
) ->
    case Left of
        ?INTERNAL2_MATCH_ALL ->
            MergedNode = ?INTERNAL4(
                E1,
                E2,
                ParentE,
                CElem,
                %
                C1,
                C2,
                C3,
                CLeft,
                CRight
            ),

            MergedNode;
        %
        %
        %
        ?INTERNAL3_MATCH_ALL ->
            UpElem = E3,
            MovedC = C4,

            UpdatedNode = ?INTERNAL2(
                ParentE,
                CElem,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?INTERNAL2(
                E1,
                E2,
                C1,
                C2,
                C3
            ),

            ?ROTATED(UpElem, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            UpElem = E4,
            MovedC = C5,

            UpdatedNode = ?INTERNAL2(
                ParentE,
                CElem,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?INTERNAL3(
                E1,
                E2,
                E3,
                %
                C1,
                C2,
                C3,
                C4
            ),

            ?ROTATED(UpElem, UpdatedLeft, UpdatedNode)
        %
        %
    end.

%-compile({inline, rebalance_leaf_from_left_sibling/5}).
rebalance_leaf_from_left_sibling(
    CElem,
    ParentE,
    Left
) ->
    case Left of
        ?LEAF2_MATCH_ALL ->
            MergedNode = ?LEAF4(
                E1,
                E2,
                ParentE,
                CElem
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpElem = E3,

            UpdatedNode = ?LEAF2(ParentE, CElem),
            UpdatedLeft = ?LEAF2(E1, E2),

            ?ROTATED(UpElem, UpdatedLeft, UpdatedNode);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E4,

            UpdatedNode = ?LEAF2(ParentE, CElem),
            UpdatedLeft = ?LEAF3(E1, E2, E3),

            ?ROTATED(UpElem, UpdatedLeft, UpdatedNode)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Rebalance a node from either left/right sibling
%% ------------------------------------------------------------------

%-compile({inline, rebalance_internal_from_either_sibling/10}).
rebalance_internal_from_either_sibling(
    CElem,
    CLeft,
    CRight,
    %
    LParentE,
    Left,
    %
    RParentE,
    Right
) ->
    case Left of
        ?INTERNAL2_MATCH(LE1, LE2, LC1, LC2, LC3) ->
            %
            %
            case Right of
                ?INTERNAL3_MATCH_ALL ->
                    UpElem = E1,
                    MovedC = C1,

                    UpdatedNode = ?INTERNAL2(
                        CElem,
                        RParentE,
                        %
                        CLeft,
                        CRight,
                        MovedC
                    ),

                    UpdatedRight = ?INTERNAL2(
                        E2,
                        E3,
                        %
                        C2,
                        C3,
                        C4
                    ),

                    ?MID_ROTATED_FROM_RIGHT(UpElem, UpdatedNode, UpdatedRight);
                %
                %
                ?INTERNAL4_MATCH_ALL ->
                    UpElem = E1,
                    MovedC = C1,

                    UpdatedNode = ?INTERNAL2(
                        CElem,
                        RParentE,
                        %
                        CLeft,
                        CRight,
                        MovedC
                    ),

                    UpdatedRight = ?INTERNAL3(
                        E2,
                        E3,
                        E4,
                        %
                        C2,
                        C3,
                        C4,
                        C5
                    ),

                    ?MID_ROTATED_FROM_RIGHT(UpElem, UpdatedNode, UpdatedRight);
                %
                %
                _ ->
                    % Merge with left since we already unpacked it
                    MergedNode = ?INTERNAL4(
                        LE1,
                        LE2,
                        LParentE,
                        CElem,
                        %
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
        ?INTERNAL3_MATCH_ALL ->
            UpElem = E3,
            MovedC = C4,

            UpdatedNode = ?INTERNAL2(
                LParentE,
                CElem,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?INTERNAL2(
                E1,
                E2,
                %
                C1,
                C2,
                C3
            ),

            ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedLeft, UpdatedNode);
        %
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            UpElem = E4,
            MovedC = C5,

            UpdatedNode = ?INTERNAL2(
                LParentE,
                CElem,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?INTERNAL3(
                E1,
                E2,
                E3,
                %
                C1,
                C2,
                C3,
                C4
            ),

            ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedLeft, UpdatedNode)
    end.

% -compile({inline, rebalance_leaf_from_either_sibling/8}).
rebalance_leaf_from_either_sibling(
    CElem,
    %
    LParentE,
    Left,
    %
    RParentE,
    Right
) ->
    case Left of
        ?LEAF2_MATCH(LE1, LE2) ->
            %
            case Right of
                ?LEAF4_MATCH_ALL ->
                    UpElem = E1,

                    UpdatedNode = ?LEAF2(CElem, RParentE),
                    UpdatedRight = ?LEAF3(E2, E3, E4),

                    ?MID_ROTATED_FROM_RIGHT(UpElem, UpdatedNode, UpdatedRight);
                %
                %
                ?LEAF3_MATCH_ALL ->
                    UpElem = E1,

                    UpdatedNode = ?LEAF2(CElem, RParentE),
                    UpdatedRight = ?LEAF2(E2, E3),

                    ?MID_ROTATED_FROM_RIGHT(UpElem, UpdatedNode, UpdatedRight);
                %
                %
                _ ->
                    % Merge with left since we already unpacked it
                    MergedNode = ?LEAF4(
                        LE1,
                        LE2,
                        LParentE,
                        CElem
                    ),

                    ?MID_MERGED(MergedNode)
            end;
        %
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpElem = E3,

            UpdatedNode = ?LEAF2(LParentE, CElem),
            UpdatedLeft = ?LEAF2(E1, E2),

            ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E4,

            UpdatedNode = ?LEAF2(LParentE, CElem),
            UpdatedLeft = ?LEAF3(E1, E2, E3),

            ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedLeft, UpdatedNode)
    end.
