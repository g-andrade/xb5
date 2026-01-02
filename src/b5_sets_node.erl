% TODO document
-module(b5_sets_node).

-export([
    delete/2,
    difference/2,
    does_root_look_legit/2,
    filter/2,
    filtermap/2,
    fold/3,
    insert/2,
    intersection/2,
    is_disjoint/4,
    is_equal/2,
    is_member/2,
    is_subset/2,
    iterator/2,
    iterator_from/3,
    larger/2,
    largest/1,
    map/2,
    new/0,
    next/1,
    smaller/2,
    smallest/1,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    union/4
]).

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
-define(INTERNAL4_ARITY_PLUS1, 10).
-define(INTERNAL4_ARITY_PLUS2, 11).
-define(INTERNAL4_ARITY_MINUS2, 7).

-define(INTERNAL4_C1(UpdatedC1), ?new_INTERNAL4(E1, E2, E3, E4, UpdatedC1, C2, C3, C4, C5)).
-define(INTERNAL4_C2(UpdatedC2), ?new_INTERNAL4(E1, E2, E3, E4, C1, UpdatedC2, C3, C4, C5)).
-define(INTERNAL4_C3(UpdatedC3), ?new_INTERNAL4(E1, E2, E3, E4, C1, C2, UpdatedC3, C4, C5)).
-define(INTERNAL4_C4(UpdatedC4), ?new_INTERNAL4(E1, E2, E3, E4, C1, C2, C3, UpdatedC4, C5)).
-define(INTERNAL4_C5(UpdatedC5), ?new_INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, UpdatedC5)).

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
-define(INTERNAL3_ARITY_PLUS1, 8).
-define(INTERNAL3_ARITY_PLUS2, 9).
-define(INTERNAL3_ARITY_MINUS2, 5).

-define(INTERNAL3_C1(UpdatedC1), ?new_INTERNAL3(E1, E2, E3, UpdatedC1, C2, C3, C4)).
-define(INTERNAL3_C2(UpdatedC2), ?new_INTERNAL3(E1, E2, E3, C1, UpdatedC2, C3, C4)).
-define(INTERNAL3_C3(UpdatedC3), ?new_INTERNAL3(E1, E2, E3, C1, C2, UpdatedC3, C4)).
-define(INTERNAL3_C4(UpdatedC4), ?new_INTERNAL3(E1, E2, E3, C1, C2, C3, UpdatedC4)).

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
-define(INTERNAL2_ARITY_PLUS1, 6).
-define(INTERNAL2_ARITY_PLUS2, 7).
-define(INTERNAL2_ARITY_MINUS2, 3).

-define(INTERNAL2_C1(UpdatedC1), ?new_INTERNAL2(E1, E2, UpdatedC1, C2, C3)).
-define(INTERNAL2_C2(UpdatedC2), ?new_INTERNAL2(E1, E2, C1, UpdatedC2, C3)).
-define(INTERNAL2_C3(UpdatedC3), ?new_INTERNAL2(E1, E2, C1, C2, UpdatedC3)).

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
-define(INTERNAL1_ARITY_PLUS1, 4).

-define(INTERNAL1_C1(UpdatedC1), ?new_INTERNAL1(E1, UpdatedC1, C2)).
-define(INTERNAL1_C2(UpdatedC2), ?new_INTERNAL1(E1, C1, UpdatedC2)).

-define(INTERNAL1_C1_REBALANCE(UpdatedC1), rebalance_INTERNAL1_C1(E1, UpdatedC1, C2)).
-define(INTERNAL1_C2_REBALANCE(UpdatedC2), rebalance_INTERNAL1_C2(E1, C1, UpdatedC2)).

-define(INTERNAL1_E1_C2_REBALANCE(ReplacementE, UpdatedC2),
    rebalance_INTERNAL1_C2(ReplacementE, C1, UpdatedC2)
).

-define(INTERNAL1_ARGS_IGN_E1, _, C1, C2).

%% ?LEAF4

-define(LEAF4_ARGS, E1, E2, E3, E4).
-define(LEAF4_ARITY, 4).
-define(LEAF4_ARITY_PLUS1, 5).
-define(LEAF4_ARITY_PLUS2, 6).

%% ?LEAF3

-define(LEAF3_ARGS, E1, E2, E3).
-define(LEAF3_ARITY, 3).
-define(LEAF3_ARITY_PLUS1, 4).
-define(LEAF3_ARITY_PLUS2, 5).

%% ?LEAF2

-define(LEAF2_ARGS, E1, E2).
-define(LEAF2_ARITY, 2).
-define(LEAF2_ARITY_PLUS1, 3).
-define(LEAF2_ARITY_PLUS2, 4).

%% ?LEAF1

-define(LEAF1_ARGS, E1).
-define(LEAF1_ARITY, 1).
-define(LEAF1_ARITY_PLUS1, 2).

%%

-define(TAKEN(Taken, UpdatedNode), [Taken | UpdatedNode]).

%%

-define(NODE_CHECK_ENABLED, defined(TEST)).

-if(?NODE_CHECK_ENABLED).
-define(CHECK_NODE(Node), check_node(?LINE, Node)).
-define(CHECK_NODE_RECUR(Node), check_node_recur(?LINE, Node)).
-else.
-define(CHECK_NODE(Node), Node).
-define(CHECK_NODE_RECUR(Node), Node).
-endif.

%

-define(new_INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, C5),
    ?CHECK_NODE_RECUR(?INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, C5))
).

-define(new_INTERNAL3(E1, E2, E3, C1, C2, C3, C4),
    ?CHECK_NODE_RECUR(?INTERNAL3(E1, E2, E3, C1, C2, C3, C4))
).

-define(new_INTERNAL2(E1, E2, C1, C2, C3), ?CHECK_NODE_RECUR(?INTERNAL2(E1, E2, C1, C2, C3))).

-define(new_INTERNAL1(E1, C1, C2), ?CHECK_NODE(?INTERNAL1(E1, C1, C2))).

%

-define(new_LEAF4(E1, E2, E3, E4), ?CHECK_NODE_RECUR(?LEAF4(E1, E2, E3, E4))).

-define(new_LEAF3(E1, E2, E3), ?CHECK_NODE_RECUR(?LEAF3(E1, E2, E3))).

-define(new_LEAF2(E1, E2), ?CHECK_NODE_RECUR(?LEAF2(E1, E2))).

-define(new_LEAF1(E1), ?CHECK_NODE(?LEAF1(E1))).

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
    error_badkey(Elem);
delete(Elem, Root) ->
    delete_recur(Elem, Root).

-spec difference(t(Elem), t(Elem)) -> nonempty_improper_list(non_neg_integer(), t(Elem)).
difference(Root1, Root2) ->
    RemovedCount = 0,

    try smallest(Root1) of
        MinElem ->
            MaxElem = largest(Root1),
            Iter = iterator_from(MinElem, Root2, ordered),
            difference_recur(Iter, MaxElem, RemovedCount, Root1)
    catch
        error:empty_set ->
            [RemovedCount | Root1]
    end.

does_root_look_legit(Root, 0) ->
    Root =:= ?LEAF0;
does_root_look_legit(Root, Size) when is_integer(Size) ->
    case Root of
        ?INTERNAL4(_, _, _, _, _, _, _, _, _) ->
            Size >= 14;
        %
        ?INTERNAL3(_, _, _, _, _, _, _) ->
            Size >= 11;
        %
        ?INTERNAL2(_, _, _, _, _) ->
            Size >= 8;
        %
        ?INTERNAL1(_, _, _) ->
            Size >= 5;
        %
        ?LEAF4(_, _, _, _) ->
            Size =:= 4;
        %
        ?LEAF3(_, _, _) ->
            Size =:= 3;
        %
        ?LEAF2(_, _) ->
            Size =:= 2;
        %
        ?LEAF1(_) ->
            Size =:= 1;
        %
        _ ->
            false
    end;
does_root_look_legit(_, _) ->
    false.

filter(Fun, Root) ->
    Count = 0,
    Filtered = new(),
    filter_root(Fun, Count, Root, Filtered).

filtermap(Fun, Root) ->
    Count = 0,
    Filtered = new(),
    filtermap_root(Fun, Count, Root, Filtered).

fold(Fun, Acc, ?INTERNAL1_MATCH_ALL) ->
    Acc2 = fold_recur(Fun, Acc, C1),
    Acc3 = Fun(E1, Acc2),
    fold_recur(Fun, Acc3, C2);
fold(Fun, Acc, ?LEAF1_MATCH_ALL) ->
    Fun(E1, Acc);
fold(_Fun, Acc, ?LEAF0_MATCH_ALL) ->
    Acc;
fold(Fun, Acc, Root) ->
    fold_recur(Fun, Acc, Root).

insert(Elem, ?INTERNAL1_MATCH_ALL) ->
    insert_INTERNAL1(Elem, ?INTERNAL1_ARGS);
insert(Elem, ?LEAF1_MATCH_ALL) ->
    insert_LEAF1(Elem, ?LEAF1_ARGS);
insert(Elem, ?LEAF0_MATCH_ALL) ->
    ?new_LEAF1(Elem);
insert(Elem, Root) ->
    case insert_recur(Elem, Root) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL1(SplitE, SplitL, SplitR);
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
            IterA = bound_fwd_iterator(MinElem, Root1),
            IterB = bound_fwd_iterator(MinElem, Root2),
            intersection_recur(IterA, IterB, Count, NewRoot)
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

is_equal(Root1, Root2) ->
    Iter1 = fwd_iterator(Root1),
    Iter2 = fwd_iterator(Root2),
    is_equal_recur(Iter1, Iter2).

is_member(Elem, ?INTERNAL1_MATCH_ALL) ->
    is_member_INTERNAL1(Elem, ?INTERNAL1_ARGS);
is_member(Elem, ?LEAF1_MATCH_ALL) ->
    is_member_LEAF1(Elem, ?LEAF1_ARGS);
is_member(_Elem, ?LEAF0_MATCH_ALL) ->
    false;
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
    bound_fwd_iterator(Elem, Root);
iterator_from(Elem, Root, reversed) ->
    Acc = bound_rev_iterator(Elem, Root),
    [?REV_ITER_TAG | Acc].

larger(Elem, ?INTERNAL1_MATCH_ALL) ->
    larger_INTERNAL1(Elem, ?INTERNAL1_ARGS);
larger(Elem, ?LEAF1_MATCH_ALL) ->
    larger_LEAF1(Elem, ?LEAF1_ARGS);
larger(_Elem, ?LEAF0_MATCH_ALL) ->
    none;
larger(Elem, Root) ->
    larger_recur(Elem, Root).

largest(?INTERNAL1_MATCH(_, _, C2)) ->
    largest_recur(C2);
largest(?LEAF1_MATCH(E1)) ->
    E1;
largest(?LEAF0_MATCH) ->
    error_empty_set();
largest(Root) ->
    largest_recur(Root).

map(Fun, Node) ->
    Count = 0,
    Acc = [Count | new()],
    map_root(Fun, Node, Acc).

new() ->
    ?LEAF0.

next([Head | Tail]) ->
    next(Head, Tail);
next([]) ->
    none.

smaller(Elem, ?INTERNAL1_MATCH_ALL) ->
    smaller_INTERNAL1(Elem, ?INTERNAL1_ARGS);
smaller(Elem, ?LEAF1_MATCH_ALL) ->
    smaller_LEAF1(Elem, ?LEAF1_ARGS);
smaller(_Elem, ?LEAF0_MATCH_ALL) ->
    none;
smaller(Elem, Root) ->
    smaller_recur(Elem, Root).

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

to_list(?INTERNAL1_MATCH_ALL) ->
    Acc2 = to_list_recur(C2, []),
    Acc3 = [E1 | Acc2],
    to_list_recur(C1, Acc3);
to_list(?LEAF1_MATCH_ALL) ->
    [E1];
to_list(?LEAF0_MATCH_ALL) ->
    [];
to_list(Root) ->
    to_list_recur(Root, []).

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

-compile({inline, delete_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
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

-compile({inline, delete_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC1 = delete_recur(Elem, C1),

    ?INTERNAL4_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC2 = delete_recur(Elem, C2),

    ?INTERNAL4_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC3 = delete_recur(Elem, C3),

    ?INTERNAL4_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
delete_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC4 = delete_recur(Elem, C4),

    ?INTERNAL4_C4_REBALANCE(UpdatedC4).

-compile({inline, delete_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
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

-compile({inline, delete_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
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

-compile({inline, delete_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC1 = delete_recur(Elem, C1),

    ?INTERNAL3_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC2 = delete_recur(Elem, C2),

    ?INTERNAL3_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
delete_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC3 = delete_recur(Elem, C3),

    ?INTERNAL3_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
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

-compile({inline, delete_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
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

-compile({inline, delete_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS) ->
    UpdatedC1 = delete_recur(Elem, C1),

    ?INTERNAL2_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
delete_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS) ->
    UpdatedC2 = delete_recur(Elem, C2),

    ?INTERNAL2_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
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

-compile({inline, delete_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
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

-compile({inline, delete_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
delete_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    UpdatedC1 = delete_recur(Elem, C1),
    ?INTERNAL1_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
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

-compile({inline, delete_LEAF4 / ?LEAF4_ARITY_PLUS1}).
delete_LEAF4(Elem, ?LEAF4_ARGS) ->
    if
        Elem == E1 ->
            ?new_LEAF3(E2, E3, E4);
        %
        Elem == E2 ->
            ?new_LEAF3(E1, E3, E4);
        %
        Elem == E3 ->
            ?new_LEAF3(E1, E2, E4);
        %
        Elem == E4 ->
            ?new_LEAF3(E1, E2, E3);
        %
        true ->
            error_badkey(Elem)
    end.

%%
%% ?LEAF3
%%

-compile({inline, delete_LEAF3 / ?LEAF3_ARITY_PLUS1}).
delete_LEAF3(Elem, ?LEAF3_ARGS) ->
    if
        Elem == E1 ->
            ?new_LEAF2(E2, E3);
        %
        Elem == E2 ->
            ?new_LEAF2(E1, E3);
        %
        Elem == E3 ->
            ?new_LEAF2(E1, E2);
        %
        true ->
            error_badkey(Elem)
    end.

%%
%% ?LEAF2
%%

-compile({inline, delete_LEAF2 / ?LEAF2_ARITY_PLUS1}).
delete_LEAF2(Elem, ?LEAF2_ARGS) ->
    if
        Elem == E1 ->
            ?new_LEAF1(E2);
        %
        Elem == E2 ->
            ?new_LEAF1(E1);
        %
        true ->
            error_badkey(Elem)
    end.

%%
%% ?LEAF1
%%

-compile({inline, delete_LEAF1 / ?LEAF1_ARITY_PLUS1}).
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

difference_recur([Head | Next], MaxElem, Count, Root) ->
    difference_recur(Head, Next, MaxElem, Count, Root);
difference_recur([], _MaxElem, Count, Root) ->
    [Count | Root].

-compile({inline, difference_recur/5}).
difference_recur(?ITER_ELEM(Elem), Next, MaxElem, Count, Root) ->
    %
    % ITER_ELEM was accumulated when building the iterator. It will almost
    % always come from internal nodes (25% of total), which makes it a good
    % periodic check for MaxElem (without doing it for every element).
    %
    try delete(Elem, Root) of
        UpdatedRoot ->
            difference_recur(Next, MaxElem, Count + 1, UpdatedRoot)
    catch
        error:{badkey, K} when K =:= Elem ->
            %
            case Elem =< MaxElem of
                true ->
                    difference_recur(Next, MaxElem, Count, Root);
                _ ->
                    % No more elements can be removed
                    [Count | Root]
            end
    end;
difference_recur(Node, Next, MaxElem, Count, Root) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            difference_batch2(E1, E2, Next, MaxElem, Count, Root);
        %
        ?LEAF3_MATCH_ALL ->
            difference_batch3(E1, E2, E3, Next, MaxElem, Count, Root);
        %
        ?LEAF4_MATCH_ALL ->
            difference_batch4(E1, E2, E3, E4, Next, MaxElem, Count, Root);
        %
        ?INTERNAL2_MATCH_ALL ->
            % TODO optimize batches like in filter/2?
            [Count2 | Root2] = difference_recur(C1, [], MaxElem, Count, Root),
            [Count3 | Root3] = difference_recur(C2, [], MaxElem, Count2, Root2),
            [Count4 | Root4] = difference_recur(C3, [], MaxElem, Count3, Root3),

            difference_batch2(E1, E2, Next, MaxElem, Count4, Root4);
        %
        ?INTERNAL3_MATCH_ALL ->
            % TODO optimize batches like in filter/2?
            [Count2 | Root2] = difference_recur(C1, [], MaxElem, Count, Root),
            [Count3 | Root3] = difference_recur(C2, [], MaxElem, Count2, Root2),
            [Count4 | Root4] = difference_recur(C3, [], MaxElem, Count3, Root3),
            [Count5 | Root5] = difference_recur(C4, [], MaxElem, Count4, Root4),

            difference_batch3(E1, E2, E3, Next, MaxElem, Count5, Root5);
        %
        ?INTERNAL4_MATCH_ALL ->
            % TODO optimize batches like in filter/2?
            [Count2 | Root2] = difference_recur(C1, [], MaxElem, Count, Root),
            [Count3 | Root3] = difference_recur(C2, [], MaxElem, Count2, Root2),
            [Count4 | Root4] = difference_recur(C3, [], MaxElem, Count3, Root3),
            [Count5 | Root5] = difference_recur(C4, [], MaxElem, Count4, Root4),
            [Count6 | Root6] = difference_recur(C5, [], MaxElem, Count5, Root5),

            difference_batch4(E1, E2, E3, E4, Next, MaxElem, Count6, Root6)
    end.

%% INTERNAL4 and LEAF4

-compile({inline, difference_batch4/8}).
difference_batch4(E1, E2, E3, E4, Next, MaxElem, Count, Root) ->
    % INTERNAL4 + LEAF4 nodes make up about 20% of internal nodes. When we run
    % into either, it means a node not previously encountered when building the
    % `iterator_from'.
    %
    % Therefore, they're a good period check for MaxElem.

    try delete(E1, Root) of
        UpdatedRoot ->
            difference_batch3(E2, E3, E4, Next, MaxElem, Count + 1, UpdatedRoot)
    catch
        error:{badkey, K} when K =:= E1 ->
            case E1 =< MaxElem of
                true ->
                    difference_batch3(E2, E3, E4, Next, MaxElem, Count, Root);
                _ ->
                    % No point in continuing, no more elements can be removed
                    [Count | Root]
            end
    end.

%% INTERNAL3 and LEAF3

-compile({inline, difference_batch3/7}).
difference_batch3(E1, E2, E3, Next, MaxElem, Count, Root) ->
    try delete(E1, Root) of
        UpdatedRoot ->
            difference_batch2(E2, E3, Next, MaxElem, Count + 1, UpdatedRoot)
    catch
        error:{badkey, K} when K =:= E1 ->
            difference_batch2(E2, E3, Next, MaxElem, Count, Root)
    end.

%% INTERNAL2 and LEAF2

-compile({inline, difference_batch2/6}).
difference_batch2(E1, E2, Next, MaxElem, Count, Root) ->
    try delete(E1, Root) of
        UpdatedRoot ->
            difference_batch2_E2(E2, Next, MaxElem, Count + 1, UpdatedRoot)
    catch
        error:{badkey, K} when K =:= E1 ->
            difference_batch2_E2(E2, Next, MaxElem, Count, Root)
    end.

-compile({inline, difference_batch2_E2/5}).
difference_batch2_E2(E2, Next, MaxElem, Count, Root) ->
    try delete(E2, Root) of
        UpdatedRoot ->
            difference_recur(Next, MaxElem, Count + 1, UpdatedRoot)
    catch
        error:{badkey, K} when K =:= E2 ->
            difference_recur(Next, MaxElem, Count, Root)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: filter/2
%% ------------------------------------------------------------------

filter_root(Fun, Count, Root, Filtered) ->
    case Root of
        ?INTERNAL1_MATCH_ALL ->
            filter_internal_batch1(Fun, E1, C1, C2, Count, Filtered);
        %
        ?LEAF1_MATCH_ALL ->
            filter_single_element(Fun, E1, Count, Filtered);
        %
        ?LEAF0 ->
            [Count | Filtered];
        %
        _ ->
            filter_recur(Fun, Root, Count, Filtered)
    end.

filter_recur(Fun, Node, Count, Filtered) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            filter_leaf_batch2(Fun, E1, E2, Count, Filtered);
        %
        ?LEAF3_MATCH_ALL ->
            filter_leaf_batch3(Fun, E1, E2, E3, Count, Filtered);
        %
        ?LEAF4_MATCH_ALL ->
            filter_leaf_batch4(Fun, E1, E2, E3, E4, Count, Filtered);
        %
        ?INTERNAL2_MATCH_ALL ->
            filter_internal_batch2(Fun, E1, E2, C1, C2, C3, Count, Filtered);
        %
        ?INTERNAL3_MATCH_ALL ->
            filter_internal_batch3(Fun, E1, E2, E3, C1, C2, C3, C4, Count, Filtered);
        %
        ?INTERNAL4_MATCH_ALL ->
            filter_internal_batch4(Fun, E1, E2, E3, E4, C1, C2, C3, C4, C5, Count, Filtered)
    end.

%% INTERNAL4

-compile({inline, filter_internal_batch4/12}).
filter_internal_batch4(Fun, E1, E2, E3, E4, C1, C2, C3, C4, C5, Count, Filtered) ->
    [Count2 | Filtered2] = filter_recur(Fun, C1, Count, Filtered),

    case Fun(E1) of
        true ->
            filter_internal_batch3(
                Fun, E2, E3, E4, C2, C3, C4, C5, Count2 + 1, insert(E1, Filtered2)
            );
        %
        false ->
            filter_internal_batch3(Fun, E2, E3, E4, C2, C3, C4, C5, Count2, Filtered2)
    end.

%% INTERNAL3

-compile({inline, filter_internal_batch3/10}).
filter_internal_batch3(Fun, E1, E2, E3, C1, C2, C3, C4, Count, Filtered) ->
    [Count2 | Filtered2] = filter_recur(Fun, C1, Count, Filtered),

    case Fun(E1) of
        true ->
            filter_internal_batch2(Fun, E2, E3, C2, C3, C4, Count2 + 1, insert(E1, Filtered2));
        %
        false ->
            filter_internal_batch2(Fun, E2, E3, C2, C3, C4, Count2, Filtered2)
    end.

%% INTERNAL2

-compile({inline, filter_internal_batch2/8}).
filter_internal_batch2(Fun, E1, E2, C1, C2, C3, Count, Filtered) ->
    [Count2 | Filtered2] = filter_recur(Fun, C1, Count, Filtered),

    case Fun(E1) of
        true ->
            filter_internal_batch1(Fun, E2, C2, C3, Count2 + 1, insert(E1, Filtered2));
        %
        false ->
            filter_internal_batch1(Fun, E2, C2, C3, Count2, Filtered2)
    end.

%% INTERNAL1

-compile({inline, filter_internal_batch1/6}).
filter_internal_batch1(Fun, E1, C1, C2, Count, Filtered) ->
    [Count2 | Filtered2] = filter_recur(Fun, C1, Count, Filtered),

    case Fun(E1) of
        true ->
            filter_recur(Fun, C2, Count2 + 1, insert(E1, Filtered2));
        %
        false ->
            filter_recur(Fun, C2, Count2, Filtered2)
    end.

%% LEAF4

-compile({inline, filter_leaf_batch4/7}).
filter_leaf_batch4(Fun, E1, E2, E3, E4, Count, Filtered) ->
    case Fun(E1) of
        true ->
            filter_leaf_batch3(Fun, E2, E3, E4, Count + 1, insert(E1, Filtered));
        %
        false ->
            filter_leaf_batch3(Fun, E2, E3, E4, Count, Filtered)
    end.

%% LEAF3

-compile({inline, filter_leaf_batch3/6}).
filter_leaf_batch3(Fun, E1, E2, E3, Count, Filtered) ->
    case Fun(E1) of
        true ->
            filter_leaf_batch2(Fun, E2, E3, Count + 1, insert(E1, Filtered));
        %
        false ->
            filter_leaf_batch2(Fun, E2, E3, Count, Filtered)
    end.

%% LEAF2

-compile({inline, filter_leaf_batch2/5}).
filter_leaf_batch2(Fun, E1, E2, Count, Filtered) ->
    case Fun(E1) of
        true ->
            filter_single_element(Fun, E2, Count + 1, insert(E1, Filtered));
        %
        false ->
            filter_single_element(Fun, E2, Count, Filtered)
    end.

%% LEAF1

-compile({inline, filter_single_element/4}).
filter_single_element(Fun, E1, Count, Filtered) ->
    case Fun(E1) of
        true ->
            [Count + 1 | insert(E1, Filtered)];
        %
        false ->
            [Count | Filtered]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: filtermap/2
%% ------------------------------------------------------------------

filtermap_root(Fun, Count, Root, Filtered) ->
    case Root of
        ?INTERNAL1_MATCH_ALL ->
            [Count2 | Filtered2] = filtermap_recur(Fun, C1, Count, Filtered),
            [Count3 | Filtered3] = filtermap_single_element(Fun, E1, Count2, Filtered2),
            filtermap_recur(Fun, C2, Count3, Filtered3);
        %
        ?LEAF1_MATCH_ALL ->
            filtermap_single_element(Fun, E1, Count, Filtered);
        %
        ?LEAF0 ->
            [Count | Filtered];
        %
        _ ->
            filtermap_recur(Fun, Root, Count, Filtered)
    end.

filtermap_recur(Fun, Node, Count, Filtered) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            filtermap_batch2(Fun, E1, E2, Count, Filtered);
        %
        ?LEAF3_MATCH_ALL ->
            filtermap_batch3(Fun, E1, E2, E3, Count, Filtered);
        %
        ?LEAF4_MATCH_ALL ->
            filtermap_batch4(Fun, E1, E2, E3, E4, Count, Filtered);
        %
        ?INTERNAL2_MATCH_ALL ->
            % TODO optimize batches like in filter/2
            [Count2 | Filtered2] = filtermap_recur(Fun, C1, Count, Filtered),
            [Count3 | Filtered3] = filtermap_recur(Fun, C2, Count2, Filtered2),
            [Count4 | Filtered4] = filtermap_recur(Fun, C3, Count3, Filtered3),

            filtermap_batch2(Fun, E1, E2, Count4, Filtered4);
        %
        ?INTERNAL3_MATCH_ALL ->
            % TODO optimize batches like in filter/2
            [Count2 | Filtered2] = filtermap_recur(Fun, C1, Count, Filtered),
            [Count3 | Filtered3] = filtermap_recur(Fun, C2, Count2, Filtered2),
            [Count4 | Filtered4] = filtermap_recur(Fun, C3, Count3, Filtered3),
            [Count5 | Filtered5] = filtermap_recur(Fun, C4, Count4, Filtered4),

            filtermap_batch3(Fun, E1, E2, E3, Count5, Filtered5);
        %
        ?INTERNAL4_MATCH_ALL ->
            % TODO optimize batches like in filter/2
            [Count2 | Filtered2] = filtermap_recur(Fun, C1, Count, Filtered),
            [Count3 | Filtered3] = filtermap_recur(Fun, C2, Count2, Filtered2),
            [Count4 | Filtered4] = filtermap_recur(Fun, C3, Count3, Filtered3),
            [Count5 | Filtered5] = filtermap_recur(Fun, C4, Count4, Filtered4),
            [Count6 | Filtered6] = filtermap_recur(Fun, C5, Count5, Filtered5),

            filtermap_batch4(Fun, E1, E2, E3, E4, Count6, Filtered6)
    end.

%% INTERNAL4 and LEAF4

-compile({inline, filtermap_batch4/7}).
filtermap_batch4(Fun, E1, E2, E3, E4, Count, Filtered) ->
    case Fun(E1) of
        {true, MappedE1} ->
            try insert(MappedE1, Filtered) of
                UpFiltered ->
                    filtermap_batch3(Fun, E2, E3, E4, Count + 1, UpFiltered)
            catch
                error:{key_exists, K} when K =:= MappedE1 ->
                    filtermap_batch3(Fun, E2, E3, E4, Count, Filtered)
            end;
        %
        true ->
            try insert(E1, Filtered) of
                UpFiltered ->
                    filtermap_batch3(Fun, E2, E3, E4, Count + 1, UpFiltered)
            catch
                error:{key_exists, K} when K =:= E1 ->
                    filtermap_batch3(Fun, E2, E3, E4, Count, Filtered)
            end;
        %
        false ->
            filtermap_batch3(Fun, E2, E3, E4, Count, Filtered)
    end.

%% INTERNAL3 and LEAF3

-compile({inline, filtermap_batch3/6}).
filtermap_batch3(Fun, E1, E2, E3, Count, Filtered) ->
    case Fun(E1) of
        {true, MappedE1} ->
            try insert(MappedE1, Filtered) of
                UpFiltered ->
                    filtermap_batch2(Fun, E2, E3, Count + 1, UpFiltered)
            catch
                error:{key_exists, K} when K =:= MappedE1 ->
                    filtermap_batch2(Fun, E2, E3, Count, Filtered)
            end;
        %
        true ->
            try insert(E1, Filtered) of
                UpFiltered ->
                    filtermap_batch2(Fun, E2, E3, Count + 1, UpFiltered)
            catch
                error:{key_exists, K} when K =:= E1 ->
                    filtermap_batch2(Fun, E2, E3, Count, Filtered)
            end;
        %
        false ->
            filtermap_batch2(Fun, E2, E3, Count, Filtered)
    end.

%% INTERNAL2 and LEAF2

-compile({inline, filtermap_batch2/5}).
filtermap_batch2(Fun, E1, E2, Count, Filtered) ->
    case Fun(E1) of
        {true, MappedE1} ->
            try insert(MappedE1, Filtered) of
                UpFiltered ->
                    filtermap_single_element(Fun, E2, Count + 1, UpFiltered)
            catch
                error:{key_exists, K} when K =:= MappedE1 ->
                    filtermap_single_element(Fun, E2, Count, Filtered)
            end;
        %
        true ->
            try insert(E1, Filtered) of
                UpFiltered ->
                    filtermap_single_element(Fun, E2, Count + 1, UpFiltered)
            catch
                error:{key_exists, K} when K =:= E1 ->
                    filtermap_single_element(Fun, E2, Count, Filtered)
            end;
        %
        false ->
            filtermap_single_element(Fun, E2, Count, Filtered)
    end.

%% INTERNAL1 and LEAF1

-compile({inline, filtermap_single_element/4}).
filtermap_single_element(Fun, E1, Count, Filtered) ->
    case Fun(E1) of
        {true, MappedE1} ->
            try insert(MappedE1, Filtered) of
                UpFiltered ->
                    [Count + 1 | UpFiltered]
            catch
                error:{key_exists, K} when K =:= MappedE1 ->
                    [Count | Filtered]
            end;
        %
        true ->
            try insert(E1, Filtered) of
                UpFiltered ->
                    [Count + 1 | UpFiltered]
            catch
                error:{key_exists, K} when K =:= E1 ->
                    [Count | Filtered]
            end;
        %
        false ->
            [Count | Filtered]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: fold/3
%% ------------------------------------------------------------------

fold_recur(Fun, Acc, Node) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            Fun(E2, Fun(E1, Acc));
        %
        ?LEAF3_MATCH_ALL ->
            Fun(E3, Fun(E2, Fun(E1, Acc)));
        %
        ?LEAF4_MATCH_ALL ->
            Fun(E4, Fun(E3, Fun(E2, Fun(E1, Acc))));
        %
        ?INTERNAL2_MATCH_ALL ->
            Acc2 = fold_recur(Fun, Acc, C1),
            Acc3 = fold_recur(Fun, Fun(E1, Acc2), C2),
            _Acc4 = fold_recur(Fun, Fun(E2, Acc3), C3);
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = fold_recur(Fun, Acc, C1),
            Acc3 = fold_recur(Fun, Fun(E1, Acc2), C2),
            Acc4 = fold_recur(Fun, Fun(E2, Acc3), C3),
            _Acc5 = fold_recur(Fun, Fun(E3, Acc4), C4);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = fold_recur(Fun, Acc, C1),
            Acc3 = fold_recur(Fun, Fun(E1, Acc2), C2),
            Acc4 = fold_recur(Fun, Fun(E2, Acc3), C3),
            Acc5 = fold_recur(Fun, Fun(E3, Acc4), C4),
            _Acc6 = fold_recur(Fun, Fun(E4, Acc5), C5)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: insert/2
%% ------------------------------------------------------------------

insert_recur(Elem, Node) ->
    case Node of
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

-compile({inline, insert_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
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

-compile({inline, insert_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
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

-compile({inline, insert_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
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

-compile({inline, insert_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
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

-compile({inline, insert_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
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

-compile({inline, insert_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
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

-compile({inline, insert_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
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

-compile({inline, insert_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
insert_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL4(
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

-compile({inline, insert_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
insert_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL4(
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

-compile({inline, insert_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
insert_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C3) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL4(
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

-compile({inline, insert_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
insert_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS) ->
    case insert_recur(Elem, C4) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL4(
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

-compile({inline, insert_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
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

-compile({inline, insert_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
insert_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL3(
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

-compile({inline, insert_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
insert_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL3(
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

-compile({inline, insert_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
insert_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS) ->
    case insert_recur(Elem, C3) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL3(
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

-compile({inline, insert_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
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

-compile({inline, insert_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
insert_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    case insert_recur(Elem, C1) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL2(
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

-compile({inline, insert_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
insert_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS) ->
    case insert_recur(Elem, C2) of
        ?SPLIT_MATCH(SplitE, SplitL, SplitR) ->
            ?new_INTERNAL2(
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

-compile({inline, insert_LEAF4 / ?LEAF4_ARITY_PLUS1}).
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

-compile({inline, insert_LEAF4_POS1 / ?LEAF4_ARITY_PLUS1}).
insert_LEAF4_POS1(Elem, ?LEAF4_ARGS) ->
    split_leaf(Elem, E1, E2, E3, E4).

-compile({inline, insert_LEAF4_POS2 / ?LEAF4_ARITY_PLUS1}).
insert_LEAF4_POS2(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, Elem, E2, E3, E4).

-compile({inline, insert_LEAF4_POS3 / ?LEAF4_ARITY_PLUS1}).
insert_LEAF4_POS3(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, Elem, E3, E4).

-compile({inline, insert_LEAF4_POS4 / ?LEAF4_ARITY_PLUS1}).
insert_LEAF4_POS4(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, E3, Elem, E4).

-compile({inline, insert_LEAF4_POS5 / ?LEAF4_ARITY_PLUS1}).
insert_LEAF4_POS5(Elem, ?LEAF4_ARGS) ->
    split_leaf(E1, E2, E3, E4, Elem).

%%
%% ?LEAF3
%%

-compile({inline, insert_LEAF3 / ?LEAF3_ARITY_PLUS1}).
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

-compile({inline, insert_LEAF3_POS1 / ?LEAF3_ARITY_PLUS1}).
insert_LEAF3_POS1(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(Elem, E1, E2, E3).

-compile({inline, insert_LEAF3_POS2 / ?LEAF3_ARITY_PLUS1}).
insert_LEAF3_POS2(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(E1, Elem, E2, E3).

-compile({inline, insert_LEAF3_POS3 / ?LEAF3_ARITY_PLUS1}).
insert_LEAF3_POS3(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(E1, E2, Elem, E3).

-compile({inline, insert_LEAF3_POS4 / ?LEAF3_ARITY_PLUS1}).
insert_LEAF3_POS4(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(E1, E2, E3, Elem).

%%
%% ?LEAF2
%%

-compile({inline, insert_LEAF2 / ?LEAF2_ARITY_PLUS1}).
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

-compile({inline, insert_LEAF2_POS1 / ?LEAF2_ARITY_PLUS1}).
insert_LEAF2_POS1(Elem, ?LEAF2_ARGS) ->
    ?new_LEAF3(Elem, E1, E2).

-compile({inline, insert_LEAF2_POS2 / ?LEAF2_ARITY_PLUS1}).
insert_LEAF2_POS2(Elem, ?LEAF2_ARGS) ->
    ?new_LEAF3(E1, Elem, E2).

-compile({inline, insert_LEAF2_POS3 / ?LEAF2_ARITY_PLUS1}).
insert_LEAF2_POS3(Elem, ?LEAF2_ARGS) ->
    ?new_LEAF3(E1, E2, Elem).

%%
%% ?LEAF1
%%

-compile({inline, insert_LEAF1 / ?LEAF1_ARITY_PLUS1}).
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

-compile({inline, insert_LEAF1_POS1 / ?LEAF1_ARITY_PLUS1}).
insert_LEAF1_POS1(Elem, ?LEAF1_ARGS) ->
    ?new_LEAF2(Elem, E1).

-compile({inline, insert_LEAF1_POS2 / ?LEAF1_ARITY_PLUS1}).
insert_LEAF1_POS2(Elem, ?LEAF1_ARGS) ->
    ?new_LEAF2(E1, Elem).

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

    SplitL = ?new_INTERNAL2(E1, E2, C1, C2, C3),
    SplitR = ?new_INTERNAL2(E4, E5, C4, C5, C6),

    ?SPLIT(SplitE, SplitL, SplitR).

-compile({inline, split_leaf/5}).
split_leaf(
    E1, E2, E3, E4, E5
) ->
    SplitE = E3,

    SplitL = ?new_LEAF2(E1, E2),
    SplitR = ?new_LEAF2(E4, E5),

    ?SPLIT(SplitE, SplitL, SplitR).

%% ------------------------------------------------------------------
%% Internal Function Definitions: intersection/1
%% ------------------------------------------------------------------

intersection_recur([HeadA | TailA], IterB, Count, Root) ->
    intersection_iterA(HeadA, TailA, IterB, Count, Root);
intersection_recur([], _IterB, Count, Root) ->
    [Count | Root].

-compile({intersection_iterA/5}).
intersection_iterA(HeadA, TailA, IterB, Count, Root) ->
    case HeadA of
        ?ITER_ELEM(ElemA) ->
            NextA = TailA,
            intersection_iterB(ElemA, NextA, IterB, Count, Root);
        %
        NodeA ->
            [?ITER_ELEM(ElemA) | NextA] = fwd_iterator_recur(NodeA, TailA),
            intersection_iterB(ElemA, NextA, IterB, Count, Root)
    end.

intersection_iterB(ElemA, NextA, [HeadB | TailB], Count, Root) ->
    case HeadB of
        ?ITER_ELEM(ElemB) ->
            NextB = TailB,
            intersection_intersect(ElemA, NextA, ElemB, NextB, Count, Root);
        %
        NodeB ->
            [?ITER_ELEM(ElemB) | NextB] = fwd_iterator_recur(NodeB, TailB),
            intersection_intersect(ElemA, NextA, ElemB, NextB, Count, Root)
    end;
intersection_iterB(_ElemA, _NextA, [], Count, Root) ->
    [Count | Root].

-compile({intersection_intersect/6}).
intersection_intersect(ElemA, NextA, ElemB, NextB, Count, Root) ->
    if
        ElemA < ElemB ->
            intersection_iterB(ElemB, NextB, NextA, Count, Root);
        %
        ElemA > ElemB ->
            intersection_iterB(ElemA, NextA, NextB, Count, Root);
        %
        true ->
            UpdatedRoot = insert(ElemA, Root),
            UpdatedCount = Count + 1,
            intersection_recur(NextA, NextB, UpdatedCount, UpdatedRoot)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_disjoint/2
%% ------------------------------------------------------------------

is_disjoint_root(Root1, Root2) ->
    try smallest(Root2) of
        MinElem ->
            MaxElem = largest(Root2),
            Iter = bound_fwd_iterator(MinElem, Root1),
            is_disjoint_recur(Iter, Root2, MaxElem)
    catch
        error:empty_set ->
            true
    end.

is_disjoint_recur([Head | Tail], Root2, MaxElem) ->
    is_disjoint_iter(Head, Tail, Root2, MaxElem);
is_disjoint_recur([], _Root2, _MaxElem) ->
    true.

-compile({inline, is_disjoint_iter/4}).
is_disjoint_iter(Head, Tail, Root2, MaxElem) ->
    case Head of
        ?ITER_ELEM(Elem) ->
            Next = Tail,
            is_disjoint_check(Elem, Next, Root2, MaxElem);
        %
        Node ->
            [?ITER_ELEM(Elem) | Next] = fwd_iterator_recur(Node, Tail),
            is_disjoint_check(Elem, Next, Root2, MaxElem)
    end.

is_disjoint_check(Elem, Next, Root2, MaxElem) ->
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
%% Internal Function Definitions: is_equal/2
%% ------------------------------------------------------------------

is_equal_recur([HeadA | TailA], IterB) ->
    is_equal_iterA(HeadA, TailA, IterB);
is_equal_recur([], []) ->
    true.

-compile({is_equal_iterA/3}).
is_equal_iterA(HeadA, TailA, IterB) ->
    case HeadA of
        ?ITER_ELEM(ElemA) ->
            NextA = TailA,
            is_equal_iterB(ElemA, NextA, IterB);
        %
        NodeA ->
            [?ITER_ELEM(ElemA) | NextA] = fwd_iterator_recur(NodeA, TailA),
            is_equal_iterB(ElemA, NextA, IterB)
    end.

is_equal_iterB(ElemA, NextA, [HeadB | TailB]) ->
    case HeadB of
        ?ITER_ELEM(ElemB) ->
            NextB = TailB,
            is_equal_check(ElemA, NextA, ElemB, NextB);
        %
        NodeB ->
            [?ITER_ELEM(ElemB) | NextB] = fwd_iterator_recur(NodeB, TailB),
            is_equal_check(ElemA, NextA, ElemB, NextB)
    end.

-compile({is_equal_check/4}).
is_equal_check(ElemA, NextA, ElemB, NextB) ->
    (ElemA == ElemB) andalso is_equal_recur(NextA, NextB).

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

-compile({inline, is_member_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
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

-compile({inline, is_member_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
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

-compile({inline, is_member_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
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

-compile({inline, is_member_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
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

-compile({inline, is_member_LEAF4 / ?LEAF4_ARITY_PLUS1}).
is_member_LEAF4(Elem, ?LEAF4_ARGS) ->
    (Elem == E1 orelse
        Elem == E2 orelse
        Elem == E3 orelse
        Elem == E4).

%%
%% ?LEAF3
%%

-compile({inline, is_member_LEAF3 / ?LEAF3_ARITY_PLUS1}).
is_member_LEAF3(Elem, ?LEAF3_ARGS) ->
    (Elem == E1 orelse
        Elem == E2 orelse
        Elem == E3).

%%
%% ?LEAF2
%%

-compile({inline, is_member_LEAF2 / ?LEAF2_ARITY_PLUS1}).
is_member_LEAF2(Elem, ?LEAF2_ARGS) ->
    Elem == E1 orelse Elem == E2.

%%
%% ?LEAF1
%%

-compile({inline, is_member_LEAF1 / ?LEAF1_ARITY_PLUS1}).
is_member_LEAF1(Elem, ?LEAF1_ARGS) ->
    Elem == E1.

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_subset/2
%% ------------------------------------------------------------------

is_subset_recur([Head | Tail], Root2) ->
    is_subset_iter(Head, Tail, Root2);
is_subset_recur([], _Root2) ->
    true.

-compile({is_subset_iter/3}).
is_subset_iter(Head, Tail, Root2) ->
    case Head of
        ?ITER_ELEM(Elem) ->
            Next = Tail,
            is_subset_check(Elem, Next, Root2);
        %
        Node ->
            [?ITER_ELEM(Elem) | Next] = fwd_iterator_recur(Node, Tail),
            is_subset_check(Elem, Next, Root2)
    end.

-compile({is_subset_check/3}).
is_subset_check(Elem, Next, Root2) ->
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

-compile({inline, bound_fwd_iterator_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc) ->
    if
        Elem > E4 ->
            case Acc of
                [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Elem, C5, Acc)
            end;
        %
        Elem > E3 ->
            Acc2 = [?ITER_ELEM(E4), C5 | Acc],
            bound_fwd_iterator_recur(Elem, C4, Acc2);
        %
        Elem > E2 ->
            Acc2 = [?ITER_ELEM(E3), C4, ?ITER_ELEM(E4), C5 | Acc],
            bound_fwd_iterator_recur(Elem, C3, Acc2);
        %
        Elem > E1 ->
            Acc2 = [?ITER_ELEM(E2), C3, ?ITER_ELEM(E3), C4, ?ITER_ELEM(E4), C5 | Acc],
            bound_fwd_iterator_recur(Elem, C2, Acc2);
        %
        true ->
            Acc2 = [
                ?ITER_ELEM(E1), C2, ?ITER_ELEM(E2), C3, ?ITER_ELEM(E3), C4, ?ITER_ELEM(E4), C5 | Acc
            ],
            bound_fwd_iterator_recur(Elem, C1, Acc2)
    end.

%% INTERNAL3

-compile({inline, bound_fwd_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc) ->
    if
        Elem > E3 ->
            case Acc of
                [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Elem, C4, Acc)
            end;
        %
        Elem > E2 ->
            Acc2 = [?ITER_ELEM(E3), C4 | Acc],
            bound_fwd_iterator_recur(Elem, C3, Acc2);
        %
        Elem > E1 ->
            Acc2 = [?ITER_ELEM(E2), C3, ?ITER_ELEM(E3), C4 | Acc],
            bound_fwd_iterator_recur(Elem, C2, Acc2);
        %
        true ->
            Acc2 = [?ITER_ELEM(E1), C2, ?ITER_ELEM(E2), C3, ?ITER_ELEM(E3), C4 | Acc],
            bound_fwd_iterator_recur(Elem, C1, Acc2)
    end.

%% INTERNAL2

-compile({inline, bound_fwd_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc) ->
    if
        Elem > E2 ->
            case Acc of
                [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_fwd_iterator_recur(Elem, C3, Acc)
            end;
        %
        Elem > E1 ->
            Acc2 = [?ITER_ELEM(E2), C3 | Acc],
            bound_fwd_iterator_recur(Elem, C2, Acc2);
        %
        true ->
            Acc2 = [?ITER_ELEM(E1), C2, ?ITER_ELEM(E2), C3 | Acc],
            bound_fwd_iterator_recur(Elem, C1, Acc2)
    end.

%% INTERNAL1

-compile({inline, bound_fwd_iterator_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
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
            _Acc = [?ITER_ELEM(E1), C2]
    end.

%% LEAF4

-compile({inline, bound_fwd_iterator_LEAF4 / ?LEAF4_ARITY_PLUS2}).
bound_fwd_iterator_LEAF4(Elem, ?LEAF4_ARGS, Acc) ->
    if
        Elem > E4 ->
            Acc;
        %
        Elem > E3 ->
            [?ITER_ELEM(E4) | Acc];
        %
        Elem > E2 ->
            [?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc];
        %
        Elem > E1 ->
            [?ITER_ELEM(E2), ?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc];
        %
        true ->
            [?ITER_ELEM(E1), ?ITER_ELEM(E2), ?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc]
    end.

%% LEAF3

-compile({inline, bound_fwd_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_fwd_iterator_LEAF3(Elem, ?LEAF3_ARGS, Acc) ->
    if
        Elem > E3 ->
            Acc;
        %
        Elem > E2 ->
            [?ITER_ELEM(E3) | Acc];
        %
        Elem > E1 ->
            [?ITER_ELEM(E2), ?ITER_ELEM(E3) | Acc];
        %
        true ->
            [?ITER_ELEM(E1), ?ITER_ELEM(E2), ?ITER_ELEM(E3) | Acc]
    end.

%% LEAF2

-compile({inline, bound_fwd_iterator_LEAF2 / ?LEAF2_ARITY_PLUS2}).
bound_fwd_iterator_LEAF2(Elem, ?LEAF2_ARGS, Acc) ->
    if
        Elem > E2 ->
            Acc;
        %
        Elem > E1 ->
            [?ITER_ELEM(E2) | Acc];
        %
        true ->
            [?ITER_ELEM(E1), ?ITER_ELEM(E2) | Acc]
    end.

%% LEAF1

-compile({inline, bound_fwd_iterator_LEAF1 / ?LEAF1_ARITY_PLUS1}).
bound_fwd_iterator_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem > E1 ->
            [];
        %
        true ->
            [?ITER_ELEM(E1)]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator_from/3 - forward
%% ------------------------------------------------------------------

bound_rev_iterator(Elem, Root) ->
    case Root of
        ?INTERNAL1_MATCH_ALL ->
            bound_rev_iterator_INTERNAL1(Elem, ?INTERNAL1_ARGS);
        %
        ?LEAF1_MATCH_ALL ->
            bound_rev_iterator_LEAF1(Elem, ?LEAF1_ARGS);
        %
        ?LEAF0_MATCH ->
            Iter = [],
            Iter;
        %
        _ ->
            Acc = [],
            bound_rev_iterator_recur(Elem, Root, Acc)
    end.

bound_rev_iterator_recur(Elem, Node, Acc) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            bound_rev_iterator_LEAF2(Elem, ?LEAF2_ARGS, Acc);
        %
        ?LEAF3_MATCH_ALL ->
            bound_rev_iterator_LEAF3(Elem, ?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            bound_rev_iterator_LEAF4(Elem, ?LEAF4_ARGS, Acc);
        %
        ?INTERNAL2_MATCH_ALL ->
            bound_rev_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            bound_rev_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            bound_rev_iterator_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc)
    end.

%% INTERNAL4

-compile({inline, bound_rev_iterator_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc) ->
    if
        Elem < E1 ->
            case Acc of
                [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Elem, C1, Acc)
            end;
        %
        Elem < E2 ->
            Acc2 = [?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C2, Acc2);
        %
        Elem < E3 ->
            Acc2 = [?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C3, Acc2);
        %
        Elem < E4 ->
            Acc2 = [?ITER_ELEM(E3), C3, ?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C4, Acc2);
        %
        true ->
            Acc2 = [
                ?ITER_ELEM(E4), C4, ?ITER_ELEM(E3), C3, ?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc
            ],
            bound_rev_iterator_recur(Elem, C5, Acc2)
    end.

%% INTERNAL3

-compile({inline, bound_rev_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc) ->
    if
        Elem < E1 ->
            case Acc of
                [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Elem, C1, Acc)
            end;
        %
        Elem < E2 ->
            Acc2 = [?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C2, Acc2);
        %
        Elem < E3 ->
            Acc2 = [?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C3, Acc2);
        %
        true ->
            Acc2 = [?ITER_ELEM(E3), C3, ?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C4, Acc2)
    end.

%% INTERNAL2

-compile({inline, bound_rev_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc) ->
    if
        Elem < E1 ->
            case Acc of
                [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                    % We overshot when recursing from this node's parent, stop here
                    % since no more elements can possibly be included.
                    Acc;
                _ ->
                    bound_rev_iterator_recur(Elem, C1, Acc)
            end;
        %
        Elem < E2 ->
            Acc2 = [?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C2, Acc2);
        %
        true ->
            Acc2 = [?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C3, Acc2)
    end.

%% INTERNAL1

-compile({inline, bound_rev_iterator_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
bound_rev_iterator_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem > E1 ->
            Acc = [?ITER_ELEM(E1), C1],
            bound_rev_iterator_recur(Elem, C2, Acc);
        %
        Elem < E1 ->
            Acc = [],
            bound_rev_iterator_recur(Elem, C1, Acc);
        %
        true ->
            _Acc = [?ITER_ELEM(E1), C1]
    end.

%% LEAF4

-compile({inline, bound_rev_iterator_LEAF4 / ?LEAF4_ARITY_PLUS2}).
bound_rev_iterator_LEAF4(Elem, ?LEAF4_ARGS, Acc) ->
    if
        Elem < E1 ->
            Acc;
        %
        Elem < E2 ->
            [?ITER_ELEM(E1) | Acc];
        %
        Elem < E3 ->
            [?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc];
        %
        Elem < E4 ->
            [?ITER_ELEM(E3), ?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc];
        %
        true ->
            [?ITER_ELEM(E4), ?ITER_ELEM(E3), ?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc]
    end.

%% LEAF3

-compile({inline, bound_rev_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_rev_iterator_LEAF3(Elem, ?LEAF3_ARGS, Acc) ->
    if
        Elem < E1 ->
            Acc;
        %
        Elem < E2 ->
            [?ITER_ELEM(E1) | Acc];
        %
        Elem < E3 ->
            [?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc];
        %
        true ->
            [?ITER_ELEM(E3), ?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc]
    end.

%% LEAF2

-compile({inline, bound_rev_iterator_LEAF2 / ?LEAF2_ARITY_PLUS2}).
bound_rev_iterator_LEAF2(Elem, ?LEAF2_ARGS, Acc) ->
    if
        Elem < E1 ->
            Acc;
        %
        Elem < E2 ->
            [?ITER_ELEM(E1) | Acc];
        %
        true ->
            [?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc]
    end.

%% LEAF1

-compile({inline, bound_rev_iterator_LEAF1 / ?LEAF1_ARITY_PLUS1}).
bound_rev_iterator_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem < E1 ->
            [];
        %
        true ->
            [?ITER_ELEM(E1)]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: larger/2
%% ------------------------------------------------------------------

larger_recur(Elem, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            larger_INTERNAL2(Elem, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            larger_INTERNAL3(Elem, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            larger_INTERNAL4(Elem, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            larger_LEAF2(Elem, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            larger_LEAF3(Elem, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            larger_LEAF4(Elem, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, larger_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
larger_INTERNAL4(Elem, ?INTERNAL4_ARGS) ->
    if
        Elem < E2 ->
            case Elem < E1 of
                true ->
                    case larger_recur(Elem, C1) of
                        none -> {found, E1};
                        Found -> Found
                    end;
                _ ->
                    case larger_recur(Elem, C2) of
                        none -> {found, E2};
                        Found -> Found
                    end
            end;
        %
        Elem < E3 ->
            case larger_recur(Elem, C3) of
                none -> {found, E3};
                Found -> Found
            end;
        %
        Elem < E4 ->
            case larger_recur(Elem, C4) of
                none -> {found, E4};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Elem, C5)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, larger_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
larger_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    if
        Elem < E2 ->
            case Elem < E1 of
                true ->
                    case larger_recur(Elem, C1) of
                        none -> {found, E1};
                        Found -> Found
                    end;
                _ ->
                    case larger_recur(Elem, C2) of
                        none -> {found, E2};
                        Found -> Found
                    end
            end;
        %
        Elem < E3 ->
            case larger_recur(Elem, C3) of
                none -> {found, E3};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Elem, C4)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, larger_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
larger_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    if
        Elem < E1 ->
            case larger_recur(Elem, C1) of
                none -> {found, E1};
                Found -> Found
            end;
        %
        Elem < E2 ->
            case larger_recur(Elem, C2) of
                none -> {found, E2};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Elem, C3)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, larger_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
larger_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem < E1 ->
            case larger_recur(Elem, C1) of
                none -> {found, E1};
                Found -> Found
            end;
        %
        true ->
            larger_recur(Elem, C2)
    end.

%%
%% ?LEAF4
%%

-compile({inline, larger_LEAF4 / ?LEAF4_ARITY_PLUS1}).
larger_LEAF4(Elem, ?LEAF4_ARGS) ->
    if
        Elem < E2 ->
            case Elem < E1 of
                true ->
                    {found, E1};
                _ ->
                    {found, E2}
            end;
        %
        Elem < E3 ->
            {found, E3};
        %
        Elem < E4 ->
            {found, E4};
        %
        true ->
            none
    end.

%%
%% ?LEAF3
%%

-compile({inline, larger_LEAF3 / ?LEAF3_ARITY_PLUS1}).
larger_LEAF3(Elem, ?LEAF3_ARGS) ->
    if
        Elem < E2 ->
            case Elem < E1 of
                true ->
                    {found, E1};
                _ ->
                    {found, E2}
            end;
        %
        Elem < E3 ->
            {found, E3};
        %
        true ->
            none
    end.

%%
%% ?LEAF2
%%

-compile({inline, larger_LEAF2 / ?LEAF2_ARITY_PLUS1}).
larger_LEAF2(Elem, ?LEAF2_ARGS) ->
    if
        Elem < E1 ->
            {found, E1};
        %
        Elem < E2 ->
            {found, E2};
        %
        true ->
            none
    end.

%%
%% ?LEAF1
%%

-compile({inline, larger_LEAF1 / ?LEAF1_ARITY_PLUS1}).
larger_LEAF1(Elem, ?LEAF1_ARGS) ->
    case Elem < E1 of
        true ->
            {found, E1};
        %
        _ ->
            none
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
%% Internal Function Definitions: map/2
%% ------------------------------------------------------------------

map_root(Fun, Node, Acc) ->
    case Node of
        ?INTERNAL1_MATCH_ALL ->
            Acc2 = map_recur(Fun, C1, Acc),
            Acc3 = map_elem(Fun, E1, Acc2),

            _Acc4 = map_recur(Fun, C2, Acc3);
        %
        ?LEAF1_MATCH_ALL ->
            _Acc2 = map_elem(Fun, E1, Acc);
        %
        ?LEAF0 ->
            Acc;
        %
        _ ->
            map_recur(Fun, Node, Acc)
    end.

map_recur(Fun, Node, Acc) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            Acc2 = map_elem(Fun, E1, Acc),
            _Acc3 = map_elem(Fun, E2, Acc2);
        %
        ?LEAF3_MATCH_ALL ->
            Acc2 = map_elem(Fun, E1, Acc),
            Acc3 = map_elem(Fun, E2, Acc2),
            _Acc4 = map_elem(Fun, E3, Acc3);
        %
        ?LEAF4_MATCH_ALL ->
            Acc2 = map_elem(Fun, E1, Acc),
            Acc3 = map_elem(Fun, E2, Acc2),
            Acc4 = map_elem(Fun, E3, Acc3),
            _Acc5 = map_elem(Fun, E4, Acc4);
        %
        ?INTERNAL2_MATCH_ALL ->
            Acc2 = map_recur(Fun, C1, Acc),
            Acc3 = map_elem(Fun, E1, Acc2),

            Acc4 = map_recur(Fun, C2, Acc3),
            Acc5 = map_elem(Fun, E2, Acc4),

            _Acc6 = map_recur(Fun, C3, Acc5);
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = map_recur(Fun, C1, Acc),
            Acc3 = map_elem(Fun, E1, Acc2),

            Acc4 = map_recur(Fun, C2, Acc3),
            Acc5 = map_elem(Fun, E2, Acc4),

            Acc6 = map_recur(Fun, C3, Acc5),
            Acc7 = map_elem(Fun, E3, Acc6),

            _Acc8 = map_recur(Fun, C4, Acc7);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = map_recur(Fun, C1, Acc),
            Acc3 = map_elem(Fun, E1, Acc2),

            Acc4 = map_recur(Fun, C2, Acc3),
            Acc5 = map_elem(Fun, E2, Acc4),

            Acc6 = map_recur(Fun, C3, Acc5),
            Acc7 = map_elem(Fun, E3, Acc6),

            Acc8 = map_recur(Fun, C4, Acc7),
            Acc9 = map_elem(Fun, E4, Acc8),

            _Acc10 = map_recur(Fun, C5, Acc9)
    end.

map_elem(Fun, Elem, [Count | Root] = Acc) ->
    Mapped = Fun(Elem),

    try insert(Mapped, Root) of
        UpdatedRoot ->
            [Count + 1 | UpdatedRoot]
    catch
        error:{key_exists, K} when K =:= Mapped ->
            Acc
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
            [?ITER_ELEM(Elem) | NewTail] = fwd_iterator_recur(Node, Tail),
            Iter2 = NewTail,
            {Elem, Iter2}
    end.

rev_next([Head | Tail]) ->
    rev_next(Head, Tail);
rev_next([]) ->
    none.

rev_next(Head, Tail) ->
    case Head of
        ?ITER_ELEM(Elem) ->
            Iter2 = [?REV_ITER_TAG | Tail],
            {Elem, Iter2};
        %
        Node ->
            [?ITER_ELEM(Elem) | NewTail] = rev_iterator_recur(Node, Tail),
            Iter2 = [?REV_ITER_TAG | NewTail],
            {Elem, Iter2}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: smaller/2
%% ------------------------------------------------------------------

smaller_recur(Elem, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            smaller_INTERNAL2(Elem, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            smaller_INTERNAL3(Elem, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            smaller_INTERNAL4(Elem, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            smaller_LEAF2(Elem, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            smaller_LEAF3(Elem, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            smaller_LEAF4(Elem, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, smaller_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
smaller_INTERNAL4(Elem, ?INTERNAL4_ARGS) ->
    if
        Elem > E3 ->
            case Elem > E4 of
                true ->
                    case smaller_recur(Elem, C5) of
                        none -> {found, E4};
                        Found -> Found
                    end;
                _ ->
                    case smaller_recur(Elem, C4) of
                        none -> {found, E3};
                        Found -> Found
                    end
            end;
        %
        Elem > E2 ->
            case smaller_recur(Elem, C3) of
                none -> {found, E2};
                Found -> Found
            end;
        %
        Elem > E1 ->
            case smaller_recur(Elem, C2) of
                none -> {found, E1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Elem, C1)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, smaller_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
smaller_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    if
        Elem > E2 ->
            case Elem > E3 of
                true ->
                    case smaller_recur(Elem, C4) of
                        none -> {found, E3};
                        Found -> Found
                    end;
                _ ->
                    case smaller_recur(Elem, C3) of
                        none -> {found, E2};
                        Found -> Found
                    end
            end;
        %
        Elem > E1 ->
            case smaller_recur(Elem, C2) of
                none -> {found, E1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Elem, C1)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, smaller_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
smaller_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    if
        Elem > E2 ->
            case smaller_recur(Elem, C3) of
                none -> {found, E2};
                Found -> Found
            end;
        %
        Elem > E1 ->
            case smaller_recur(Elem, C2) of
                none -> {found, E1};
                Found -> Found
            end;
        %
        true ->
            smaller_recur(Elem, C1)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, smaller_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
smaller_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    case Elem > E1 of
        true ->
            case smaller_recur(Elem, C2) of
                none -> {found, E1};
                Found -> Found
            end;
        %
        _ ->
            smaller_recur(Elem, C1)
    end.

%%
%% ?LEAF4
%%

-compile({inline, smaller_LEAF4 / ?LEAF4_ARITY_PLUS1}).
smaller_LEAF4(Elem, ?LEAF4_ARGS) ->
    if
        Elem > E3 ->
            case Elem > E4 of
                true ->
                    {found, E4};
                _ ->
                    {found, E3}
            end;
        %
        Elem > E2 ->
            {found, E2};
        %
        Elem > E1 ->
            {found, E1};
        %
        true ->
            none
    end.

%%
%% ?LEAF3
%%

-compile({inline, smaller_LEAF3 / ?LEAF3_ARITY_PLUS1}).
smaller_LEAF3(Elem, ?LEAF3_ARGS) ->
    if
        Elem > E2 ->
            case Elem > E3 of
                true ->
                    {found, E3};
                _ ->
                    {found, E2}
            end;
        %
        Elem > E1 ->
            {found, E1};
        %
        true ->
            none
    end.

%%
%% ?LEAF2
%%

-compile({inline, smaller_LEAF2 / ?LEAF2_ARITY_PLUS1}).
smaller_LEAF2(Elem, ?LEAF2_ARGS) ->
    if
        Elem > E2 ->
            {found, E2};
        %
        Elem > E1 ->
            {found, E1};
        %
        true ->
            none
    end.

%%
%% ?LEAF1
%%

-compile({inline, smaller_LEAF1 / ?LEAF1_ARITY_PLUS1}).
smaller_LEAF1(Elem, ?LEAF1_ARGS) ->
    case Elem > E1 of
        true ->
            {found, E1};
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
    ?TAKEN(E4, ?new_LEAF3(E1, E2, E3)).

%%
%% ?LEAF3
%%

-compile({inline, take_largest_LEAF3 / ?LEAF3_ARITY}).
take_largest_LEAF3(?LEAF3_ARGS) ->
    ?TAKEN(E3, ?new_LEAF2(E1, E2)).

%%
%% ?LEAF2
%%

-compile({inline, take_largest_LEAF2 / ?LEAF2_ARITY}).
take_largest_LEAF2(?LEAF2_ARGS) ->
    ?TAKEN(E2, ?new_LEAF1(E1)).

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
    ?TAKEN(E1, ?new_LEAF3(E2, E3, E4)).

%%
%% ?LEAF3
%%

-compile({inline, take_smallest_LEAF3 / ?LEAF3_ARITY}).
take_smallest_LEAF3(?LEAF3_ARGS) ->
    ?TAKEN(E1, ?new_LEAF2(E2, E3)).

%%
%% ?LEAF2
%%

-compile({inline, take_smallest_LEAF2 / ?LEAF2_ARITY}).
take_smallest_LEAF2(?LEAF2_ARGS) ->
    ?TAKEN(E1, ?new_LEAF1(E2)).

%%
%% ?LEAF1
%%

-compile({inline, take_smallest_LEAF1 / ?LEAF1_ARITY}).
take_smallest_LEAF1(?LEAF1_ARGS) ->
    ?TAKEN(E1, ?LEAF0).

%% ------------------------------------------------------------------
%% Internal Function Definitions: to_list/1
%% ------------------------------------------------------------------

to_list_recur(Node, Acc) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            [E1, E2 | Acc];
        %
        ?LEAF3_MATCH_ALL ->
            [E1, E2, E3 | Acc];
        %
        ?LEAF4_MATCH_ALL ->
            [E1, E2, E3, E4 | Acc];
        %
        ?INTERNAL2_MATCH_ALL ->
            Acc2 = to_list_recur(C3, Acc),
            Acc3 = to_list_recur(C2, [E2 | Acc2]),
            _Acc4 = to_list_recur(C1, [E1 | Acc3]);
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = to_list_recur(C4, Acc),
            Acc3 = to_list_recur(C3, [E3 | Acc2]),
            Acc4 = to_list_recur(C2, [E2 | Acc3]),
            _Acc5 = to_list_recur(C1, [E1 | Acc4]);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = to_list_recur(C5, Acc),
            Acc3 = to_list_recur(C4, [E4 | Acc2]),
            Acc4 = to_list_recur(C3, [E3 | Acc3]),
            Acc5 = to_list_recur(C2, [E2 | Acc4]),
            _Acc6 = to_list_recur(C1, [E1 | Acc5])
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: union/2
%% ------------------------------------------------------------------

union_root(Root, Size, Target) ->
    case Target of
        ?INTERNAL1_MATCH_ALL ->
            [Size2 | Root2] = union_recur(C1, Size, Root),
            [Size3 | Root3] = union_recur(C2, Size2, Root2),
            union_single_element(E1, Size3, Root3);
        %
        ?LEAF1_MATCH_ALL ->
            union_single_element(E1, Size, Root);
        %
        ?LEAF0 ->
            [Size | Root];
        %
        _ ->
            union_recur(Target, Size, Root)
    end.

union_recur(Node, Size, Root) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            union_batch2(E1, E2, Size, Root);
        %
        ?LEAF3_MATCH_ALL ->
            union_batch3(E1, E2, E3, Size, Root);
        %
        ?LEAF4_MATCH_ALL ->
            union_batch4(E1, E2, E3, E4, Size, Root);
        %
        ?INTERNAL2_MATCH_ALL ->
            % TODO optimize batches like in filter/2
            [Size2 | Root2] = union_recur(C1, Size, Root),
            [Size3 | Root3] = union_recur(C2, Size2, Root2),
            [Size4 | Root4] = union_recur(C3, Size3, Root3),

            union_batch2(E1, E2, Size4, Root4);
        %
        ?INTERNAL3_MATCH_ALL ->
            % TODO optimize batches like in filter/2
            [Size2 | Root2] = union_recur(C1, Size, Root),
            [Size3 | Root3] = union_recur(C2, Size2, Root2),
            [Size4 | Root4] = union_recur(C3, Size3, Root3),
            [Size5 | Root5] = union_recur(C4, Size4, Root4),

            union_batch3(E1, E2, E3, Size5, Root5);
        %
        ?INTERNAL4_MATCH_ALL ->
            % TODO optimize batches like in filter/2
            [Size2 | Root2] = union_recur(C1, Size, Root),
            [Size3 | Root3] = union_recur(C2, Size2, Root2),
            [Size4 | Root4] = union_recur(C3, Size3, Root3),
            [Size5 | Root5] = union_recur(C4, Size4, Root4),
            [Size6 | Root6] = union_recur(C5, Size5, Root5),

            union_batch4(E1, E2, E3, E4, Size6, Root6)
    end.

%% INTERNAL4 and LEAF4

-compile({inline, union_batch4/6}).
union_batch4(E1, E2, E3, E4, Size, Root) ->
    try insert(E1, Root) of
        UpdatedRoot ->
            union_batch3(E2, E3, E4, Size + 1, UpdatedRoot)
    catch
        error:{key_exists, K} when K =:= E1 ->
            union_batch3(E2, E3, E4, Size, Root)
    end.

%% INTERNAL3 and LEAF3

-compile({inline, union_batch3/5}).
union_batch3(E1, E2, E3, Size, Root) ->
    try insert(E1, Root) of
        UpdatedRoot ->
            union_batch2(E2, E3, Size + 1, UpdatedRoot)
    catch
        error:{key_exists, K} when K =:= E1 ->
            union_batch2(E2, E3, Size, Root)
    end.

%% INTERNAL2 and LEAF2

-compile({inline, union_batch2/4}).
union_batch2(E1, E2, Size, Root) ->
    try insert(E1, Root) of
        UpdatedRoot ->
            union_single_element(E2, Size + 1, UpdatedRoot)
    catch
        error:{key_exists, K} when K =:= E1 ->
            union_single_element(E2, Size, Root)
    end.

%% INTERNAL2 and LEAF2

-compile({inline, union_single_element/3}).
union_single_element(E1, Size, Root) ->
    try insert(E1, Root) of
        UpdatedRoot ->
            [Size + 1 | UpdatedRoot]
    catch
        error:{key_exists, K} when K =:= E1 ->
            [Size | Root]
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

-compile({inline, rebalance_INTERNAL4_C1_finish / ?INTERNAL4_ARITY_MINUS2}).
rebalance_INTERNAL4_C1_finish(Result, E2, E3, E4, C3, C4, C5) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL4(UpElem, E2, E3, E4, UpdatedC1, UpdatedC2, C3, C4, C5);
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL3(E2, E3, E4, MergedC1C2, C3, C4, C5)
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
            ?new_INTERNAL3(
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
            ?new_INTERNAL4(
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
            ?new_INTERNAL4(
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
            ?new_INTERNAL3(
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
            ?new_INTERNAL4(
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
            ?new_INTERNAL4(
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
            ?new_INTERNAL3(
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
            ?new_INTERNAL4(
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
            ?new_INTERNAL4(
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

-compile({inline, rebalance_INTERNAL4_C5_finish / ?INTERNAL4_ARITY_MINUS2}).
rebalance_INTERNAL4_C5_finish(Result, E1, E2, E3, C1, C2, C3) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC4, RebalancedC5) ->
            ?new_INTERNAL4(
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
            ?new_INTERNAL3(
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

-compile({inline, rebalance_INTERNAL3_C1_finish / ?INTERNAL3_ARITY_MINUS2}).
rebalance_INTERNAL3_C1_finish(Result, E2, E3, C3, C4) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL3(UpElem, E2, E3, UpdatedC1, UpdatedC2, C3, C4);
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL2(E2, E3, MergedC1C2, C3, C4)
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
            ?new_INTERNAL2(
                E2,
                E3,
                %
                MergedC1C2,
                C3,
                C4
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpElem, RebalancedC2, UpdatedC3) ->
            ?new_INTERNAL3(
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
            ?new_INTERNAL3(
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
            ?new_INTERNAL2(
                E1,
                E3,
                %
                C1,
                MergedC2C3,
                C4
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpElem, RebalancedC3, UpdatedC4) ->
            ?new_INTERNAL3(
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
            ?new_INTERNAL3(
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

-compile({inline, rebalance_INTERNAL3_C4_finish / ?INTERNAL3_ARITY_MINUS2}).
rebalance_INTERNAL3_C4_finish(Result, E1, E2, C1, C2) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC3, RebalancedC4) ->
            ?new_INTERNAL3(
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
            ?new_INTERNAL2(
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

-compile({inline, rebalance_INTERNAL2_C1_finish / ?INTERNAL2_ARITY_MINUS2}).
rebalance_INTERNAL2_C1_finish(Result, E2, C3) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL2(UpElem, E2, UpdatedC1, UpdatedC2, C3);
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL1(E2, MergedC1C2, C3)
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
            ?new_INTERNAL1(
                E2,
                %
                MergedC1C2,
                C3
            );
        %
        ?MID_ROTATED_FROM_RIGHT(UpElem, RebalancedC2, UpdatedC3) ->
            ?new_INTERNAL2(
                E1,
                UpElem,
                %
                C1,
                RebalancedC2,
                UpdatedC3
            );
        %
        ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL2(
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

-compile({inline, rebalance_INTERNAL2_C3_finish / ?INTERNAL2_ARITY_MINUS2}).
rebalance_INTERNAL2_C3_finish(Result, E1, C1) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC2, RebalancedC3) ->
            ?new_INTERNAL2(
                E1,
                UpElem,
                %
                C1,
                UpdatedC2,
                RebalancedC3
            );
        %
        ?MERGED(MergedC2C3) ->
            ?new_INTERNAL1(
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

%-compile({inline, rebalance_INTERNAL1_finish/?INTERNAL1_ARITY_MINUS2}).
rebalance_INTERNAL1_finish(Result) ->
    case Result of
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL1(UpElem, UpdatedC1, UpdatedC2);
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
        ?INTERNAL2_MATCH_ALL ->
            MergedNode = ?new_INTERNAL4(
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

            UpdatedNode = ?new_INTERNAL2(
                CElem,
                ParentE,
                %
                CLeft,
                CRight,
                MovedC
            ),

            UpdatedRight = ?new_INTERNAL2(
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

            UpdatedNode = ?new_INTERNAL2(
                CElem,
                ParentE,
                %
                CLeft,
                CRight,
                MovedC
            ),

            UpdatedRight = ?new_INTERNAL3(
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
            MergedNode = ?new_LEAF4(
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

            UpdatedNode = ?new_LEAF2(CElem, ParentE),
            UpdatedRight = ?new_LEAF2(E2, E3),

            ?ROTATED(UpElem, UpdatedNode, UpdatedRight);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E1,

            UpdatedNode = ?new_LEAF2(CElem, ParentE),
            UpdatedRight = ?new_LEAF3(E2, E3, E4),

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
            MergedNode = ?new_INTERNAL4(
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

            UpdatedNode = ?new_INTERNAL2(
                ParentE,
                CElem,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?new_INTERNAL2(
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

            UpdatedNode = ?new_INTERNAL2(
                ParentE,
                CElem,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?new_INTERNAL3(
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
            MergedNode = ?new_LEAF4(
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

            UpdatedNode = ?new_LEAF2(ParentE, CElem),
            UpdatedLeft = ?new_LEAF2(E1, E2),

            ?ROTATED(UpElem, UpdatedLeft, UpdatedNode);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E4,

            UpdatedNode = ?new_LEAF2(ParentE, CElem),
            UpdatedLeft = ?new_LEAF3(E1, E2, E3),

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

                    UpdatedNode = ?new_INTERNAL2(
                        CElem,
                        RParentE,
                        %
                        CLeft,
                        CRight,
                        MovedC
                    ),

                    UpdatedRight = ?new_INTERNAL2(
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

                    UpdatedNode = ?new_INTERNAL2(
                        CElem,
                        RParentE,
                        %
                        CLeft,
                        CRight,
                        MovedC
                    ),

                    UpdatedRight = ?new_INTERNAL3(
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
                    MergedNode = ?new_INTERNAL4(
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

            UpdatedNode = ?new_INTERNAL2(
                LParentE,
                CElem,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?new_INTERNAL2(
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

            UpdatedNode = ?new_INTERNAL2(
                LParentE,
                CElem,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?new_INTERNAL3(
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

                    UpdatedNode = ?new_LEAF2(CElem, RParentE),
                    UpdatedRight = ?new_LEAF3(E2, E3, E4),

                    ?MID_ROTATED_FROM_RIGHT(UpElem, UpdatedNode, UpdatedRight);
                %
                %
                ?LEAF3_MATCH_ALL ->
                    UpElem = E1,

                    UpdatedNode = ?new_LEAF2(CElem, RParentE),
                    UpdatedRight = ?new_LEAF2(E2, E3),

                    ?MID_ROTATED_FROM_RIGHT(UpElem, UpdatedNode, UpdatedRight);
                %
                %
                _ ->
                    % Merge with left since we already unpacked it
                    MergedNode = ?new_LEAF4(
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

            UpdatedNode = ?new_LEAF2(LParentE, CElem),
            UpdatedLeft = ?new_LEAF2(E1, E2),

            ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E4,

            UpdatedNode = ?new_LEAF2(LParentE, CElem),
            UpdatedLeft = ?new_LEAF3(E1, E2, E3),

            ?MID_ROTATED_FROM_LEFT(UpElem, UpdatedLeft, UpdatedNode)
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
        ?INTERNAL1_MATCH(_, _, _) ->
            'INTERNAL1';
        %
        ?LEAF1_MATCH(_) ->
            'LEAF1';
        %
        _ ->
            recur_node_type(Node)
    end.

check_node_recur(LineNumber, Node) ->
    Type = recur_node_type(Node),
    List = to_list_recur(Node, []),
    MissortedKeys = check_node_keys(List),

    case MissortedKeys of
        [] ->
            Node;
        %
        [_ | _] ->
            fail_node_check(LineNumber, Type, {missorted_keys, MissortedKeys})
    end.

recur_node_type(Node) ->
    case Node of
        ?INTERNAL2_MATCH(_, _, _, _, _) ->
            'INTERNAL2';
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, _) ->
            'INTERNAL3';
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, _) ->
            'INTERNAL4';
        %
        ?LEAF2_MATCH(_, _) ->
            'LEAF2';
        %
        ?LEAF3_MATCH(_, _, _) ->
            'LEAF3';
        %
        ?LEAF4_MATCH(_, _, _, _) ->
            'LEAF4'
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

check_node_keys(E1, [E2 | Next]) ->
    case E1 >= E2 of
        false ->
            check_node_keys(E2, Next);
        %
        true ->
            [{E1, E2} | check_node_keys(E2, Next)]
    end;
check_node_keys(_, []) ->
    [].

% -if(?NODE_CHECK_ENABLED).
-endif.
