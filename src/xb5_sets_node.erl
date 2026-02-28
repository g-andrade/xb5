-module(xb5_sets_node).

-moduledoc """
API for operating over `m:xb5_sets` internal nodes directly.

> ℹ️
> You're likely looking for `m:xb5_sets`.

<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
<br/>
""".

-include("src/xb5_search_helpers.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    delete_att/2,
    difference/4,
    does_root_look_legit/2,
    filtermap/2,
    fold/3,
    from_ordset/2,
    insert_att/2,
    intersection/4,
    is_disjoint/4,
    is_equal/2,
    is_member/2,
    is_subset/4,
    iterator/2,
    iterator_from/3,
    larger/2,
    largest/1,
    map/2,
    new/0,
    next/1,
    singleton/1,
    smaller/2,
    smallest/1,
    structural_stats/1,
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
-define(INTERNAL3(E1, E2, E3, C1, C2, C3, C4),
    {E1, E2, E3, C1, C2, C3, C4}
).
-define(INTERNAL3_MATCH(E1, E2, E3, C1, C2, C3, C4),
    {E1, E2, E3, C1, C2, C3, C4}
).
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

%%%%%%%%

% Cannot clash with any node type.

-define(SPLIT(Pos, Args), [Pos | Args]).
-define(SPLIT_MATCH(Pos, Args), [Pos | Args]).

%%%%%%%%%

%%%

% 3 elements
-define(ROTATED(UpElem, UpdatedLeft, UpdatedRight),
    {UpElem, UpdatedLeft, UpdatedRight}
).

-define(MERGED(MergedNode), (MergedNode)).

%%%%%%%

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

-define(INTERNAL4_C1_REBALANCE(UpdatedC1),
    del_rebalance_INTERNAL4_C1(E1, E2, E3, E4, UpdatedC1, C2, C3, C4, C5)
).
-define(INTERNAL4_C2_REBALANCE(UpdatedC2),
    del_rebalance_INTERNAL4_C2(E1, E2, E3, E4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_C3_REBALANCE(UpdatedC3),
    del_rebalance_INTERNAL4_C3(E1, E2, E3, E4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_C4_REBALANCE(UpdatedC4),
    del_rebalance_INTERNAL4_C4(E1, E2, E3, E4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_C5_REBALANCE(UpdatedC5),
    del_rebalance_INTERNAL4_C5(E1, E2, E3, E4, C1, C2, C3, C4, UpdatedC5)
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

-define(INTERNAL3_UPD_C1(UpdatedC1), ?new_INTERNAL3(E1, E2, E3, UpdatedC1, C2, C3, C4)).
-define(INTERNAL3_UPD_C2(UpdatedC2), ?new_INTERNAL3(E1, E2, E3, C1, UpdatedC2, C3, C4)).
-define(INTERNAL3_UPD_C3(UpdatedC3), ?new_INTERNAL3(E1, E2, E3, C1, C2, UpdatedC3, C4)).
-define(INTERNAL3_UPD_C4(UpdatedC4), ?new_INTERNAL3(E1, E2, E3, C1, C2, C3, UpdatedC4)).

-define(INTERNAL3_C1_REBALANCE(UpdatedC1),
    del_rebalance_INTERNAL3_C1(E1, E2, E3, UpdatedC1, C2, C3, C4)
).
-define(INTERNAL3_C2_REBALANCE(UpdatedC2),
    del_rebalance_INTERNAL3_C2(E1, E2, E3, C1, UpdatedC2, C3, C4)
).
-define(INTERNAL3_C3_REBALANCE(UpdatedC3),
    del_rebalance_INTERNAL3_C3(E1, E2, E3, C1, C2, UpdatedC3, C4)
).
-define(INTERNAL3_C4_REBALANCE(UpdatedC4),
    del_rebalance_INTERNAL3_C4(E1, E2, E3, C1, C2, C3, UpdatedC4)
).

-define(INTERNAL3_ARGS_IGN_E1, _, E2, E3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_E2, E1, _, E3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_E3, E1, E2, _, C1, C2, C3, C4).

%% ?INTERNAL2

-define(INTERNAL2_ARGS, E1, E2, C1, C2, C3).
-define(INTERNAL2_ARITY, 5).
-define(INTERNAL2_ARITY_PLUS1, 6).
-define(INTERNAL2_ARITY_PLUS2, 7).

-define(INTERNAL2_UPD_C1(UpdatedC1), ?new_INTERNAL2(E1, E2, UpdatedC1, C2, C3)).
-define(INTERNAL2_UPD_C2(UpdatedC2), ?new_INTERNAL2(E1, E2, C1, UpdatedC2, C3)).
-define(INTERNAL2_UPD_C3(UpdatedC3), ?new_INTERNAL2(E1, E2, C1, C2, UpdatedC3)).

-define(INTERNAL2_C1_REBALANCE(UpdatedC1),
    del_rebalance_INTERNAL2_C1(E1, E2, UpdatedC1, C2, C3)
).
-define(INTERNAL2_C2_REBALANCE(UpdatedC2),
    del_rebalance_INTERNAL2_C2(E1, E2, C1, UpdatedC2, C3)
).
-define(INTERNAL2_C3_REBALANCE(UpdatedC3),
    del_rebalance_INTERNAL2_C3(E1, E2, C1, C2, UpdatedC3)
).

-define(INTERNAL2_ARGS_IGN_E1, _, E2, C1, C2, C3).
-define(INTERNAL2_ARGS_IGN_E2, E1, _, C1, C2, C3).

%% ?INTERNAL1

-define(INTERNAL1_ARGS, E1, C1, C2).
-define(INTERNAL1_ARITY, 3).
-define(INTERNAL1_ARITY_PLUS1, 4).
-define(INTERNAL1_ARITY_PLUS2, 5).

-define(INTERNAL1_UPD_C1(UpdatedC1), ?new_INTERNAL1(E1, UpdatedC1, C2)).
-define(INTERNAL1_UPD_C2(UpdatedC2), ?new_INTERNAL1(E1, C1, UpdatedC2)).

-define(INTERNAL1_C1_REBALANCE(UpdatedC1), del_rebalance_INTERNAL1_C1(E1, UpdatedC1, C2)).
-define(INTERNAL1_C2_REBALANCE(UpdatedC2), del_rebalance_INTERNAL1_C2(E1, C1, UpdatedC2)).

-define(INTERNAL1_E1_C2_REBALANCE(ReplacementE, UpdatedC2),
    del_rebalance_INTERNAL1_C2(ReplacementE, C1, UpdatedC2)
).

-define(INTERNAL1_ARGS_IGN_E1, _, C1, C2).

%% ?LEAF4

-define(LEAF4_ARGS, E1, E2, E3, E4).
-define(LEAF4_ARITY_PLUS1, 5).
-define(LEAF4_ARITY_PLUS2, 6).

%% ?LEAF3

-define(LEAF3_ARGS, E1, E2, E3).
-define(LEAF3_ARITY_PLUS1, 4).
-define(LEAF3_ARITY_PLUS2, 5).

%% ?LEAF2

-define(LEAF2_ARGS, E1, E2).
-define(LEAF2_ARITY_PLUS1, 3).
-define(LEAF2_ARITY_PLUS2, 4).

%% ?LEAF1

-define(LEAF1_ARGS, E1).
-define(LEAF1_ARITY_PLUS1, 2).

%%

-define(TAKEN(Elem, UpdatedNode), [Elem | UpdatedNode]).

%%

% defined(TEST)).
-define(NODE_CHECK_ENABLED, false).

-if(?NODE_CHECK_ENABLED).
-define(CHECK_NODE(Node), check_node(?LINE, Node, top)).
-define(CHECK_NODE_RECUR(Node), check_node(?LINE, Node, recur)).
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

-type nonempty_node(Elem) ::
    (node_INTERNAL1(Elem)
    | node_LEAF1(Elem)
    | deep_node(Elem)).

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

-type node_LEAF2(Elem) :: ?LEAF2(Elem, Elem).

-type node_LEAF1(Elem) :: ?LEAF1(Elem).

%%%%%%%%%%%

-type split_internal_result(Elem) :: split_result(
    Elem, node_INTERNAL2(Elem), node_INTERNAL2(Elem)
).

-type split_leaf_result(Elem) :: split_result(
    Elem, node_LEAF2(Elem), node_LEAF2(Elem)
).

-type split_result(Elem, SplitL, SplitR) :: {split, Elem, SplitL, SplitR}.

%%%%%%%%%%%

-type take_result(Elem) :: nonempty_improper_list(Elem, t(Elem)).
-export_type([take_result/1]).

%%%%%%%%%%%

-opaque iter(Elem) :: forward_iter(Elem) | reverse_iter(Elem).
-export_type([iter/1]).

-type forward_iter(Elem) :: [iterator_step(Elem)].
-type reverse_iter(Elem) :: nonempty_improper_list(reversed, [iterator_step(Elem)]).

-type iterator_step(Elem) ::
    (iter_elem(Elem)
    | deep_node(Elem)).

-type iter_elem(Elem) :: [Elem, ...].

%%%%%%%%%%%%

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec delete_att(_, t(Elem)) -> badkey | t(Elem).
delete_att(Elem, ?INTERNAL1_MATCH_ALL) ->
    delete_att_INTERNAL1(Elem, ?INTERNAL1_ARGS);
delete_att(Elem, ?LEAF1_MATCH_ALL) ->
    delete_att_LEAF1(Elem, ?LEAF1_ARGS);
delete_att(_Elem, ?LEAF0) ->
    badkey;
delete_att(Elem, Root) ->
    delete_att_recur(Elem, Root).

-spec difference(non_neg_integer(), t(Elem1), non_neg_integer(), t(_)) ->
    nonempty_improper_list(NewSize :: non_neg_integer(), NewRoot :: t(Elem1)).
difference(Size1, Root1, Size2, Root2) ->
    difference_root(Size1, to_rev_list(Root1), Size2, Root2).

-spec does_root_look_legit(term(), term()) -> boolean().
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

-spec filtermap(fun((Elem) -> {true, MappedElem} | boolean()), t(Elem)) ->
    nonempty_improper_list(FilteredSize, t(Elem | MappedElem))
when
    FilteredSize :: non_neg_integer().
filtermap(Fun, Root) ->
    List = to_rev_list(Root),
    filtermap_recur(Fun, List, 0, [], []).

-spec fold(fun((Elem, Acc1) -> Acc2), Acc0, t(Elem)) -> AccN when
    Acc0 :: term(),
    Acc1 :: term(),
    Acc2 :: term(),
    AccN :: term().
fold(Fun, Acc, ?INTERNAL1_MATCH_ALL) ->
    Acc2 = fold_recur(Fun, Acc, C1),
    Acc3 = Fun(E1, Acc2),
    fold_recur(Fun, Acc3, C2);
fold(Fun, Acc, ?LEAF1_MATCH_ALL) ->
    Fun(E1, Acc);
fold(_Fun, Acc, ?LEAF0) ->
    Acc;
fold(Fun, Acc, Root) ->
    fold_recur(Fun, Acc, Root).

-spec from_ordset(ordsets:ordset(Elem), non_neg_integer()) -> t(Elem).
from_ordset([], 0) ->
    ?LEAF0;
from_ordset([E1], 1) ->
    ?new_LEAF1(E1);
from_ordset(L, S) ->
    [BatchOffset | BatchSize] = from_ordset_initial_batch_params(S),
    AtRoot = true,

    [Root | []] = from_ordset_recur(L, S, BatchOffset, BatchSize, AtRoot),
    Root.

-spec insert_att(NewElem, t(PrevElem)) -> key_exists | t(NewElem | PrevElem).
insert_att(Elem, ?INTERNAL1_MATCH_ALL) ->
    insert_att_INTERNAL1(Elem, ?INTERNAL1_ARGS);
insert_att(Elem, ?LEAF1_MATCH_ALL) ->
    insert_att_LEAF1(Elem, ?LEAF1_ARGS);
insert_att(Elem, ?LEAF0) ->
    ?new_LEAF1(Elem);
insert_att(Elem, Root) ->
    case insert_att_recur(Elem, Root) of
        ?SPLIT_MATCH(Pos, Args) ->
            insert_split_root(Elem, Pos, Args, Root);
        %
        UpdatedRoot ->
            UpdatedRoot
    end.

-spec intersection(non_neg_integer(), t(Elem1), non_neg_integer(), t(Elem2)) ->
    nonempty_improper_list(NewSize :: non_neg_integer(), t(Elem1 | Elem2)).
intersection(Size1, Root1, Size2, Root2) ->
    if
        Size2 < Size1 ->
            intersection_root(Size2, to_rev_list(Root2), Size1, Root1);
        %
        true ->
            intersection_root(Size1, to_rev_list(Root1), Size2, Root2)
    end.

-spec is_disjoint(t(_), non_neg_integer(), t(_), non_neg_integer()) -> boolean().
is_disjoint(Root1, Size1, Root2, Size2) ->
    if
        Size1 =:= 0 orelse Size2 =:= 0 ->
            true;
        %
        Size1 < Size2 ->
            is_disjoint_root(Root1, Root2);
        %
        true ->
            is_disjoint_root(Root2, Root1)
    end.

-spec is_equal(t(_), t(_)) -> boolean().
is_equal(Root1, Root2) ->
    Iter1 = fwd_iterator(Root1),
    Iter2 = fwd_iterator(Root2),
    is_equal_recur(Iter1, Iter2).

-spec is_member(_, t(_)) -> boolean().
is_member(Elem, ?INTERNAL1_MATCH_ALL) ->
    is_member_INTERNAL1(Elem, ?INTERNAL1_ARGS);
is_member(Elem, ?LEAF1_MATCH_ALL) ->
    is_member_LEAF1(Elem, ?LEAF1_ARGS);
is_member(_Elem, ?LEAF0) ->
    false;
is_member(Elem, Root) ->
    is_member_recur(Elem, Root).

-spec is_subset(non_neg_integer(), t(_), non_neg_integer(), t(_)) -> boolean().
is_subset(Size1, Root1, Size2, Root2) ->
    is_subset_root(Size1, to_rev_list(Root1), Size2, Root2).

-spec iterator(t(Elem), ordered | reversed) -> iter(Elem).
iterator(Root, ordered) ->
    fwd_iterator(Root);
iterator(Root, reversed) ->
    Acc = rev_iterator(Root),
    [?REV_ITER_TAG | Acc].

-spec iterator_from(_, t(Elem), ordered | reversed) -> iter(Elem).
iterator_from(Elem, Root, ordered) ->
    bound_fwd_iterator(Elem, Root);
iterator_from(Elem, Root, reversed) ->
    Acc = bound_rev_iterator(Elem, Root),
    [?REV_ITER_TAG | Acc].

-spec larger(_, t(Elem)) -> {found, Elem} | none.
larger(Elem, ?INTERNAL1_MATCH_ALL) ->
    larger_INTERNAL1(Elem, ?INTERNAL1_ARGS);
larger(Elem, ?LEAF1_MATCH_ALL) ->
    larger_LEAF1(Elem, ?LEAF1_ARGS);
larger(_Elem, ?LEAF0) ->
    none;
larger(Elem, Root) ->
    larger_recur(Elem, Root).

-spec largest(t(Elem)) -> Elem.
largest(?INTERNAL1_MATCH(_, _, C2)) ->
    largest_recur(C2);
largest(?LEAF1_MATCH(E1)) ->
    E1;
largest(Root) ->
    largest_recur(Root).

-spec map(fun((Elem) -> MappedElem), t(Elem)) -> nonempty_improper_list(NewSize, t(MappedElem)) when
    NewSize :: non_neg_integer().
map(Fun, Root) ->
    List = to_list(Root),
    map_recur(Fun, List, 0, new()).

-spec new() -> t(_).
new() ->
    ?LEAF0.

-spec next(iter(Elem)) -> {Elem, iter(Elem)} | none.
next([Head | Tail]) ->
    next(Head, Tail);
next([]) ->
    none.

-spec singleton(Elem) -> t(Elem).
singleton(Elem) ->
    ?LEAF1(Elem).

-spec smaller(_, t(Elem)) -> {found, Elem} | none.
smaller(Elem, ?INTERNAL1_MATCH_ALL) ->
    smaller_INTERNAL1(Elem, ?INTERNAL1_ARGS);
smaller(Elem, ?LEAF1_MATCH_ALL) ->
    smaller_LEAF1(Elem, ?LEAF1_ARGS);
smaller(_Elem, ?LEAF0) ->
    none;
smaller(Elem, Root) ->
    smaller_recur(Elem, Root).

-spec smallest(t(Elem)) -> Elem.
smallest(?INTERNAL1_MATCH(_, C1, _)) ->
    smallest_recur(C1);
smallest(?LEAF1_MATCH(E1)) ->
    E1;
smallest(Root) ->
    smallest_recur(Root).

-spec structural_stats(t(_)) -> xb5_structural_stats:t().
structural_stats(Root) ->
    Acc = xb5_structural_stats:new(),

    case Root of
        ?INTERNAL1_MATCH(_, C1, C2) ->
            Height = 1,
            Acc2 = xb5_structural_stats:inc_count(internal1, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            xb5_structural_stats:return(Acc4);
        %
        ?LEAF1_MATCH(_) ->
            Height = 1,
            Acc2 = xb5_structural_stats:inc_count(leaf1, Acc),
            Acc3 = xb5_structural_stats:set_height(Height, Acc2),
            xb5_structural_stats:return(Acc3);
        %
        ?LEAF0 ->
            xb5_structural_stats:return(Acc);
        %
        _ ->
            Height = 1,
            Acc2 = structural_stats_recur(Root, Acc, Height),
            xb5_structural_stats:return(Acc2)
    end.

-spec take_largest(t(Elem)) -> take_result(Elem).
take_largest(?INTERNAL1_MATCH_ALL) ->
    ?TAKEN(Taken, UpdatedC2) = take_largest_recur(C2),
    ?TAKEN(Taken, ?INTERNAL1_C2_REBALANCE(UpdatedC2));
take_largest(?LEAF1_MATCH_ALL) ->
    ?TAKEN(E1, ?LEAF0);
take_largest(Root) ->
    take_largest_recur(Root).

-spec take_smallest(t(Elem)) -> take_result(Elem).
take_smallest(?INTERNAL1_MATCH_ALL) ->
    ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
    ?TAKEN(Taken, ?INTERNAL1_C1_REBALANCE(UpdatedC1));
take_smallest(?LEAF1_MATCH_ALL) ->
    ?TAKEN(E1, ?LEAF0);
take_smallest(Root) ->
    take_smallest_recur(Root).

-spec to_list(t(Elem)) -> [Elem].
to_list(?INTERNAL1_MATCH_ALL) ->
    Acc2 = to_list_recur(C2, []),
    Acc3 = [E1 | Acc2],
    to_list_recur(C1, Acc3);
to_list(?LEAF1_MATCH_ALL) ->
    [E1];
to_list(?LEAF0) ->
    [];
to_list(Root) ->
    to_list_recur(Root, []).

-spec union(Size1, t(Elem1), Size2, t(Elem2)) ->
    nonempty_improper_list(NewSize, t(Elem1 | Elem2))
when
    Size1 :: non_neg_integer(),
    Size2 :: non_neg_integer(),
    NewSize :: non_neg_integer().
union(Size1, Root1, Size2, Root2) when Size2 < Size1 ->
    union_root(Size2, Root2, Size1, Root1);
union(Size1, Root1, Size2, Root2) ->
    union_root(Size1, Root1, Size2, Root2).

%% ------------------------------------------------------------------
%% Internal Function Definitions: delete_att/2
%% ------------------------------------------------------------------

delete_att_recur(Elem, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            delete_att_INTERNAL2(Elem, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            delete_att_INTERNAL3(Elem, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            delete_att_INTERNAL4(Elem, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            delete_att_LEAF2(Elem, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            delete_att_LEAF3(Elem, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            delete_att_LEAF4(Elem, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, delete_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4(Elem, ?INTERNAL4_ARGS) ->
    ?GAP_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        delete_att_INTERNAL4_E1(?INTERNAL4_ARGS),
        delete_att_INTERNAL4_E2(?INTERNAL4_ARGS),
        delete_att_INTERNAL4_E3(?INTERNAL4_ARGS),
        delete_att_INTERNAL4_E4(?INTERNAL4_ARGS),
        %
        delete_att_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS),
        delete_att_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS),
        delete_att_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS),
        delete_att_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS),
        delete_att_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS)
    ).

-compile({inline, delete_att_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC1 = delete_att_recur(Elem, C1),

    ?INTERNAL4_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_att_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC2 = delete_att_recur(Elem, C2),

    ?INTERNAL4_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_att_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC3 = delete_att_recur(Elem, C3),

    ?INTERNAL4_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_att_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC4 = delete_att_recur(Elem, C4),

    ?INTERNAL4_C4_REBALANCE(UpdatedC4).

-compile({inline, delete_att_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
delete_att_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS) ->
    UpdatedC5 = delete_att_recur(Elem, C5),

    ?INTERNAL4_C5_REBALANCE(UpdatedC5).

%%

-compile({inline, delete_att_INTERNAL4_E1 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_E1(?INTERNAL4_ARGS_IGN_E1) ->
    ?TAKEN(ReplacementE, UpdatedC2) = take_smallest_recur(C2),

    del_rebalance_INTERNAL4_C2(
        ReplacementE,
        E2,
        E3,
        E4,
        %
        C1,
        UpdatedC2,
        C3,
        C4,
        C5
    ).

-compile({inline, delete_att_INTERNAL4_E2 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_E2(?INTERNAL4_ARGS_IGN_E2) ->
    ?TAKEN(ReplacementE, UpdatedC2) = take_largest_recur(C2),

    del_rebalance_INTERNAL4_C2(
        E1,
        ReplacementE,
        E3,
        E4,
        %
        C1,
        UpdatedC2,
        C3,
        C4,
        C5
    ).

-compile({inline, delete_att_INTERNAL4_E3 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_E3(?INTERNAL4_ARGS_IGN_E3) ->
    ?TAKEN(ReplacementE, UpdatedC4) = take_smallest_recur(C4),

    del_rebalance_INTERNAL4_C4(
        E1,
        E2,
        ReplacementE,
        E4,
        %
        C1,
        C2,
        C3,
        UpdatedC4,
        C5
    ).

-compile({inline, delete_att_INTERNAL4_E4 / ?INTERNAL4_ARITY}).
delete_att_INTERNAL4_E4(?INTERNAL4_ARGS_IGN_E4) ->
    ?TAKEN(ReplacementE, UpdatedC4) = take_largest_recur(C4),

    del_rebalance_INTERNAL4_C4(
        E1,
        E2,
        E3,
        ReplacementE,
        %
        C1,
        C2,
        C3,
        UpdatedC4,
        C5
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, delete_att_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    ?GAP_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        delete_att_INTERNAL3_E1(?INTERNAL3_ARGS),
        delete_att_INTERNAL3_E2(?INTERNAL3_ARGS),
        delete_att_INTERNAL3_E3(?INTERNAL3_ARGS),
        %
        delete_att_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS),
        delete_att_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS),
        delete_att_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS),
        delete_att_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS)
    ).

-compile({inline, delete_att_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC1 = delete_att_recur(Elem, C1),

    ?INTERNAL3_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_att_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC2 = delete_att_recur(Elem, C2),

    ?INTERNAL3_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_att_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC3 = delete_att_recur(Elem, C3),

    ?INTERNAL3_C3_REBALANCE(UpdatedC3).

-compile({inline, delete_att_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
delete_att_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS) ->
    UpdatedC4 = delete_att_recur(Elem, C4),

    ?INTERNAL3_C4_REBALANCE(UpdatedC4).

%%

-compile({inline, delete_att_INTERNAL3_E1 / ?INTERNAL3_ARITY}).
delete_att_INTERNAL3_E1(?INTERNAL3_ARGS_IGN_E1) ->
    ?TAKEN(ReplacementE, UpdatedC2) = take_smallest_recur(C2),

    del_rebalance_INTERNAL3_C2(
        ReplacementE,
        E2,
        E3,
        %
        C1,
        UpdatedC2,
        C3,
        C4
    ).

-compile({inline, delete_att_INTERNAL3_E2 / ?INTERNAL3_ARITY}).
delete_att_INTERNAL3_E2(?INTERNAL3_ARGS_IGN_E2) ->
    ?TAKEN(ReplacementE, UpdatedC3) = take_smallest_recur(C3),

    del_rebalance_INTERNAL3_C3(
        E1,
        ReplacementE,
        E3,
        %
        C1,
        C2,
        UpdatedC3,
        C4
    ).

-compile({inline, delete_att_INTERNAL3_E3 / ?INTERNAL3_ARITY}).
delete_att_INTERNAL3_E3(?INTERNAL3_ARGS_IGN_E3) ->
    ?TAKEN(ReplacementE, UpdatedC3) = take_largest_recur(C3),

    del_rebalance_INTERNAL3_C3(
        E1,
        E2,
        ReplacementE,
        %
        C1,
        C2,
        UpdatedC3,
        C4
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, delete_att_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
delete_att_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    ?GAP_SEARCH2(
        Elem,
        E1,
        E2,
        %
        delete_att_INTERNAL2_E1(?INTERNAL2_ARGS),
        delete_att_INTERNAL2_E2(?INTERNAL2_ARGS),
        %
        delete_att_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS),
        delete_att_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS),
        delete_att_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS)
    ).

-compile({inline, delete_att_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
delete_att_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS) ->
    UpdatedC1 = delete_att_recur(Elem, C1),

    ?INTERNAL2_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_att_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
delete_att_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS) ->
    UpdatedC2 = delete_att_recur(Elem, C2),

    ?INTERNAL2_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_att_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
delete_att_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS) ->
    UpdatedC3 = delete_att_recur(Elem, C3),

    ?INTERNAL2_C3_REBALANCE(UpdatedC3).

%%

-compile({inline, delete_att_INTERNAL2_E1 / ?INTERNAL2_ARITY}).
delete_att_INTERNAL2_E1(?INTERNAL2_ARGS_IGN_E1) ->
    ?TAKEN(ReplacementE, UpdatedC2) = take_smallest_recur(C2),

    del_rebalance_INTERNAL2_C2(
        ReplacementE,
        E2,
        %
        C1,
        UpdatedC2,
        C3
    ).

-compile({inline, delete_att_INTERNAL2_E2 / ?INTERNAL2_ARITY}).
delete_att_INTERNAL2_E2(?INTERNAL2_ARGS_IGN_E2) ->
    ?TAKEN(ReplacementE, UpdatedC2) = take_largest_recur(C2),

    del_rebalance_INTERNAL2_C2(
        E1,
        ReplacementE,
        %
        C1,
        UpdatedC2,
        C3
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, delete_att_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
delete_att_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    ?GAP_SEARCH1(
        Elem,
        E1,
        %
        delete_att_INTERNAL1_E1(?INTERNAL1_ARGS),
        delete_att_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS),
        delete_att_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS)
    ).

-compile({inline, delete_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
delete_att_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    UpdatedC1 = delete_att_recur(Elem, C1),
    ?INTERNAL1_C1_REBALANCE(UpdatedC1).

-compile({inline, delete_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
delete_att_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS) ->
    UpdatedC2 = delete_att_recur(Elem, C2),
    ?INTERNAL1_C2_REBALANCE(UpdatedC2).

-compile({inline, delete_att_INTERNAL1_E1 / ?INTERNAL1_ARITY}).
delete_att_INTERNAL1_E1(?INTERNAL1_ARGS_IGN_E1) ->
    ?TAKEN(ReplacementE, UpdatedC2) = take_smallest_recur(C2),
    ?INTERNAL1_E1_C2_REBALANCE(ReplacementE, UpdatedC2).

%%
%% Leaves
%%

-compile({inline, delete_att_LEAF4 / ?LEAF4_ARITY_PLUS1}).
delete_att_LEAF4(Elem, ?LEAF4_ARGS) ->
    ?EXACT_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        ?new_LEAF3(E2, E3, E4),
        ?new_LEAF3(E1, E3, E4),
        ?new_LEAF3(E1, E2, E4),
        ?new_LEAF3(E1, E2, E3),
        badkey
    ).

-compile({inline, delete_att_LEAF3 / ?LEAF3_ARITY_PLUS1}).
delete_att_LEAF3(Elem, ?LEAF3_ARGS) ->
    ?EXACT_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        ?new_LEAF2(E2, E3),
        ?new_LEAF2(E1, E3),
        ?new_LEAF2(E1, E2),
        badkey
    ).

-compile({inline, delete_att_LEAF2 / ?LEAF2_ARITY_PLUS1}).
delete_att_LEAF2(Elem, ?LEAF2_ARGS) ->
    ?EXACT_SEARCH2(
        Elem,
        E1,
        E2,
        ?new_LEAF1(E2),
        ?new_LEAF1(E1),
        badkey
    ).

-compile({inline, delete_att_LEAF1 / ?LEAF1_ARITY_PLUS1}).
delete_att_LEAF1(Elem, ?LEAF1_ARGS) ->
    ?EXACT_SEARCH1(
        Elem,
        E1,
        ?LEAF0,
        badkey
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: difference/2
%% ------------------------------------------------------------------

difference_root(Size1, List1, Size2, Root2) when Size2 < 10 ->
    difference_2(Size1, List1, to_rev_list(Root2));
difference_root(Size1, List1, Size2, Root2) ->
    ThresholdSize = Size1 * round(math:log2(Size2)),

    if
        Size2 < ThresholdSize ->
            difference_2(Size1, List1, to_rev_list(Root2));
        %
        true ->
            difference_1(List1, Root2)
    end.

difference_1(List1, Root2) ->
    difference_1(List1, Root2, 0, []).

difference_1([Element | Next], Root2, AccSize, Acc) ->
    case is_member(Element, Root2) of
        true ->
            difference_1(Next, Root2, AccSize, Acc);
        %
        _ ->
            difference_1(Next, Root2, AccSize + 1, [Element | Acc])
    end;
difference_1([], _, AccSize, Acc) ->
    [AccSize | from_ordset(Acc, AccSize)].

difference_2(Size1, List1, List2) ->
    difference_2(List1, List2, Size1, []).

difference_2([Element1 | Next1] = List1, [Element2 | Next2] = List2, AccSize, Acc) ->
    if
        Element1 > Element2 ->
            difference_2(Next1, List2, AccSize, [Element1 | Acc]);
        %
        Element1 < Element2 ->
            difference_2(List1, Next2, AccSize, Acc);
        %
        true ->
            difference_2(Next1, Next2, AccSize - 1, Acc)
    end;
difference_2([], _List2, AccSize, Acc) ->
    [AccSize | from_ordset(Acc, AccSize)];
difference_2(List1, [], AccSize, Acc) ->
    FinalAcc = lists:reverse(List1, Acc),
    [AccSize | from_ordset(FinalAcc, AccSize)].

%% ------------------------------------------------------------------
%% Internal Function Definitions: filtermap/2
%% ------------------------------------------------------------------

filtermap_recur(Fun, [Element | Prev], FilteredSize, Filtered, Mapped) ->
    case Fun(Element) of
        {true, MappedElement} ->
            filtermap_recur(Fun, Prev, FilteredSize, Filtered, [MappedElement | Mapped]);
        %
        true ->
            filtermap_recur(Fun, Prev, FilteredSize + 1, [Element | Filtered], Mapped);
        %
        false ->
            filtermap_recur(Fun, Prev, FilteredSize, Filtered, Mapped)
    end;
filtermap_recur(_Fun, [], FilteredSize, Filtered, Mapped) ->
    NewRoot = from_ordset(Filtered, FilteredSize),
    filtermap_insert_mapped(FilteredSize, NewRoot, Mapped).

filtermap_insert_mapped(Size, Root, [Element | Next]) ->
    case insert_att(Element, Root) of
        key_exists ->
            filtermap_insert_mapped(Size, Root, Next);
        %
        UpdatedRoot ->
            filtermap_insert_mapped(Size + 1, UpdatedRoot, Next)
    end;
filtermap_insert_mapped(Size, Root, []) ->
    [Size | Root].

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
%% Internal Function Definitions: from_ordset/2
%% ------------------------------------------------------------------

from_ordset_initial_batch_params(S) when S >= 1 ->
    from_ordset_initial_batch_params(S, 1, 1).

from_ordset_initial_batch_params(S, BatchOffset, BatchSize) ->
    case BatchOffset + (4 * BatchSize) of
        NextOffset when NextOffset =< S ->
            from_ordset_initial_batch_params(S, NextOffset, BatchSize * 4);
        _ ->
            [BatchOffset | BatchSize]
    end.

from_ordset_recur(L, S, BatchOffset, BatchSize, AtRoot) when S >= 5 ->
    ChildrenBatchOffset = BatchOffset - BatchSize,
    ChildrenBatchSize = BatchSize bsr 2,

    case (S - BatchOffset) div BatchSize of
        2 ->
            S1 = S2 = BatchSize - 1,
            [S3 | S4] = from_ordset_right_children_sizes(S - (BatchSize bsl 1), BatchSize),
            from_ordset_INTERNAL3(L, S1, S2, S3, S4, ChildrenBatchOffset, ChildrenBatchSize);
        %
        3 ->
            S1 = S2 = S3 = BatchSize - 1,
            [S4 | S5] = from_ordset_right_children_sizes(S - (BatchSize * 3), BatchSize),
            from_ordset_INTERNAL4(L, S1, S2, S3, S4, S5, ChildrenBatchOffset, ChildrenBatchSize);
        %
        Splits when Splits =:= 1 orelse (Splits =:= 0 andalso not AtRoot) ->
            S1 = BatchSize - 1,
            [S2 | S3] = from_ordset_right_children_sizes(S - BatchSize, BatchSize),
            from_ordset_INTERNAL2(L, S1, S2, S3, ChildrenBatchOffset, ChildrenBatchSize);
        %
        0 ->
            [S1 | S2] = from_ordset_right_children_sizes(S, BatchSize),
            from_ordset_INTERNAL1(L, S1, S2, ChildrenBatchOffset, ChildrenBatchSize)
    end;
from_ordset_recur(L, 4, _, _, _) ->
    [E1, E2, E3, E4 | Next] = L,
    [?new_LEAF4(E1, E2, E3, E4) | Next];
from_ordset_recur(L, 3, _, _, _) ->
    [E1, E2, E3 | Next] = L,
    [?new_LEAF3(E1, E2, E3) | Next];
from_ordset_recur(L, 2, _, _, _) ->
    [E1, E2 | Next] = L,
    [?new_LEAF2(E1, E2) | Next].

from_ordset_right_children_sizes(RemainingSize, BatchSize) ->
    case RemainingSize bsr 1 < BatchSize of
        true ->
            SLeft = (BatchSize * 3 div 4) - 1,
            SRight = RemainingSize - SLeft - 1,
            [SLeft | SRight];
        %
        false ->
            SLeft = BatchSize - 1,
            SRight = RemainingSize - SLeft - 1,
            [SLeft | SRight]
    end.

-compile({inline, from_ordset_INTERNAL1/5}).
from_ordset_INTERNAL1(L, S1, S2, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [E1 | L2]] = from_ordset_recur(L, S1, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C2 | []] = from_ordset_recur(L2, S2, ChildrenBatchOffset, ChildrenBatchSize, false),

    Node = ?new_INTERNAL1(E1, C1, C2),

    [Node | []].

-compile({inline, from_ordset_INTERNAL2/6}).
from_ordset_INTERNAL2(L, S1, S2, S3, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [E1 | L2]] = from_ordset_recur(L, S1, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C2 | [E2 | L3]] = from_ordset_recur(L2, S2, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C3 | L4] = from_ordset_recur(L3, S3, ChildrenBatchOffset, ChildrenBatchSize, false),

    Node = ?new_INTERNAL2(E1, E2, C1, C2, C3),

    [Node | L4].

-compile({inline, from_ordset_INTERNAL3/7}).
from_ordset_INTERNAL3(L, S1, S2, S3, S4, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [E1 | L2]] = from_ordset_recur(L, S1, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C2 | [E2 | L3]] = from_ordset_recur(L2, S2, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C3 | [E3 | L4]] = from_ordset_recur(L3, S3, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C4 | L5] = from_ordset_recur(L4, S4, ChildrenBatchOffset, ChildrenBatchSize, false),

    Node = ?new_INTERNAL3(E1, E2, E3, C1, C2, C3, C4),

    [Node | L5].

-compile({inline, from_ordset_INTERNAL4/8}).
from_ordset_INTERNAL4(L, S1, S2, S3, S4, S5, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [E1 | L2]] = from_ordset_recur(L, S1, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C2 | [E2 | L3]] = from_ordset_recur(L2, S2, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C3 | [E3 | L4]] = from_ordset_recur(L3, S3, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C4 | [E4 | L5]] = from_ordset_recur(L4, S4, ChildrenBatchOffset, ChildrenBatchSize, false),
    [C5 | L6] = from_ordset_recur(L5, S5, ChildrenBatchOffset, ChildrenBatchSize, false),

    Node = ?new_INTERNAL4(E1, E2, E3, E4, C1, C2, C3, C4, C5),

    [Node | L6].

%% ------------------------------------------------------------------
%% Internal Function Definitions: insert_att/2
%% ------------------------------------------------------------------

insert_att_recur(Elem, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            insert_att_INTERNAL2(Elem, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            insert_att_INTERNAL3(Elem, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            insert_att_INTERNAL4(Elem, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            insert_att_LEAF2(Elem, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            insert_att_LEAF3(Elem, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            insert_att_LEAF4(Elem, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, insert_att_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
insert_att_INTERNAL4(Elem, ?INTERNAL4_ARGS) ->
    ?GAP_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        key_exists,
        key_exists,
        key_exists,
        key_exists,
        %
        insert_att_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS),
        insert_att_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS),
        insert_att_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS),
        insert_att_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS),
        insert_att_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS)
    ).

-compile({inline, insert_att_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
insert_att_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Elem, C1),
    ins_rebalance_INTERNAL4_C1(Result, Elem, ?INTERNAL4_ARGS).

-compile({inline, insert_att_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
insert_att_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Elem, C2),
    ins_rebalance_INTERNAL4_C2(Result, Elem, ?INTERNAL4_ARGS).

-compile({inline, insert_att_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
insert_att_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Elem, C3),
    ins_rebalance_INTERNAL4_C3(Result, Elem, ?INTERNAL4_ARGS).

-compile({inline, insert_att_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
insert_att_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Elem, C4),
    ins_rebalance_INTERNAL4_C4(Result, Elem, ?INTERNAL4_ARGS).

-compile({inline, insert_att_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
insert_att_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS) ->
    Result = insert_att_recur(Elem, C5),
    ins_rebalance_INTERNAL4_C5(Result, Elem, ?INTERNAL4_ARGS).

%%
%% ?INTERNAL3
%%

-compile({inline, insert_att_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
insert_att_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    ?GAP_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        key_exists,
        key_exists,
        key_exists,
        %
        insert_att_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS),
        insert_att_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS),
        insert_att_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS),
        insert_att_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS)
    ).

-compile({inline, insert_att_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
insert_att_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS) ->
    Result = insert_att_recur(Elem, C1),
    ins_rebalance_INTERNAL3_C1(Result, Elem, ?INTERNAL3_ARGS).

-compile({inline, insert_att_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
insert_att_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS) ->
    Result = insert_att_recur(Elem, C2),
    ins_rebalance_INTERNAL3_C2(Result, Elem, ?INTERNAL3_ARGS).

-compile({inline, insert_att_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
insert_att_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS) ->
    Result = insert_att_recur(Elem, C3),
    ins_rebalance_INTERNAL3_C3(Result, Elem, ?INTERNAL3_ARGS).

-compile({inline, insert_att_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
insert_att_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS) ->
    Result = insert_att_recur(Elem, C4),
    ins_rebalance_INTERNAL3_C4(Result, Elem, ?INTERNAL3_ARGS).

%%
%% ?INTERNAL2
%%

-compile({inline, insert_att_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
insert_att_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    ?GAP_SEARCH2(
        Elem,
        E1,
        E2,
        %
        key_exists,
        key_exists,
        %
        insert_att_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS),
        insert_att_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS),
        insert_att_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS)
    ).

-compile({inline, insert_att_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
insert_att_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS) ->
    Result = insert_att_recur(Elem, C1),
    ins_rebalance_INTERNAL2_C1(Result, Elem, ?INTERNAL2_ARGS).

-compile({inline, insert_att_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
insert_att_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS) ->
    Result = insert_att_recur(Elem, C2),
    ins_rebalance_INTERNAL2_C2(Result, Elem, ?INTERNAL2_ARGS).

-compile({inline, insert_att_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
insert_att_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS) ->
    Result = insert_att_recur(Elem, C3),
    ins_rebalance_INTERNAL2_C3(Result, Elem, ?INTERNAL2_ARGS).

%%
%% ?INTERNAL1
%%

-compile({inline, insert_att_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
insert_att_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    ?GAP_SEARCH1(
        Elem,
        E1,
        %
        key_exists,
        %
        insert_att_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS),
        insert_att_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS)
    ).

-compile({inline, insert_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
insert_att_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    Result = insert_att_recur(Elem, C1),
    ins_rebalance_INTERNAL1_C1(Result, Elem, ?INTERNAL1_ARGS).

-compile({inline, insert_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
insert_att_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS) ->
    Result = insert_att_recur(Elem, C2),
    ins_rebalance_INTERNAL1_C2(Result, Elem, ?INTERNAL1_ARGS).

%%
%% Leaves
%%

-compile({inline, insert_att_LEAF4 / ?LEAF4_ARITY_PLUS1}).
insert_att_LEAF4(Elem, ?LEAF4_ARGS) ->
    ?GAP_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        key_exists,
        key_exists,
        key_exists,
        key_exists,
        %
        ?SPLIT(1, []),
        ?SPLIT(2, []),
        ?SPLIT(3, []),
        ?SPLIT(4, []),
        ?SPLIT(5, [])
    ).

-compile({inline, insert_att_LEAF3 / ?LEAF3_ARITY_PLUS1}).
insert_att_LEAF3(Elem, ?LEAF3_ARGS) ->
    ?GAP_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        key_exists,
        key_exists,
        key_exists,
        %
        ?new_LEAF4(Elem, E1, E2, E3),
        ?new_LEAF4(E1, Elem, E2, E3),
        ?new_LEAF4(E1, E2, Elem, E3),
        ?new_LEAF4(E1, E2, E3, Elem)
    ).

-compile({inline, insert_att_LEAF2 / ?LEAF2_ARITY_PLUS1}).
insert_att_LEAF2(Elem, ?LEAF2_ARGS) ->
    ?GAP_SEARCH2(
        Elem,
        E1,
        E2,
        %
        key_exists,
        key_exists,
        %
        ?new_LEAF3(Elem, E1, E2),
        ?new_LEAF3(E1, Elem, E2),
        ?new_LEAF3(E1, E2, Elem)
    ).

-compile({inline, insert_att_LEAF1 / ?LEAF1_ARITY_PLUS1}).
insert_att_LEAF1(Elem, ?LEAF1_ARGS) ->
    ?GAP_SEARCH1(
        Elem,
        E1,
        %
        key_exists,
        %
        ?new_LEAF2(Elem, E1),
        ?new_LEAF2(E1, Elem)
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: intersection/4
%% ------------------------------------------------------------------

intersection_root(_Size1, List1, Size2, Root2) when Size2 < 10 ->
    intersection_2(List1, to_rev_list(Root2));
intersection_root(Size1, List1, Size2, Root2) ->
    ThresholdSize = Size1 * round(math:log2(Size2)),

    if
        Size2 < ThresholdSize ->
            intersection_2(List1, to_rev_list(Root2));
        %
        true ->
            intersection_1(List1, Root2)
    end.

intersection_1(List, Root) ->
    intersection_1(List, Root, 0, []).

intersection_1([Element | Next], Root, AccSize, Acc) ->
    case is_member(Element, Root) of
        true ->
            intersection_1(Next, Root, AccSize + 1, [Element | Acc]);
        %
        _ ->
            intersection_1(Next, Root, AccSize, Acc)
    end;
intersection_1([], _Root, AccSize, Acc) ->
    [AccSize | from_ordset(Acc, AccSize)].

intersection_2(List1, List2) ->
    intersection_2(List1, List2, 0, []).

intersection_2([Element1 | Next1] = List1, [Element2 | Next2] = List2, AccSize, Acc) ->
    if
        Element1 > Element2 ->
            intersection_2(Next1, List2, AccSize, Acc);
        %
        Element1 < Element2 ->
            intersection_2(Next2, List1, AccSize, Acc);
        %
        true ->
            intersection_2(Next1, Next2, AccSize + 1, [Element1 | Acc])
    end;
intersection_2([], _List2, AccSize, Acc) ->
    [AccSize | from_ordset(Acc, AccSize)];
intersection_2(_List1, [], AccSize, Acc) ->
    [AccSize | from_ordset(Acc, AccSize)].

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_disjoint/4
%% ------------------------------------------------------------------

is_disjoint_root(Root1, Root2) ->
    MinElem = smallest(Root2),
    MaxElem = largest(Root2),
    Iter = bound_fwd_iterator(MinElem, Root1),
    is_disjoint_recur(Iter, Root2, MaxElem).

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
    ?GAP_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        true,
        true,
        true,
        true,
        %
        is_member_recur(Elem, C1),
        is_member_recur(Elem, C2),
        is_member_recur(Elem, C3),
        is_member_recur(Elem, C4),
        is_member_recur(Elem, C5)
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, is_member_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
is_member_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    ?GAP_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        true,
        true,
        true,
        %
        is_member_recur(Elem, C1),
        is_member_recur(Elem, C2),
        is_member_recur(Elem, C3),
        is_member_recur(Elem, C4)
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, is_member_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
is_member_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    ?GAP_SEARCH2(
        Elem,
        E1,
        E2,
        %
        true,
        true,
        %
        is_member_recur(Elem, C1),
        is_member_recur(Elem, C2),
        is_member_recur(Elem, C3)
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, is_member_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
is_member_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    ?GAP_SEARCH1(
        Elem,
        E1,
        %
        true,
        %
        is_member_recur(Elem, C1),
        is_member_recur(Elem, C2)
    ).

%%
%% Leaves
%%

-compile({inline, is_member_LEAF4 / ?LEAF4_ARITY_PLUS1}).
is_member_LEAF4(Elem, ?LEAF4_ARGS) ->
    (Elem == E1 orelse
        Elem == E2 orelse
        Elem == E3 orelse
        Elem == E4).

-compile({inline, is_member_LEAF3 / ?LEAF3_ARITY_PLUS1}).
is_member_LEAF3(Elem, ?LEAF3_ARGS) ->
    (Elem == E1 orelse
        Elem == E2 orelse
        Elem == E3).

-compile({inline, is_member_LEAF2 / ?LEAF2_ARITY_PLUS1}).
is_member_LEAF2(Elem, ?LEAF2_ARGS) ->
    Elem == E1 orelse Elem == E2.

-compile({inline, is_member_LEAF1 / ?LEAF1_ARITY_PLUS1}).
is_member_LEAF1(Elem, ?LEAF1_ARGS) ->
    Elem == E1.

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_subset/4
%% ------------------------------------------------------------------

is_subset_root(_Size1, List1, Size2, Root2) when Size2 < 10 ->
    is_subset_2(List1, to_rev_list(Root2));
is_subset_root(Size1, List1, Size2, Root2) ->
    ThresholdSize = Size1 * round(math:log2(Size2)),

    if
        Size2 < ThresholdSize ->
            is_subset_2(List1, to_rev_list(Root2));
        %
        true ->
            is_subset_1(List1, Root2)
    end.

is_subset_1([Element | Next], Root2) ->
    is_member(Element, Root2) andalso is_subset_1(Next, Root2);
is_subset_1([], _) ->
    true.

is_subset_2([Element1 | Next1] = List1, [Element2 | Next2]) ->
    if
        Element1 > Element2 ->
            false;
        %
        Element1 < Element2 ->
            is_subset_2(List1, Next2);
        %
        true ->
            is_subset_2(Next1, Next2)
    end;
is_subset_2([], _) ->
    true;
is_subset_2(_, []) ->
    false.

%% ------------------------------------------------------------------
%% Internal Function Definitions: iterator/2 - forward
%% ------------------------------------------------------------------

fwd_iterator(?INTERNAL1_MATCH(E1, C1, C2)) ->
    Acc = [?ITER_ELEM(E1), C2],
    fwd_iterator_recur(C1, Acc);
fwd_iterator(?LEAF1_MATCH(E1)) ->
    Iter = [?ITER_ELEM(E1)],
    Iter;
fwd_iterator(?LEAF0) ->
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
rev_iterator(?LEAF0) ->
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
        ?LEAF0 ->
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
    ?SMALLER_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        %
        begin
            Acc2 = [
                ?ITER_ELEM(E1), C2, ?ITER_ELEM(E2), C3, ?ITER_ELEM(E3), C4, ?ITER_ELEM(E4), C5 | Acc
            ],
            bound_fwd_iterator_recur(Elem, C1, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E2), C3, ?ITER_ELEM(E3), C4, ?ITER_ELEM(E4), C5 | Acc],
            bound_fwd_iterator_recur(Elem, C2, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E3), C4, ?ITER_ELEM(E4), C5 | Acc],
            bound_fwd_iterator_recur(Elem, C3, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E4), C5 | Acc],
            bound_fwd_iterator_recur(Elem, C4, Acc2)
        end,
        %
        %
        case Acc of
            [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_fwd_iterator_recur(Elem, C5, Acc)
        end
    ).

%% INTERNAL3

-compile({inline, bound_fwd_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc) ->
    ?SMALLER_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E1), C2, ?ITER_ELEM(E2), C3, ?ITER_ELEM(E3), C4 | Acc],
            bound_fwd_iterator_recur(Elem, C1, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E2), C3, ?ITER_ELEM(E3), C4 | Acc],
            bound_fwd_iterator_recur(Elem, C2, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E3), C4 | Acc],
            bound_fwd_iterator_recur(Elem, C3, Acc2)
        end,
        %
        %
        case Acc of
            [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_fwd_iterator_recur(Elem, C4, Acc)
        end
    ).

%% INTERNAL2

-compile({inline, bound_fwd_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
bound_fwd_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc) ->
    ?SMALLER_SEARCH2(
        Elem,
        E1,
        E2,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E1), C2, ?ITER_ELEM(E2), C3 | Acc],
            bound_fwd_iterator_recur(Elem, C1, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E2), C3 | Acc],
            bound_fwd_iterator_recur(Elem, C2, Acc2)
        end,
        %
        %
        case Acc of
            [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_fwd_iterator_recur(Elem, C3, Acc)
        end
    ).

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
    ?SMALLER_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        [?ITER_ELEM(E1), ?ITER_ELEM(E2), ?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc],
        [?ITER_ELEM(E2), ?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc],
        [?ITER_ELEM(E3), ?ITER_ELEM(E4) | Acc],
        [?ITER_ELEM(E4) | Acc],
        Acc
    ).

%% LEAF3

-compile({inline, bound_fwd_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_fwd_iterator_LEAF3(Elem, ?LEAF3_ARGS, Acc) ->
    ?SMALLER_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        [?ITER_ELEM(E1), ?ITER_ELEM(E2), ?ITER_ELEM(E3) | Acc],
        [?ITER_ELEM(E2), ?ITER_ELEM(E3) | Acc],
        [?ITER_ELEM(E3) | Acc],
        Acc
    ).

%% LEAF2

-compile({inline, bound_fwd_iterator_LEAF2 / ?LEAF2_ARITY_PLUS2}).
bound_fwd_iterator_LEAF2(Elem, ?LEAF2_ARGS, Acc) ->
    ?SMALLER_SEARCH2(
        Elem,
        E1,
        E2,
        %
        [?ITER_ELEM(E1), ?ITER_ELEM(E2) | Acc],
        [?ITER_ELEM(E2) | Acc],
        Acc
    ).

%% LEAF1

-compile({inline, bound_fwd_iterator_LEAF1 / ?LEAF1_ARITY_PLUS1}).
bound_fwd_iterator_LEAF1(Elem, ?LEAF1_ARGS) ->
    ?SMALLER_SEARCH1(
        Elem,
        E1,
        %
        [?ITER_ELEM(E1)],
        []
    ).

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
        ?LEAF0 ->
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
    ?LARGER_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        %
        case Acc of
            [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_rev_iterator_recur(Elem, C1, Acc)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C2, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C3, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E3), C3, ?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C4, Acc2)
        end,
        %
        %
        begin
            Acc2 = [
                ?ITER_ELEM(E4), C4, ?ITER_ELEM(E3), C3, ?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc
            ],
            bound_rev_iterator_recur(Elem, C5, Acc2)
        end
    ).

%% INTERNAL3

-compile({inline, bound_rev_iterator_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc) ->
    ?LARGER_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        %
        case Acc of
            [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_rev_iterator_recur(Elem, C1, Acc)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C2, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C3, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E3), C3, ?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C4, Acc2)
        end
    ).

%% INTERNAL2

-compile({inline, bound_rev_iterator_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
bound_rev_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc) ->
    ?LARGER_SEARCH2(
        Elem,
        E1,
        E2,
        %
        %
        case Acc of
            [?ITER_ELEM(AccNextElem) | _] when AccNextElem == Elem ->
                % We overshot when recursing from this node's parent, stop here
                % since no more elements can possibly be included.
                Acc;
            _ ->
                bound_rev_iterator_recur(Elem, C1, Acc)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C2, Acc2)
        end,
        %
        %
        begin
            Acc2 = [?ITER_ELEM(E2), C2, ?ITER_ELEM(E1), C1 | Acc],
            bound_rev_iterator_recur(Elem, C3, Acc2)
        end
    ).

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
    ?LARGER_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        Acc,
        [?ITER_ELEM(E1) | Acc],
        [?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc],
        [?ITER_ELEM(E3), ?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc],
        [?ITER_ELEM(E4), ?ITER_ELEM(E3), ?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc]
    ).

%% LEAF3

-compile({inline, bound_rev_iterator_LEAF3 / ?LEAF3_ARITY_PLUS2}).
bound_rev_iterator_LEAF3(Elem, ?LEAF3_ARGS, Acc) ->
    ?LARGER_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        Acc,
        [?ITER_ELEM(E1) | Acc],
        [?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc],
        [?ITER_ELEM(E3), ?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc]
    ).

%% LEAF2

-compile({inline, bound_rev_iterator_LEAF2 / ?LEAF2_ARITY_PLUS2}).
bound_rev_iterator_LEAF2(Elem, ?LEAF2_ARGS, Acc) ->
    ?LARGER_SEARCH2(
        Elem,
        E1,
        E2,
        %
        Acc,
        [?ITER_ELEM(E1) | Acc],
        [?ITER_ELEM(E2), ?ITER_ELEM(E1) | Acc]
    ).

%% LEAF1

-compile({inline, bound_rev_iterator_LEAF1 / ?LEAF1_ARITY_PLUS1}).
bound_rev_iterator_LEAF1(Elem, ?LEAF1_ARGS) ->
    ?LARGER_SEARCH1(
        Elem,
        E1,
        %
        [],
        [?ITER_ELEM(E1)]
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: larger/2
%% ------------------------------------------------------------------

larger_recur(Elem, Node) ->
    case Node of
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
    ?LARGER_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        case larger_recur(Elem, C1) of
            none -> {found, E1};
            Found -> Found
        end,
        %
        case larger_recur(Elem, C2) of
            none -> {found, E2};
            Found -> Found
        end,
        %
        case larger_recur(Elem, C3) of
            none -> {found, E3};
            Found -> Found
        end,
        %
        case larger_recur(Elem, C4) of
            none -> {found, E4};
            Found -> Found
        end,
        %
        larger_recur(Elem, C5)
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, larger_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
larger_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    ?LARGER_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        case larger_recur(Elem, C1) of
            none -> {found, E1};
            Found -> Found
        end,
        %
        case larger_recur(Elem, C2) of
            none -> {found, E2};
            Found -> Found
        end,
        %
        case larger_recur(Elem, C3) of
            none -> {found, E3};
            Found -> Found
        end,
        %
        larger_recur(Elem, C4)
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, larger_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
larger_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    ?LARGER_SEARCH2(
        Elem,
        E1,
        E2,
        %
        case larger_recur(Elem, C1) of
            none -> {found, E1};
            Found -> Found
        end,
        %
        case larger_recur(Elem, C2) of
            none -> {found, E2};
            Found -> Found
        end,
        %
        larger_recur(Elem, C3)
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, larger_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
larger_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    ?LARGER_SEARCH1(
        Elem,
        E1,
        %
        case larger_recur(Elem, C1) of
            none -> {found, E1};
            Found -> Found
        end,
        %
        larger_recur(Elem, C2)
    ).

%%
%% Leaves
%%

-compile({inline, larger_LEAF4 / ?LEAF4_ARITY_PLUS1}).
larger_LEAF4(Elem, ?LEAF4_ARGS) ->
    ?LARGER_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        {found, E1},
        {found, E2},
        {found, E3},
        {found, E4},
        none
    ).

-compile({inline, larger_LEAF3 / ?LEAF3_ARITY_PLUS1}).
larger_LEAF3(Elem, ?LEAF3_ARGS) ->
    ?LARGER_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        {found, E1},
        {found, E2},
        {found, E3},
        none
    ).

-compile({inline, larger_LEAF2 / ?LEAF2_ARITY_PLUS1}).
larger_LEAF2(Elem, ?LEAF2_ARGS) ->
    ?LARGER_SEARCH2(
        Elem,
        E1,
        E2,
        %
        {found, E1},
        {found, E2},
        none
    ).

-compile({inline, larger_LEAF1 / ?LEAF1_ARITY_PLUS1}).
larger_LEAF1(Elem, ?LEAF1_ARGS) ->
    ?LARGER_SEARCH1(
        Elem,
        E1,
        %
        {found, E1},
        none
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: largest/1
%% ------------------------------------------------------------------

largest_recur(Node) ->
    case Node of
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

map_recur(Fun, [Element | Next], AccSize, Acc) ->
    case insert_att(Fun(Element), Acc) of
        key_exists ->
            map_recur(Fun, Next, AccSize, Acc);
        %
        UpdatedAcc ->
            map_recur(Fun, Next, AccSize + 1, UpdatedAcc)
    end;
map_recur(_Fun, [], AccSize, Acc) ->
    [AccSize | Acc].

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
    ?SMALLER_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        smaller_recur(Elem, C1),
        %
        case smaller_recur(Elem, C2) of
            none -> {found, E1};
            Found -> Found
        end,
        %
        case smaller_recur(Elem, C3) of
            none -> {found, E2};
            Found -> Found
        end,
        %
        case smaller_recur(Elem, C4) of
            none -> {found, E3};
            Found -> Found
        end,
        %
        case smaller_recur(Elem, C5) of
            none -> {found, E4};
            Found -> Found
        end
    ).

%%
%% ?INTERNAL3
%%

-compile({inline, smaller_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
smaller_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    ?SMALLER_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        smaller_recur(Elem, C1),
        %
        case smaller_recur(Elem, C2) of
            none -> {found, E1};
            Found -> Found
        end,
        %
        case smaller_recur(Elem, C3) of
            none -> {found, E2};
            Found -> Found
        end,
        %
        case smaller_recur(Elem, C4) of
            none -> {found, E3};
            Found -> Found
        end
    ).

%%
%% ?INTERNAL2
%%

-compile({inline, smaller_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
smaller_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    ?SMALLER_SEARCH2(
        Elem,
        E1,
        E2,
        %
        smaller_recur(Elem, C1),
        %
        case smaller_recur(Elem, C2) of
            none -> {found, E1};
            Found -> Found
        end,
        %
        case smaller_recur(Elem, C3) of
            none -> {found, E2};
            Found -> Found
        end
    ).

%%
%% ?INTERNAL1
%%

-compile({inline, smaller_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
smaller_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    ?SMALLER_SEARCH1(
        Elem,
        E1,
        %
        smaller_recur(Elem, C1),
        %
        case smaller_recur(Elem, C2) of
            none -> {found, E1};
            Found -> Found
        end
    ).

%%
%% Leaves
%%

-compile({inline, smaller_LEAF4 / ?LEAF4_ARITY_PLUS1}).
smaller_LEAF4(Elem, ?LEAF4_ARGS) ->
    ?SMALLER_SEARCH4(
        Elem,
        E1,
        E2,
        E3,
        E4,
        %
        none,
        {found, E1},
        {found, E2},
        {found, E3},
        {found, E4}
    ).

-compile({inline, smaller_LEAF3 / ?LEAF3_ARITY_PLUS1}).
smaller_LEAF3(Elem, ?LEAF3_ARGS) ->
    ?SMALLER_SEARCH3(
        Elem,
        E1,
        E2,
        E3,
        %
        none,
        {found, E1},
        {found, E2},
        {found, E3}
    ).

-compile({inline, smaller_LEAF2 / ?LEAF2_ARITY_PLUS1}).
smaller_LEAF2(Elem, ?LEAF2_ARGS) ->
    ?SMALLER_SEARCH2(
        Elem,
        E1,
        E2,
        %
        none,
        {found, E1},
        {found, E2}
    ).

-compile({inline, smaller_LEAF1 / ?LEAF1_ARITY_PLUS1}).
smaller_LEAF1(Elem, ?LEAF1_ARGS) ->
    ?SMALLER_SEARCH1(
        Elem,
        E1,
        %
        none,
        {found, E1}
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions: smallest/1
%% ------------------------------------------------------------------

smallest_recur(Node) ->
    case Node of
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
%% Internal Function Definitions: structural_stats/1
%% ------------------------------------------------------------------

structural_stats_recur(Node, Acc, Height) ->
    case Node of
        ?LEAF2_MATCH(_, _) ->
            Acc2 = xb5_structural_stats:inc_count(leaf2, Acc),
            xb5_structural_stats:set_height(Height, Acc2);
        %
        ?LEAF3_MATCH(_, _, _) ->
            Acc2 = xb5_structural_stats:inc_count(leaf3, Acc),
            xb5_structural_stats:set_height(Height, Acc2);
        %
        ?LEAF4_MATCH(_, _, _, _) ->
            Acc2 = xb5_structural_stats:inc_count(leaf4, Acc),
            xb5_structural_stats:set_height(Height, Acc2);
        %
        ?INTERNAL2_MATCH(_, _, C1, C2, C3) ->
            Acc2 = xb5_structural_stats:inc_count(internal2, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            _Acc5 = structural_stats_recur(C3, Acc4, Height + 1);
        %
        ?INTERNAL3_MATCH(_, _, _, C1, C2, C3, C4) ->
            Acc2 = xb5_structural_stats:inc_count(internal3, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            _Acc6 = structural_stats_recur(C4, Acc5, Height + 1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, C1, C2, C3, C4, C5) ->
            Acc2 = xb5_structural_stats:inc_count(internal4, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            Acc6 = structural_stats_recur(C4, Acc5, Height + 1),
            _Acc7 = structural_stats_recur(C5, Acc6, Height + 1)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_largest/2
%% ------------------------------------------------------------------

take_largest_recur(Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC3) = take_largest_recur(C3),
            ?TAKEN(Taken, ?INTERNAL2_C3_REBALANCE(UpdatedC3));
        %
        ?INTERNAL3_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC4) = take_largest_recur(C4),
            ?TAKEN(Taken, ?INTERNAL3_C4_REBALANCE(UpdatedC4));
        %
        ?INTERNAL4_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC5) = take_largest_recur(C5),
            ?TAKEN(Taken, ?INTERNAL4_C5_REBALANCE(UpdatedC5));
        %
        ?LEAF2_MATCH_ALL ->
            ?TAKEN(E2, ?new_LEAF1(E1));
        %
        ?LEAF3_MATCH_ALL ->
            ?TAKEN(E3, ?new_LEAF2(E1, E2));
        %
        ?LEAF4_MATCH_ALL ->
            ?TAKEN(E4, ?new_LEAF3(E1, E2, E3))
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: take_smallest/2
%% ------------------------------------------------------------------

take_smallest_recur(Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
            ?TAKEN(Taken, ?INTERNAL2_C1_REBALANCE(UpdatedC1));
        %
        ?INTERNAL3_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
            ?TAKEN(Taken, ?INTERNAL3_C1_REBALANCE(UpdatedC1));
        %
        ?INTERNAL4_MATCH_ALL ->
            ?TAKEN(Taken, UpdatedC1) = take_smallest_recur(C1),
            ?TAKEN(Taken, ?INTERNAL4_C1_REBALANCE(UpdatedC1));
        %
        ?LEAF2_MATCH_ALL ->
            ?TAKEN(E1, ?new_LEAF1(E2));
        %
        ?LEAF3_MATCH_ALL ->
            ?TAKEN(E1, ?new_LEAF2(E2, E3));
        %
        ?LEAF4_MATCH_ALL ->
            ?TAKEN(E1, ?new_LEAF3(E2, E3, E4))
    end.

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
%% Internal Function Definitions: to_rev_list/1
%% ------------------------------------------------------------------

-spec to_rev_list(t(Elem)) -> [Elem].
to_rev_list(?INTERNAL1_MATCH_ALL) ->
    Acc2 = to_rev_list_recur(C1, []),
    Acc3 = [E1 | Acc2],
    to_rev_list_recur(C2, Acc3);
to_rev_list(?LEAF1_MATCH_ALL) ->
    [E1];
to_rev_list(?LEAF0) ->
    [];
to_rev_list(Root) ->
    to_rev_list_recur(Root, []).

to_rev_list_recur(Node, Acc) ->
    case Node of
        ?LEAF2_MATCH_ALL ->
            [E2, E1 | Acc];
        %
        ?LEAF3_MATCH_ALL ->
            [E3, E2, E1 | Acc];
        %
        ?LEAF4_MATCH_ALL ->
            [E4, E3, E2, E1 | Acc];
        %
        ?INTERNAL2_MATCH_ALL ->
            Acc2 = to_rev_list_recur(C1, Acc),
            Acc3 = to_rev_list_recur(C2, [E1 | Acc2]),
            _Acc4 = to_rev_list_recur(C3, [E2 | Acc3]);
        %
        ?INTERNAL3_MATCH_ALL ->
            Acc2 = to_rev_list_recur(C1, Acc),
            Acc3 = to_rev_list_recur(C2, [E1 | Acc2]),
            Acc4 = to_rev_list_recur(C3, [E2 | Acc3]),
            _Acc5 = to_rev_list_recur(C4, [E3 | Acc4]);
        %
        ?INTERNAL4_MATCH_ALL ->
            Acc2 = to_rev_list_recur(C1, Acc),
            Acc3 = to_rev_list_recur(C2, [E1 | Acc2]),
            Acc4 = to_rev_list_recur(C3, [E2 | Acc3]),
            Acc5 = to_rev_list_recur(C4, [E3 | Acc4]),
            _Acc6 = to_rev_list_recur(C5, [E4 | Acc5])
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: union/2
%% ------------------------------------------------------------------

union_root(0, _, Size2, Root2) ->
    [Size2 | Root2];
union_root(Size1, Root1, Size2, Root2) when Size2 < 10 ->
    List1 = to_rev_list(Root1),
    List2 = to_rev_list(Root2),
    union_2(List1, List2, Size1 + Size2);
union_root(Size1, Root1, Size2, Root2) ->
    ThresholdSize = Size1 * round(math:log2(Size2)),

    if
        Size2 < ThresholdSize ->
            List1 = to_rev_list(Root1),
            List2 = to_rev_list(Root2),
            union_2(List1, List2, Size1 + Size2);
        %
        true ->
            List1 = to_rev_list(Root1),
            union_1(List1, Size2, Root2)
    end.

union_1([Element | Next], Size2, Root2) ->
    case insert_att(Element, Root2) of
        key_exists ->
            union_1(Next, Size2, Root2);
        %
        UpdatedRoot2 ->
            union_1(Next, Size2 + 1, UpdatedRoot2)
    end;
union_1([], Size2, Root2) ->
    [Size2 | Root2].

union_2(List1, List2, AccSize) ->
    union_2(List1, List2, AccSize, []).

union_2([Element1 | Next1] = List1, [Element2 | Next2] = List2, AccSize, Acc) ->
    if
        Element1 > Element2 ->
            union_2(Next1, List2, AccSize, [Element1 | Acc]);
        %
        Element1 < Element2 ->
            union_2(Next2, List1, AccSize, [Element2 | Acc]);
        %
        true ->
            union_2(Next1, Next2, AccSize - 1, [Element1 | Acc])
    end;
union_2([], List2, AccSize, Acc) ->
    FinalAcc = lists:reverse(List2, Acc),
    [AccSize | from_ordset(FinalAcc, AccSize)];
union_2(List1, [], AccSize, Acc) ->
    FinalAcc = lists:reverse(List1, Acc),
    [AccSize | from_ordset(FinalAcc, AccSize)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Insertion Rebalance handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing INTERNAL4
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS2}).
ins_rebalance_INTERNAL4_C1(Result, Elem, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_right_sibling_maybe(Elem, C1, Pos, Args, E1, C2) of
                {UpElem, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL4(
                        UpElem,
                        E2,
                        E3,
                        E4,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3,
                        C4,
                        C5
                    );
                %
                Split ->
                    ?SPLIT(1, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC1 ->
            ?INTERNAL4_UPD_C1(UpdatedC1)
    end.

-compile({inline, ins_rebalance_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS2}).
ins_rebalance_INTERNAL4_C2(Result, Elem, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C2, Pos, Args, E1, C1) of
                {UpElem, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL4(
                        UpElem,
                        E2,
                        E3,
                        E4,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3,
                        C4,
                        C5
                    );
                %
                Split ->
                    ?SPLIT(2, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC2 ->
            ?INTERNAL4_UPD_C2(UpdatedC2)
    end.

-compile({inline, ins_rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS2}).
ins_rebalance_INTERNAL4_C3(Result, Elem, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C3, Pos, Args, E2, C2) of
                {UpElem, UpdatedC2, UpdatedC3} ->
                    ?new_INTERNAL4(
                        E1,
                        UpElem,
                        E3,
                        E4,
                        %
                        C1,
                        UpdatedC2,
                        UpdatedC3,
                        C4,
                        C5
                    );
                %
                Split ->
                    ?SPLIT(3, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC3 ->
            ?INTERNAL4_UPD_C3(UpdatedC3)
    end.

-compile({inline, ins_rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS2}).
ins_rebalance_INTERNAL4_C4(Result, Elem, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C4, Pos, Args, E3, C3) of
                {UpElem, UpdatedC3, UpdatedC4} ->
                    ?new_INTERNAL4(
                        E1,
                        E2,
                        UpElem,
                        E4,
                        %
                        C1,
                        C2,
                        UpdatedC3,
                        UpdatedC4,
                        C5
                    );
                %
                Split ->
                    ?SPLIT(4, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC4 ->
            ?INTERNAL4_UPD_C4(UpdatedC4)
    end.

-compile({inline, ins_rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS2}).
ins_rebalance_INTERNAL4_C5(Result, Elem, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C5, Pos, Args, E4, C4) of
                {UpElem, UpdatedC4, UpdatedC5} ->
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
                        UpdatedC5
                    );
                %
                Split ->
                    ?SPLIT(5, Split)
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC5 ->
            ?INTERNAL4_UPD_C5(UpdatedC5)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing INTERNAL3
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS2}).
ins_rebalance_INTERNAL3_C1(Result, Elem, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_right_sibling_maybe(Elem, C1, Pos, Args, E1, C2) of
                {UpElem, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL3(
                        UpElem,
                        E2,
                        E3,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3,
                        C4
                    );
                %
                {split, SplitE, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC1 ->
            ?INTERNAL3_UPD_C1(UpdatedC1)
    end.

-compile({inline, ins_rebalance_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS2}).
ins_rebalance_INTERNAL3_C2(Result, Elem, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C2, Pos, Args, E1, C1) of
                {UpElem, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL3(
                        UpElem,
                        E2,
                        E3,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3,
                        C4
                    );
                %
                {split, SplitE, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC2 ->
            ?INTERNAL3_UPD_C2(UpdatedC2)
    end.

-compile({inline, ins_rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS2}).
ins_rebalance_INTERNAL3_C3(Result, Elem, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C3, Pos, Args, E2, C2) of
                {UpElem, UpdatedC2, UpdatedC3} ->
                    ?new_INTERNAL3(
                        E1,
                        UpElem,
                        E3,
                        %
                        C1,
                        UpdatedC2,
                        UpdatedC3,
                        C4
                    );
                %
                {split, SplitE, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC3 ->
            ?INTERNAL3_UPD_C3(UpdatedC3)
    end.

-compile({inline, ins_rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS2}).
ins_rebalance_INTERNAL3_C4(Result, Elem, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C4, Pos, Args, E3, C3) of
                {UpElem, UpdatedC3, UpdatedC4} ->
                    ?new_INTERNAL3(
                        E1,
                        E2,
                        UpElem,
                        %
                        C1,
                        C2,
                        UpdatedC3,
                        UpdatedC4
                    );
                %
                {split, SplitE, SplitL, SplitR} ->
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
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC4 ->
            ?INTERNAL3_UPD_C4(UpdatedC4)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing INTERNAL2
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS2}).
ins_rebalance_INTERNAL2_C1(Result, Elem, ?INTERNAL2_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_right_sibling_maybe(Elem, C1, Pos, Args, E1, C2) of
                {UpElem, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL2(
                        UpElem,
                        E2,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3
                    );
                %
                {split, SplitE, SplitL, SplitR} ->
                    ?new_INTERNAL3(
                        SplitE,
                        E1,
                        E2,
                        %
                        SplitL,
                        SplitR,
                        C2,
                        C3
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC1 ->
            ?INTERNAL2_UPD_C1(UpdatedC1)
    end.

-compile({inline, ins_rebalance_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS2}).
ins_rebalance_INTERNAL2_C2(Result, Elem, ?INTERNAL2_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C2, Pos, Args, E1, C1) of
                {UpElem, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL2(
                        UpElem,
                        E2,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3
                    );
                %
                {split, SplitE, SplitL, SplitR} ->
                    ?new_INTERNAL3(
                        E1,
                        SplitE,
                        E2,
                        %
                        C1,
                        SplitL,
                        SplitR,
                        C3
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC2 ->
            ?INTERNAL2_UPD_C2(UpdatedC2)
    end.

-compile({inline, ins_rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS2}).
ins_rebalance_INTERNAL2_C3(Result, Elem, ?INTERNAL2_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C3, Pos, Args, E2, C2) of
                {UpElem, UpdatedC2, UpdatedC3} ->
                    ?new_INTERNAL2(
                        E1,
                        UpElem,
                        %
                        C1,
                        UpdatedC2,
                        UpdatedC3
                    );
                %
                {split, SplitE, SplitL, SplitR} ->
                    ?new_INTERNAL3(
                        E1,
                        E2,
                        SplitE,
                        %
                        C1,
                        C2,
                        SplitL,
                        SplitR
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC3 ->
            ?INTERNAL2_UPD_C3(UpdatedC3)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing INTERNAL1
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS2}).
ins_rebalance_INTERNAL1_C1(Result, Elem, ?INTERNAL1_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_right_sibling_maybe(Elem, C1, Pos, Args, E1, C2) of
                {UpElem, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL1(UpElem, UpdatedC1, UpdatedC2);
                %
                {split, SplitE, SplitL, SplitR} ->
                    ?new_INTERNAL2(
                        SplitE,
                        E1,
                        %
                        SplitL,
                        SplitR,
                        C2
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC1 ->
            ?new_INTERNAL1(E1, UpdatedC1, C2)
    end.

-compile({inline, ins_rebalance_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS2}).
ins_rebalance_INTERNAL1_C2(Result, Elem, ?INTERNAL1_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C2, Pos, Args, E1, C1) of
                {UpElem, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL1(UpElem, UpdatedC1, UpdatedC2);
                %
                {split, SplitE, SplitL, SplitR} ->
                    ?new_INTERNAL2(
                        E1,
                        SplitE,
                        %
                        C1,
                        SplitL,
                        SplitR
                    )
            end;
        %
        key_exists ->
            key_exists;
        %
        UpdatedC2 ->
            ?new_INTERNAL1(E1, C1, UpdatedC2)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Node Split
%% ------------------------------------------------------------------

-compile({inline, insert_split_root/4}).
insert_split_root(NewElem, Pos, [], Root) ->
    ?LEAF4_MATCH_ALL = Root,

    case Pos of
        1 ->
            ?new_INTERNAL1(
                E2,
                ?new_LEAF2(NewElem, E1),
                ?new_LEAF2(E3, E4)
            );
        %
        2 ->
            ?new_INTERNAL1(
                E2,
                ?new_LEAF2(E1, NewElem),
                ?new_LEAF2(E3, E4)
            );
        %
        3 ->
            ?new_INTERNAL1(
                NewElem,
                ?new_LEAF2(E1, E2),
                ?new_LEAF2(E3, E4)
            );
        %
        4 ->
            ?new_INTERNAL1(
                E3,
                ?new_LEAF2(E1, E2),
                ?new_LEAF2(NewElem, E4)
            );
        %
        5 ->
            ?new_INTERNAL1(
                E3,
                ?new_LEAF2(E1, E2),
                ?new_LEAF2(E4, NewElem)
            )
    end;
insert_split_root(_NewElem, Pos, {split, SplitE, SplitL, SplitR}, Root) ->
    ?INTERNAL4_MATCH_ALL = Root,

    case Pos of
        1 ->
            insert_split_root_internal(
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
        2 ->
            insert_split_root_internal(
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
        3 ->
            insert_split_root_internal(
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
        4 ->
            insert_split_root_internal(
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
        5 ->
            insert_split_root_internal(
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
            )
    end.

-spec insert_split_root_internal(
    E,
    E,
    E,
    E,
    E,
    %
    C,
    C,
    C,
    C,
    C,
    C
) -> node_INTERNAL1(E) when
    C :: nonempty_node(E).
-compile({inline, insert_split_root_internal/11}).
insert_split_root_internal(
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

    SplitL = ?new_INTERNAL2(
        E1,
        E2,
        %
        C1,
        C2,
        C3
    ),

    SplitR = ?new_INTERNAL2(
        E4,
        E5,
        %
        C4,
        C5,
        C6
    ),

    ?new_INTERNAL1(SplitE, SplitL, SplitR).

-compile({inline, ins_rebalance_split_leaf/3}).
ins_rebalance_split_leaf(?LEAF4_MATCH_ALL, Pos, NewElem) ->
    case Pos of
        1 ->
            split_leaf(
                NewElem,
                E1,
                E2,
                E3,
                E4
            );
        %
        2 ->
            split_leaf(
                E1,
                NewElem,
                E2,
                E3,
                E4
            );
        %
        3 ->
            split_leaf(
                E1,
                E2,
                NewElem,
                E3,
                E4
            );
        %
        4 ->
            split_leaf(
                E1,
                E2,
                E3,
                NewElem,
                E4
            );
        %
        5 ->
            split_leaf(
                E1,
                E2,
                E3,
                E4,
                NewElem
            )
    end.

-spec split_internal(
    E,
    E,
    E,
    E,
    E,
    %
    C,
    C,
    C,
    C,
    C,
    C
) -> split_internal_result(E) when
    C :: nonempty_node(E).
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

    {split, SplitE, SplitL, SplitR}.

-spec split_leaf(
    E,
    E,
    E,
    E,
    E
) -> split_leaf_result(E).
-compile({inline, split_leaf/5}).
split_leaf(
    E1,
    E2,
    E3,
    E4,
    E5
) ->
    SplitE = E3,

    SplitL = ?new_LEAF2(E1, E2),
    SplitR = ?new_LEAF2(E4, E5),

    {split, SplitE, SplitL, SplitR}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing from left sibling
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_into_left_sibling_maybe/6}).
ins_rebalance_into_left_sibling_maybe(NewElem, Node, Pos, Args, ParentE, Left) ->
    case Args of
        {split, SplitE, SplitL, SplitR} ->
            ins_rebalance_into_left_internal_maybe(
                Node, Pos, SplitE, SplitL, SplitR, ParentE, Left
            );
        %
        [] ->
            ins_rebalance_into_left_leaf_maybe(Node, Pos, NewElem, ParentE, Left)
    end.

-compile({inline, ins_rebalance_into_left_internal_maybe/7}).
ins_rebalance_into_left_internal_maybe(
    ?INTERNAL4_MATCH_ALL, Pos, SplitE, SplitL, SplitR, ParentE, Left
) ->
    case Pos of
        1 ->
            ins_rebalance_into_left_internal(
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
                C5,
                %
                ParentE,
                Left
            );
        %
        2 ->
            ins_rebalance_into_left_internal(
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
                C5,
                %
                ParentE,
                Left
            );
        %
        3 ->
            ins_rebalance_into_left_internal(
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
                C5,
                %
                ParentE,
                Left
            );
        %
        4 ->
            ins_rebalance_into_left_internal(
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
                C5,
                %
                ParentE,
                Left
            );
        %
        5 ->
            ins_rebalance_into_left_internal(
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
                SplitR,
                %
                ParentE,
                Left
            )
    end.

-compile({inline, ins_rebalance_into_left_internal/13}).
ins_rebalance_into_left_internal(
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
    C6,
    %
    ParentE,
    Left
) ->
    case Left of
        ?INTERNAL2_MATCH(
            LE1,
            LE2,
            %
            LC1,
            LC2,
            LC3
        ) ->
            UpElem = E1,

            UpdatedLeft = ?new_INTERNAL3(
                LE1,
                LE2,
                ParentE,
                %
                LC1,
                LC2,
                LC3,
                C1
            ),

            UpdatedNode = ?new_INTERNAL4(
                E2,
                E3,
                E4,
                E5,
                %
                C2,
                C3,
                C4,
                C5,
                C6
            ),

            {UpElem, UpdatedLeft, UpdatedNode};
        %
        %
        %
        %
        _ ->
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
            )
    end.

-compile({inline, ins_rebalance_into_left_leaf_maybe/5}).
ins_rebalance_into_left_leaf_maybe(Node, Pos, NewElem, ParentE, Left) ->
    case Left of
        ?LEAF2_MATCH(LE1, LE2) ->
            UpdatedLeft = ?new_LEAF3(LE1, LE2, ParentE),
            ins_rebalance_into_left_leaf(Node, Pos, NewElem, UpdatedLeft);
        %
        _ ->
            ins_rebalance_split_leaf(Node, Pos, NewElem)
    end.

-compile({inline, ins_rebalance_into_left_leaf/4}).
ins_rebalance_into_left_leaf(?LEAF4_MATCH_ALL = Node, Pos, NewElem, UpdatedLeft) ->
    case Pos of
        1 ->
            UpElem = NewElem,
            UpdatedNode = Node,
            {UpElem, UpdatedLeft, UpdatedNode};
        %
        2 ->
            UpElem = E1,
            UpdatedNode = ?new_LEAF4(NewElem, E2, E3, E4),
            {UpElem, UpdatedLeft, UpdatedNode};
        %
        3 ->
            UpElem = E1,
            UpdatedNode = ?new_LEAF4(E2, NewElem, E3, E4),
            {UpElem, UpdatedLeft, UpdatedNode};
        %
        4 ->
            UpElem = E1,
            UpdatedNode = ?new_LEAF4(E2, E3, NewElem, E4),
            {UpElem, UpdatedLeft, UpdatedNode};
        %
        5 ->
            UpElem = E1,
            UpdatedNode = ?new_LEAF4(E2, E3, E4, NewElem),
            {UpElem, UpdatedLeft, UpdatedNode}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing from right sibling
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_into_right_sibling_maybe/6}).
ins_rebalance_into_right_sibling_maybe(NewElem, Node, Pos, Args, ParentE, Right) ->
    case Args of
        {split, SplitE, SplitL, SplitR} ->
            ins_rebalance_into_right_internal_maybe(
                Node, Pos, SplitE, SplitL, SplitR, ParentE, Right
            );
        %
        [] ->
            ins_rebalance_into_right_leaf_maybe(
                Node, Pos, NewElem, ParentE, Right
            )
    end.

-compile({inline, ins_rebalance_into_right_internal_maybe/7}).
ins_rebalance_into_right_internal_maybe(
    ?INTERNAL4_MATCH_ALL, Pos, SplitE, SplitL, SplitR, ParentE, Right
) ->
    case Pos of
        1 ->
            ins_rebalance_into_right_internal(
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
                C5,
                %
                ParentE,
                Right
            );
        %
        2 ->
            ins_rebalance_into_right_internal(
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
                C5,
                %
                ParentE,
                Right
            );
        %
        3 ->
            ins_rebalance_into_right_internal(
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
                C5,
                %
                ParentE,
                Right
            );
        %
        4 ->
            ins_rebalance_into_right_internal(
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
                C5,
                %
                ParentE,
                Right
            );
        %
        5 ->
            ins_rebalance_into_right_internal(
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
                SplitR,
                %
                ParentE,
                Right
            )
    end.

-compile({inline, ins_rebalance_into_right_internal/13}).
ins_rebalance_into_right_internal(
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
    C6,
    %
    ParentE,
    Right
) ->
    case Right of
        ?INTERNAL2_MATCH(
            RE1,
            RE2,
            %
            RC1,
            RC2,
            RC3
        ) ->
            UpElem = E5,

            UpdatedNode = ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                C1,
                C2,
                C3,
                C4,
                C5
            ),

            UpdatedRight = ?new_INTERNAL3(
                ParentE,
                RE1,
                RE2,
                %
                C6,
                RC1,
                RC2,
                RC3
            ),

            {UpElem, UpdatedNode, UpdatedRight};
        %
        %
        %
        %
        _ ->
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
            )
    end.

-compile({inline, ins_rebalance_into_right_leaf_maybe/5}).
ins_rebalance_into_right_leaf_maybe(Node, Pos, NewElem, ParentE, Right) ->
    case Right of
        ?LEAF2_MATCH(RE1, RE2) ->
            UpdatedRight = ?new_LEAF3(ParentE, RE1, RE2),
            ins_rebalance_into_right_leaf(Node, Pos, NewElem, UpdatedRight);
        %
        _ ->
            ins_rebalance_split_leaf(Node, Pos, NewElem)
    end.

-compile({inline, ins_rebalance_into_right_leaf/4}).
ins_rebalance_into_right_leaf(?LEAF4_MATCH_ALL = Node, Pos, NewElem, UpdatedRight) ->
    case Pos of
        1 ->
            UpElem = E4,
            UpdatedNode = ?new_LEAF4(NewElem, E1, E2, E3),
            {UpElem, UpdatedNode, UpdatedRight};
        %
        2 ->
            UpElem = E4,
            UpdatedNode = ?new_LEAF4(E1, NewElem, E2, E3),
            {UpElem, UpdatedNode, UpdatedRight};
        %
        3 ->
            UpElem = E4,
            UpdatedNode = ?new_LEAF4(E1, E2, NewElem, E3),
            {UpElem, UpdatedNode, UpdatedRight};
        %
        4 ->
            UpElem = E4,
            UpdatedNode = ?new_LEAF4(E1, E2, E3, NewElem),
            {UpElem, UpdatedNode, UpdatedRight};
        %
        5 ->
            UpElem = NewElem,
            UpdatedNode = Node,
            {UpElem, UpdatedNode, UpdatedRight}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Deletion Rebalance handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------
%% Internal Function Definitions: Deletion - Rebalancing INTERNAL4
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, del_rebalance_INTERNAL4_C1 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C1(?INTERNAL4_ARGS) ->
    case del_rebalance_maybe_from_right_sibling(C1, E1, C2) of
        balanced ->
            ?INTERNAL4_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL4(
                UpElem,
                E2,
                E3,
                E4,
                %
                UpdatedC1,
                UpdatedC2,
                C3,
                C4,
                C5
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL3(
                E2,
                E3,
                E4,
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

-compile({inline, del_rebalance_INTERNAL4_C2 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C2(?INTERNAL4_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C2,
            %
            E1,
            C1
        )
    of
        balanced ->
            ?INTERNAL4_UPD_C2(C2);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC1, RebalancedC2) ->
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
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL3(
                E2,
                E3,
                E4,
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

-compile({inline, del_rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C3(?INTERNAL4_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C3,
            %
            E2,
            C2
        )
    of
        balanced ->
            ?INTERNAL4_UPD_C3(C3);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC2, RebalancedC3) ->
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
            );
        %
        ?MERGED(MergedC2C3) ->
            ?new_INTERNAL3(
                E1,
                E3,
                E4,
                %
                C1,
                MergedC2C3,
                C4,
                C5
            )
    end.

%%
%% C4
%%

-compile({inline, del_rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C4(?INTERNAL4_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C4,
            %
            E3,
            C3
        )
    of
        balanced ->
            ?INTERNAL4_UPD_C4(C4);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC3, RebalancedC4) ->
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
            );
        %
        ?MERGED(MergedC3C4) ->
            ?new_INTERNAL3(
                E1,
                E2,
                E4,
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

-compile({inline, del_rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C5(?INTERNAL4_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C5, E4, C4) of
        balanced ->
            ?INTERNAL4_UPD_C5(C5);
        %
        badkey ->
            badkey;
        %
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
%% Internal Function Definitions: Deletion - Rebalancing INTERNAL3
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, del_rebalance_INTERNAL3_C1 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C1(?INTERNAL3_ARGS) ->
    case del_rebalance_maybe_from_right_sibling(C1, E1, C2) of
        balanced ->
            ?INTERNAL3_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL3(
                UpElem,
                E2,
                E3,
                %
                UpdatedC1,
                UpdatedC2,
                C3,
                C4
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL2(
                E2,
                E3,
                %
                MergedC1C2,
                C3,
                C4
            )
    end.

%%
%% C2
%%

-compile({inline, del_rebalance_INTERNAL3_C2 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C2(?INTERNAL3_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C2,
            %
            E1,
            C1
        )
    of
        balanced ->
            ?INTERNAL3_UPD_C2(C2);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL3(
                UpElem,
                E2,
                E3,
                %
                UpdatedC1,
                RebalancedC2,
                C3,
                C4
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL2(
                E2,
                E3,
                %
                MergedC1C2,
                C3,
                C4
            )
    end.

%%
%% C3
%%

-compile({inline, del_rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C3(?INTERNAL3_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C3,
            %
            E2,
            C2
        )
    of
        balanced ->
            ?INTERNAL3_UPD_C3(C3);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC2, RebalancedC3) ->
            ?new_INTERNAL3(
                E1,
                UpElem,
                E3,
                %
                C1,
                UpdatedC2,
                RebalancedC3,
                C4
            );
        %
        ?MERGED(MergedC2C3) ->
            ?new_INTERNAL2(
                E1,
                E3,
                %
                C1,
                MergedC2C3,
                C4
            )
    end.

%%
%% C4
%%

-compile({inline, del_rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C4(?INTERNAL3_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C4, E3, C3) of
        balanced ->
            ?INTERNAL3_UPD_C4(C4);
        %
        badkey ->
            badkey;
        %
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
%% Internal Function Definitions: Deletion - Rebalancing INTERNAL2
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, del_rebalance_INTERNAL2_C1 / ?INTERNAL2_ARITY}).
del_rebalance_INTERNAL2_C1(?INTERNAL2_ARGS) ->
    case del_rebalance_maybe_from_right_sibling(C1, E1, C2) of
        balanced ->
            ?INTERNAL2_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL2(
                UpElem,
                E2,
                %
                UpdatedC1,
                UpdatedC2,
                C3
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL1(E2, MergedC1C2, C3)
    end.

%%
%% C2
%%

-compile({inline, del_rebalance_INTERNAL2_C2 / ?INTERNAL2_ARITY}).
del_rebalance_INTERNAL2_C2(?INTERNAL2_ARGS) ->
    case
        del_rebalance_maybe_from_left_sibling(
            C2,
            %
            E1,
            C1
        )
    of
        balanced ->
            ?INTERNAL2_UPD_C2(C2);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC1, RebalancedC2) ->
            ?new_INTERNAL2(
                UpElem,
                E2,
                %
                UpdatedC1,
                RebalancedC2,
                C3
            );
        %
        ?MERGED(MergedC1C2) ->
            ?new_INTERNAL1(
                E2,
                %
                MergedC1C2,
                C3
            )
    end.

%%
%% C3
%%

-compile({inline, del_rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY}).
del_rebalance_INTERNAL2_C3(?INTERNAL2_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C3, E2, C2) of
        balanced ->
            ?INTERNAL2_UPD_C3(C3);
        %
        badkey ->
            badkey;
        %
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
%% Internal Function Definitions: Deletion - Rebalancing INTERNAL1
%% ------------------------------------------------------------------

%%
%% C1
%%

-compile({inline, del_rebalance_INTERNAL1_C1 / ?INTERNAL1_ARITY}).
del_rebalance_INTERNAL1_C1(?INTERNAL1_ARGS) ->
    case del_rebalance_maybe_from_right_sibling(C1, E1, C2) of
        balanced ->
            ?INTERNAL1_UPD_C1(C1);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL1(UpElem, UpdatedC1, UpdatedC2);
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen at root - height is reduced
            ?CHECK_NODE(MergedC1C2)
    end.

%%
%% C2
%%

-compile({inline, del_rebalance_INTERNAL1_C2 / ?INTERNAL1_ARITY}).
del_rebalance_INTERNAL1_C2(?INTERNAL1_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C2, E1, C1) of
        balanced ->
            ?INTERNAL1_UPD_C2(C2);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL1(UpElem, UpdatedC1, UpdatedC2);
        %
        ?MERGED(MergedC1C2) ->
            % Can only happen at root - height is reduced
            ?CHECK_NODE(MergedC1C2)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Deletion - Rebalance a node from its right sibling
%% ------------------------------------------------------------------

-compile({inline, del_rebalance_maybe_from_right_sibling/3}).
del_rebalance_maybe_from_right_sibling(Child, RParentE, Right) ->
    case Child of
        ?INTERNAL1_MATCH(CElem, CLeft, CRight) ->
            del_rebalance_internal_from_right_sibling(
                CElem,
                CLeft,
                CRight,
                %
                RParentE,
                Right
            );
        %
        ?LEAF1_MATCH(CElem) ->
            del_rebalance_leaf_from_right_sibling(
                CElem,

                %
                RParentE,
                Right
            );
        %
        badkey ->
            badkey;
        %
        _ ->
            balanced
    end.

%-compile({inline, del_rebalance_internal_from_right_sibling/7}).
del_rebalance_internal_from_right_sibling(
    CElem,
    CLeft,
    CRight,
    %
    RParentE,
    Right
) ->
    case Right of
        ?INTERNAL2_MATCH_ALL ->
            MergedNode = ?new_INTERNAL4(
                CElem,
                RParentE,
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

            ?ROTATED(UpElem, UpdatedNode, UpdatedRight);
        %
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

            ?ROTATED(UpElem, UpdatedNode, UpdatedRight)
        %
        %
    end.

%-compile({inline, del_rebalance_leaf_from_right_sibling/5}).
del_rebalance_leaf_from_right_sibling(CElem, RParentE, Right) ->
    case Right of
        ?LEAF2_MATCH_ALL ->
            MergedNode = ?new_LEAF4(
                CElem,
                RParentE,
                E1,
                E2
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpElem = E1,

            UpdatedNode = ?new_LEAF2(CElem, RParentE),
            UpdatedRight = ?new_LEAF2(E2, E3),

            ?ROTATED(UpElem, UpdatedNode, UpdatedRight);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E1,

            UpdatedNode = ?new_LEAF2(CElem, RParentE),
            UpdatedRight = ?new_LEAF3(E2, E3, E4),

            ?ROTATED(UpElem, UpdatedNode, UpdatedRight)
        %
        %
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Deletion - Rebalance a node from its left sibling
%% ------------------------------------------------------------------

-compile({inline, del_rebalance_maybe_from_left_sibling/3}).
del_rebalance_maybe_from_left_sibling(Child, LParentE, Left) ->
    case Child of
        ?INTERNAL1_MATCH(CElem, CLeft, CRight) ->
            del_rebalance_internal_from_left_sibling(
                CElem,

                CLeft,
                CRight,
                %
                LParentE,
                Left
            );
        %
        ?LEAF1_MATCH(CElem) ->
            del_rebalance_leaf_from_left_sibling(
                CElem,

                %
                LParentE,
                Left
            );
        %
        badkey ->
            badkey;
        %
        _ ->
            balanced
    end.

%-compile({inline, del_rebalance_internal_from_left_sibling/7}).
del_rebalance_internal_from_left_sibling(
    CElem,

    CLeft,
    CRight,
    %
    LParentE,
    Left
) ->
    case Left of
        ?INTERNAL2_MATCH_ALL ->
            MergedNode = ?new_INTERNAL4(
                E1,
                E2,
                LParentE,
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

            ?ROTATED(UpElem, UpdatedLeft, UpdatedNode);
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

            ?ROTATED(UpElem, UpdatedLeft, UpdatedNode)
        %
        %
    end.

%-compile({inline, del_rebalance_leaf_from_left_sibling/5}).
del_rebalance_leaf_from_left_sibling(
    CElem,

    LParentE,
    Left
) ->
    case Left of
        ?LEAF2_MATCH_ALL ->
            MergedNode = ?new_LEAF4(
                E1,
                E2,
                LParentE,
                CElem
            ),

            MergedNode;
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpElem = E3,

            UpdatedNode = ?new_LEAF2(LParentE, CElem),
            UpdatedLeft = ?new_LEAF2(E1, E2),

            ?ROTATED(UpElem, UpdatedLeft, UpdatedNode);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E4,

            UpdatedNode = ?new_LEAF2(LParentE, CElem),
            UpdatedLeft = ?new_LEAF3(E1, E2, E3),

            ?ROTATED(UpElem, UpdatedLeft, UpdatedNode)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------
%% Internal Function Definitions: Node Well-Formedness Checks
%% ------------------------------------------------------------------

-if(?NODE_CHECK_ENABLED).

check_node(LineNumber, Node, Variant) ->
    try do_check_node(LineNumber, Node, Variant) of
        ok ->
            Node
    catch
        error:{bad_node, _} = Reason:Stacktrace ->
            erlang:raise(error, Reason, Stacktrace);
        %
        Class:Reason:Stacktrace ->
            fail_node_check(LineNumber, unknown, Node, {Class, Reason, Stacktrace})
    end.

do_check_node(LineNumber, Node, Variant) ->
    Type = node_type(Node, Variant),
    check_node_elements(LineNumber, Type, Node),
    ok.

node_type(Node, top) ->
    node_type(Node);
node_type(Node, recur) ->
    recur_node_type(Node).

check_node_elements(LineNumber, Type, Node) ->
    List = to_list(Node),
    MissortedElements = check_node_elements(List),

    case MissortedElements of
        [] ->
            Node;
        %
        [_ | _] ->
            fail_node_check(LineNumber, Type, Node, {missorted_elements, MissortedElements})
    end.

node_type(Node) ->
    case Node of
        ?INTERNAL1(_, _, _) ->
            'INTERNAL1';
        %
        ?LEAF1(_) ->
            'LEAF1';
        %
        _ ->
            recur_node_type(Node)
    end.

recur_node_type(Node) ->
    case Node of
        ?INTERNAL2(_, _, _, _, _) ->
            'INTERNAL2';
        %
        ?INTERNAL3(_, _, _, _, _, _, _) ->
            'INTERNAL3';
        %
        ?INTERNAL4(_, _, _, _, _, _, _, _, _) ->
            'INTERNAL4';
        %
        ?LEAF2(_, _) ->
            'LEAF2';
        %
        ?LEAF3(_, _, _) ->
            'LEAF3';
        %
        ?LEAF4(_, _, _, _) ->
            'LEAF4'
    end.

fail_node_check(LineNumber, Type, Node, Reason) ->
    error(
        {bad_node, [
            {line, LineNumber},
            {type, Type},
            {reason, Reason},
            {node, Node}
        ]}
    ).

check_node_elements([H | T]) ->
    check_node_elements(H, T);
check_node_elements([]) ->
    [].

check_node_elements(E1, [E2 | Next]) ->
    case E1 >= E2 of
        false ->
            check_node_elements(E2, Next);
        %
        true ->
            [{E1, E2} | check_node_elements(E2, Next)]
    end;
check_node_elements(_, []) ->
    [].

% -if(?NODE_CHECK_ENABLED).
-endif.
