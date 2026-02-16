-module(xb5_bag_node).

-moduledoc """
API for operating over `m:xb5_bag` internal nodes directly.

> ℹ️
> You're likely looking for `m:xb5_bag`.

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

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    add/2,
    delete_att/2,
    filtermap/2,
    fold/3,
    from_ordered_list/2,
    insert_att/2,
    is_member/2,
    iterator/2,
    iterator_from/3,
    larger/2,
    largest/1,
    map/2,
    merge/4,
    new/0,
    next/1,
    nth/2,
    nth_and_nthp1/2,
    rank/2,
    rank_larger/2,
    rank_smaller/2,
    smaller/2,
    smallest/1,
    structural_stats/1,
    take_largest/1,
    take_smallest/1,
    to_list/1
]).

%% ------------------------------------------------------------------
%% Macro Definitions: Nodes
%% ------------------------------------------------------------------

% 7 elements
-define(INTERNAL2(E1, E2, O1, O2, C1, C2, C3), {E1, E2, O1, O2, C1, C2, C3}).
-define(INTERNAL2_MATCH(E1, E2, O1, O2, C1, C2, C3), {E1, E2, O1, O2, C1, C2, C3}).
-define(INTERNAL2_MATCH_ALL, {E1, E2, O1, O2, C1, C2, C3}).
-define(INTERNAL2_MATCH_NO_SIZES, {E1, E2, _, _, C1, C2, C3}).

% 2 elements
-define(LEAF2(E1, E2), {E1, E2}).
-define(LEAF2_MATCH(E1, E2), {E1, E2}).
-define(LEAF2_MATCH_ALL, {E1, E2}).

% 10 elements
-define(INTERNAL3(E1, E2, E3, O1, O2, O3, C1, C2, C3, C4),
    {E1, E2, E3, O1, O2, O3, C1, C2, C3, C4}
).
-define(INTERNAL3_MATCH(E1, E2, E3, O1, O2, O3, C1, C2, C3, C4),
    {E1, E2, E3, O1, O2, O3, C1, C2, C3, C4}
).
-define(INTERNAL3_MATCH_ALL, {E1, E2, E3, O1, O2, O3, C1, C2, C3, C4}).
-define(INTERNAL3_MATCH_NO_SIZES, {E1, E2, E3, _, _, _, C1, C2, C3, C4}).

% 3 elements
-define(LEAF3(E1, E2, E3), {E1, E2, E3}).
-define(LEAF3_MATCH(E1, E2, E3), {E1, E2, E3}).
-define(LEAF3_MATCH_ALL, {E1, E2, E3}).

% 13 elements
-define(INTERNAL4(E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5),
    {E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5}
).
-define(INTERNAL4_MATCH(E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5),
    {E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5}
).
-define(INTERNAL4_MATCH_ALL,
    {E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5}
).
-define(INTERNAL4_MATCH_NO_SIZES,
    {E1, E2, E3, E4, _, _, _, _, C1, C2, C3, C4, C5}
).

% 4 elements
-define(LEAF4(E1, E2, E3, E4), {E1, E2, E3, E4}).
-define(LEAF4_MATCH(E1, E2, E3, E4), {E1, E2, E3, E4}).
-define(LEAF4_MATCH_ALL, {E1, E2, E3, E4}).

% 5 elements
-define(INTERNAL1(E1, O1, C1, C2), {internal1, E1, O1, C1, C2}).
-define(INTERNAL1_MATCH(E1, O1, C1, C2), {_, E1, O1, C1, C2}).
-define(INTERNAL1_MATCH_ALL, {_, E1, O1, C1, C2}).
-define(INTERNAL1_MATCH_NO_SIZES, {_, E1, _, C1, C2}).

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

% 4 elements
-define(ROTATED(UpElem, MovedSize, UpdatedLeft, UpdatedRight),
    {UpElem, MovedSize, UpdatedLeft, UpdatedRight}
).

-define(MERGED(MergedNode), ([MergedNode])).
-define(MERGED_MATCH(MergedNode), ([MergedNode | _])).

%%%%%%%

-define(ITER_ELEM(Elem), [Elem]).
-define(REV_ITER_TAG, reversed).

%% ------------------------------------------------------------------
%% Macro Definitions: Boilerplate Helpers
%% ------------------------------------------------------------------

%% ?INTERNAL4

-define(INTERNAL4_ARGS, E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARITY, 13).
-define(INTERNAL4_ARITY_PLUS1, 14).
-define(INTERNAL4_ARITY_PLUS2, 15).

-define(INTERNAL4_ARGS_EXCEPT_SIZES, E1, E2, E3, E4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARITY_EXCEPT_SIZES_PLUS1, 10).
-define(INTERNAL4_ARITY_EXCEPT_SIZES_PLUS2, 11).

-define(INTERNAL4_UPD_C5(UpdatedC5),
    ?new_INTERNAL4(E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, UpdatedC5)
).

-define(INTERNAL4_C1_REBALANCE(UpdatedC1),
    del_rebalance_INTERNAL4_C1(E1, E2, E3, E4, O1, O2, O3, O4, UpdatedC1, C2, C3, C4, C5)
).
-define(INTERNAL4_C2_REBALANCE(UpdatedC2),
    del_rebalance_INTERNAL4_C2(E1, E2, E3, E4, O1, O2, O3, O4, C1, UpdatedC2, C3, C4, C5)
).
-define(INTERNAL4_C3_REBALANCE(UpdatedC3),
    del_rebalance_INTERNAL4_C3(E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, UpdatedC3, C4, C5)
).
-define(INTERNAL4_C4_REBALANCE(UpdatedC4),
    del_rebalance_INTERNAL4_C4(E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, UpdatedC4, C5)
).
-define(INTERNAL4_C5_REBALANCE(UpdatedC5),
    del_rebalance_INTERNAL4_C5(E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, UpdatedC5)
).

-define(INTERNAL4_ARGS_IGN_E1, _, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_E2, E1, _, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_E3, E1, E2, _, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5).
-define(INTERNAL4_ARGS_IGN_E4, E1, E2, E3, _, O1, O2, O3, O4, C1, C2, C3, C4, C5).

%% ?INTERNAL3

-define(INTERNAL3_ARGS, E1, E2, E3, O1, O2, O3, C1, C2, C3, C4).
-define(INTERNAL3_ARITY, 10).
-define(INTERNAL3_ARITY_PLUS1, 11).
-define(INTERNAL3_ARITY_PLUS2, 12).

-define(INTERNAL3_ARGS_EXCEPT_SIZES, E1, E2, E3, C1, C2, C3, C4).
-define(INTERNAL3_ARITY_EXCEPT_SIZES_PLUS1, 8).
-define(INTERNAL3_ARITY_EXCEPT_SIZES_PLUS2, 9).

-define(INTERNAL3_UPD_C4(UpdatedC4), ?new_INTERNAL3(E1, E2, E3, O1, O2, O3, C1, C2, C3, UpdatedC4)).

-define(INTERNAL3_C1_REBALANCE(UpdatedC1),
    del_rebalance_INTERNAL3_C1(E1, E2, E3, O1, O2, O3, UpdatedC1, C2, C3, C4)
).
-define(INTERNAL3_C2_REBALANCE(UpdatedC2),
    del_rebalance_INTERNAL3_C2(E1, E2, E3, O1, O2, O3, C1, UpdatedC2, C3, C4)
).
-define(INTERNAL3_C3_REBALANCE(UpdatedC3),
    del_rebalance_INTERNAL3_C3(E1, E2, E3, O1, O2, O3, C1, C2, UpdatedC3, C4)
).
-define(INTERNAL3_C4_REBALANCE(UpdatedC4),
    del_rebalance_INTERNAL3_C4(E1, E2, E3, O1, O2, O3, C1, C2, C3, UpdatedC4)
).

-define(INTERNAL3_ARGS_IGN_E1, _, E2, E3, O1, O2, O3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_E2, E1, _, E3, O1, O2, O3, C1, C2, C3, C4).
-define(INTERNAL3_ARGS_IGN_E3, E1, E2, _, O1, O2, O3, C1, C2, C3, C4).

%% ?INTERNAL2

-define(INTERNAL2_ARGS, E1, E2, O1, O2, C1, C2, C3).
-define(INTERNAL2_ARITY, 7).
-define(INTERNAL2_ARITY_PLUS1, 8).
-define(INTERNAL2_ARITY_PLUS2, 9).

-define(INTERNAL2_ARGS_EXCEPT_SIZES, E1, E2, C1, C2, C3).
-define(INTERNAL2_ARITY_EXCEPT_SIZES_PLUS1, 6).
-define(INTERNAL2_ARITY_EXCEPT_SIZES_PLUS2, 7).

-define(INTERNAL2_UPD_C3(UpdatedC3), ?new_INTERNAL2(E1, E2, O1, O2, C1, C2, UpdatedC3)).

-define(INTERNAL2_C1_REBALANCE(UpdatedC1),
    del_rebalance_INTERNAL2_C1(E1, E2, O1, O2, UpdatedC1, C2, C3)
).
-define(INTERNAL2_C2_REBALANCE(UpdatedC2),
    del_rebalance_INTERNAL2_C2(E1, E2, O1, O2, C1, UpdatedC2, C3)
).
-define(INTERNAL2_C3_REBALANCE(UpdatedC3),
    del_rebalance_INTERNAL2_C3(E1, E2, O1, O2, C1, C2, UpdatedC3)
).

-define(INTERNAL2_ARGS_IGN_E1, _, E2, O1, O2, C1, C2, C3).
-define(INTERNAL2_ARGS_IGN_E2, E1, _, O1, O2, C1, C2, C3).

%% ?INTERNAL1

-define(INTERNAL1_ARGS, E1, O1, C1, C2).
-define(INTERNAL1_ARITY, 4).
-define(INTERNAL1_ARITY_PLUS1, 5).
-define(INTERNAL1_ARITY_PLUS2, 6).

-define(INTERNAL1_ARGS_EXCEPT_SIZES, E1, C1, C2).
-define(INTERNAL1_ARITY_EXCEPT_SIZES_PLUS1, 4).

-define(INTERNAL1_UPD_C2(UpdatedC2), ?new_INTERNAL1(E1, O1, C1, UpdatedC2)).

-define(INTERNAL1_C1_REBALANCE(UpdatedC1), del_rebalance_INTERNAL1_C1(E1, O1, UpdatedC1, C2)).
-define(INTERNAL1_C2_REBALANCE(UpdatedC2), del_rebalance_INTERNAL1_C2(E1, O1, C1, UpdatedC2)).

-define(INTERNAL1_E1_C2_REBALANCE(ReplacementE, UpdatedC2),
    del_rebalance_INTERNAL1_C2(ReplacementE, O1, C1, UpdatedC2)
).

-define(INTERNAL1_ARGS_IGN_E1, _, O1, C1, C2).

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

-define(TAKEN(Elem, UpdatedNode), [Elem | UpdatedNode]).

%%

% defined(TEST)).
-define(NODE_CHECK_ENABLED, false).

-if(?NODE_CHECK_ENABLED).

-include_lib("stdlib/include/assert.hrl").

-define(CHECK_NODE(Node), check_node(?LINE, Node, top)).
-define(CHECK_NODE_RECUR(Node), check_node(?LINE, Node, recur)).

-else.
-define(CHECK_NODE(Node), Node).
-define(CHECK_NODE_RECUR(Node), Node).
-endif.

%

-define(new_INTERNAL4(E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5),
    ?CHECK_NODE_RECUR(?INTERNAL4(E1, E2, E3, E4, O1, O2, O3, O4, C1, C2, C3, C4, C5))
).

-define(new_INTERNAL3(E1, E2, E3, O1, O2, O3, C1, C2, C3, C4),
    ?CHECK_NODE_RECUR(?INTERNAL3(E1, E2, E3, O1, O2, O3, C1, C2, C3, C4))
).

-define(new_INTERNAL2(E1, E2, O1, O2, C1, C2, C3),
    ?CHECK_NODE_RECUR(?INTERNAL2(E1, E2, O1, O2, C1, C2, C3))
).

-define(new_INTERNAL1(E1, O1, C1, C2), ?CHECK_NODE(?INTERNAL1(E1, O1, C1, C2))).

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
        pos_integer(),
        pos_integer(),
        pos_integer(),
        pos_integer(),
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
        pos_integer(),
        pos_integer(),
        pos_integer(),
        deep_node(Elem),
        deep_node(Elem),
        deep_node(Elem),
        deep_node(Elem)
    )).

-type node_INTERNAL2(Elem) ::
    (?INTERNAL2(
        Elem,
        Elem,
        pos_integer(),
        pos_integer(),
        deep_node(Elem),
        deep_node(Elem),
        deep_node(Elem)
    )).

-type node_INTERNAL1(Elem) ::
    (?INTERNAL1(
        Elem,
        pos_integer(),
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
    Elem, pos_integer(), node_INTERNAL2(Elem), node_INTERNAL2(Elem)
).

-type split_leaf_result(Elem) :: split_result(
    Elem, 3, node_LEAF2(Elem), node_LEAF2(Elem)
).

-type split_result(Elem, SplitO, SplitL, SplitR) :: {split, Elem, SplitO, SplitL, SplitR}.

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

-spec add(NewElem, t(PrevElem)) -> t(NewElem | PrevElem).
add(Elem, ?INTERNAL1_MATCH_ALL) ->
    add_INTERNAL1(Elem, ?INTERNAL1_ARGS);
add(Elem, ?LEAF1_MATCH_ALL) ->
    add_LEAF1(Elem, ?LEAF1_ARGS);
add(Elem, ?LEAF0) ->
    ?new_LEAF1(Elem);
add(Elem, Root) ->
    case add_recur(Elem, Root) of
        ?SPLIT_MATCH(Pos, Args) ->
            insert_split_root(Elem, Pos, Args, Root);
        %
        UpdatedRoot ->
            UpdatedRoot
    end.

-spec delete_att(_, t(Elem)) -> badkey | t(Elem).
delete_att(Elem, ?INTERNAL1_MATCH_ALL) ->
    delete_att_INTERNAL1(Elem, ?INTERNAL1_ARGS);
delete_att(Elem, ?LEAF1_MATCH_ALL) ->
    delete_att_LEAF1(Elem, ?LEAF1_ARGS);
delete_att(_Elem, ?LEAF0) ->
    badkey;
delete_att(Elem, Root) ->
    delete_att_recur(Elem, Root).

-spec filtermap(fun((Elem) -> {true, MappedElem} | boolean()), t(Elem)) ->
    nonempty_improper_list(FilteredSize, t(Elem | MappedElem))
when
    FilteredSize :: non_neg_integer().
filtermap(Fun, Root) ->
    Count = 0,
    Filtered = new(),
    filtermap_root(Fun, Count, Root, Filtered).

-spec fold(fun((Elem, Acc1) -> Acc2), Acc0, t(Elem)) -> AccN when
    Acc0 :: term(),
    Acc1 :: term(),
    Acc2 :: term(),
    AccN :: term().
fold(Fun, Acc, ?INTERNAL1_MATCH_NO_SIZES) ->
    Acc2 = fold_recur(Fun, Acc, C1),
    Acc3 = Fun(E1, Acc2),
    fold_recur(Fun, Acc3, C2);
fold(Fun, Acc, ?LEAF1_MATCH_ALL) ->
    Fun(E1, Acc);
fold(_Fun, Acc, ?LEAF0) ->
    Acc;
fold(Fun, Acc, Root) ->
    fold_recur(Fun, Acc, Root).

-spec from_ordered_list([Elem], non_neg_integer()) -> t(Elem).
from_ordered_list([], 0) ->
    ?LEAF0;
from_ordered_list([E1], 1) ->
    ?new_LEAF1(E1);
from_ordered_list(L, S) ->
    [BatchOffset | BatchSize] = from_ordered_list_initial_batch_params(S),
    AtRoot = true,

    [Root | []] = from_ordered_list_recur(L, S, BatchOffset, BatchSize, AtRoot),
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

-spec is_member(_, t(_)) -> boolean().
is_member(Elem, ?INTERNAL1_MATCH_NO_SIZES) ->
    is_member_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES);
is_member(Elem, ?LEAF1_MATCH_ALL) ->
    is_member_LEAF1(Elem, ?LEAF1_ARGS);
is_member(_Elem, ?LEAF0) ->
    false;
is_member(Elem, Root) ->
    is_member_recur(Elem, Root).

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
larger(Elem, ?INTERNAL1_MATCH_NO_SIZES) ->
    larger_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES);
larger(Elem, ?LEAF1_MATCH_ALL) ->
    larger_LEAF1(Elem, ?LEAF1_ARGS);
larger(_Elem, ?LEAF0) ->
    none;
larger(Elem, Root) ->
    larger_recur(Elem, Root).

-spec largest(t(Elem)) -> Elem.
largest(?INTERNAL1_MATCH(_, _, _, C2)) ->
    largest_recur(C2);
largest(?LEAF1_MATCH(E1)) ->
    E1;
largest(Root) ->
    largest_recur(Root).

-spec map(fun((Elem) -> MappedElem), t(Elem)) -> t(MappedElem).
map(Fun, Node) ->
    Acc = new(),
    map_root(Fun, Node, Acc).

-spec merge(non_neg_integer(), t(Elem1), non_neg_integer(), t(Elem2)) -> t(Elem1 | Elem2).
merge(Size1, Root1, Size2, Root2) when Size1 < Size2 ->
    merge_root(Root1, Root2);
merge(_Size1, Root1, _Size2, Root2) ->
    merge_root(Root2, Root1).

-spec new() -> t(_).
new() ->
    xb5_utils:dialyzer_opaque_term(?LEAF0).

-spec next(iter(Elem)) -> {Elem, iter(Elem)} | none.
next([Head | Tail]) ->
    next(Head, Tail);
next([]) ->
    none.

-spec nth(pos_integer(), t(E)) -> E.
nth(Rank, ?INTERNAL1_MATCH_ALL) ->
    nth_INTERNAL1(Rank, ?INTERNAL1_ARGS);
nth(Rank, ?LEAF1_MATCH_ALL) ->
    nth_LEAF1(Rank, ?LEAF1_ARGS);
nth(Rank, Root) ->
    nth_recur(Rank, Root).

-spec nth_and_nthp1(pos_integer(), t(E)) -> E.
nth_and_nthp1(Rank, ?INTERNAL1_MATCH_ALL) ->
    nth_and_nthp1_INTERNAL1(Rank, ?INTERNAL1_ARGS);
nth_and_nthp1(Rank, Root) ->
    nth_and_nthp1_recur(Rank, Root, nil).

-spec rank(_, t(_)) -> pos_integer() | none.
rank(Elem, ?INTERNAL1_MATCH_ALL) ->
    rank_INTERNAL1(Elem, ?INTERNAL1_ARGS);
rank(Elem, ?LEAF1_MATCH_ALL) ->
    rank_LEAF1(Elem, ?LEAF1_ARGS);
rank(_Elem, ?LEAF0) ->
    none;
rank(Elem, Root) ->
    rank_recur(Elem, Root, 0).

-spec rank_larger(_, t(E)) -> nonempty_improper_list(pos_integer(), E) | none.
rank_larger(Elem, ?INTERNAL1_MATCH_ALL) ->
    rank_larger_INTERNAL1(Elem, ?INTERNAL1_ARGS);
rank_larger(Elem, ?LEAF1_MATCH_ALL) ->
    rank_larger_LEAF1(Elem, ?LEAF1_ARGS);
rank_larger(_Elem, ?LEAF0) ->
    none;
rank_larger(Elem, Root) ->
    rank_larger_recur(Elem, Root, 0).

-spec rank_smaller(_, t(E)) -> nonempty_improper_list(pos_integer(), E) | none.
rank_smaller(Elem, ?INTERNAL1_MATCH_ALL) ->
    rank_smaller_INTERNAL1(Elem, ?INTERNAL1_ARGS);
rank_smaller(Elem, ?LEAF1_MATCH_ALL) ->
    rank_smaller_LEAF1(Elem, ?LEAF1_ARGS);
rank_smaller(_Elem, ?LEAF0) ->
    none;
rank_smaller(Elem, Root) ->
    rank_smaller_recur(Elem, Root, 0).

-spec smaller(_, t(Elem)) -> {found, Elem} | none.
smaller(Elem, ?INTERNAL1_MATCH_NO_SIZES) ->
    smaller_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES);
smaller(Elem, ?LEAF1_MATCH_ALL) ->
    smaller_LEAF1(Elem, ?LEAF1_ARGS);
smaller(_Elem, ?LEAF0) ->
    none;
smaller(Elem, Root) ->
    smaller_recur(Elem, Root).

-spec smallest(t(Elem)) -> Elem.
smallest(?INTERNAL1_MATCH(_, _, C1, _)) ->
    smallest_recur(C1);
smallest(?LEAF1_MATCH(E1)) ->
    E1;
smallest(Root) ->
    smallest_recur(Root).

-spec structural_stats(t(_)) -> xb5_structural_stats:t().
structural_stats(Root) ->
    Acc = xb5_structural_stats:new(),

    case Root of
        ?INTERNAL1_MATCH(_, _, C1, C2) ->
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
    take_largest_INTERNAL1(?INTERNAL1_ARGS);
take_largest(?LEAF1_MATCH_ALL) ->
    take_largest_LEAF1(?LEAF1_ARGS);
take_largest(Root) ->
    take_largest_recur(Root).

-spec take_smallest(t(Elem)) -> take_result(Elem).
take_smallest(?INTERNAL1_MATCH_ALL) ->
    take_smallest_INTERNAL1(?INTERNAL1_ARGS);
take_smallest(?LEAF1_MATCH_ALL) ->
    take_smallest_LEAF1(?LEAF1_ARGS);
take_smallest(Root) ->
    take_smallest_recur(Root).

-spec to_list(t(Elem)) -> [Elem].
to_list(?INTERNAL1_MATCH_NO_SIZES) ->
    Acc2 = to_list_recur(C2, []),
    Acc3 = [E1 | Acc2],
    to_list_recur(C1, Acc3);
to_list(?LEAF1_MATCH_ALL) ->
    [E1];
to_list(?LEAF0) ->
    [];
to_list(Root) ->
    to_list_recur(Root, []).

%% ------------------------------------------------------------------
%% Internal Function Definitions: add/2
%% ------------------------------------------------------------------

add_recur(Elem, Node) ->
    case Node of
        ?INTERNAL2_MATCH_ALL ->
            add_INTERNAL2(Elem, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            add_INTERNAL3(Elem, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            add_INTERNAL4(Elem, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            add_LEAF2(Elem, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            add_LEAF3(Elem, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            add_LEAF4(Elem, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, add_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
add_INTERNAL4(Elem, ?INTERNAL4_ARGS) ->
    if
        Elem > E2 ->
            %
            if
                Elem =< E4 ->
                    %
                    if
                        Elem > E3 ->
                            add_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS);
                        %
                        true ->
                            add_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS)
                    end;
                %
                true ->
                    add_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS)
            end;
        %
        Elem =< E1 ->
            add_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS);
        %
        Elem > E1 ->
            add_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS)
    end.

-compile({inline, add_INTERNAL4_C1 / ?INTERNAL4_ARITY_PLUS1}).
add_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS) ->
    Result = add_recur(Elem, C1),
    ins_rebalance_INTERNAL4_C1(Result, Elem, ?INTERNAL4_ARGS).

-compile({inline, add_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS1}).
add_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS) ->
    Result = add_recur(Elem, C2),
    ins_rebalance_INTERNAL4_C2(Result, Elem, ?INTERNAL4_ARGS).

-compile({inline, add_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS1}).
add_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS) ->
    Result = add_recur(Elem, C3),
    ins_rebalance_INTERNAL4_C3(Result, Elem, ?INTERNAL4_ARGS).

-compile({inline, add_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS1}).
add_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS) ->
    Result = add_recur(Elem, C4),
    ins_rebalance_INTERNAL4_C4(Result, Elem, ?INTERNAL4_ARGS).

-compile({inline, add_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS1}).
add_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS) ->
    Result = add_recur(Elem, C5),
    ins_rebalance_INTERNAL4_C5(Result, Elem, ?INTERNAL4_ARGS).

%%
%% ?INTERNAL3
%%

-compile({inline, add_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
add_INTERNAL3(Elem, ?INTERNAL3_ARGS) ->
    if
        Elem =< E2 ->
            %
            if
                Elem =< E1 ->
                    add_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS);
                %
                true ->
                    add_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS)
            end;
        %
        Elem =< E3 ->
            add_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS);
        %
        true ->
            add_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS)
    end.

-compile({inline, add_INTERNAL3_C1 / ?INTERNAL3_ARITY_PLUS1}).
add_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS) ->
    Result = add_recur(Elem, C1),
    ins_rebalance_INTERNAL3_C1(Result, Elem, ?INTERNAL3_ARGS).

-compile({inline, add_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS1}).
add_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS) ->
    Result = add_recur(Elem, C2),
    ins_rebalance_INTERNAL3_C2(Result, Elem, ?INTERNAL3_ARGS).

-compile({inline, add_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS1}).
add_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS) ->
    Result = add_recur(Elem, C3),
    ins_rebalance_INTERNAL3_C3(Result, Elem, ?INTERNAL3_ARGS).

-compile({inline, add_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS1}).
add_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS) ->
    Result = add_recur(Elem, C4),
    ins_rebalance_INTERNAL3_C4(Result, Elem, ?INTERNAL3_ARGS).

%%
%% ?INTERNAL2
%%

-compile({inline, add_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
add_INTERNAL2(Elem, ?INTERNAL2_ARGS) ->
    if
        Elem > E1 ->
            %
            if
                Elem =< E2 ->
                    add_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS);
                %
                true ->
                    add_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS)
            end;
        %
        true ->
            add_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS)
    end.

-compile({inline, add_INTERNAL2_C1 / ?INTERNAL2_ARITY_PLUS1}).
add_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS) ->
    Result = add_recur(Elem, C1),
    ins_rebalance_INTERNAL2_C1(Result, Elem, ?INTERNAL2_ARGS).

-compile({inline, add_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS1}).
add_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS) ->
    Result = add_recur(Elem, C2),
    ins_rebalance_INTERNAL2_C2(Result, Elem, ?INTERNAL2_ARGS).

-compile({inline, add_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS1}).
add_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS) ->
    Result = add_recur(Elem, C3),
    ins_rebalance_INTERNAL2_C3(Result, Elem, ?INTERNAL2_ARGS).

%%
%% ?INTERNAL1
%%

-compile({inline, add_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
add_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem =< E1 ->
            add_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS);
        %
        true ->
            add_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS)
    end.

-compile({inline, add_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
add_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    Result = add_recur(Elem, C1),
    ins_rebalance_INTERNAL1_C1(Result, Elem, ?INTERNAL1_ARGS).

-compile({inline, add_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
add_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS) ->
    Result = add_recur(Elem, C2),
    ins_rebalance_INTERNAL1_C2(Result, Elem, ?INTERNAL1_ARGS).

%%
%% ?LEAF4
%%

-compile({inline, add_LEAF4 / ?LEAF4_ARITY_PLUS1}).
add_LEAF4(Elem, ?LEAF4_ARGS) ->
    if
        Elem > E2 ->
            %
            if
                Elem =< E4 ->
                    %
                    if
                        Elem > E3 ->
                            ?SPLIT(4, []);
                        %
                        true ->
                            ?SPLIT(3, [])
                    end;
                %
                true ->
                    ?SPLIT(5, [])
            end;
        %
        Elem =< E1 ->
            ?SPLIT(1, []);
        %
        true ->
            ?SPLIT(2, [])
    end.

%%
%% ?LEAF3
%%

-compile({inline, add_LEAF3 / ?LEAF3_ARITY_PLUS1}).
add_LEAF3(Elem, ?LEAF3_ARGS) ->
    if
        Elem =< E2 ->
            %
            if
                Elem =< E1 ->
                    put_LEAF3_POS1(Elem, ?LEAF3_ARGS);
                %
                true ->
                    put_LEAF3_POS2(Elem, ?LEAF3_ARGS)
            end;
        %
        Elem =< E3 ->
            put_LEAF3_POS3(Elem, ?LEAF3_ARGS);
        %
        true ->
            put_LEAF3_POS4(Elem, ?LEAF3_ARGS)
    end.

-compile({inline, put_LEAF3_POS1 / ?LEAF3_ARITY_PLUS1}).
put_LEAF3_POS1(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(Elem, E1, E2, E3).

-compile({inline, put_LEAF3_POS2 / ?LEAF3_ARITY_PLUS1}).
put_LEAF3_POS2(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(E1, Elem, E2, E3).

-compile({inline, put_LEAF3_POS3 / ?LEAF3_ARITY_PLUS1}).
put_LEAF3_POS3(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(E1, E2, Elem, E3).

-compile({inline, put_LEAF3_POS4 / ?LEAF3_ARITY_PLUS1}).
put_LEAF3_POS4(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(E1, E2, E3, Elem).

%%
%% ?LEAF2
%%

-compile({inline, add_LEAF2 / ?LEAF2_ARITY_PLUS1}).
add_LEAF2(Elem, ?LEAF2_ARGS) ->
    if
        Elem > E1 ->
            %
            if
                Elem =< E2 ->
                    put_LEAF2_POS2(Elem, ?LEAF2_ARGS);
                %
                true ->
                    put_LEAF2_POS3(Elem, ?LEAF2_ARGS)
            end;
        %
        true ->
            put_LEAF2_POS1(Elem, ?LEAF2_ARGS)
    end.

-compile({inline, put_LEAF2_POS1 / ?LEAF2_ARITY_PLUS1}).
put_LEAF2_POS1(Elem, ?LEAF2_ARGS) ->
    ?new_LEAF3(Elem, E1, E2).

-compile({inline, put_LEAF2_POS2 / ?LEAF2_ARITY_PLUS1}).
put_LEAF2_POS2(Elem, ?LEAF2_ARGS) ->
    ?new_LEAF3(E1, Elem, E2).

-compile({inline, put_LEAF2_POS3 / ?LEAF2_ARITY_PLUS1}).
put_LEAF2_POS3(Elem, ?LEAF2_ARGS) ->
    ?new_LEAF3(E1, E2, Elem).

%%
%% ?LEAF1
%%

-compile({inline, add_LEAF1 / ?LEAF1_ARITY_PLUS1}).
add_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem =< E1 ->
            put_LEAF1_POS1(Elem, ?LEAF1_ARGS);
        %
        true ->
            put_LEAF1_POS2(Elem, ?LEAF1_ARGS)
    end.

-compile({inline, put_LEAF1_POS1 / ?LEAF1_ARITY_PLUS1}).
put_LEAF1_POS1(Elem, ?LEAF1_ARGS) ->
    ?new_LEAF2(Elem, E1).

-compile({inline, put_LEAF1_POS2 / ?LEAF1_ARITY_PLUS1}).
put_LEAF1_POS2(Elem, ?LEAF1_ARGS) ->
    ?new_LEAF2(E1, Elem).

%% ------------------------------------------------------------------
%% Internal Function Definitions: delete_att/2
%% ------------------------------------------------------------------

delete_att_recur(Elem, Node) ->
    case Node of
        %
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
    if
        Elem > E2 ->
            %
            if
                Elem < E4 ->
                    %
                    if
                        Elem > E3 ->
                            delete_att_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS);
                        %
                        Elem < E3 ->
                            delete_att_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS);
                        %
                        true ->
                            delete_att_INTERNAL4_E3(?INTERNAL4_ARGS)
                    end;
                %
                Elem > E4 ->
                    delete_att_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_att_INTERNAL4_E4(?INTERNAL4_ARGS)
            end;
        %
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    delete_att_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS);
                %
                Elem > E1 ->
                    delete_att_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS);
                %
                true ->
                    delete_att_INTERNAL4_E1(?INTERNAL4_ARGS)
            end;
        %
        true ->
            delete_att_INTERNAL4_E2(?INTERNAL4_ARGS)
    end.

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
        O1,
        O2,
        O3,
        O4,
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
        O1,
        O2,
        O3,
        O4,
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
        O1,
        O2,
        O3,
        O4,
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
        O1,
        O2,
        O3,
        O4,
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
    if
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    delete_att_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS);
                %
                Elem > E1 ->
                    delete_att_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_att_INTERNAL3_E1(?INTERNAL3_ARGS)
            end;
        %
        Elem > E2 ->
            %
            if
                Elem < E3 ->
                    delete_att_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS);
                %
                Elem > E3 ->
                    delete_att_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS);
                %
                true ->
                    delete_att_INTERNAL3_E3(?INTERNAL3_ARGS)
            end;
        %
        true ->
            delete_att_INTERNAL3_E2(?INTERNAL3_ARGS)
    end.

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
        O1,
        O2,
        O3,
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
        O1,
        O2,
        O3,
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
        O1,
        O2,
        O3,
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
    if
        Elem > E1 ->
            %
            if
                Elem < E2 ->
                    delete_att_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS);
                %
                Elem > E2 ->
                    delete_att_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS);
                %
                true ->
                    delete_att_INTERNAL2_E2(?INTERNAL2_ARGS)
            end;
        %
        Elem < E1 ->
            delete_att_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS);
        %
        true ->
            delete_att_INTERNAL2_E1(?INTERNAL2_ARGS)
    end.

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
        O1,
        O2,
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
        O1,
        O2,
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
    if
        Elem < E1 ->
            delete_att_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS);
        %
        Elem > E1 ->
            delete_att_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS);
        %
        true ->
            delete_att_INTERNAL1_E1(?INTERNAL1_ARGS)
    end.

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
%% ?LEAF4
%%

-compile({inline, delete_att_LEAF4 / ?LEAF4_ARITY_PLUS1}).
delete_att_LEAF4(Elem, ?LEAF4_ARGS) ->
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
            badkey
    end.

%%
%% ?LEAF3
%%

-compile({inline, delete_att_LEAF3 / ?LEAF3_ARITY_PLUS1}).
delete_att_LEAF3(Elem, ?LEAF3_ARGS) ->
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
            badkey
    end.

%%
%% ?LEAF2
%%

-compile({inline, delete_att_LEAF2 / ?LEAF2_ARITY_PLUS1}).
delete_att_LEAF2(Elem, ?LEAF2_ARGS) ->
    if
        Elem == E1 ->
            ?new_LEAF1(E2);
        %
        Elem == E2 ->
            ?new_LEAF1(E1);
        %
        true ->
            badkey
    end.

%%
%% ?LEAF1
%%

-compile({inline, delete_att_LEAF1 / ?LEAF1_ARITY_PLUS1}).
delete_att_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem == E1 ->
            ?LEAF0;
        %
        true ->
            badkey
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: filtermap/2
%% ------------------------------------------------------------------

filtermap_root(Fun, Count, Root, Filtered) ->
    case Root of
        ?INTERNAL1_MATCH_NO_SIZES ->
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
            filtermap_leaf_batch2(Fun, E1, E2, Count, Filtered);
        %
        ?LEAF3_MATCH_ALL ->
            filtermap_leaf_batch3(Fun, E1, E2, E3, Count, Filtered);
        %
        ?LEAF4_MATCH_ALL ->
            filtermap_leaf_batch4(Fun, E1, E2, E3, E4, Count, Filtered);
        %
        ?INTERNAL2_MATCH_NO_SIZES ->
            filtermap_internal_batch2(Fun, E1, E2, C1, C2, C3, Count, Filtered);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            filtermap_internal_batch3(Fun, E1, E2, E3, C1, C2, C3, C4, Count, Filtered);
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
            filtermap_internal_batch4(Fun, E1, E2, E3, E4, C1, C2, C3, C4, C5, Count, Filtered)
    end.

%% INTERNAL4

-compile({inline, filtermap_internal_batch4/12}).
filtermap_internal_batch4(Fun, E1, E2, E3, E4, C1, C2, C3, C4, C5, Count, Filtered) ->
    [Count2 | Filtered2] = filtermap_recur(Fun, C1, Count, Filtered),

    case Fun(E1) of
        false ->
            filtermap_internal_batch3(Fun, E2, E3, E4, C2, C3, C4, C5, Count2, Filtered2);
        %
        True ->
            Elem = filtermap_true(True, E1),

            Filtered3 = add(Elem, Filtered2),
            filtermap_internal_batch3(
                Fun, E2, E3, E4, C2, C3, C4, C5, Count2 + 1, Filtered3
            )
    end.

%% INTERNAL3

-compile({inline, filtermap_internal_batch3/10}).
filtermap_internal_batch3(Fun, E1, E2, E3, C1, C2, C3, C4, Count, Filtered) ->
    [Count2 | Filtered2] = filtermap_recur(Fun, C1, Count, Filtered),

    case Fun(E1) of
        false ->
            filtermap_internal_batch2(Fun, E2, E3, C2, C3, C4, Count2, Filtered2);
        %
        True ->
            Elem = filtermap_true(True, E1),

            Filtered3 = add(Elem, Filtered2),
            filtermap_internal_batch2(Fun, E2, E3, C2, C3, C4, Count2 + 1, Filtered3)
    end.

%% INTERNAL2

-compile({inline, filtermap_internal_batch2/8}).
filtermap_internal_batch2(Fun, E1, E2, C1, C2, C3, Count, Filtered) ->
    [Count2 | Filtered2] = filtermap_recur(Fun, C1, Count, Filtered),

    case Fun(E1) of
        false ->
            filtermap_internal_batch1(Fun, E2, C2, C3, Count2, Filtered2);
        %
        True ->
            Elem = filtermap_true(True, E1),

            Filtered3 = add(Elem, Filtered2),
            filtermap_internal_batch1(Fun, E2, C2, C3, Count2 + 1, Filtered3)
    end.

%% INTERNAL1

-compile({inline, filtermap_internal_batch1/6}).
filtermap_internal_batch1(Fun, E1, C1, C2, Count, Filtered) ->
    [Count2 | Filtered2] = filtermap_recur(Fun, C1, Count, Filtered),

    case Fun(E1) of
        false ->
            filtermap_recur(Fun, C2, Count2, Filtered2);
        %
        True ->
            Elem = filtermap_true(True, E1),

            Filtered3 = add(Elem, Filtered2),
            filtermap_recur(Fun, C2, Count2 + 1, Filtered3)
    end.

%% LEAF4

-compile({inline, filtermap_leaf_batch4/7}).
filtermap_leaf_batch4(Fun, E1, E2, E3, E4, Count, Filtered) ->
    case Fun(E1) of
        false ->
            filtermap_leaf_batch3(Fun, E2, E3, E4, Count, Filtered);
        %
        True ->
            Elem = filtermap_true(True, E1),

            Filtered2 = add(Elem, Filtered),
            filtermap_leaf_batch3(Fun, E2, E3, E4, Count + 1, Filtered2)
    end.

%% LEAF3

-compile({inline, filtermap_leaf_batch3/6}).
filtermap_leaf_batch3(Fun, E1, E2, E3, Count, Filtered) ->
    case Fun(E1) of
        false ->
            filtermap_leaf_batch2(Fun, E2, E3, Count, Filtered);
        %
        True ->
            Elem = filtermap_true(True, E1),

            Filtered2 = add(Elem, Filtered),
            filtermap_leaf_batch2(Fun, E2, E3, Count + 1, Filtered2)
    end.

%% LEAF2

-compile({inline, filtermap_leaf_batch2/5}).
filtermap_leaf_batch2(Fun, E1, E2, Count, Filtered) ->
    case Fun(E1) of
        false ->
            filtermap_single_element(Fun, E2, Count, Filtered);
        %
        True ->
            Elem = filtermap_true(True, E1),

            Filtered2 = add(Elem, Filtered),
            filtermap_single_element(Fun, E2, Count + 1, Filtered2)
    end.

%% LEAF1

-compile({inline, filtermap_single_element/4}).
filtermap_single_element(Fun, E1, Count, Filtered) ->
    case Fun(E1) of
        false ->
            [Count | Filtered];
        %
        True ->
            Elem = filtermap_true(True, E1),

            Filtered2 = add(Elem, Filtered),
            [Count + 1 | Filtered2]
    end.

%%

-compile({inline, filtermap_true/2}).
filtermap_true(True, PrevElem) ->
    case True of
        {true, NewElem} ->
            NewElem;
        %
        true ->
            PrevElem
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
        ?INTERNAL2_MATCH_NO_SIZES ->
            Acc2 = fold_recur(Fun, Acc, C1),
            Acc3 = fold_recur(Fun, Fun(E1, Acc2), C2),
            _Acc4 = fold_recur(Fun, Fun(E2, Acc3), C3);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            Acc2 = fold_recur(Fun, Acc, C1),
            Acc3 = fold_recur(Fun, Fun(E1, Acc2), C2),
            Acc4 = fold_recur(Fun, Fun(E2, Acc3), C3),
            _Acc5 = fold_recur(Fun, Fun(E3, Acc4), C4);
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
            Acc2 = fold_recur(Fun, Acc, C1),
            Acc3 = fold_recur(Fun, Fun(E1, Acc2), C2),
            Acc4 = fold_recur(Fun, Fun(E2, Acc3), C3),
            Acc5 = fold_recur(Fun, Fun(E3, Acc4), C4),
            _Acc6 = fold_recur(Fun, Fun(E4, Acc5), C5)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: from_ordered_list/2
%% ------------------------------------------------------------------

from_ordered_list_initial_batch_params(S) when S >= 1 ->
    from_ordered_list_initial_batch_params(S, 1, 1).

from_ordered_list_initial_batch_params(S, BatchOffset, BatchSize) ->
    case BatchOffset + (4 * BatchSize) of
        NextOffset when NextOffset =< S ->
            from_ordered_list_initial_batch_params(S, NextOffset, BatchSize * 4);
        _ ->
            [BatchOffset | BatchSize]
    end.

from_ordered_list_recur(L, S, BatchOffset, BatchSize, AtRoot) when S >= 5 ->
    ChildrenBatchOffset = BatchOffset - BatchSize,
    ChildrenBatchSize = BatchSize bsr 2,

    case (S - BatchOffset) div BatchSize of
        2 ->
            S1 = S2 = BatchSize - 1,
            [S3 | S4] = from_ordered_list_right_children_sizes(S - (BatchSize bsl 1), BatchSize),
            from_ordered_list_INTERNAL3(L, S1, S2, S3, S4, ChildrenBatchOffset, ChildrenBatchSize);
        %
        3 ->
            S1 = S2 = S3 = BatchSize - 1,
            [S4 | S5] = from_ordered_list_right_children_sizes(S - (BatchSize * 3), BatchSize),
            from_ordered_list_INTERNAL4(
                L,
                S1,
                S2,
                S3,
                S4,
                S5,
                ChildrenBatchOffset,
                ChildrenBatchSize
            );
        %
        Splits when Splits =:= 1 orelse (Splits =:= 0 andalso not AtRoot) ->
            S1 = BatchSize - 1,
            [S2 | S3] = from_ordered_list_right_children_sizes(S - BatchSize, BatchSize),
            from_ordered_list_INTERNAL2(L, S1, S2, S3, ChildrenBatchOffset, ChildrenBatchSize);
        %
        0 ->
            [S1 | S2] = from_ordered_list_right_children_sizes(S, BatchSize),
            from_ordered_list_INTERNAL1(L, S1, S2, ChildrenBatchOffset, ChildrenBatchSize)
    end;
from_ordered_list_recur(L, 4, _, _, _) ->
    [E1, E2, E3, E4 | Next] = L,
    [?new_LEAF4(E1, E2, E3, E4) | Next];
from_ordered_list_recur(L, 3, _, _, _) ->
    [E1, E2, E3 | Next] = L,
    [?new_LEAF3(E1, E2, E3) | Next];
from_ordered_list_recur(L, 2, _, _, _) ->
    [E1, E2 | Next] = L,
    [?new_LEAF2(E1, E2) | Next].

from_ordered_list_right_children_sizes(RemainingSize, BatchSize) ->
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

-compile({inline, from_ordered_list_INTERNAL1/5}).
from_ordered_list_INTERNAL1(L, S1, S2, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [E1 | L2]] = from_ordered_list_recur(
        L,
        S1,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C2 | []] = from_ordered_list_recur(
        L2,
        S2,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),

    Node = ?new_INTERNAL1(
        E1,
        %
        S1 + 1,
        %
        C1,
        C2
    ),

    [Node | []].

-compile({inline, from_ordered_list_INTERNAL2/6}).
from_ordered_list_INTERNAL2(L, S1, S2, S3, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [E1 | L2]] = from_ordered_list_recur(
        L,
        S1,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C2 | [E2 | L3]] = from_ordered_list_recur(
        L2,
        S2,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C3 | L4] = from_ordered_list_recur(
        L3,
        S3,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),

    Node = ?new_INTERNAL2(
        E1,
        E2,
        %
        S1 + 1,
        S1 + S2 + 2,
        %,
        C1,
        C2,
        C3
    ),

    [Node | L4].

-compile({inline, from_ordered_list_INTERNAL3/7}).
from_ordered_list_INTERNAL3(L, S1, S2, S3, S4, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [E1 | L2]] = from_ordered_list_recur(
        L,
        S1,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C2 | [E2 | L3]] = from_ordered_list_recur(
        L2,
        S2,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C3 | [E3 | L4]] = from_ordered_list_recur(
        L3,
        S3,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C4 | L5] = from_ordered_list_recur(
        L4,
        S4,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),

    S12 = S1 + S2,

    Node = ?new_INTERNAL3(
        E1,
        E2,
        E3,
        %
        S1 + 1,
        S12 + 2,
        S12 + S3 + 3,
        %,
        C1,
        C2,
        C3,
        C4
    ),

    [Node | L5].

-compile({inline, from_ordered_list_INTERNAL4/8}).
from_ordered_list_INTERNAL4(L, S1, S2, S3, S4, S5, ChildrenBatchOffset, ChildrenBatchSize) ->
    [C1 | [E1 | L2]] = from_ordered_list_recur(
        L,
        S1,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C2 | [E2 | L3]] = from_ordered_list_recur(
        L2,
        S2,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C3 | [E3 | L4]] = from_ordered_list_recur(
        L3,
        S3,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C4 | [E4 | L5]] = from_ordered_list_recur(
        L4,
        S4,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),
    [C5 | L6] = from_ordered_list_recur(
        L5,
        S5,
        ChildrenBatchOffset,
        ChildrenBatchSize,
        false
    ),

    S12 = S1 + S2,
    S123 = S12 + S3,

    Node = ?new_INTERNAL4(
        E1,
        E2,
        E3,
        E4,
        %
        S1 + 1,
        S12 + 2,
        S123 + 3,
        S123 + S4 + 4,
        %,
        C1,
        C2,
        C3,
        C4,
        C5
    ),

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
    if
        Elem > E2 ->
            %
            if
                Elem < E4 ->
                    %
                    if
                        Elem > E3 ->
                            insert_att_INTERNAL4_C4(Elem, ?INTERNAL4_ARGS);
                        %
                        Elem < E3 ->
                            insert_att_INTERNAL4_C3(Elem, ?INTERNAL4_ARGS);
                        %
                        true ->
                            key_exists
                    end;
                %
                Elem > E4 ->
                    insert_att_INTERNAL4_C5(Elem, ?INTERNAL4_ARGS);
                %
                true ->
                    key_exists
            end;
        %
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    insert_att_INTERNAL4_C1(Elem, ?INTERNAL4_ARGS);
                %
                Elem > E1 ->
                    insert_att_INTERNAL4_C2(Elem, ?INTERNAL4_ARGS);
                %
                true ->
                    key_exists
            end;
        %
        true ->
            key_exists
    end.

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
    if
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    insert_att_INTERNAL3_C1(Elem, ?INTERNAL3_ARGS);
                %
                Elem > E1 ->
                    insert_att_INTERNAL3_C2(Elem, ?INTERNAL3_ARGS);
                %
                true ->
                    key_exists
            end;
        %
        Elem > E2 ->
            %
            if
                Elem < E3 ->
                    insert_att_INTERNAL3_C3(Elem, ?INTERNAL3_ARGS);
                %
                Elem > E3 ->
                    insert_att_INTERNAL3_C4(Elem, ?INTERNAL3_ARGS);
                %
                true ->
                    key_exists
            end;
        %
        true ->
            key_exists
    end.

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
    if
        Elem > E1 ->
            %
            if
                Elem < E2 ->
                    insert_att_INTERNAL2_C2(Elem, ?INTERNAL2_ARGS);
                %
                Elem > E2 ->
                    insert_att_INTERNAL2_C3(Elem, ?INTERNAL2_ARGS);
                %
                true ->
                    key_exists
            end;
        %
        Elem < E1 ->
            insert_att_INTERNAL2_C1(Elem, ?INTERNAL2_ARGS);
        %
        true ->
            key_exists
    end.

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
    if
        Elem < E1 ->
            insert_att_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS);
        %
        Elem > E1 ->
            insert_att_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS);
        %
        true ->
            key_exists
    end.

-compile({inline, insert_att_INTERNAL1_C1 / ?INTERNAL1_ARITY_PLUS1}).
insert_att_INTERNAL1_C1(Elem, ?INTERNAL1_ARGS) ->
    Result = insert_att_recur(Elem, C1),
    ins_rebalance_INTERNAL1_C1(Result, Elem, ?INTERNAL1_ARGS).

-compile({inline, insert_att_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS1}).
insert_att_INTERNAL1_C2(Elem, ?INTERNAL1_ARGS) ->
    Result = insert_att_recur(Elem, C2),
    ins_rebalance_INTERNAL1_C2(Result, Elem, ?INTERNAL1_ARGS).

%%
%% ?LEAF4
%%

-compile({inline, insert_att_LEAF4 / ?LEAF4_ARITY_PLUS1}).
insert_att_LEAF4(Elem, ?LEAF4_ARGS) ->
    if
        Elem > E2 ->
            %
            if
                Elem < E4 ->
                    %
                    if
                        Elem > E3 ->
                            ?SPLIT(4, []);
                        %
                        Elem < E3 ->
                            ?SPLIT(3, []);
                        %
                        true ->
                            key_exists
                    end;
                %
                Elem > E4 ->
                    ?SPLIT(5, []);
                %
                true ->
                    key_exists
            end;
        %
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    ?SPLIT(1, []);
                %
                Elem > E1 ->
                    ?SPLIT(2, []);
                %
                true ->
                    key_exists
            end;
        %
        true ->
            key_exists
    end.

%%
%% ?LEAF3
%%

-compile({inline, insert_att_LEAF3 / ?LEAF3_ARITY_PLUS1}).
insert_att_LEAF3(Elem, ?LEAF3_ARGS) ->
    if
        Elem < E2 ->
            %
            if
                Elem < E1 ->
                    insert_att_LEAF3_POS1(Elem, ?LEAF3_ARGS);
                %
                Elem > E1 ->
                    insert_att_LEAF3_POS2(Elem, ?LEAF3_ARGS);
                %
                true ->
                    key_exists
            end;
        %
        Elem > E2 ->
            %
            if
                Elem < E3 ->
                    insert_att_LEAF3_POS3(Elem, ?LEAF3_ARGS);
                %
                Elem > E3 ->
                    insert_att_LEAF3_POS4(Elem, ?LEAF3_ARGS);
                %
                true ->
                    key_exists
            end;
        %
        true ->
            key_exists
    end.

-compile({inline, insert_att_LEAF3_POS1 / ?LEAF3_ARITY_PLUS1}).
insert_att_LEAF3_POS1(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(Elem, E1, E2, E3).

-compile({inline, insert_att_LEAF3_POS2 / ?LEAF3_ARITY_PLUS1}).
insert_att_LEAF3_POS2(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(E1, Elem, E2, E3).

-compile({inline, insert_att_LEAF3_POS3 / ?LEAF3_ARITY_PLUS1}).
insert_att_LEAF3_POS3(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(E1, E2, Elem, E3).

-compile({inline, insert_att_LEAF3_POS4 / ?LEAF3_ARITY_PLUS1}).
insert_att_LEAF3_POS4(Elem, ?LEAF3_ARGS) ->
    ?new_LEAF4(E1, E2, E3, Elem).

%%
%% ?LEAF2
%%

-compile({inline, insert_att_LEAF2 / ?LEAF2_ARITY_PLUS1}).
insert_att_LEAF2(Elem, ?LEAF2_ARGS) ->
    if
        Elem > E1 ->
            %
            if
                Elem < E2 ->
                    insert_att_LEAF2_POS2(Elem, ?LEAF2_ARGS);
                %
                Elem > E2 ->
                    insert_att_LEAF2_POS3(Elem, ?LEAF2_ARGS);
                %
                true ->
                    key_exists
            end;
        %
        Elem < E1 ->
            insert_att_LEAF2_POS1(Elem, ?LEAF2_ARGS);
        %
        true ->
            key_exists
    end.

-compile({inline, insert_att_LEAF2_POS1 / ?LEAF2_ARITY_PLUS1}).
insert_att_LEAF2_POS1(Elem, ?LEAF2_ARGS) ->
    ?new_LEAF3(Elem, E1, E2).

-compile({inline, insert_att_LEAF2_POS2 / ?LEAF2_ARITY_PLUS1}).
insert_att_LEAF2_POS2(Elem, ?LEAF2_ARGS) ->
    ?new_LEAF3(E1, Elem, E2).

-compile({inline, insert_att_LEAF2_POS3 / ?LEAF2_ARITY_PLUS1}).
insert_att_LEAF2_POS3(Elem, ?LEAF2_ARGS) ->
    ?new_LEAF3(E1, E2, Elem).

%%
%% ?LEAF1
%%

-compile({inline, insert_att_LEAF1 / ?LEAF1_ARITY_PLUS1}).
insert_att_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem < E1 ->
            insert_att_LEAF1_POS1(Elem, ?LEAF1_ARGS);
        %
        Elem > E1 ->
            insert_att_LEAF1_POS2(Elem, ?LEAF1_ARGS);
        %
        true ->
            key_exists
    end.

-compile({inline, insert_att_LEAF1_POS1 / ?LEAF1_ARITY_PLUS1}).
insert_att_LEAF1_POS1(Elem, ?LEAF1_ARGS) ->
    ?new_LEAF2(Elem, E1).

-compile({inline, insert_att_LEAF1_POS2 / ?LEAF1_ARITY_PLUS1}).
insert_att_LEAF1_POS2(Elem, ?LEAF1_ARGS) ->
    ?new_LEAF2(E1, Elem).

%% ------------------------------------------------------------------
%% Internal Function Definitions: is_member/2
%% ------------------------------------------------------------------

is_member_recur(Elem, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_NO_SIZES ->
            is_member_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            is_member_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES);
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
            is_member_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES);
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

-compile({inline, is_member_INTERNAL4 / ?INTERNAL4_ARITY_EXCEPT_SIZES_PLUS1}).
is_member_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES) ->
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

-compile({inline, is_member_INTERNAL3 / ?INTERNAL3_ARITY_EXCEPT_SIZES_PLUS1}).
is_member_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES) ->
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

-compile({inline, is_member_INTERNAL2 / ?INTERNAL2_ARITY_EXCEPT_SIZES_PLUS1}).
is_member_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES) ->
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

-compile({inline, is_member_INTERNAL1 / ?INTERNAL1_ARITY_EXCEPT_SIZES_PLUS1}).
is_member_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES) ->
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
%% Internal Function Definitions: iterator/2 - forward
%% ------------------------------------------------------------------

fwd_iterator(?INTERNAL1_MATCH_NO_SIZES) ->
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
fwd_iterator_recur(?INTERNAL2_MATCH_NO_SIZES, Acc) ->
    Acc2 = [
        ?ITER_ELEM(E1),
        C2,
        ?ITER_ELEM(E2),
        C3
        | Acc
    ],
    fwd_iterator_recur(C1, Acc2);
fwd_iterator_recur(?INTERNAL3_MATCH_NO_SIZES, Acc) ->
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
fwd_iterator_recur(?INTERNAL4_MATCH_NO_SIZES, Acc) ->
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

rev_iterator(?INTERNAL1_MATCH_NO_SIZES) ->
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
rev_iterator_recur(?INTERNAL2_MATCH_NO_SIZES, Acc) ->
    Acc2 = [
        ?ITER_ELEM(E2),
        C2,
        ?ITER_ELEM(E1),
        C1
        | Acc
    ],
    rev_iterator_recur(C3, Acc2);
rev_iterator_recur(?INTERNAL3_MATCH_NO_SIZES, Acc) ->
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
rev_iterator_recur(?INTERNAL4_MATCH_NO_SIZES, Acc) ->
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
        ?INTERNAL1_MATCH_NO_SIZES ->
            bound_fwd_iterator_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES);
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
        ?INTERNAL2_MATCH_NO_SIZES ->
            bound_fwd_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES, Acc);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            bound_fwd_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES, Acc);
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
            bound_fwd_iterator_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES, Acc)
    end.

%% INTERNAL4

-compile({inline, bound_fwd_iterator_INTERNAL4 / ?INTERNAL4_ARITY_EXCEPT_SIZES_PLUS2}).
bound_fwd_iterator_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES, Acc) ->
    if
        Elem > E4 ->
            bound_fwd_iterator_recur(Elem, C5, Acc);
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

-compile({inline, bound_fwd_iterator_INTERNAL3 / ?INTERNAL3_ARITY_EXCEPT_SIZES_PLUS2}).
bound_fwd_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES, Acc) ->
    if
        Elem > E3 ->
            bound_fwd_iterator_recur(Elem, C4, Acc);
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

-compile({inline, bound_fwd_iterator_INTERNAL2 / ?INTERNAL2_ARITY_EXCEPT_SIZES_PLUS2}).
bound_fwd_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES, Acc) ->
    if
        Elem > E2 ->
            bound_fwd_iterator_recur(Elem, C3, Acc);
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

-compile({inline, bound_fwd_iterator_INTERNAL1 / ?INTERNAL1_ARITY_EXCEPT_SIZES_PLUS1}).
bound_fwd_iterator_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES) ->
    if
        Elem =< E1 ->
            Acc = [?ITER_ELEM(E1), C2],
            bound_fwd_iterator_recur(Elem, C1, Acc);
        %
        true ->
            Acc = [],
            bound_fwd_iterator_recur(Elem, C2, Acc)
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
        ?INTERNAL1_MATCH_NO_SIZES ->
            bound_rev_iterator_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES);
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
        ?INTERNAL2_MATCH_NO_SIZES ->
            bound_rev_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES, Acc);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            bound_rev_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES, Acc);
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
            bound_rev_iterator_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES, Acc)
    end.

%% INTERNAL4

-compile({inline, bound_rev_iterator_INTERNAL4 / ?INTERNAL4_ARITY_EXCEPT_SIZES_PLUS2}).
bound_rev_iterator_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES, Acc) ->
    if
        Elem < E1 ->
            bound_rev_iterator_recur(Elem, C1, Acc);
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

-compile({inline, bound_rev_iterator_INTERNAL3 / ?INTERNAL3_ARITY_EXCEPT_SIZES_PLUS2}).
bound_rev_iterator_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES, Acc) ->
    if
        Elem < E1 ->
            bound_rev_iterator_recur(Elem, C1, Acc);
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

-compile({inline, bound_rev_iterator_INTERNAL2 / ?INTERNAL2_ARITY_EXCEPT_SIZES_PLUS2}).
bound_rev_iterator_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES, Acc) ->
    if
        Elem < E1 ->
            bound_rev_iterator_recur(Elem, C1, Acc);
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

-compile({inline, bound_rev_iterator_INTERNAL1 / ?INTERNAL1_ARITY_EXCEPT_SIZES_PLUS1}).
bound_rev_iterator_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES) ->
    if
        Elem >= E1 ->
            Acc = [?ITER_ELEM(E1), C1],
            bound_rev_iterator_recur(Elem, C2, Acc);
        %
        true ->
            Acc = [],
            bound_rev_iterator_recur(Elem, C1, Acc)
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
        ?INTERNAL2_MATCH_NO_SIZES ->
            larger_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            larger_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES);
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
            larger_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES);
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

-compile({inline, larger_INTERNAL4 / ?INTERNAL4_ARITY_EXCEPT_SIZES_PLUS1}).
larger_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES) ->
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

-compile({inline, larger_INTERNAL3 / ?INTERNAL3_ARITY_EXCEPT_SIZES_PLUS1}).
larger_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES) ->
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

-compile({inline, larger_INTERNAL2 / ?INTERNAL2_ARITY_EXCEPT_SIZES_PLUS1}).
larger_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES) ->
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

-compile({inline, larger_INTERNAL1 / ?INTERNAL1_ARITY_EXCEPT_SIZES_PLUS1}).
larger_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES) ->
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
        ?INTERNAL2_MATCH(_, _, _, _, _, _, C3) ->
            largest_recur(C3);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, _, _, _, C4) ->
            largest_recur(C4);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, _, _, _, _, C5) ->
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
        ?INTERNAL1_MATCH_NO_SIZES ->
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
    % TODO optimize like in filtermap
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
        ?INTERNAL2_MATCH_NO_SIZES ->
            Acc2 = map_recur(Fun, C1, Acc),
            Acc3 = map_elem(Fun, E1, Acc2),

            Acc4 = map_recur(Fun, C2, Acc3),
            Acc5 = map_elem(Fun, E2, Acc4),

            _Acc6 = map_recur(Fun, C3, Acc5);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            Acc2 = map_recur(Fun, C1, Acc),
            Acc3 = map_elem(Fun, E1, Acc2),

            Acc4 = map_recur(Fun, C2, Acc3),
            Acc5 = map_elem(Fun, E2, Acc4),

            Acc6 = map_recur(Fun, C3, Acc5),
            Acc7 = map_elem(Fun, E3, Acc6),

            _Acc8 = map_recur(Fun, C4, Acc7);
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
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

-compile({inline, map_elem/3}).
map_elem(Fun, Elem, Root) ->
    Mapped = Fun(Elem),
    add(Mapped, Root).

%% ------------------------------------------------------------------
%% Internal Function Definitions: merge/2
%% ------------------------------------------------------------------

merge_root(Node, Acc) ->
    case Node of
        ?INTERNAL1_MATCH_NO_SIZES ->
            Acc2 = merge_recur(C1, Acc),
            Acc3 = add(E1, Acc2),
            _Acc4 = merge_recur(C2, Acc3);
        %
        ?LEAF1_MATCH_ALL ->
            _Acc2 = add(E1, Acc);
        %
        ?LEAF0 ->
            Acc;
        %
        _ ->
            merge_recur(Node, Acc)
    end.

merge_recur(Node, Acc) ->
    % TODO optimize like in filter, filtermerge, union, etc
    case Node of
        ?LEAF2_MATCH_ALL ->
            add(E2, add(E1, Acc));
        %
        ?LEAF3_MATCH_ALL ->
            add(E3, add(E2, add(E1, Acc)));
        %
        ?LEAF4_MATCH_ALL ->
            add(E4, add(E3, add(E2, add(E1, Acc))));
        %
        ?INTERNAL2_MATCH_NO_SIZES ->
            Acc2 = add(E1, merge_recur(C1, Acc)),
            Acc3 = add(E2, merge_recur(C2, Acc2)),
            _Acc3 = merge_recur(C3, Acc3);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            Acc2 = add(E1, merge_recur(C1, Acc)),
            Acc3 = add(E2, merge_recur(C2, Acc2)),
            Acc4 = add(E3, merge_recur(C3, Acc3)),
            _Acc5 = merge_recur(C4, Acc4);
        %
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
            Acc2 = add(E1, merge_recur(C1, Acc)),
            Acc3 = add(E2, merge_recur(C2, Acc2)),
            Acc4 = add(E3, merge_recur(C3, Acc3)),
            Acc5 = add(E4, merge_recur(C4, Acc4)),
            _Acc6 = merge_recur(C5, Acc5)
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
%% Internal Function Definitions: nth/2
%% ------------------------------------------------------------------

nth_recur(Rank, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            nth_INTERNAL2(Rank, ?INTERNAL2_ARGS);
        %
        ?INTERNAL3_MATCH_ALL ->
            nth_INTERNAL3(Rank, ?INTERNAL3_ARGS);
        %
        ?INTERNAL4_MATCH_ALL ->
            nth_INTERNAL4(Rank, ?INTERNAL4_ARGS);
        %
        ?LEAF2_MATCH_ALL ->
            nth_LEAF2(Rank, ?LEAF2_ARGS);
        %
        ?LEAF3_MATCH_ALL ->
            nth_LEAF3(Rank, ?LEAF3_ARGS);
        %
        ?LEAF4_MATCH_ALL ->
            nth_LEAF4(Rank, ?LEAF4_ARGS)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, nth_INTERNAL4 / ?INTERNAL4_ARITY_PLUS1}).
nth_INTERNAL4(Rank, ?INTERNAL4_ARGS) ->
    if
        Rank < O3 ->
            if
                Rank > O1 ->
                    if
                        Rank < O2 ->
                            nth_recur(Rank - O1, C2);
                        %
                        Rank > O2 ->
                            nth_recur(Rank - O2, C3);
                        %
                        true ->
                            E2
                    end;
                %
                Rank < O1 ->
                    nth_recur(Rank, C1);
                %
                true ->
                    E1
            end;
        %
        Rank > O3 ->
            if
                Rank < O4 ->
                    nth_recur(Rank - O3, C4);
                %
                Rank > O4 ->
                    nth_recur(Rank - O4, C5);
                %
                true ->
                    E4
            end;
        %
        true ->
            E3
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, nth_INTERNAL3 / ?INTERNAL3_ARITY_PLUS1}).
nth_INTERNAL3(Rank, ?INTERNAL3_ARGS) ->
    if
        Rank < O2 ->
            if
                Rank < O1 ->
                    nth_recur(Rank, C1);
                %
                Rank > O1 ->
                    nth_recur(Rank - O1, C2);
                %
                true ->
                    E1
            end;
        %
        Rank > O2 ->
            if
                Rank < O3 ->
                    nth_recur(Rank - O2, C3);
                %
                Rank > O3 ->
                    nth_recur(Rank - O3, C4);
                %
                true ->
                    E3
            end;
        %
        true ->
            E2
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, nth_INTERNAL2 / ?INTERNAL2_ARITY_PLUS1}).
nth_INTERNAL2(Rank, ?INTERNAL2_ARGS) ->
    if
        Rank > O1 ->
            if
                Rank < O2 ->
                    nth_recur(Rank - O1, C2);
                %
                Rank > O2 ->
                    nth_recur(Rank - O2, C3);
                %
                true ->
                    E2
            end;
        %
        Rank < O1 ->
            nth_recur(Rank, C1);
        %
        true ->
            E1
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, nth_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
nth_INTERNAL1(Rank, ?INTERNAL1_ARGS) ->
    if
        Rank < O1 ->
            nth_recur(Rank, C1);
        %
        Rank > O1 ->
            nth_recur(Rank - O1, C2);
        %
        true ->
            E1
    end.

%%
%% ?LEAF4
%%

-compile({inline, nth_LEAF4 / ?LEAF4_ARITY_PLUS1}).
nth_LEAF4(Rank, ?LEAF4_ARGS) ->
    case Rank of
        1 -> E1;
        2 -> E2;
        3 -> E3;
        4 -> E4
    end.

%%
%% ?LEAF3
%%

-compile({inline, nth_LEAF3 / ?LEAF3_ARITY_PLUS1}).
nth_LEAF3(Rank, ?LEAF3_ARGS) ->
    case Rank of
        1 -> E1;
        2 -> E2;
        3 -> E3
    end.

%%
%% ?LEAF2
%%

-compile({inline, nth_LEAF2 / ?LEAF2_ARITY_PLUS1}).
nth_LEAF2(Rank, ?LEAF2_ARGS) ->
    case Rank of
        1 -> E1;
        2 -> E2
    end.

%%
%% ?LEAF1
%%

-compile({inline, nth_LEAF1 / ?LEAF1_ARITY_PLUS1}).
nth_LEAF1(1, ?LEAF1_ARGS) ->
    E1.

%% ------------------------------------------------------------------
%% Internal Function Definitions: nth_and_nthp1/2
%% ------------------------------------------------------------------

nth_and_nthp1_recur(Rank, Node, Plus1) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            nth_and_nthp1_INTERNAL2(Rank, ?INTERNAL2_ARGS, Plus1);
        %
        ?INTERNAL3_MATCH_ALL ->
            nth_and_nthp1_INTERNAL3(Rank, ?INTERNAL3_ARGS, Plus1);
        %
        ?INTERNAL4_MATCH_ALL ->
            nth_and_nthp1_INTERNAL4(Rank, ?INTERNAL4_ARGS, Plus1);
        %
        ?LEAF2_MATCH_ALL ->
            nth_and_nthp1_LEAF2(Rank, ?LEAF2_ARGS, Plus1);
        %
        ?LEAF3_MATCH_ALL ->
            nth_and_nthp1_LEAF3(Rank, ?LEAF3_ARGS, Plus1);
        %
        ?LEAF4_MATCH_ALL ->
            nth_and_nthp1_LEAF4(Rank, ?LEAF4_ARGS, Plus1)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, nth_and_nthp1_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
nth_and_nthp1_INTERNAL4(Rank, ?INTERNAL4_ARGS, Plus1) ->
    if
        Rank < O3 ->
            if
                Rank > O1 ->
                    if
                        Rank < O2 ->
                            nth_and_nthp1_recur(Rank - O1, C2, E2);
                        %
                        Rank > O2 ->
                            nth_and_nthp1_recur(Rank - O2, C3, E3);
                        %
                        true ->
                            [E2 | smallest_recur(C3)]
                    end;
                %
                Rank < O1 ->
                    nth_and_nthp1_recur(Rank, C1, E1);
                %
                true ->
                    [E1 | smallest_recur(C2)]
            end;
        %
        Rank > O3 ->
            if
                Rank < O4 ->
                    nth_and_nthp1_recur(Rank - O3, C4, E4);
                %
                Rank > O4 ->
                    nth_and_nthp1_recur(Rank - O4, C5, Plus1);
                %
                true ->
                    [E4 | smallest_recur(C5)]
            end;
        %
        true ->
            [E3 | smallest_recur(C4)]
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, nth_and_nthp1_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
nth_and_nthp1_INTERNAL3(Rank, ?INTERNAL3_ARGS, Plus1) ->
    if
        Rank < O2 ->
            if
                Rank < O1 ->
                    nth_and_nthp1_recur(Rank, C1, E1);
                %
                Rank > O1 ->
                    nth_and_nthp1_recur(Rank - O1, C2, E2);
                %
                true ->
                    [E1 | smallest_recur(C2)]
            end;
        %
        Rank > O2 ->
            if
                Rank < O3 ->
                    nth_and_nthp1_recur(Rank - O2, C3, E3);
                %
                Rank > O3 ->
                    nth_and_nthp1_recur(Rank - O3, C4, Plus1);
                %
                true ->
                    [E3 | smallest_recur(C4)]
            end;
        %
        true ->
            [E2 | smallest_recur(C3)]
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, nth_and_nthp1_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
nth_and_nthp1_INTERNAL2(Rank, ?INTERNAL2_ARGS, Plus1) ->
    if
        Rank > O1 ->
            if
                Rank < O2 ->
                    nth_and_nthp1_recur(Rank - O1, C2, E2);
                %
                Rank > O2 ->
                    nth_and_nthp1_recur(Rank - O2, C3, Plus1);
                %
                true ->
                    [E2 | smallest_recur(C3)]
            end;
        %
        Rank < O1 ->
            nth_and_nthp1_recur(Rank, C1, E1);
        %
        true ->
            [E1 | smallest_recur(C2)]
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, nth_and_nthp1_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
nth_and_nthp1_INTERNAL1(Rank, ?INTERNAL1_ARGS) ->
    if
        Rank < O1 ->
            nth_and_nthp1_recur(Rank, C1, E1);
        %
        Rank > O1 ->
            nth_and_nthp1_recur(Rank - O1, C2, nil);
        %
        true ->
            [E1 | smallest_recur(C2)]
    end.

%%
%% ?LEAF4
%%

-compile({inline, nth_and_nthp1_LEAF4 / ?LEAF4_ARITY_PLUS2}).
nth_and_nthp1_LEAF4(Rank, ?LEAF4_ARGS, Plus1) ->
    case Rank of
        1 -> [E1 | E2];
        2 -> [E2 | E3];
        3 -> [E3 | E4];
        4 -> [E4 | Plus1]
    end.

%%
%% ?LEAF3
%%

-compile({inline, nth_and_nthp1_LEAF3 / ?LEAF3_ARITY_PLUS2}).
nth_and_nthp1_LEAF3(Rank, ?LEAF3_ARGS, Plus1) ->
    case Rank of
        1 -> [E1 | E2];
        2 -> [E2 | E3];
        3 -> [E3 | Plus1]
    end.

%%
%% ?LEAF2
%%

-compile({inline, nth_and_nthp1_LEAF2 / ?LEAF2_ARITY_PLUS2}).
nth_and_nthp1_LEAF2(Rank, ?LEAF2_ARGS, Plus1) ->
    case Rank of
        1 -> [E1 | E2];
        2 -> [E2 | Plus1]
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: rank/2
%% ------------------------------------------------------------------

rank_recur(Elem, Node, Acc) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            rank_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            rank_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            rank_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc);
        %
        ?LEAF2_MATCH_ALL ->
            rank_LEAF2(Elem, ?LEAF2_ARGS, Acc);
        %
        ?LEAF3_MATCH_ALL ->
            rank_LEAF3(Elem, ?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            rank_LEAF4(Elem, ?LEAF4_ARGS, Acc)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, rank_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
rank_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc) ->
    if
        Elem =< E2 ->
            if
                Elem =< E1 ->
                    case rank_recur(Elem, C1, Acc) of
                        none when Elem == E1 -> Acc + O1;
                        Rank -> Rank
                    end;
                %
                true ->
                    case rank_recur(Elem, C2, Acc + O1) of
                        none when Elem == E2 -> Acc + O2;
                        Rank -> Rank
                    end
            end;
        %
        Elem =< E3 ->
            case rank_recur(Elem, C3, Acc + O2) of
                none when Elem == E3 -> Acc + O3;
                Rank -> Rank
            end;
        %
        Elem =< E4 ->
            case rank_recur(Elem, C4, Acc + O3) of
                none when Elem == E4 -> Acc + O4;
                Rank -> Rank
            end;
        %
        true ->
            rank_recur(Elem, C5, Acc + O4)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, rank_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
rank_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc) ->
    if
        Elem =< E2 ->
            if
                Elem =< E1 ->
                    case rank_recur(Elem, C1, Acc) of
                        none when Elem == E1 -> Acc + O1;
                        Rank -> Rank
                    end;
                %
                true ->
                    case rank_recur(Elem, C2, Acc + O1) of
                        none when Elem == E2 -> Acc + O2;
                        Rank -> Rank
                    end
            end;
        %
        Elem =< E3 ->
            case rank_recur(Elem, C3, Acc + O2) of
                none when Elem == E3 -> Acc + O3;
                Rank -> Rank
            end;
        %
        true ->
            rank_recur(Elem, C4, Acc + O3)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, rank_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
rank_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc) ->
    if
        Elem =< E1 ->
            case rank_recur(Elem, C1, Acc) of
                none when Elem == E1 -> Acc + O1;
                Rank -> Rank
            end;
        %
        Elem =< E2 ->
            case rank_recur(Elem, C2, Acc + O1) of
                none when Elem == E2 -> Acc + O2;
                Rank -> Rank
            end;
        %
        true ->
            rank_recur(Elem, C3, Acc + O2)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, rank_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
rank_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem < E1 ->
            rank_recur(Elem, C1, 0);
        %
        Elem > E1 ->
            rank_recur(Elem, C2, O1);
        %
        true ->
            case rank_recur(Elem, C1, 0) of
                none -> O1;
                Rank -> Rank
            end
    end.

%%
%% ?LEAF4
%%

-compile({inline, rank_LEAF4 / ?LEAF4_ARITY_PLUS2}).
rank_LEAF4(Elem, ?LEAF4_ARGS, Acc) ->
    if
        Elem == E1 ->
            Acc + 1;
        %
        Elem == E2 ->
            Acc + 2;
        %
        Elem == E3 ->
            Acc + 3;
        %
        Elem == E4 ->
            Acc + 4;
        %
        true ->
            none
    end.

%%
%% ?LEAF3
%%

-compile({inline, rank_LEAF3 / ?LEAF3_ARITY_PLUS2}).
rank_LEAF3(Elem, ?LEAF3_ARGS, Acc) ->
    if
        Elem == E1 ->
            Acc + 1;
        %
        Elem == E2 ->
            Acc + 2;
        %
        Elem == E3 ->
            Acc + 3;
        %
        true ->
            none
    end.

%%
%% ?LEAF2
%%

-compile({inline, rank_LEAF2 / ?LEAF2_ARITY_PLUS2}).
rank_LEAF2(Elem, ?LEAF2_ARGS, Acc) ->
    if
        Elem == E1 ->
            Acc + 1;
        %
        Elem == E2 ->
            Acc + 2;
        %
        true ->
            none
    end.

%%
%% ?LEAF1
%%

-compile({inline, rank_LEAF1 / ?LEAF1_ARITY_PLUS1}).
rank_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem == E1 ->
            1;
        %
        true ->
            none
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: rank_larger/2
%% ------------------------------------------------------------------

rank_larger_recur(Elem, Node, Acc) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            rank_larger_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            rank_larger_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            rank_larger_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc);
        %
        ?LEAF2_MATCH_ALL ->
            rank_larger_LEAF2(Elem, ?LEAF2_ARGS, Acc);
        %
        ?LEAF3_MATCH_ALL ->
            rank_larger_LEAF3(Elem, ?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            rank_larger_LEAF4(Elem, ?LEAF4_ARGS, Acc)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, rank_larger_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
rank_larger_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc) ->
    if
        Elem < E2 ->
            if
                Elem < E1 ->
                    case rank_larger_recur(Elem, C1, Acc) of
                        none -> [Acc + O1 | E1];
                        Rank -> Rank
                    end;
                %
                true ->
                    case rank_larger_recur(Elem, C2, Acc + O1) of
                        none -> [Acc + O2 | E2];
                        Rank -> Rank
                    end
            end;
        %
        Elem < E3 ->
            case rank_larger_recur(Elem, C3, Acc + O2) of
                none -> [Acc + O3 | E3];
                Rank -> Rank
            end;
        %
        Elem < E4 ->
            case rank_larger_recur(Elem, C4, Acc + O3) of
                none -> [Acc + O4 | E4];
                Rank -> Rank
            end;
        %
        true ->
            rank_larger_recur(Elem, C5, Acc + O4)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, rank_larger_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
rank_larger_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc) ->
    if
        Elem < E2 ->
            if
                Elem < E1 ->
                    case rank_larger_recur(Elem, C1, Acc) of
                        none -> [Acc + O1 | E1];
                        Rank -> Rank
                    end;
                %
                true ->
                    case rank_larger_recur(Elem, C2, Acc + O1) of
                        none -> [Acc + O2 | E2];
                        Rank -> Rank
                    end
            end;
        %
        Elem < E3 ->
            case rank_larger_recur(Elem, C3, Acc + O2) of
                none -> [Acc + O3 | E3];
                Rank -> Rank
            end;
        %
        true ->
            rank_larger_recur(Elem, C4, Acc + O3)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, rank_larger_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
rank_larger_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc) ->
    if
        Elem < E2 ->
            if
                Elem < E1 ->
                    case rank_larger_recur(Elem, C1, Acc) of
                        none -> [Acc + O1 | E1];
                        Rank -> Rank
                    end;
                %
                true ->
                    case rank_larger_recur(Elem, C2, Acc + O1) of
                        none -> [Acc + O2 | E2];
                        Rank -> Rank
                    end
            end;
        %
        true ->
            rank_larger_recur(Elem, C3, Acc + O2)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, rank_larger_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
rank_larger_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem < E1 ->
            case rank_larger_recur(Elem, C1, 0) of
                none -> [O1 | E1];
                Rank -> Rank
            end;
        %
        true ->
            Acc = O1,
            rank_larger_recur(Elem, C2, Acc)
    end.

%%
%% ?LEAF4
%%

-compile({inline, rank_larger_LEAF4 / ?LEAF4_ARITY_PLUS2}).
rank_larger_LEAF4(Elem, ?LEAF4_ARGS, Acc) ->
    if
        Elem < E2 ->
            if
                Elem < E1 ->
                    [Acc + 1 | E1];
                %
                true ->
                    [Acc + 2 | E2]
            end;
        %
        Elem < E3 ->
            [Acc + 3 | E3];
        %
        Elem < E4 ->
            [Acc + 4 | E4];
        %
        true ->
            none
    end.

%%
%% ?LEAF3
%%

-compile({inline, rank_larger_LEAF3 / ?LEAF3_ARITY_PLUS2}).
rank_larger_LEAF3(Elem, ?LEAF3_ARGS, Acc) ->
    if
        Elem < E1 ->
            [Acc + 1 | E1];
        %
        Elem < E2 ->
            [Acc + 2 | E2];
        %
        Elem < E3 ->
            [Acc + 3 | E3];
        %
        true ->
            none
    end.

%%
%% ?LEAF2
%%

-compile({inline, rank_larger_LEAF2 / ?LEAF2_ARITY_PLUS2}).
rank_larger_LEAF2(Elem, ?LEAF2_ARGS, Acc) ->
    if
        Elem < E1 ->
            [Acc + 1 | E1];
        %
        Elem < E2 ->
            [Acc + 2 | E2];
        %
        true ->
            none
    end.

%%
%% ?LEAF1
%%

-compile({inline, rank_larger_LEAF1 / ?LEAF1_ARITY_PLUS1}).
rank_larger_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem < E1 ->
            [1 | E1];
        %
        true ->
            none
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: rank_smaller/2
%% ------------------------------------------------------------------

rank_smaller_recur(Elem, Node, Acc) ->
    case Node of
        %
        ?INTERNAL2_MATCH_ALL ->
            rank_smaller_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc);
        %
        ?INTERNAL3_MATCH_ALL ->
            rank_smaller_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc);
        %
        ?INTERNAL4_MATCH_ALL ->
            rank_smaller_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc);
        %
        ?LEAF2_MATCH_ALL ->
            rank_smaller_LEAF2(Elem, ?LEAF2_ARGS, Acc);
        %
        ?LEAF3_MATCH_ALL ->
            rank_smaller_LEAF3(Elem, ?LEAF3_ARGS, Acc);
        %
        ?LEAF4_MATCH_ALL ->
            rank_smaller_LEAF4(Elem, ?LEAF4_ARGS, Acc)
    end.

%%
%% ?INTERNAL4
%%

-compile({inline, rank_smaller_INTERNAL4 / ?INTERNAL4_ARITY_PLUS2}).
rank_smaller_INTERNAL4(Elem, ?INTERNAL4_ARGS, Acc) ->
    if
        Elem > E3 ->
            if
                Elem > E4 ->
                    UpdatedAcc = Acc + O4,
                    case rank_smaller_recur(Elem, C5, UpdatedAcc) of
                        none -> [UpdatedAcc | E4];
                        Rank -> Rank
                    end;
                %
                true ->
                    UpdatedAcc = Acc + O3,
                    case rank_smaller_recur(Elem, C4, UpdatedAcc) of
                        none -> [UpdatedAcc | E3];
                        Rank -> Rank
                    end
            end;
        %
        Elem > E2 ->
            UpdatedAcc = Acc + O2,
            case rank_smaller_recur(Elem, C3, UpdatedAcc) of
                none -> [UpdatedAcc | E2];
                Rank -> Rank
            end;
        %
        Elem > E1 ->
            UpdatedAcc = Acc + O1,
            case rank_smaller_recur(Elem, C2, UpdatedAcc) of
                none -> [UpdatedAcc | E1];
                Rank -> Rank
            end;
        %
        true ->
            rank_smaller_recur(Elem, C1, Acc)
    end.

%%
%% ?INTERNAL3
%%

-compile({inline, rank_smaller_INTERNAL3 / ?INTERNAL3_ARITY_PLUS2}).
rank_smaller_INTERNAL3(Elem, ?INTERNAL3_ARGS, Acc) ->
    if
        Elem > E2 ->
            if
                Elem > E3 ->
                    UpdatedAcc = Acc + O3,
                    case rank_smaller_recur(Elem, C4, UpdatedAcc) of
                        none -> [UpdatedAcc | E3];
                        Rank -> Rank
                    end;
                %
                true ->
                    UpdatedAcc = Acc + O2,
                    case rank_smaller_recur(Elem, C3, UpdatedAcc) of
                        none -> [UpdatedAcc | E2];
                        Rank -> Rank
                    end
            end;
        %
        Elem > E1 ->
            UpdatedAcc = Acc + O1,
            case rank_smaller_recur(Elem, C2, UpdatedAcc) of
                none -> [UpdatedAcc | E1];
                Rank -> Rank
            end;
        %
        true ->
            rank_smaller_recur(Elem, C1, Acc)
    end.

%%
%% ?INTERNAL2
%%

-compile({inline, rank_smaller_INTERNAL2 / ?INTERNAL2_ARITY_PLUS2}).
rank_smaller_INTERNAL2(Elem, ?INTERNAL2_ARGS, Acc) ->
    if
        Elem > E1 ->
            if
                Elem > E2 ->
                    UpdatedAcc = Acc + O2,
                    case rank_smaller_recur(Elem, C3, UpdatedAcc) of
                        none -> [UpdatedAcc | E2];
                        Rank -> Rank
                    end;
                %
                true ->
                    UpdatedAcc = Acc + O1,
                    case rank_smaller_recur(Elem, C2, UpdatedAcc) of
                        none -> [UpdatedAcc | E1];
                        Rank -> Rank
                    end
            end;
        %
        true ->
            rank_smaller_recur(Elem, C1, Acc)
    end.

%%
%% ?INTERNAL1
%%

-compile({inline, rank_smaller_INTERNAL1 / ?INTERNAL1_ARITY_PLUS1}).
rank_smaller_INTERNAL1(Elem, ?INTERNAL1_ARGS) ->
    if
        Elem > E1 ->
            case rank_smaller_recur(Elem, C2, O1) of
                none -> [O1 | E1];
                Rank -> Rank
            end;
        %
        true ->
            rank_smaller_recur(Elem, C1, 0)
    end.

%%
%% ?LEAF4
%%

-compile({inline, rank_smaller_LEAF4 / ?LEAF4_ARITY_PLUS2}).
rank_smaller_LEAF4(Elem, ?LEAF4_ARGS, Acc) ->
    if
        Elem > E3 ->
            if
                Elem > E4 ->
                    [Acc + 4 | E4];
                %
                true ->
                    [Acc + 3 | E3]
            end;
        %
        Elem > E2 ->
            [Acc + 2 | E2];
        %
        Elem > E1 ->
            [Acc + 1 | E1];
        %
        true ->
            none
    end.

%%
%% ?LEAF3
%%

-compile({inline, rank_smaller_LEAF3 / ?LEAF3_ARITY_PLUS2}).
rank_smaller_LEAF3(Elem, ?LEAF3_ARGS, Acc) ->
    if
        Elem > E3 ->
            [Acc + 3 | E3];
        %
        Elem > E2 ->
            [Acc + 2 | E2];
        %
        Elem > E1 ->
            [Acc + 1 | E1];
        %
        true ->
            none
    end.

%%
%% ?LEAF2
%%

-compile({inline, rank_smaller_LEAF2 / ?LEAF2_ARITY_PLUS2}).
rank_smaller_LEAF2(Elem, ?LEAF2_ARGS, Acc) ->
    if
        Elem > E2 ->
            [Acc + 2 | E2];
        %
        Elem > E1 ->
            [Acc + 1 | E1];
        %
        true ->
            none
    end.

%%
%% ?LEAF1
%%

-compile({inline, rank_smaller_LEAF1 / ?LEAF1_ARITY_PLUS1}).
rank_smaller_LEAF1(Elem, ?LEAF1_ARGS) ->
    if
        Elem > E1 ->
            [1 | E1];
        %
        true ->
            none
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: smaller/2
%% ------------------------------------------------------------------

smaller_recur(Elem, Node) ->
    case Node of
        %
        ?INTERNAL2_MATCH_NO_SIZES ->
            smaller_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            smaller_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES);
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
            smaller_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES);
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

-compile({inline, smaller_INTERNAL4 / ?INTERNAL4_ARITY_EXCEPT_SIZES_PLUS1}).
smaller_INTERNAL4(Elem, ?INTERNAL4_ARGS_EXCEPT_SIZES) ->
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

-compile({inline, smaller_INTERNAL3 / ?INTERNAL3_ARITY_EXCEPT_SIZES_PLUS1}).
smaller_INTERNAL3(Elem, ?INTERNAL3_ARGS_EXCEPT_SIZES) ->
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

-compile({inline, smaller_INTERNAL2 / ?INTERNAL2_ARITY_EXCEPT_SIZES_PLUS1}).
smaller_INTERNAL2(Elem, ?INTERNAL2_ARGS_EXCEPT_SIZES) ->
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

-compile({inline, smaller_INTERNAL1 / ?INTERNAL1_ARITY_EXCEPT_SIZES_PLUS1}).
smaller_INTERNAL1(Elem, ?INTERNAL1_ARGS_EXCEPT_SIZES) ->
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
        ?INTERNAL2_MATCH(_, _, _, _, C1, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, C1, _, _, _) ->
            smallest_recur(C1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, C1, _, _, _, _) ->
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
        ?INTERNAL2_MATCH(_, _, _, _, C1, C2, C3) ->
            Acc2 = xb5_structural_stats:inc_count(internal2, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            _Acc5 = structural_stats_recur(C3, Acc4, Height + 1);
        %
        ?INTERNAL3_MATCH(_, _, _, _, _, _, C1, C2, C3, C4) ->
            Acc2 = xb5_structural_stats:inc_count(internal3, Acc),
            Acc3 = structural_stats_recur(C1, Acc2, Height + 1),
            Acc4 = structural_stats_recur(C2, Acc3, Height + 1),
            Acc5 = structural_stats_recur(C3, Acc4, Height + 1),
            _Acc6 = structural_stats_recur(C4, Acc5, Height + 1);
        %
        ?INTERNAL4_MATCH(_, _, _, _, _, _, _, _, C1, C2, C3, C4, C5) ->
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

%% TODO continue from here

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
        ?INTERNAL2_MATCH_NO_SIZES ->
            Acc2 = to_list_recur(C3, Acc),
            Acc3 = to_list_recur(C2, [E2 | Acc2]),
            _Acc4 = to_list_recur(C1, [E1 | Acc3]);
        %
        ?INTERNAL3_MATCH_NO_SIZES ->
            Acc2 = to_list_recur(C4, Acc),
            Acc3 = to_list_recur(C3, [E3 | Acc2]),
            Acc4 = to_list_recur(C2, [E2 | Acc3]),
            _Acc5 = to_list_recur(C1, [E1 | Acc4]);
        %
        ?INTERNAL4_MATCH_NO_SIZES ->
            Acc2 = to_list_recur(C5, Acc),
            Acc3 = to_list_recur(C4, [E4 | Acc2]),
            Acc4 = to_list_recur(C3, [E3 | Acc3]),
            Acc5 = to_list_recur(C2, [E2 | Acc4]),
            _Acc6 = to_list_recur(C1, [E1 | Acc5])
    end.

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
            case ins_rebalance_into_right_sibling_maybe(Elem, C1, O1, Pos, Args, E1, C2) of
                {UpElem, MovedSize, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL4(
                        UpElem,
                        E2,
                        E3,
                        E4,
                        %
                        O1 - MovedSize,
                        O2 + 1,
                        O3 + 1,
                        O4 + 1,
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
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                O1 + 1,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                %
                UpdatedC1,
                C2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, ins_rebalance_INTERNAL4_C2 / ?INTERNAL4_ARITY_PLUS2}).
ins_rebalance_INTERNAL4_C2(Result, Elem, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C2, Pos, Args, E1, C1, O1) of
                {UpElem, MovedSize, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL4(
                        UpElem,
                        E2,
                        E3,
                        E4,
                        %
                        O1 + MovedSize,
                        O2 + 1,
                        O3 + 1,
                        O4 + 1,
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
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                O1,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                %
                C1,
                UpdatedC2,
                C3,
                C4,
                C5
            )
    end.

-compile({inline, ins_rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY_PLUS2}).
ins_rebalance_INTERNAL4_C3(Result, Elem, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C3, Pos, Args, E2, C2, O2 - O1) of
                {UpElem, MovedSize, UpdatedC2, UpdatedC3} ->
                    ?new_INTERNAL4(
                        E1,
                        UpElem,
                        E3,
                        E4,
                        %
                        O1,
                        O2 + MovedSize,
                        O3 + 1,
                        O4 + 1,
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
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                O1,
                O2,
                O3 + 1,
                O4 + 1,
                %
                C1,
                C2,
                UpdatedC3,
                C4,
                C5
            )
    end.

-compile({inline, ins_rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY_PLUS2}).
ins_rebalance_INTERNAL4_C4(Result, Elem, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C4, Pos, Args, E3, C3, O3 - O2) of
                {UpElem, MovedSize, UpdatedC3, UpdatedC4} ->
                    ?new_INTERNAL4(
                        E1,
                        E2,
                        UpElem,
                        E4,
                        %
                        O1,
                        O2,
                        O3 + MovedSize,
                        O4 + 1,
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
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                O1,
                O2,
                O3,
                O4 + 1,
                %
                C1,
                C2,
                C3,
                UpdatedC4,
                C5
            )
    end.

-compile({inline, ins_rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY_PLUS2}).
ins_rebalance_INTERNAL4_C5(Result, Elem, ?INTERNAL4_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C5, Pos, Args, E4, C4, O4 - O3) of
                {UpElem, MovedSize, UpdatedC4, UpdatedC5} ->
                    ?new_INTERNAL4(
                        E1,
                        E2,
                        E3,
                        UpElem,
                        %
                        O1,
                        O2,
                        O3,
                        O4 + MovedSize,
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
            case ins_rebalance_into_right_sibling_maybe(Elem, C1, O1, Pos, Args, E1, C2) of
                {UpElem, MovedSize, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL3(
                        UpElem,
                        E2,
                        E3,
                        %
                        O1 - MovedSize,
                        O2 + 1,
                        O3 + 1,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3,
                        C4
                    );
                %
                {split, SplitE, SplitO, SplitL, SplitR} ->
                    ?new_INTERNAL4(
                        SplitE,
                        E1,
                        E2,
                        E3,
                        %
                        SplitO,
                        O1 + 1,
                        O2 + 1,
                        O3 + 1,
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
            ?new_INTERNAL3(
                E1,
                E2,
                E3,
                %
                O1 + 1,
                O2 + 1,
                O3 + 1,
                %
                UpdatedC1,
                C2,
                C3,
                C4
            )
    end.

-compile({inline, ins_rebalance_INTERNAL3_C2 / ?INTERNAL3_ARITY_PLUS2}).
ins_rebalance_INTERNAL3_C2(Result, Elem, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C2, Pos, Args, E1, C1, O1) of
                {UpElem, MovedSize, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL3(
                        UpElem,
                        E2,
                        E3,
                        %
                        O1 + MovedSize,
                        O2 + 1,
                        O3 + 1,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3,
                        C4
                    );
                %
                {split, SplitE, SplitO, SplitL, SplitR} ->
                    ?new_INTERNAL4(
                        E1,
                        SplitE,
                        E2,
                        E3,
                        %
                        O1,
                        O1 + SplitO,
                        O2 + 1,
                        O3 + 1,
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
            ?new_INTERNAL3(
                E1,
                E2,
                E3,
                %
                O1,
                O2 + 1,
                O3 + 1,
                %
                C1,
                UpdatedC2,
                C3,
                C4
            )
    end.

-compile({inline, ins_rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY_PLUS2}).
ins_rebalance_INTERNAL3_C3(Result, Elem, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C3, Pos, Args, E2, C2, O2 - O1) of
                {UpElem, MovedSize, UpdatedC2, UpdatedC3} ->
                    ?new_INTERNAL3(
                        E1,
                        UpElem,
                        E3,
                        %
                        O1,
                        O2 + MovedSize,
                        O3 + 1,
                        %
                        C1,
                        UpdatedC2,
                        UpdatedC3,
                        C4
                    );
                %
                {split, SplitE, SplitO, SplitL, SplitR} ->
                    ?new_INTERNAL4(
                        E1,
                        E2,
                        SplitE,
                        E3,
                        %
                        O1,
                        O2,
                        O2 + SplitO,
                        O3 + 1,
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
            ?new_INTERNAL3(
                E1,
                E2,
                E3,
                %
                O1,
                O2,
                O3 + 1,
                %
                C1,
                C2,
                UpdatedC3,
                C4
            )
    end.

-compile({inline, ins_rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY_PLUS2}).
ins_rebalance_INTERNAL3_C4(Result, Elem, ?INTERNAL3_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C4, Pos, Args, E3, C3, O3 - O2) of
                {UpElem, MovedSize, UpdatedC3, UpdatedC4} ->
                    ?new_INTERNAL3(
                        E1,
                        E2,
                        UpElem,
                        %
                        O1,
                        O2,
                        O3 + MovedSize,
                        %
                        C1,
                        C2,
                        UpdatedC3,
                        UpdatedC4
                    );
                %
                {split, SplitE, SplitO, SplitL, SplitR} ->
                    ?new_INTERNAL4(
                        E1,
                        E2,
                        E3,
                        SplitE,
                        %
                        O1,
                        O2,
                        O3,
                        O3 + SplitO,
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
            case ins_rebalance_into_right_sibling_maybe(Elem, C1, O1, Pos, Args, E1, C2) of
                {UpElem, MovedSize, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL2(
                        UpElem,
                        E2,
                        %
                        O1 - MovedSize,
                        O2 + 1,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3
                    );
                %
                {split, SplitE, SplitO, SplitL, SplitR} ->
                    ?new_INTERNAL3(
                        SplitE,
                        E1,
                        E2,
                        %
                        SplitO,
                        O1 + 1,
                        O2 + 1,
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
            ?new_INTERNAL2(
                E1,
                E2,
                %
                O1 + 1,
                O2 + 1,
                %
                UpdatedC1,
                C2,
                C3
            )
    end.

-compile({inline, ins_rebalance_INTERNAL2_C2 / ?INTERNAL2_ARITY_PLUS2}).
ins_rebalance_INTERNAL2_C2(Result, Elem, ?INTERNAL2_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C2, Pos, Args, E1, C1, O1) of
                {UpElem, MovedSize, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL2(
                        UpElem,
                        E2,
                        %
                        O1 + MovedSize,
                        O2 + 1,
                        %
                        UpdatedC1,
                        UpdatedC2,
                        C3
                    );
                %
                {split, SplitE, SplitO, SplitL, SplitR} ->
                    ?new_INTERNAL3(
                        E1,
                        SplitE,
                        E2,
                        %
                        O1,
                        O1 + SplitO,
                        O2 + 1,
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
            ?new_INTERNAL2(
                E1,
                E2,
                %
                O1,
                O2 + 1,
                %
                C1,
                UpdatedC2,
                C3
            )
    end.

-compile({inline, ins_rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY_PLUS2}).
ins_rebalance_INTERNAL2_C3(Result, Elem, ?INTERNAL2_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C3, Pos, Args, E2, C2, O2 - O1) of
                {UpElem, MovedSize, UpdatedC2, UpdatedC3} ->
                    ?new_INTERNAL2(
                        E1,
                        UpElem,
                        %
                        O1,
                        O2 + MovedSize,
                        %
                        C1,
                        UpdatedC2,
                        UpdatedC3
                    );
                %
                {split, SplitE, SplitO, SplitL, SplitR} ->
                    ?new_INTERNAL3(
                        E1,
                        E2,
                        SplitE,
                        %
                        O1,
                        O2,
                        O2 + SplitO,
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
            case ins_rebalance_into_right_sibling_maybe(Elem, C1, O1, Pos, Args, E1, C2) of
                {UpElem, MovedSize, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL1(
                        UpElem,
                        %
                        O1 - MovedSize,
                        %
                        UpdatedC1,
                        UpdatedC2
                    );
                %
                {split, SplitE, SplitO, SplitL, SplitR} ->
                    ?new_INTERNAL2(
                        SplitE,
                        E1,
                        %
                        SplitO,
                        O1 + 1,
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
            ?new_INTERNAL1(
                E1,
                %
                O1 + 1,
                %
                UpdatedC1,
                C2
            )
    end.

-compile({inline, ins_rebalance_INTERNAL1_C2 / ?INTERNAL1_ARITY_PLUS2}).
ins_rebalance_INTERNAL1_C2(Result, Elem, ?INTERNAL1_ARGS) ->
    case Result of
        ?SPLIT_MATCH(Pos, Args) ->
            case ins_rebalance_into_left_sibling_maybe(Elem, C2, Pos, Args, E1, C1, O1) of
                {UpElem, MovedSize, UpdatedC1, UpdatedC2} ->
                    ?new_INTERNAL1(
                        UpElem,
                        %
                        O1 + MovedSize,
                        %
                        UpdatedC1,
                        UpdatedC2
                    );
                %
                {split, SplitE, SplitO, SplitL, SplitR} ->
                    ?new_INTERNAL2(
                        E1,
                        SplitE,
                        %
                        O1,
                        O1 + SplitO,
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
            ?new_INTERNAL1(
                E1,
                %
                O1,
                %
                C1,
                UpdatedC2
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Node Split
%% ------------------------------------------------------------------

insert_split_root(NewElem, Pos, [], Root) ->
    ?LEAF4_MATCH_ALL = Root,

    case Pos of
        1 ->
            ?new_INTERNAL1(
                E2,
                3,
                ?new_LEAF2(NewElem, E1),
                ?new_LEAF2(E3, E4)
            );
        %
        2 ->
            ?new_INTERNAL1(
                E2,
                3,
                ?new_LEAF2(E1, NewElem),
                ?new_LEAF2(E3, E4)
            );
        %
        3 ->
            ?new_INTERNAL1(
                NewElem,
                3,
                ?new_LEAF2(E1, E2),
                ?new_LEAF2(E3, E4)
            );
        %
        4 ->
            ?new_INTERNAL1(
                E3,
                3,
                ?new_LEAF2(E1, E2),
                ?new_LEAF2(NewElem, E4)
            );
        %
        5 ->
            ?new_INTERNAL1(
                E3,
                3,
                ?new_LEAF2(E1, E2),
                ?new_LEAF2(E4, NewElem)
            )
    end;
insert_split_root(_NewElem, Pos, {split, SplitE, SplitO, SplitL, SplitR}, Root) ->
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
                SplitO,
                O1 + 1,
                O2 + 1,
                O3 + 1,
                O4 + 1,
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
                O1,
                O1 + SplitO,
                O2 + 1,
                O3 + 1,
                O4 + 1,
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
                O1,
                O2,
                O2 + SplitO,
                O3 + 1,
                O4 + 1,
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
                O1,
                O2,
                O3,
                O3 + SplitO,
                O4 + 1,
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
                O1,
                O2,
                O3,
                O4,
                O4 + SplitO,
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
    pos_integer(),
    pos_integer(),
    pos_integer(),
    pos_integer(),
    pos_integer(),
    %
    C,
    C,
    C,
    C,
    C,
    C
) -> node_INTERNAL1(E) when
    C :: nonempty_node(E).
-compile({inline, insert_split_root_internal/16}).
insert_split_root_internal(
    E1,
    E2,
    E3,
    E4,
    E5,
    %
    O1,
    O2,
    O3,
    O4,
    O5,
    %
    C1,
    C2,
    C3,
    C4,
    C5,
    C6
) ->
    SplitE = E3,
    SplitO = O3,

    SplitL = ?new_INTERNAL2(
        E1,
        E2,
        %
        O1,
        O2,
        %
        C1,
        C2,
        C3
    ),

    SplitR = ?new_INTERNAL2(
        E4,
        E5,
        %
        O4 - O3,
        O5 - O3,
        %
        C4,
        C5,
        C6
    ),

    ?new_INTERNAL1(SplitE, SplitO, SplitL, SplitR).

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
    pos_integer(),
    pos_integer(),
    pos_integer(),
    pos_integer(),
    pos_integer(),
    %
    C,
    C,
    C,
    C,
    C,
    C
) -> split_internal_result(E) when
    C :: nonempty_node(E).
-compile({inline, split_internal/16}).
split_internal(
    E1,
    E2,
    E3,
    E4,
    E5,
    %
    O1,
    O2,
    O3,
    O4,
    O5,
    %
    C1,
    C2,
    C3,
    C4,
    C5,
    C6
) ->
    SplitE = E3,
    SplitO = O3,

    SplitL = ?new_INTERNAL2(
        E1,
        E2,
        %
        O1,
        O2,
        %
        C1,
        C2,
        C3
    ),

    SplitR = ?new_INTERNAL2(
        E4,
        E5,
        %
        O4 - O3,
        O5 - O3,
        %
        C4,
        C5,
        C6
    ),

    {split, SplitE, SplitO, SplitL, SplitR}.

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
    SplitO = 3,

    SplitL = ?new_LEAF2(E1, E2),
    SplitR = ?new_LEAF2(E4, E5),

    {split, SplitE, SplitO, SplitL, SplitR}.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing from left sibling
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_into_left_sibling_maybe/7}).
ins_rebalance_into_left_sibling_maybe(NewElem, Node, Pos, Args, ParentE, Left, LeftSize) ->
    case Args of
        {split, SplitE, SplitO, SplitL, SplitR} ->
            ins_rebalance_into_left_internal_maybe(
                Node, Pos, SplitE, SplitO, SplitL, SplitR, ParentE, Left, LeftSize
            );
        %
        [] ->
            ins_rebalance_into_left_leaf_maybe(Node, Pos, NewElem, ParentE, Left)
    end.

-compile({inline, ins_rebalance_into_left_internal_maybe/9}).
ins_rebalance_into_left_internal_maybe(
    ?INTERNAL4_MATCH_ALL, Pos, SplitE, SplitO, SplitL, SplitR, ParentE, Left, LeftSize
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
                SplitO,
                O1 + 1,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                %
                SplitL,
                SplitR,
                C2,
                C3,
                C4,
                C5,
                %
                ParentE,
                Left,
                LeftSize
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
                O1,
                O1 + SplitO,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                %
                C1,
                SplitL,
                SplitR,
                C3,
                C4,
                C5,
                %
                ParentE,
                Left,
                LeftSize
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
                O1,
                O2,
                O2 + SplitO,
                O3 + 1,
                O4 + 1,
                %
                C1,
                C2,
                SplitL,
                SplitR,
                C4,
                C5,
                %
                ParentE,
                Left,
                LeftSize
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
                O1,
                O2,
                O3,
                O3 + SplitO,
                O4 + 1,
                %
                C1,
                C2,
                C3,
                SplitL,
                SplitR,
                C5,
                %
                ParentE,
                Left,
                LeftSize
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
                O1,
                O2,
                O3,
                O4,
                O4 + SplitO,
                %
                C1,
                C2,
                C3,
                C4,
                SplitL,
                SplitR,
                %
                ParentE,
                Left,
                LeftSize
            )
    end.

-compile({inline, ins_rebalance_into_left_internal/19}).
ins_rebalance_into_left_internal(
    E1,
    E2,
    E3,
    E4,
    E5,
    %
    O1,
    O2,
    O3,
    O4,
    O5,
    %
    C1,
    C2,
    C3,
    C4,
    C5,
    C6,
    %
    ParentE,
    Left,
    LeftSize
) ->
    case Left of
        ?INTERNAL2_MATCH(
            LE1,
            LE2,
            %
            LO1,
            LO2,
            %
            LC1,
            LC2,
            LC3
        ) ->
            UpElem = E1,
            MovedSize = O1,

            UpdatedLeft = ?new_INTERNAL3(
                LE1,
                LE2,
                ParentE,
                %
                LO1,
                LO2,
                LeftSize,
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
                %%
                O2 - O1,
                O3 - O1,
                O4 - O1,
                O5 - O1,
                %
                C2,
                C3,
                C4,
                C5,
                C6
            ),

            {UpElem, MovedSize, UpdatedLeft, UpdatedNode};
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
                O1,
                O2,
                O3,
                O4,
                O5,
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
    MovedSize = 1,

    case Pos of
        1 ->
            UpElem = NewElem,
            UpdatedNode = Node,
            {UpElem, MovedSize, UpdatedLeft, UpdatedNode};
        %
        2 ->
            UpElem = E1,
            UpdatedNode = ?new_LEAF4(NewElem, E2, E3, E4),
            {UpElem, MovedSize, UpdatedLeft, UpdatedNode};
        %
        3 ->
            UpElem = E1,
            UpdatedNode = ?new_LEAF4(E2, NewElem, E3, E4),
            {UpElem, MovedSize, UpdatedLeft, UpdatedNode};
        %
        4 ->
            UpElem = E1,
            UpdatedNode = ?new_LEAF4(E2, E3, NewElem, E4),
            {UpElem, MovedSize, UpdatedLeft, UpdatedNode};
        %
        5 ->
            UpElem = E1,
            UpdatedNode = ?new_LEAF4(E2, E3, E4, NewElem),
            {UpElem, MovedSize, UpdatedLeft, UpdatedNode}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Insertion - Rebalancing from right sibling
%% ------------------------------------------------------------------

-compile({inline, ins_rebalance_into_right_sibling_maybe/7}).
ins_rebalance_into_right_sibling_maybe(NewElem, Node, NodeOffset, Pos, Args, ParentE, Right) ->
    case Args of
        {split, SplitE, SplitO, SplitL, SplitR} ->
            ins_rebalance_into_right_internal_maybe(
                Node, NodeOffset, Pos, SplitE, SplitO, SplitL, SplitR, ParentE, Right
            );
        %
        [] ->
            ins_rebalance_into_right_leaf_maybe(
                Node, Pos, NewElem, ParentE, Right
            )
    end.

-compile({inline, ins_rebalance_into_right_internal_maybe/9}).
ins_rebalance_into_right_internal_maybe(
    ?INTERNAL4_MATCH_ALL, NodeOffset, Pos, SplitE, SplitO, SplitL, SplitR, ParentE, Right
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
                SplitO,
                O1 + 1,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                %
                SplitL,
                SplitR,
                C2,
                C3,
                C4,
                C5,
                %
                NodeOffset + 1,
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
                O1,
                O1 + SplitO,
                O2 + 1,
                O3 + 1,
                O4 + 1,
                %
                C1,
                SplitL,
                SplitR,
                C3,
                C4,
                C5,
                %
                NodeOffset + 1,
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
                O1,
                O2,
                O2 + SplitO,
                O3 + 1,
                O4 + 1,
                %
                C1,
                C2,
                SplitL,
                SplitR,
                C4,
                C5,
                %
                NodeOffset + 1,
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
                O1,
                O2,
                O3,
                O3 + SplitO,
                O4 + 1,
                %
                C1,
                C2,
                C3,
                SplitL,
                SplitR,
                C5,
                %
                NodeOffset + 1,
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
                O1,
                O2,
                O3,
                O4,
                O4 + SplitO,
                %
                C1,
                C2,
                C3,
                C4,
                SplitL,
                SplitR,
                %
                NodeOffset + 1,
                %
                ParentE,
                Right
            )
    end.

-compile({inline, ins_rebalance_into_right_internal/19}).
ins_rebalance_into_right_internal(
    E1,
    E2,
    E3,
    E4,
    E5,
    %
    O1,
    O2,
    O3,
    O4,
    O5,
    %
    C1,
    C2,
    C3,
    C4,
    C5,
    C6,
    %
    NodeOffset,
    %
    ParentE,
    Right
) ->
    case Right of
        ?INTERNAL2_MATCH(
            RE1,
            RE2,
            %
            RO1,
            RO2,
            %
            RC1,
            RC2,
            RC3
        ) ->
            UpElem = E5,
            RightMovedSize = NodeOffset - O5,
            MovedSize = RightMovedSize - 1,

            UpdatedNode = ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                O1,
                O2,
                O3,
                O4,
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
                RightMovedSize,
                RightMovedSize + RO1,
                RightMovedSize + RO2,
                %
                C6,
                RC1,
                RC2,
                RC3
            ),

            {UpElem, MovedSize, UpdatedNode, UpdatedRight};
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
                O1,
                O2,
                O3,
                O4,
                O5,
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
    MovedSize = 0,

    case Pos of
        1 ->
            UpElem = E4,
            UpdatedNode = ?new_LEAF4(NewElem, E1, E2, E3),
            {UpElem, MovedSize, UpdatedNode, UpdatedRight};
        %
        2 ->
            UpElem = E4,
            UpdatedNode = ?new_LEAF4(E1, NewElem, E2, E3),
            {UpElem, MovedSize, UpdatedNode, UpdatedRight};
        %
        3 ->
            UpElem = E4,
            UpdatedNode = ?new_LEAF4(E1, E2, NewElem, E3),
            {UpElem, MovedSize, UpdatedNode, UpdatedRight};
        %
        4 ->
            UpElem = E4,
            UpdatedNode = ?new_LEAF4(E1, E2, E3, NewElem),
            {UpElem, MovedSize, UpdatedNode, UpdatedRight};
        %
        5 ->
            UpElem = NewElem,
            UpdatedNode = Node,
            {UpElem, MovedSize, UpdatedNode, UpdatedRight}
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
    UpdatedO1 = O1 - 1,
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,
    UpdatedO4 = O4 - 1,

    case del_rebalance_maybe_from_right_sibling(C1, UpdatedO1, E1, C2) of
        balanced ->
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                UpdatedO1,
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C2,
                C3,
                C4,
                C5
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL4(
                UpElem,
                E2,
                E3,
                E4,
                %
                UpdatedO1 + MovedSize,
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
                %
                UpdatedC1,
                UpdatedC2,
                C3,
                C4,
                C5
            );
        %
        ?MERGED_MATCH(MergedC1C2) ->
            ?new_INTERNAL3(
                E2,
                E3,
                E4,
                %
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
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
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,
    UpdatedO4 = O4 - 1,

    case
        del_rebalance_maybe_from_right_sibling(
            C2,
            UpdatedO2 - O1,
            %
            E2,
            C3
        )
    of
        balanced ->
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C2,
                C3,
                C4,
                C5
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, RebalancedC2, UpdatedC3) ->
            ?new_INTERNAL4(
                E1,
                UpElem,
                E3,
                E4,
                %
                O1,
                UpdatedO2 + MovedSize,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                RebalancedC2,
                UpdatedC3,
                C4,
                C5
            );
        %
        ?MERGED_MATCH(MergedC2C3) ->
            ?new_INTERNAL3(
                E1,
                E3,
                E4,
                %
                O1,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                MergedC2C3,
                C4,
                C5
            )
    end.

%%
%% C3
%%

-compile({inline, del_rebalance_INTERNAL4_C3 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C3(?INTERNAL4_ARGS) ->
    UpdatedO3 = O3 - 1,
    UpdatedO4 = O4 - 1,

    case
        del_rebalance_maybe_from_right_sibling(
            C3,
            UpdatedO3 - O2,
            %
            E3,
            C4
        )
    of
        balanced ->
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                O1,
                O2,
                UpdatedO3,
                UpdatedO4,
                %
                C1,
                C2,
                C3,
                C4,
                C5
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, RebalancedC3, UpdatedC4) ->
            ?new_INTERNAL4(
                E1,
                E2,
                UpElem,
                E4,
                %
                O1,
                O2,
                UpdatedO3 + MovedSize,
                UpdatedO4,
                %
                C1,
                C2,
                RebalancedC3,
                UpdatedC4,
                C5
            );
        %
        ?MERGED_MATCH(MergedC3C4) ->
            ?new_INTERNAL3(
                E1,
                E2,
                E4,
                %
                O1,
                O2,
                UpdatedO4,
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

-compile({inline, del_rebalance_INTERNAL4_C4 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C4(?INTERNAL4_ARGS) ->
    UpdatedO4 = O4 - 1,

    case
        del_rebalance_maybe_from_right_sibling(
            C4,
            UpdatedO4 - O3,
            %
            E4,
            C5
        )
    of
        balanced ->
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                E4,
                %
                O1,
                O2,
                O3,
                UpdatedO4,
                %
                C1,
                C2,
                C3,
                C4,
                C5
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, RebalancedC4, UpdatedC5) ->
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                UpElem,
                %
                O1,
                O2,
                O3,
                UpdatedO4 + MovedSize,
                %
                C1,
                C2,
                C3,
                RebalancedC4,
                UpdatedC5
            );
        %
        ?MERGED_MATCH(MergedC4C5) ->
            ?new_INTERNAL3(
                E1,
                E2,
                E3,
                %
                O1,
                O2,
                O3,
                %
                C1,
                C2,
                C3,
                MergedC4C5
            )
    end.

%%
%% C5
%%

-compile({inline, del_rebalance_INTERNAL4_C5 / ?INTERNAL4_ARITY}).
del_rebalance_INTERNAL4_C5(?INTERNAL4_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C5, E4, C4, O4 - O3) of
        balanced ->
            ?INTERNAL4_UPD_C5(C5);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, UpdatedC4, RebalancedC5) ->
            ?new_INTERNAL4(
                E1,
                E2,
                E3,
                UpElem,
                %
                O1,
                O2,
                O3,
                O4 - MovedSize,
                %
                C1,
                C2,
                C3,
                UpdatedC4,
                RebalancedC5
            );
        %
        ?MERGED_MATCH(MergedC4C5) ->
            ?new_INTERNAL3(
                E1,
                E2,
                E3,
                %
                O1,
                O2,
                O3,
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
    UpdatedO1 = O1 - 1,
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,

    case del_rebalance_maybe_from_right_sibling(C1, UpdatedO1, E1, C2) of
        balanced ->
            ?new_INTERNAL3(
                E1,
                E2,
                E3,
                %
                UpdatedO1,
                UpdatedO2,
                UpdatedO3,
                %
                C1,
                C2,
                C3,
                C4
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL3(
                UpElem,
                E2,
                E3,
                %
                UpdatedO1 + MovedSize,
                UpdatedO2,
                UpdatedO3,
                %
                UpdatedC1,
                UpdatedC2,
                C3,
                C4
            );
        %
        ?MERGED_MATCH(MergedC1C2) ->
            ?new_INTERNAL2(
                E2,
                E3,
                %
                UpdatedO2,
                UpdatedO3,
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
    UpdatedO2 = O2 - 1,
    UpdatedO3 = O3 - 1,

    case
        del_rebalance_maybe_from_right_sibling(
            C2,
            UpdatedO2 - O1,
            %
            E2,
            C3
        )
    of
        balanced ->
            ?new_INTERNAL3(
                E1,
                E2,
                E3,
                %
                O1,
                UpdatedO2,
                UpdatedO3,
                %
                C1,
                C2,
                C3,
                C4
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, RebalancedC2, UpdatedC3) ->
            ?new_INTERNAL3(
                E1,
                UpElem,
                E3,
                %
                O1,
                UpdatedO2 + MovedSize,
                UpdatedO3,
                %
                C1,
                RebalancedC2,
                UpdatedC3,
                C4
            );
        %
        ?MERGED_MATCH(MergedC2C3) ->
            ?new_INTERNAL2(
                E1,
                E3,
                %
                O1,
                UpdatedO3,
                %
                C1,
                MergedC2C3,
                C4
            )
    end.

%%
%% C3
%%

-compile({inline, del_rebalance_INTERNAL3_C3 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C3(?INTERNAL3_ARGS) ->
    UpdatedO3 = O3 - 1,

    case
        del_rebalance_maybe_from_right_sibling(
            C3,
            UpdatedO3 - O2,
            %
            E3,
            C4
        )
    of
        balanced ->
            ?new_INTERNAL3(
                E1,
                E2,
                E3,
                %
                O1,
                O2,
                UpdatedO3,
                %
                C1,
                C2,
                C3,
                C4
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, RebalancedC3, UpdatedC4) ->
            ?new_INTERNAL3(
                E1,
                E2,
                UpElem,
                %
                O1,
                O2,
                UpdatedO3 + MovedSize,
                %
                C1,
                C2,
                RebalancedC3,
                UpdatedC4
            );
        %
        ?MERGED_MATCH(MergedC3C4) ->
            ?new_INTERNAL2(
                E1,
                E2,
                %
                O1,
                O2,
                %
                C1,
                C2,
                MergedC3C4
            )
    end.

%%
%% C4
%%

-compile({inline, del_rebalance_INTERNAL3_C4 / ?INTERNAL3_ARITY}).
del_rebalance_INTERNAL3_C4(?INTERNAL3_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C4, E3, C3, O3 - O2) of
        balanced ->
            ?INTERNAL3_UPD_C4(C4);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, UpdatedC3, RebalancedC4) ->
            ?new_INTERNAL3(
                E1,
                E2,
                UpElem,
                %
                O1,
                O2,
                O3 - MovedSize,
                %
                C1,
                C2,
                UpdatedC3,
                RebalancedC4
            );
        %
        ?MERGED_MATCH(MergedC3C4) ->
            ?new_INTERNAL2(
                E1,
                E2,
                %
                O1,
                O2,
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
    UpdatedO1 = O1 - 1,
    UpdatedO2 = O2 - 1,

    case del_rebalance_maybe_from_right_sibling(C1, UpdatedO1, E1, C2) of
        balanced ->
            ?new_INTERNAL2(
                E1,
                E2,
                %
                UpdatedO1,
                UpdatedO2,
                %
                C1,
                C2,
                C3
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL2(
                UpElem,
                E2,
                %
                UpdatedO1 + MovedSize,
                UpdatedO2,
                %
                UpdatedC1,
                UpdatedC2,
                C3
            );
        %
        ?MERGED_MATCH(MergedC1C2) ->
            ?new_INTERNAL1(
                E2,
                %
                UpdatedO2,
                %
                MergedC1C2,
                C3
            )
    end.

%%
%% C2
%%

-compile({inline, del_rebalance_INTERNAL2_C2 / ?INTERNAL2_ARITY}).
del_rebalance_INTERNAL2_C2(?INTERNAL2_ARGS) ->
    UpdatedO2 = O2 - 1,

    case
        del_rebalance_maybe_from_right_sibling(
            C2,
            UpdatedO2 - O1,
            %
            E2,
            C3
        )
    of
        balanced ->
            ?new_INTERNAL2(
                E1,
                E2,
                %
                O1,
                UpdatedO2,
                %
                C1,
                C2,
                C3
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, RebalancedC2, UpdatedC3) ->
            ?new_INTERNAL2(
                E1,
                UpElem,
                %
                O1,
                UpdatedO2 + MovedSize,
                %
                C1,
                RebalancedC2,
                UpdatedC3
            );
        %
        ?MERGED_MATCH(MergedC2C3) ->
            ?new_INTERNAL1(
                E1,
                %
                O1,
                %
                C1,
                MergedC2C3
            )
    end.

%%
%% C3
%%

-compile({inline, del_rebalance_INTERNAL2_C3 / ?INTERNAL2_ARITY}).
del_rebalance_INTERNAL2_C3(?INTERNAL2_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C3, E2, C2, O2 - O1) of
        balanced ->
            ?INTERNAL2_UPD_C3(C3);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, UpdatedC2, RebalancedC3) ->
            ?new_INTERNAL2(
                E1,
                UpElem,
                %
                O1,
                O2 - MovedSize,
                %
                C1,
                UpdatedC2,
                RebalancedC3
            );
        %
        ?MERGED_MATCH(MergedC2C3) ->
            ?new_INTERNAL1(
                E1,
                %
                O1,
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
    UpdatedO1 = O1 - 1,

    case del_rebalance_maybe_from_right_sibling(C1, UpdatedO1, E1, C2) of
        balanced ->
            ?new_INTERNAL1(
                E1,
                %
                UpdatedO1,
                %
                C1,
                C2
            );
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL1(
                UpElem,
                %
                UpdatedO1 + MovedSize,
                %
                UpdatedC1,
                UpdatedC2
            );
        %
        ?MERGED_MATCH(MergedC1C2) ->
            % Can only happen at root - height is reduced
            ?CHECK_NODE(MergedC1C2)
    end.

%%
%% C2
%%

-compile({inline, del_rebalance_INTERNAL1_C2 / ?INTERNAL1_ARITY}).
del_rebalance_INTERNAL1_C2(?INTERNAL1_ARGS) ->
    case del_rebalance_maybe_from_left_sibling(C2, E1, C1, O1) of
        balanced ->
            ?INTERNAL1_UPD_C2(C2);
        %
        badkey ->
            badkey;
        %
        ?ROTATED(UpElem, MovedSize, UpdatedC1, UpdatedC2) ->
            ?new_INTERNAL1(
                UpElem,
                %
                O1 - MovedSize,
                %
                UpdatedC1,
                UpdatedC2
            );
        %
        ?MERGED_MATCH(MergedC1C2) ->
            % Can only happen at root - height is reduced
            ?CHECK_NODE(MergedC1C2)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Deletion - Rebalance a node from its right sibling
%% ------------------------------------------------------------------

-compile({inline, del_rebalance_maybe_from_right_sibling/4}).
del_rebalance_maybe_from_right_sibling(Child, ChildOffset, RParentE, Right) ->
    case Child of
        ?INTERNAL1_MATCH(CElem, CLeftOffset, CLeft, CRight) ->
            del_rebalance_internal_from_right_sibling(
                CElem,
                CLeftOffset,
                CLeft,
                CRight,
                %
                ChildOffset,
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

%-compile({inline,
del_rebalance_internal_from_right_sibling(
    CElem,
    CLeftOffset,
    CLeft,
    CRight,
    %
    ChildOffset,
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
                CLeftOffset,
                ChildOffset,
                ChildOffset + O1,
                ChildOffset + O2,
                %
                CLeft,
                CRight,
                C1,
                C2,
                C3
            ),

            ?MERGED(MergedNode);
        %
        %
        %
        ?INTERNAL3_MATCH_ALL ->
            UpElem = E1,
            MovedC = C1,
            MovedSize = O1,

            UpdatedNode = ?new_INTERNAL2(
                CElem,
                RParentE,
                %
                CLeftOffset,
                ChildOffset,
                %
                CLeft,
                CRight,
                MovedC
            ),

            UpdatedRight = ?new_INTERNAL2(
                E2,
                E3,
                %
                O2 - O1,
                O3 - O1,
                %
                C2,
                C3,
                C4
            ),

            ?ROTATED(UpElem, MovedSize, UpdatedNode, UpdatedRight);
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            UpElem = E1,
            MovedC = C1,
            MovedSize = O1,

            UpdatedNode = ?new_INTERNAL2(
                CElem,
                RParentE,
                %
                CLeftOffset,
                ChildOffset,
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
                O2 - O1,
                O3 - O1,
                O4 - O1,
                %
                C2,
                C3,
                C4,
                C5
            ),

            ?ROTATED(UpElem, MovedSize, UpdatedNode, UpdatedRight)
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

            ?MERGED(MergedNode);
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpElem = E1,
            MovedSize = 1,

            UpdatedNode = ?new_LEAF2(CElem, RParentE),
            UpdatedRight = ?new_LEAF2(E2, E3),

            ?ROTATED(UpElem, MovedSize, UpdatedNode, UpdatedRight);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E1,
            MovedSize = 1,

            UpdatedNode = ?new_LEAF2(CElem, RParentE),
            UpdatedRight = ?new_LEAF3(E2, E3, E4),

            ?ROTATED(UpElem, MovedSize, UpdatedNode, UpdatedRight)
        %
        %
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions: Deletion - Rebalance a node from its left sibling
%% ------------------------------------------------------------------

-compile({inline, del_rebalance_maybe_from_left_sibling/4}).
del_rebalance_maybe_from_left_sibling(Child, LParentE, Left, LeftOffset) ->
    case Child of
        ?INTERNAL1_MATCH(CElem, CLeftOffset, CLeft, CRight) ->
            del_rebalance_internal_from_left_sibling(
                CElem,
                CLeftOffset,
                CLeft,
                CRight,
                %
                LParentE,
                Left,
                LeftOffset
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
    CLeftOffset,
    CLeft,
    CRight,
    %
    LParentE,
    Left,
    LeftOffset
) ->
    case Left of
        ?INTERNAL2_MATCH_ALL ->
            MergedNode = ?new_INTERNAL4(
                E1,
                E2,
                LParentE,
                CElem,
                %
                O1,
                O2,
                LeftOffset,
                LeftOffset + CLeftOffset,
                %
                C1,
                C2,
                C3,
                CLeft,
                CRight
            ),

            ?MERGED(MergedNode);
        %
        %
        %
        ?INTERNAL3_MATCH_ALL ->
            UpElem = E3,
            MovedC = C4,
            MovedSize = LeftOffset - O3,

            UpdatedNode = ?new_INTERNAL2(
                LParentE,
                CElem,
                %
                MovedSize,
                MovedSize + CLeftOffset,
                %
                MovedC,
                CLeft,
                CRight
            ),

            UpdatedLeft = ?new_INTERNAL2(
                E1,
                E2,
                %
                O1,
                O2,
                %
                C1,
                C2,
                C3
            ),

            ?ROTATED(UpElem, MovedSize, UpdatedLeft, UpdatedNode);
        %
        %
        %
        ?INTERNAL4_MATCH_ALL ->
            UpElem = E4,
            MovedC = C5,
            MovedSize = LeftOffset - O4,

            UpdatedNode = ?new_INTERNAL2(
                LParentE,
                CElem,
                %
                MovedSize,
                MovedSize + CLeftOffset,
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
                O1,
                O2,
                O3,
                %
                C1,
                C2,
                C3,
                C4
            ),

            ?ROTATED(UpElem, MovedSize, UpdatedLeft, UpdatedNode)
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

            ?MERGED(MergedNode);
        %
        %
        ?LEAF3_MATCH_ALL ->
            UpElem = E3,
            MovedSize = 1,

            UpdatedNode = ?new_LEAF2(LParentE, CElem),
            UpdatedLeft = ?new_LEAF2(E1, E2),

            ?ROTATED(UpElem, MovedSize, UpdatedLeft, UpdatedNode);
        %
        %
        ?LEAF4_MATCH_ALL ->
            UpElem = E4,
            MovedSize = 1,

            UpdatedNode = ?new_LEAF2(LParentE, CElem),
            UpdatedLeft = ?new_LEAF3(E1, E2, E3),

            ?ROTATED(UpElem, MovedSize, UpdatedLeft, UpdatedNode)
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
    check_node_offsets(LineNumber, Type, Node),
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
        ?INTERNAL1(_, _, _, _) ->
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
        ?INTERNAL2(_, _, _, _, _, _, _) ->
            'INTERNAL2';
        %
        ?INTERNAL3(_, _, _, _, _, _, _, _, _, _) ->
            'INTERNAL3';
        %
        ?INTERNAL4(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
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
    case E1 > E2 of
        false ->
            check_node_elements(E2, Next);
        %
        true ->
            [{E1, E2} | check_node_elements(E2, Next)]
    end;
check_node_elements(_, []) ->
    [].
check_node_offsets(LineNumber, Type, Node) ->
    try check_node_offsets_recur(Node) of
        _ ->
            Node
    catch
        error:{assertEqual, Details} ->
            fail_node_check(LineNumber, Type, Node, {bad_offsets, Details})
    end.

check_node_offsets_recur(Node) ->
    case Node of
        ?INTERNAL1(_, O1, C1, C2) ->
            S1 = check_node_offsets_recur(C1),
            ?assertEqual(S1 + 1, O1),

            S2 = check_node_offsets_recur(C2),
            S1 + S2 + 1;
        %
        ?INTERNAL2(_, _, O1, O2, C1, C2, C3) ->
            S1 = check_node_offsets_recur(C1),
            ?assertEqual(S1 + 1, O1),

            S2 = check_node_offsets_recur(C2),
            ?assertEqual(S1 + S2 + 2, O2),

            S3 = check_node_offsets_recur(C3),
            S1 + S2 + S3 + 2;
        %
        ?INTERNAL3(_, _, _, O1, O2, O3, C1, C2, C3, C4) ->
            S1 = check_node_offsets_recur(C1),
            ?assertEqual(S1 + 1, O1),

            S2 = check_node_offsets_recur(C2),
            ?assertEqual(S1 + S2 + 2, O2),

            S3 = check_node_offsets_recur(C3),
            ?assertEqual(S1 + S2 + S3 + 3, O3),

            S4 = check_node_offsets_recur(C4),
            S1 + S2 + S3 + S4 + 3;
        %
        ?INTERNAL4(_, _, _, _, O1, O2, O3, O4, C1, C2, C3, C4, C5) ->
            S1 = check_node_offsets_recur(C1),
            ?assertEqual(S1 + 1, O1),

            S2 = check_node_offsets_recur(C2),
            ?assertEqual(S1 + S2 + 2, O2),

            S3 = check_node_offsets_recur(C3),
            ?assertEqual(S1 + S2 + S3 + 3, O3),

            S4 = check_node_offsets_recur(C4),
            ?assertEqual(S1 + S2 + S3 + S4 + 4, O4),

            S5 = check_node_offsets_recur(C5),
            S1 + S2 + S3 + S4 + S5 + 4;
        %
        ?LEAF1(_) ->
            1;
        %
        ?LEAF2(_, _) ->
            2;
        %
        ?LEAF3(_, _, _) ->
            3;
        %
        ?LEAF4(_, _, _, _) ->
            4
    end.

% -if(?NODE_CHECK_ENABLED).
-endif.
