-module(b5_sets).

-export([
    % add/2,
    % % `sets' compatibility alias
    % add_element/2,
    % balance/1,
    % del_element/2,
    delete/2,
    % delete_any/2,
    difference/2,
    % empty/0,
    % filter/2,
    % filtermap/2,
    % fold/3,
    from_list/1,
    % from_ordset/1,
    insert/2,
    intersection/1,
    intersection/2,
    is_disjoint/2,
    % % `sets' compatibility alias
    % is_element/2,
    % is_empty/1,
    is_equal/2,
    is_member/2,
    % is_set/1
    is_subset/2,
    % iterator/1,
    % iterator/2,
    % iterator_from/2,
    % iterator_from/3,
    % larger/2,
    % largest/1,
    map/2,
    % next/1,
    new/0,
    % singleton/,
    % size/1,
    % smaller/2,
    % subtract/2,
    % smallest/1,
    % take_largest/1,
    % take_smallest/1,
    % to_list/1,
    union/1,
    union/2
]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-record(b5_sets, {size, root}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

delete(Elem, #b5_sets{size = Size, root = Root} = Set) ->
    UpdatedRoot = b5_sets_node:delete(Elem, Root),
    Set#b5_sets{size = Size - 1, root = UpdatedRoot}.

difference(#b5_sets{size = Size1, root = Root1} = Set1, #b5_sets{root = Root2}) ->
    [RemovedCount | UpdatedRoot1] = b5_sets_node:difference(Root1, Root2),
    Set1#b5_sets{size = Size1 - RemovedCount, root = UpdatedRoot1}.

from_list(List) ->
    Root = b5_sets_node:new(),
    Size = 0,
    from_list_recur(List, Root, Size).

insert(Elem, #b5_sets{size = Size, root = Root} = Set) ->
    UpdatedRoot = b5_sets_node:insert(Elem, Root),
    Set#b5_sets{size = Size + 1, root = UpdatedRoot}.

intersection([#b5_sets{size = Size1, root = Root1} | Others]) ->
    intersection_recur(Root1, Size1, Others);
intersection([]) ->
    new().

intersection(#b5_sets{root = Root1}, #b5_sets{root = Root2}) ->
    [NewSize | NewRoot] = b5_sets_node:intersection(Root1, Root2),
    #b5_sets{size = NewSize, root = NewRoot}.

is_disjoint(#b5_sets{root = Root1, size = Size1}, #b5_sets{root = Root2, size = Size2}) ->
    b5_sets_node:is_disjoint(Root1, Size1, Root2, Size2).

is_equal(#b5_sets{root = Root1, size = Size1}, #b5_sets{root = Root2, size = Size2}) ->
    (Size1 =:= Size2) andalso b5_sets_node:is_equal(Root1, Root2).

is_member(Elem, #b5_sets{root = Root}) ->
    b5_sets_node:is_member(Elem, Root).

is_subset(#b5_sets{root = Root1}, #b5_sets{root = Root2}) ->
    b5_sets_node:is_subset(Root1, Root2).

map(Fun, #b5_sets{root = Root}) ->
    [NewSize | MappedRoot] = b5_sets_node:map(Fun, Root),
    #b5_sets{size = NewSize, root = MappedRoot}.

new() ->
    #b5_sets{size = 0, root = b5_sets_node:new()}.

union([#b5_sets{size = Size1, root = Root1} | Others]) ->
    union_recur(Root1, Size1, Others);
union([]) ->
    new().

union(#b5_sets{root = Root1, size = Size1}, #b5_sets{root = Root2, size = Size2}) ->
    [NewSize | NewRoot] = b5_sets_node:union(Root1, Size1, Root2, Size2),
    #b5_sets{size = NewSize, root = NewRoot}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

from_list_recur([Elem | Next], Root, Size) ->
    try b5_sets_node:insert(Elem, Root) of
        UpdatedRoot ->
            UpdatedSize = Size + 1,
            from_list_recur(Next, UpdatedRoot, UpdatedSize)
    catch
        error:{key_exists, K} when K =:= Elem ->
            from_list_recur(Next, Root, Size)
    end;
from_list_recur([], Root, Size) ->
    #b5_sets{size = Size, root = Root}.

intersection_recur(Root1, _Size1, [#b5_sets{root = Root2} | Next]) ->
    [NewSize | NewRoot] = b5_sets_node:intersection(Root1, Root2),
    intersection_recur(NewRoot, NewSize, Next);
intersection_recur(Root, Size, []) ->
    #b5_sets{size = Size, root = Root}.

union_recur(Root1, Size1, [#b5_sets{root = Root2, size = Size2} | Next]) ->
    [NewSize | NewRoot] = b5_sets_node:union(Root1, Size1, Root2, Size2),
    union_recur(NewRoot, NewSize, Next);
union_recur(Root, Size, []) ->
    #b5_sets{size = Size, root = Root}.
