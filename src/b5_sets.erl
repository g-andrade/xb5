-module(b5_sets).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    empty/0,
    from_list/1,
    insert/2,
    is_element/2,
    new/0,
    to_list/1,
    union/2
]).

-ignore_xref([
]).

%% ------------------------------------------------------------------
%% API Type Definitions
%% ------------------------------------------------------------------

-record(b5_sets, {size, root}).

-opaque set(Key, Value) :: #b5_sets{
    size :: non_neg_integer(),
    root :: b5_sets_node:t(Key, Value)
}.
-export_type([set/2]).

%%%%%%%

-type iter(Key, Value) :: b5_sets_node:iter(Key, Value).
-export_type([iter/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

empty() ->
    new().

enter(Element, Set) ->
    try
        insert(Element, Set)
    catch
        error:{element_exists, E} when E =:= Element ->
            Set
    end.

from_list(List) ->
    lists:foldl(fun enter/2, new(), List).

insert(Key, #b5_sets{size = Size, root = Root} = Set) ->
    Set#b5_sets{
        size = Size + 1,
        root = b5_sets_node:insert(Key, Root)
    }.

is_element(Key, #b5_sets{root = Root}) ->
    b5_sets_node:is_element(Key, Root).

new() ->
    #b5_sets{size = 0, root = b5_sets_node:new()}.

to_list(#b5_sets{root = Root}) ->
    b5_sets_node:to_list(Root).

union(#b5_sets{root = Root1, size = Size1} = Set1, #b5_sets{root = Root2, size = Size2} = Set2) ->
    case Size2 < Size1 of
        true ->
            [SizeInc | UpdatedRoot1] = b5_sets_node:union(Root2, Root1),
            Set1#b5_sets{
                size = Size1 + SizeInc,
                root = UpdatedRoot1
            };

        _ ->
            [SizeInc | UpdatedRoot2] = b5_sets_node:union(Root2, Root1),
            Set2#b5_sets{
                size = Size2 + SizeInc,
                root = UpdatedRoot2
            }
    end.
