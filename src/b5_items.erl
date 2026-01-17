-module(b5_items).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% TODO adapt insert tweaks from tree

-export([
    add/2,
    delete/2,
    delete_any/2,
    filter/2,
    filtermap/2,
    fold/3,
    from_list/1,
    from_list/2,
    from_ordset/1,
    from_ordset/2,
    insert/2,
    is_empty/1,
    is_member/2,
    iterator/1,
    iterator/2,
    iterator_from/2,
    iterator_from/3,
    larger/2,
    largest/1,
    map/2,
    new/0,
    new/1,
    next/1,
    size/1,
    smaller/2,
    smallest/1,
    structural_stats/1,
    take_largest/1,
    take_smallest/1,
    to_list/1
]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-record(b5_items, {unique, size, root}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

add(Element, #b5_items{unique = Unique, size = Size, root = Root} = Items) ->
    case Unique of
        false ->
            UpdatedRoot = b5_items_node:add(Element, Root),
            Items#b5_items{size = Size + 1, root = UpdatedRoot};
        %
        true ->
            case b5_items_node:insert(Element, Root) of
                none ->
                    Items;
                %
                UpdatedRoot ->
                    Items#b5_items{size = Size + 1, root = UpdatedRoot}
            end
    end.

delete(Element, #b5_items{size = Size, root = Root} = Items) ->
    case b5_items_node:delete_att(Element, Root) of
        none ->
            error_badkey(Element);
        %
        UpdatedRoot ->
            Items#b5_items{size = Size - 1, root = UpdatedRoot}
    end.

delete_any(Element, #b5_items{size = Size, root = Root} = Items) ->
    case b5_items_node:delete_att(Element, Root) of
        none ->
            Items;
        %
        UpdatedRoot ->
            Items#b5_items{size = Size - 1, root = UpdatedRoot}
    end.

filter(Fun, #b5_items{unique = Unique, root = Root}) ->
    [FilteredSize | FilteredRoot] = b5_items_node:filter(Fun, Root),
    #b5_items{unique = Unique, size = FilteredSize, root = FilteredRoot}.

filtermap(Fun, #b5_items{unique = Unique, root = Root}) ->
    case Unique of
        false ->
            [FilteredSize | FilteredRoot] = b5_items_node:filtermap(Fun, Root),
            #b5_items{unique = Unique, size = FilteredSize, root = FilteredRoot};
        %
        true ->
            [FilteredSize | FilteredRoot] = b5_items_node:filtermap_unique(Fun, Root),
            #b5_items{unique = Unique, size = FilteredSize, root = FilteredRoot}
    end.

fold(Fun, Acc, #b5_items{root = Root}) ->
    b5_items_node:fold(Fun, Acc, Root).

from_list(List) ->
    from_list(List, []).

from_list(List, Opts) ->
    Root = b5_items_node:new(),
    Size = 0,

    case get_opt_unique(Opts) of
        false ->
            from_list_recur(List, Size, Root);
        %
        true ->
            from_list_unique_recur(List, Size, Root)
    end.

from_ordset(Ordset) ->
    from_ordset(Ordset, []).

from_ordset(Ordset, Opts) ->
    List = ordsets:to_list(Ordset),
    from_list(List, Opts).

insert(Element, #b5_items{size = Size, root = Root} = Items) ->
    case b5_items_node:insert_att(Element, Root) of
        none ->
            error_key_exists(Element);
        %
        UpdatedRoot ->
            Items#b5_items{size = Size + 1, root = UpdatedRoot}
    end.

is_empty(#b5_items{size = Size}) ->
    Size =:= 0.

is_member(Element, #b5_items{root = Root}) ->
    b5_items_node:is_member(Element, Root).

iterator(Items) ->
    iterator(Items, ordered).

iterator(#b5_items{root = Root}, Order) ->
    b5_items_node:iterator(Root, Order).

iterator_from(Element, Items) ->
    iterator_from(Element, Items, ordered).

iterator_from(Element, #b5_items{root = Root}, Order) ->
    b5_items_node:iterator_from(Element, Root, Order).

larger(Element, #b5_items{root = Root}) ->
    b5_items_node:larger(Element, Root).

largest(#b5_items{size = Size, root = Root}) when Size =/= 0 ->
    b5_items_node:largest(Root);
largest(#b5_items{}) ->
    error_empty_items().

map(Fun, #b5_items{root = Root}) ->
    [NewSize | MappedRoot] = b5_items_node:map(Fun, Root),
    #b5_items{size = NewSize, root = MappedRoot}.

new() ->
    new([]).

new(Opts) ->
    #b5_items{unique = get_opt_unique(Opts), size = 0, root = b5_items_node:new()}.

next(Iter) ->
    b5_items_node:next(Iter).

size(#b5_items{size = Size}) ->
    Size.

smaller(Element, #b5_items{root = Root}) ->
    b5_items_node:smaller(Element, Root).

smallest(#b5_items{size = Size, root = Root}) when Size =/= 0 ->
    b5_items_node:smallest(Root);
smallest(#b5_items{}) ->
    error_empty_items().

structural_stats(#b5_items{root = Root}) ->
    b5_items_node:structural_stats(Root).

take_largest(#b5_items{root = Root, size = Size} = Items) when Size =/= 0 ->
    [Largest | UpdatedRoot] = b5_items_node:take_largest(Root),
    {Largest, Items#b5_items{root = UpdatedRoot, size = Size - 1}};
take_largest(#b5_items{}) ->
    error_empty_items().

take_smallest(#b5_items{root = Root, size = Size} = Items) when Size =/= 0 ->
    [Smallest | UpdatedRoot] = b5_items_node:take_smallest(Root),
    {Smallest, Items#b5_items{root = UpdatedRoot, size = Size - 1}};
take_smallest(#b5_items{}) ->
    error_empty_items().

to_list(#b5_items{root = Root}) ->
    b5_items_node:to_list(Root).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-compile({inline, error_badkey/1}).
-spec error_badkey(_) -> no_return().
error_badkey(Elem) ->
    error({badkey, Elem}).

-compile({inline, error_empty_items/0}).
-spec error_empty_items() -> no_return().
error_empty_items() ->
    error(empty_items).

-compile({inline, error_key_exists/1}).
-spec error_key_exists(_) -> no_return().
error_key_exists(Elem) ->
    error({key_exists, Elem}).

get_opt_unique(Opts) ->
    case proplists:get_value(unique, Opts, false) of
        true -> true;
        false -> false
    end.

from_list_recur([Element | Next], Size, Root) ->
    UpdatedRoot = b5_items_node:add(Element, Root),
    from_list_recur(Next, Size + 1, UpdatedRoot);
from_list_recur([], Size, Root) ->
    #b5_items{unique = false, size = Size, root = Root}.

from_list_unique_recur([Element | Next], Size, Root) ->
    case b5_items_node:insert_att(Element, Root) of
        none ->
            from_list_unique_recur(Next, Size, Root);
        %
        UpdatedRoot ->
            from_list_unique_recur(Next, Size + 1, UpdatedRoot)
    end;
from_list_unique_recur([], Size, Root) ->
    #b5_items{unique = true, size = Size, root = Root}.
