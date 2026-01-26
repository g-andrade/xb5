-module(b5_items).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    merge/2,
    new/0,
    new/1,
    next/1,
    nth/2,
    percentile/2,
    percentile/3,
    percentile_bracket/2,
    percentile_bracket/3,
    percentile_rank/2,
    rank_larger/2,
    rank_smaller/2,
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

-opaque items(E) :: #b5_items{
    unique :: boolean(), size :: non_neg_integer(), root :: b5_items_node:t(E)
}.
-export_type([items/1]).

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
            case b5_items_node:insert_att(Element, Root) of
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

merge(
    #b5_items{unique = Unique, size = Size1, root = Root1},
    #b5_items{size = Size2, root = Root2}
) ->
    case Unique of
        false ->
            MergedRoot = b5_items_node:merge(Size1, Root1, Size2, Root2),
            #b5_items{unique = false, size = Size1 + Size2, root = MergedRoot};
        %
        true ->
            [MergedSize | MergedRoot] = b5_items_node:merge_unique(Size1, Root1, Size2, Root2),
            #b5_items{unique = true, size = MergedSize, root = MergedRoot}
    end.

new() ->
    new([]).

new(Opts) ->
    #b5_items{unique = get_opt_unique(Opts), size = 0, root = b5_items_node:new()}.

next(Iter) ->
    b5_items_node:next(Iter).

nth(Rank, #b5_items{size = Size, root = Root}) when is_integer(Rank), Rank >= 1, Rank =< Size ->
    b5_items_node:nth(Rank, Root);
nth(Rank, #b5_items{}) ->
    error({badarg, Rank}).

percentile(Percentile, Items) ->
    percentile(Percentile, Items, []).

percentile(Percentile, Items, Opts) ->
    Bracket = percentile_bracket(Percentile, Items, Opts),
    linear_interpolated_percentile(Bracket).

percentile_bracket(Percentile, Items) ->
    percentile_bracket(Percentile, Items, []).

percentile_bracket(Percentile, #b5_items{size = Size, root = Root}, Opts) when
    is_number(Percentile), Percentile >= 0.0, Percentile =< 1.0
->
    case Size of
        0 ->
            none;
        %
        _ ->
            % Inclusive: as in Excel PERCENTILE.INC:
            % * https://support.microsoft.com/en-us/office/percentile-inc-function-680f9539-45eb-410b-9a5e-c1355e5fe2ed
            % * (Claude says this is Hyndman-Fan Type 7)
            %
            % Exclusive: as in Excel PERCENTILE.EXC:
            % * https://support.microsoft.com/en-us/office/percentile-exc-function-bbaa7204-e9e1-4010-85bf-c31dc5dce4ba
            % * (Claude says this is Hyndman-Fan Type 6)
            %
            % Nearest rank: as described in Wikipedia:
            % * https://en.wikipedia.org/wiki/Percentile#The_nearest-rank_method
            %
            Method = proplists:get_value(method, Opts, inclusive),
            Pos = percentile_bracket_pos(Percentile, Size, Method),
            percentile_bracket_for_pos(Percentile, Pos, Size, Root, Method)
    end;
percentile_bracket(Percentile, #b5_items{}, _Opts) ->
    error({badarg, Percentile}).

percentile_rank(Elem, #b5_items{size = Size, root = Root}) when Size > 0 ->
    % As described in Wikipedia:
    % https://en.wikipedia.org/wiki/Percentile_rank
    Smaller = b5_items_node:rank_smaller(Elem, Root),
    Larger = b5_items_node:rank_larger(Elem, Root),

    [CF | F] = percentile_rank_params(Smaller, Larger, Size),

    (CF - 0.5 * F) / Size;
percentile_rank(_, #b5_items{}) ->
    error_empty_items().

rank_larger(Elem, #b5_items{root = Root}) ->
    case b5_items_node:rank_larger(Elem, Root) of
        [Rank | Larger] ->
            {Rank, Larger};
        %
        none ->
            none
    end.

rank_smaller(Elem, #b5_items{root = Root}) ->
    case b5_items_node:rank_smaller(Elem, Root) of
        [Rank | Smaller] ->
            {Rank, Smaller};
        %
        none ->
            none
    end.

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

%%%%%%%%

percentile_bracket_pos(Percentile, Size, inclusive) ->
    1 + (Size - 1) * Percentile;
percentile_bracket_pos(Percentile, Size, exclusive) ->
    (Size + 1) * Percentile;
percentile_bracket_pos(Percentile, Size, nearest_rank) ->
    ceil(Percentile * Size).

percentile_bracket_for_pos(Percentile, Pos, Size, Root, Method) ->
    LowRank = floor(Pos),
    HighRank = ceil(Pos),

    if
        LowRank < 1 orelse HighRank > Size ->
            none;
        %
        LowRank == HighRank ->
            ExactRank = LowRank,
            ExactElem = b5_items_node:nth(ExactRank, Root),
            {exact, ExactElem};
        %
        true ->
            [LowElem | HighElem] = b5_items_node:nth_and_nthp1(LowRank, Root),

            case HighElem == LowElem of
                true ->
                    {exact, LowElem};
                %
                _ ->
                    LowPerc = percentile_bracket_perc(LowRank, Size, Method),
                    HighPerc = percentile_bracket_perc(HighRank, Size, Method),

                    PercRange = HighPerc - LowPerc,
                    HighWeight = (Percentile - LowPerc) / PercRange,
                    LowWeight = 1.0 - HighWeight,

                    {between, percentile_bracket_bound(LowWeight, LowPerc, LowElem),
                        percentile_bracket_bound(HighWeight, HighPerc, HighElem)}
            end
    end.

percentile_bracket_perc(Rank, Size, inclusive) ->
    (Rank - 1) / (Size - 1);
percentile_bracket_perc(Rank, Size, exclusive) ->
    Rank / (Size + 1).

percentile_bracket_bound(Weight, Perc, Elem) ->
    #{
        percentile => Perc,
        weight => Weight,
        value => Elem
    }.

linear_interpolated_percentile({exact, ExactElem}) ->
    {value, ExactElem};
linear_interpolated_percentile({between, LowBound, HighBound}) ->
    #{
        weight := LowWeight,
        value := LowElem
    } = LowBound,

    #{
        weight := HighWeight,
        value := HighElem
    } = HighBound,

    if
        not is_number(LowElem) ->
            error({bracket_value_not_a_number, LowBound});
        %
        not is_number(HighElem) ->
            error({bracket_value_not_a_number, HighBound});
        %
        true ->
            {value, (LowWeight * LowElem) + (HighWeight * HighElem)}
    end;
linear_interpolated_percentile(none) ->
    none.

%%%%%%%%

percentile_rank_params(none, none, Size) ->
    CF = Size,
    F = Size,
    [CF | F];
percentile_rank_params(none, [LargerRank | _], _) ->
    F = LargerRank - 1,
    CF = F,
    [CF | F];
percentile_rank_params([SmallerRank | _], none, Size) ->
    F = Size - SmallerRank,
    CF = Size,
    [CF | F];
percentile_rank_params([SmallerRank | _], [LargerRank | _], _) ->
    CF = LargerRank - 1,
    F = LargerRank - SmallerRank - 1,
    [CF | F].
