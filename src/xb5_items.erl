-module(xb5_items).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    add/2,
    delete/2,
    delete_any/2,
    filter/2,
    filtermap/2,
    fold/3,
    from_list/1,
    from_ordset/1,
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
    to_list/1,
    unwrap/1,
    wrap/1
]).

%% ------------------------------------------------------------------
%% Linter Tweaks
%% ------------------------------------------------------------------

-elvis([
    % Large URLs below require this
    {elvis_text_style, line_length, #{limit => 120}}
]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-record(xb5_items, {size, root}).

-opaque items(E) :: #xb5_items{
    size :: non_neg_integer(), root :: xb5_items_node:t(E)
}.
-export_type([items/1]).

-type items() :: items(_).
-export_type([items/0]).

-type iter(Element) :: xb5_items_node:iter(Element).
-export_type([iter/1]).

-type iter() :: iter(_).
-export_type([iter/0]).

%%%

-type percentile_bracket_opt() ::
    ({method, percentile_bracket_method()}).
-export_type([percentile_bracket_opt/0]).

-type percentile_bracket_method() ::
    (inclusive
    | exclusive
    | nearest_rank).

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

-export_type([percentile_bracket_method/0]).

-type percentile_bracket(Element) ::
    ({exact, Element}
    | {between, percentile_bracket_bound(Element), percentile_bracket_bound(Element)}
    | none).
-export_type([percentile_bracket/1]).

-type percentile_bracket_bound(Element) :: #{
    percentile := float(),
    weight := float(),
    value := Element
}.
-export_type([percentile_bracket_bound/1]).

%%

-type unwrapped_items(Element) :: #{
    size := non_neg_integer(),
    root := xb5_items_node:t(Element)
}.
-export_type([unwrapped_items/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec add(Element, Items1) -> Items2 when
    Items1 :: items(Element),
    Items2 :: items(Element).

add(Element, #xb5_items{size = Size, root = Root} = Items) ->
    UpdatedRoot = xb5_items_node:add(Element, Root),
    Items#xb5_items{size = Size + 1, root = UpdatedRoot}.

%%

-spec delete(Element, Items1) -> Items2 | no_return() when
    Element :: term(),
    Items1 :: items(Element),
    Items2 :: items(Element).

delete(Element, #xb5_items{size = Size, root = Root} = Items) ->
    case xb5_items_node:delete_att(Element, Root) of
        none ->
            error_badkey(Element);
        %
        UpdatedRoot ->
            Items#xb5_items{size = Size - 1, root = UpdatedRoot}
    end.

%%

-spec delete_any(Element, Items1) -> Items2 when
    Element :: term(),
    Items1 :: items(Element),
    Items2 :: items(Element).

delete_any(Element, #xb5_items{size = Size, root = Root} = Items) ->
    case xb5_items_node:delete_att(Element, Root) of
        none ->
            Items;
        %
        UpdatedRoot ->
            Items#xb5_items{size = Size - 1, root = UpdatedRoot}
    end.

%%

-spec filter(Pred, Items1) -> Items2 when
    Pred :: fun((Element) -> boolean()),
    Items1 :: items(Element),
    Items2 :: items(Element).

filter(Fun, #xb5_items{root = Root} = Items) ->
    [FilteredSize | FilteredRoot] = xb5_items_node:filter(Fun, Root),
    Items#xb5_items{size = FilteredSize, root = FilteredRoot}.

%%

-spec filtermap(Fun, Items1) -> Items2 when
    Fun :: fun((Element1) -> boolean() | {true, Element2}),
    Items1 :: items(Element1),
    Items2 :: items(Element1 | Element2).

filtermap(Fun, #xb5_items{root = Root} = Items) ->
    [FilteredSize | FilteredRoot] = xb5_items_node:filtermap(Fun, Root),
    Items#xb5_items{size = FilteredSize, root = FilteredRoot}.

%%

-spec fold(Function, Acc0, Items) -> Acc1 when
    Function :: fun((Element, AccIn) -> AccOut),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc,
    Items :: items(Element).

fold(Fun, Acc, #xb5_items{root = Root}) ->
    xb5_items_node:fold(Fun, Acc, Root).

%%

-spec from_list(List) -> Items when
    List :: [Element],
    Items :: items(Element).

from_list(List) ->
    Root = xb5_items_node:new(),
    Size = 0,
    from_list_recur(List, Size, Root).

%%

-spec from_ordset(List) -> Items when
    List :: ordsets:ordset(Element),
    Items :: items(Element).

from_ordset(Ordset) ->
    List = ordsets:to_list(Ordset),
    from_list(List).

%%

-spec insert(Element, Items1) -> Items2 when
    Items1 :: items(Element),
    Items2 :: items(Element).

insert(Element, #xb5_items{size = Size, root = Root} = Items) ->
    case xb5_items_node:insert_att(Element, Root) of
        none ->
            error_key_exists(Element);
        %
        UpdatedRoot ->
            Items#xb5_items{size = Size + 1, root = UpdatedRoot}
    end.

%%

-spec is_empty(Items) -> boolean() when
    Items :: items().

is_empty(#xb5_items{size = Size}) ->
    Size =:= 0.

%%

is_member(Element, #xb5_items{root = Root}) ->
    xb5_items_node:is_member(Element, Root).

%%

-spec iterator(Items) -> Iter when
    Items :: items(Element),
    Iter :: iter(Element).

iterator(Items) ->
    iterator(Items, ordered).

%%

-spec iterator(Items, Order) -> Iter when
    Items :: items(Element),
    Iter :: iter(Element),
    Order :: ordered | reversed.

iterator(#xb5_items{root = Root}, Order) ->
    xb5_items_node:iterator(Root, Order).

%%

-spec iterator_from(Element, Items) -> Iter when
    Items :: items(Element),
    Iter :: iter(Element).

iterator_from(Element, Items) ->
    iterator_from(Element, Items, ordered).

%%

-spec iterator_from(Element, Items, Order) -> Iter when
    Items :: items(Element),
    Iter :: iter(Element),
    Order :: ordered | reversed.

iterator_from(Element, #xb5_items{root = Root}, Order) ->
    xb5_items_node:iterator_from(Element, Root, Order).

%%

-spec larger(Element1, Items) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Items :: items(Element).

larger(Element, #xb5_items{root = Root}) ->
    xb5_items_node:larger(Element, Root).

%%

-spec largest(Items) -> Element when
    Items :: items(Element).

largest(#xb5_items{size = Size, root = Root}) when Size =/= 0 ->
    xb5_items_node:largest(Root);
largest(#xb5_items{}) ->
    error_empty_items().

%%

-spec map(Fun, Items1) -> Items2 when
    Fun :: fun((Element1) -> Element2),
    Items1 :: items(Element1),
    Items2 :: items(Element2).

map(Fun, #xb5_items{root = Root} = Items) ->
    MappedRoot = xb5_items_node:map(Fun, Root),
    Items#xb5_items{root = MappedRoot}.

%%

-spec merge(Items1, Items2) -> Items3 when
    Items1 :: items(Element),
    Items2 :: items(Element),
    Items3 :: items(Element).

merge(
    #xb5_items{size = Size1, root = Root1} = Items1,
    #xb5_items{size = Size2, root = Root2}
) ->
    MergedRoot = xb5_items_node:merge(Size1, Root1, Size2, Root2),
    Items1#xb5_items{size = Size1 + Size2, root = MergedRoot}.

%%

-spec new() -> Items when
    Items :: items(_).

new() ->
    #xb5_items{size = 0, root = xb5_items_node:new()}.

%%

-spec next(Iter1) -> {Element, Iter2} | 'none' when
    Iter1 :: iter(Element),
    Iter2 :: iter(Element).

next(Iter) ->
    xb5_items_node:next(Iter).

%%

-spec nth(Rank, Items) -> Element when Rank :: pos_integer(), Items :: items(Element).

nth(Rank, #xb5_items{size = Size, root = Root}) when is_integer(Rank), Rank >= 1, Rank =< Size ->
    xb5_items_node:nth(Rank, Root);
nth(Rank, #xb5_items{}) ->
    error({badarg, Rank}).

%%

-spec percentile(Percentile, Items) -> {value, Element | InterpolationResult} | none when
    Percentile :: float() | 0 | 1,
    Items :: items(Element),
    InterpolationResult :: number().

percentile(Percentile, Items) ->
    percentile(Percentile, Items, []).

%%

-spec percentile(Percentile, Items, Opts) -> {value, Element | InterpolationResult} | none when
    Percentile :: float() | 0 | 1,
    Items :: items(Element),
    InterpolationResult :: number(),
    Opts :: [percentile_bracket_opt()].

percentile(Percentile, Items, Opts) ->
    Bracket = percentile_bracket(Percentile, Items, Opts),
    linear_interpolated_percentile(Bracket).

%%

-spec percentile_bracket(Percentile, Items) -> Bracket when
    Percentile :: float() | 0 | 1,
    Items :: items(Element),
    Bracket :: percentile_bracket(Element).

percentile_bracket(Percentile, Items) ->
    percentile_bracket(Percentile, Items, []).

%%

-spec percentile_bracket(Percentile, Items, Opts) -> Bracket when
    Percentile :: float() | 0 | 1,
    Items :: items(Element),
    Opts :: [percentile_bracket_opt()],
    Bracket :: percentile_bracket(Element).

percentile_bracket(Percentile, #xb5_items{size = Size, root = Root}, Opts) when
    is_number(Percentile), Percentile >= 0.0, Percentile =< 1.0
->
    case Size of
        0 ->
            none;
        %
        _ ->
            Method = proplists:get_value(method, Opts, inclusive),
            Pos = percentile_bracket_pos(Percentile, Size, Method),
            percentile_bracket_for_pos(Percentile, Pos, Size, Root, Method)
    end;
percentile_bracket(Percentile, #xb5_items{}, _Opts) ->
    error({badarg, Percentile}).

%%

-spec percentile_rank(Element, Items) -> Rank when
    Items :: items(Element),
    Rank :: float().

percentile_rank(Elem, #xb5_items{size = Size, root = Root}) when Size > 0 ->
    % As described in Wikipedia:
    % https://en.wikipedia.org/wiki/Percentile_rank
    Smaller = xb5_items_node:rank_smaller(Elem, Root),
    Larger = xb5_items_node:rank_larger(Elem, Root),

    [CF | F] = percentile_rank_params(Smaller, Larger, Size),

    (CF - 0.5 * F) / Size;
percentile_rank(_, #xb5_items{}) ->
    error_empty_items().

%%

-spec rank_larger(Element1, Items) -> {Rank, Element2} | none when
    Element1 :: Element,
    Element2 :: Element,
    Items :: items(Element),
    Rank :: pos_integer().

rank_larger(Elem, #xb5_items{root = Root}) ->
    case xb5_items_node:rank_larger(Elem, Root) of
        [Rank | Larger] ->
            {Rank, Larger};
        %
        none ->
            none
    end.

%%

-spec rank_smaller(Element1, Items) -> {Rank, Element2} | none when
    Element1 :: Element,
    Element2 :: Element,
    Items :: items(Element),
    Rank :: pos_integer().

rank_smaller(Elem, #xb5_items{root = Root}) ->
    case xb5_items_node:rank_smaller(Elem, Root) of
        [Rank | Smaller] ->
            {Rank, Smaller};
        %
        none ->
            none
    end.

%%

-spec size(Items) -> non_neg_integer() when
    Items :: items().

size(#xb5_items{size = Size}) ->
    Size.

%%

-spec smaller(Element1, Items) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Items :: items(Element).

smaller(Element, #xb5_items{root = Root}) ->
    xb5_items_node:smaller(Element, Root).

%%

-spec smallest(Items) -> Element when
    Items :: items(Element).

smallest(#xb5_items{size = Size, root = Root}) when Size =/= 0 ->
    xb5_items_node:smallest(Root);
smallest(#xb5_items{}) ->
    error_empty_items().

%%

-spec structural_stats(Items) -> Stats when
    Items :: items(),
    Stats :: xb5_structural_stats:t().

structural_stats(#xb5_items{root = Root}) ->
    xb5_items_node:structural_stats(Root).

%%

-spec take_largest(Items1) -> {Element, Items2} when
    Items1 :: items(Element),
    Items2 :: items(Element).

take_largest(#xb5_items{root = Root, size = Size} = Items) when Size =/= 0 ->
    [Largest | UpdatedRoot] = xb5_items_node:take_largest(Root),
    {Largest, Items#xb5_items{root = UpdatedRoot, size = Size - 1}};
take_largest(#xb5_items{}) ->
    error_empty_items().

%%

-spec take_smallest(Items1) -> {Element, Items2} when
    Items1 :: items(Element),
    Items2 :: items(Element).

take_smallest(#xb5_items{root = Root, size = Size} = Items) when Size =/= 0 ->
    [Smallest | UpdatedRoot] = xb5_items_node:take_smallest(Root),
    {Smallest, Items#xb5_items{root = UpdatedRoot, size = Size - 1}};
take_smallest(#xb5_items{}) ->
    error_empty_items().

%%

-spec to_list(Items) -> List when
    Items :: items(Element),
    List :: [Element].

to_list(#xb5_items{root = Root}) ->
    xb5_items_node:to_list(Root).

%%

-spec unwrap(Term) -> {ok, Unwrapped} | {error, Reason} when
    Term :: items(Element) | term(),
    Unwrapped :: unwrapped_items(Element),
    Reason :: term().

unwrap(#xb5_items{size = Size, root = Root} = Term) when is_integer(Size), Size >= 0 ->
    try xb5_items_node:structural_stats(Root) of
        Stats ->
            case lists:keyfind(total_keys, 1, Stats) of
                {_, TotalKeys} when TotalKeys =:= Size ->
                    {ok, #{size => Size, root => Root}};
                %
                {_, _} ->
                    {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_items_collection, Term})}
            end
    catch
        Class:Reason when Class =/= error; Reason =/= undef ->
            {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_items_collection, Term})}
    end;
unwrap(Term) ->
    {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_items_collection, Term})}.

%%

-spec wrap(unwrapped_items(Element)) -> items(Element).

wrap(#{root := Root, size := Size}) when is_integer(Size), Size >= 0 ->
    #xb5_items{root = Root, size = Size}.

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

from_list_recur([Element | Next], Size, Root) ->
    UpdatedRoot = xb5_items_node:add(Element, Root),
    from_list_recur(Next, Size + 1, UpdatedRoot);
from_list_recur([], Size, Root) ->
    #xb5_items{size = Size, root = Root}.

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
            ExactElem = xb5_items_node:nth(ExactRank, Root),
            {exact, ExactElem};
        %
        true ->
            [LowElem | HighElem] = xb5_items_node:nth_and_nthp1(LowRank, Root),

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
            % TODO avoid truncation when interpolating large enough integers (> 2**52)
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
