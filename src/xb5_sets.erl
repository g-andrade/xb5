-module(xb5_sets).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    add/2,
    % `sets' compatibility alias
    add_element/2,
    balance/1,
    % `sets' compatibility alias
    del_element/2,
    delete/2,
    delete_any/2,
    difference/2,
    empty/0,
    filter/2,
    filtermap/2,
    fold/3,
    from_list/1,
    from_ordset/1,
    insert/2,
    intersection/1,
    intersection/2,
    is_disjoint/2,
    % `sets' compatibility alias
    is_element/2,
    is_empty/1,
    is_equal/2,
    is_member/2,
    is_set/1,
    is_subset/2,
    iterator/1,
    iterator/2,
    iterator_from/2,
    iterator_from/3,
    larger/2,
    largest/1,
    map/2,
    new/0,
    next/1,
    singleton/1,
    size/1,
    smaller/2,
    smallest/1,
    structural_stats/1,
    % `sets' compatibility alias
    subtract/2,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    union/1,
    union/2,
    unwrap/1,
    wrap/1
]).

-ignore_xref([
    add/2,
    add_element/2,
    balance/1,
    del_element/2,
    delete/2,
    delete_any/2,
    difference/2,
    empty/0,
    filter/2,
    filtermap/2,
    fold/3,
    from_list/1,
    from_ordset/1,
    insert/2,
    intersection/1,
    intersection/2,
    is_disjoint/2,
    is_element/2,
    is_empty/1,
    is_equal/2,
    is_member/2,
    is_set/1,
    is_subset/2,
    iterator/1,
    iterator/2,
    iterator_from/2,
    iterator_from/3,
    larger/2,
    largest/1,
    map/2,
    new/0,
    next/1,
    singleton/1,
    size/1,
    smaller/2,
    smallest/1,
    structural_stats/1,
    subtract/2,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    union/1,
    union/2,
    unwrap/1,
    wrap/1
]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-record(xb5_set, {size, root}).

-opaque set(Element) :: #xb5_set{size :: non_neg_integer(), root :: xb5_sets_node:t(Element)}.
-export_type([set/1]).

-type set() :: set(_).
-export_type([set/0]).

-type iter(Element) :: xb5_sets_node:iter(Element).
-export_type([iter/1]).

-type iter() :: iter(_).
-export_type([iter/0]).

%%

-type unwrapped_set(Element) :: #{
    size := non_neg_integer(),
    root := xb5_sets_node:t(Element)
}.
-export_type([unwrapped_set/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec add(Element, Set1) -> Set2 when
    Set1 :: set(Element),
    Set2 :: set(Element).

add(Element, #xb5_set{size = Size, root = Root} = Set) ->
    case xb5_sets_node:insert_att(Element, Root) of
        none ->
            Set;
        %
        UpdatedRoot ->
            Set#xb5_set{size = Size + 1, root = UpdatedRoot}
    end.

%%

-spec add_element(Element, Set1) -> Set2 when
    Set1 :: set(Element),
    Set2 :: set(Element).

add_element(Element, Set) ->
    add(Element, Set).

%%

-spec balance(Set1) -> Set2 when
    Set1 :: set(Element),
    Set2 :: set(Element).

balance(#xb5_set{} = Set) ->
    % There's no need to balance, this function is only here to ease migration
    % from `gb_sets'.
    Set.

%%

-spec del_element(Element, Set1) -> Set2 when
    Element :: term(),
    Set1 :: set(Element),
    Set2 :: set(Element).

del_element(Element, Set) ->
    delete_any(Element, Set).

%%

-spec delete(Element, Set1) -> Set2 | no_return() when
    Element :: term(),
    Set1 :: set(Element),
    Set2 :: set(Element).

delete(Element, #xb5_set{size = Size, root = Root} = Set) ->
    case xb5_sets_node:delete_att(Element, Root) of
        none ->
            error_badkey(Element);
        %
        UpdatedRoot ->
            Set#xb5_set{size = Size - 1, root = UpdatedRoot}
    end.

%%

-spec delete_any(Element, Set1) -> Set2 when
    Element :: term(),
    Set1 :: set(Element),
    Set2 :: set(Element).

delete_any(Element, #xb5_set{size = Size, root = Root} = Set) ->
    case xb5_sets_node:delete_att(Element, Root) of
        none ->
            Set;
        %
        UpdatedRoot ->
            Set#xb5_set{size = Size - 1, root = UpdatedRoot}
    end.

%%

-spec difference(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).

difference(#xb5_set{size = Size1, root = Root1} = Set1, #xb5_set{root = Root2}) ->
    [RemovedCount | UpdatedRoot1] = xb5_sets_node:difference(Root1, Root2),
    Set1#xb5_set{size = Size1 - RemovedCount, root = UpdatedRoot1}.

%%

-spec empty() -> set(_).

empty() ->
    new().

%%

-spec filter(Pred, Set1) -> Set2 when
    Pred :: fun((Element) -> boolean()),
    Set1 :: set(Element),
    Set2 :: set(Element).

filter(Fun, #xb5_set{root = Root}) ->
    [FilteredSize | FilteredRoot] = xb5_sets_node:filter(Fun, Root),
    #xb5_set{size = FilteredSize, root = FilteredRoot}.

%%

-spec filtermap(Fun, Set1) -> Set2 when
    Fun :: fun((Element1) -> boolean() | {true, Element2}),
    Set1 :: set(Element1),
    Set2 :: set(Element1 | Element2).

filtermap(Fun, #xb5_set{root = Root}) ->
    [FilteredSize | FilteredRoot] = xb5_sets_node:filtermap(Fun, Root),
    #xb5_set{size = FilteredSize, root = FilteredRoot}.

%%

-spec fold(Function, Acc0, Set) -> Acc1 when
    Function :: fun((Element, AccIn) -> AccOut),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc,
    Set :: set(Element).

fold(Fun, Acc, #xb5_set{root = Root}) ->
    xb5_sets_node:fold(Fun, Acc, Root).

%%

-spec from_list(List) -> Set when
    List :: [Element],
    Set :: set(Element).

from_list(List) ->
    Root = xb5_sets_node:new(),
    Size = 0,
    from_list_recur(List, Root, Size).

%%

-spec from_ordset(List) -> Set when
    List :: ordsets:ordset(Element),
    Set :: set(Element).

from_ordset(Ordset) ->
    List = ordsets:to_list(Ordset),
    from_list(List).

%%

-spec insert(Element, Set1) -> Set2 when
    Set1 :: set(Element),
    Set2 :: set(Element).

insert(Element, #xb5_set{size = Size, root = Root} = Set) ->
    case xb5_sets_node:insert_att(Element, Root) of
        none ->
            error_key_exists(Element);
        %
        UpdatedRoot ->
            Set#xb5_set{size = Size + 1, root = UpdatedRoot}
    end.

%%

-spec intersection(SetList) -> Set when
    SetList :: [set(Element), ...],
    Set :: set(Element).

intersection(#xb5_set{root = Root1}, #xb5_set{root = Root2}) ->
    [NewSize | NewRoot] = xb5_sets_node:intersection(Root1, Root2),
    #xb5_set{size = NewSize, root = NewRoot}.

%%

-spec intersection(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).

intersection([#xb5_set{size = Size1, root = Root1} | Others]) ->
    intersection_recur(Root1, Size1, Others).

%%

-spec is_disjoint(Set1, Set2) -> boolean() when
    Set1 :: set(Element),
    Set2 :: set(Element).

is_disjoint(#xb5_set{root = Root1, size = Size1}, #xb5_set{root = Root2, size = Size2}) ->
    xb5_sets_node:is_disjoint(Root1, Size1, Root2, Size2).

%%

-spec is_element(Element, Set) -> boolean() when
    Set :: set(Element).

is_element(Element, Set) ->
    is_member(Element, Set).

%%

-spec is_empty(Set) -> boolean() when
    Set :: set().

is_empty(#xb5_set{size = Size}) ->
    Size =:= 0.

%%

-spec is_equal(Set1, Set2) -> boolean() when
    Set1 :: set(),
    Set2 :: set().

is_equal(#xb5_set{root = Root1, size = Size1}, #xb5_set{root = Root2, size = Size2}) ->
    (Size1 =:= Size2) andalso xb5_sets_node:is_equal(Root1, Root2).

%%

-spec is_member(Element, Set) -> boolean() when
    Set :: set(Element).

is_member(Element, #xb5_set{root = Root}) ->
    xb5_sets_node:is_member(Element, Root).

%%

-spec is_set(Term) -> boolean() when
    Term :: term().

is_set(#xb5_set{size = Size, root = Root}) ->
    xb5_sets_node:does_root_look_legit(Root, Size);
is_set(_) ->
    false.

%%

-spec is_subset(Set1, Set2) -> boolean() when
    Set1 :: set(Element),
    Set2 :: set(Element).

is_subset(#xb5_set{root = Root1}, #xb5_set{root = Root2}) ->
    xb5_sets_node:is_subset(Root1, Root2).

%%

-spec iterator(Set) -> Iter when
    Set :: set(Element),
    Iter :: iter(Element).

iterator(Set) ->
    iterator(Set, ordered).

%%

-spec iterator(Set, Order) -> Iter when
    Set :: set(Element),
    Iter :: iter(Element),
    Order :: ordered | reversed.

iterator(#xb5_set{root = Root}, Order) ->
    xb5_sets_node:iterator(Root, Order).

%%

-spec iterator_from(Element, Set) -> Iter when
    Set :: set(Element),
    Iter :: iter(Element).

iterator_from(Element, Set) ->
    iterator_from(Element, Set, ordered).

%%

-spec iterator_from(Element, Set, Order) -> Iter when
    Set :: set(Element),
    Iter :: iter(Element),
    Order :: ordered | reversed.

iterator_from(Element, #xb5_set{root = Root}, Order) ->
    xb5_sets_node:iterator_from(Element, Root, Order).

%%

-spec larger(Element1, Set) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Set :: set(Element).

larger(Element, #xb5_set{root = Root}) ->
    xb5_sets_node:larger(Element, Root).

%%

-spec largest(Set) -> Element when
    Set :: set(Element).

largest(#xb5_set{size = Size, root = Root}) when Size =/= 0 ->
    xb5_sets_node:largest(Root);
largest(#xb5_set{}) ->
    error_empty_set().

%%

-spec map(Fun, Set1) -> Set2 when
    Fun :: fun((Element1) -> Element2),
    Set1 :: set(Element1),
    Set2 :: set(Element2).

map(Fun, #xb5_set{root = Root}) ->
    [NewSize | MappedRoot] = xb5_sets_node:map(Fun, Root),
    #xb5_set{size = NewSize, root = MappedRoot}.

%%

-spec new() -> Set when
    Set :: set(_).

new() ->
    #xb5_set{size = 0, root = xb5_sets_node:new()}.

%%

-spec next(Iter1) -> {Element, Iter2} | none when
    Iter1 :: iter(Element),
    Iter2 :: iter(Element).

next(Iter) ->
    xb5_sets_node:next(Iter).

%%

-spec singleton(Element) -> set(Element).

singleton(Element) ->
    #xb5_set{size = 1, root = xb5_sets_node:singleton(Element)}.

%%

-spec size(Set) -> non_neg_integer() when
    Set :: set().

size(#xb5_set{size = Size}) ->
    Size.

%%

-spec smaller(Element1, Set) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Set :: set(Element).

smaller(Element, #xb5_set{root = Root}) ->
    xb5_sets_node:smaller(Element, Root).

%%

-spec smallest(Set) -> Element when
    Set :: set(Element).

smallest(#xb5_set{size = Size, root = Root}) when Size =/= 0 ->
    xb5_sets_node:smallest(Root);
smallest(#xb5_set{}) ->
    error_empty_set().

%%

-spec structural_stats(Set) -> Stats when
    Set :: set(),
    Stats :: xb5_structural_stats:t().

structural_stats(#xb5_set{root = Root}) ->
    xb5_sets_node:structural_stats(Root).

%%

-spec subtract(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).

subtract(Set1, Set2) ->
    difference(Set1, Set2).

%%

-spec take_largest(Set1) -> {Element, Set2} when
    Set1 :: set(Element),
    Set2 :: set(Element).

take_largest(#xb5_set{size = Size, root = Root} = Set) when Size =/= 0 ->
    [Largest | UpdatedRoot] = xb5_sets_node:take_largest(Root),
    {Largest, Set#xb5_set{root = UpdatedRoot, size = Size - 1}};
take_largest(#xb5_set{}) ->
    error_empty_set().

%%

-spec take_smallest(Set1) -> {Element, Set2} when
    Set1 :: set(Element),
    Set2 :: set(Element).

take_smallest(#xb5_set{size = Size, root = Root} = Set) when Size =/= 0 ->
    [Smallest | UpdatedRoot] = xb5_sets_node:take_smallest(Root),
    {Smallest, Set#xb5_set{root = UpdatedRoot, size = Size - 1}};
take_smallest(#xb5_set{}) ->
    error_empty_set().

%%

-spec to_list(Set) -> List when
    Set :: set(Element),
    List :: [Element].

to_list(#xb5_set{root = Root}) ->
    xb5_sets_node:to_list(Root).

%%

-spec union(SetList) -> Set when
    SetList :: [set(Element)],
    Set :: set(Element).

union([#xb5_set{size = Size1, root = Root1} | Others]) ->
    union_recur(Root1, Size1, Others);
union([]) ->
    new().

%%

-spec union(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).

union(#xb5_set{root = Root1, size = Size1}, #xb5_set{root = Root2, size = Size2}) ->
    [NewSize | NewRoot] = xb5_sets_node:union(Root1, Size1, Root2, Size2),
    #xb5_set{size = NewSize, root = NewRoot}.

%%

-spec unwrap(Term) -> {ok, Unwrapped} | {error, Reason} when
    Term :: set(Element) | term(),
    Unwrapped :: unwrapped_set(Element),
    Reason :: term().

unwrap(#xb5_set{size = Size, root = Root} = Term) when is_integer(Size), Size >= 0 ->
    try xb5_sets_node:structural_stats(Root) of
        Stats ->
            case lists:keyfind(total_keys, 1, Stats) of
                {_, TotalKeys} when TotalKeys =:= Size ->
                    {ok, #{size => Size, root => Root}};
                %
                {_, _} ->
                    {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_set, Term})}
            end
    catch
        Class:Reason when Class =/= error; Reason =/= undef ->
            {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_set, Term})}
    end;
unwrap(Term) ->
    {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_set, Term})}.

%%

-spec wrap(unwrapped_set(Element)) -> set(Element).

wrap(#{root := Root, size := Size}) when is_integer(Size), Size >= 0 ->
    #xb5_set{root = Root, size = Size}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-compile({inline, error_badkey/1}).
-spec error_badkey(_) -> no_return().
error_badkey(Elem) ->
    error({badkey, Elem}).

-compile({inline, error_empty_set/0}).
-spec error_empty_set() -> no_return().
error_empty_set() ->
    error(empty_set).

-compile({inline, error_key_exists/1}).
-spec error_key_exists(_) -> no_return().
error_key_exists(Elem) ->
    error({key_exists, Elem}).

%%

from_list_recur([Element | Next], Root, Size) ->
    case xb5_sets_node:insert_att(Element, Root) of
        none ->
            from_list_recur(Next, Root, Size);
        %
        UpdatedRoot ->
            UpdatedSize = Size + 1,
            from_list_recur(Next, UpdatedRoot, UpdatedSize)
    end;
from_list_recur([], Root, Size) ->
    #xb5_set{size = Size, root = Root}.

%%

intersection_recur(Root1, _Size1, [#xb5_set{root = Root2} | Next]) ->
    [NewSize | NewRoot] = xb5_sets_node:intersection(Root1, Root2),
    intersection_recur(NewRoot, NewSize, Next);
intersection_recur(Root, Size, []) ->
    #xb5_set{size = Size, root = Root}.

%%

union_recur(Root1, Size1, [#xb5_set{root = Root2, size = Size2} | Next]) ->
    [NewSize | NewRoot] = xb5_sets_node:union(Root1, Size1, Root2, Size2),
    union_recur(NewRoot, NewSize, Next);
union_recur(Root, Size, []) ->
    #xb5_set{size = Size, root = Root}.
