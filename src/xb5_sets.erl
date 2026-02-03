-module(xb5_sets).

-moduledoc """
An ordered set implementation using a B-tree of order 5.

The representation of a set is not defined and is opaque to the user.
Elements are ordered using the Erlang term order, comparing with `==`
rather than `=:=`. This means that `1` and `1.0` are considered the same
element.

Unlike `m:gb_sets`, the tree is always balanced after every insertion and
deletion; there is no need to call `balance/1` explicitly. That function
exists only to ease migration from `m:gb_sets`.

See also `m:gb_sets` for a similar API, and `m:xb5_trees` for the
key-value counterpart.
""".

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
%% Linter Tweaks
%% ------------------------------------------------------------------

-elvis([
    % Large URLs below require this
    {elvis_text_style, line_length, #{limit => 105}}
]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-record(xb5_set, {size, root}).

-doc "An ordered set containing elements of type `Element`.".
-opaque set(Element) :: #xb5_set{size :: non_neg_integer(), root :: xb5_sets_node:t(Element)}.
-export_type([set/1]).

-doc "Shorthand for `set(_)`.".
-type set() :: set(_).
-export_type([set/0]).

-doc "An iterator over elements of type `Element`. See `iterator/1` and `next/1`.".
-type iter(Element) :: xb5_sets_node:iter(Element).
-export_type([iter/1]).

-doc "Shorthand for `iter(_)`.".
-type iter() :: iter(_).
-export_type([iter/0]).

%%

-doc """
A plain-map representation of a set, suitable for cross-language
serialization (for example, converting to or from an Elixir struct that
uses the same underlying node structures).

See `unwrap/1` and `wrap/1`.
""".
-type unwrapped_set(Element) :: #{
    size := non_neg_integer(),
    root := xb5_sets_node:t(Element)
}.
-export_type([unwrapped_set/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-doc """
Adds element `Element` to set `Set1`, returning a new set `Set2`.
If `Element` is already a member of `Set1`, `Set1` is returned unchanged.

## Examples

```erlang
> S0 = xb5_sets:new().
> xb5_sets:to_list(xb5_sets:add(1, S0)).
[1]
> S1 = xb5_sets:from_list([1, 2, 3]).
> xb5_sets:to_list(xb5_sets:add(2, S1)).
[1, 2, 3]
> xb5_sets:to_list(xb5_sets:add(4, S1)).
[1, 2, 3, 4]
```
""".
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

-doc #{equiv => add / 2}.
-spec add_element(Element, Set1) -> Set2 when
    Set1 :: set(Element),
    Set2 :: set(Element).

add_element(Element, Set) ->
    add(Element, Set).

%%

-doc """
Returns `Set` unchanged.

This function exists only to ease migration from `m:gb_sets`. Since xb5
B-trees are always balanced, calling this function is never necessary.

## Examples

```erlang
> S = xb5_sets:from_list([3, 1, 2]).
> xb5_sets:to_list(xb5_sets:balance(S)).
[1, 2, 3]
```
""".
-spec balance(Set1) -> Set2 when
    Set1 :: set(Element),
    Set2 :: set(Element).

balance(#xb5_set{} = Set) ->
    % There's no need to balance, this function is only here to ease migration
    % from `gb_sets'.
    Set.

%%

-doc #{equiv => delete_any / 2}.
-spec del_element(Element, Set1) -> Set2 when
    Element :: term(),
    Set1 :: set(Element),
    Set2 :: set(Element).

del_element(Element, Set) ->
    delete_any(Element, Set).

%%

-doc """
Removes element `Element` from set `Set1`, returning a new set `Set2`.

Raises a `{badkey, Element}` error if `Element` is not a member of `Set1`.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> xb5_sets:to_list(xb5_sets:delete(2, S)).
[1, 3]
```
""".
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

-doc """
Removes element `Element` from set `Set1` if present, returning a new set
`Set2`. If `Element` is not a member, `Set1` is returned unchanged.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> xb5_sets:to_list(xb5_sets:delete_any(2, S)).
[1, 3]
> xb5_sets:to_list(xb5_sets:delete_any(42, S)).
[1, 2, 3]
```
""".
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

-doc """
Returns the elements of `Set1` that are not in `Set2`.

## Examples

```erlang
> S1 = xb5_sets:from_list([1, 2, 3, 4]).
> S2 = xb5_sets:from_list([2, 4, 5]).
> xb5_sets:to_list(xb5_sets:difference(S1, S2)).
[1, 3]
```
""".
-spec difference(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).

difference(#xb5_set{size = Size1, root = Root1} = Set1, #xb5_set{root = Root2}) ->
    [RemovedCount | UpdatedRoot1] = xb5_sets_node:difference(Root1, Root2),
    Set1#xb5_set{size = Size1 - RemovedCount, root = UpdatedRoot1}.

%%

-doc #{equiv => new / 0}.
-spec empty() -> set(_).

empty() ->
    new().

%%

-doc """
Filters elements of `Set1` using predicate function `Pred`, returning a
new set `Set2` containing only the elements for which `Pred` returns `true`.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3, 4, 5]).
> xb5_sets:to_list(xb5_sets:filter(fun(X) -> X > 3 end, S)).
[4, 5]
```
""".
-spec filter(Pred, Set1) -> Set2 when
    Pred :: fun((Element) -> boolean()),
    Set1 :: set(Element),
    Set2 :: set(Element).

filter(Fun, #xb5_set{root = Root}) ->
    [FilteredSize | FilteredRoot] = xb5_sets_node:filter(Fun, Root),
    #xb5_set{size = FilteredSize, root = FilteredRoot}.

%%

-doc """
Filters and maps elements of `Set1` using `Fun`, returning a new set `Set2`.

For each element, `Fun` must return either `true` (keep the element),
`false` (discard it), or `{true, NewElement}` (replace it with `NewElement`).

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3, 4, 5]).
> xb5_sets:to_list(xb5_sets:filtermap(fun(X) when X rem 2 =:= 0 -> {true, X * 10}; (_) -> false end, S)).
[20, 40]
```
""".
-spec filtermap(Fun, Set1) -> Set2 when
    Fun :: fun((Element1) -> boolean() | {true, Element2}),
    Set1 :: set(Element1),
    Set2 :: set(Element1 | Element2).

filtermap(Fun, #xb5_set{root = Root}) ->
    [FilteredSize | FilteredRoot] = xb5_sets_node:filtermap(Fun, Root),
    #xb5_set{size = FilteredSize, root = FilteredRoot}.

%%

-doc """
Folds `Function` over every element in `Set`, returning the final
accumulator value. Elements are visited in Erlang term order.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> xb5_sets:fold(fun(X, Acc) -> X + Acc end, 0, S).
6
```
""".
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

-doc """
Returns a set of the elements in `List`. Duplicate elements are removed.

## Examples

```erlang
> xb5_sets:to_list(xb5_sets:from_list([3, 1, 2, 1])).
[1, 2, 3]
> xb5_sets:to_list(xb5_sets:from_list([])).
[]
```
""".
-spec from_list(List) -> Set when
    List :: [Element],
    Set :: set(Element).

from_list(List) ->
    Root = xb5_sets_node:new(),
    Size = 0,
    from_list_recur(List, Root, Size).

%%

-doc """
Returns a set built from the ordered set `Ordset`.

## Examples

```erlang
> xb5_sets:to_list(xb5_sets:from_ordset([1, 2, 3])).
[1, 2, 3]
```
""".
-spec from_ordset(List) -> Set when
    List :: ordsets:ordset(Element),
    Set :: set(Element).

from_ordset(Ordset) ->
    List = ordsets:to_list(Ordset),
    from_list(List).

%%

-doc """
Inserts element `Element` into set `Set1`, returning a new set `Set2`.

Raises a `{key_exists, Element}` error if `Element` is already a member
of `Set1`.

## Examples

```erlang
> S0 = xb5_sets:new().
> S1 = xb5_sets:insert(1, S0).
> xb5_sets:to_list(S1).
[1]
```
""".
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

-doc """
Returns the intersection of `Set1` and `Set2`.

## Examples

```erlang
> S1 = xb5_sets:from_list([1, 2, 3, 4]).
> S2 = xb5_sets:from_list([2, 4, 5, 6]).
> xb5_sets:to_list(xb5_sets:intersection(S1, S2)).
[2, 4]
```
""".
-spec intersection(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).

intersection(#xb5_set{root = Root1}, #xb5_set{root = Root2}) ->
    [NewSize | NewRoot] = xb5_sets_node:intersection(Root1, Root2),
    #xb5_set{size = NewSize, root = NewRoot}.

%%

-doc """
Returns the intersection of the non-empty list of sets.

## Examples

```erlang
> S1 = xb5_sets:from_list([1, 2, 3]).
> S2 = xb5_sets:from_list([2, 3, 4]).
> S3 = xb5_sets:from_list([3, 4, 5]).
> xb5_sets:to_list(xb5_sets:intersection([S1, S2, S3])).
[3]
```
""".
-spec intersection(SetList) -> Set when
    SetList :: [set(Element), ...],
    Set :: set(Element).

intersection([#xb5_set{size = Size1, root = Root1} | Others]) ->
    intersection_recur(Root1, Size1, Others).

%%

-doc """
Returns `true` if `Set1` and `Set2` are disjoint (have no elements in
common), otherwise `false`.

## Examples

```erlang
> S1 = xb5_sets:from_list([1, 2, 3]).
> S2 = xb5_sets:from_list([4, 5, 6]).
> xb5_sets:is_disjoint(S1, S2).
true
> S3 = xb5_sets:from_list([3, 4, 5]).
> xb5_sets:is_disjoint(S1, S3).
false
```
""".
-spec is_disjoint(Set1, Set2) -> boolean() when
    Set1 :: set(Element),
    Set2 :: set(Element).

is_disjoint(#xb5_set{root = Root1, size = Size1}, #xb5_set{root = Root2, size = Size2}) ->
    xb5_sets_node:is_disjoint(Root1, Size1, Root2, Size2).

%%

-doc #{equiv => is_member / 2}.
-spec is_element(Element, Set) -> boolean() when
    Set :: set(Element).

is_element(Element, Set) ->
    is_member(Element, Set).

%%

-doc """
Returns `true` if `Set` is empty, otherwise `false`.

## Examples

```erlang
> xb5_sets:is_empty(xb5_sets:new()).
true
> xb5_sets:is_empty(xb5_sets:from_list([1])).
false
```
""".
-spec is_empty(Set) -> boolean() when
    Set :: set().

is_empty(#xb5_set{size = Size}) ->
    Size =:= 0.

%%

-doc """
Returns `true` if `Set1` and `Set2` contain the same elements, otherwise
`false`.

## Examples

```erlang
> S1 = xb5_sets:from_list([1, 2, 3]).
> S2 = xb5_sets:from_list([3, 1, 2]).
> xb5_sets:is_equal(S1, S2).
true
> S3 = xb5_sets:from_list([1, 2]).
> xb5_sets:is_equal(S1, S3).
false
```
""".
-spec is_equal(Set1, Set2) -> boolean() when
    Set1 :: set(),
    Set2 :: set().

is_equal(#xb5_set{root = Root1, size = Size1}, #xb5_set{root = Root2, size = Size2}) ->
    (Size1 =:= Size2) andalso xb5_sets_node:is_equal(Root1, Root2).

%%

-doc """
Returns `true` if `Element` is a member of `Set`, otherwise `false`.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> xb5_sets:is_member(2, S).
true
> xb5_sets:is_member(4, S).
false
```
""".
-spec is_member(Element, Set) -> boolean() when
    Set :: set(Element).

is_member(Element, #xb5_set{root = Root}) ->
    xb5_sets_node:is_member(Element, Root).

%%

-doc """
Returns `true` if `Term` appears to be a set, otherwise `false`.

## Examples

```erlang
> xb5_sets:is_set(xb5_sets:new()).
true
> xb5_sets:is_set(xb5_sets:from_list([1, 2])).
true
> xb5_sets:is_set(not_a_set).
false
```
""".
-spec is_set(Term) -> boolean() when
    Term :: term().

is_set(#xb5_set{size = Size, root = Root}) ->
    xb5_sets_node:does_root_look_legit(Root, Size);
is_set(_) ->
    false.

%%

-doc """
Returns `true` if every element of `Set1` is also a member of `Set2`,
otherwise `false`.

## Examples

```erlang
> S1 = xb5_sets:from_list([1, 2]).
> S2 = xb5_sets:from_list([1, 2, 3]).
> xb5_sets:is_subset(S1, S2).
true
> xb5_sets:is_subset(S2, S1).
false
```
""".
-spec is_subset(Set1, Set2) -> boolean() when
    Set1 :: set(Element),
    Set2 :: set(Element).

is_subset(#xb5_set{root = Root1}, #xb5_set{root = Root2}) ->
    xb5_sets_node:is_subset(Root1, Root2).

%%

-doc """
Returns an iterator that can be used for traversing the entries of `Set`;
see `next/1`. Equivalent to `iterator(Set, ordered)`.

## Examples

```erlang
> S = xb5_sets:from_list([3, 1, 2]).
> I = xb5_sets:iterator(S).
> {1, I2} = xb5_sets:next(I).
> {2, I3} = xb5_sets:next(I2).
> {3, I4} = xb5_sets:next(I3).
> xb5_sets:next(I4).
none
```
""".
-spec iterator(Set) -> Iter when
    Set :: set(Element),
    Iter :: iter(Element).

iterator(Set) ->
    iterator(Set, ordered).

%%

-doc """
Returns an iterator that can be used for traversing the entries of `Set`
in the given `Order`; see `next/1`.

`Order` must be `ordered` (ascending) or `reversed` (descending).

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> I = xb5_sets:iterator(S, reversed).
> {3, I2} = xb5_sets:next(I).
> {2, I3} = xb5_sets:next(I2).
> {1, I4} = xb5_sets:next(I3).
> xb5_sets:next(I4).
none
```
""".
-spec iterator(Set, Order) -> Iter when
    Set :: set(Element),
    Iter :: iter(Element),
    Order :: ordered | reversed.

iterator(#xb5_set{root = Root}, Order) ->
    xb5_sets_node:iterator(Root, Order).

%%

-doc """
Returns an iterator that can be used for traversing the entries of `Set`
starting from element `Element`; see `next/1`. Equivalent to
`iterator_from(Element, Set, ordered)`.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3, 4, 5]).
> I = xb5_sets:iterator_from(3, S).
> {3, I2} = xb5_sets:next(I).
> {4, I3} = xb5_sets:next(I2).
> {5, I4} = xb5_sets:next(I3).
> xb5_sets:next(I4).
none
```
""".
-spec iterator_from(Element, Set) -> Iter when
    Set :: set(Element),
    Iter :: iter(Element).

iterator_from(Element, Set) ->
    iterator_from(Element, Set, ordered).

%%

-doc """
Returns an iterator that can be used for traversing the entries of `Set`
starting from element `Element` in the given `Order`; see `next/1`.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3, 4, 5]).
> I = xb5_sets:iterator_from(3, S, reversed).
> {3, I2} = xb5_sets:next(I).
> {2, I3} = xb5_sets:next(I2).
> {1, I4} = xb5_sets:next(I3).
> xb5_sets:next(I4).
none
```
""".
-spec iterator_from(Element, Set, Order) -> Iter when
    Set :: set(Element),
    Iter :: iter(Element),
    Order :: ordered | reversed.

iterator_from(Element, #xb5_set{root = Root}, Order) ->
    xb5_sets_node:iterator_from(Element, Root, Order).

%%

-doc """
Returns `{found, Element2}` where `Element2` is the smallest element
in `Set` that is strictly greater than `Element1`, or `none` if no such
element exists.

## Examples

```erlang
> S = xb5_sets:from_list([1, 3, 5]).
> xb5_sets:larger(2, S).
{found, 3}
> xb5_sets:larger(5, S).
none
```
""".
-spec larger(Element1, Set) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Set :: set(Element).

larger(Element, #xb5_set{root = Root}) ->
    xb5_sets_node:larger(Element, Root).

%%

-doc """
Returns the largest element in `Set`. Raises an `empty_set` error if the
set is empty.

## Examples

```erlang
> xb5_sets:largest(xb5_sets:from_list([1, 2, 3])).
3
```
""".
-spec largest(Set) -> Element when
    Set :: set(Element).

largest(#xb5_set{size = Size, root = Root}) when Size =/= 0 ->
    xb5_sets_node:largest(Root);
largest(#xb5_set{}) ->
    error_empty_set().

%%

-doc """
Maps `Fun` over all elements of `Set1`, returning a new set `Set2`.
Duplicates arising from the mapping are removed.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> xb5_sets:to_list(xb5_sets:map(fun(X) -> X * 10 end, S)).
[10, 20, 30]
```
""".
-spec map(Fun, Set1) -> Set2 when
    Fun :: fun((Element1) -> Element2),
    Set1 :: set(Element1),
    Set2 :: set(Element2).

map(Fun, #xb5_set{root = Root}) ->
    [NewSize | MappedRoot] = xb5_sets_node:map(Fun, Root),
    #xb5_set{size = NewSize, root = MappedRoot}.

%%

-doc """
Returns a new empty set.

## Examples

```erlang
> xb5_sets:is_empty(xb5_sets:new()).
true
> xb5_sets:size(xb5_sets:new()).
0
```
""".
-spec new() -> Set when
    Set :: set(_).

new() ->
    #xb5_set{size = 0, root = xb5_sets_node:new()}.

%%

-doc """
Returns `{Element, Iter2}` where `Element` is the next element referred
to by iterator `Iter1` and `Iter2` is the updated iterator, or `none`
if no more elements remain.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2]).
> I = xb5_sets:iterator(S).
> {1, I2} = xb5_sets:next(I).
> {2, I3} = xb5_sets:next(I2).
> xb5_sets:next(I3).
none
```
""".
-spec next(Iter1) -> {Element, Iter2} | none when
    Iter1 :: iter(Element),
    Iter2 :: iter(Element).

next(Iter) ->
    xb5_sets_node:next(Iter).

%%

-doc """
Returns a set containing only element `Element`.

## Examples

```erlang
> xb5_sets:to_list(xb5_sets:singleton(42)).
[42]
> xb5_sets:size(xb5_sets:singleton(42)).
1
```
""".
-spec singleton(Element) -> set(Element).

singleton(Element) ->
    #xb5_set{size = 1, root = xb5_sets_node:singleton(Element)}.

%%

-doc """
Returns the number of elements in `Set`.

## Examples

```erlang
> xb5_sets:size(xb5_sets:new()).
0
> xb5_sets:size(xb5_sets:from_list([1, 2, 3])).
3
```
""".
-spec size(Set) -> non_neg_integer() when
    Set :: set().

size(#xb5_set{size = Size}) ->
    Size.

%%

-doc """
Returns `{found, Element2}` where `Element2` is the largest element
in `Set` that is strictly less than `Element1`, or `none` if no such
element exists.

## Examples

```erlang
> S = xb5_sets:from_list([1, 3, 5]).
> xb5_sets:smaller(4, S).
{found, 3}
> xb5_sets:smaller(1, S).
none
```
""".
-spec smaller(Element1, Set) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Set :: set(Element).

smaller(Element, #xb5_set{root = Root}) ->
    xb5_sets_node:smaller(Element, Root).

%%

-doc """
Returns the smallest element in `Set`. Raises an `empty_set` error if the
set is empty.

## Examples

```erlang
> xb5_sets:smallest(xb5_sets:from_list([3, 1, 2])).
1
```
""".
-spec smallest(Set) -> Element when
    Set :: set(Element).

smallest(#xb5_set{size = Size, root = Root}) when Size =/= 0 ->
    xb5_sets_node:smallest(Root);
smallest(#xb5_set{}) ->
    error_empty_set().

%%

-doc """
Returns structural statistics about the B-tree backing `Set`.

This is primarily intended for debugging and testing. The result
is a proplist with keys such as `height`, `total_keys`,
`node_counts`, `node_percentages`, and others.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> Stats = xb5_sets:structural_stats(S).
> {height, 1} = lists:keyfind(height, 1, Stats).
> {total_keys, 3} = lists:keyfind(total_keys, 1, Stats).
```
""".
-spec structural_stats(Set) -> Stats when
    Set :: set(),
    Stats :: xb5_structural_stats:t().

structural_stats(#xb5_set{root = Root}) ->
    xb5_sets_node:structural_stats(Root).

%%

-doc #{equiv => difference / 2}.
-spec subtract(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).

subtract(Set1, Set2) ->
    difference(Set1, Set2).

%%

-doc """
Returns `{Element, Set2}`, where `Element` is the largest element in
`Set1` and `Set2` is the remaining set. Raises an `empty_set` error
if the set is empty.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> {3, S2} = xb5_sets:take_largest(S).
> xb5_sets:to_list(S2).
[1, 2]
```
""".
-spec take_largest(Set1) -> {Element, Set2} when
    Set1 :: set(Element),
    Set2 :: set(Element).

take_largest(#xb5_set{size = Size, root = Root} = Set) when Size =/= 0 ->
    [Largest | UpdatedRoot] = xb5_sets_node:take_largest(Root),
    {Largest, Set#xb5_set{root = UpdatedRoot, size = Size - 1}};
take_largest(#xb5_set{}) ->
    error_empty_set().

%%

-doc """
Returns `{Element, Set2}`, where `Element` is the smallest element in
`Set1` and `Set2` is the remaining set. Raises an `empty_set` error
if the set is empty.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> {1, S2} = xb5_sets:take_smallest(S).
> xb5_sets:to_list(S2).
[2, 3]
```
""".
-spec take_smallest(Set1) -> {Element, Set2} when
    Set1 :: set(Element),
    Set2 :: set(Element).

take_smallest(#xb5_set{size = Size, root = Root} = Set) when Size =/= 0 ->
    [Smallest | UpdatedRoot] = xb5_sets_node:take_smallest(Root),
    {Smallest, Set#xb5_set{root = UpdatedRoot, size = Size - 1}};
take_smallest(#xb5_set{}) ->
    error_empty_set().

%%

-doc """
Returns the elements of `Set` as an ordered list.

## Examples

```erlang
> xb5_sets:to_list(xb5_sets:from_list([3, 1, 2])).
[1, 2, 3]
> xb5_sets:to_list(xb5_sets:new()).
[]
```
""".
-spec to_list(Set) -> List when
    Set :: set(Element),
    List :: [Element].

to_list(#xb5_set{root = Root}) ->
    xb5_sets_node:to_list(Root).

%%

-doc """
Returns the union of a list of sets.

## Examples

```erlang
> S1 = xb5_sets:from_list([1, 2]).
> S2 = xb5_sets:from_list([2, 3]).
> S3 = xb5_sets:from_list([3, 4]).
> xb5_sets:to_list(xb5_sets:union([S1, S2, S3])).
[1, 2, 3, 4]
> xb5_sets:to_list(xb5_sets:union([])).
[]
```
""".
-spec union(SetList) -> Set when
    SetList :: [set(Element)],
    Set :: set(Element).

union([#xb5_set{size = Size1, root = Root1} | Others]) ->
    union_recur(Root1, Size1, Others);
union([]) ->
    new().

%%

-doc """
Returns the union of `Set1` and `Set2`.

## Examples

```erlang
> S1 = xb5_sets:from_list([1, 2, 3]).
> S2 = xb5_sets:from_list([3, 4, 5]).
> xb5_sets:to_list(xb5_sets:union(S1, S2)).
[1, 2, 3, 4, 5]
```
""".
-spec union(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).

union(#xb5_set{root = Root1, size = Size1}, #xb5_set{root = Root2, size = Size2}) ->
    [NewSize | NewRoot] = xb5_sets_node:union(Root1, Size1, Root2, Size2),
    #xb5_set{size = NewSize, root = NewRoot}.

%%

-doc """
Unwraps an opaque set into a plain map representation suitable for
cross-language interop (for example, converting to an Elixir struct
that uses the same underlying node module). Returns `{ok, Unwrapped}`
on success or `{error, Reason}` if `Term` is not a valid set.

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> {ok, Unwrapped} = xb5_sets:unwrap(S).
> maps:get(size, Unwrapped).
3
> {error, _} = xb5_sets:unwrap(not_a_set).
```
""".
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

-doc """
Wraps a plain map representation back into an opaque set.

This is the inverse of `unwrap/1` and is intended for cross-language
interop (for example, converting from an Elixir struct that shares the
same underlying node module).

## Examples

```erlang
> S = xb5_sets:from_list([1, 2, 3]).
> {ok, U} = xb5_sets:unwrap(S).
> S2 = xb5_sets:wrap(U).
> xb5_sets:to_list(S2).
[1, 2, 3]
```
""".
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
