-module(xb5_bag).

-moduledoc """
An ordered [multiset](https://en.wikipedia.org/wiki/Multiset) (bag)
implementation using a [B-tree](https://en.wikipedia.org/wiki/B-tree) of order
5.

Elements are ordered using the Erlang term order, comparing with `==`
rather than `=:=`. This means that `1` and `1.0` are considered the same
element.

**Unlike `m:xb5_sets`, duplicate elements are preserved**: adding an element
that is already present increases its count rather than being a no-op.

The tree is always balanced after every insertion and deletion.

In addition to the standard container operations, **`xb5_bag` supports
[order-statistic](https://en.wikipedia.org/wiki/Order_statistic_tree)
queries: `nth/2`, `rank/2`, and percentile functions**
(`percentile/2`, `percentile/3`, `percentile_bracket/2`,
`percentile_bracket/3`, `percentile_rank/2`). These all run in
logarithmic time.

See also:
- `m:xb5_sets` for the unique-element counterpart, supporting set operations (union, intersection, difference)
- `m:xb5_trees` for the key-value counterpart
""".

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    add/2,
    delete/2,
    delete_any/2,
    enter/2,
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
    rank/2,
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

-ignore_xref([
    add/2,
    delete/2,
    delete_any/2,
    enter/2,
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
    rank/2,
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
    {elvis_text_style, line_length, #{limit => 131}}
]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-record(xb5_bag, {size, root}).

-doc "An ordered multiset (bag) containing elements of type `Element`.".
-opaque bag(E) :: #xb5_bag{
    size :: non_neg_integer(), root :: xb5_bag_node:t(E)
}.
-export_type([bag/1]).

-doc "Shorthand for `bag(_)`.".
-type bag() :: bag(_).
-export_type([bag/0]).

-doc "An iterator over elements of type `Element`. See `iterator/1` and `next/1`.".
-opaque iter(Element) :: xb5_bag_node:iter(Element).
-export_type([iter/1]).

-doc "Shorthand for `iter(_)`.".
-type iter() :: iter(_).
-export_type([iter/0]).

%%%

-doc "An option for percentile functions. Currently only `{method, Method}` is supported; see `t:percentile_bracket_method/0`.".
-type percentile_bracket_opt() ::
    ({method, percentile_bracket_method()}).
-export_type([percentile_bracket_opt/0]).

-doc """
The method used to calculate a percentile bracket.

- `inclusive` (default) -- Pos = 1 + (N - 1) * P. Equivalent to Excel
  [PERCENTILE.INC](https://support.microsoft.com/en-us/office/percentile-inc-function-680f9539-45eb-410b-9a5e-c1355e5fe2ed)
  (Hyndman-Fan Type 7). Covers the full `[0.0, 1.0]` range.

- `exclusive` -- Pos = (N + 1) * P. Equivalent to Excel
  [PERCENTILE.EXC](https://support.microsoft.com/en-us/office/percentile-exc-function-bbaa7204-e9e1-4010-85bf-c31dc5dce4ba)
  (Hyndman-Fan Type 6). Returns `none` for percentiles outside the
  representable range.

- `nearest_rank` -- Pos = ceil(P * N). As described in
  [Wikipedia](https://en.wikipedia.org/wiki/Percentile#The_nearest-rank_method).
  Always returns an exact element (no interpolation). Returns `none` for
  percentile `0`.
""".
-type percentile_bracket_method() ::
    (inclusive
    | exclusive
    | nearest_rank).

-export_type([percentile_bracket_method/0]).

-doc """
The result of a percentile bracket calculation.

- `{exact, Element}` -- the percentile falls exactly on an element.
- `{between, Low, High}` -- the percentile falls between two elements.
  `Low` and `High` are `t:percentile_bracket_bound/1` maps with
  interpolation weights.
- `none` -- the percentile cannot be calculated (empty bag, or out of
  range for the chosen method).
""".
-type percentile_bracket(Element) ::
    ({exact, Element}
    | {between, percentile_bracket_bound(Element), percentile_bracket_bound(Element)}
    | none).
-export_type([percentile_bracket/1]).

-doc "A map with keys `percentile` (the element's percentile), `weight` (interpolation weight), and `value` (the element itself).".
-type percentile_bracket_bound(Element) :: #{
    percentile := float(),
    weight := float(),
    value := Element
}.
-export_type([percentile_bracket_bound/1]).

%%

-doc """
A plain-map representation of a bag, suitable for cross-language
serialization (for example, converting to or from an Elixir struct that
uses the same underlying node structures).

See `unwrap/1` and `wrap/1`.
""".
-type unwrapped_bag(Element) :: #{
    size := non_neg_integer(),
    root := xb5_bag_node:t(Element)
}.
-export_type([unwrapped_bag/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-doc """
Adds element `Element` to `Bag1`, returning a new bag `Bag2`.
If `Element` is already present, a duplicate is added.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> xb5_bag:to_list(xb5_bag:add(2, B)).
[1, 2, 2, 3]
> xb5_bag:size(xb5_bag:add(2, B)).
4
```
""".
-spec add(Element, Bag1) -> Bag2 when
    Bag1 :: bag(Element),
    Bag2 :: bag(Element).

add(Element, #xb5_bag{size = Size, root = Root} = Bag) ->
    UpdatedRoot = xb5_bag_node:add(Element, Root),
    Bag#xb5_bag{size = Size + 1, root = UpdatedRoot}.

%%

-doc """
Removes one occurrence of `Element` from `Bag1`, returning a new bag
`Bag2`.

Raises a `{badkey, Element}` error if `Element` is not present.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 2, 3]).
> xb5_bag:to_list(xb5_bag:delete(2, B)).
[1, 2, 3]
```
""".
-spec delete(Element, Bag1) -> Bag2 | no_return() when
    Element :: term(),
    Bag1 :: bag(Element),
    Bag2 :: bag(Element).

delete(Element, #xb5_bag{size = Size, root = Root} = Bag) ->
    case xb5_bag_node:delete_att(Element, Root) of
        badkey ->
            error_badkey(Element);
        %
        UpdatedRoot ->
            Bag#xb5_bag{size = Size - 1, root = UpdatedRoot}
    end.

%%

-doc """
Removes one occurrence of `Element` from `Bag1` if present, otherwise
returns `Bag1` unchanged.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 2, 3]).
> xb5_bag:to_list(xb5_bag:delete_any(2, B)).
[1, 2, 3]
> xb5_bag:to_list(xb5_bag:delete_any(42, B)).
[1, 2, 2, 3]
```
""".
-spec delete_any(Element, Bag1) -> Bag2 when
    Element :: term(),
    Bag1 :: bag(Element),
    Bag2 :: bag(Element).

delete_any(Element, #xb5_bag{size = Size, root = Root} = Bag) ->
    case xb5_bag_node:delete_att(Element, Root) of
        badkey ->
            Bag;
        %
        UpdatedRoot ->
            Bag#xb5_bag{size = Size - 1, root = UpdatedRoot}
    end.

%%

-doc """
Adds element `Element` to `Bag1` only if it is not already present.
If `Element` is already a member, `Bag1` is returned unchanged.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> xb5_bag:to_list(xb5_bag:enter(4, B)).
[1, 2, 3, 4]
> xb5_bag:to_list(xb5_bag:enter(2, B)).
[1, 2, 3]
```
""".
-spec enter(Element, Bag1) -> Bag2 when
    Element :: term(),
    Bag1 :: bag(Element),
    Bag2 :: bag(Element).

enter(Element, #xb5_bag{size = Size, root = Root} = Bag) ->
    case xb5_bag_node:insert_att(Element, Root) of
        key_exists ->
            Bag;
        %
        UpdatedRoot ->
            Bag#xb5_bag{size = Size + 1, root = UpdatedRoot}
    end.

%%

-doc """
Filters elements of `Bag1` using predicate function `Pred`, returning a
new bag `Bag2` containing only the elements for which `Pred` returns `true`.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3, 4, 5]).
> xb5_bag:to_list(xb5_bag:filter(fun(X) -> X > 3 end, B)).
[4, 5]
```
""".
-spec filter(Pred, Bag1) -> Bag2 when
    Pred :: fun((Element) -> boolean()),
    Bag1 :: bag(Element),
    Bag2 :: bag(Element).

filter(Fun, #xb5_bag{root = Root} = Bag) ->
    [FilteredSize | FilteredRoot] = xb5_bag_node:filter(Fun, Root),
    Bag#xb5_bag{size = FilteredSize, root = FilteredRoot}.

%%

-doc """
Filters and maps elements of `Bag1` using `Fun`, returning a new bag
`Bag2`.

For each element, `Fun` must return either `true` (keep the element),
`false` (discard it), or `{true, NewElement}` (replace it with `NewElement`).

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3, 4, 5]).
> xb5_bag:to_list(xb5_bag:filtermap(fun(X) when X rem 2 =:= 0 -> {true, X * 10}; (_) -> false end, B)).
[20, 40]
```
""".
-spec filtermap(Fun, Bag1) -> Bag2 when
    Fun :: fun((Element1) -> boolean() | {true, Element2}),
    Bag1 :: bag(Element1),
    Bag2 :: bag(Element1 | Element2).

filtermap(Fun, #xb5_bag{root = Root} = Bag) ->
    [FilteredSize | FilteredRoot] = xb5_bag_node:filtermap(Fun, Root),
    Bag#xb5_bag{size = FilteredSize, root = FilteredRoot}.

%%

-doc """
Folds `Function` over every element in `Bag`, returning the final
accumulator value. Elements are visited in Erlang term order.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> xb5_bag:fold(fun(X, Acc) -> X + Acc end, 0, B).
6
```
""".
-spec fold(Function, Acc0, Bag) -> Acc1 when
    Function :: fun((Element, AccIn) -> AccOut),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc,
    Bag :: bag(Element).

fold(Fun, Acc, #xb5_bag{root = Root}) ->
    xb5_bag_node:fold(Fun, Acc, Root).

%%

-doc """
Returns a bag of the elements in `List`. Unlike `xb5_sets:from_list/1`,
duplicate elements are preserved.

## Examples

```erlang
> xb5_bag:to_list(xb5_bag:from_list([3, 1, 2, 1])).
[1, 1, 2, 3]
> xb5_bag:to_list(xb5_bag:from_list([])).
[]
```
""".
-spec from_list(List) -> Bag when
    List :: [Element],
    Bag :: bag(Element).

from_list(List) ->
    Root = xb5_bag_node:new(),
    Size = 0,
    from_list_recur(List, Size, Root).

%%

-doc """
Returns a bag built from the ordered set `Ordset`.

## Examples

```erlang
> xb5_bag:to_list(xb5_bag:from_ordset([1, 2, 3])).
[1, 2, 3]
```
""".
-spec from_ordset(List) -> Bag when
    List :: ordsets:ordset(Element),
    Bag :: bag(Element).

from_ordset(Ordset) ->
    Root = xb5_bag_node:new(),
    Size = 0,
    from_ordset_recur(Ordset, Root, Size).

%%

-doc """
Inserts element `Element` into `Bag1`, returning a new bag `Bag2`.

Raises a `{key_exists, Element}` error if `Element` is already present.

## Examples

```erlang
> B0 = xb5_bag:new().
> B1 = xb5_bag:insert(1, B0).
> xb5_bag:to_list(B1).
[1]
```
""".
-spec insert(Element, Bag1) -> Bag2 when
    Bag1 :: bag(Element),
    Bag2 :: bag(Element).

insert(Element, #xb5_bag{size = Size, root = Root} = Bag) ->
    case xb5_bag_node:insert_att(Element, Root) of
        key_exists ->
            error_key_exists(Element);
        %
        UpdatedRoot ->
            Bag#xb5_bag{size = Size + 1, root = UpdatedRoot}
    end.

%%

-doc """
Returns `true` if `Bag` is empty, otherwise `false`.

## Examples

```erlang
> xb5_bag:is_empty(xb5_bag:new()).
true
> xb5_bag:is_empty(xb5_bag:from_list([1])).
false
```
""".
-spec is_empty(Bag) -> boolean() when
    Bag :: bag().

is_empty(#xb5_bag{size = Size}) ->
    Size =:= 0.

%%

-doc """
Returns `true` if `Element` is a member of `Bag`, otherwise `false`.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> xb5_bag:is_member(2, B).
true
> xb5_bag:is_member(4, B).
false
```
""".
-spec is_member(Element, Bag) -> boolean() when
    Bag :: bag(Element).

is_member(Element, #xb5_bag{root = Root}) ->
    xb5_bag_node:is_member(Element, Root).

%%

-doc """
Returns an iterator that can be used for traversing the elements of
`Bag`; see `next/1`. Equivalent to `iterator(Bag, ordered)`.

## Examples

```erlang
> B = xb5_bag:from_list([3, 1, 2]).
> I = xb5_bag:iterator(B).
> {1, I2} = xb5_bag:next(I).
> {2, I3} = xb5_bag:next(I2).
> {3, I4} = xb5_bag:next(I3).
> xb5_bag:next(I4).
none
```
""".
-spec iterator(Bag) -> Iter when
    Bag :: bag(Element),
    Iter :: iter(Element).

iterator(Bag) ->
    iterator(Bag, ordered).

%%

-doc """
Returns an iterator that can be used for traversing the elements of
`Bag` in the given `Order`; see `next/1`.

`Order` must be `ordered` (ascending) or `reversed` (descending).

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> I = xb5_bag:iterator(B, reversed).
> {3, I2} = xb5_bag:next(I).
> {2, I3} = xb5_bag:next(I2).
> {1, I4} = xb5_bag:next(I3).
> xb5_bag:next(I4).
none
```
""".
-spec iterator(Bag, Order) -> Iter when
    Bag :: bag(Element),
    Iter :: iter(Element),
    Order :: ordered | reversed.

iterator(#xb5_bag{root = Root}, Order) ->
    xb5_bag_node:iterator(Root, Order).

%%

-doc """
Returns an iterator that can be used for traversing the elements of
`Bag` starting from element `Element`; see `next/1`. Equivalent to
`iterator_from(Element, Bag, ordered)`.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3, 4, 5]).
> I = xb5_bag:iterator_from(3, B).
> {3, I2} = xb5_bag:next(I).
> {4, I3} = xb5_bag:next(I2).
> {5, I4} = xb5_bag:next(I3).
> xb5_bag:next(I4).
none
```
""".
-spec iterator_from(Element, Bag) -> Iter when
    Bag :: bag(Element),
    Iter :: iter(Element).

iterator_from(Element, Bag) ->
    iterator_from(Element, Bag, ordered).

%%

-doc """
Returns an iterator that can be used for traversing the elements of
`Bag` starting from element `Element` in the given `Order`; see `next/1`.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3, 4, 5]).
> I = xb5_bag:iterator_from(3, B, reversed).
> {3, I2} = xb5_bag:next(I).
> {2, I3} = xb5_bag:next(I2).
> {1, I4} = xb5_bag:next(I3).
> xb5_bag:next(I4).
none
```
""".
-spec iterator_from(Element, Bag, Order) -> Iter when
    Bag :: bag(Element),
    Iter :: iter(Element),
    Order :: ordered | reversed.

iterator_from(Element, #xb5_bag{root = Root}, Order) ->
    xb5_bag_node:iterator_from(Element, Root, Order).

%%

-doc """
Returns `{found, Element2}` where `Element2` is the least element
strictly greater than `Element1` in `Bag`, or `none` if no such
element exists.

## Examples

```erlang
> B = xb5_bag:from_list([1, 3, 5]).
> xb5_bag:larger(2, B).
{found, 3}
> xb5_bag:larger(5, B).
none
```
""".
-spec larger(Element1, Bag) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Bag :: bag(Element).

larger(Element, #xb5_bag{root = Root}) ->
    xb5_bag_node:larger(Element, Root).

%%

-doc """
Returns the largest element in `Bag`.

Raises an `empty_bag` error if the bag is empty.

## Examples

```erlang
> xb5_bag:largest(xb5_bag:from_list([3, 1, 2])).
3
```
""".
-spec largest(Bag) -> Element when
    Bag :: bag(Element).

largest(#xb5_bag{size = Size, root = Root}) when Size =/= 0 ->
    xb5_bag_node:largest(Root);
largest(#xb5_bag{}) ->
    error_empty_bag().

%%

-doc """
Maps `Fun` over all elements of `Bag1`, returning a new bag `Bag2`.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> xb5_bag:to_list(xb5_bag:map(fun(X) -> X * 10 end, B)).
[10, 20, 30]
```
""".
-spec map(Fun, Bag1) -> Bag2 when
    Fun :: fun((Element1) -> Element2),
    Bag1 :: bag(Element1),
    Bag2 :: bag(Element2).

map(Fun, #xb5_bag{root = Root} = Bag) ->
    MappedRoot = xb5_bag_node:map(Fun, Root),
    Bag#xb5_bag{root = MappedRoot}.

%%

-doc """
Merges two bags into one. All elements from both bags are kept, so
counts are combined.

## Examples

```erlang
> B1 = xb5_bag:from_list([1, 2, 3]).
> B2 = xb5_bag:from_list([2, 3, 4]).
> xb5_bag:to_list(xb5_bag:merge(B1, B2)).
[1, 2, 2, 3, 3, 4]
```
""".
-spec merge(Bag1, Bag2) -> Bag3 when
    Bag1 :: bag(Element),
    Bag2 :: bag(Element),
    Bag3 :: bag(Element).

merge(
    #xb5_bag{size = Size1, root = Root1} = Bag1,
    #xb5_bag{size = Size2, root = Root2}
) ->
    MergedRoot = xb5_bag_node:merge(Size1, Root1, Size2, Root2),
    Bag1#xb5_bag{size = Size1 + Size2, root = MergedRoot}.

%%

-doc """
Returns a new empty bag.

## Examples

```erlang
> xb5_bag:is_empty(xb5_bag:new()).
true
> xb5_bag:size(xb5_bag:new()).
0
```
""".
-spec new() -> Bag when
    Bag :: bag(_).

new() ->
    #xb5_bag{size = 0, root = xb5_bag_node:new()}.

%%

-doc """
Returns `{Element, Iter2}` where `Element` is the next element referred
to by iterator `Iter1` and `Iter2` is the updated iterator, or `none`
if no more elements remain.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2]).
> I = xb5_bag:iterator(B).
> {1, I2} = xb5_bag:next(I).
> {2, I3} = xb5_bag:next(I2).
> xb5_bag:next(I3).
none
```
""".
-spec next(Iter1) -> {Element, Iter2} | none when
    Iter1 :: iter(Element),
    Iter2 :: iter(Element).

next(Iter) ->
    xb5_bag_node:next(Iter).

%%

-doc """
Returns the element at 1-based rank `Rank` in `Bag` in O(log n)
time.

Raises a `{badarg, Rank}` error if `Rank` is not a valid position
(i.e. less than 1 or greater than the bag size).

## Examples

```erlang
> B = xb5_bag:from_list([10, 20, 30]).
> xb5_bag:nth(1, B).
10
> xb5_bag:nth(3, B).
30
```
""".
-spec nth(Rank, Bag) -> Element when Rank :: pos_integer(), Bag :: bag(Element).

nth(Rank, #xb5_bag{size = Size, root = Root}) when is_integer(Rank), Rank >= 1, Rank =< Size ->
    xb5_bag_node:nth(Rank, Root);
nth(Rank, #xb5_bag{}) ->
    error({badarg, Rank}).

%%

-doc """
Calculates a percentile value in O(log n) time, using linear
interpolation of the `inclusive` percentile bracket. Returns
`{value, Result}` or `none`. Equivalent to
`percentile(Percentile, Bag, [])`.

Raises a `{bracket_value_not_a_number, Bound}` error if linear
interpolation is required but the bracketing elements are not numbers.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3, 4]).
> xb5_bag:percentile(0, B).
{value, 1}
> xb5_bag:percentile(0.5, B).
{value, 2.5}
> xb5_bag:percentile(1, B).
{value, 4}
```
""".
-spec percentile(Percentile, Bag) -> {value, Element | InterpolationResult} | none when
    Percentile :: float() | 0 | 1,
    Bag :: bag(Element),
    InterpolationResult :: number().

percentile(Percentile, Bag) ->
    percentile(Percentile, Bag, []).

%%

-doc """
Like `percentile/2`, but accepts a list of options. Runs in O(log n)
time. The only supported option is `{method, Method}`; see
`t:percentile_bracket_method/0`.

Raises a `{bracket_value_not_a_number, Bound}` error if linear
interpolation is required but the bracketing elements are not numbers.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3, 4]).
> xb5_bag:percentile(0.5, B, [{method, nearest_rank}]).
{value, 2}
```
""".
-spec percentile(Percentile, Bag, Opts) -> {value, Element | InterpolationResult} | none when
    Percentile :: float() | 0 | 1,
    Bag :: bag(Element),
    InterpolationResult :: number(),
    Opts :: [percentile_bracket_opt()].

percentile(Percentile, Bag, Opts) ->
    Bracket = percentile_bracket(Percentile, Bag, Opts),
    linear_interpolated_percentile(Bracket).

%%

-doc """
Returns the percentile bracket for `Percentile` in `Bag` in O(log n)
time, using the `inclusive` method. Returns `{exact, Element}` when the
percentile falls exactly on an element, `{between, Low, High}` when it
falls between two elements, or `none` if the bag is empty.

Raises a `{badarg, Percentile}` error if `Percentile` is not a number
in `[0.0, 1.0]`. See `percentile_bracket/3` for other methods.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3, 4]).
> xb5_bag:percentile_bracket(0, B).
{exact, 1}
> xb5_bag:percentile_bracket(1, B).
{exact, 4}
```
""".
-spec percentile_bracket(Percentile, Bag) -> Bracket when
    Percentile :: float() | 0 | 1,
    Bag :: bag(Element),
    Bracket :: percentile_bracket(Element).

percentile_bracket(Percentile, Bag) ->
    percentile_bracket(Percentile, Bag, []).

%%

-doc """
Like `percentile_bracket/2`, but accepts a list of options. Runs in
O(log n) time. The only supported option is `{method, Method}`; see
`t:percentile_bracket_method/0`.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3, 4]).
> xb5_bag:percentile_bracket(0.5, B, [{method, nearest_rank}]).
{exact, 2}
```
""".
-spec percentile_bracket(Percentile, Bag, Opts) -> Bracket when
    Percentile :: float() | 0 | 1,
    Bag :: bag(Element),
    Opts :: [percentile_bracket_opt()],
    Bracket :: percentile_bracket(Element).

percentile_bracket(Percentile, #xb5_bag{size = Size, root = Root}, Opts) when
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
percentile_bracket(Percentile, #xb5_bag{}, _Opts) ->
    error({badarg, Percentile}).

%%

-doc """
Returns the [percentile rank](https://en.wikipedia.org/wiki/Percentile_rank)
of `Element` in `Bag` as a float in `[0.0, 1.0]`, in O(log n) time.

`Element` does not have to be in the bag.

Raises an `empty_bag` error if the bag is empty.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3, 4]).
> xb5_bag:percentile_rank(2, B).
0.375
```
""".
-spec percentile_rank(Element, Bag) -> Rank when
    Bag :: bag(Element),
    Rank :: float().

percentile_rank(Elem, #xb5_bag{size = Size, root = Root}) when Size > 0 ->
    % As described in Wikipedia:
    % https://en.wikipedia.org/wiki/Percentile_rank
    Smaller = xb5_bag_node:rank_smaller(Elem, Root),
    Larger = xb5_bag_node:rank_larger(Elem, Root),

    [CF | F] = percentile_rank_params(Smaller, Larger, Size),

    (CF - 0.5 * F) / Size;
percentile_rank(_, #xb5_bag{}) ->
    error_empty_bag().

%%

-doc """
Returns `{rank, Rank}` where `Rank` is the 1-based position of `Element`
in `Bag`, or `none` if `Element` is not present. When duplicates exist,
the lowest rank is returned. Runs in O(log n) time.

## Examples

```erlang
> B = xb5_bag:from_list([10, 20, 30]).
> xb5_bag:rank(20, B).
{rank, 2}
> xb5_bag:rank(42, B).
none
```

With duplicates, the lowest rank is returned:

```erlang
> B = xb5_bag:from_list([f, g, g, h, i]).
> xb5_bag:rank(g, B).
{rank, 2}
```
""".
-spec rank(Element, Bag) -> {rank, Rank} | none when
    Bag :: bag(Element),
    Rank :: pos_integer().

rank(Elem, #xb5_bag{root = Root}) ->
    case xb5_bag_node:rank(Elem, Root) of
        none ->
            none;
        %
        Rank ->
            {rank, Rank}
    end.

%%

-doc """
Returns `{Rank, Element2}` where `Element2` is the least element
strictly greater than `Element1` and `Rank` is its 1-based position, or
`none` if no such element exists. When `Element2` has duplicates, the
lowest rank (first occurrence) is returned. Runs in O(log n) time.

## Examples

```erlang
> B = xb5_bag:from_list([10, 20, 30]).
> xb5_bag:rank_larger(15, B).
{2, 20}
> xb5_bag:rank_larger(30, B).
none
```

With duplicates, the lowest rank (first occurrence) is returned:

```erlang
> B = xb5_bag:from_list([f, g, g, h, i]).
> xb5_bag:rank_larger(f, B).
{2, g}
```
""".
-spec rank_larger(Element1, Bag) -> {Rank, Element2} | none when
    Element1 :: Element,
    Element2 :: Element,
    Bag :: bag(Element),
    Rank :: pos_integer().

rank_larger(Elem, #xb5_bag{root = Root}) ->
    case xb5_bag_node:rank_larger(Elem, Root) of
        [Rank | Larger] ->
            {Rank, Larger};
        %
        none ->
            none
    end.

%%

-doc """
Returns `{Rank, Element2}` where `Element2` is the greatest element
strictly less than `Element1` and `Rank` is its 1-based position, or
`none` if no such element exists. When `Element2` has duplicates, the
highest rank (last occurrence) is returned. Runs in O(log n) time.

## Examples

```erlang
> B = xb5_bag:from_list([10, 20, 30]).
> xb5_bag:rank_smaller(25, B).
{2, 20}
> xb5_bag:rank_smaller(10, B).
none
```

With duplicates, the highest rank (last occurrence) is returned:

```erlang
> B = xb5_bag:from_list([f, g, g, g, h]).
> xb5_bag:rank_smaller(h, B).
{4, g}
```
""".
-spec rank_smaller(Element1, Bag) -> {Rank, Element2} | none when
    Element1 :: Element,
    Element2 :: Element,
    Bag :: bag(Element),
    Rank :: pos_integer().

rank_smaller(Elem, #xb5_bag{root = Root}) ->
    case xb5_bag_node:rank_smaller(Elem, Root) of
        [Rank | Smaller] ->
            {Rank, Smaller};
        %
        none ->
            none
    end.

%%

-doc """
Returns the number of elements in `Bag`, including duplicates.

## Examples

```erlang
> xb5_bag:size(xb5_bag:new()).
0
> xb5_bag:size(xb5_bag:from_list([1, 2, 2, 3])).
4
```
""".
-spec size(Bag) -> non_neg_integer() when
    Bag :: bag().

size(#xb5_bag{size = Size}) ->
    Size.

%%

-doc """
Returns `{found, Element2}` where `Element2` is the greatest element
in `Bag` that is strictly less than `Element1`, or `none` if no such
element exists.

## Examples

```erlang
> B = xb5_bag:from_list([1, 3, 5]).
> xb5_bag:smaller(4, B).
{found, 3}
> xb5_bag:smaller(1, B).
none
```
""".
-spec smaller(Element1, Bag) -> none | {found, Element2} when
    Element1 :: Element,
    Element2 :: Element,
    Bag :: bag(Element).

smaller(Element, #xb5_bag{root = Root}) ->
    xb5_bag_node:smaller(Element, Root).

%%

-doc """
Returns the smallest element in `Bag`.

Raises an `empty_bag` error if the bag is empty.

## Examples

```erlang
> xb5_bag:smallest(xb5_bag:from_list([3, 1, 2])).
1
```
""".
-spec smallest(Bag) -> Element when
    Bag :: bag(Element).

smallest(#xb5_bag{size = Size, root = Root}) when Size =/= 0 ->
    xb5_bag_node:smallest(Root);
smallest(#xb5_bag{}) ->
    error_empty_bag().

%%

-doc """
Returns structural statistics about the B-tree backing `Bag`.

This is primarily intended for debugging and testing. The result
is a proplist with keys such as `height`, `total_keys`,
`node_counts`, `node_percentages`, and others.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> Stats = xb5_bag:structural_stats(B).
> {height, 1} = lists:keyfind(height, 1, Stats).
> {total_keys, 3} = lists:keyfind(total_keys, 1, Stats).
```
""".
-spec structural_stats(Bag) -> Stats when
    Bag :: bag(),
    Stats :: xb5_structural_stats:t().

structural_stats(#xb5_bag{root = Root}) ->
    xb5_bag_node:structural_stats(Root).

%%

-doc """
Returns `{Element, Bag2}`, where `Element` is the largest element in
`Bag1` and `Bag2` is the remaining bag.

Raises an `empty_bag` error if the bag is empty.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> {3, B2} = xb5_bag:take_largest(B).
> xb5_bag:to_list(B2).
[1, 2]
```
""".
-spec take_largest(Bag1) -> {Element, Bag2} when
    Bag1 :: bag(Element),
    Bag2 :: bag(Element).

take_largest(#xb5_bag{root = Root, size = Size} = Bag) when Size =/= 0 ->
    [Largest | UpdatedRoot] = xb5_bag_node:take_largest(Root),
    {Largest, Bag#xb5_bag{root = UpdatedRoot, size = Size - 1}};
take_largest(#xb5_bag{}) ->
    error_empty_bag().

%%

-doc """
Returns `{Element, Bag2}`, where `Element` is the smallest element in
`Bag1` and `Bag2` is the remaining bag.

Raises an `empty_bag` error if the bag is empty.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> {1, B2} = xb5_bag:take_smallest(B).
> xb5_bag:to_list(B2).
[2, 3]
```
""".
-spec take_smallest(Bag1) -> {Element, Bag2} when
    Bag1 :: bag(Element),
    Bag2 :: bag(Element).

take_smallest(#xb5_bag{root = Root, size = Size} = Bag) when Size =/= 0 ->
    [Smallest | UpdatedRoot] = xb5_bag_node:take_smallest(Root),
    {Smallest, Bag#xb5_bag{root = UpdatedRoot, size = Size - 1}};
take_smallest(#xb5_bag{}) ->
    error_empty_bag().

%%

-doc """
Returns the elements of `Bag` as an ordered list, including duplicates.

## Examples

```erlang
> xb5_bag:to_list(xb5_bag:from_list([3, 1, 2, 1])).
[1, 1, 2, 3]
> xb5_bag:to_list(xb5_bag:new()).
[]
```
""".
-spec to_list(Bag) -> List when
    Bag :: bag(Element),
    List :: [Element].

to_list(#xb5_bag{root = Root}) ->
    xb5_bag_node:to_list(Root).

%%

-doc """
Unwraps an opaque bag into a plain map representation suitable for
cross-language interop (for example, converting to an Elixir struct
that uses the same underlying node module). Returns `{ok, Unwrapped}`
on success or `{error, Reason}` if `Term` is not a valid bag.

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> {ok, Unwrapped} = xb5_bag:unwrap(B).
> maps:get(size, Unwrapped).
3
> {error, _} = xb5_bag:unwrap(not_a_bag).
```
""".
-spec unwrap(Term) -> {ok, Unwrapped} | {error, Reason} when
    Term :: bag(Element) | term(),
    Unwrapped :: unwrapped_bag(Element),
    Reason :: term().

unwrap(#xb5_bag{size = Size, root = Root} = Term) when is_integer(Size), Size >= 0 ->
    try xb5_bag_node:structural_stats(Root) of
        Stats ->
            case lists:keyfind(total_keys, 1, Stats) of
                {_, TotalKeys} when TotalKeys =:= Size ->
                    {ok, #{size => Size, root => Root}};
                %
                {_, _} ->
                    {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_bag_collection, Term})}
            end
    catch
        Class:Reason when Class =/= error; Reason =/= undef ->
            {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_bag_collection, Term})}
    end;
unwrap(Term) ->
    {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_bag_collection, Term})}.

%%

-doc """
Wraps a plain map representation back into an opaque bag.

This is the inverse of `unwrap/1` and is intended for cross-language
interop (for example, converting from an Elixir struct that shares the
same underlying node module).

## Examples

```erlang
> B = xb5_bag:from_list([1, 2, 3]).
> {ok, U} = xb5_bag:unwrap(B).
> B2 = xb5_bag:wrap(U).
> xb5_bag:to_list(B2).
[1, 2, 3]
```
""".
-spec wrap(unwrapped_bag(Element)) -> bag(Element).

wrap(#{root := Root, size := Size}) when is_integer(Size), Size >= 0 ->
    #xb5_bag{root = Root, size = Size}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-compile({inline, error_badkey/1}).
-spec error_badkey(_) -> no_return().
error_badkey(Elem) ->
    error({badkey, Elem}).

-compile({inline, error_empty_bag/0}).
-spec error_empty_bag() -> no_return().
error_empty_bag() ->
    error(empty_bag).

-compile({inline, error_key_exists/1}).
-spec error_key_exists(_) -> no_return().
error_key_exists(Elem) ->
    error({key_exists, Elem}).

%%

from_list_recur([Element | Next], Size, Root) ->
    UpdatedRoot = xb5_bag_node:add(Element, Root),
    from_list_recur(Next, Size + 1, UpdatedRoot);
from_list_recur([], Size, Root) ->
    #xb5_bag{size = Size, root = Root}.

%%

from_ordset_recur([Element | Next], Root, Size) ->
    UpdatedRoot = xb5_bag_node:append(Element, Root),
    from_ordset_recur(Next, UpdatedRoot, Size + 1);
from_ordset_recur([], Root, Size) ->
    #xb5_bag{size = Size, root = Root}.

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
            ExactElem = xb5_bag_node:nth(ExactRank, Root),
            {exact, ExactElem};
        %
        true ->
            [LowElem | HighElem] = xb5_bag_node:nth_and_nthp1(LowRank, Root),

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
