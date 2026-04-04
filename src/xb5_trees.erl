-module(xb5_trees).

-ifdef(E48).
-moduledoc """
An ordered key-value store (dictionary) using a
[B-tree](https://en.wikipedia.org/wiki/B-tree) of order 5.

Keys are ordered using the Erlang term order, comparing with `==`
rather than `=:=`. This means that `1` and `1.0` are considered the same
key.

The tree is always balanced after every insertion and deletion.

API is the same as `m:gb_trees`.

See also:
- `m:xb5_sets` for the unique-element counterpart, supporting set operations
(union, intersection, difference)
- `m:xb5_bag` for a multiset supporting [order
statistic](https://en.wikipedia.org/wiki/Order_statistic_tree) operations (get nth,
rank, percentiles).
""".
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    balance/1,
    delete/2,
    delete_any/2,
    empty/0,
    enter/3,
    foldl/3,
    foldr/3,
    from_list/1,
    from_orddict/1,
    get/2,
    insert/3,
    insert_with/3,
    intersect/2,
    intersect_with/3,
    is_defined/2,
    is_empty/1,
    is_equal/2,
    iterator/1,
    iterator/2,
    iterator_from/2,
    iterator_from/3,
    keys/1,
    larger/2,
    largest/1,
    lookup/2,
    map/2,
    merge/2,
    merge_with/3,
    new/0,
    next/1,
    size/1,
    smaller/2,
    smallest/1,
    structural_stats/1,
    take/2,
    take_any/2,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    unwrap/1,
    update/3,
    update_with/3,
    update_with/4,
    values/1,
    wrap/1
]).

-ignore_xref([
    balance/1,
    delete/2,
    delete_any/2,
    empty/0,
    enter/3,
    foldl/3,
    foldr/3,
    from_list/1,
    from_orddict/1,
    get/2,
    insert/3,
    insert_with/3,
    intersect/2,
    intersect_with/3,
    is_defined/2,
    is_empty/1,
    is_equal/2,
    iterator/1,
    iterator/2,
    iterator_from/2,
    iterator_from/3,
    keys/1,
    larger/2,
    largest/1,
    lookup/2,
    map/2,
    merge/2,
    merge_with/3,
    new/0,
    next/1,
    size/1,
    smaller/2,
    smallest/1,
    structural_stats/1,
    take/2,
    take_any/2,
    take_largest/1,
    take_smallest/1,
    to_list/1,
    unwrap/1,
    update/3,
    update_with/3,
    update_with/4,
    values/1,
    wrap/1
]).

%% ------------------------------------------------------------------
%% Static Check Tweaks
%% ------------------------------------------------------------------

-hank([
    {unnecessary_function_arguments, [
        {get_found, 2, 1},
        {is_defined_found, 2, 1},
        {is_defined_found, 2, 2},
        {is_defined_not_found, 1, 1},
        {lookup_found, 2, 1},
        {lookup_not_found, 1, 1}
    ]}
]).

%% ------------------------------------------------------------------
%% API Type Definitions
%% ------------------------------------------------------------------

-record(xb5_tree, {size, root}).

-ifdef(E48).
-doc "An ordered key-value tree containing entries of type `{Key, Value}`.".
-endif.
-opaque tree(Key, Value) :: #xb5_tree{
    size :: non_neg_integer(),
    root :: xb5_trees_node:t(Key, Value)
}.
-export_type([tree/2]).

-ifdef(E48).
-doc "Shorthand for `tree(_, _)`.".
-endif.
-type tree() :: tree(_, _).
-export_type([tree/0]).

-ifdef(E48).
-doc "An iterator over entries of type `{Key, Value}`. See `iterator/1` and `next/1`.".
-endif.
-opaque iter(Key, Value) :: xb5_trees_node:iter(Key, Value).
-export_type([iter/2]).

-ifdef(E48).
-doc "Shorthand for `iter(_, _)`.".
-endif.
-type iter() :: iter(_, _).
-export_type([iter/0]).

%%

-ifdef(E48).
-doc """
A plain-map representation of a tree, suitable for cross-language
serialization (for example, converting to or from an Elixir struct that
uses the same underlying node structures).

See `unwrap/1` and `wrap/1`.
""".
-endif.
-type unwrapped_tree(Key, Value) :: #{
    size := non_neg_integer(),
    root := xb5_trees_node:t(Key, Value)
}.
-export_type([unwrapped_tree/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-ifdef(E48).
-doc """
Returns `Tree` unchanged.

This function exists only to ease migration from `m:gb_trees`. Since xb5
B-trees are always balanced, calling this function is never necessary.

## Examples

```erlang
> T = xb5_trees:from_list([{3, c}, {1, a}, {2, b}]).
> xb5_trees:to_list(xb5_trees:balance(T)).
[{1, a}, {2, b}, {3, c}]
```
""".
-endif.
-spec balance(Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

balance(#xb5_tree{} = Tree) ->
    % There's no need to balance, this function is only here to ease migration
    % from `gb_trees'.
    Tree.

%%

-ifdef(E48).
-doc """
Removes key `Key` from `Tree1`, returning a new tree `Tree2`.

Raises a `{badkey, Key}` error if the key is not present.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> xb5_trees:to_list(xb5_trees:delete(2, T)).
[{1, a}, {3, c}]
```
""".
-endif.
-spec delete(Key, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

delete(Key, #xb5_tree{root = Root, size = Size} = Tree) ->
    case xb5_trees_node:delete_att(Key, Root) of
        badkey ->
            error_badkey(Key);
        %
        UpdatedRoot ->
            Tree#xb5_tree{
                root = UpdatedRoot,
                size = Size - 1
            }
    end.

%%

-ifdef(E48).
-doc """
Removes key `Key` from `Tree1` if present, returning a new tree `Tree2`.
If `Key` is not present, `Tree1` is returned unchanged.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> xb5_trees:to_list(xb5_trees:delete_any(2, T)).
[{1, a}, {3, c}]
> xb5_trees:to_list(xb5_trees:delete_any(42, T)).
[{1, a}, {2, b}, {3, c}]
```
""".
-endif.
-spec delete_any(Key, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

delete_any(Key, #xb5_tree{root = Root, size = Size} = Tree) ->
    case xb5_trees_node:delete_att(Key, Root) of
        badkey ->
            Tree;
        %
        UpdatedRoot ->
            Tree#xb5_tree{
                root = UpdatedRoot,
                size = Size - 1
            }
    end.

%%

-ifdef(E48).
-doc #{equiv => new / 0}.
-endif.
-spec empty() -> tree().

empty() -> new().

%%

-ifdef(E48).
-doc """
Inserts `Key` with value `Value` into `Tree1` if the key is not present,
otherwise updates `Key` to value `Value` in `Tree1`.

## Examples

```erlang
> T0 = xb5_trees:new().
> T1 = xb5_trees:enter(1, a, T0).
> xb5_trees:to_list(T1).
[{1, a}]
> T2 = xb5_trees:enter(1, z, T1).
> xb5_trees:to_list(T2).
[{1, z}]
> T3 = xb5_trees:enter(2, b, T2).
> xb5_trees:to_list(T3).
[{1, z}, {2, b}]
```
""".
-endif.
-spec enter(Key, Value, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

enter(Key, Value, #xb5_tree{size = Size, root = Root} = Tree) ->
    case xb5_trees_node:insert_att(Key, eager, Value, Root) of
        key_exists ->
            UpdatedRoot = xb5_trees_node:update_att(Key, eager, Value, Root),
            Tree#xb5_tree{root = UpdatedRoot};
        %
        UpdatedRoot ->
            Tree#xb5_tree{
                root = UpdatedRoot,
                size = Size + 1
            }
    end.

%%

-ifdef(E48).
-doc """
Folds `Function` over all entries of `Tree` in ascending key order,
calling `Function(Key, Value, AccIn)` for each entry. Returns the final
accumulator value.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> xb5_trees:foldl(fun(K, V, Acc) -> [{K, V} | Acc] end, [], T).
[{3, c}, {2, b}, {1, a}]
```
""".
-endif.
-spec foldl(Function, Acc0, Tree) -> Acc1 when
    Function :: fun((Key, Value, AccIn) -> AccOut),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc,
    Tree :: tree(Key, Value).

foldl(Fun, Acc0, #xb5_tree{root = Root}) ->
    xb5_trees_node:foldl(Fun, Acc0, Root).

%%

-ifdef(E48).
-doc """
Folds `Function` over all entries of `Tree` in descending key order,
calling `Function(Key, Value, AccIn)` for each entry. Returns the final
accumulator value.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> xb5_trees:foldr(fun(K, V, Acc) -> [{K, V} | Acc] end, [], T).
[{1, a}, {2, b}, {3, c}]
```
""".
-endif.
-spec foldr(Function, Acc0, Tree) -> Acc1 when
    Function :: fun((Key, Value, AccIn) -> AccOut),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc,
    Tree :: tree(Key, Value).

foldr(Fun, Acc0, #xb5_tree{root = Root}) ->
    xb5_trees_node:foldr(Fun, Acc0, Root).

%%

-ifdef(E48).
-doc """
Returns a tree of the key-value pairs in `List`. Duplicate keys are
resolved by keeping the last occurrence.

It sorts the `List` by key and then delegates to `from_orddict/1` - see that
function for performance characteristics.

## Examples

```erlang
> xb5_trees:to_list(xb5_trees:from_list([{3, c}, {1, a}, {2, b}])).
[{1, a}, {2, b}, {3, c}]
> xb5_trees:to_list(xb5_trees:from_list([{1, a}, {1, z}])).
[{1, z}]
> xb5_trees:to_list(xb5_trees:from_list([])).
[]
```
""".
-endif.
-spec from_list(List) -> Tree when
    List :: [{Key, Value}],
    Tree :: tree(Key, Value).

from_list(List) ->
    from_orddict(orddict:from_list(List)).

%%

-ifdef(E48).
-doc """
Returns a tree built from the ordered dictionary `Orddict`.

The tree is built by recursively splitting the list top-down rather than by
sequential insertion, yielding an optimally balanced result without intermediate
allocations or element comparisons. This is analogous to `gb_trees:from_orddict/1`.

## Examples

```erlang
> xb5_trees:to_list(xb5_trees:from_orddict([{1, a}, {2, b}])).
[{1, a}, {2, b}]
```
""".
-endif.
-spec from_orddict(Orddict) -> Tree when
    Orddict :: orddict:orddict(Key, Value),
    Tree :: tree(Key, Value).

from_orddict(Orddict) ->
    S = length(Orddict),
    Root = xb5_trees_node:from_orddict(S, Orddict),
    #xb5_tree{size = S, root = Root}.

%%

-ifdef(E48).
-doc """
Retrieves the value stored with `Key` in `Tree`.

Raises a `{badkey, Key}` error if the key is not present.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> xb5_trees:get(2, T).
b
```
""".
-endif.
-spec get(Key, Tree) -> Value when
    Tree :: tree(Key, Value).

get(Key, #xb5_tree{root = Root}) ->
    xb5_trees_node:get_att(
        Key,
        Root,
        fun get_found/2,
        fun get_not_found/1
    ).

%%

-ifdef(E48).
-doc """
Inserts `Key` with value `Value` into `Tree1`, returning a new tree
`Tree2`.

Raises a `{key_exists, Key}` error if the key is already present.

## Examples

```erlang
> T0 = xb5_trees:new().
> T1 = xb5_trees:insert(1, a, T0).
> xb5_trees:to_list(T1).
[{1, a}]
> T2 = xb5_trees:insert(2, b, T1).
> xb5_trees:to_list(T2).
[{1, a}, {2, b}]
```
""".
-endif.
-spec insert(Key, Value, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

insert(Key, Value, #xb5_tree{size = Size, root = Root} = Tree) ->
    case xb5_trees_node:insert_att(Key, eager, Value, Root) of
        key_exists ->
            error_key_exists(Key);
        %
        UpdatedRoot ->
            Tree#xb5_tree{
                size = Size + 1,
                root = UpdatedRoot
            }
    end.

%%

-ifdef(E48).
-doc """
Like `insert/3`, but takes a zero-arity fun that is only evaluated when
the key is not yet present. Returns a new tree `Tree2` with the key
inserted.

Raises a `{key_exists, Key}` error if the key already exists.

## Examples

```erlang
> T0 = xb5_trees:new().
> T1 = xb5_trees:insert_with(1, fun() -> a end, T0).
> xb5_trees:to_list(T1).
[{1, a}]
```
""".
-endif.
-spec insert_with(Key, Fun, Tree1) -> Tree2 when
    Fun :: fun(() -> Value),
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

insert_with(Key, Fun, #xb5_tree{size = Size, root = Root} = Tree) ->
    case xb5_trees_node:insert_att(Key, lazy, Fun, Root) of
        key_exists ->
            error_key_exists(Key);
        %
        UpdatedRoot ->
            Tree#xb5_tree{
                size = Size + 1,
                root = UpdatedRoot
            }
    end.

%%

-ifdef(E48).
-doc """
Returns the intersection of `Tree1` and `Tree2`, i.e., a tree containing only
the keys present in both trees. For keys present in both, the value from
`Tree2` is kept.

## Examples

```erlang
> T1 = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> T2 = xb5_trees:from_list([{2, x}, {3, y}, {4, z}]).
> xb5_trees:to_list(xb5_trees:intersect(T1, T2)).
[{2, x}, {3, y}]
```
""".
-endif.
-spec intersect(Tree1, Tree2) -> Tree3 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value),
    Tree3 :: tree(Key, Value).

intersect(#xb5_tree{size = Size1, root = Root1}, #xb5_tree{size = Size2, root = Root2}) ->
    [NewSize | NewRoot] = xb5_trees_node:intersect(Size1, Root1, Size2, Root2),
    #xb5_tree{size = NewSize, root = NewRoot}.

%%

-ifdef(E48).
-doc """
Returns the intersection of `Tree1` and `Tree2`, using `Fun` to compute the
value for each key present in both trees. `Fun` is called as
`Fun(Key, Value1, Value2)` where `Value1` is from `Tree1` and `Value2` is
from `Tree2`.

## Examples

```erlang
> T1 = xb5_trees:from_list([{1, 10}, {2, 20}, {3, 30}]).
> T2 = xb5_trees:from_list([{2, 2}, {3, 3}, {4, 4}]).
> F = fun(_K, V1, V2) -> V1 * V2 end.
> xb5_trees:to_list(xb5_trees:intersect_with(F, T1, T2)).
[{2, 40}, {3, 90}]
```
""".
-endif.
-spec intersect_with(Fun, Tree1, Tree2) -> Tree3 when
    Fun :: fun((Key1 | Key2, Value1, Value2) -> Value3),
    Tree1 :: tree(Key1, Value1),
    Tree2 :: tree(Key2, Value2),
    Tree3 :: tree(Key1 | Key2, Value3).

intersect_with(Fun, #xb5_tree{size = Size1, root = Root1}, #xb5_tree{size = Size2, root = Root2}) ->
    [NewSize | NewRoot] = xb5_trees_node:intersect_with(Fun, Size1, Root1, Size2, Root2),
    #xb5_tree{size = NewSize, root = NewRoot}.

%%

-ifdef(E48).
-doc """
Returns `true` if `Key` is present in `Tree`, otherwise `false`.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> xb5_trees:is_defined(2, T).
true
> xb5_trees:is_defined(42, T).
false
```
""".
-endif.
-spec is_defined(Key, Tree) -> boolean() when
    Tree :: tree(Key, Value :: term()).

is_defined(Key, #xb5_tree{root = Root}) ->
    xb5_trees_node:get_att(
        Key,
        Root,
        fun is_defined_found/2,
        fun is_defined_not_found/1
    ).

%%

-ifdef(E48).
-doc """
Returns `true` if `Tree` is empty, otherwise `false`.

## Examples

```erlang
> xb5_trees:is_empty(xb5_trees:new()).
true
> xb5_trees:is_empty(xb5_trees:from_list([{1, a}])).
false
```
""".
-endif.
-spec is_empty(Tree) -> boolean() when
    Tree :: tree().

is_empty(#xb5_tree{size = Size}) ->
    Size =:= 0.

%%

-ifdef(E48).
-doc """
Returns `true` if `Tree1` and `Tree2` contain the same keys and values, otherwise
`false`.

Keys are compared with `==`, values with `=:=`.

## Examples

```erlang
> S1 = xb5_trees:from_list([{1, a} {2, b} {3, c}]).
> S2 = xb5_trees:from_list([{3, c}, {1.0, a} {2, b}]).
> xb5_trees:is_equal(S1, S2).
true
> S3 = xb5_trees:from_list([{1, x}, {2, y}]).
> xb5_trees:is_equal(S1, S3).
false
```
""".
-endif.
-spec is_equal(Tree1, Tree2) -> boolean() when
    Tree1 :: tree(),
    Tree2 :: tree().

is_equal(#xb5_tree{size = Size1, root = Root1}, #xb5_tree{size = Size2, root = Root2}) ->
    xb5_trees_node:is_equal(Size1, Root1, Size2, Root2).

%%

-ifdef(E48).
-doc """
Returns an iterator that can be used for traversing the entries of `Tree`;
see `next/1`. Equivalent to `iterator(Tree, ordered)`.

## Examples

```erlang
> T = xb5_trees:from_list([{3, c}, {1, a}, {2, b}]).
> I = xb5_trees:iterator(T).
> {1, a, I2} = xb5_trees:next(I).
> {2, b, I3} = xb5_trees:next(I2).
> {3, c, I4} = xb5_trees:next(I3).
> xb5_trees:next(I4).
none
```
""".
-endif.
-spec iterator(Tree) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value).

iterator(Tree) ->
    iterator(Tree, ordered).

%%

-ifdef(E48).
-doc """
Returns an iterator that can be used for traversing the entries of `Tree`
in the given `Order`; see `next/1`.

`Order` must be `ordered` (ascending) or `reversed` (descending).

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> I = xb5_trees:iterator(T, reversed).
> {3, c, I2} = xb5_trees:next(I).
> {2, b, I3} = xb5_trees:next(I2).
> {1, a, I4} = xb5_trees:next(I3).
> xb5_trees:next(I4).
none
```
""".
-endif.
-spec iterator(Tree, Order) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value),
    Order :: ordered | reversed.

iterator(#xb5_tree{root = Root}, Order) ->
    xb5_trees_node:iterator(Root, Order).

%%

-ifdef(E48).
-doc """
Returns an iterator that can be used for traversing the entries of `Tree`
starting from the first key greater than or equal to `Key`; see `next/1`.
Equivalent to `iterator_from(Key, Tree, ordered)`.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}, {4, d}, {5, e}]).
> I = xb5_trees:iterator_from(3, T).
> {3, c, I2} = xb5_trees:next(I).
> {4, d, I3} = xb5_trees:next(I2).
> {5, e, I4} = xb5_trees:next(I3).
> xb5_trees:next(I4).
none
```
""".
-endif.
-spec iterator_from(Key, Tree) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value).

iterator_from(Key, Tree) ->
    iterator_from(Key, Tree, ordered).

%%

-ifdef(E48).
-doc """
Returns an iterator that can be used for traversing the entries of `Tree`
starting from the key nearest to `Key` in the given `Order`; see `next/1`.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}, {4, d}, {5, e}]).
> I = xb5_trees:iterator_from(3, T, reversed).
> {3, c, I2} = xb5_trees:next(I).
> {2, b, I3} = xb5_trees:next(I2).
> {1, a, I4} = xb5_trees:next(I3).
> xb5_trees:next(I4).
none
```
""".
-endif.
-spec iterator_from(Key, Tree, Order) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value),
    Order :: ordered | reversed.

iterator_from(Key, #xb5_tree{root = Root}, Order) ->
    xb5_trees_node:iterator_from(Key, Root, Order).

%%

-ifdef(E48).
-doc """
Returns the keys in `Tree` as an ordered list.

## Examples

```erlang
> T = xb5_trees:from_list([{3, c}, {1, a}, {2, b}]).
> xb5_trees:keys(T).
[1, 2, 3]
```
""".
-endif.
-spec keys(Tree) -> [Key] when
    Tree :: tree(Key, _).

keys(#xb5_tree{root = Root}) ->
    xb5_trees_node:keys(Root).

%%

-ifdef(E48).
-doc """
Returns `{Key2, Value}` where `Key2` is the least key strictly greater
than `Key1` in `Tree`, or `none` if no such key exists.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {3, c}, {5, e}]).
> xb5_trees:larger(2, T).
{3, c}
> xb5_trees:larger(5, T).
none
```
""".
-endif.
-spec larger(Key1, Tree) -> none | {Key2, Value} when
    Key1 :: Key,
    Key2 :: Key,
    Tree :: tree(Key, Value).

larger(Key, #xb5_tree{root = Root}) ->
    xb5_trees_node:larger(Key, Root).

%%

-ifdef(E48).
-doc """
Returns `{Key, Value}` where `Key` is the largest key in `Tree`.

Raises an `empty_tree` error if the tree is empty.

## Examples

```erlang
> xb5_trees:largest(xb5_trees:from_list([{1, a}, {2, b}, {3, c}])).
{3, c}
```
""".
-endif.
-spec largest(Tree) -> {Key, Value} when
    Tree :: tree(Key, Value).

largest(#xb5_tree{size = Size, root = Root}) when Size =/= 0 ->
    xb5_trees_node:largest(Root);
largest(#xb5_tree{}) ->
    error_empty_tree().

%%

-ifdef(E48).
-doc """
Looks up `Key` in `Tree`. Returns `{value, Value}` if `Key` is present,
or `none` if `Key` is not present.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> xb5_trees:lookup(2, T).
{value, b}
> xb5_trees:lookup(42, T).
none
```
""".
-endif.
-spec lookup(Key, Tree) -> none | {value, Value} when
    Tree :: tree(Key, Value).

lookup(Key, #xb5_tree{root = Root}) ->
    xb5_trees_node:get_att(
        Key,
        Root,
        fun lookup_found/2,
        fun lookup_not_found/1
    ).

%%

-ifdef(E48).
-doc """
Maps `Function` over all values in `Tree1`, returning a new tree `Tree2`
with the same keys. For each entry, `Function(Key, Value1)` must return
the new value `Value2`.

## Examples

```erlang
> T = xb5_trees:from_list([{1, 10}, {2, 20}, {3, 30}]).
> xb5_trees:to_list(xb5_trees:map(fun(_K, V) -> V * 2 end, T)).
[{1, 20}, {2, 40}, {3, 60}]
```
""".
-endif.
-spec map(Function, Tree1) -> Tree2 when
    Function :: fun((K :: Key, V1 :: Value1) -> V2 :: Value2),
    Tree1 :: tree(Key, Value1),
    Tree2 :: tree(Key, Value2).

map(Fun, #xb5_tree{root = Root} = Tree) ->
    Tree#xb5_tree{root = xb5_trees_node:map(Fun, Root)}.

%%

-ifdef(E48).
-doc """
Merges `Tree1` and `Tree2` into a single tree. For keys present in both trees,
the value from `Tree2` is kept.

## Examples

```erlang
> T1 = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> T2 = xb5_trees:from_list([{2, x}, {4, y}]).
> xb5_trees:to_list(xb5_trees:merge(T1, T2)).
[{1, a}, {2, x}, {3, c}, {4, y}]
```
""".
-endif.
-spec merge(Tree1, Tree2) -> Tree3 when
    Tree1 :: tree(KeyA, ValueA),
    Tree2 :: tree(KeyB, ValueB),
    Tree3 :: tree(KeyA | KeyB, ValueA | ValueB).

merge(#xb5_tree{size = Size1, root = Root1}, #xb5_tree{size = Size2, root = Root2}) ->
    [NewSize | NewRoot] = xb5_trees_node:merge(Size1, Root1, Size2, Root2),
    #xb5_tree{size = NewSize, root = NewRoot}.

%%

-ifdef(E48).
-doc """
Merges `Tree1` and `Tree2` into a single tree, using `Fun` to compute the
value for each key present in both trees. `Fun` is called as
`Fun(Key, Value1, Value2)` where `Value1` is from `Tree1` and `Value2` is
from `Tree2`.

## Examples

```erlang
> T1 = xb5_trees:from_list([{1, 10}, {2, 20}, {3, 30}]).
> T2 = xb5_trees:from_list([{2, 2}, {4, 40}]).
> F = fun(_K, V1, V2) -> V1 + V2 end.
> xb5_trees:to_list(xb5_trees:merge_with(F, T1, T2)).
[{1, 10}, {2, 22}, {3, 30}, {4, 40}]
```
""".
-endif.
-spec merge_with(Fun, Tree1, Tree2) -> Tree3 when
    Fun :: fun((KeyA | KeyB, ValueA, ValueB) -> MergeValue),
    Tree1 :: tree(KeyA, ValueA),
    Tree2 :: tree(KeyB, ValueB),
    Tree3 :: tree(KeyA | KeyB, ValueA | ValueB | MergeValue).

merge_with(Fun, #xb5_tree{size = Size1, root = Root1}, #xb5_tree{size = Size2, root = Root2}) ->
    [NewSize | NewRoot] = xb5_trees_node:merge_with(Fun, Size1, Root1, Size2, Root2),
    #xb5_tree{size = NewSize, root = NewRoot}.

%%

-ifdef(E48).
-doc """
Returns a new empty tree.

## Examples

```erlang
> xb5_trees:is_empty(xb5_trees:new()).
true
> xb5_trees:size(xb5_trees:new()).
0
```
""".
-endif.
-spec new() -> tree().

new() -> #xb5_tree{root = xb5_trees_node:new(), size = 0}.

%%

-ifdef(E48).
-doc """
Returns `{Key, Value, Iter2}` where `Key` and `Value` are the next entry
referred to by iterator `Iter1` and `Iter2` is the updated iterator, or
`none` if no more entries remain.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}]).
> I = xb5_trees:iterator(T).
> {1, a, I2} = xb5_trees:next(I).
> {2, b, I3} = xb5_trees:next(I2).
> xb5_trees:next(I3).
none
```
""".
-endif.
-spec next(Iter1) -> none | {Key, Value, Iter2} when
    Iter1 :: iter(Key, Value),
    Iter2 :: iter(Key, Value).

next(Iter) ->
    xb5_trees_node:next(Iter).

%%

-ifdef(E48).
-doc """
Returns the number of entries in `Tree`.

## Examples

```erlang
> xb5_trees:size(xb5_trees:new()).
0
> xb5_trees:size(xb5_trees:from_list([{1, a}, {2, b}, {3, c}])).
3
```
""".
-endif.
-spec size(Tree) -> non_neg_integer() when
    Tree :: tree().

size(#xb5_tree{size = Size}) -> Size.

%%

-ifdef(E48).
-doc """
Returns `{Key2, Value}` where `Key2` is the greatest key strictly less
than `Key1` in `Tree`, or `none` if no such key exists.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {3, c}, {5, e}]).
> xb5_trees:smaller(4, T).
{3, c}
> xb5_trees:smaller(1, T).
none
```
""".
-endif.
-spec smaller(Key1, Tree) -> none | {Key2, Value} when
    Key1 :: Key,
    Key2 :: Key,
    Tree :: tree(Key, Value).

smaller(Key, #xb5_tree{root = Root}) ->
    xb5_trees_node:smaller(Key, Root).

%%

-ifdef(E48).
-doc """
Returns `{Key, Value}` where `Key` is the smallest key in `Tree`.

Raises an `empty_tree` error if the tree is empty.

## Examples

```erlang
> xb5_trees:smallest(xb5_trees:from_list([{3, c}, {1, a}, {2, b}])).
{1, a}
```
""".
-endif.
-spec smallest(Tree) -> {Key, Value} when
    Tree :: tree(Key, Value).

smallest(#xb5_tree{size = Size, root = Root}) when Size =/= 0 ->
    xb5_trees_node:smallest(Root);
smallest(#xb5_tree{}) ->
    error_empty_tree().

%%

-ifdef(E48).
-doc """
Returns structural statistics about the B-tree backing `Tree`.

This is primarily intended for debugging and testing. The result
is a proplist with keys such as `height`, `total_keys`,
`node_counts`, `node_percentages`, and others.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> Stats = xb5_trees:structural_stats(T).
> {height, 1} = lists:keyfind(height, 1, Stats).
> {total_keys, 3} = lists:keyfind(total_keys, 1, Stats).
```
""".
-endif.
-spec structural_stats(Tree) -> Stats when
    Tree :: tree(),
    Stats :: xb5_structural_stats:t().

structural_stats(#xb5_tree{root = Root}) ->
    xb5_trees_node:structural_stats(Root).

%%

-ifdef(E48).
-doc """
Returns `{Value, Tree2}` where `Value` is the value associated with `Key`
and `Tree2` is `Tree1` with `Key` removed.

Raises a `{badkey, Key}` error if the key is not present.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> {b, T2} = xb5_trees:take(2, T).
> xb5_trees:to_list(T2).
[{1, a}, {3, c}]
```
""".
-endif.
-spec take(Key, Tree1) -> {Value, Tree2} when
    Tree1 :: tree(Key, _),
    Tree2 :: tree(Key, _),
    Key :: term(),
    Value :: term().

take(Key, #xb5_tree{size = Size, root = Root} = Tree) ->
    case xb5_trees_node:take_att(Key, Root) of
        badkey ->
            error_badkey(Key);
        %
        [TakenPair | UpdatedRoot] ->
            UpdatedTree = Tree#xb5_tree{size = Size - 1, root = UpdatedRoot},

            [_ | Value] = TakenPair,
            {Value, UpdatedTree}
    end.

%%

-ifdef(E48).
-doc """
Like `take/2`, but returns `error` if the key is not present instead of
raising an exception.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> {b, T2} = xb5_trees:take_any(2, T).
> xb5_trees:to_list(T2).
[{1, a}, {3, c}]
> xb5_trees:take_any(42, T).
error
```
""".
-endif.
-spec take_any(Key, Tree1) -> {Value, Tree2} | error when
    Tree1 :: tree(Key, _),
    Tree2 :: tree(Key, _),
    Key :: term(),
    Value :: term().

take_any(Key, #xb5_tree{size = Size, root = Root} = Tree) ->
    case xb5_trees_node:take_att(Key, Root) of
        badkey ->
            error;
        %
        [TakenPair | UpdatedRoot] ->
            UpdatedTree = Tree#xb5_tree{size = Size - 1, root = UpdatedRoot},

            [_ | Value] = TakenPair,
            {Value, UpdatedTree}
    end.

%%

-ifdef(E48).
-doc """
Returns `{Key, Value, Tree2}` where `Key` is the largest key in `Tree1`,
`Value` is its associated value, and `Tree2` is `Tree1` with that entry
removed.

Raises an `empty_tree` error if the tree is empty.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> {3, c, T2} = xb5_trees:take_largest(T).
> xb5_trees:to_list(T2).
[{1, a}, {2, b}]
```
""".
-endif.
-spec take_largest(Tree1) -> {Key, Value, Tree2} when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

take_largest(#xb5_tree{size = Size, root = Root} = Tree) when Size =/= 0 ->
    [TakenPair | UpdatedRoot] = xb5_trees_node:take_largest(Root),
    UpdatedTree = Tree#xb5_tree{size = Size - 1, root = UpdatedRoot},

    [Key | Value] = TakenPair,
    {Key, Value, UpdatedTree};
take_largest(#xb5_tree{}) ->
    error_empty_tree().

%%

-ifdef(E48).
-doc """
Returns `{Key, Value, Tree2}` where `Key` is the smallest key in `Tree1`,
`Value` is its associated value, and `Tree2` is `Tree1` with that entry
removed.

Raises an `empty_tree` error if the tree is empty.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> {1, a, T2} = xb5_trees:take_smallest(T).
> xb5_trees:to_list(T2).
[{2, b}, {3, c}]
```
""".
-endif.
-spec take_smallest(Tree1) -> {Key, Value, Tree2} when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

take_smallest(#xb5_tree{size = Size, root = Root} = Tree) when Size =/= 0 ->
    [TakenPair | UpdatedRoot] = xb5_trees_node:take_smallest(Root),
    UpdatedTree = Tree#xb5_tree{size = Size - 1, root = UpdatedRoot},

    [Key | Value] = TakenPair,
    {Key, Value, UpdatedTree};
take_smallest(#xb5_tree{}) ->
    error_empty_tree().

%%

-ifdef(E48).
-doc """
Converts `Tree` into an ordered list of `{Key, Value}` tuples.

## Examples

```erlang
> xb5_trees:to_list(xb5_trees:from_list([{3, c}, {1, a}, {2, b}])).
[{1, a}, {2, b}, {3, c}]
> xb5_trees:to_list(xb5_trees:new()).
[]
```
""".
-endif.
-spec to_list(Tree) -> [{Key, Value}] when
    Tree :: tree(Key, Value).

to_list(#xb5_tree{root = Root}) ->
    xb5_trees_node:to_list(Root).

%%

-ifdef(E48).
-doc """
Unwraps an opaque tree into a plain map representation suitable for
cross-language interop (for example, converting to an Elixir struct
that uses the same underlying node module). Returns `{ok, Unwrapped}`
on success or `{error, Reason}` if `Term` is not a valid tree.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> {ok, Unwrapped} = xb5_trees:unwrap(T).
> maps:get(size, Unwrapped).
3
> {error, _} = xb5_trees:unwrap(not_a_tree).
```
""".
-endif.
-spec unwrap(Term :: _) -> {ok, unwrapped_tree(_, _)} | {error, Reason :: _}.

unwrap(#xb5_tree{size = Size, root = Root} = Term) when is_integer(Size), Size >= 0 ->
    try xb5_trees_node:structural_stats(Root) of
        Stats ->
            case lists:keyfind(total_keys, 1, Stats) of
                {_, TotalKeys} when TotalKeys =:= Size ->
                    {ok, #{size => Size, root => Root}};
                %
                {_, _} ->
                    {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_tree, Term})}
            end
    catch
        Class:Reason when Class =/= error; Reason =/= undef ->
            {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_tree, Term})}
    end;
unwrap(Term) ->
    {error, xb5_utils:dialyzer_opaque_term({not_an_xb5_tree, Term})}.

%%

-ifdef(E48).
-doc """
Updates `Key` to value `Value` in `Tree1`, returning a new tree `Tree2`.

Raises a `{badkey, Key}` error if the key is not present.

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> xb5_trees:to_list(xb5_trees:update(2, z, T)).
[{1, a}, {2, z}, {3, c}]
```
""".
-endif.
-spec update(Key, Value, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

update(Key, Value, #xb5_tree{root = Root} = Tree) ->
    case xb5_trees_node:update_att(Key, eager, Value, Root) of
        badkey ->
            error_badkey(Key);
        %
        UpdatedRoot ->
            Tree#xb5_tree{root = UpdatedRoot}
    end.

%%

-ifdef(E48).
-doc """
Applies `Fun` to the current value of `Key` in `Tree1`, storing the
result as the new value and returning the updated tree `Tree2`.

Raises a `{badkey, Key}` error if the key is not present.

## Examples

```erlang
> T = xb5_trees:from_list([{1, 10}, {2, 20}, {3, 30}]).
> xb5_trees:to_list(xb5_trees:update_with(2, fun(V) -> V + 1 end, T)).
[{1, 10}, {2, 21}, {3, 30}]
```
""".
-endif.
-spec update_with(Key, Fun, Tree1) -> Tree2 when
    Fun :: fun((Value1) -> Value2),
    Tree1 :: tree(Key, Value | Value1),
    Tree2 :: tree(Key, Value | Value2).

update_with(Key, Fun, #xb5_tree{root = Root} = Tree) ->
    case xb5_trees_node:update_att(Key, lazy, Fun, Root) of
        badkey ->
            error_badkey(Key);
        %
        UpdatedRoot ->
            Tree#xb5_tree{root = UpdatedRoot}
    end.

%%

-ifdef(E48).
-doc """
Like `update_with/3`, but inserts `Init` as the value if `Key` is not
present in `Tree1`.

## Examples

```erlang
> T = xb5_trees:from_list([{1, 10}, {2, 20}]).
> T2 = xb5_trees:update_with(2, fun(V) -> V + 1 end, 0, T).
> xb5_trees:to_list(T2).
[{1, 10}, {2, 21}]
> T3 = xb5_trees:update_with(3, fun(V) -> V + 1 end, 0, T).
> xb5_trees:to_list(T3).
[{1, 10}, {2, 20}, {3, 0}]
```
""".
-endif.
-spec update_with(Key, Fun, Init, Tree1) -> Tree2 when
    Fun :: fun((Value1) -> Value2),
    Tree1 :: tree(Key, Value | Value1),
    Tree2 :: tree(Key, Value | Value2 | Init).

update_with(Key, Fun, Init, #xb5_tree{root = Root} = Tree) ->
    case xb5_trees_node:update_att(Key, lazy, Fun, Root) of
        badkey ->
            Tree#xb5_tree{
                root = xb5_trees_node:insert_att(Key, eager, Init, Root),
                size = Tree#xb5_tree.size + 1
            };
        %
        UpdatedRoot ->
            Tree#xb5_tree{root = UpdatedRoot}
    end.

%%

-ifdef(E48).
-doc """
Returns the values in `Tree` as a list, ordered by their corresponding
keys.

## Examples

```erlang
> T = xb5_trees:from_list([{3, c}, {1, a}, {2, b}]).
> xb5_trees:values(T).
[a, b, c]
```
""".
-endif.
-spec values(Tree) -> [Value] when
    Tree :: tree(_, Value).

values(#xb5_tree{root = Root}) ->
    xb5_trees_node:values(Root).

%%

-ifdef(E48).
-doc """
Wraps a plain map representation back into an opaque tree.

This is the inverse of `unwrap/1` and is intended for cross-language
interop (for example, converting from an Elixir struct that shares the
same underlying node module).

## Examples

```erlang
> T = xb5_trees:from_list([{1, a}, {2, b}, {3, c}]).
> {ok, U} = xb5_trees:unwrap(T).
> T2 = xb5_trees:wrap(U).
> xb5_trees:to_list(T2).
[{1, a}, {2, b}, {3, c}]
```
""".
-endif.
-spec wrap(unwrapped_tree(Key, Value)) -> tree(Key, Value).

wrap(#{root := Root, size := Size}) when is_integer(Size), Size >= 0 ->
    #xb5_tree{root = Root, size = Size}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-compile({inline, error_badkey/1}).
-spec error_badkey(term()) -> no_return().
error_badkey(Key) ->
    error({badkey, Key}).

-compile({inline, error_empty_tree/0}).
-spec error_empty_tree() -> no_return().
error_empty_tree() ->
    error(empty_tree).

-compile({inline, error_key_exists/1}).
-spec error_key_exists(term()) -> no_return().
error_key_exists(Key) ->
    error({key_exists, Key}).

%%

is_defined_found(_Key, _Value) -> true.

is_defined_not_found(_Key) -> false.

%%

get_found(_Key, Value) -> Value.

-spec get_not_found(_) -> no_return().
get_not_found(Key) -> error_badkey(Key).

%%

lookup_found(_Key, Value) -> {value, Value}.

lookup_not_found(_Key) -> none.
