-module(b5_ranks).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    delete/2,
    empty/0,
    enter/3,
    foldl/3,
    foldr/3,
    from_list/1,
    get/2,
    insert/3,
    insert_with/3,
    is_defined/2,
    is_empty/1,
    iterator/1,
    iterator/2,
    iterator_from/2,
    iterator_from/3,
    keys/1,
    larger/2,
    largest/1,
    lookup/2,
    map/2,
    new/0,
    next/1,
    nth/2,
    size/1,
    smaller/2,
    smallest/1,
    take/2,
    take_any/2,
    take_largest/1,
    take_nth/2,
    take_smallest/1,
    to_list/1,
    update/3,
    update_with/3,
    update_with/4,
    validate/1,
    values/1
]).

-ignore_xref([
    delete/2,
    empty/0,
    enter/3,
    foldl/3,
    foldr/3,
    from_list/1,
    get/2,
    insert/3,
    insert_with/3,
    is_defined/2,
    is_empty/1,
    iterator/1,
    iterator/2,
    iterator_from/2,
    iterator_from/3,
    keys/1,
    larger/2,
    largest/1,
    lookup/2,
    map/2,
    new/0,
    next/1,
    size/1,
    smaller/2,
    smallest/1,
    take/2,
    take_any/2,
    take_largest/1,
    take_nth/2,
    take_smallest/1,
    to_list/1,
    update/3,
    update_with/3,
    update_with/4,
    validate/1,
    values/1
]).

-export([
    from_constituent_parts/1,
    to_constituent_parts/1
]).

-ignore_xref([
    from_constituent_parts/1,
    to_constituent_parts/1
]).

%% ------------------------------------------------------------------
%% API Type Definitions
%% ------------------------------------------------------------------

-record(b5_ranks, {size, h2b, root}).

-opaque tree(Key, Value) :: #b5_ranks{
    size :: non_neg_integer(),
    h2b :: b5_ranks_h2b:t(),
    root :: b5_ranks_node:t(Key, Value)
}.
-export_type([tree/2]).

%%%%%%%

-type iter(Key, Value) :: b5_ranks_node:iter(Key, Value).
-export_type([iter/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-if(?OTP_RELEASE >= 27).
-doc """
Removes the node with key `Key` from `Tree` and returns the new tree. The call
fails with a `{badkey, Key}` exception if the key is not present in the tree.
Returns the new tree.
""".
-endif.
-spec delete(Key, Tree) -> UpdatedTree when
    Key :: term(), Value :: term(), Tree :: tree(Key, Value), UpdatedTree :: tree(Key, Value).
delete(Key, #b5_ranks{root = Root, h2b = H2B, size = Size} = Tree) ->
    case b5_ranks_node:delete(Key, H2B, Root) of
        {height_decreased, UpdatedRoot} ->
            Tree#b5_ranks{
                size = Size - 1,
                h2b = tl(H2B),
                root = UpdatedRoot
            };
        UpdatedRoot ->
            Tree#b5_ranks{
                size = Size - 1,
                root = UpdatedRoot
            }
    end.

% TODO make this hidden, merely a convenience function
-if(?OTP_RELEASE >= 27).
-doc "Returns a new empty tree.".
-endif.
-spec empty() -> tree(_, _).
empty() -> new().

-if(?OTP_RELEASE >= 27).
-doc """
Inserts `Key` with value `Value` into the tree. The call fails with a
`{key_exists, Key}` exception if the key is already present in the tree.
Returns the new tree.
""".
-endif.
-spec enter(Key, Value, tree(Key, Value)) -> tree(Key, Value).
enter(Key, Value, #b5_ranks{size = Size, h2b = H2B, root = Root} = Tree) ->
    try b5_ranks_node:update(Key, eager, Value, Root) of
        UpdatedRoot ->
            Tree#b5_ranks{root = UpdatedRoot}
    catch
        error:{badkey, K} when K =:= Key ->
            case b5_ranks_node:insert(Key, eager, Value, H2B, Root) of
                {height_increased, UpdatedH2B, UpdatedRoot} ->
                    Tree#b5_ranks{
                        size = Size + 1,
                        h2b = UpdatedH2B,
                        root = UpdatedRoot
                    };
                UpdatedRoot ->
                    Tree#b5_ranks{
                        size = Size + 1,
                        root = UpdatedRoot
                    }
            end
    end.

-if(?OTP_RELEASE >= 27).
-doc """
Folds the tree from left to right (i.e., from the smallest key to the largest).
Returns the final value of the accumulator.
""".
-endif.
-spec foldl(fun((Key, Value, Acc1) -> Acc2), Acc0, tree(Key, Value)) -> AccN when
    AccN :: Acc2, Acc2 :: Acc1, Acc1 :: Acc0.
foldl(Fun, Acc0, #b5_ranks{root = Root}) ->
    b5_ranks_node:foldl(Fun, Acc0, Root).

-if(?OTP_RELEASE >= 27).
-doc """
Folds the tree from right to left (i.e., from the largest key to the smallest).
Returns the final value of the accumulator.
""".
-endif.
-spec foldr(fun((Key, Value, Acc1) -> Acc2), Acc0, tree(Key, Value)) -> AccN when
    AccN :: Acc2, Acc2 :: Acc1, Acc1 :: Acc0.
foldr(Fun, Acc0, #b5_ranks{root = Root}) ->
    b5_ranks_node:foldr(Fun, Acc0, Root).

-if(?OTP_RELEASE >= 27).
-doc "Turns a list of key-value tuples into a tree.".
-endif.
-spec from_list([{Key, Value}]) -> tree(Key, Value).
from_list(List) ->
    lists:foldl(
        fun({K, V}, Acc) ->
            enter(K, V, Acc)
        end,
        new(),
        List
    ).

-if(?OTP_RELEASE >= 27).
-doc """
Retrieves the value stored with `Key` in `Tree`. The call fails with a
`{badkey, Key}` exception if the key is not present in the tree.
""".
-endif.
-spec get(Key, Tree) -> Value when Key :: term(), Tree :: tree(Key, Value).
get(Key, #b5_ranks{root = Root}) ->
    b5_ranks_node:get(Key, Root).

-if(?OTP_RELEASE >= 27).
-doc """
Inserts `Key` with value `Value` into `Tree` and returns the new tree. The call
fails with a `{key_exists, Key}` exception if the key is already present in the
tree.
""".
-endif.
-spec insert(Key, Value, Tree) -> UpdatedTree when
    Tree :: tree(Key, Value), UpdatedTree :: tree(Key, Value).
insert(Key, Value, #b5_ranks{size = Size, h2b = H2B, root = Root} = Tree) ->
    case b5_ranks_node:insert(Key, eager, Value, H2B, Root) of
        {height_increased, UpdatedH2B, UpdatedRoot} ->
            Tree#b5_ranks{
                size = Size + 1,
                h2b = UpdatedH2B,
                root = UpdatedRoot
            };
        UpdatedRoot ->
            Tree#b5_ranks{
                size = Size + 1,
                root = UpdatedRoot
            }
    end.

-if(?OTP_RELEASE >= 27).
-doc """
Inserts `Key` with value `Value` into `Tree` and returns the new tree.

This function is useful in case you want to compute the value to put under key
only if key is not already present, as for example, when the value is expensive
to calculate or generally difficult to setup and teardown again.

The call fails with a `{key_exists, Key}` exception if the key is already
present in the tree.
""".
-endif.
-spec insert_with(Key, Fun, Tree) -> UpdatedTree when
    Tree :: tree(Key, Value),
    Fun :: fun(() -> Value),
    UpdatedTree :: tree(Key, Value).
insert_with(Key, Fun, #b5_ranks{size = Size, h2b = H2B, root = Root} = Tree) ->
    case b5_ranks_node:insert(Key, lazy, Fun, H2B, Root) of
        {height_increased, UpdatedH2B, UpdatedRoot} ->
            Tree#b5_ranks{
                size = Size + 1,
                h2b = UpdatedH2B,
                root = UpdatedRoot
            };
        UpdatedRoot ->
            Tree#b5_ranks{
                size = Size + 1,
                root = UpdatedRoot
            }
    end.

-if(?OTP_RELEASE >= 27).
-doc "Returns `true` if `Key` is present in `Tree`, otherwise `false`.".
-endif.
-spec is_defined(Key, tree(Key, _)) -> boolean().
is_defined(Key, Tree) ->
    try get(Key, Tree) of
        _ ->
            true
    catch
        error:{badkey, K} when K =:= Key ->
            false
    end.

-if(?OTP_RELEASE >= 27).
-doc "Returns `true` if `Tree` is an empty tree, otherwise `false`.".
-endif.
-spec is_empty(tree(_, _)) -> boolean().
is_empty(#b5_ranks{size = Size}) ->
    Size =:= 0.

-if(?OTP_RELEASE >= 27).
-doc """
Returns an iterator that can be used for traversing the entries of `Tree` using
`next/1`.
Equivalent to `iterator(Tree, ordered)`.
""".
-endif.
-spec iterator(Tree) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: b5_ranks_node:iter(Key, Value).

iterator(Tree) ->
    iterator(Tree, ordered).

% TODO confirm performance claims
-if(?OTP_RELEASE >= 27).
-doc """
Returns an iterator that can be used for traversing the entries of `Tree` in
either `ordered` or `reversed` direction using `next/1`. The implementation is
very efficient; traversing the whole tree using `next/1` is only slightly
slower than getting the list of all elements using `to_list/1`.
""".
-endif.
%% @see next/1
-spec iterator(Tree, Order) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value),
    Order :: ordered | reversed.

iterator(#b5_ranks{root = Root}, Order) ->
    b5_ranks_node:iterator(Root, Order).

-if(?OTP_RELEASE >= 27).
-doc """
Returns an iterator that can be used for traversing the entries of `Tree`,
starting from the first key greater than or equal to `Key`, using `next/1`.
Equivalent to `iterator_from(Key, Tree, ordered)`.
""".
-endif.
%% @see next/1
-spec iterator_from(Key, Tree) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value).

iterator_from(Key, Tree) ->
    iterator_from(Key, Tree, ordered).

-if(?OTP_RELEASE >= 27).
-doc """
Returns an iterator that can be used for traversing the entries of `Tree` in
either `ordered` or `reversed` direction, starting from the first key greater
than or equal to `Key`, using `next/1`.
""".
-endif.
%% @see next/1
-spec iterator_from(Key, Tree, Order) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value),
    Order :: ordered | reversed.

iterator_from(Key, #b5_ranks{root = Root}, Order) ->
    b5_ranks_node:iterator_from(Key, Root, Order).

-if(?OTP_RELEASE >= 27).
-doc "Returns the keys in `Tree` as an ordered list.".
-endif.
-spec keys(Tree) -> [Key] when Tree :: tree(Key, _).
keys(#b5_ranks{root = Root}) ->
    b5_ranks_node:keys(Root).

-if(?OTP_RELEASE >= 27).
-doc """
Returns the smallest key-value pair where the key is larger than the given key,
or `none` if no such key exists.
""".
-endif.
-spec larger(Key, Tree) -> {Key, Value} | none when Tree :: tree(Key, Value).
larger(Key, #b5_ranks{root = Root}) ->
    b5_ranks_node:larger(Key, Root).

-if(?OTP_RELEASE >= 27).
-doc """
Returns the largest key-value pair in the tree. The call fails with an
`empty_tree` exception if the tree is empty.
""".
-endif.
-spec largest(Tree) -> {Key, Value} when Tree :: tree(Key, Value).
largest(#b5_ranks{root = Root}) ->
    b5_ranks_node:largest(Root).

-if(?OTP_RELEASE >= 27).
-doc """
Looks up `Key` in `Tree`.
Returns `{value, Value}`, or `none` if the key is not present.
""".
-endif.
-spec lookup(Key, Tree) -> {value, Value} | none when Tree :: tree(Key, Value).
lookup(Key, Tree) ->
    try get(Key, Tree) of
        Value ->
            {value, Value}
    catch
        error:{badkey, K} when K =:= Key ->
            none
    end.

-if(?OTP_RELEASE >= 27).
-doc """
Maps function `Fun(Key, Value1) -> Value2` to all key-value pairs of `Tree`.
Returns a new tree with the same set of keys and the new set of values.
""".
-endif.
-spec map(Fun, Tree) -> Tree2 when
    Fun :: fun((Key, Value) -> Value2),
    Tree :: tree(Key, Value),
    Tree2 :: tree(Key, Value2).

map(Fun, #b5_ranks{root = Root} = Tree) ->
    Tree#b5_ranks{root = b5_ranks_node:map(Fun, Root)}.

-if(?OTP_RELEASE >= 27).
-doc "Returns a new empty tree.".
-endif.
-spec new() -> tree(_, _).
new() ->
    #b5_ranks{
        size = 0,
        h2b = [],
        root = b5_ranks_node:new()
    }.

-if(?OTP_RELEASE >= 27).
-doc """
Returns the next key-value pair from the iterator.
Returns `{Key, Value, NewIter}` or `none` if no more entries remain.
""".
-endif.
-spec next(Iter) -> {Key, Value, Iter} | none when Iter :: iter(Key, Value).
next(Iter) ->
    b5_ranks_node:next(Iter).

%% TODO document
nth(N, #b5_ranks{size = Size, h2b = H2B, root = Root}) ->
    % Should we optimize for first and last? (Call smallest and largest)
    case N < 1 orelse N > Size of
        false ->
            b5_ranks_node:nth(N, H2B, Root);
        _ ->
            error({badarg, N})
    end.

-if(?OTP_RELEASE >= 27).
-doc "Returns the number of nodes in the tree.".
-endif.
-spec size(Tree) -> non_neg_integer() when Tree :: tree(_, _).
size(#b5_ranks{size = Size}) -> Size.

-if(?OTP_RELEASE >= 27).
-doc """
Returns the largest key-value pair where the key is smaller than the given key.
Returns `none` if no such key exists.
""".
-endif.
-spec smaller(Key, Tree) -> {Key, Value} | none when Tree :: tree(Key, Value).
smaller(Key, #b5_ranks{root = Root}) ->
    b5_ranks_node:smaller(Key, Root).

-if(?OTP_RELEASE >= 27).
-doc """
Returns the smallest key-value pair in the tree. The call fails with an
`empty_tree` exception if the tree is empty.
""".
-endif.
-spec smallest(Tree) -> {Key, Value} when Tree :: tree(Key, Value).
smallest(#b5_ranks{root = Root}) ->
    b5_ranks_node:smallest(Root).

-if(?OTP_RELEASE >= 27).
-doc """
Returns a value `Value` from node with key `Key` and new `Tree2` without the
node with this value. The call fails with a `{badkey, Key}` exception if the
key is not present in the tree.
""".
-endif.
-spec take(Key, Tree) -> {Value, Tree2} when
    Tree :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

take(Key, #b5_ranks{size = Size, h2b = H2B, root = Root} = Tree) ->
    case b5_ranks_node:take(Key, H2B, Root) of
        [TakenPair | UpdatedRoot] ->
            UpdatedTree = Tree#b5_ranks{
                size = Size - 1,
                root = UpdatedRoot
            },
            [_ | Value] = TakenPair,
            {Value, UpdatedTree};
        {height_decreased, TakenPair, UpdatedRoot} ->
            UpdatedTree = Tree#b5_ranks{
                size = Size - 1,
                h2b = tl(H2B),
                root = UpdatedRoot
            },
            [_ | Value] = TakenPair,
            {Value, UpdatedTree}
    end.

-if(?OTP_RELEASE >= 27).
-doc """
Returns a value `Value` from node with key `Key` and new `Tree2` without the
node with this value.
Returns `error` if the node with the key is not present in the tree.
""".
-endif.
-spec take_any(Key, Tree) -> {Value, Tree2} | error when
    Tree :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

take_any(Key, Tree) ->
    try
        take(Key, Tree)
    catch
        error:{badkey, K} when K =:= Key ->
            error
    end.

-if(?OTP_RELEASE >= 27).
-doc """
Returns `{Key, Value, Tree2}`, where `Key` is the largest key in `Tree`,
`Value` is the value associated with this key, and `Tree2` is this tree with
the corresponding node deleted. The call fails with an `empty_tree` exception
if the tree is empty.
""".
-endif.
-spec take_largest(Tree) -> {Key, Value, Tree2} when
    Tree :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

take_largest(#b5_ranks{size = Size, h2b = H2B, root = Root} = Tree) ->
    case b5_ranks_node:take_largest(H2B, Root) of
        [TakenPair | UpdatedRoot] ->
            UpdatedTree = Tree#b5_ranks{
                size = Size - 1,
                root = UpdatedRoot
            },
            [Key | Value] = TakenPair,
            {Key, Value, UpdatedTree};
        {height_decreased, TakenPair, UpdatedRoot} ->
            UpdatedTree = Tree#b5_ranks{
                size = Size - 1,
                h2b = tl(H2B),
                root = UpdatedRoot
            },
            [Key | Value] = TakenPair,
            {Key, Value, UpdatedTree}
    end.

%% TODO document
take_nth(N, #b5_ranks{size = Size, h2b = H2B, root = Root} = Tree) when N >= 0 andalso N =< Size ->
    % TODO Optimize for first and last?

    case b5_ranks_node:take_nth(N, H2B, Root) of
        [TakenPair | UpdatedRoot] ->
            UpdatedTree = Tree#b5_ranks{
                size = Size - 1,
                root = UpdatedRoot
            },
            [Key | Value] = TakenPair,
            {Key, Value, UpdatedTree};
        {height_decreased, TakenPair, UpdatedRoot} ->
            UpdatedTree = Tree#b5_ranks{
                size = Size - 1,
                h2b = tl(H2B),
                root = UpdatedRoot
            },
            [Key | Value] = TakenPair,
            {Key, Value, UpdatedTree}
    end.

-if(?OTP_RELEASE >= 27).
-doc """
Returns `{Key, Value, Tree2}`, where `Key` is the smallest key in `Tree`,
`Value` is the value associated with this key, and `Tree2` is this tree with
the the corresponding node deleted. The call fails with an `empty_tree`
exception if the tree is empty.
""".
-endif.
-spec take_smallest(Tree) -> {Key, Value, Tree2} when
    Tree :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

take_smallest(#b5_ranks{size = Size, h2b = H2B, root = Root} = Tree) ->
    case b5_ranks_node:take_smallest(H2B, Root) of
        [TakenPair | UpdatedRoot] ->
            UpdatedTree = Tree#b5_ranks{
                size = Size - 1,
                root = UpdatedRoot
            },
            [Key | Value] = TakenPair,
            {Key, Value, UpdatedTree};
        {height_decreased, TakenPair, UpdatedRoot} ->
            UpdatedTree = Tree#b5_ranks{
                size = Size - 1,
                h2b = tl(H2B),
                root = UpdatedRoot
            },
            [Key | Value] = TakenPair,
            {Key, Value, UpdatedTree}
    end.

-if(?OTP_RELEASE >= 27).
-doc "Converts a tree into an ordered list of key-value tuples.".
-endif.
-spec to_list(Tree) -> [{Key, Value}] when Tree :: tree(Key, Value).

to_list(#b5_ranks{root = Root}) ->
    b5_ranks_node:to_list(Root).

-if(?OTP_RELEASE >= 27).
-doc """
Updates `Key` to value `Value` in `Tree` and returns the new tree. The call
fails with a `{badkey, Key}` exception if the key is not present in the tree.
""".
-endif.
-spec update(Key, Value, Tree) -> Tree2 when
    Tree :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

update(Key, Value, #b5_ranks{root = Root} = Tree) ->
    Tree#b5_ranks{root = b5_ranks_node:update(Key, eager, Value, Root)}.

-if(?OTP_RELEASE >= 27).
-doc """
Updates the value associated with `Key` in `Tree` using the given function.

In comparison to `update/3`, this can be useful when it's expensive to
calculate the value.

The call fails with a `{badkey, Key}` exception if the key is not present in
the tree.
""".
-endif.
-spec update_with(Key, Fun, Tree) -> Tree2 when
    Fun :: fun((Value) -> Value2),
    Tree :: tree(Key, Value),
    Tree2 :: tree(Key, Value | Value2).

update_with(Key, Fun, #b5_ranks{root = Root} = Tree) ->
    Tree#b5_ranks{root = b5_ranks_node:update(Key, lazy, Fun, Root)}.

-if(?OTP_RELEASE >= 27).
-doc """
Updates the value associated with `Key` in `Tree` using the given function,
or inserts `Key` with value `Init` if the key is not present.
If the key exists, the function is applied to its current value.
If the key does not exist, `Init` is inserted as the value.
""".
-endif.
-spec update_with(Key, Fun, Init, Tree) -> Tree2 when
    Fun :: fun((Value) -> Value2),
    Tree :: tree(Key, Value),
    Tree2 :: tree(Key, Value | Value2 | Init).

update_with(Key, Fun, Init, #b5_ranks{size = Size, h2b = H2B, root = Root} = Tree) ->
    try b5_ranks_node:update(Key, lazy, Fun, Root) of
        UpdatedRoot ->
            Tree#b5_ranks{root = UpdatedRoot}
    catch
        error:{badkey, K} when K =:= Key ->
            case b5_ranks_node:insert(Key, eager, Init, H2B, Root) of
                {height_increased, UpdatedH2B, UpdatedRoot} ->
                    Tree#b5_ranks{
                        size = Tree#b5_ranks.size + 1,
                        h2b = UpdatedH2B,
                        root = UpdatedRoot
                    };
                UpdatedRoot ->
                    Tree#b5_ranks{
                        size = Size + 1,
                        root = UpdatedRoot
                    }
            end
    end.

-if(?OTP_RELEASE >= 27).
-doc """
Validates the internal structure of the tree.
Returns information about the tree structure for debugging purposes.
""".
-endif.
-spec validate(tree(_, _)) ->
    {ok, b5_ranks_node:valid_stats()} | {error, term()}.
validate(#b5_ranks{size = Size, h2b = H2B, root = Root}) ->
    b5_ranks_node:validate(Size, H2B, Root).

-if(?OTP_RELEASE >= 27).
-doc """
Returns the values in `Tree` as an ordered list, sorted by their corresponding keys.
Duplicates are not removed.
""".
-endif.
-spec values(Tree) -> [Value] when Tree :: tree(_, Value).
values(#b5_ranks{root = Root}) ->
    b5_ranks_node:values(Root).

-if(?OTP_RELEASE >= 27).
-doc """
Constructs a tree from its constituent parts.

This function is intended for use by alternative implementations that
want to reuse `b5_ranks_node` functionality while wrapping the tree
structure differently (e.g., in an Elixir struct).

The input map must contain `root` and `size` fields.
""".
-endif.
-spec from_constituent_parts(#{
    root := b5_ranks_node:t(Key, Value), size := non_neg_integer()
}) -> tree(Key, Value).
from_constituent_parts(#{root := Root, h2b := H2B, size := Size}) when
    is_integer(Size), Size >= 0
->
    #b5_ranks{root = Root, h2b = H2B, size = Size}.

-if(?OTP_RELEASE >= 27).
-doc """
Extracts the constituent parts of a tree.

Returns `{ok, Map}` where the map contains the `root` node and `size`.
This function is intended for use by alternative implementations that
want to reuse `b5_ranks_node` functionality while wrapping the tree
structure differently.

Returns `error` if the input is not a valid tree.
""".
-endif.
-spec to_constituent_parts(tree(Key, Value) | term()) ->
    {ok, #{root := b5_ranks_node:t(Key, Value), h2b := b5_ranks_h2b:t(), size := non_neg_integer()}}
    | error.
to_constituent_parts(#b5_ranks{root = Root, h2b = H2B, size = Size}) when
    is_integer(Size), Size >= 0
->
    {ok, #{root => Root, h2b => H2B, size => Size}};
to_constituent_parts(_) ->
    error.
