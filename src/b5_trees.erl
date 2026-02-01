-module(b5_trees).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
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
%% API Type Definitions
%% ------------------------------------------------------------------

-record(b5_trees, {size, root}).

-opaque tree(Key, Value) :: #b5_trees{
    size :: non_neg_integer(),
    root :: b5_trees_node:t(Key, Value)
}.
-export_type([tree/2]).

-type tree() :: tree(_, _).
-export_type([tree/0]).

-type iter(Key, Value) :: b5_trees_node:iter(Key, Value).
-export_type([iter/2]).

-type iter() :: iter(_, _).
-export_type([iter/0]).

%%

-type unwrapped_tree(Key, Value) :: #{
    size := non_neg_integer(),
    root := b5_trees_node:t(Key, Value)
}.
-export_type([unwrapped_tree/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec delete(Key, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

delete(Key, #b5_trees{root = Root, size = Size} = Tree) ->
    case b5_trees_node:delete_att(Key, Root) of
        none ->
            error_badkey(Key);
        %
        UpdatedRoot ->
            Tree#b5_trees{
                root = UpdatedRoot,
                size = Size - 1
            }
    end.

%%

-spec delete_any(Key, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

delete_any(Key, #b5_trees{root = Root, size = Size} = Tree) ->
    case b5_trees_node:delete_att(Key, Root) of
        none ->
            Tree;
        %
        UpdatedRoot ->
            Tree#b5_trees{
                root = UpdatedRoot,
                size = Size - 1
            }
    end.

%%

-spec empty() -> tree().

empty() -> new().

%%

-spec enter(Key, Value, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

enter(Key, Value, #b5_trees{size = Size, root = Root} = Tree) ->
    case b5_trees_node:insert_att(Key, eager, Value, Root) of
        none ->
            UpdatedRoot = b5_trees_node:update_att(Key, eager, Value, Root),
            Tree#b5_trees{root = UpdatedRoot};
        %
        UpdatedRoot ->
            Tree#b5_trees{
                root = UpdatedRoot,
                size = Size + 1
            }
    end.

%%

-spec foldl(Function, Acc0, Tree) -> Acc1 when
    Function :: fun((Key, Value, AccIn) -> AccOut),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc,
    Tree :: tree(Key, Value).

foldl(Fun, Acc0, #b5_trees{root = Root}) ->
    b5_trees_node:foldl(Fun, Acc0, Root).

%%

-spec foldr(Function, Acc0, Tree) -> Acc1 when
    Function :: fun((Key, Value, AccIn) -> AccOut),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc,
    Tree :: tree(Key, Value).

foldr(Fun, Acc0, #b5_trees{root = Root}) ->
    b5_trees_node:foldr(Fun, Acc0, Root).

%%

-spec from_list(List) -> Tree when
    List :: [{Key, Value}],
    Tree :: tree(Key, Value).

from_list(List) ->
    Size = 0,
    Root = b5_trees_node:new(),
    from_list_recur(List, Size, Root).

%%

-spec from_orddict(Orddict) -> Tree when
    Orddict :: orddict:orddict(Key, Value),
    Tree :: tree(Key, Value).

from_orddict(Orddict) ->
    List = orddict:to_list(Orddict),
    from_list(List).

%%

-spec get(Key, Tree) -> Value when
    Tree :: tree(Key, Value).

get(Key, #b5_trees{root = Root}) ->
    b5_trees_node:get(Key, Root).

%%

-spec insert(Key, Value, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

insert(Key, Value, #b5_trees{size = Size, root = Root} = Tree) ->
    case b5_trees_node:insert_att(Key, eager, Value, Root) of
        none ->
            error_key_exists(Key);
        %
        UpdatedRoot ->
            Tree#b5_trees{
                size = Size + 1,
                root = UpdatedRoot
            }
    end.

%%

-spec insert_with(Key, Fun, Tree1) -> Tree2 when
    Fun :: fun(() -> Value),
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

insert_with(Key, Fun, #b5_trees{size = Size, root = Root} = Tree) ->
    case b5_trees_node:insert_att(Key, lazy, Fun, Root) of
        none ->
            error_key_exists(Key);
        %
        UpdatedRoot ->
            Tree#b5_trees{
                size = Size + 1,
                root = UpdatedRoot
            }
    end.

%%

-spec is_defined(Key, Tree) -> boolean() when
    Tree :: tree(Key, Value :: term()).

is_defined(Key, #b5_trees{root = Root}) ->
    b5_trees_node:is_defined(Key, Root).

%%

-spec is_empty(Tree) -> boolean() when
    Tree :: tree().

is_empty(#b5_trees{size = Size}) ->
    Size =:= 0.

%%

-spec iterator(Tree) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value).

iterator(Tree) ->
    iterator(Tree, ordered).

%%

-spec iterator(Tree, Order) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value),
    Order :: ordered | reversed.

iterator(#b5_trees{root = Root}, Order) ->
    b5_trees_node:iterator(Root, Order).

%%

-spec iterator_from(Key, Tree) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value).

iterator_from(Key, Tree) ->
    iterator_from(Key, Tree, ordered).

%%

-spec iterator_from(Key, Tree, Order) -> Iter when
    Tree :: tree(Key, Value),
    Iter :: iter(Key, Value),
    Order :: ordered | reversed.

iterator_from(Key, #b5_trees{root = Root}, Order) ->
    b5_trees_node:iterator_from(Key, Root, Order).

%%

-spec keys(Tree) -> [Key] when
    Tree :: tree(Key, _).

keys(#b5_trees{root = Root}) ->
    b5_trees_node:keys(Root).

%%

-spec larger(Key1, Tree) -> none | {Key2, Value} when
    Key1 :: Key,
    Key2 :: Key,
    Tree :: tree(Key, Value).

larger(Key, #b5_trees{root = Root}) ->
    b5_trees_node:larger(Key, Root).

%%

-spec largest(Tree) -> {Key, Value} when
    Tree :: tree(Key, Value).

largest(#b5_trees{size = Size, root = Root}) when Size =/= 0 ->
    b5_trees_node:largest(Root);
largest(#b5_trees{}) ->
    error_empty_tree().

%%

-spec lookup(Key, Tree) -> 'none' | {'value', Value} when
    Tree :: tree(Key, Value).

lookup(Key, #b5_trees{root = Root}) ->
    b5_trees_node:lookup(Key, Root).

%%

-spec map(Function, Tree1) -> Tree2 when
    Function :: fun((K :: Key, V1 :: Value1) -> V2 :: Value2),
    Tree1 :: tree(Key, Value1),
    Tree2 :: tree(Key, Value2).

map(Fun, #b5_trees{root = Root} = Tree) ->
    Tree#b5_trees{root = b5_trees_node:map(Fun, Root)}.

%%

-spec new() -> tree().

new() -> #b5_trees{root = b5_trees_node:new(), size = 0}.

%%

-spec next(Iter1) -> none | {Key, Value, Iter2} when
    Iter1 :: iter(Key, Value),
    Iter2 :: iter(Key, Value).

next(Iter) ->
    b5_trees_node:next(Iter).

%%

-spec size(Tree) -> non_neg_integer() when
    Tree :: tree().

size(#b5_trees{size = Size}) -> Size.

%%

-spec smaller(Key1, Tree) -> none | {Key2, Value} when
    Key1 :: Key,
    Key2 :: Key,
    Tree :: tree(Key, Value).

smaller(Key, #b5_trees{root = Root}) ->
    b5_trees_node:smaller(Key, Root).

%%

-spec smallest(Tree) -> {Key, Value} when
    Tree :: tree(Key, Value).

smallest(#b5_trees{size = Size, root = Root}) when Size =/= 0 ->
    b5_trees_node:smallest(Root);
smallest(#b5_trees{}) ->
    error_empty_tree().

%%

-spec structural_stats(Tree) -> Stats when
    Tree :: tree(),
    Stats :: b5_structural_stats:t().

structural_stats(#b5_trees{root = Root}) ->
    b5_trees_node:structural_stats(Root).

%%

-spec take(Key, Tree1) -> {Value, Tree2} when
    Tree1 :: tree(Key, _),
    Tree2 :: tree(Key, _),
    Key :: term(),
    Value :: term().

take(Key, #b5_trees{size = Size, root = Root} = Tree) ->
    case b5_trees_node:take_att(Key, Root) of
        none ->
            error_badkey(Key);
        %
        [TakenPair | UpdatedRoot] ->
            UpdatedTree = Tree#b5_trees{size = Size - 1, root = UpdatedRoot},

            [_ | Value] = TakenPair,
            {Value, UpdatedTree}
    end.

%%

-spec take_any(Key, Tree1) -> {Value, Tree2} | error when
    Tree1 :: tree(Key, _),
    Tree2 :: tree(Key, _),
    Key :: term(),
    Value :: term().

take_any(Key, #b5_trees{size = Size, root = Root} = Tree) ->
    case b5_trees_node:take_att(Key, Root) of
        none ->
            error;
        %
        [TakenPair | UpdatedRoot] ->
            UpdatedTree = Tree#b5_trees{size = Size - 1, root = UpdatedRoot},

            [_ | Value] = TakenPair,
            {Value, UpdatedTree}
    end.

%%

-spec take_largest(Tree1) -> {Key, Value, Tree2} when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

take_largest(#b5_trees{size = Size, root = Root} = Tree) when Size =/= 0 ->
    [TakenPair | UpdatedRoot] = b5_trees_node:take_largest(Root),
    UpdatedTree = Tree#b5_trees{size = Size - 1, root = UpdatedRoot},

    [Key | Value] = TakenPair,
    {Key, Value, UpdatedTree};
take_largest(#b5_trees{}) ->
    error_empty_tree().

%%

-spec take_smallest(Tree1) -> {Key, Value, Tree2} when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

take_smallest(#b5_trees{size = Size, root = Root} = Tree) when Size =/= 0 ->
    [TakenPair | UpdatedRoot] = b5_trees_node:take_smallest(Root),
    UpdatedTree = Tree#b5_trees{size = Size - 1, root = UpdatedRoot},

    [Key | Value] = TakenPair,
    {Key, Value, UpdatedTree};
take_smallest(#b5_trees{}) ->
    error_empty_tree().

%%

-spec to_list(Tree) -> [{Key, Value}] when
    Tree :: tree(Key, Value).

to_list(#b5_trees{root = Root}) ->
    b5_trees_node:to_list(Root).

%%

-spec unwrap(Tree) -> Unwrapped when
    Tree :: tree(Key, Value),
    Unwrapped :: unwrapped_tree(Key, Value).

unwrap(#b5_trees{size = Size, root = Root}) ->
    #{size => Size, root => Root}.

%%

-spec update(Key, Value, Tree1) -> Tree2 when
    Tree1 :: tree(Key, Value),
    Tree2 :: tree(Key, Value).

update(Key, Value, #b5_trees{root = Root} = Tree) ->
    case b5_trees_node:update_att(Key, eager, Value, Root) of
        none ->
            error_badkey(Key);
        %
        UpdatedRoot ->
            Tree#b5_trees{root = UpdatedRoot}
    end.

%%

-spec update_with(Key, Fun, Tree1) -> Tree2 when
    Fun :: fun((Value1) -> Value2),
    Tree1 :: tree(Key, Value | Value1),
    Tree2 :: tree(Key, Value | Value2).

update_with(Key, Fun, #b5_trees{root = Root} = Tree) ->
    case b5_trees_node:update_att(Key, lazy, Fun, Root) of
        none ->
            error_badkey(Key);
        %
        UpdatedRoot ->
            Tree#b5_trees{root = UpdatedRoot}
    end.

%%

-spec update_with(Key, Fun, Init, Tree1) -> Tree2 when
    Fun :: fun((Value1) -> Value2),
    Tree1 :: tree(Key, Value | Value1),
    Tree2 :: tree(Key, Value | Value2 | Init).

update_with(Key, Fun, Init, #b5_trees{root = Root} = Tree) ->
    case b5_trees_node:update_att(Key, lazy, Fun, Root) of
        none ->
            Tree#b5_trees{
                root = b5_trees_node:insert_att(Key, eager, Init, Root),
                size = Tree#b5_trees.size + 1
            };
        %
        UpdatedRoot ->
            Tree#b5_trees{root = UpdatedRoot}
    end.

%%

-spec values(Tree) -> [Value] when
    Tree :: tree(_, Value).

values(#b5_trees{root = Root}) ->
    b5_trees_node:values(Root).

%%

-spec wrap(_) -> {ok, Tree} | error when Tree :: tree().
wrap(#{root := Root, size := Size}) when is_integer(Size) andalso Size >= 0 ->
    try b5_trees_node:structural_stats(Root) of
        Stats ->
            {_, TotalKeys} = lists:keyfind(total_keys, 1, Stats),

            case TotalKeys =:= Size of
                true ->
                    {ok, #b5_trees{root = Root, size = Size}};
                %
                false ->
                    error
            end
    catch
        _Class:_Reason ->
            error
    end;
wrap(_) ->
    ok.

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

from_list_recur([{Key, Value} | Next], Size, Root) ->
    case b5_trees_node:insert_att(Key, eager, Value, Root) of
        none ->
            UpdatedRoot = b5_trees_node:update_att(Key, eager, Value, Root),
            from_list_recur(Next, Size, UpdatedRoot);
        %
        UpdatedRoot ->
            from_list_recur(Next, Size + 1, UpdatedRoot)
    end;
from_list_recur([], Size, Root) ->
    #b5_trees{size = Size, root = Root}.
