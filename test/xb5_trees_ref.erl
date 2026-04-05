-module(xb5_trees_ref).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% API Exports
%% ------------------------------------------------------------------

-export([
    balance/1,
    delete/2,
    delete_any/2,
    empty/0,
    enter/3,
    extract_xb5/1,
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
    update/3,
    update_with/3,
    update_with/4,
    values/1
]).

%% ------------------------------------------------------------------
%% Macros
%% ------------------------------------------------------------------

-define(COMPARE_WITH_STDLIB, (?OTP_RELEASE >= 28)).

%% ------------------------------------------------------------------
%% API Functions
%% ------------------------------------------------------------------

balance(States) ->
    call_all(balance, [state], States).

delete(Key, States) ->
    call_common2(delete, Key, States).

delete_any(Key, States) ->
    call_common2(delete_any, Key, States).

empty() ->
    call_all(empty, [], no_states()).

enter(Key, Value, States) ->
    call_common3(enter, Key, Value, States).

extract_xb5(States) ->
    {_, State} = lists:keyfind(xb5_trees, 1, States),
    State.

foldl(Fun, Acc0, States) ->
    {_, State} = lists:keyfind(xb5_trees, 1, States),
    xb5_trees:foldl(Fun, Acc0, State).

foldr(Fun, Acc0, States) ->
    {_, State} = lists:keyfind(xb5_trees, 1, States),
    xb5_trees:foldr(Fun, Acc0, State).

%%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
from_list(List) ->
    Gb = lists:foldl(fun({K, V}, Acc) -> gb_trees:enter(K, V, Acc) end, gb_trees:empty(), List),
    Xb5 = xb5_trees:from_list(List),
    assert_states_equivalent(gb_trees, Gb, xb5_trees, Xb5).

-else.
from_list(List) ->
    Xb5 = xb5_trees:from_list(List),
    [{xb5_trees, Xb5}].

-endif.

%%%%%%%%%%%%%%%

from_orddict(Orddict) ->
    call_all(from_orddict, [{v, Orddict}], no_states()).

get(Key, States) ->
    call_all(get, common2(Key), generic, States).

insert(Key, Value, States) ->
    call_common3(insert, Key, Value, States).

%%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
insert_with(Key, Fun, [{gb_trees, Gb}, {xb5_trees, Xb5}]) ->
    try xb5_trees:insert_with(Key, Fun, Xb5) of
        UpdatedXb5 ->
            Value = Fun(),
            UpdatedGb = gb_trees:insert(Key, Value, Gb),
            assert_states_equivalent(gb_trees, UpdatedGb, xb5_trees, UpdatedXb5)
    catch
        error:{key_exists, K}:Stacktrace when K =:= Key ->
            ?assertEqual(true, gb_trees:is_defined(Key, Gb)),
            erlang:raise(error, {key_exists, K}, Stacktrace)
    end.

-else.
insert_with(Key, Fun, [{xb5_trees, Xb5}]) ->
    UpdatedXb5 = xb5_trees:insert_with(Key, Fun, Xb5),
    [{xb5_trees, UpdatedXb5}].

-endif.

%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
intersect([{gb_trees, GbA}, {xb5_trees, Xb5A}], [{gb_trees, GbB}, {xb5_trees, Xb5B}]) ->
    NewXb5 = xb5_trees:intersect(Xb5A, Xb5B),
    %
    KeysA = ordsets:from_list(gb_trees:keys(GbA)),
    KeysB = ordsets:from_list(gb_trees:keys(GbB)),
    KeysToDelete = ordsets:subtract(KeysA, KeysB),
    %
    MergedGb = lists:foldl(
        fun({K, V}, Acc) ->
            try
                gb_trees:update(K, V, Acc)
            catch
                error:function_clause ->
                    Acc
            end
        end,
        GbA,
        gb_trees:to_list(GbB)
    ),
    %
    NewGb = lists:foldl(fun gb_trees:delete/2, MergedGb, KeysToDelete),
    %
    assert_states_equivalent(gb_trees, NewGb, xb5_trees, NewXb5).

-else.
intersect([{xb5_trees, Xb5A}], [{xb5_trees, Xb5B}]) ->
    NewXb5 = xb5_trees:intersect(Xb5A, Xb5B),
    [{xb5_trees, NewXb5}].

-endif.

%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
intersect_with(Fun, [{gb_trees, GbA}, {xb5_trees, Xb5A}], [{gb_trees, GbB}, {xb5_trees, Xb5B}]) ->
    NewXb5 = xb5_trees:intersect_with(Fun, Xb5A, Xb5B),
    %
    KeysA = ordsets:from_list(gb_trees:keys(GbA)),
    KeysB = ordsets:from_list(gb_trees:keys(GbB)),
    KeysToDelete = ordsets:subtract(KeysA, KeysB),
    %
    MergedGb = lists:foldl(
        fun({K, V2}, Acc) ->
            try gb_trees:get(K, Acc) of
                V1 ->
                    MergedV = Fun(K, V1, V2),
                    gb_trees:update(K, MergedV, Acc)
            catch
                error:function_clause ->
                    Acc
            end
        end,
        GbA,
        gb_trees:to_list(GbB)
    ),
    %
    NewGb = lists:foldl(fun gb_trees:delete/2, MergedGb, KeysToDelete),
    %
    assert_states_equivalent(gb_trees, NewGb, xb5_trees, NewXb5).

-else.
intersect_with(Fun, [{xb5_trees, Xb5A}], [{xb5_trees, Xb5B}]) ->
    NewXb5 = xb5_trees:intersect_with(Fun, Xb5A, Xb5B),
    [{xb5_trees, NewXb5}].

-endif.

%%%%%%%%%%%%%%

is_defined(Key, States) ->
    call_all(is_defined, common2(Key), generic, States).

is_empty(States) ->
    call_all(is_empty, [state], generic, States).

%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
is_equal([{gb_trees, GbA}, {xb5_trees, Xb5A}], [{gb_trees, GbB}, {xb5_trees, Xb5B}]) ->
    Result = xb5_trees:is_equal(Xb5A, Xb5B),

    ExpectedResult =
        (gb_trees:size(GbA) =:= gb_trees:size(GbB) andalso
            (lists:keymap(fun xb5_test_utils:canon_key/1, 1, gb_trees:to_list(GbA)) =:=
                lists:keymap(fun xb5_test_utils:canon_key/1, 1, gb_trees:to_list(GbB)))),

    ?assertEqual(ExpectedResult, Result),
    Result.

-else.
is_equal([{xb5_trees, Xb5A}], [{xb5_trees, Xb5B}]) ->
    xb5_trees:is_equal(Xb5A, Xb5B).

-endif.

%%%%%%%%%%%%%%

iterator(States) ->
    call_all(iterator, [state], iterator, States).

iterator(States, Order) ->
    call_all(iterator, [state, {v, Order}], iterator, States).

iterator_from(Key, States) ->
    call_all(iterator_from, common2(Key), iterator, States).

iterator_from(Key, States, Order) ->
    call_all(iterator_from, [{v, Key}, state, {v, Order}], iterator, States).

keys(States) ->
    call_all(keys, [state], key_list, States).

larger(Key, States) ->
    call_all(larger, common2(Key), found_kv_or_none, States).

largest(States) ->
    call_all(largest, [state], found_kv_or_none, States).

lookup(Key, States) ->
    call_all(lookup, common2(Key), generic, States).

map(Fun, States) ->
    call_common2(map, Fun, States).

%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
merge([{gb_trees, GbA}, {xb5_trees, Xb5A}], [{gb_trees, GbB}, {xb5_trees, Xb5B}]) ->
    NewXb5 = xb5_trees:merge(Xb5A, Xb5B),
    %
    NewGb = lists:foldl(
        fun({K, V}, Acc) ->
            gb_trees:enter(K, V, Acc)
        end,
        GbA,
        gb_trees:to_list(GbB)
    ),
    %
    assert_states_equivalent(gb_trees, NewGb, xb5_trees, NewXb5).

-else.
merge([{xb5_trees, Xb5A}], [{xb5_trees, Xb5B}]) ->
    NewXb5 = xb5_trees:merge(Xb5A, Xb5B),
    [{xb5_trees, NewXb5}].

-endif.

%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
merge_with(Fun, [{gb_trees, GbA}, {xb5_trees, Xb5A}], [{gb_trees, GbB}, {xb5_trees, Xb5B}]) ->
    NewXb5 = xb5_trees:merge_with(Fun, Xb5A, Xb5B),
    %
    NewGb = lists:foldl(
        fun({K, V2}, Acc) ->
            try gb_trees:get(K, Acc) of
                V1 ->
                    MergedV = Fun(K, V1, V2),
                    gb_trees:update(K, MergedV, Acc)
            catch
                error:function_clause ->
                    gb_trees:insert(K, V2, Acc)
            end
        end,
        GbA,
        gb_trees:to_list(GbB)
    ),
    %
    assert_states_equivalent(gb_trees, NewGb, xb5_trees, NewXb5).

-else.
merge_with(Fun, [{xb5_trees, Xb5A}], [{xb5_trees, Xb5B}]) ->
    NewXb5 = xb5_trees:merge_with(Fun, Xb5A, Xb5B),
    [{xb5_trees, NewXb5}].

-endif.

%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
new() ->
    [
        {gb_trees, gb_trees:empty()},
        {xb5_trees, xb5_trees:new()}
    ].

-else.
new() ->
    [{xb5_trees, xb5_trees:new()}].

-endif.

%%%%%%%%%%%%%%

next(Iterators) ->
    call_all(next, [state], tree_iteration, Iterators).

size(States) ->
    call_all(size, [state], exact, States).

smaller(Key, States) ->
    call_all(smaller, common2(Key), found_kv_or_none, States).

smallest(States) ->
    call_all(smallest, [state], found_kv_or_none, States).

structural_stats(States) ->
    {_, State} = lists:keyfind(xb5_trees, 1, States),
    xb5_trees:structural_stats(State).

take(Key, States) ->
    call_all(take, common2(Key), taken_value, States).

take_any(Key, States) ->
    call_all(take_any, common2(Key), taken_value, States).

take_largest(States) ->
    call_all(take_largest, [state], taken_key_value, States).

take_smallest(States) ->
    call_all(take_smallest, [state], taken_key_value, States).

to_list(States) ->
    call_all(to_list, [state], kv_list, States).

update(Key, Value, States) ->
    call_common3(update, Key, Value, States).

%%%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
update_with(Key, Fun, [{gb_trees, Gb}, {xb5_trees, Xb5}]) ->
    try xb5_trees:update_with(Key, Fun, Xb5) of
        UpdatedXb5 ->
            PrevValue = gb_trees:get(Key, Gb),
            Value = Fun(PrevValue),
            UpdatedGb = gb_trees:update(Key, Value, Gb),
            assert_states_equivalent(gb_trees, UpdatedGb, xb5_trees, UpdatedXb5)
    catch
        error:{badkey, K}:Stacktrace when K =:= Key ->
            ?assertEqual(false, gb_trees:is_defined(Key, Gb)),
            erlang:raise(error, {badkey, K}, Stacktrace)
    end.

-else.
update_with(Key, Fun, [{xb5_trees, Xb5}]) ->
    UpdatedXb5 = xb5_trees:update_with(Key, Fun, Xb5),
    [{xb5_trees, UpdatedXb5}].

-endif.

%%%%%%%%%%%%%%%%

-if(?COMPARE_WITH_STDLIB).
update_with(Key, Fun, Init, [{gb_trees, Gb}, {xb5_trees, Xb5}]) ->
    UpdatedXb5 = xb5_trees:update_with(Key, Fun, Init, Xb5),

    case gb_trees:is_defined(Key, Gb) of
        true ->
            PrevValue = gb_trees:get(Key, Gb),
            Value = Fun(PrevValue),
            UpdatedGb = gb_trees:update(Key, Value, Gb),
            assert_states_equivalent(gb_trees, UpdatedGb, xb5_trees, UpdatedXb5);
        %
        false ->
            UpdatedGb = gb_trees:insert(Key, Init, Gb),
            assert_states_equivalent(gb_trees, UpdatedGb, xb5_trees, UpdatedXb5)
    end.

-else.
update_with(Key, Fun, Init, [{xb5_trees, Xb5}]) ->
    UpdatedXb5 = xb5_trees:update_with(Key, Fun, Init, Xb5),
    [{xb5_trees, UpdatedXb5}].

-endif.

%%%%%%%%%%%%%%%%

values(States) ->
    call_all(values, [state], value_list, States).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

common2(Arg1) ->
    [{v, Arg1}, state].

%%%

-if(?COMPARE_WITH_STDLIB).
no_states() ->
    [{gb_trees, undefined}, {xb5_trees, undefined}].

-else.
no_states() ->
    [{xb5_trees, undefined}].

-endif.

%%%

call_common2(Function, Arg1, States) ->
    call_all(Function, [{v, Arg1}, state], States).

call_common3(Function, Arg1, Arg2, States) ->
    call_all(Function, [{v, Arg1}, {v, Arg2}, state], States).

call_all(Function, ArgsTemplate, States) ->
    call_all(Function, ArgsTemplate, state, States).

-if(not ?COMPARE_WITH_STDLIB).
call_all(Function, ArgsTemplate, ReturnTemplate, [{ModB, StateB}]) ->
    ArgsB = concrete_args(ArgsTemplate, ModB, StateB),
    ReturnB = apply(ModB, Function, ArgsB),

    case ReturnTemplate of
        _ when ReturnTemplate =:= state; ReturnTemplate =:= iterator ->
            [{ModB, ReturnB}];
        %
        _ when ReturnTemplate =:= tree_iteration; ReturnTemplate =:= taken_key_value ->
            case ReturnB of
                {KeyB, ValueB, UpdatedStateB} ->
                    {KeyB, ValueB, [{ModB, UpdatedStateB}]};
                %
                none ->
                    none
            end;
        %
        _ when ReturnTemplate =:= taken_value ->
            case ReturnB of
                {ValueB, UpdatedStateB} ->
                    {ValueB, [{ModB, UpdatedStateB}]};
                %
                error ->
                    error
            end;
        %
        _ ->
            ReturnB
    end.

-else.
call_all(Function, ArgsTemplate, ReturnTemplate, [{ModA, StateA}, {ModB, StateB}]) ->
    ArgsA = concrete_args(ArgsTemplate, ModA, StateA),
    ArgsB = concrete_args(ArgsTemplate, ModB, StateB),

    try apply(ModB, Function, ArgsB) of
        ReturnB ->
            ReturnA = apply(ModA, Function, ArgsA),

            case ReturnTemplate of
                state ->
                    assert_states_equivalent(ModA, ReturnA, ModB, ReturnB);
                %
                _ when
                    ReturnTemplate =:= generic; ReturnTemplate =:= key; ReturnTemplate =:= key_list
                ->
                    ?assertMatch(_ when ReturnB == ReturnA, {ReturnA, ReturnB}),
                    ReturnB;
                %
                value_list ->
                    ?assertMatch(_ when ReturnB =:= ReturnA, {ReturnA, ReturnB}),
                    ReturnB;
                %
                kv_list ->
                    ?assertEqual(
                        lists:keymap(fun xb5_test_utils:canon_key/1, 1, ReturnA),
                        lists:keymap(fun xb5_test_utils:canon_key/1, 1, ReturnB)
                    ),
                    ReturnB;
                %
                iterator ->
                    [{ModA, ReturnA}, {ModB, ReturnB}];
                %
                tree_iteration ->
                    case ReturnA of
                        {KeyA, ValueA, IteratorA} ->
                            {KeyB, ValueB, IteratorB} = ReturnB,
                            ?assertMatch(_ when KeyB == KeyA, {KeyA, KeyB}),
                            ?assertMatch(_ when ValueB == ValueA, {ValueA, ValueB}),
                            UpdatedIterators = [{ModA, IteratorA}, {ModB, IteratorB}],
                            {KeyB, ValueB, UpdatedIterators};
                        %
                        none ->
                            ?assertEqual(none, ReturnB),
                            none
                    end;
                %
                exact ->
                    ?assertEqual(ReturnA, ReturnB),
                    ReturnB;
                %
                found_kv_or_none ->
                    case ReturnA of
                        {KeyA, ValueA} ->
                            {KeyB, ValueB} = ReturnB,
                            ?assertMatch(_ when KeyB == KeyA, {KeyA, KeyB}),
                            ?assertMatch(_ when ValueB =:= ValueA, {ValueA, ValueB}),
                            {KeyB, ValueB};
                        %
                        none ->
                            ?assertEqual(none, ReturnB),
                            none
                    end;
                %
                taken_key_value ->
                    {KeyA, ValueA, UpdatedStateA} = ReturnA,
                    {KeyB, ValueB, UpdatedStateB} = ReturnB,
                    ?assertMatch(_ when KeyB == KeyA, {KeyA, KeyB}),
                    ?assertMatch(_ when ValueB =:= ValueA, {ValueA, ValueB}),
                    {KeyB, ValueB,
                        assert_states_equivalent(ModA, UpdatedStateA, ModB, UpdatedStateB)};
                %
                taken_value ->
                    case ReturnA of
                        {ValueA, UpdatedStateA} ->
                            {ValueB, UpdatedStateB} = ReturnB,
                            ?assertMatch(_ when ValueB =:= ValueA, {ValueA, ValueB}),
                            {ValueB,
                                assert_states_equivalent(ModA, UpdatedStateA, ModB, UpdatedStateB)};
                        %
                        error ->
                            ?assertEqual(error, ReturnB),
                            error
                    end
            end
    catch
        error:ReasonB:StacktraceB ->
            {'EXIT', {ReasonA, _}} = catch (apply(ModA, Function, ArgsB)),
            assert_error_reasons_equivalent(ReasonB, ReasonA),
            erlang:raise(error, ReasonB, StacktraceB)
    end.
-endif.

concrete_args(Template, Mod, State) ->
    lists:map(
        fun
            ({v, V}) ->
                V;
            %
            (state) ->
                State;
            %
            ({left_state, LeftStates}) ->
                {_, LeftState} = lists:keyfind(Mod, 1, LeftStates),
                LeftState;
            %
            ({subtemplate, SubTemplate}) ->
                concrete_args(SubTemplate, Mod, State)
        end,
        Template
    ).

%%%

-if(?COMPARE_WITH_STDLIB).

assert_states_equivalent(ModA, StateA, ModB, StateB) ->
    ?assertEqual(ModA:size(StateA), ModB:size(StateB)),
    ListA = ModA:to_list(StateA),
    ListB = ModB:to_list(StateB),

    ?assertEqual(
        lists:keymap(fun xb5_test_utils:canon_key/1, 1, ListA),
        lists:keymap(fun xb5_test_utils:canon_key/1, 1, ListB)
    ),

    [{ModA, StateA}, {ModB, StateB}].

assert_error_reasons_equivalent(ReasonA, ReasonB) when ReasonB =:= ReasonA ->
    ok;
assert_error_reasons_equivalent({badkey, _}, function_clause) ->
    ok;
assert_error_reasons_equivalent({key_exists, _}, function_clause) ->
    ok;
assert_error_reasons_equivalent(empty_tree, function_clause) ->
    ok.

-endif.
