-module(xb5_sets_ref).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% API Exports
%% ------------------------------------------------------------------

-export([
    add/2,
    add_element/2,
    balance/1,
    del_element/2,
    delete/2,
    delete_any/2,
    difference/2,
    empty/0,
    extract_xb5/1,
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
    union/2
]).

%% ------------------------------------------------------------------
%% Macros
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Types
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% API Functions
%% ------------------------------------------------------------------

add(Elem, States) ->
    call_common2(add, Elem, States).

add_element(Elem, States) ->
    call_common2(add_element, Elem, States).

balance(States) ->
    call_all(balance, [state], States).

del_element(Elem, States) ->
    call_common2(del_element, Elem, States).

delete(Elem, States) ->
    call_common2(delete, Elem, States).

delete_any(Elem, States) ->
    call_common2(delete_any, Elem, States).

difference(LeftStates, RightStates) ->
    call_op2(difference, LeftStates, RightStates).

empty() ->
    call_all(empty, [], no_states()).

extract_xb5(States) ->
    {_, State} = lists:keyfind(xb5_sets, 1, States),
    State.

filter(Fun, States) ->
    call_common2(filter, Fun, States).

filtermap(Fun, States) ->
    call_common2(filtermap, Fun, States).

fold(Fun, Acc0, States) ->
    call_all(fold, [{v, Fun}, {v, Acc0}, state], generic, States).

from_list(List) ->
    call_all(from_list, [{v, List}], no_states()).

from_ordset(Ordset) ->
    call_all(from_ordset, [{v, Ordset}], no_states()).

insert(Elem, States) ->
    call_common2(insert, Elem, States).

intersection(List) ->
    call_op_many(intersection, List).

intersection(LeftStates, RightStates) ->
    call_op2(intersection, LeftStates, RightStates).

is_disjoint(LeftStates, RightStates) ->
    call_op2_predicate(is_disjoint, LeftStates, RightStates).

is_element(Elem, States) ->
    call_all(is_element, common2(Elem), generic, States).

is_empty(States) ->
    call_all(is_empty, [state], generic, States).

is_equal(LeftStates, RightStates) ->
    call_op2_predicate(is_equal, LeftStates, RightStates).

is_member(Elem, States) ->
    call_all(is_member, common2(Elem), generic, States).

is_set([{_, _}, {_, _}] = States) ->
    call_all(is_set, [state], generic, States);
is_set(Other) ->
    xb5_sets:is_set(Other).

is_subset(LeftStates, RightStates) ->
    call_op2_predicate(is_subset, LeftStates, RightStates).

iterator(States) ->
    call_all(iterator, [state], iterator, States).

iterator(States, Order) ->
    call_all(iterator, [state, {v, Order}], iterator, States).

iterator_from(Elem, States) ->
    call_all(iterator_from, common2(Elem), iterator, States).

iterator_from(Elem, States, Order) ->
    call_all(iterator_from, [{v, Elem}, state, {v, Order}], iterator, States).

larger(Elem, States) ->
    call_all(larger, common2(Elem), found_key_or_none, States).

largest(States) ->
    call_all(largest, [state], key, States).

map(Fun, States) ->
    call_common2(map, Fun, States).

new() ->
    call_all(new, [], no_states()).

next(Iterators) ->
    call_all(next, [state], set_iteration, Iterators).

singleton(Elem) ->
    call_all(singleton, [{v, Elem}], no_states()).

size(States) ->
    call_all(size, [state], exact, States).

smaller(Elem, States) ->
    call_all(smaller, common2(Elem), found_key_or_none, States).

smallest(States) ->
    call_all(smallest, [state], key, States).

structural_stats(States) ->
    {_, State} = lists:keyfind(xb5_sets, 1, States),
    xb5_sets:structural_stats(State).

subtract(LeftStates, RightStates) ->
    call_op2(subtract, LeftStates, RightStates).

take_largest(States) ->
    call_all(take_largest, [state], taken_key, States).

take_smallest(States) ->
    call_all(take_smallest, [state], taken_key, States).

to_list(States) ->
    call_all(to_list, [state], key_list, States).

union(LeftStates, RightStates) ->
    call_op2(union, LeftStates, RightStates).

union(List) ->
    call_op_many(union, List).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

common2(Arg1) ->
    [{v, Arg1}, state].

no_states() ->
    [{gb_sets, undefined}, {xb5_sets, undefined}].

call_common2(Function, Arg1, States) ->
    call_all(Function, [{v, Arg1}, state], States).

call_op2(Function, LeftStates, RightStates) ->
    call_all(Function, [{left_state, LeftStates}, state], RightStates).

call_op2_predicate(Function, LeftStates, RightStates) ->
    call_all(Function, [{left_state, LeftStates}, state], generic, RightStates).

call_op_many(Function, List) ->
    ArgsTemplate = [
        {subtemplate, lists:map(fun(LeftStates) -> {left_state, LeftStates} end, List)}
    ],
    call_all(Function, ArgsTemplate, no_states()).

call_all(Function, ArgsTemplate, States) ->
    call_all(Function, ArgsTemplate, state, States).

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
                iterator ->
                    [{ModA, ReturnA}, {ModB, ReturnB}];
                %
                set_iteration ->
                    case ReturnA of
                        {KeyA, IteratorA} ->
                            {KeyB, IteratorB} = ReturnB,
                            ?assertMatch(_ when KeyB == KeyA, {KeyA, KeyB}),
                            UpdatedIterators = [{ModA, IteratorA}, {ModB, IteratorB}],
                            {KeyB, UpdatedIterators};
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
                found_key_or_none ->
                    case ReturnA of
                        {found, KeyA} ->
                            {found, KeyB} = ReturnB,
                            ?assertMatch(_ when KeyB == KeyA, {KeyA, KeyB}),
                            {found, KeyB};
                        %
                        none ->
                            ?assertEqual(none, ReturnB),
                            none
                    end;
                %
                taken_key ->
                    {KeyA, UpdatedStateA} = ReturnA,
                    {KeyB, UpdatedStateB} = ReturnB,
                    ?assertMatch(_ when KeyB == KeyA, {KeyA, KeyB}),
                    {KeyB, assert_states_equivalent(ModA, UpdatedStateA, ModB, UpdatedStateB)}
            end
    catch
        error:ReasonB:StacktraceB ->
            {'EXIT', {ReasonA, _}} = catch (apply(ModA, Function, ArgsB)),
            assert_error_reasons_equivalent(ReasonB, ReasonA),
            erlang:raise(error, ReasonB, StacktraceB)
    end.

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

assert_states_equivalent(ModA, StateA, ModB, StateB) ->
    ?assertEqual(ModA:size(StateA), ModB:size(StateB)),
    ListA = ModA:to_list(StateA),
    ListB = ModB:to_list(StateB),
    _ = assert_key_lists_equivalent(ListA, ListB),
    [{ModA, StateA}, {ModB, StateB}].

assert_key_lists_equivalent([KeyA | NextA], [KeyB | NextB]) ->
    assert_keys_equivalent(KeyA, KeyB),
    assert_key_lists_equivalent(NextA, NextB);
assert_key_lists_equivalent([], []) ->
    ok.

assert_keys_equivalent(KeyA, KeyB) when KeyB == KeyA ->
    ok.

assert_error_reasons_equivalent(ReasonA, ReasonB) when ReasonB =:= ReasonA ->
    ok;
assert_error_reasons_equivalent({badkey, _}, function_clause) ->
    ok;
assert_error_reasons_equivalent({key_exists, _}, function_clause) ->
    ok;
assert_error_reasons_equivalent(empty_set, function_clause) ->
    ok.
