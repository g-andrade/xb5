-module(b5_leaves).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    get_root/2,
    get/2,
    insert_root/4,
    insert/4,
    update_root/4,
    update/4
]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

% 0 elements
-define(LEAF0, leaf0).

-define(LEAF0_MATCH, leaf0).

% 2 elements
-define(LEAF1(K1, V1), [K1 | V1]).

-define(LEAF1_MATCH(K1, V1), [K1 | V1]).

% 4 elements
-define(LEAF2(K1, K2, V1, V2), {K1, K2, V1, V2}).

-define(LEAF2_MATCH(K1, K2, V1, V2), {K1, K2, V1, V2}).

% 6 elements
-define(LEAF3(K1, K2, K3, V1, V2, V3), {K1, K2, K3, V1, V2, V3}).

-define(LEAF3_MATCH(K1, K2, K3, V1, V2, V3), {K1, K2, K3, V1, V2, V3}).

% 8 elements
-define(LEAF4(K1, K2, K3, K4, V1, V2, V3, V4), {K1, K2, K3, K4, V1, V2, V3, V4}).

-define(LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4), {K1, K2, K3, K4, V1, V2, V3, V4}).

% 10 elements
-define(LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5), {K1, K2, K3, K4, K5, V1, V2, V3, V4, V5}).

-define(LEAF5_MATCH(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5),
    {K1, K2, K3, K4, K5, V1, V2, V3, V4, V5}
).

% 12 elements
-define(LEAF6(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6),
    {K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6}
).

-define(LEAF6_MATCH(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6),
    {K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6}
).

% 14 elements
-define(LEAF7(K1, K2, K3, K4, K5, K6, K7, V1, V2, V3, V4, V5, V6, V7),
    {K1, K2, K3, K4, K5, K6, K7, V1, V2, V3, V4, V5, V6, V7}
).

-define(LEAF7_MATCH(K1, K2, K3, K4, K5, K6, K7, V1, V2, V3, V4, V5, V6, V7),
    {K1, K2, K3, K4, K5, K6, K7, V1, V2, V3, V4, V5, V6, V7}
).

% 16 elements
-define(LEAF8(K1, K2, K3, K4, K5, K6, K7, K8, V1, V2, V3, V4, V5, V6, V7, V8),
    {K1, K2, K3, K4, K5, K6, K7, K8, V1, V2, V3, V4, V5, V6, V7, V8}
).

-define(LEAF8_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, V1, V2, V3, V4, V5, V6, V7, V8),
    {K1, K2, K3, K4, K5, K6, K7, K8, V1, V2, V3, V4, V5, V6, V7, V8}
).

% 18 elements
-define(LEAF9(K1, K2, K3, K4, K5, K6, K7, K8, K9, V1, V2, V3, V4, V5, V6, V7, V8, V9),
    {K1, K2, K3, K4, K5, K6, K7, K8, K9, V1, V2, V3, V4, V5, V6, V7, V8, V9}
).

-define(LEAF9_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, K9, V1, V2, V3, V4, V5, V6, V7, V8, V9),
    {K1, K2, K3, K4, K5, K6, K7, K8, K9, V1, V2, V3, V4, V5, V6, V7, V8, V9}
).

% 20 elements
-define(LEAF10(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10),
    {K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}
).

-define(LEAF10_MATCH(
    K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10
),
    {K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}
).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_root(Key, ?LEAF0_MATCH) ->
    error({badkey, Key});
get_root(Key, ?LEAF1_MATCH(K1, V1)) ->
    if
        Key == K1 ->
            V1;
        %
        true ->
            error({badkey, Key})
    end;
get_root(Key, ?LEAF2_MATCH(K1, K2, V1, V2)) ->
    if
        Key == K1 ->
            V1;
        %
        Key == K2 ->
            V2;
        %
        true ->
            error({badkey, Key})
    end;
get_root(Key, ?LEAF3_MATCH(K1, K2, K3, V1, V2, V3)) ->
    if
        Key < K2 ->
            if
                Key == K1 ->
                    V1;
                %
                true ->
                    error({badkey, Key})
            end;
        Key > K2 ->
            if
                Key == K3 ->
                    V3;
                %
                true ->
                    error({badkey, Key})
            end;
        true ->
            V2
    end;
get_root(Key, ?LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    if
        Key < K3 ->
            if
                Key == K1 ->
                    V1;
                %
                Key == K2 ->
                    V2;
                %
                true ->
                    error({badkey, Key})
            end;
        Key > K3 ->
            if
                Key == K4 ->
                    V4;
                %
                true ->
                    error({badkey, Key})
            end;
        true ->
            V3
    end;
get_root(Key, Node) ->
    get(Key, Node).

get(Key, ?LEAF5_MATCH(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5)) ->
    if
        Key < K3 ->
            if
                Key == K1 ->
                    V1;
                %
                Key == K2 ->
                    V2;
                %
                true ->
                    error({badkey, Key})
            end;
        Key > K3 ->
            if
                Key == K4 ->
                    V4;
                %
                Key == K5 ->
                    V5;
                %
                true ->
                    error({badkey, Key})
            end;
        true ->
            V3
    end;
get(Key, ?LEAF6_MATCH(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6)) ->
    if
        Key < K4 ->
            if
                Key < K2 ->
                    if
                        Key == K1 ->
                            V1;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K2 ->
                    if
                        Key == K3 ->
                            V3;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    V2
            end;
        Key > K4 ->
            if
                Key == K5 ->
                    V5;
                %
                Key == K6 ->
                    V6;
                %
                true ->
                    error({badkey, Key})
            end;
        true ->
            V4
    end;
get(Key, ?LEAF7_MATCH(K1, K2, K3, K4, K5, K6, K7, V1, V2, V3, V4, V5, V6, V7)) ->
    if
        Key < K4 ->
            if
                Key < K2 ->
                    if
                        Key == K1 ->
                            V1;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K2 ->
                    if
                        Key == K3 ->
                            V3;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    V2
            end;
        Key > K4 ->
            if
                Key < K6 ->
                    if
                        Key == K5 ->
                            V5;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K6 ->
                    if
                        Key == K7 ->
                            V7;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    V6
            end;
        true ->
            V4
    end;
get(Key, ?LEAF8_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, V1, V2, V3, V4, V5, V6, V7, V8)) ->
    if
        Key < K5 ->
            if
                Key < K3 ->
                    if
                        Key == K1 ->
                            V1;
                        %
                        Key == K2 ->
                            V2;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K3 ->
                    if
                        Key == K4 ->
                            V4;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    V3
            end;
        Key > K5 ->
            if
                Key < K7 ->
                    if
                        Key == K6 ->
                            V6;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K7 ->
                    if
                        Key == K8 ->
                            V8;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    V7
            end;
        true ->
            V5
    end;
get(Key, ?LEAF9_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, K9, V1, V2, V3, V4, V5, V6, V7, V8, V9)) ->
    if
        Key < K5 ->
            if
                Key < K3 ->
                    if
                        Key == K1 ->
                            V1;
                        %
                        Key == K2 ->
                            V2;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K3 ->
                    if
                        Key == K4 ->
                            V4;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    V3
            end;
        Key > K5 ->
            if
                Key < K8 ->
                    if
                        Key == K6 ->
                            V6;
                        %
                        Key == K7 ->
                            V7;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K8 ->
                    if
                        Key == K9 ->
                            V9;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    V8
            end;
        true ->
            V5
    end;
get(
    Key,
    ?LEAF10_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10)
) ->
    if
        Key < K6 ->
            if
                Key < K3 ->
                    if
                        Key == K1 ->
                            V1;
                        %
                        Key == K2 ->
                            V2;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K3 ->
                    if
                        Key == K4 ->
                            V4;
                        %
                        Key == K5 ->
                            V5;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    V3
            end;
        Key > K6 ->
            if
                Key < K9 ->
                    if
                        Key == K7 ->
                            V7;
                        %
                        Key == K8 ->
                            V8;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K9 ->
                    if
                        Key == K10 ->
                            V10;
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    V9
            end;
        true ->
            V6
    end.

insert_root(Key, ValueEval, ValueWrap, ?LEAF0_MATCH) ->
    Value = eval_insert_value(ValueEval, ValueWrap),
    ?LEAF1(Key, Value);
insert_root(Key, ValueEval, ValueWrap, ?LEAF1_MATCH(K1, V1)) ->
    if
        Key < K1 ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?LEAF2(Key, K1, Value, V1);
        %
        Key > K1 ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?LEAF2(K1, Key, V1, Value);
        %
        true ->
            error({key_exists, Key})
    end;
insert_root(Key, ValueEval, ValueWrap, ?LEAF2_MATCH(K1, K2, V1, V2)) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF3(Key, K1, K2, Value, V1, V2);
                %
                Key > K1 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF3(K1, Key, K2, V1, Value, V2);
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        Key > K2 ->
            Value = eval_insert_value(ValueEval, ValueWrap),
            ?LEAF3(Key, K1, K2, Value, V1, V2);
        %
        true ->
            error({key_exists, Key})
    end;
insert_root(Key, ValueEval, ValueWrap, ?LEAF3_MATCH(K1, K2, K3, V1, V2, V3)) ->
    if
        Key < K2 ->
            if
                Key < K1 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF4(Key, K1, K2, K3, Value, V1, V2, V3);
                %
                Key > K1 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF4(K1, Key, K2, K3, V1, Value, V2, V3);
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        Key > K2 ->
            if
                Key < K3 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF4(K1, K2, Key, K3, V1, V2, Value, V3);
                %
                Key > K3 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF4(K1, K2, K3, Key, V1, V2, V3, Value);
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        true ->
            error({key_exists, Key})
    end;
insert_root(Key, ValueEval, ValueWrap, ?LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    if
        Key < K3 ->
            if
                Key < K2 ->
                    if
                        Key < K1 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF5(Key, K1, K2, K3, K4, Value, V1, V2, V3, V4);
                        %
                        Key > K1 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF5(K1, Key, K2, K3, K4, V1, Value, V2, V3, V4);
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K2 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF5(Key, K1, K2, K3, K4, Value, V1, V2, V3, V4);
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        Key > K3 ->
            if
                Key < K4 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF5(K1, K2, K3, Key, K4, V1, V2, V3, Value, V4);
                %
                Key > K4 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF5(K1, K2, K3, K4, Key, V1, V2, V3, V4, Value);
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        true ->
            error({key_exists, Key})
    end;
insert_root(Key, ValueEval, ValueWrap, Node) ->
    insert(Key, ValueEval, ValueWrap, Node).

insert(Key, ValueEval, ValueWrap, ?LEAF5_MATCH(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5)) ->
    if
        Key < K3 ->
            if
                Key < K2 ->
                    if
                        Key < K1 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF6(Key, K1, K2, K3, K4, K5, Value, V1, V2, V3, V4, V5);
                        %
                        Key > K1 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF6(K1, Key, K2, K3, K4, K5, V1, Value, V2, V3, V4, V5);
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K2 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF6(Key, K1, K2, K3, K4, K5, Value, V1, V2, V3, V4, V5);
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        Key > K3 ->
            if
                Key < K5 ->
                    if
                        Key < K4 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF6(K1, K2, K3, Key, K4, K5, V1, V2, V3, Value, V4, V5);
                        %
                        Key > K4 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF6(K1, K2, K3, K4, Key, K5, V1, V2, V3, V4, Value, V5);
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K5 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF6(Key, K1, K2, K3, K4, K5, Value, V1, V2, V3, V4, V5);
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        true ->
            error({key_exists, Key})
    end;
insert(Key, ValueEval, ValueWrap, ?LEAF6_MATCH(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6)) ->
    if
        Key < K4 ->
            if
                Key < K2 ->
                    if
                        Key < K1 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF7(Key, K1, K2, K3, K4, K5, K6, Value, V1, V2, V3, V4, V5, V6);
                        %
                        Key > K1 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF7(K1, Key, K2, K3, K4, K5, K6, V1, Value, V2, V3, V4, V5, V6);
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K2 ->
                    if
                        Key < K3 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF7(K1, K2, Key, K3, K4, K5, K6, V1, V2, Value, V3, V4, V5, V6);
                        %
                        Key > K3 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF7(K1, K2, K3, Key, K4, K5, K6, V1, V2, V3, Value, V4, V5, V6);
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        Key > K4 ->
            if
                Key < K6 ->
                    if
                        Key < K5 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF7(K1, K2, K3, K4, Key, K5, K6, V1, V2, V3, V4, Value, V5, V6);
                        %
                        Key > K5 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF7(K1, K2, K3, K4, K5, Key, K6, V1, V2, V3, V4, V5, Value, V6);
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K6 ->
                    Value = eval_insert_value(ValueEval, ValueWrap),
                    ?LEAF7(Key, K1, K2, K3, K4, K5, K6, Value, V1, V2, V3, V4, V5, V6);
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        true ->
            error({key_exists, Key})
    end;
insert(
    Key, ValueEval, ValueWrap, ?LEAF7_MATCH(K1, K2, K3, K4, K5, K6, K7, V1, V2, V3, V4, V5, V6, V7)
) ->
    if
        Key < K4 ->
            if
                Key < K2 ->
                    if
                        Key < K1 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF8(
                                Key, K1, K2, K3, K4, K5, K6, K7, Value, V1, V2, V3, V4, V5, V6, V7
                            );
                        %
                        Key > K1 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF8(
                                K1, Key, K2, K3, K4, K5, K6, K7, V1, Value, V2, V3, V4, V5, V6, V7
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K2 ->
                    if
                        Key < K3 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF8(
                                K1, K2, Key, K3, K4, K5, K6, K7, V1, V2, Value, V3, V4, V5, V6, V7
                            );
                        %
                        Key > K3 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF8(
                                K1, K2, K3, Key, K4, K5, K6, K7, V1, V2, V3, Value, V4, V5, V6, V7
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        Key > K4 ->
            if
                Key < K6 ->
                    if
                        Key < K5 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF8(
                                K1, K2, K3, K4, Key, K5, K6, K7, V1, V2, V3, V4, Value, V5, V6, V7
                            );
                        %
                        Key > K5 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF8(
                                K1, K2, K3, K4, K5, Key, K6, K7, V1, V2, V3, V4, V5, Value, V6, V7
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K6 ->
                    if
                        Key < K7 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF8(
                                K1, K2, K3, K4, K5, K6, Key, K7, V1, V2, V3, V4, V5, V6, Value, V7
                            );
                        %
                        Key > K7 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF8(
                                K1, K2, K3, K4, K5, K6, K7, Key, V1, V2, V3, V4, V5, V6, V7, Value
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        true ->
            error({key_exists, Key})
    end;
insert(
    Key,
    ValueEval,
    ValueWrap,
    ?LEAF8_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, V1, V2, V3, V4, V5, V6, V7, V8)
) ->
    if
        Key < K5 ->
            if
                Key < K3 ->
                    if
                        Key < K2 ->
                            if
                                Key < K1 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    ?LEAF9(
                                        Key,
                                        K1,
                                        K2,
                                        K3,
                                        K4,
                                        K5,
                                        K6,
                                        K7,
                                        K8,
                                        Value,
                                        V1,
                                        V2,
                                        V3,
                                        V4,
                                        V5,
                                        V6,
                                        V7,
                                        V8
                                    );
                                %
                                Key > K1 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    ?LEAF9(
                                        K1,
                                        Key,
                                        K2,
                                        K3,
                                        K4,
                                        K5,
                                        K6,
                                        K7,
                                        K8,
                                        V1,
                                        Value,
                                        V2,
                                        V3,
                                        V4,
                                        V5,
                                        V6,
                                        V7,
                                        V8
                                    );
                                %
                                true ->
                                    error({key_exists, Key})
                            end;
                        %
                        Key > K2 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF9(
                                Key,
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                Value,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K3 ->
                    if
                        Key < K4 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                Key,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                V1,
                                V2,
                                V3,
                                Value,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8
                            );
                        %
                        Key > K4 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                K4,
                                Key,
                                K5,
                                K6,
                                K7,
                                K8,
                                V1,
                                V2,
                                V3,
                                V4,
                                Value,
                                V5,
                                V6,
                                V7,
                                V8
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        Key > K5 ->
            if
                Key < K7 ->
                    if
                        Key < K6 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                Key,
                                K6,
                                K7,
                                K8,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                Value,
                                V6,
                                V7,
                                V8
                            );
                        %
                        Key > K6 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                Key,
                                K7,
                                K8,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                Value,
                                V7,
                                V8
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K7 ->
                    if
                        Key < K8 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                Key,
                                K8,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                Value,
                                V8
                            );
                        %
                        Key > K8 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                Key,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                Value
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        true ->
            error({key_exists, Key})
    end;
insert(
    Key,
    ValueEval,
    ValueWrap,
    ?LEAF9_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, K9, V1, V2, V3, V4, V5, V6, V7, V8, V9)
) ->
    if
        Key < K5 ->
            if
                Key < K3 ->
                    if
                        Key < K2 ->
                            if
                                Key < K1 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    ?LEAF10(
                                        Key,
                                        K1,
                                        K2,
                                        K3,
                                        K4,
                                        K5,
                                        K6,
                                        K7,
                                        K8,
                                        K9,
                                        Value,
                                        V1,
                                        V2,
                                        V3,
                                        V4,
                                        V5,
                                        V6,
                                        V7,
                                        V8,
                                        V9
                                    );
                                %
                                Key > K1 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    ?LEAF10(
                                        K1,
                                        Key,
                                        K2,
                                        K3,
                                        K4,
                                        K5,
                                        K6,
                                        K7,
                                        K8,
                                        K9,
                                        V1,
                                        Value,
                                        V2,
                                        V3,
                                        V4,
                                        V5,
                                        V6,
                                        V7,
                                        V8,
                                        V9
                                    );
                                %
                                true ->
                                    error({key_exists, Key})
                            end;
                        %
                        Key > K2 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF10(
                                Key,
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                Value,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K3 ->
                    if
                        Key < K4 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF10(
                                K1,
                                K2,
                                K3,
                                Key,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                V1,
                                V2,
                                V3,
                                Value,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9
                            );
                        %
                        Key > K4 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF10(
                                K1,
                                K2,
                                K3,
                                K4,
                                Key,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                V1,
                                V2,
                                V3,
                                V4,
                                Value,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        Key > K5 ->
            if
                Key < K8 ->
                    if
                        Key < K7 ->
                            if
                                Key < K6 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    ?LEAF10(
                                        K1,
                                        K2,
                                        K3,
                                        K4,
                                        K5,
                                        Key,
                                        K6,
                                        K7,
                                        K8,
                                        K9,
                                        V1,
                                        V2,
                                        V3,
                                        V4,
                                        V5,
                                        Value,
                                        V6,
                                        V7,
                                        V8,
                                        V9
                                    );
                                %
                                Key > K6 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    ?LEAF10(
                                        K1,
                                        K2,
                                        K3,
                                        K4,
                                        K5,
                                        K6,
                                        Key,
                                        K7,
                                        K8,
                                        K9,
                                        V1,
                                        V2,
                                        V3,
                                        V4,
                                        V5,
                                        V6,
                                        Value,
                                        V7,
                                        V8,
                                        V9
                                    );
                                %
                                true ->
                                    error({key_exists, Key})
                            end;
                        %
                        Key > K7 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF10(
                                Key,
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                Value,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K8 ->
                    if
                        Key < K9 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF10(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                Key,
                                K9,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                Value,
                                V9
                            );
                        %
                        Key > K9 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            ?LEAF10(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                Key,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9,
                                Value
                            );
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        true ->
            error({key_exists, Key})
    end;
insert(
    Key,
    ValueEval,
    ValueWrap,
    ?LEAF10_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10)
) ->
    if
        Key < K6 ->
            if
                Key < K3 ->
                    if
                        Key < K2 ->
                            if
                                Key < K1 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    Left = ?LEAF5(Key, K1, K2, K3, K4, Value, V1, V2, V3, V4),
                                    Right = ?LEAF5(K6, K7, K8, K9, K10, V6, V7, V8, V9, V10),
                                    {split, K5, V5, Left, Right};
                                %
                                Key > K1 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    Left = ?LEAF5(K1, Key, K2, K3, K4, V1, Value, V2, V3, V4),
                                    Right = ?LEAF5(K6, K7, K8, K9, K10, V6, V7, V8, V9, V10),
                                    {split, K5, V5, Left, Right};
                                %
                                true ->
                                    error({key_exists, Key})
                            end;
                        %
                        Key > K2 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            Left = ?LEAF5(Key, K1, K2, K3, K4, Value, V1, V2, V3, V4),
                            Right = ?LEAF5(K6, K7, K8, K9, K10, V6, V7, V8, V9, V10),
                            {split, K5, V5, Left, Right};
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K3 ->
                    if
                        Key < K5 ->
                            if
                                Key < K4 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    Left = ?LEAF5(K1, K2, K3, Key, K4, V1, V2, V3, Value, V4),
                                    Right = ?LEAF5(K6, K7, K8, K9, K10, V6, V7, V8, V9, V10),
                                    {split, K5, V5, Left, Right};
                                %
                                Key > K4 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    Left = ?LEAF5(K1, K2, K3, K4, Key, V1, V2, V3, V4, Value),
                                    Right = ?LEAF5(K6, K7, K8, K9, K10, V6, V7, V8, V9, V10),
                                    {split, K5, V5, Left, Right};
                                %
                                true ->
                                    error({key_exists, Key})
                            end;
                        %
                        Key > K5 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            Left = ?LEAF5(Key, K1, K2, K3, K4, Value, V1, V2, V3, V4),
                            Right = ?LEAF5(K6, K7, K8, K9, K10, V6, V7, V8, V9, V10),
                            {split, K5, V5, Left, Right};
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        Key > K6 ->
            if
                Key < K9 ->
                    if
                        Key < K8 ->
                            if
                                Key < K7 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    Left = ?LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5),
                                    Right = ?LEAF5(Key, K7, K8, K9, K10, Value, V7, V8, V9, V10),
                                    {split, K6, V6, Left, Right};
                                %
                                Key > K7 ->
                                    Value = eval_insert_value(ValueEval, ValueWrap),
                                    Left = ?LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5),
                                    Right = ?LEAF5(K7, Key, K8, K9, K10, V7, Value, V8, V9, V10),
                                    {split, K6, V6, Left, Right};
                                %
                                true ->
                                    error({key_exists, Key})
                            end;
                        %
                        Key > K8 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            Left = ?LEAF5(Key, K1, K2, K3, K4, Value, V1, V2, V3, V4),
                            Right = ?LEAF5(K6, K7, K8, K9, K10, V6, V7, V8, V9, V10),
                            {split, K5, V5, Left, Right};
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                Key > K9 ->
                    if
                        Key < K10 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            Left = ?LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5),
                            Right = ?LEAF5(K7, K8, K9, Key, K10, V7, V8, V9, Value, V10),
                            {split, K6, V6, Left, Right};
                        %
                        Key > K10 ->
                            Value = eval_insert_value(ValueEval, ValueWrap),
                            Left = ?LEAF5(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5),
                            Right = ?LEAF5(K7, K8, K9, K10, Key, V7, V8, V9, V10, Value),
                            {split, K6, V6, Left, Right};
                        %
                        true ->
                            error({key_exists, Key})
                    end;
                %
                true ->
                    error({key_exists, Key})
            end;
        %
        true ->
            error({key_exists, Key})
    end.

update_root(Key, _ValueEval, _ValueWrap, ?LEAF0_MATCH) ->
    error({badkey, Key});
update_root(Key, ValueEval, ValueWrap, ?LEAF1_MATCH(K1, V1)) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF1(Key, Value);
        %
        true ->
            error({badkey, Key})
    end;
update_root(Key, ValueEval, ValueWrap, ?LEAF2_MATCH(K1, K2, V1, V2)) ->
    if
        Key == K1 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF2(Key, K2, Value, V2);
        %
        Key == K2 ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF2(K1, Key, V1, Value);
        %
        true ->
            error({badkey, Key})
    end;
update_root(Key, ValueEval, ValueWrap, ?LEAF3_MATCH(K1, K2, K3, V1, V2, V3)) ->
    if
        Key < K2 ->
            if
                Key == K1 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF3(Key, K2, K3, Value, V2, V3);
                %
                true ->
                    error({badkey, Key})
            end;
        Key > K2 ->
            if
                Key == K3 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF3(K1, K2, Key, V1, V2, Value);
                %
                true ->
                    error({badkey, Key})
            end;
        true ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF3(K1, Key, K3, V1, Value, V3)
    end;
update_root(Key, ValueEval, ValueWrap, ?LEAF4_MATCH(K1, K2, K3, K4, V1, V2, V3, V4)) ->
    if
        Key < K3 ->
            if
                Key == K1 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF4(Key, K2, K3, K4, Value, V2, V3, V4);
                %
                Key == K2 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF4(K1, Key, K3, K4, V1, Value, V3, V4);
                %
                true ->
                    error({badkey, Key})
            end;
        Key > K3 ->
            if
                Key == K4 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF4(K1, K2, K3, Key, V1, V2, V3, Value);
                %
                true ->
                    error({badkey, Key})
            end;
        true ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF4(K1, K2, Key, K4, V1, V2, Value, V4)
    end;
update_root(Key, ValueEval, ValueWrap, Node) ->
    update(Key, ValueEval, ValueWrap, Node).

update(Key, ValueEval, ValueWrap, ?LEAF5_MATCH(K1, K2, K3, K4, K5, V1, V2, V3, V4, V5)) ->
    if
        Key < K3 ->
            if
                Key == K1 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF5(Key, K2, K3, K4, K5, Value, V2, V3, V4, V5);
                %
                Key == K2 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF5(K1, Key, K3, K4, K5, V1, Value, V3, V4, V5);
                %
                true ->
                    error({badkey, Key})
            end;
        Key > K3 ->
            if
                Key == K4 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF5(K1, K2, K3, Key, K5, V1, V2, V3, Value, V5);
                %
                Key == K5 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF5(K1, K2, K3, K4, Key, V1, V2, V3, V4, Value);
                %
                true ->
                    error({badkey, Key})
            end;
        true ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF5(K1, K2, Key, K4, K5, V1, V2, Value, V4, V5)
    end;
update(Key, ValueEval, ValueWrap, ?LEAF6_MATCH(K1, K2, K3, K4, K5, K6, V1, V2, V3, V4, V5, V6)) ->
    if
        Key < K4 ->
            if
                Key < K2 ->
                    if
                        Key == K1 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF6(Key, K2, K3, K4, K5, K6, Value, V2, V3, V4, V5, V6);
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K2 ->
                    if
                        Key == K3 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF6(K1, K2, Key, K4, K5, K6, V1, V2, Value, V4, V5, V6);
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF6(K1, Key, K3, K4, K5, K6, V1, Value, V3, V4, V5, V6)
            end;
        Key > K4 ->
            if
                Key == K5 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF6(K1, K2, K3, K4, Key, K6, V1, V2, V3, V4, Value, V6);
                %
                Key == K6 ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF6(K1, K2, K3, K4, K5, Key, V1, V2, V3, V4, V5, Value);
                %
                true ->
                    error({badkey, Key})
            end;
        true ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF6(K1, K2, K3, Key, K5, K6, V1, V2, V3, Value, V5, V6)
    end;
update(
    Key, ValueEval, ValueWrap, ?LEAF7_MATCH(K1, K2, K3, K4, K5, K6, K7, V1, V2, V3, V4, V5, V6, V7)
) ->
    if
        Key < K4 ->
            if
                Key < K2 ->
                    if
                        Key == K1 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF7(Key, K2, K3, K4, K5, K6, K7, Value, V2, V3, V4, V5, V6, V7);
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K2 ->
                    if
                        Key == K3 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF7(K1, K2, Key, K4, K5, K6, K7, V1, V2, Value, V4, V5, V6, V7);
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF7(K1, Key, K3, K4, K5, K6, K7, V1, Value, V3, V4, V5, V6, V7)
            end;
        Key > K4 ->
            if
                Key < K6 ->
                    if
                        Key == K5 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF7(K1, K2, K3, K4, Key, K6, K7, V1, V2, V3, V4, Value, V6, V7);
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K6 ->
                    if
                        Key == K7 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF7(K1, K2, K3, K4, K5, K6, Key, V1, V2, V3, V4, V5, V6, Value);
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF7(K1, K2, K3, K4, K5, Key, K7, V1, V2, V3, V4, V5, Value, V7)
            end;
        true ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF7(K1, K2, K3, Key, K5, K6, K7, V1, V2, V3, Value, V5, V6, V7)
    end;
update(
    Key,
    ValueEval,
    ValueWrap,
    ?LEAF8_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, V1, V2, V3, V4, V5, V6, V7, V8)
) ->
    if
        Key < K5 ->
            if
                Key < K3 ->
                    if
                        Key == K1 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF8(
                                Key, K2, K3, K4, K5, K6, K7, K8, Value, V2, V3, V4, V5, V6, V7, V8
                            );
                        %
                        Key == K2 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF8(
                                K1, Key, K3, K4, K5, K6, K7, K8, V1, Value, V3, V4, V5, V6, V7, V8
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K3 ->
                    if
                        Key == K4 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF8(
                                K1, K2, K3, Key, K5, K6, K7, K8, V1, V2, V3, Value, V5, V6, V7, V8
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF8(K1, K2, Key, K4, K5, K6, K7, K8, V1, V2, Value, V4, V5, V6, V7, V8)
            end;
        Key > K5 ->
            if
                Key < K7 ->
                    if
                        Key == K6 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF8(
                                K1, K2, K3, K4, K5, Key, K7, K8, V1, V2, V3, V4, V5, Value, V7, V8
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K7 ->
                    if
                        Key == K8 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF8(
                                K1, K2, K3, K4, K5, K6, K7, Key, V1, V2, V3, V4, V5, V6, V7, Value
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF8(K1, K2, K3, K4, K5, K6, Key, K8, V1, V2, V3, V4, V5, V6, Value, V8)
            end;
        true ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF8(K1, K2, K3, K4, Key, K6, K7, K8, V1, V2, V3, V4, Value, V6, V7, V8)
    end;
update(
    Key,
    ValueEval,
    ValueWrap,
    ?LEAF9_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, K9, V1, V2, V3, V4, V5, V6, V7, V8, V9)
) ->
    if
        Key < K5 ->
            if
                Key < K3 ->
                    if
                        Key == K1 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF9(
                                Key,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                Value,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9
                            );
                        %
                        Key == K2 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF9(
                                K1,
                                Key,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                V1,
                                Value,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K3 ->
                    if
                        Key == K4 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                Key,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                V1,
                                V2,
                                V3,
                                Value,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF9(
                        K1, K2, Key, K4, K5, K6, K7, K8, K9, V1, V2, Value, V4, V5, V6, V7, V8, V9
                    )
            end;
        Key > K5 ->
            if
                Key < K8 ->
                    if
                        Key == K6 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                Key,
                                K7,
                                K8,
                                K9,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                Value,
                                V7,
                                V8,
                                V9
                            );
                        %
                        Key == K7 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                Key,
                                K8,
                                K9,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                Value,
                                V8,
                                V9
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K8 ->
                    if
                        Key == K9 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF9(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                Key,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                Value
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF9(
                        K1, K2, K3, K4, K5, K6, K7, Key, K9, V1, V2, V3, V4, V5, V6, V7, Value, V9
                    )
            end;
        true ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF9(K1, K2, K3, K4, Key, K6, K7, K8, K9, V1, V2, V3, V4, Value, V6, V7, V8, V9)
    end;
update(
    Key,
    ValueEval,
    ValueWrap,
    ?LEAF10_MATCH(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10)
) ->
    if
        Key < K6 ->
            if
                Key < K3 ->
                    if
                        Key == K1 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF10(
                                Key,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                K10,
                                Value,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9,
                                V10
                            );
                        %
                        Key == K2 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF10(
                                K1,
                                Key,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                K10,
                                V1,
                                Value,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9,
                                V10
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K3 ->
                    if
                        Key == K4 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF10(
                                K1,
                                K2,
                                K3,
                                Key,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                K10,
                                V1,
                                V2,
                                V3,
                                Value,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9,
                                V10
                            );
                        %
                        Key == K5 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF10(
                                K1,
                                K2,
                                K3,
                                K4,
                                Key,
                                K6,
                                K7,
                                K8,
                                K9,
                                K10,
                                V1,
                                V2,
                                V3,
                                V4,
                                Value,
                                V6,
                                V7,
                                V8,
                                V9,
                                V10
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF10(
                        K1,
                        K2,
                        Key,
                        K4,
                        K5,
                        K6,
                        K7,
                        K8,
                        K9,
                        K10,
                        V1,
                        V2,
                        Value,
                        V4,
                        V5,
                        V6,
                        V7,
                        V8,
                        V9,
                        V10
                    )
            end;
        Key > K6 ->
            if
                Key < K9 ->
                    if
                        Key == K7 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF10(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                Key,
                                K8,
                                K9,
                                K10,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                Value,
                                V8,
                                V9,
                                V10
                            );
                        %
                        Key == K8 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF10(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                Key,
                                K9,
                                K10,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                Value,
                                V9,
                                V10
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                Key > K9 ->
                    if
                        Key == K10 ->
                            Value = eval_update_value(ValueEval, ValueWrap, V1),
                            ?LEAF10(
                                K1,
                                K2,
                                K3,
                                K4,
                                K5,
                                K6,
                                K7,
                                K8,
                                K9,
                                Key,
                                V1,
                                V2,
                                V3,
                                V4,
                                V5,
                                V6,
                                V7,
                                V8,
                                V9,
                                Value
                            );
                        %
                        true ->
                            error({badkey, Key})
                    end;
                true ->
                    Value = eval_update_value(ValueEval, ValueWrap, V1),
                    ?LEAF10(
                        K1,
                        K2,
                        K3,
                        K4,
                        K5,
                        K6,
                        K7,
                        K8,
                        Key,
                        K10,
                        V1,
                        V2,
                        V3,
                        V4,
                        V5,
                        V6,
                        V7,
                        V8,
                        Value,
                        V10
                    )
            end;
        true ->
            Value = eval_update_value(ValueEval, ValueWrap, V1),
            ?LEAF10(
                K1, K2, K3, K4, K5, Key, K7, K8, K9, K10, V1, V2, V3, V4, V5, Value, V7, V8, V9, V10
            )
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

eval_insert_value(eager, Value) -> Value;
eval_insert_value(lazy, Fun) -> Fun().

eval_update_value(eager, Value, _) -> Value;
eval_update_value(lazy, Fun, PrevValue) -> Fun(PrevValue).
