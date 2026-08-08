%% Branch-selection macros, shared by xb5_sets_node, xb5_trees_node and
%% xb5_bag_node.
%%
%% Each compares Key against the 1 to 4 keys of one node and expands to exactly
%% one of the branch expressions handed to it. The branches are substituted,
%% not evaluated, which is what lets a caller pass a recursive call as a branch
%% - as the insertion and deletion paths do throughout.
%%
%% Four families, differing in what the caller needs to learn:
%%
%%   ?GAP_SEARCH<N>      Key sits on K<i> (FoundK<i>), or in the gap before
%%                       child i (FoundC<i>). Those N+1 gap branches are what
%%                       make this the one for descending into a child, and
%%                       for choosing an insertion slot within a leaf.
%%
%%   ?EXACT_SEARCH<N>    Key is one of K1..K<n> (FoundK<i>) or absent
%%                       (NotFound). Used when deleting from a leaf, where
%%                       landing in a gap means the key is nowhere in the tree.
%%
%%   ?SMALLER_SEARCH<N>  Places Key by LTE<i> (K<i-1> < Key =< K<i>) or GT<n>
%%                       (Key > K<n>).
%%
%%   ?LARGER_SEARCH<N>   The mirror image: LT<i> (Key < K<i>) or GTE<n>
%%                       (Key >= K<n>).
%%
%% The last two back smaller/2 and larger/2, plus the bounded iterators.
%%
%% Comparisons are by Erlang term order, never `=:=`, as the public API
%% promises.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(GAP_SEARCH4(
    Key,
    K1,
    K2,
    K3,
    K4,
    %
    FoundK1,
    FoundK2,
    FoundK3,
    FoundK4,
    %
    FoundC1,
    FoundC2,
    FoundC3,
    FoundC4,
    FoundC5
),
    (begin
        if
            Key > K2 ->
                %
                if
                    Key < K4 ->
                        %
                        if
                            Key > K3 ->
                                FoundC4;
                            %
                            Key < K3 ->
                                FoundC3;
                            %
                            true ->
                                FoundK3
                        end;
                    %
                    Key > K4 ->
                        FoundC5;
                    %
                    true ->
                        FoundK4
                end;
            %
            Key < K2 ->
                %
                if
                    Key > K1 ->
                        FoundC2;
                    %
                    Key < K1 ->
                        FoundC1;
                    %
                    true ->
                        FoundK1
                end;
            %
            true ->
                FoundK2
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(GAP_SEARCH3(
    Key,
    K1,
    K2,
    K3,
    %
    FoundK1,
    FoundK2,
    FoundK3,
    %
    FoundC1,
    FoundC2,
    FoundC3,
    FoundC4
),
    (begin
        if
            Key > K2 ->
                %
                if
                    Key < K3 ->
                        FoundC3;
                    %
                    Key > K3 ->
                        FoundC4;
                    %
                    true ->
                        FoundK3
                end;
            %
            Key == K2 ->
                FoundK2;
            %
            Key < K1 ->
                FoundC1;
            %
            Key > K1 ->
                FoundC2;
            %
            true ->
                FoundK1
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(GAP_SEARCH2(
    Key,
    K1,
    K2,
    %
    FoundK1,
    FoundK2,
    %
    FoundC1,
    FoundC2,
    FoundC3
),
    (begin
        if
            Key > K1 ->
                %
                if
                    Key < K2 ->
                        FoundC2;
                    %
                    Key > K2 ->
                        FoundC3;
                    %
                    true ->
                        FoundK2
                end;
            %
            Key < K1 ->
                FoundC1;
            %
            true ->
                FoundK1
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(GAP_SEARCH1(
    Key,
    K1,
    %
    FoundK1,
    %
    FoundC1,
    FoundC2
),
    (begin
        if
            Key < K1 ->
                FoundC1;
            %
            Key > K1 ->
                FoundC2;
            %
            true ->
                FoundK1
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EXACT_SEARCH4(Key, K1, K2, K3, K4, FoundK1, FoundK2, FoundK3, FoundK4, NotFound),
    (begin
        if
            Key == K1 ->
                FoundK1;
            %
            Key == K2 ->
                FoundK2;
            %
            Key == K3 ->
                FoundK3;
            %
            Key == K4 ->
                FoundK4;
            %
            true ->
                NotFound
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EXACT_SEARCH3(Key, K1, K2, K3, FoundK1, FoundK2, FoundK3, NotFound),
    (begin
        if
            Key == K1 ->
                FoundK1;
            %
            Key == K2 ->
                FoundK2;
            %
            Key == K3 ->
                FoundK3;
            %
            true ->
                NotFound
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EXACT_SEARCH2(Key, K1, K2, FoundK1, FoundK2, NotFound),
    (begin
        if
            Key == K1 ->
                FoundK1;
            %
            Key == K2 ->
                FoundK2;
            %
            true ->
                NotFound
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EXACT_SEARCH1(Key, K1, FoundK1, NotFound),
    (begin
        if
            Key == K1 ->
                FoundK1;
            %
            true ->
                NotFound
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SMALLER_SEARCH4(Key, K1, K2, K3, K4, LTE1, LTE2, LTE3, LTE4, GT4),
    (begin
        if
            Key > K3 ->
                %
                if
                    Key > K4 ->
                        GT4;
                    %
                    true ->
                        LTE4
                end;
            %
            Key > K2 ->
                LTE3;
            %
            Key > K1 ->
                LTE2;
            %
            true ->
                LTE1
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SMALLER_SEARCH3(Key, K1, K2, K3, LTE1, LTE2, LTE3, GT3),
    (begin
        if
            Key > K2 ->
                if
                    Key > K3 ->
                        GT3;
                    %
                    true ->
                        LTE3
                end;
            %
            Key > K1 ->
                LTE2;
            %
            true ->
                LTE1
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SMALLER_SEARCH2(Key, K1, K2, LTE1, LTE2, GT2),
    (begin
        if
            Key > K2 ->
                GT2;
            %
            Key > K1 ->
                LTE2;
            %
            true ->
                LTE1
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SMALLER_SEARCH1(Key, K1, LTE1, GT1),
    (begin
        if
            Key > K1 ->
                GT1;
            %
            true ->
                LTE1
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LARGER_SEARCH4(Key, K1, K2, K3, K4, LT1, LT2, LT3, LT4, GTE4),
    (begin
        if
            Key < K2 ->
                %
                if
                    Key < K1 ->
                        LT1;
                    %
                    true ->
                        LT2
                end;
            %
            Key < K3 ->
                LT3;
            %
            Key < K4 ->
                LT4;
            %
            true ->
                GTE4
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LARGER_SEARCH3(Key, K1, K2, K3, LT1, LT2, LT3, GTE3),
    (begin
        if
            Key < K2 ->
                %
                if
                    Key < K1 ->
                        LT1;
                    %
                    true ->
                        LT2
                end;
            %
            Key < K3 ->
                LT3;
            %
            true ->
                GTE3
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LARGER_SEARCH2(Key, K1, K2, LT1, LT2, GTE2),
    (begin
        if
            Key < K1 ->
                LT1;
            %
            Key < K2 ->
                LT2;
            %
            true ->
                GTE2
        end
    end)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LARGER_SEARCH1(Key, K1, LT1, GTE1),
    (begin
        if
            Key < K1 ->
                LT1;
            %
            true ->
                GTE1
        end
    end)
).
