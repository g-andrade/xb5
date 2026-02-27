%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(GAP_SEARCH4(
    Key,
    K1,
    K2,
    K3,
    K4,
    FoundK1,
    FoundK2,
    FoundK3,
    FoundK4,
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
    FoundK1,
    FoundK2,
    FoundK3,
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
    FoundK1,
    FoundK2,
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
    FoundK1,
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
