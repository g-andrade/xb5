% From: https://gist.github.com/nicklasos/c177478b972e74872b3b

-module(pmap).
-export([pmap/2]).

%%% Map

pmap(F, L) ->
  S = self(),
  Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
  pmap_gather(Pids).

pmap_gather([H|T]) ->
  receive
      {RetH, Ret} when RetH =:= H -> [Ret|pmap_gather(T)]
  end;
pmap_gather([]) ->
  [].

pmap_f(Parent, F, I) ->
  Parent ! {self(), (catch F(I))}.
