%% @private
-module(b5_trees_util).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([dialyzer_opaque_term/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec dialyzer_opaque_term(term()) -> term().
dialyzer_opaque_term(V) ->
    V.
