-module(b5_sets).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    empty/0,
    from_list/1,
    insert/2,
    is_element/2,
    new/0,
    nth/2,
    to_list/1,
    union/2
]).

-ignore_xref([
]).

%% ------------------------------------------------------------------
%% API Type Definitions
%% ------------------------------------------------------------------

-opaque set(Key, Value) :: b5_sets_node:t(Key, Value).
-export_type([set/2]).

%%%%%%%

-type iter(Key, Value) :: b5_sets_node:iter(Key, Value).
-export_type([iter/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

empty() ->
    new().

from_list(List) ->
    lists:foldl(fun b5_sets_node:enter/2, new(), List).

insert(Key, Set) ->
    b5_sets_node:insert(Key, Set).

is_element(Key, Set) ->
    b5_sets_node:is_element(Key, Set).

new() ->
    b5_sets_node:new().

nth(N, Set) ->
    b5_sets_node:nth(N, Set).

to_list(Set) ->
    b5_sets_node:to_list(Set).

union(Set1, Set2) ->
    b5_sets_node:union(Set1, Set2).
