%% @private
-module(xb5_docs_transform).

%%-------------------------------------------------------------------
%% Callback Function Exports
%%-------------------------------------------------------------------

-export([parse_transform/2]).

-ignore_xref([parse_transform/2]).

%%-------------------------------------------------------------------
%% Hank Tweaks
%%-------------------------------------------------------------------

-hank([{unnecessary_function_arguments, [{filter_form, 1, 1}]}]).

%%-------------------------------------------------------------------
%% Callback Function Definitions
%%-------------------------------------------------------------------

-spec parse_transform(Forms, Options) -> NewForms when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
    NewForms :: Forms,
    Options :: [compile:option()].
parse_transform(Forms, _Options) ->
    % write_terms("ast_before.txt", Forms),
    _FilteredForms = lists:filter(fun filter_form/1, Forms).

%%-------------------------------------------------------------------
%% Internal Function Definitions
%%-------------------------------------------------------------------

-if(OTP_RELEASE < 27).

filter_form({attribute, _Location, Name, _Content}) ->
    % Remove new doc attributes that are incompatible with OTP 26 or older
    not lists:member(Name, [moduledoc, doc]);
filter_form(_) ->
    true.

-else.

filter_form(_) ->
    true.

-endif.

%%%%

% write_terms(FilenameSuffix, List) ->
%     {attribute, _Anno, module, Module} = lists:keyfind(module, 3, List),
%     Filename = atom_to_list(Module) ++ "." ++ FilenameSuffix,
%     Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
%     Text = lists:map(Format, List),
%     file:write_file(Filename, Text).
