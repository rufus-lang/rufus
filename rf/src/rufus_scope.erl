-module(rufus_scope).

-include_lib("rufus_type.hrl").

%% API exports

-export([globals/1]).

%% API

-spec globals(list(rufus_form())) -> {ok, #{atom() => list(rufus_form())}}.
globals(RufusForms) ->
    globals(#{}, RufusForms).

-spec globals(map(), list(rufus_form())) -> {ok, #{atom() => list(rufus_form())}}.
globals(Acc, [Form = {func_decl, #{spec := Spec}}|T]) ->
    Forms = maps:get(Spec, Acc, []),
    globals(Acc#{Spec => Forms ++ [Form]}, T);
globals(Acc, [_H|T]) ->
    globals(Acc, T);
globals(Acc, []) ->
    {ok, Acc}.
