%% rufus_typecheck_func_args enforces the invariant that the type for each
%% argument passed to a function matches the type specified in the function
%% signature.
-module(rufus_typecheck_apply).

-include_lib("rufus_type.hrl").

%% API exports

-export([forms/1]).

-ifdef(EUNIT).
-export([globals/1]).
-endif.

%% API

%% forms iterates over RufusForms and determines whether the type of each
%% argument matches the argument types defined in the function signature.
%% Iteration stops at the first error. Return values:
%% - `{ok, RufusForms}` if no issues are found.
%% - `{error, invalid_arg_type, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to Rufus types if return value types are
%%   unmatched.
%% - `{error, incorrect_arg_count, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to the number of args received and the number
%%   of args expected, respectively
-spec forms(list(rufus_form())) -> {ok, list(rufus_form())}.
forms(RufusForms) ->
    {ok, Globals} = globals(RufusForms),
    case forms(Globals, RufusForms) of
        Error = {error, _Reason, _Data} ->
            Error;
        ok ->
            {ok, RufusForms}
    end.

%% Private API

-spec globals(list(rufus_form())) -> {ok, #{atom() => list(rufus_form())}}.
globals(RufusForms) ->
    globals(#{}, RufusForms).

globals(Acc, [Form = {func, #{spec := Spec}}|T]) ->
    Forms = maps:get(Spec, Acc, []),
    globals(Acc#{Spec => Forms ++ [Form]}, T);
globals(Acc, [_H|T]) ->
    globals(Acc, T);
globals(Acc, []) ->
    {ok, Acc}.

forms(Globals, [{apply, #{spec := _Spec}}|T]) ->
    %% TODO: Type check function call.
    forms(Globals, T);
forms(Globals, [_H|T]) ->
    forms(Globals, T);
forms(_Globals, []) ->
    ok.
