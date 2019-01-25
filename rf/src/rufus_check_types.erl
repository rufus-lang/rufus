%% rufus_check_types enforces the invariant that the type of a function's return
%% value matches the return type specified in the function signature.
-module(rufus_check_types).

%% API exports

-export([forms/1]).

%% API

%% forms iterates over RufusForms and determines whether the type of each
%% function return value matches the return type defined in the function
%% signature. Iteration stops at the first error. Return values:
%% - `{ok, RufusForms}` if no issues are found.
%% - `{error, unmatched_return_type, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to Rufus types if return value types are
%%   unmatched.
forms(RufusForms) ->
    forms(RufusForms, RufusForms).

%% Private API

forms(Forms, [H|T]) ->
    case check_expr(H) of
        ok ->
            forms(Forms, T);
        Error ->
            Error
    end;
forms(Forms, []) ->
    {ok, Forms}.

check_expr({func, #{return_type := ReturnType, exprs := Exprs}}) ->
    check_return_expr(ReturnType, lists:last(Exprs));
check_expr(_) ->
    ok.

check_return_expr({type, #{spec := ReturnType}}, {FormType, #{type := {type, #{spec := ReturnType}}}}) ->
    io:format("ReturnType => ~p  FormType => ~p~n", [ReturnType, FormType]),
    ok;
check_return_expr({type, #{spec := ReturnType}}, {FormType, #{type := {type, #{spec := ActualReturnType}}}}) ->
    io:format("ReturnType => ~p  ActualReturnType => ~p  FormType => ~p~n", [ReturnType, ActualReturnType, FormType]),
    Data = #{expected => ReturnType, actual => ActualReturnType},
    {error, unmatched_return_type, Data}.
