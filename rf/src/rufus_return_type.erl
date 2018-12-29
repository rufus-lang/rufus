%% rufus_return_type enforces the invariant that the type of a function's return
%% value matches the return type specified in the function signature.
-module(rufus_return_type).

%% API exports

-export([check/1]).

%% API

%% check iterates over Rufus forms and determines whether the type of each
%% function return value matches the return type defined in the function
%% signature. Iteration stops at the first error. Return values:
%% - `ok` if no issues are found.
%% - `{error, unmatched_return_type, Data}` with `Data` containing
%%   `actual_return_value` and `expected_return_value` atom keys pointing to
%%   Rufus types if return value types are unmatched.
check(Forms) ->
    check(Forms, Forms).

%% Private API

check(Forms, [H|T]) ->
    case check_expr(H) of
        ok ->
            check(Forms, T);
        Error ->
            Error
    end;
check(Forms, []) ->
    {ok, Forms}.

check_expr({func, _LineNumber, _Name, _Arguments, ReturnType, Exprs}) ->
    check_expr(ReturnType, lists:last(Exprs));
check_expr(_) ->
    ok.

check_expr(ReturnType, {expr, _LineNumber, {ReturnType, _Literal}}) ->
    ok;
check_expr(ReturnType, {expr, _LineNumber, {ActualReturnType, _Literal}}) ->
    Data = #{expected => ReturnType, actual => ActualReturnType},
    {error, unmatched_return_type, Data}.
