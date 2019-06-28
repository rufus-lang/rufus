%% rufus_typecheck_func_return_type enforces the invariant that the type of a
%% function's return value matches the return type specified in the function
%% signature.
-module(rufus_typecheck_func_return_type).

-include_lib("rufus_type.hrl").

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
-spec forms(list(rufus_form())) -> {ok, list(rufus_form())}.
forms(RufusForms) ->
    forms(RufusForms, RufusForms).

%% Private API

-spec forms(list(rufus_form()), list(rufus_form())) -> {ok, list(rufus_form())}.
forms([H|T], Forms) ->
    case check_expr(H) of
        ok ->
            forms(T, Forms);
        Error ->
            Error
    end;
forms([], Forms) ->
    {ok, Forms}.

-spec check_expr(rufus_form()) -> ok | {error, rufus_error(), map()}.
check_expr({func, #{return_type := ReturnType, exprs := Exprs}}) ->
    check_return_expr(ReturnType, lists:last(Exprs));
check_expr(_) ->
    ok.

-spec check_return_expr(type_form(), identifier_form()) -> ok | {error, rufus_error(), map()}.
check_return_expr({type, #{spec := ReturnType}}, {identifier, #{locals := Locals, spec := Spec}}) ->
    case maps:is_key(Spec, Locals) of
        true ->
            {type, TypeData} = maps:get(Spec, Locals),
            IdentifierType = maps:get(spec, TypeData),
            case IdentifierType of
                ReturnType ->
                    ok;
                _ ->
                    Data = #{expected => ReturnType, actual => IdentifierType},
                    {error, unmatched_return_type, Data}
            end;
        false ->
            Data = #{spec => Spec},
            {error, unknown_variable, Data}
    end;
check_return_expr({type, #{spec := _ReturnType}}, {_FormType, #{type := {type, #{spec := _ReturnType}}}}) ->
    ok;
check_return_expr({type, #{spec := ExpectedReturnType}}, {_FormType, #{type := {type, #{spec := ActualReturnType}}}}) ->
    Data = #{expected => ExpectedReturnType, actual => ActualReturnType},
    {error, unmatched_return_type, Data}.
