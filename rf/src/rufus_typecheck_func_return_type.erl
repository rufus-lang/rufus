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
    case typecheck(H) of
        ok ->
            forms(T, Forms);
        Error ->
            Error
    end;
forms([], Forms) ->
    {ok, Forms}.

-spec typecheck(rufus_form()) -> ok | error_triple().
typecheck({func_decl, #{return_type := ReturnType, exprs := Exprs}}) ->
    typecheck_return_value(ReturnType, lists:last(Exprs));
typecheck(_) ->
    ok.

-spec typecheck_return_value(type_form(), identifier_form()) -> ok | error_triple().
typecheck_return_value({type, #{spec := ReturnType}}, {identifier, #{locals := Locals, spec := Spec}}) ->
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
typecheck_return_value({type, #{spec := _ReturnType}}, {_FormType, #{type := {type, #{spec := _ReturnType}}}}) ->
    ok;
typecheck_return_value({type, #{spec := ExpectedReturnType}}, {_FormType, #{type := {type, #{spec := ActualReturnType}}}}) ->
    Data = #{expected => ExpectedReturnType, actual => ActualReturnType},
    {error, unmatched_return_type, Data}.
