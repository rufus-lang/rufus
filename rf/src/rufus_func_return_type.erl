%% rufus_func_return_type enforces the invariant that the type of a function's
%% return value matches the return type specified in the function signature.
-module(rufus_func_return_type).

-include_lib("rufus_type.hrl").

%% API exports

-export([typecheck/1]).

%% API

%% typecheck iterates over RufusForms and determines whether the type of each
%% function return value matches the return type defined in the function
%% signature. Iteration stops at the first error. Return values:
%% - `{ok, RufusForms}` if no issues are found.
%% - `{error, unmatched_return_type, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to Rufus types if return value types are
%%   unmatched.
-spec typecheck(list(rufus_form())) -> {ok, list(rufus_form())}.
typecheck(RufusForms) ->
    try
        typecheck_forms(RufusForms, RufusForms)
    catch
        {error, Code, Data} -> {error, Code, Data}
    end.

%% Private API

-spec typecheck_forms(list(rufus_form()), list(rufus_form())) -> {ok, list(rufus_form())} | no_return().
typecheck_forms([H|T], Forms) ->
    typecheck_forms(H),
    typecheck_forms(T, Forms);
typecheck_forms([], Forms) ->
    {ok, Forms}.

-spec typecheck_forms(rufus_form()) -> ok | no_return().
typecheck_forms({func_decl, #{return_type := ReturnType, exprs := Exprs}}) ->
    typecheck_return_value(ReturnType, lists:last(Exprs));
typecheck_forms(_) ->
    ok.

-spec typecheck_return_value(type_form(), identifier_form()) -> ok | no_return().
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
                    throw({error, unmatched_return_type, Data})
            end;
        false ->
            Data = #{spec => Spec},
            throw({error, unknown_variable, Data})
    end;
typecheck_return_value({type, #{spec := _ReturnType}}, {_FormType, #{type := {type, #{spec := _ReturnType}}}}) ->
    ok;
typecheck_return_value({type, #{spec := ExpectedReturnType}}, {_FormType, #{type := {type, #{spec := ActualReturnType}}}}) ->
    Data = #{expected => ExpectedReturnType, actual => ActualReturnType},
    throw({error, unmatched_return_type, Data}).
