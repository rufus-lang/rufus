%% rufus_type exposes type information for Rufus forms. Type information is
%% inferred in cases where it isn't already present.
-module(rufus_type).

-include_lib("rufus_type.hrl").

%% API exports

-export([resolve/2]).

%% API

%% resolve returns a type form for Form. If Form already has a type form
%% associated with it, it will be returned. Otherwise, the type is inferred.
-spec resolve(#{atom() => list(rufus_form())}, rufus_form()) -> {ok, type_form()} | error_triple().
resolve(Globals, Form) ->
    try
        resolve_type(Globals, Form)
    catch
        {error, Code, Data} -> {error, Code, Data}
    end.

%% Private API

-spec resolve_type(#{atom() => list(rufus_form())}, rufus_form()) -> {ok, type_form()} | no_return().
resolve_type(_Globals, {_Form, #{type := Type}}) ->
    {ok, Type};
resolve_type(_Globals, {func_decl, #{return_type := Type}}) ->
    {ok, Type};
resolve_type(Globals, Form = {call, #{spec := Spec, args := ArgExprs}}) ->
    case maps:get(Spec, Globals, undefined) of
        undefined ->
            throw({error, unknown_func, #{spec => Spec, args => ArgExprs}});
        FuncDecls ->
            case find_matching_func_decls(FuncDecls, ArgExprs) of
                {error, Reason, Data} ->
                    throw({error, Reason, Data});
                {ok, MatchingFuncDecls} when length(MatchingFuncDecls) > 1 ->
                    %% TODO(jkakar): We need to handle cases where more than one
                    %% function matches a given set of parameters. For example,
                    %% consider two functions:
                    %%
                    %% func Echo(:hello) atom { :hello }
                    %% func Echo(:goodbye) string { "goodbye" }
                    %%
                    %% These both match an args list with a single atom arg
                    %% type, but they have different return types. We need to
                    %% account for all possible return types. When a callsite
                    %% specifies a literal value such as :hello or :goodbye we
                    %% should select the correct singular return type.
                    erlang:error({not_implemented, [Globals, Form]});
                {ok, MatchingFuncDecls} when length(MatchingFuncDecls) =:= 1 ->
                    [FuncDecl] = MatchingFuncDecls,
                    {ok, rufus_form:return_type(FuncDecl)}
            end
    end;
resolve_type(Globals, Form = {binary_op, #{op := Op, left := Left, right := Right}}) ->
    {ok, LeftType} = resolve_type(Globals, Left),
    LeftTypeSpec = rufus_form:type_spec(LeftType),
    {ok, RightType} = resolve_type(Globals, Right),
    RightTypeSpec = rufus_form:type_spec(RightType),
    case supported_type(Op, LeftTypeSpec) and supported_type(Op, RightTypeSpec) of
        true ->
            case supported_type_pair(LeftTypeSpec, RightTypeSpec) of
                true ->
                    {ok, LeftType};
                false ->
                    throw({error, unmatched_operand_type, #{form => Form}})
            end;
        false ->
            throw({error, unsupported_operand_type, #{form => Form}})
    end.

%% call form helpers

-spec find_matching_func_decls(list(func_decl_form()), list(rufus_form())) -> {ok, list(func_decl_form())} | error_triple().
find_matching_func_decls(FuncDecls, ArgExprs) ->
    FuncDeclsWithMatchingArity = lists:filter(fun({func_decl, #{params := Params}}) ->
        length(Params) =:= length(ArgExprs)
    end, FuncDecls),

    case length(FuncDeclsWithMatchingArity) of
        Length when Length > 0 ->
            Result = lists:filter(fun({func_decl, #{params := Params}}) ->
                Zipped = lists:zip(Params, ArgExprs),
                lists:all(fun({{param, #{type := {type, #{spec := ParamTypeSpec}}}},
                               {_, #{type := {type, #{spec := ArgTypeSpec}}}}}) ->
                        ParamTypeSpec =:= ArgTypeSpec
                end, Zipped)
            end, FuncDeclsWithMatchingArity),
            case Result of
                Result when length(Result) =:= 0 ->
                    {error, unmatched_args, #{func_decls => FuncDeclsWithMatchingArity, arg_exprs => ArgExprs}};
                _ ->
                    {ok, Result}
            end;
        _ ->
            {error, unknown_arity, #{func_decls => FuncDecls, arg_exprs => ArgExprs}}
    end.

%% binary_op form helpers

-spec supported_type(atom(), float | int | atom()) -> boolean().
supported_type('%', float) -> false;
supported_type(_, float) -> true;
supported_type(_, int) -> true;
supported_type(_, _) -> false.

-spec supported_type_pair(float | int | atom(), float | int | atom()) -> boolean().
supported_type_pair(float, float) -> true;
supported_type_pair(int, int) -> true;
supported_type_pair(_, _) -> false.
