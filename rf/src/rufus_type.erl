%% rufus_type exposes type information for Rufus forms. Type information is
%% inferred in cases where it isn't already present.
-module(rufus_type).

-include_lib("rufus_type.hrl").

%% API exports

-export([resolve/2]).

%% API

%% resolve returns a type form for Form. If Form already has a type form
%% associated with it, it will be returned. Otherwise, the type is inferred.
-spec resolve(#{atom() => list(rufus_form())}, rufus_form()) -> {ok, type_form()} | error_tuple().
resolve(Globals, Form) ->
    resolve_type(Globals, Form).

%% Private API

-spec resolve_type(#{atom() => list(rufus_form())}, rufus_form()) -> {ok, type_form()} | error_tuple().
resolve_type(_Globals, {_Form, #{type := Type}}) ->
    {ok, Type};
resolve_type(_Globals, {func_decl, #{return_type := Type}}) ->
    {ok, Type};
resolve_type(Globals, {apply, #{spec := Spec, args := ArgExprs}}) ->
    case maps:get(Spec, Globals, undefined) of
        undefined ->
            {error, unknown_func, #{spec => Spec, args => ArgExprs}};
        FormDecls ->
            case find_matching_form_decls(FormDecls, ArgExprs) of
                {error, Reason, Data} ->
                    {error, Reason, Data};
                {ok, Result} when length(Result) > 1 ->
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
                    [{_, #{return_type := Type}}|_] = FormDecls,
                    {ok, Type};
                {ok, Result} when length(Result) =:= 1 ->
                    [{_, #{return_type := Type}}] = FormDecls,
                    {ok, Type}
            end
    end.

find_matching_form_decls(FormDecls, ArgExprs) ->
    FormDeclsWithMatchingArity = lists:filter(fun({func_decl, #{args := ArgDecls}}) ->
        length(ArgDecls) =:= length(ArgExprs)
    end, FormDecls),

    case length(FormDeclsWithMatchingArity) of
        Length when Length > 0 ->
            Result = lists:filter(fun({func_decl, #{args := ArgDecls}}) ->
                Zipped = lists:zip(ArgDecls, ArgExprs),
                lists:all(fun({{arg_decl, #{type := {type, #{spec := ArgDeclTypeSpec}}}},
                               {_, #{type := {type, #{spec := ArgTypeSpec}}}}}) ->
                        ArgDeclTypeSpec =:= ArgTypeSpec
                end, Zipped)
            end, FormDeclsWithMatchingArity),
            {ok, Result};
        _ ->
            {error, unknown_arity, #{form_decls => FormDecls, arg_exprs => ArgExprs}}
    end.
