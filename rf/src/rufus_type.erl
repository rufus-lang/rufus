%% rufus_annotate_locals adds a 'locals' map to each Rufus form that links
%% variable names to type information.
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
resolve_type(_Globals, {_Form, #{type := TypeForm}}) ->
    {ok, TypeForm};
resolve_type(Globals, {apply, #{spec := Spec, args := ArgExprs}}) ->
    io:format("Globals => ~p~nSpec => ~p~nArgsExprs => ~p~n", [Globals, Spec, ArgExprs]),
    case maps:get(Spec, Globals, undefined) of
        undefined ->
            {error, unknown_func, #{spec => Spec, args => ArgExprs}};
        FormDecls ->
            case find_matching_form_decls(FormDecls, ArgExprs) of
                {error, Reason, Data} ->
                    {error, Reason, Data};
                Result when length(Result) > 1 ->
                    %% TODO(jkakar): We need to handle cases where more than one
                    %% function matches a given set of arg types. For example,
                    %% consider two functions:
                    %%
                    %% func Echo(:hello) atom { :hello }
                    %% func Echo(:goodbye) string { "goodbye" }
                    %%
                    %% These both match an args list with a single atom arg
                    %% type, but they have different return types. We need to
                    %% account for all the possible return types. When a
                    %% callsite specifies a literal value such as :hello or
                    %% :goodbye we should select the correct singular return
                    %% type.
                    [{_, #{type := TypeForm}}|_] = FormDecls,
                    TypeForm;
                Result when length(Result) =:= 1 ->
                    [{_, #{type := TypeForm}}] = FormDecls,
                    TypeForm
            end
    end.

find_matching_form_decls(FormDecls, ArgExprs) ->
    FormDeclsWithMatchingArity = lists:filter(fun({func_decl, #{args := ArgDecls}}) ->
        length(ArgDecls) =:= length(ArgExprs)
    end, FormDecls),
    case length(FormDeclsWithMatchingArity) of
        Length when Length > 0 ->
            lists:filter(fun({func_decl, #{args := ArgDecls}}) ->
                Zipped = lists:zip(ArgDecls, ArgExprs),
                lists:all(fun({{arg_decl, #{type := {type, #{spec := ArgDeclTypeSpec}}}},
                               {_, #{type := {type, #{spec := ArgTypeSpec}}}}}) ->
                    ArgDeclTypeSpec =:= ArgTypeSpec
                end, Zipped)
                         end, FormDeclsWithMatchingArity);
        _ ->
            {error, unknown_arity, #{form_decls => FormDecls, arg_exprs => ArgExprs}}
    end.
