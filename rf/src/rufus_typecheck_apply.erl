%% rufus_typecheck_apply enforces the invariant that the type for each argument
%% associated with an apply form matches the type specified in the related
%% function signature. It also annotates the apply form with a type form that
%% describes the possible return types.
-module(rufus_typecheck_apply).

-include_lib("rufus_type.hrl").

%% API exports

-export([forms/1]).

%% API

%% forms iterates over RufusForms and determines whether the type of each
%% argument matches the argument types defined in the function signature.
%% Iteration stops at the first error. Return values:
%% - `{ok, RufusForms}` if no issues are found.
%% - `{error, unknown_func, Data}` with `Data` containing a `spec` key that has
%%   the function name.
%% - `{error, incorrect_arg_count, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to the number of args received and the number
%%   of args expected, respectively
%% - `{error, invalid_arg_type, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to Rufus types if return value types are
%%   unmatched.
-spec forms(list(rufus_form())) -> {ok, list(rufus_form())}.
forms(RufusForms) ->
    {ok, Globals} = rufus_scope:globals(RufusForms),
    case forms(Globals, RufusForms) of
        ok ->
            {ok, RufusForms};
        Error ->
            Error
    end.

%% Private API

forms(Globals, [{func_decl, #{exprs := Exprs}}|T]) ->
    case forms(Globals, Exprs) of
        {ok, Globals, _Forms} ->
            forms(Globals, T);
        Error ->
            Error
    end;
forms(Globals, Form = [{apply, _Context}|T]) ->
    io:format("YO DAWG!"),
    Handlers = [fun validate_function_exists/2,
                fun validate_matching_arity/2,
                fun validate_matching_arg_types/2
               ],
    case chain_validators(Globals, Form, Handlers) of
        {ok, Globals, Form} ->
            forms(Globals, T);
        Error ->
            Error
    end;
forms(Globals, [_H|T]) ->
    forms(Globals, T);
forms(_Globals, []) ->
    ok.

%% chain_validators runs each handler H as H(Globals, Form) in order. Each
%% handler result must match {ok, Globals, Form}. Any other response is treated
%% as an error. Processing stops when a handler returns an error.
-spec chain_validators(map(), any(), list(fun((_, _) -> any()))) -> ok_tuple() | error_tuple().
chain_validators(Globals, Form = {apply, _Context}, [H|T]) ->
    io:format("H(Form) => ~p(~p)~n", [H, Form]),
    case H(Globals, Form) of
        {ok, Globals, Form} ->
            chain_validators(Globals, Form, T);
        Error ->
            Error
    end;
chain_validators(Globals, Form, []) ->
    {ok, Globals, Form}.

validate_function_exists(Globals, Form = {apply, #{spec := Spec}}) ->
    case map:is_key(Spec, Globals) of
        true ->
            {ok, Globals, Form};
        false ->
            {error, unknown_func, #{spec => Spec}}
    end.

validate_matching_arity(Globals, Form = {apply, #{spec := Spec, args := ArgExprs}}) ->
    FormDecls = maps:get(Spec, Globals),
    FormDeclHasMatchingArity = lists:any(fun({func_decl, #{args := ArgDecls}}) ->
        length(ArgDecls) =:= length(ArgExprs)
    end, FormDecls),
    case FormDeclHasMatchingArity of
        true ->
            {ok, Globals, Form};
        false ->
            {error, incorrect_arg_count, #{actual => length(ArgExprs)}}
    end.

validate_matching_arg_types(Globals, Form = {apply, #{spec := Spec, args := ArgExprs}}) ->
    FormDecls = maps:get(Spec, Globals),
    FormDeclsWithMatchingArity = lists:filter(fun({func_decl, #{args := ArgDecls}}) ->
        length(ArgDecls) =:= length(ArgExprs)
    end, FormDecls),
    FormDeclHasMatchingArgTypes = lists:any(fun({func_decl, #{args := ArgDecls}}) ->
        ArgPairs = lists:zip(ArgDecls, ArgExprs),
        lists:all(fun({arg_decl, #{type := {type, #{spec := Spec1}}}}, {_, #{type := {type, #{spec := Spec2}}}}) ->
            Spec1 =:= Spec2
        end, ArgPairs)
    end, FormDeclsWithMatchingArity),
    case FormDeclHasMatchingArgTypes of
        true ->
            {ok, Globals, Form};
        false ->
            {error, incorrect_arg_type, #{}}
    end.
