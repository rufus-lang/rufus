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
    case forms([], Globals, RufusForms) of
        {ok, AnnotatedForms} ->
            {ok, AnnotatedForms};
        Error ->
            Error
    end.

%% Private API

forms(Acc, Globals, [Form = {func_decl, #{exprs := Exprs}}|T]) ->
    case forms([], Globals, Exprs) of
        {ok, AnnotatedExprs} ->
            AnnotatedForm = rufus_form:annotate(Form, exprs, AnnotatedExprs),
            forms([AnnotatedForm|Acc], Globals, T);
        Error ->
            Error
    end;
forms(Acc, Globals, [Form = {apply, _Context}|T]) ->
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = rufus_form:annotate(Form, type, TypeForm),
            forms([AnnotatedForm|Acc], Globals, T);
        Error ->
            Error
    end;
forms(Acc, Globals, [H|T]) ->
    forms([H|Acc], Globals, T);
forms(Acc, _Globals, []) ->
    {ok, lists:reverse(Acc)}.
