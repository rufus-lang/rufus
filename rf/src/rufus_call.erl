%% rufus_call enforces the invariant that the type for each argument in a
%% function call matches the type specified in the related function declaration.
%% It also annotates the call form with a type form that describes the possible
%% return types.
-module(rufus_call).

-include_lib("rufus_type.hrl").

%% API exports

-export([typecheck_and_annotate/1]).

%% API

%% typecheck_and_annotate iterates over RufusForms and determines whether the
%% type of each argument matches the argument types defined in the function
%% signature. Iteration stops at the first error. Return values:
%% - `{ok, RufusForms}` if no issues are found.
%% - `{error, unknown_func, Data}` with `Data` containing a `spec` key that has
%%   the function name.
%% - `{error, incorrect_arg_count, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to the number of args received and the number
%%   of args expected, respectively
%% - `{error, invalid_arg_type, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to Rufus types if return value types are
%%   unmatched.
-spec typecheck_and_annotate(list(rufus_form())) -> {ok, list(rufus_form())} | error_triple().
typecheck_and_annotate(RufusForms) ->
    {ok, Globals} = rufus_scope:globals(RufusForms),
    try
        typecheck_and_annotate([], Globals, RufusForms)
    catch
        {error, Code, Data} -> {error, Code, Data}
    end.

%% Private API

-spec typecheck_and_annotate(list(rufus_form()), #{atom => rufus_form()}, list(rufus_form())) -> {ok, list(rufus_form())} | no_return().
typecheck_and_annotate(Acc, Globals, [Form = {func_decl, #{exprs := Exprs}}|T]) ->
    {ok, AnnotatedExprs} = typecheck_and_annotate([], Globals, Exprs),
    AnnotatedForm = rufus_form:annotate(Form, exprs, AnnotatedExprs),
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, T);
typecheck_and_annotate(Acc, Globals, [Form = {call, _Context}|T]) ->
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = rufus_form:annotate(Form, type, TypeForm),
            typecheck_and_annotate([AnnotatedForm|Acc], Globals, T);
        Error ->
            throw(Error)
    end;
typecheck_and_annotate(Acc, Globals, [H|T]) ->
    typecheck_and_annotate([H|Acc], Globals, T);
typecheck_and_annotate(Acc, _Globals, []) ->
    {ok, lists:reverse(Acc)}.
