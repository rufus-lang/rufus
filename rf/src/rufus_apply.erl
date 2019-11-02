%% rufus_apply enforces the invariant that the type for each argument associated
%% with an apply form matches the type specified in the related function
%% signature. It also annotates the apply form with a type form that describes
%% the possible return types.
-module(rufus_apply).

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
    case typecheck_and_annotate([], Globals, RufusForms) of
        {ok, AnnotatedForms} ->
            {ok, AnnotatedForms};
        {error, Error, Data} ->
            {error, Error, Data}
    end.

%% Private API

-spec typecheck_and_annotate(list(rufus_form()), #{atom => rufus_form()}, list(rufus_form())) -> {ok, list(rufus_form())}.
typecheck_and_annotate(Acc, Globals, [Form = {func_decl, #{exprs := Exprs}}|T]) ->
    case typecheck_and_annotate([], Globals, Exprs) of
        {ok, AnnotatedExprs} ->
            AnnotatedForm = rufus_form:annotate(Form, exprs, AnnotatedExprs),
            typecheck_and_annotate([AnnotatedForm|Acc], Globals, T);
        {error, Error, Data} ->
            {error, Error, Data}
    end;
typecheck_and_annotate(Acc, Globals, [Form = {apply, _Context}|T]) ->
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = rufus_form:annotate(Form, type, TypeForm),
            typecheck_and_annotate([AnnotatedForm|Acc], Globals, T);
        {error, Error, Data} ->
            {error, Error, Data}
    end;
typecheck_and_annotate(Acc, Globals, [H|T]) ->
    typecheck_and_annotate([H|Acc], Globals, T);
typecheck_and_annotate(Acc, _Globals, []) ->
    {ok, lists:reverse(Acc)}.
