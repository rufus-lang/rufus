%% rufus_scope calculates visibility into global names and maps them to Rufus
%% forms. It also annotates Rufus forms representing variables with type
%% information.
-module(rufus_scope).

-include_lib("rufus_type.hrl").

%% API exports

-export([
    globals/1,
    typecheck_and_annotate/1
]).

%% API

%% globals creates a map of function names to func_decl forms for all top-level
%% functions in RufusForms.
-spec globals(list(rufus_form())) -> {ok, #{atom() => list(rufus_form())}}.
globals(RufusForms) ->
    globals(#{}, RufusForms).

%% typecheck_and_annotate iterates over RufusForms and adds type information
%% from the current scope to each form. Iteration stops at the first error.
%% Return values:
%% - `{ok, AnnotatedRufusForms}` if no issues are found.
%% - `{
typecheck_and_annotate(RufusForms) ->
    {ok, Globals} = rufus_scope:globals(RufusForms),
    try
        {ok, _Locals, AnnotatedForms} = typecheck_and_annotate([], Globals, #{}, RufusForms),
        {ok, AnnotatedForms}
    catch
        {error, Code, Data} -> {error, Code, Data}
    end.

%% Private API

-spec globals(map(), list(rufus_form())) -> {ok, #{atom() => list(rufus_form())}}.
globals(Acc, [Form = {func_decl, #{spec := Spec}}|T]) ->
    Forms = maps:get(Spec, Acc, []),
    globals(Acc#{Spec => Forms ++ [Form]}, T);
globals(Acc, [_H|T]) ->
    globals(Acc, T);
globals(Acc, []) ->
    {ok, Acc}.

-spec typecheck_and_annotate(list(rufus_form()), globals(), locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
typecheck_and_annotate(Acc, Globals, Locals, [{func_decl, Context = #{params := Params, exprs := Exprs}}|T]) ->
    {ok, NewLocals1, AnnotatedParams} = typecheck_and_annotate([], Globals, Locals, Params),
    {ok, NewLocals2, AnnotatedExprs} = typecheck_and_annotate([], Globals, NewLocals1, Exprs),
    AnnotatedForm = {func_decl, Context#{params => AnnotatedParams, exprs => AnnotatedExprs}},
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, NewLocals2, T);
typecheck_and_annotate(Acc, Globals, Locals, [{param, Context = #{spec := Spec, type := Type}}|T]) ->
    NewLocals = Locals#{Spec => Type},
    AnnotatedForm = {param, Context},
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, NewLocals, T);
typecheck_and_annotate(Acc, Globals, Locals, [{binary_op, Context = #{left := Left, right := Right}}|T]) ->
    {ok, Locals, [AnnotatedLeft]} = typecheck_and_annotate([], Globals, Locals, [Left]),
    {ok, Locals, [AnnotatedRight]} = typecheck_and_annotate([], Globals, Locals, [Right]),
    AnnotatedForm = {binary_op, Context#{left => AnnotatedLeft, right => AnnotatedRight}},
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, Globals, Locals, [{identifier, Context}|T]) ->
    AnnotatedForm = {identifier, Context#{locals => Locals}},
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, Globals, Locals, [Form = {call, _Context}|T]) ->
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = rufus_form:annotate(Form, type, TypeForm),
            typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
        Error ->
            throw(Error)
    end;
typecheck_and_annotate(Acc, Globals, Locals, [H|T]) ->
    typecheck_and_annotate([H|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, _Globals, Locals, []) ->
    {ok, Locals, lists:reverse(Acc)}.
