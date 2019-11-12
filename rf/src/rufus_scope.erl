%% rufus_scope calculates visibility into global names and maps them to Rufus
%% forms. It also annotates Rufus forms representing variables with type
%% information.
-module(rufus_scope).

-include_lib("rufus_type.hrl").

%% API exports

-export([
    annotate_locals/1,
    globals/1
]).

%% API

%% annotate_locals adds a type form to each identifier form in RufusForms.
-spec annotate_locals(list(rufus_form())) -> {ok, list(rufus_form())}.
annotate_locals(RufusForms) ->
    {ok, _Locals, AnnotatedForms} = annotate_locals([], #{}, RufusForms),
    {ok, AnnotatedForms}.

%% globals creates a map of function names to func_decl forms for all top-level
%% functions in RufusForms.
-spec globals(list(rufus_form())) -> {ok, #{atom() => list(rufus_form())}}.
globals(RufusForms) ->
    globals(#{}, RufusForms).

%% Private API

-spec globals(map(), list(rufus_form())) -> {ok, #{atom() => list(rufus_form())}}.
globals(Acc, [Form = {func_decl, #{spec := Spec}}|T]) ->
    Forms = maps:get(Spec, Acc, []),
    globals(Acc#{Spec => Forms ++ [Form]}, T);
globals(Acc, [_H|T]) ->
    globals(Acc, T);
globals(Acc, []) ->
    {ok, Acc}.

-spec annotate_locals(list(rufus_form()), locals(), list(rufus_form())) -> {ok, list(rufus_form())}.
annotate_locals(Acc, Locals, [{func_decl, Context = #{params := Params, exprs := Exprs}}|T]) ->
    {ok, NewLocals1, AnnotatedParams} = annotate_locals([], Locals, Params),
    {ok, NewLocals2, AnnotatedExprs} = annotate_locals([], NewLocals1, Exprs),
    AnnotatedForm = {func_decl, Context#{params => AnnotatedParams, exprs => AnnotatedExprs}},
    annotate_locals([AnnotatedForm|Acc], NewLocals2, T);
annotate_locals(Acc, Locals, [{param, Context = #{spec := Spec, type := Type}}|T]) ->
    NewLocals = Locals#{Spec => Type},
    AnnotatedForm = {param, Context},
    annotate_locals([AnnotatedForm|Acc], NewLocals, T);
annotate_locals(Acc, Locals, [{binary_op, Context = #{left := Left, right := Right}}|T]) ->
    {ok, Locals, [AnnotatedLeft]} = annotate_locals([], Locals, [Left]),
    {ok, Locals, [AnnotatedRight]} = annotate_locals([], Locals, [Right]),
    AnnotatedForm = {binary_op, Context#{left => AnnotatedLeft, right => AnnotatedRight}},
    annotate_locals([AnnotatedForm|Acc], Locals, T);
annotate_locals(Acc, Locals, [{identifier, Context}|T]) ->
    AnnotatedForm = {identifier, Context#{locals => Locals}},
    annotate_locals([AnnotatedForm|Acc], Locals, T);
annotate_locals(Acc, Locals, [H|T]) ->
    annotate_locals([H|Acc], Locals, T);
annotate_locals(Acc, Locals, []) ->
    {ok, Locals, lists:reverse(Acc)}.
