%% rufus_scope calculates visibility into global names and maps them to Rufus
%% forms.
-module(rufus_scope).

-include_lib("rufus_type.hrl").

%% API exports

-export([
    annotate_locals/1,
    globals/1
]).

%% API

%% annotate_locals adds a 'locals' map to each form in RufusForms that links
%% variable names to type information.
-spec annotate_locals(list(rufus_form())) -> {ok, list(rufus_form())}.
annotate_locals(RufusForms) ->
    annotate([], #{}, RufusForms).

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

-spec annotate(list(rufus_form()), locals(), list(rufus_form())) -> {ok, list(rufus_form())}.
annotate(Acc, Locals, [H|T]) ->
    {ok, NewLocals, NewForm} = annotate_form(Locals, H),
    annotate([NewForm|Acc], NewLocals, T);
annotate(Acc, _Locals, []) ->
    {ok, lists:reverse(Acc)}.

-spec annotate_form(map(), func_decl_form()) -> {ok, map(), func_decl_form()}.
annotate_form(Locals, {func_decl, Context = #{args := Args, exprs := Exprs}}) ->
    % walk over functions args and add locals to the context
    {ok, NewLocals1, NewArgs} = annotate_func_args(Locals, Args),
    % walk over exprs and add locals to the context
    {ok, NewLocals2, NewExprs} = annotate_func_exprs(NewLocals1, Exprs),
    AnnotatedForm = {func_decl, Context#{args => NewArgs, exprs => NewExprs}},
    {ok, NewLocals2, AnnotatedForm};
annotate_form(Locals, Form) ->
    {ok, Locals, Form}.

-spec annotate_func_args(locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_args(Locals, Args) ->
    annotate_func_args(Locals, [], Args).

-spec annotate_func_args(locals(), list(arg_decl_form()), list(arg_decl_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_args(Locals, Acc, [{arg_decl, Context = #{spec := Spec, type := Type}}|T]) ->
    NewLocals = Locals#{Spec => Type},
    NewArg = {arg_decl, Context},
    annotate_func_args(NewLocals, [NewArg|Acc], T);
annotate_func_args(Locals, Acc, []) ->
    {ok, Locals, lists:reverse(Acc)}.

-spec annotate_func_exprs(locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_exprs(Locals, Exprs) ->
    annotate_func_exprs(Locals, [], Exprs).

-spec annotate_func_exprs(locals(), list(rufus_form()), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_exprs(Locals, Acc, [{FormType, Context}|T]) ->
    NewContext = Context#{locals => Locals},
    annotate_func_exprs(Locals, [{FormType, NewContext}|Acc], T);
annotate_func_exprs(Locals, Acc, []) ->
    {ok, Locals, lists:reverse(Acc)}.
