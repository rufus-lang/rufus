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
    annotate_locals([], #{}, RufusForms).

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
annotate_locals(Acc, Locals, [H|T]) ->
    {ok, NewLocals, NewForm} = annotate_form(Locals, H),
    annotate_locals([NewForm|Acc], NewLocals, T);
annotate_locals(Acc, _Locals, []) ->
    {ok, lists:reverse(Acc)}.

-spec annotate_form(map(), func_decl_form()) -> {ok, map(), func_decl_form()}.
annotate_form(Locals, {func_decl, Context = #{params := Params, exprs := Exprs}}) ->
    % walk over functions parameters and add locals to the context
    {ok, NewLocals1, NewParams} = annotate_func_params(Locals, Params),
    % walk over exprs and add locals to the context
    {ok, NewLocals2, NewExprs} = annotate_func_exprs(NewLocals1, Exprs),
    AnnotatedForm = {func_decl, Context#{params => NewParams, exprs => NewExprs}},
    {ok, NewLocals2, AnnotatedForm};
annotate_form(Locals, {binary_op, Context = #{left := {LeftFormName, LeftContext}, right := {RightFormName, RightContext}}}) ->
    io:format("WTF?~n"),
    AnnotatedLeft = {LeftFormName, LeftContext#{locals => Locals}},
    AnnotatedRight ={RightFormName,  RightContext#{locals => Locals}},
    AnnotatedForm = {binary_op, Context#{left => AnnotatedLeft, right => AnnotatedRight}},
    {ok, Locals, AnnotatedForm};
annotate_form(Locals, {identifier, Context}) ->
    AnnotatedForm = {identifier, Context#{locals => Locals}},
    {ok, Locals, AnnotatedForm};
annotate_form(Locals, Form) ->
    {ok, Locals, Form}.

-spec annotate_func_params(locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_params(Locals, Params) ->
    annotate_func_params(Locals, [], Params).

-spec annotate_func_params(locals(), list(param_form()), list(param_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_params(Locals, Acc, [{param, Context = #{spec := Spec, type := Type}}|T]) ->
    NewLocals = Locals#{Spec => Type},
    NewParam = {param, Context},
    annotate_func_params(NewLocals, [NewParam|Acc], T);
annotate_func_params(Locals, Acc, []) ->
    {ok, Locals, lists:reverse(Acc)}.

-spec annotate_func_exprs(locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_exprs(Locals, Exprs) ->
    annotate_func_exprs(Locals, [], Exprs).

-spec annotate_func_exprs(locals(), list(rufus_form()), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_exprs(Locals, Acc, [Form|T]) ->
    io:format("annotate_form(Locals, Form) => annotate_form(~p, ~p)~n", [Locals, Form]),
    io:format("Form => ~p~n", [Form]),
    io:format("Locals => ~p~n", [Locals]),
    {ok, NewLocals, AnnotatedForm} = annotate_form(Locals, Form),
    io:format("AnnotatedForm => ~p~n", [AnnotatedForm]),
    io:format("NewLocals => ~p~n", [NewLocals]),
    annotate_func_exprs(NewLocals, [AnnotatedForm|Acc], T);
annotate_func_exprs(Locals, Acc, []) ->
    {ok, Locals, lists:reverse(Acc)}.
