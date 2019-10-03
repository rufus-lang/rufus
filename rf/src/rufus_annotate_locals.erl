%% rufus_annotate_locals adds a 'locals' map to each Rufus form that links
%% variable names to type information.
-module(rufus_annotate_locals).

-include_lib("rufus_type.hrl").

%% API exports

-export([forms/1]).

%% API

%% forms adds a 'locals' map to each form in RufusForms that links variable
%% names to type information.
-spec forms(list(rufus_form())) -> {ok, list(rufus_form())}.
forms(RufusForms) ->
    forms([], #{}, RufusForms).

%% Private API

-spec forms(list(rufus_form()), locals(), list(rufus_form())) -> {ok, list(rufus_form())}.
forms(Acc, Locals, [H|T]) ->
    {ok, NewLocals, NewForm} = annotate_form(Locals, H),
    forms([NewForm|Acc], NewLocals, T);
forms(Acc, _Locals, []) ->
    {ok, lists:reverse(Acc)}.

-spec annotate_form(map(), func_form()) -> {ok, map(), func_form()}.
annotate_form(Locals, {func_decl, Metadata = #{args := Args, exprs := Exprs}}) ->
    % walk over functions args and add locals to the context
    {ok, NewLocals1, NewArgs} = annotate_func_args(Locals, Args),
    % walk over exprs and add locals to the context
    {ok, NewLocals2, NewExprs} = annotate_func_exprs(NewLocals1, Exprs),
    AnnotatedForm = {func_decl, Metadata#{args => NewArgs, exprs => NewExprs}},
    {ok, NewLocals2, AnnotatedForm};
annotate_form(Locals, Form) ->
    {ok, Locals, Form}.

-spec annotate_func_args(locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_args(Locals, Args) ->
    annotate_func_args(Locals, [], Args).

-spec annotate_func_args(locals(), list(arg_form()), list(arg_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_args(Locals, Acc, [{arg, Metadata = #{spec := Spec, type := Type}}|T]) ->
    NewLocals = Locals#{Spec => Type},
    NewArg = {arg, Metadata},
    annotate_func_args(NewLocals, [NewArg|Acc], T);
annotate_func_args(Locals, Acc, []) ->
    {ok, Locals, lists:reverse(Acc)}.

-spec annotate_func_exprs(locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_exprs(Locals, Exprs) ->
    annotate_func_exprs(Locals, [], Exprs).

-spec annotate_func_exprs(locals(), list(rufus_form()), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_exprs(Locals, Acc, [{FormType, Metadata}|T]) ->
    NewMetadata = Metadata#{locals => Locals},
    annotate_func_exprs(Locals, [{FormType, NewMetadata}|Acc], T);
annotate_func_exprs(Locals, Acc, []) ->
    {ok, Locals, lists:reverse(Acc)}.
