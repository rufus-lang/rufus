%% rufus_annotate_globals adds a 'globals' map to each Rufus form that links
%% variable names to type information.
-module(rufus_annotate_globals).

-include_lib("rufus_type.hrl").

%% API exports

-export([forms/1]).

%% API

%% forms adds a 'globals' map to each form in RufusForms that links variable
%% names to type information.
-spec forms(list(rufus_form())) -> {ok, list(rufus_form())}.
forms(RufusForms) ->
    {ok, Globals} = globals(#{}, RufusForms),
    {ok, Globals, RufusForms}.

%% Private API

-spec globals(globals(), list(rufus_form())) -> {ok, globals()}.
globals(Globals, [H|T]) ->
    {ok, NewGlobals} = annotate_form(Globals, H),
    globals(NewGlobals, T);
globals(Globals, []) ->
    {ok, Globals}.

-spec annotate_form(map(), func_form()) -> {ok, map(), func_form()}.
annotate_form(Globals, {func, Metadata = #{args := Args, exprs := Exprs}}) ->
    % walk over functions args and add locals to the context
    {ok, NewGlobals1, NewArgs} = annotate_func_args(Globals, Args),
    % walk over exprs and add locals to the context
    {ok, NewGlobals2, NewExprs} = annotate_func_exprs(NewGlobals1, Exprs),
    AnnotatedForm = {func, Metadata#{args => NewArgs, exprs => NewExprs}},
    {ok, NewGlobals2, AnnotatedForm};
annotate_form(Globals, Form) ->
    {ok, Globals, Form}.

-spec annotate_func_args(locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_args(Globals, Args) ->
    annotate_func_args(Globals, [], Args).

-spec annotate_func_args(locals(), list(arg_form()), list(arg_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_args(Globals, Acc, [{arg, Metadata = #{spec := Spec, type := Type}}|T]) ->
    NewGlobals = Globals#{Spec => Type},
    NewArg = {arg, Metadata},
    annotate_func_args(NewGlobals, [NewArg|Acc], T);
annotate_func_args(Globals, Acc, []) ->
    {ok, Globals, lists:reverse(Acc)}.

-spec annotate_func_exprs(locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_exprs(Globals, Exprs) ->
    annotate_func_exprs(Globals, [], Exprs).

-spec annotate_func_exprs(locals(), list(rufus_form()), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
annotate_func_exprs(Globals, Acc, [{FormType, Metadata}|T]) ->
    NewMetadata = Metadata#{locals => Globals},
    annotate_func_exprs(Globals, [{FormType, NewMetadata}|Acc], T);
annotate_func_exprs(Globals, Acc, []) ->
    {ok, Globals, lists:reverse(Acc)}.
