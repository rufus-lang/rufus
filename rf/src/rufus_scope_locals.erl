%% rufus_scope_locals annotates each Rufus form with a 'locals' map that links
%% variable names to type information.
-module(rufus_scope_locals).

%% API exports

-export([forms/1]).

%% API

%% forms adds a 'locals' map to each form in RufusForms that links variable
%% names to type information.
forms(RufusForms) ->
    forms([], #{}, RufusForms).

%% Private API

forms(Acc, Locals, [H|T]) ->
    case annotate_form(Locals, H) of
        {ok, NewLocals, NewForm} ->
            forms([NewForm|Acc], NewLocals, T);
        Error ->
            Error
    end;
forms(Acc, _Locals, []) ->
    {ok, lists:reverse(Acc)}.

annotate_form(Locals, {func, Metadata = #{args := Args, exprs := Exprs}}) ->
    % walk over functions args and add locals to the context
    {ok, NewLocals1, NewArgs} = annotate_func_args(Locals, Args),
    % walk over exprs and add locals to the context
    {ok, NewLocals2, NewExprs} = annotate_func_exprs(NewLocals1, Exprs),
    io:format("Args => ~p~nLocals => ~p~nNewLocals1 => ~p~nNewLocals2 => ~p~nExprs => ~p~n", [Args, Locals, NewLocals1, NewLocals2, Exprs]),
    AnnotatedForm = {func, Metadata#{args => NewArgs, exprs => NewExprs}},
    {ok, NewLocals2, AnnotatedForm};
annotate_form(Locals, Form) ->
    {ok, Locals, Form}.

annotate_func_args(Locals, Args) ->
    annotate_func_args(Locals, [], Args).
annotate_func_args(Locals, Acc, [{arg, Metadata = #{spec := Spec, type := Type}}|T]) ->
    NewLocals = Locals#{Spec => Type},
    NewArg = {arg, Metadata},
    annotate_func_args(NewLocals, [NewArg|Acc], T);
annotate_func_args(Locals, Acc, []) ->
    {ok, Locals, lists:reverse(Acc)}.

annotate_func_exprs(Locals, Exprs) ->
    annotate_func_exprs(Locals, [], Exprs).
annotate_func_exprs(Locals, Acc, [{FormType, Metadata}|T]) ->
    NewMetadata = Metadata#{locals => Locals},
    annotate_func_exprs(Locals, [{FormType, NewMetadata}|Acc], T);
annotate_func_exprs(Locals, Acc, []) ->
    {ok, Locals, lists:reverse(Acc)}.
