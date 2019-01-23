%% rufus_compile_erlang transforms Rufus's abstract form into Erlang's abstract
%% form.
-module(rufus_compile_erlang).

%% API exports

-export([forms/1]).

%% API

%% forms transforms RufusForms into Erlang forms that can be compiled with
%% compile:forms/1 and then loaded with code:load_binary/3.
forms(RufusForms) ->
    ErlangForms = lists:reverse(forms([], RufusForms)),
    {ok, ErlangForms}.

%% Private API

forms(Acc, [{arg, #{line := Line, spec := Name, type := Type}}|T]) ->
    Form = {tuple, Line, [{atom, Line, Type}, {var, Line, Name}]},
    forms([Form|Acc], T);
forms(Acc, [{expr, Line, {bool, Value}}|T]) ->
    Form = box({bool, Line, Value}),
    forms([Form|Acc], T);
forms(Acc, [{expr, Line, {float, Value}}|T]) ->
    Form = box({float, Line, Value}),
    forms([Form|Acc], T);
forms(Acc, [{expr, Line, {int, Value}}|T]) ->
    Form = box({integer, Line, Value}),
    forms([Form|Acc], T);
forms(Acc, [{expr, Line, {string, Value}}|T]) ->
    StringExpr = {bin_element, Line, {string, Line, Value}, default, default},
    Form = box({bin, Line, [StringExpr]}),
    forms([Form|Acc], T);
forms(Acc, [{expr, Line, {identifier, Name}}|T]) ->
    Form = {var, Line, Name},
    forms([Form|Acc], T);
forms(Acc, [{func, Line, Name, Args, _ReturnType, Exprs}|T]) ->
    ArgsForms = forms([], Args),
    ExprForms = lists:reverse(forms([], Exprs)),
    FunctionForms = [{clause, Line, ArgsForms, [], ExprForms}],
    ExportForms = {attribute, Line, export, [{list_to_atom(Name), length(Args)}]},
    Forms = {function, Line, list_to_atom(Name), length(Args), FunctionForms},
    forms([Forms|[ExportForms|Acc]], T);
forms(Acc, [{module, #{line := Line, spec := Name}}|T]) ->
    Form = {attribute, Line, module, Name},
    forms([Form|Acc], T);
forms(Acc, []) ->
    Acc.

%% box converts Rufus types into Erlang `{<type>, <value>}` 2-tuples, such as
%% turning `3.14159265359` into `{float, 3.14159265359}`, for example.
box(Expr = {bin, Line, _Value}) ->
    {tuple, Line, [{atom, Line, string}, Expr]};
box({bool, Line, Value}) ->
    {tuple, Line, [{atom, Line, bool}, {atom, Line, Value}]};
box(Expr = {float, Line, _Value}) ->
    {tuple, Line, [{atom, Line, float}, Expr]};
box(Expr = {integer, Line, _Value}) ->
    {tuple, Line, [{atom, Line, int}, Expr]}.
