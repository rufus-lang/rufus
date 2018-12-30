%% rufus_erlang_compiler transforms Rufus's abstract form into Erlang's abstract
%% form.
-module(rufus_erlang_compiler).

%% API exports

-export([forms/1]).

%% API

forms(RufusForms) ->
    ErlangForms = lists:reverse(forms([], RufusForms)),
    {ok, ErlangForms}.

%% Private API

forms(Acc, [{expr, LineNumber, {float, Value}}|T]) ->
    Form = {clause, LineNumber, [], [], [box({float, LineNumber, Value})]},
    forms([Form|Acc], T);
forms(Acc, [{expr, LineNumber, {int, Value}}|T]) ->
    Form = {clause, LineNumber, [], [], [box({integer, LineNumber, Value})]},
    forms([Form|Acc], T);
forms(Acc, [{expr, LineNumber, {string, Value}}|T]) ->
    StringExpr = {bin_element, LineNumber, {string, LineNumber, Value}, default, default},
    Form = {clause, LineNumber, [], [], [box({bin, LineNumber, [StringExpr]})]},
    forms([Form|Acc], T);
forms(Acc, [{func, LineNumber, Name, _Args, _ReturnType, Exprs}|T]) ->
    ExprForms = lists:reverse(forms([], Exprs)),
    ExportForms = {attribute, LineNumber, export, [{list_to_atom(Name), 0}]},
    Forms = {function, LineNumber, list_to_atom(Name), 0, ExprForms},
    forms([Forms|[ExportForms|Acc]], T);
forms(Acc, [{package, LineNumber, Name}|T]) ->
    Form = {attribute, LineNumber, module, list_to_atom(Name)},
    forms([Form|Acc], T);
forms(Acc, []) ->
    Acc.

%% box converts Rufus types into Erlang `{<type>, <value>}` 2-tuples, such as
%% turning `3.14159265359` into `{float, 3.14159265359}`, for example.
box(Expr = {bin, LineNumber, _Value}) ->
    {tuple, LineNumber, [{atom, LineNumber, string}, Expr]};
box(Expr = {float, LineNumber, _Value}) ->
    {tuple, LineNumber, [{atom, LineNumber, float}, Expr]};
box(Expr = {integer, LineNumber, _Value}) ->
    {tuple, LineNumber, [{atom, LineNumber, int}, Expr]}.
