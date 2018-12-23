%% rufus_erlang_compiler transforms Rufus's abstract form into Erlang's abstract
%% form.
-module(rufus_erlang_compiler).

%% API exports

-export([forms/1]).

%% API

forms(Forms) ->
    ErlangForms = lists:reverse(forms([], Forms)),
    {ok, ErlangForms}.

%% Private API

forms(Acc, [{expr, LineNumber, {float, Value}}|T]) ->
    Form = {clause,LineNumber,[],[],[{float, LineNumber, Value}]},
    forms([Form|Acc], T);
forms(Acc, [{expr, LineNumber, {int, Value}}|T]) ->
    Form = {clause,LineNumber,[],[],[{integer, LineNumber, Value}]},
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
