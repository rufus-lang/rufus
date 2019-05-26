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

forms(Acc, [{module, #{line := Line, spec := Name}}|T]) ->
    Form = {attribute, Line, module, Name},
    forms([Form|Acc], T);
forms(Acc, [{bool_lit, _Context} = BoolLit|T]) ->
    Form = box(BoolLit),
    forms([Form|Acc], T);
forms(Acc, [{float_lit, _Context} = FloatLit|T]) ->
    Form = box(FloatLit),
    forms([Form|Acc], T);
forms(Acc, [{int_lit, _Context} = IntLit|T]) ->
    Form = box(IntLit),
    forms([Form|Acc], T);
forms(Acc, [{string_lit, _Context} = StringLit|T]) ->
    Form = box(StringLit),
    forms([Form|Acc], T);
forms(Acc, [{identifier, #{line := Line, spec := Name, locals := Locals}}|T]) ->
    Type = maps:get(Name, Locals),
    Form = case type_spec(Type) of
        float ->
            {var, Line, Name};
        int ->
            {var, Line, Name};
        _ ->
            {tuple, Line, [{atom, Line, type_spec(Type)}, {var, Line, Name}]}
    end,
    forms([Form|Acc], T);
forms(Acc, [{func, #{line := Line, spec := Name, args := Args, exprs := Exprs}}|T]) ->
    ArgsForms = forms([], Args),
    GuardForms = guards([], Args),
    ExprForms = lists:reverse(forms([], Exprs)),
    FunctionForms = [{clause, Line, ArgsForms, GuardForms, ExprForms}],
    ExportForms = {attribute, Line, export, [{Name, length(Args)}]},
    Forms = {function, Line, Name, length(Args), FunctionForms},
    forms([Forms|[ExportForms|Acc]], T);
forms(Acc, [{arg, #{line := Line, spec := Name, type := Type}}|T]) ->
    Form = case type_spec(Type) of
        float ->
            {var, Line, Name};
        int ->
            {var, Line, Name};
        _ ->
            {tuple, Line, [{atom, Line, type_spec(Type)}, {var, Line, Name}]}
    end,
    %% Form = {tuple, Line, [{atom, Line, type_spec(Type)}, {var, Line, Name}]},
    forms([Form|Acc], T);
forms(Acc, [{binary_op, #{line := Line, op := Op, left := Left, right := Right}}|T]) ->
    [LeftExpr] = forms([], Left),
    [RightExpr] = forms([], Right),
    Form = {op, Line, Op, LeftExpr, RightExpr},
    forms([Form|Acc], T);
forms(Acc, []) ->
    Acc;
forms(Acc, _Unhandled) ->
    io:format("unhandled form ->~n~p~n", [_Unhandled]),
    Acc.

% guards generates function guards for floats and integers.
guards(Acc, [{arg, #{line := Line, spec := Name, type := {float, _}}}|T]) ->
    GuardExpr = {call, Line, {remote, Line, erlang, is_float, [{float, Line, Name}]}},
    guards([GuardExpr|Acc], T);
guards(Acc, [{arg, #{line := Line, spec := Name, type := {int, _}}}|T]) ->
    GuardExpr = {call, Line, {remote, Line, erlang, is_integer, [{integer, Line, Name}]}},
    guards([GuardExpr|Acc], T);
guards(Acc, [_|T]) ->
    guards(Acc, T);
guards(Acc, []) ->
    Acc.

%% box converts Rufus forms for primitive values into Erlang forms.
box({bool_lit, #{line := Line, spec := Value}}) ->
    {tuple, Line, [{atom, Line, bool}, {atom, Line, Value}]};
box({float_lit, #{line := Line, spec := Value}}) ->
    {float, Line, Value};
box({int_lit, #{line := Line, spec := Value}}) ->
    {integer, Line, Value};
box({string_lit, #{line := Line, spec := Value}}) ->
    StringExpr = {bin_element, Line, {string, Line, binary_to_list(Value)}, default, default},
    {tuple, Line, [{atom, Line, string}, {bin, Line, [StringExpr]}]}.

type_spec({type, #{spec := Type}}) ->
    Type.
