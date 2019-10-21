%% rufus_compile_erlang transforms Rufus's abstract form into Erlang's abstract
%% form.
-module(rufus_compile_erlang).

-include_lib("rufus_type.hrl").

%% API exports

-export([forms/1]).

%% API

%% forms transforms RufusForms into Erlang forms that can be compiled with
%% compile:forms/1 and then loaded with code:load_binary/3.
-spec forms(list(rufus_form())) -> {ok, list(erlang_form())}.
forms(RufusForms) ->
    forms([], RufusForms).

%% Private API

-spec forms(list(erlang_form()), list(rufus_form())) -> {ok, list(erlang_form())}.
forms(Acc, [{module, #{line := Line, spec := Name}}|T]) ->
    Form = {attribute, Line, module, Name},
    forms([Form|Acc], T);
forms(Acc, [{atom_lit, _Context} = AtomLit|T]) ->
    Form = box(AtomLit),
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
    TypeSpec = rufus_form:spec(Type),
    Form = case TypeSpec of
        atom ->
            {var, Line, Name};
        float ->
            {var, Line, Name};
        int ->
            {var, Line, Name};
        _ ->
            {tuple, Line, [{atom, Line, TypeSpec}, {var, Line, Name}]}
    end,
    forms([Form|Acc], T);
forms(Acc, [{func_decl, #{line := Line, spec := Name, args := Args, exprs := Exprs}}|T]) ->
    {ok, ArgsForms} = forms([], Args),
    {ok, GuardForms} = guard_forms([], Args),
    {ok, ExprForms} = forms([], Exprs),
    FunctionForms = [{clause, Line, ArgsForms, GuardForms, ExprForms}],
    ExportForms = {attribute, Line, export, [{Name, length(Args)}]},
    Forms = {function, Line, Name, length(Args), FunctionForms},
    forms([Forms|[ExportForms|Acc]], T);
forms(Acc, [Form = {arg_decl, #{line := Line, spec := Name}}|T]) ->
    TypeSpec = rufus_form:type_spec(Form),
    ErlangForm = case TypeSpec of
        atom ->
            {var, Line, Name};
        float ->
            {var, Line, Name};
        int ->
            {var, Line, Name};
        _ ->
            {tuple, Line, [{atom, Line, TypeSpec}, {var, Line, Name}]}
    end,
    forms([ErlangForm|Acc], T);
forms(Acc, [{binary_op, #{line := Line, op := Op, left := Left, right := Right}}|T]) ->
    {ok, [LeftExpr]} = forms([], [Left]),
    {ok, [RightExpr]} = forms([], [Right]),
    ErlangOp = rufus_operator_to_erlang_operator(Op, rufus_form:type_spec(Left)),
    Form = {op, Line, ErlangOp, LeftExpr, RightExpr},
    forms([Form|Acc], T);
forms(Acc, [{type, _Context}|T]) ->
    forms(Acc, T); %% no-op to satisfy Dialyzer
forms(Acc, [{apply, _Context}|T]) ->
    forms(Acc, T); %% no-op to satisfy Dialyzer
forms(Acc, []) ->
    {ok, lists:reverse(Acc)};
forms(Acc, Form) ->
    erlang:error(unhandled_form, [Acc, Form]).

rufus_operator_to_erlang_operator('/', float) -> '/';
rufus_operator_to_erlang_operator('/', int) -> 'div';
rufus_operator_to_erlang_operator('%', int) -> 'rem';
rufus_operator_to_erlang_operator('%', float) -> erlang:error(unsupported_operand_type, ['%', float]);
rufus_operator_to_erlang_operator(Op, _) -> Op.

% guard_forms generates function guard_forms for floats and integers.
-spec guard_forms(list(erlang_form()) | list(list()), list(arg_decl_form())) -> {ok, list(erlang_form())}.
guard_forms(Acc, [{arg_decl, #{line := Line, spec := Name, type := {type, #{spec := atom}}}}|T]) ->
    GuardExpr = [{call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, is_atom}}, [{var, Line, Name}]}],
    guard_forms([GuardExpr|Acc], T);
guard_forms(Acc, [{arg_decl, #{line := Line, spec := Name, type := {type, #{spec := float}}}}|T]) ->
    GuardExpr = [{call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, is_float}}, [{var, Line, Name}]}],
    guard_forms([GuardExpr|Acc], T);
guard_forms(Acc, [{arg_decl, #{line := Line, spec := Name, type := {type, #{spec := int}}}}|T]) ->
    GuardExpr = [{call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, is_integer}}, [{var, Line, Name}]}],
    guard_forms([GuardExpr|Acc], T);
guard_forms(Acc, [_|T]) ->
    guard_forms(Acc, T);
guard_forms(Acc, []) ->
    %% TODO(jkakar): Should we be reversing Acc here? Does ordering affect guard
    %% behavior?
    {ok, Acc}.

-spec box(atom_lit_form() | bool_lit_form() | float_lit_form() | int_lit_form() | string_lit_form()) -> erlang3_form().
box({atom_lit, #{line := Line, spec := Value}}) ->
    {atom, Line, Value};
box({bool_lit, #{line := Line, spec := Value}}) ->
    {tuple, Line, [{atom, Line, bool}, {atom, Line, Value}]};
box({float_lit, #{line := Line, spec := Value}}) ->
    {float, Line, Value};
box({int_lit, #{line := Line, spec := Value}}) ->
    {integer, Line, Value};
box({string_lit, #{line := Line, spec := Value}}) ->
    StringExpr = {bin_element, Line, {string, Line, binary_to_list(Value)}, default, default},
    {tuple, Line, [{atom, Line, string}, {bin, Line, [StringExpr]}]}.
