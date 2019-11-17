%% rufus_erlang transforms Rufus abstract forms into Erlang abstract forms.
-module(rufus_erlang).

-include_lib("rufus_type.hrl").

%% API exports

-export([forms/1]).

%% API

%% forms transforms RufusForms into Erlang forms that can be compiled with
%% compile:forms/1 and then loaded with code:load_binary/3.
-spec forms(list(rufus_form())) -> {ok, list(erlang_form())}.
forms(RufusForms) ->
    {ok, ErlangForms} = forms([], RufusForms),
    annotate_exports(ErlangForms).

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
forms(Acc, [{func, #{line := Line, spec := Spec, params := Params, exprs := Exprs}}|T]) ->
    {ok, ParamForms} = forms([], Params),
    {ok, GuardForms} = guard_forms([], Params),
    {ok, ExprForms} = forms([], Exprs),
    FunctionForms = [{clause, Line, ParamForms, GuardForms, ExprForms}],
    Form = {function, Line, Spec, length(Params), FunctionForms},
    forms([Form|Acc], T);
forms(Acc, [Form = {param, #{line := Line, spec := Name}}|T]) ->
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
forms(Acc, [{call, #{spec := Spec, args := Args, line := Line}}|T]) ->
    {ok, ArgsForms} = forms([], Args),
    Form = {call, Line, {atom, Line, Spec}, ArgsForms},
    forms([Form|Acc], T);
forms(Acc, [{binary_op, #{line := Line, op := Op, left := Left, right := Right}}|T]) ->
    {ok, [LeftExpr]} = forms([], [Left]),
    {ok, [RightExpr]} = forms([], [Right]),
    ErlangOp = rufus_operator_to_erlang_operator(Op, rufus_form:type_spec(Left)),
    Form = {op, Line, ErlangOp, LeftExpr, RightExpr},
    forms([Form|Acc], T);
forms(Acc, [{type, _Context}|T]) ->
    forms(Acc, T); %% no-op to satisfy Dialyzer
forms(Acc, [{match, _Context}|T]) ->
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
-spec guard_forms(list(erlang_form()) | list(list()), list(param_form())) -> {ok, list(erlang_form())}.
guard_forms(Acc, [{param, #{line := Line, spec := Name, type := {type, #{spec := atom}}}}|T]) ->
    GuardExpr = [{call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, is_atom}}, [{var, Line, Name}]}],
    guard_forms([GuardExpr|Acc], T);
guard_forms(Acc, [{param, #{line := Line, spec := Name, type := {type, #{spec := float}}}}|T]) ->
    GuardExpr = [{call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, is_float}}, [{var, Line, Name}]}],
    guard_forms([GuardExpr|Acc], T);
guard_forms(Acc, [{param, #{line := Line, spec := Name, type := {type, #{spec := int}}}}|T]) ->
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

%% annotate_exports creates export attributes for all exported functions and
%% injects them into the sequence of Erlang forms. They're defined before
%% function definitions to avoid crashing the Erlang compiler.
-spec annotate_exports(list(erlang_form())) -> {ok, list(erlang_form())}.
annotate_exports(Forms) ->
    annotate_exports([], Forms).

-spec annotate_exports(list(erlang_form()), list(erlang_form())) -> {ok, list(erlang_form())}.
annotate_exports(Acc, [Form = {attribute, _Line, module, _Name}|T]) ->
    {ok, ExportForms} = make_export_forms(T),
    annotate_exports(Acc ++ ExportForms ++ [Form], T);
annotate_exports(Acc, [Form|T]) ->
    annotate_exports([Form|Acc], T);
annotate_exports(Acc, []) ->
    {ok, lists:reverse(Acc)}.

-spec make_export_forms(list(erlang_form())) -> {ok, list(export_attribute_erlang_form())}.
make_export_forms(Forms) ->
    make_export_forms([], Forms).

-spec make_export_forms(list(erlang_form()), list(erlang_form())) -> {ok, list(export_attribute_erlang_form())}.
make_export_forms(Acc, [{function, Line, Spec, Arity, _Forms}|T]) ->
    %% TODO(jkakar) We're exporting all functions for now, to move quickly, but
    %% we need to apply the rules from the 'Exported identifiers' RDR here.
    Form = {attribute, Line, export, [{Spec, Arity}]},
    make_export_forms([Form|Acc], T);
make_export_forms(Acc, [_Form|T]) ->
    make_export_forms(Acc, T);
make_export_forms(Acc, []) ->
    {ok, lists:reverse(Acc)}.
