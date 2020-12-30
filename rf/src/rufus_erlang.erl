%% rufus_erlang transforms Rufus abstract forms into Erlang abstract forms.
-module(rufus_erlang).

-include_lib("rufus_type.hrl").

%% API exports

-export([forms/1]).

%% API

%% forms transforms RufusForms into Erlang forms that can be compiled with
%% compile:forms/1 and then loaded with code:load_binary/3.
-spec forms(list(module_form() | func_form())) -> {ok, list(erlang_form())}.
forms(RufusForms) ->
    {ok, GroupedRufusForms} = group_forms_by_func(RufusForms),
    {ok, ErlangForms} = forms([], GroupedRufusForms),
    annotate_exports(ErlangForms).

%% Private API

%% group_forms_by_func transforms a list of Rufus forms with individual entries
%% for func expressions of the same name and arity into a list of func_group
%% forms, one per name/arity pair.
-spec group_forms_by_func(rufus_forms()) -> {ok, list(module_form() | {func_group, context()})}.
group_forms_by_func(Forms) ->
    MatchModuleForm = fun
        ({module, _Context}) ->
            true;
        (_) ->
            false
    end,
    {value, ModuleForm} = lists:search(MatchModuleForm, Forms),
    {ok, FuncGroups} = group_forms_by_func(#{}, Forms),
    {ok, [ModuleForm | FuncGroups]}.

-spec group_forms_by_func(map(), rufus_forms()) ->
    {ok, list(module_form() | {func_group, context()})}.
group_forms_by_func(Acc, [{module, _Context} | T]) ->
    group_forms_by_func(Acc, T);
group_forms_by_func(Acc, [Form = {func, #{line := Line, spec := Spec, params := Params}} | T]) ->
    Arity = length(Params),
    FuncGroupSpec = list_to_atom(
        unicode:characters_to_list([atom_to_list(Spec), "/", integer_to_list(Arity)])
    ),
    {func_group, Context = #{forms := Forms}} = maps:get(
        FuncGroupSpec,
        Acc,
        {func_group, #{line => Line, spec => Spec, arity => length(Params), forms => []}}
    ),
    NewFuncGroup = {func_group, Context#{forms => [Form | Forms]}},
    group_forms_by_func(maps:put(FuncGroupSpec, NewFuncGroup, Acc), T);
group_forms_by_func(Acc, []) ->
    {ok, maps:values(Acc)}.

-spec forms(list(erlang_form()), list(rufus_form() | {func_group, context()})) ->
    {ok, list(erlang_form())}.
forms(Acc, [{atom_lit, _Context} = AtomLit | T]) ->
    Form = box(AtomLit),
    forms([Form | Acc], T);
forms(Acc, [{binary_op, #{line := Line, op := Op, left := Left, right := Right}} | T]) ->
    {ok, [LeftForm]} = forms([], [Left]),
    {ok, [RightForm]} = forms([], [Right]),
    ErlangOp = rufus_operator_to_erlang_operator(Op, rufus_form:type_spec(Left)),
    Form = {op, Line, ErlangOp, LeftForm, RightForm},
    forms([Form | Acc], T);
forms(Acc, [{bool_lit, _Context} = BoolLit | T]) ->
    Form = box(BoolLit),
    forms([Form | Acc], T);
forms(Acc, [{call, Context = #{spec := Spec, args := Args, line := Line}} | T]) ->
    {ok, ArgsForms} = forms([], Args),
    Name =
        case maps:get(kind, Context, named) of
            anonymous ->
                {var, Line, Spec};
            named ->
                {atom, Line, Spec}
        end,
    forms([{call, Line, Name, ArgsForms} | Acc], T);
forms(Acc, [{cons, #{head := Head, tail := Tail, line := Line}} | T]) ->
    {ok, [HeadForm]} = forms([], [Head]),
    {ok, [TailForm]} = forms([], [Tail]),
    Form = {cons, Line, HeadForm, TailForm},
    forms([Form | Acc], T);
forms(Acc, [{float_lit, _Context} = FloatLit | T]) ->
    Form = box(FloatLit),
    forms([Form | Acc], T);
forms(Acc, [{func, #{line := Line, params := Params, exprs := Exprs}} | T]) ->
    {ok, ParamForms} = forms([], Params),
    {ok, GuardForms} = guard_forms([], Params),
    {ok, ExprForms} = forms([], Exprs),
    FuncClause = {clause, Line, ParamForms, GuardForms, ExprForms},
    Form = {'fun', Line, {clauses, [FuncClause]}},
    forms([Form | Acc], T);
forms(Acc, [{func_group, #{line := Line1, spec := Spec, arity := Arity, forms := Forms}} | T]) ->
    FuncClauses = lists:map(
        fun(Form) ->
            {func, #{line := Line2, spec := Spec, params := Params, exprs := Exprs}} = Form,
            {ok, ParamForms} = forms([], Params),
            {ok, GuardForms} = guard_forms([], Params),
            {ok, ExprForms} = forms([], Exprs),
            {clause, Line2, ParamForms, GuardForms, ExprForms}
        end,
        Forms
    ),
    Form = {function, Line1, Spec, Arity, FuncClauses},
    forms([Form | Acc], T);
forms(Acc, [{identifier, #{line := Line, spec := Spec, type := Type}} | T]) ->
    Form =
        case Type of
            {type, #{spec := atom}} ->
                {var, Line, Spec};
            {type, #{spec := bool}} ->
                {var, Line, Spec};
            {type, #{spec := float}} ->
                {var, Line, Spec};
            {type, #{spec := int}} ->
                {var, Line, Spec};
            {type, #{kind := list}} ->
                {var, Line, Spec};
            {type, #{kind := func}} ->
                {var, Line, Spec};
            _ ->
                TypeSpec = rufus_form:spec(Type),
                {tuple, Line, [{atom, Line, TypeSpec}, {var, Line, Spec}]}
        end,
    forms([Form | Acc], T);
forms(Acc, [{int_lit, _Context} = IntLit | T]) ->
    Form = box(IntLit),
    forms([Form | Acc], T);
forms(Acc, [{list_lit, _Context} = ListLit | T]) ->
    Form = box(ListLit),
    forms([Form | Acc], T);
forms(Acc, [{match_op, #{line := Line, left := Left, right := Right}} | T]) ->
    {ok, [LeftForm]} = forms([], [Left]),
    {ok, [RightForm]} = forms([], [Right]),
    Form = {match, Line, LeftForm, RightForm},
    forms([Form | Acc], T);
forms(Acc, [{module, #{line := Line, spec := Spec}} | T]) ->
    Form = {attribute, Line, module, Spec},
    forms([Form | Acc], T);
forms(Acc, [{param, #{line := Line, spec := Spec, type := Type}} | T]) ->
    Form =
        case Type of
            {type, #{spec := atom}} ->
                {var, Line, Spec};
            {type, #{spec := bool}} ->
                {var, Line, Spec};
            {type, #{spec := float}} ->
                {var, Line, Spec};
            {type, #{spec := int}} ->
                {var, Line, Spec};
            {type, #{kind := list}} ->
                {var, Line, Spec};
            {type, #{kind := func}} ->
                {var, Line, Spec};
            _ ->
                TypeSpec = rufus_form:spec(Type),
                {tuple, Line, [{atom, Line, TypeSpec}, {var, Line, Spec}]}
        end,
    forms([Form | Acc], T);
forms(Acc, [{string_lit, _Context} = StringLit | T]) ->
    Form = box(StringLit),
    forms([Form | Acc], T);
forms(Acc, [{type, _Context} | T]) ->
    %% no-op to satisfy Dialyzer
    forms(Acc, T);
forms(Acc, []) ->
    {ok, lists:reverse(Acc)};
forms(Acc, Form) ->
    erlang:error(unhandled_form, [Acc, Form]).

%% rufus_operator_to_erlang_operator converts a Rufus operator to the Erlang
%% equivalent.
-spec rufus_operator_to_erlang_operator(atom(), atom()) -> atom().
rufus_operator_to_erlang_operator('/', float) ->
    '/';
rufus_operator_to_erlang_operator('/', int) ->
    'div';
rufus_operator_to_erlang_operator('%', int) ->
    'rem';
rufus_operator_to_erlang_operator('%', float) ->
    erlang:error(unsupported_operand_type, ['%', float]);
rufus_operator_to_erlang_operator('and', bool) ->
    'andalso';
rufus_operator_to_erlang_operator('or', bool) ->
    'orelse';
rufus_operator_to_erlang_operator('==', _) ->
    '=:=';
rufus_operator_to_erlang_operator('!=', _) ->
    '=/=';
rufus_operator_to_erlang_operator('<=', _) ->
    '=<';
rufus_operator_to_erlang_operator(Op, _) ->
    Op.

%% guard_forms generates function guard forms for various scalar parameter
%% types.
-spec guard_forms(list(erlang_form()) | list(list()), list(param_form())) ->
    {ok, list(erlang_form())}.
guard_forms(Acc, [{param, #{line := Line, spec := Name, type := {type, #{spec := atom}}}} | T]) ->
    GuardExpr = [
        {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, is_atom}}, [
            {var, Line, Name}
        ]}
    ],
    guard_forms([GuardExpr | Acc], T);
guard_forms(Acc, [{param, #{line := Line, spec := Name, type := {type, #{spec := bool}}}} | T]) ->
    GuardExpr = [
        {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, is_boolean}}, [
            {var, Line, Name}
        ]}
    ],
    guard_forms([GuardExpr | Acc], T);
guard_forms(Acc, [{param, #{line := Line, spec := Name, type := {type, #{spec := float}}}} | T]) ->
    GuardExpr = [
        {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, is_float}}, [
            {var, Line, Name}
        ]}
    ],
    guard_forms([GuardExpr | Acc], T);
guard_forms(Acc, [{param, #{line := Line, spec := Name, type := {type, #{spec := int}}}} | T]) ->
    GuardExpr = [
        {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, is_integer}}, [
            {var, Line, Name}
        ]}
    ],
    guard_forms([GuardExpr | Acc], T);
guard_forms(Acc, [_ | T]) ->
    guard_forms(Acc, T);
guard_forms(Acc, []) ->
    %% TODO(jkakar): Should we be reversing Acc here? Does ordering affect guard
    %% behavior?
    {ok, Acc}.

%% box converts a Rufus literal to its representation in Erlang. atom, bool,
%% float and int are all represented as scalar values in Erlang, while string is
%% represented as an annotated {string, BinaryValue} tuple.
-spec box(
    atom_lit_form()
    | bool_lit_form()
    | float_lit_form()
    | int_lit_form()
    | list_lit_form()
    | string_lit_form()
) -> (erlang3_form() | erlang4_form()).
box({atom_lit, #{spec := Value, line := Line}}) ->
    {atom, Line, Value};
box({bool_lit, #{spec := Value, line := Line}}) ->
    {atom, Line, Value};
box({float_lit, #{spec := Value, line := Line}}) ->
    {float, Line, Value};
box({int_lit, #{spec := Value, line := Line}}) ->
    {integer, Line, Value};
box({list_lit, #{elements := Elements, line := Line}}) ->
    list_to_cons(Elements, Line);
box({string_lit, #{line := Line, spec := Value}}) ->
    StringExpr = {bin_element, Line, {string, Line, binary_to_list(Value)}, default, default},
    {tuple, Line, [{atom, Line, string}, {bin, Line, [StringExpr]}]}.

%% Visibility helpers

%% annotate_exports creates export attributes for all exported functions and
%% injects them into the sequence of Erlang forms. They're defined before
%% function definitions to avoid crashing the Erlang compiler.
-spec annotate_exports(list(erlang_form())) -> {ok, list(erlang_form())}.
annotate_exports(Forms) ->
    annotate_exports([], Forms).

-spec annotate_exports(list(erlang_form()), list(erlang_form())) -> {ok, list(erlang_form())}.
annotate_exports(Acc, [Form = {attribute, _Line, module, _Name} | T]) ->
    {ok, ExportForms} = make_export_forms(T),
    %% Inject export forms directly after the module declaration.
    annotate_exports(Acc ++ ExportForms ++ [Form], T);
annotate_exports(Acc, [Form | T]) ->
    annotate_exports([Form | Acc], T);
annotate_exports(Acc, []) ->
    {ok, lists:reverse(Acc)}.

%% make_export_forms generates export attributes for all public functions.
-spec make_export_forms(list(erlang_form())) -> {ok, list(export_attribute_erlang_form())}.
make_export_forms(Forms) ->
    make_export_forms([], Forms).

-spec make_export_forms(list(erlang_form()), list(erlang_form())) ->
    {ok, list(export_attribute_erlang_form())}.
make_export_forms(Acc, [{function, Line, Spec, Arity, _Forms} | T]) ->
    case is_public(Spec) of
        true ->
            Form = {attribute, Line, export, [{Spec, Arity}]},
            make_export_forms([Form | Acc], T);
        false ->
            make_export_forms(Acc, T)
    end;
make_export_forms(Acc, [_Form | T]) ->
    make_export_forms(Acc, T);
make_export_forms(Acc, []) ->
    {ok, lists:reverse(Acc)}.

%% is_public returns true if Name represents a public function that should be
%% exported from the module, otherwise it returns false.
-spec is_public(atom()) -> boolean().
is_public(Name) ->
    LeadingChar = hd(string:slice(atom_to_list(Name), 0, 1)),
    not is_private(LeadingChar).

%% is_private returns true if Name represents a private function that should be
%% exported from the module, otherwise it returns false.
-spec is_private(integer()) -> boolean().
is_private(LeadingChar) ->
    (LeadingChar >= $a) and (LeadingChar =< $z).

%% list helpers

%% list_to_cons transforms a list of Rufus form elements in a list_lit form into
%% an Erlang cons form.
-spec list_to_cons(rufus_forms(), integer()) -> term().
list_to_cons([Form | []], Line) ->
    {ok, [Head | _]} = forms([], [Form]),
    {cons, Line, Head, {nil, Line}};
list_to_cons([Form | T], Line) ->
    {ok, [Head | _]} = forms([], [Form]),
    {cons, Line, Head, list_to_cons(T, Line)};
list_to_cons([], Line) ->
    {nil, Line}.
