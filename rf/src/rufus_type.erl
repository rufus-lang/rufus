%% rufus_type exposes type information for Rufus forms. Type information is
%% inferred in cases where it isn't already present.
-module(rufus_type).

-include_lib("rufus_type.hrl").

%% API exports

-export([
    resolve/2,
    resolve/3
]).

%% API

%% resolve returns a type form for Form. If Form already has a type form
%% associated with it, it will be returned. Otherwise, the type is inferred.
-spec resolve(#{atom() => rufus_forms()}, rufus_form()) -> {ok, type_form()} | error_triple().
resolve(Globals, Form) ->
    resolve([], Globals, Form).

-spec resolve(rufus_stack(), #{atom() => rufus_forms()}, rufus_form()) ->
    {ok, type_form()} | error_triple().
resolve(Stack, Globals, Form) ->
    try
        resolve_type(Stack, Globals, Form)
    catch
        {error, Code, Data} -> {error, Code, Data}
    end.

%% Private API

-spec resolve_type(rufus_stack(), #{atom() => rufus_forms()}, rufus_form()) ->
    {ok, type_form()} | no_return().
resolve_type(Stack, Globals, Form = {cons, _Context}) ->
    resolve_cons_type(Stack, Globals, Form);
resolve_type(Stack, Globals, Form = {list_lit, _Context}) ->
    resolve_list_lit_type(Stack, Globals, Form);
resolve_type(_Stack, _Globals, {_Form, #{type := Type}}) ->
    {ok, Type};
resolve_type(Stack, Globals, Form = {binary_op, _Context}) ->
    resolve_binary_op_type(Stack, Globals, Form);
resolve_type(Stack, Globals, Form = {call, _Context}) ->
    resolve_call_type(Stack, Globals, Form);
resolve_type(Stack, Globals, Form = {'case', _Context}) ->
    resolve_case_type(Stack, Globals, Form);
resolve_type(Stack, Globals, Form = {case_clause, _Context}) ->
    resolve_case_clause_type(Stack, Globals, Form);
resolve_type(Stack, Globals, Form = {catch_clause, _Context}) ->
    resolve_catch_clause_type(Stack, Globals, Form);
resolve_type(_Stack, _Globals, {func, #{return_type := Type}}) ->
    {ok, Type};
resolve_type(Stack, Globals, Form = {identifier, _Context}) ->
    resolve_identifier_type(Stack, Globals, Form);
resolve_type(Stack, Globals, Form = {throw, _Context}) ->
    resolve_throw_type(Stack, Globals, Form);
resolve_type(Stack, Globals, Form = {try_catch_after, _Context}) ->
    resolve_try_catch_after_type(Stack, Globals, Form).

%% binary_op form helpers

-spec resolve_binary_op_type(rufus_stack(), globals(), binary_op_form()) ->
    {ok, type_form()} | no_return().
resolve_binary_op_type(
    Stack,
    Globals,
    Form = {binary_op, #{op := Op, left := Left, right := Right, line := Line}}
) ->
    {ok, LeftType} = resolve_type(Stack, Globals, Left),
    {ok, RightType} = resolve_type(Stack, Globals, Right),
    LeftTypeSpec = rufus_form:type_spec(LeftType),
    RightTypeSpec = rufus_form:type_spec(RightType),
    {AllowType, AllowTypePair} =
        case Op of
            '+' ->
                {
                    fun allow_type_with_mathematical_operator/2,
                    fun allow_type_pair_with_mathematical_operator/2
                };
            '-' ->
                {
                    fun allow_type_with_mathematical_operator/2,
                    fun allow_type_pair_with_mathematical_operator/2
                };
            '*' ->
                {
                    fun allow_type_with_mathematical_operator/2,
                    fun allow_type_pair_with_mathematical_operator/2
                };
            '/' ->
                {
                    fun allow_type_with_mathematical_operator/2,
                    fun allow_type_pair_with_mathematical_operator/2
                };
            '%' ->
                {
                    fun allow_type_with_mathematical_operator/2,
                    fun allow_type_pair_with_mathematical_operator/2
                };
            'and' ->
                {
                    fun allow_type_with_conditional_operator/2,
                    fun allow_type_pair_with_conditional_operator/2
                };
            'or' ->
                {
                    fun allow_type_with_conditional_operator/2,
                    fun allow_type_pair_with_conditional_operator/2
                };
            '==' ->
                {
                    fun allow_type_with_comparison_operator/2,
                    fun allow_type_pair_with_comparison_operator/2
                };
            '!=' ->
                {
                    fun allow_type_with_comparison_operator/2,
                    fun allow_type_pair_with_comparison_operator/2
                };
            '<' ->
                {
                    fun allow_type_with_comparison_operator/2,
                    fun allow_type_pair_with_comparison_operator/2
                };
            '<=' ->
                {
                    fun allow_type_with_comparison_operator/2,
                    fun allow_type_pair_with_comparison_operator/2
                };
            '>' ->
                {
                    fun allow_type_with_comparison_operator/2,
                    fun allow_type_pair_with_comparison_operator/2
                };
            '>=' ->
                {
                    fun allow_type_with_comparison_operator/2,
                    fun allow_type_pair_with_comparison_operator/2
                }
        end,

    ok =
        case AllowType(Op, LeftTypeSpec) and AllowType(Op, RightTypeSpec) of
            true ->
                ok;
            false ->
                throw({error, unsupported_operand_type, #{form => Form}})
        end,

    {ok, LeftType} =
        case AllowTypePair(LeftTypeSpec, RightTypeSpec) of
            true ->
                {ok, LeftType};
            false ->
                throw({error, unmatched_operand_type, #{form => Form}})
        end,

    case binary_op_type(Op, Line) of
        default ->
            {ok, LeftType};
        Type ->
            {ok, Type}
    end.

binary_op_type('==', Line) -> rufus_form:make_type(bool, Line);
binary_op_type('!=', Line) -> rufus_form:make_type(bool, Line);
binary_op_type('<', Line) -> rufus_form:make_type(bool, Line);
binary_op_type('<=', Line) -> rufus_form:make_type(bool, Line);
binary_op_type('>', Line) -> rufus_form:make_type(bool, Line);
binary_op_type('>=', Line) -> rufus_form:make_type(bool, Line);
binary_op_type(_, _) -> default.

%% allow_type_with_mathematical_operator returns true if the specified type may
%% be used with the specified arithmetic operator, otherwise false.
-spec allow_type_with_mathematical_operator(arithmetic_operator(), float | int | atom()) ->
    boolean().
allow_type_with_mathematical_operator('%', float) -> false;
allow_type_with_mathematical_operator(_, float) -> true;
allow_type_with_mathematical_operator(_, int) -> true;
allow_type_with_mathematical_operator(_, _) -> false.

%% allow_type_pair_with_mathematical_operator returns true if the specified pair
%% of types are either both of type float, or both of type int, which are the
%% only types that may be used with an arithmetic operator.
-spec allow_type_pair_with_mathematical_operator(float | int | atom(), float | int | atom()) ->
    boolean().
allow_type_pair_with_mathematical_operator(float, float) -> true;
allow_type_pair_with_mathematical_operator(int, int) -> true;
allow_type_pair_with_mathematical_operator(_, _) -> false.

%% allow_type_with_conditional_operator returns true if the specified type may
%% be used with the specified boolean operator, otherwise false.
-spec allow_type_with_conditional_operator(boolean_operator(), bool | atom()) -> boolean().
allow_type_with_conditional_operator(_, bool) -> true;
allow_type_with_conditional_operator(_, _) -> false.

%% allow_type_pair_with_conditional_operator returns true if the specified pair
%% of types are both of type bool, which is the only type that may be used with
%% a boolean operator.
-spec allow_type_pair_with_conditional_operator(bool | atom(), bool | atom()) -> boolean().
allow_type_pair_with_conditional_operator(bool, bool) -> true;
allow_type_pair_with_conditional_operator(_, _) -> false.

%% allow_type_with_comparison_operator returns true if the specified type may be
%% used with the specified comparison operator, otherwise false.
-spec allow_type_with_comparison_operator(comparison_operator(), bool | atom()) -> boolean().
allow_type_with_comparison_operator('==', _) -> true;
allow_type_with_comparison_operator('!=', _) -> true;
allow_type_with_comparison_operator('<', int) -> true;
allow_type_with_comparison_operator('<', float) -> true;
allow_type_with_comparison_operator('<=', int) -> true;
allow_type_with_comparison_operator('<=', float) -> true;
allow_type_with_comparison_operator('>', int) -> true;
allow_type_with_comparison_operator('>', float) -> true;
allow_type_with_comparison_operator('>=', int) -> true;
allow_type_with_comparison_operator('>=', float) -> true;
allow_type_with_comparison_operator(_, _) -> false.

%% allow_type_pair_with_comparison_operator returns true if the specified pair
%% of types are both of same type, which is a requirement for comparisons.
-spec allow_type_pair_with_comparison_operator(type_spec(), type_spec()) -> boolean().
allow_type_pair_with_comparison_operator(Spec, Spec) -> true;
allow_type_pair_with_comparison_operator(_, _) -> false.

%% call form helpers

-spec resolve_call_type(rufus_stack(), globals(), call_form()) -> {ok, type_form()} | no_return().
resolve_call_type(
    Stack,
    Globals,
    Form = {call, #{spec := Spec, args := Args, locals := Locals}}
) ->
    FuncTypes = maps:get(Spec, Globals, undefined),
    case maps:get(Spec, Locals, FuncTypes) of
        undefined ->
            Data = #{
                form => Form,
                globals => Globals,
                stack => Stack
            },
            throw({error, unknown_func, Data});
        Types ->
            case find_matching_types(Types, Args) of
                {ok, MatchingTypes} when length(MatchingTypes) > 1 ->
                    UniqueTypes = lists:usort(
                        fun
                            (
                                {type, #{kind := func, spec := FuncSpec}},
                                {type, #{kind := func, spec := FuncSpec}}
                            ) ->
                                true;
                            (_, _) ->
                                false
                        end,
                        MatchingTypes
                    ),
                    case length(UniqueTypes) of
                        1 ->
                            [Type | _Tail] = MatchingTypes,
                            {ok, rufus_form:return_type(Type)};
                        _ ->
                            %% TODO(jkakar): We need to handle cases where more
                            %% than one function matches a given set of
                            %% parameters. For example, consider two functions:
                            %%
                            %% func Echo(:hello) atom { :hello }
                            %% func Echo(:goodbye) string { "goodbye" }
                            %%
                            %% These both match an args list with a single atom
                            %% arg type, but they have different return types.
                            %% We need to account for all possible return types.
                            %% When a callsite specifies a literal value such as
                            %% :hello or :goodbye we should select the correct
                            %% singular return type.
                            erlang:error({not_implemented, [Stack, Globals, Form]})
                    end;
                {ok, MatchingTypes} when length(MatchingTypes) =:= 1 ->
                    [Type] = MatchingTypes,
                    {ok, rufus_form:return_type(Type)};
                {error, Reason1, Data1} ->
                    throw({error, Reason1, Data1})
            end
    end.

-spec find_matching_types(list(type_form()), rufus_forms()) ->
    {ok, list(type_form())} | error_triple().
find_matching_types(Types, Args) ->
    ArgTypes = lists:map(
        fun({_Form, #{type := ArgType}}) -> ArgType end,
        Args
    ),

    TypesWithMatchingArity = lists:filter(
        fun
            ({type, #{kind := func, param_types := ParamTypes}}) ->
                length(ParamTypes) =:= length(ArgTypes);
            (_Form) ->
                false
        end,
        Types
    ),

    case length(TypesWithMatchingArity) of
        Length when Length > 0 ->
            Result = lists:filter(
                fun({type, #{param_types := ParamTypes}}) ->
                    Zipped = lists:zip(ParamTypes, ArgTypes),
                    lists:all(
                        fun(
                            {
                                {type, #{spec := ParamTypeSpec}},
                                {type, #{spec := ArgTypeSpec}}
                            }
                        ) ->
                            ParamTypeSpec =:= ArgTypeSpec
                        end,
                        Zipped
                    )
                end,
                TypesWithMatchingArity
            ),
            case Result of
                Result when length(Result) =:= 0 ->
                    {error, unmatched_args, #{types => TypesWithMatchingArity, args => Args}};
                _ ->
                    {ok, Result}
            end;
        _ ->
            {error, unknown_arity, #{types => Types, args => Args}}
    end.

%% case form helpers

-spec resolve_case_type(rufus_stack(), globals(), case_form()) -> {ok, type_form()} | no_return().
resolve_case_type(Stack, Globals, {'case', #{clauses := Clauses}}) ->
    LastClause = lists:last(Clauses),
    resolve_type(Stack, Globals, LastClause).

-spec resolve_case_clause_type(rufus_stack(), globals(), case_clause_form()) ->
    {ok, type_form()} | no_return().
resolve_case_clause_type(Stack, Globals, {case_clause, #{exprs := Exprs}}) ->
    LastExpr = lists:last(Exprs),
    resolve_type(Stack, Globals, LastExpr).

%% cons form helpers

-spec resolve_cons_type(rufus_stack(), globals(), cons_form()) -> {ok, type_form()} | no_return().
resolve_cons_type(Stack, Globals, Form = {cons, #{head := Head, tail := Tail, type := Type}}) ->
    {type, #{element_type := ElementType}} = Type,
    {ok, HeadType} = resolve_type(Stack, Globals, Head),
    {ok, {type, #{element_type := TailElementType}}} = resolve_type(Stack, Globals, Tail),
    case pair_types_match_cons_type(ElementType, HeadType, TailElementType) of
        true ->
            {ok, Type};
        false ->
            Data = #{
                form => Form,
                element_type => ElementType,
                head_type => HeadType,
                tail_element_type => TailElementType
            },
            throw({error, unexpected_element_type, Data})
    end.

pair_types_match_cons_type(
    {type, #{spec := Spec}},
    {type, #{spec := Spec}},
    {type, #{spec := Spec}}
) ->
    true;
pair_types_match_cons_type(_, _, _) ->
    false.

%% identifier form helpers

-spec resolve_identifier_type(rufus_stack(), globals(), identifier_form()) ->
    {ok, type_form()} | no_return().
resolve_identifier_type(Stack, Globals, Form = {identifier, #{spec := Spec, locals := Locals}}) ->
    case maps:get(Spec, Locals, undefined) of
        [{type, _Context}] = Type ->
            {ok, Type};
        undefined ->
            case lookup_identifier_type(Stack) of
                {ok, Type} ->
                    {ok, Type};
                _Error ->
                    Data = #{
                        globals => Globals,
                        locals => Locals,
                        form => Form,
                        stack => Stack
                    },
                    throw({error, unknown_identifier, Data})
            end
    end.

%% lookup_identifier_type walks up the stack, finds, and returns the first type
%% it encounters. An error is returned if type information cannot be found.
-spec lookup_identifier_type(rufus_stack()) -> {ok, type_form()} | error_triple().
lookup_identifier_type(Stack) ->
    try
        lookup_identifier_type(Stack, Stack)
    catch
        {error, Reason, Data} ->
            {error, Reason, Data}
    end.

-spec lookup_identifier_type(rufus_stack(), rufus_stack()) -> {ok, type_form()} | no_return().
lookup_identifier_type(
    [{case_clause, _Context1} | [{'case', #{match_expr := CaseMatchExpr}} | _T]], Stack
) ->
    case allow_variable_binding(Stack) of
        true ->
            {ok, rufus_form:type(CaseMatchExpr)};
        false ->
            Data = #{stack => Stack},
            throw({error, unknown_identifier, Data})
    end;
lookup_identifier_type(
    [
        {catch_clause, #{match_expr := {_, #{spec := '_'}}, line := Line}}
        | [{try_catch_after, #{try_exprs := TryExprs}} | _T]
    ],
    Stack
) ->
    case allow_variable_binding(Stack) of
        true ->
            UnknownType = rufus_form:make_type(unknown, Line),
            {ok, UnknownType};
        false ->
            Data = #{stack => Stack},
            throw({error, unknown_identifier, Data})
    end;
lookup_identifier_type([{cons_head, _Context1} | [{cons, #{type := Type}} | _T]], Stack) ->
    case allow_variable_binding(Stack) of
        true ->
            {ok, rufus_form:element_type(Type)};
        false ->
            Data = #{stack => Stack},
            throw({error, unknown_identifier, Data})
    end;
lookup_identifier_type([{cons_tail, _Context1} | [{cons, #{type := Type}} | _T]], Stack) ->
    case allow_variable_binding(Stack) of
        true ->
            {ok, Type};
        false ->
            Data = #{stack => Stack},
            throw({error, unknown_identifier, Data})
    end;
lookup_identifier_type([{list_lit, #{type := Type}} | _T], Stack) ->
    case allow_variable_binding(Stack) of
        true ->
            {ok, rufus_form:element_type(Type)};
        false ->
            Data = #{stack => Stack},
            throw({error, unknown_identifier, Data})
    end;
lookup_identifier_type(
    [{match_op_left, _Context1} | [{match_op, #{right := {_FormSpec, #{type := Type}}}} | _T]],
    _Stack
) ->
    {ok, Type};
lookup_identifier_type(
    [{match_op_right, _Context1} | [{match_op, #{right := {_FormSpec, #{type := Type}}}} | _T]],
    _Stack
) ->
    {ok, Type};
lookup_identifier_type(
    [{match_op_right, _Context1} | [{match_op, #{right := _Context2}} | _T]],
    Stack
) ->
    Data = #{stack => Stack},
    throw({error, unknown_type, Data});
lookup_identifier_type([_H | T], Stack) ->
    lookup_identifier_type(T, Stack);
lookup_identifier_type([], Stack) ->
    Data = #{stack => Stack},
    throw({error, unknown_type, Data}).

%% allow_variable_binding returns true if the identifier is part of an
%% expression in a function parameter list, is part of the left operand of a
%% match expression, is part of a catch clause expression, or is part of a case
%% clause expression.
-spec allow_variable_binding(rufus_stack()) -> boolean().
allow_variable_binding(Stack) ->
    lists:any(
        fun
            ({case_clause, _Context}) ->
                true;
            ({catch_clause, _Context}) ->
                true;
            ({func_params, _Context}) ->
                true;
            ({match_op_left, _Context}) ->
                true;
            (_Form) ->
                false
        end,
        Stack
    ).

%% list_lit form helpers

-spec resolve_list_lit_type(rufus_stack(), globals(), list_lit_form()) ->
    {ok, type_form()} | no_return().
resolve_list_lit_type(Stack, Globals, Form = {list_lit, #{elements := Elements, type := Type1}}) ->
    ElementType = rufus_form:element_type(Type1),
    ExpectedTypeSpec = rufus_form:spec(ElementType),
    ElementTypeSpecs = lists:map(
        fun(ElementForm) ->
            {ok, Type2} = resolve_type(Stack, Globals, ElementForm),
            rufus_form:spec(Type2)
        end,
        Elements
    ),

    CompareFun = fun(TypeSpec) -> TypeSpec == ExpectedTypeSpec end,
    case lists:all(CompareFun, ElementTypeSpecs) of
        true ->
            {ok, Type1};
        false ->
            Data = #{form => Form},
            throw({error, unexpected_element_type, Data})
    end.

%% throw helpers

resolve_throw_type(Stack, Globals, {throw, #{expr := Expr, line := Line}}) ->
    {ok, ExprTypeForm} = resolve_type(Stack, Globals, Expr),
    TypeForm = rufus_form:make_type(throw, ExprTypeForm, Line),
    {ok, TypeForm}.

%% try/catch/after helpers

-spec resolve_catch_clause_type(rufus_stack(), globals(), catch_clause_form()) -> {ok, type_form()}.
resolve_catch_clause_type(Stack, Globals, {catch_clause, #{exprs := Exprs}}) ->
    LastExpr = lists:last(Exprs),
    resolve_type(Stack, Globals, LastExpr).

-spec resolve_try_catch_after_type(rufus_stack(), globals(), try_catch_after_form()) ->
    {ok, type_form()}.
resolve_try_catch_after_type(Stack, Globals, {try_catch_after, #{try_exprs := TryExprs}}) ->
    LastTryExpr = lists:last(TryExprs),
    resolve_type(Stack, Globals, LastTryExpr).
