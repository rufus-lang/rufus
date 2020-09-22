%% rufus_type exposes type information for Rufus forms. Type information is
%% inferred in cases where it isn't already present.
-module(rufus_type).

-include_lib("rufus_type.hrl").

%% API exports

-export([resolve/2]).

%% API

%% resolve returns a type form for Form. If Form already has a type form
%% associated with it, it will be returned. Otherwise, the type is inferred.
-spec resolve(#{atom() => rufus_forms()}, rufus_form()) -> {ok, type_form()} | error_triple().
resolve(Globals, Form) ->
    try
        resolve_type(Globals, Form)
    catch
        {error, Code, Data} -> {error, Code, Data}
    end.

%% Private API

-spec resolve_type(#{atom() => rufus_forms()}, rufus_form()) -> {ok, type_form()} | no_return().
resolve_type(Globals, Form = {cons, _Context}) ->
    resolve_cons_type(Globals, Form);
resolve_type(Globals, Form = {list_lit, _Context}) ->
    resolve_list_lit_type(Globals, Form);
resolve_type(_Globals, {_Form, #{type := Type}}) ->
    {ok, Type};
resolve_type(Globals, Form = {binary_op, _Context}) ->
    resolve_binary_op_type(Globals, Form);
resolve_type(Globals, Form = {call, _Context}) ->
    resolve_call_type(Globals, Form);
resolve_type(_Globals, {func, #{return_type := Type}}) ->
    {ok, Type};
resolve_type(Globals, Form = {identifier, _Context}) ->
    resolve_identifier_type(Globals, Form).

%% binary_op form helpers

-spec resolve_binary_op_type(globals(), binary_op_form()) -> {ok, type_form()} | no_return().
resolve_binary_op_type(Globals, Form = {binary_op, #{op := Op, left := Left, right := Right, line := Line}}) ->
    {ok, LeftType} = resolve_type(Globals, Left),
    {ok, RightType} = resolve_type(Globals, Right),
    LeftTypeSpec = rufus_form:type_spec(LeftType),
    RightTypeSpec = rufus_form:type_spec(RightType),
    {AllowType, AllowTypePair} = case Op of
        '+'   -> {fun allow_type_with_mathematical_operator/2, fun allow_type_pair_with_mathematical_operator/2};
        '-'   -> {fun allow_type_with_mathematical_operator/2, fun allow_type_pair_with_mathematical_operator/2};
        '*'   -> {fun allow_type_with_mathematical_operator/2, fun allow_type_pair_with_mathematical_operator/2};
        '/'   -> {fun allow_type_with_mathematical_operator/2, fun allow_type_pair_with_mathematical_operator/2};
        '%'   -> {fun allow_type_with_mathematical_operator/2, fun allow_type_pair_with_mathematical_operator/2};
        'and' -> {fun allow_type_with_conditional_operator/2,  fun allow_type_pair_with_conditional_operator/2};
        'or'  -> {fun allow_type_with_conditional_operator/2,  fun allow_type_pair_with_conditional_operator/2};
        '=='  -> {fun allow_type_with_comparison_operator/2,   fun allow_type_pair_with_comparison_operator/2};
        '!='  -> {fun allow_type_with_comparison_operator/2,   fun allow_type_pair_with_comparison_operator/2};
        '<'   -> {fun allow_type_with_comparison_operator/2,   fun allow_type_pair_with_comparison_operator/2};
        '<='  -> {fun allow_type_with_comparison_operator/2,   fun allow_type_pair_with_comparison_operator/2};
        '>'   -> {fun allow_type_with_comparison_operator/2,   fun allow_type_pair_with_comparison_operator/2};
        '>='  -> {fun allow_type_with_comparison_operator/2,   fun allow_type_pair_with_comparison_operator/2}
    end,

    ok = case AllowType(Op, LeftTypeSpec) and AllowType(Op, RightTypeSpec) of
        true ->
            ok;
        false ->
            throw({error, unsupported_operand_type, #{form => Form}})
    end,

    {ok, LeftType} = case AllowTypePair(LeftTypeSpec, RightTypeSpec) of
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

binary_op_type('==', Line) -> rufus_form:make_inferred_type(bool, Line);
binary_op_type('!=', Line) -> rufus_form:make_inferred_type(bool, Line);
binary_op_type('<', Line) -> rufus_form:make_inferred_type(bool, Line);
binary_op_type('<=', Line) -> rufus_form:make_inferred_type(bool, Line);
binary_op_type('>', Line) -> rufus_form:make_inferred_type(bool, Line);
binary_op_type('>=', Line) -> rufus_form:make_inferred_type(bool, Line);
binary_op_type(_, _) -> default.

%% allow_type_with_mathematical_operator returns true if the specified type may
%% be used with the specified arithmetic operator, otherwise false.
-spec allow_type_with_mathematical_operator(arithmetic_operator(), float | int | atom()) -> boolean().
allow_type_with_mathematical_operator('%', float) -> false;
allow_type_with_mathematical_operator(_, float) -> true;
allow_type_with_mathematical_operator(_, int) -> true;
allow_type_with_mathematical_operator(_, _) -> false.

%% allow_type_pair_with_mathematical_operator returns true if the specified pair
%% of types are either both of type float, or both of type int, which are the
%% only types that may be used with an arithmetic operator.
-spec allow_type_pair_with_mathematical_operator(float | int | atom(), float | int | atom()) -> boolean().
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

-spec resolve_call_type(globals(), call_form()) -> {ok, type_form()} | no_return().
resolve_call_type(Globals, Form = {call, #{spec := Spec, args := Args}}) ->
        case maps:get(Spec, Globals, undefined) of
        undefined ->
            throw({error, unknown_func, #{spec => Spec, args => Args}});
        Funcs ->
            case find_matching_funcs(Funcs, Args) of
                {error, Reason, Data} ->
                    throw({error, Reason, Data});
                {ok, MatchingFuncs} when length(MatchingFuncs) > 1 ->
                    %% TODO(jkakar): We need to handle cases where more than one
                    %% function matches a given set of parameters. For example,
                    %% consider two functions:
                    %%
                    %% func Echo(:hello) atom { :hello }
                    %% func Echo(:goodbye) string { "goodbye" }
                    %%
                    %% These both match an args list with a single atom arg
                    %% type, but they have different return types. We need to
                    %% account for all possible return types. When a callsite
                    %% specifies a literal value such as :hello or :goodbye we
                    %% should select the correct singular return type.
                    erlang:error({not_implemented, [Globals, Form]});
                {ok, MatchingFuncs} when length(MatchingFuncs) =:= 1 ->
                    [Func] = MatchingFuncs,
                    {ok, rufus_form:return_type(Func)}
            end
    end.

-spec find_matching_funcs(list(func_form()), rufus_forms()) -> {ok, list(func_form())} | error_triple().
find_matching_funcs(Funcs, Args) ->
    FuncsWithMatchingArity = lists:filter(fun({func, #{params := Params}}) ->
        length(Params) =:= length(Args)
    end, Funcs),

    case length(FuncsWithMatchingArity) of
        Length when Length > 0 ->
            Result = lists:filter(fun({func, #{params := Params}}) ->
                Zipped = lists:zip(Params, Args),
                lists:all(fun({{param, #{type := {type, #{spec := ParamTypeSpec}}}},
                               {_, #{type := {type, #{spec := ArgTypeSpec}}}}}) ->
                        ParamTypeSpec =:= ArgTypeSpec
                end, Zipped)
            end, FuncsWithMatchingArity),
            case Result of
                Result when length(Result) =:= 0 ->
                    {error, unmatched_args, #{funcs => FuncsWithMatchingArity, args => Args}};
                _ ->
                    {ok, Result}
            end;
        _ ->
            {error, unknown_arity, #{funcs => Funcs, args => Args}}
    end.

%% cons form helpers

-spec resolve_cons_type(globals(), cons_form()) -> {ok, type_form()} | no_return().
resolve_cons_type(Globals, Form = {cons, #{head := Head, tail := Tail, type := Type}}) ->
    {type, #{element_type := ElementType}} = Type,
    {ok, HeadType} = resolve_type(Globals, Head),
    {ok, {type, #{element_type := TailElementType}}} = resolve_type(Globals, Tail),
    case pair_types_match_cons_type(ElementType, HeadType, TailElementType) of
        true ->
            {ok, Type};
        false ->
            Data = #{form => Form,
                     element_type => ElementType,
                     head_type => HeadType,
                     tail_element_type => TailElementType},
            throw({error, unexpected_element_type, Data})
    end.

pair_types_match_cons_type({type, #{spec := Spec}}, {type, #{spec := Spec}}, {type, #{spec := Spec}}) ->
    true;
pair_types_match_cons_type(_, _, _) ->
    false.

%% identifier form helpers

-spec resolve_identifier_type(globals(), identifier_form()) -> {ok, type_form()} | no_return().
resolve_identifier_type(Globals, Form = {identifier, #{spec := Spec, locals := Locals}}) ->
    case maps:get(Spec, Locals, undefined) of
        {type, _Context} = Type ->
            {ok, Type};
        undefined ->
            Data = #{globals => Globals, locals => Locals, form => Form},
            throw({error, unknown_identifier, Data})
    end.

%% list_lit form helpers

-spec resolve_list_lit_type(globals(), list_lit_form()) -> {ok, type_form()} | no_return().
resolve_list_lit_type(Globals, Form = {list_lit, #{elements := Elements, type := Type}}) ->
    {type, #{element_type := {type, #{spec := ElementTypeSpec}}}} = Type,
    ElementTypes = lists:map(fun(ElementForm) ->
        {ok, ElementType} = resolve_type(Globals, ElementForm),
        ElementType
    end, Elements),

    case element_types_match_list_type(ElementTypeSpec, ElementTypes) of
        true ->
            {ok, Type};
        false ->
            Data = #{form => Form},
            throw({error, unexpected_element_type, Data})
    end.

-spec element_types_match_list_type(type_spec(), rufus_forms()) -> boolean().
element_types_match_list_type(Expected, ElementTypes) ->
    TypesMatch = fun({type, #{spec := Actual}}) -> Actual == Expected end,
    lists:all(TypesMatch, ElementTypes).
