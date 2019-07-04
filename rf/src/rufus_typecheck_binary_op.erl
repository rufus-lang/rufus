%% rufus_typecheck_binary_op enforces the invariant that a binary operation may
%% only be performed exclusively with ints or exclusively with floats, not both
%% at the same time. No other types are supported with binary operators.
-module(rufus_typecheck_binary_op).

-include_lib("rufus_type.hrl").

%% API exports

-export([forms/1]).

%% API

%% forms iterates over RufusForms and typechecks binary operations to ensure
%% that the operands are exclusively ints or exclusively floats. Iteration stops
%% at the first error. Returns values:
%% - `{ok, AnnotatedForms}` if no issues are found with all `binary_op` forms
%%   have type annotations.
%% - `{error, unmatched_operand_type, Data}` if an `int` operand is mixed with a
%%   `float` operand. `Data` contains `left` and `right` atom keys pointing to
%%   the illegal operands.
%% - `{error, unsupported_operand_type, Data}` if a type other than an int is
%%   used as an operand. `Data` contains `left` and `right` atom keys pointing
%%   to the illegal operands.
-spec forms(list(rufus_form())) -> {ok, list(rufus_form())}.
forms(RufusForms) ->
    forms([], RufusForms).

%% Private API

forms(Acc, [{func, Context=#{exprs := Exprs}}|T]) ->
    case forms([], Exprs) of
        {ok, AnnotatedExprs} ->
            Form = {func, Context#{exprs => AnnotatedExprs}},
            forms([Form|Acc], T);
        Error ->
            Error
    end;
forms(Acc, [Form={binary_op, _Context}|T]) ->
    case typecheck(Form) of
        {ok, AnnotatedForm} ->
            forms([AnnotatedForm|Acc], T);
        Error ->
            Error
    end;
forms(Acc, [H|T]) ->
    forms([H|Acc], T);
forms(Acc, []) ->
    {ok, lists:reverse(Acc)}.

typecheck({binary_op, Context = #{op := Op, left := [Left], right := [Right]}}) ->
    {ok, LeftType} = typecheck(Left),
    {ok, RightType} = typecheck(Right),
    case infer_expr_type(Op, LeftType, RightType) of
        ExprType={type, _Context} ->
            AnnotatedLeft = rufus_form:maybe_annotate_type(Left, ExprType),
            AnnotatedRight = rufus_form:maybe_annotate_type(Right, ExprType),
            {ok, {binary_op, Context#{left => [AnnotatedLeft], right => [AnnotatedRight]}}};
        Error ->
            Error
        end;
typecheck(Form = {_, #{type := _Type}}) ->
    {ok, Form}.

infer_expr_type(_Op, {_, #{type := {type, #{spec := float, line := Line}}}}, {_, #{type := {type, #{spec := float}}}}) ->
    rufus_form:make_inferred_type(float, Line);
infer_expr_type(_Op, {_, #{type := {type, #{spec := int, line := Line}}}}, {_, #{type := {type, #{spec := int}}}}) ->
    rufus_form:make_inferred_type(int, Line);
infer_expr_type(Op, Left = {_, #{type := {type, #{spec := float}}}}, Right = {_, #{type := {type, #{spec := int}}}}) ->
    Data = #{op => Op, left => Left, right => Right},
    {error, unmatched_operand_type, Data};
infer_expr_type(Op, Left = {_, #{type := {type, #{spec := int}}}}, Right = {_, #{type := {type, #{spec := float}}}}) ->
    Data = #{op => Op, left => Left, right => Right},
    {error, unmatched_operand_type, Data};
infer_expr_type(Op, Left, Right) ->
    Data = #{op => Op, left => Left, right => Right},
    {error, unsupported_operand_type, Data}.
