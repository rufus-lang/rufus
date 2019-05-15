%% rufus_typecheck_binary_op enforces the invariant that a binary operation may
%% only be performed exclusively with ints or exclusively with floats, not both
%% at the same time.
-module(rufus_typecheck_binary_op).

%% API exports

-export([forms/1]).

%% API

%% forms iterates over RufusForms and typechecks binary operations to ensure
%% that the operands are exclusively ints or exclusively floats. Iteration stops
%% at the first error. Returns values:
%% - `{ok, RufusForms}` if no issues are found.
%% - `{error, unmatched_operand_type, Data}` if an `int` operand is mixed with a
%%   `float` operand. `Data` contains `left` and `right` atom keys pointing to
%%   the illegal operands.
%% - `{error, unsupported_operand_type, Data}` if a type other than an int is
%%   used as an operand. `Data` contains `left` and `right` atom keys pointing
%%   to the illegal operands.
forms(RufusForms) ->
    forms(RufusForms, RufusForms).

%% Private API

forms([H|T], Forms) ->
    case check_expr(H) of
        ok ->
            forms(T, Forms);
        Error ->
            Error
    end;
forms([], Forms) ->
    {ok, Forms}.

check_expr({func, #{exprs := Exprs}}) ->
    check_binary_op_exprs(Exprs);
check_expr(_) ->
    ok.

check_binary_op_exprs([{binary_op, #{op := Op, left := [Left], right := [Right]}}|T]) ->
    {_, #{type := {type, #{spec := LeftType}}}} = Left,
    {_, #{type := {type, #{spec := RightType}}}} = Right,

    LeftTypeSupported = check_operand_type_supported(LeftType),
    RightTypeSupported = check_operand_type_supported(RightType),
    OperandTypesMatch = check_operand_types_match(LeftType, RightType),
    Data = #{op => Op, left => Left, right => Right},
    case check_outcomes(Data, LeftTypeSupported, RightTypeSupported, OperandTypesMatch) of
        ok ->
            check_binary_op_exprs(T);
        Error ->
            Error
    end;
check_binary_op_exprs([_|T]) ->
    check_binary_op_exprs(T);
check_binary_op_exprs([]) ->
    ok.

check_operand_types_match(_Type, _Type) ->
    ok;
check_operand_types_match(_LeftType, _RightType) ->
    error.

check_operand_type_supported(float) ->
    ok;
check_operand_type_supported(int) ->
    ok;
check_operand_type_supported(_) ->
    error.

check_outcomes(Data, error, _RightTypeSupported, _OperandTypesMatch) ->
    {error, unsupported_operand_type, Data};
check_outcomes(Data, _LeftTypeSupported, error, _OperandTypesMatch) ->
    {error, unsupported_operand_type, Data};
check_outcomes(Data, _LeftTypeSupported, _RightTypeSupported, error) ->
    {error, unmatched_operand_type, Data};
check_outcomes(_Data, _LeftTypeSupported, _RightTypeSupported, _OperandTypesMatch) ->
    ok.
