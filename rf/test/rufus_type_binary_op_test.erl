-module(rufus_type_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

resolve_arithmetic_binary_op_with_ints_test() ->
    lists:map(fun(Op) ->
        Left = rufus_form:make_literal(int, 5, 9),
        Right = rufus_form:make_literal(int, 7, 9),
        Form = rufus_form:make_binary_op(Op, Left, Right, 9),
        Expected = rufus_form:make_inferred_type(int, 9),
        ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form))
    end, ['+', '-', '*', '/', '%']).

resolve_arithmetic_binary_op_with_floats_test() ->
    lists:map(fun(Op) ->
        Left = rufus_form:make_literal(float, 1.0, 9),
        Right = rufus_form:make_literal(float, 2.14159265359, 9),
        Form = rufus_form:make_binary_op(Op, Left, Right, 9),
        Expected = rufus_form:make_inferred_type(float, 9),
        ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form))
    end, ['+', '-', '*', '/']).

resolve_arithmetic_unmatched_operand_type_error_test() ->
    lists:map(fun(Op) ->
        Left = rufus_form:make_literal(int, 5, 9),
        Right = rufus_form:make_literal(float, 2.14159265359, 9),
        Form = rufus_form:make_binary_op(Op, Left, Right, 9),
        ?assertEqual({error, unmatched_operand_type, #{form => Form}}, rufus_type:resolve(#{}, Form))
    end, ['+', '-', '*', '/']).

resolve_arithmetic_nested_unmatched_operand_type_error_test() ->
    lists:map(fun(Op) ->
        Left = rufus_form:make_literal(float, 13.0, 9),
        Right = rufus_form:make_literal(float, 6.0, 9),
        LeftForm = rufus_form:make_binary_op(Op, Left, Right, 9),
        RightForm = rufus_form:make_literal(int, 23, 9),
        Form = rufus_form:make_binary_op(Op, LeftForm, RightForm, 9),
        ?assertEqual({error, unmatched_operand_type, #{form => Form}}, rufus_type:resolve(#{}, Form))
    end, ['+', '-', '*', '/']).

resolve_arithmetic_unsupported_operand_type_error_test() ->
    lists:map(fun(Op) ->
        Left = rufus_form:make_literal(bool, true, 9),
        Right = rufus_form:make_literal(bool, false, 9),
        Form = rufus_form:make_binary_op(Op, Left, Right, 9),
        ?assertEqual({error, unsupported_operand_type, #{form => Form}}, rufus_type:resolve(#{}, Form))
    end, ['+', '-', '*', '/', '%']).

resolve_boolean_binary_op_with_bools_test() ->
    lists:map(fun(Op) ->
        Left = rufus_form:make_literal(bool, true, 9),
        Right = rufus_form:make_literal(bool, false, 9),
        Form = rufus_form:make_binary_op(Op, Left, Right, 9),
        Expected = rufus_form:make_inferred_type(bool, 9),
        ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form))
    end, ['and', 'or', 'xor']).

resolve_boolean_unsupported_operand_type_error_test() ->
    lists:map(fun(Op) ->
        Left = rufus_form:make_literal(int, 3, 9),
        Right = rufus_form:make_literal(bool, true, 9),
        Form = rufus_form:make_binary_op(Op, Left, Right, 9),
        ?assertEqual({error, unsupported_operand_type, #{form => Form}}, rufus_type:resolve(#{}, Form))
    end, ['and', 'or', 'xor']).

resolve_boolean_nested_unsupported_operand_type_error_test() ->
    lists:map(fun(Op) ->
        Left = rufus_form:make_literal(bool, true, 9),
        Right = rufus_form:make_literal(bool, false, 9),
        LeftForm = rufus_form:make_binary_op(Op, Left, Right, 9),
        RightForm = rufus_form:make_literal(int, 23, 9),
        Form = rufus_form:make_binary_op(Op, LeftForm, RightForm, 9),
        ?assertEqual({error, unsupported_operand_type, #{form => Form}}, rufus_type:resolve(#{}, Form))
    end, ['and', 'or', 'xor']).
