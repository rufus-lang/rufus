-module(rufus_compile_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a sum of literal values for scalar types

eval_with_function_returning_a_sum_of_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() int { 19 + 23 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'FortyTwo'()).

eval_with_function_returning_a_sum_of_three_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FiftyNine() int { 19 + 23 + 17 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(59, example:'FiftyNine'()).

eval_with_function_returning_a_sum_of_float_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Pi() float { 1.0 + 2.14159265359 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'Pi'()).

%% Arity-0 functions returning a difference of literal values for scalar types

eval_with_function_returning_a_difference_of_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() int { 55 - 13 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'FortyTwo'()).

eval_with_function_returning_a_difference_of_three_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func ThirteenThirtyFive() int { 1500 - 150 - 15 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(1335, example:'ThirteenThirtyFive'()).

eval_with_function_returning_a_difference_of_float_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Pi() float { 4.14159265359 - 1.0 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'Pi'()).

%% Arity-0 functions returning a product of literal values for scalar types

eval_with_function_returning_a_product_of_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() int { 3 * 14 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'FortyTwo'()).

eval_with_function_returning_a_product_of_three_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func ThirteenThirtyFive() int { 3 * 5 * 89 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(1335, example:'ThirteenThirtyFive'()).

eval_with_function_returning_a_product_of_float_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Pi() float { 1.0 * 3.14159265359 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'Pi'()).

%% Arity-0 functions returning a division of literal values for scalar types

eval_with_function_returning_a_division_of_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() int { 84 / 2 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'FortyTwo'()).

eval_with_function_returning_a_division_of_three_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Five() int { 100 / 10 / 2 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(5, example:'Five'()).

eval_with_function_returning_a_division_of_float_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func TwoPointSevenFive() float { 5.5 / 2.0 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(2.75, example:'TwoPointSevenFive'()).

%% Arity-0 functions returning a division of literal values for int types

eval_with_function_returning_a_remainder_of_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Six() int { 27 % 7 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(6, example:'Six'()).

eval_with_function_returning_a_remainder_of_three_int_literals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Four() int { 100 % 13 % 5 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(4, example:'Four'()).

%% Arity-0 functions returning the result of a boolean operation

eval_with_function_returning_the_result_of_an_and_operation_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { true and false }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(false, example:'Falsy'()).

eval_with_function_returning_the_result_of_an_and_operation_with_a_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func False() bool { false }\n"
        "    func Falsy() bool { true and False() }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(false, example:'Falsy'()).

eval_with_function_returning_the_result_of_an_or_operation_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Truthy() bool { true or false }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Truthy'()).

eval_with_function_returning_the_result_of_an_or_operation_with_a_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func True() bool { true }\n"
        "    func Truthy() bool { True() or false }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Truthy'()).

eval_function_returning_a_boolean_from_a_nested_boolean_operation_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Falsy() bool { false or false and true }\n"
        "    func Truthy() bool { true and true or false }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(false, example:'Falsy'()),
    ?assertEqual(true, example:'Truthy'()).

%% Comparison operators

eval_function_with_an_equality_comparison_operation_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Truthy() bool { :truth == :truth }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Truthy'()).

eval_function_with_an_inequality_comparison_operation_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Truthy() bool { :truth != :fiction }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Truthy'()).

eval_function_with_a_less_than_comparison_operation_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Truthy() bool { 1 < 2 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Truthy'()).

eval_function_with_a_less_than_or_greater_comparison_operation_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Truthy() bool { 1 <= 2 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Truthy'()).

eval_function_with_a_greater_than_comparison_operation_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Truthy() bool { 2 > 1 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Truthy'()).

eval_function_with_a_greater_than_or_greater_comparison_operation_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Truthy() bool { 2 >= 1 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Truthy'()).
