-module(rufus_compile_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a sum of literal values for scalar types

eval_with_function_returning_a_sum_of_int_literals_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 19 + 23 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'FortyTwo'()).

eval_with_function_returning_a_sum_of_three_int_literals_test() ->
    RufusText = "
    module example
    func FiftyNine() int { 19 + 23 + 17 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(59, example:'FiftyNine'()).

eval_with_function_returning_a_sum_of_float_literals_test() ->
    RufusText = "
    module example
    func Pi() float { 1.0 + 2.14159265359 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'Pi'()).

%% Arity-0 functions returning a difference of literal values for scalar types

eval_with_function_returning_a_difference_of_int_literals_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 55 - 13 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'FortyTwo'()).

eval_with_function_returning_a_difference_of_three_int_literals_test() ->
    RufusText = "
    module example
    func ThirteenThirtyFive() int { 1500 - 150 - 15 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(1335, example:'ThirteenThirtyFive'()).

eval_with_function_returning_a_difference_of_float_literals_test() ->
    RufusText = "
    module example
    func Pi() float { 4.14159265359 - 1.0 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'Pi'()).

%% Arity-0 functions returning a product of literal values for scalar types

eval_with_function_returning_a_product_of_int_literals_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 3 * 14 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'FortyTwo'()).

eval_with_function_returning_a_product_of_three_int_literals_test() ->
    RufusText = "
    module example
    func ThirteenThirtyFive() int { 3 * 5 * 89 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(1335, example:'ThirteenThirtyFive'()).

eval_with_function_returning_a_product_of_float_literals_test() ->
    RufusText = "
    module example
    func Pi() float { 1.0 * 3.14159265359 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'Pi'()).

%% Arity-0 functions returning a division of literal values for scalar types

eval_with_function_returning_a_division_of_int_literals_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 84 / 2 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'FortyTwo'()).

eval_with_function_returning_a_division_of_three_int_literals_test() ->
    RufusText = "
    module example
    func Five() int { 100 / 10 / 2 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(5, example:'Five'()).

eval_with_function_returning_a_division_of_float_literals_test() ->
    RufusText = "
    module example
    func TwoPointSevenFive() float { 5.5 / 2.0 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(2.75, example:'TwoPointSevenFive'()).

%% Arity-0 functions returning a division of literal values for int types

eval_with_function_returning_a_remainder_of_int_literals_test() ->
    RufusText = "
    module example
    func Six() int { 27 % 7 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(6, example:'Six'()).

eval_with_function_returning_a_remainder_of_three_int_literals_test() ->
    RufusText = "
    module example
    func Four() int { 100 % 13 % 5 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(4, example:'Four'()).