-module(rufus_compile_test).

-include_lib("eunit/include/eunit.hrl").

eval_chain_without_handlers_test() ->
    ?assertEqual({ok, "hello!"}, rufus_compile:eval_chain("hello!", [])).

eval_chain_with_ok_handler_test() ->
    Reverse = fun(Word) -> {ok, lists:reverse(Word)} end,
    ?assertEqual({ok, "ahah"}, rufus_compile:eval_chain("haha", [Reverse])).

eval_chain_with_error_handler_test() ->
    Error = fun(_) -> {error, reason} end,
    Explode = fun(_) -> throw(explode) end,
    %% Explode never runs because eval_chain stops iterating over handlers when
    %% it encounters a failure from Error.
    ?assertEqual({error, reason}, rufus_compile:eval_chain("haha", [Error, Explode])).

%% Arity-0 functions returning a literal value for scalar types

eval_with_function_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func True() bool { true }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({bool, true}, example:'True'()).

eval_with_function_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func Pi() float { 3.14159265359 }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3.14159265359, example:'Pi'()).

eval_with_function_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(42, example:'Number'()).

eval_with_function_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func Greeting() string { \"Hello\" }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({string, <<"Hello">>}, example:'Greeting'()).

%% Arity-1 functions taking an unused parameter for scalar types

eval_with_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n bool) bool { true }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({bool, true}, example:'MaybeEcho'({bool, false})).

eval_with_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n float) float { 3.14159265359 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'MaybeEcho'(3.14)).

eval_with_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n int) int { 42 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'MaybeEcho'(42)).

eval_with_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n string) string { \"Hello\" }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"Hello">>}, example:'MaybeEcho'({string, <<"Good morning">>})).

%% Arity-1 functions taking an argument and returning it for scalar types

eval_with_function_taking_a_bool_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(b bool) bool { b }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({bool, false}, example:'Echo'({bool, false})).

eval_with_function_taking_a_float_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(n float) float { n }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'Echo'(3.14159265359)).

eval_with_function_taking_an_int_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(n int) int { n }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'Echo'(42)).

eval_with_function_taking_a_string_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(s string) string { s }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"Hello">>}, example:'Echo'({string, <<"Hello">>})).

%% Type checking return values

eval_with_function_having_unmatched_return_types_test() ->
    RufusText = "
    module example
    func Number() float { 42 }
    ",
    Expected = {error, unmatched_return_type, #{actual => int, expected => float}},
    ?assertEqual(Expected, rufus_compile:eval(RufusText)).

%% Arity-1 functions taking and returning an argument with a mismatched return
%% type

eval_with_function_taking_a_bool_and_returning_it_with_a_mismatched_return_type_test() ->
    RufusText = "
    module example
    func MismatchedReturnType(b bool) int { b }
    ",
    Expected = {error, unmatched_return_type, #{actual => bool, expected => int}},
    ?assertEqual(Expected, rufus_compile:eval(RufusText)).

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
