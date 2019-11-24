-module(rufus_compile_match_test).

-include_lib("eunit/include/eunit.hrl").

eval_a_function_with_a_match_that_binds_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping() atom {
        response = :pong
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(pong, example:'Ping'()).

eval_a_function_with_a_match_that_binds_a_bool_literal_test() ->
    RufusText = "
    module example
    func Truthy() bool {
        response = true
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({bool, true}, example:'Truthy'()).

eval_a_function_with_a_match_that_binds_a_float_literal_test() ->
    RufusText = "
    module example
    func Pi() float {
        response = 3.14159265359
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3.14159265359, example:'Pi'()).

eval_a_function_with_a_match_that_binds_an_int_literal_test() ->
    RufusText = "
    module example
    func FortyTwo() int {
        response = 42
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(42, example:'FortyTwo'()).

eval_a_function_with_a_match_that_binds_a_string_literal_test() ->
    RufusText = "
    module example
    func Greeting() string {
        response = \"hello\"
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({string, <<"hello">>}, example:'Greeting'()).

%% match expressions involving binary_op expressions

typecheck_and_annotate_function_with_a_match_that_has_a_left_binary_op_operand_test() ->
    RufusText = "
    module example
    func Random() int {
        n = 3
        1 + 2 = n
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3, example:'Random'()).

typecheck_and_annotate_function_with_a_match_that_has_a_right_binary_op_operand_test() ->
    RufusText = "
    module example
    func Random() int { n = 1 + 2 }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3, example:'Random'()).

typecheck_and_annotate_function_with_a_match_that_has_left_and_right_binary_op_operands_test() ->
    RufusText = "
    module example
    func Random() int {
        1 + 2 = 2 + 1
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3, example:'Random'()).

%% match expressions involving function calls

typecheck_and_annotate_function_with_a_match_that_has_a_right_call_operand_test() ->
    RufusText = "
    module example
    func Two() int { 2 }
    func Random() int { n = Two() }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(2, example:'Random'()).
