-module(rufus_compile_call_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions making function calls

eval_for_function_call_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Four() int { 100 % 13 % 5 }\n"
        "    func Random() int { Four() }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(4, example:'Random'()).

%% Arity-1 functions making function calls

eval_with_function_call_with_an_atom_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(a atom) atom { a }\n"
        "    func Random() atom { Echo(:hello) }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(hello, example:'Random'()).

eval_with_function_call_with_a_bool_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(b bool) bool { b }\n"
        "    func Random() bool { Echo(true) }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Random'()).

eval_with_function_call_with_a_float_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n float) float { n }\n"
        "    func Random() float { Echo(4.0) }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(4.0, example:'Random'()).

eval_with_function_call_with_an_int_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n int) int { n }\n"
        "    func Random() int { Echo(4) }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(4, example:'Random'()).

eval_with_function_call_with_a_string_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(t string) string { t }\n"
        "    func Random() string { Echo(\"hello\") }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"hello">>}, example:'Random'()).

%% Arity-2 functions making function calls

eval_with_function_call_with_two_int_arguments_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Sum(m int, n int) int { m + n }\n"
        "    func Random() int { Sum(1, 2) }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3, example:'Random'()).

eval_with_function_call_with_two_float_arguments_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Sum(m float, n float) float { m + n }\n"
        "    func Random() float { Sum(1.2, 2.3) }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.5, example:'Random'()).

%% Function calls with binary_op arguments

eval_with_function_call_with_binary_op_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func double(n int) int { n * 2 }\n"
        "    func SumAndDouble(m int, n int) int { double(m + n) }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(20, example:'SumAndDouble'(3, 7)).

%% Multiple function heads

eval_with_function_call_with_one_argument_and_many_function_heads_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(name atom) atom { name }\n"
        "    func Echo(n float) float { n }\n"
        "    func Echo(n int) int { n }\n"
        "    func Echo(text string) string { text }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(hello, example:'Echo'(hello)),
    ?assertEqual(3.14159265359, example:'Echo'(3.14159265359)),
    ?assertEqual(42, example:'Echo'(42)),
    ?assertEqual({string, <<"hello">>}, example:'Echo'({string, <<"hello">>})).
