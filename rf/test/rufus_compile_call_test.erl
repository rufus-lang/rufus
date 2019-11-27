-module(rufus_compile_call_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions making function calls

eval_for_function_call_test() ->
    RufusText = "
    module example
    func Four() int { 100 % 13 % 5 }
    func Random() int { Four() }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(4, example:'Random'()).

%% Arity-1 functions making function calls

eval_with_function_call_with_an_atom_argument_test() ->
    RufusText = "
    module example
    func Echo(a atom) atom { a }
    func Random() atom { Echo(:hello) }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(hello, example:'Random'()).

eval_with_function_call_with_a_bool_argument_test() ->
    RufusText = "
    module example
    func Echo(b bool) bool { b }
    func Random() bool { Echo(true) }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({bool, true}, example:'Random'()).

eval_with_function_call_with_a_float_argument_test() ->
    RufusText = "
    module example
    func Echo(n float) float { n }
    func Random() float { Echo(4.0) }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(4.0, example:'Random'()).

eval_with_function_call_with_an_int_argument_test() ->
    RufusText = "
    module example
    func Echo(n int) int { n }
    func Random() int { Echo(4) }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(4, example:'Random'()).

eval_with_function_call_with_a_string_argument_test() ->
    RufusText = "
    module example
    func Echo(t string) string { t }
    func Random() string { Echo(\"hello\") }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"hello">>}, example:'Random'()).

%% Arity-2 functions making function calls

eval_with_function_call_with_two_int_arguments_test() ->
    RufusText = "
    module example
    func Sum(m int, n int) int { m + n }
    func Random() int { Sum(1, 2) }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3, example:'Random'()).

eval_with_function_call_with_two_float_arguments_test() ->
    RufusText = "
    module example
    func Sum(m float, n float) float { m + n }
    func Random() float { Sum(1.2, 2.3) }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.5, example:'Random'()).
