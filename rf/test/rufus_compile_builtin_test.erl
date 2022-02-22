-module(rufus_compile_builtin_test).

-include_lib("eunit/include/eunit.hrl").

eval_print_call_with_atom_literal_test() ->
    RufusText =
        "module example\n"
        "func PrintGreeting() atom {\n"
        "    print(:hello)\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintGreeting'()),
    ?assertEqual("hello", ?capturedOutput).

eval_print_call_with_bool_literal_test() ->
    RufusText =
        "module example\n"
        "func PrintTruth() atom {\n"
        "    print(true)\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintTruth'()),
    ?assertEqual("true", ?capturedOutput).

eval_print_call_with_float_literal_test() ->
    RufusText =
        "module example\n"
        "func PrintPi() atom {\n"
        "    print(3.14159265359)\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintPi'()),
    ?assertEqual("3.14159265359", ?capturedOutput).

eval_print_call_with_int_literal_test() ->
    RufusText =
        "module example\n"
        "func PrintPi() atom {\n"
        "    print(42)\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintPi'()),
    ?assertEqual("42", ?capturedOutput).

eval_print_call_with_string_literal_test() ->
    RufusText =
        "module example\n"
        "func PrintGreeting() atom {\n"
        "    print(\"Hello, world!\n\")\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintGreeting'()),
    ?assertEqual("Hello, world!\n", ?capturedOutput).

eval_print_call_with_list_of_atom_literals_test() ->
    RufusText =
        "module example\n"
        "func PrintAtomicParticles() atom {\n"
        "    print(list[atom]{:electron, :proton, :neutron})\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintAtomicParticles'()),
    ?assertEqual("[electron,proton,neutron]", ?capturedOutput).

eval_print_call_with_list_of_bool_literals_test() ->
    RufusText =
        "module example\n"
        "func PrintTruthyValues() atom {\n"
        "    print(list[bool]{true, false})\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintTruthyValues'()),
    ?assertEqual("[true,false]", ?capturedOutput).

eval_print_call_with_list_of_float_literals_test() ->
    RufusText =
        "module example\n"
        "func PrintFloats() atom {\n"
        "    print(list[float]{1.1, 1.34})\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintFloats'()),
    ?assertEqual("[1.1,1.34]", ?capturedOutput).

eval_print_call_with_list_of_int_literals_test() ->
    RufusText =
        "module example\n"
        "func PrintFibonacci() atom {\n"
        "    print(list[int]{0, 1, 1, 2, 3, 5, 8})\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintFibonacci'()),
    ?assertEqual("[0,1,1,2,3,5,8]", ?capturedOutput).

eval_print_call_with_list_of_string_literals_test() ->
    RufusText =
        "module example\n"
        "func PrintGreetings() atom {\n"
        "    print(list[string]{\"Hi\", \"Hello\", \"Hey\"})\n"
        "    :ok\n"
        "}",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'PrintGreetings'()),
    ?assertEqual(
        "[{string,<<\"Hi\">>},{string,<<\"Hello\">>},{string,<<\"Hey\">>}]", ?capturedOutput
    ).
