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
