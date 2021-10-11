-module(rufus_compile_case_test).

-include_lib("eunit/include/eunit.hrl").

eval_function_with_case_block_with_single_atom_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value atom) string {\n"
        "    case value {\n"
        "    match :true ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"true">>}, example:'MaybeConvert'(true)).

eval_function_with_case_block_with_single_bool_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value bool) string {\n"
        "    case value {\n"
        "    match true ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"true">>}, example:'MaybeConvert'(true)).

eval_function_with_case_block_with_single_float_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value float) string {\n"
        "    case value {\n"
        "    match 3.14 ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"true">>}, example:'MaybeConvert'(3.14)).

eval_function_with_case_block_with_single_int_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value int) string {\n"
        "    case value {\n"
        "    match 42 ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"true">>}, example:'MaybeConvert'(42)).

eval_function_with_case_block_with_single_string_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value string) atom {\n"
        "    case value {\n"
        "    match \"ok\" ->\n"
        "        :ok\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'MaybeConvert'({string, <<"ok">>})).

eval_function_with_case_block_with_single_identifier_clause_test() ->
    RufusText =
        "module example\n"
        "func Echo(value string) string {\n"
        "    case value {\n"
        "    match v ->\n"
        "        v\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"hello">>}, example:'Echo'({string, <<"hello">>})).

eval_function_with_case_block_with_single_identifier_and_type_clause_test() ->
    RufusText =
        "module example\n"
        "func Echo(value string) string {\n"
        "    case value {\n"
        "    match v string ->\n"
        "        v\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"hello">>}, example:'Echo'({string, <<"hello">>})).

eval_function_with_case_block_and_unmatched_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value atom) string {\n"
        "    case value {\n"
        "    match :true ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertException(error, {case_clause, false}, example:'MaybeConvert'(false)).

eval_function_with_case_block_with_clause_matching_the_anonymous_variable_test() ->
    RufusText =
        "module example\n"
        "func Convert(value atom) bool {\n"
        "    case value {\n"
        "    match :true ->\n"
        "        true\n"
        "    match _ ->\n"
        "        false\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Convert'(true)),
    ?assertEqual(false, example:'Convert'(false)),
    ?assertEqual(false, example:'Convert'(untrue)).

eval_function_with_case_block_with_anonymous_variable_match_expression_test() ->
    RufusText =
        "module example\n"
        "func Echo(value atom) atom {\n"
        "    case value {\n"
        "    match _ ->\n"
        "        value\n"
        "    }\n"
        "}\n",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Echo'(true)),
    ?assertEqual(false, example:'Echo'(false)),
    ?assertEqual(untrue, example:'Echo'(untrue)).
