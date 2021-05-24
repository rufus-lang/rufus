-module(rufus_compile_try_catch_after_test).

-include_lib("eunit/include/eunit.hrl").

%% Bare try/catch/after blocks

eval_function_with_bare_catch_block_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "        :error\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(ok, example:'Maybe'()).

eval_function_with_try_and_catch_blocks_both_returning_an_atom_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        :error\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(ok, example:'Maybe'()).

eval_function_with_try_and_catch_blocks_both_returning_a_bool_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() bool {\n"
        "    try {\n"
        "        true\n"
        "    } catch :error {\n"
        "        false\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(true, example:'Maybe'()).

eval_function_with_try_and_catch_blocks_both_returning_a_float_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() float {\n"
        "    try {\n"
        "        42.0\n"
        "    } catch :error {\n"
        "        13.8\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(42.0, example:'Maybe'()).

eval_function_with_try_and_catch_blocks_both_returning_an_int_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() int {\n"
        "    try {\n"
        "        42\n"
        "    } catch :error {\n"
        "        13\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(42, example:'Maybe'()).

eval_function_with_try_and_catch_blocks_both_returning_a_string_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() string {\n"
        "    try {\n"
        "        \"ok\"\n"
        "    } catch :error {\n"
        "        \"error\"\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({string, <<"ok">>}, example:'Maybe'()).

eval_function_with_try_after_block_test() ->
    RufusText =
        "module example\n"
        "func cleanup() atom { :cleanup }\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } after {\n"
        "        cleanup()\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(ok, example:'Maybe'()).

eval_function_with_bare_catch_block_and_an_after_block_test() ->
    RufusText =
        "module example\n"
        "func cleanup() atom { :cleanup }\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "        :error\n"
        "    } after {\n"
        "        cleanup()\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(ok, example:'Maybe'()).

eval_function_with_try_and_multiple_catch_blocks_returning_an_atom_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "    match :error ->\n"
        "        :error\n"
        "    match :failure ->\n"
        "        :failure\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(ok, example:'Maybe'()).

eval_function_with_try_block_in_match_op_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    result = try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        :error\n"
        "    }\n"
        "    result\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(ok, example:'Maybe'()).

eval_function_with_try_catch_and_after_blocks_accessing_variables_from_outer_scope_test() ->
    RufusText =
        "module example\n"
        "func cleanup(value atom) atom { value }\n"
        "func Maybe() atom {\n"
        "    ok = :ok\n"
        "    error = :error\n"
        "    value = :cleanup\n"
        "    try {\n"
        "        ok\n"
        "    } catch :error {\n"
        "        error\n"
        "    } after {\n"
        "        cleanup(value)\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(ok, example:'Maybe'()).

%% try/catch/after blocks with throws

eval_function_with_try_and_catch_blocks_that_catch_a_throw_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    try {\n"
        "        throw :error\n"
        "    } catch :error {\n"
        "        :kaboom\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(kaboom, example:'Explode'()).

eval_function_with_try_and_catch_blocks_that_catch_a_throw_with_different_types_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    try {\n"
        "        throw 42\n"
        "    } catch 42 {\n"
        "        :kaboom\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(kaboom, example:'Explode'()).

eval_function_with_try_catch_and_after_blocks_that_catch_a_throw_with_different_types_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    try {\n"
        "        throw 42\n"
        "    } catch 42 {\n"
        "        :kaboom\n"
        "    } after {\n"
        "        :after\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(kaboom, example:'Explode'()).

eval_function_with_try_catch_and_after_blocks_accessing_variables_from_outer_scope_in_a_throw_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    value = 42\n"
        "    try {\n"
        "        throw value\n"
        "    } catch 42 {\n"
        "        :kaboom\n"
        "    } after {\n"
        "        13\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(kaboom, example:'Explode'()).
