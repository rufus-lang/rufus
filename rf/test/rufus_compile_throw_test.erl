-module(rufus_compile_throw_test).

-include_lib("eunit/include/eunit.hrl").

eval_function_with_throw_atom_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw :kaboom\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertThrow(kaboom, example:'Explode'()).

eval_function_with_throw_bool_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw true\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertThrow(true, example:'Explode'()).

eval_function_with_throw_float_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw 42.0\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertThrow(42.0, example:'Explode'()).

eval_function_with_throw_int_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw 42\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertThrow(42, example:'Explode'()).

eval_function_with_throw_string_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw \"kaboom\"\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertThrow({string, <<"kaboom">>}, example:'Explode'()).

eval_function_with_throw_variable_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    a = :kaboom\n"
        "    throw a\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertThrow(kaboom, example:'Explode'()).

eval_function_with_throw_cons_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    head = 1\n"
        "    tail = list[int]{2, 3, 4}\n"
        "    throw list[int]{head|tail}\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertThrow([1, 2, 3, 4], example:'Explode'()).

eval_function_with_throw_match_op_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw 1 = 1\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertThrow(1, example:'Explode'()).
