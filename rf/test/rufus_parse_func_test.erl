-module(rufus_parse_func_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a literal value for scalar types

parse_function_returning_an_atom_test() ->
    RufusText = "func Color() atom { :indigo }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {atom_lit, #{
                    line => 1,
                    spec => indigo,
                    type =>
                        {type, #{
                            line => 1,
                            spec => atom,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => atom
                }},
            spec => 'Color'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_a_bool_test() ->
    RufusText = "func True() bool { true }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {bool_lit, #{
                    line => 1,
                    spec => true,
                    type =>
                        {type, #{
                            line => 1,
                            spec => bool,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'True'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_a_float_test() ->
    RufusText = "func Pi() float { 3.14159265359 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {float_lit, #{
                    line => 1,
                    spec => 3.14159265359,
                    type =>
                        {type, #{
                            line => 1,
                            spec => float,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => float
                }},
            spec => 'Pi'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_an_int_test() ->
    RufusText = "func Number() int { 42 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {int_lit, #{
                    line => 1,
                    spec => 42,
                    type =>
                        {type, #{
                            line => 1,
                            spec => int,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'Number'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_a_string_test() ->
    RufusText = "func Greeting() string { \"Hello\" }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {string_lit, #{
                    line => 1,
                    spec => <<"Hello">>,
                    type =>
                        {type, #{
                            line => 1,
                            spec => string,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => string
                }},
            spec => 'Greeting'
        }}
    ],
    ?assertEqual(Expected, Forms).

%% Arity-0 functions with multiple function expressions

forms_for_function_with_multiple_expressions_test() ->
    RufusText =
        "\n"
        "    func Multiple() atom {\n"
        "        42\n"
        "        :fortytwo\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {int_lit, #{
                    line => 3,
                    spec => 42,
                    type =>
                        {type, #{
                            line => 3,
                            spec => int
                        }}
                }},
                {atom_lit, #{
                    line => 4,
                    spec => fortytwo,
                    type =>
                        {type, #{
                            line => 4,
                            spec => atom
                        }}
                }}
            ],
            line => 2,
            return_type =>
                {type, #{
                    line => 2,
                    spec => atom
                }},
            spec => 'Multiple'
        }}
    ],
    ?assertEqual(Expected, Forms).

forms_for_function_with_multiple_expressions_with_blank_lines_test() ->
    RufusText =
        "\n"
        "    func Multiple() atom {\n"
        "        42\n"
        "\n"
        "        :fortytwo\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {int_lit, #{
                    line => 3,
                    spec => 42,
                    type =>
                        {type, #{
                            line => 3,
                            spec => int
                        }}
                }},
                {atom_lit, #{
                    line => 5,
                    spec => fortytwo,
                    type =>
                        {type, #{
                            line => 5,
                            spec => atom
                        }}
                }}
            ],
            line => 2,
            return_type =>
                {type, #{
                    line => 2,
                    spec => atom
                }},
            spec => 'Multiple'
        }}
    ],
    ?assertEqual(Expected, Forms).

forms_for_function_with_multiple_expressions_separated_by_semicolons_test() ->
    RufusText = "func Multiple() atom { 42; :fortytwo }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {int_lit, #{
                    line => 1,
                    spec => 42,
                    type =>
                        {type, #{
                            line => 1,
                            spec => int
                        }}
                }},
                {atom_lit, #{
                    line => 1,
                    spec => fortytwo,
                    type =>
                        {type, #{
                            line => 1,
                            spec => atom
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => atom
                }},
            spec => 'Multiple'
        }}
    ],
    ?assertEqual(Expected, Forms).

forms_for_function_with_multiple_expressions_without_end_of_expression_separator_test() ->
    RufusText = "func Multiple() atom { 42 :fortytwo }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {error, Reason} = rufus_parse:parse(Tokens),
    ?assertEqual({1, rufus_parse, ["syntax error before: ", ["fortytwo"]]}, Reason).

%% Arity-1 functions using an argument

parse_function_taking_an_atom_and_returning_an_atom_test() ->
    RufusText = "func Color(c atom) atom { :indigo }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [
                {param, #{
                    line => 1,
                    spec => c,
                    type =>
                        {type, #{
                            line => 1,
                            spec => atom
                        }}
                }}
            ],
            exprs => [
                {atom_lit, #{
                    line => 1,
                    spec => indigo,
                    type =>
                        {type, #{
                            line => 1,
                            spec => atom,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => atom
                }},
            spec => 'Color'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_a_bool_and_returning_a_bool_test() ->
    RufusText = "func Echo(n bool) bool { true }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [
                {param, #{
                    line => 1,
                    spec => n,
                    type =>
                        {type, #{
                            line => 1,
                            spec => bool
                        }}
                }}
            ],
            exprs => [
                {bool_lit, #{
                    line => 1,
                    spec => true,
                    type =>
                        {type, #{
                            line => 1,
                            spec => bool,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_an_float_and_returning_an_float_test() ->
    RufusText = "func Echo(n float) float { 3.14159265359 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [
                {param, #{
                    line => 1,
                    spec => n,
                    type =>
                        {type, #{
                            line => 1,
                            spec => float
                        }}
                }}
            ],
            exprs => [
                {float_lit, #{
                    line => 1,
                    spec => 3.14159265359,
                    type =>
                        {type, #{
                            line => 1,
                            spec => float,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => float
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_an_int_and_returning_an_int_test() ->
    RufusText = "func Echo(n int) int { 42 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [
                {param, #{
                    line => 1,
                    spec => n,
                    type =>
                        {type, #{
                            line => 1,
                            spec => int
                        }}
                }}
            ],
            exprs => [
                {int_lit, #{
                    line => 1,
                    spec => 42,
                    type =>
                        {type, #{
                            line => 1,
                            spec => int,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_an_string_and_returning_an_string_test() ->
    RufusText = "func Echo(n string) string { \"Hello\" }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [
                {param, #{
                    line => 1,
                    spec => n,
                    type =>
                        {type, #{
                            line => 1,
                            spec => string
                        }}
                }}
            ],
            exprs => [
                {string_lit, #{
                    line => 1,
                    spec => <<"Hello">>,
                    type =>
                        {type, #{
                            line => 1,
                            spec => string,
                            source => inferred
                        }}
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => string
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

%% Parameter patterns with literal values

parse_function_taking_an_atom_literal_test() ->
    RufusText = "func Echo(:ok) atom { :ok }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {atom_lit, #{
                    line => 1,
                    spec => ok,
                    type =>
                        {type, #{
                            line => 1,
                            spec => atom
                        }}
                }}
            ],
            line => 1,
            params => [
                {atom_lit, #{
                    line => 1,
                    spec => ok,
                    type =>
                        {type, #{
                            line => 1,
                            spec => atom
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    line => 1,
                    spec => atom
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_a_bool_literal_test() ->
    RufusText = "func Echo(true) bool { true }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {bool_lit, #{
                    line => 1,
                    spec => true,
                    type =>
                        {type, #{
                            line => 1,
                            spec => bool
                        }}
                }}
            ],
            line => 1,
            params => [
                {bool_lit, #{
                    line => 1,
                    spec => true,
                    type =>
                        {type, #{
                            line => 1,
                            spec => bool
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    line => 1,
                    spec => bool
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_a_float_literal_test() ->
    RufusText = "func Echo(1.0) float { 1.0 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {float_lit, #{
                    line => 1,
                    spec => 1.0,
                    type =>
                        {type, #{
                            line => 1,
                            spec => float
                        }}
                }}
            ],
            line => 1,
            params => [
                {float_lit, #{
                    line => 1,
                    spec => 1.0,
                    type =>
                        {type, #{
                            line => 1,
                            spec => float
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    line => 1,
                    spec => float
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_an_int_literal_test() ->
    RufusText = "func Echo(1) int { 1 }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {int_lit, #{
                    line => 1,
                    spec => 1,
                    type =>
                        {type, #{
                            line => 1,
                            spec => int
                        }}
                }}
            ],
            line => 1,
            params => [
                {int_lit, #{
                    line => 1,
                    spec => 1,
                    type =>
                        {type, #{
                            line => 1,
                            spec => int
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_a_string_literal_test() ->
    RufusText = "func Echo(\"ok\") string { \"ok\" }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {string_lit, #{
                    line => 1,
                    spec => <<"ok">>,
                    type =>
                        {type, #{
                            line => 1,
                            spec => string
                        }}
                }}
            ],
            line => 1,
            params => [
                {string_lit, #{
                    line => 1,
                    spec => <<"ok">>,
                    type =>
                        {type, #{
                            line => 1,
                            spec => string
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    line => 1,
                    spec => string
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

%% Anonymous functions

parse_function_taking_and_returning_a_function_test() ->
    RufusText =
        "\n"
        "    func Echo(fn func() int) func() int {\n"
        "        fn\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [{identifier, #{line => 3, spec => fn}}],
            line => 2,
            params => [
                {param, #{
                    line => 2,
                    spec => fn,
                    type =>
                        {type, #{
                            kind => func,
                            line => 2,
                            param_types => [],
                            return_type =>
                                {type, #{line => 2, spec => int}},
                            spec => 'func() int'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type =>
                        {type, #{line => 2, spec => int}},
                    spec => 'func() int'
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_a_function_test() ->
    RufusText =
        "\n"
        "    func NumberFunc() func() int {\n"
        "        func() int { 42 }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {int_lit, #{
                            line => 3,
                            spec => 42,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
                    params => [],
                    return_type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 2,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type =>
                        {type, #{line => 2, spec => int}},
                    spec => 'func() int'
                }},
            spec => 'NumberFunc'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_a_function_variable_test() ->
    RufusText =
        "\n"
        "    func NumberFunc() func() int {\n"
        "        fn = func() int { 21 + 21 }\n"
        "        fn\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {match, #{
                    left => {identifier, #{line => 3, spec => fn}},
                    line => 3,
                    right =>
                        {func, #{
                            exprs => [
                                {binary_op, #{
                                    left =>
                                        {int_lit, #{
                                            line => 3,
                                            spec => 21,
                                            type =>
                                                {type, #{
                                                    line => 3,
                                                    spec => int
                                                }}
                                        }},
                                    line => 3,
                                    op => '+',
                                    right =>
                                        {int_lit, #{
                                            line => 3,
                                            spec => 21,
                                            type =>
                                                {type, #{
                                                    line => 3,
                                                    spec => int
                                                }}
                                        }}
                                }}
                            ],
                            line => 3,
                            params => [],
                            return_type =>
                                {type, #{line => 3, spec => int}}
                        }}
                }},
                {identifier, #{line => 4, spec => fn}}
            ],
            line => 2,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type =>
                        {type, #{line => 2, spec => int}},
                    spec => 'func() int'
                }},
            spec => 'NumberFunc'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_returning_a_nested_function_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func NumberFunc() func() int {\n"
        "        f = func() func() int {\n"
        "            func() int { 42 }\n"
        "        }\n"
        "        f()\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match, #{
                    left => {identifier, #{line => 4, spec => f}},
                    line => 4,
                    right =>
                        {func, #{
                            exprs => [
                                {func, #{
                                    exprs => [
                                        {int_lit, #{
                                            line => 5,
                                            spec => 42,
                                            type =>
                                                {type, #{
                                                    line => 5,
                                                    spec => int
                                                }}
                                        }}
                                    ],
                                    line => 5,
                                    params => [],
                                    return_type =>
                                        {type, #{
                                            line => 5,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 4,
                            params => [],
                            return_type =>
                                {type, #{
                                    kind => func,
                                    line => 4,
                                    param_types => [],
                                    return_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    spec => 'func() int'
                                }}
                        }}
                }},
                {call, #{args => [], line => 7, spec => f}}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }},
            spec => 'NumberFunc'
        }}
    ],
    ?assertEqual(Expected, Forms).
