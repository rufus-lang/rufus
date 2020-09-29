-module(rufus_parse_match_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_with_a_match_that_binds_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    func Ping() atom {\n"
        "        response = :pong\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            spec => response
                        }},
                    line => 3,
                    right =>
                        {atom_lit, #{
                            line => 3,
                            spec => pong,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => atom
                                }}
                        }}
                }},
                {identifier, #{
                    line => 4,
                    spec => response
                }}
            ],
            line => 2,
            params => [],
            return_type =>
                {type, #{
                    line => 2,
                    source => rufus_text,
                    spec => atom
                }},
            spec => 'Ping'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    func Truthy() bool {\n"
        "        response = true\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            spec => response
                        }},
                    line => 3,
                    right =>
                        {bool_lit, #{
                            line => 3,
                            spec => true,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => bool
                                }}
                        }}
                }},
                {identifier, #{
                    line => 4,
                    spec => response
                }}
            ],
            line => 2,
            params => [],
            return_type =>
                {type, #{
                    line => 2,
                    source => rufus_text,
                    spec => bool
                }},
            spec => 'Truthy'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_float_literal_test() ->
    RufusText =
        "\n"
        "    func FortyTwo() float {\n"
        "        response = 42.0\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            spec => response
                        }},
                    line => 3,
                    right =>
                        {float_lit, #{
                            line => 3,
                            spec => 42.0,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => float
                                }}
                        }}
                }},
                {identifier, #{
                    line => 4,
                    spec => response
                }}
            ],
            line => 2,
            params => [],
            return_type =>
                {type, #{
                    line => 2,
                    source => rufus_text,
                    spec => float
                }},
            spec => 'FortyTwo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_string_literal_test() ->
    RufusText =
        "\n"
        "    func Ping() string {\n"
        "        response = \"pong\"\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            spec => response
                        }},
                    line => 3,
                    right =>
                        {string_lit, #{
                            line => 3,
                            spec => <<"pong">>,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => string
                                }}
                        }}
                }},
                {identifier, #{
                    line => 4,
                    spec => response
                }}
            ],
            line => 2,
            params => [],
            return_type =>
                {type, #{
                    line => 2,
                    source => rufus_text,
                    spec => string
                }},
            spec => 'Ping'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_list_literal_test() ->
    RufusText =
        "\n"
        "    func Unbox(names list[string]) string {\n"
        "        list[string]{name} = names\n"
        "        name\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {list_lit, #{
                            elements => [{identifier, #{line => 3, spec => name}}],
                            line => 3,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 3,
                                            source => rufus_text,
                                            spec => string
                                        }},
                                    line => 3,
                                    source => rufus_text,
                                    spec => 'list[string]'
                                }}
                        }},
                    line => 3,
                    right => {identifier, #{line => 3, spec => names}}
                }},
                {identifier, #{line => 4, spec => name}}
            ],
            line => 2,
            params => [
                {param, #{
                    line => 2,
                    spec => names,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 2,
                                    source => rufus_text,
                                    spec => string
                                }},
                            line => 2,
                            source => rufus_text,
                            spec => 'list[string]'
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 2, source => rufus_text, spec => string}},
            spec => 'Unbox'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_cons_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(items list[int]) list[int] {\n"
        "        list[int]{head|tail} = items\n"
        "        list[int]{head|tail}\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {cons, #{
                            head => {identifier, #{line => 4, spec => head}},
                            line => 4,
                            tail => {identifier, #{line => 4, spec => tail}},
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            source => rufus_text,
                                            spec => int
                                        }},
                                    line => 4,
                                    source => rufus_text,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 4,
                    right => {identifier, #{line => 4, spec => items}}
                }},
                {cons, #{
                    head => {identifier, #{line => 5, spec => head}},
                    line => 5,
                    tail => {identifier, #{line => 5, spec => tail}},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 5, source => rufus_text, spec => int}},
                            line => 5,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => items,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, source => rufus_text, spec => int}},
                            line => 3,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    line => 3,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_cons_head_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func First(items list[int]) int {\n"
        "        list[int]{head|list[int]{2, 3}} = items\n"
        "        head\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {cons, #{
                            head => {identifier, #{line => 4, spec => head}},
                            line => 4,
                            tail =>
                                {list_lit, #{
                                    elements => [
                                        {int_lit, #{
                                            line => 4,
                                            spec => 2,
                                            type =>
                                                {type, #{
                                                    line => 4,
                                                    source => inferred,
                                                    spec => int
                                                }}
                                        }},
                                        {int_lit, #{
                                            line => 4,
                                            spec => 3,
                                            type =>
                                                {type, #{
                                                    line => 4,
                                                    source => inferred,
                                                    spec => int
                                                }}
                                        }}
                                    ],
                                    line => 4,
                                    type =>
                                        {type, #{
                                            kind => list,
                                            element_type =>
                                                {type, #{
                                                    line => 4,
                                                    source => rufus_text,
                                                    spec => int
                                                }},
                                            line => 4,
                                            source => rufus_text,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            source => rufus_text,
                                            spec => int
                                        }},
                                    line => 4,
                                    source => rufus_text,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 4,
                    right => {identifier, #{line => 4, spec => items}}
                }},
                {identifier, #{line => 5, spec => head}}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => items,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, source => rufus_text, spec => int}},
                            line => 3,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => int}},
            spec => 'First'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_cons_tail_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Rest(items list[int]) list[int] {\n"
        "        list[int]{1|tail} = items\n"
        "        tail\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {cons, #{
                            head =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => int
                                        }}
                                }},
                            line => 4,
                            tail => {identifier, #{line => 4, spec => tail}},
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            source => rufus_text,
                                            spec => int
                                        }},
                                    line => 4,
                                    source => rufus_text,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 4,
                    right => {identifier, #{line => 4, spec => items}}
                }},
                {identifier, #{line => 5, spec => tail}}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => items,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, source => rufus_text, spec => int}},
                            line => 3,
                            source => rufus_text,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    line => 3,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'Rest'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_variable_test() ->
    RufusText =
        "\n"
        "    func Echo(n int) int {\n"
        "        m = n\n"
        "        m\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),

    Expected = [
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            spec => m
                        }},
                    line => 3,
                    right =>
                        {identifier, #{
                            line => 3,
                            spec => n
                        }}
                }},
                {identifier, #{
                    line => 4,
                    spec => m
                }}
            ],
            line => 2,
            params => [
                {param, #{
                    line => 2,
                    spec => n,
                    type =>
                        {type, #{
                            line => 2,
                            source => rufus_text,
                            spec => int
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    line => 2,
                    source => rufus_text,
                    spec => int
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).

parse_function_taking_a_match_pattern_test() ->
    RufusText = "func Double(b = a int) int { a + b }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            exprs => [
                {binary_op, #{
                    left => {identifier, #{line => 1, spec => a}},
                    line => 1,
                    op => '+',
                    right => {identifier, #{line => 1, spec => b}}
                }}
            ],
            line => 1,
            params => [
                {match, #{
                    left => {identifier, #{line => 1, spec => b}},
                    line => 1,
                    right =>
                        {param, #{
                            line => 1,
                            spec => a,
                            type =>
                                {type, #{
                                    line => 1,
                                    source => rufus_text,
                                    spec => int
                                }}
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 1, source => rufus_text, spec => int}},
            spec => 'Double'
        }}
    ],
    ?assertEqual(Expected, Forms).
