-module(rufus_expr_call_test).

-include_lib("eunit/include/eunit.hrl").

typecheck_and_annotate_with_function_calling_an_unknown_function_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(text string) string { Ping() }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_expr:typecheck_and_annotate(Forms),
    Data = #{
        form =>
            {call, #{
                args => [],
                line => 3,
                locals => #{
                    text => [
                        {type, #{
                            line => 3,
                            spec => string
                        }}
                    ]
                },
                spec => 'Ping'
            }},
        globals => #{
            'Echo' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            line => 3,
                            spec => string
                        }}
                    ],
                    return_type =>
                        {type, #{
                            line => 3,
                            spec => string
                        }},
                    spec => 'func(string) string'
                }}
            ]
        },
        stack => []
    },
    ?assertEqual({error, unknown_func, Data}, Result).

typecheck_and_annotate_with_function_calling_a_function_with_a_missing_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n string) string { \"Hello\" }\n"
        "    func Broken() string { Echo() }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_expr:typecheck_and_annotate(Forms),
    Data = #{
        args => [],
        types => [
            {type, #{
                kind => func,
                line => 3,
                param_types => [{type, #{line => 3, spec => string}}],
                return_type =>
                    {type, #{line => 3, spec => string}},
                spec => 'func(string) string'
            }}
        ]
    },
    ?assertEqual({error, unknown_arity, Data}, Result).

typecheck_and_annotate_with_function_calling_a_function_with_a_mismatched_argument_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n string) string { \"Hello\" }\n"
        "    func Broken() string { Echo(42) }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_expr:typecheck_and_annotate(Forms),
    Data = #{
        args => [
            {int_lit, #{
                line => 4,
                spec => 42,
                type =>
                    {type, #{line => 4, spec => int}}
            }}
        ],
        types => [
            {type, #{
                kind => func,
                line => 3,
                param_types => [{type, #{line => 3, spec => string}}],
                return_type =>
                    {type, #{line => 3, spec => string}},
                spec => 'func(string) string'
            }}
        ]
    },
    ?assertEqual({error, unmatched_args, Data}, Result).

typecheck_and_annotate_with_function_calling_a_function_with_one_argument_test() ->
    RufusText =
        "\n"
        "    module math\n"
        "    func Echo(text string) string { text }\n"
        "    func Greeting() string { Echo(\"hello\") }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => math}},
        {func, #{
            exprs => [
                {identifier, #{
                    line => 3,
                    spec => text,
                    type =>
                        {type, #{
                            line => 3,
                            spec => string
                        }}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => text,
                    type =>
                        {type, #{
                            line => 3,
                            spec => string
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => string}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => string}}],
                    return_type =>
                        {type, #{line => 3, spec => string}},
                    spec => 'func(string) string'
                }}
        }},
        {func, #{
            exprs => [
                {call, #{
                    args => [
                        {string_lit, #{
                            line => 4,
                            spec => <<"hello">>,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => string
                                }}
                        }}
                    ],
                    line => 4,
                    spec => 'Echo',
                    type =>
                        {type, #{
                            line => 3,
                            spec => string
                        }}
                }}
            ],
            line => 4,
            params => [],
            return_type =>
                {type, #{line => 4, spec => string}},
            spec => 'Greeting',
            type =>
                {type, #{
                    kind => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, spec => string}},
                    spec => 'func() string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_with_function_calling_a_function_with_two_arguments_test() ->
    RufusText =
        "\n"
        "    module math\n"
        "    func Sum(m int, n int) int { m + n }\n"
        "    func Random() int { Sum(1, 2) }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => math}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            spec => m,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
                    line => 3,
                    op => '+',
                    right =>
                        {identifier, #{
                            line => 3,
                            spec => n,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => m,
                    type =>
                        {type, #{line => 3, spec => int}}
                }},
                {param, #{
                    line => 3,
                    spec => n,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'Sum',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{line => 3, spec => int}},
                        {type, #{line => 3, spec => int}}
                    ],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func(int, int) int'
                }}
        }},
        {func, #{
            exprs => [
                {call, #{
                    args => [
                        {int_lit, #{
                            line => 4,
                            spec => 1,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => int
                                }}
                        }},
                        {int_lit, #{
                            line => 4,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 4,
                    spec => 'Sum',
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 4,
            params => [],
            return_type =>
                {type, #{line => 4, spec => int}},
            spec => 'Random',
            type =>
                {type, #{
                    kind => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, spec => int}},
                    spec => 'func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Function calls with binary_op arguments

typecheck_and_annotate_function_call_with_binary_op_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func double(n int) int { n * 2 }\n"
        "    func SumAndDouble(m int, n int) int { double(m + n) }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {binary_op, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            spec => n,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
                    line => 3,
                    op => '*',
                    right =>
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{line => 3, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => n,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => double,
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => int}}],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func(int) int'
                }}
        }},
        {func, #{
            exprs => [
                {call, #{
                    args => [
                        {binary_op, #{
                            left =>
                                {identifier, #{
                                    line => 4,
                                    spec => m,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
                            line => 4,
                            op => '+',
                            right =>
                                {identifier, #{
                                    line => 4,
                                    spec => n,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 4,
                    spec => double,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 4,
            params => [
                {param, #{
                    line => 4,
                    spec => m,
                    type =>
                        {type, #{line => 4, spec => int}}
                }},
                {param, #{
                    line => 4,
                    spec => n,
                    type =>
                        {type, #{line => 4, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 4, spec => int}},
            spec => 'SumAndDouble',
            type =>
                {type, #{
                    kind => func,
                    line => 4,
                    param_types => [
                        {type, #{line => 4, spec => int}},
                        {type, #{line => 4, spec => int}}
                    ],
                    return_type =>
                        {type, #{line => 4, spec => int}},
                    spec => 'func(int, int) int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Function calls with match arguments

typecheck_and_annotate_function_call_with_match_argument_test() ->
    RufusText =
        "func Echo(n int) int { n }\n"
        "func Random() int { 42 = Echo(42) }\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {func, #{
            exprs => [
                {identifier, #{
                    line => 1,
                    spec => n,
                    type =>
                        {type, #{line => 1, spec => int}}
                }}
            ],
            line => 1,
            params => [
                {param, #{
                    line => 1,
                    spec => n,
                    type =>
                        {type, #{line => 1, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 1, spec => int}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => int}}],
                    return_type =>
                        {type, #{line => 1, spec => int}},
                    spec => 'func(int) int'
                }}
        }},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {int_lit, #{
                            line => 2,
                            spec => 42,
                            type =>
                                {type, #{line => 2, spec => int}}
                        }},
                    line => 2,
                    right =>
                        {call, #{
                            args => [
                                {int_lit, #{
                                    line => 2,
                                    spec => 42,
                                    type =>
                                        {type, #{
                                            line => 2,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 2,
                            spec => 'Echo',
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => int
                                }}
                        }},
                    type =>
                        {type, #{line => 1, spec => int}}
                }}
            ],
            line => 2,
            params => [],
            return_type =>
                {type, #{line => 2, spec => int}},
            spec => 'Random',
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
    ?assertEqual(Expected, AnnotatedForms).

%% Function calls with case arguments

typecheck_and_annotate_function_call_with_case_argument_test() ->
    RufusText =
        "func Echo(s string) string { s }\n"
        "func Name(n int) string {\n"
        "    Echo(case n {\n"
        "    match 1 -> \"one\"\n"
        "    match _ -> \"not one\"\n"
        "    })\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {func, #{
            exprs =>
                [
                    {identifier, #{
                        line => 1,
                        spec => s,
                        type => {type, #{line => 1, spec => string}}
                    }}
                ],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => s,
                        type => {type, #{line => 1, spec => string}}
                    }}
                ],
            return_type => {type, #{line => 1, spec => string}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [{type, #{line => 1, spec => string}}],
                    return_type => {type, #{line => 1, spec => string}},
                    spec => 'func(string) string'
                }}
        }},
        {func, #{
            exprs =>
                [
                    {call, #{
                        args =>
                            [
                                {'case', #{
                                    clauses =>
                                        [
                                            {case_clause, #{
                                                exprs =>
                                                    [
                                                        {string_lit, #{
                                                            line => 4,
                                                            spec => <<"one">>,
                                                            type =>
                                                                {type, #{line => 4, spec => string}}
                                                        }}
                                                    ],
                                                line => 4,
                                                match_expr =>
                                                    {int_lit, #{
                                                        line => 4,
                                                        spec => 1,
                                                        type =>
                                                            {type, #{line => 4, spec => int}}
                                                    }},
                                                type =>
                                                    {type, #{line => 4, spec => string}}
                                            }},
                                            {case_clause, #{
                                                exprs =>
                                                    [
                                                        {string_lit, #{
                                                            line => 5,
                                                            spec => <<"not one">>,
                                                            type =>
                                                                {type, #{line => 5, spec => string}}
                                                        }}
                                                    ],
                                                line => 5,
                                                match_expr =>
                                                    {identifier, #{
                                                        line => 5,
                                                        locals =>
                                                            #{
                                                                n =>
                                                                    [
                                                                        {type, #{
                                                                            line => 2, spec => int
                                                                        }}
                                                                    ]
                                                            },
                                                        spec => '_',
                                                        type =>
                                                            {type, #{line => 2, spec => int}}
                                                    }},
                                                type =>
                                                    {type, #{line => 5, spec => string}}
                                            }}
                                        ],
                                    line => 3,
                                    match_expr =>
                                        {identifier, #{
                                            line => 3,
                                            spec => n,
                                            type => {type, #{line => 2, spec => int}}
                                        }},
                                    type => {type, #{line => 5, spec => string}}
                                }}
                            ],
                        line => 3,
                        spec => 'Echo',
                        type => {type, #{line => 1, spec => string}}
                    }}
                ],
            line => 2,
            params =>
                [
                    {param, #{
                        line => 2,
                        spec => n,
                        type => {type, #{line => 2, spec => int}}
                    }}
                ],
            return_type => {type, #{line => 2, spec => string}},
            spec => 'Name',
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [{type, #{line => 2, spec => int}}],
                    return_type => {type, #{line => 2, spec => string}},
                    spec => 'func(int) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).
