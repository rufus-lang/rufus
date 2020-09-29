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
    Data = #{args => [], spec => 'Ping'},
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
        funcs => [
            {func, #{
                params => [
                    {param, #{
                        line => 3,
                        spec => n,
                        type =>
                            {type, #{
                                line => 3,
                                source => rufus_text,
                                spec => string
                            }}
                    }}
                ],
                exprs => [
                    {string_lit, #{
                        line => 3,
                        spec => <<"Hello">>,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => string
                            }}
                    }}
                ],
                line => 3,
                return_type =>
                    {type, #{
                        line => 3,
                        source => rufus_text,
                        spec => string
                    }},
                spec => 'Echo'
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
                    {type, #{
                        line => 4,
                        source => inferred,
                        spec => int
                    }}
            }}
        ],
        funcs => [
            {func, #{
                params => [
                    {param, #{
                        line => 3,
                        spec => n,
                        type =>
                            {type, #{
                                line => 3,
                                source => rufus_text,
                                spec => string
                            }}
                    }}
                ],
                exprs => [
                    {string_lit, #{
                        line => 3,
                        spec => <<"Hello">>,
                        type =>
                            {type, #{
                                line => 3,
                                source => inferred,
                                spec => string
                            }}
                    }}
                ],
                line => 3,
                return_type =>
                    {type, #{
                        line => 3,
                        source => rufus_text,
                        spec => string
                    }},
                spec => 'Echo'
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
                            source => rufus_text,
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
                            source => rufus_text,
                            spec => string
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => string}},
            spec => 'Echo',
            type =>
                {type, #{
                    decl_type => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => string}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => string}},
                    source => rufus_text,
                    spec => 'func (string) string'
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
                                    source => inferred,
                                    spec => string
                                }}
                        }}
                    ],
                    line => 4,
                    spec => 'Echo',
                    type =>
                        {type, #{
                            line => 3,
                            source => rufus_text,
                            spec => string
                        }}
                }}
            ],
            line => 4,
            params => [],
            return_type =>
                {type, #{line => 4, source => rufus_text, spec => string}},
            spec => 'Greeting',
            type =>
                {type, #{
                    decl_type => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, source => rufus_text, spec => string}},
                    source => rufus_text,
                    spec => 'func () string'
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
                                    source => rufus_text,
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
                                    source => rufus_text,
                                    spec => int
                                }}
                        }},
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => m,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }},
                {param, #{
                    line => 3,
                    spec => n,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => int}},
            spec => 'Sum',
            type =>
                {type, #{
                    decl_type => func,
                    line => 3,
                    param_types => [
                        {type, #{line => 3, source => rufus_text, spec => int}},
                        {type, #{line => 3, source => rufus_text, spec => int}}
                    ],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func (int, int) int'
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
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                        {int_lit, #{
                            line => 4,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 4,
                    spec => 'Sum',
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }}
            ],
            line => 4,
            params => [],
            return_type =>
                {type, #{line => 4, source => rufus_text, spec => int}},
            spec => 'Random',
            type =>
                {type, #{
                    decl_type => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func () int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Function calls with binary_op arguments

eval_with_function_call_with_binary_op_argument_test() ->
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
                                    source => rufus_text,
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
                                {type, #{line => 3, source => inferred, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => n,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => int}},
            spec => double,
            type =>
                {type, #{
                    decl_type => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => int}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func (int) int'
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
                                            source => rufus_text,
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
                                            source => rufus_text,
                                            spec => int
                                        }}
                                }},
                            type =>
                                {type, #{
                                    line => 4,
                                    source => rufus_text,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 4,
                    spec => double,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }}
            ],
            line => 4,
            params => [
                {param, #{
                    line => 4,
                    spec => m,
                    type =>
                        {type, #{line => 4, source => rufus_text, spec => int}}
                }},
                {param, #{
                    line => 4,
                    spec => n,
                    type =>
                        {type, #{line => 4, source => rufus_text, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 4, source => rufus_text, spec => int}},
            spec => 'SumAndDouble',
            type =>
                {type, #{
                    decl_type => func,
                    line => 4,
                    param_types => [
                        {type, #{line => 4, source => rufus_text, spec => int}},
                        {type, #{line => 4, source => rufus_text, spec => int}}
                    ],
                    return_type =>
                        {type, #{line => 4, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func (int, int) int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Function calls with match arguments

eval_with_function_call_with_match_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n int) int { n }\n"
        "    func Random() int { 42 = Echo(42) }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {identifier, #{
                    line => 3,
                    spec => n,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => n,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => int}},
            spec => 'Echo',
            type =>
                {type, #{
                    decl_type => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => int}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func (int) int'
                }}
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {int_lit, #{
                            line => 4,
                            spec => 42,
                            type =>
                                {type, #{line => 4, source => inferred, spec => int}}
                        }},
                    line => 4,
                    right =>
                        {call, #{
                            args => [
                                {int_lit, #{
                                    line => 4,
                                    spec => 42,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 4,
                            spec => 'Echo',
                            type =>
                                {type, #{
                                    line => 3,
                                    source => rufus_text,
                                    spec => int
                                }}
                        }},
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => int}}
                }}
            ],
            line => 4,
            params => [],
            return_type =>
                {type, #{line => 4, source => rufus_text, spec => int}},
            spec => 'Random',
            type =>
                {type, #{
                    decl_type => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func () int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).
