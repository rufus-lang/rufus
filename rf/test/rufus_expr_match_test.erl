-module(rufus_expr_match_test).

-include_lib("eunit/include/eunit.hrl").

%% match expressions that bind the anonymous variable

typecheck_and_annotate_function_with_a_match_that_binds_the_anonymous_variable_test() ->
    RufusText =
        "func Ignore() atom {\n"
        "    _ = :ok\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {func, #{
            exprs =>
                [
                    {match_op, #{
                        left =>
                            {identifier, #{
                                line => 2,
                                locals => #{},
                                spec => '_',
                                type => {type, #{line => 2, spec => atom}}
                            }},
                        line => 2,
                        right =>
                            {atom_lit, #{
                                line => 2,
                                spec => ok,
                                type => {type, #{line => 2, spec => atom}}
                            }},
                        type => {type, #{line => 2, spec => atom}}
                    }}
                ],
            line => 1,
            params => [],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Ignore',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types => [],
                    return_type => {type, #{line => 1, spec => atom}},
                    spec => 'func() atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% match expressions that bind variables

typecheck_and_annotate_function_with_a_match_that_binds_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Ping() atom {\n"
        "        response = :pong\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => atom
                                }}
                        }},
                    line => 4,
                    right =>
                        {atom_lit, #{
                            line => 4,
                            spec => pong,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => atom
                                }}
                        }},
                    type =>
                        {type, #{line => 4, spec => atom}}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{line => 4, spec => atom}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => atom}},
            spec => 'Ping',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => atom}},
                    spec => 'func() atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Truthy() bool {\n"
        "        response = true\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => bool
                                }}
                        }},
                    line => 4,
                    right =>
                        {bool_lit, #{
                            line => 4,
                            spec => true,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => bool
                                }}
                        }},
                    type =>
                        {type, #{line => 4, spec => bool}}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{line => 4, spec => bool}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => bool}},
            spec => 'Truthy',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => bool}},
                    spec => 'func() bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() float {\n"
        "        response = 42.0\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => float
                                }}
                        }},
                    line => 4,
                    right =>
                        {float_lit, #{
                            line => 4,
                            spec => 42.0,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => float
                                }}
                        }},
                    type =>
                        {type, #{line => 4, spec => float}}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{line => 4, spec => float}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => float}},
            spec => 'FortyTwo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => float}},
                    spec => 'func() float'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() int {\n"
        "        response = 42\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    line => 4,
                    right =>
                        {int_lit, #{
                            line => 4,
                            spec => 42,
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    type =>
                        {type, #{line => 4, spec => int}}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{line => 4, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'FortyTwo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Ping() string {\n"
        "        response = \"pong\"\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => string
                                }}
                        }},
                    line => 4,
                    right =>
                        {string_lit, #{
                            line => 4,
                            spec => <<"pong">>,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => string
                                }}
                        }},
                    type =>
                        {type, #{line => 4, spec => string}}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{line => 4, spec => string}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => string}},
            spec => 'Ping',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => string}},
                    spec => 'func() string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_list_of_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() list[int] {\n"
        "        response = list[int]{42}\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 4,
                    right =>
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 4,
                                    spec => 42,
                                    type =>
                                        {type, #{
                                            line => 4,
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
                                            spec => int
                                        }},
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 4, spec => int}},
                            line => 4,
                            spec => 'list[int]'
                        }}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 4, spec => int}},
                            line => 4,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{line => 3, spec => int}},
                    line => 3,
                    spec => 'list[int]'
                }},
            spec => 'FortyTwo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec => 'func() list[int]'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_list_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Unbox(names list[string]) string {\n"
        "        list[string]{name} = names\n"
        "        name\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {list_lit, #{
                            elements => [
                                {identifier, #{
                                    line => 4,
                                    locals => #{
                                        names => [
                                            {type, #{
                                                kind => list,
                                                element_type =>
                                                    {type, #{
                                                        line => 3,
                                                        spec => string
                                                    }},
                                                line => 3,
                                                spec => 'list[string]'
                                            }}
                                        ]
                                    },
                                    spec => name,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => string
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
                                            spec => string
                                        }},
                                    line => 4,
                                    spec => 'list[string]'
                                }}
                        }},
                    line => 4,
                    right =>
                        {identifier, #{
                            line => 4,
                            spec => names,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 3,
                                            spec => string
                                        }},
                                    line => 3,
                                    spec => 'list[string]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 3,
                                    spec => string
                                }},
                            line => 3,
                            spec => 'list[string]'
                        }}
                }},
                {identifier, #{
                    line => 5,
                    spec => name,
                    type =>
                        {type, #{
                            line => 4,
                            spec => string
                        }}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => names,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 3,
                                    spec => string
                                }},
                            line => 3,
                            spec => 'list[string]'
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => string}},
            spec => 'Unbox',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{
                                    line => 3,
                                    spec => string
                                }},
                            line => 3,
                            spec => 'list[string]'
                        }}
                    ],
                    return_type =>
                        {type, #{line => 3, spec => string}},
                    spec => 'func(list[string]) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_cons_test() ->
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
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {cons, #{
                            head =>
                                {identifier, #{
                                    line => 4,
                                    locals => #{
                                        items => [
                                            {type, #{
                                                kind => list,
                                                element_type =>
                                                    {type, #{
                                                        line => 3,
                                                        spec => int
                                                    }},
                                                line => 3,
                                                spec => 'list[int]'
                                            }}
                                        ]
                                    },
                                    spec => head,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
                            line => 4,
                            tail =>
                                {identifier, #{
                                    line => 4,
                                    locals => #{
                                        head => [
                                            {type, #{
                                                line => 4,
                                                spec => int
                                            }}
                                        ],
                                        items => [
                                            {type, #{
                                                kind => list,
                                                element_type =>
                                                    {type, #{
                                                        line => 3,
                                                        spec => int
                                                    }},
                                                line => 3,
                                                spec => 'list[int]'
                                            }}
                                        ]
                                    },
                                    spec => tail,
                                    type =>
                                        {type, #{
                                            kind => list,
                                            element_type =>
                                                {type, #{
                                                    line => 4,
                                                    spec => int
                                                }},
                                            line => 4,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 4,
                    right =>
                        {identifier, #{
                            line => 4,
                            spec => items,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }},
                                    line => 3,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                }},
                {cons, #{
                    head =>
                        {identifier, #{
                            line => 5,
                            spec => head,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => int
                                }}
                        }},
                    line => 5,
                    tail =>
                        {identifier, #{
                            line => 5,
                            spec => tail,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 5, spec => int}},
                            line => 5,
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
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{line => 3, spec => int}},
                    line => 3,
                    spec => 'list[int]'
                }},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                    ],
                    return_type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec => 'func(list[int]) list[int]'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_cons_head_test() ->
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
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {cons, #{
                            head =>
                                {identifier, #{
                                    line => 4,
                                    locals => #{
                                        items => [
                                            {type, #{
                                                kind => list,
                                                element_type =>
                                                    {type, #{
                                                        line => 3,
                                                        spec => int
                                                    }},
                                                line => 3,
                                                spec => 'list[int]'
                                            }}
                                        ]
                                    },
                                    spec => head,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
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
                                                    spec => int
                                                }}
                                        }},
                                        {int_lit, #{
                                            line => 4,
                                            spec => 3,
                                            type =>
                                                {type, #{
                                                    line => 4,
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
                                                    spec => int
                                                }},
                                            line => 4,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 4,
                    right =>
                        {identifier, #{
                            line => 4,
                            spec => items,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }},
                                    line => 3,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                }},
                {identifier, #{
                    line => 5,
                    spec => head,
                    type =>
                        {type, #{line => 4, spec => int}}
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
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'First',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                    ],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func(list[int]) int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_cons_tail_test() ->
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
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {cons, #{
                            head =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
                            line => 4,
                            tail =>
                                {identifier, #{
                                    line => 4,
                                    locals => #{
                                        items => [
                                            {type, #{
                                                kind => list,
                                                element_type =>
                                                    {type, #{
                                                        line => 3,
                                                        spec => int
                                                    }},
                                                line => 3,
                                                spec => 'list[int]'
                                            }}
                                        ]
                                    },
                                    spec => tail,
                                    type =>
                                        {type, #{
                                            kind => list,
                                            element_type =>
                                                {type, #{
                                                    line => 4,
                                                    spec => int
                                                }},
                                            line => 4,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 4,
                    right =>
                        {identifier, #{
                            line => 4,
                            spec => items,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }},
                                    line => 3,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                }},
                {identifier, #{
                    line => 5,
                    spec => tail,
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 4, spec => int}},
                            line => 4,
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
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{line => 3, spec => int}},
                    line => 3,
                    spec => 'list[int]'
                }},
            spec => 'Rest',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                    ],
                    return_type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec => 'func(list[int]) list[int]'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_taking_a_match_pattern_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Double(b = a int) int {\n"
        "        a + b\n"
        "    }\n"
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
                            line => 4,
                            spec => a,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
                    line => 4,
                    op => '+',
                    right =>
                        {identifier, #{
                            line => 4,
                            spec => b,
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
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            locals => #{
                                a => [
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }}
                                ]
                            },
                            spec => b,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
                    line => 3,
                    right =>
                        {param, #{
                            line => 3,
                            spec => a,
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
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'Double',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => int}}],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func(int) int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_taking_a_match_pattern_with_a_literal_left_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFortyTwo(42 = a int) int {\n"
        "        a\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {identifier, #{
                    line => 4,
                    spec => a,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 3,
            params => [
                {match_op, #{
                    left =>
                        {int_lit, #{
                            line => 3,
                            spec => 42,
                            type =>
                                {type, #{line => 3, spec => int}}
                        }},
                    line => 3,
                    right =>
                        {param, #{
                            line => 3,
                            spec => a,
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
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'EchoFortyTwo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => int}}],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func(int) int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_taking_a_cons_in_match_pattern_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func First(list[int]{head|tail} = items list[int]) int {\n"
        "        head\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {identifier, #{
                    line => 4,
                    spec => head,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 3,
            params => [
                {match_op, #{
                    left =>
                        {cons, #{
                            head =>
                                {identifier, #{
                                    line => 3,
                                    locals => #{
                                        items => [
                                            {type, #{
                                                kind => list,
                                                element_type =>
                                                    {type, #{
                                                        line => 3,
                                                        spec => int
                                                    }},
                                                line => 3,
                                                spec => 'list[int]'
                                            }}
                                        ]
                                    },
                                    spec => head,
                                    type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }}
                                }},
                            line => 3,
                            tail =>
                                {identifier, #{
                                    line => 3,
                                    locals => #{
                                        head => [
                                            {type, #{
                                                line => 3,
                                                spec => int
                                            }}
                                        ],
                                        items => [
                                            {type, #{
                                                kind => list,
                                                element_type =>
                                                    {type, #{
                                                        line => 3,
                                                        spec => int
                                                    }},
                                                line => 3,
                                                spec => 'list[int]'
                                            }}
                                        ]
                                    },
                                    spec => tail,
                                    type =>
                                        {type, #{
                                            kind => list,
                                            element_type =>
                                                {type, #{
                                                    line => 3,
                                                    spec => int
                                                }},
                                            line => 3,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }},
                                    line => 3,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 3,
                    right =>
                        {param, #{
                            line => 3,
                            spec => items,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }},
                                    line => 3,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'First',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }}
                    ],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func(list[int]) int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_has_left_and_right_operands_with_mismatched_types_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Random() int {\n"
        "        2.0 = 2\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        globals => #{
            'Random' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        left =>
            {float_lit, #{
                line => 4,
                spec => 2.0,
                type =>
                    {type, #{line => 4, spec => float}}
            }},
        locals => #{},
        right =>
            {int_lit, #{
                line => 4,
                spec => 2,
                type =>
                    {type, #{line => 4, spec => int}}
            }}
    },
    ?assertEqual({error, unmatched_types, Data}, rufus_expr:typecheck_and_annotate(Forms)).

%% match expressions involving binary_op expressions

typecheck_and_annotate_function_with_a_match_that_has_a_left_binary_op_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Random() int {\n"
        "        n = 3\n"
        "        1 + 2 = n\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => n,
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    line => 4,
                    right =>
                        {int_lit, #{
                            line => 4,
                            spec => 3,
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    type =>
                        {type, #{line => 4, spec => int}}
                }},
                {match_op, #{
                    left =>
                        {binary_op, #{
                            left =>
                                {int_lit, #{
                                    line => 5,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            spec => int
                                        }}
                                }},
                            line => 5,
                            op => '+',
                            right =>
                                {int_lit, #{
                                    line => 5,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            spec => int
                                        }}
                                }},
                            type =>
                                {type, #{line => 5, spec => int}}
                        }},
                    line => 5,
                    right =>
                        {identifier, #{
                            line => 5,
                            spec => n,
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    type =>
                        {type, #{line => 4, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'Random',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_has_a_right_binary_op_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Random() int { n = 1 + 2 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            locals => #{},
                            spec => n,
                            type =>
                                {type, #{line => 3, spec => int}}
                        }},
                    line => 3,
                    right =>
                        {binary_op, #{
                            left =>
                                {int_lit, #{
                                    line => 3,
                                    spec => 1,
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
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }}
                                }},
                            type =>
                                {type, #{line => 3, spec => int}}
                        }},
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'Random',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_has_left_and_right_binary_op_operands_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Random() int {\n"
        "        1 + 2 = 2 + 1\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {binary_op, #{
                            left =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
                            line => 4,
                            op => '+',
                            right =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    line => 4,
                    right =>
                        {binary_op, #{
                            left =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
                            line => 4,
                            op => '+',
                            right =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    type =>
                        {type, #{line => 4, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'Random',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% match expressions involving function calls

typecheck_and_annotate_function_with_a_match_that_has_a_right_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int { n = Two() }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {int_lit, #{
                    line => 3,
                    spec => 2,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'Two',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
        }},
        {func, #{
            exprs => [
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => n,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
                    line => 4,
                    right =>
                        {call, #{
                            args => [],
                            line => 4,
                            spec => 'Two',
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

typecheck_and_annotate_function_with_a_match_that_has_a_left_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int { Two() = 2 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {match_op, #{
                left =>
                    {call, #{
                        args => [],
                        line => 4,
                        spec => 'Two',
                        type =>
                            {type, #{line => 3, spec => int}}
                    }},
                line => 4,
                right =>
                    {int_lit, #{
                        line => 4,
                        spec => 2,
                        type =>
                            {type, #{line => 4, spec => int}}
                    }},
                type =>
                    {type, #{line => 4, spec => int}}
            }},
        globals => #{
            'Random' => [
                {type, #{
                    kind => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, spec => int}},
                    spec => 'func() int'
                }}
            ],
            'Two' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        locals => #{}
    },
    ?assertEqual({error, illegal_pattern, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_a_match_that_has_a_left_binary_op_operand_with_a_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int {\n"
        "        n = 3\n"
        "        1 + Two() = n\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {match_op, #{
                left =>
                    {binary_op, #{
                        left =>
                            {int_lit, #{
                                line => 6,
                                spec => 1,
                                type =>
                                    {type, #{
                                        line => 6,
                                        spec => int
                                    }}
                            }},
                        line => 6,
                        op => '+',
                        right =>
                            {call, #{
                                args => [],
                                line => 6,
                                spec => 'Two',
                                type =>
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }}
                            }},
                        type =>
                            {type, #{line => 6, spec => int}}
                    }},
                line => 6,
                right =>
                    {identifier, #{
                        line => 6,
                        spec => n,
                        type =>
                            {type, #{line => 5, spec => int}}
                    }},
                type =>
                    {type, #{line => 5, spec => int}}
            }},
        globals => #{
            'Random' => [
                {type, #{
                    kind => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, spec => int}},
                    spec => 'func() int'
                }}
            ],
            'Two' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        locals => #{
            n => [{type, #{line => 5, spec => int}}]
        }
    },
    ?assertEqual({error, illegal_pattern, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_a_match_that_has_a_left_binary_op_operand_with_an_unbound_identifer_as_an_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int {\n"
        "        n = 1\n"
        "        1 + n = 2\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {match_op, #{
                left =>
                    {binary_op, #{
                        left =>
                            {int_lit, #{
                                line => 5,
                                spec => 1,
                                type =>
                                    {type, #{
                                        line => 5,
                                        spec => int
                                    }}
                            }},
                        line => 5,
                        op => '+',
                        right =>
                            {identifier, #{
                                line => 5,
                                spec => n,
                                type =>
                                    {type, #{
                                        line => 4,
                                        spec => int
                                    }}
                            }},
                        type =>
                            {type, #{line => 5, spec => int}}
                    }},
                line => 5,
                right =>
                    {int_lit, #{
                        line => 5,
                        spec => 2,
                        type =>
                            {type, #{line => 5, spec => int}}
                    }},
                type =>
                    {type, #{line => 5, spec => int}}
            }},
        globals => #{
            'Two' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        locals => #{
            n => [{type, #{line => 4, spec => int}}]
        }
    },
    ?assertEqual({error, illegal_pattern, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_a_match_that_has_a_left_and_right_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int { Two() = Two() }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {match_op, #{
                left =>
                    {call, #{
                        args => [],
                        line => 4,
                        spec => 'Two',
                        type =>
                            {type, #{line => 3, spec => int}}
                    }},
                line => 4,
                right =>
                    {call, #{
                        args => [],
                        line => 4,
                        spec => 'Two',
                        type =>
                            {type, #{line => 3, spec => int}}
                    }},
                type =>
                    {type, #{line => 3, spec => int}}
            }},
        globals => #{
            'Random' => [
                {type, #{
                    kind => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, spec => int}},
                    spec => 'func() int'
                }}
            ],
            'Two' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        locals => #{}
    },
    ?assertEqual({error, illegal_pattern, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_a_match_that_has_a_right_call_operand_with_a_mismatched_left_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int {\n"
        "        n = \"hello\"\n"
        "        n = Two()\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        globals => #{
            'Random' => [
                {type, #{
                    kind => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, spec => int}},
                    spec => 'func() int'
                }}
            ],
            'Two' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        left =>
            {identifier, #{
                line => 6,
                spec => n,
                type =>
                    {type, #{line => 5, spec => string}}
            }},
        locals => #{
            n => [{type, #{line => 5, spec => string}}]
        },
        right =>
            {call, #{
                args => [],
                line => 6,
                spec => 'Two',
                type =>
                    {type, #{line => 3, spec => int}}
            }}
    },
    ?assertEqual({error, unmatched_types, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_a_match_that_has_a_right_call_operand_with_a_mismatched_arg_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n int) int { n }\n"
        "    func Random() int {\n"
        "        2 = Echo(:two)\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        args => [
            {atom_lit, #{
                line => 5,
                spec => two,
                type =>
                    {type, #{line => 5, spec => atom}}
            }}
        ],
        types => [
            {type, #{
                kind => func,
                line => 3,
                param_types => [{type, #{line => 3, spec => int}}],
                return_type =>
                    {type, #{line => 3, spec => int}},
                spec => 'func(int) int'
            }}
        ]
    },
    ?assertEqual({error, unmatched_args, Data}, rufus_expr:typecheck_and_annotate(Forms)).

%% match expressions with type constraint violations

typecheck_and_annotate_function_with_a_match_that_has_an_unbound_variable_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() int {\n"
        "        value = 1\n"
        "        value = unbound\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {identifier, #{
                line => 5,
                locals => #{
                    value => [{type, #{line => 4, spec => int}}]
                },
                spec => unbound
            }},
        globals => #{
            'Broken' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        locals => #{
            value => [{type, #{line => 4, spec => int}}]
        },
        stack => [
            {match_op_right, #{line => 5}},
            {match_op, #{
                left => {identifier, #{line => 5, spec => value}},
                line => 5,
                right => {identifier, #{line => 5, spec => unbound}}
            }},
            {func_exprs, #{line => 3}},
            {func, #{
                exprs => [
                    {match_op, #{
                        left => {identifier, #{line => 4, spec => value}},
                        line => 4,
                        right =>
                            {int_lit, #{
                                line => 4,
                                spec => 1,
                                type =>
                                    {type, #{
                                        line => 4,
                                        spec => int
                                    }}
                            }}
                    }},
                    {match_op, #{
                        left => {identifier, #{line => 5, spec => value}},
                        line => 5,
                        right =>
                            {identifier, #{line => 5, spec => unbound}}
                    }}
                ],
                line => 3,
                locals => #{},
                params => [],
                return_type =>
                    {type, #{line => 3, spec => int}},
                spec => 'Broken',
                type =>
                    {type, #{
                        kind => func,
                        line => 3,
                        param_types => [],
                        return_type =>
                            {type, #{line => 3, spec => int}},
                        spec => 'func() int'
                    }}
            }}
        ]
    },
    ?assertEqual({error, unknown_identifier, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_a_match_that_has_unbound_variables_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() int {\n"
        "        unbound1 = unbound2\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {identifier, #{line => 4, locals => #{}, spec => unbound2}},
        globals => #{
            'Broken' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        locals => #{},
        stack => [
            {match_op_right, #{line => 4}},
            {match_op, #{
                left => {identifier, #{line => 4, spec => unbound1}},
                line => 4,
                right => {identifier, #{line => 4, spec => unbound2}}
            }},
            {func_exprs, #{line => 3}},
            {func, #{
                exprs => [
                    {match_op, #{
                        left =>
                            {identifier, #{line => 4, spec => unbound1}},
                        line => 4,
                        right =>
                            {identifier, #{line => 4, spec => unbound2}}
                    }}
                ],
                line => 3,
                locals => #{},
                params => [],
                return_type =>
                    {type, #{line => 3, spec => int}},
                spec => 'Broken',
                type =>
                    {type, #{
                        kind => func,
                        line => 3,
                        param_types => [],
                        return_type =>
                            {type, #{line => 3, spec => int}},
                        spec => 'func() int'
                    }}
            }}
        ]
    },
    ?assertEqual({error, unknown_identifier, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_a_match_that_has_unmatched_types_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() int {\n"
        "        a = :hello\n"
        "        i = 42\n"
        "        a = i\n"
        "        i\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        globals => #{
            'Broken' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        left =>
            {identifier, #{
                line => 6,
                spec => a,
                type =>
                    {type, #{line => 4, spec => atom}}
            }},
        locals => #{
            a => [{type, #{line => 4, spec => atom}}],
            i => [{type, #{line => 5, spec => int}}]
        },
        right =>
            {identifier, #{
                line => 6,
                spec => i,
                type =>
                    {type, #{line => 5, spec => int}}
            }}
    },
    ?assertEqual({error, unmatched_types, Data}, rufus_expr:typecheck_and_annotate(Forms)).

%% match expressions with case expressions

typecheck_and_annotate_function_with_a_match_that_binds_a_case_expression_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Authenticate(password string) atom {\n"
        "        response = case password {\n"
        "        match \"hunter2\" -> :allow\n"
        "        match _ -> :deny\n"
        "        }\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs =>
                [
                    {match_op, #{
                        left =>
                            {identifier, #{
                                line => 4,
                                locals =>
                                    #{
                                        password =>
                                            [{type, #{line => 3, spec => string}}]
                                    },
                                spec => response,
                                type => {type, #{line => 6, spec => atom}}
                            }},
                        line => 4,
                        right =>
                            {'case', #{
                                clauses =>
                                    [
                                        {case_clause, #{
                                            exprs =>
                                                [
                                                    {atom_lit, #{
                                                        line => 5,
                                                        spec => allow,
                                                        type =>
                                                            {type, #{line => 5, spec => atom}}
                                                    }}
                                                ],
                                            line => 5,
                                            match_expr =>
                                                {string_lit, #{
                                                    line => 5,
                                                    spec => <<"hunter2">>,
                                                    type =>
                                                        {type, #{line => 5, spec => string}}
                                                }},
                                            type => {type, #{line => 5, spec => atom}}
                                        }},
                                        {case_clause, #{
                                            exprs =>
                                                [
                                                    {atom_lit, #{
                                                        line => 6,
                                                        spec => deny,
                                                        type =>
                                                            {type, #{line => 6, spec => atom}}
                                                    }}
                                                ],
                                            line => 6,
                                            match_expr =>
                                                {identifier, #{
                                                    line => 6,
                                                    locals =>
                                                        #{
                                                            password =>
                                                                [
                                                                    {type, #{
                                                                        line => 3, spec => string
                                                                    }}
                                                                ]
                                                        },
                                                    spec => '_',
                                                    type =>
                                                        {type, #{line => 3, spec => string}}
                                                }},
                                            type => {type, #{line => 6, spec => atom}}
                                        }}
                                    ],
                                line => 4,
                                match_expr =>
                                    {identifier, #{
                                        line => 4,
                                        spec => password,
                                        type => {type, #{line => 3, spec => string}}
                                    }},
                                type => {type, #{line => 6, spec => atom}}
                            }},
                        type => {type, #{line => 6, spec => atom}}
                    }},
                    {identifier, #{
                        line => 8,
                        spec => response,
                        type => {type, #{line => 6, spec => atom}}
                    }}
                ],
            line => 3,
            params =>
                [
                    {param, #{
                        line => 3,
                        spec => password,
                        type => {type, #{line => 3, spec => string}}
                    }}
                ],
            return_type => {type, #{line => 3, spec => atom}},
            spec => 'Authenticate',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => string}}],
                    return_type => {type, #{line => 3, spec => atom}},
                    spec => 'func(string) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).
