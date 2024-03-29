-module(rufus_expr_list_test).

-include_lib("eunit/include/eunit.hrl").

typecheck_and_annotate_with_function_returning_an_empty_list_of_ints_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [],
                    line => 3,
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
            spec => 'Numbers',
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

typecheck_and_annotate_with_function_returning_a_list_of_one_int_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{10} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [
                        {int_lit, #{
                            line => 3,
                            spec => 10,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
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
            spec => 'Numbers',
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

typecheck_and_annotate_with_function_returning_a_list_of_one_int_binary_op_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{10 + 2} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [
                        {binary_op, #{
                            left =>
                                {int_lit, #{
                                    line => 3,
                                    spec => 10,
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
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
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
            spec => 'Numbers',
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

typecheck_and_annotate_with_function_returning_a_list_of_int_with_mismatched_element_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{1, 42.0, 6} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {list_lit, #{
                elements => [
                    {int_lit, #{
                        line => 3,
                        spec => 1,
                        type =>
                            {type, #{
                                line => 3,
                                spec => int
                            }}
                    }},
                    {float_lit, #{
                        line => 3,
                        spec => 42.0,
                        type =>
                            {type, #{
                                line => 3,
                                spec => float
                            }}
                    }},
                    {int_lit, #{
                        line => 3,
                        spec => 6,
                        type =>
                            {type, #{
                                line => 3,
                                spec => int
                            }}
                    }}
                ],
                line => 3,
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
            }}
    },
    ?assertEqual({error, unexpected_element_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_with_function_taking_a_list_and_returning_a_list_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(numbers list[int]) list[int] { numbers }\n"
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
                    spec => numbers,
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
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => numbers,
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

typecheck_and_annotate_with_function_taking_an_int_and_returning_a_list_of_int_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func ToList(n int) list[int] { list[int]{n} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [
                        {identifier, #{
                            line => 3,
                            spec => n,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
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
                {type, #{
                    kind => list,
                    element_type =>
                        {type, #{line => 3, spec => int}},
                    line => 3,
                    spec => 'list[int]'
                }},
            spec => 'ToList',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => int}}],
                    return_type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec => 'func(int) list[int]'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_with_function_returning_a_list_of_int_with_an_unknown_variable_as_an_element_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{unknown} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {identifier, #{line => 3, locals => #{}, spec => unknown}},
        globals => #{
            'Numbers' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            element_type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec => 'func() list[int]'
                }}
            ]
        },
        locals => #{},
        stack => [
            {list_lit, #{
                elements => [{identifier, #{line => 3, spec => unknown}}],
                line => 3,
                type =>
                    {type, #{
                        element_type =>
                            {type, #{line => 3, spec => int}},
                        kind => list,
                        line => 3,
                        spec => 'list[int]'
                    }}
            }},
            {func_exprs, #{line => 3}},
            {func, #{
                exprs => [
                    {list_lit, #{
                        elements => [{identifier, #{line => 3, spec => unknown}}],
                        line => 3,
                        type =>
                            {type, #{
                                element_type =>
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }},
                                kind => list,
                                line => 3,
                                spec => 'list[int]'
                            }}
                    }}
                ],
                line => 3,
                locals => #{},
                params => [],
                return_type =>
                    {type, #{
                        element_type =>
                            {type, #{line => 3, spec => int}},
                        kind => list,
                        line => 3,
                        spec => 'list[int]'
                    }},
                spec => 'Numbers',
                type =>
                    {type, #{
                        kind => func,
                        line => 3,
                        param_types => [],
                        return_type =>
                            {type, #{
                                element_type =>
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }},
                                kind => list,
                                line => 3,
                                spec => 'list[int]'
                            }},
                        spec => 'func() list[int]'
                    }}
            }}
        ]
    },
    ?assertEqual({error, unknown_identifier, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_with_function_returning_a_cons_literal_with_literal_pair_values_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{1|{2}} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {cons, #{
                    head =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, spec => int}}
                        }},
                    line => 3,
                    tail =>
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 3,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 3,
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
            spec => 'Numbers',
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

typecheck_and_annotate_with_function_returning_a_cons_literal_with_multiple_literal_pair_values_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{1|{2, 3, 4}} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {cons, #{
                    head =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, spec => int}}
                        }},
                    line => 3,
                    tail =>
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 3,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 3,
                                    spec => 3,
                                    type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 3,
                                    spec => 4,
                                    type =>
                                        {type, #{
                                            line => 3,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 3,
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
            spec => 'Numbers',
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

typecheck_and_annotate_with_function_returning_a_cons_literal_with_variable_pair_values_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] {\n"
        "        head = 1\n"
        "        tail = list[int]{2, 3, 4}\n"
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
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => head,
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    line => 4,
                    right =>
                        {int_lit, #{
                            line => 4,
                            spec => 1,
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    type =>
                        {type, #{line => 4, spec => int}}
                }},
                {match_op, #{
                    left =>
                        {identifier, #{
                            line => 5,
                            locals => #{
                                head => [
                                    {type, #{
                                        line => 4,
                                        spec => int
                                    }}
                                ]
                            },
                            spec => tail,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 5,
                                            spec => int
                                        }},
                                    line => 5,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 5,
                    right =>
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 5,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 5,
                                    spec => 3,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 5,
                                    spec => 4,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 5,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 5,
                                            spec => int
                                        }},
                                    line => 5,
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
                }},
                {cons, #{
                    head =>
                        {identifier, #{
                            line => 6,
                            spec => head,
                            type =>
                                {type, #{line => 4, spec => int}}
                        }},
                    line => 6,
                    tail =>
                        {identifier, #{
                            line => 6,
                            spec => tail,
                            type =>
                                {type, #{
                                    kind => list,
                                    element_type =>
                                        {type, #{
                                            line => 5,
                                            spec => int
                                        }},
                                    line => 5,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 6, spec => int}},
                            line => 6,
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
            spec => 'Numbers',
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

typecheck_and_annotate_with_function_returning_a_cons_literal_with_an_unexpected_head_value_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() list[int] { list[int]{:unexpected|{2, 3, 4}} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        element_type =>
            {type, #{
                line => 3,
                spec => int
            }},
        form =>
            {cons, #{
                head =>
                    {atom_lit, #{
                        line => 3,
                        spec => unexpected,
                        type =>
                            {type, #{
                                line => 3,
                                spec => atom
                            }}
                    }},
                line => 3,
                tail =>
                    {list_lit, #{
                        elements => [
                            {int_lit, #{
                                line => 3,
                                spec => 2,
                                type =>
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }}
                            }},
                            {int_lit, #{
                                line => 3,
                                spec => 3,
                                type =>
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }}
                            }},
                            {int_lit, #{
                                line => 3,
                                spec => 4,
                                type =>
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }}
                            }}
                        ],
                        line => 3,
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
        head_type =>
            {type, #{
                line => 3,
                spec => atom
            }},
        tail_element_type =>
            {type, #{
                line => 3,
                spec => int
            }}
    },
    ?assertEqual({error, unexpected_element_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_with_function_returning_a_cons_literal_with_an_unexpected_tail_element_value_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() list[int] { list[int]{1|{:unexpected}} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {list_lit, #{
                elements => [
                    {atom_lit, #{
                        line => 3,
                        spec => unexpected,
                        type =>
                            {type, #{
                                line => 3,
                                spec => atom
                            }}
                    }}
                ],
                line => 3,
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
            }}
    },
    ?assertEqual({error, unexpected_element_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_with_function_returning_a_cons_literal_with_an_unexpected_head_identifier_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() list[int] {\n"
        "        unexpected = :unexpected\n"
        "        list[int]{unexpected|{2, 3, 4}} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        element_type =>
            {type, #{
                line => 5,
                spec => int
            }},
        form =>
            {cons, #{
                head =>
                    {identifier, #{
                        line => 5,
                        spec => unexpected,
                        type =>
                            {type, #{
                                line => 4,
                                spec => atom
                            }}
                    }},
                line => 5,
                tail =>
                    {list_lit, #{
                        elements => [
                            {int_lit, #{
                                line => 5,
                                spec => 2,
                                type =>
                                    {type, #{
                                        line => 5,
                                        spec => int
                                    }}
                            }},
                            {int_lit, #{
                                line => 5,
                                spec => 3,
                                type =>
                                    {type, #{
                                        line => 5,
                                        spec => int
                                    }}
                            }},
                            {int_lit, #{
                                line => 5,
                                spec => 4,
                                type =>
                                    {type, #{
                                        line => 5,
                                        spec => int
                                    }}
                            }}
                        ],
                        line => 5,
                        type =>
                            {type, #{
                                kind => list,
                                element_type =>
                                    {type, #{
                                        line => 5,
                                        spec => int
                                    }},
                                line => 5,
                                spec => 'list[int]'
                            }}
                    }},
                type =>
                    {type, #{
                        kind => list,
                        element_type =>
                            {type, #{
                                line => 5,
                                spec => int
                            }},
                        line => 5,
                        spec => 'list[int]'
                    }}
            }},
        head_type =>
            {type, #{
                line => 4,
                spec => atom
            }},
        tail_element_type =>
            {type, #{
                line => 5,
                spec => int
            }}
    },
    ?assertEqual({error, unexpected_element_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_with_function_returning_a_cons_literal_with_an_unexpected_tail_element_identifier_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() list[int] {\n"
        "        unexpected = list[atom]{:unexpected}\n"
        "        list[int]{1|unexpected}\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        element_type =>
            {type, #{
                line => 5,
                spec => int
            }},
        form =>
            {cons, #{
                head =>
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
                tail =>
                    {identifier, #{
                        line => 5,
                        spec => unexpected,
                        type =>
                            {type, #{
                                kind => list,
                                element_type =>
                                    {type, #{
                                        line => 4,
                                        spec => atom
                                    }},
                                line => 4,
                                spec => 'list[atom]'
                            }}
                    }},
                type =>
                    {type, #{
                        kind => list,
                        element_type =>
                            {type, #{
                                line => 5,
                                spec => int
                            }},
                        line => 5,
                        spec => 'list[int]'
                    }}
            }},
        head_type =>
            {type, #{
                line => 5,
                spec => int
            }},
        tail_element_type =>
            {type, #{
                line => 4,
                spec => atom
            }}
    },
    ?assertEqual({error, unexpected_element_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_with_function_taking_a_cons_pattern_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(list[int]{head|tail}) list[int] {\n"
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
                {cons, #{
                    head =>
                        {identifier, #{
                            line => 4,
                            spec => head,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
                    line => 4,
                    tail =>
                        {identifier, #{
                            line => 4,
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
                                {type, #{line => 4, spec => int}},
                            line => 4,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 3,
            params => [
                {cons, #{
                    head =>
                        {identifier, #{
                            line => 3,
                            locals => #{},
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

typecheck_and_annotate_with_function_taking_a_cons_pattern_and_returning_the_head_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func First(list[int]{head|tail}) int {\n"
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
                {cons, #{
                    head =>
                        {identifier, #{
                            line => 3,
                            locals => #{},
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

typecheck_and_annotate_with_function_taking_a_cons_pattern_and_returning_the_tail_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Rest(list[int]{1|tail}) list[int] {\n"
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
                {identifier, #{
                    line => 4,
                    spec => tail,
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
            line => 3,
            params => [
                {cons, #{
                    head =>
                        {int_lit, #{
                            line => 3,
                            spec => 1,
                            type =>
                                {type, #{line => 3, spec => int}}
                        }},
                    line => 3,
                    tail =>
                        {identifier, #{
                            line => 3,
                            locals => #{},
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

typecheck_and_annotate_with_function_returning_a_cons_pattern_without_data_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() list[int] { list[int]{head|tail} }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {identifier, #{line => 3, locals => #{}, spec => head}},
        globals => #{
            'Broken' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            element_type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec => 'func() list[int]'
                }}
            ]
        },
        locals => #{},
        stack => [
            {cons_head, #{line => 3}},
            {cons, #{
                head => {identifier, #{line => 3, spec => head}},
                line => 3,
                tail => {identifier, #{line => 3, spec => tail}},
                type =>
                    {type, #{
                        element_type =>
                            {type, #{line => 3, spec => int}},
                        kind => list,
                        line => 3,
                        spec => 'list[int]'
                    }}
            }},
            {func_exprs, #{line => 3}},
            {func, #{
                exprs => [
                    {cons, #{
                        head => {identifier, #{line => 3, spec => head}},
                        line => 3,
                        tail => {identifier, #{line => 3, spec => tail}},
                        type =>
                            {type, #{
                                element_type =>
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }},
                                kind => list,
                                line => 3,
                                spec => 'list[int]'
                            }}
                    }}
                ],
                line => 3,
                locals => #{},
                params => [],
                return_type =>
                    {type, #{
                        element_type =>
                            {type, #{line => 3, spec => int}},
                        kind => list,
                        line => 3,
                        spec => 'list[int]'
                    }},
                spec => 'Broken',
                type =>
                    {type, #{
                        kind => func,
                        line => 3,
                        param_types => [],
                        return_type =>
                            {type, #{
                                element_type =>
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }},
                                kind => list,
                                line => 3,
                                spec => 'list[int]'
                            }},
                        spec => 'func() list[int]'
                    }}
            }}
        ]
    },
    ?assertEqual({error, unknown_identifier, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_with_function_taking_a_list_lit_pattern_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Reverse(list[int]{a, b, c}) list[int] {\n"
        "        list[int]{c, b, a}\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {list_lit, #{
                    elements => [
                        {identifier, #{
                            line => 4,
                            spec => c,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
                        {identifier, #{
                            line => 4,
                            spec => b,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
                        {identifier, #{
                            line => 4,
                            spec => a,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 4,
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
                {list_lit, #{
                    elements => [
                        {identifier, #{
                            line => 3,
                            locals => #{},
                            spec => a,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }},
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
                        {identifier, #{
                            line => 3,
                            locals => #{
                                a => [
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }}
                                ],
                                b => [
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }}
                                ]
                            },
                            spec => c,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
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
            spec => 'Reverse',
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

typecheck_and_annotate_with_function_returning_a_list_with_a_value_from_a_case_expression_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Listify(n int) list[string] {\n"
        "        list[string]{\n"
        "            case n {\n"
        "            match 1 -> \"one\"\n"
        "            match _ -> \"not one\"\n"
        "            },\n"
        "        }\n"
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
                    {list_lit, #{
                        elements =>
                            [
                                {'case', #{
                                    clauses =>
                                        [
                                            {case_clause, #{
                                                exprs =>
                                                    [
                                                        {string_lit, #{
                                                            line => 6,
                                                            spec => <<"one">>,
                                                            type =>
                                                                {type, #{line => 6, spec => string}}
                                                        }}
                                                    ],
                                                line => 6,
                                                match_expr =>
                                                    {int_lit, #{
                                                        line => 6,
                                                        spec => 1,
                                                        type =>
                                                            {type, #{line => 6, spec => int}}
                                                    }},
                                                type =>
                                                    {type, #{line => 6, spec => string}}
                                            }},
                                            {case_clause, #{
                                                exprs =>
                                                    [
                                                        {string_lit, #{
                                                            line => 7,
                                                            spec => <<"not one">>,
                                                            type =>
                                                                {type, #{line => 7, spec => string}}
                                                        }}
                                                    ],
                                                line => 7,
                                                match_expr =>
                                                    {identifier, #{
                                                        line => 7,
                                                        locals =>
                                                            #{
                                                                n =>
                                                                    [
                                                                        {type, #{
                                                                            line => 3, spec => int
                                                                        }}
                                                                    ]
                                                            },
                                                        spec => '_',
                                                        type =>
                                                            {type, #{line => 3, spec => int}}
                                                    }},
                                                type =>
                                                    {type, #{line => 7, spec => string}}
                                            }}
                                        ],
                                    line => 5,
                                    match_expr =>
                                        {identifier, #{
                                            line => 5,
                                            spec => n,
                                            type => {type, #{line => 3, spec => int}}
                                        }},
                                    type => {type, #{line => 7, spec => string}}
                                }}
                            ],
                        line => 4,
                        type =>
                            {type, #{
                                element_type =>
                                    {type, #{line => 4, spec => string}},
                                kind => list,
                                line => 4,
                                spec => 'list[string]'
                            }}
                    }}
                ],
            line => 3,
            params =>
                [
                    {param, #{
                        line => 3,
                        spec => n,
                        type => {type, #{line => 3, spec => int}}
                    }}
                ],
            return_type =>
                {type, #{
                    element_type => {type, #{line => 3, spec => string}},
                    kind => list,
                    line => 3,
                    spec => 'list[string]'
                }},
            spec => 'Listify',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => int}}],
                    return_type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 3, spec => string}},
                            kind => list,
                            line => 3,
                            spec => 'list[string]'
                        }},
                    spec => 'func(int) list[string]'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).
