-module(rufus_compile_match_test).

-include_lib("eunit/include/eunit.hrl").

eval_function_with_a_match_that_binds_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Ping() atom {\n"
        "        response = :pong\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(pong, example:'Ping'()).

eval_function_with_a_match_that_binds_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Truthy() bool {\n"
        "        response = true\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(true, example:'Truthy'()).

eval_function_with_a_match_that_binds_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Pi() float {\n"
        "        response = 3.14159265359\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3.14159265359, example:'Pi'()).

eval_function_with_a_match_that_binds_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func FortyTwo() int {\n"
        "        response = 42\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(42, example:'FortyTwo'()).

eval_function_with_a_match_that_binds_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Greeting() string {\n"
        "        response = \"hello\"\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({string, <<"hello">>}, example:'Greeting'()).

eval_function_with_a_match_that_binds_a_list_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Unbox(names list[string]) string {\n"
        "        list[string]{name} = names\n"
        "        name\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({string, <<"Rufus">>}, example:'Unbox'([{string, <<"Rufus">>}])).

eval_function_with_a_match_that_binds_a_cons_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(items list[int]) list[int] {\n"
        "        list[int]{head|tail} = items\n"
        "        list[int]{head|tail}\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual([1, 2, 3], example:'Echo'([1, 2, 3])).

eval_function_with_a_match_that_binds_a_cons_head_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func First(items list[int]) int {\n"
        "        list[int]{head|list[int]{2, 3}} = items\n"
        "        head\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(1, example:'First'([1, 2, 3])).

eval_function_with_a_match_that_binds_a_cons_tail_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Rest(items list[int]) list[int] {\n"
        "        list[int]{1|tail} = items\n"
        "        tail\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual([2, 3], example:'Rest'([1, 2, 3])).

%% match expressions involving binary_op expressions

eval_function_with_a_match_that_has_a_left_binary_op_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Random() int {\n"
        "        n = 3\n"
        "        1 + 2 = n\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3, example:'Random'()).

eval_function_with_a_match_that_has_a_right_binary_op_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Random() int { n = 1 + 2 }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3, example:'Random'()).

eval_function_with_a_match_that_has_left_and_right_binary_op_operands_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Random() int { 1 + 2 = 2 + 1 }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3, example:'Random'()).

%% match expressions involving function calls

eval_function_with_a_match_that_has_a_right_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int { n = Two() }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(2, example:'Random'()).

eval_function_taking_a_match_pattern_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Double(b = a int) int {\n"
        "        a + b\n"
        "    }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(4, example:'Double'(2)).

eval_function_with_a_match_that_has_a_left_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int { Two() = 2 }\n"
        "    ",
    Data = #{
        form =>
            {match, #{
                left =>
                    {call, #{
                        args => [],
                        line => 4,
                        spec => 'Two',
                        type =>
                            {type, #{line => 3, source => rufus_text, spec => int}}
                    }},
                line => 4,
                right =>
                    {int_lit, #{
                        line => 4,
                        spec => 2,
                        type =>
                            {type, #{line => 4, source => inferred, spec => int}}
                    }},
                type =>
                    {type, #{line => 4, source => inferred, spec => int}}
            }},
        globals => #{
            'Random' => [
                {func, #{
                    exprs => [
                        {match, #{
                            left =>
                                {call, #{args => [], line => 4, spec => 'Two'}},
                            line => 4,
                            right =>
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
                        }}
                    ],
                    line => 4,
                    params => [],
                    return_type =>
                        {type, #{line => 4, source => rufus_text, spec => int}},
                    spec => 'Random'
                }}
            ],
            'Two' => [
                {func, #{
                    exprs => [
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
                    params => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    spec => 'Two'
                }}
            ]
        },
        locals => #{}
    },
    ?assertEqual({error, illegal_pattern, Data}, rufus_compile:eval(RufusText)).

eval_function_with_a_match_that_has_a_left_binary_op_operand_with_a_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int {\n"
        "        n = 3\n"
        "        1 + Two() = n\n"
        "    }\n"
        "    ",
    Data = #{
        form =>
            {match, #{
                left =>
                    {binary_op, #{
                        left =>
                            {int_lit, #{
                                line => 6,
                                spec => 1,
                                type =>
                                    {type, #{
                                        line => 6,
                                        source => inferred,
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
                                        source => rufus_text,
                                        spec => int
                                    }}
                            }},
                        type =>
                            {type, #{line => 6, source => inferred, spec => int}}
                    }},
                line => 6,
                right =>
                    {identifier, #{
                        line => 6,
                        spec => n,
                        type =>
                            {type, #{line => 5, source => inferred, spec => int}}
                    }},
                type =>
                    {type, #{line => 5, source => inferred, spec => int}}
            }},
        globals => #{
            'Random' => [
                {func, #{
                    exprs => [
                        {match, #{
                            left => {identifier, #{line => 5, spec => n}},
                            line => 5,
                            right =>
                                {int_lit, #{
                                    line => 5,
                                    spec => 3,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            source => inferred,
                                            spec => int
                                        }}
                                }}
                        }},
                        {match, #{
                            left =>
                                {binary_op, #{
                                    left =>
                                        {int_lit, #{
                                            line => 6,
                                            spec => 1,
                                            type =>
                                                {type, #{
                                                    line => 6,
                                                    source => inferred,
                                                    spec => int
                                                }}
                                        }},
                                    line => 6,
                                    op => '+',
                                    right =>
                                        {call, #{
                                            args => [],
                                            line => 6,
                                            spec => 'Two'
                                        }}
                                }},
                            line => 6,
                            right =>
                                {identifier, #{line => 6, spec => n}}
                        }}
                    ],
                    line => 4,
                    params => [],
                    return_type =>
                        {type, #{line => 4, source => rufus_text, spec => int}},
                    spec => 'Random'
                }}
            ],
            'Two' => [
                {func, #{
                    exprs => [
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
                    params => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    spec => 'Two'
                }}
            ]
        },
        locals => #{
            n =>
                {type, #{line => 5, source => inferred, spec => int}}
        }
    },
    ?assertEqual({error, illegal_pattern, Data}, rufus_compile:eval(RufusText)).

eval_function_with_a_match_that_has_a_left_and_right_call_operand_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int { Two() = Two() }\n"
        "    ",
    Data = #{
        form =>
            {match, #{
                left =>
                    {call, #{
                        args => [],
                        line => 4,
                        spec => 'Two',
                        type =>
                            {type, #{line => 3, source => rufus_text, spec => int}}
                    }},
                line => 4,
                right =>
                    {call, #{
                        args => [],
                        line => 4,
                        spec => 'Two',
                        type =>
                            {type, #{line => 3, source => rufus_text, spec => int}}
                    }},
                type =>
                    {type, #{line => 3, source => rufus_text, spec => int}}
            }},
        globals => #{
            'Random' => [
                {func, #{
                    exprs => [
                        {match, #{
                            left =>
                                {call, #{args => [], line => 4, spec => 'Two'}},
                            line => 4,
                            right =>
                                {call, #{args => [], line => 4, spec => 'Two'}}
                        }}
                    ],
                    line => 4,
                    params => [],
                    return_type =>
                        {type, #{line => 4, source => rufus_text, spec => int}},
                    spec => 'Random'
                }}
            ],
            'Two' => [
                {func, #{
                    exprs => [
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
                    params => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    spec => 'Two'
                }}
            ]
        },
        locals => #{}
    },
    ?assertEqual({error, illegal_pattern, Data}, rufus_compile:eval(RufusText)).

eval_function_with_a_match_that_has_a_right_call_operand_with_a_mismatched_left_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Two() int { 2 }\n"
        "    func Random() int {\n"
        "        n = \"hello\"\n"
        "        n = Two()\n"
        "    }\n"
        "    ",
    Data = #{
        globals => #{
            'Random' => [
                {func, #{
                    exprs => [
                        {match, #{
                            left =>
                                {identifier, #{
                                    line => 5,
                                    spec => n
                                }},
                            line => 5,
                            right =>
                                {string_lit, #{
                                    line => 5,
                                    spec => <<"hello">>,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            source => inferred,
                                            spec => string
                                        }}
                                }}
                        }},
                        {match, #{
                            left =>
                                {identifier, #{
                                    line => 6,
                                    spec => n
                                }},
                            line => 6,
                            right =>
                                {call, #{
                                    args => [],
                                    line => 6,
                                    spec => 'Two'
                                }}
                        }}
                    ],
                    line => 4,
                    params => [],
                    return_type =>
                        {type, #{
                            line => 4,
                            source => rufus_text,
                            spec => int
                        }},
                    spec => 'Random'
                }}
            ],
            'Two' => [
                {func, #{
                    exprs => [
                        {int_lit, #{
                            line => 3,
                            spec => 2,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
                    params => [],
                    return_type =>
                        {type, #{
                            line => 3,
                            source => rufus_text,
                            spec => int
                        }},
                    spec => 'Two'
                }}
            ]
        },
        left =>
            {identifier, #{
                line => 6,
                spec => n,
                type =>
                    {type, #{
                        line => 5,
                        source => inferred,
                        spec => string
                    }}
            }},
        locals => #{
            n =>
                {type, #{
                    line => 5,
                    source => inferred,
                    spec => string
                }}
        },
        right =>
            {call, #{
                args => [],
                line => 6,
                spec => 'Two',
                type =>
                    {type, #{
                        line => 3,
                        source => rufus_text,
                        spec => int
                    }}
            }}
    },
    ?assertEqual({error, unmatched_types, Data}, rufus_compile:eval(RufusText)).

eval_function_with_a_match_that_has_a_right_call_operand_with_a_mismatched_arg_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n int) int { n }\n"
        "    func Random() int {\n"
        "        2 = Echo(:two)\n"
        "    }\n"
        "    ",
    Data = #{
        args => [
            {atom_lit, #{
                line => 5,
                spec => two,
                type =>
                    {type, #{
                        line => 5,
                        source => inferred,
                        spec => atom
                    }}
            }}
        ],
        funcs => [
            {func, #{
                exprs => [
                    {identifier, #{
                        line => 3,
                        spec => n
                    }}
                ],
                line => 3,
                params => [
                    {param, #{
                        line => 3,
                        spec => n,
                        type =>
                            {type, #{
                                line => 3,
                                source => rufus_text,
                                spec => int
                            }}
                    }}
                ],
                return_type =>
                    {type, #{
                        line => 3,
                        source => rufus_text,
                        spec => int
                    }},
                spec => 'Echo'
            }}
        ]
    },
    ?assertEqual({error, unmatched_args, Data}, rufus_compile:eval(RufusText)).

%% match expressions with type constraint violations

eval_function_with_a_match_that_has_an_unbound_variable_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() int {\n"
        "        value = 1\n"
        "        value = unbound\n"
        "    }\n"
        "    ",
    Data = #{
        form =>
            {identifier, #{
                line => 5,
                locals => #{
                    value =>
                        {type, #{line => 4, source => inferred, spec => int}}
                },
                spec => unbound
            }},
        globals => #{
            'Broken' => [
                {func, #{
                    exprs => [
                        {match, #{
                            left =>
                                {identifier, #{line => 4, spec => value}},
                            line => 4,
                            right =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => int
                                        }}
                                }}
                        }},
                        {match, #{
                            left =>
                                {identifier, #{line => 5, spec => value}},
                            line => 5,
                            right =>
                                {identifier, #{line => 5, spec => unbound}}
                        }}
                    ],
                    line => 3,
                    params => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    spec => 'Broken'
                }}
            ]
        },
        locals => #{
            value =>
                {type, #{line => 4, source => inferred, spec => int}}
        },
        stack => [
            {match_right, #{line => 5}},
            {match, #{
                left => {identifier, #{line => 5, spec => value}},
                line => 5,
                right => {identifier, #{line => 5, spec => unbound}}
            }},
            {func_exprs, #{line => 3}},
            {func, #{
                exprs => [
                    {match, #{
                        left => {identifier, #{line => 4, spec => value}},
                        line => 4,
                        right =>
                            {int_lit, #{
                                line => 4,
                                spec => 1,
                                type =>
                                    {type, #{
                                        line => 4,
                                        source => inferred,
                                        spec => int
                                    }}
                            }}
                    }},
                    {match, #{
                        left => {identifier, #{line => 5, spec => value}},
                        line => 5,
                        right =>
                            {identifier, #{line => 5, spec => unbound}}
                    }}
                ],
                line => 3,
                params => [],
                return_type =>
                    {type, #{line => 3, source => rufus_text, spec => int}},
                spec => 'Broken'
            }}
        ]
    },
    ?assertEqual({error, unknown_identifier, Data}, rufus_compile:eval(RufusText)).

eval_function_with_a_match_that_has_unbound_variables_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Broken() int {\n"
        "        unbound1 = unbound2\n"
        "    }\n"
        "    ",
    Data = #{
        form =>
            {identifier, #{line => 4, locals => #{}, spec => unbound2}},
        globals => #{
            'Broken' => [
                {func, #{
                    exprs => [
                        {match, #{
                            left =>
                                {identifier, #{line => 4, spec => unbound1}},
                            line => 4,
                            right =>
                                {identifier, #{line => 4, spec => unbound2}}
                        }}
                    ],
                    line => 3,
                    params => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    spec => 'Broken'
                }}
            ]
        },
        locals => #{},
        stack => [
            {match_right, #{line => 4}},
            {match, #{
                left => {identifier, #{line => 4, spec => unbound1}},
                line => 4,
                right => {identifier, #{line => 4, spec => unbound2}}
            }},
            {func_exprs, #{line => 3}},
            {func, #{
                exprs => [
                    {match, #{
                        left =>
                            {identifier, #{line => 4, spec => unbound1}},
                        line => 4,
                        right =>
                            {identifier, #{line => 4, spec => unbound2}}
                    }}
                ],
                line => 3,
                params => [],
                return_type =>
                    {type, #{line => 3, source => rufus_text, spec => int}},
                spec => 'Broken'
            }}
        ]
    },
    ?assertEqual({error, unknown_identifier, Data}, rufus_compile:eval(RufusText)).

eval_function_with_a_match_that_has_unmatched_types_test() ->
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
    Data = #{
        globals => #{
            'Broken' => [
                {func, #{
                    exprs => [
                        {match, #{
                            left =>
                                {identifier, #{
                                    line => 4,
                                    spec => a
                                }},
                            line => 4,
                            right =>
                                {atom_lit, #{
                                    line => 4,
                                    spec => hello,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => atom
                                        }}
                                }}
                        }},
                        {match, #{
                            left =>
                                {identifier, #{
                                    line => 5,
                                    spec => i
                                }},
                            line => 5,
                            right =>
                                {int_lit, #{
                                    line => 5,
                                    spec => 42,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            source => inferred,
                                            spec => int
                                        }}
                                }}
                        }},
                        {match, #{
                            left =>
                                {identifier, #{
                                    line => 6,
                                    spec => a
                                }},
                            line => 6,
                            right =>
                                {identifier, #{
                                    line => 6,
                                    spec => i
                                }}
                        }},
                        {identifier, #{
                            line => 7,
                            spec => i
                        }}
                    ],
                    line => 3,
                    params => [],
                    return_type =>
                        {type, #{
                            line => 3,
                            source => rufus_text,
                            spec => int
                        }},
                    spec => 'Broken'
                }}
            ]
        },
        left =>
            {identifier, #{
                line => 6,
                spec => a,
                type =>
                    {type, #{
                        line => 4,
                        source => inferred,
                        spec => atom
                    }}
            }},
        locals => #{
            a =>
                {type, #{
                    line => 4,
                    source => inferred,
                    spec => atom
                }},
            i =>
                {type, #{
                    line => 5,
                    source => inferred,
                    spec => int
                }}
        },
        right =>
            {identifier, #{
                line => 6,
                spec => i,
                type =>
                    {type, #{
                        line => 5,
                        source => inferred,
                        spec => int
                    }}
            }}
    },
    ?assertEqual({error, unmatched_types, Data}, rufus_compile:eval(RufusText)).
