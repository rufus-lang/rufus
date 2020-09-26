-module(rufus_expr_match_test).

-include_lib("eunit/include/eunit.hrl").

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
                {match, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
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
                                    source => inferred,
                                    spec => atom
                                }}
                        }},
                    type =>
                        {type, #{line => 4, source => inferred, spec => atom}}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{line => 4, source => inferred, spec => atom}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => atom}},
            spec => 'Ping'
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
        {module, #{
            line => 2,
            spec => example
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
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
                                    source => inferred,
                                    spec => bool
                                }}
                        }},
                    type =>
                        {type, #{
                            line => 4,
                            source => inferred,
                            spec => bool
                        }}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{
                            line => 4,
                            source => inferred,
                            spec => bool
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    line => 3,
                    source => rufus_text,
                    spec => bool
                }},
            spec => 'Truthy'
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
        {module, #{
            line => 2,
            spec => example
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
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
                                    source => inferred,
                                    spec => float
                                }}
                        }},
                    type =>
                        {type, #{
                            line => 4,
                            source => inferred,
                            spec => float
                        }}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{
                            line => 4,
                            source => inferred,
                            spec => float
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    line => 3,
                    source => rufus_text,
                    spec => float
                }},
            spec => 'FortyTwo'
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
        {module, #{
            line => 2,
            spec => example
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                    line => 4,
                    right =>
                        {int_lit, #{
                            line => 4,
                            spec => 42,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                    type =>
                        {type, #{
                            line => 4,
                            source => inferred,
                            spec => int
                        }}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{
                            line => 4,
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
            spec => 'FortyTwo'
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
        {module, #{
            line => 2,
            spec => example
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
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
                                    source => inferred,
                                    spec => string
                                }}
                        }},
                    type =>
                        {type, #{
                            line => 4,
                            source => inferred,
                            spec => string
                        }}
                }},
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{
                            line => 4,
                            source => inferred,
                            spec => string
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    line => 3,
                    source => rufus_text,
                    spec => string
                }},
            spec => 'Ping'
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
        {module, #{
            line => 2,
            spec => example
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => response,
                            type =>
                                {type, #{
                                    collection_type => list,
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
                    right =>
                        {list_lit, #{
                            elements => [
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
                            type =>
                                {type, #{
                                    collection_type => list,
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
                            collection_type => list,
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
                {identifier, #{
                    line => 5,
                    spec => response,
                    type =>
                        {type, #{
                            collection_type => list,
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
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    collection_type => list,
                    element_type =>
                        {type, #{
                            line => 3,
                            source => rufus_text,
                            spec => int
                        }},
                    line => 3,
                    source => rufus_text,
                    spec => 'list[int]'
                }},
            spec => 'FortyTwo'
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
                {match, #{
                    left =>
                        {list_lit, #{
                            elements => [
                                {identifier, #{
                                    line => 4,
                                    locals => #{
                                        names =>
                                            {type, #{
                                                collection_type => list,
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
                                    },
                                    spec => name,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => rufus_text,
                                            spec => string
                                        }}
                                }}
                            ],
                            line => 4,
                            type =>
                                {type, #{
                                    collection_type => list,
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            source => rufus_text,
                                            spec => string
                                        }},
                                    line => 4,
                                    source => rufus_text,
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
                                    collection_type => list,
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
                    type =>
                        {type, #{
                            collection_type => list,
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
                {identifier, #{
                    line => 5,
                    spec => name,
                    type =>
                        {type, #{
                            line => 4,
                            source => rufus_text,
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
                            collection_type => list,
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
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => string}},
            spec => 'Unbox'
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
                {func, #{
                    exprs => [
                        {match, #{
                            left =>
                                {float_lit, #{
                                    line => 4,
                                    spec => 2.0,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            source => inferred,
                                            spec => float
                                        }}
                                }},
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
                    line => 3,
                    params => [],
                    return_type =>
                        {type, #{
                            line => 3,
                            source => rufus_text,
                            spec => int
                        }},
                    spec => 'Random'
                }}
            ]
        },
        left =>
            {float_lit, #{
                line => 4,
                spec => 2.0,
                type =>
                    {type, #{
                        line => 4,
                        source => inferred,
                        spec => float
                    }}
            }},
        locals => #{},
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
        {module, #{
            line => 2,
            spec => example
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => n,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                    line => 4,
                    right =>
                        {int_lit, #{
                            line => 4,
                            spec => 3,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                    type =>
                        {type, #{
                            line => 4,
                            source => inferred,
                            spec => int
                        }}
                }},
                {match, #{
                    left =>
                        {binary_op, #{
                            left =>
                                {int_lit, #{
                                    line => 5,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            source => inferred,
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
                                            source => inferred,
                                            spec => int
                                        }}
                                }},
                            type =>
                                {type, #{
                                    line => 5,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                    line => 5,
                    right =>
                        {identifier, #{
                            line => 5,
                            spec => n,
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                    type =>
                        {type, #{
                            line => 4,
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
            spec => 'Random'
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
        {module, #{
            line => 2,
            spec => example
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 3,
                            locals => #{},
                            spec => n,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => int
                                }}
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
                                            source => inferred,
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
                                            source => inferred,
                                            spec => int
                                        }}
                                }},
                            type =>
                                {type, #{
                                    line => 3,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
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
            spec => 'Random'
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
        {module, #{
            line => 2,
            spec => example
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {binary_op, #{
                            left =>
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
                            op => '+',
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
                                }},
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => int
                                }}
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
                                            source => inferred,
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
                                            source => inferred,
                                            spec => int
                                        }}
                                }},
                            type =>
                                {type, #{
                                    line => 4,
                                    source => inferred,
                                    spec => int
                                }}
                        }},
                    type =>
                        {type, #{
                            line => 4,
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
            spec => 'Random'
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% %% match expressions involving function calls

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
        {module, #{
            line => 2,
            spec => example
        }},
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
        }},
        {func, #{
            exprs => [
                {match, #{
                    left =>
                        {identifier, #{
                            line => 4,
                            locals => #{},
                            spec => n,
                            type =>
                                {type, #{
                                    line => 3,
                                    source => rufus_text,
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
                                    source => rufus_text,
                                    spec => int
                                }}
                        }},
                    type =>
                        {type, #{
                            line => 3,
                            source => rufus_text,
                            spec => int
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
            {match, #{
                left =>
                    {binary_op, #{
                        left =>
                            {int_lit, #{
                                line => 5,
                                spec => 1,
                                type =>
                                    {type, #{
                                        line => 5,
                                        source => inferred,
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
                                        source => inferred,
                                        spec => int
                                    }}
                            }},
                        type =>
                            {type, #{line => 5, source => inferred, spec => int}}
                    }},
                line => 5,
                right =>
                    {int_lit, #{
                        line => 5,
                        spec => 2,
                        type =>
                            {type, #{line => 5, source => inferred, spec => int}}
                    }},
                type =>
                    {type, #{line => 5, source => inferred, spec => int}}
            }},
        globals => #{
            'Two' => [
                {func, #{
                    exprs => [
                        {match, #{
                            left => {identifier, #{line => 4, spec => n}},
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
                                {binary_op, #{
                                    left =>
                                        {int_lit, #{
                                            line => 5,
                                            spec => 1,
                                            type =>
                                                {type, #{
                                                    line => 5,
                                                    source => inferred,
                                                    spec => int
                                                }}
                                        }},
                                    line => 5,
                                    op => '+',
                                    right =>
                                        {identifier, #{line => 5, spec => n}}
                                }},
                            line => 5,
                            right =>
                                {int_lit, #{
                                    line => 5,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 5,
                                            source => inferred,
                                            spec => int
                                        }}
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
                {type, #{line => 4, source => inferred, spec => int}}
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
    ?assertEqual({error, unmatched_args, Data}, rufus_expr:typecheck_and_annotate(Forms)).

%% %% match expressions with type constraint violations

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
            {exprs, #{line => 3}},
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
            {exprs, #{line => 3}},
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
    ?assertEqual({error, unmatched_types, Data}, rufus_expr:typecheck_and_annotate(Forms)).
