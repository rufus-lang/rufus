-module(rufus_expr_func_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

typecheck_and_annotate_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Number() int { 42 }\n"
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
                    spec => 42,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'Number',
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

typecheck_and_annotate_does_not_allow_locals_to_escape_function_scope_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n string) string {\n"
        "        a = n\n"
        "        a\n"
        "    }\n"
        "    func Broken() string { a }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_expr:typecheck_and_annotate(Forms),
    Data = #{
        form => {identifier, #{line => 7, locals => #{}, spec => a}},
        globals => #{
            'Broken' => [
                {type, #{
                    kind => func,
                    line => 7,
                    param_types => [],
                    return_type =>
                        {type, #{
                            line => 7,
                            spec => string
                        }},
                    spec => 'func() string'
                }}
            ],
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
        locals => #{},
        stack => [
            {func_exprs, #{line => 7}},
            {func, #{
                exprs => [{identifier, #{line => 7, spec => a}}],
                line => 7,
                locals => #{},
                params => [],
                return_type =>
                    {type, #{line => 7, spec => string}},
                spec => 'Broken',
                type =>
                    {type, #{
                        kind => func,
                        line => 7,
                        param_types => [],
                        return_type =>
                            {type, #{
                                line => 7,
                                spec => string
                            }},
                        spec => 'func() string'
                    }}
            }}
        ]
    },
    ?assertEqual({error, unknown_identifier, Data}, Result).

typecheck_and_annotate_does_not_rely_on_function_definition_order_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoList() list[int] {\n"
        "        MakeList(list[int]{1, 2})\n"
        "    }\n"
        "    func MakeList(list[int]{1, 2} = items list[int]) list[int] {\n"
        "        items\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {call, #{
                    args => [
                        {list_lit, #{
                            elements => [
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
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }}
                    ],
                    line => 4,
                    spec => 'MakeList',
                    type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 6, spec => int}},
                            kind => list,
                            line => 6,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    element_type =>
                        {type, #{line => 3, spec => int}},
                    kind => list,
                    line => 3,
                    spec => 'list[int]'
                }},
            spec => 'EchoList',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec => 'func() list[int]'
                }}
        }},
        {func, #{
            exprs => [
                {identifier, #{
                    line => 7,
                    spec => items,
                    type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 6, spec => int}},
                            kind => list,
                            line => 6,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 6,
            params => [
                {match_op, #{
                    left =>
                        {list_lit, #{
                            elements => [
                                {int_lit, #{
                                    line => 6,
                                    spec => 1,
                                    type =>
                                        {type, #{
                                            line => 6,
                                            spec => int
                                        }}
                                }},
                                {int_lit, #{
                                    line => 6,
                                    spec => 2,
                                    type =>
                                        {type, #{
                                            line => 6,
                                            spec => int
                                        }}
                                }}
                            ],
                            line => 6,
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 6,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 6,
                                    spec => 'list[int]'
                                }}
                        }},
                    line => 6,
                    right =>
                        {param, #{
                            line => 6,
                            spec => items,
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 6,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 6,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 6, spec => int}},
                            kind => list,
                            line => 6,
                            spec => 'list[int]'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    element_type =>
                        {type, #{line => 6, spec => int}},
                    kind => list,
                    line => 6,
                    spec => 'list[int]'
                }},
            spec => 'MakeList',
            type =>
                {type, #{
                    kind => func,
                    line => 6,
                    param_types => [
                        {type, #{
                            element_type =>
                                {type, #{line => 6, spec => int}},
                            kind => list,
                            line => 6,
                            spec => 'list[int]'
                        }}
                    ],
                    return_type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 6, spec => int}},
                            kind => list,
                            line => 6,
                            spec => 'list[int]'
                        }},
                    spec => 'func(list[int]) list[int]'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Arity-1 functions taking an anonymous argument and returning a literal

typecheck_and_annotate_for_function_taking_an_anonymous_argument_and_returning_an_atom_literal_test() ->
    RufusText =
        "func Ignore(_ atom) atom {\n"
        "    :ok\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {func, #{
            exprs =>
                [
                    {atom_lit, #{
                        line => 2,
                        spec => ok,
                        type => {type, #{line => 2, spec => atom}}
                    }}
                ],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => '_',
                        type => {type, #{line => 1, spec => atom}}
                    }}
                ],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Ignore',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types =>
                        [{type, #{line => 1, spec => atom}}],
                    return_type =>
                        {type, #{line => 1, spec => atom}},
                    spec => 'func(atom) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_more_than_one_anonymous_argument_and_returning_an_atom_literal_test() ->
    RufusText =
        "func Ignore(_ atom, _ atom) atom {\n"
        "    :ok\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {func, #{
            exprs =>
                [
                    {atom_lit, #{
                        line => 2,
                        spec => ok,
                        type => {type, #{line => 2, spec => atom}}
                    }}
                ],
            line => 1,
            params =>
                [
                    {param, #{
                        line => 1,
                        spec => '_',
                        type => {type, #{line => 1, spec => atom}}
                    }},
                    {param, #{
                        line => 1,
                        spec => '_',
                        type => {type, #{line => 1, spec => atom}}
                    }}
                ],
            return_type => {type, #{line => 1, spec => atom}},
            spec => 'Ignore',
            type =>
                {type, #{
                    kind => func,
                    line => 1,
                    param_types =>
                        [
                            {type, #{line => 1, spec => atom}},
                            {type, #{line => 1, spec => atom}}
                        ],
                    return_type =>
                        {type, #{line => 1, spec => atom}},
                    spec => 'func(atom, atom) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_an_anonymous_argument_and_using_it_test() ->
    RufusText =
        "func Broken(_ atom) atom {\n"
        "    _\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        form =>
            {identifier, #{line => 2, locals => #{}, spec => '_'}},
        globals =>
            #{
                'Broken' =>
                    [
                        {type, #{
                            kind => func,
                            line => 1,
                            param_types =>
                                [{type, #{line => 1, spec => atom}}],
                            return_type =>
                                {type, #{line => 1, spec => atom}},
                            spec => 'func(atom) atom'
                        }}
                    ]
            },
        locals => #{},
        stack =>
            [
                {func_exprs, #{line => 1}},
                {func, #{
                    exprs => [{identifier, #{line => 2, spec => '_'}}],
                    line => 1,
                    locals => #{},
                    params =>
                        [
                            {param, #{
                                line => 1,
                                spec => '_',
                                type => {type, #{line => 1, spec => atom}}
                            }}
                        ],
                    return_type => {type, #{line => 1, spec => atom}},
                    spec => 'Broken',
                    type =>
                        {type, #{
                            kind => func,
                            line => 1,
                            param_types =>
                                [{type, #{line => 1, spec => atom}}],
                            return_type =>
                                {type, #{line => 1, spec => atom}},
                            spec => 'func(atom) atom'
                        }}
                }}
            ]
    },
    ?assertEqual({error, unknown_identifier, Data}, rufus_expr:typecheck_and_annotate(Forms)).

%% Arity-1 functions taking an argument and returning a literal

typecheck_and_annotate_for_function_taking_an_atom_and_returning_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Ping(m atom) atom { :pong }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {atom_lit, #{
                    line => 3,
                    spec => pong,
                    type =>
                        {type, #{line => 3, spec => atom}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => m,
                    type =>
                        {type, #{line => 3, spec => atom}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => atom}},
            spec => 'Ping',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => atom}}],
                    return_type =>
                        {type, #{line => 3, spec => atom}},
                    spec => 'func(atom) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(b bool) bool { true }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {bool_lit, #{
                    line => 3,
                    spec => true,
                    type =>
                        {type, #{line => 3, spec => bool}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => b,
                    type =>
                        {type, #{line => 3, spec => bool}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => bool}},
            spec => 'MaybeEcho',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => bool}}],
                    return_type =>
                        {type, #{line => 3, spec => bool}},
                    spec => 'func(bool) bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(n float) float { 3.14159265359 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {float_lit, #{
                    line => 3,
                    spec => 3.14159265359,
                    type =>
                        {type, #{line => 3, spec => float}}
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
                            spec => float
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => float}},
            spec => 'MaybeEcho',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => float}}],
                    return_type =>
                        {type, #{line => 3, spec => float}},
                    spec => 'func(float) float'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(n int) int { 42 }\n"
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
                    spec => 42,
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
            spec => 'MaybeEcho',
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

typecheck_and_annotate_for_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(s string) string { \"Hello\" }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {string_lit, #{
                    line => 3,
                    spec => <<"Hello">>,
                    type =>
                        {type, #{line => 3, spec => string}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => s,
                    type =>
                        {type, #{
                            line => 3,
                            spec => string
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => string}},
            spec => 'MaybeEcho',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => string}}],
                    return_type =>
                        {type, #{line => 3, spec => string}},
                    spec => 'func(string) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_list_and_returning_a_list_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(n list[int]) list[int] { list[int]{42} }\n"
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
                            spec => 42,
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
            spec => 'MaybeEcho',
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

%% Arity-1 functions taking and returning an argument

typecheck_and_annotate_for_function_taking_an_atom_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(b atom) atom { b }\n"
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
                    spec => b,
                    type =>
                        {type, #{line => 3, spec => atom}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => b,
                    type =>
                        {type, #{line => 3, spec => atom}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => atom}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => atom}}],
                    return_type =>
                        {type, #{line => 3, spec => atom}},
                    spec => 'func(atom) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_bool_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(b bool) bool { b }\n"
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
                    spec => b,
                    type =>
                        {type, #{line => 3, spec => bool}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => b,
                    type =>
                        {type, #{line => 3, spec => bool}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => bool}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => bool}}],
                    return_type =>
                        {type, #{line => 3, spec => bool}},
                    spec => 'func(bool) bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_float_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n float) float { n }\n"
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
                        {type, #{
                            line => 3,
                            spec => float
                        }}
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
                            spec => float
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => float}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => float}}],
                    return_type =>
                        {type, #{line => 3, spec => float}},
                    spec => 'func(float) float'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_an_int_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n int) int { n }\n"
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
            spec => 'Echo',
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

typecheck_and_annotate_for_function_taking_a_string_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(s string) string { s }\n"
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
                    spec => s,
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
                    spec => s,
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
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_list_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n list[int]) list[int] { n }\n"
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

%% func expressions that have various return value and return type combinations

typecheck_and_annotate_function_with_literal_return_value_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Number() int { 42 }\n"
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
                    spec => 42,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'Number',
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

typecheck_and_annotate_function_with_unmatched_return_types_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Number() int { 42.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        actual => float,
        expected => int,
        expr =>
            {float_lit, #{
                line => 3,
                spec => 42.0,
                type =>
                    {type, #{line => 3, spec => float}}
            }},
        globals => #{
            'Number' => [
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
        return_type =>
            {type, #{line => 3, spec => int}}
    },
    ?assertEqual({error, unmatched_return_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

%% Functions with parameter patterns containing literal values

typecheck_and_annotate_for_function_taking_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(:ok) atom { :ok }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {atom_lit, #{
                    line => 3,
                    spec => ok,
                    type =>
                        {type, #{line => 3, spec => atom}}
                }}
            ],
            line => 3,
            params => [
                {atom_lit, #{
                    line => 3,
                    spec => ok,
                    type =>
                        {type, #{line => 3, spec => atom}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => atom}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => atom}}],
                    return_type =>
                        {type, #{line => 3, spec => atom}},
                    spec => 'func(atom) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(true) bool { true }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {bool_lit, #{
                    line => 3,
                    spec => true,
                    type =>
                        {type, #{line => 3, spec => bool}}
                }}
            ],
            line => 3,
            params => [
                {bool_lit, #{
                    line => 3,
                    spec => true,
                    type =>
                        {type, #{line => 3, spec => bool}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => bool}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => bool}}],
                    return_type =>
                        {type, #{line => 3, spec => bool}},
                    spec => 'func(bool) bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(1.0) float { 1.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {float_lit, #{
                    line => 3,
                    spec => 1.0,
                    type =>
                        {type, #{line => 3, spec => float}}
                }}
            ],
            line => 3,
            params => [
                {float_lit, #{
                    line => 3,
                    spec => 1.0,
                    type =>
                        {type, #{line => 3, spec => float}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => float}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => float}}],
                    return_type =>
                        {type, #{line => 3, spec => float}},
                    spec => 'func(float) float'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(1) int { 1 }\n"
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
                    spec => 1,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            line => 3,
            params => [
                {int_lit, #{
                    line => 3,
                    spec => 1,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 3, spec => int}},
            spec => 'Echo',
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

typecheck_and_annotate_for_function_taking_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(\"ok\") string { \"ok\" }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {string_lit, #{
                    line => 3,
                    spec => <<"ok">>,
                    type =>
                        {type, #{line => 3, spec => string}}
                }}
            ],
            line => 3,
            params => [
                {string_lit, #{
                    line => 3,
                    spec => <<"ok">>,
                    type =>
                        {type, #{line => 3, spec => string}}
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
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Anonymous functions

typecheck_and_annotate_function_taking_and_returning_a_function_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(fn func() int) func() int {\n"
        "        fn\n"
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
                    spec => fn,
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
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => fn,
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
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [],
                            return_type =>
                                {type, #{line => 3, spec => int}},
                            spec => 'func() int'
                        }}
                    ],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [],
                            return_type =>
                                {type, #{line => 3, spec => int}},
                            spec => 'func() int'
                        }},
                    spec => 'func(func() int) func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_returning_a_function_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func NumberFunc() func() int {\n"
        "        func() int { 42 }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
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
                    params => [],
                    return_type =>
                        {type, #{line => 4, spec => int}},
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
            spec => 'NumberFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [],
                            return_type =>
                                {type, #{line => 3, spec => int}},
                            spec => 'func() int'
                        }},
                    spec => 'func() func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_returning_a_function_variable_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func NumberFunc() func() int {\n"
        "        fn = func() int { 21 + 21 }\n"
        "        fn\n"
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
                            spec => fn,
                            type =>
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
                        }},
                    line => 4,
                    right =>
                        {func, #{
                            exprs => [
                                {binary_op, #{
                                    left =>
                                        {int_lit, #{
                                            line => 4,
                                            spec => 21,
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
                                            spec => 21,
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
                            params => [],
                            return_type =>
                                {type, #{line => 4, spec => int}},
                            type =>
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
                        }},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [],
                            return_type =>
                                {type, #{line => 4, spec => int}},
                            spec => 'func() int'
                        }}
                }},
                {identifier, #{
                    line => 5,
                    spec => fn,
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
            spec => 'NumberFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [],
                            return_type =>
                                {type, #{line => 3, spec => int}},
                            spec => 'func() int'
                        }},
                    spec => 'func() func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_returning_a_nested_function_test() ->
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
                            spec => f,
                            type =>
                                {type, #{
                                    kind => func,
                                    line => 4,
                                    param_types => [],
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
                                        }},
                                    spec => 'func() func() int'
                                }}
                        }},
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
                                        }},
                                    type =>
                                        {type, #{
                                            kind => func,
                                            line => 5,
                                            param_types => [],
                                            return_type =>
                                                {type, #{
                                                    line => 5,
                                                    spec => int
                                                }},
                                            spec => 'func() int'
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
                                }},
                            type =>
                                {type, #{
                                    kind => func,
                                    line => 4,
                                    param_types => [],
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
                                        }},
                                    spec => 'func() func() int'
                                }}
                        }},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [],
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
                                }},
                            spec => 'func() func() int'
                        }}
                }},
                {call, #{
                    kind => anonymous,
                    args => [],
                    line => 7,
                    spec => f,
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
            spec => 'NumberFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [],
                            return_type =>
                                {type, #{line => 3, spec => int}},
                            spec => 'func() int'
                        }},
                    spec => 'func() func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_anonymous_function_taking_an_atom_and_returning_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(atom) atom {\n"
        "        func(value atom) atom { value }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {identifier, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => atom
                                }}
                        }}
                    ],
                    line => 4,
                    params => [
                        {param, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => atom
                                }}
                        }}
                    ],
                    return_type =>
                        {type, #{line => 4, spec => atom}},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [
                                {type, #{
                                    line => 4,
                                    spec => atom
                                }}
                            ],
                            return_type =>
                                {type, #{
                                    line => 4,
                                    spec => atom
                                }},
                            spec => 'func(atom) atom'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => atom}}],
                    return_type =>
                        {type, #{line => 3, spec => atom}},
                    spec => 'func(atom) atom'
                }},
            spec => 'EchoFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [
                                {type, #{
                                    line => 3,
                                    spec => atom
                                }}
                            ],
                            return_type =>
                                {type, #{line => 3, spec => atom}},
                            spec => 'func(atom) atom'
                        }},
                    spec => 'func() func(atom) atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_anonymous_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(bool) bool {\n"
        "        func(value bool) bool { value }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {identifier, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => bool
                                }}
                        }}
                    ],
                    line => 4,
                    params => [
                        {param, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => bool
                                }}
                        }}
                    ],
                    return_type =>
                        {type, #{line => 4, spec => bool}},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [
                                {type, #{
                                    line => 4,
                                    spec => bool
                                }}
                            ],
                            return_type =>
                                {type, #{
                                    line => 4,
                                    spec => bool
                                }},
                            spec => 'func(bool) bool'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => bool}}],
                    return_type =>
                        {type, #{line => 3, spec => bool}},
                    spec => 'func(bool) bool'
                }},
            spec => 'EchoFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [
                                {type, #{
                                    line => 3,
                                    spec => bool
                                }}
                            ],
                            return_type =>
                                {type, #{line => 3, spec => bool}},
                            spec => 'func(bool) bool'
                        }},
                    spec => 'func() func(bool) bool'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_anonymous_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(float) float {\n"
        "        func(value float) float { value }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {identifier, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => float
                                }}
                        }}
                    ],
                    line => 4,
                    params => [
                        {param, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => float
                                }}
                        }}
                    ],
                    return_type =>
                        {type, #{line => 4, spec => float}},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [
                                {type, #{
                                    line => 4,
                                    spec => float
                                }}
                            ],
                            return_type =>
                                {type, #{
                                    line => 4,
                                    spec => float
                                }},
                            spec => 'func(float) float'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => float}}],
                    return_type =>
                        {type, #{line => 3, spec => float}},
                    spec => 'func(float) float'
                }},
            spec => 'EchoFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [
                                {type, #{
                                    line => 3,
                                    spec => float
                                }}
                            ],
                            return_type =>
                                {type, #{line => 3, spec => float}},
                            spec => 'func(float) float'
                        }},
                    spec => 'func() func(float) float'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_anonymous_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(int) int {\n"
        "        func(value int) int { value }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {identifier, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 4,
                    params => [
                        {param, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => int
                                }}
                        }}
                    ],
                    return_type =>
                        {type, #{line => 4, spec => int}},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [
                                {type, #{
                                    line => 4,
                                    spec => int
                                }}
                            ],
                            return_type =>
                                {type, #{
                                    line => 4,
                                    spec => int
                                }},
                            spec => 'func(int) int'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => int}}],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func(int) int'
                }},
            spec => 'EchoFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                            ],
                            return_type =>
                                {type, #{line => 3, spec => int}},
                            spec => 'func(int) int'
                        }},
                    spec => 'func() func(int) int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_anonymous_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(string) string {\n"
        "        func(value string) string { value }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {identifier, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => string
                                }}
                        }}
                    ],
                    line => 4,
                    params => [
                        {param, #{
                            line => 4,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => string
                                }}
                        }}
                    ],
                    return_type =>
                        {type, #{line => 4, spec => string}},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [
                                {type, #{
                                    line => 4,
                                    spec => string
                                }}
                            ],
                            return_type =>
                                {type, #{
                                    line => 4,
                                    spec => string
                                }},
                            spec => 'func(string) string'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => string}}],
                    return_type =>
                        {type, #{line => 3, spec => string}},
                    spec => 'func(string) string'
                }},
            spec => 'EchoFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
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
                                {type, #{line => 3, spec => string}},
                            spec => 'func(string) string'
                        }},
                    spec => 'func() func(string) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_anonymous_function_taking_a_cons_expression_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoNumberListFunc() func(list[int]) list[int] {\n"
        "        func(list[int]{head|tail}) list[int] { list[int]{head|tail} }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {cons, #{
                            head =>
                                {identifier, #{
                                    line => 4,
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
                                    spec => tail,
                                    type =>
                                        {type, #{
                                            element_type =>
                                                {type, #{
                                                    line => 4,
                                                    spec => int
                                                }},
                                            kind => list,
                                            line => 4,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }}
                    ],
                    line => 4,
                    params => [
                        {cons, #{
                            head =>
                                {identifier, #{
                                    line => 4,
                                    locals => #{},
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
                                        ]
                                    },
                                    spec => tail,
                                    type =>
                                        {type, #{
                                            element_type =>
                                                {type, #{
                                                    line => 4,
                                                    spec => int
                                                }},
                                            kind => list,
                                            line => 4,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }}
                    ],
                    return_type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 4, spec => int}},
                            kind => list,
                            line => 4,
                            spec => 'list[int]'
                        }},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                            ],
                            return_type =>
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }},
                            spec => 'func(list[int]) list[int]'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }}
                    ],
                    return_type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec => 'func(list[int]) list[int]'
                }},
            spec => 'EchoNumberListFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [
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
                            ],
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
                            spec => 'func(list[int]) list[int]'
                        }},
                    spec => 'func() func(list[int]) list[int]'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_anonymous_function_taking_a_list_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoNumberListFunc() func(list[int]) list[int] {\n"
        "        func(numbers = list[int]{1, 2, 3}) list[int] { numbers }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {identifier, #{
                            line => 4,
                            spec => numbers,
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }}
                    ],
                    line => 4,
                    params => [
                        {match_op, #{
                            left =>
                                {identifier, #{
                                    line => 4,
                                    locals => #{},
                                    spec => numbers,
                                    type =>
                                        {type, #{
                                            element_type =>
                                                {type, #{
                                                    line => 4,
                                                    spec => int
                                                }},
                                            kind => list,
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
                                            element_type =>
                                                {type, #{
                                                    line => 4,
                                                    spec => int
                                                }},
                                            kind => list,
                                            line => 4,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                        }}
                    ],
                    return_type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 4, spec => int}},
                            kind => list,
                            line => 4,
                            spec => 'list[int]'
                        }},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }}
                            ],
                            return_type =>
                                {type, #{
                                    element_type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }},
                                    kind => list,
                                    line => 4,
                                    spec => 'list[int]'
                                }},
                            spec => 'func(list[int]) list[int]'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }}
                    ],
                    return_type =>
                        {type, #{
                            element_type =>
                                {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec => 'func(list[int]) list[int]'
                }},
            spec => 'EchoNumberListFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [
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
                            ],
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
                            spec => 'func(list[int]) list[int]'
                        }},
                    spec => 'func() func(list[int]) list[int]'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_anonymous_function_taking_a_match_param_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFortyTwoFunc() func(int) int {\n"
        "        func(42 = value int) int {\n"
        "            value\n"
        "        }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {identifier, #{
                            line => 5,
                            spec => value,
                            type =>
                                {type, #{
                                    line => 4,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 4,
                    params => [
                        {match_op, #{
                            left =>
                                {int_lit, #{
                                    line => 4,
                                    spec => 42,
                                    type =>
                                        {type, #{
                                            line => 4,
                                            spec => int
                                        }}
                                }},
                            line => 4,
                            right =>
                                {param, #{
                                    line => 4,
                                    spec => value,
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
                    return_type =>
                        {type, #{line => 4, spec => int}},
                    type =>
                        {type, #{
                            kind => func,
                            line => 4,
                            param_types => [
                                {type, #{
                                    line => 4,
                                    spec => int
                                }}
                            ],
                            return_type =>
                                {type, #{line => 4, spec => int}},
                            spec => 'func(int) int'
                        }}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => int}}],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func(int) int'
                }},
            spec => 'EchoFortyTwoFunc',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                            ],
                            return_type =>
                                {type, #{line => 3, spec => int}},
                            spec => 'func(int) int'
                        }},
                    spec => 'func() func(int) int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_closure_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Memoize(num int) func() int {\n"
        "        func() int { num }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{
            exprs => [
                {func, #{
                    exprs => [
                        {identifier, #{
                            line => 4,
                            spec => num,
                            type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 4,
                    params => [],
                    return_type =>
                        {type, #{line => 4, spec => int}},
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
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => num,
                    type =>
                        {type, #{line => 3, spec => int}}
                }}
            ],
            return_type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, spec => int}},
                    spec => 'func() int'
                }},
            spec => 'Memoize',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, spec => int}}],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [],
                            return_type =>
                                {type, #{line => 3, spec => int}},
                            spec => 'func() int'
                        }},
                    spec => 'func(int) func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_anonymous_function_with_unmatched_return_types_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func() int {\n"
        "        func() int { 42.0 }\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{
        actual => float,
        expected => int,
        expr =>
            {float_lit, #{
                line => 4,
                spec => 42.0,
                type =>
                    {type, #{line => 4, spec => float}}
            }},
        globals => #{
            'EchoFunc' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [],
                            return_type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }},
                            spec => 'func() int'
                        }},
                    spec => 'func() func() int'
                }}
            ]
        },
        return_type =>
            {type, #{line => 4, spec => int}}
    },
    ?assertEqual({error, unmatched_return_type, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_does_not_allow_locals_to_escape_anonymous_function_scope_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func() int {\n"
        "        fn = func() int {\n"
        "            num = 42\n"
        "            num\n"
        "        }\n"
        "        escape = num\n"
        "        fn\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_expr:typecheck_and_annotate(Forms),
    Data = #{
        form =>
            {identifier, #{
                line => 8,
                locals => #{
                    fn => [
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
                    ]
                },
                spec => num
            }},
        globals => #{
            'EchoFunc' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [],
                            return_type =>
                                {type, #{
                                    line => 3,
                                    spec => int
                                }},
                            spec => 'func() int'
                        }},
                    spec => 'func() func() int'
                }}
            ]
        },
        locals => #{
            fn => [
                {type, #{
                    kind => func,
                    line => 4,
                    param_types => [],
                    return_type =>
                        {type, #{line => 4, spec => int}},
                    spec => 'func() int'
                }}
            ]
        },
        stack => [
            {match_op_right, #{line => 8}},
            {match_op, #{
                left => {identifier, #{line => 8, spec => escape}},
                line => 8,
                right => {identifier, #{line => 8, spec => num}}
            }},
            {func_exprs, #{line => 3}},
            {func, #{
                exprs => [
                    {match_op, #{
                        left => {identifier, #{line => 4, spec => fn}},
                        line => 4,
                        right =>
                            {func, #{
                                exprs => [
                                    {match_op, #{
                                        left =>
                                            {identifier, #{line => 5, spec => num}},
                                        line => 5,
                                        right =>
                                            {int_lit, #{
                                                line => 5,
                                                spec => 42,
                                                type =>
                                                    {type, #{
                                                        line => 5,
                                                        spec => int
                                                    }}
                                            }}
                                    }},
                                    {identifier, #{line => 6, spec => num}}
                                ],
                                line => 4,
                                params => [],
                                return_type =>
                                    {type, #{
                                        line => 4,
                                        spec => int
                                    }}
                            }}
                    }},
                    {match_op, #{
                        left => {identifier, #{line => 8, spec => escape}},
                        line => 8,
                        right => {identifier, #{line => 8, spec => num}}
                    }},
                    {identifier, #{line => 9, spec => fn}}
                ],
                line => 3,
                locals => #{},
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
                spec => 'EchoFunc',
                type =>
                    {type, #{
                        kind => func,
                        line => 3,
                        param_types => [],
                        return_type =>
                            {type, #{
                                kind => func,
                                line => 3,
                                param_types => [],
                                return_type =>
                                    {type, #{
                                        line => 3,
                                        spec => int
                                    }},
                                spec => 'func() int'
                            }},
                        spec => 'func() func() int'
                    }}
            }}
        ]
    },
    ?assertEqual({error, unknown_identifier, Data}, Result).

%% Functions with multiple parameters

typecheck_and_annotate_function_with_mixed_params_and_patterns_in_parameter_list_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Map(acc list[int], list[int]{head|tail}, fn func(int) int) list[int] {\n"
        "        item = fn(head)\n"
        "        Map(list[int]{item|acc}, tail, fn)\n"
        "    }\n"
        "    func Map(acc list[int], list[int]{}, fn func(int) int) list[int] {\n"
        "        acc\n"
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
                            locals => #{
                                acc => [
                                    {type, #{
                                        element_type =>
                                            {type, #{line => 3, spec => int}},
                                        kind => list,
                                        line => 3,
                                        spec => 'list[int]'
                                    }}
                                ],
                                fn => [
                                    {type, #{
                                        kind => func,
                                        line => 3,
                                        param_types => [{type, #{line => 3, spec => int}}],
                                        return_type =>
                                            {type, #{line => 3, spec => int}},
                                        spec => 'func(int) int'
                                    }}
                                ],
                                head => [{type, #{line => 3, spec => int}}],
                                tail => [
                                    {type, #{
                                        element_type =>
                                            {type, #{line => 3, spec => int}},
                                        kind => list,
                                        line => 3,
                                        spec => 'list[int]'
                                    }}
                                ]
                            },
                            spec => item,
                            type => {type, #{line => 3, spec => int}}
                        }},
                    line => 4,
                    right =>
                        {call, #{
                            args => [
                                {identifier, #{
                                    line => 4,
                                    spec => head,
                                    type => {type, #{line => 3, spec => int}}
                                }}
                            ],
                            kind => anonymous,
                            line => 4,
                            spec => fn,
                            type => {type, #{line => 3, spec => int}}
                        }},
                    type => {type, #{line => 3, spec => int}}
                }},
                {call, #{
                    args => [
                        {cons, #{
                            head =>
                                {identifier, #{
                                    line => 5,
                                    spec => item,
                                    type => {type, #{line => 3, spec => int}}
                                }},
                            line => 5,
                            tail =>
                                {identifier, #{
                                    line => 5,
                                    spec => acc,
                                    type =>
                                        {type, #{
                                            element_type =>
                                                {type, #{line => 3, spec => int}},
                                            kind => list,
                                            line => 3,
                                            spec => 'list[int]'
                                        }}
                                }},
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{line => 5, spec => int}},
                                    kind => list,
                                    line => 5,
                                    spec => 'list[int]'
                                }}
                        }},
                        {identifier, #{
                            line => 5,
                            spec => tail,
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{line => 3, spec => int}},
                                    kind => list,
                                    line => 3,
                                    spec => 'list[int]'
                                }}
                        }},
                        {identifier, #{
                            line => 5,
                            spec => fn,
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
                    line => 5,
                    spec => 'Map',
                    type =>
                        {type, #{
                            element_type => {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => acc,
                    type =>
                        {type, #{
                            element_type => {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }}
                }},
                {cons, #{
                    head =>
                        {identifier, #{
                            line => 3,
                            locals => #{
                                acc => [
                                    {type, #{
                                        element_type =>
                                            {type, #{line => 3, spec => int}},
                                        kind => list,
                                        line => 3,
                                        spec => 'list[int]'
                                    }}
                                ]
                            },
                            spec => head,
                            type => {type, #{line => 3, spec => int}}
                        }},
                    line => 3,
                    tail =>
                        {identifier, #{
                            line => 3,
                            locals => #{
                                acc => [
                                    {type, #{
                                        element_type =>
                                            {type, #{line => 3, spec => int}},
                                        kind => list,
                                        line => 3,
                                        spec => 'list[int]'
                                    }}
                                ],
                                head => [{type, #{line => 3, spec => int}}]
                            },
                            spec => tail,
                            type =>
                                {type, #{
                                    element_type =>
                                        {type, #{line => 3, spec => int}},
                                    kind => list,
                                    line => 3,
                                    spec => 'list[int]'
                                }}
                        }},
                    type =>
                        {type, #{
                            element_type => {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }}
                }},
                {param, #{
                    line => 3,
                    spec => fn,
                    type =>
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [{type, #{line => 3, spec => int}}],
                            return_type => {type, #{line => 3, spec => int}},
                            spec => 'func(int) int'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    element_type => {type, #{line => 3, spec => int}},
                    kind => list,
                    line => 3,
                    spec => 'list[int]'
                }},
            spec => 'Map',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            element_type => {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }},
                        {type, #{
                            element_type => {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }},
                        {type, #{
                            kind => func,
                            line => 3,
                            param_types => [{type, #{line => 3, spec => int}}],
                            return_type => {type, #{line => 3, spec => int}},
                            spec => 'func(int) int'
                        }}
                    ],
                    return_type =>
                        {type, #{
                            element_type => {type, #{line => 3, spec => int}},
                            kind => list,
                            line => 3,
                            spec => 'list[int]'
                        }},
                    spec =>
                        'func(list[int], list[int], func(int) int) list[int]'
                }}
        }},
        {func, #{
            exprs => [
                {identifier, #{
                    line => 8,
                    spec => acc,
                    type =>
                        {type, #{
                            element_type => {type, #{line => 7, spec => int}},
                            kind => list,
                            line => 7,
                            spec => 'list[int]'
                        }}
                }}
            ],
            line => 7,
            params => [
                {param, #{
                    line => 7,
                    spec => acc,
                    type =>
                        {type, #{
                            element_type => {type, #{line => 7, spec => int}},
                            kind => list,
                            line => 7,
                            spec => 'list[int]'
                        }}
                }},
                {list_lit, #{
                    elements => [],
                    line => 7,
                    type =>
                        {type, #{
                            element_type => {type, #{line => 7, spec => int}},
                            kind => list,
                            line => 7,
                            spec => 'list[int]'
                        }}
                }},
                {param, #{
                    line => 7,
                    spec => fn,
                    type =>
                        {type, #{
                            kind => func,
                            line => 7,
                            param_types => [{type, #{line => 7, spec => int}}],
                            return_type => {type, #{line => 7, spec => int}},
                            spec => 'func(int) int'
                        }}
                }}
            ],
            return_type =>
                {type, #{
                    element_type => {type, #{line => 7, spec => int}},
                    kind => list,
                    line => 7,
                    spec => 'list[int]'
                }},
            spec => 'Map',
            type =>
                {type, #{
                    kind => func,
                    line => 7,
                    param_types => [
                        {type, #{
                            element_type => {type, #{line => 7, spec => int}},
                            kind => list,
                            line => 7,
                            spec => 'list[int]'
                        }},
                        {type, #{
                            element_type => {type, #{line => 7, spec => int}},
                            kind => list,
                            line => 7,
                            spec => 'list[int]'
                        }},
                        {type, #{
                            kind => func,
                            line => 7,
                            param_types => [{type, #{line => 7, spec => int}}],
                            return_type => {type, #{line => 7, spec => int}},
                            spec => 'func(int) int'
                        }}
                    ],
                    return_type =>
                        {type, #{
                            element_type => {type, #{line => 7, spec => int}},
                            kind => list,
                            line => 7,
                            spec => 'list[int]'
                        }},
                    spec =>
                        'func(list[int], list[int], func(int) int) list[int]'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).
