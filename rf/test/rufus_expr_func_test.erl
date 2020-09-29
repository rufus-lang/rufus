-module(rufus_expr_func_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

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
                {func, #{
                    exprs => [{identifier, #{line => 7, spec => a}}],
                    line => 7,
                    params => [],
                    return_type =>
                        {type, #{
                            line => 7,
                            source => rufus_text,
                            spec => string
                        }},
                    spec => 'Broken'
                }}
            ],
            'Echo' => [
                {func, #{
                    exprs => [
                        {match, #{
                            left => {identifier, #{line => 4, spec => a}},
                            line => 4,
                            right =>
                                {identifier, #{line => 4, spec => n}}
                        }},
                        {identifier, #{line => 5, spec => a}}
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
                                    spec => string
                                }}
                        }}
                    ],
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
        locals => #{},
        stack => [
            {func_exprs, #{line => 7}},
            {func, #{
                exprs => [{identifier, #{line => 7, spec => a}}],
                line => 7,
                params => [],
                return_type =>
                    {type, #{line => 7, source => rufus_text, spec => string}},
                spec => 'Broken'
            }}
        ]
    },
    ?assertEqual({error, unknown_identifier, Data}, Result).

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
                        {type, #{line => 3, source => inferred, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => int}},
            spec => 'Number',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func() int'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

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
                        {type, #{line => 3, source => inferred, spec => atom}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => m,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => atom}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => atom}},
            spec => 'Ping',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => atom}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => atom}},
                    source => rufus_text,
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
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => b,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'MaybeEcho',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => bool}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
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
                        {type, #{line => 3, source => inferred, spec => float}}
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
                            spec => float
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => float}},
            spec => 'MaybeEcho',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => float}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => float}},
                    source => rufus_text,
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
                        {type, #{line => 3, source => inferred, spec => int}}
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
            spec => 'MaybeEcho',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => int}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
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
                        {type, #{line => 3, source => inferred, spec => string}}
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
                            source => rufus_text,
                            spec => string
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => string}},
            spec => 'MaybeEcho',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => string}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => string}},
                    source => rufus_text,
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
                                    source => inferred,
                                    spec => int
                                }}
                        }}
                    ],
                    line => 3,
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
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => n,
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
            spec => 'MaybeEcho',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, source => rufus_text, spec => int}},
                            line => 3,
                            source => rufus_text,
                            spec => 'list[int]'
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
                    source => rufus_text,
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
                        {type, #{line => 3, source => rufus_text, spec => atom}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => b,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => atom}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => atom}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => atom}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => atom}},
                    source => rufus_text,
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
                        {type, #{line => 3, source => rufus_text, spec => bool}}
                }}
            ],
            line => 3,
            params => [
                {param, #{
                    line => 3,
                    spec => b,
                    type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => bool}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
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
                            source => rufus_text,
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
                            source => rufus_text,
                            spec => float
                        }}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => float}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => float}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => float}},
                    source => rufus_text,
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
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => int}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
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
                            source => rufus_text,
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
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => rufus_text, spec => string}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => string}},
                    source => rufus_text,
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
                                {type, #{line => 3, source => rufus_text, spec => int}},
                            line => 3,
                            source => rufus_text,
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
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            kind => list,
                            element_type =>
                                {type, #{line => 3, source => rufus_text, spec => int}},
                            line => 3,
                            source => rufus_text,
                            spec => 'list[int]'
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
                    source => rufus_text,
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
                        {type, #{line => 3, source => inferred, spec => int}}
                }}
            ],
            line => 3,
            params => [],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => int}},
            spec => 'Number',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
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
    Expected =
        {error, unmatched_return_type, #{
            expr =>
                {float_lit, #{
                    line => 3,
                    spec => 42.0,
                    type =>
                        {type, #{
                            line => 3,
                            source => inferred,
                            spec => float
                        }}
                }},
            return_type =>
                {type, #{
                    line => 3,
                    source => rufus_text,
                    spec => int
                }}
        }},
    ?assertEqual(Expected, rufus_expr:typecheck_and_annotate(Forms)).

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
                        {type, #{line => 3, source => inferred, spec => atom}}
                }}
            ],
            line => 3,
            params => [
                {atom_lit, #{
                    line => 3,
                    spec => ok,
                    type =>
                        {type, #{line => 3, source => inferred, spec => atom}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => atom}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => inferred, spec => atom}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => atom}},
                    source => rufus_text,
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
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            line => 3,
            params => [
                {bool_lit, #{
                    line => 3,
                    spec => true,
                    type =>
                        {type, #{line => 3, source => inferred, spec => bool}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => bool}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => inferred, spec => bool}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => bool}},
                    source => rufus_text,
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
                        {type, #{line => 3, source => inferred, spec => float}}
                }}
            ],
            line => 3,
            params => [
                {float_lit, #{
                    line => 3,
                    spec => 1.0,
                    type =>
                        {type, #{line => 3, source => inferred, spec => float}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => float}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => inferred, spec => float}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => float}},
                    source => rufus_text,
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
                        {type, #{line => 3, source => inferred, spec => int}}
                }}
            ],
            line => 3,
            params => [
                {int_lit, #{
                    line => 3,
                    spec => 1,
                    type =>
                        {type, #{line => 3, source => inferred, spec => int}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => int}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => inferred, spec => int}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
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
                        {type, #{line => 3, source => inferred, spec => string}}
                }}
            ],
            line => 3,
            params => [
                {string_lit, #{
                    line => 3,
                    spec => <<"ok">>,
                    type =>
                        {type, #{line => 3, source => inferred, spec => string}}
                }}
            ],
            return_type =>
                {type, #{line => 3, source => rufus_text, spec => string}},
            spec => 'Echo',
            type =>
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [{type, #{line => 3, source => inferred, spec => string}}],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => string}},
                    source => rufus_text,
                    spec => 'func(string) string'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Anonymous functions

%% typecheck_and_annotate_anonymous_function_test() ->
%%     RufusText =
%%         "\n"
%%         "    module example\n"
%%         "    func NumberFunc() func() int {\n"
%%         "        func() int { 42 }\n"
%%         "    }\n"
%%         "    ",
%%     {ok, Tokens} = rufus_tokenize:string(RufusText),
%%     {ok, Forms} = rufus_parse:parse(Tokens),
%%     {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
%%     Expected = [
%%         {module, #{line => 2, spec => example}},
%%         {func, #{
%%             params => [],
%%             exprs => [
%%                 {int_lit, #{
%%                     line => 3,
%%                     spec => 42,
%%                     type => {type, #{line => 3, spec => int, source => inferred}}
%%                 }}
%%             ],
%%             line => 3,
%%             return_type => {type, #{line => 3, spec => int, source => rufus_text}},
%%             spec => 'Number'
%%         }}
%%     ],
%%     ?assertEqual(Expected, AnnotatedForms).
