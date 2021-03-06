-module(rufus_erlang_func_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a literal value for scalar types

forms_for_function_returning_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Ping() atom { :pong }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    AtomExpr = {atom, 3, pong},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Ping', 0}]},
        {function, 3, 'Ping', 0, [{clause, 3, [], [], [AtomExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func False() bool { false }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'False', 0}]},
        {function, 3, 'False', 0, [{clause, 3, [], [], [{atom, 3, false}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Pi() float { 3.14159265359 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Pi', 0}]},
        {function, 3, 'Pi', 0, [{clause, 3, [], [], [{float, 3, 3.14159265359}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Number() int { 42 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Number', 0}]},
        {function, 3, 'Number', 0, [{clause, 3, [], [], [{integer, 3, 42}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Greeting() string { \"Hello\" }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    StringExpr = {bin, 3, [{bin_element, 3, {string, 3, "Hello"}, default, default}]},
    BoxedStringExpr = {tuple, 3, [{atom, 3, string}, StringExpr]},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Greeting', 0}]},
        {function, 3, 'Greeting', 0, [{clause, 3, [], [], [BoxedStringExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-0 functions with multiple function expressions

forms_for_function_with_multiple_expressions_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Multiple() atom {\n"
        "        42;\n"
        "        :fortytwo;\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Exprs = [{integer, 4, 42}, {atom, 5, fortytwo}],
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Multiple', 0}]},
        {function, 3, 'Multiple', 0, [{clause, 3, [], [], Exprs}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_multiple_expressions_with_blank_lines_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Multiple() atom {\n"
        "        42;\n"
        "\n"
        "        :fortytwo;\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Exprs = [{integer, 4, 42}, {atom, 6, fortytwo}],
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Multiple', 0}]},
        {function, 3, 'Multiple', 0, [{clause, 3, [], [], Exprs}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-1 functions taking an unused argument

forms_for_function_taking_an_unused_atom_and_returning_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Ping(m atom) atom { :pong }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Ping', 1}]},
        {function, 3, 'Ping', 1, [
            {clause, 3, [{var, 3, m}],
                [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_atom}}, [{var, 3, m}]}]], [
                    {atom, 3, pong}
                ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_bool_and_returning_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(b bool) bool { true }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'MaybeEcho', 1}]},
        {function, 3, 'MaybeEcho', 1, [
            {clause, 3, [{var, 3, b}],
                [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_boolean}}, [{var, 3, b}]}]],
                [{atom, 3, true}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_float_and_returning_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(n float) float { 3.14159265359 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'MaybeEcho', 1}]},
        {function, 3, 'MaybeEcho', 1, [
            {clause, 3, [{var, 3, n}],
                [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_float}}, [{var, 3, n}]}]], [
                    {float, 3, 3.14159265359}
                ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_int_and_returning_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(n int) int { 42 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'MaybeEcho', 1}]},
        {function, 3, 'MaybeEcho', 1, [
            {clause, 3, [{var, 3, n}],
                [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [{var, 3, n}]}]],
                [{integer, 3, 42}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_string_and_returning_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(s string) string { \"Hello\" }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    StringExpr = {bin_element, 3, {string, 3, "Hello"}, default, default},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'MaybeEcho', 1}]},
        {function, 3, 'MaybeEcho', 1, [
            {clause, 3, [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}], [], [
                {tuple, 3, [{atom, 3, string}, {bin, 3, [StringExpr]}]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-1 functions taking and using an argument

forms_for_function_taking_an_atom_and_returning_an_atom_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(m atom) atom { m }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [
            {clause, 3, [{var, 3, m}],
                [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_atom}}, [{var, 3, m}]}]], [
                    {var, 3, m}
                ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_bool_and_returning_a_bool_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(b bool) bool { b }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [
            {clause, 3, [{var, 3, b}],
                [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_boolean}}, [{var, 3, b}]}]],
                [{var, 3, b}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_float_and_returning_a_float_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n float) float { n }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [
            {clause, 3, [{var, 3, n}],
                [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_float}}, [{var, 3, n}]}]], [
                    {var, 3, n}
                ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_int_and_returning_an_int_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n int) int { n }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [
            {clause, 3, [{var, 3, n}],
                [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [{var, 3, n}]}]],
                [{var, 3, n}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_string_and_returning_a_string_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(s string) string { s }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [
            {clause, 3, [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}], [], [
                {tuple, 3, [{atom, 3, string}, {var, 3, s}]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Function exports

forms_for_private_functions_with_leading_lowercase_character_are_not_exported_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func echo(s string) string { s }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {function, 3, 'echo', 1, [
            {clause, 3, [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}], [], [
                {tuple, 3, [{atom, 3, string}, {var, 3, s}]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Functions with parameter patterns containing literal values

forms_for_function_taking_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(:ok) atom { :ok }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [{clause, 3, [{atom, 3, ok}], [], [{atom, 3, ok}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(true) bool { true }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [{clause, 3, [{atom, 3, true}], [], [{atom, 3, true}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(1.0) float { 1.0 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [{clause, 3, [{float, 3, 1.0}], [], [{float, 3, 1.0}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(1) int { 1 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [{clause, 3, [{integer, 3, 1}], [], [{integer, 3, 1}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(\"ok\") string { \"ok\" }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [
            {clause, 3,
                [
                    {tuple, 3, [
                        {atom, 3, string},
                        {bin, 3, [{bin_element, 3, {string, 3, "ok"}, default, default}]}
                    ]}
                ],
                [], [
                    {tuple, 3, [
                        {atom, 3, string},
                        {bin, 3, [{bin_element, 3, {string, 3, "ok"}, default, default}]}
                    ]}
                ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Anonymous functions

forms_for_function_taking_and_returning_a_function_test() ->
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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [{clause, 3, [{var, 3, fn}], [], [{var, 4, fn}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_function_test() ->
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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'NumberFunc', 0}]},
        {function, 3, 'NumberFunc', 0, [
            {clause, 3, [], [], [{'fun', 4, {clauses, [{clause, 4, [], [], [{integer, 4, 42}]}]}}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_function_variable_test() ->
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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'NumberFunc', 0}]},
        {function, 3, 'NumberFunc', 0, [
            {clause, 3, [], [], [
                {match, 4, {var, 4, fn},
                    {'fun', 4,
                        {clauses, [
                            {clause, 4, [], [], [{op, 4, '+', {integer, 4, 21}, {integer, 4, 21}}]}
                        ]}}},
                {var, 5, fn}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_nested_function_test() ->
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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'NumberFunc', 0}]},
        {function, 3, 'NumberFunc', 0, [
            {clause, 3, [], [], [
                {match, 4, {var, 4, f},
                    {'fun', 4,
                        {clauses, [
                            {clause, 4, [], [], [
                                {'fun', 5, {clauses, [{clause, 5, [], [], [{integer, 5, 42}]}]}}
                            ]}
                        ]}}},
                {call, 7, {var, 7, f}, []}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_anonymous_function_taking_an_atom_and_returning_an_atom_literal_test() ->
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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'EchoFunc', 0}]},
        {function, 3, 'EchoFunc', 0, [
            {clause, 3, [], [], [
                {'fun', 4,
                    {clauses, [
                        {clause, 4, [{var, 4, value}],
                            [
                                [
                                    {call, 4, {remote, 4, {atom, 4, erlang}, {atom, 4, is_atom}}, [
                                        {var, 4, value}
                                    ]}
                                ]
                            ],
                            [{var, 4, value}]}
                    ]}}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'EchoFunc', 0}]},
        {function, 3, 'EchoFunc', 0, [
            {clause, 3, [], [], [
                {'fun', 4,
                    {clauses, [
                        {clause, 4, [{var, 4, value}],
                            [
                                [
                                    {call, 4, {remote, 4, {atom, 4, erlang}, {atom, 4, is_boolean}},
                                        [{var, 4, value}]}
                                ]
                            ],
                            [{var, 4, value}]}
                    ]}}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'EchoFunc', 0}]},
        {function, 3, 'EchoFunc', 0, [
            {clause, 3, [], [], [
                {'fun', 4,
                    {clauses, [
                        {clause, 4, [{var, 4, value}],
                            [
                                [
                                    {call, 4, {remote, 4, {atom, 4, erlang}, {atom, 4, is_float}}, [
                                        {var, 4, value}
                                    ]}
                                ]
                            ],
                            [{var, 4, value}]}
                    ]}}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'EchoFunc', 0}]},
        {function, 3, 'EchoFunc', 0, [
            {clause, 3, [], [], [
                {'fun', 4,
                    {clauses, [
                        {clause, 4, [{var, 4, value}],
                            [
                                [
                                    {call, 4, {remote, 4, {atom, 4, erlang}, {atom, 4, is_integer}},
                                        [{var, 4, value}]}
                                ]
                            ],
                            [{var, 4, value}]}
                    ]}}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'EchoFunc', 0}]},
        {function, 3, 'EchoFunc', 0, [
            {clause, 3, [], [], [
                {'fun', 4,
                    {clauses, [
                        {clause, 4, [{tuple, 4, [{atom, 4, string}, {var, 4, value}]}], [], [
                            {tuple, 4, [{atom, 4, string}, {var, 4, value}]}
                        ]}
                    ]}}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'EchoNumberListFunc', 0}]},
        {function, 3, 'EchoNumberListFunc', 0, [
            {clause, 3, [], [], [
                {'fun', 4,
                    {clauses, [
                        {clause, 4, [{cons, 4, {var, 4, head}, {var, 4, tail}}], [], [
                            {cons, 4, {var, 4, head}, {var, 4, tail}}
                        ]}
                    ]}}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'EchoNumberListFunc', 0}]},
        {function, 3, 'EchoNumberListFunc', 0, [
            {clause, 3, [], [], [
                {'fun', 4,
                    {clauses, [
                        {clause, 4,
                            [
                                {match, 4, {var, 4, numbers},
                                    {cons, 4, {integer, 4, 1},
                                        {cons, 4, {integer, 4, 2},
                                            {cons, 4, {integer, 4, 3}, {nil, 4}}}}}
                            ],
                            [], [{var, 4, numbers}]}
                    ]}}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'EchoFortyTwoFunc', 0}]},
        {function, 3, 'EchoFortyTwoFunc', 0, [
            {clause, 3, [], [], [
                {'fun', 4,
                    {clauses, [
                        {clause, 4, [{match, 4, {integer, 4, 42}, {var, 4, value}}], [], [
                            {var, 5, value}
                        ]}
                    ]}}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Memoize', 1}]},
        {function, 3, 'Memoize', 1, [
            {clause, 3, [{var, 3, num}],
                [
                    [
                        {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [
                            {var, 3, num}
                        ]}
                    ]
                ],
                [{'fun', 4, {clauses, [{clause, 4, [], [], [{var, 4, num}]}]}}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).
