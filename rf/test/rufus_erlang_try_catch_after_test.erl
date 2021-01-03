-module(rufus_erlang_try_catch_after_test).

-include_lib("eunit/include/eunit.hrl").

forms_for_function_with_bare_catch_block_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "        :error\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Maybe', 0}]},
        {function, 2, 'Maybe', 0, [
            {clause, 2, [], [], [
                {'try', 3, [{atom, 4, ok}], [],
                    [
                        {clause, 5, [{tuple, 5, [{atom, 5, throw}, {var, 5, '_'}, {var, 5, '_'}]}],
                            [], [{atom, 6, error}]}
                    ],
                    []}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_try_and_catch_blocks_both_returning_an_atom_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        :error\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Maybe', 0}]},
        {function, 2, 'Maybe', 0, [
            {clause, 2, [], [], [
                {'try', 3, [{atom, 4, ok}], [],
                    [
                        {clause, 5,
                            [{tuple, 5, [{atom, 5, throw}, {atom, 5, error}, {var, 5, '_'}]}], [], [
                                {atom, 6, error}
                            ]}
                    ],
                    []}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_try_and_catch_blocks_both_returning_a_bool_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() bool {\n"
        "    try {\n"
        "        true\n"
        "    } catch :error {\n"
        "        false\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Maybe', 0}]},
        {function, 2, 'Maybe', 0, [
            {clause, 2, [], [], [
                {'try', 3, [{atom, 4, true}], [],
                    [
                        {clause, 5,
                            [{tuple, 5, [{atom, 5, throw}, {atom, 5, error}, {var, 5, '_'}]}], [], [
                                {atom, 6, false}
                            ]}
                    ],
                    []}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_try_and_catch_blocks_both_returning_a_float_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() float {\n"
        "    try {\n"
        "        42.0\n"
        "    } catch :error {\n"
        "        13.8\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Maybe', 0}]},
        {function, 2, 'Maybe', 0, [
            {clause, 2, [], [], [
                {'try', 3, [{float, 4, 42.0}], [],
                    [
                        {clause, 5,
                            [{tuple, 5, [{atom, 5, throw}, {atom, 5, error}, {var, 5, '_'}]}], [], [
                                {float, 6, 13.8}
                            ]}
                    ],
                    []}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_try_and_catch_blocks_both_returning_an_int_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() int {\n"
        "    try {\n"
        "        42\n"
        "    } catch :error {\n"
        "        13\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Maybe', 0}]},
        {function, 2, 'Maybe', 0, [
            {clause, 2, [], [], [
                {'try', 3, [{integer, 4, 42}], [],
                    [
                        {clause, 5,
                            [{tuple, 5, [{atom, 5, throw}, {atom, 5, error}, {var, 5, '_'}]}], [], [
                                {integer, 6, 13}
                            ]}
                    ],
                    []}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_try_and_catch_blocks_both_returning_a_string_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() string {\n"
        "    try {\n"
        "        \"ok\"\n"
        "    } catch :error {\n"
        "        \"error\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Maybe', 0}]},
        {function, 2, 'Maybe', 0, [
            {clause, 2, [], [], [
                {'try', 3,
                    [
                        {tuple, 4, [
                            {atom, 4, string},
                            {bin, 4, [{bin_element, 4, {string, 4, "ok"}, default, default}]}
                        ]}
                    ],
                    [],
                    [
                        {clause, 5,
                            [{tuple, 5, [{atom, 5, throw}, {atom, 5, error}, {var, 5, '_'}]}], [], [
                                {tuple, 6, [
                                    {atom, 6, string},
                                    {bin, 6, [
                                        {bin_element, 6, {string, 6, "error"}, default, default}
                                    ]}
                                ]}
                            ]}
                    ],
                    []}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_try_after_block_test() ->
    RufusText =
        "module example\n"
        "func cleanup() atom { :cleanup }\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } after {\n"
        "        cleanup()\n"
        "    }\n"
        "}",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 3, export, [{'Maybe', 0}]},
        {function, 3, 'Maybe', 0, [
            {clause, 3, [], [], [
                {'try', 4, [{atom, 5, ok}], [], [], [{call, 7, {atom, 7, cleanup}, []}]}
            ]}
        ]},
        {function, 2, cleanup, 0, [{clause, 2, [], [], [{atom, 2, cleanup}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_bare_catch_block_and_an_after_block_test() ->
    RufusText =
        "module example\n"
        "func cleanup() atom { :cleanup }\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "        :error\n"
        "    } after {\n"
        "        cleanup()\n"
        "    }\n"
        "}",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 3, export, [{'Maybe', 0}]},
        {function, 3, 'Maybe', 0, [
            {clause, 3, [], [], [
                {'try', 4, [{atom, 5, ok}], [],
                    [
                        {clause, 6, [{tuple, 6, [{atom, 6, throw}, {var, 6, '_'}, {var, 6, '_'}]}],
                            [], [{atom, 7, error}]}
                    ],
                    [{call, 9, {atom, 9, cleanup}, []}]}
            ]}
        ]},
        {function, 2, cleanup, 0, [{clause, 2, [], [], [{atom, 2, cleanup}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).
