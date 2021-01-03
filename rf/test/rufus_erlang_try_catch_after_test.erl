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
