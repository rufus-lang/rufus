-module(rufus_erlang_throw_test).

-include_lib("eunit/include/eunit.hrl").

forms_for_function_with_throw_atom_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw :kaboom\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Explode', 0}]},
        {function, 2, 'Explode', 0, [
            {clause, 2, [], [], [{call, 3, {atom, 3, throw}, [{atom, 3, kaboom}]}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

form_for_function_with_throw_bool_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw true\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Explode', 0}]},
        {function, 2, 'Explode', 0, [
            {clause, 2, [], [], [{call, 3, {atom, 3, throw}, [{atom, 3, true}]}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

form_for_function_with_throw_float_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw 42.0\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Explode', 0}]},
        {function, 2, 'Explode', 0, [
            {clause, 2, [], [], [{call, 3, {atom, 3, throw}, [{float, 3, 42.0}]}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

form_for_function_with_throw_int_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw 42\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Explode', 0}]},
        {function, 2, 'Explode', 0, [
            {clause, 2, [], [], [{call, 3, {atom, 3, throw}, [{integer, 3, 42}]}]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

form_for_function_with_throw_string_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw \"kaboom\"\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Explode', 0}]},
        {function, 2, 'Explode', 0, [
            {clause, 2, [], [], [
                {call, 3, {atom, 3, throw}, [
                    {tuple, 3, [
                        {atom, 3, string},
                        {bin, 3, [{bin_element, 3, {string, 3, "kaboom"}, default, default}]}
                    ]}
                ]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

form_for_function_with_throw_variable_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    a = :kaboom\n"
        "    throw a\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Explode', 0}]},
        {function, 2, 'Explode', 0, [
            {clause, 2, [], [], [
                {match, 3, {var, 3, a}, {atom, 3, kaboom}},
                {call, 4, {atom, 4, throw}, [{var, 4, a}]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

form_for_function_with_throw_cons_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    head = 1\n"
        "    tail = list[int]{2, 3, 4}\n"
        "    throw list[int]{head|tail}\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Explode', 0}]},
        {function, 2, 'Explode', 0, [
            {clause, 2, [], [], [
                {match, 3, {var, 3, head}, {integer, 3, 1}},
                {match, 4, {var, 4, tail},
                    {cons, 4, {integer, 4, 2},
                        {cons, 4, {integer, 4, 3}, {cons, 4, {integer, 4, 4}, {nil, 4}}}}},
                {call, 5, {atom, 5, throw}, [{cons, 5, {var, 5, head}, {var, 5, tail}}]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

form_for_function_with_throw_match_op_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw 1 = 1\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'Explode', 0}]},
        {function, 2, 'Explode', 0, [
            {clause, 2, [], [], [
                {call, 3, {atom, 3, throw}, [{match, 3, {integer, 3, 1}, {integer, 3, 1}}]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).
