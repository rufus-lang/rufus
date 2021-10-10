-module(rufus_erlang_case_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a literal value for scalar types

forms_for_function_with_case_block_with_single_atom_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value atom) string {\n"
        "    case value {\n"
        "    match :true ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'MaybeConvert', 1}]},
        {function, 2, 'MaybeConvert', 1, [
            {clause, 2, [{var, 2, value}],
                [
                    [
                        {call, 2, {remote, 2, {atom, 2, erlang}, {atom, 2, is_atom}}, [
                            {var, 2, value}
                        ]}
                    ]
                ],
                [
                    {'case', 3, {var, 3, value}, [
                        {clause, 4, [{atom, 4, true}], [], [
                            {tuple, 5, [
                                {atom, 5, string},
                                {bin, 5, [{bin_element, 5, {string, 5, "true"}, default, default}]}
                            ]}
                        ]}
                    ]}
                ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_case_block_with_single_bool_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value bool) string {\n"
        "    case value {\n"
        "    match true ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'MaybeConvert', 1}]},
        {function, 2, 'MaybeConvert', 1, [
            {clause, 2, [{var, 2, value}],
                [
                    [
                        {call, 2, {remote, 2, {atom, 2, erlang}, {atom, 2, is_boolean}}, [
                            {var, 2, value}
                        ]}
                    ]
                ],
                [
                    {'case', 3, {var, 3, value}, [
                        {clause, 4, [{atom, 4, true}], [], [
                            {tuple, 5, [
                                {atom, 5, string},
                                {bin, 5, [{bin_element, 5, {string, 5, "true"}, default, default}]}
                            ]}
                        ]}
                    ]}
                ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_case_block_with_single_float_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value float) string {\n"
        "    case value {\n"
        "    match 1.0 ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'MaybeConvert', 1}]},
        {function, 2, 'MaybeConvert', 1, [
            {clause, 2, [{var, 2, value}],
                [
                    [
                        {call, 2, {remote, 2, {atom, 2, erlang}, {atom, 2, is_float}}, [
                            {var, 2, value}
                        ]}
                    ]
                ],
                [
                    {'case', 3, {var, 3, value}, [
                        {clause, 4, [{float, 4, 1.0}], [], [
                            {tuple, 5, [
                                {atom, 5, string},
                                {bin, 5, [{bin_element, 5, {string, 5, "true"}, default, default}]}
                            ]}
                        ]}
                    ]}
                ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_case_block_with_single_int_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value int) string {\n"
        "    case value {\n"
        "    match 42 ->\n"
        "        \"true\"\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'MaybeConvert', 1}]},
        {function, 2, 'MaybeConvert', 1, [
            {clause, 2, [{var, 2, value}],
                [
                    [
                        {call, 2, {remote, 2, {atom, 2, erlang}, {atom, 2, is_integer}}, [
                            {var, 2, value}
                        ]}
                    ]
                ],
                [
                    {'case', 3, {var, 3, value}, [
                        {clause, 4, [{integer, 4, 42}], [], [
                            {tuple, 5, [
                                {atom, 5, string},
                                {bin, 5, [{bin_element, 5, {string, 5, "true"}, default, default}]}
                            ]}
                        ]}
                    ]}
                ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_case_block_with_single_string_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value string) atom {\n"
        "    case value {\n"
        "    match \"true\" ->\n"
        "        :true\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'MaybeConvert', 1}]},
        {function, 2, 'MaybeConvert', 1, [
            {clause, 2, [{tuple, 2, [{atom, 2, string}, {var, 2, value}]}], [], [
                {'case', 3, {tuple, 3, [{atom, 3, string}, {var, 3, value}]}, [
                    {clause, 4,
                        [
                            {tuple, 4, [
                                {atom, 4, string},
                                {bin, 4, [{bin_element, 4, {string, 4, "true"}, default, default}]}
                            ]}
                        ],
                        [], [{atom, 5, true}]}
                ]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_case_block_with_single_identifier_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value string) atom {\n"
        "    case value {\n"
        "    match v ->\n"
        "        :true\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'MaybeConvert', 1}]},
        {function, 2, 'MaybeConvert', 1, [
            {clause, 2, [{tuple, 2, [{atom, 2, string}, {var, 2, value}]}], [], [
                {'case', 3, {tuple, 3, [{atom, 3, string}, {var, 3, value}]}, [
                    {clause, 4, [{tuple, 4, [{atom, 4, string}, {var, 4, v}]}], [], [
                        {atom, 5, true}
                    ]}
                ]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_case_block_with_single_identifier_and_type_clause_test() ->
    RufusText =
        "module example\n"
        "func MaybeConvert(value string) atom {\n"
        "    case value {\n"
        "    match v string ->\n"
        "        :true\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 1, module, example},
        {attribute, 2, export, [{'MaybeConvert', 1}]},
        {function, 2, 'MaybeConvert', 1, [
            {clause, 2, [{tuple, 2, [{atom, 2, string}, {var, 2, value}]}], [], [
                {'case', 3, {tuple, 3, [{atom, 3, string}, {var, 3, value}]}, [
                    {clause, 4, [{tuple, 4, [{atom, 4, string}, {var, 4, v}]}], [], [
                        {atom, 5, true}
                    ]}
                ]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).
