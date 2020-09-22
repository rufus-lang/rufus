-module(rufus_erlang_match_test).

-include_lib("eunit/include/eunit.hrl").

forms_for_function_with_a_match_that_binds_an_atom_literal_test() ->
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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Ping', 0}]},
        {function, 3, 'Ping', 0, [
            {clause, 3, [], [], [
                {match, 4, {var, 4, response}, {atom, 4, pong}},
                {var, 5, response}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_a_match_that_binds_a_bool_literal_test() ->
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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Truthy', 0}]},
        {function, 3, 'Truthy', 0, [
            {clause, 3, [], [], [
                {match, 4, {var, 4, response}, {atom, 4, true}},
                {var, 5, response}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_a_match_that_binds_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Pi() float {\n"
        "        response = 3.14159265359\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Pi', 0}]},
        {function, 3, 'Pi', 0, [
            {clause, 3, [], [], [
                {match, 4, {var, 4, response}, {float, 4, 3.14159265359}},
                {var, 5, response}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_a_match_that_binds_an_int_literal_test() ->
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
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'FortyTwo', 0}]},
        {function, 3, 'FortyTwo', 0, [
            {clause, 3, [], [], [
                {match, 4, {var, 4, response}, {integer, 4, 42}},
                {var, 5, response}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_a_match_that_binds_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Greeting() string {\n"
        "        response = \"hello\"\n"
        "        response\n"
        "    }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Greeting', 0}]},
        {function, 3, 'Greeting', 0, [
            {clause, 3, [], [], [
                {match, 4, {tuple, 4, [{atom, 4, string}, {var, 4, response}]},
                    {tuple, 4, [
                        {atom, 4, string},
                        {bin, 4, [{bin_element, 4, {string, 4, "hello"}, default, default}]}
                    ]}},
                {tuple, 5, [{atom, 5, string}, {var, 5, response}]}
            ]}
        ]}
    ],
    ?assertEqual(Expected, ErlangForms).
