-module(rufus_erlang_call_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions being called

forms_for_function_call_test() ->
    RufusText = "
    module example
    func Four() int { 4 }
    func Random() int { Four() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 4, export, [{'Random', 0}]},
        {attribute, 3, export, [{'Four', 0}]},
        {function, 3, 'Four', 0, [{clause, 3, [], [], [{integer, 3, 4}]}]},
        {function, 4, 'Random', 0, [{clause, 4, [], [], [{call, 4, {atom, 4, 'Four'}, []}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-1 functions being called

forms_for_function_call_with_an_atom_argument_test() ->
    RufusText = "
    module example
    func Echo(a atom) atom { a }
    func Random() atom { Echo(:hello) }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 4, export, [{'Random', 0}]},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [{clause, 3, [{var, 3, a}], [
                [{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_atom}}, [{var, 3, a}]}]
            ], [{var, 3, a}]}]},
        {function, 4, 'Random', 0, [{clause, 4, [], [], [{call, 4, {atom, 4, 'Echo'}, [{atom, 4, hello}]}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_call_with_a_bool_argument_test() ->
    RufusText = "
    module example
    func Echo(b bool) bool { b }
    func Random() bool { Echo(true) }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 4, export, [{'Random', 0}]},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [
            {clause, 3, [{var, 3, b}], [
                [{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_boolean}}, [{var, 3, b}]}]],
                [{var, 3, b}]}
            ]},
        {function, 4, 'Random', 0, [
            {clause, 4, [], [], [
                {call, 4, {atom, 4, 'Echo'}, [{atom, 4, true}]}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_call_with_a_float_argument_test() ->
    RufusText = "
    module example
    func Echo(n float) float { n }
    func Random() float { Echo(4.0) }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 4, export, [{'Random', 0}]},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [{clause, 3, [{var, 3, n}], [
                [{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_float}}, [{var, 3, n}]}]
            ], [{var, 3, n}]}]},
        {function, 4, 'Random', 0, [{clause, 4, [], [], [{call, 4, {atom, 4, 'Echo'}, [{float, 4, 4.0}]}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_call_with_an_int_argument_test() ->
    RufusText = "
    module example
    func Echo(n int) int { n }
    func Random() int { Echo(4) }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 4, export, [{'Random', 0}]},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [{clause, 3, [{var, 3, n}], [
                [{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [{var, 3, n}]}]
            ], [{var, 3, n}]}]},
        {function, 4, 'Random', 0, [{clause, 4, [], [], [{call, 4, {atom, 4, 'Echo'}, [{integer, 4, 4}]}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_call_with_a_string_argument_test() ->
    RufusText = "
    module example
    func Echo(t string) string { t }
    func Random() string { Echo(\"hello\") }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 4, export, [{'Random', 0}]},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1, [
                {clause, 3, [{tuple, 3, [{atom, 3, string}, {var, 3, t}]}], [], [{tuple, 3, [{atom, 3, string}, {var, 3, t}]}]}
            ]},
        {function, 4, 'Random', 0, [
                {clause, 4, [], [], [{call, 4, {atom, 4, 'Echo'}, [{tuple, 4, [{atom, 4, string}, {bin, 4, [{bin_element, 4, {string, 4, "hello"}, default, default}]}]}]}]}
            ]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-2 functions being called

forms_for_function_call_with_two_int_arguments_test() ->
    RufusText = "
    module example
    func Sum(m int, n int) int { m + n }
    func Random() int { Sum(1, 2) }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Sum', 2}]},
        {attribute, 4, export, [{'Random', 0}]},
        {function, 4, 'Random', 0,
            [{clause, 4,
                 [],
                 [],
                 [{call, 4, {atom, 4, 'Sum'}, [{integer, 4, 1}, {integer, 4, 2}]}]}]
        },
        {function, 3, 'Sum', 2,
            [{clause, 3,
                 [{var, 3, m}, {var, 3, n}],
                 [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [{var, 3, n}]}],
                  [{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [{var, 3, m}]}]],
                 [{op, 3, '+', {var, 3, m}, {var, 3, n}}]}]
        }
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_call_with_two_float_arguments_test() ->
    RufusText = "
    module example
    func Sum(m float, n float) float { m + n }
    func Random() float { Sum(1.2, 2.3) }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Sum', 2}]},
        {attribute, 4, export, [{'Random', 0}]},
        {function, 4, 'Random', 0,
            [{clause, 4,
                 [],
                 [],
                 [{call, 4, {atom, 4, 'Sum'}, [{float, 4, 1.2}, {float, 4, 2.3}]}]}]
        },
        {function, 3, 'Sum', 2,
            [{clause, 3,
                 [{var, 3, m}, {var, 3, n}],
                 [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_float}}, [{var, 3, n}]}],
                  [{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_float}}, [{var, 3, m}]}]],
                 [{op, 3, '+', {var, 3, m}, {var, 3, n}}]}]
        }
    ],
    ?assertEqual(Expected, ErlangForms).
