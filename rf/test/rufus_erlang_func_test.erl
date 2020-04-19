-module(rufus_erlang_func_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a literal value for scalar types

forms_for_function_returning_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping() atom { :pong }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    AtomExpr = {atom, 3, pong},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Ping', 0}]},
        {function, 3, 'Ping', 0, [{clause, 3, [], [], [AtomExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func False() bool { false }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'False', 0}]},
        {function, 3, 'False', 0, [{clause, 3, [], [], [{atom, 3, false}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func Pi() float { 3.14159265359 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Pi', 0}]},
        {function, 3, 'Pi', 0, [{clause, 3, [], [], [{float, 3, 3.14159265359}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Number', 0}]},
        {function, 3, 'Number', 0, [{clause, 3, [], [], [{integer, 3, 42}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func Greeting() string { \"Hello\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
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
    RufusText = "
    module example
    func Multiple() atom {
        42;
        :fortytwo;
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Exprs = [{integer, 4, 42}, {atom, 5, fortytwo}],
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Multiple', 0}]},
        {function, 3, 'Multiple', 0, [{clause, 3, [], [], Exprs}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_multiple_expressions_with_blank_lines_test() ->
    RufusText = "
    module example
    func Multiple() atom {
        42;

        :fortytwo;
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Exprs = [{integer, 4, 42}, {atom, 6, fortytwo}],
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Multiple', 0}]},
        {function, 3, 'Multiple', 0, [{clause, 3, [], [], Exprs}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-1 functions taking an unused argument

forms_for_function_taking_an_unused_atom_and_returning_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping(m atom) atom { :pong }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Ping', 1}]},
        {function, 3, 'Ping', 1, [
            {clause, 3, [{var, 3, m}],
                        [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_atom}}, [{var, 3, m}]}]],
                        [{atom, 3, pong}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_bool_and_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(b bool) bool { true }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),

    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'MaybeEcho', 1}]},
        {function, 3, 'MaybeEcho', 1,
            [{clause, 3, [{var, 3, b}],
                         [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_boolean}}, [{var, 3, b}]}]],
                         [{atom, 3, true}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_float_and_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n float) float { 3.14159265359 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'MaybeEcho', 1}]},
        {function, 3, 'MaybeEcho', 1, [
            {clause, 3, [{var, 3, n}],
                        [[{call,3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_float}}, [{var, 3, n}]}]],
                        [{float, 3, 3.14159265359}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_int_and_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n int) int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'MaybeEcho', 1}]},
        {function, 3, 'MaybeEcho', 1,
            [{clause, 3, [{var, 3, n}],
                         [[{call,3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [{var, 3, n}]}]],
                         [{integer, 3, 42}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_string_and_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(s string) string { \"Hello\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    StringExpr = {bin_element, 3, {string, 3, "Hello"}, default, default},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'MaybeEcho', 1}]},
        {function, 3, 'MaybeEcho', 1,
            [{clause, 3, [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}],
                         [],
                         [{tuple, 3, [{atom, 3, string}, {bin, 3, [StringExpr]}]}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-1 functions taking and using an argument

forms_for_function_taking_an_atom_and_returning_an_atom_test() ->
    RufusText = "
    module example
    func Echo(m atom) atom { m }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1,
            [{clause, 3, [{var, 3, m}],
                         [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_atom}}, [{var, 3, m}]}]],
                         [{var, 3, m}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_bool_and_returning_a_bool_test() ->
    RufusText = "
    module example
    func Echo(b bool) bool { b }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1,
            [{clause, 3, [{var, 3, b}],
                         [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_boolean}}, [{var, 3, b}]}]],
                         [{var, 3, b}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_float_and_returning_a_float_test() ->
    RufusText = "
    module example
    func Echo(n float) float { n }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1,
            [{clause, 3, [{var, 3, n}],
                         [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_float}}, [{var, 3, n}]}]],
                         [{var, 3, n}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_int_and_returning_an_int_test() ->
    RufusText = "
    module example
    func Echo(n int) int { n }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1,
            [{clause, 3, [{var, 3, n}],
                         [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [{var, 3, n}]}]],
                         [{var, 3, n}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_string_and_returning_a_string_test() ->
    RufusText = "
    module example
    func Echo(s string) string { s }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1,
            [{clause, 3, [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}],
                         [],
                         [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Function exports

forms_for_private_functions_with_leading_lowercase_character_are_not_exported_test() ->
    RufusText = "
    module example
    func echo(s string) string { s }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {function, 3, 'echo', 1,
            [{clause, 3, [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}],
                         [],
                         [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Functions with parameter patterns containing literal values

forms_for_function_taking_an_atom_literal_test() ->
    RufusText = "
    module example
    func Echo(:ok) atom { :ok }
    ",
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
    RufusText = "
    module example
    func Echo(true) bool { true }
    ",
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
    RufusText = "
    module example
    func Echo(1.0) float { 1.0 }
    ",
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
    RufusText = "
    module example
    func Echo(1) int { 1 }
    ",
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
    RufusText = "
    module example
    func Echo(\"ok\") string { \"ok\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Echo', 1}]},
        {function, 3, 'Echo', 1,
         [{clause, 3, [{tuple, 3, [{atom, 3, string}, {bin, 3, [{bin_element, 3, {string, 3, "ok"}, default, default}]}]}],
           [],
           [{tuple, 3, [{atom, 3, string}, {bin, 3, [{bin_element, 3, {string, 3, "ok"}, default, default}]}]}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).
