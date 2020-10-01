-module(rufus_compile_func_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a literal value for scalar types

eval_function_returning_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Ping() atom { :pong }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(pong, example:'Ping'()).

eval_function_returning_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func True() bool { true }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(true, example:'True'()).

eval_function_returning_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Pi() float { 3.14159265359 }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3.14159265359, example:'Pi'()).

eval_function_returning_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Number() int { 42 }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(42, example:'Number'()).

eval_function_returning_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Greeting() string { \"Hello\" }\n"
        "    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({string, <<"Hello">>}, example:'Greeting'()).

%% Arity-0 functions with multiple function expressions

forms_for_function_with_multiple_expressions_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Multiple() atom {\n"
        "        42\n"
        "        :fortytwo\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(fortytwo, example:'Multiple'()).

%% Arity-1 functions taking an unused parameter for scalar types

eval_function_taking_an_atom_and_returning_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Ping(m atom) atom { :pong }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(pong, example:'Ping'(hello)).

eval_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(n bool) bool { true }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'MaybeEcho'(false)).

eval_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(n float) float { 3.14159265359 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'MaybeEcho'(3.14)).

eval_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(n int) int { 42 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'MaybeEcho'(42)).

eval_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MaybeEcho(n string) string { \"Hello\" }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"Hello">>}, example:'MaybeEcho'({string, <<"Good morning">>})).

%% Arity-1 functions taking an argument and returning it for scalar types

eval_function_taking_an_atom_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(m atom) atom { m }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(hello, example:'Echo'(hello)).

eval_function_taking_a_bool_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(b bool) bool { b }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(false, example:'Echo'(false)).

eval_function_taking_a_float_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n float) float { n }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'Echo'(3.14159265359)).

eval_function_taking_an_int_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n int) int { n }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'Echo'(42)).

eval_function_taking_a_string_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(s string) string { s }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"Hello">>}, example:'Echo'({string, <<"Hello">>})).

%% Type checking return values

eval_function_having_unmatched_return_types_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Number() float { 42 }\n"
        "    ",
    Data = #{
        expr =>
            {int_lit, #{
                line => 3,
                spec => 42,
                type =>
                    {type, #{
                        line => 3,
                        source => inferred,
                        spec => int
                    }}
            }},
        return_type =>
            {type, #{
                line => 3,
                source => rufus_text,
                spec => float
            }}
    },
    ?assertEqual({error, unmatched_return_type, Data}, rufus_compile:eval(RufusText)).

%% Arity-1 functions taking and returning an argument with a mismatched return
%% type

eval_function_taking_a_bool_and_returning_it_with_a_mismatched_return_type_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func MismatchedReturnType(b bool) int { b }\n"
        "    ",
    Data = #{
        expr =>
            {identifier, #{
                line => 3,
                spec => b,
                type =>
                    {type, #{
                        line => 3,
                        source => rufus_text,
                        spec => bool
                    }}
            }},
        return_type =>
            {type, #{
                line => 3,
                source => rufus_text,
                spec => int
            }}
    },
    ?assertEqual({error, unmatched_return_type, Data}, rufus_compile:eval(RufusText)).

%% Function exports

eval_private_function_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func echo(s string) string { s }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertError(undef, example:echo({string, <<"Hello">>})).

%% Functions with parameter patterns containing literal values

eval_function_taking_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(:ok) atom { :ok }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(ok, example:'Echo'(ok)).

eval_function_taking_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(true) bool { true }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(true, example:'Echo'(true)).

eval_function_taking_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(1.0) float { 1.0 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(1.0, example:'Echo'(1.0)).

eval_function_taking_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(1) int { 1 }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(1, example:'Echo'(1)).

eval_function_taking_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(\"ok\") string { \"ok\" }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"ok">>}, example:'Echo'({string, <<"ok">>})).

eval_function_taking_an_atom_literal_in_multiple_function_heads_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(:alright) atom { :alright }\n"
        "    func Echo(:ok) atom { :ok }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(alright, example:'Echo'(alright)),
    ?assertEqual(ok, example:'Echo'(ok)).

eval_function_taking_an_atom_literal_and_passing_an_invalid_value_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(:ok) atom { :ok }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertError(function_clause, example:'Echo'(fine)).

%% Anonymous functions

rval_function_returning_a_nested_function_test() ->
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
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertError(42, (example:'NumberFunc'())()).
