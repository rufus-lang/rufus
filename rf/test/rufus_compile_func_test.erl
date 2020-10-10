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
        actual => int,
        expected => float,
        expr =>
            {int_lit, #{
                line => 3,
                spec => 42,
                type =>
                    {type, #{line => 3, source => inferred, spec => int}}
            }},
        globals => #{
            'Number' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => float}},
                    source => rufus_text,
                    spec => 'func() float'
                }}
            ]
        },
        return_type =>
            {type, #{line => 3, source => rufus_text, spec => float}}
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
        actual => bool,
        expected => int,
        expr =>
            {identifier, #{
                line => 3,
                spec => b,
                type =>
                    {type, #{line => 3, source => rufus_text, spec => bool}}
            }},
        globals => #{
            'MismatchedReturnType' => [
                {type, #{
                    kind => func,
                    line => 3,
                    param_types => [
                        {type, #{
                            line => 3,
                            source => rufus_text,
                            spec => bool
                        }}
                    ],
                    return_type =>
                        {type, #{line => 3, source => rufus_text, spec => int}},
                    source => rufus_text,
                    spec => 'func(bool) int'
                }}
            ]
        },
        return_type =>
            {type, #{line => 3, source => rufus_text, spec => int}}
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

eval_function_taking_and_returning_a_function_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(fn func() int) func() int {\n"
        "        fn\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    Fun = example:'Echo'(fun() -> 42 end),
    ?assert(is_function(Fun)),
    ?assertEqual(42, Fun()).

eval_function_returning_a_function_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func NumberFunc() func() int {\n"
        "        func() int { 42 }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    Fun = example:'NumberFunc'(),
    ?assert(is_function(Fun)),
    ?assertEqual(42, Fun()).

eval_function_returning_a_function_variable_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func NumberFunc() func() int {\n"
        "        fn = func() int { 21 + 21 }\n"
        "        fn\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    NumberFunc = example:'NumberFunc'(),
    ?assert(is_function(NumberFunc)),
    ?assertEqual(42, NumberFunc()).

eval_function_returning_a_nested_function_test() ->
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
    NumberFunc = example:'NumberFunc'(),
    ?assert(is_function(NumberFunc)),
    ?assertEqual(42, NumberFunc()).

eval_anonymous_function_taking_an_atom_and_returning_an_atom_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(atom) atom {\n"
        "        func(value atom) atom { value }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    EchoFunc = example:'EchoFunc'(),
    ?assert(is_function(EchoFunc)),
    ?assertEqual(rufus, EchoFunc(rufus)).

eval_for_anonymous_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(bool) bool {\n"
        "        func(value bool) bool { value }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    EchoFunc = example:'EchoFunc'(),
    ?assert(is_function(EchoFunc)),
    ?assertEqual(true, EchoFunc(true)).

eval_for_anonymous_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(float) float {\n"
        "        func(value float) float { value }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    EchoFunc = example:'EchoFunc'(),
    ?assert(is_function(EchoFunc)),
    ?assertEqual(42.0, EchoFunc(42.0)).

eval_for_anonymous_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(int) int {\n"
        "        func(value int) int { value }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    EchoFunc = example:'EchoFunc'(),
    ?assert(is_function(EchoFunc)),
    ?assertEqual(42, EchoFunc(42)).

eval_for_anonymous_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoFunc() func(string) string {\n"
        "        func(value string) string { value }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    EchoFunc = example:'EchoFunc'(),
    ?assert(is_function(EchoFunc)),
    ?assertEqual({string, <<"rufus">>}, EchoFunc({string, <<"rufus">>})).

typecheck_and_annotate_for_anonymous_function_taking_a_cons_expression_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoNumberListFunc() func(list[int]) list[int] {\n"
        "        func(list[int]{head|tail}) list[int] { list[int]{head|tail} }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    EchoNumberListFunc = example:'EchoNumberListFunc'(),
    ?assert(is_function(EchoNumberListFunc)),
    ?assertEqual([1, 2, 3], EchoNumberListFunc([1, 2, 3])).

typecheck_and_annotate_for_anonymous_function_taking_a_list_literal_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func EchoNumberListFunc() func(list[int]) list[int] {\n"
        "        func(numbers = list[int]{1, 2, 3}) list[int] { numbers }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    EchoNumberListFunc = example:'EchoNumberListFunc'(),
    ?assert(is_function(EchoNumberListFunc)),
    ?assertEqual([1, 2, 3], EchoNumberListFunc([1, 2, 3])).

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
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    EchoFortyTwoFunc = example:'EchoFortyTwoFunc'(),
    ?assert(is_function(EchoFortyTwoFunc)),
    ?assertEqual(42, EchoFortyTwoFunc(42)).

typecheck_and_annotate_closure_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Memoize(num int) func() int {\n"
        "        func() int { num }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    FortyTwoFunc = example:'Memoize'(42),
    ?assert(is_function(FortyTwoFunc)),
    ?assertEqual(42, FortyTwoFunc()),
    ?assertEqual(42, FortyTwoFunc()).
